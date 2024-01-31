/* --- Copyright University of Nottingham 2011. All rights reserved. ------
 > File:            atms_nodes.p
 > Purpose:         A simple D-ATMS in Pop-11.
 > Author:          Hai Nguyen & Brian Logan, June 20 2011
 > Documentation:   Node-level interface to the ATMS.
 > Related Files:
 */

;;; Simple ATMS based on Justin Forder's Common Lisp ATMS extended to
;;; incorporate disjunctions.  Other changes include allowing
;;; assumptions to be justified and the omission of premises, i.e., we
;;; don't propagate contradictions back through justifications.

;;; This file containts the definitions of the following procedures.
vars procedure atms_init,

     procedure create_atms_environment,
     procedure get_atms_environment,
     procedure atms_subsumes,
     procedure atms_cseq_subsumes,
     procedure atms_union_environments,
     procedure atms_merge_environments,
     procedure atms_nogood_p,

     procedure create_atms_node,
     procedure atms_node_true_p,
     procedure atms_node_false_p,
     procedure create_atms_assumption,
     procedure create_atms_premise,

     procedure create_atms_justification,
     procedure update_atms_det_justification,
     procedure update_atms_nondet_justifications,
     procedure update_atms_node_label,
     procedure atms_update_falsity_label,
     procedure atms_remove_subsumed,
     procedure atms_remove_nogoods,
     procedure atms_union_labels,
     procedure atms_union_environment_list,
     procedure atms_merge_environment_list,
     procedure atms_sort_environment_list,
     procedure atms_label_=,
     procedure atms_get_longest_choice_sequences,
     procedure atms_update_longest_choice_sequences,
     procedure cartesian_product,
     procedure atms_node_justifs,
     procedure atms_lazy_node_label,
     procedure atms_mark_justifs,
     procedure atms_mark_justif,
     procedure atms_apply_justif_updates,
     procedure pprint_atms_justification;


uses atms_types
uses lisp_utils

;;; *** review for potential use of destructive list operations ***
;;; *** need I/O functions for assumptions, asets and environments ***
;;; *** should put in a package eventually ***

;;; Assumptions are named, and sets of assumptions are represented as
;;; integer bit-sequences.  Environment structures are discarded if they are
;;; not referenced by any nodes, so nogoods are represented as sets of
;;; assumptions, rather than as structures.

;;; The ATMS data node representing falsity
constant atms_falsity = consatms_node("falsity", [], true, [], []); 

;;; DO WE STILL NEED THIS?
;;; The empty choice sequence is held in the_empty_choice_sequence.
constant the_empty_choice_sequence = consatms_choice_sequence(0, 0, 0, 0, 0);

;;; A node is true (in all consistent environments, i.e. independent of
;;; assumptions) if its label contains the environment holding the empty
;;; set of assumptions.  As labels are minimised (i.e. subsumed
;;; environments are removed), this will be the only environment in such
;;; a node's label.  This is what de Kleer calls a premise. To make
;;; checking for truth efficient, the empty environment is held in
;;; the_empty_environment.
constant the_empty_environment = consatms_environment(0, 0, the_empty_choice_sequence, []);

;;; ATMS initial array size for the extensible vectors.
constant atms_initial_dimension = 700;

;;; The fill pointers or lengths of the following extensible vectors can
;;; be used to find the bit number for the next assumption to be created
;;; (fill-pointer *assumptions*) or (length *assumptions*), and the
;;; largest active entries in *environments* and *nogoods*.

;;; An extensible vector atms_choice_sequences is used to hold lists of
;;; choice sequences indexed by size. Only for debugging.
constant atms_choice_sequences = make_extensible_vector(atms_initial_dimension, 0, nil);

;;; An extensible vector atms_longest_choice_sequences is used to hold lists of
;;; longest (non subsumed) choice sequences indexed by length.
constant atms_longest_choice_sequences = make_extensible_vector(atms_initial_dimension, 0, nil);

;;; DO WE STILL NEED THIS?
;;; Put the_empty_choice_sequence in the vector of longest choice sequences
[ ^the_empty_choice_sequence ] -> atms_longest_choice_sequences(0);

;;; An extensible vector atms_environments is used to hold lists of
;;; environment structures indexed by size (i.e. number of assumptions).
constant atms_environments = make_extensible_vector(atms_initial_dimension, 0, nil);
				
;;; the_empty_environment is the only zero length environment
[ ^the_empty_environment ] -> atms_environments(0);

;;; An extensible vector atms_nogoods is used to hold lists of nogood
;;; (contradictory) environments indexed by size.
constant atms_nogoods = make_extensible_vector(atms_initial_dimension, 0, nil);

;;; An extensible vector atms_assumptions is used to hold assumption nodes,
;;; indexed by their bit numbers.  This allows conversion from a set of bits
;;; to a set of nodes.
constant atms_assumptions = make_extensible_vector(atms_initial_dimension, 0, nil);
   
;;; WE SHOULD REPRESENT DISJUNCTIONS BY THE NUMBER OF DISJUNCTS --
;;; INFORMATION ABOUT THE DISJUNCTS DOES NOT BELONG AT THE NODE LEVEL.

;;; An extensible vector atms_disjunctions is used to hold disjuctions,
;;; indexed by disjunction id. Each disjuction is represented by a list of
;;; disjunct instances.
constant atms_disjunctions = make_extensible_vector(atms_initial_dimension, 0, nil);

;;; An extensible vector atms_justifications is used to hold justifications,
;;; indexed by justif_id.
constant atms_justifications = make_extensible_vector(atms_initial_dimension, 0, nil);

;;; A property atms_apply_justif_p is used to mark justifications to be
;;; applied during the current lazy label update.
vars atms_apply_justif_p = newanyproperty([], 101, 1, false, syshash, 
					  nonop ==, "perm", false, false);


;;; Index of of the next justification to be created.
;;;vars atms_justification_index = 0;



;;; (Re)initialise the ATMS global variables (assumptions, environments and
;;; nogoods).  This is really only useful for debugging.
define atms_init();
    lvars struct, the_vector, initial_element, len, i;

    for struct in 
	[ ^atms_choice_sequences 
	  ^atms_longest_choice_sequences
	  ^atms_environments 
	  ^atms_nogoods 
	  ^atms_assumptions 
	  ^atms_disjunctions
 	  ^atms_justifications ] do

    	extensible_vector_initial_element(struct) -> initial_element;
	extensible_vector_length(struct) -> len;
    	for i from 0 to len - 1 do
	    initial_element -> struct(i);
	    endfor;
    	0 -> fill_pointer(struct);
	endfor;	

    ;;; Handle the special cases
    [ ^the_empty_choice_sequence ] -> atms_longest_choice_sequences(0);
    [ ^the_empty_environment ] -> atms_environments(0);

    ;;; Reinitialise the false node.
    [] -> atms_node_label_field(atms_falsity);
    [] -> atms_node_antecedents_field(atms_falsity);
    [] -> atms_node_consequents_field(atms_falsity);
    
    newanyproperty([], 101, 1, false, syshash, 
		   nonop =, "perm", false, false) -> atms_apply_justif_p;
		   
;;;    0 -> atms_justification_index;
    enddefine;


;;; Procedures to create the basic ATMS datastructures: environments, nodes,
;;; assumptions, and justifications.  It would be cleaner if the only way to
;;; create a node was to claim a justification, but we need a wrapper layer
;;; that maintains the uniqueness of node contents for that.

;;; Create an atms_environment and add it to atms_environments. We
;;; assume that the environment does not already exist (i.e., that this
;;; is called from create_atms_assumption).  Environments are created as
;;; needed to go in node labels, so the node for which this is an
;;; environment is passed as an argument, and is included in the
;;; atms_environment_nodes field. The caller is responsible for putting
;;; the new environment in the node's label.  Note that we do not update
;;; atms_choice_sequences here, as the fact that the enviroment is new
;;; does not mean that the choice sequence is new. The only place we
;;; know a choice sequence is guaranteed to be new is when we create a
;;; new nondet justification.
define create_atms_environment(aset, aset_size, 
			            disjunctions, disjunctions_length, 
				    choices, choices_length, choices_mask, 
			       	    node) -> environment;

    lvars cseq = consatms_choice_sequence(disjunctions, disjunctions_length, 
					  choices, choices_length, choices_mask);
    consatms_environment(aset, aset_size, cseq, [^node]) -> environment;
    ;;; ADD environment TO THE PROPERTY WITH aset|cset|choices AS KEY
    cons(environment, subscrextensible_vector(aset_size, atms_environments))
        -> subscrextensible_vector(aset_size, atms_environments);
    enddefine;

;;; Given an environment <e> created during label updating, check whether it
;;; is already registered with the atms, and associate it with <node>. We
;;; assume that <e> is not subsumed any any environment in the label of
;;; <node>.
define get_atms_environment(lw_env, node) -> environment;
    lvars aset = atms_aset(lw_env),
	  aset_size = atms_aset_size(lw_env),
	  disjunctions = atms_cseq_disjunctions(lw_env),
	  disjunctions_length = atms_cseq_disjunctions_length(lw_env),
	  choices = atms_cseq_choices(lw_env),
	  choices_length = atms_cseq_choices_length(lw_env),
	  choices_mask = atms_cseq_choices_mask(lw_env),
	  ;;; note that looking for the list of environments may extend the
	  ;;; vector ...
          environments = subscrextensible_vector(aset_size, atms_environments),
	  environment = false, e;

    ;;; REPLACE THIS WITH A PROPERTY LOOKUP
    for e in environments do 
	if aset_size = atms_aset_size(e) and 
	   disjunctions_length = atms_cseq_disjunctions_length(e) and 
	   choices_length = atms_cseq_choices_length(e) and 
	   choices_mask = atms_cseq_choices_mask(e) and 
	   aset = atms_aset(e) and
	   disjunctions = atms_cseq_disjunctions(e) and
	   choices = atms_cseq_choices(e) then
	    e -> environment;
	    quitloop();
	    endif;
	endfor;

    if environment then
	;;; Environment already exists: record that its now also an
	;;; environment for <node>
	cons(node, atms_environment_nodes(environment))
	    -> atms_environment_nodes(environment);
    else
	;;; This is a new environment: record that its an environment for
	;;; <node> and add it to the known atms_environments.
	[^node] -> atms_environment_nodes(lw_env);
    	cons(lw_env, subscrextensible_vector(aset_size, atms_environments))
            -> subscrextensible_vector(aset_size, atms_environments);
	lw_env -> environment;
    endif;
enddefine;


;;; Subsumption of environments. Assumption set aset1 of <env1> subsumes
;;; assumption set aset2 of <env2> if all aset1's bits appear in aset2.
;;; The sequence of choice sets <seq1> of <env1> subsumes the sequence
;;; of choice sets <seq2> of <env2> if for every choice set (d, b) in
;;; <seq1> there is a choice set (c, b') in <seq2> such that b' subsumes
;;; b, i.e., <seq1> is true for at least as many disjuncts of
;;; disjunction d.
define atms_subsumes(env1, env2) -> bool;
    lvars aset1 = atms_aset(env1), 
	  aset1_size = atms_aset_size(env1),
	  aset2 = atms_aset(env2),
	  aset2_size = atms_aset_size(env2);

    ;;; Use = or ==# in case of bigintegers
    aset1_size <= aset2_size and 
    (aset1 ==# (aset1 && aset2)) and
    atms_cseq_subsumes(env1, env2) -> bool;
    enddefine;


define atms_cseq_subsumes(env1, env2) -> bool;
    lvars disjunctions1 = atms_cseq_disjunctions(env1), 
	  disjunctions1_length = atms_cseq_disjunctions_length(env1), 
	  choices1 = atms_cseq_choices(env1), 
	  choices1_length = atms_cseq_choices_length(env1), 

	  disjunctions2 = atms_cseq_disjunctions(env2), 
	  disjunctions2_length = atms_cseq_disjunctions_length(env2), 
	  choices2 = atms_cseq_choices(env2), 
	  choices2_length = atms_cseq_choices_length(env2), 

    	  choices2_prefix, bool = false;

    if disjunctions1_length <= disjunctions2_length then
    	;;; A zero length choice sequence subsumes all other choice sequences
    	(disjunctions1_length == 0) or
	((disjunctions1 ==# (disjunctions1 && disjunctions2)) and
	 choices1_length <= choices2_length and 
	 ((choices2 >> (choices2_length - choices1_length)) ->> choices2_prefix) and
	 (choices2_prefix ==# (choices2_prefix && choices1))) 
	-> bool;
	endif;
    enddefine;


;;; Return the union of two environments or false if their union is not
;;; defined. The environment returned (if any) is not associated with any
;;; node(s).
define atms_union_environments(env1, env2) -> union;
    lvars aset, union = false;

    if atms_cseq_disjunctions_length(env1) <=
       atms_cseq_disjunctions_length(env2) then
       if atms_cseq_subsumes(env1, env2) then

    	   atms_aset(env1) || atms_aset(env2) -> aset;
    	   consatms_environment(aset, 
			     	integer_bitcount(aset), 
			     	atms_cseq(env2),
			     	[]) -> union;
	   endif;
    else
       	if atms_cseq_subsumes(env2, env1) then

    	    atms_aset(env1) || atms_aset(env2) -> aset;
    	    consatms_environment(aset, 
			     	 integer_bitcount(aset), 
			     	 atms_cseq(env1),
			     	 []) -> union;
	    endif;
	endif;
    enddefine;

;;; Return the merge two environments or false if their merge is not
;;; defined. The merge is defined if the choice sequences of each
;;; environment differ only in the choices of their last choice set. The
;;; returned environment is not associated with any node(s).
define atms_merge_environments(env1, env2) -> merge;
    lvars disjunctions1 = atms_cseq_disjunctions(env1),
	  disjunctions1_length = atms_cseq_disjunctions_length(env1),
	  choices1 = atms_cseq_choices(env1),
	  choices1_length = atms_cseq_choices_length(env1),
	  choices1_mask = atms_cseq_choices_mask(env1),

	  disjunctions2 = atms_cseq_disjunctions(env2),
 	  disjunctions2_length = atms_cseq_disjunctions_length(env2),
	  choices2 = atms_cseq_choices(env2),
	  choices2_length = atms_cseq_choices_length(env2),
	  choices2_mask = atms_cseq_choices_mask(env2),

	  aset, cseq, disjunctions_prefix, 
	  choices1_prefix = 0, choices2_prefix = 0, choices_prefix_mask = 0,
	  choices1_suffix, choices2_suffix, choices_suffix, all_choices,
	  merge = false;

    ;;; Preliminary Check that the merge is defined
    if disjunctions1_length >= 1 and 
       disjunctions1_length = disjunctions2_length and 
       choices1_length = choices2_length and 
       choices1_mask = choices2_mask and
       disjunctions1 ==# disjunctions2 then

        unless choices1_length = choices1_mask then 
            ;;; Sequence does not contain a single choice set so extract
            ;;; the prefix (recall that choices index from most
            ;;; significant bit)
       	    choices1 >> choices1_mask -> choices1_prefix;
       	    choices2 >> choices2_mask -> choices2_prefix;
	    endunless;

    	;;; For the merge to be defined, choices prefixes must be the same
	if choices1_prefix ==# choices2_prefix then

            ;;; Merge the environments
            atms_aset(env1) || atms_aset(env2) -> aset;
	    2 ** choices1_mask - 1 -> all_choices;
	    choices1 && all_choices -> choices1_suffix;
       	    choices2 && all_choices -> choices2_suffix;
	    choices1_suffix || choices2_suffix -> choices_suffix;

	    if choices_suffix = all_choices then
	    	;;; Datum has been derived on all branches: eliminate the
	    	;;; record of its dependency on the last record in the
	    	;;; sequence of choice sets

	    	;;; Remove the last disjunction from the sequence of choice
	    	;;; sets (perhaps replace with XOR?)
	    	false -> testbit(disjunctions1, 
				 integer_length(disjunctions1) - 1) 
	            -> disjunctions_prefix;

		unless disjunctions_prefix = 0 then
	    	    ;;; Mask for the choices prefix is the number of
	    	    ;;; disjuncts for the maximal disjunction index in the
	    	    ;;; disjunctions prefix
	    	    listlength(atms_disjunctions(integer_length(disjunctions_prefix) - 1)) 
	                -> choices_prefix_mask;
		    endunless;

	    	consatms_choice_sequence(disjunctions_prefix,
				     	 disjunctions1_length - 1,
				     	 choices1_prefix,
				     	 choices1_length - choices1_mask,
				     	 choices_prefix_mask) -> cseq;
    	    	consatms_environment(aset, 
				     integer_bitcount(aset), 
				     cseq,
			     	     []) -> merge;
	    
    	    else
	    	;;; Datum has not been derived on all branches
	    	consatms_choice_sequence(disjunctions1,
				     	 disjunctions1_length,
				     	 ;;; choices1 and choices2 are
				     	 ;;; identical up to the last choice
				     	 ;;; set, so or'ing them is safe.
				     	 choices1 || choices2,
				     	 choices1_length,
				     	 choices1_mask) -> cseq;
    	    	consatms_environment(aset, 
				     integer_bitcount(aset), 
				     cseq,
			     	     []) -> merge
		endif;
	    endif;
	endif;
    enddefine;

;;; Returns true if the environment <env> is subsumed by a nogood.
define atms_nogood_p(env) -> bool;
    lvars size = atms_aset_size(env),
	  bool = false,
	  i = 1;

    until i > size or i >= extensible_vector_length(atms_nogoods) do
	quitif(some(atms_nogoods(i),
	       	    procedure(nogood) -> bool;
		   	atms_subsumes(nogood, env) -> bool;
		   	endprocedure) ->> bool);
        i + 1 -> i;
	enduntil;	
    enddefine;


;;; Nodes and assumptions

;;; Creating a data node with a given datum.
;;; At the user level, it should be impossible to create a node without
;;; creating a justification for it -- use the EDS approach of claim_justif.
define create_atms_node(datum) -> atms_node;
    consatms_node(datum, [], false, [], []) -> atms_node;
    enddefine;

;;; Returns true if <node> is a premise.
define atms_node_true_p(node) -> bool;
    lvars environments = atms_node_label(node);
    ;;; The environments in a label are sorted by environment size, rather
    ;;; than indexed by size so the fact that Pop-11 lists index from 1
    ;;; doesn't matter.
    environments /== [] and hd(environments) == the_empty_environment -> bool;
    enddefine;

;;; Returns true if <node> currently has no consistent environment.
define atms_node_false_p(node) -> bool;
    atms_node_label(node) == []  -> bool;
    enddefine;

;;; The representation of assumptions is rather confusing: the assumption
;;; bit positions are allocated from 0, and the first assumption is put in
;;; 0th position of the atms_assumptions extensible_vector.  By default this
;;; is called 'ASSUMPTION 0' (if no other name is supplied by the user) and
;;; the environment consisting of just this assumption prints as [0].
;;; However the aset is actually the integer 1 (i.e. the integer with the
;;; 0th bit set).  It is not clear if it is better to represent assumptions
;;; by their position in the assumptions vector/bit position or their
;;; integer representation, but this may be confusing the datatype with its
;;; representation.

;;; We require that each assumption is named by its correspoinding
;;; datum. As a special case, if <name> is false, we generate a name.
;;; Each assumption is held in the next free position (corresponding to
;;; its bit number) in the atms_assumptions vector.
define create_atms_assumption(name) -> assumption;
    lvars number = fill_pointer(atms_assumptions),
	  bit = 1 << number,
	  datum = if name then name, else 'ASSUMPTION ' >< number, endif,
          assumption = consatms_assumption_node(datum, [], false, [], [], bit),
	  environment = create_atms_environment(bit, 1, 0, 0, 0, 0, 0, assumption);
   
    [ ^environment ] -> atms_assumption_node_label_field(assumption);

    ;;; We can use vector_push as the bits are allocated in sequence.
    extensible_vector_push_extend(assumption, atms_assumptions) ->;
    enddefine;

define create_atms_premise(name) -> assumption;
    lvars number = fill_pointer(atms_assumptions),
	  datum = if name then name, else 'PREMISE ' >< number, endif,
          assumption = consatms_assumption_node(datum, [], false, [], [],0);
	 
    [^the_empty_environment]-> atms_assumption_node_label_field(assumption);
  
    ;;; We can use vector_push as the bits are allocated in sequence.
    extensible_vector_push_extend(assumption, atms_assumptions) ->;
    enddefine;


;;; Justifications and labels

;;; Creating a justification for a data node involves creating a
;;; justification with the node as consequent and the given informant
;;; and antecedents, adding this justification to the antecedents of the
;;; node and to the consequents of each antecedent.
define create_atms_justification(consequent, informant, antecedents) -> justif;
    lvars id = fill_pointer(atms_justifications),
	  justif = consatms_justification(consequent, 
					  informant, 
					  id, 
					  antecedents, 
					  false),
	  antecedent;

    ;;; Add the justification to the antecedents of the consequent and
    ;;; the consequents of each antecedent
    cons(justif, atms_node_antecedents(consequent))
        -> atms_node_antecedents(consequent);
    for antecedent in antecedents do
    	cons(justif, atms_node_consequents(antecedent))
            -> atms_node_consequents(antecedent);
    	endfor;
    extensible_vector_push_extend(justif, atms_justifications) ->;
    enddefine;
	
;;; Updating the label of a node given a new (deterministic)
;;; justification for the node involves calculating assumption sets from
;;; the antecedents by taking the cartesian product of the antecedents'
;;; label environments, taking the union of each set, minimsing them by
;;; removing subsumed environments and removing nogoods, and then if the
;;; consequent is contradictory, make them nogoods. Note that for the
;;; justification to be valid, all the antecedents must be on the same
;;; branch of the tableaux, so the resulting environments can't be
;;; merged. We do the update if atms_lazy_labels is false, or if update
;;; is lazy, we traverse a justification if it leads ultimately to the
;;; node whose label is being computed (i.e., its apply_flag is true),
;;; and if its id is less than or equal to the limit_id for this update
;;; (i.e., the justification would have existed if the update were not
;;; lazy). Note that this may be called recursively by
;;; update_atms_node_label, so we need to check atms_lazy_labels here as
;;; well as in atms_det_justify.
define update_atms_det_justification_old(justif, limit_id);
    if not(atms_lazy_labels) or 
       (atms_justification_applied_flag(justif) and 
	atms_justification_id(justif) <= limit_id) then

    	lvars consequent = atms_justification_consequent(justif),
	      ;;; Filter out premises
	      antecedents = remove_if(atms_node_true_p, atms_justification_antecedents(justif));

    	if antecedents /== [] and not(atms_node_true_p(consequent)) then
            lvars eset = [% applist(cartesian_product(maplist(antecedents,
						              atms_node_label)),
	                    atms_union_environment_list) %],
       	      	  new_eset = atms_remove_subsumed(atms_remove_nogoods(eset));

	    ;;; With lazy label update, new_eset can be empty
	    unless new_eset == [] then 
	    	if atms_node_contradictory(consequent) then
       	    	    ;;; This is a new justification for the false node.
	    	    atms_update_falsity_label(new_eset);
    	    	else
	    	    update_atms_node_label(consequent, new_eset, limit_id);
	    	    endif;
	    	endunless;
	    endif;
	endif;
    enddefine;

define update_atms_det_justification(justif, limit_id);
    if not(atms_lazy_labels) or 
       (atms_justification_applied_flag(justif) and 
	atms_justification_id(justif) <= limit_id) then

    	lvars consequent = atms_justification_consequent(justif),
	      antecedents = remove_if(atms_node_true_p, 
				      atms_justification_antecedents(justif));

    	if not(atms_node_true_p(consequent)) then
	    lvars eset, new_eset;

	    if antecedents == [] then
		[ ^the_empty_environment ] -> new_eset;
            else
            	[% applist(cartesian_product(maplist(antecedents,
						     atms_node_label)),
	                   atms_union_environment_list) %] -> eset;
       	      	  atms_remove_subsumed(atms_remove_nogoods(eset)) -> new_eset;
		  endif;

	    ;;; With lazy label update, new_eset can be empty
	    unless new_eset == [] then 
	    	if atms_node_contradictory(consequent) then
       	    	    ;;; This is a new justification for the false node.
	    	    atms_update_falsity_label(new_eset);
    	    	else
	    	    update_atms_node_label(consequent, new_eset, limit_id);
	    	    endif;
	    	endunless;
	    endif;
	endif;
    enddefine;

;;; Update the label of a node given a new nodeterministic justification.
;;; We assume that <justifs> are in order of disjunct_id. If atms_lazy_labels 
;;; is false this is called directly by atms_nondet_justify. During lazy
;;; evaluation, it is only ever called by atms_apply_justif_updates, so
;;; we want to do the update.
define update_atms_nondet_justifications(justifs, disjunction_id);
    ;;; All the justifs have the same antecedent, and the label of the
    ;;; antecedent is assumed to be minimal and not nogood.
    lvars antecedent = hd(atms_justification_antecedents(hd(justifs))),
	  limit_id = atms_justification_id(hd(justifs)),
	  disjunction_label = atms_node_label(antecedent),
	  cmap = newproperty([], 20, false, "tmparg"), 
	  longest_cseq_prefixes = [], env, prefix_envs;
    
    ;;; For each environment in disjunction_label, find the longest
    ;;; sequences of choice sets which extend/are subsumed by the
    ;;; sequence of choice sets of the environment. We assume that the
    ;;; current longest choice sequences are not mergable.
    [% for env in disjunction_label do
	   lvars aset = atms_aset(env),
	      	 aset_size = atms_aset_size(env),
	      	 cseq = atms_cseq(env),
		 cseq_suffixes,
		 cseq_suffix_envs;
	   
	   unless cmap(cseq) ->> cseq_suffix_envs then
	       atms_get_longest_choice_sequences(cseq) -> cseq_suffixes;
	       ;;; Remember the suffixes for updating
	       ;;; atms_longest_choice_sequences
	       cseq_suffixes :: longest_cseq_prefixes -> longest_cseq_prefixes;
	       ;;; Make environments that extend the choice sequence in env
	       maplist(cseq_suffixes,
		       procedure(cseq_suffix);
	       	   	   consatms_environment(aset, 
						aset_size,
						cseq_suffix,
						[]);
			   endprocedure) -> cseq_suffix_envs;
	       cseq_suffix_envs -> cmap(cseq);
	       endunless;
	   dl(cseq_suffix_envs);
	   endfor; %] -> prefix_envs;
    
    ;;; Extend the choice sequence of each environment in <prefix_envs> with
    ;;; an additional choice corresponding to a disjunct/justif, and use
    ;;; these extended environments as a label update for the disjunct.
    lvars num_disjuncts = listlength(justifs), 
	  disjunct_id = 0, 
	  longest_cseqs = [], 
	  cseq, justif;
    
    for justif in justifs do
	lvars consequent = atms_justification_consequent(justif),
	      cseqs = [],
	      prefix_env, envs;
	
	[% for prefix_env in prefix_envs do
	       lvars aset = atms_aset(prefix_env),
	      	     aset_size = atms_aset_size(prefix_env),
	       	     disjunctions = atms_cseq_disjunctions(prefix_env) || 
		         (1 << disjunction_id),
	      	     disjunctions_length = atms_cseq_disjunctions_length(prefix_env) + 1,
	      	     choices = ((atms_cseq_choices(prefix_env) << num_disjuncts) 
				|| (1 << disjunct_id)),
	      	     choices_length = atms_cseq_choices_length(prefix_env) + 
		         num_disjuncts,
	      	     choices_mask = num_disjuncts;
	       
	       ;;; Make the extended choice sequence and remember it
	       consatms_choice_sequence(disjunctions, 
				    	disjunctions_length, 
				    	choices, 
				    	choices_length, 
				    	choices_mask) -> cseq;
	       cseq :: cseqs -> cseqs;
	       
	       ;;; Note that create_atms_enviroment registers the created
	       ;;; environment in atms_environments and doesn't update
	       ;;; atms_choice_sequences, which is not what we want here.
	       consatms_environment(aset, aset_size, cseq, []);
	       endfor %] -> envs;
	
	;;; With lazy label update, new_eset can be empty
	unless envs == [] then 
	    update_atms_node_label(consequent, envs, limit_id);
	    endunless;
	disjunct_id + 1 -> disjunct_id;
	;;; Remember the new longest choice sequences for updating
        ;;; atms_longest_choice_sequences
        cseqs :: longest_cseqs -> longest_cseqs;
	endfor;
    
    ;;; Update atms_longest_choice_sequences with the new choice sequences
    atms_update_longest_choice_sequences(longest_cseq_prefixes, longest_cseqs);
    enddefine;

;;; Update the label of <node> with label update <eset> (a set of
;;; lightweight environments), and propagate the update if label of
;;; <node> changes.
define update_atms_node_label(node, eset, limit_id);
    lvars label_envs = atms_node_label(node),
	  ;;; Merge environments corresponding to different branches of
	  ;;; a disjunction. This gives a label that's a mixture of full
	  ;;; and lightweight environments, but we call
	  ;;; get_atms_environment for the merged environments anyway.
	  union_eset = atms_remove_nogoods(atms_union_labels(label_envs, eset)),
	  new_eset = atms_remove_subsumed(label_envs <> union_eset),
	  new_envs;

    unless atms_label_=(label_envs, new_eset) then
        ;;; Find or create a full environment any remaining lightweight
    	;;; environments in <eset>, and for all environments record that
    	;;; this is now an evironment for node.
	maplist(new_eset,
		procedure(e) -> environment;
		    if member(node, atms_environment_nodes(e)) then
			e -> environment;
		    else
			;;; <e> is either a lightweight environment or
			;;; new for <node>
		    	get_atms_environment(e, node) -> environment;
			endif;
		    endprocedure) -> new_envs;

    	;;; Remove the node from any environments which are not in the new label.
    	applist(label_envs,
	    	procedure(e);
		    lvars nodes, aset_size;
		    unless member(e, new_envs) then
		    	ncdelete(node, atms_environment_nodes(e)) -> nodes;
		    	nodes -> atms_environment_nodes(e);
		    	;;; If this leaves atms_environment_nodes empty for this
		    	;;; environment, delete the environment from
		    	;;; atms_environments
		    	if nodes == [] then
			    atms_aset_size(e) -> aset_size;
			    ncdelete(e, atms_environments(aset_size))
			        -> atms_environments(aset_size);
		    	    endif;
		    	endunless;
		    endprocedure);

    	;;; Update the node label
    	new_envs -> atms_node_label(node);
        ;;; Propagate the new label through the justification.
        applist(atms_node_consequents(node), 
	    	update_atms_det_justification(% limit_id %));
	endunless;
    enddefine;


;;; Update atms_nogoods with <new_nogoods> and remove all environments
;;; subsumed by a new nogood. Note the environments in <new_nogoods> are
;;; minimal and that any environments which are subsumed by an
;;; previously known nogood have been removed from <new_nogoods> in the
;;; caller.
define atms_update_falsity_label(new_nogoods);
    lvars nogoods_changed = false, new_nogood, nogoods, nogood, i;

    for new_nogood in new_nogoods do
    	;;; Ensure that <new_nogood> is not an environment for any node(s)
    	[] -> atms_environment_nodes(new_nogood);
    	;;; Remove any nogoods in atms_nogoods subsumed by new_nogood. Note
    	;;; that we only check from size + 1 because atms_nogoods(size) was
    	;;; checked in the caller of update_falsity_label, when existing
    	;;; nogoods were removed from new_nogoods.
	for i from atms_aset_size(new_nogood) + 1 to 
	    extensible_vector_length(atms_nogoods) - 1 do
	    atms_nogoods(i) -> nogoods;
	    for nogood in nogoods do
		if atms_subsumes(new_nogood, nogood) then
		    ncdelete(nogood, nogoods) -> nogoods;
		    true -> nogoods_changed;
		    endif;
		endfor;
	    if nogoods_changed then
		nogoods -> atms_nogoods(i);
		endif;
	    endfor;
	endfor;

    ;;; Merge any mergable environments in the nogood database and new_nogoods
    lvars falsity_label = [], union_envs;
    for i from 1 to extensible_vector_length(atms_nogoods) - 1 do
	atms_nogoods(i) <> falsity_label -> falsity_label;
	endfor;
    ;;; Note that we have to remove subsumed environments from the union
    ;;; of the nogood label and the label update, as the previous
    ;;; removal of subsumed environments may result in environments with
    ;;; no corresponding environment with which to merge (e.g., with
    ;;; binary disjunctions, we may end up with an odd number of
    ;;; environments in the nogood label + new_nogoods).
    atms_union_labels(falsity_label, new_nogoods) -> union_envs;
    unless union_envs == [] then
    	atms_remove_subsumed(falsity_label <> union_envs) -> nogoods;
	;;; The fact that we could merge new nogoods with nogoods
	;;; currently in the label of the false node, does not mean that
	;;; the label of the false node has changed since the merged
	;;; environments may not subsume the environments currently in
	;;; the label of the false node, but the test fails safe.
	true -> nogoods_changed;
	endunless;

    ;;; Note that with lazy evaluation of labels, there seems to be
    ;;; little reason to keep the labels of datum nodes consistent. So
    ;;; long as the label of the false node is minimal, we can filter
    ;;; any nogood environments from the labels of other nodes when
    ;;; generating explanations.
    if nogoods_changed then 
	;;; Nogoods currently in the label of the false node may have
	;;; changed -- rebuild atms_nogoods from scratch
    	for i from 0 to extensible_vector_length(atms_nogoods) do
	    nil -> atms_nogoods(i);
	    endfor;
    	0 -> fill_pointer(atms_nogoods);
    else
    	;;; new nogoods did not subsume and were not mergeable with any
    	;;; existing nogood environments
	new_nogoods -> nogoods;
	endif;

    ;;; This is very inefficient	
    for nogood in nogoods do
	lvars size = atms_aset_size(nogood);
        nogood :: atms_nogoods(size) -> subscrextensible_vector(size, atms_nogoods);
	;;; Update node labels by removing all environments subsumed by nogood
	for i from size to extensible_vector_length(atms_environments) - 1 do
	    lvars environments = [],
		  environment;
	    for environment in atms_environments(i) do
		if atms_subsumes(nogood, environment) then
		    applist(atms_environment_nodes(environment),
			    procedure(node);
				lvars label = atms_node_label(node);
				;;; For the following to work, all labels
				;;; must contain a reference to the
				;;; same environment.
				ncdelete(environment, label, nonop ==) -> label;
				label -> atms_node_label(node);
				endprocedure);
		else
		    cons(environment, environments) -> environments;
		    endif;
		endfor;
	    environments -> atms_environments(i);
	    endfor;
	endfor;	
    enddefine;


;;; Remove environments in <eset> subsumed by another environment in
;;; <eset>. Returned environments are associated with nodes if those in
;;; <eset> are.
define atms_remove_subsumed(eset) -> result;
;;;    lvars sorted_eset = 
;;;	      syssort(eset,
;;;		      procedure(env1, env2) -> bool;
;;;			  atms_aset_size(env1) <= atms_aset_size(env2) and 
;;;			  atms_cseq_disjunctions_length(env1) <=
;;;		          atms_cseq_disjunctions_length(env2) and 
;;;			  atms_cseq_choices_length(env2) <=
;;;			  atms_cseq_choices_length(env1) -> bool;
;;;			  endprocedure),
    lvars sorted_eset = atms_sort_environment_list(eset),
	  result = [], e, r, subsumed = false;

    ;;; Note that the following reverses the order of environments in
    ;;; the label wrt atms_environment_<, but as this is done uniformly
    ;;; for all labels, it doesn't matter.
    unless sorted_eset == [] then
    	[ ^(hd(sorted_eset))] -> result;
    	for e in tl(sorted_eset) do
	    false -> subsumed;
    	    for r in result do
	    	if atms_subsumes(r, e) then
		    true -> subsumed;
		    quitloop();
		    endif;
	    	endfor;
	    unless subsumed then
	    	cons(e, result) -> result;
	    	endunless;
	    endfor;
	endunless;

    enddefine;

;;; Remove all nogoods from <eset>. Returned environments are associated
;;; with nodes if those in <eset> are.
define atms_remove_nogoods(eset) -> valid_eset;
    remove_if(atms_nogood_p, eset) -> valid_eset;
    enddefine;
  
;;; Repeatedly replace any two environments env1 and env2 from the union
;;; of label_envs and update_envs with their merge (where this is
;;; defined) until there are no such pairs of environments left. We
;;; assume that the environments in <label_envs> and <update_envs> are
;;; already merged (or are known not to be mergeable). The returned
;;; environments may be lightweight, i.e., not currently associated with
;;; any node(s).
define atms_union_labels(label_envs, update_envs) -> merged_label;
    lvars merged_label = [],
	  label_env, update_env;

    if update_envs == [] then
	;;; We assume that no environments in label_envs can be merged.
     	label_envs -> merged_label;
    else
	atms_merge_environment_list(label_envs <> update_envs) -> merged_label;
	endif;
    enddefine;


;;; Return the union of a list of environments or nothing if their union is
;;; not defined.  The returned environments are lightweight, i.e., not
;;; currently associated with any node(s).
define atms_union_environment_list(environments);
    lvars union = hd(environments),
	  env, aset, cseq;
	    		
    fast_for env in tl(environments) do
	atms_union_environments(union, env) -> union;
    	if (union == false) then
	    return;
	    endif;
	endfor;
    return(union);
    enddefine;

;;; Return the merge of a list of environments, i.e, a list of environments
;;; which can't be further merged.  The returned environments may be
;;; lightweight, i.e., not currently associated with any node(s), even when
;;; the input <environments> are associated with nodes.
define atms_merge_environment_list(environments) -> nomerges;
    lvars nomerges = [],
	  dmap = newproperty([], 20, [], "tmparg"),
	  disjunctions, disjunctions_length, max_disjunctions_length = 0,
	  envs, envs2, unmerged_envs, env, merged_env, merged_disjunctions_length;

    ;;; Split the environments according to the length of their choice
    ;;; sequence.
    for env in environments do
	atms_cseq_disjunctions_length(env) -> disjunctions_length;
	cons(env, dmap(disjunctions_length)) -> dmap(disjunctions_length);
	if disjunctions_length > max_disjunctions_length then
	    disjunctions_length -> max_disjunctions_length;
	    endif;
	endfor;

    ;;; Process the environments in order of decreasing choice sequence
    ;;; length
    for disjunctions_length from max_disjunctions_length by -1 to 1 do 
	dmap(disjunctions_length) -> envs;

    	while (envs /== []) do
	    hd(envs) -> env;
	    tl(envs) -> envs;
	    [] -> unmerged_envs;
	    false -> merged_env;
	    for envs2 on envs do
	    	if (atms_merge_environments(env, hd(envs2)) ->> merged_env) then
		    atms_cseq_disjunctions_length(merged_env) 
		        -> merged_disjunctions_length;
		    if merged_disjunctions_length < disjunctions_length then
			cons(merged_env, dmap(merged_disjunctions_length)) 
			    -> dmap(merged_disjunctions_length);
			unmerged_envs <> tl(envs2) -> unmerged_envs;
			quitloop();
		    else
    			;;; Accumulate merges.
		    	merged_env -> env;
			endif;
	    	else
	    	    ;;; <unmerged_envs> contains environments that are not
	    	    ;;; mergeable with env
	            hd(envs2) :: unmerged_envs -> unmerged_envs;
		    endif;
	    	endfor;
	    unless merged_env and 
		   merged_disjunctions_length < disjunctions_length then
	    	;;; env can't be merged further
	        env :: nomerges -> nomerges;
		endunless;
	    unmerged_envs -> envs;
	    endwhile;	
	endfor;

    ;;; Environments with empty choice sequences are not mergable
    dmap(0) <> nomerges -> nomerges;
    enddefine;


;;; Sort a list of full or lightweight environments (e.g., a label or a
;;; label update) in standard order.
define atms_sort_environment_list(eset) -> sorted_eset;
    syssort(eset, atms_environment_<) -> sorted_eset;
    enddefine;

;;; Returns true if labels <eset1> and <eset2> (lists of full or
;;; lightweight environmets) are equal. Assumes that labels are sorted
;;; in standard order.
define atms_label_=(eset1, eset2) -> bool;
    lvars e1, e2, bool = false;
    
    if listlength(eset1) = listlength(eset2) then
	for e1, e2 in eset1, eset2 do
	    unless atms_environment_=(e1, e2) then
		return;
		endunless;
	    endfor;
	true -> bool;
	endif;
    enddefine;

;;; Given a choice sequence <prefix_cseq>, return a list of the longest
;;; suffixes of <prefix_cseq>. Note: atms_choice_sequences is a vector of
;;; list of contexts indexed by size
define atms_get_longest_choice_sequences(prefix_cseq) -> longest_cseqs;
    lvars disjunctions_length = atms_cseq_disjunctions_length(prefix_cseq),
	  longest_cseqs = [],
	  i, cseqs, cseq;
	  
    for i from disjunctions_length + 1 to
	fill_pointer(atms_longest_choice_sequences) - 1 do
	
	atms_longest_choice_sequences(i) -> cseqs;
	fast_for cseq in cseqs do
	    if atms_cseq_subsumes(prefix_cseq, cseq) then
                cseq :: longest_cseqs -> longest_cseqs;
		endif;
	    endfor;
	endfor;

    if longest_cseqs == [] then
	[^prefix_cseq] -> longest_cseqs;
	endif;
    enddefine;

;;; Update the longest choice sequences to incorporate the choice
;;; sequences in <update_cseqs> generated by adding a nondeterministic
;;; justification. The choice sequences in <cseqs> contain the choice
;;; sequences that were extended to create the choice sequences in
;;; <update_cseqs> and which can now be discarded. 
define atms_update_longest_choice_sequences(cseqs, update_cseqs);
    lvars disjunctions_length;
	  
    ;;; Delete the choice sequences that were extended
    applist(cseqs,
	    procedure(cseq_prefixes);
		applist(cseq_prefixes,
	    		procedure(cseq);
			    atms_cseq_disjunctions_length(cseq) -> disjunctions_length;
			    unless disjunctions_length == 0 then
				delete(cseq, atms_longest_choice_sequences(disjunctions_length)) 
			            -> subscrextensible_vector(disjunctions_length, atms_longest_choice_sequences);
				endunless;
			    endprocedure)
		endprocedure);

    ;;; Record the new longest choice sequences. Note that <update_envs> is a
    ;;; list of lists, one for each justif.
    applist(update_cseqs,
	    procedure(cseq_suffixes);
		applist(cseq_suffixes,
	    		procedure(cseq);
			    atms_cseq_disjunctions_length(cseq) -> disjunctions_length;

			    unless disjunctions_length == 0 then
			    	;;; Note that cseq is guaranteed to be unique
			    	cons(cseq, atms_longest_choice_sequences(disjunctions_length)) 
			            -> subscrextensible_vector(disjunctions_length, atms_longest_choice_sequences);
				
			    	;;; Debugging only -- remember all choice sequences
			    	cons(cseq, atms_choice_sequences(disjunctions_length)) 
			            -> subscrextensible_vector(disjunctions_length, atms_choice_sequences);
				endunless;
			    endprocedure)
		endprocedure);
    enddefine;

;;; Cartesian product of any number of sets, represented as lists.  This
;;; conses a list for each input set to hold the intermediate results the
;;; elements of these lists are resused and we only iterate over the members
;;; of each input set once.  This seems to be the fastest version if we
;;; don't mind the resulting list sharing structure.
define cartesian_product(sets) -> set;
    if sets /== [] then
	lvars this_set = hd(sets),
 	      tsets, element, set;
	cartesian_product(tl(sets)) -> tsets;
	;;; The original did this using mapcan, but our version of mapcan
	;;; uses maplist, and is very inefficient.
	[% for element in this_set do
	       for set in tsets do
		   cons(element, set),
		   endfor;
	       endfor %] -> set;
	sys_grbg_list(tsets);
    else
	[[]] -> set;
	endif;
   enddefine;


;;; Return all the justifications for nodes, recursively. We assume that
;;; all the nodes in <nodes> are unique.
define atms_node_justifs(nodes) -> justifs;
    lvars justifs = [],
	  justif_queue = flatten(maplist(nodes, atms_node_antecedents)),
	  antecedent_nodes, antecedent_justifs, justif;

    while justif_queue /== [] do
	hd(justif_queue) -> justif;
    	remove_if(member(% nodes %), 
		  atms_justification_antecedents(justif)) -> antecedent_nodes;
    	flatten(maplist(antecedent_nodes,
			atms_node_antecedents)) -> antecedent_justifs;
	justif :: justifs -> justifs;
	nodes <> antecedent_nodes -> nodes;
	tl(justif_queue) <> antecedent_justifs -> justif_queue;
	endwhile;

    syssort(justifs, 
	    procedure(justif1, justif2) -> bool;
		atms_justification_id(justif1) <=
		atms_justification_id(justif2) -> bool;
		endprocedure) -> justifs;
    enddefine;

;;; For incremental label update in lazy evaluation, We need to
;;; distinguish between justifs that have been processed before, and
;;; those that are new for this label update. The easiest way to do this
;;; is to keep a hashtable for the current update and use the justif
;;; apply flag to decide if the justif should be added to the table (it
;;; goes in the table if the apply flag is not set). We walk the justifs
;;; checking if the current justif is in the hash table, and if so
;;; update the label of the justified node. We can then use
;;; clearproperty to reinitialise for the next lazy evaluation. Rename
;;; flags atms_justification_applied_flag and the hash table
;;; atms_apply_justif_p

;;; Compute the labels of <nodes> from an unevaluated justification structure.
define atms_lazy_node_label(nodes);

    ;;; Skip if lazy evaluation is false (allows non lazy evaluation of
    ;;; labels for debugging)
    if atms_lazy_labels then
    	clearproperty(atms_apply_justif_p);
    	atms_mark_justifs(nodes);
    	atms_apply_justif_updates();
	endif;
    enddefine;


;;; Flag all the justifications for nodes, recursively. Justifs with the
;;; flag set will be processed by atms_apply_justif_updates. We use
;;; nodes for the closed list to avoid having to check both the open and
;;; closed lists on expansion.
define atms_mark_justifs(nodes);
    lvars justifs = flatten(maplist(nodes, atms_node_antecedents)),
	  antecedent_nodes, antecedent_justifs, justif;

    while justifs /== [] do
	hd(justifs) -> justif;
	atms_mark_justif(justif);
    	remove_if(member(% nodes %), 
		  atms_justification_antecedents(justif)) -> antecedent_nodes;
    	flatten(maplist(antecedent_nodes,
			atms_node_antecedents)) -> antecedent_justifs;
	nodes <> antecedent_nodes -> nodes;
	tl(justifs) <> antecedent_justifs -> justifs;
	endwhile;
    enddefine;


;;; Mark a justification for lazy evalution.
define atms_mark_justif(justif);
    ;;; We only want to apply a justif if it hasn't previuously been applied
    unless(atms_justification_applied_flag(justif)) then
    	true -> atms_apply_justif_p(justif);
    	endunless;
    enddefine;


define atms_apply_justif_updates();
    lvars justif, informant, limit_id, nondet_justifs, i = 0;

    while i < extensible_vector_length(atms_justifications) do
	atms_justifications(i) -> justif;
	atms_justification_informant(justif) -> informant;,

	if informant = 'DET' or informant = 'CONTRADICTION' then
	    if atms_apply_justif_p(justif) then
	    	atms_justification_id(justif) -> limit_id;, 
	    	true -> atms_justification_applied_flag(justif);
	        update_atms_det_justification(justif, limit_id);
		endif;
	    i + 1 -> i;
	else
	    ;;; Build a list of nondeteriministic justifications to be
	    ;;; updated. We rely on the fact that nondeterministic
	    ;;; justifications are created sequentially and have the
	    ;;; same disjunction_id for their informant. Note that the
	    ;;; apply_flag is ignored: 
	    i + 1 -> i;
	    [% justif,
	       until i = extensible_vector_length(atms_justifications) or
		     atms_justification_informant(atms_justifications(i)) /= informant do
		 
		   atms_justifications(i);
		   i + 1 -> i;
		   enduntil %] -> nondet_justifs;

	    ;;; Check that the target node has been derived on all branches.
	    if every(nondet_justifs, atms_apply_justif_p) then
	    	applist(nondet_justifs, 
			procedure(nondet_justif);
	    		    true -> atms_justification_applied_flag(nondet_justif);
	    		    endprocedure);
		update_atms_nondet_justifications(nondet_justifs, informant);
		endif;
	    endif;
	endwhile;	
    enddefine;

define pprint_atms_justification(justif);
   
    printf('\n<atms_justification: %p >\n %p\n DERIVED FROM\n',
	   [^(atms_justification_id(justif))
	    ^(atms_node_datum(atms_justification_consequent(justif)))]);
    atms_node_data(atms_justification_antecedents(justif))==>
enddefine;
    
;;; atms_nodes ends here
vars atms_nodes = true;
