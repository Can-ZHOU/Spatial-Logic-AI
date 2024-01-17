/* --- Copyright University of Nottingham 2011. All rights reserved. ------
 > File:            atms_types.p
 > Purpose:	    A simple D-ATMS in Pop-11.
 > Author:          Hai Nguyen &Brian Logan, June 20 2011
 > Documentation:   Datatypes for the D-ATMS.
 > Related Files:
 */

;;; This file contains the definitions of the following procedures
vars procedure make_atms_environment,
     procedure print_atms_environment,
     procedure atms_aset,
     procedure atms_aset_size,
     procedure atms_cseq,
     procedure atms_cseq_disjunctions,
     procedure atms_cseq_disjunctions_length,
     procedure atms_cseq_choices,
     procedure atms_cseq_choices_length,
     procedure atms_cseq_choices_mask,
     procedure atms_environment_<,
     procedure atms_environment_=,

     procedure make_atms_node,
     procedure print_atms_node,
     procedure make_atms_assumption_node,
     procedure print_atms_assumption_node,
     procedure atms_node_datum,
     procedure atms_node_label,
     procedure atms_node_contradictory,
     procedure atms_node_antecedents,
     procedure atms_node_consequents,

     procedure make_atms_justification,
     procedure print_atms_justification,

     procedure printable_atms_environment,
     procedure printable_atms_aset,
     procedure printable_atms_cseq,
     procedure printable_atms_label,
     procedure atms_justifs_consequent_data,
     procedure atms_justifs_antecedents_data,
     procedure atms_node_data,

     procedure make_extensible_vector,
     procedure print_extensible_vector,
     procedure extensible_vector_apply,
     procedure fill_pointer,
     procedure extensible_vector_length,
     procedure extensible_vector_push,
     procedure extensible_vector_push_extend,
     procedure extensible_vector_pop,
     procedure subscrextensible_vector,
     procedure extensible_vector_index,
     procedure trim_extensible_vector;

;;; The following is awkward: we shouldn't need to know things at the
;;; popatms level.
vars atms_disjunctions;

;;; It would be nice to make the fields of the records defined below lvars,
;;; but for efficiency it is useful to be able to get at them from the atms
;;; code.

;;; An environment consists of an assumption set and a choice sequence
;;; and the list of nodes referencing the environment. An assumption set
;;; is represented by fields containing its assumption set (an integer)
;;; and the number of assumptions in the set, and the maximal assumption
;;; id. Choice sequences are represented by an atms_choice_sequence
;;; record, as we need "bare" choice sequences when adding new
;;; nondeterministic justifications. We cache values that are tested
;;; frequently (such as sizes), but not those (such as maximal
;;; assumption and disjunction ids) which only elminate a small number
;;; of comparisons.
recordclass atms_environment
     atms_aset 
     atms_aset_size
     atms_environment_cseq_field
     atms_environment_nodes;

;;; A sequences of choice sets is represented by fields containing the
;;; choice sequence itself, i.e., a sequence of disjunction indicies (an
;;; integer), the length of the sequence of disjunction indices, the set
;;; of choices for each disjuction in the choice sequence under which a
;;; datum holds (an integer), the size of the set of choices, and the
;;; size of last set of choices in the sequence (required for merging
;;; environments). In the disjunctions_field (like the atms_aset field
;;; in atms_environments), the most significant bit represents the last
;;; disjunction_id in the choice sequence. However, in the interests of
;;; efficiency, in the choices_field, the least significant bits
;;; represent the choices (disjunct ids) for the last disjunction in the
;;; choice sequence.
recordclass atms_choice_sequence
     atms_choice_sequence_disjunctions_field
     atms_choice_sequence_disjunctions_length_field
     atms_choice_sequence_choices_field 
     atms_choice_sequence_choices_length_field
     atms_choice_sequence_choices_mask_field;


;;; atms_environment accessor functions. 
;;; Note that atms_environments are immutable.

define atms_cseq(environment) -> cseq;
    if isatms_environment(environment) then
	atms_environment_cseq_field(environment) -> cseq;
	endif;
    enddefine;

define updaterof atms_environment_cseq_field(cseq, environment);
    mishap('attempt to update disjunctions field for', [^environment]);
    enddefine;
    
define updaterof atms_aset(aset, environment);
    mishap('attempt to update aset field for', [^environment]);
    enddefine;

define updaterof atms_aset_size(size, environment);
    mishap('attempt to update aset_size field for', [^environment]);
    enddefine;

define updaterof atms_environment_cseq_field(cseq, environment);
    mishap('attempt to update cseq field for', [^environment]);
    enddefine;
    
;;; atms_choice_sequence accessor functions. 
;;; Note that atms_choice_sequences are immutable.

define atms_cseq_disjunctions(cseq) -> disjunctions;
    if isatms_environment(cseq) then
	atms_environment_cseq_field(cseq) -> cseq;
	endif;
    atms_choice_sequence_disjunctions_field(cseq) -> disjunctions;
    enddefine;

define updaterof atms_cseq_disjunctions(disjunctions, cseq);
    mishap('attempt to update disjunctions field for', [^cseq]);
    enddefine;
    
define atms_cseq_disjunctions_length(cseq) -> disjunctions_length;
    if isatms_environment(cseq) then
	atms_environment_cseq_field(cseq) -> cseq;
	endif;
    atms_choice_sequence_disjunctions_length_field(cseq) -> disjunctions_length;
    enddefine;

define updaterof atms_cseq_disjunctions_length(len, cseq);
    mishap('attempt to update disjunctions_length field for', [^cseq]);
    enddefine;
    
define atms_cseq_choices(cseq) -> choices;
    if isatms_environment(cseq) then
	atms_environment_cseq_field(cseq) -> cseq;
	endif;
    atms_choice_sequence_choices_field(cseq) -> choices;
    enddefine;

define updaterof atms_cseq_choices(choices, cseq);
    mishap('attempt to update choices field for', [^cseq]);
    enddefine;
    
define atms_cseq_choices_length(cseq) -> choices_length;
    if isatms_environment(cseq) then
	atms_environment_cseq_field(cseq) -> cseq;
	endif;
    atms_choice_sequence_choices_length_field(cseq) -> choices_length;
    enddefine;

define updaterof atms_cseq_choices_length(len, cseq);
    mishap('attempt to update choices_length field for', [^cseq]);
    enddefine;
    
define atms_cseq_choices_mask(cseq) -> choices_mask;
    if isatms_environment(cseq) then
	atms_environment_cseq_field(cseq) -> cseq;
	endif;
    atms_choice_sequence_choices_mask_field(cseq) -> choices_mask;
    enddefine;

define updaterof atms_cseq_choices_mask(mask, cseq);
    mishap('attempt to update choices_mask field for', [^cseq]);
    enddefine;


;;; Make a default atms_environment
define make_atms_environment() -> the_atms_environment;
    consatms_environment(0, 0, 
			 consatms_choice_sequence(0, 0, 0, 0, 0), 
			 []) -> the_atms_environment;
    enddefine;

;;; Print an atms_environment
define print_atms_environment(environment);
    lvars aset = atms_aset(environment),
    	  cseq = atms_cseq(environment);

    printf('<atms_environment: %p %p>', [ ^(printable_atms_aset(aset))
      	     				  ^(printable_atms_cseq(cseq)) ]);
    enddefine;
print_atms_environment -> class_print(atms_environment_key);

;;; True if environment <env1> comes before <env2> in the standard order.
define atms_environment_<(env1, env2);

    unless atms_aset_size(env1) == atms_aset_size(env2) then
	return (atms_aset_size(env1) < atms_aset_size(env2));
    elseunless atms_cseq_disjunctions_length(env1) ==
     	       atms_cseq_disjunctions_length(env2) then
        return (atms_cseq_disjunctions_length(env1) <
     	     	atms_cseq_disjunctions_length(env2));
    elseunless atms_cseq_choices_length(env1) == 
	       atms_cseq_choices_length(env2) then
        return (atms_cseq_choices_length(env1) < 
		atms_cseq_choices_length(env2));
    elseunless atms_aset(env1) ==# atms_aset(env2) then
	return (atms_aset(env1) < atms_aset(env2));
    elseunless atms_cseq_disjunctions(env1) ==# 
	       atms_cseq_disjunctions(env2) then
        return (atms_cseq_disjunctions(env1) < atms_cseq_disjunctions(env2));
    else
       return (atms_cseq_choices(env1) < atms_cseq_choices(env2));
       endunless;

    enddefine;

;;; True if environments <env1> and <env2> are equal. Note that we don't
;;; consider environment_nodes to allow comparison of full and
;;; lightweight environments.
define atms_environment_=(env1, env2) -> bool;
    atms_aset_size(env1) == atms_aset_size(env2) and
    atms_cseq_disjunctions_length(env1) == atms_cseq_disjunctions_length(env2) and
    atms_cseq_choices_length(env1) == atms_cseq_choices_length(env2) and

    atms_aset(env1) ==# atms_aset(env2) and
    atms_cseq_disjunctions(env1) ==# atms_cseq_disjunctions(env2) and
    atms_cseq_choices(env1) ==# atms_cseq_choices(env2)
    ;;; If the choices are the same, the mask must be the same.
    -> bool;
    enddefine;


;;; An ATMS node has a datum (holding the statement represented by the
;;; node), a label (holding a list of environments), a contradictory
;;; flag, an antecedents field, holding a list of justifications for
;;; which the node is the consequent (not used in assumptions, but
;;; having the field there and empty saves a type check in the code for
;;; propagating falsity), and a consequents field holding a list of
;;; justifications for which the node is an antecedent.
recordclass atms_node
     atms_node_datum_field
     atms_node_label_field
     atms_node_contradictory_field
     atms_node_antecedents_field
     atms_node_consequents_field;

;;; Make a default atms_node
define make_atms_node() -> the_atms_node;
    consatms_node(false, [], false, [], []) -> the_atms_node;
    enddefine;

;;; Print an atms_node.
;;; Note that this now prints the antecedents and consequents as lists of
;;; `short' justifications (the antecedents of the justif in the case of
;;; antecedents and the consequent of the justif in the case of the
;;; consequents) as it was not clear why the same datum should appear more
;;; than once in the list of consequents.  It is not clear if this is an
;;; improvement.
define print_atms_node(node);
    lvars label_strings = maplist(printable_atms_label(atms_node_label_field(node)),
				  sprintf(% '< atms_environment: %p >' %)), 
	  antecedents = atms_node_antecedents_field(node),
	  antecedents_data = atms_justifs_antecedents_data(antecedents),
	  antecedents_strings = maplist(antecedents_data,
				     	sprintf(% '<atms_justif: %p>' %)),
    	  consequents = atms_node_consequents_field(node),
	  consequents_data = atms_justifs_consequent_data(consequents),
	  consequents_strings = maplist(consequents_data,
				     	sprintf(% '<atms_justif: %p>' %));

    printf('< atms_node: %p %p\n     justifications %p\n    justifies %p >',
	   [^(atms_node_datum_field(node))
	    ^label_strings
	    ^antecedents_strings
	    ^consequents_strings]);
    enddefine;
print_atms_node -> class_print(atms_node_key);


;;; An ATMS assumption node has a datum (holding the statement
;;; represented by the node), a label (holding a list of environments),
;;; a contradictory flag, an antecedents field, holding a list of
;;; justifications for which the node is the consequent, a consequents
;;; field holding a list of justifications for which the node is an
;;; antecedent, and a field holding the integer representation (a
;;; bitvector in which the ith field is set, where this is the ith
;;; assumption).
recordclass atms_assumption_node
     atms_assumption_node_datum_field
     atms_assumption_node_label_field
     atms_assumption_node_contradictory_field
     atms_assumption_node_antecedents_field
     atms_assumption_node_consequents_field
     atms_assumption_node_assumption;

;;; Make a default atms_assuption_node
define make_atms_assumption_node() -> the_atms_assumption_node;
    consatms_assumption_node(false, [], false, [], [], 0)
        -> the_atms_assumption_node;
    enddefine;

;;; Print an atms_assumption_node.
;;; Note that this prints the assumption set field as the `label' of
;;; the assumption, and doesn't print antecedents (since there
;;; shouldn't be any). If we ever add justified assumptions this will
;;; need to change.
define print_atms_assumption_node(node);
    lvars consequents = atms_assumption_node_consequents_field(node),
	  consequents_data = atms_justifs_consequent_data(consequents),
	  consequents_strings = maplist(consequents_data,
				     	sprintf(% '< atms_justif %p >' %));

    printf('< atms_assumption_node: %p %p\n     justifies %p >',
	   [^(atms_assumption_node_datum_field(node))
	    ^(integer_length(atms_assumption_node_assumption(node)) - 1)
	    ^consequents_strings]);
    enddefine;
print_atms_assumption_node -> class_print(atms_assumption_node_key);


;;; Accessor functions for the common fields.  
;;; Note that the datum of an atms_node or atms_assumption_node can't be
;;; updated after the node has been created.

define atms_node_datum(node) -> datum;
    if isatms_node(node) then
	atms_node_datum_field(node) -> datum;
    elseif isatms_assumption_node(node) then
	atms_assumption_node_datum_field(node) -> datum;
    else
	mishap('atms_node or atms_assumption_node expected', [^node]);
	endif;
    enddefine;

define updaterof atms_node_datum(datum, node);
    mishap('attempt to update datum field for', [^node]);
    enddefine;

define atms_node_label(node) -> label;
    if isatms_node(node) then
	atms_node_label_field(node) -> label;
    elseif isatms_assumption_node(node) then
	atms_assumption_node_label_field(node) -> label;
    else
	mishap('atms_node or atms_assumption_node expected', [^node]);
	endif;
    enddefine;

define updaterof atms_node_label(label, node);
    if isatms_node(node) then
	label -> atms_node_label_field(node);
    elseif isatms_assumption_node(node) then
	label -> atms_assumption_node_label_field(node);
    else
	mishap('atms_node expected', [^node]);
	endif;
    enddefine;

;;; Should atms_assumption_nodes be contradictory?
define atms_node_contradictory(node) -> contradictory;
    if isatms_node(node) then
	atms_node_contradictory_field(node) -> contradictory;
    elseif isatms_assumption_node(node) then
	atms_assumption_node_contradictory_field(node) -> contradictory;
    else
	mishap('atms_node or atms_assumption_node expected', [^node]);
	endif;
    enddefine;

define updaterof atms_node_contradictory(contradictory, node);
    if isatms_node(node) then
	contradictory -> atms_node_contradictory_field(node);
    elseif isatms_assumption_node(node) then
	contradictory -> atms_assumption_node_contradictory_field(node);
	endif;
    enddefine;

define atms_node_antecedents(node) -> antecedents;
    if isatms_node(node) then
	atms_node_antecedents_field(node) -> antecedents;
    elseif isatms_assumption_node(node) then
	atms_assumption_node_antecedents_field(node) -> antecedents;
    else
	mishap('atms_node or atms_assumption_node expected', [^node]);
	endif;
    enddefine;

define updaterof atms_node_antecedents(antecedents, node);
    if isatms_node(node) then
	antecedents -> atms_node_antecedents_field(node);
    elseif isatms_assumption_node(node) then
	antecedents -> atms_assumption_node_antecedents_field(node);
    else
	mishap('atms_node or atms_assumptions_node expected', [^node]);
	endif;
    enddefine;

define atms_node_consequents(node) -> consequents;
    if isatms_node(node) then
	atms_node_consequents_field(node) -> consequents;
    elseif isatms_assumption_node(node) then
	atms_assumption_node_consequents_field(node) -> consequents;
    else
	mishap('atms_node or atms_assumption_node expected', [^node]);
	endif;
    enddefine;


define updaterof atms_node_consequents(consequents, node);
    if isatms_node(node) then
	consequents -> atms_node_consequents_field(node);
    elseif isatms_assumption_node(node) then
	consequents -> atms_assumption_node_consequents_field(node);
    else
	mishap('atms_node or atms_assumption_node expected', [^node]);
	endif;
    enddefine;


;;; Justifications have a consequent (data) node, an informant, an id, a
;;; list of antecedent nodes (which may be data nodes or assumption
;;; nodes) and an applied_flag which indicates whether this justification
;;; has been traversed during lazy update.
recordclass atms_justification
     atms_justification_consequent
     atms_justification_informant
     atms_justification_id
     atms_justification_antecedents
     atms_justification_applied_flag;

;;; Accessor functions for the common fields.  
;;; Note that atms_justifications are immutable.

define updaterof atms_justification_consequent(consequent, justification);
    mishap('attempt to update consequent field for', [^justification]);
    enddefine;

define updaterof atms_justification_informant(informant, justification);
    mishap('attempt to update informant field for justification',
	   [^justification]);
    enddefine;

define updaterof atms_justification_id(id, justification);
    mishap('attempt to update id field for justification',
	   [^justification]);
    enddefine;

define updaterof atms_justification_antecedents(antecedents, justification);
    mishap('attempt to update antecedents field for justification',
	   [^justification]);
    enddefine;


;;; Make a default atms_justification
define make_atms_justification() -> the_atms_justification;
    consatms_justification(false, false, 0, [], false) -> the_atms_justification;
    enddefine;

;;; Print an atms_justification
define print_atms_justification(justif);
    printf('\n<atms_justification: %p %p\n %p\n %p\n %p>',
	   [^(atms_justification_id(justif))
	    ^(atms_justification_applied_flag(justif))
	    ^(atms_node_datum(atms_justification_consequent(justif)))
	    ^(atms_justification_informant(justif))
	    ^(atms_node_data(atms_justification_antecedents(justif)))]);
    enddefine;
print_atms_justification -> class_print(atms_justification_key);


;;; Information for the class print procedures.

;;; Return a list, the first element of which is a lists of the
;;; assumption numbers in <environment> and the second element is a
;;; representation of its choice sequence
define printable_atms_environment(environment) -> numbers;
    lvars aset = atms_aset(environment),
    	  cseq = atms_cseq(environment);

    [ ^(printable_atms_aset(aset))
      ^(printable_atms_cseq(cseq)) ] -> numbers;
    enddefine;

;;; Return a list of the assumptions numbers in the assumption set <aset>
;;; This is a little harder than for environments, since we don't know the
;;; length of the assumption set.
define printable_atms_aset(aset) -> numbers;
    lvars size = integer_length(aset),
	  i;

    [% for i from 0 to size - 1 do
	   if testbit(aset, i) then
	       i,
	       endif;
	   endfor %] -> numbers;
    enddefine;

;;; Return a list of the disjunction numbers and choices in
;;; <disjunctions> and <choices>.
define printable_atms_cseq(cseq) -> numbers;
    lvars disjunctions = atms_cseq_disjunctions(cseq),
    	  choices = atms_cseq_choices(cseq),
	  disjunction_numbers = printable_atms_aset(disjunctions),
	  disjunction_number,
	  position = atms_cseq_choices_length(cseq),
	  num_disjuncts,
	  choices_numbers,
    	  numbers = [];

    ;;; We print a sequence of choice sets as a list of pairs.  The
    ;;; first element of each pair is the index of a disjunction and the
    ;;; second element is a list of disjunct indices which are true for
    ;;; the disjunction index
    for disjunction_number in disjunction_numbers do
	listlength(atms_disjunctions(disjunction_number)) -> num_disjuncts;
	position - num_disjuncts -> position;
	printable_atms_aset(integer_field(num_disjuncts, position)(choices)) 
	    -> choices_numbers;
	;;; Increment the choices numbers so that the first disjunct has
	;;; index 1 rather than index 0 (for consistency with the old
	;;; version of the code)
	maplist(choices_numbers, nonop +(% 1 %)) -> choices_numbers;    
	numbers <> [ ^disjunction_number ^choices_numbers ] -> numbers;
	endfor;
    enddefine;

;;; Return a list of lists of the assumptions numbers in <label> where
;;; <label> is a list of environments.
define printable_atms_label(label) -> numbers;
    maplist(label, printable_atms_environment) -> numbers;
    enddefine;

;;; Return a list of datum fields which are the consequents ofg the justifs in
;;; <justif_list> (this is used for printing nodes, among other things).
define atms_justifs_consequent_data(justif_list) -> data_list;
    atms_node_data(maplist(justif_list, atms_justification_consequent)) -> data_list;
    enddefine;

;;; Return a list of lists of datum fields which are the antecedents of the
;;; justifications in <justif_list>  (this is used for printing nodes, among
;;; other things).
define atms_justifs_antecedents_data(justif_list) -> data_list;
    maplist(maplist(justif_list, atms_justification_antecedents), atms_node_data) -> data_list;
    enddefine;

;;; Return the datum fields of a list of nodes (e.g. an antecedents list)
define atms_node_data(node_list) -> data_list;
    maplist(node_list, atms_node_datum) -> data_list;
    enddefine;



;;; A recordclass to represent an extensible vector (adjustable_array in
;;; common lisp or strechy_vector in dylan).  Note can we make this
;;; lvars or otherwise hide the names of the fields?

;;; The default amount to extend a extensible_vector when it is full.
;;; Perhaps we should just double the current size of the vector?
lconstant extensible_vector_extension = 100;

defclass extensible_vector
  { extensible_vector_vector,
    extensible_vector_fill_pointer,
    extensible_vector_initial_element };

define make_extensible_vector(size, fill_position, init) -> extensible_vector;
    consextensible_vector(initvectorclass(size, init, vector_key),
			  fill_position + 1, init) -> extensible_vector;
    enddefine;

;;; Print an extensible_vector.
;;; Not clear whether we should print all the contents of the vector or
;;; only up to the fill pointer ... if we really want to concatenate all
;;; the elements of the vector before printing them, we could use >< or
;;; sprintf.  We don't print the default value.
define print_extensible_vector(extensible_vector);
    lvars the_vector = extensible_vector_vector(extensible_vector),
	  fill_pointer = extensible_vector_fill_pointer(extensible_vector),
	  vector_length = length(the_vector),
	  i;

    printf('{ %p:%p ', [^vector_length ^(fill_pointer - 1)]);
    ;;; Print all the elements up to the fill pointer.
    fast_for i from 1 to fill_pointer - 1 do
	pr(the_vector(i));
	endfor;
    pr(' }\n');
    enddefine;
print_extensible_vector -> class_print(extensible_vector_key);

;;; Like aref, these ignore the fill pointer and don't extend the array.
define extensible_vector_apply(index, extensible_vector) -> element;
    extensible_vector_vector(extensible_vector)(index + 1) -> element;
    enddefine;

define updaterof extensible_vector_apply(element, index, extensible_vector);
    element -> extensible_vector_vector(extensible_vector)(index + 1);
    enddefine;
extensible_vector_apply -> class_apply(extensible_vector_key);

;;; Return the fill pointer of the vector.
;;; The fill pointer of an extensible_vector is a number between 0 and the
;;; the size of the vector (inclusive).
define fill_pointer(extensible_vector) -> index;
    extensible_vector_fill_pointer(extensible_vector) - 1 -> index;
    enddefine;

define updaterof fill_pointer(fill_pointer, extensible_vector);
    fill_pointer + 1 -> extensible_vector_fill_pointer(extensible_vector);
    enddefine;

;;; Return the active length of an extensible_vector
;;; This is redundant for extensible_vectors since they index from 0 (the
;;; length is the same as the fill pointer).
define extensible_vector_length(extensible_vector) -> vector_length;
    extensible_vector_fill_pointer(extensible_vector) - 1 -> vector_length;
    enddefine;

;;; vector_push attempts to store <new_element> in the element of the vector
;;; designated by the fill pointer and increase the fill pointer by one.
;;; <extensible_vector> must be an extensible vector and <new-element> may
;;; be any object.  If the fill pointer does not designate an element of the
;;; vector, the vector is unchanged and vector_push returns false.
;;; Otherwise the store and increment take place and vector_push returns the
;;; former value of the fill pointer, i.e the index of the new element
;;; pushed.
define extensible_vector_push(new_element, extensible_vector) -> index;
    lvars the_vector = extensible_vector_vector(extensible_vector),
	  fill_pointer = extensible_vector_fill_pointer(extensible_vector),
	  vector_length = length(the_vector);

    if fill_pointer <= vector_length then
	new_element -> the_vector(fill_pointer);
	fill_pointer + 1 -> extensible_vector_fill_pointer(extensible_vector);
	fill_pointer - 1 -> index;
    else
	false -> index;
	endif;
    enddefine;

;;; extensible_vector_push_extend is just like vector_push except that
;;; of the fill pointer gets too large, the vector is extended.  The
;;; optional argument <extension>, which must be a positive integer, is
;;; the nunber of elements to be added to the vector if it is extended
;;; -- it defaults to a `reasonable' value;
define extensible_vector_push_extend(new_element, extensible_vector) -> index;
    lvars extension = extensible_vector_extension;
    ;;; Check for optional extension argument.
    unless isextensible_vector(extensible_vector) then
	;;; shuffle the arguments
	extensible_vector -> extension;
	new_element -> extensible_vector;
	-> new_element;
	endunless;

    lvars the_vector = extensible_vector_vector(extensible_vector),
	  fill_pointer = extensible_vector_fill_pointer(extensible_vector),
	  initial_element = extensible_vector_initial_element(extensible_vector),
	  vector_length = length(the_vector);

    if fill_pointer <= vector_length then
	new_element -> the_vector(fill_pointer);
	fill_pointer + 1 -> extensible_vector_fill_pointer(extensible_vector);
	fill_pointer - 1 -> index;
    else
	;;; otherwise extend the vector.
	consvector(explode(the_vector),
	     	   repeat extension times initial_element, endrepeat,
	     	   vector_length + extension) -> the_vector;
	new_element -> the_vector(fill_pointer);
	the_vector -> extensible_vector_vector(extensible_vector);
	fill_pointer + 1 -> extensible_vector_fill_pointer(extensible_vector);
	fill_pointer - 1 -> index;
	endif;
    enddefine;

;;; Decrease the fill pointer by one, and the element designated by the new
;;; value of the fill pointer is returned.  If the fill pointer is 0
;;; (externally, i.e. 1 internally) , an error is signallled.
define extensible_vector_pop(extensible_vector) -> element;
    lvars fill_pointer = extensible_vector_fill_pointer(extensible_vector);
    unless fill_pointer == 1 then
	fill_pointer - 1 -> fill_pointer;
	extensible_vector_vector(extensible_vector)(fill_pointer) -> element;
	fill_pointer -> extensible_vector_fill_pointer(extensible_vector);
    else
	mishap('attempt to reference beyond beginning of an extensible vector',
	       [^fill_pointer ^extensible_vector]);
	endunless;
    enddefine;

;;; Like vector_push_extend, but allows you to specify the index, if this is
;;; greater than the actual length of the vector (including the inactive
;;; portion) the vector is extended.  Note that this never changes the value
;;; of the fill pointer, i.e. just looking at an element past the fill pointer
;;; does not move the fill pointer.

;;; This shouldn't really be called subscrextensible_vector since the
;;; behaviour is different from the () syntax and there is no corresponding
;;; fast version, but I couldn't think of anything better ...
define subscrextensible_vector(index, extensible_vector) -> element;
    lvars extension = extensible_vector_extension;
    ;;; Check for optional extension argument.
    unless isextensible_vector(extensible_vector) then
	;;; shuffle the arguments
	extensible_vector -> extension;
	index -> extensible_vector;
	-> index;
	endunless;

    lvars the_vector = extensible_vector_vector(extensible_vector),
	  fill_pointer = extensible_vector_fill_pointer(extensible_vector),
	  initial_element = extensible_vector_initial_element(extensible_vector),
	  vector_length = length(the_vector),
	  offset = index + 1;

    if offset <= vector_length then
	the_vector(offset) -> element;
    else
	;;; otherwise extend the vector.
	consvector(explode(the_vector),
	     	   repeat extension times initial_element, endrepeat,
	     	   vector_length + extension) -> the_vector;
	the_vector(offset) -> element;
	the_vector -> extensible_vector_vector(extensible_vector);
	endif;
    enddefine;

;;; Note that this only updates the fill pointer if <index> is greater than
;;; the current value of the fill pointer.
define updaterof subscrextensible_vector(new_element, index, extensible_vector);
    lvars extension = extensible_vector_extension;
    ;;; Check for optional extension argument.
    unless isextensible_vector(extensible_vector) then
	;;; shuffle the arguments
	extensible_vector -> extension;
	index -> extensible_vector;
	new_element -> index;
	-> new_element;
	endunless;

    lvars the_vector = extensible_vector_vector(extensible_vector),
	  fill_pointer = extensible_vector_fill_pointer(extensible_vector),
	  initial_element = extensible_vector_initial_element(extensible_vector),
	  vector_length = length(the_vector),
	  offset = index + 1;

    if offset <= vector_length then
	new_element -> the_vector(offset);
	if offset >= fill_pointer then
	    offset + 1 -> extensible_vector_fill_pointer(extensible_vector);
	    endif;		
    else
	;;; otherwise extend the vector.
	consvector(explode(the_vector),
	     	   repeat extension times initial_element, endrepeat,
	     	   vector_length + extension) -> the_vector;
	new_element -> the_vector(offset);
	the_vector -> extensible_vector_vector(extensible_vector);
	offset + 1 -> extensible_vector_fill_pointer(extensible_vector);
	endif;
    enddefine;

;;; Return the index of element.
define extensible_vector_index(element, extensible_vector) -> index;
    lvars index = -1,
	  the_vector = extensible_vector_vector(extensible_vector),
	  fill_pointer = extensible_vector_fill_pointer(extensible_vector),
	  i;
	  
    fast_for i from 1 to fill_pointer - 1 do
	if the_vector(i) = element then
	    i-1 -> index;
	    quitloop;
	    endif;
	endfor;
    enddefine;

;;; Move fill pointer down if necessary; return length.  Note, this relies
;;; on the non-active parts of the extensible vector being reinitialised with
;;; the vector's initial_element.
define trim_extensible_vector(extensible_vector) -> index;
    lvars index = length(extensible_vector),
	  initial_element = extensible_vector_initial_element(extensible_vector);

    until index == 0 or extensible_vector(index - 1) /= initial_element do
	index - 1 -> index;
	enduntil;
    index -> fill_pointer(extensible_vector);
    
    enddefine;
    
;;; atms_types ends here
vars atms_types = true;
