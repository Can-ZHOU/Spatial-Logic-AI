/* --- Copyright University of Nottingham 2011. All rights reserved. ------
 > File:            popatms.p
 > Purpose:	    A simple ATMS with disjunctions in Pop-11.
 > Author:          Hai Nguyen & Brian Logan, June 20 2011
 > Documentation:   User-level interface to the ATMS.
 > Related Files:
 */

;;; This file defines the `recommended' API for the ATMS in terms of data items.

;;; This file contains the definitions of the following procedures:
vars procedure popatms_init,
     procedure atms_assume_all,
     procedure atms_premise_all,
     procedure atms_assume,
     procedure atms_premise,
     procedure atms_det_justify,
     procedure atms_nondet_justify,
     procedure atms_inconsistent,
     procedure atms_assumption_p,
     procedure atms_datum_p,
     procedure atms_justification_p,
     procedure atms_inconsistent_p,
     procedure atms_consistent_p,
     procedure atms_nested_choices_p,
     procedure atms_node_for,
     procedure atms_node_list,
     procedure increment_gc_count,
     procedure print_atms_stats;

;;; If the reasoner is guaranteed not to claim the same assumption or
;;; justification twice, we can skip some time consuming checks when
;;; creating new ATMS data structures. Note that this does not apply to
;;; datum nodes, as typically the reasoner will create multiple
;;; justifications for the same datum. This is true by default.
vars atms_check_dups = true;

;;; Equality procedure used to test whether assumptions and datum values
;;; are equal. If the reasoner is guaranteed to always return references
;;; to the same datum, this can be replaced with ==. Note that if
;;; atms_check_dups is false, we dont' check for duplicated
;;; justifications, so the equality test used for the justification
;;; table can be =. This is = by default.
vars atms_datum_eq = nonop =;

;;; If atms_lazy_labels is true, calls to atms_[non]det_justify and
;;; atms_inconsistent only build the justification structure and do not
;;; update labels. It is the reasoner's responsibility to call
;;; atms_lazy_node_label to compute the label of a node. This is false
;;; by default.
vars atms_lazy_labels = false;

;;; Load the core ATMS code.
uses atms_nodes


;;; Hash tables to record ATMS datastructures.
;;; While the reasoner may delete assumptions or data (e.g., if it can
;;; regenerate them when required), we assume that we never delete ATMS
;;; assumptions, data, or justifications, so the hash tables are either
;;; "perm" or "tmpval". Making datum and justif tables tmpval has the
;;; advantage that if we ever do want to delete assumtions, everything
;;; else that depends on them will automatically be gc'd. However this
;;; requires care to ensure that we always have a live reference to a
;;; datum node or justif during creation, so play safe for now.
					    
;;; Hash table to record ATMS datum nodes. 
;;; This maps from user data items (assumptions or derived datums) to
;;; ATMS nodes.  It is assumed that the user data will be something
;;; simple like lists which can be hashed with syshash, or a user
;;; datastructure for which a class_hash procedure has beed defined.  In
;;; normal use, this can't be the only reference to a datum node (the
;;; datum node must be the consequent of some justification), but the
;;; node may be gc'd before it's made the consequent of the justif, so
;;; we use a permanent property.
vars atms_datum_table = newanyproperty([], 101, 1, false, syshash, 
				       atms_datum_eq, "perm", false, false);

;;; Hash table to record ATMS justification nodes.
;;; For now we use the simple approach of hashing on a list containing
;;; the consequent and the antecedents of the justification.  If
;;; pop_hash_lim has the default value 3, this will hash on the
;;; consequent and two of the antecendents, which hopefully will give
;;; reasonable results.  However some more complex scheme may be
;;; necessary.  Also it is not clear how to hash justifications if we
;;; want to be able distinguish justifs with the same consequent and
;;; antecedents but different informant fields.  As above, in normal
;;; use, this can't be the only reference to a justif (all justifs are
;;; ultimately referenced from assumptions), but the justif may be gc'd
;;; before it's made the consequent of an assumption or node, so we use
;;; a permanent property.
vars atms_justification_table = newanyproperty([], 101, 1, false, syshash, 
					       nonop =, "perm", false, false);

;;; The number of times the garbage collector has been invoked since the
;;; ATMS was last initialised.
vars atms_gc_count = 0;


;;; (Re)initialise the ATMS global variables.  This is only really useful
;;; for debugging.
define popatms_init();
    ;;; Reinitialise the node-level datastructures.
    atms_init();
    ;;; Reinitalise the hash tables.
    newanyproperty([], 101, 1, false, syshash, 
		   atms_datum_eq, "perm", false, false) -> atms_datum_table;

    newanyproperty([], 101, 1, false, syshash, 
		   nonop =, "perm", false, false) -> atms_justification_table;

    ;;; Reset the debugging statistics
    0 -> atms_gc_count;
    enddefine;


;;; Note that the basic interface procedures below don't return the ATMS
;;; structures they create, as the user is assumed not to care about these.
;;; If you need access to the ATMS at that level, you should probably be
;;; using the procedures in the file atms_nodes.p.

;;; Make each datum in <data> an ATMS assumption.
define atms_assume_all(data);
    applist(data, atms_assume);
 enddefine;

;;; Make each datum in <data> an ATMS premise.
define atms_premise_all(data);
    applist(data, atms_premise);
 enddefine;

;;; Make <datum> as an ATMS assumption.  If <datum> is already an
;;; assumption, this does nothing.  Note that at present, it is illegal to
;;; claim a derived datum.
define atms_assume(datum);
    unless atms_assumption_p(datum) then
	;;; Make a new assumption
	create_atms_assumption(datum) -> atms_datum_table(datum);
	endunless;
    enddefine;

define atms_premise(datum);
    unless atms_assumption_p(datum) then
	;;; Make a new pemise
	create_atms_premise(datum) -> atms_datum_table(datum);
	endunless;
    enddefine;

;;; Record the fact that the truth of <datum> depends on <data>. If
;;; there already exists a justification of <datum> from <data>, this
;;; does nothing.  For convenience, we allow the user to create a
;;; justification where there are no corresponding nodes for the
;;; antcedents -- nodes for antecedents are created as required.  Calls
;;; update_atms_det_justification if label update is non-lazy to update
;;; belief status appropriately.
define atms_det_justify(datum, data);

    unless atms_justification_p(datum, data) then
	;;; Make a new justification
	lvars c_node = hd(atms_node_list([^datum])),
	      a_nodes = atms_node_list(data),
	      justif = create_atms_justification(c_node, 'DET', a_nodes);

 	justif -> atms_justification_table([^datum ^^data]);
	unless atms_lazy_labels then
    	    update_atms_det_justification(justif, atms_justification_id(justif));
	    endunless;
	endunless;
    enddefine;

;;; Record the fact that the truth of each disjunct in <disjuncts> depends
;;; on <disjunction>.
define atms_nondet_justify(disjuncts, disjunction);
    lvars a_nodes = atms_node_list([^disjunction]),
    	  disjunction_id, disjunct, justifs;

    ;;; Allocate a disjunction_id for this disjunction elimination. Note
    ;;; that eliminating the same disjunction twice creates a new
    ;;; tableaux context.
    extensible_vector_push_extend(disjuncts, atms_disjunctions) -> disjunction_id;

    ;;; A set of disjuncts can't be an assumption, so we skip the
    ;;; atms_assumption_p check
    [% for disjunct in disjuncts do
	   unless atms_justification_p(disjunct, [^disjunction]) then
	       ;;; Make a new justification
	       lvars c_node = hd(atms_node_list([^disjunct]));
	       create_atms_justification(c_node, disjunction_id, a_nodes) ->>
	           atms_justification_table([^disjunct ^disjunction]);
	       endunless;
	   endfor %] -> justifs;

    unless atms_lazy_labels then
    	update_atms_nondet_justifications(justifs, disjunction_id);
	endunless;
    enddefine;    
    
;;; Record the fact that <data> are inconsistent.  This triggers ATMS
;;; processing so that any derived datum that depends only on <data> will be
;;; marked as having no support (i.e. no valid justifications).  Calling
;;; atms_consistent_p on such a datum will return false.  For convenience, we
;;; allow the user to claim inconsistency between data for which there are no
;;; corresponding nodes, creating the nodes as required.
define atms_inconsistent(data);
    unless atms_justification_p(atms_falsity, data) then
    	lvars antecedents = atms_node_list(data),
    	      justif = create_atms_justification(atms_falsity, 'CONTRADICTION', 
					     	 antecedents);

    	      justif -> atms_justification_table([^atms_falsity ^^data]);
    	unless atms_lazy_labels then
    	    update_atms_det_justification(justif, atms_justification_id(justif));
	    endunless;
	endunless;
    enddefine;

;;; We could follow the Pop-11 convention and call the following procedures
;;; isatms_<foo>, but they are not recognisers, rather they return true if
;;; there is an ATMS structure corresponding to their arguments.

;;; Returns true if there an ATMS assumption corresponding to <datum>.
define atms_assumption_p(datum) -> bool;
    lvars node, bool = false;
    if atms_check_dups and atms_datum_table(datum) ->> node then
     	isatms_assumption_node(node) -> bool;
     	endif;
    enddefine;

;;; Returns true if there an ATMS datum corresponding to <datum>.  
define atms_datum_p(datum) -> bool;
    not(not(atms_datum_table(datum))) -> bool;
    enddefine;

;;; Returns true if there an ATMS justification of <datum> form <data>.
define atms_justification_p(datum, data) -> bool;
    (atms_check_dups and not(not(atms_justification_table([^datum ^^data])))) -> bool;
    enddefine;

;;; Returns true if <data> are not currently known to be mutually
;;; inconsistent.
define atms_consistent_p(data) -> bool;
    lvars nodes = atms_node_list(data, false),
	  esets = maplist(nodes, atms_node_label),
	  union = [% applist(cartesian_product(esets), 
			     atms_union_environment_list) %];
     
    ;;; Check to see if at least one of the environments is consistent.
    some(union, atms_nogood_p <> not) -> bool;
    enddefine;


;;; Returns true if <data> can be combined to infer new datum.  This
;;; checks if the choice sequences are nested or equal.
define atms_nested_choices_p(data) -> bool;
    lvars nodes = atms_node_list(data, false),
	  esets = maplist(nodes, atms_node_label);

    maplist(cartesian_product(esets), atms_union_environment_list) /== [] -> bool;
    enddefine;


;;; Return the ATMS assumption or datum node corresponding to <datum>,
;;; creating a new node if <datum> is not currently maintained by the
;;; ATMS.  If optional second argument is false, MISHAPS if <datum> has
;;; no corresponding ATMS node.
define atms_node_for(datum /* &optional create */) -> node;
    lvars create = true;
    unless islist(datum) then
	;;; shuffle the arguments
	datum -> create;
	-> datum;
	endunless;

    unless atms_datum_table(datum) ->> node then
	if create then
	    ;;; Make a new datum node for this antecedent.
	    create_atms_node(datum) -> node;
	    node -> atms_datum_table(datum);
	else
	    mishap('no ATMS node for datum', [^datum]);
	    endif;
	endunless;
    enddefine;

;;; Return a list of ATMS assumption/data nodes corresponding to the data
;;; items in <data>, creating new nodes for any data not currently
;;; maintained by the ATMS.  If optional second argument is false, MISHAPS
;;; if any of the data items has no corresponding ATMS node.
define atms_node_list(data /* &optional create */) -> node_list;
    lvars create = true;
    unless islist(data) then
	;;; shuffle the arguments
	data -> create;
	-> data;
	endunless;

    maplist(data, atms_node_for(% create %)) -> node_list;

;;;    maplist(data, 
;;;	    procedure(datum) -> node;
;;;		;;; If datum is maintained by the ATMS it must be either
;;;		;;; an assumption or a datum node, but not both ...
;;;		lvars node = atms_assumption_table(datum) or 
;;;		      atms_datum_table(datum);
;;;		unless node then
;;;		    if create then
;;;		    	;;; Make a new datum node for this antecedent.
;;;		    	create_atms_node(datum) -> node;
;;;		    	node -> atms_datum_table(datum);
;;;		    else
;;;			mishap('no ATMS node for datum', [^datum]);
;;;			endif;
;;;		    endunless;
;;;		endprocedure) -> node_list;
    enddefine;

;;; Increment the gc count.
define increment_atms_gc_count();
    atms_gc_count + 1 -> atms_gc_count;
    enddefine;
increment_atms_gc_count -> pop_after_gc;

;;; Print ATMS statistics since the last time the ATMS was initialised.
define print_atms_stats();
    ;;; Print information about ATMS assumptions, nodes and justifications
    printf('\nATMS statistics\n');
    printf('%p assumptions, %p nodes, %p justifications\n', 
;;;	       [ ^(length(datalist(atms_assumption_table)))
	       [ ^(extensible_vector_length(atms_assumptions))
		 ^(length(datalist(atms_datum_table)))
		 ^(extensible_vector_length(atms_justifications)) ]);

    printf('%p assumption ids, %p disjunction ids\n', 
	       [ ^(extensible_vector_length(atms_assumptions))
		 ^(extensible_vector_length(atms_disjunctions)) ]);

    ;;; Justifications for falsity
    printf('\n%p justifs for ATMS falsity\n', 
	   [ ^(listlength(atms_node_justifs([ ^atms_falsity ]))) ]);

    ;;; Print information about nogoods
    printf('\nATMS nogoods\n\n');
    lvars i, nogood_count;
    for i from 1 to extensible_vector_length(atms_nogoods) - 1 do
	printf('size: %p nogoods: %p\n', 
	       [ ^i ^(length(atms_nogoods(i))) ]);
	nogood_count + length(atms_nogoods(i)) -> nogood_count;
	endfor;
    printf('\nTotal number of nogoods: %p\n', [^nogood_count]);

    ;;; Print information about environments
    printf('\nATMS environments\n\n');
    lvars environment_count;
    for i from 1 to extensible_vector_length(atms_environments) - 1 do
	printf('size: %p environments: %p\n', 
	       [ ^i ^(length(atms_environments(i))) ]);
	environment_count + length(atms_environments(i)) -> environment_count;
	endfor;
    printf('\nTotal number of environments: %p\n', [ ^environment_count ]);

    ;;; Print information about choice sequences
    printf('\nATMS choice sequences\n\n');
    lvars cseq_count, longest_cseq_count;
    for i from 1 to max(extensible_vector_length(atms_choice_sequences) - 1,
 			extensible_vector_length(atms_longest_choice_sequences) - 1) do
	printf('size: %p choice sequences: %p longest sequences: %p\n', 
	       [ ^i ^(length(atms_choice_sequences(i))) 
		    ^(length(atms_longest_choice_sequences(i))) ]);
	cseq_count + length(atms_choice_sequences(i)) -> cseq_count;
	longest_cseq_count + length(atms_longest_choice_sequences(i)) 
	    -> longest_cseq_count;
	endfor;
    printf('\nTotal numbers of choice sequences: %p longest sequences: %p\n', 
	   [ ^cseq_count ^longest_cseq_count ]);

    printf('\n%p garbage collections\n', [ ^atms_gc_count ]);
    enddefine;

;;; popatms ends here
vars popatms = true;
