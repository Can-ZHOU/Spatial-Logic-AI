/* --- Copyright University of Nottingham 2011. All rights reserved. ------
 > File:            prb_atms.p
 > Purpose:	    A simple D-ATMS in Pop-11.
 > Author:          Hai Nguyen & Brian Logan, June 20 2011
 > Documentation:   POPRULEBASE interface to the D-ATMS.
 > Related Files:
*/

;;; This file defines a very simple POPRULEBASE interface to the ATMS
;;; which extends the set of POPRULEBASE actions to include new action
;;; keywords.

;;; This file contains the definitions of the following procedures:
vars procedure doTESTADDALL,
     procedure doATMS_ASSUME,
     procedure doATMS_JUSTIFY,
     procedure doATMS_DISJOINT_JUSTIFY,
     procedure doATMS_AND_JUSTIFY,
     procedure doATMS_OR_JUSTIFY,
     procedure doATMS_INCONSISTENT;

uses fmatches
uses popatms

;;; Action to add a list of items to the prb database if they are not
;;; already present, i.e., extends TESTADD functionality to ADDALL.
define doTESTADDALL(rule_instance, action);
    applist(fast_back(action), 
	    procedure(datum);
		unless prb_instance_present(datum) then 
		    prb_add(datum);
 		    endunless; 
		endprocedure);
    enddefine;
"doTESTADDALL" -> prb_action_type("TESTADDALL");

;;; Action to create a new ATMS assumption.  For convenience, we allow a
;;; variable number of arguments (i.e. the user can assume more that one
;;; datum at at a time).  Perhaps we should create a merged ADD + ASSUME
;;; action, ATMS_ADD which adds the items to the database if they are not
;;; there and truth maintains them.
define doATMS_ASSUME(rule_instance, action);
    lvars data, datum;

    if action fmatches [ ATMS_ASSUME ??data ] then
	unless null(data) then
	    for datum in data do
	    	atms_assume(datum);
	    	endfor;
    	else
	    mishap('No data for ATMS assumption', data);
	    endunless;
	endif;
    enddefine;
"doATMS_ASSUME" -> prb_action_type("ATMS_ASSUME");

;;; Action to create a new ATMS justification.
define doATMS_JUSTIFY(rule_instance, action);
    lvars data, consequent, antecedents;

    if action fmatches [ ATMS_JUSTIFY ??data ] then
    	if action fmatches [ ATMS_JUSTIFY ?consequent ?antecedents ] then
	    atms_det_justify(consequent, antecedents);
    	else
	    mishap('No data for ATMS justification', data);
	    endif;
	endif;
    enddefine;
"doATMS_JUSTIFY" -> prb_action_type("ATMS_JUSTIFY");

;;; Action to create a new ATMS disjoint justification.
define doATMS_DISJOINT_JUSTIFY(rule_instance, action);
    lvars  data, negated_instances, antecedents, negated_instance;
    
    if action fmatches [ ATMS_DISJOINT_JUSTIFY ??data ]  then
    	if action fmatches [ ATMS_DISJOINT_JUSTIFY ?negated_instances ?antecedents ]  then
    	    for negated_instance in negated_instances do
    	    	atms_det_justify(negated_instance, antecedents);
		endfor;
    	else
	    mishap('No data for ATMS DISJOINT justification', data);
	    endif;
    	endif;
    enddefine;
"doATMS_DISJOINT_JUSTIFY" -> prb_action_type("ATMS_DISJOINT_JUSTIFY");

;;; Action to create a new ATMS AND justification.
define doATMS_AND_JUSTIFY(rule_instance, action);
    lvars data, conjuncts, conjunction, antecedents, conjunct;
    
    if action fmatches [ ATMS_AND_JUSTIFY ??data ] then
    	if action fmatches [ ATMS_AND_JUSTIFY ?conjuncts ?conjunction ] then
	    [^conjunction] -> antecedents;
    	    for conjunct in conjuncts do
    	    	atms_det_justify(conjunct, antecedents);
		endfor;
    	else
	    mishap('No data for ATMS AND justification', data);
	    endif;
	endif;
    enddefine;
"doATMS_AND_JUSTIFY" -> prb_action_type("ATMS_AND_JUSTIFY");

;;; Action to create a new ATMS OR justification.
define doATMS_OR_JUSTIFY(rule_instance, action);
    lvars data, disjuncts, disjunction;
    
    if action fmatches [ ATMS_OR_JUSTIFY ??data ] then
    	if action fmatches [ ATMS_OR_JUSTIFY ?disjuncts ?disjunction ] then
	    atms_nondet_justify(disjuncts, disjunction);
    	else
	    mishap('No data for ATMS OR justification', data);
	    endif;
    	endif;
    enddefine;
"doATMS_OR_JUSTIFY" -> prb_action_type("ATMS_OR_JUSTIFY");

;;; Action to create a new ATMS nogood.
;;; Unlike the other actions, this creates a single nogood, otherwise we
;;; would have to put each set of inconsistent data items in a list, which
;;; for a single set of data items is pain.  Note that we allow the creation
;;; of nogoods with a single member.
define doATMS_INCONSISTENT(rule_instance, action);
    lvars data;
    if action fmatches [ ATMS_INCONSISTENT ??data ] then
	if length(data) >= 1 then 
	    atms_inconsistent(data);
    	else
	    mishap('No data for ATMS nogood', data);
	    endif;
	endif;
    enddefine;
"doATMS_INCONSISTENT" -> prb_action_type("ATMS_INCONSISTENT");


;;; Intialise the ATMS
atms_init();

;;; prb_atms ends here
vars prb_atms = true;
