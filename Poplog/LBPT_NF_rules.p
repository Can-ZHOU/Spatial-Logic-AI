/* --- Copyright University of Nottingham 2011. All rights reserved. ------
 > File:            ontology_rules.p
 > Purpose:	    Rules for a simple LBP reasoner.
 > Author:          Hai Nguyen & Brian Logan, July 12 2014
 > Documentation:   
 > Related Files:   LBPT_rules.p
 */


vars lbpt_ruleset;

define :ruleset lbpt_ruleset;
;;;    [DLOCAL [prb_show_conditions = true]];
    [VARS prb_allrules trigger_db];

	RULE Axiom_1
	[NEAR ?A ?A] [->> a1]
	[WHERE some_in_db_p([^a1], trigger_db)]
    ==>
	[SAYIF lbpt 'Axiom_1 Inconsistent data' ?a1]
	[ATMS_INCONSISTENT ?a1]

	RULE Axiom_2
	[NEAR ?A ?B] [->> a1]
	[FAR ?A ?B] [->> a2]
	[WHERE some_in_db_p([^a1 ^a2], trigger_db)]
    ==>
	[SAYIF lbpt 'Axiom_2 Inconsistent data' ?a1 ?a2]
	[ATMS_INCONSISTENT ?a1 ?a2]

	RULE Axiom_3
	[NEAR ?A ?B] [->> a1]
	[WHERE some_in_db_p([^a1], trigger_db)]
	[LVARS [consequent = add_new_formula([NEAR ^B ^A])]]
    ==>
	[SAYIF lbpt 'Justifying datum' ?consequent ?a1]
	[ATMS_JUSTIFY ?consequent [?a1]]

	RULE axiom_4
	[FAR ?A ?B] [->> a1]
	[WHERE some_in_db_p([^a1], trigger_db)]
	[LVARS [consequent = add_new_formula([FAR ^B ^A])]]
    ==>
	[SAYIF lbpt 'Justifying datum' ?consequent ?a1]
	[ATMS_JUSTIFY ?consequent [?a1]]

enddefine;

;;; LBPT_rules ends here
vars LBPT_rules = true;
