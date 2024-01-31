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

    RULE axiom_2
  	[NEAR ?A ?B] [->> a1]
	[WHERE some_in_db_p([^a1], trigger_db)]
	[LVARS [consequent = add_new_formula([NEAR ^B ^A])]]
    ==>
	[SAYIF lbpt 'Justifying datum' ?consequent ?a1]
	[ATMS_JUSTIFY ?consequent [?a1]]
	
    RULE axiom_3
  	[FAR ?A ?B] [->> a1]
	[WHERE some_in_db_p([^a1], trigger_db)]
	[LVARS [consequent = add_new_formula([FAR ^B ^A])]]
    ==>
	[SAYIF lbpt 'Justifying datum' ?consequent ?a1]
	[ATMS_JUSTIFY ?consequent [?a1]]

    RULE axiom_4
        [BPT ?A ?B] [->> a1]
	[BPT ?B ?C] [->> a2]
	[WHERE some_in_db_p([^a1 ^a2], trigger_db)]
	[LVARS [consequent = add_new_formula([NEAR ^C ^A])]]
    ==>
	[SAYIF lbpt 'Justifying datum' ?consequent ?a1 ?a2]
	[ATMS_JUSTIFY ?consequent [?a1 ?a2]]

    RULE axiom_5
        [BPT ?B ?A] [->> a1]
	[BPT ?B ?C] [->> a2]
	[WHERE some_in_db_p([^a1 ^a2], trigger_db)]
	[LVARS [consequent = add_new_formula( [NEAR ^C ^A])]]
    ==>
	[SAYIF lbpt 'Justifying datum' ?consequent ?a1 ?a2]
	[ATMS_JUSTIFY ?consequent [?a1 ?a2]]

    RULE axiom_6
        [BPT ?B ?A] [->> a1]
	[NEAR ?B ?C] [->> a2]
	[BPT ?C ?D] [->> a3]
	[FAR ?D ?A] [->> a4]
	[WHERE some_in_db_p([^a1 ^a2 ^a3 ^a4], trigger_db)]
;;;	[LVARS [cdata = maplist([^a1 ^a2 ^a3 ^a4], cannonicalise_datum)]]
    ==>
	[SAYIF lbpt 'Inconsistent data' ?a1 ?a2 ?a3 ?a4]
	[ATMS_INCONSISTENT ?a1 ?a2 ?a3 ?a4]
;;;	[ATMS_INCONSISTENT ??cdata]
	
    RULE axiom_7
        [NEAR ?A ?B] [->> a1]
	[BPT ?B ?C] [->> a2]
	[BPT ?C ?D] [->> a3]
	[FAR ?D ?A] [->> a4]
	[WHERE some_in_db_p([^a1 ^a2 ^a3 ^a4], trigger_db)]
;;;	[LVARS [cdata = maplist([^a1 ^a2 ^a3 ^a4], cannonicalise_datum)]]
    ==>
	[SAYIF lbpt 'Inconsistent data' ?a1 ?a2 ?a3 ?a4]
	[ATMS_INCONSISTENT ?a1 ?a2 ?a3 ?a4]
;;;	[ATMS_INCONSISTENT ??cdata]

    ;;; There are no LBPT axioms for BEQ, but splitting a BEQ relation in
    ;;; the input into two BPT relations increases the number of assumptions
    ;;; and potentially the number of nogoods
    RULE definition_beq1
	[BPT ?A ?B] [->> a1]
	[BPT ?B ?A] [->> a2]
	[WHERE some_in_db_p([^a1 ^a2], trigger_db)]
	[LVARS [consequent=add_new_formula([BEQ ^A ^B])]]
    ==>
	[SAYIF lbpt 'Justifying datum' ?consequent ?a1 ?a2]
	[ATMS_JUSTIFY ?consequent [?a1 ?a2]]
	
    RULE definition_beq2
	[BEQ ?A ?B] [->> a]
	[WHERE some_in_db_p([^a], trigger_db)]
	[LVARS [consequent1 = add_new_formula([BPT ^A ^B])]
	       [consequent2 = add_new_formula([BPT ^B ^A])]]
	==>
	[SAYIF lbpt_beq2 'Justifying datum' ?consequent1 ?consequent2 ?a]
	[ATMS_AND_JUSTIFY [?consequent1 ?consequent2] ?a]

    RULE near_far
        [NEAR ?A ?B] [->> a1]
	[FAR ?A ?B] [->> a2]
	[WHERE some_in_db_p([^a1 ^a2], trigger_db)]
;;;	[LVARS [cdata = maplist([^a1 ^a2], cannonicalise_datum)]]
    ==>
	[SAYIF lbpt 'Inconsistent data' ?a1 ?a2]
	[ATMS_INCONSISTENT ?a1 ?a2]
;;;	[ATMS_INCONSISTENT ??cdata]

enddefine;

;;; LBPT_rules ends here
vars LBPT_rules = true;
