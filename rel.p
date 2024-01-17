uses lisp_utils

;;; Property containing all the spatial objects in both data sets
vars objects = newassoc([]); 

vars os_url = consword('http://www.ordnancesurvey.co.uk/ontology/BuildingsAndPlaces/v1.1/BuildingsAndPlaces.owl'),
     sm_url = consword('http://www.semanticweb.org/ontologies/2012/2/OpenStreetMapFeatures.owl');

vars prb_name_prefix = newassoc([[^os_url 'os'] [^sm_url 'sm']]);

define read_relations(filename) -> relations;
    lvars item, items;

    lvars itemrep = incharitem(discin(filename));
    1 -> item_chartype(`/`, itemrep);
    1 -> item_chartype(`:`, itemrep);
    1 -> item_chartype(`.`, itemrep);

    ;;; Forget objects in any previous data set.
    clearproperty(objects);

    ;;; Each relation in the input is split into 9 items. We assume that the
    ;;; data files are well formed, i.e., don't contain partial relations.
    [% until itemrep() == termin do
	[% until (itemrep() ->> item) = "]" do
	       item;
	       enduntil %] -> items;
	[% items(1), 
	   prb_name(items(2), items(4)),
	   prb_name(items(5), items(7)) %];
	enduntil %] -> relations;
    enddefine;

;;; Convert an owl url/id to a word for prb, and remember it.
define prb_name(url, id) -> name;
    consword(prb_name_prefix(url) >< id) -> name;
    true -> objects(name);
    enddefine;

;;; Returns true if <relation> is a mapping relation, i.e., relates spatial
;;; objects in different data sets. This should only be BPT relations, but
;;; the data also contains BEQ mapping relation. Only mapping relations can
;;; be assumptions -- other relations are premises.
define mapping_relation(relation) -> bool;
    (subword(1, 2, relation(2)) = subword(1, 2, relation(3))) -> bool;
    enddefine;

;;; Add assumptions first. This keeps the environments small, since
;;; assumption ids determine bit position. It also means that is something
;;; is both an assumption and a premise, the node will have a non-empty
;;; environment in its label.
