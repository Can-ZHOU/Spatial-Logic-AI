/* --- Copyright University of Birmingham 1998. All rights reserved. ------
 > File:            demolibdir/lib/lisp_utils.p
 > Purpose:         Some Common Lisp utilities
 > Author:          Brian Logan, Apr 24 1998
 > Documentation:   
 > Related Files:   
 > RCS Info:        $Id: lisp_utils.p,v 2.14 1998/04/24 13:35:06 bsl Exp $
 */

;;; A simple minded attempt to reproduce some of the commoner Common
;;; Lisp functions.  Although these are based on the descriptions in
;;; CLtL2, they are not complete implementations of the Common Lisp 
;;; equivalents (this is inevitable given that we don't have &optional
;;; &rest or &key arguments).

;;; STILL TO DO:
;;; need a definition of pop and push (deferred)

;;; For some reason lmember is protected, even though member isn't, and
;;; unprotecting and redefining it causes lots of problems, so we have to
;;; compromise on the name ...

;;; This file contains the definitions of the following procedures
vars procedure lacons,
     procedure lassoc,
     procedure remove_if,
     procedure remove_if_not,
     procedure adjoin,
     procedure member,
     procedure lisp_lmember,
     procedure lmember_if,
     procedure lmember_if_not,
     procedure lunion,
     procedure lintersection,
     procedure every,
     procedure some,
     procedure reduce,
     procedure mapcar,
     procedure mapc,
     procedure mapcan,
     procedure floor,
     procedure ceiling,
     procedure truncate,
     procedure oddp,
     procedure evenp;

;;; Make a new association list by adding the pair (key . datum) to the old
;;; alist.  Alists are represented as lists of pairs as in lisp, rather than
;;; association procedures.
define lacons(key, datum, alist) -> new_alist;
    cons(conspair(key, datum), alist) -> new_alist;
    enddefine;

;;; Search <alist> for a pair with key <key>, and return the pair (as in
;;; lisp), or false if there is no such pair in the alist.  Takes an
;;; optional third argument specifying the equality test to use -- this
;;; defaults to `='
define lassoc(key, alist /* &optional equal */) -> pair;
    lvars procedure equal = nonop =;

    unless islist(alist) then
	;;; shuffle the arguments
	alist -> equal;
	key -> alist;
	-> key;
	endunless;
    
    for pair in alist do
	returnif(equal(front(pair), key));
	endfor;
    false -> pair;
    enddefine;

;;; A cut down version of the common lisp function remove-if
;;; Takes a list a returns another list with the elements which
;;; satisfy the predicate removed
define remove_if(predicate, list) -> new_list;
    lvars element;
    [% for element in list do
	   unless predicate(element) then
	       element
	       endunless;
	   endfor; %] -> new_list;
    enddefine;

;;; A cut down version of the common lisp function remove-if-not
;;; Takes a list a returns another list with the elements which don't 
;;; satisfy the predicate removed
define remove_if_not(predicate, list) -> new_list;
    lvars element;
    [% for element in list do
	   if predicate(element) then
	       element
	       endif;
	   endfor; %] -> new_list;
    enddefine;

;;; Add an element to a set, provided it is not already a member.  
;;; Takes an optional third argument specifying the equality test 
;;; to use -- this defaults to `='
define adjoin(item, list /* &optional equal */) -> new_list;
    lvars procedure equal = nonop =;

    unless islist(list) then
	;;; shuffle the arguments
	list -> equal;
	item -> list;
	-> item;
	endunless;

    if member(item, list, equal) then
	list -> new_list;
    else
	cons(item, list) -> new_list;
	endif;
    enddefine;

;;; A replacement for member which takes an optional third argument
;;; specifying the equality test to use -- this defaults to `='
define member(item, list /* &optional equal */) -> bool;
    lvars procedure equal = nonop =;

    unless islist(list) then
	;;; shuffle the arguments
	list -> equal;
	item -> list;
	-> item;
	endunless;

    lvars element;
    ;;; not worth making a closure here ...
    fast_for element in list do
	if equal(item, element) then
	    true -> bool;
	    return;
    	    endif;
	endfast_for;
    false -> bool;
    enddefine;

;;; As above, but returns the tail of the list beginning with the first
;;; element that satisfies the test is returned (as in lisp).  For
;;; compatibility with the built-in version of lmember, the equality test
;;; defaults `==', rather than `='.
define lisp_lmember(item, list /* &optional equal */) -> sublist;
    lvars procedure equal = nonop ==;

    unless islist(list) then
	;;; shuffle the arguments
	list -> equal;
	item -> list;
	-> item;
	endunless;

    lvars l;
    ;;; we could do this with fast_front/back to save the calls to hd ...
    fast_for l on list do
	if equal(item, hd(l)) then
	    l -> sublist;
	    return;
    	    endif;
	endfast_for;
    [] -> sublist;
    enddefine;

;;; Search list for an element that satisfies the test.  If none is found,
;;; return the emepty list, otherwise return the tail of the list beginning
;;; with the first element that satisfies the test.
define lmember_if(predicate, list) -> sublist;
    lvars l;
    ;;; we could do this with fast_front/back to save the calls to hd ...
    fast_for l on list do
	if predicate(hd(l)) then
	    l -> sublist;
	    return;
    	    endif;
	endfast_for;
    [] -> sublist;
    enddefine;

;;; Search list for an element that does not satisfy the test.  If none is
;;; found, return the emepty list, otherwise return the tail of the list
;;; beginning with the first element that satisfies the test.
define lmember_if_not(predicate, list) -> sublist;
    lvars l;
    ;;; we could do this with fast_front/back to save the calls to hd ...
    fast_for l on list do
	unless predicate(hd(l)) then
	    l -> sublist;
	    return;
	    endunless;
	endfast_for;
    [] -> sublist;
    enddefine;

;;; Takes two lists and returns a new list containing everything that is an
;;; element of either argument lists.  If there is a duplication between the
;;; two lists, only one of the duplicate instances will be in the result.
;;; If either list has duplicate entries, the redundant entries may or may
;;; not appear in the result.  (In the current implementation they do not,
;;; if there are no duplicates in the original lists.)
define lunion(list1, list2) -> union;
    lvars element;

    [% dl(list1), 
       for element in list2 do
	   unless member(element, list1) then
	       element,
	       endunless;
	   endfor; %] -> union;
    enddefine;

;;; Takes two lists and returns a new list containing everything that is an
;;; element of both argument lists.  If either list has duplicate entries,
;;; the redundant entries may or may not appear in the result.  (In the
;;; current implementation they do not, if there are no duplicates in the
;;; original lists.)
define lintersection(list1, list2) -> intersection;
    lvars element;

    [% for element in list1 do
	   if member(element, list2) then
	       element,
	      endif;
	   endfor; %] -> intersection;
    enddefine;

;;; Returns <true> if the (n-ary) predicate <pred> succeeds for each set of
;;; args from <seq_1> ... <seq_n>.  The predicate must take as many
;;; arguments as there are sequences (lists or vectors) provided.  The
;;; predicate is first applied to the elements with index 1 in each of the
;;; sequences, and possibly then to the elements with index 2 and so on
;;; until the predicate returns false or the end of the shortest sequence is
;;; reached.  Note: returns <false> if any of the sequences are empty.
define every(/* seq1 ... seqn */ procedure pred) -> bool;
    lvars nargs = pdnargs(pred), 
	  stacklen = stacklength(),
	  bool = true;

    unless stacklen >= nargs then
	mishap('Wrong number of arguments: expected ' >< 
	       nargs >< ' found ' >< stacklen, 
	       [^pred]);
	endunless;

    ;;; Find the shortest sequence
    lvars seqs = conslist(nargs),
    	  seqlen = length(seqs(1)),
	  seq, i;
    for seq in seqs do
    	if length(seq) == 0 then
	    false -> bool;
	    return();
	elseif length(seq) < seqlen then
	    length(seq) -> seqlen;
	    endif;
	endfor;

    ;;; Work down the sequences, testing for false ...
    for i from 1 to seqlen do
	if not(pred(for seq in seqs do seq(i) endfor)) then
	    false -> bool;
	    return();
	    endif;
	endfor;
    enddefine;
	
;;; Returns <true> if the (n-ary) predicate <pred> succeeds for at least one
;;; of the sets of args from <seq_1> ... <seq_n>.  The predicate must take
;;; as many arguments as there are sequences (lists or vectors) provided.
;;; The predicate is first applied to the elements with index 1 in each of
;;; the sequences, and possibly then to the elements with index 2 and so on
;;; until the predicate returns true or the end of the shortest sequence is
;;; reached.  Note: returns <false> if any of the sequences are empty.
define some(/* seq1 ... seqn */ procedure pred) -> bool;
    lvars nargs = pdnargs(pred), 
	  stacklen = stacklength(),
	  bool = false;

    unless stacklen >= nargs then
	mishap('Wrong number of arguments: expected ' >< 
	       nargs >< ' found ' >< stacklen, 
	       [^pred]);
	endunless;

    ;;; Find the shortest sequence
    lvars seqs = conslist(nargs),
    	  seqlen = length(seqs(1)),
	  seq, i;
    for seq in seqs do
    	if length(seq) == 0 then
	    false -> bool;
	    return();
	elseif length(seq) < seqlen then
	    length(seq) -> seqlen;
	    endif;
	endfor;

    ;;; Work down the sequences, testing for false ...
    for i from 1 to seqlen do
	if pred(for seq in seqs do seq(i) endfor) then
	    true -> bool;
	    return();
	    endif;
	endfor;
    enddefine;

;;; Combine all the elements of a list using a binary operation.  Unlike the
;;; lisp version, this doesn't work for arbitratry sequences, you can't
;;; specify which end to start from, or what the initial value should be.
;;; Also the argument order is reversed, so that it can be used with the
;;; Pop-11 maplist.  However, like the lisp version, if it is given a list
;;; of one element, it returns the element.
define reduce(list, procedure fn) -> result;
    lvars result = hd(list),
          element;

    ;;; another cleaner, but slower, way to do this would be:
    ;;; hd(list), applist(tl(list), fn) -> result;
    fast_for element in tl(list) do
	fn(result, element) -> result;
	endfor;
    enddefine;

;;; Apply the procedure <fn> to each set of args taken from <seq_1>
;;; ... <seq_n>.  The procedure must take as many arguments as there are
;;; sequences (lists or vectors) provided.  The procedure is first applied
;;; to the hd of each sequence and then to the hd(tl) and so on until the
;;; end of the shortest sequence is reached.  Returns a list of the results
;;; of the successive calls to the procedure. 
define mapcar(/* seq1 ... seqn */ procedure fn) -> results;
    lvars nargs = pdnargs(fn), 
	  stacklen = stacklength(),
	  results = [];

    unless stacklen >= nargs then
	mishap('Wrong number of arguments: expected ' >< 
	       nargs >< ' found ' >< stacklen, 
	       [^fn]);
	endunless;

    ;;; Find the shortest sequence
    lvars seqs = conslist(nargs),
    	  seqlen = length(seqs(1)),
	  seq, i;
    for seq in seqs do
    	if length(seq) == 0 then
	    return();
	elseif length(seq) < seqlen then
	    length(seq) -> seqlen;
	    endif;
	endfor;

    ;;; Work down the sequences, accumulating the results ...
    [ % for i from 1 to seqlen do
	    fn(for seq in seqs do seq(i) endfor);
	    endfor; %] -> results;
    enddefine;

;;; As above, but does not accumulate the results of calling the procedure.
define mapc(/* seq1 ... seqn */ procedure fn);
    lvars nargs = pdnargs(fn), 
	  stacklen = stacklength();

    unless stacklen >= nargs then
	mishap('Wrong number of arguments: expected ' >< 
	       nargs >< ' found ' >< stacklen, 
	       [^fn]);
	endunless;

    ;;; Find the shortest sequence
    lvars seqs = conslist(nargs),
    	  seqlen = length(seqs(1)),
	  seq, i;
    for seq in seqs do
    	if length(seq) == 0 then
	    return();
	elseif length(seq) < seqlen then
	    length(seq) -> seqlen;
	    endif;
	endfor;

    ;;; Work down the sequences, calling <fn> for its side effects ...
    for i from 1 to seqlen do
	fn(for seq in seqs do seq(i) endfor);
	endfor;
    enddefine;

;;; A very inefficient implementation of mapcan.  mapcan is like maplist
;;; except that it combines the results of the function using nc_<> rather
;;; than <>.  Note that this can't have an updater as the sturcture of the
;;; result list is lost, e.g. mapcan(tl, [[1 2 3] [a b]]) => [2 3 b].
;;; See also the built-in procedure ncmaplist.
define mapcan(procedure fn, list) -> result;
    reduce(nonop nc_<>, maplist(list, fn)) -> result;
    enddefine;

;;; Truncate <number> towards negative infinity, i.e. the result is the 
;;; largest integer that is not larger than the argument.
define floor(number) -> flr;
    if number >= 0 then 
	intof(number) -> flr;
    else
	round(number - 1/2) -> flr;
	endif;
    enddefine;
	
;;; Truncate <number> towards positive infinity, i.e. the result is the 
;;; smallest integer that the is not smaller than the argument.
define ceiling(number) -> ceil;
    if number > 0 and fracof(number) > 0 then
	intof(number + 1) -> ceil;
    else
	intof(number) -> ceil;
	endif;
    enddefine;

;;; Truncate <number> towards zero, i.e. the result is the integer with the
;;; same sign as the argument and which has the greatest integral magnitude
;;; not greater than that of the argument.  Provided for consistency only.
define truncate(number) -> num;
    intof(number) -> num;
    enddefine;

;;; Note that the definition of round is essentially the same as in Common Lisp

;;; Predicate which returns true if the argument integer is odd (not divisible
;;; by 2) and otherwise is false.  It is an error if the argument is not an
;;; integer
define oddp(integer) -> bool;
    if isintegral(integer) then
    	integer mod 2 /= 0 -> bool;
    else
	mishap('Not an integer', [^integer]);
	endif;
    enddefine;

;;; Predicate which returns true if the argument integer is even (divisible
;;; by 2) and otherwise is false.  It is an error if the argument is not an
;;; integer
define evenp(integer) -> bool;
    if isintegral(integer) then
    	integer mod 2 == 0 -> bool;
    else
	mishap('Not an integer', [^integer]);
	endif;
    enddefine;

;;; A sort-of implementation of the Common Lisp pop macro
;;; <place> is the name of variable containing a list .  The result of 
;;; pop is the hd of the contents of <place>, and as side effect the tl
;;; of the contents of <place> is stored back into place.  If the list
;;; held in <place> is viewed as a push-down stack, then pop pops an 
;;; element from the top of the stack and returns it.


;;; lisp_utils ends here.
vars lisp_utils = true;
