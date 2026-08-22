/**
 * # List functions
 *
 * Functions for working with lists.
 *
 * ## Description
 *
 * Lists are implemented in terms of JavaScript arrays. There is,
 * however, no direct JavaScript equivalent to the Lisp concept of a
 * *cons cell*, such as `'(1 . 2)`. Instead, such values are
 * represented as a *dotted list*, which is a list where the
 * penultimate value is the symbol `|.|` (in JavaScript,
 * `Symbol.for('.')`).
 *
 *     > (dotted-list? '(1 . 2))
 *     #t
 *     > (dotted-list? '(1 2 . 3))
 *     #t
 *     > (dotted-list? (list 1 '|.| 2))
 *     #t
 *
 * The simplifying assumption is made that dotted lists are only used
 * to represent values that cannot be expressed without a dot. This
 * permits most list functions to be implemented as simple array
 * operations, although some checks are necessary in places since
 * dotted lists are also implemented as arrays.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Whether something is a pair, i.e., a cons cell.
 *
 * Similar to [`pair?` in Racket][rkt:pairp] and
 * [`consp` in Common Lisp][cl:consp].
 *
 * [rkt:pairp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._pair~3f%29%29
 * [cl:consp]: http://clhs.lisp.se/Body/f_consp.htm
 */
declare function pairp_(x: any): any;
declare namespace pairp_ {
    var fsource: (symbol | (symbol | (number | symbol | symbol[])[])[])[];
}
/**
 * Whether something is the empty list.
 *
 * Similar to [`null?` in Racket][rkt:nullp].
 *
 * [rkt:nullp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._null~3f%29%29
 */
declare function nullp_(x: any): any;
declare namespace nullp_ {
    var fsource: (symbol | (symbol | (number | symbol | symbol[])[])[])[];
}
/**
 * Whether something is a list.
 *
 * This function checks whether the list is a proper list, i.e.,
 * whether the list is terminated by the empty list. In that regard,
 * this function is more similar to [`list?` in Racket][rkt:listp]
 * than to [`listp` in Common Lisp][cl:listp].
 *
 * [rkt:listp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list~3f%29%29
 * [cl:listp]: http://clhs.lisp.se/Body/f_listp.htm#listp
 */
declare function listp_(x: any): any;
declare namespace listp_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Whether something is a pair or a list.
 *
 * Similar to [`listp` in Common Lisp][cl:listp] and
 * [`listp` in Emacs Lisp], which are not as
 * rigorous as `list?` in Scheme.
 *
 * [cl:listp]: http://clhs.lisp.se/Body/f_listp.htm#listp
 * [el:listp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/List_002drelated-Predicates.html#index-listp
 */
declare function pairOrListP_(x: any): any;
declare namespace pairOrListP_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Make a list.
 *
 * Similar to [`list` in Racket][rkt:list] and
 * [`list` in Common Lisp][cl:list].
 *
 * [rkt:list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list%29%29
 * [cl:list]: http://clhs.lisp.se/Body/f_list_.htm
 */
declare function list_(...args: any[]): any;
declare namespace list_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Create a cons cell whose CAR is `x` and CDR is `y`.
 *
 * Similar to [`cons` in Racket][rkt:cons] and
 * [`cons` in Common Lisp][cl:cons].
 *
 * [rkt:cons]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cons%29%29
 * [cl:cons]: http://clhs.lisp.se/Body/f_cons.htm
 */
declare function cons_(x: any, y: any): any;
declare namespace cons_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[][])[])[];
}
/**
 * Make a dotted list. Like `list`, but the final argument
 * is used as the tail, instead of as the final element.
 *
 *     > (list* 1 '())
 *     '(1)
 *     > (list* 1 2)
 *     '(1 . 2)
 *     > (list* 1 2 3)
 *     '(1 2 . 3)
 *
 * Also known as `cons*`. Similar to
 * [`list*` in Racket][rkt:list-star].
 *
 * [rkt:list-star]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list%2A%29%29
 */
declare function listStar_(...args: any[]): any;
declare namespace listStar_ {
    var fsource: (symbol | (symbol | ((number | symbol | symbol[])[] | undefined)[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
/**
 * Make a list of `n` elements. The function `proc` is applied
 * to the integers from `0` to `n - 1`.
 *
 * Similar to [`build-list` in Racket][rkt:build-list].
 *
 * [rkt:build-list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._build-list%29%29
 */
declare function buildList_(n: any, proc: any): any;
declare namespace buildList_ {
    var fsource: (symbol | (symbol | (number | symbol)[])[])[];
}
/**
 * Make a list of length `k`, where every element is the value `v`.
 *
 * Similar to [`make-list` in Racket][rkt:make-list].
 *
 * [rkt:make-list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._make-list%29%29
 */
declare function makeList_(k: any, v: any): any;
declare namespace makeList_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | never[])[])[][] | (symbol | symbol[] | (symbol | (number | symbol)[])[][])[])[])[];
}
/**
 * List concatenation.
 *
 * Similar to [`append` in Racket][rkt:append] and
 * [`append` in Common Lisp][cl:append].
 *
 * [rkt:append]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._append%29%29
 * [cl:append]: http://clhs.lisp.se/Body/f_append.htm#append
 */
declare function append_(...args: any[]): any;
declare namespace append_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Flatten an arbitrarily nested list.
 *
 * Similar to [`flatten` in Racket][rkt:flatten].
 *
 * [rkt:flatten]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._flatten%29%29
 */
declare function flatten_(lst: any): any;
declare namespace flatten_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[];
}
/**
 * Return the first element of a list.
 *
 * Similar to [`first` in Racket][rkt:first].
 *
 * [rkt:first]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._first%29%29
 */
declare function first_(lst: any): any;
declare namespace first_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the second element of a list.
 *
 * Similar to [`second` in Racket][rkt:second].
 *
 * [rkt:second]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._second%29%29
 */
declare function second_(lst: any): any;
declare namespace second_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the third element of a list.
 *
 * Similar to [`third` in Racket][rkt:third].
 *
 * [rkt:third]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._third%29%29
 */
declare function third_(lst: any): any;
declare namespace third_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the fourth element of a list.
 *
 * Similar to [`fourth` in Racket][rkt:fourth].
 *
 * [rkt:fourth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fourth%29%29
 */
declare function fourth_(lst: any): any;
declare namespace fourth_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the fifth element of a list.
 *
 * Similar to [`fifth` in Racket][rkt:fifth].
 *
 * [rkt:fifth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fifth%29%29
 */
declare function fifth_(lst: any): any;
declare namespace fifth_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the sixth element of a list.
 *
 * Similar to [`sixth` in Racket][rkt:sixth].
 *
 * [rkt:sixth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._sixth%29%29
 */
declare function sixth_(lst: any): any;
declare namespace sixth_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the seventh element of a list.
 *
 * Similar to [`seventh` in Racket][rkt:seventh].
 *
 * [rkt:seventh]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._seventh%29%29
 */
declare function seventh_(lst: any): any;
declare namespace seventh_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the eighth element of a list.
 *
 * Similar to [`eighth` in Racket][rkt:eighth].
 *
 * [rkt:eighth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._eighth%29%29
 */
declare function eighth_(lst: any): any;
declare namespace eighth_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the ninth element of a list.
 *
 * Similar to [`ninth` in Racket][rkt:ninth].
 *
 * [rkt:ninth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._ninth%29%29
 */
declare function ninth_(lst: any): any;
declare namespace ninth_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the tenth element of a list.
 *
 * Similar to [`tenth` in Racket][rkt:tenth].
 *
 * [rkt:tenth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._tenth%29%29
 */
declare function tenth_(lst: any): any;
declare namespace tenth_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the tail of a list.
 *
 * Interprets `lst` as a linked list of pairs, and returns the second
 * element of the first pair.
 *
 * Similar to [`cdr` in Racket][rkt:cdr] and
 * [`cdr` in Common Lisp][cl:cdr].
 *
 * [rkt:cdr]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cdr%29%29
 * [cl:cdr]: http://clhs.lisp.se/Body/f_car_c.htm#cdr
 */
declare function cdr_(lst: any): any;
declare namespace cdr_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the tail of a list.
 *
 * Similar to [`rest` in Racket][rkt:rest].
 *
 * [rkt:rest]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._rest%29%29
 */
declare function rest_(lst: any): any;
declare namespace rest_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Access the list element indicated by
 * one or more `indices`.
 */
declare function listRef_(lst: any, ...indices: any[]): any;
declare namespace listRef_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Return the `n`-th element of a list.
 *
 * Similar to [`nth` in Racket][rkt:nth] and
 * [`nth` in Common Lisp][cl:nth].
 *
 * [rkt:nth]: https://docs.racket-lang.org/collections/collections-api.html#%28def._%28%28lib._data%2Fcollection..rkt%29._nth%29%29
 * [cl:nth]: http://clhs.lisp.se/Body/f_nth.htm#nth
 */
declare function nth_(n: any, lst: any): any;
declare namespace nth_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Set a list position to a given value.
 * Returns a new list.
 */
declare function listSet_(lst: any, ...indicesAndValue: any[]): any;
declare namespace listSet_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | ((symbol | (symbol | symbol[])[])[] | (number | symbol | symbol[])[])[])[])[])[])[];
}
/**
 * Set a list position to a given value.
 * Modifies the original list.
 */
declare function listSetX_(lst: any, ...indicesAndValue: any[]): any;
declare namespace listSetX_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Return the `n`-th CDR element of a list.
 */
declare function listTail_(lst: any, n: any): any;
declare namespace listTail_ {
    var fsource: (symbol | (symbol | (number | symbol)[] | (symbol | (number | symbol)[])[])[])[];
}
/**
 * Return the `n`-th CDR element of a list.
 *
 * Similar to [`nthcdr` in Common Lisp][cl:nthcdr].
 *
 * [cl:nth]: http://clhs.lisp.se/Body/f_nthcdr.htm#nthcdr
 */
declare function nthcdr_(n: any, lst: any): any;
declare namespace nthcdr_ {
    var fsource: (symbol | (symbol | (number | symbol)[] | (symbol | (number | symbol)[])[])[])[];
}
/**
 * Take the `n` first elements from `lst`.
 *
 * Similar to [`take` in Racket][rkt:take].
 *
 * [rkt:take]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._take%29%29
 */
declare function take_(lst: any, n: any): any;
declare namespace take_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Return the list obtained by dropping
 * the first `n` elements from `lst`.
 *
 * Similar to [`drop` in Racket][rkt:drop].
 *
 * [rkt:drop]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop%29%29
 */
declare function drop_(lst: any, n: any): any;
declare namespace drop_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the list obtained by dropping
 * the last `n` elements from `lst`.
 *
 * Similar to [`drop-right` in Racket][rkt:drop-right].
 *
 * [rkt:drop-right]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop-right%29%29
 */
declare function dropRight_(lst: any, n: any): any;
declare namespace dropRight_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Reverse the order of a list.
 * Returns a new list.
 *
 * Similar to [`reverse` in Racket][rkt:reverse].
 *
 * [rkt:reverse]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._reverse%29%29
 */
declare function reverse_(lst: any): any;
declare namespace reverse_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Reverse the order of a list.
 */
declare function reversex_(lst: any): any;
declare namespace reversex_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return a list where the last `n` conses have been omitted.
 *
 * Similar to [`butlast` in Common Lisp][cl:butlast].
 *
 * [cl:butlast]: http://clhs.lisp.se/Body/f_butlas.htm#butlast
 */
declare function butlast_(x: any, n?: any): any;
declare namespace butlast_ {
    var fsource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | symbol[][])[])[][])[])[];
}
/**
 * Return a list where the last `n` conses have been omitted.
 * Changes the original list.
 *
 * Similar to [`nbutlast` in Common Lisp][cl:nbutlast].
 *
 * [cl:nbutlast]: http://clhs.lisp.se/Body/f_butlas.htm#nbutlast
 */
declare function nbutlast_(x: any, n?: any): any;
declare namespace nbutlast_ {
    var fsource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[];
}
/**
 * Pop an element off the beginning of a list.
 *
 * Similar to [`pop` in Common Lisp][cl:pop].
 *
 * [cl:pop]: http://clhs.lisp.se/Body/m_pop.htm#pop
 */
declare function popLeftX_(lst: any): any;
declare namespace popLeftX_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Pop an element off the end of a list.
 *
 * Similar to [`Array.prototype.pop()` in JavaScript][js:pop].
 *
 * [js:pop]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop
 */
declare function popRightX_(lst: any): any;
declare namespace popRightX_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Push an element onto the beginning of a list.
 *
 * Similar to [`push` in Common Lisp][cl:push].
 *
 * [cl:push]: http://clhs.lisp.se/Body/m_push.htm#push
 */
declare function pushLeftX_(lst: any, x: any): any;
declare namespace pushLeftX_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Push an element onto the end of a list.
 *
 * Similar to [`Array.prototype.push()` in JavaScript][js:push].
 *
 * [js:push]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push
 */
declare function pushRightX_(lst: any, x: any): any;
declare namespace pushRightX_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the length of a list.
 *
 * Similar to [`length` in Racket][rkt:length] and
 * [`length` in Common Lisp][cl:length].
 *
 * [rkt:length]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._length%29%29
 * [cl:length]: http://clhs.lisp.se/Body/f_length.htm#length
 */
declare function length_(lst: any): any;
declare namespace length_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the last element of a list.
 *
 * Similar to [`last` in Racket][rkt:last].
 *
 * [rkt:last]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last%29%29
 */
declare function last_(lst: any): any;
declare namespace last_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the last pair of a list.
 *
 * Similar to [`last-pair` in Racket][rkt:last-pair] and
 * [`last` in Common Lisp][cl:last].
 *
 * [rkt:last-pair]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last-pair%29%29
 * [cl:last]: http://clhs.lisp.se/Body/f_last.htm#last
 */
declare function lastPair_(lst: any): any;
declare namespace lastPair_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | ((symbol | symbol[])[] | undefined)[] | (symbol | (symbol | undefined)[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
/**
 * Return the last cdr of a list, i.e., the terminating empty list.
 */
declare function lastCdr_(lst: any): any;
declare namespace lastCdr_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | ((symbol | symbol[])[] | undefined)[])[])[];
}
/**
 * Set the CAR of a list.
 *
 * Similar to [`set-car!` in Racket][rkt:set-car].
 *
 * [rkt:set-car]: https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html#%25_idx_416
 */
declare function setCarX_(x: any, y: any): any;
declare namespace setCarX_ {
    var fsource: (symbol | (symbol | (number | symbol | symbol[])[])[] | undefined)[];
}
/**
 * Set the CDR of a list.
 *
 * Similar to [`set-cdr!` in Racket][rkt:set-cdr].
 *
 * [rkt:set-cdr]: https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html#%25_idx_418
 */
declare function setCdrX_(x: any, y: any): any;
declare namespace setCdrX_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[][] | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | symbol[] | symbol[][])[][])[])[])[] | undefined)[];
}
/**
 * Whether something is a dotted list.
 *
 * Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
 *
 * [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
 */
declare function dottedListP_(x: any): any;
declare namespace dottedListP_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (number | symbol | symbol[])[])[])[];
}
/**
 * Whether something is a dotted pair.
 */
declare function dottedPairP_(x: any): any;
declare namespace dottedPairP_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (number | symbol | symbol[])[])[])[];
}
/**
 * Whether something is a proper dotted list.
 */
declare function dottedProperListP_(x: any): any;
declare namespace dottedProperListP_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (number | symbol | symbol[])[])[])[];
}
/**
 * Whether something is an improper dotted list.
 */
declare function dottedImproperListP_(x: any): any;
declare namespace dottedImproperListP_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | symbol[])[])[] | (number | symbol | symbol[])[])[])[];
}
/**
 * Return the head of a dotted list.
 */
declare function dottedListHead_(lst: any): any;
declare namespace dottedListHead_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the tail of a dotted list.
 */
declare function dottedListTail_(lst: any): any;
declare namespace dottedListTail_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Create a dotted list link.
 */
declare function dottedListLink_(x: any): any;
declare namespace dottedListLink_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Return the CDR of a dotted pair.
 */
declare function dottedPairCdr_(x: any): any;
declare namespace dottedPairCdr_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Parse a dotted list.
 */
declare function dottedListParse_(lst: any): any;
declare namespace dottedListParse_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the length of a dotted list.
 */
declare function dottedListLength_(lst: any): any;
declare namespace dottedListLength_ {
    var fsource: (symbol | (number | symbol)[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[];
}
/**
 * Access the dotted list element indicated by
 * one or more `indices`.
 */
declare function dottedListRef_(lst: any, ...indices: any[]): any;
declare namespace dottedListRef_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol)[] | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[])[])[])[];
}
/**
 * Set a dotted list position to a given value.
 * Returns a new list.
 */
declare function dottedListSet_(lst: any, ...indicesAndValue: any[]): any;
declare namespace dottedListSet_ {
    var fsource: (symbol | (symbol | ((number | symbol | symbol[])[] | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[] | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | (number | symbol)[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[][])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[] | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | (number | symbol)[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[])[])[])[])[])[];
}
/**
 * Set a dotted list position to a given value.
 * Modifies the original list.
 */
declare function dottedListSetX_(lst: any, ...indicesAndValue: any[]): any;
declare namespace dottedListSetX_ {
    var fsource: (symbol | (symbol | (number | symbol)[] | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol)[] | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[])[])[])[];
}
/**
 * Return the first element of a dotted list.
 */
declare function dottedListFirst_(lst: any): any;
declare namespace dottedListFirst_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the second element of a dotted list.
 */
declare function dottedListSecond_(lst: any): any;
declare namespace dottedListSecond_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the third element of a dotted list.
 */
declare function dottedListThird_(lst: any): any;
declare namespace dottedListThird_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fourth element of a dotted list.
 */
declare function dottedListFourth_(lst: any): any;
declare namespace dottedListFourth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fifth element of a dotted list.
 */
declare function dottedListFifth_(lst: any): any;
declare namespace dottedListFifth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the sixth element of a dotted list.
 */
declare function dottedListSixth_(lst: any): any;
declare namespace dottedListSixth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the seventh element of a dotted list.
 */
declare function dottedListSeventh_(lst: any): any;
declare namespace dottedListSeventh_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the eighth element of a dotted list.
 */
declare function dottedListEighth_(lst: any): any;
declare namespace dottedListEighth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the ninth element of a dotted list.
 */
declare function dottedListNinth_(lst: any): any;
declare namespace dottedListNinth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the tenth element of a dotted list.
 */
declare function dottedListTenth_(lst: any): any;
declare namespace dottedListTenth_ {
    var fsource: (symbol | (number | symbol)[])[];
}
/**
 * Return the last element of a dotted list.
 */
declare function dottedListLast_(lst: any): any;
declare namespace dottedListLast_ {
    var fsource: (symbol | (symbol | undefined)[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[];
}
/**
 * Make a dotted list.
 */
declare function makeDottedList_(car: any, cdr: any): any;
declare namespace makeDottedList_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Make a dotted pair.
 */
declare function makePair_(car: any, cdr: any): any;
declare namespace makePair_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Whether something is a proper list,
 * i.e., a list that is terminated by
 * the empty list.
 *
 * Similar to [`proper-list?` in Racket][rkt:proper-list-p].
 *
 * [rkt:proper-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#proper-list-p
 */
declare function properListP_(x: any): any;
declare namespace properListP_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Whether something is an improper list,
 * i.e., a dotted list.
 *
 * Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
 *
 * [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
 */
declare function improperListP_(x: any): any;
declare namespace improperListP_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Whether something is a circular list.
 *
 * Similar to [`circular-list?` in Racket][rkt:circular-list-p].
 *
 * [rkt:circular-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#circular-list-p
 */
declare function circularListP_(x: any): any;
declare namespace circularListP_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Convert an array list to a linked list.
 */
declare function listToDottedList_(x: any): any;
declare namespace listToDottedList_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[];
}
/**
 * Convert a linked list to an array list.
 */
declare function dottedListToList_(x: any): any;
declare namespace dottedListToList_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[][])[])[];
}
export { append_ as append, buildList_ as buildList, butlast_ as butlast, cdr_ as cdr, cdr_ as tail, cdr_ as tail_, circularListP_ as circularListP, cons_ as cons, dottedListP_ as dottedListP, dropRight_ as dropRight, drop_ as drop, eighth_ as eighth, fifth_ as fifth, first_ as car, first_ as car_, first_ as first, first_ as head, first_ as head_, flatten_ as flatten, fourth_ as fourth, improperListP_ as improperListP, lastCdr_ as dottedListLastCdr_, lastCdr_ as lastCdr, lastPair_ as lastCons_, lastPair_ as lastPair, last_ as last, length_ as length, listStar_ as listStar, listp_ as listp, listp_ as properListP, list_ as list, makeDottedList_ as makeDottedList, makeList_ as makeList, makePair_ as makePair, nbutlast_ as nbutlast, ninth_ as ninth, nth_ as dottedListNth, nth_ as dottedListNth_, nth_ as nth, nthcdr_ as dottedListNthcdr, nthcdr_ as dottedListNthcdr_, nthcdr_ as nthcdr, nullp_ as nullp, pairp_ as consp, pairp_ as consp_, pairp_ as pairp, popLeftX_ as popx, popLeftX_ as popx_, popLeftX_ as popLeftX, popRightX_ as popRightX, pushLeftX_ as pushx, pushLeftX_ as pushLeftX, pushRightX_ as appendToList, pushRightX_ as pushRightX, rest_ as rest, reverse_ as reverse, second_ as cadr_, second_ as second, setCarX_ as setCarX, setCdrX_ as setCdrX, seventh_ as seventh, sixth_ as sixth, take_ as take, tenth_ as tenth, third_ as third, append_, buildList_, butlast_, cdr_, circularListP_, cons_, dottedImproperListP_, dottedListToList_, dottedListEighth_, dottedListFifth_, dottedListFirst_, dottedListFourth_, dottedListHead_, dottedListLast_, dottedListLength_, dottedListNinth_, dottedListParse_, dottedListRef_, dottedListSecond_, dottedListSetX_, dottedListSet_, dottedListSeventh_, dottedListSixth_, dottedListTail_, dottedListTenth_, dottedListLink_, dottedListThird_, dottedListP_, dottedPairCdr_, dottedPairP_, dottedProperListP_, dropRight_, drop_, eighth_, fifth_, first_, flatten_, fourth_, improperListP_, lastCdr_, lastPair_, last_, length_, listToDottedList_, listRef_, listSetX_, listSet_, listStar_, listTail_, listp_, list_, makeDottedList_, makeList_, makePair_, nbutlast_, ninth_, nth_, nthcdr_, nullp_, pairOrListP_, pairp_, popLeftX_, popRightX_, properListP_, pushLeftX_, pushRightX_, rest_, reversex_, reverse_, second_, setCarX_, setCdrX_, seventh_, sixth_, take_, tenth_, third_ };
