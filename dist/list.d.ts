/**
 * # List functions
 *
 * Functions for working with lists.
 *
 * ## Description
 *
 * Lists are implemented in terms of JavaScript arrays. There is,
 * however, no direct JavaScript equivalent to the Lisp concept of a
 * *cons cell*, such as `'(1 . 2)`. Therefore, Roselisp defines a
 * list type called a linked list, whose constituent links correspond
 * to the cons cell concept.
 *
 * ### Terminology
 *
 * The *empty list* is the list `'()`, represented by the JavaScript
 * array `[]`. The function `null?` returns `#t` only when passed the
 * empty list.
 *
 *     > (null? '())
 *     #t
 *
 * A list with one or more elements is a *nonempty list*. There are
 * two forms of nonempty lists: *array lists* and *linked lists*. An
 * array list is a list that is straightforwardly implemented in
 * terms of a JavaScript array, such as `'(1)`, `'(1 2)` and
 * `'(1 2 3)`. These correspond to the JavaScript arrays `[1]`,
 * `[1, 2]` and `[1, 2, 3]`, respectively. The function `array-list?`
 * returns `#t` when passed an array list.
 *
 *     > (array-list? '(1))
 *     #t
 *     > (array-list? '(1 2))
 *     #t
 *     > (array-list? '(1 2 3))
 *     #t
 *
 * The other type of nonempty list is the *linked list*, which is
 * composed out of *linked list links*. A linked list link is an
 * expression such as `'(1 . ())`, which is implemented in terms of
 * the JavaScript array `[1, Symbol.for('.'), []]`. Here, the
 * penultimate array element is the special symbol `.` (i.e., `'.`),
 * which is also referred to as the *cons dot*. The function
 * `linked-list-link?` returns `#t` when passed a linked list link.
 *
 *     > (linked-list-link? '(1 . ()))
 *     #t
 *     > (linked-list-link? '(1 2 . ()))
 *     #t
 *
 * By chaining such links together, a sequence of values is obtained.
 * If the final chain is the empty list, then the resulting structure
 * is referred to as a *linked list*. For example, the linked list
 * `'(1 . (2 . ()))` may be considered to represent the same sequence
 * as the array list `(1 2)`, even though the underlying JavaScript
 * arrays are different. The function `linked-list?` returns `#t`
 * when passed a linked list.
 *
 *     > (linked-list? '(1 . ()))
 *     #t
 *     > (linked-list? '(1 . (2 . ())))
 *     #t
 *     > (linked-list? '(1 2 . (3 . ())))
 *     #t
 *
 * However, if the final chain is *not* the empty list, then the
 * sequence is called a *dotted list*. The function `dotted-list?`
 * returns `#t` when passed a dotted list.
 *
 *     > (dotted-list? '(1 . 2))
 *     #t
 *     > (dotted-list? '(1 2 . 3))
 *     #t
 *     > (dotted-list? '(1 . (2 . 3)))
 *     #t
 *     > (dotted-list? '(1 2 . (3 . 4)))
 *     #t
 *
 * Dotted lists are sometimes referred to as "improper lists", to
 * distinguish them from "proper lists", which are `()`-terminated.
 * Thus, array lists and linked lists are proper lists, while dotted
 * lists are improper. The function `list?` returns `#t` when passed
 * a proper list, but not when passed an improper list.
 *
 *     > (list? '())
 *     #t
 *     > (list? '(1))
 *     #t
 *     > (list? '(1 . ()))
 *     #t
 *     > (list? '(1 . (2 . ())))
 *     #t
 *     > (list? '(1 2 . (3 . ())))
 *     #t
 *
 * While array lists are the fastest and are preferable for most
 * tasks, linked lists are more versatile. For example, a linked list
 * may be turned into a *circular list*, i.e., a list that loops back
 * on itself. The function `circular-list?` returns `#t` when passed
 * a circular list.
 *
 *     > (define circ-lst
 *         '(1 . ()))
 *     undefined
 *     > (set-cdr! circ-lst circ-lst)
 *     undefined
 *     > (circular-list? circ-lst)
 *     #t))))
 *
 * It should be appreciated that Lisp's "list" concept is not a type,
 * but a structural property. Whether something is a list or not can
 * be determined only by inspecting the structure itself. One can
 * avoid this check by working exclusively with array lists and using
 * `array-list?` to determine whether something is an array list or
 * not.
 *
 * ### Subtleties
 *
 * Note that the function `array-list?` also returns `#t` when passed
 * a linked list:
 *
 *     > (array-list? '(1 . ()))
 *     #t
 *
 * Roselisp allows for the possibility that you simply wants to treat
 * the list `'(1 . ())` as an array list of three elements, with the
 * cons dot as the second element. If you wish to distinguish linked
 * lists from array lists, you should use `linked-list?`, not
 * `array-list?`. The standard list functions---`cdr`, `nth`,
 * `first`, `second`, `third`, etc.---treat the input as a linked
 * list if `linked-list?` returns `#t`, and as an array list
 * otherwise.
 *
 * The term "dotted list" refers exclusively to improper lists.
 * Therefore, while the linked list `'(1 . ())` does indeed contain
 * a dot, it is not regarded as a dotted list. The improper list
 * `'(1 . 2)`, on the other hand, is regarded as dotted.
 *
 * ### Cons dot
 *
 * TODO: Merge this section into the other text.
 *
 * There is no direct analogue to the cons cell concept in
 * JavaScript. Therefore, Roselisp represents cons cells in terms of
 * *dotted lists*, which are lists where the penultimate element is
 * the *cons dot*, the symbol `.` (i.e., `|.|`, or `Symbol.for('.')`
 * in JavaScript).
 *
 * Thus, in Roselisp, the expression `'(1 . 2)` is the same as
 * `(list 1 '. 2)`, or, in JavaScript, `[1, Symbol.for('.'), 2]`.
 * A cons cell can be considered from two perspectives: either as a
 * pair of two elements, or as a list where the second element is the
 * cons dot.
 *
 * Cons cells are implemented as a *dotted list*, i.e., as a
 * three-element list with a special cons dot symbol as the second
 * element. Thus, the cons cell `(a . b)` is represented as the list
 * `(a <cons-dot> b)`, where `<cons-dot>` is a special value defined
 * in this file. It is just the symbol `.` (also written `|.|`).
 *
 * It is possible to have more than one element before the dot, as in
 * `(a b c . d)`. Thus, dotted lists can be understood as a
 * generalization of cons cells, with a cons cell being a dotted list
 * with two elements.
 *
 * It is also possible to represent circular lists in terms of dotted
 * lists. In Roselisp, a circular list is a dotted list containing
 * itself.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Cons dot value.
 */
declare const consDot_: any;
/**
 * Compiled cons dot value.
 */
declare const consDotCompiled_: any;
/**
 * `cons-dot` function.
 */
declare function consDotF_(): any;
declare namespace consDotF_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether a value is the cons dot.
 */
declare function consDotP_(obj: any): any;
declare namespace consDotP_ {
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[][])[])[])[])[];
}
/**
 * Whether something is a cons cell.
 *
 * Similar to [`cons?` in Racket][rkt:consp] and
 * [`consp` in Common Lisp][cl:consp].
 *
 * [rkt:consp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._cons~3f%29%29
 * [cl:consp]: http://clhs.lisp.se/Body/f_consp.htm
 */
declare function consp_(obj: any): any;
declare namespace consp_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[][])[])[])[])[])[])[];
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
    var lispSource: (symbol | (symbol | (number | symbol)[])[])[];
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
    var lispSource: (symbol | (symbol | (symbol | (symbol | never[])[])[][] | (symbol | symbol[] | (symbol | (number | symbol)[])[][])[])[])[];
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
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
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
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[] | (symbol | symbol[])[][])[])[])[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | symbol[][])[])[][])[])[];
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
    var lispSource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
/**
 * Return the last cdr of a list, i.e., the terminating empty list.
 */
declare function lastCdr_(lst: any): any;
declare namespace lastCdr_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
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
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[])[])[];
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
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[] | (symbol | (number | symbol | symbol[])[])[][])[][] | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | symbol[])[] | (symbol | symbol[] | symbol[][])[][])[])[])[])[];
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
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[])[])[];
}
/**
 * Whether something is an array list.
 *
 * An array list is a list implemented in terms of an array.
 * It corresponds roughly to the
 * [`ArrayList` class in Java][java:ArrayList].
 *
 * [java:ArrayList]: https://docs.oracle.com/javase/8/docs/api/java/util/ArrayList.html
 */
declare function arrayListP_(x: any): any;
declare namespace arrayListP_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the length of an array list.
 */
declare function arrayListLength_(lst: any): any;
declare namespace arrayListLength_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the first element of an array list.
 */
declare function arrayListFirst_(lst: any): any;
declare namespace arrayListFirst_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the second element of an array list.
 */
declare function arrayListSecond_(lst: any): any;
declare namespace arrayListSecond_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the third element of an array list.
 */
declare function arrayListThird_(lst: any): any;
declare namespace arrayListThird_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the fourth element of an array list.
 */
declare function arrayListFourth_(lst: any): any;
declare namespace arrayListFourth_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the fifth element of an array list.
 */
declare function arrayListFifth_(lst: any): any;
declare namespace arrayListFifth_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the sixth element of an array list.
 */
declare function arrayListSixth_(lst: any): any;
declare namespace arrayListSixth_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the seventh element of an array list.
 */
declare function arrayListSeventh_(lst: any): any;
declare namespace arrayListSeventh_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the eighth element of an array list.
 */
declare function arrayListEighth_(lst: any): any;
declare namespace arrayListEighth_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the ninth element of an array list.
 */
declare function arrayListNinth_(lst: any): any;
declare namespace arrayListNinth_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the tenth element of an array list.
 */
declare function arrayListTenth_(lst: any): any;
declare namespace arrayListTenth_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the last element of an array list.
 */
declare function arrayListLast_(lst: any): any;
declare namespace arrayListLast_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the `n`-th element of an array list.
 */
declare function arrayListNth_(n: any, lst: any): any;
declare namespace arrayListNth_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the `n`-th CDR of an array list.
 */
declare function arrayListNthcdr_(n: any, lst: any): any;
declare namespace arrayListNthcdr_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the CDR of an array list.
 */
declare function arrayListCdr_(lst: any): any;
declare namespace arrayListCdr_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the tail of an array list.
 */
declare function arrayListRest_(lst: any): any;
declare namespace arrayListRest_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Take the `n` first elements from an array list.
 */
declare function arrayListTake_(lst: any, n: any): any;
declare namespace arrayListTake_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the list obtained by dropping
 * the first `n` elements from an array list.
 */
declare function arrayListDrop_(lst: any, n: any): any;
declare namespace arrayListDrop_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the list obtained by dropping
 * the last `n` elements from an array list.
 */
declare function arrayListDropRight_(lst: any, n: any): any;
declare namespace arrayListDropRight_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Reverse the order of an array list.
 * Returns a new array list.
 */
declare function arrayListReverse_(lst: any): any;
declare namespace arrayListReverse_ {
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[];
}
/**
 * Whether something is a linked list.
 *
 * An linked list is a list implemented as a chain of links.
 * It corresponds roughly to the
 * [`LinkedList` class in Java][java:LinkedList].
 *
 * [java:LinkedList]: https://docs.oracle.com/javase/8/docs/api/java/util/LinkedList.html
 */
declare function linkedListP_(x: any): any;
declare namespace linkedListP_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[];
}
/**
 * Whether something is a linked list link.
 */
declare function linkedListLinkP_(x: any): any;
declare namespace linkedListLinkP_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[];
}
/**
 * Whether something is a linked pair.
 */
declare function linkedPairP_(x: any): any;
declare namespace linkedPairP_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (number | symbol)[])[])[])[];
}
/**
 * Return the CAR of a linked list link.
 */
declare function linkedListLinkCar_(x: any): any;
declare namespace linkedListLinkCar_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the CDR of a linked list link.
 */
declare function linkedListLinkCdr_(x: any): any;
declare namespace linkedListLinkCdr_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the CAR of a linked pair.
 */
declare function linkedPairCar_(x: any): any;
declare namespace linkedPairCar_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the CDR of a linked pair.
 */
declare function linkedPairCdr_(x: any): any;
declare namespace linkedPairCdr_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the length of a linked list.
 */
declare function linkedListLength_(lst: any): any;
declare namespace linkedListLength_ {
    var lispSource: (symbol | (number | symbol)[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[];
}
/**
 * Return the first element of a linked list.
 */
declare function linkedListFirst_(lst: any): any;
declare namespace linkedListFirst_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the second element of a linked list.
 */
declare function linkedListSecond_(lst: any): any;
declare namespace linkedListSecond_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the third element of a linked list.
 */
declare function linkedListThird_(lst: any): any;
declare namespace linkedListThird_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fourth element of a linked list.
 */
declare function linkedListFourth_(lst: any): any;
declare namespace linkedListFourth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the fifth element of a linked list.
 */
declare function linkedListFifth_(lst: any): any;
declare namespace linkedListFifth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the sixth element of a linked list.
 */
declare function linkedListSixth_(lst: any): any;
declare namespace linkedListSixth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the seventh element of a linked list.
 */
declare function linkedListSeventh_(lst: any): any;
declare namespace linkedListSeventh_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the eighth element of a linked list.
 */
declare function linkedListEighth_(lst: any): any;
declare namespace linkedListEighth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the ninth element of a linkedd list.
 */
declare function linkedListNinth_(lst: any): any;
declare namespace linkedListNinth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the tenth element of a linkedd list.
 */
declare function linkedListTenth_(lst: any): any;
declare namespace linkedListTenth_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the last element of a linked list.
 */
declare function linkedListLast_(lst: any): any;
declare namespace linkedListLast_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[];
}
/**
 * Return the `n`-th element of a linked list.
 */
declare function linkedListNth_(n: any, lst: any): any;
declare namespace linkedListNth_ {
    var lispSource: (symbol | (symbol | (number | symbol)[] | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Return the `n`-th CDR of a linked list.
 */
declare function linkedListNthcdr_(n: any, lst: any): any;
declare namespace linkedListNthcdr_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[])[])[];
}
/**
 * Return the list obtained by dropping
 * the first `n` elements from a linked list.
 */
declare function linkedListDrop_(lst: any, pos: any): any;
declare namespace linkedListDrop_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the list obtained by dropping
 * the last `n` elements from a linked list.
 */
declare function linkedListDropRight_(lst: any, n: any): any;
declare namespace linkedListDropRight_ {
    var lispSource: (symbol | (symbol | (number | symbol)[])[])[];
}
/**
 * Return the CAR of a linked list.
 */
declare function linkedListCar_(lst: any): any;
declare namespace linkedListCar_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the CDR of a linked list.
 */
declare function linkedListCdr_(lst: any): any;
declare namespace linkedListCdr_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Return the head of a linked list.
 */
declare function linkedListHead_(lst: any): any;
declare namespace linkedListHead_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Return the tail of a linked list.
 */
declare function linkedListTail_(lst: any): any;
declare namespace linkedListTail_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Parse a linked list.
 */
declare function linkedListParse_(lst: any): any;
declare namespace linkedListParse_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Make a linked list.
 */
declare function makeDottedList_(car: any, cdr: any): any;
declare namespace makeDottedList_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Make a linked pair.
 */
declare function makePair_(car: any, cdr: any): any;
declare namespace makePair_ {
    var lispSource: (symbol | (symbol | symbol[][])[])[];
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
    var lispSource: (symbol | (symbol | symbol[])[])[];
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
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
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
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Convert an array list to a linked list.
 */
declare function arrayListToLinkedList_(x: any): any;
declare namespace arrayListToLinkedList_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol)[])[][])[])[];
}
/**
 * Convert a linked list to an array list.
 */
declare function linkedListToArrayList_(x: any): any;
declare namespace linkedListToArrayList_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[][])[])[];
}
export { append_ as append, arrayListToLinkedList_, arrayListToLinkedList_ as properListToDottedList_, butlast_ as butlast, cdr_ as cdr, cdr_ as tail, cdr_ as tail_, consp_ as pairp, consp_ as pairp_, dottedListP_ as dottedListP, dropRight_ as dropRight, drop_ as drop, drop_ as listTail, drop_ as listTail_, eighth_ as eighth, fifth_ as fifth, first_ as car, first_ as car_, first_ as first, first_ as head, flatten_ as flatten, fourth_ as fourth, lastCdr_ as linkedListLastCdr_, lastPair_ as lastCons_, lastPair_ as linkedListLastCons_, lastPair_ as linkedListLastPair_, last_ as last, length_ as length, linkedListToArrayList_, linkedListLast_, linkedPairP_ as dottedPairP_, listStar_ as listStar, listp_ as listp, listp_, listp_ as properListP, list_ as list, makeList_ as makeList, nbutlast_ as nbutlast, ninth_ as ninth, nth_ as nth, nthcdr_ as nthcdr, nullp_ as nullp, nullp_, popLeftX_ as popX, popLeftX_ as popX_, popLeftX_ as pop, popLeftX_ as popLeftX, popLeftX_ as popLeft, popLeftX_ as popLeft_, popLeftX_ as pop_, popRightX_ as popRightX, popRightX_ as popRight, popRightX_ as popRight_, pushLeftX_ as pushX, pushLeftX_ as pushX_, pushLeftX_ as push, pushLeftX_ as pushLeftX, pushLeftX_ as pushLeft, pushLeftX_ as pushLeft_, pushLeftX_ as push_, pushRightX_ as appendToList, pushRightX_ as pushRightX, pushRightX_ as pushRight, pushRightX_ as pushRight_, rest_ as rest, reverse_ as reverse, second_ as cadr, second_ as cadr_, second_ as second, setCarX_ as setCar_, setCdrX_ as setCdr_, seventh_ as seventh, sixth_ as sixth, tenth_ as tenth, third_ as third, append_, arrayListCdr_, arrayListDropRight_, arrayListDrop_, arrayListEighth_, arrayListFifth_, arrayListFirst_, arrayListFourth_, arrayListLast_, arrayListLength_, arrayListNinth_, arrayListNth_, arrayListNthcdr_, arrayListRest_, arrayListReverse_, arrayListSecond_, arrayListSeventh_, arrayListSixth_, arrayListTake_, arrayListTenth_, arrayListThird_, arrayListP_, buildList_, butlast_, cdr_, circularListP_, consDotCompiled_, consDotF_, consDotP_, consDot_, consp_, cons_, dottedListP_, dropRight_, drop_, eighth_, fifth_, first_, flatten_, fourth_, improperListP_, lastCdr_, lastPair_, last_, length_, linkedListCar_, linkedListCdr_, linkedListDropRight_, linkedListDrop_, linkedListEighth_, linkedListFifth_, linkedListFirst_, linkedListFourth_, linkedListHead_, linkedListLength_, linkedListLinkCar_, linkedListLinkCdr_, linkedListLinkP_, linkedListNinth_, linkedListNth_, linkedListNthcdr_, linkedListParse_, linkedListSecond_, linkedListSeventh_, linkedListSixth_, linkedListTail_, linkedListTenth_, linkedListThird_, linkedListP_, linkedPairCar_, linkedPairCdr_, linkedPairP_, listStar_, list_, makeDottedList_, makeList_, makePair_, nbutlast_, ninth_, nth_, nthcdr_, popLeftX_, popRightX_, properListP_, pushLeftX_, pushRightX_, rest_, reverse_, second_, setCarX_, setCdrX_, seventh_, sixth_, take_, tenth_, third_ };
