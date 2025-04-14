/**
 * # Procedures
 *
 * Various procedures.
 *
 * ## Description
 *
 * This file defines various procedures that are part of the language
 * environment.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Call `f` with `args`, using the last arg as a list of args.
 * Returns the value `f` returns.
 *
 * Similar to [`apply` in Racket][rkt:apply] and
 * [`apply` in Common Lisp][cl:apply].
 *
 * [rkt:apply]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._apply%29%29
 * [cl:apply]: http://clhs.lisp.se/Body/f_apply.htm#apply
 */
declare function apply_(f: any, ...args: any[]): any;
declare namespace apply_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (number | symbol)[])[])[])[])[];
}
/**
 * Call `f` with `args`.
 * Returns the value `f` returns.
 *
 * Similar to [`funcall` in Common Lisp][cl:funcall].
 *
 * [cl:funcall]: http://clhs.lisp.se/Body/f_funcal.htm#funcall
 */
declare function funcall_(f: any, ...args: any[]): any;
declare namespace funcall_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether `obj` is a procedure (i.e., a function).
 *
 * Similar to [`procedure?` in Racket][rkt:procedurep] and
 * [`functionp` in Common Lisp][cl:functionp].
 *
 * [rkt:procedurep]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28quote._~23~25kernel%29._procedure~3f%29%29
 * [cl:functionp]: http://clhs.lisp.se/Body/f_fnp.htm#functionp
 */
declare function procedurep_(obj: any): any;
declare namespace procedurep_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether `obj` is a fexpr, that is, a procedure that
 * does not evaluate its arguments.
 */
declare function fexprp_(obj: any): any;
declare namespace fexprp_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Logical negation.
 *
 * Similar to [`not` in Racket][rkt:not] and
 * [`not` in Common Lisp][cl:not].
 *
 * [rkt:not]: https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._not%29%29
 * [cl:not]: http://clhs.lisp.se/Body/f_not.htm
 */
declare function not_(x: any): any;
declare namespace not_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Map a procedure over a list.
 *
 * Similar to [`map` in Racket][rkt:map] and
 * [`mapcar` in Common Lisp][cl:mapcar].
 *
 * [rkt:map]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29
 * [cl:mapcar]: http://clhs.lisp.se/Body/f_mapc_.htm#mapcar
 */
declare function map_(f: any, seq: any): any;
declare namespace map_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Less than operator.
 *
 * Similar to [`<` in Racket][rkt:lt] and [`<` in Common Lisp][cl:lt].
 *
 * [rkt:lt]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3c%29%29
 * [cl:lt]: http://clhs.lisp.se/Body/f_eq_sle.htm#LT
 */
declare function lt_(...args: any[]): any;
declare namespace lt_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[])[])[];
}
/**
 * Less than or equal operator.
 *
 * Similar to [`<=` in Racket][rkt:lte] and [`<=` in Common Lisp][cl:lte].
 *
 * [rkt:lte]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3c~3d%29%29
 * [cl:lte]: http://clhs.lisp.se/Body/f_eq_sle.htm#LTEQ
 */
declare function lte_(...args: any[]): any;
declare namespace lte_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[])[])[];
}
/**
 * Greater than operator.
 *
 * Similar to [`>` in Racket][rkt:gt] and [`>` in Common Lisp][cl:gt].
 *
 * [rkt:gt]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3e%29%29
 * [cl:gt]: http://clhs.lisp.se/Body/f_eq_sle.htm#GT
 */
declare function gt_(...args: any[]): any;
declare namespace gt_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[])[])[];
}
/**
 * Greater than or equal operator.
 *
 * Similar to [`>=` in Racket][rkt:gte] and [`>=` in Common Lisp][cl:gte].
 *
 * [rkt:gte]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3e~3d%29%29
 * [cl:gte]: http://clhs.lisp.se/Body/f_eq_sle.htm#GTEQ
 */
declare function gte_(...args: any[]): any;
declare namespace gte_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[])[])[];
}
/**
 * Modulo operation.
 *
 * Similar to [`modulo` in Racket][rkt:modulo] and
 * [`mod` in Common Lisp][cl:mod].
 *
 * [rkt:modulo]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._modulo%29%29
 * [cl:mod]: http://clhs.lisp.se/Body/f_mod_r.htm
 */
declare function modulo_(x: any, y: any): any;
declare namespace modulo_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Addition.
 *
 * Similar to [`+` in Racket][rkt:add] and [`+` in Common Lisp][cl:add].
 *
 * [rkt:add]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2B%29%29
 * [cl:add]: http://clhs.lisp.se/Body/f_pl.htm
 */
declare function add_(...args: any[]): any;
declare namespace add_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (number | symbol)[][])[])[];
}
/**
 * Return `(+ x 1)`.
 *
 * Similar to [`add1` in Racket][rkt:add1].
 *
 * [rkt:add1]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._add1%29%29
 */
declare function add1_(x: any): any;
declare namespace add1_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Subtraction.
 *
 * Similar to [`-` in Racket][rkt:sub] and [`-` in Common Lisp][cl:sub].
 *
 * [rkt:sub]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._-%29%29
 * [cl:sub]: http://clhs.lisp.se/Body/f__.htm
 */
declare function sub_(...args: any[]): any;
declare namespace sub_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (number | symbol)[])[][])[])[])[] | (number | symbol[])[])[])[])[];
}
/**
 * Return `(- x 1)`.
 *
 * Similar to [`sub1` in Racket][rkt:sub1].
 *
 * [rkt:sub1]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._sub1%29%29
 */
declare function sub1_(x: any): any;
declare namespace sub1_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Multiplication.
 *
 * Similar to [`*` in Racket][rkt:mul] and [`*` in Common Lisp][cl:mul].
 *
 * [rkt:mul]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2A%29%29
 * [cl:mul]: http://clhs.lisp.se/Body/f_st.htm
 */
declare function mul_(...args: any[]): any;
declare namespace mul_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (number | symbol)[][])[])[];
}
/**
 * Division.
 *
 * Similar to [`/` in Racket][rkt:div] and [`/` in Common Lisp][cl:div].
 *
 * [rkt:div]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2F%29%29
 * [cl:div]: http://clhs.lisp.se/Body/f_sl.htm
 */
declare function div_(...args: any[]): any;
declare namespace div_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[][] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[])[])[])[];
}
/**
 * Whether a value is the number zero.
 *
 * Similar to [`zerop` in Racket][rkt:zerop] and
 * [`zerop` in Common Lisp][cl:zerop].
 *
 * [rkt:zerop]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._zero~3f%29%29
 * [cl:zerop]: http://clhs.lisp.se/Body/f_zerop.htm#zerop
 */
declare function zerop_(n: any): any;
declare namespace zerop_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Whether a value is the number one.
 */
declare function onep_(n: any): any;
declare namespace onep_ {
    var lispSource: (symbol | (number | symbol)[])[];
}
/**
 * Whether a number is odd.
 *
 * Similar to [`odd?` in Racket][rkt:oddp] and
 * [`oddp` in Common Lisp][rkt:oddp].
 *
 * [rkt:oddp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._odd~3f%29%29
 * [cl:oddp]: http://clhs.lisp.se/Body/f_evenpc.htm#oddp
 */
declare function oddp_(n: any): any;
declare namespace oddp_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Whether a number is even.
 *
 * Similar to [`even?` in Racket][rkt:evenp] and
 * [`evenp` in Common Lisp][rkt:evenp].
 *
 * [rkt:oddp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._even~3f%29%29
 * [cl:oddp]: http://clhs.lisp.se/Body/f_evenpc.htm#evenp
 */
declare function evenp_(n: any): any;
declare namespace evenp_ {
    var lispSource: (symbol | (symbol | (number | symbol)[])[])[];
}
/**
 * Whether a value is truthy.
 */
declare function truep(x: any): any;
declare namespace truep {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol | (symbol | symbol[])[])[])[])[])[])[];
}
/**
 * Whether a value is falsy.
 */
declare function falsep(x: any): any;
declare namespace falsep {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * The identity function.
 *
 * Similar to [`identity` in Racket][rkt:identity] and
 * [`identity` in Common Lisp][cl:identity].
 *
 * [rkt:identity]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._identity%29%29
 * [cl:identity]: http://clhs.lisp.se/Body/f_identi.htm#identity
 */
declare function identity_(x: any): any;
declare namespace identity_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Returns a procedure that accepts any arguments and returns `x`.
 *
 * Similar to [`const` in Racket][rkt:const] and
 * [`constantly` in Common Lisp][cl:constantly].
 *
 * [rkt:const]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._const%29%29
 * [cl:constantly]: http://clhs.lisp.se/Body/f_cons_1.htm#constantly
 */
declare function const_(x?: any): any;
declare namespace const_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return a tuple of multiple values.
 *
 * Similar to [`values` in Racket][rkt:values] and
 * [`values` in Common Lisp][cl:values].
 *
 * [rkt:values]: https://docs.racket-lang.org/reference/values.html#%28def._%28%28quote._~23~25kernel%29._values%29%29
 * [cl:values]: http://clhs.lisp.se/Body/f_values.htm
 */
declare function values_(...args: any[]): any;
declare namespace values_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether something is a keyword, i.e., a symbol
 * whose first character is `:`.
 *
 * Similar to [`keyword?` in Racket][rkt:keywordp] and
 * [`keywordp` in Common Lisp][cl:keywordp].
 *
 * [rkt:keywordp]: https://docs.racket-lang.org/reference/keywords.html#%28def._%28%28quote._~23~25kernel%29._keyword~3f%29%29
 * [cl:keywordp]: http://clhs.lisp.se/Body/f_kwdp.htm#keywordp
 */
declare function keywordp_(obj: any): any;
declare namespace keywordp_ {
    var lispSource: (symbol | (symbol | (symbol | (string | symbol)[])[])[])[];
}
/**
 * Whether something is a number.
 *
 * Similar to [`number?` in Racket][rkt:numberp] and
 * [`numberp` in Common Lisp][cl:numberp].
 *
 * [rkt:numberp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._number~3f%29%29
 * [cl:numberp]: http://clhs.lisp.se/Body/f_nump.htm#numberp
 */
declare function numberp_(obj: any): any;
declare namespace numberp_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether something is a boolean value.
 *
 * Similar to [`boolean?` in Racket][rkt:booleanp] and
 * [`booleanp` in Emacs Lisp][el:booleanp].
 *
 * [rkt:booleanp]: https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._boolean~3f%29%29
 * [el:booleanp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/nil-and-t.html#index-booleanp
 */
declare function booleanp_(obj: any): any;
declare namespace booleanp_ {
    var lispSource: (symbol | (string | symbol | symbol[])[])[];
}
/**
 * Whether something is the value `undefined`.
 */
declare function undefinedp_(obj: any): any;
declare namespace undefinedp_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Fold up a list left to right.
 *
 * Similar to [`foldl` in Racket][rkt:foldl] and
 * [`reduce` in Common Lisp][cl:reduce].
 *
 * [rkt:foldl]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldl%29%29
 * [cl:reduce]: http://clhs.lisp.se/Body/f_reduce.htm#reduce
 */
declare function foldl_(f: any, v: any, lst: any): any;
declare namespace foldl_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Fold up a list right to left.
 *
 * Similar to [`foldr` in Racket][rkt:foldr] and
 * [`reduce` in Common Lisp][cl:reduce].
 *
 * [rkt:foldr]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldr%29%29
 * [cl:reduce]: http://clhs.lisp.se/Body/f_reduce.htm#reduce
 */
declare function foldr_(f: any, v: any, lst: any): any;
declare namespace foldr_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether a list contains a value.
 * Returns a sublist if found, otherwise `#f`.
 * By default, comparison is done with `equal?`.
 *
 * Similar to [`member` in Racket][rkt:member] and
 * [`member` in Common Lisp][cl:member].
 *
 * [rkt:member]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._member%29%29
 * [cl:member]: http://clhs.lisp.se/Body/f_mem_m.htm
 */
declare function member_(v: any, lst: any, isEqual?: any): any;
declare namespace member_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[][])[])[];
}
/**
 * Whether a list contains a value.
 * Like `member`, but always returns a boolean value.
 */
declare function memberp_(v: any, lst: any, isEqual?: any): any;
declare namespace memberp_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Whether a list contains a value.
 * Like `member`, but comparison is done with `eq?`.
 *
 * Similar to [`meq` in Racket][rkt:memq] and
 * [`memq` in Emacs Lisp][el:memq].
 *
 * [rkt:memq]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._memq%29%29
 * [el:memq]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Sets-And-Lists.html#index-memq
 */
declare function memq_(v: any, lst: any): any;
declare namespace memq_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[][])[])[];
}
/**
 * Whether a list contains a value,
 * using `eq?` for comparisons. Like `memq`,
 * but always returns a boolean value.
 */
declare function memqp_(v: any, lst: any): any;
declare namespace memqp_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether a list contains a value matching a predicate.
 * Applies the predicate `proc` to elements in the list
 * and returns a sublist if found, otherwise `#f`.
 *
 * Similar to [`memf` in Racket][rkt:memf] and
 * [`member-if` in Common Lisp][cl:member-if].
 *
 * [rkt:memf]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._memf%29%29
 * [cl:member-if]: http://clhs.lisp.se/Body/f_mem_m.htm#member-if
 */
declare function memf_(proc: any, lst: any, notFound?: any): any;
declare namespace memf_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | symbol[])[][])[])[];
}
/**
 * Whether a list contains a value matching a predicate.
 * Like `memf`, but always returns a boolean value.
 */
declare function memfp_(proc: any, lst: any): any;
declare namespace memfp_ {
    var lispSource: (symbol | (number | symbol | symbol[])[])[];
}
/**
 * Find a list element matching a predicate.
 *
 * Similar to [`findf` in Racket][rkt:findf] and
 * [`find-if` in Common Lisp][cl:find-if].
 *
 * [rkt:findf]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._findf%29%29
 * [cl:find-if]: http://clhs.lisp.se/Body/f_find_.htm#find-if
 */
declare function findf_(proc: any, lst: any, notFound?: any): any;
declare namespace findf_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | symbol[])[][])[])[];
}
/**
 * Find the index of a list element matching a predicate.
 *
 * Similar to [`index-where` in Racket][rkt:index-where] and
 * [`position-if` in Common Lisp][cl:position-if].
 *
 * [rkt:index-where]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-where%29%29
 * [cl:position-if]: http://clhs.lisp.se/Body/f_pos_p.htm#position-if
 */
declare function findfIndex_(proc: any, seq: any, notFound?: any): any;
declare namespace findfIndex_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | symbol[])[][])[])[];
}
/**
 * Find the index of a list element matching a predicate.
 *
 * Similar to [`index-where` in Racket][rkt:index-where] and
 * [`position-if` in Common Lisp][cl:position-if].
 *
 * [rkt:index-where]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-where%29%29
 * [cl:position-if]: http://clhs.lisp.se/Body/f_pos_p.htm#position-if
 */
declare function indexWhere_(seq: any, proc: any, notFound?: any): any;
declare namespace indexWhere_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | symbol[])[][])[])[];
}
/**
 * Find the index of a list element.
 *
 * Similar to [`index-of` in Racket][rkt:index-of] and
 * [`position` in Common Lisp][cl:position].
 *
 * [rkt:index-of]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-of%29%29
 * [cl:position]: http://clhs.lisp.se/Body/f_pos_p.htm#position
 */
declare function indexOf_(seq: any, v: any, isEqual?: any): any;
declare namespace indexOf_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Return the intersection of multiple lists.
 *
 * Similar to [`intersection` in Common Lisp][cl:intersection].
 *
 * [cl:intersection]: http://clhs.lisp.se/Body/f_isec_.htm#intersection
 */
declare function intersection_(...args: any[]): any;
declare namespace intersection_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[][] | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
/**
 * Return the union of multiple lists.
 *
 * Similar to [`union` in Common Lisp][cl:union].
 *
 * [cl:union]: http://clhs.lisp.se/Body/f_unionc.htm#union
 */
declare function union_(...args: any[]): any;
declare namespace union_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | never[])[])[][])[])[])[];
}
/**
 * Produce a list of numbers from `start`, inclusive,
 * to `end`, exclusive.
 *
 * Similar to [`range` in Racket][rkt:range].
 *
 * [rkt:range]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._range%29%29
 */
declare function range_(start: any, end?: any, step?: any): any;
declare namespace range_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | symbol[] | (symbol | symbol[])[][])[])[])[];
}
/**
 * Right-to-left function composition.
 *
 * Similar to [`compose` in Racket][rkt:compose].
 *
 * [rkt:compose]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._compose%29%29
 */
declare function compose_(...args: any[]): any;
declare namespace compose_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (number | symbol)[])[][])[])[];
}
/**
 * Left-to-right function composition.
 *
 * Like `compose`, but in the other direction.
 */
declare function pipe_(...args: any[]): any;
declare namespace pipe_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[];
}
/**
 * Filter a list by a predicate.
 *
 * Similar to [`filter` in Racket][rkt:filter] and
 * [`remove-if-not` in Common Lisp][cl:remove-if-not].
 *
 * [rkt:filter]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29
 * [cl:remove-if-not]: http://clhs.lisp.se/Body/f_rm_rm.htm#remove-if-not
 */
declare function filter_(pred: any, lst: any): any;
declare namespace filter_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether a value is self-evaluating.
 */
declare function selfEvaluatingP_(x: any): any;
declare namespace selfEvaluatingP_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Write an error message to the console
 * if the assertion is false.
 *
 * Similar to [`assert` in Clojure][clj:assert]
 * and [`assert` in Racket][rkt:assert].
 *
 * [clj:assert]: https://clojuredocs.org/clojure.core/assert
 * [rkt:assert]: https://docs.racket-lang.org/ts-reference/Utilities.html#(def._((lib._typed-racket/base-env/extra-procs..rkt)._assert))
 */
declare function assert_(x: any, ...args: any[]): any;
declare namespace assert_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Output a message to the console.
 *
 * Similar to [`display` in Racket][rkt:display] and
 * [`print` in Common Lisp][cl:print].
 *
 * [rkt:display]: https://docs.racket-lang.org/reference/Writing.html#%28def._%28%28quote._~23~25kernel%29._display%29%29
 * [cl:print]: http://clhs.lisp.se/Body/f_wr_pr.htm#print
 */
declare function display_(...args: any[]): any;
declare namespace display_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Throw an error.
 *
 * Similar to [`error` in Racket][rkt:error] and
 * [`error` in Common Lisp][cl:error].
 *
 * [rkt:error]: https://docs.racket-lang.org/reference/exns.html#%28def._%28%28quote._~23~25kernel%29._error%29%29
 * [cl:error]: http://clhs.lisp.se/Body/f_error.htm
 */
declare function error_(arg?: any): any;
declare namespace error_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Get the type of a value.
 *
 * Similar to [`type-of` in Common Lisp][cl:type-of].
 *
 * [cl:type-of]: http://clhs.lisp.se/Body/f_tp_of.htm#type-of
 */
declare function typeOf_(x: any): any;
declare namespace typeOf_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether `obj` is an instance of `cls`.
 *
 * Similar to [`is-a?` in Racket][rkt:is-a-p].
 *
 * [rkt:is-a-p]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._is-a~3f%29%29
 */
declare function isAP_(obj: any, cls: any): any;
declare namespace isAP_ {
    var lispSource: (symbol | symbol[])[];
}
export { add1_ as add1, add_ as _add, add_ as add, add_ as plus, apply_ as apply, compose_ as compose, display_ as display, div_ as _div, div_ as div, error_ as error, falsep as falsep_, fexprp_ as fexprp, findfIndex_ as findfIndex, findf_ as findf, foldl_ as foldl, foldr_ as foldr, funcall_ as funcall, gt_ as gt, gte_ as gte, intersection_ as intersection, isAP_ as instanceOf, isAP_ as instanceOfP, isAP_ as instanceOfP_, isAP_ as instanceOf_, isAP_ as instanceofp, isAP_ as isAP, keywordp_ as keywordp, lt_ as lt, lte_ as lte, map_ as map, map_ as mapcar, memberp_ as memberP, memberp_ as memberP_, memberp_ as memberp, memberp_, member_ as member, member_ as memq, memf_ as memf, mul_ as _mul, mul_ as mul, not_ as not, numberp_ as numberp, pipe_ as pipe, procedurep_ as functionp, procedurep_ as procedurep, range_ as range, sub1_ as sub1, sub_ as _sub, sub_ as minus, sub_ as sub, sub_ as subtract, truep as truep_, typeOf_ as typeOf, union_ as union, values_ as values, zerop_ as zerop, add1_, add_, apply_, assert_, booleanp_, compose_, const_, display_, div_, error_, evenp_, falsep, fexprp_, filter_, findfIndex_, findf_, foldl_, foldr_, funcall_, gt_, gte_, identity_, indexOf_, indexWhere_, intersection_, isAP_, keywordp_, lt_, lte_, map_, member_, memfp_, memf_, memqp_, memq_, modulo_, mul_, not_, numberp_, oddp_, onep_, pipe_, procedurep_, range_, selfEvaluatingP_, sub1_, sub_, truep, typeOf_, undefinedp_, union_, values_, zerop_ };
