/**
 * # Macros
 *
 * Macro definitions.
 *
 * ## Description
 *
 * This file provides macro implementations of some special forms.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Expand a `(defun ...)` expression.
 */
declare function defun_(exp: any, env: any): any;
declare namespace defun_ {
    var fsource: (symbol | (symbol | (symbol | symbol[] | symbol[][])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(define/private ...)` expression.
 */
declare function definePrivate_(exp: any, env: any): any;
declare namespace definePrivate_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(define/public ...)` expression.
 */
declare function definePublic_(exp: any, env: any): any;
declare namespace definePublic_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(defclass ...)` expression.
 */
declare function defclass_(exp: any, env: any): any;
declare namespace defclass_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(define-macro ...)` expression.
 *
 * Similar to [`define-macro` in Guile][guile:define-macro] and
 * [`defmacro` in Common Lisp][cl:defmacro].
 *
 * [guile:define-macro]: https://www.gnu.org/software/guile/docs/docs-2.2/guile-ref/Defmacros.html
 * [cl:defmacro]: http://clhs.lisp.se/Body/m_defmac.htm#defmacro
 */
declare function defineMacro_(exp: any, env: any): any;
declare namespace defineMacro_ {
    var fsource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Create a macro function on the basis of a
 * `(define-macro ...)` expression.
 */
declare function defineMacroToFunction(exp: any, env: any): any;
declare namespace defineMacroToFunction {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Create a `(lambda ...)` form for a macro function
 * on the basis of a `(define-macro ...)` expression.
 */
declare function defineMacroToLambdaForm(exp: any): any;
declare namespace defineMacroToLambdaForm {
    var fsource: (symbol | (symbol | (number | symbol)[])[] | (symbol | undefined)[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | symbol[])[] | ((number | symbol)[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | ((symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[][])[])[])[])[])[])[];
}
/**
 * Expand a `(defmacro ...)` expression.
 */
declare function defmacro_(exp: any, env: any): any;
declare namespace defmacro_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(define-fexpr ...)` expression.
 */
declare function defineFexpr_(exp: any, env: any): any;
declare namespace defineFexpr_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(declare ...)` expression.
 */
declare function declare_(exp: any, env: any): any;
declare namespace declare_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(declare-macro ...)` expression.
 */
declare function declareMacro_(exp: any, env: any): any;
declare namespace declareMacro_ {
    var fsource: (symbol | (symbol | (symbol | (string | symbol)[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(declare-fexpr ...)` expression.
 */
declare function declareFexpr_(exp: any, env: any): any;
declare namespace declareFexpr_ {
    var fsource: (symbol | (symbol | (symbol | (string | symbol)[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(begin0 ...)` or `(prog1 ...)` expression.
 */
declare function begin0_(exp: any, env: any): any;
declare namespace begin0_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | symbol[] | symbol[][][])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(multiple-values-bind ...)` expression.
 */
declare function multipleValueBind_(exp: any, env: any): any;
declare namespace multipleValueBind_ {
    var fsource: (symbol | (symbol | (symbol | symbol[] | symbol[][][])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(rkt/new ...)' expression.
 */
declare function rktNew_(exp: any, env: any): any;
declare namespace rktNew_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand an `(if ...)` expression.
 */
declare function if_(exp: any, env: any): any;
declare namespace if_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | never[])[])[] | ((number | symbol | symbol[])[] | (symbol | (symbol | symbol[])[][])[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(when ...)` expression.
 */
declare function when_(exp: any, env: any): any;
declare namespace when_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand an `(unless ...)` expression.
 */
declare function unless_(exp: any, env: any): any;
declare namespace unless_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand an `(as~> ...)` expression.
 *
 * Similar to the [`as->` macro][clj:thread-as] in Clojure.
 *
 * [clj:thread-as]: https://clojuredocs.org/clojure.core/as-%3E
 */
declare function threadAs_(exp: any, env: any): any;
declare namespace threadAs_ {
    var fsource: (symbol | (boolean | symbol)[] | (symbol | (symbol | (symbol | (symbol | ((number | symbol)[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (boolean | symbol)[] | (symbol | (symbol | (symbol | symbol[])[] | symbol[][][])[])[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Evaluate a `(~> ...)` expression. Based on the
 * [`->` macro][clj:thread-first] in Clojure (also known as
 * the "`thread-first` macro").
 *
 * [clj:thread-first]: https://clojuredocs.org/clojure.core/-%3E
 */
declare function threadFirst_(exp: any, env: any): any;
declare namespace threadFirst_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[] | ((number | symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (symbol[] | (symbol | symbol[])[][])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Evaluate a `(~>> ...)` expression. Based on the
 * [`->>` macro][clj:thread-last] in Clojure (also known as
 * the "`thread-last` macro").
 *
 * [clj:thread-last]: https://clojuredocs.org/clojure.core/-%3E%3E
 */
declare function threadLast_(exp: any, env: any): any;
declare namespace threadLast_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[] | (number | symbol | (symbol | (symbol | symbol[])[])[])[][])[])[])[];
    var ftype: string;
}
/**
 * Expand an `(unwind-protect ...)` expression.
 */
declare function unwindProtect_(exp: any, env: any): any;
declare namespace unwindProtect_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(do ...)` expression.
 */
declare function do_(exp: any, env: any): any;
declare namespace do_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(while ...)` expression.
 */
declare function while_(exp: any, env: any): any;
declare namespace while_ {
    var fsource: (symbol | (symbol | (symbol | symbol[] | (symbol | symbol[])[][])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(js/for ...)` expression.
 */
declare function jsFor_(exp: any, env: any): any;
declare namespace jsFor_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | symbol[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(js/for-in ...)` expression.
 */
declare function jsForIn_(exp: any, env: any): any;
declare namespace jsForIn_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(js/for-of ...)` expression.
 */
declare function jsForOf_(exp: any, env: any): any;
declare namespace jsForOf_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(case ...)` expression.
 */
declare function case_(exp: any, env: any): any;
declare namespace case_ {
    var fsource: (symbol | (boolean | symbol)[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (boolean | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[][])[])[])[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(case/eq ...)` expression.
 */
declare function caseEq_(exp: any, env: any): any;
declare namespace caseEq_ {
    var fsource: (symbol | (boolean | symbol)[] | (symbol | (symbol | (boolean | symbol)[] | (symbol | (number | symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[][])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(let-env ...)` expression.
 */
declare function letEnv_(exp: any, env: any): any;
declare namespace letEnv_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(set ...)` expression.
 */
declare function set_(exp: any, env: any): any;
declare namespace set_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(new/apply ...)` expression.
 */
declare function newApply_(exp: any, env: any): any;
declare namespace newApply_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(clj/try ...)` expression.
 *
 * Similar to the [`try` special form][clj:try] in Clojure.
 *
 * [clj:try]: https://clojuredocs.org/clojure.core/try
 */
declare function cljTry_(exp: any, env: any): any;
declare namespace cljTry_ {
    var fsource: (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | ((symbol | (number | symbol | symbol[])[] | (symbol | (symbol | symbol[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | ((symbol | (number | symbol)[])[] | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[])[])[])[])[])[];
    var ftype: string;
}
export { begin0_, caseEq_, case_, cljTry_, declareFexpr_, declareMacro_, declare_, defclass_, defineFexpr_, defineMacroToFunction, defineMacroToLambdaForm, defineMacro_, definePrivate_, definePublic_, defmacro_, defun_, do_, if_, jsForIn_, jsForOf_, jsFor_, letEnv_, multipleValueBind_, newApply_, rktNew_, set_, threadAs_, threadFirst_, threadLast_, unless_, unwindProtect_, when_, while_ };
