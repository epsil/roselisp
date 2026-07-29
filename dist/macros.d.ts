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
 *
 * Similar to [`defun` in Common Lisp][cl:defun] and
 * [`defun` in Emacs Lisp][el:defun].
 *
 * [cl:defun]: http://clhs.lisp.se/Body/m_defun.htm
 * [el:defun]: https://www.gnu.org/software/emacs/manual/html_node/eintr/defun.html
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
 *
 * Similar to [`defclass` in Common Lisp][cl:defclass].
 *
 * [cl:defclass]: http://clhs.lisp.se/Body/m_defcla.htm
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
 *
 * Similar to [`defmacro` in Common Lisp][cl:defmacro]
 * and [`defmacro` in Emacs Lisp][el:defmacro].
 *
 * [cl:defmacro]: http://clhs.lisp.se/Body/m_defmac.htm
 * [el:defmacro]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Macros.html#index-defmacro
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
 *
 * Similar to [`declare` in Common Lisp] and
 * [`declare` in Emacs Lisp][el:declare].
 *
 * [cl:declare]: http://clhs.lisp.se/Body/s_declar.htm#declare
 * [el:declare]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Declare-Form.html
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
 * Expand a `(begin0 ...)` expression.
 *
 * Similar to [`begin0` in Racket] and
 * [`prog1` in Common Lisp][cl:prog1].
 *
 * [rkt:begin0]: https://docs.racket-lang.org/reference/begin.html#%28form._%28%28quote._~23~25kernel%29._begin0%29%29
 * [cl:prog1]: http://clhs.lisp.se/Body/m_prog1c.htm
 */
declare function begin0_(exp: any, env: any): any;
declare namespace begin0_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | symbol[] | symbol[][][])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(multiple-value-bind ...)` expression.
 *
 * Similar to [`multiple-value-bind` in
 * Common Lisp][cl:multiple-value-bind].
 *
 * [cl:multiple-value-bind]: http://clhs.lisp.se/Body/m_multip.htm
 */
declare function multipleValueBind_(exp: any, env: any): any;
declare namespace multipleValueBind_ {
    var fsource: (symbol | (symbol | (symbol | symbol[] | symbol[][][])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(rkt/new ...)' expression.
 *
 * Similar to [`new` in Racket][rkt:new].
 *
 * [rkt:new]: https://docs.racket-lang.org/reference/objcreation.html#%28form._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._new%29%29
 */
declare function rktNew_(exp: any, env: any): any;
declare namespace rktNew_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(when ...)` expression.
 *
 * Similar to [`when` in Racket][rkt:when], [`when` in Guile][guile:when],
 * [`when` in Common Lisp][cl:when] and [`when` in Emacs Lisp][el:when].
 *
 * [rkt:when]: https://docs.racket-lang.org/reference/when_unless.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._when%29%29
 * [guile:when]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/Conditionals.html#index-when-1
 * [cl:when]: http://clhs.lisp.se/Body/m_when_.htm
 * [el:when]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html#index-when
 */
declare function when_(exp: any, env: any): any;
declare namespace when_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand an `(unless ...)` expression.
 *
 * Similar to [`unless` in Racket][rkt:unless], [`unless` in Guile][guile:unless],
 * [`unless` in Common Lisp][cl:unless] and [`unless` in Emacs Lisp][el:unless].
 *
 * [rkt:unless]: https://docs.racket-lang.org/reference/when_unless.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._unless%29%29
 * [guile:unless]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/Conditionals.html#index-unless-1
 * [cl:unless]: http://clhs.lisp.se/Body/m_when_.htm
 * [el:unless]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html#index-unless
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
 *
 * Similar to [`unwind-protect` in Common Lisp][cl:unwind-protect]
 * and [`unwind-protect` in Emacs Lisp][el:unwind-protect]
 *
 * [cl:unwind-protect]: http://clhs.lisp.se/Body/s_unwind.htm
 * [el:unwind-protect]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Cleanups.html#index-unwind_002dprotect
 */
declare function unwindProtect_(exp: any, env: any): any;
declare namespace unwindProtect_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(do ...)` expression.
 *
 * Similar to [`do` in Racket][rkt:do] and
 * [`do` in Guile][guile:do].
 *
 * [rkt:do]: https://docs.racket-lang.org/reference/for.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._do%29%29
 * [guile:do]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/while-do.html#index-do
 */
declare function do_(exp: any, env: any): any;
declare namespace do_ {
    var fsource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(while ...)` expression.
 *
 * Similar to [`while` in Guile][guile:while] and
 * [`while` in Emacs Lisp][el:while].
 *
 * [guile:while]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/while-do.html#index-while
 * [el:while]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Iteration.html#index-while
 */
declare function while_(exp: any, env: any): any;
declare namespace while_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(for ...)` expression.
 *
 * Similar to [`for` in Racket][rkt:for].
 *
 * [rkt:for]: https://docs.racket-lang.org/reference/for.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._for%29%29
 */
declare function for_(exp: any, env: any): any;
declare namespace for_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | ((symbol | (number | symbol | symbol[])[])[] | (symbol | ((symbol | (symbol | (string | symbol)[] | undefined)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[][][])[])[])[][] | (symbol | symbol[] | (symbol | (symbol | symbol[])[])[][][])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[] | (symbol | (number | symbol)[] | (symbol | (symbol | symbol[])[])[])[][])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (number | symbol)[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[][])[])[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Expand a `(case ...)` expression.
 *
 * Similar to [`case` in Racket][rkt:case].
 *
 * [rkt:case]: https://docs.racket-lang.org/reference/case.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._case%29%29
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
 *
 * Similar to [`set` in Common Lisp][cl:set] and
 * [`set` in Emacs Lisp][el:set].
 *
 * [cl:set]: http://clhs.lisp.se/Body/f_set.htm
 * [el:set]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-set
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
 * Expand a `(try ...)` expression.
 *
 * Similar to the [`try` special form][clj:try] in Clojure.
 *
 * [clj:try]: https://clojuredocs.org/clojure.core/try
 */
declare function try_(exp: any, env: any): any;
declare namespace try_ {
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
export { begin0_, caseEq_, case_, cljTry_, declareFexpr_, declareMacro_, declare_, defclass_, defineFexpr_, defineMacroToFunction, defineMacroToLambdaForm, defineMacro_, definePrivate_, definePublic_, defmacro_, defun_, do_, for_, letEnv_, multipleValueBind_, newApply_, rktNew_, set_, threadAs_, threadFirst_, threadLast_, try_, unless_, unwindProtect_, when_, while_ };
