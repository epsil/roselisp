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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | symbol[] | symbol[][])[])[])[];
}
/**
 * Expand a `(define/private ...)` expression.
 */
declare function definePrivate_(exp: any, env: any): any;
declare namespace definePrivate_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (string | symbol)[])[])[];
}
/**
 * Expand a `(define/public ...)` expression.
 */
declare function definePublic_(exp: any, env: any): any;
declare namespace definePublic_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (string | symbol)[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (string | symbol)[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Expand a `(macro ...)` expression.
 *
 * Somewhat similar to [`macro` in Emacs Lisp][el:macro].
 *
 * [el:macro]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Macros.html#index-defmacro
 */
declare function macro_(exp: any, env: any): any;
declare namespace macro_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (string | symbol)[])[])[])[])[])[])[];
}
/**
 * Expand a `(define-compiler-macro ...)` expression.
 */
declare function defineCompilerMacro_(exp: any, env: any): any;
declare namespace defineCompilerMacro_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Expand a `(syntax ...)` expression.
 */
declare function syntax_(exp: any, env: any): any;
declare namespace syntax_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (boolean | symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Expand a `(quasisyntax ...)` expression.
 */
declare function quasisyntax_(exp: any, env: any): any;
declare namespace quasisyntax_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (boolean | symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Expand a `(define-syntax ...)` expression.
 */
declare function defineSyntax_(exp: any, env: any): any;
declare namespace defineSyntax_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[];
}
/**
 * Expand a `(syntax-macro ...)` expression.
 */
declare function syntaxMacro_(exp: any, env: any): any;
declare namespace syntaxMacro_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[];
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
declare function defineMacroToLambdaForm(exp: any, options?: any): any;
declare namespace defineMacroToLambdaForm {
    var fsource: (symbol | (symbol | (number | symbol)[])[] | (symbol | undefined)[] | (boolean | symbol)[] | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | symbol[])[] | ((number | symbol)[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | ((symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol[] | undefined)[])[])[])[])[] | ((symbol | (number | symbol)[])[] | (boolean | symbol)[])[] | ((symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[][])[])[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (string | symbol)[])[])[];
}
/**
 * Expand a `(defsubst ...)` expression.
 *
 * Similar to [`defsubst` in Emacs Lisp][el:defsubst].
 *
 * [el:defsubst]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Inline-Functions.html#index-defsubst
 */
declare function defsubst_(exp: any, env: any): any;
declare namespace defsubst_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | symbol[] | symbol[][])[])[])[];
}
/**
 * Expand a `(define-inline ...)` expression.
 */
declare function defineInline_(exp: any, env: any): any;
declare namespace defineInline_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (boolean | symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[];
}
/**
 * Expand a `(define-fexpr ...)` expression.
 */
declare function defineFexpr_(exp: any, env: any): any;
declare namespace defineFexpr_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[])[];
}
/**
 * Expand a `(nlambda ...)` expression.
 *
 * Similar to [`nlambda` in Interlisp][il:nlambda].
 *
 * [il:nlambda]: https://interlisp.org/software/using-medley/cl-using/#lambda--nlambda--cllambda
 */
declare function nlambda_(exp: any, env: any): any;
declare namespace nlambda_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[])[])[];
}
/**
 * Expand a `(declare-macro ...)` expression.
 */
declare function declareMacro_(exp: any, env: any): any;
declare namespace declareMacro_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | undefined)[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[])[])[];
}
/**
 * Expand a `(declare-syntax-macro ...)` expression.
 */
declare function declareSyntaxMacro_(exp: any, env: any): any;
declare namespace declareSyntaxMacro_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | undefined)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
/**
 * Expand a `(declare-fexpr ...)` expression.
 */
declare function declareFexpr_(exp: any, env: any): any;
declare namespace declareFexpr_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | undefined)[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | symbol[] | symbol[][][])[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | symbol[] | symbol[][][])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (string | symbol)[])[])[];
}
/**
 * Expand an `(and ...)` expression.
 *
 * Similar to [`and` in Racket][rkt:and], [`and` in Guile][guile:and],
 * [`and` in Common Lisp][cl:and] and [`and` in Emacs Lisp][el:and].
 *
 * [rkt:and]: https://docs.racket-lang.org/reference/if.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._and%29%29
 * [guile:and]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/and-or.html#index-and
 * [cl:and]: http://clhs.lisp.se/Body/m_and.htm
 * [el:and]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Combining-Conditions.html#index-and
 */
declare function and_(stx: any): any;
declare namespace and_ {
    var ftype: symbol[];
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[];
}
/**
 * Expand an `(or ...)` expression.
 *
 * Similar to [`or` in Racket][rkt:or], [`or` in Guile][guile:or],
 * [`or` in Common Lisp][cl:or] and [`or` in Emacs Lisp][el:or].
 *
 * [rkt:or]: https://docs.racket-lang.org/reference/if.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._or%29%29
 * [guile:or]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/and-or.html#index-or
 * [cl:or]: http://clhs.lisp.se/Body/m_or.htm
 * [el:or]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Combining-Conditions.html#index-or
 */
declare function or_(stx: any): any;
declare namespace or_ {
    var ftype: symbol[];
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[];
}
/**
 * Expand a `(cond ...)` expression.
 *
 * Similar to [`cond` in Racket][rkt:cond] and
 * [`cond` in Guile][guile:cond].
 *
 * [rkt:cond]: https://docs.racket-lang.org/reference/if.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._cond%29%29
 * [guile:cond]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/Conditionals.html#index-cond-1
 */
declare function cond_(stx: any): any;
declare namespace cond_ {
    var ftype: symbol[];
    var fsource: (symbol | (symbol | undefined)[] | (symbol | (symbol | (symbol | (number | symbol)[])[] | (number | symbol | symbol[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | undefined)[])[] | (symbol | ((symbol | (symbol | (string | symbol)[])[])[] | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (number | symbol)[])[])[])[] | (boolean | symbol | (symbol | (symbol | (symbol | (symbol | (symbol | never[])[])[])[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[] | (symbol | (boolean | symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | never[])[])[])[])[])[])[])[])[])[] | (symbol | (symbol | (boolean | symbol | (symbol | (symbol | symbol[] | symbol[][])[])[])[])[])[])[];
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
declare function when_(stx: any): any;
declare namespace when_ {
    var ftype: symbol[];
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[];
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
declare function unless_(stx: any): any;
declare namespace unless_ {
    var ftype: symbol[];
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[];
}
/**
 * Expand an `(el/if ...)` expression.
 *
 * Similar to [`if` in Emacs Lisp][el:if].
 *
 * [el:if]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html#index-if
 */
declare function elIf_(exp: any, env: any): any;
declare namespace elIf_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (number | symbol | symbol[])[][])[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (boolean | symbol)[] | (symbol | (symbol | (symbol | (symbol | ((number | symbol)[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (boolean | symbol)[] | (symbol | (symbol | (symbol | symbol[])[] | symbol[][][])[])[])[])[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[] | ((number | symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (symbol[] | (symbol | symbol[])[][])[])[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[] | (number | symbol | (symbol | (symbol | symbol[])[])[])[][])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (string | symbol)[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[];
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
declare function while_(stx: any): any;
declare namespace while_ {
    var ftype: symbol[];
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | undefined)[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (number | symbol | symbol[])[])[][] | (symbol | (symbol | undefined)[] | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | symbol[][])[])[] | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (number | symbol | (symbol | symbol[])[])[])[])[] | (symbol | (symbol | (symbol | symbol[][])[])[] | (symbol | (string | symbol)[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[] | (symbol | (number | symbol)[] | (symbol | (symbol | symbol[])[])[])[][])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (number | symbol)[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[][])[])[])[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (boolean | symbol)[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (boolean | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[][])[])[])[])[])[])[])[])[])[];
}
/**
 * Expand a `(case/eq ...)` expression.
 */
declare function caseEq_(exp: any, env: any): any;
declare namespace caseEq_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (boolean | symbol)[] | (symbol | (symbol | (boolean | symbol)[] | (symbol | (number | symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[][])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[])[])[])[])[];
}
/**
 * Expand a `(let-env ...)` expression.
 */
declare function letEnv_(exp: any, env: any): any;
declare namespace letEnv_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (string | symbol)[])[])[];
}
/**
 * Expand a `(setq ...)` expression.
 *
 * Similar to [`setq` in Common Lisp][cl:setq]
 * and [`setq` in Emacs Lisp][el:setq].
 *
 * [cl:setq]: http://clhs.lisp.se/Body/s_setq.htm#setq
 * [el:setq]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-setq
 */
declare function setq_(exp: any, env: any): any;
declare namespace setq_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[] | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (number | symbol)[])[])[])[])[];
}
/**
 * Expand a `(new/apply ...)` expression.
 */
declare function newApply_(exp: any, env: any): any;
declare namespace newApply_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (string | symbol)[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (string | symbol)[])[])[];
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
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (number | symbol | symbol[])[] | (symbol | ((symbol | (symbol | (symbol | symbol[])[])[] | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | ((symbol | (number | symbol)[])[] | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[])[])[])[])[])[];
}
/**
 * Expand a `(match ...)` expression.
 *
 * Similar to [`match` in Racket] and, to a lesser extent,
 * [`match` in Guile][guile:match].
 *
 * [rkt:match]: https://docs.racket-lang.org/reference/match.html#%28form._%28%28lib._racket%2Fmatch..rkt%29._match%29%29
 * [guile:match]: https://doc.guix.gnu.org/guile/latest/en/html_node/Pattern-Matching.html#index-match
 */
declare function match_(exp1: any, env: any): any;
declare namespace match_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (boolean | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (boolean | symbol[])[] | (symbol | (symbol | (string | symbol)[])[][] | (symbol | (symbol | (symbol | symbol[])[] | symbol[][][])[])[])[][] | (symbol | (boolean | symbol)[] | (boolean | (symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (boolean | symbol | symbol[])[])[])[])[][] | (boolean | symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[][] | (symbol | ((boolean | symbol)[] | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (boolean | symbol)[])[] | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[][] | (symbol | (symbol | (boolean | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (boolean | symbol)[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[] | (symbol | (symbol | (symbol | symbol[])[][])[] | (symbol | (number | symbol)[] | (symbol | (boolean | symbol)[])[])[])[][] | ((symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | ((number | symbol | symbol[])[] | (boolean | symbol | symbol[])[])[])[])[])[][])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[] | (symbol | symbol[])[][])[])[] | (boolean | (symbol | (symbol | symbol[])[] | (boolean | symbol)[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (string | symbol)[])[][] | (symbol | (symbol | (symbol | symbol[])[] | symbol[][][])[])[])[][] | (symbol | (symbol | (boolean | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[] | (symbol | (symbol | symbol[])[][])[])[])[])[])[])[])[];
}
/**
 * Expand a `(cl/loop ...)` expression.
 *
 * Similar to [`loop` in Common Lisp][cl:loop].
 *
 * [cl:loop]: http://clhs.lisp.se/Body/m_loop.htm#loop
 */
declare function clLoop_(exp: any, env: any): any;
declare namespace clLoop_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | undefined)[] | (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[][])[])[])[])[])[])[])[])[];
}
/**
 * `with-gensyms` macro as defined in
 * Peter Seibel's [*Practical Common Lisp*][book:pcl].
 *
 * [book:pcl]: https://gigamonkeys.com/book/macros-defining-your-own#macro-writing-macros
 */
declare function withGensyms_(exp: any, env: any): any;
declare namespace withGensyms_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[])[])[])[])[];
}
/**
 * `once-only` macro, adapted from the one described in
 * Peter Seibel's [*Practical Common Lisp*][book:pcl].
 *
 * [book:pcl]: https://gigamonkeys.com/book/macros-defining-your-own#macro-writing-macros
 */
declare function onceOnly_(exp: any, env: any): any;
declare namespace onceOnly_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[][])[])[])[])[])[][] | (symbol | (symbol | symbol[] | (symbol | (symbol | (symbol | symbol[][])[])[])[][])[])[])[])[])[])[])[])[];
}
/**
 * Alternative implementation of `once-only` that skips over atomic
 * expressions. Expands to a nested `cond` form that only invokes
 * `once-only` on variables that are bound to complex expressions.
 *
 * Note that this gets rather verbose when there are many variables.
 * In that case, it may be better to define a recursive macro
 * instead.
 */
declare function onceOnlystar_(exp: any, env: any): any;
declare namespace onceOnlystar_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[])[])[])[];
}
export { defineInline_ as defineSubst_, and_, begin0_, caseEq_, case_, clLoop_, cljTry_, cond_, declareFexpr_, declareMacro_, declareSyntaxMacro_, declare_, defclass_, defineCompilerMacro_, defineFexpr_, defineInline_, defineMacroToFunction, defineMacroToLambdaForm, defineMacro_, definePrivate_, definePublic_, defineSyntax_, defmacro_, defsubst_, defun_, do_, elIf_, for_, letEnv_, macro_, match_, multipleValueBind_, newApply_, nlambda_, onceOnly_, onceOnlystar_, or_, quasisyntax_, rktNew_, set_, setq_, syntaxMacro_, syntax_, threadAs_, threadFirst_, threadLast_, try_, unless_, unwindProtect_, when_, while_, withGensyms_ };
