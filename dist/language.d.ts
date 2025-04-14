/**
 * # Language
 *
 * Language environment and compiler implementation.
 *
 * ## Description
 *
 * This file defines the language environment. It also contains most
 * of the compiler code.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
import { cljTry_, definePublic_ } from './macros';
import { read, readRose, readSexp, tokenize } from './parser';
import { isAP_, typeOf_ } from './procedures';
import { s, sexp } from './sexp';
import { quotep } from './util';
/**
 * Compile a Lisp expression to JavaScript or TypeScript.
 * Returns a string of JavaScript or TypeScript code.
 *
 * `exp` may be an S-expression, an S-expression wrapped in a rose
 * tree, or a module object.
 */
declare function compile(exp: any, env?: any, options?: any): any;
declare namespace compile {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (string | symbol)[] | (symbol | (symbol | (string | symbol)[])[])[])[];
}
/**
 * Compile a set of modules together.
 * The modules may reference one another.
 */
declare function compileModules(modules: any, env: any, options?: any): any;
declare namespace compileModules {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (string | symbol | (string | symbol)[])[])[])[])[];
}
/**
 * Compile a module map.
 * Returns a new map containing compiled modules.
 */
declare function compileModuleMap(moduleMap: any, env: any, options?: any): any;
declare namespace compileModuleMap {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[] | (symbol | symbol[])[][])[])[];
}
/**
 * Compile a set of files.
 * This function writes to disk.
 */
declare function compileFilesX(files: any, options?: any): any;
declare namespace compileFilesX {
    var lispSource: (symbol | (symbol | (string | symbol | (string | symbol)[])[])[] | (symbol | (symbol | (string | symbol | symbol[])[])[])[] | (symbol | (string | symbol | (symbol | (string | symbol | (string | symbol)[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (string | symbol | (string | symbol)[])[])[])[])[])[])[])[])[];
}
/**
 * Compile a file.
 * This function writes to disk.
 */
declare function compileFileX(infile: any, outfile: any, options?: any): any;
declare namespace compileFileX {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Evaluate a Lisp expression `exp` with environment `env`.
 *
 * `env`, if specified, must be a Lisp environment as returned
 * by {@link Environment}. The expression is evaluated in
 * context of a basic Lisp environment defining such constructs
 * as `(if ...)`, `(cond ...)`, and so on.
 */
declare const interpret: any;
/**
 * Interpret a string of Lisp code.
 */
declare function interpretString(str: any, env?: any, options?: any): any;
declare namespace interpretString {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Interpret a list of files.
 */
declare function interpretFiles(files: any, env?: any, options?: any): any;
declare namespace interpretFiles {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (string | symbol | (string | symbol)[])[])[])[])[])[])[];
}
/**
 * Interpret a string of Lisp code.
 * Alias for `interpret-string`.
 */
declare function lisp(str: any, env?: any): any;
declare namespace lisp {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Make a Lisp environment.
 */
declare function makeLisp(variables?: any, isLisp2?: any): any;
declare namespace makeLisp {
    var lispSource: (symbol | (symbol | (symbol | (symbol | never[])[])[])[])[];
}
/**
 * Split up a string containing multiple comments.
 */
declare function splitComments(str: any): any;
declare namespace splitComments {
    var lispSource: (symbol | (number | symbol)[] | (string | symbol)[] | (symbol | (string | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | ((string | symbol)[] | (symbol | (symbol | (symbol | (string | symbol)[])[])[] | ((string | symbol)[] | (symbol | (string | symbol)[])[])[])[])[] | (symbol | (symbol | (string | symbol)[])[] | (symbol | (symbol | (string | symbol)[] | (symbol | (string | symbol)[])[])[])[])[])[])[])[];
}
/**
 * Convert a function to a macro on the basis
 * of its `(define ...)` form.
 */
declare function definitionToMacro(exp: any, args: any): any;
declare namespace definitionToMacro {
    var lispSource: (symbol | (symbol | (number | symbol)[])[] | (symbol | ((number | symbol | symbol[])[] | (symbol | (number | symbol | symbol[])[][] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | symbol[] | ((symbol | (number | symbol | symbol[])[])[] | (number | symbol)[] | (symbol | (symbol | (symbol | symbol[] | symbol[][])[])[])[])[])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (number | symbol)[] | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[][] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | (symbol | symbol[] | symbol[][])[])[])[])[])[])[])[])[])[])[];
}
/**
 * Create a `(lambda ...)` form for a macro function
 * on the basis of a `(define-macro ...)` expression.
 */
declare function defineMacroToLambdaForm(exp: any): any;
declare namespace defineMacroToLambdaForm {
    var lispSource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | symbol[])[] | ((number | symbol)[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | ((symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[][])[])[])[])[])[])[];
}
/**
 * Create a macro function on the basis of a
 * `(define-macro ...)` expression.
 */
declare function defineMacroToFunction(exp: any, env: any): any;
declare namespace defineMacroToFunction {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Convert a `(define ... (class ...))` expression to
 * a `(define-class ...)` expression.
 */
declare function defineToDefineClass(node: any): any;
declare namespace defineToDefineClass {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | ((symbol | (number | symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol | (number | symbol)[])[])[])[])[])[])[])[])[])[];
}
/**
 * "NO-OP" operation.
 */
declare function nop_(exp: any, env: any): any;
declare namespace nop_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Expand the macro call `exp` in `env`, and keep
 * expanding the result until something that is not
 * a macro call is obtained.
 *
 * Similar to [`macroexpand` in Common Lisp][cl:macroexpand]
 * and [`macroexpand` in Emacs Lisp][el:macroexpand].
 *
 * [cl:macroexpand]: http://clhs.lisp.se/Body/f_mexp_.htm#macroexpand
 * [el:macroexpand]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand
 */
declare function macroexpand(exp: any, env: any): any;
declare namespace macroexpand {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Expand the macro call `exp` in `env`.
 *
 * Similar to [`macroexpand-1` in Common Lisp][cl:macroexpand-1]
 * and [`macroexpand-1` in Emacs Lisp][el:macroexpand-1].
 *
 * [cl:macroexpand-1]: http://clhs.lisp.se/Body/f_mexp_.htm#macroexpand-1
 * [el:macroexpand-1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand_002d1
 */
declare function macroexpand1(exp: any, env: any): any;
declare namespace macroexpand1 {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[] | (string | symbol)[])[])[])[])[];
}
/**
 * Expand the macro call `exp` in `env`, and keep
 * expanding the result until `pred` returns `#f`,
 * or until something that is not a macro call
 * is obtained.
 */
declare function macroexpandUntil(exp: any, env: any, pred: any): any;
declare namespace macroexpandUntil {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Expand all macro calls in `exp` in `env`.
 *
 * Similar to [`macroexpand-all` in Emacs Lisp][el:macroexpand-all].
 *
 * [el:macroexpand-all]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand_002dall
 */
declare function macroexpandAll(exp: any, env: any): any;
declare namespace macroexpandAll {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Expand the macro calls in `exp` in `env`, and keep
 * expanding until `pred` returns `#f`, or until
 * something that is not a macro call is obtained.
 */
declare function macroexpandAllUntil(exp: any, env: any, pred?: any, stack?: any, bindings?: any): any;
declare namespace macroexpandAllUntil {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[])[])[];
}
/**
 * Expand a `(quote ...)` expression.
 *
 * Similar to [`quote` in Racket][rkt:quote] and
 * [`quote` in Common Lisp][cl:quote].
 *
 * [rkt:quote]: https://docs.racket-lang.org/reference/quote.html
 * [cl:quote]: http://clhs.lisp.se/Body/s_quote.htm#quote
 */
declare function quote_(exp: any, env: any): any;
declare namespace quote_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(quasiquote ...)` form.
 * Like `(quote ...)`, but treats `(unquote ...)` and
 * `(unquote-splicing ...)` forms as escaping mechanisms.
 *
 * Similar to [`quasiquote` in Racket][rkt:quasiquote].
 * Also known as "[backquote][cl:backquote]".
 *
 * [rkt:quasiquote]: https://docs.racket-lang.org/reference/quasiquote.html
 * [cl:backquote]: http://clhs.lisp.se/Body/02_df.htm
 */
declare function quasiquote_(exp: any, env: any): any;
declare namespace quasiquote_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(set! ...)` expression.
 *
 * Similar to [`set!` in Racket][rkt:setx] and
 * [`setq` in Common Lisp][cl:setq].
 *
 * [rkt:setx]: https://docs.racket-lang.org/reference/set_.html#%28form._%28%28quote._~23~25kernel%29._set%21%29%29
 * [cl:setq]: http://clhs.lisp.se/Body/s_setq.htm#setq
 */
declare function setX_(exp: any, env: any): any;
declare namespace setX_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(module ...)` expression.
 */
declare function module_(exp: any, env: any): any;
declare namespace module_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(begin ...)` expression.
 */
declare function begin_(exp: any, env: any): any;
declare namespace begin_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(block ...)` expression.
 */
declare function block_(exp: any, env: any): any;
declare namespace block_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(let* ...)` expression.
 */
declare function letStar_(exp: any, env: any): any;
declare namespace letStar_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(let-values ...)` expression.
 */
declare function letValues_(exp: any, env: any): any;
declare namespace letValues_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(define-values ...)` expression.
 */
declare function defineValues_(exp: any, env: any): any;
declare namespace defineValues_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(set!-values ...)` expression.
 */
declare function setValues_(exp: any, env: any): any;
declare namespace setValues_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(define ...)` expression.
 */
declare function define_(exp: any, env: any): any;
declare namespace define_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(define/generator ...)` expression.
 */
declare function defineGenerator_(exp: any, env: any): any;
declare namespace defineGenerator_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(define/async ...)` expression.
 */
declare function defineAsync_(exp: any, env: any): any;
declare namespace defineAsync_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
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
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol)[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(for ...)` expression.
 */
declare function for_(exp: any, env: any): any;
declare namespace for_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(break)` expression.
 */
declare function break_(exp: any, env: any): any;
declare namespace break_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(continue)` expression.
 */
declare function continue_(exp: any, env: any): any;
declare namespace continue_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(yield ...)` expression.
 */
declare function yield_(exp: any, env: any): any;
declare namespace yield_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(return ...)` expression.
 */
declare function return_(exp: any, env: any): any;
declare namespace return_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(throw ...)` expression.
 *
 * Similar to the [`throw`][clj:throw] special form in Clojure.
 *
 * [clj:throw]: https://clojuredocs.org/clojure.core/throw
 */
declare function throw_(exp: any, env: any): any;
declare namespace throw_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(js/async ...)` expression.
 */
declare function jsAsync_(exp: any, env: any): any;
declare namespace jsAsync_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(js/await ...)` expression.
 */
declare function jsAwait_(exp: any, env: any): any;
declare namespace jsAwait_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(lambda ...)` expression.
 *
 * Returns an anonymous function. The name `lambda` is
 * a reference to [lambda calculus][w:Lambda calculus].
 *
 * [w:Lambda calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
 */
declare function lambda_(exp: any, env: any): any;
declare namespace lambda_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(cond ...)` expression.
 */
declare function cond_(exp: any, env: any): any;
declare namespace cond_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand an `(and ...)` expression.
 */
declare function and_(exp: any, env: any): any;
declare namespace and_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand an `(or ...)` expression.
 */
declare function or_(exp: any, env: any): any;
declare namespace or_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Call a method on an object.
 */
declare function sendMethod(...args: any[]): any;
declare namespace sendMethod {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[] | (string | symbol | symbol[])[][])[])[];
}
/**
 * Expand a `(send ...)` expression.
 *
 * Similar to [`send`][rkt:send] in Racket.
 *
 * [rkt:send]: https://docs.racket-lang.org/guide/classes.html#(part._methods)
 */
declare function send_(exp: any, env: any): any;
declare namespace send_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(send/apply ...)` expression.
 */
declare function sendApply_(exp: any, env: any): any;
declare namespace sendApply_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(. ...)` expression.
 *
 * Similar to the [`.` special form][clj:dot] in Clojure and
 * [ClojureScript][cljs:dot].
 *
 * [clj:dot]: https://clojure.org/reference/java_interop#dot
 * [cljs:dot]: https://cljs.github.io/api/syntax/dot
 */
declare function dot_(exp: any, env: any): any;
declare namespace dot_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(get-field ...)` expression.
 */
declare function getField_(exp: any, env: any): any;
declare namespace getField_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(set-field! ...)` expression.
 */
declare function setField_(exp: any, env: any): any;
declare namespace setField_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Evaluate an `(make-object ...)` expression.
 */
declare function new_(constructor: any, ...args: any[]): any;
declare namespace new_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Expand a `(class ...)` expression.
 *
 * Loosely based on [`class` in Racket][rkt:class] and
 * [`define-class` in CLOS][cl:define-class].
 *
 * [rkt:class]: https://docs.racket-lang.org/guide/classes.html
 * [cl:define-class]: http://clhs.lisp.se/Body/07_.htm
 */
declare function class_(exp: any, env: any): any;
declare namespace class_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(define-class ...)` expression.
 *
 * Loosely based on [`class` in Racket][rkt:class] and
 * [`defclass` in CLOS][cl:defclass].
 *
 * [rkt:class]: https://docs.racket-lang.org/guide/classes.html
 * [cl:defclass]: http://clhs.lisp.se/Body/m_defcla.htm#defclass
 */
declare function defineClass_(exp: any, env: any): any;
declare namespace defineClass_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(provide ...)` expression.
 */
declare function provide_(exp: any, env: any): any;
declare namespace provide_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(require ...)` expression.
 */
declare function require_(exp: any, env: any): any;
declare namespace require_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Evaluate a JavaScript string.
 */
declare function js_(str: any): any;
declare namespace js_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Get the Lisp source of a function.
 */
declare function source(x: any): any;
declare namespace source {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether a function has Lisp source.
 */
declare function sourcep(x: any): any;
declare namespace sourcep {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Map the function `f` over the rose tree-wrapped
 * S-expression `node`. The S-expression is processed
 * in bottom-up order.
 */
declare function mapRose(f: any, node: any, env?: any, stack?: any, bindings?: any): any;
declare namespace mapRose {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[] | (symbol | symbol[])[][])[] | (symbol | (symbol | (symbol | never[])[])[])[])[];
}
/**
 * Map a function `f` over a rose tree using the Visitor pattern.
 */
declare function mapVisitRose(f: any, node: any, env?: any, stack?: any, bindings?: any): any;
declare namespace mapVisitRose {
    var lispSource: (symbol | (symbol | (number | symbol)[] | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[] | (string | symbol)[][])[])[])[])[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[])[])[])[] | (symbol | (string | symbol)[][] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol)[])[])[])[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[])[])[])[] | (symbol | (symbol | (string | symbol)[])[] | ((string | symbol)[] | (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol)[])[])[])[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[] | (string | symbol)[] | (symbol | (string | symbol)[][] | (symbol | (symbol | (symbol | symbol[])[][] | (string | symbol)[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[][])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[][])[])[])[])[])[])[])[])[];
}
/**
 * Map the function `f` over the S-expression `exp`.
 * The S-expression is processed in bottom-up order.
 */
declare function mapSexp(f: any, exp: any, env?: any, stack?: any, bindings?: any): any;
declare namespace mapSexp {
    var lispSource: (symbol | (symbol | (symbol | (symbol | never[])[])[])[] | (symbol | (symbol | symbol[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[][])[])[])[][])[])[];
}
/**
 * Call the function `f` on each node of a rose tree,
 * but do not create a new rose tree in the process.
 */
declare function iterateRose(f: any, node: any, env?: any): any;
declare namespace iterateRose {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Expand an `(ann ...)` expression.
 */
declare function ann_(exp: any, env: any): any;
declare namespace ann_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(: ...)` expression.
 */
declare function colon_(exp: any, env: any): any;
declare namespace colon_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(define-type ...)` expression.
 */
declare function defineType_(exp: any, env: any): any;
declare namespace defineType_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(let-js-obj ...)` expression.
 */
declare function letJsObj_(exp: any, env: any): any;
declare namespace letJsObj_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(define-js-obj ...)` expression.
 */
declare function defineJsObj_(exp: any, env: any): any;
declare namespace defineJsObj_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Expand a `(set!-js-obj ...)` expression.
 */
declare function setJsObj_(exp: any, env: any): any;
declare namespace setJsObj_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
    var lispMacro: boolean;
}
/**
 * Simple `call-with-current-continuation` implementation.
 * Also known as `call/cc`.
 *
 * Similar to
 * [`call-with-current-continuation` in Racket][rkt:call-with-current-continuation].
 *
 * [rkt:call-with-current-continuation]: https://docs.racket-lang.org/reference/cont.html#%28def._%28%28quote._~23~25kernel%29._call-with-current-continuation%29%29
 */
declare function callWithCurrentContinuation_(proc: any, promptTag?: any): any;
declare namespace callWithCurrentContinuation_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[];
}
/**
 * Traverse an ESTree tree.
 */
declare function traverseEstree(node: any, enter?: any, leave?: any, replace?: any): any;
declare namespace traverseEstree {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | symbol[])[] | (symbol | (number | symbol | symbol[])[])[][])[][])[])[])[];
}
/**
 * Find ESTree nodes matching a predicate.
 */
declare function findEstree(pred: any, node: any): any;
declare namespace findEstree {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Optimize an S-expression.
 */
declare function optimizeSexp(exp: any, env: any): any;
declare namespace optimizeSexp {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Optimize a rose tree-wrapped S-expression.
 */
declare function optimizeRose(exp: any, env: any): any;
declare namespace optimizeRose {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Optimize a module.
 */
declare function optimizeModule(m: any, env: any): any;
declare namespace optimizeModule {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Optimize an ESTree tree.
 */
declare function optimizeEstree(exp: any): any;
declare namespace optimizeEstree {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
declare function letVarsToConstVars(program: any): any;
declare namespace letVarsToConstVars {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | ((string | symbol)[] | (symbol | ((string | symbol | symbol[])[] | (symbol | (symbol | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | symbol[])[])[][])[])[])[])[] | ((string | symbol)[] | (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[] | (symbol | (string | symbol)[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[] | (string | symbol)[])[])[])[])[])[])[])[])[])[])[])[])[])[])[])[];
}
/**
 * Apply optimizations to `node`.
 */
declare function applyOptimizations(node: any, env: any, rules?: any): any;
declare namespace applyOptimizations {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * List of `(predicate optimizer)` tuples.
 */
declare const optimizations: any;
/**
 * Module class.
 */
declare class Module {
    name: any;
    headerExpressions: any;
    headerNodes: any;
    requireExpressions: any;
    requireNodes: any;
    provideExpressions: any;
    provideNodes: any;
    mainExpressions: any;
    mainNodes: any;
    expressions: any;
    nodes: any;
    inlineLispSourcesFlag: any;
    seenModules: any;
    environment: any;
    parentEnvironment: any;
    moduleMap: any;
    symbolMap: any;
    constructor(nodes?: any, parent?: any, name?: any);
    getExpressions(): any;
    getEnvironment(): any;
    getModuleMap(): any;
    getName(): any;
    /**
     * Whether a particular symbol is bound in this module's scope
     * (i.e., whether the module imports or defines the symbol).
     */
    hasSymbol(sym: any): any;
    makeHeaderNode(nodes?: any): any;
    findInlineLispSourcesComment(comments?: any): any;
    initializeNodes(nodes?: any): any;
    makeEnvironment(parent?: any): any;
    setModuleMap(moduleMap: any): any;
    setNodes(nodes: any): any;
    setExpressions(expressions?: any): any;
    setInlineLispSourcesFlag(val: any): any;
    getInlineLispSourcesFlag(): any;
}
/**
 * Convert a map of `module` forms to a map of `Module` objects,
 * interlinking them in the process.
 */
declare function makeModuleMap(moduleExpressionMap: any, env: any): any;
declare namespace makeModuleMap {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
/**
 * Convert a `(module ...)` expression to a
 * `Module` object.
 */
declare function moduleExpressionToModuleObject(node: any, env: any): any;
declare namespace moduleExpressionToModuleObject {
    var lispSource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[])[];
}
/**
 * Lisp environment.
 */
declare const lispEnvironment: any;
/**
 * Interpretation environment.
 * Includes `eval`.
 */
declare const interpretationEnvironment: any;
/**
 * Compilation environment.
 */
declare const compilationEnvironment: any;
/**
 * Language environment.
 */
declare const langEnvironment: any;
export * from './array';
export * from './constants';
export * from './curry';
export * from './env';
export * from './equal';
export * from './eval';
export * from './hash';
export * from './javascript';
export * from './list';
export * from './macros';
export * from './object';
export * from './plist';
export * from './printer';
export * from './procedures';
export * from './regexp';
export * from './rose';
export * from './string';
export * from './symbol';
export { and_ as and, ann_ as ann, begin_ as begin, block_ as block, callWithCurrentContinuation_ as callWithCurrentContinuation, callWithCurrentContinuation_ as callCc, cljTry_ as try, cljTry_ as try_, colon_ as colon, compile as compileLisp, compile as compileLispToJavascript, cond_ as cond, defineAsync_ as defineAsync, defineClass_ as defineClass, defineGenerator_ as defineGenerator, defineJsObj_ as defineJsObj, defineMacro_ as defineMacro, definePublic_ as definePublic, defineType_ as defineType, defineValues_ as defineValues, define_ as define, dot_ as dot, getField_ as getField, jsAsync_ as async, jsAsync_ as async_, jsAsync_ as jsAsync, jsAwait_ as await, jsAwait_ as await_, jsAwait_ as jsAwait, js_ as js, lambda_ as compileFunction, lambda_ as fn, lambda_ as lambda, letJsObj_ as letJsObj, letStar_ as letStar, letStar_ as let_, letStar_ as letrec, letValues_ as letstarValues, letValues_ as letValues, letValues_ as letrecValues, lispEnvironment as lisp1Environment, new_ as jsNew, new_ as make, new_ as makeObject, new_ as makeObject_, new_ as newStar, new_ as rktMakeObject, new_ as scmNew, nop_ as nop, or_ as or, provide_ as provide, quasiquote_ as quasiquote, quote_ as quote, require_ as require, sendApply_ as sendApply, send_ as callMethod, send_ as send, setX_ as setX, setX_ as setq, setX_ as setq_, setField_ as setFieldX, setField_ as setField, setJsObj_ as setXJsObj, setJsObj_ as setJsObjX, setJsObj_ as setJsObj, setValues_ as setXValues, setValues_ as setValues, sexp as readFromString, Module, and_, ann_, applyOptimizations, begin_, break_, class_, cljTry_, colon_, compilationEnvironment, compile, compileFileX, compileFilesX, compileModuleMap, compileModules, cond_, continue_, defineToDefineClass, defineAsync_, defineGenerator_, defineJsObj_, defineMacroToFunction, defineMacroToLambdaForm, defineMacro_, defineType_, defineValues_, define_, definitionToMacro, dot_, findEstree, for_, getField_, interpret, interpretFiles, interpretString, interpretationEnvironment, isAP_, iterateRose, jsAsync_, jsAwait_, js_, lambda_, langEnvironment, letJsObj_, letStar_, letValues_, letVarsToConstVars, lisp, lispEnvironment, macroexpand, macroexpand1, macroexpandAll, macroexpandAllUntil, macroexpandUntil, makeLisp, makeModuleMap, mapRose, mapSexp, mapVisitRose, moduleExpressionToModuleObject, module_, new_, nop_, optimizations, optimizeEstree, optimizeModule, optimizeRose, optimizeSexp, or_, provide_, quasiquote_, quotep, quote_, read, readRose, readSexp, require_, return_, s, sendApply_, sendMethod, send_, setX_, setField_, setJsObj_, setValues_, sexp, source, sourcep, splitComments, throw_, tokenize, traverseEstree, typeOf_, yield_ };
