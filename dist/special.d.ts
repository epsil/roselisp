/**
 * # Special forms
 *
 * Interpreter procedures for special forms.
 *
 * ## Description
 *
 * The default approach for the interpreter is to compile the Lisp
 * code to JavaScript code and then evaluate the JavaScript code.
 * That is to say that the S-expression is compiled to an ESTree
 * tree, which is then evaluated.
 *
 * However, it is possible to make the intepretation more efficient
 * by skipping the compilation step and interpreting the S-expression
 * directly. This file defines interpreter procedures which do just
 * that.
 *
 * The functions defined in this file are *special forms* and must be
 * typed as such in the language environment. A special form receives
 * its arguments unevaluated and returns a value that is used
 * directly. It is similar to a macro, except that the value returned
 * by a macro is re-evaluated, while the value returned by a special
 * form is used as-is.
 *
 * Care must be taken to implement these functions correctly, as
 * their behavior should be identical to the standard behavior of
 * compiling the code to an ESTree tree and then evaluating. In other
 * words, the only purpose of the code in this file is to make
 * interpretation faster.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Evaluate a `(quote ...)` form.
 */
declare function quoteSpecial_(exp: any, env: any): any;
declare namespace quoteSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(quasiquote ...)` form.
 */
declare function quasiquoteSpecial_(exp: any, env: any): any;
declare namespace quasiquoteSpecial_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Evaluate a `(setq ...)` form.
 */
declare function setqSpecial_(exp: any, env: any): any;
declare namespace setqSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (number | symbol | (number | symbol | symbol[])[])[])[][])[] | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Evaluate a `(set ...)` form.
 */
declare function setSpecial_(exp: any, env: any): any;
declare namespace setSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[][])[])[][] | (symbol | (symbol | (symbol | (string | symbol)[])[] | ((symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[][])[])[])[])[])[])[];
}
/**
 * Evaluate a `(fset ...)` form.
 */
declare function fsetSpecial_(exp: any, env: any): any;
declare namespace fsetSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol)[])[];
}
/**
 * Evaluate a `(module ...)` form.
 */
declare function moduleSpecial_(exp: any, env: any): any;
declare namespace moduleSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[])[])[];
}
/**
 * Evaluate a `(begin ...)` form.
 */
declare function beginSpecial_(exp: any, env: any): any;
declare namespace beginSpecial_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Evaluate a `(let* ...)` form.
 */
declare function letStarSpecial_(exp: any, env: any): any;
declare namespace letStarSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (string | symbol | symbol[])[])[])[])[])[])[];
}
/**
 * Evaluate a `(let-values ...)` form.
 */
declare function letValuesSpecial_(exp: any, env: any): any;
declare namespace letValuesSpecial_ {
    var lispSource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[] | (symbol | (symbol | (string | symbol | (symbol | (number | symbol)[])[])[])[])[])[])[];
}
/**
 * Evaluate a `(define-values ...)` form.
 */
declare function defineValuesSpecial_(exp: any, env: any): any;
declare namespace defineValuesSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[];
}
/**
 * Evaluate a `(set!-values ...)` form.
 */
declare function setValuesSpecial_(exp: any, env: any): any;
declare namespace setValuesSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[];
}
/**
 * Evaluate a `(define ...)` form.
 */
declare function defineSpecial_(exp: any, env: any): any;
declare namespace defineSpecial_ {
    var lispSource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[][])[])[];
}
/**
 * Evaluate a `(define/public ...)` form.
 */
declare function definePublicSpecial_(exp: any, env: any): any;
declare namespace definePublicSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(define/generator ...)` form.
 */
declare function defineGeneratorSpecial_(exp: any, env: any): any;
declare namespace defineGeneratorSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(define/async ...)` form.
 */
declare function defineAsyncSpecial_(exp: any, env: any): any;
declare namespace defineAsyncSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(defmacro ...)` form.
 */
declare function defmacroSpecial_(exp: any, env: any): any;
declare namespace defmacroSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol)[])[];
}
/**
 * Evaluate a `(define-macro ...)` form.
 */
declare function defineMacroSpecial_(exp: any, env: any): any;
declare namespace defineMacroSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[] | (string | symbol)[])[];
}
/**
 * Evaluate a `(for ...)` form.
 */
declare function forSpecial_(exp: any, env: any): any;
declare namespace forSpecial_ {
    var lispSource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[] | symbol[][][])[])[])[])[])[])[])[])[];
}
/**
 * Evaluate a `(js/while ...)` form.
 */
declare function jsWhileSpecial_(exp: any, env: any): any;
declare namespace jsWhileSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Evaluate a `(js/do-while ...)` form.
 */
declare function jsDoWhileSpecial_(exp: any, env: any): any;
declare namespace jsDoWhileSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Evaluate a `(break)` form.
 */
declare function breakSpecial_(exp: any, env: any): any;
declare namespace breakSpecial_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Evaluate a `(continue)` form.
 */
declare function continueSpecial_(exp: any, env: any): any;
declare namespace continueSpecial_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Evaluate a `(yield ...)` form.
 */
declare function yieldSpecial_(exp: any, env: any): any;
declare namespace yieldSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Evaluate a `(return ...)` form.
 */
declare function returnSpecial_(exp: any, env: any): any;
declare namespace returnSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Evaluate a `(throw ...)` form.
 */
declare function throwSpecial_(exp: any, env: any): any;
declare namespace throwSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Evaluate an `(async ...)` form.
 */
declare function asyncSpecial_(exp: any, env: any): any;
declare namespace asyncSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Evaluate an `(await ...)` form.
 */
declare function awaitSpecial_(exp: any, env: any): Promise<any>;
declare namespace awaitSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Evaluate a `(lambda ...)` form.
 */
declare function lambdaSpecial_(exp: any, env: any): any;
declare namespace lambdaSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(js/function ...)` form.
 */
declare function jsFunctionSpecial_(exp: any, env: any): any;
declare namespace jsFunctionSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[];
}
/**
 * Evaluate a `(js/arrow ...)` form.
 */
declare function jsArrowSpecial_(exp: any, env: any): any;
declare namespace jsArrowSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[];
}
/**
 * Evaluate a `(cond ...)` form.
 */
declare function condSpecial_(exp: any, env: any): any;
declare namespace condSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[])[];
}
/**
 * Evaluate an `(and ...)` form.
 */
declare function andSpecial_(exp: any, env: any): any;
declare namespace andSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
/**
 * Evaluate an `(or ...)` form.
 */
declare function orSpecial_(exp: any, env: any): any;
declare namespace orSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
/**
 * Evaluate a `(send ...)` form.
 */
declare function sendSpecial_(exp: any, env: any): any;
declare namespace sendSpecial_ {
    var lispSource: (symbol | (symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Evaluate a `(send/apply ...)` form.
 */
declare function sendApplySpecial_(exp: any, env: any): any;
declare namespace sendApplySpecial_ {
    var lispSource: (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (number | symbol)[])[])[])[])[];
}
/**
 * Evaluate a `(. ...)` form.
 */
declare function dotSpecial_(exp: any, env: any): any;
declare namespace dotSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[] | ((symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (symbol | (symbol | (string | symbol)[])[])[])[])[])[])[];
}
/**
 * Evaluate a `(get-field ...)` form.
 */
declare function getFieldSpecial_(exp: any, env: any): any;
declare namespace getFieldSpecial_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Evaluate a `(js/optional-chaining ...)` form.
 */
declare function jsOptionalChainingSpecial_(exp: any, env: any): any;
declare namespace jsOptionalChainingSpecial_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Evaluate a `(set-field! ...)` form.
 */
declare function setFieldSpecial_(exp: any, env: any): any;
declare namespace setFieldSpecial_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Evaluate a `(define-class ...)` form.
 */
declare function defineClassSpecial_(exp: any, env: any): any;
declare namespace defineClassSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (string | symbol)[] | (symbol | (string | symbol)[])[])[])[] | (symbol | (string | symbol | symbol[])[])[] | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[] | (symbol | (symbol | symbol[])[])[][])[])[])[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (string | symbol)[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (symbol | (number | symbol)[])[][] | (symbol | (symbol | (symbol | symbol[])[][])[])[])[])[])[])[])[];
}
/**
 * Evaluate a `(try ...)` form.
 */
declare function trySpecial_(exp: any, env: any): any;
declare namespace trySpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | symbol[])[])[])[])[] | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[] | (symbol | symbol[])[][][])[])[])[])[])[])[])[] | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[])[])[])[])[])[])[])[];
}
/**
 * Evaluate a `(provide ...)` form.
 */
declare function provideSpecial_(exp: any, env: any): any;
declare namespace provideSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(require ...)` form.
 */
declare function requireSpecial_(exp: any, env: any): any;
declare namespace requireSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate an `(ann ...)` form.
 */
declare function annSpecial_(exp: any, env: any): any;
declare namespace annSpecial_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Evaluate a `(colon ...)` form.
 */
declare function colonSpecial_(exp: any, env: any): any;
declare namespace colonSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(define-type ...)` form.
 */
declare function defineTypeSpecial_(exp: any, env: any): any;
declare namespace defineTypeSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(let-js-obj ...)` form.
 */
declare function letJsObjSpecial_(exp: any, env: any): any;
declare namespace letJsObjSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(define-js-obj ...)` form.
 */
declare function defineJsObjSpecial_(exp: any, env: any): any;
declare namespace defineJsObjSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(set!-js-obj ...)` form.
 */
declare function setJsObjSpecial_(exp: any, env: any): any;
declare namespace setJsObjSpecial_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Evaluate a `(let-env ...)` form.
 */
declare function letEnvSpecial_(exp: any, env: any): any;
declare namespace letEnvSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Evaluate a `(macrop ...)` form.
 *
 * Whether `macro` is a macro in the environment `env`.
 * `macro` may be a symbol, a string, or a macro function.
 */
declare function macropSpecial_(val: any): any;
declare namespace macropSpecial_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Evaluate an `(nlambda ...)` form.
 */
declare function nlambdaSpecial_(exp: any, env: any): any;
declare namespace nlambdaSpecial_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
export { andSpecial_, annSpecial_, asyncSpecial_, awaitSpecial_, beginSpecial_, breakSpecial_, colonSpecial_, condSpecial_, continueSpecial_, defineAsyncSpecial_, defineClassSpecial_, defineGeneratorSpecial_, defineJsObjSpecial_, defineMacroSpecial_, definePublicSpecial_, defineSpecial_, defineTypeSpecial_, defineValuesSpecial_, defmacroSpecial_, dotSpecial_, forSpecial_, fsetSpecial_, getFieldSpecial_, jsArrowSpecial_, jsDoWhileSpecial_, jsFunctionSpecial_, jsOptionalChainingSpecial_, jsWhileSpecial_, lambdaSpecial_, letEnvSpecial_, letJsObjSpecial_, letStarSpecial_, letValuesSpecial_, macropSpecial_, moduleSpecial_, nlambdaSpecial_, orSpecial_, provideSpecial_, quasiquoteSpecial_, quoteSpecial_, requireSpecial_, returnSpecial_, sendApplySpecial_, sendSpecial_, setFieldSpecial_, setJsObjSpecial_, setSpecial_, setValuesSpecial_, setqSpecial_, throwSpecial_, trySpecial_, yieldSpecial_ };
