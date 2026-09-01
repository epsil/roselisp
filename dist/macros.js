"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.while_ = exports.when_ = exports.unwindProtect_ = exports.unless_ = exports.try_ = exports.threadLast_ = exports.threadFirst_ = exports.threadAs_ = exports.syntax_ = exports.setq_ = exports.set_ = exports.rktNew_ = exports.quasisyntax_ = exports.or_ = exports.newApply_ = exports.multipleValueBind_ = exports.match_ = exports.letEnv_ = exports.for_ = exports.elIf_ = exports.do_ = exports.defun_ = exports.defmacro_ = exports.defineSyntax_ = exports.definePublic_ = exports.definePrivate_ = exports.defineMacro_ = exports.defineMacroToLambdaForm = exports.defineMacroToFunction = exports.defineFexpr_ = exports.defclass_ = exports.declare_ = exports.declareMacro_ = exports.declareFexpr_ = exports.cljTry_ = exports.case_ = exports.caseEq_ = exports.begin0_ = exports.and_ = void 0;
const eval_1 = require("./eval");
const util_1 = require("./util");
const [listStar, setCarX] = (() => {
    function listStar_(...args) {
        if (args.length === 0) {
            return undefined;
        }
        else if (args.length === 1) {
            return args[0];
        }
        else {
            const tailLst = args.at(-1);
            const headLst = args.slice(0, -1);
            if (Array.isArray(tailLst)) {
                return [...headLst, ...tailLst];
            }
            else {
                return [...headLst, Symbol.for('.'), tailLst];
            }
        }
    }
    function setCarX_(x, y) {
        if (x.length > 0) {
            x[0] = y;
        }
        return undefined;
    }
    return [listStar_, setCarX_];
})();
/**
 * Expand a `(defun ...)` expression.
 *
 * Similar to [`defun` in Common Lisp][cl:defun] and
 * [`defun` in Emacs Lisp][el:defun].
 *
 * [cl:defun]: http://clhs.lisp.se/Body/m_defun.htm
 * [el:defun]: https://www.gnu.org/software/emacs/manual/html_node/eintr/defun.html
 */
function defun_(exp, env) {
    const [name, args, ...body] = exp.slice(1);
    return [Symbol.for('define'), [name, ...args], ...body];
}
exports.defun_ = defun_;
defun_.fsource = [Symbol.for('define'), [Symbol.for('defun_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name'), Symbol.for('args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('unquote-splicing'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
defun_.ftype = 'macro';
/**
 * Expand a `(define/private ...)` expression.
 */
function definePrivate_(exp, env) {
    const body = exp.slice(1);
    return [Symbol.for('define'), ...body];
}
exports.definePrivate_ = definePrivate_;
definePrivate_.fsource = [Symbol.for('define'), [Symbol.for('define-private_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
definePrivate_.ftype = 'macro';
/**
 * Expand a `(define/public ...)` expression.
 */
function definePublic_(exp, env) {
    const body = exp.slice(1);
    return [Symbol.for('define'), ...body];
}
exports.definePublic_ = definePublic_;
definePublic_.fsource = [Symbol.for('define'), [Symbol.for('define-public_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
definePublic_.ftype = 'macro';
/**
 * Expand a `(defclass ...)` expression.
 *
 * Similar to [`defclass` in Common Lisp][cl:defclass].
 *
 * [cl:defclass]: http://clhs.lisp.se/Body/m_defcla.htm
 */
function defclass_(exp, env) {
    const body = exp.slice(1);
    return [Symbol.for('define-class'), ...body];
}
exports.defclass_ = defclass_;
defclass_.fsource = [Symbol.for('define'), [Symbol.for('defclass_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define-class'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
defclass_.ftype = 'macro';
/**
 * Expand a `(define-macro ...)` expression.
 *
 * Similar to [`define-macro` in Guile][guile:define-macro] and
 * [`defmacro` in Common Lisp][cl:defmacro].
 *
 * [guile:define-macro]: https://www.gnu.org/software/guile/docs/docs-2.2/guile-ref/Defmacros.html
 * [cl:defmacro]: http://clhs.lisp.se/Body/m_defmac.htm#defmacro
 */
function defineMacro_(exp, env) {
    const [nameAndArgs, ...body] = exp.slice(1);
    const name = nameAndArgs[0];
    const macroFnForm = defineMacroToLambdaForm([Symbol.for('define-macro'), nameAndArgs, ...body]);
    const args = macroFnForm[1];
    const macroBody = macroFnForm.slice(2);
    return [Symbol.for('begin'), [Symbol.for('define'), [name, ...args], ...macroBody], [Symbol.for('declare-macro'), name]];
}
exports.defineMacro_ = defineMacro_;
defineMacro_.fsource = [Symbol.for('define'), [Symbol.for('define-macro_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name-and-args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('macro-fn-form'), [Symbol.for('define-macro->lambda-form'), [Symbol.for('quasiquote'), [Symbol.for('define-macro'), [Symbol.for('unquote'), Symbol.for('name-and-args')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('second'), Symbol.for('macro-fn-form')]], [Symbol.for('define'), Symbol.for('macro-body'), [Symbol.for('drop'), Symbol.for('macro-fn-form'), 2]], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('unquote-splicing'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), Symbol.for('macro-body')]], [Symbol.for('declare-macro'), [Symbol.for('unquote'), Symbol.for('name')]]]]];
defineMacro_.ftype = 'macro';
/**
 * Expand a `(syntax ...)` expression.
 */
function syntax_(exp, env) {
    const [v] = exp.slice(1);
    return [Symbol.for('datum->syntax'), false, [Symbol.for('quote'), v]];
}
exports.syntax_ = syntax_;
syntax_.fsource = [Symbol.for('define'), [Symbol.for('syntax_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('v')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('datum->syntax'), false, [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('v')]]]]];
syntax_.ftype = 'macro';
/**
 * Expand a `(quasisyntax ...)` expression.
 */
function quasisyntax_(exp, env) {
    const [v] = exp.slice(1);
    return [Symbol.for('datum->syntax'), false, [Symbol.for('quasiquote'), [Symbol.for('unquote'), Symbol.for('v')]]];
}
exports.quasisyntax_ = quasisyntax_;
quasisyntax_.fsource = [Symbol.for('define'), [Symbol.for('quasisyntax_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('v')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('datum->syntax'), false, [Symbol.for('quasiquote'), [Symbol.for('unquote'), Symbol.for('v')]]]]];
quasisyntax_.ftype = 'macro';
/**
 * Expand a `(define-syntax ...)` expression.
 */
function defineSyntax_(exp, env) {
    const [nameAndArgs, ...body] = exp.slice(1);
    const name = (typeof nameAndArgs === 'symbol') ? nameAndArgs : nameAndArgs[0];
    return [Symbol.for('begin'), [Symbol.for('define'), nameAndArgs, ...body], [Symbol.for('declare'), name, [Symbol.for('ftype'), [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')]]]];
}
exports.defineSyntax_ = defineSyntax_;
defineSyntax_.fsource = [Symbol.for('define'), [Symbol.for('define-syntax_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name-and-args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('if'), [Symbol.for('symbol?'), Symbol.for('name-and-args')], Symbol.for('name-and-args'), [Symbol.for('car'), Symbol.for('name-and-args')]]], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('name-and-args')], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('declare'), [Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('ftype'), [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')]]]]]];
defineSyntax_.ftype = 'macro';
/**
 * Create a macro function on the basis of a
 * `(define-macro ...)` expression.
 */
function defineMacroToFunction(exp, env) {
    const macroFn = defineMacroToLambdaForm(exp);
    return (0, eval_1.eval_)(macroFn, env);
}
exports.defineMacroToFunction = defineMacroToFunction;
defineMacroToFunction.fsource = [Symbol.for('define'), [Symbol.for('define-macro->function'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('macro-fn'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')]], [Symbol.for('eval_'), Symbol.for('macro-fn'), Symbol.for('env')]];
/**
 * Create a `(lambda ...)` form for a macro function
 * on the basis of a `(define-macro ...)` expression.
 */
function defineMacroToLambdaForm(exp, options = {}) {
    const nameAndArgs = exp[1];
    const name = nameAndArgs[0];
    const args = ((nameAndArgs.length === 3) && (nameAndArgs[1] === Symbol.for('.'))) ? nameAndArgs[2] : nameAndArgs.slice(1);
    const body = exp.slice(2);
    let expArg = options['exp'] || Symbol('exp');
    let envArg = options['env'] || Symbol('env');
    let macroArgs = [];
    let restArg = undefined;
    if (Array.isArray(args) && !((args.length >= 3) && (args.at(-2) === Symbol.for('.')) && !Array.isArray(args.at(-1)))) {
        let i = 0;
        while (i < args.length) {
            const arg = args[i];
            if (arg === Symbol.for('&rest')) {
                restArg = args[i + 1];
                i = i + 2;
            }
            else if (arg === Symbol.for('&whole')) {
                expArg = args[i + 1];
                i = i + 2;
            }
            else if (arg === Symbol.for('&environment')) {
                envArg = args[i + 1];
                i = i + 2;
            }
            else {
                macroArgs.push(arg);
                i++;
            }
        }
    }
    else {
        macroArgs = args;
    }
    if (restArg) {
        if (Array.isArray(macroArgs) && (macroArgs.length === 0)) {
            macroArgs = restArg;
        }
        else {
            macroArgs = listStar(...[...macroArgs, restArg]);
        }
    }
    return [Symbol.for('lambda'), [expArg, envArg], ...((Array.isArray(macroArgs) && (macroArgs.length === 0)) ? [] : [[Symbol.for('define-values'), macroArgs, [Symbol.for('rest'), expArg]]]), ...body];
}
exports.defineMacroToLambdaForm = defineMacroToLambdaForm;
defineMacroToLambdaForm.fsource = [Symbol.for('define'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('cdr'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('exp-arg'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':exp')], [Symbol.for('gensym'), 'exp']]], [Symbol.for('define'), Symbol.for('env-arg'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':env')], [Symbol.for('gensym'), 'env']]], [Symbol.for('define'), Symbol.for('macro-args'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-arg'), undefined], [Symbol.for('cond'), [[Symbol.for('list?'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('while'), [Symbol.for('<'), Symbol.for('i'), [Symbol.for('length'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&rest')]], [Symbol.for('set!'), Symbol.for('rest-arg'), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&whole')]], [Symbol.for('set!'), Symbol.for('exp-arg'), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&environment')]], [Symbol.for('set!'), Symbol.for('env-arg'), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('macro-args'), Symbol.for('arg')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('args')]]], [Symbol.for('when'), Symbol.for('rest-arg'), [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('rest-arg')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), [Symbol.for('apply'), Symbol.for('list*'), [Symbol.for('append'), Symbol.for('macro-args'), [Symbol.for('list'), Symbol.for('rest-arg')]]]]]]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [[Symbol.for('unquote'), Symbol.for('exp-arg')], [Symbol.for('unquote'), Symbol.for('env-arg')]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('quote'), []], [Symbol.for('quasiquote'), [[Symbol.for('define-values'), [Symbol.for('unquote'), Symbol.for('macro-args')], [Symbol.for('rest'), [Symbol.for('unquote'), Symbol.for('exp-arg')]]]]]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
/**
 * Expand a `(defmacro ...)` expression.
 *
 * Similar to [`defmacro` in Common Lisp][cl:defmacro]
 * and [`defmacro` in Emacs Lisp][el:defmacro].
 *
 * [cl:defmacro]: http://clhs.lisp.se/Body/m_defmac.htm
 * [el:defmacro]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Macros.html#index-defmacro
 */
function defmacro_(exp, env) {
    const [name, args, ...body] = exp.slice(1);
    return [Symbol.for('define-macro'), [name, ...(Array.isArray(args) ? args : [Symbol.for('.'), args])], ...body];
}
exports.defmacro_ = defmacro_;
defmacro_.fsource = [Symbol.for('define'), [Symbol.for('defmacro_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name'), Symbol.for('args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define-macro'), [Symbol.for('unquote'), [Symbol.for('cons'), Symbol.for('name'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
defmacro_.ftype = 'macro';
/**
 * Expand a `(define-fexpr ...)` expression.
 */
function defineFexpr_(exp, env) {
    const [nameAndArgs, ...body] = exp.slice(1);
    return [Symbol.for('begin'), [Symbol.for('define'), nameAndArgs, ...body], [Symbol.for('declare-fexpr'), nameAndArgs[0]]];
}
exports.defineFexpr_ = defineFexpr_;
defineFexpr_.fsource = [Symbol.for('define'), [Symbol.for('define-fexpr_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name-and-args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('name-and-args')], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('declare-fexpr'), [Symbol.for('unquote'), [Symbol.for('car'), Symbol.for('name-and-args')]]]]]];
defineFexpr_.ftype = 'macro';
/**
 * Expand a `(declare ...)` expression.
 *
 * Similar to [`declare` in Common Lisp] and
 * [`declare` in Emacs Lisp][el:declare].
 *
 * [cl:declare]: http://clhs.lisp.se/Body/s_declar.htm#declare
 * [el:declare]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Declare-Form.html
 */
function declare_(exp, env) {
    const [name, ...specs] = exp.slice(1);
    return [Symbol.for('begin'), ...specs.map(function (spec) {
            return [Symbol.for('set-field!'), spec[0], name, [Symbol.for('quote'), spec[1]]];
        })];
}
exports.declare_ = declare_;
declare_.fsource = [Symbol.for('define'), [Symbol.for('declare_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name'), Symbol.for('.'), Symbol.for('specs')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('spec')], [Symbol.for('quasiquote'), [Symbol.for('set-field!'), [Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('spec')]], [Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('spec')]]]]]], Symbol.for('specs')]]]]];
declare_.ftype = 'macro';
/**
 * Expand a `(declare-macro ...)` expression.
 */
function declareMacro_(exp, env) {
    const [name] = exp.slice(1);
    return [Symbol.for('declare'), name, [Symbol.for('ftype'), 'macro']];
}
exports.declareMacro_ = declareMacro_;
declareMacro_.fsource = [Symbol.for('define'), [Symbol.for('declare-macro_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('declare'), [Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('ftype'), 'macro']]]];
declareMacro_.ftype = 'macro';
/**
 * Expand a `(declare-fexpr ...)` expression.
 */
function declareFexpr_(exp, env) {
    const [name] = exp.slice(1);
    return [Symbol.for('declare'), name, [Symbol.for('ftype'), 'fexpr']];
}
exports.declareFexpr_ = declareFexpr_;
declareFexpr_.fsource = [Symbol.for('define'), [Symbol.for('declare-fexpr_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('declare'), [Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('ftype'), 'fexpr']]]];
declareFexpr_.ftype = 'macro';
/**
 * Expand a `(begin0 ...)` expression.
 *
 * Similar to [`begin0` in Racket] and
 * [`prog1` in Common Lisp][cl:prog1].
 *
 * [rkt:begin0]: https://docs.racket-lang.org/reference/begin.html#%28form._%28%28quote._~23~25kernel%29._begin0%29%29
 * [cl:prog1]: http://clhs.lisp.se/Body/m_prog1c.htm
 */
function begin0_(exp, env) {
    const [x, ...xs] = exp.slice(1);
    if (xs.length === 0) {
        return x;
    }
    else {
        let result = Symbol('begin0-result');
        return [Symbol.for('let'), [[result, x]], ...xs, result];
    }
}
exports.begin0_ = begin0_;
begin0_.fsource = [Symbol.for('define'), [Symbol.for('begin0_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('xs')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('length'), Symbol.for('xs')], 0], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('gensym'), 'begin0-result']], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('result')], [Symbol.for('unquote'), Symbol.for('x')]]], [Symbol.for('unquote-splicing'), Symbol.for('xs')], [Symbol.for('unquote'), Symbol.for('result')]]]]]];
begin0_.ftype = 'macro';
/**
 * Expand a `(multiple-value-bind ...)` expression.
 *
 * Similar to [`multiple-value-bind` in
 * Common Lisp][cl:multiple-value-bind].
 *
 * [cl:multiple-value-bind]: http://clhs.lisp.se/Body/m_multip.htm
 */
function multipleValueBind_(exp, env) {
    const [bindings, expression, ...body] = exp.slice(1);
    return [Symbol.for('let-values'), [[bindings, expression]], ...body];
}
exports.multipleValueBind_ = multipleValueBind_;
multipleValueBind_.fsource = [Symbol.for('define'), [Symbol.for('multiple-value-bind_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('bindings'), Symbol.for('expression'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('let-values'), [[[Symbol.for('unquote'), Symbol.for('bindings')], [Symbol.for('unquote'), Symbol.for('expression')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
multipleValueBind_.ftype = 'macro';
/**
 * Expand a `(rkt/new ...)' expression.
 *
 * Similar to [`new` in Racket][rkt:new].
 *
 * [rkt:new]: https://docs.racket-lang.org/reference/objcreation.html#%28form._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._new%29%29
 */
function rktNew_(exp, env) {
    const [constructor, ...args] = exp.slice(1);
    // We are not able to do much here other than to rewrite the
    // expression to a `(make-object ...)` expression. JavaScript lacks
    // support for creating a new object on the basis of by-name
    // initialization arguments; it only supports by-position
    // initialization arguments, which are passed to the constructor.
    return [Symbol.for('make-object'), constructor, ...args.map(function (x) {
            return x[1];
        })];
}
exports.rktNew_ = rktNew_;
rktNew_.fsource = [Symbol.for('define'), [Symbol.for('rkt/new_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('constructor'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('make-object'), [Symbol.for('unquote'), Symbol.for('constructor')], [Symbol.for('unquote-splicing'), [Symbol.for('map'), Symbol.for('second'), Symbol.for('args')]]]]];
rktNew_.ftype = 'macro';
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
function and_(exp, env) {
    const args = exp.slice(1);
    return [Symbol.for('js/&&'), ...args];
}
exports.and_ = and_;
and_.fsource = [Symbol.for('define'), [Symbol.for('and_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('js/&&'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];
and_.ftype = 'macro';
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
function or_(exp, env) {
    const args = exp.slice(1);
    return [Symbol.for('js/||'), ...args];
}
exports.or_ = or_;
or_.fsource = [Symbol.for('define'), [Symbol.for('or_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('js/||'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];
or_.ftype = 'macro';
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
function when_(exp, env) {
    const [condition, ...body] = exp.slice(1);
    return [Symbol.for('if'), condition, [Symbol.for('begin'), ...body]];
}
exports.when_ = when_;
when_.fsource = [Symbol.for('define'), [Symbol.for('when_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('condition'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('if'), [Symbol.for('unquote'), Symbol.for('condition')], [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]];
when_.ftype = 'macro';
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
function unless_(exp, env) {
    const [condition, ...body] = exp.slice(1);
    return [Symbol.for('if'), [Symbol.for('not'), condition], [Symbol.for('begin'), ...body]];
}
exports.unless_ = unless_;
unless_.fsource = [Symbol.for('define'), [Symbol.for('unless_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('condition'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('if'), [Symbol.for('not'), [Symbol.for('unquote'), Symbol.for('condition')]], [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]];
unless_.ftype = 'macro';
/**
 * Expand an `(el/if ...)` expression.
 *
 * Similar to [`if` in Emacs Lisp][el:if].
 *
 * [el:if]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html#index-if
 */
function elIf_(exp, env) {
    const [condExp, thenExp, ...elseExps] = exp.slice(1);
    // Emacs Lisp's `if` accepts more than three arguments.
    return [Symbol.for('if'), condExp, thenExp, ...((elseExps.length === 0) ? [] : ((elseExps.length === 1) ? [elseExps[0]] : [[Symbol.for('begin'), ...elseExps]]))];
}
exports.elIf_ = elIf_;
elIf_.fsource = [Symbol.for('define'), [Symbol.for('el/if_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('cond-exp'), Symbol.for('then-exp'), Symbol.for('.'), Symbol.for('else-exps')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('if'), [Symbol.for('unquote'), Symbol.for('cond-exp')], [Symbol.for('unquote'), Symbol.for('then-exp')], [Symbol.for('unquote-splicing'), [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('length'), Symbol.for('else-exps')], 0], [Symbol.for('quote'), []]], [[Symbol.for('='), [Symbol.for('length'), Symbol.for('else-exps')], 1], [Symbol.for('list'), [Symbol.for('first'), Symbol.for('else-exps')]]], [Symbol.for('else'), [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('else-exps')]]]]]]]]]];
elIf_.ftype = 'macro';
/**
 * Expand an `(as~> ...)` expression.
 *
 * Similar to the [`as->` macro][clj:thread-as] in Clojure.
 *
 * [clj:thread-as]: https://clojuredocs.org/clojure.core/as-%3E
 */
function threadAs_(exp, env) {
    let [val, sym, ...forms] = exp.slice(1);
    // This macro goes to some lengths to avoid introducing a `let`
    // variable unless it is absolutely necessary. In many cases, the
    // forms can simply be chained together, using `sym` as the
    // insertion point. A variable is needed only if `sym` occurs more
    // than once in the same form.
    // Whether a `let` variable has been defined.
    let isLet = false;
    // Reducer function.
    function f(form, exp) {
        if (isLet) {
            // If we are in the process of creating a `let` expression,
            // simply append a `set!` expression to it.
            return [...exp, [Symbol.for('set!'), sym, form]];
        }
        else {
            // Otherwise, count the occurrences of `sym` in the form
            // in order to determine what to do.
            const n = (0, util_1.countTree)(function (el) {
                return el === sym;
            }, form);
            if (n === 0) {
                // If `sym` occurs zero times in the form, create a
                // `begin` expression to chain things togethr.
                if ((0, util_1.taggedListP)(exp, Symbol.for('begin'))) {
                    // If chaining two `begin` expressions together,
                    // simply append the latter to the former.
                    return [...exp, form];
                }
                else {
                    return [Symbol.for('begin'), exp, form];
                }
            }
            else if (n === 1) {
                // If `sym` occurs exactly once in the form, chain it together
                // with the preceding expression, using `sym` as the insertion
                // point.
                return (0, util_1.mapTree)(function (x) {
                    if (x === sym) {
                        return exp;
                    }
                    else {
                        return x;
                    }
                }, form);
            }
            else {
                // If `sym` occurs more than once in the form, create a
                // `let` expression with `sym` as a variable.
                isLet = true;
                return [Symbol.for('let'), [[sym, exp]], [Symbol.for('set!'), sym, form]];
            }
        }
    }
    f.fsource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('form'), Symbol.for('exp')], [Symbol.for('cond'), [Symbol.for('is-let'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('exp')], [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('form')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('n'), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('el')], [Symbol.for('eq?'), Symbol.for('el'), Symbol.for('sym')]], Symbol.for('form')]], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('begin')]], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('form')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('form')]]]]]], [[Symbol.for('='), Symbol.for('n'), 1], [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('sym')], Symbol.for('exp'), Symbol.for('x')]], Symbol.for('form')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('is-let'), true], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('exp')]]], [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('form')]]]]]]]]];
    // Fold up `forms` left-to-right.
    let result = forms.reduce(function (acc, x) {
        return f(x, acc);
    }, val);
    // If a `let` expression was indeed created, add `sym` as
    // the final expression.
    if (isLet) {
        result = [...result, sym];
    }
    return result;
}
exports.threadAs_ = threadAs_;
threadAs_.fsource = [Symbol.for('define'), [Symbol.for('thread-as_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('val'), Symbol.for('sym'), Symbol.for('.'), Symbol.for('forms')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('is-let'), false], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('form'), Symbol.for('exp')], [Symbol.for('cond'), [Symbol.for('is-let'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('exp')], [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('form')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('n'), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('el')], [Symbol.for('eq?'), Symbol.for('el'), Symbol.for('sym')]], Symbol.for('form')]], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('begin')]], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('form')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('form')]]]]]], [[Symbol.for('='), Symbol.for('n'), 1], [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('sym')], Symbol.for('exp'), Symbol.for('x')]], Symbol.for('form')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('is-let'), true], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('exp')]]], [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('form')]]]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('val'), Symbol.for('forms')]], [Symbol.for('when'), Symbol.for('is-let'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('result')], [Symbol.for('unquote'), Symbol.for('sym')]]]]], Symbol.for('result')];
threadAs_.ftype = 'macro';
/**
 * Evaluate a `(~> ...)` expression. Based on the
 * [`->` macro][clj:thread-first] in Clojure (also known as
 * the "`thread-first` macro").
 *
 * [clj:thread-first]: https://clojuredocs.org/clojure.core/-%3E
 */
function threadFirst_(exp, env) {
    let [x, ...forms] = exp.slice(1);
    let holeMarker = Symbol.for('_');
    if ((forms.length > 1) && (forms[0] === Symbol.for(':hole-marker'))) {
        holeMarker = forms[1];
        forms = forms.slice(2);
    }
    function f(val, acc) {
        if (typeof val === 'symbol') {
            return [...acc, [val, Symbol.for('_')]];
        }
        else if ((0, util_1.countTree)(function (x) {
            return x === holeMarker;
        }, val) === 0) {
            return [...acc, [val[0], holeMarker, ...val.slice(1)]];
        }
        else {
            return [...acc, val];
        }
    }
    f.fsource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('val'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('val')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), Symbol.for('val')], Symbol.for('_')]]]], [[Symbol.for('='), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')]], Symbol.for('val')], 0], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('val')]], [Symbol.for('unquote'), Symbol.for('hole-marker')], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('val')]]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [Symbol.for('unquote'), Symbol.for('val')]]]]]];
    const asExp = [Symbol.for('as~>'), x, holeMarker];
    return forms.reduce(function (acc, x) {
        return f(x, acc);
    }, asExp);
}
exports.threadFirst_ = threadFirst_;
threadFirst_.fsource = [Symbol.for('define'), [Symbol.for('thread-first_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('forms')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('hole-marker'), [Symbol.for('quote'), Symbol.for('_')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('>'), [Symbol.for('length'), Symbol.for('forms')], 1], [Symbol.for('eq?'), [Symbol.for('first'), Symbol.for('forms')], [Symbol.for('quote'), Symbol.for(':hole-marker')]]], [Symbol.for('set!'), Symbol.for('hole-marker'), [Symbol.for('second'), Symbol.for('forms')]], [Symbol.for('set!'), Symbol.for('forms'), [Symbol.for('drop'), Symbol.for('forms'), 2]]], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('val'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('val')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), Symbol.for('val')], Symbol.for('_')]]]], [[Symbol.for('='), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')]], Symbol.for('val')], 0], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('val')]], [Symbol.for('unquote'), Symbol.for('hole-marker')], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('val')]]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [Symbol.for('unquote'), Symbol.for('val')]]]]]], [Symbol.for('define'), Symbol.for('as-exp'), [Symbol.for('quasiquote'), [Symbol.for('as~>'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('hole-marker')]]]], [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('as-exp'), Symbol.for('forms')]];
threadFirst_.ftype = 'macro';
/**
 * Evaluate a `(~>> ...)` expression. Based on the
 * [`->>` macro][clj:thread-last] in Clojure (also known as
 * the "`thread-last` macro").
 *
 * [clj:thread-last]: https://clojuredocs.org/clojure.core/-%3E%3E
 */
function threadLast_(exp, env) {
    let [x, ...forms] = exp.slice(1);
    let holeMarker = Symbol.for('_');
    if ((forms.length > 1) && (forms[0] === Symbol.for(':hole-marker'))) {
        holeMarker = forms[1];
        forms = forms.slice(2);
    }
    function f(val, acc) {
        if (typeof val === 'symbol') {
            return [...acc, [val, Symbol.for('_')]];
        }
        else if ((0, util_1.countTree)(function (x) {
            return x === holeMarker;
        }, val) === 0) {
            return [...acc, [...val, holeMarker]];
        }
        else {
            return [...acc, val];
        }
    }
    f.fsource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('val'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('val')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), Symbol.for('val')], Symbol.for('_')]]]], [[Symbol.for('='), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')]], Symbol.for('val')], 0], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote-splicing'), Symbol.for('val')], [Symbol.for('unquote'), Symbol.for('hole-marker')]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [Symbol.for('unquote'), Symbol.for('val')]]]]]];
    const asExp = [Symbol.for('as~>'), x, holeMarker];
    return forms.reduce(function (acc, x) {
        return f(x, acc);
    }, asExp);
}
exports.threadLast_ = threadLast_;
threadLast_.fsource = [Symbol.for('define'), [Symbol.for('thread-last_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('forms')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('hole-marker'), [Symbol.for('quote'), Symbol.for('_')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('>'), [Symbol.for('length'), Symbol.for('forms')], 1], [Symbol.for('eq?'), [Symbol.for('first'), Symbol.for('forms')], [Symbol.for('quote'), Symbol.for(':hole-marker')]]], [Symbol.for('set!'), Symbol.for('hole-marker'), [Symbol.for('second'), Symbol.for('forms')]], [Symbol.for('set!'), Symbol.for('forms'), [Symbol.for('drop'), Symbol.for('forms'), 2]]], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('val'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('val')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), Symbol.for('val')], Symbol.for('_')]]]], [[Symbol.for('='), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')]], Symbol.for('val')], 0], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote-splicing'), Symbol.for('val')], [Symbol.for('unquote'), Symbol.for('hole-marker')]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [Symbol.for('unquote'), Symbol.for('val')]]]]]], [Symbol.for('define'), Symbol.for('as-exp'), [Symbol.for('quasiquote'), [Symbol.for('as~>'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('hole-marker')]]]], [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('as-exp'), Symbol.for('forms')]];
threadLast_.ftype = 'macro';
/**
 * Expand an `(unwind-protect ...)` expression.
 *
 * Similar to [`unwind-protect` in Common Lisp][cl:unwind-protect]
 * and [`unwind-protect` in Emacs Lisp][el:unwind-protect]
 *
 * [cl:unwind-protect]: http://clhs.lisp.se/Body/s_unwind.htm
 * [el:unwind-protect]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Cleanups.html#index-unwind_002dprotect
 */
function unwindProtect_(exp, env) {
    const [bodyForm, ...unwindForms] = exp.slice(1);
    return [Symbol.for('try'), bodyForm, [Symbol.for('finally'), ...unwindForms]];
}
exports.unwindProtect_ = unwindProtect_;
unwindProtect_.fsource = [Symbol.for('define'), [Symbol.for('unwind-protect_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('body-form'), Symbol.for('.'), Symbol.for('unwind-forms')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('try'), [Symbol.for('unquote'), Symbol.for('body-form')], [Symbol.for('finally'), [Symbol.for('unquote-splicing'), Symbol.for('unwind-forms')]]]]];
unwindProtect_.ftype = 'macro';
/**
 * Expand a `(do ...)` expression.
 *
 * Similar to [`do` in Racket][rkt:do] and
 * [`do` in Guile][guile:do].
 *
 * [rkt:do]: https://docs.racket-lang.org/reference/for.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._do%29%29
 * [guile:do]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/while-do.html#index-do
 */
function do_(exp, env) {
    const [bindings, tests, ...body] = exp.slice(1);
    if (bindings.length === 0) {
        // For expressions with no bindings, we wrap
        // the expansion in `begin`.
        let result = [Symbol.for('begin'), [Symbol.for('while'), [Symbol.for('not'), tests[0]], ...body], ...tests.slice(1)];
        // If there is no finishing expression,
        // the code can be simplified further.
        if (result.length === 2) {
            result = result[1];
        }
        return result;
    }
    else {
        // For expressions with bindings, we wrap
        // the expansion in `let`.
        const letBindings = [];
        const setters = [];
        for (let binding of bindings) {
            letBindings.push(binding.slice(0, -(binding.length - 2) || undefined));
            if (binding.length === 3) {
                setters.push([Symbol.for('set!'), binding[0], binding[2]]);
            }
        }
        let result = [Symbol.for('let'), letBindings, [Symbol.for('while'), [Symbol.for('not'), tests[0]], ...body, ...setters], ...tests.slice(1)];
        return result;
    }
}
exports.do_ = do_;
do_.fsource = [Symbol.for('define'), [Symbol.for('do_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('bindings'), Symbol.for('tests'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('length'), Symbol.for('bindings')], 0], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('while'), [Symbol.for('not'), [Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('tests')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('tests'), 1]]]]], [Symbol.for('when'), [Symbol.for('='), [Symbol.for('length'), Symbol.for('result')], 2], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('second'), Symbol.for('result')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('setters'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('binding'), Symbol.for('bindings')]], [Symbol.for('push-right!'), Symbol.for('let-bindings'), [Symbol.for('take'), Symbol.for('binding'), 2]], [Symbol.for('when'), [Symbol.for('='), [Symbol.for('length'), Symbol.for('binding')], 3], [Symbol.for('push-right!'), Symbol.for('setters'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('binding')]], [Symbol.for('unquote'), [Symbol.for('third'), Symbol.for('binding')]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('let'), [Symbol.for('unquote'), Symbol.for('let-bindings')], [Symbol.for('while'), [Symbol.for('not'), [Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('tests')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')], [Symbol.for('unquote-splicing'), Symbol.for('setters')]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('tests'), 1]]]]], Symbol.for('result')]]];
do_.ftype = 'macro';
/**
 * Expand a `(while ...)` expression.
 *
 * Similar to [`while` in Guile][guile:while] and
 * [`while` in Emacs Lisp][el:while].
 *
 * [guile:while]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/while-do.html#index-while
 * [el:while]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Iteration.html#index-while
 */
function while_(exp, env) {
    const [test, ...body] = exp.slice(1);
    return [Symbol.for('js/while'), test, ...body];
}
exports.while_ = while_;
while_.fsource = [Symbol.for('define'), [Symbol.for('while_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('test'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('js/while'), [Symbol.for('unquote'), Symbol.for('test')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
while_.ftype = 'macro';
/**
 * Expand a `(for ...)` expression.
 *
 * Similar to [`for` in Racket][rkt:for].
 *
 * [rkt:for]: https://docs.racket-lang.org/reference/for.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._for%29%29
 */
function for_(exp, env) {
    const [args, ...body] = exp.slice(1);
    const [decl] = args;
    const [sym, val] = decl;
    if ((0, util_1.taggedListP)(val, Symbol.for('range'))) {
        const start = val[1];
        const end = val[2];
        const step = val[3] || 1;
        if (Array.isArray(start) || Array.isArray(end) || Array.isArray(step)) {
            // If `start`, `end` or `step` is a function call,
            // then rewrite the expression to a `let` expression
            // so that the function is called only once.
            const startVar = Array.isArray(start) ? Symbol('_start') : undefined;
            const endVar = Array.isArray(end) ? Symbol('_end') : undefined;
            const stepVar = Array.isArray(step) ? Symbol('_step') : undefined;
            return [Symbol.for('let'), [...(startVar ? [[startVar, start]] : []), ...(endVar ? [[endVar, end]] : []), ...(stepVar ? [[stepVar, step]] : [])], [Symbol.for('for'), [[sym, [Symbol.for('range'), startVar ? startVar : start, endVar ? endVar : end, stepVar ? stepVar : step]]], ...body]];
        }
        else {
            // Otherwise, proceed to create a `js/for` loop.
            const init = [sym, start];
            const test = Number.isFinite(step) ? ((step < 0) ? [Symbol.for('>'), sym, end] : [Symbol.for('<'), sym, end]) : [Symbol.for('if'), [Symbol.for('<'), step, 0], [Symbol.for('>'), sym, end], [Symbol.for('<'), sym, end]];
            const update = Number.isFinite(step) ? ((step < 0) ? [Symbol.for('-'), sym, Math.abs(step)] : [Symbol.for('+'), sym, step]) : [Symbol.for('+'), sym, step];
            return [Symbol.for('js/for'), [init, test, update], ...body];
        }
    }
    else {
        // If the loop cannot easily be expressed as a
        // `js/for` loop, create a `js/for-of` loop instead.
        return [Symbol.for('js/for-of'), args, ...body];
    }
}
exports.for_ = for_;
for_.fsource = [Symbol.for('define'), [Symbol.for('for_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define-values'), [Symbol.for('decl')], Symbol.for('args')], [Symbol.for('define-values'), [Symbol.for('sym'), Symbol.for('val')], Symbol.for('decl')], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('val'), [Symbol.for('quote'), Symbol.for('range')]], [Symbol.for('define'), Symbol.for('start'), [Symbol.for('second'), Symbol.for('val')]], [Symbol.for('define'), Symbol.for('end'), [Symbol.for('third'), Symbol.for('val')]], [Symbol.for('define'), Symbol.for('step'), [Symbol.for('or'), [Symbol.for('fourth'), Symbol.for('val')], 1]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('pair-or-list?'), Symbol.for('start')], [Symbol.for('pair-or-list?'), Symbol.for('end')], [Symbol.for('pair-or-list?'), Symbol.for('step')]], [Symbol.for('define'), Symbol.for('start-var'), [Symbol.for('if'), [Symbol.for('pair-or-list?'), Symbol.for('start')], [Symbol.for('gensym'), '_start'], undefined]], [Symbol.for('define'), Symbol.for('end-var'), [Symbol.for('if'), [Symbol.for('pair-or-list?'), Symbol.for('end')], [Symbol.for('gensym'), '_end'], undefined]], [Symbol.for('define'), Symbol.for('step-var'), [Symbol.for('if'), [Symbol.for('pair-or-list?'), Symbol.for('step')], [Symbol.for('gensym'), '_step'], undefined]], [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('unquote-splicing'), [Symbol.for('if'), Symbol.for('start-var'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('start-var')], [Symbol.for('unquote'), Symbol.for('start')]]]], [Symbol.for('quote'), []]]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), Symbol.for('end-var'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('end-var')], [Symbol.for('unquote'), Symbol.for('end')]]]], [Symbol.for('quote'), []]]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), Symbol.for('step-var'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('step-var')], [Symbol.for('unquote'), Symbol.for('step')]]]], [Symbol.for('quote'), []]]]], [Symbol.for('for'), [[[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('range'), [Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('start-var'), Symbol.for('start-var'), Symbol.for('start')]], [Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('end-var'), Symbol.for('end-var'), Symbol.for('end')]], [Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('step-var'), Symbol.for('step-var'), Symbol.for('step')]]]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('init'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('start')]]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('step')], [Symbol.for('if'), [Symbol.for('<'), Symbol.for('step'), 0], [Symbol.for('quasiquote'), [Symbol.for('>'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('end')]]], [Symbol.for('quasiquote'), [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('end')]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('if'), [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('step')], 0], [Symbol.for('>'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('end')]], [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('end')]]]]]]], [Symbol.for('define'), Symbol.for('update'), [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('step')], [Symbol.for('if'), [Symbol.for('<'), Symbol.for('step'), 0], [Symbol.for('quasiquote'), [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), [Symbol.for('abs'), Symbol.for('step')]]]], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('step')]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('step')]]]]]], [Symbol.for('quasiquote'), [Symbol.for('js/for'), [[Symbol.for('unquote'), Symbol.for('init')], [Symbol.for('unquote'), Symbol.for('test')], [Symbol.for('unquote'), Symbol.for('update')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('js/for-of'), [Symbol.for('unquote'), Symbol.for('args')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]];
for_.ftype = 'macro';
/**
 * Expand a `(case ...)` expression.
 *
 * Similar to [`case` in Racket][rkt:case].
 *
 * [rkt:case]: https://docs.racket-lang.org/reference/case.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._case%29%29
 */
function case_(exp, env) {
    const [val, ...clauses] = exp.slice(1);
    let hasComplexClauses = false;
    function simpleValueP(x) {
        return (typeof x === 'boolean') || Number.isFinite(x) || (typeof x === 'symbol') || (typeof x === 'string');
    }
    simpleValueP.fsource = [Symbol.for('define'), [Symbol.for('simple-value?'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('boolean?'), Symbol.for('x')], [Symbol.for('number?'), Symbol.for('x')], [Symbol.for('symbol?'), Symbol.for('x')], [Symbol.for('string?'), Symbol.for('x')]]];
    function complexValueP(x) {
        return !simpleValueP(x);
    }
    complexValueP.fsource = [Symbol.for('define'), [Symbol.for('complex-value?'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('simple-value?'), Symbol.for('x')]]];
    for (let x of clauses) {
        if ((x[0] !== Symbol.for('else')) && (x[0].findIndex(complexValueP) >= 0)) {
            hasComplexClauses = true;
            break;
        }
    }
    if (hasComplexClauses) {
        // Complex case: there is one or more regular clauses that contain
        // patterns that must be matched against with `equal?`, not `eq? `.
        const isComplexVal = typeof val !== 'symbol';
        const valueVar = isComplexVal ? Symbol('_value') : val;
        const condClauses = clauses.map(function (x) {
            if (x[0] === Symbol.for('else')) {
                return x;
            }
            else {
                return [[Symbol.for('member?'), valueVar, [Symbol.for('quote'), x[0]], Symbol.for('equal?')], ...x.slice(1)];
            }
        });
        let result = [Symbol.for('cond'), ...condClauses];
        // If `val` is a complex expression, we get a
        // `(let ... (cond ...))` form.
        if (isComplexVal) {
            result = [Symbol.for('let'), [[valueVar, val]], result];
        }
        return result;
    }
    else {
        // Simple case: all patterns can be matched against with `eq?`,
        // so translate the entire form to a `case/eq` form.
        return [Symbol.for('case/eq'), val, ...clauses];
    }
}
exports.case_ = case_;
case_.fsource = [Symbol.for('define'), [Symbol.for('case_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('val'), Symbol.for('.'), Symbol.for('clauses')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('has-complex-clauses'), false], [Symbol.for('define'), [Symbol.for('simple-value?'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('boolean?'), Symbol.for('x')], [Symbol.for('number?'), Symbol.for('x')], [Symbol.for('symbol?'), Symbol.for('x')], [Symbol.for('string?'), Symbol.for('x')]]], [Symbol.for('define'), [Symbol.for('complex-value?'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('simple-value?'), Symbol.for('x')]]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('clauses')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]]], [Symbol.for('memf?'), Symbol.for('complex-value?'), [Symbol.for('first'), Symbol.for('x')]]], [Symbol.for('set!'), Symbol.for('has-complex-clauses'), true], [Symbol.for('break')]]], [Symbol.for('cond'), [Symbol.for('has-complex-clauses'), [Symbol.for('define'), Symbol.for('is-complex-val'), [Symbol.for('not'), [Symbol.for('symbol?'), Symbol.for('val')]]], [Symbol.for('define'), Symbol.for('value-var'), [Symbol.for('if'), Symbol.for('is-complex-val'), [Symbol.for('gensym'), '_value'], Symbol.for('val')]], [Symbol.for('define'), Symbol.for('cond-clauses'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('member?'), [Symbol.for('unquote'), Symbol.for('value-var')], [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('x')]]], Symbol.for('equal?')], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('x')]]]]]]], Symbol.for('clauses')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('cond'), [Symbol.for('unquote-splicing'), Symbol.for('cond-clauses')]]]], [Symbol.for('when'), Symbol.for('is-complex-val'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('value-var')], [Symbol.for('unquote'), Symbol.for('val')]]], [Symbol.for('unquote'), Symbol.for('result')]]]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('case/eq'), [Symbol.for('unquote'), Symbol.for('val')], [Symbol.for('unquote-splicing'), Symbol.for('clauses')]]]]]];
case_.ftype = 'macro';
/**
 * Expand a `(case/eq ...)` expression.
 */
function caseEq_(exp, env) {
    const [val, ...clauses] = exp.slice(1);
    let hasComplexClauses = false;
    for (let x of clauses) {
        if ((x[0] !== Symbol.for('else')) && (x[0].length > 1)) {
            hasComplexClauses = true;
            break;
        }
    }
    if (hasComplexClauses) {
        // Complex case: there is one or more regular clauses that contain
        // multiple patterns. This is translatable to a `(cond ...)` form
        // that performs pattern matching.
        const isComplexVal = typeof val !== 'symbol';
        const valueVar = isComplexVal ? Symbol('_value') : val;
        const condClauses = clauses.map(function (x) {
            if (x[0] === Symbol.for('else')) {
                return x;
            }
            else {
                return [[Symbol.for('member?'), valueVar, [Symbol.for('quote'), x[0]]], ...x.slice(1)];
            }
        });
        let result = [Symbol.for('cond'), ...condClauses];
        // If `val` is a complex expression, we get a
        // `(let ... (cond ...))` form.
        if (isComplexVal) {
            result = [Symbol.for('let'), [[valueVar, val]], result];
        }
        return result;
    }
    else {
        // Simple case: each regular clause contains exactly one pattern.
        // This is translatable to a `(js/swith ...)` form.
        const switchClauses = clauses.map(function (x) {
            if (x[0] === Symbol.for('else')) {
                return [Symbol.for('default'), ...x.slice(1)];
            }
            else {
                return [Symbol.for('case'), [Symbol.for('quote'), x[0][0]], ...x.slice(1), [Symbol.for('break')]];
            }
        });
        return [Symbol.for('js/switch'), val, ...switchClauses];
    }
}
exports.caseEq_ = caseEq_;
caseEq_.fsource = [Symbol.for('define'), [Symbol.for('case-eq_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('val'), Symbol.for('.'), Symbol.for('clauses')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('has-complex-clauses'), false], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('clauses')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]]], [Symbol.for('>'), [Symbol.for('length'), [Symbol.for('first'), Symbol.for('x')]], 1]], [Symbol.for('set!'), Symbol.for('has-complex-clauses'), true], [Symbol.for('break')]]], [Symbol.for('cond'), [Symbol.for('has-complex-clauses'), [Symbol.for('define'), Symbol.for('is-complex-val'), [Symbol.for('not'), [Symbol.for('symbol?'), Symbol.for('val')]]], [Symbol.for('define'), Symbol.for('value-var'), [Symbol.for('if'), Symbol.for('is-complex-val'), [Symbol.for('gensym'), '_value'], Symbol.for('val')]], [Symbol.for('define'), Symbol.for('cond-clauses'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('member?'), [Symbol.for('unquote'), Symbol.for('value-var')], [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('x')]]]], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('x')]]]]]]], Symbol.for('clauses')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('cond'), [Symbol.for('unquote-splicing'), Symbol.for('cond-clauses')]]]], [Symbol.for('when'), Symbol.for('is-complex-val'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('value-var')], [Symbol.for('unquote'), Symbol.for('val')]]], [Symbol.for('unquote'), Symbol.for('result')]]]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('switch-clauses'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]], [Symbol.for('quasiquote'), [Symbol.for('default'), [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('x')]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('case'), [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('first'), [Symbol.for('first'), Symbol.for('x')]]]], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('x')]], [Symbol.for('break')]]]]]], Symbol.for('clauses')]], [Symbol.for('quasiquote'), [Symbol.for('js/switch'), [Symbol.for('unquote'), Symbol.for('val')], [Symbol.for('unquote-splicing'), Symbol.for('switch-clauses')]]]]]];
caseEq_.ftype = 'macro';
/**
 * Expand a `(let-env ...)` expression.
 */
function letEnv_(exp, env) {
    const [x, ...body] = exp.slice(1);
    return [Symbol.for('scm/eval'), [Symbol.for('quote'), [Symbol.for('begin'), ...body]], [Symbol.for('extend-environment'), x, [Symbol.for('current-environment')]]];
}
exports.letEnv_ = letEnv_;
letEnv_.fsource = [Symbol.for('define'), [Symbol.for('let-env_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('scm/eval'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]], [Symbol.for('extend-environment'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('current-environment')]]]]];
letEnv_.ftype = 'macro';
/**
 * Expand a `(set ...)` expression.
 *
 * Similar to [`set` in Common Lisp][cl:set] and
 * [`set` in Emacs Lisp][el:set].
 *
 * [cl:set]: http://clhs.lisp.se/Body/f_set.htm
 * [el:set]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-set
 */
function set_(exp, env) {
    const [sym, val] = exp.slice(1);
    return [Symbol.for('set!'), sym[1], val];
}
exports.set_ = set_;
set_.fsource = [Symbol.for('define'), [Symbol.for('set_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('sym'), Symbol.for('val')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('sym')]], [Symbol.for('unquote'), Symbol.for('val')]]]];
set_.ftype = 'macro';
/**
 * Expand a `(setq ...)` expression.
 *
 * Similar to [`setq` in Common Lisp][cl:setq]
 * and [`setq` in Emacs Lisp][el:setq].
 *
 * [cl:setq]: http://clhs.lisp.se/Body/s_setq.htm#setq
 * [el:setq]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-setq
 */
function setq_(exp, env) {
    const bindings = exp.slice(1);
    const bindings1 = [];
    const _end = bindings.length;
    for (let i = 0; i < _end; i = i + 2) {
        const sym = bindings[i];
        const val = bindings[i + 1];
        bindings1.push([Symbol.for('set!'), sym, val]);
    }
    if (bindings1.length === 1) {
        return bindings1[0];
    }
    else {
        return [Symbol.for('begin'), ...bindings1];
    }
}
exports.setq_ = setq_;
setq_.fsource = [Symbol.for('define'), [Symbol.for('setq_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('bindings'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('bindings1'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('bindings')], 2]]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('list-ref'), Symbol.for('bindings'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('list-ref'), Symbol.for('bindings'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('push-right!'), Symbol.for('bindings1'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('val')]]]]], [Symbol.for('if'), [Symbol.for('='), [Symbol.for('length'), Symbol.for('bindings1')], 1], [Symbol.for('first'), Symbol.for('bindings1')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('bindings1')]]]]];
setq_.ftype = 'macro';
/**
 * Expand a `(new/apply ...)` expression.
 */
function newApply_(exp, env) {
    const args = exp.slice(1);
    return [Symbol.for('apply'), Symbol.for('new'), ...args];
}
exports.newApply_ = newApply_;
newApply_.fsource = [Symbol.for('define'), [Symbol.for('new/apply_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('apply'), Symbol.for('new'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];
newApply_.ftype = 'macro';
/**
 * Expand a `(try ...)` expression.
 *
 * Similar to the [`try` special form][clj:try] in Clojure.
 *
 * [clj:try]: https://clojuredocs.org/clojure.core/try
 */
function try_(exp, env) {
    const body = exp.slice(1);
    return [Symbol.for('clj/try'), ...body];
}
exports.try_ = try_;
try_.fsource = [Symbol.for('define'), [Symbol.for('try_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('clj/try'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
try_.ftype = 'macro';
/**
 * Expand a `(clj/try ...)` expression.
 *
 * Similar to the [`try` special form][clj:try] in Clojure.
 *
 * [clj:try]: https://clojuredocs.org/clojure.core/try
 */
function cljTry_(exp, env) {
    const body = exp.slice(1);
    const bodyExps = [];
    let catchClauses = [];
    const cljCatchClauses = [];
    const finalizerClauses = [];
    for (let x of body) {
        if ((0, util_1.taggedListP)(x, Symbol.for('catch'))) {
            cljCatchClauses.push(x);
        }
        else if ((0, util_1.taggedListP)(x, Symbol.for('finally'))) {
            finalizerClauses.push(x);
        }
        else {
            bodyExps.push(x);
        }
    }
    if (cljCatchClauses.length > 0) {
        const exception = cljCatchClauses[0][1];
        const sym = cljCatchClauses[0][2];
        if ((cljCatchClauses.length === 1) && [Symbol.for('_'), Symbol.for('js/Object'), Symbol.for('Object'), Symbol.for('object%')].includes(exception)) {
            const cljCatchClause = cljCatchClauses[0];
            const catchClause = [Symbol.for('catch'), sym, ...cljCatchClause.slice(3)];
            catchClauses = [catchClause];
        }
        else {
            const condExp = [Symbol.for('cond'), ...cljCatchClauses.map(function (x) {
                    return [[Symbol.for('is-a?'), sym, x[1]], ...x.slice(3)];
                }), [Symbol.for('else'), [Symbol.for('throw'), sym]]];
            const catchClause = [Symbol.for('catch'), sym, condExp];
            catchClauses = [catchClause];
        }
    }
    return [Symbol.for('js/try'), ...bodyExps, ...catchClauses, ...finalizerClauses];
}
exports.cljTry_ = cljTry_;
cljTry_.fsource = [Symbol.for('define'), [Symbol.for('clj/try_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('body-exps'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('catch-clauses'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('clj-catch-clauses'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('finalizer-clauses'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('body')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('catch')]], [Symbol.for('push-right!'), Symbol.for('clj-catch-clauses'), Symbol.for('x')]], [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('finally')]], [Symbol.for('push-right!'), Symbol.for('finalizer-clauses'), Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('body-exps'), Symbol.for('x')]]]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('length'), Symbol.for('clj-catch-clauses')], 0], [Symbol.for('define'), Symbol.for('exception'), [Symbol.for('second'), [Symbol.for('first'), Symbol.for('clj-catch-clauses')]]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('third'), [Symbol.for('first'), Symbol.for('clj-catch-clauses')]]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('length'), Symbol.for('clj-catch-clauses')], 1], [Symbol.for('memq?'), Symbol.for('exception'), [Symbol.for('quote'), [Symbol.for('_'), Symbol.for('js/Object'), Symbol.for('Object'), Symbol.for('object%')]]]], [Symbol.for('define'), Symbol.for('clj-catch-clause'), [Symbol.for('first'), Symbol.for('clj-catch-clauses')]], [Symbol.for('define'), Symbol.for('catch-clause'), [Symbol.for('quasiquote'), [Symbol.for('catch'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('clj-catch-clause'), 3]]]]], [Symbol.for('set!'), Symbol.for('catch-clauses'), [Symbol.for('list'), Symbol.for('catch-clause')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('cond-exp'), [Symbol.for('quasiquote'), [Symbol.for('cond'), [Symbol.for('unquote-splicing'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('quasiquote'), [[Symbol.for('is-a?'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('x')]]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('x'), 3]]]]], Symbol.for('clj-catch-clauses')]], [Symbol.for('else'), [Symbol.for('throw'), [Symbol.for('unquote'), Symbol.for('sym')]]]]]], [Symbol.for('define'), Symbol.for('catch-clause'), [Symbol.for('quasiquote'), [Symbol.for('catch'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('cond-exp')]]]], [Symbol.for('set!'), Symbol.for('catch-clauses'), [Symbol.for('list'), Symbol.for('catch-clause')]]]]], [Symbol.for('quasiquote'), [Symbol.for('js/try'), [Symbol.for('unquote-splicing'), Symbol.for('body-exps')], [Symbol.for('unquote-splicing'), Symbol.for('catch-clauses')], [Symbol.for('unquote-splicing'), Symbol.for('finalizer-clauses')]]]];
cljTry_.ftype = 'macro';
/**
 * Expand a `(match ...)` expression.
 *
 * Similar to [`match` in Racket] and, to a lesser extent,
 * [`match` in Guile][guile:match].
 *
 * [rkt:match]: https://docs.racket-lang.org/reference/match.html#%28form._%28%28lib._racket%2Fmatch..rkt%29._match%29%29
 * [guile:match]: https://doc.guix.gnu.org/guile/latest/en/html_node/Pattern-Matching.html#index-match
 */
function match_(exp1, env) {
    const [exp, ...clauses] = exp1.slice(1);
    function patternBind(pat, exp) {
        if (pat === Symbol.for('_')) {
            return [];
        }
        else if (typeof pat === 'symbol') {
            return [[Symbol.for('define'), pat, exp]];
        }
        else if (Array.isArray(pat)) {
            if (Array.isArray(pat) && (pat.length === 0)) {
                return [];
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('quote'))) {
                return [];
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('var'))) {
                return [[Symbol.for('define'), pat[1], exp]];
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('cons'))) {
                return patternBind([Symbol.for('list*'), ...pat.slice(1)], exp);
            }
            else if ((0, util_1.taggedListP)(pat, [Symbol.for('list'), Symbol.for('list*')])) {
                return [[Symbol.for('define-values'), (0, util_1.listExpressionToPattern)(pat), exp]];
            }
            else {
                return [];
            }
        }
        else {
            return [];
        }
    }
    patternBind.fsource = [Symbol.for('define'), [Symbol.for('pattern-bind'), Symbol.for('pat'), Symbol.for('exp')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('_')]], [Symbol.for('quote'), []]], [[Symbol.for('symbol?'), Symbol.for('pat')], [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('pat')], [Symbol.for('unquote'), Symbol.for('exp')]]]]], [[Symbol.for('pair-or-list?'), Symbol.for('pat')], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('pat')], [Symbol.for('quote'), []]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('quote')]], [Symbol.for('quote'), []]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('var')]], [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('pat')]], [Symbol.for('unquote'), Symbol.for('exp')]]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('cons')]], [Symbol.for('pattern-bind'), [Symbol.for('quasiquote'), [Symbol.for('list*'), [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('pat')]]]], Symbol.for('exp')]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), [Symbol.for('list'), Symbol.for('list*')]]], [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote'), [Symbol.for('list-expression->pattern'), Symbol.for('pat')]], [Symbol.for('unquote'), Symbol.for('exp')]]]]], [Symbol.for('else'), [Symbol.for('quote'), []]]]], [Symbol.for('else'), [Symbol.for('quote'), []]]]];
    function patternMatch(pat, exp, makeLet = true) {
        if (makeLet && Array.isArray(exp)) {
            const patternMatchVal = Symbol('pattern-match-val');
            return [Symbol.for('let'), [[patternMatchVal, exp]], patternMatch(pat, patternMatchVal)];
        }
        else if (typeof pat === 'symbol') {
            return true;
        }
        else if (Array.isArray(pat)) {
            if (Array.isArray(pat) && (pat.length === 0)) {
                return [Symbol.for('null?'), exp];
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('quote'))) {
                return [Array.isArray(pat[1]) ? Symbol.for('equal?') : Symbol.for('eq?'), exp, pat];
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('var'))) {
                return true;
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('not'))) {
                return [Symbol.for('not'), patternMatch(pat[1], exp, false)];
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('and'))) {
                return combineExpressions([Symbol.for('and')], ...pat.slice(1).map(function (x) {
                    return patternMatch(x, exp, false);
                }));
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('or'))) {
                return combineExpressions([Symbol.for('or')], ...pat.slice(1).map(function (x) {
                    return patternMatch(x, exp, false);
                }));
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('cons'))) {
                return patternMatch([Symbol.for('list*'), ...pat.slice(1)], exp, false);
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('list'))) {
                if (pat.at(-1) === Symbol.for('...')) {
                    const head = pat.slice(1).slice(0, -2);
                    const tail = pat[pat.length - 2];
                    const pat1 = [Symbol.for('list*'), ...head, tail];
                    return patternMatch(pat1, exp, false);
                }
                else {
                    const len = pat.length - 1;
                    let result = [Symbol.for('and'), [Symbol.for('pair-or-list?'), exp], [Symbol.for('='), [Symbol.for('length'), exp], len]];
                    const _end = pat.length;
                    for (let i = 1; i < _end; i++) {
                        const pat1 = pat[i];
                        const exp1 = [Symbol.for('list-ref'), exp, i - 1];
                        const result1 = patternMatch(pat1, exp1, false);
                        result = combineExpressions(result, result1);
                    }
                    return result;
                }
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('list*'))) {
                const head = pat.slice(1).slice(0, -1);
                const tail = pat.at(-1);
                const len = head.length;
                let result = [Symbol.for('and'), [Symbol.for('pair-or-list?'), exp], [Symbol.for('>='), [Symbol.for('length'), exp], head.length]];
                const _end1 = head.length;
                for (let i = 0; i < _end1; i++) {
                    const pat1 = head[i];
                    const exp1 = [Symbol.for('list-ref'), exp, i];
                    const result1 = patternMatch(pat1, exp1, false);
                    result = combineExpressions(result, result1);
                }
                const exp2 = [Symbol.for('drop'), exp, len];
                const result2 = patternMatch(tail, exp2, false);
                result = combineExpressions(result, result2);
                return result;
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('regexp'))) {
                return [Symbol.for('regexp-match'), pat, exp];
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('?'))) {
                return combineExpressions([Symbol.for('and')], [pat[1], exp], ...pat.slice(2).map(function (x) {
                    return patternMatch(x, exp, false);
                }));
            }
            else if ((0, util_1.taggedListP)(pat, Symbol.for('app'))) {
                const pats = pat.slice(2);
                const exp1 = [pat[1], exp];
                if (pats.length === 1) {
                    return patternMatch(pats[0], exp1, false);
                }
                else {
                    return patternMatch([Symbol.for('and'), ...pats], exp1);
                }
            }
            else {
                return false;
            }
        }
        else {
            return [Symbol.for('eq?'), exp, pat];
        }
    }
    patternMatch.fsource = [Symbol.for('define'), [Symbol.for('pattern-match'), Symbol.for('pat'), Symbol.for('exp'), [Symbol.for('make-let'), true]], [Symbol.for('cond'), [[Symbol.for('and'), Symbol.for('make-let'), [Symbol.for('pair-or-list?'), Symbol.for('exp')]], [Symbol.for('let'), [[Symbol.for('pattern-match-val'), [Symbol.for('gensym'), 'pattern-match-val']]], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('pattern-match-val')], [Symbol.for('unquote'), Symbol.for('exp')]]], [Symbol.for('unquote'), [Symbol.for('pattern-match'), Symbol.for('pat'), Symbol.for('pattern-match-val')]]]]]], [[Symbol.for('symbol?'), Symbol.for('pat')], true], [[Symbol.for('pair-or-list?'), Symbol.for('pat')], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('pat')], [Symbol.for('quasiquote'), [Symbol.for('null?'), [Symbol.for('unquote'), Symbol.for('exp')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('quote')]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), [Symbol.for('pair-or-list?'), [Symbol.for('second'), Symbol.for('pat')]], [Symbol.for('quote'), Symbol.for('equal?')], [Symbol.for('quote'), Symbol.for('eq?')]]], [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('pat')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('var')]], true], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('not')]], [Symbol.for('quasiquote'), [Symbol.for('not'), [Symbol.for('unquote'), [Symbol.for('pattern-match'), [Symbol.for('second'), Symbol.for('pat')], Symbol.for('exp'), false]]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('and')]], [Symbol.for('apply'), Symbol.for('combine-expressions'), [Symbol.for('quote'), [Symbol.for('and')]], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('pattern-match'), Symbol.for('x'), Symbol.for('exp'), false]], [Symbol.for('rest'), Symbol.for('pat')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('or')]], [Symbol.for('apply'), Symbol.for('combine-expressions'), [Symbol.for('quote'), [Symbol.for('or')]], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('pattern-match'), Symbol.for('x'), Symbol.for('exp'), false]], [Symbol.for('rest'), Symbol.for('pat')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('cons')]], [Symbol.for('pattern-match'), [Symbol.for('quasiquote'), [Symbol.for('list*'), [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('pat')]]]], Symbol.for('exp'), false]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('list')]], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('last'), Symbol.for('pat')], [Symbol.for('quote'), Symbol.for('...')]], [Symbol.for('define'), Symbol.for('head'), [Symbol.for('~>'), [Symbol.for('drop'), Symbol.for('pat'), 1], [Symbol.for('drop-right'), Symbol.for('_'), 2]]], [Symbol.for('define'), Symbol.for('tail'), [Symbol.for('list-ref'), Symbol.for('pat'), [Symbol.for('-'), [Symbol.for('length'), Symbol.for('pat')], 2]]], [Symbol.for('define'), Symbol.for('pat1'), [Symbol.for('quasiquote'), [Symbol.for('list*'), [Symbol.for('unquote-splicing'), Symbol.for('head')], [Symbol.for('unquote'), Symbol.for('tail')]]]], [Symbol.for('pattern-match'), Symbol.for('pat1'), Symbol.for('exp'), false]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('len'), [Symbol.for('-'), [Symbol.for('length'), Symbol.for('pat')], 1]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('and'), [Symbol.for('pair-or-list?'), [Symbol.for('unquote'), Symbol.for('exp')]], [Symbol.for('='), [Symbol.for('length'), [Symbol.for('unquote'), Symbol.for('exp')]], [Symbol.for('unquote'), Symbol.for('len')]]]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('pat')]]]], [Symbol.for('define'), Symbol.for('pat1'), [Symbol.for('list-ref'), Symbol.for('pat'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('exp1'), [Symbol.for('quasiquote'), [Symbol.for('list-ref'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), [Symbol.for('-'), Symbol.for('i'), 1]]]]], [Symbol.for('define'), Symbol.for('result1'), [Symbol.for('pattern-match'), Symbol.for('pat1'), Symbol.for('exp1'), false]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('combine-expressions'), Symbol.for('result'), Symbol.for('result1')]]], Symbol.for('result')]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('list*')]], [Symbol.for('define'), Symbol.for('head'), [Symbol.for('~>'), [Symbol.for('drop'), Symbol.for('pat'), 1], [Symbol.for('drop-right'), Symbol.for('_'), 1]]], [Symbol.for('define'), Symbol.for('tail'), [Symbol.for('last'), Symbol.for('pat')]], [Symbol.for('define'), Symbol.for('len'), [Symbol.for('length'), Symbol.for('head')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('and'), [Symbol.for('pair-or-list?'), [Symbol.for('unquote'), Symbol.for('exp')]], [Symbol.for('>='), [Symbol.for('length'), [Symbol.for('unquote'), Symbol.for('exp')]], [Symbol.for('unquote'), [Symbol.for('length'), Symbol.for('head')]]]]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('head')]]]], [Symbol.for('define'), Symbol.for('pat1'), [Symbol.for('list-ref'), Symbol.for('head'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('exp1'), [Symbol.for('quasiquote'), [Symbol.for('list-ref'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('i')]]]], [Symbol.for('define'), Symbol.for('result1'), [Symbol.for('pattern-match'), Symbol.for('pat1'), Symbol.for('exp1'), false]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('combine-expressions'), Symbol.for('result'), Symbol.for('result1')]]], [Symbol.for('define'), Symbol.for('exp2'), [Symbol.for('quasiquote'), [Symbol.for('drop'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('len')]]]], [Symbol.for('define'), Symbol.for('result2'), [Symbol.for('pattern-match'), Symbol.for('tail'), Symbol.for('exp2'), false]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('combine-expressions'), Symbol.for('result'), Symbol.for('result2')]], Symbol.for('result')], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('regexp')]], [Symbol.for('quasiquote'), [Symbol.for('regexp-match'), [Symbol.for('unquote'), Symbol.for('pat')], [Symbol.for('unquote'), Symbol.for('exp')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('?')]], [Symbol.for('apply'), Symbol.for('combine-expressions'), [Symbol.for('quote'), [Symbol.for('and')]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('pat')]], [Symbol.for('unquote'), Symbol.for('exp')]]], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('pattern-match'), Symbol.for('x'), Symbol.for('exp'), false]], [Symbol.for('drop'), Symbol.for('pat'), 2]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('app')]], [Symbol.for('define'), Symbol.for('pats'), [Symbol.for('drop'), Symbol.for('pat'), 2]], [Symbol.for('define'), Symbol.for('exp1'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('pat')]], [Symbol.for('unquote'), Symbol.for('exp')]]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('length'), Symbol.for('pats')], 1], [Symbol.for('pattern-match'), [Symbol.for('first'), Symbol.for('pats')], Symbol.for('exp1'), false]], [Symbol.for('else'), [Symbol.for('pattern-match'), [Symbol.for('quasiquote'), [Symbol.for('and'), [Symbol.for('unquote-splicing'), Symbol.for('pats')]]], Symbol.for('exp1')]]]], [Symbol.for('else'), false]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('eq?'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('pat')]]]]]];
    function combineExpressions(...exps) {
        return exps.slice(1).reduce(function (acc, x) {
            if (!Array.isArray(acc)) {
                return acc;
            }
            else if ((0, util_1.taggedListP)(x, Symbol.for('and'))) {
                for (let x1 of x.slice(1)) {
                    acc.push(x1);
                }
                return acc;
            }
            else if (Array.isArray(x)) {
                acc.push(x);
                return acc;
            }
            else if ((x === true) && (0, util_1.taggedListP)(acc, Symbol.for('or'))) {
                return true;
            }
            else if ((x === false) && (0, util_1.taggedListP)(acc, Symbol.for('and'))) {
                return false;
            }
            else {
                return acc;
            }
        }, exps[0]);
    }
    combineExpressions.fsource = [Symbol.for('define'), [Symbol.for('combine-expressions'), Symbol.for('.'), Symbol.for('exps')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('pair-or-list?'), Symbol.for('acc')]], Symbol.for('acc')], [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('and')]], [Symbol.for('for'), [[Symbol.for('x1'), [Symbol.for('rest'), Symbol.for('x')]]], [Symbol.for('push-right!'), Symbol.for('acc'), Symbol.for('x1')]], Symbol.for('acc')], [[Symbol.for('pair-or-list?'), Symbol.for('x')], [Symbol.for('push-right!'), Symbol.for('acc'), Symbol.for('x')], Symbol.for('acc')], [[Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('x'), true], [Symbol.for('tagged-list?'), Symbol.for('acc'), [Symbol.for('quote'), Symbol.for('or')]]], true], [[Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('x'), false], [Symbol.for('tagged-list?'), Symbol.for('acc'), [Symbol.for('quote'), Symbol.for('and')]]], false], [Symbol.for('else'), Symbol.for('acc')]]], [Symbol.for('first'), Symbol.for('exps')], [Symbol.for('rest'), Symbol.for('exps')]]];
    if (Array.isArray(exp)) {
        const matchVal = Symbol('match-val');
        return [Symbol.for('let'), [[matchVal, exp]], [Symbol.for('match'), matchVal, ...clauses]];
    }
    else {
        const condClauses = clauses.map(function (x) {
            const pat = x[0];
            const body = x.slice(1);
            return [patternMatch(pat, exp), ...patternBind(pat, exp), ...body];
        });
        const lastCondClause = condClauses.at(-1);
        if (lastCondClause[0] === true) {
            setCarX(lastCondClause, Symbol.for('else'));
        }
        return [Symbol.for('cond'), ...condClauses];
    }
}
exports.match_ = match_;
match_.fsource = [Symbol.for('define'), [Symbol.for('match_'), Symbol.for('exp1'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('exp'), Symbol.for('.'), Symbol.for('clauses')], [Symbol.for('rest'), Symbol.for('exp1')]], [Symbol.for('define'), [Symbol.for('pattern-bind'), Symbol.for('pat'), Symbol.for('exp')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('_')]], [Symbol.for('quote'), []]], [[Symbol.for('symbol?'), Symbol.for('pat')], [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('pat')], [Symbol.for('unquote'), Symbol.for('exp')]]]]], [[Symbol.for('pair-or-list?'), Symbol.for('pat')], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('pat')], [Symbol.for('quote'), []]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('quote')]], [Symbol.for('quote'), []]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('var')]], [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('pat')]], [Symbol.for('unquote'), Symbol.for('exp')]]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('cons')]], [Symbol.for('pattern-bind'), [Symbol.for('quasiquote'), [Symbol.for('list*'), [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('pat')]]]], Symbol.for('exp')]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), [Symbol.for('list'), Symbol.for('list*')]]], [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote'), [Symbol.for('list-expression->pattern'), Symbol.for('pat')]], [Symbol.for('unquote'), Symbol.for('exp')]]]]], [Symbol.for('else'), [Symbol.for('quote'), []]]]], [Symbol.for('else'), [Symbol.for('quote'), []]]]], [Symbol.for('define'), [Symbol.for('pattern-match'), Symbol.for('pat'), Symbol.for('exp'), [Symbol.for('make-let'), true]], [Symbol.for('cond'), [[Symbol.for('and'), Symbol.for('make-let'), [Symbol.for('pair-or-list?'), Symbol.for('exp')]], [Symbol.for('let'), [[Symbol.for('pattern-match-val'), [Symbol.for('gensym'), 'pattern-match-val']]], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('pattern-match-val')], [Symbol.for('unquote'), Symbol.for('exp')]]], [Symbol.for('unquote'), [Symbol.for('pattern-match'), Symbol.for('pat'), Symbol.for('pattern-match-val')]]]]]], [[Symbol.for('symbol?'), Symbol.for('pat')], true], [[Symbol.for('pair-or-list?'), Symbol.for('pat')], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('pat')], [Symbol.for('quasiquote'), [Symbol.for('null?'), [Symbol.for('unquote'), Symbol.for('exp')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('quote')]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), [Symbol.for('pair-or-list?'), [Symbol.for('second'), Symbol.for('pat')]], [Symbol.for('quote'), Symbol.for('equal?')], [Symbol.for('quote'), Symbol.for('eq?')]]], [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('pat')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('var')]], true], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('not')]], [Symbol.for('quasiquote'), [Symbol.for('not'), [Symbol.for('unquote'), [Symbol.for('pattern-match'), [Symbol.for('second'), Symbol.for('pat')], Symbol.for('exp'), false]]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('and')]], [Symbol.for('apply'), Symbol.for('combine-expressions'), [Symbol.for('quote'), [Symbol.for('and')]], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('pattern-match'), Symbol.for('x'), Symbol.for('exp'), false]], [Symbol.for('rest'), Symbol.for('pat')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('or')]], [Symbol.for('apply'), Symbol.for('combine-expressions'), [Symbol.for('quote'), [Symbol.for('or')]], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('pattern-match'), Symbol.for('x'), Symbol.for('exp'), false]], [Symbol.for('rest'), Symbol.for('pat')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('cons')]], [Symbol.for('pattern-match'), [Symbol.for('quasiquote'), [Symbol.for('list*'), [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('pat')]]]], Symbol.for('exp'), false]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('list')]], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('last'), Symbol.for('pat')], [Symbol.for('quote'), Symbol.for('...')]], [Symbol.for('define'), Symbol.for('head'), [Symbol.for('~>'), [Symbol.for('drop'), Symbol.for('pat'), 1], [Symbol.for('drop-right'), Symbol.for('_'), 2]]], [Symbol.for('define'), Symbol.for('tail'), [Symbol.for('list-ref'), Symbol.for('pat'), [Symbol.for('-'), [Symbol.for('length'), Symbol.for('pat')], 2]]], [Symbol.for('define'), Symbol.for('pat1'), [Symbol.for('quasiquote'), [Symbol.for('list*'), [Symbol.for('unquote-splicing'), Symbol.for('head')], [Symbol.for('unquote'), Symbol.for('tail')]]]], [Symbol.for('pattern-match'), Symbol.for('pat1'), Symbol.for('exp'), false]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('len'), [Symbol.for('-'), [Symbol.for('length'), Symbol.for('pat')], 1]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('and'), [Symbol.for('pair-or-list?'), [Symbol.for('unquote'), Symbol.for('exp')]], [Symbol.for('='), [Symbol.for('length'), [Symbol.for('unquote'), Symbol.for('exp')]], [Symbol.for('unquote'), Symbol.for('len')]]]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('length'), Symbol.for('pat')]]]], [Symbol.for('define'), Symbol.for('pat1'), [Symbol.for('list-ref'), Symbol.for('pat'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('exp1'), [Symbol.for('quasiquote'), [Symbol.for('list-ref'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), [Symbol.for('-'), Symbol.for('i'), 1]]]]], [Symbol.for('define'), Symbol.for('result1'), [Symbol.for('pattern-match'), Symbol.for('pat1'), Symbol.for('exp1'), false]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('combine-expressions'), Symbol.for('result'), Symbol.for('result1')]]], Symbol.for('result')]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('list*')]], [Symbol.for('define'), Symbol.for('head'), [Symbol.for('~>'), [Symbol.for('drop'), Symbol.for('pat'), 1], [Symbol.for('drop-right'), Symbol.for('_'), 1]]], [Symbol.for('define'), Symbol.for('tail'), [Symbol.for('last'), Symbol.for('pat')]], [Symbol.for('define'), Symbol.for('len'), [Symbol.for('length'), Symbol.for('head')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('and'), [Symbol.for('pair-or-list?'), [Symbol.for('unquote'), Symbol.for('exp')]], [Symbol.for('>='), [Symbol.for('length'), [Symbol.for('unquote'), Symbol.for('exp')]], [Symbol.for('unquote'), [Symbol.for('length'), Symbol.for('head')]]]]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('head')]]]], [Symbol.for('define'), Symbol.for('pat1'), [Symbol.for('list-ref'), Symbol.for('head'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('exp1'), [Symbol.for('quasiquote'), [Symbol.for('list-ref'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('i')]]]], [Symbol.for('define'), Symbol.for('result1'), [Symbol.for('pattern-match'), Symbol.for('pat1'), Symbol.for('exp1'), false]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('combine-expressions'), Symbol.for('result'), Symbol.for('result1')]]], [Symbol.for('define'), Symbol.for('exp2'), [Symbol.for('quasiquote'), [Symbol.for('drop'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('len')]]]], [Symbol.for('define'), Symbol.for('result2'), [Symbol.for('pattern-match'), Symbol.for('tail'), Symbol.for('exp2'), false]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('combine-expressions'), Symbol.for('result'), Symbol.for('result2')]], Symbol.for('result')], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('regexp')]], [Symbol.for('quasiquote'), [Symbol.for('regexp-match'), [Symbol.for('unquote'), Symbol.for('pat')], [Symbol.for('unquote'), Symbol.for('exp')]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('?')]], [Symbol.for('apply'), Symbol.for('combine-expressions'), [Symbol.for('quote'), [Symbol.for('and')]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('pat')]], [Symbol.for('unquote'), Symbol.for('exp')]]], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('pattern-match'), Symbol.for('x'), Symbol.for('exp'), false]], [Symbol.for('drop'), Symbol.for('pat'), 2]]]], [[Symbol.for('tagged-list?'), Symbol.for('pat'), [Symbol.for('quote'), Symbol.for('app')]], [Symbol.for('define'), Symbol.for('pats'), [Symbol.for('drop'), Symbol.for('pat'), 2]], [Symbol.for('define'), Symbol.for('exp1'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('pat')]], [Symbol.for('unquote'), Symbol.for('exp')]]]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('length'), Symbol.for('pats')], 1], [Symbol.for('pattern-match'), [Symbol.for('first'), Symbol.for('pats')], Symbol.for('exp1'), false]], [Symbol.for('else'), [Symbol.for('pattern-match'), [Symbol.for('quasiquote'), [Symbol.for('and'), [Symbol.for('unquote-splicing'), Symbol.for('pats')]]], Symbol.for('exp1')]]]], [Symbol.for('else'), false]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('eq?'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('pat')]]]]]], [Symbol.for('define'), [Symbol.for('combine-expressions'), Symbol.for('.'), Symbol.for('exps')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('pair-or-list?'), Symbol.for('acc')]], Symbol.for('acc')], [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('and')]], [Symbol.for('for'), [[Symbol.for('x1'), [Symbol.for('rest'), Symbol.for('x')]]], [Symbol.for('push-right!'), Symbol.for('acc'), Symbol.for('x1')]], Symbol.for('acc')], [[Symbol.for('pair-or-list?'), Symbol.for('x')], [Symbol.for('push-right!'), Symbol.for('acc'), Symbol.for('x')], Symbol.for('acc')], [[Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('x'), true], [Symbol.for('tagged-list?'), Symbol.for('acc'), [Symbol.for('quote'), Symbol.for('or')]]], true], [[Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('x'), false], [Symbol.for('tagged-list?'), Symbol.for('acc'), [Symbol.for('quote'), Symbol.for('and')]]], false], [Symbol.for('else'), Symbol.for('acc')]]], [Symbol.for('first'), Symbol.for('exps')], [Symbol.for('rest'), Symbol.for('exps')]]], [Symbol.for('cond'), [[Symbol.for('pair-or-list?'), Symbol.for('exp')], [Symbol.for('let'), [[Symbol.for('match-val'), [Symbol.for('gensym'), 'match-val']]], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('match-val')], [Symbol.for('unquote'), Symbol.for('exp')]]], [Symbol.for('match'), [Symbol.for('unquote'), Symbol.for('match-val')], [Symbol.for('unquote-splicing'), Symbol.for('clauses')]]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('cond-clauses'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('pat'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('x')]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('pattern-match'), Symbol.for('pat'), Symbol.for('exp')]], [Symbol.for('unquote-splicing'), [Symbol.for('pattern-bind'), Symbol.for('pat'), Symbol.for('exp')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]], Symbol.for('clauses')]], [Symbol.for('define'), Symbol.for('last-cond-clause'), [Symbol.for('last'), Symbol.for('cond-clauses')]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('first'), Symbol.for('last-cond-clause')], true], [Symbol.for('set-car!'), Symbol.for('last-cond-clause'), [Symbol.for('quote'), Symbol.for('else')]]], [Symbol.for('quasiquote'), [Symbol.for('cond'), [Symbol.for('unquote-splicing'), Symbol.for('cond-clauses')]]]]]];
match_.ftype = 'macro';
