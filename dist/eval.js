"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Evaluator
 *
 * S-expression evaluator and ESTree evaluator.
 *
 * ## Description
 *
 * Implements two evaluators: one for S-expressions (raw or wrapped
 * in rose trees) and one for ESTree nodes.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.jsEval_ = exports.evaluatorp = exports.eval_ = exports.eval1 = exports.evalSexp = exports.evalRose = exports.evalEstree = exports.defaultEvaluator = exports.callEvaluator = exports.Evaluator = exports.seval = void 0;
const curry_1 = require("./curry");
const estree_1 = require("./estree");
const env_1 = require("./env");
const exception_1 = require("./exception");
const javascript_1 = require("./javascript");
Object.defineProperty(exports, "jsEval_", { enumerable: true, get: function () { return javascript_1.jsEval_; } });
const rose_1 = require("./rose");
const [keywordp, lastCdr, cons, fexprp] = (() => {
    function keywordp_(obj) {
        return (typeof obj === 'symbol') && obj.description.match(new RegExp('^:'));
    }
    function lastCdr_(lst) {
        if (!Array.isArray(lst)) {
            return undefined;
        }
        else if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
            let result = lst;
            while (Array.isArray(result) && (result.length >= 3) && (result[result.length - 2] === Symbol.for('.'))) {
                result = result[result.length - 1];
            }
            return result;
        }
        else {
            return [];
        }
    }
    function cons_(x, y) {
        if (Array.isArray(y)) {
            return [x, ...y];
        }
        else {
            return [x, Symbol.for('.'), y];
        }
    }
    function fexprp_(obj) {
        return (obj instanceof Function) && obj.fexpr;
    }
    return [keywordp_, lastCdr_, cons_, fexprp_];
})();
/**
 * The default evaluator.
 */
const defaultEvaluator = eval1;
exports.defaultEvaluator = defaultEvaluator;
/**
 * Evaluate a S-expression `exp` in the Lisp environment `env`.
 *
 * `env` may be an {@link Environment}, or it may be an
 * {@link Evaluator}. In the latter case, evaluation takes place
 * by calling its `.eval()` method.
 *
 * This function is more fundamental and basic than `interpret`, which
 * is probably what you want. `interpret` stacks the given environment
 * on top of a Lisp environment, while `eval_` is a low-level function
 * that performs no such stacking.
 */
const eval_ = (0, curry_1.dashify)(function (exp, env = undefined, options = {}) {
    const evaluator = options['evaluator'] || defaultEvaluator;
    return callEvaluator(evaluator, exp, env, options);
});
exports.seval = eval_;
exports.eval_ = eval_;
/**
 * Call an evaluator on an expression.
 */
function callEvaluator(evaluator, exp, env = undefined, options = {}) {
    if (evaluator instanceof Evaluator) {
        return evaluator.eval(exp, env, options);
    }
    else if (evaluator instanceof Function) {
        return evaluator(exp, env, options);
    }
    else {
        return undefined;
    }
}
exports.callEvaluator = callEvaluator;
callEvaluator.lispSource = [Symbol.for('define'), [Symbol.for('call-evaluator'), Symbol.for('evaluator'), Symbol.for('exp'), [Symbol.for('env'), Symbol.for('undefined')], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('evaluator'), Symbol.for('Evaluator')], [Symbol.for('send'), Symbol.for('evaluator'), Symbol.for('eval'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('procedure?'), Symbol.for('evaluator')], [Symbol.for('evaluator'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), Symbol.for('undefined')]]];
/**
 * Whether something is an evaluator.
 */
function evaluatorp(obj) {
    return (obj instanceof Function) || (obj instanceof Evaluator);
}
exports.evaluatorp = evaluatorp;
evaluatorp.lispSource = [Symbol.for('define'), [Symbol.for('evaluator?'), Symbol.for('obj')], [Symbol.for('or'), [Symbol.for('procedure?'), Symbol.for('obj')], [Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('Evaluator')]]];
/**
 * Evaluator class.
 */
class Evaluator {
    /**
     * The simplest possible evaluator is the
     * identity function.
     */
    eval(exp, env = undefined, options = {}) {
        return exp;
    }
}
exports.Evaluator = Evaluator;
/**
 * Lisp-1 evaluator function.
 */
function eval1(exp, env, options = {}) {
    if (exp instanceof rose_1.Rose) {
        return evalRose(exp, env, options);
    }
    else {
        return evalSexp(exp, env, options);
    }
}
exports.eval1 = eval1;
eval1.lispSource = [Symbol.for('define'), [Symbol.for('eval1'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('eval-rose'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('eval-sexp'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Evaluate an S-expression.
 *
 * This is a case-by-case function.
 */
function evalSexp(exp, env, options = {}) {
    return (0, env_1.withEnvironment)(env, function () {
        if (Array.isArray(exp) && (exp.length === 0)) {
            return exp;
        }
        else if ((() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) {
            const [op, ...args] = exp;
            if (typeof op === 'symbol') {
                const name = op.description;
                let match;
                if ((match = name.match(new RegExp('^\\.(.+)$')))) {
                    // Method call expression
                    const method = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && (() => {
                        const x = lastCdr(match);
                        return Array.isArray(x) && (x.length === 0);
                    })()) ? (() => {
                        let i = 1;
                        let result = match;
                        while (i > 0) {
                            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                result = match[match.length - 1];
                            }
                            else {
                                result = match.slice(1);
                            }
                            i--;
                        }
                        if (Array.isArray(result)) {
                            result = result[0];
                        }
                        return result;
                    })() : match[1];
                    const [obj, ...fargs] = args;
                    const dotExp = [Symbol.for('.'), obj, Symbol.for(method), ...fargs];
                    return evalSexp(dotExp, env, options);
                }
                else {
                    const [f, bindingType] = env.getTypedValue(op);
                    if (bindingType === 'macro') {
                        // Macros are implemented with a macro function that
                        // has the signature `(exp, env) => value`. The arguments
                        // to the macro are *not* evaluated, but the macro's
                        // return value---the macro expansion---*is* evaluated:
                        // that is, it is pushed back on the expressions stack
                        // for further evaluation.
                        const expansion = f(exp, env);
                        return evalSexp(expansion, env, options);
                    }
                    else if (bindingType === 'fexpr') {
                        // A fexpr is a function that receives its arguments
                        // unevaluated, like a macro. However, unlike a macro,
                        // the return value is not re-evaluated---it is simply
                        // returned.
                        return f(...args);
                    }
                    else if (bindingType === 'special') {
                        // Special form
                        return f(exp, env);
                    }
                    else if (['function', 'procedure'].includes(bindingType) || ((bindingType === 'variable') && (f instanceof Function))) {
                        // Function call
                        if (fexprp(f)) {
                            return f(...args);
                        }
                        else if (f.lispMacro) {
                            // Macro function
                            const expansion = f(exp, env);
                            return evalSexp(expansion, env, options);
                        }
                        else {
                            // Apply `f` to evaluated arguments
                            return f(...args.map(function (arg) {
                                return evalSexp(arg, env, options);
                            }));
                        }
                    }
                }
            }
            else if (!op) {
                return undefined;
            }
            else if (op instanceof Function) {
                // `(<fn> ...)` call. The first element is a
                // function object. If it is a fexpr call, the function
                // is called with its arguments unevaluated. Otherwise,
                // the arguments have to be evaluated first.
                const f = op;
                if ((args.length === 0) || fexprp(f)) {
                    // Fexpr call. The function is called with its
                    // arguments unevaluated.
                    return f(...args);
                }
                else {
                    // Regular call. The arguments are evaluated,
                    // and the values are passed to the function.
                    return f(...args.map(function (arg) {
                        return evalSexp(arg, env, options);
                    }));
                }
            }
            else {
                // `((...) ...)` call. The first element is a expression
                // that has to be evaluated before function application
                // can proceed.
                return evalSexp(cons(evalSexp(op, env, options), args), env, options);
            }
        }
        else if (keywordp(exp)) {
            // Keyword
            return exp;
        }
        else if (typeof exp === 'symbol') {
            // Variable
            const name = exp.description;
            const binding = env.getTypedValue(exp);
            if (binding) {
                const [value] = binding;
                return value;
            }
            else {
                throw new Error('Could not find symbol: ' + exp.description);
            }
        }
        else if (typeof exp === 'string') {
            // String
            return exp;
        }
        else if ((0, estree_1.estreep)(exp)) {
            // ESTree
            return evalEstree(exp, env);
        }
        else {
            // Self-evaluating value
            return exp;
        }
    });
}
exports.evalSexp = evalSexp;
evalSexp.lispSource = [Symbol.for('define'), [Symbol.for('eval-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('with-environment'), Symbol.for('env'), [Symbol.for('lambda'), [], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('exp')], Symbol.for('exp')], [[Symbol.for('list?'), Symbol.for('exp')], [Symbol.for('define-values'), [Symbol.for('op'), Symbol.for('.'), Symbol.for('args')], Symbol.for('exp')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('op')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('symbol->string'), Symbol.for('op')]], [Symbol.for('define'), Symbol.for('match')], [Symbol.for('cond'), [[Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\.(.+)$'], Symbol.for('name')]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('define-values'), [Symbol.for('obj'), Symbol.for('.'), Symbol.for('fargs')], Symbol.for('args')], [Symbol.for('define'), Symbol.for('dot-exp'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('string->symbol'), '.']], [Symbol.for('unquote'), Symbol.for('obj')], [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('method')]], [Symbol.for('unquote-splicing'), Symbol.for('fargs')]]]], [Symbol.for('eval-sexp'), Symbol.for('dot-exp'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('binding-type')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-typed-value'), Symbol.for('op')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('binding-type'), 'macro'], [Symbol.for('define'), Symbol.for('expansion'), [Symbol.for('f'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('eval-sexp'), Symbol.for('expansion'), Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('eq?'), Symbol.for('binding-type'), 'fexpr'], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [[Symbol.for('eq?'), Symbol.for('binding-type'), 'special'], [Symbol.for('f'), Symbol.for('exp'), Symbol.for('env')]], [[Symbol.for('or'), [Symbol.for('memq?'), Symbol.for('binding-type'), [Symbol.for('quote'), ['function', 'procedure']]], [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('binding-type'), 'variable'], [Symbol.for('procedure?'), Symbol.for('f')]]], [Symbol.for('cond'), [[Symbol.for('fexpr?'), Symbol.for('f')], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [[Symbol.for('get-field'), Symbol.for('lispMacro'), Symbol.for('f')], [Symbol.for('define'), Symbol.for('expansion'), [Symbol.for('f'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('eval-sexp'), Symbol.for('expansion'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('apply'), Symbol.for('f'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('arg')], [Symbol.for('eval-sexp'), Symbol.for('arg'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('args')]]]]]]]]], [[Symbol.for('not'), Symbol.for('op')], Symbol.for('undefined')], [[Symbol.for('procedure?'), Symbol.for('op')], [Symbol.for('define'), Symbol.for('f'), Symbol.for('op')], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 0], [Symbol.for('fexpr?'), Symbol.for('f')]], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('apply'), Symbol.for('f'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('arg')], [Symbol.for('eval-sexp'), Symbol.for('arg'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('args')]]]]], [Symbol.for('else'), [Symbol.for('eval-sexp'), [Symbol.for('cons'), [Symbol.for('eval-sexp'), Symbol.for('op'), Symbol.for('env'), Symbol.for('options')], Symbol.for('args')], Symbol.for('env'), Symbol.for('options')]]]], [[Symbol.for('keyword?'), Symbol.for('exp')], Symbol.for('exp')], [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('symbol->string'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('binding'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-typed-value'), Symbol.for('exp')]], [Symbol.for('cond'), [Symbol.for('binding'), [Symbol.for('define-values'), [Symbol.for('value')], Symbol.for('binding')], Symbol.for('value')], [Symbol.for('else'), [Symbol.for('error'), [Symbol.for('string-append'), 'Could not find symbol: ', [Symbol.for('symbol->string'), Symbol.for('exp')]]]]]], [[Symbol.for('string?'), Symbol.for('exp')], Symbol.for('exp')], [[Symbol.for('estree?'), Symbol.for('exp')], [Symbol.for('eval-estree'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('else'), Symbol.for('exp')]]]]];
/**
 * Evaluate an S-expression wrapped in a rose tree.
 */
function evalRose(node, env, options = {}) {
    return evalSexp(node.getValue(), env, options);
}
exports.evalRose = evalRose;
evalRose.lispSource = [Symbol.for('define'), [Symbol.for('eval-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('eval-sexp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')], Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an [ESTree][github:estree] node
 * (i.e., a JavaScript [AST][w:Abstract syntax tree]).
 *
 * [github:estree]: https://github.com/estree/estree
 * [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
 */
function evalEstree(node, env, options = {}) {
    const type_ = (0, estree_1.estreeType)(node);
    const evaluator = evalEstreeMap.get(type_);
    if (evaluator) {
        return (0, env_1.withEnvironment)(env, function () {
            return evaluator(node, env, options);
        });
    }
    else {
        return undefined;
    }
}
exports.evalEstree = evalEstree;
evalEstree.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('estree-type'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('evaluator'), [Symbol.for('send'), Symbol.for('eval-estree-map'), Symbol.for('get'), Symbol.for('type_')]], [Symbol.for('cond'), [Symbol.for('evaluator'), [Symbol.for('with-environment'), Symbol.for('env'), [Symbol.for('lambda'), [], [Symbol.for('evaluator'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), Symbol.for('undefined')]]];
/**
 * Evaluate an ESTree [`Program`][estree:program] node
 * (i.e., a JavaScript program).
 *
 * [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
 */
function evalEstreeProgram(node, env, options = {}) {
    const body = node.body;
    let result = undefined;
    for (let statement of body) {
        result = evalEstree(statement, env, options);
    }
    return result;
}
evalEstreeProgram.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-program'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('for'), [[Symbol.for('statement'), Symbol.for('body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval-estree'), Symbol.for('statement'), Symbol.for('env'), Symbol.for('options')]]], Symbol.for('result')];
/**
 * Evaluate an ESTree [`BlockStatement`][estree:blockstatement] node.
 *
 * [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
 */
function evalEstreeBlockStatement(node, env, options = {}) {
    const env1 = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), env);
    return (0, env_1.withEnvironment)(env1, () => {
        try {
        }
        catch (e) {
            if (e instanceof Error) {
            }
            else {
                throw e;
            }
        }
        return evalEstreeProgram(node, env1, options);
    });
}
evalEstreeBlockStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-block-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('env1'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('env')]], [Symbol.for('with-environment'), Symbol.for('env1'), [Symbol.for('js/arrow'), [], [Symbol.for('try'), [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e')]], [Symbol.for('eval-estree-program'), Symbol.for('node'), Symbol.for('env1'), Symbol.for('options')]]]];
/**
 * Evaluate an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
 *
 * [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
 */
function evalEstreeSequenceExpression(node, env, options = {}) {
    const expressions = node.expressions;
    const program = new estree_1.Program(expressions);
    return evalEstreeProgram(program, env, options);
}
evalEstreeSequenceExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-sequence-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expressions'), [Symbol.for('get-field'), Symbol.for('expressions'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('program'), [Symbol.for('new'), Symbol.for('Program'), Symbol.for('expressions')]], [Symbol.for('eval-estree-program'), Symbol.for('program'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an ESTree [`Literal`][estree:literal] node.
 *
 * [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
 */
function evalEstreeLiteral(node, env, options = {}) {
    return node.value;
}
evalEstreeLiteral.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-literal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]];
/**
 * Evaluate an ESTree [`Identifier`][estree:identifier] node.
 *
 * [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
 */
function evalEstreeIdentifier(node, env, options = {}) {
    const name = node.name;
    return env.get(Symbol.for(name));
}
evalEstreeIdentifier.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-identifier'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('node')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), [Symbol.for('string->symbol'), Symbol.for('name')]]];
/**
 * Evaluate an ESTree [`MemberExpression`][estree:memberexpression] node.
 *
 * [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
 */
function evalEstreeMemberExpression(node, env, options = {}) {
    const object = node.object;
    const property = node.property;
    const objectVal = evalEstree(object, env, options);
    const propertyVal = (0, estree_1.estreeTypeP)(property, 'Identifier') ? property.name : evalEstree(property, env, options);
    return objectVal[propertyVal];
}
evalEstreeMemberExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-member-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('object'), [Symbol.for('get-field'), Symbol.for('object'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('property'), [Symbol.for('get-field'), Symbol.for('property'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('object-val'), [Symbol.for('eval-estree'), Symbol.for('object'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('property-val'), [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('property'), 'Identifier'], [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('property')]], [Symbol.for('else'), [Symbol.for('eval-estree'), Symbol.for('property'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('oget'), Symbol.for('object-val'), Symbol.for('property-val')]];
/**
 * Evaluate an ESTree [`CallExpression`][estree:callexpression] node.
 *
 * [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
 */
function evalEstreeCallExpression(node, env, options = {}) {
    // TODO: Handle `js/eval` calls directly?
    const callee = node.callee;
    const args = node.arguments;
    const argsVals = evalEstreeArrayExpressionHelper(args, env, options);
    if ((0, estree_1.estreeTypeP)(callee, 'MemberExpression')) {
        const obj = callee.object;
        const objVal = evalEstree(obj, env, options);
        const methodVal = evalEstree(callee, env, options);
        return methodVal.apply(objVal, argsVals);
    }
    else {
        const f = callee;
        const fVal = evalEstree(f, env, options);
        let result = fVal(...argsVals);
        return result;
    }
}
evalEstreeCallExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-call-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('get-field'), Symbol.for('callee'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('arguments'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('args-vals'), [Symbol.for('eval-estree-array-expression-helper'), Symbol.for('args'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('callee'), 'MemberExpression'], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('get-field'), Symbol.for('object'), Symbol.for('callee')]], [Symbol.for('define'), Symbol.for('obj-val'), [Symbol.for('eval-estree'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('method-val'), [Symbol.for('eval-estree'), Symbol.for('callee'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('method-val'), Symbol.for('apply'), Symbol.for('obj-val'), Symbol.for('args-vals')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('f'), Symbol.for('callee')], [Symbol.for('define'), Symbol.for('f-val'), [Symbol.for('eval-estree'), Symbol.for('f'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('apply'), Symbol.for('f-val'), Symbol.for('args-vals')]], Symbol.for('result')]]];
/**
 * Evaluate an ESTree [`BreakStatement`][estree:breakstatement] node.
 *
 * [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
 */
function evalEstreeBreakStatement(node, env, options = {}) {
    throw new exception_1.BreakException();
}
evalEstreeBreakStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-break-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('BreakException')]]];
/**
 * Evaluate an ESTree [`ContinueStatement`][estree:continuestatement] node.
 *
 * [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
 */
function evalEstreeContinueStatement(node, env, options = {}) {
    throw new exception_1.ContinueException();
}
evalEstreeContinueStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-continue-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('ContinueException')]]];
/**
 * Evaluate an ESTree [`YieldExpression`][estree:yieldexpression] node.
 *
 * [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
 */
function evalEstreeYieldExpression(node, env, options = {}) {
    const argument = node.argument;
    const argumentVal = evalEstree(argument, env, options);
    throw new exception_1.YieldException(argumentVal);
}
evalEstreeYieldExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-yield-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('argument-val'), [Symbol.for('eval-estree'), Symbol.for('argument'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('YieldException'), Symbol.for('argument-val')]]];
/**
 * Evaluate an ESTree [`ReturnStatement`][estree:returnstatement] node.
 *
 * [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
 */
function evalEstreeReturnStatement(node, env, options = {}) {
    const argument = node.argument;
    const argumentVal = evalEstree(argument, env, options);
    throw new exception_1.ReturnException(argumentVal);
}
evalEstreeReturnStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-return-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('argument-val'), [Symbol.for('eval-estree'), Symbol.for('argument'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('ReturnException'), Symbol.for('argument-val')]]];
/**
 * Evaluate an ESTree [`ThrowStatement`][estree:throwstatement] node.
 *
 * [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
 */
function evalEstreeThrowStatement(node, env, options = {}) {
    const argument = node.argument;
    const argumentVal = evalEstree(argument, env, options);
    throw argumentVal;
}
evalEstreeThrowStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-throw-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('argument-val'), [Symbol.for('eval-estree'), Symbol.for('argument'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('throw'), Symbol.for('argument-val')]];
/**
 * Evaluate an ESTree [`ThisExpression`][estree:thisexpression] node.
 *
 * [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
 */
function evalEstreeThisExpression(node, env, options = {}) {
    return currentThisValue;
}
evalEstreeThisExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-this-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], Symbol.for('current-this-value')];
/**
 * Evaluate an ESTree [`NewExpression`][estree:newexpression] node.
 *
 * [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
function evalEstreeNewExpression(node, env, options = {}) {
    const callee = node.callee;
    const args = node.arguments;
    const calleeVal = evalEstree(callee, env, options);
    const argsVals = args.map(function (x) {
        return evalEstree(x, env, options);
    });
    return new calleeVal(...argsVals);
}
evalEstreeNewExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-new-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('get-field'), Symbol.for('callee'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('arguments'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('callee-val'), [Symbol.for('eval-estree'), Symbol.for('callee'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('args-vals'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eval-estree'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('args')]], [Symbol.for('apply'), Symbol.for('new'), Symbol.for('callee-val'), Symbol.for('args-vals')]];
/**
 * Evaluate an ESTree [`ObjectExpression`][estree:objectexpression] node.
 *
 * [estree:objectexpression]: https://github.com/estree/estree/blob/master/es5.md#objectexpression
 */
function evalEstreeObjectExpression(node, env, options = {}) {
    let result = {};
    const properties = node.properties;
    for (let prop of properties) {
        if ((0, estree_1.estreeTypeP)(prop, 'SpreadElement')) {
            const argument = prop.argument;
            const argumentVal = evalEstree(argument, env, options);
            result = Object.assign(Object.assign({}, result), argumentVal);
        }
        else {
            const key = prop.key;
            const value = prop.value;
            const keyVal = (0, estree_1.estreeTypeP)(key, 'Identifier') ? key.name : key.value;
            const valueVal = evalEstree(value, env, options);
            result[keyVal] = valueVal;
        }
    }
    return result;
}
evalEstreeObjectExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-object-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('js-obj')]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('get-field'), Symbol.for('properties'), Symbol.for('node')]], [Symbol.for('for'), [[Symbol.for('prop'), Symbol.for('properties')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('prop'), 'SpreadElement'], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('prop')]], [Symbol.for('define'), Symbol.for('argument-val'), [Symbol.for('eval-estree'), Symbol.for('argument'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js-obj-append'), Symbol.for('result'), Symbol.for('argument-val')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('prop')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('prop')]], [Symbol.for('define'), Symbol.for('key-val'), [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('key'), 'Identifier'], [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('key')]], [Symbol.for('else'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('key')]]]], [Symbol.for('define'), Symbol.for('value-val'), [Symbol.for('eval-estree'), Symbol.for('value'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('oset!'), Symbol.for('result'), Symbol.for('key-val'), Symbol.for('value-val')]]]], Symbol.for('result')];
/**
 * Evaluate an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
 *
 * [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
 */
function evalEstreeVariableDeclaration(node, env, options = {}) {
    const declarations = node.declarations;
    for (let x of declarations) {
        evalEstree(x, env, options);
    }
    return undefined;
}
evalEstreeVariableDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-variable-declaration'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('declarations'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('node')]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('declarations')]], [Symbol.for('eval-estree'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('undefined')];
/**
 * Evaluate an ESTree [`VariableDeclarator`][estree:variabledeclarator] node.
 *
 * [estree:variabledeclarator]: https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
 */
function evalEstreeVariableDeclarator(node, env, options = {}) {
    const id = node.id;
    const init = node.init;
    const assignment = new estree_1.AssignmentExpression('=', id, init);
    return evalEstreeAssignmentExpressionHelper(assignment, env, options, {
        local: true
    });
}
evalEstreeVariableDeclarator.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-variable-declarator'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('assignment'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', Symbol.for('id'), Symbol.for('init')]], [Symbol.for('eval-estree-assignment-expression-helper'), Symbol.for('assignment'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'local', Symbol.for('#t')]]];
/**
 * Evaluate an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
 *
 * [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
 */
function evalEstreeAssignmentExpression(node, env, options = {}) {
    return evalEstreeAssignmentExpressionHelper(node, env, options);
}
evalEstreeAssignmentExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-assignment-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('eval-estree-assignment-expression-helper'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an ESTree [`ArrayExpression`][estree:arrayexpression] node.
 *
 * [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
 */
function evalEstreeArrayExpression(node, env, options = {}) {
    const elements = node.elements;
    return evalEstreeArrayExpressionHelper(elements, env, options);
}
evalEstreeArrayExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-array-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('elements'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('node')]], [Symbol.for('eval-estree-array-expression-helper'), Symbol.for('elements'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an ESTree [`ArrayPattern`][estree:arraypattern] node.
 *
 * [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
 */
function evalEstreeArrayPattern(node, env, options = {}) {
    return evalEstreeArrayExpression(node, env, options);
}
evalEstreeArrayPattern.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-array-pattern'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('eval-estree-array-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an ESTree [`RestElement`][estree:restelement] node.
 *
 * [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
 */
function evalEstreeRestElement(node, env, options = {}) {
    const argument = node.argument;
    return evalEstree(argument, env, options);
}
evalEstreeRestElement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-rest-element'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('eval-estree'), Symbol.for('argument'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an ESTree [`ExpressionStatement`][estree:expressionstatement] node.
 *
 * [estree:expressionstatement]: https://github.com/estree/estree/blob/master/es5.md#expressionstatement
 */
function evalEstreeExpressionStatement(node, env, options = {}) {
    const expression = node.expression;
    return evalEstree(expression, env, options);
}
evalEstreeExpressionStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-expression-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('node')]], [Symbol.for('eval-estree'), Symbol.for('expression'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
 *
 * [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
 */
function evalEstreeFunctionDeclaration(node, env, options = {}) {
    const id = node.id;
    const name = Symbol.for(id.name);
    const f = evalEstreeFunctionExpression(node, env, options);
    env.setLocal(name, f, 'function');
    return undefined;
}
evalEstreeFunctionDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-function-declaration'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('string->symbol'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('id')]]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('eval-estree-function-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('name'), Symbol.for('f'), 'function'], Symbol.for('undefined')];
/**
 * Evaluate an ESTree [`FunctionExpression`][estree:functionexpression] node.
 *
 * <https://docs.esprima.org/en/latest/syntax-tree-format.html#function-expression>
 *
 * [estree:functionexpression]: https://github.com/estree/estree/blob/master/es5.md#functionexpression
 */
function evalEstreeFunctionExpression(node, env, options = {}) {
    return evalEstreeFunctionExpressionHelper(node, env, options);
}
evalEstreeFunctionExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-function-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('eval-estree-function-expression-helper'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
 *
 * [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
 */
function evalEstreeArrowFunctionExpression(node, env, options = {}) {
    return evalEstreeFunctionExpressionHelper(node, env, options, {
        arrow: true
    });
}
evalEstreeArrowFunctionExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-arrow-function-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('eval-estree-function-expression-helper'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'arrow', Symbol.for('#t')]]];
/**
 * Evaluate an ESTree [`UnaryExpression`][estree:unaryexpression] node.
 *
 * [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
 */
function evalEstreeUnaryExpression(node, env, options = {}) {
    const operator = node.operator;
    const prefix = node.prefix;
    const argument = node.argument;
    if (operator === '!') {
        return !evalEstree(argument, env, options);
    }
    else if ((operator === '++') || (operator === '--')) {
        const isAdd = operator === '++';
        const assignment = new estree_1.AssignmentExpression('=', argument, new estree_1.BinaryExpression(isAdd ? '+' : '-', argument, new estree_1.Literal(1)));
        if (prefix) {
            return evalEstree(assignment, env, options);
        }
        else {
            const val = evalEstree(assignment, env, options);
            const valOrig = isAdd ? (val - 1) : (val + 1);
            return valOrig;
        }
    }
    else if (operator === 'typeof') {
        return typeof evalEstree(argument, env, options);
    }
    else {
        return undefined;
    }
}
evalEstreeUnaryExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-unary-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('prefix'), [Symbol.for('get-field'), Symbol.for('prefix'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('operator'), '!'], [Symbol.for('not'), [Symbol.for('eval-estree'), Symbol.for('argument'), Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('operator'), '++'], [Symbol.for('eq?'), Symbol.for('operator'), '--']], [Symbol.for('define'), Symbol.for('is-add'), [Symbol.for('eq?'), Symbol.for('operator'), '++']], [Symbol.for('define'), Symbol.for('assignment'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', Symbol.for('argument'), [Symbol.for('new'), Symbol.for('BinaryExpression'), [Symbol.for('if'), Symbol.for('is-add'), '+', '-'], Symbol.for('argument'), [Symbol.for('new'), Symbol.for('Literal'), 1]]]], [Symbol.for('cond'), [Symbol.for('prefix'), [Symbol.for('eval-estree'), Symbol.for('assignment'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('val'), [Symbol.for('eval-estree'), Symbol.for('assignment'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('val-orig'), [Symbol.for('cond'), [Symbol.for('is-add'), [Symbol.for('-'), Symbol.for('val'), 1]], [Symbol.for('else'), [Symbol.for('+'), Symbol.for('val'), 1]]]], Symbol.for('val-orig')]]], [[Symbol.for('eq?'), Symbol.for('operator'), 'typeof'], [Symbol.for('type-of'), [Symbol.for('eval-estree'), Symbol.for('argument'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), Symbol.for('undefined')]]];
/**
 * Evaluate an ESTree [`UpdateExpression`][estree:updateexpression] node.
 *
 * [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
 */
function evalEstreeUpdateExpression(node, env, options = {}) {
    return evalEstreeUnaryExpression(node, env, options);
}
evalEstreeUpdateExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-update-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('eval-estree-unary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an ESTree [`BinaryExpression`][estree:binaryexpression] node.
 *
 * [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
 */
function evalEstreeBinaryExpression(node, env, options = {}) {
    const operator = node.operator;
    const left = node.left;
    const leftVal = evalEstree(left, env, options);
    const right = node.right;
    const rightVal = evalEstree(right, env, options);
    // TODO: Replace with `case`.
    if (operator === '+') {
        return leftVal + rightVal;
    }
    else if (operator === '-') {
        return leftVal - rightVal;
    }
    else if (operator === '*') {
        return leftVal * rightVal;
    }
    else if (operator === '/') {
        return leftVal / rightVal;
    }
    else if (operator === '<') {
        return leftVal < rightVal;
    }
    else if (operator === '<=') {
        return leftVal <= rightVal;
    }
    else if (operator === '>') {
        return leftVal > rightVal;
    }
    else if (operator === '>=') {
        return leftVal >= rightVal;
    }
    else if (operator === '==') {
        return leftVal == rightVal;
    }
    else if (operator === '===') {
        return leftVal === rightVal;
    }
    else if (operator === 'in') {
        return leftVal in rightVal;
    }
    else if (operator === 'instanceof') {
        return leftVal instanceof rightVal;
    }
    else {
        return undefined;
    }
}
evalEstreeBinaryExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-binary-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-val'), [Symbol.for('eval-estree'), Symbol.for('left'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-val'), [Symbol.for('eval-estree'), Symbol.for('right'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('operator'), '+'], [Symbol.for('+'), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), '-'], [Symbol.for('-'), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), '*'], [Symbol.for('*'), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), '/'], [Symbol.for('/'), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), '<'], [Symbol.for('<'), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), '<='], [Symbol.for('<='), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), '>'], [Symbol.for('>'), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), '>='], [Symbol.for('>='), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), '=='], [Symbol.for('js/=='), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), '==='], [Symbol.for('js/==='), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), 'in'], [Symbol.for('js/in'), Symbol.for('left-val'), Symbol.for('right-val')]], [[Symbol.for('eq?'), Symbol.for('operator'), 'instanceof'], [Symbol.for('is-a?'), Symbol.for('left-val'), Symbol.for('right-val')]], [Symbol.for('else'), Symbol.for('undefined')]]];
/**
 * Evaluate an ESTree [`LogicalExpression`][estree:logicalexpression] node.
 *
 * [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
 */
function evalEstreeLogicalExpression(node, env, options = {}) {
    const operator = node.operator;
    const left = node.left;
    const right = node.right;
    if (operator === '&&') {
        const leftVal = evalEstree(left, env, options);
        if (leftVal) {
            return evalEstree(right, env, options);
        }
        else {
            return false;
        }
    }
    else if (operator === '||') {
        const leftVal = evalEstree(left, env, options);
        if (leftVal) {
            return leftVal;
        }
        else {
            return evalEstree(right, env, options);
        }
    }
    else {
        return undefined;
    }
}
evalEstreeLogicalExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-logical-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('operator'), '&&'], [Symbol.for('define'), Symbol.for('left-val'), [Symbol.for('eval-estree'), Symbol.for('left'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [Symbol.for('left-val'), [Symbol.for('eval-estree'), Symbol.for('right'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), Symbol.for('#f')]]], [[Symbol.for('eq?'), Symbol.for('operator'), '||'], [Symbol.for('define'), Symbol.for('left-val'), [Symbol.for('eval-estree'), Symbol.for('left'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [Symbol.for('left-val'), Symbol.for('left-val')], [Symbol.for('else'), [Symbol.for('eval-estree'), Symbol.for('right'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), Symbol.for('undefined')]]];
/**
 * Evaluate an ESTree [`IfStatement`][estree:ifstatement] node.
 *
 * [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
 */
function evalEstreeIfStatement(node, env, options = {}) {
    return evalEstreeConditionalExpression(node, env, options);
}
evalEstreeIfStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-if-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('eval-estree-conditional-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Evaluate an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
 *
 * [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
 */
function evalEstreeConditionalExpression(node, env, options = {}) {
    const test = node.test;
    const consequent = node.consequent;
    const alternate = node.alternate;
    if (evalEstree(test, env, options)) {
        return evalEstree(consequent, env, options);
    }
    else if (alternate) {
        return evalEstree(alternate, env, options);
    }
    else {
        return undefined;
    }
}
evalEstreeConditionalExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-conditional-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-field'), Symbol.for('consequent'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('alternate'), [Symbol.for('get-field'), Symbol.for('alternate'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('eval-estree'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('eval-estree'), Symbol.for('consequent'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('alternate'), [Symbol.for('eval-estree'), Symbol.for('alternate'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), Symbol.for('undefined')]]];
/**
 * Evaluate an ESTree [`WhileStatement`][estree:whilestatement] node.
 *
 * [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
 */
function evalEstreeWhileStatement(node, env, options = {}) {
    // TODO: Convert `BlockStatement` to `Program` fragment
    // and extend the environment manually, only once.
    const test = node.test;
    const body = node.body;
    try {
        while (evalEstree(test, env, options)) {
            try {
                evalEstree(body, env, options);
            }
            catch (e) {
                if (e instanceof exception_1.ContinueException) {
                }
                else {
                    throw e;
                }
            }
        }
    }
    catch (e) {
        if (e instanceof exception_1.BreakException) {
        }
        else {
            throw e;
        }
    }
    return undefined;
}
evalEstreeWhileStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-while-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('try'), [Symbol.for('while'), [Symbol.for('eval-estree'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('try'), [Symbol.for('eval-estree'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('catch'), Symbol.for('ContinueException'), Symbol.for('e')]]], [Symbol.for('catch'), Symbol.for('BreakException'), Symbol.for('e')]], Symbol.for('undefined')];
/**
 * Evaluate an ESTree [`ForStatement`][estree:forstatement] node.
 *
 * [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
 */
function evalEstreeForStatement(node, env, options = {}) {
    const init = node.init;
    const test = node.test;
    const update = node.update;
    const body = node.body;
    const forEnv = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), env);
    return (0, env_1.withEnvironment)(forEnv, () => {
        evalEstree(init, forEnv, options);
        try {
            while (evalEstree(test, forEnv, options)) {
                try {
                    evalEstree(body, forEnv, options);
                }
                catch (e) {
                    if (e instanceof exception_1.ContinueException) {
                    }
                    else {
                        throw e;
                    }
                }
                evalEstree(update, forEnv, options);
            }
        }
        catch (e) {
            if (e instanceof exception_1.BreakException) {
            }
            else {
                throw e;
            }
        }
        return undefined;
    });
}
evalEstreeForStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-for-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('update'), [Symbol.for('get-field'), Symbol.for('update'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('for-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('env')]], [Symbol.for('with-environment'), Symbol.for('for-env'), [Symbol.for('js/arrow'), [], [Symbol.for('eval-estree'), Symbol.for('init'), Symbol.for('for-env'), Symbol.for('options')], [Symbol.for('try'), [Symbol.for('while'), [Symbol.for('eval-estree'), Symbol.for('test'), Symbol.for('for-env'), Symbol.for('options')], [Symbol.for('try'), [Symbol.for('eval-estree'), Symbol.for('body'), Symbol.for('for-env'), Symbol.for('options')], [Symbol.for('catch'), Symbol.for('ContinueException'), Symbol.for('e')]], [Symbol.for('eval-estree'), Symbol.for('update'), Symbol.for('for-env'), Symbol.for('options')]], [Symbol.for('catch'), Symbol.for('BreakException'), Symbol.for('e')]], Symbol.for('undefined')]]];
/**
 * Evaluate an ESTree [`ForOfStatement`][estree:forofstatement] node.
 *
 * [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
 */
function evalEstreeForOfStatement(node, env, options = {}) {
    const left = node.left;
    const right = node.right;
    const body = node.body;
    const forOfEnv = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), env);
    return (0, env_1.withEnvironment)(forOfEnv, () => {
        const identifier = (0, estree_1.estreeTypeP)(left, 'VariableDeclaration') ? left.declarations[0].id : left.left;
        const rightVal = evalEstree(right, forOfEnv, options);
        try {
            for (let x of rightVal) {
                const declaration = new estree_1.VariableDeclaration([new estree_1.VariableDeclarator(identifier, new estree_1.Literal(x))], 'let');
                try {
                    evalEstree(declaration, forOfEnv, options);
                    evalEstree(body, forOfEnv, options);
                }
                catch (e) {
                    if (e instanceof exception_1.ContinueException) {
                    }
                    else {
                        throw e;
                    }
                }
            }
        }
        catch (e) {
            if (e instanceof exception_1.BreakException) {
            }
            else {
                throw e;
            }
        }
        return undefined;
    });
}
evalEstreeForOfStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-for-of-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('for-of-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('env')]], [Symbol.for('with-environment'), Symbol.for('for-of-env'), [Symbol.for('js/arrow'), [], [Symbol.for('define'), Symbol.for('identifier'), [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('left'), 'VariableDeclaration'], [Symbol.for('~>'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('_')], [Symbol.for('first'), Symbol.for('_')], [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('_')]]], [Symbol.for('else'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('left')]]]], [Symbol.for('define'), Symbol.for('right-val'), [Symbol.for('eval-estree'), Symbol.for('right'), Symbol.for('for-of-env'), Symbol.for('options')]], [Symbol.for('try'), [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('right-val')]], [Symbol.for('define'), Symbol.for('declaration'), [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('x')]]], 'let']], [Symbol.for('try'), [Symbol.for('eval-estree'), Symbol.for('declaration'), Symbol.for('for-of-env'), Symbol.for('options')], [Symbol.for('eval-estree'), Symbol.for('body'), Symbol.for('for-of-env'), Symbol.for('options')], [Symbol.for('catch'), Symbol.for('ContinueException'), Symbol.for('e')]]], [Symbol.for('catch'), Symbol.for('BreakException'), Symbol.for('e')]], Symbol.for('undefined')]]];
/**
 * Evaluate an ESTree [`TryStatement`][estree:trystatement] node.
 *
 * [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
 */
function evalEstreeTryStatement(node, env, options = {}) {
    const block = node.block;
    const handler = node.handler;
    const finalizer = node.finalizer;
    let result = undefined;
    try {
        result = evalEstree(block, env, options);
    }
    catch (err) {
        if (handler) {
            const handlerParam = handler.param;
            const handlerParamSym = Symbol.for(handlerParam.name);
            const handlerBody = handler.body;
            const handlerEnv = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), env);
            (0, env_1.withEnvironment)(handlerEnv, () => {
                handlerEnv.setLocal(handlerParamSym, err);
                return evalEstree(handlerBody, handlerEnv, options);
            });
        }
        else {
            throw err;
        }
    }
    finally {
        if (finalizer) {
            evalEstree(finalizer, env, options);
        }
    }
    return result;
}
evalEstreeTryStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-try-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('block'), [Symbol.for('get-field'), Symbol.for('block'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('handler'), [Symbol.for('get-field'), Symbol.for('handler'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('finalizer'), [Symbol.for('get-field'), Symbol.for('finalizer'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval-estree'), Symbol.for('block'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('err'), [Symbol.for('cond'), [Symbol.for('handler'), [Symbol.for('define'), Symbol.for('handler-param'), [Symbol.for('get-field'), Symbol.for('param'), Symbol.for('handler')]], [Symbol.for('define'), Symbol.for('handler-param-sym'), [Symbol.for('string->symbol'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('handler-param')]]], [Symbol.for('define'), Symbol.for('handler-body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('handler')]], [Symbol.for('define'), Symbol.for('handler-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('env')]], [Symbol.for('with-environment'), Symbol.for('handler-env'), [Symbol.for('js/arrow'), [], [Symbol.for('send'), Symbol.for('handler-env'), Symbol.for('set-local'), Symbol.for('handler-param-sym'), Symbol.for('err')], [Symbol.for('eval-estree'), Symbol.for('handler-body'), Symbol.for('handler-env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('throw'), Symbol.for('err')]]]], [Symbol.for('finally'), [Symbol.for('when'), Symbol.for('finalizer'), [Symbol.for('eval-estree'), Symbol.for('finalizer'), Symbol.for('env'), Symbol.for('options')]]]], Symbol.for('result')];
/**
 * Evaluate an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
 *
 * [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
 */
function evalEstreeClassDeclaration(node, env, options = {}) {
    const id = node.id;
    const sym = Symbol.for(id.name);
    const classExpression = evalEstreeClassExpression(node, env, options);
    env.set(sym, classExpression);
    return undefined;
}
evalEstreeClassDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-class-declaration'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('string->symbol'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('id')]]], [Symbol.for('define'), Symbol.for('class-expression'), [Symbol.for('eval-estree-class-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('sym'), Symbol.for('class-expression')], Symbol.for('undefined')];
/**
 * Evaluate an ESTree [`ClassExpression`][estree:classexpression] node.
 *
 * [estree:classexpression]: https://github.com/estree/estree/blob/master/es2015.md#classexpression
 */
function evalEstreeClassExpression(node, env, options = {}) {
    const superClass = node.superClass;
    const classBody = node.body;
    const classBodyStatements = classBody.body;
    const constructorF = function (...args) {
        let constructorInnerF = undefined;
        for (let x of classBodyStatements) {
            // TODO: Move evaluation outside---no reason to do it each
            // time we are instantiating.
            const key = x.key;
            const keyStr = key.name;
            const value = x.value;
            const valueVal = value ? evalEstree(value, env, options) : undefined;
            this[keyStr] = valueVal;
            if (keyStr === 'constructor') {
                constructorInnerF = valueVal;
            }
        }
        if (constructorInnerF) {
            constructorInnerF.apply(this, args);
        }
        return undefined;
    };
    if (superClass) {
        const superClassVal = evalEstree(superClass, env, options);
        // <https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Inheritance#setting_teachers_prototype_and_constructor_reference>
        constructorF['prototype'] = Object.create(superClassVal['prototype']);
    }
    return constructorF;
}
evalEstreeClassExpression.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-class-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('super-class'), [Symbol.for('get-field'), Symbol.for('superClass'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('class-body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('class-body-statements'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('class-body')]], [Symbol.for('define'), Symbol.for('constructor-f'), [Symbol.for('lambda'), [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('constructor-inner-f'), Symbol.for('undefined')], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('class-body-statements')]], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('key-str'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('key')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('value-val'), [Symbol.for('cond'), [Symbol.for('value'), [Symbol.for('eval-estree'), Symbol.for('value'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), Symbol.for('undefined')]]], [Symbol.for('oset!'), Symbol.for('this'), Symbol.for('key-str'), Symbol.for('value-val')], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('key-str'), 'constructor'], [Symbol.for('set!'), Symbol.for('constructor-inner-f'), Symbol.for('value-val')]]], [Symbol.for('when'), Symbol.for('constructor-inner-f'), [Symbol.for('send'), Symbol.for('constructor-inner-f'), Symbol.for('apply'), Symbol.for('this'), Symbol.for('args')]], Symbol.for('undefined')]], [Symbol.for('when'), Symbol.for('super-class'), [Symbol.for('define'), Symbol.for('super-class-val'), [Symbol.for('eval-estree'), Symbol.for('super-class'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('oset!'), Symbol.for('constructor-f'), 'prototype', [Symbol.for('send'), Symbol.for('Object'), Symbol.for('create'), [Symbol.for('oget'), Symbol.for('super-class-val'), 'prototype']]]], Symbol.for('constructor-f')];
/**
 * Evaluate an ESTree [`SwitchStatement`][estree:switchstatement] node.
 *
 * [estree:switchstatement]: https://github.com/estree/estree/blob/master/es5.md#switchstatement
 */
function evalEstreeSwitchStatement(node, env, options = {}) {
    const discriminant = node.discriminant;
    const discriminantVal = evalEstree(discriminant, env, options);
    const cases = node.cases;
    try {
        for (let x of cases) {
            const test = x.test;
            if (!test || (discriminantVal === evalEstree(test, env, options))) {
                evalEstree(x, env, options);
            }
        }
    }
    catch (e) {
        if (e instanceof exception_1.BreakException) {
        }
        else {
            throw e;
        }
    }
    return undefined;
}
evalEstreeSwitchStatement.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-switch-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('discriminant'), [Symbol.for('get-field'), Symbol.for('discriminant'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('discriminant-val'), [Symbol.for('eval-estree'), Symbol.for('discriminant'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('cases'), [Symbol.for('get-field'), Symbol.for('cases'), Symbol.for('node')]], [Symbol.for('try'), [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('cases')]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('x')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('not'), Symbol.for('test')], [Symbol.for('eq?'), Symbol.for('discriminant-val'), [Symbol.for('eval-estree'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('eval-estree'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('catch'), Symbol.for('BreakException'), Symbol.for('e')]], Symbol.for('undefined')];
/**
 * Evaluate an ESTree [`SwitchCase`][estree:switchcase] node.
 *
 * [estree:switchcase]: https://github.com/estree/estree/blob/master/es5.md#switchcase
 */
function evalEstreeSwitchCase(node, env, options = {}) {
    const consequent = node.consequent;
    for (let x of consequent) {
        evalEstree(x, env, options);
    }
    return undefined;
}
evalEstreeSwitchCase.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-switch-case'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-field'), Symbol.for('consequent'), Symbol.for('node')]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('consequent')]], [Symbol.for('eval-estree'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('undefined')];
/**
 * Evaluate an ESTree `XRawJavaScript` node.
 * This is an unofficial ESTree extension.
 */
function evalEstreeXRawJavascript(node, env, options = {}) {
    let str = node.js;
    str = '(' + str + ')';
    return eval(str);
}
evalEstreeXRawJavascript.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-x-raw-javascript'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('get-field'), Symbol.for('js'), Symbol.for('node')]], [Symbol.for('set!'), Symbol.for('str'), [Symbol.for('string-append'), '(', Symbol.for('str'), ')']], [Symbol.for('js/eval'), Symbol.for('str')]];
/**
 * Global variable used for storing the value of `this`.
 * Used for evaluating `ThisExpression`.
 */
let currentThisValue = undefined;
/**
 * Temporarily set `current-this-value` to `val`,
 * call `f`, and restore the original value.
 * Returns the result of calling `f`.
 */
function withThisValue(val, f) {
    let result = undefined;
    const tmp = currentThisValue;
    try {
        currentThisValue = val;
        result = f();
    }
    finally {
        currentThisValue = tmp;
    }
    return result;
}
withThisValue.lispSource = [Symbol.for('define'), [Symbol.for('with-this-value'), Symbol.for('val'), Symbol.for('f')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('define'), Symbol.for('tmp'), Symbol.for('current-this-value')], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('current-this-value'), Symbol.for('val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f')]], [Symbol.for('finally'), [Symbol.for('set!'), Symbol.for('current-this-value'), Symbol.for('tmp')]]], Symbol.for('result')];
/**
 * Helper function for `eval-estree-assignment-expression`.
 */
function evalEstreeAssignmentExpressionHelper(node, env, options = {}, settings = {}) {
    const localSetting = settings['local'];
    const left = node.left;
    const leftType = (0, estree_1.estreeType)(left);
    const right = node.right;
    const rightVal = right ? evalEstree(right, env, options) : undefined;
    if (leftType === 'ArrayPattern') {
        const elements = left.elements;
        const _end = elements.length;
        for (let i = 0; i < _end; i++) {
            const x = elements[i];
            const xType = (0, estree_1.estreeType)(x);
            if (xType === 'RestElement') {
                const name = x.argument.name;
                const sym = Symbol.for(name);
                const val = (i === 0) ? rightVal : rightVal.slice(i);
                if (localSetting) {
                    env.setLocal(sym, val);
                }
                else {
                    env.set(sym, val);
                }
            }
            else {
                const name = x.name;
                const sym = Symbol.for(name);
                const val = rightVal[i];
                if (localSetting) {
                    env.setLocal(sym, val);
                }
                else {
                    env.set(sym, val);
                }
            }
        }
        return rightVal;
    }
    else if (leftType === 'ObjectPattern') {
        const properties = left.properties;
        for (let prop of properties) {
            const key = prop.key;
            const value = prop.value;
            const sym = Symbol.for(value.name);
            const val = rightVal[key.name];
            if (localSetting) {
                env.setLocal(sym, val);
            }
            else {
                env.set(sym, val);
            }
        }
        return rightVal;
    }
    else if (leftType === 'Identifier') {
        const sym = Symbol.for(left.name);
        if (localSetting) {
            env.setLocal(sym, rightVal);
        }
        else {
            env.set(sym, rightVal);
        }
        return rightVal;
    }
    else if (leftType === 'MemberExpression') {
        const obj = left.object;
        const objVal = evalEstree(obj, env, options);
        const computed = left.computed;
        const prop = left.property;
        const propVal = computed ? evalEstree(prop, env, options) : ((0, estree_1.estreeTypeP)(prop, 'Identifier') ? prop.name : prop.value);
        objVal[propVal] = rightVal;
        return objVal;
    }
    else {
        // TODO: Chain expressions
        return undefined;
    }
}
evalEstreeAssignmentExpressionHelper.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-assignment-expression-helper'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('local-setting'), [Symbol.for('oget'), Symbol.for('settings'), 'local']], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-type'), [Symbol.for('estree-type'), Symbol.for('left')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-val'), [Symbol.for('cond'), [Symbol.for('right'), [Symbol.for('eval-estree'), Symbol.for('right'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), Symbol.for('undefined')]]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('left-type'), 'ArrayPattern'], [Symbol.for('define'), Symbol.for('elements'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('left')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('elements')]]]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('aget'), Symbol.for('elements'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('x-type'), [Symbol.for('estree-type'), Symbol.for('x')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('x-type'), 'RestElement'], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('~>'), Symbol.for('x'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('_')], [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('_')]]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('string->symbol'), Symbol.for('name')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('drop'), Symbol.for('right-val'), Symbol.for('i')]], [Symbol.for('cond'), [Symbol.for('local-setting'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('val')]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('sym'), Symbol.for('val')]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('string->symbol'), Symbol.for('name')]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('aget'), Symbol.for('right-val'), Symbol.for('i')]], [Symbol.for('cond'), [Symbol.for('local-setting'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('val')]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('sym'), Symbol.for('val')]]]]]], Symbol.for('right-val')], [[Symbol.for('eq?'), Symbol.for('left-type'), 'ObjectPattern'], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('get-field'), Symbol.for('properties'), Symbol.for('left')]], [Symbol.for('for'), [[Symbol.for('prop'), Symbol.for('properties')]], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('prop')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('prop')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('string->symbol'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('value')]]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('oget'), Symbol.for('right-val'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('key')]]], [Symbol.for('cond'), [Symbol.for('local-setting'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('val')]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('sym'), Symbol.for('val')]]]], Symbol.for('right-val')], [[Symbol.for('eq?'), Symbol.for('left-type'), 'Identifier'], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('string->symbol'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('left')]]], [Symbol.for('cond'), [Symbol.for('local-setting'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('right-val')]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('sym'), Symbol.for('right-val')]]], Symbol.for('right-val')], [[Symbol.for('eq?'), Symbol.for('left-type'), 'MemberExpression'], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('get-field'), Symbol.for('object'), Symbol.for('left')]], [Symbol.for('define'), Symbol.for('obj-val'), [Symbol.for('eval-estree'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('get-field'), Symbol.for('computed'), Symbol.for('left')]], [Symbol.for('define'), Symbol.for('prop'), [Symbol.for('get-field'), Symbol.for('property'), Symbol.for('left')]], [Symbol.for('define'), Symbol.for('prop-val'), [Symbol.for('cond'), [Symbol.for('computed'), [Symbol.for('eval-estree'), Symbol.for('prop'), Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('estree-type?'), Symbol.for('prop'), 'Identifier'], [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('prop')]], [Symbol.for('else'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('prop')]]]], [Symbol.for('oset!'), Symbol.for('obj-val'), Symbol.for('prop-val'), Symbol.for('right-val')], Symbol.for('obj-val')], [Symbol.for('else'), Symbol.for('undefined')]]];
/**
 * Helper function for `eval-estree-array-expression`.
 */
function evalEstreeArrayExpressionHelper(elements, env, options = {}) {
    let result = [];
    for (let x of elements) {
        if ((0, estree_1.estreeTypeP)(x, 'RestElement')) {
            result = [...result, ...evalEstree(x, env, options)];
        }
        else {
            result.push(evalEstree(x, env, options));
        }
    }
    return result;
}
evalEstreeArrayExpressionHelper.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-array-expression-helper'), Symbol.for('elements'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('elements')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('x'), 'RestElement'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), [Symbol.for('eval-estree'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('eval-estree'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]]]]], Symbol.for('result')];
/**
 * Helper function for `eval-estree-function-expression`.
 */
function evalEstreeFunctionExpressionHelper(node, env, options = {}, settings = {}) {
    const arrowSetting = settings['arrow'];
    const params = node.params;
    const body = node.body;
    if (arrowSetting) {
        return function (...args) {
            let result = undefined;
            try {
                result = evalEstree((params.length === 0) ? body : new estree_1.BlockStatement([new estree_1.VariableDeclaration([new estree_1.VariableDeclarator(new estree_1.ArrayPattern(params), (0, estree_1.wrapInEstree)(args))], 'let'), ...body.body]), env, options);
            }
            catch (e) {
                if (e instanceof exception_1.ReturnException) {
                    result = e.value;
                }
                else {
                    throw e;
                }
            }
            return result;
        };
    }
    else {
        return function (...args) {
            return withThisValue(this, function () {
                let result = undefined;
                try {
                    result = evalEstree((params.length === 0) ? body : new estree_1.BlockStatement([new estree_1.VariableDeclaration([new estree_1.VariableDeclarator(new estree_1.ArrayPattern(params), (0, estree_1.wrapInEstree)(args))], 'let'), ...body.body]), env, options);
                }
                catch (e) {
                    if (e instanceof exception_1.ReturnException) {
                        result = e.value;
                    }
                    else {
                        throw e;
                    }
                }
                return result;
            });
        };
    }
}
evalEstreeFunctionExpressionHelper.lispSource = [Symbol.for('define'), [Symbol.for('eval-estree-function-expression-helper'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('arrow-setting'), [Symbol.for('oget'), Symbol.for('settings'), 'arrow']], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('cond'), [Symbol.for('arrow-setting'), [Symbol.for('lambda'), Symbol.for('args'), [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval-estree'), [Symbol.for('if'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('params')], 0], Symbol.for('body'), [Symbol.for('new'), Symbol.for('BlockStatement'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('new'), Symbol.for('ArrayPattern'), Symbol.for('params')], [Symbol.for('wrap-in-estree'), Symbol.for('args')]]], 'let']], [Symbol.for('unquote-splicing'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('body')]]]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('catch'), Symbol.for('ReturnException'), Symbol.for('e'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('e')]]]], Symbol.for('result')]], [Symbol.for('else'), [Symbol.for('lambda'), [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('with-this-value'), Symbol.for('this'), [Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('eval-estree'), [Symbol.for('if'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('params')], 0], Symbol.for('body'), [Symbol.for('new'), Symbol.for('BlockStatement'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('new'), Symbol.for('ArrayPattern'), Symbol.for('params')], [Symbol.for('wrap-in-estree'), Symbol.for('args')]]], 'let']], [Symbol.for('unquote-splicing'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('body')]]]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('catch'), Symbol.for('ReturnException'), Symbol.for('e'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('e')]]]], Symbol.for('result')]]]]]];
/**
 * Mapping from ESTree node types to evaluator functions.
 */
const evalEstreeMap = new Map([['ArrayExpression', evalEstreeArrayExpression], ['ArrayPattern', evalEstreeArrayPattern], ['ArrowFunctionExpression', evalEstreeArrowFunctionExpression], ['AssignmentExpression', evalEstreeAssignmentExpression], ['BinaryExpression', evalEstreeBinaryExpression], ['BlockStatement', evalEstreeBlockStatement], ['BreakStatement', evalEstreeBreakStatement], ['CallExpression', evalEstreeCallExpression], ['ClassDeclaration', evalEstreeClassDeclaration], ['ClassExpression', evalEstreeClassExpression], ['ConditionalExpression', evalEstreeConditionalExpression], ['ContinueStatement', evalEstreeContinueStatement], ['ExpressionStatement', evalEstreeExpressionStatement], ['ForOfStatement', evalEstreeForOfStatement], ['ForStatement', evalEstreeForStatement], ['FunctionDeclaration', evalEstreeFunctionDeclaration], ['FunctionExpression', evalEstreeFunctionExpression], ['Identifier', evalEstreeIdentifier], ['IfStatement', evalEstreeIfStatement], ['Literal', evalEstreeLiteral], ['LogicalExpression', evalEstreeLogicalExpression], ['MemberExpression', evalEstreeMemberExpression], ['NewExpression', evalEstreeNewExpression], ['ObjectExpression', evalEstreeObjectExpression], ['Program', evalEstreeProgram], ['RestElement', evalEstreeRestElement], ['ReturnStatement', evalEstreeReturnStatement], ['SequenceExpression', evalEstreeSequenceExpression], ['SwitchStatement', evalEstreeSwitchStatement], ['SwitchCase', evalEstreeSwitchCase], ['ThisExpression', evalEstreeThisExpression], ['ThrowStatement', evalEstreeThrowStatement], ['TryStatement', evalEstreeTryStatement], ['UnaryExpression', evalEstreeUnaryExpression], ['UpdateExpression', evalEstreeUpdateExpression], ['VariableDeclaration', evalEstreeVariableDeclaration], ['VariableDeclarator', evalEstreeVariableDeclarator], ['WhileStatement', evalEstreeWhileStatement], ['YieldExpression', evalEstreeYieldExpression], ['XRawJavaScript', evalEstreeXRawJavascript]]);
