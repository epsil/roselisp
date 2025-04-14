"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.transpile = void 0;
const constants_1 = require("./constants");
const eval_1 = require("./eval");
const functions_1 = require("./functions");
const lisp_1 = require("./lisp");
const memo_1 = require("./memo");
const symbol_1 = require("./symbol");
let defaultLanguage = 'JavaScript';
class TranspilationEvaluator extends eval_1.Evaluator {
    constructor(lispEnv, transpilationEnv, options = {}) {
        super();
        this.lispEnv = lispEnv;
        this.transpilationEnv = transpilationEnv;
        this.options = Object.assign(Object.assign({}, options), {
            lispEnvironment: lispEnv,
            transpilationEnvironment: transpilationEnv
        });
    }
    eval(exp) {
        return transpileSexp(exp, this.lispEnv, this.options);
    }
}
let makeTranspilationEvaluator = (0, memo_1.memoize)(function (env, options = {}) {
    let language = options['language'];
    language = language || defaultLanguage;
    {
        let transpilationEnv = transpilationMap.get(language) || javascriptEnv;
        return new TranspilationEvaluator(env, transpilationEnv, options);
    }
});
class TranspilationEnvironment extends eval_1.TypedEnvironment {
}
let javascriptEnv = new TranspilationEnvironment([[constants_1.true_, 'true', 'variable'], [constants_1.false_, 'false', 'variable'], [constants_1.jsNull_, 'null', 'variable'], [constants_1.undefined_, 'undefined', 'variable'], [constants_1.null_, '[]', 'variable'], [constants_1.nil_, 'null', 'variable'], [lisp_1.and_, transpileAnd, 'transpiler'], [functions_1.add, transpileAdd, 'transpiler'], [functions_1.aget, transpileAget, 'transpiler'], [functions_1.append, transpileAppend, 'transpiler'], [functions_1.apply, transpileApply, 'transpiler'], [functions_1.aset, transpileAset, 'transpiler'], [lisp_1.break_, transpileBreak, 'transpiler'], [lisp_1.class_, transpileClass, 'transpiler'], [lisp_1.cond_, transpileCond, 'transpiler'], [lisp_1.continue_, transpileContinue, 'transpiler'], [lisp_1.define_, transpileDefine, 'transpiler'], [functions_1.div, transpileDiv, 'transpiler'], [lisp_1.dot_, transpileDot, 'transpiler'], [functions_1.eq_, transpileEqual, 'transpiler'], [functions_1.eql_, transpileEqual, 'transpiler'], [functions_1.equal_, transpileEqual, 'transpiler'], [functions_1.gt, transpileGreaterThan, 'transpiler'], [functions_1.gte, transpileGreaterThanOrEqual, 'transpiler'], [lisp_1.for_, transpileFor, 'transpiler'], [functions_1.funcall, transpileFuncall, 'transpiler'], [lisp_1.instanceof_, transpileInstanceof, 'transpiler'], [functions_1.jsObj_, transpileJsObj, 'transpiler'], [functions_1.jsObjAppend_, transpileJsObjAppend, 'transpiler'], [lisp_1.lambda_, transpileLambda, 'transpiler'], [lisp_1.letValues, transpileLetValues, 'transpiler'], [lisp_1.letStar_, transpileLet, 'transpiler'], [functions_1.list, transpileList, 'transpiler'], [functions_1.lt, transpileLessThan, 'transpiler'], [functions_1.lte, transpileLessThanOrEqual, 'transpiler'], [functions_1.mul, transpileMul, 'transpiler'], [lisp_1.new_, transpileNew, 'transpiler'], [functions_1.not, transpileNot, 'transpiler'], [lisp_1.or_, transpileOr, 'transpiler'], [lisp_1.progn_, transpileProgn, 'transpiler'], [lisp_1.provide_, transpileProvide, 'transpiler'], [lisp_1.quasiquote_, transpileQuasiquote, 'transpiler'], [lisp_1.quote_, transpileQuote, 'transpiler'], [lisp_1.require_, transpileRequire, 'transpiler'], [lisp_1.return_, transpileReturn, 'transpiler'], [lisp_1.send_, transpileDot, 'transpiler'], [lisp_1.setq_, transpileSetq, 'transpiler'], [functions_1.stringAppend_, transpileStringAppend, 'transpiler'], [functions_1.sub, transpileSub, 'transpiler'], [lisp_1.throw_, transpileThrow, 'transpiler'], [lisp_1.try_, transpileTry, 'transpiler'], [lisp_1.typeof_, transpileTypeof, 'transpiler'], [lisp_1.while_, transpileWhile, 'transpiler'], [lisp_1.yield_, transpileYield, 'transpiler'], [functions_1.add1, transpileAdd1Macro, 'macro'], [functions_1.display, transpileDisplayMacro, 'macro'], [functions_1.drop, transpileDropMacro, 'macro'], [functions_1.dropRight, transpileDropRightMacro, 'macro'], [functions_1.first, transpileFirstMacro, 'macro'], [functions_1.foldl, transpileFoldlMacro, 'macro'], [functions_1.foldr, transpileFoldrMacro, 'macro'], [symbol_1.gensym_, transpileGensymMacro, 'macro'], [lisp_1.if_, transpileIfMacro, 'macro'], [functions_1.last, transpileLastMacro, 'macro'], [functions_1.length, transpileLengthMacro, 'macro'], [functions_1.listp_, transpileListpMacro, 'macro'], [functions_1.map, transpileMapMacro, 'macro'], [functions_1.nth, transpileNthMacro, 'macro'], [functions_1.nthcdr, transpileNthcdrMacro, 'macro'], [functions_1.print, transpileDisplayMacro, 'macro'], [functions_1.stringp_, transpileStringpMacro, 'macro'], [functions_1.sub1, transpileSub1Macro, 'macro'], [functions_1.values, transpileValuesMacro, 'macro']]);
let transpilationMap = new Map([['JavaScript', javascriptEnv], ['TypeScript', javascriptEnv]]);
function transpile(exp, env, options = {}) {
    return (0, eval_1.eval_)(exp, makeTranspilationEvaluator(env, options));
}
exports.transpile = transpile;
function transpileSexp(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptions(options);
    let bindings = inheritedOptions['bindings'];
    if ((0, functions_1.listp_)(exp)) {
        if (exp.length === 0) {
            return '[]';
        }
        else {
            let op = exp[0];
            if ((0, symbol_1.symbolp_)(op) && bindings.has(op)) {
                return transpileFunctionCall(exp, env, inheritedOptions);
            }
            if ((0, symbol_1.symbolp_)(op) && (0, symbol_1.symbolToString_)(op).match(new RegExp('^\\.'))) {
                return transpileMethodCall(exp, env, inheritedOptions);
            }
            else {
                let [fn_, opType] = env.getBinding(op);
                if (opType === 'undefined') {
                    return transpileFunctionCall(exp, env, inheritedOptions);
                }
                else {
                    let transpilationEnvironment = inheritedOptions.transpilationEnvironment;
                    let [transpilationF, transpilationType] = transpilationEnvironment.getBinding(fn_);
                    if (transpilationType === 'macro') {
                        return transpileSexp(transpilationF(exp, env), env, inheritedOptions);
                    }
                    else if (transpilationType === 'transpiler') {
                        return transpilationF(exp, env, inheritedOptions);
                    }
                    else if (opType === 'macro') {
                        return transpileMacroCall(exp, env, inheritedOptions);
                    }
                    else {
                        return transpileFunctionCall(exp, env, inheritedOptions);
                    }
                }
            }
        }
    }
    else if ((0, functions_1.stringp_)(exp)) {
        return transpileString(exp, env, inheritedOptions);
    }
    else if ((0, symbol_1.symbolp_)(exp)) {
        return transpileSymbol(exp, env, inheritedOptions);
    }
    else {
        return transpileAtom(exp, env, inheritedOptions);
    }
}
function transpileAnd(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        wrapParens: undefined
    }, exp);
    let wrapParens = consumedOptions['wrapParens'];
    let result = transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
        monoidIdentity: true,
        monoidOperator: '&&'
    }));
    if (wrapParens) {
        result = wrapInParens(result);
    }
    return result;
}
function transpileAdd(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
        monoidIdentity: 0,
        monoidOperator: '+'
    }));
}
function transpileAget(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        wrapParens: false
    }, exp);
    let wrapParens = consumedOptions['wrap-parens'];
    let variable = exp[1];
    let indices = exp.slice(2);
    let result = transpileExpression(variable, env, inheritedOptions) + indices.reduce(function (acc, x) {
        return (acc + '[' + transpileExpression(x, env, inheritedOptions) + ']');
    }, '');
    if (wrapParens === true) {
        result = wrapInParens(result);
    }
    return returnStatementOrExpression(result, inheritedOptions);
}
function transpileApply(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let apply_ = exp[0];
    let f = exp[1];
    if ((0, functions_1.isFunctionReference)(f, env, lisp_1.new_)) {
        return returnStatementOrExpression('new ' + transpileApply([apply_, ...exp.slice(2)], env, Object.assign(Object.assign({}, inheritedOptions), {
            expressionType: 'expression',
            returnStatement: false
        })), inheritedOptions);
    }
    {
        let transpiledF = transpileExpression(f, env, inheritedOptions);
        if (transpiledF.match(new RegExp('^function '))) {
            transpiledF = wrapInParens(transpiledF);
        }
        {
            let args = exp.slice(2);
            let transpiledArgs = '';
            if (args.length > 0) {
                let regularArgs = args.slice(0, -1);
                let restArg = args[args.length - 1];
                transpiledArgs = regularArgs.map(function (arg) {
                    return transpileExpression(arg, env, inheritedOptions);
                }).join(', ') + ((regularArgs.length > 0) ? ', ' : '') + '...' + transpileExpression(restArg, env, inheritedOptions);
            }
            transpiledArgs = wrapInParens(transpiledArgs);
            return returnStatementOrExpression(transpiledF + transpiledArgs, inheritedOptions);
        }
    }
}
function transpileAset(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let arr = exp[1];
    let indices = exp.slice(2, -1);
    let value = exp[exp.length - 1];
    return returnStatementOrExpression(transpileExpression([Symbol.for('set!'), [Symbol.for('aref'), arr, ...indices], value], env, inheritedOptions), inheritedOptions);
}
function transpileAtom(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return returnStatementOrExpression(exp + '', inheritedOptions);
}
function transpileCond(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        expressionType: undefined,
        returnStatement: undefined
    }, exp);
    let expressionType = consumedOptions['expressionType'];
    let returnStatement = consumedOptions['returnStatement'];
    let indent = inheritedOptions['indent'];
    if (expressionType === 'expression') {
        let condClauses = (0, functions_1.rest_)(exp);
        return (returnStatement ? 'return ' : '') + condClauses.reduceRight(function (transpiledExp, condClause) {
            let condition = condClause[0];
            let thenClauses = (0, functions_1.rest_)(condClause);
            (thenClauses = (0, functions_1.prognWrap)(thenClauses));
            if ((condition === Symbol.for('else'))) {
                return transpileExpression(thenClauses, env, inheritedOptions);
            }
            else {
                return ('(' + transpileExpression(condition, env, inheritedOptions) + ' ? ' + transpileExpression(thenClauses, env, inheritedOptions) + ' : ' + transpiledExp + ')');
            }
        }, (((condClauses.length > 0) && (condClauses[condClauses.length - 1][0] !== Symbol.for('else'))) ? 'undefined' : ''));
    }
    else {
        let transpiledExpressions = [];
        let condClauses = (0, functions_1.rest_)(exp);
        for (let condClause of condClauses) {
            let [condition, ...thenClauses] = condClause;
            thenClauses = (0, functions_1.prognWrap)(thenClauses);
            {
                let transpiledExpression = ((transpiledExpressions.length === 0) ? '' : '\n' +
                    '} else ') + ((condition === Symbol.for('else')) ? '{\n' : ('if (' + transpileExpression(condition, env, inheritedOptions) + ') {\n')) + (0, functions_1.indentString)(transpilePossibleReturnStatement(thenClauses, env, Object.assign(Object.assign({}, inheritedOptions), consumedOptions)), indent);
                transpiledExpressions.push(transpiledExpression);
            }
        }
        if (transpiledExpressions.length > 0) {
            transpiledExpressions.push('\n' +
                '}');
        }
        return transpiledExpressions.join('');
    }
}
function transpileDefine(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let bindings = inheritedOptions['bindings'];
    let language = inheritedOptions['language'];
    let sym = exp[1];
    bindings.setEntry([sym, [true, 'variable']]);
    if ((0, functions_1.dottedListP)(exp[1])) {
        let argList = (0, functions_1.rest_)((0, functions_1.flatten)(exp[1]));
        argList = (argList.length === 0 ? argList[0] : (0, functions_1.listStar)(...argList));
        return transpileLambda([Symbol.for('lambda'), argList, ...exp.slice(2)], env, Object.assign(Object.assign({}, inheritedOptions), {
            expressionType: 'expression',
            functionName: transpileExpression(exp[1][0], env, inheritedOptions)
        }));
    }
    else if ((0, functions_1.listp_)(exp[1])) {
        return transpileLambda([Symbol.for('lambda'), (0, functions_1.rest_)(exp[1]), ...exp.slice(2)], env, Object.assign(Object.assign({}, inheritedOptions), {
            expressionType: 'expression',
            functionName: transpileExpression(exp[1][0], env, inheritedOptions)
        }));
    }
    else if (exp.length === 2) {
        return returnStatementOrExpression('let ' + transpileExpression(exp[1], env, inheritedOptions) + ((language === 'TypeScript') ? ': any' : ''), inheritedOptions);
    }
    else if ((0, functions_1.listp_)(exp[2]) && (env.get(exp[2][0]) === lisp_1.lambda_)) {
        return transpileDefine([Symbol.for('define'), [exp[1], ...exp[2][1]], ...exp[2].slice(2)], env, inheritedOptions);
    }
    else {
        let args = (0, functions_1.rest_)(exp).map(function (arg) {
            return transpileExpression(arg, env, inheritedOptions);
        });
        if (language === 'TypeScript') {
            return returnStatementOrExpression('let ' + args[0] + ': any = ' + args[1], Object.assign(Object.assign({}, inheritedOptions), {
                returnStatement: false
            }));
        }
        else {
            return returnStatementOrExpression('let ' + args[0] + ' = ' + args[1], Object.assign(Object.assign({}, inheritedOptions), {
                returnStatement: false
            }));
        }
    }
}
function transpileDiv(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
        monoidIdentity: 1,
        monoidOperator: '/'
    }));
}
function transpileDot(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return returnStatementOrExpression(transpileExpression(exp[1], env, inheritedOptions) + '.' + transpileFunctionCall(exp.slice(2), env, Object.assign(Object.assign({}, inheritedOptions), {
        expressionType: 'expression',
        returnStatement: false
    })), inheritedOptions);
}
function transpileDropMacro(exp, env) {
    return [Symbol.for('.slice'), exp[1], exp[2]];
}
function transpileDropRightMacro(exp, env) {
    return [Symbol.for('.slice'), exp[1], 0, [Symbol.for('-'), exp[2]]];
}
function transpileEqual(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
        monoidIdentity: true,
        monoidOperator: '==='
    }));
}
function transpileExpression(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let result = transpileSexp(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
        expressionType: 'expression',
        returnStatement: false
    }));
    return result;
}
function transpileFoldlMacro(exp, env) {
    let flip = [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')]]];
    return [Symbol.for('.reduce'), exp[3], [flip, exp[1]], exp[2]];
}
function transpileFoldrMacro(exp, env) {
    let flip = [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')]]];
    return [Symbol.for('.reduceRight'), exp[3], [flip, exp[1]], exp[2]];
}
function transpileFuncall(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        wrapParens: undefined
    }, exp);
    let wrapParens = consumedOptions['wrapParens'];
    let result = transpileFunctionCall((0, functions_1.rest_)(exp), env, inheritedOptions);
    if (wrapParens === true) {
        result = wrapInParens(result);
    }
    return result;
}
function transpileFunctionCall(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptions(options);
    let f = transpileExpression(exp[0], env, inheritedOptions);
    if (f.match(new RegExp('^function '))) {
        f = wrapInParens(f);
    }
    {
        let args = (0, functions_1.rest_)(exp).map(function (arg) {
            return transpileExpression(arg, env, inheritedOptions);
        });
        return returnStatementOrExpression(f + wrapInParens(args.join(', ')), inheritedOptions);
    }
}
function transpileGreaterThan(exp, env, options = {}) {
    if (exp.length < 3) {
        return transpileSexp(true, env, options);
    }
    else if (exp.length === 3) {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
            monoidIdentity: true,
            monoidOperator: '>'
        }));
    }
    else {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        let andExp = [Symbol.for('and')];
        for (let i = 2; i < exp.length; i++) {
            andExp.push([Symbol.for('>'), exp[i - 1], exp[i]]);
        }
        return transpileSexp(andExp, env, inheritedOptions);
    }
}
function transpileGreaterThanOrEqual(exp, env, options = {}) {
    if (exp.length < 3) {
        return transpileSexp(true, env, options);
    }
    else if (exp.length === 3) {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
            monoidIdentity: true,
            monoidOperator: '>='
        }));
    }
    else {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        let andExp = [Symbol.for('and')];
        for (let i = 2; i < exp.length; i++) {
            andExp.push([Symbol.for('>='), exp[i - 1], exp[i]]);
        }
        return transpileSexp(andExp, env, inheritedOptions);
    }
}
function transpileIfMacro(exp, env) {
    let condition = exp[1];
    let thenClause = exp[2];
    let elseClauses = exp.slice(3);
    let condExp = [Symbol.for('cond'), [condition, thenClause], ...(elseClauses.length > 0 ? [[Symbol.for('else'), ...elseClauses]] : [])];
    return condExp;
}
function transpileMonoid(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptions(options, {
        monoidIdentity: undefined,
        monoidOperator: undefined,
        wrapParens: undefined
    });
    let monoidIdentity = consumedOptions['monoidIdentity'];
    let monoidOperator = consumedOptions['monoidOperator'];
    let wrapParens = consumedOptions['wrapParens'];
    let args = (0, functions_1.rest_)(exp);
    if (args.length === 0) {
        return returnStatementOrExpression(transpileExpression(monoidIdentity, env, inheritedOptions), options);
    }
    else if (args.length === 1) {
        return returnStatementOrExpression(transpileExpression(args[0], env, Object.assign(Object.assign({}, options), {
            wrapParens: 'smart'
        })), inheritedOptions);
    }
    else {
        let transpiledArgs = args.map(function (arg) {
            return transpileExpression(arg, env, Object.assign(Object.assign({}, inheritedOptions), {
                wrapParens: 'smart'
            }));
        });
        let result = transpiledArgs.join(' ' + monoidOperator + ' ');
        if (wrapParens) {
            result = wrapInParens(result);
        }
        return returnStatementOrExpression(result, inheritedOptions);
    }
}
function transpileLambda(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        functionName: undefined,
        returnType: undefined
    }, exp);
    let functionName = consumedOptions['functionName'];
    let returnType = consumedOptions['returnType'];
    let indent = inheritedOptions['indent'];
    let language = inheritedOptions['language'];
    let transpiledArgs = '';
    let argsList;
    let regularArgs;
    let restArg;
    if ((0, symbol_1.symbolp_)(exp[1])) {
        restArg = exp[1];
    }
    else if ((0, functions_1.dottedListP)(exp[1])) {
        argsList = (0, functions_1.flatten)(exp[1]);
        regularArgs = argsList.slice(0, -1);
        restArg = argsList[argsList.length - 1];
    }
    else {
        regularArgs = exp[1];
    }
    if (regularArgs) {
        transpiledArgs = regularArgs.map(function (arg) {
            if ((0, functions_1.listp_)(arg)) {
                return transpileExpression(arg[0], env, inheritedOptions) + ((language === 'TypeScript') ? ': any' : '') + ' = ' + transpileExpression(arg[1], env, inheritedOptions);
            }
            else {
                return transpileExpression(arg, env, inheritedOptions) + ((language === 'TypeScript') ? ': any' : '');
            }
        }).join(', ');
    }
    if (restArg) {
        transpiledArgs = transpiledArgs + (regularArgs ? ', ' : '') + '...' + transpileExpression(restArg, env, inheritedOptions) + ((language === 'TypeScript') ? ': any[]' : '');
    }
    transpiledArgs = wrapInParens(transpiledArgs) + ((language === 'TypeScript') ? (': ' + (returnType || 'any')) : '');
    {
        let statements = exp.slice(2);
        let statementsTranspiled = '';
        statementsTranspiled = transpilePossibleReturnStatement((0, functions_1.prognWrap)(statements), env, Object.assign(Object.assign({}, inheritedOptions), {
            returnStatement: returnType !== 'void'
        }));
        return returnStatementOrExpression('function ' + (functionName ? functionName : '') + transpiledArgs + ' {\n' + (0, functions_1.indentString)(statementsTranspiled, indent) + '\n' +
            '}', options);
    }
}
function transpileLengthMacro(exp, env) {
    return [Symbol.for('.-length'), exp[1]];
}
function transpileLessThan(exp, env, options = {}) {
    if (exp.length < 3) {
        return transpileSexp(true, env, options);
    }
    else if (exp.length === 3) {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
            monoidIdentity: true,
            monoidOperator: '<'
        }));
    }
    else {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        let andExp = [Symbol.for('and')];
        for (let i = 2; i < exp.length; i++) {
            andExp.push([Symbol.for('<'), exp[i - 1], exp[i]]);
        }
        return transpileSexp(andExp, env, inheritedOptions);
    }
}
function transpileLessThanOrEqual(exp, env, options = {}) {
    if (exp.length < 3) {
        return transpileSexp(true, env, options);
    }
    else if (exp.length === 3) {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
            monoidIdentity: true,
            monoidOperator: '<='
        }));
    }
    else {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        let andExp = [Symbol.for('and')];
        for (let i = 2; i < exp.length; i++) {
            andExp.push([Symbol.for('<='), exp[i - 1], exp[i]]);
        }
        return transpileSexp(andExp, env, inheritedOptions);
    }
}
function shouldWrapBraces(letVariables, parent, env) {
    let wrapBraces = false;
    if ((0, functions_1.isCall)(parent, env, lisp_1.letValues)) {
        let parentVariables = (0, functions_1.flatten)(parent[1].map(function (x) {
            if ((0, functions_1.listp_)(x)) {
                return x[0];
            }
            else {
                return x;
            }
        }));
        wrapBraces = !((parent.length === 3) && ((0, functions_1.intersection)(letVariables, parentVariables).length === 0));
    }
    else if (isLetExpression(parent, env)) {
        let parentVariables = parent[1].map(function (x) {
            if ((0, functions_1.listp_)(x)) {
                return x[0];
            }
            else {
                return x;
            }
        });
        wrapBraces = !((parent.length === 3) && ((0, functions_1.intersection)(letVariables, parentVariables).length === 0));
    }
    else if ((0, functions_1.isCall)(parent, env, lisp_1.lambda_)) {
        let params = parent[1];
        let parentVariables = ((0, functions_1.dottedListP)(params) ? (0, functions_1.flatten)(params) : ((0, functions_1.listp_)(params) ? params : [])).map(function (x) {
            if ((0, functions_1.listp_)(x)) {
                return x[0];
            }
            else {
                return x;
            }
        });
        wrapBraces = !((parent.length === 3) && ((0, functions_1.intersection)(letVariables, parentVariables).length === 0));
    }
    else if ((0, functions_1.isCall)(parent, env, lisp_1.if_)) {
        let parentVariables = parent[1].map(function (x) {
            if ((0, functions_1.listp_)(x)) {
                return x[0];
            }
            else {
                return x;
            }
        });
        wrapBraces = !((parent.length === 3) && ((0, functions_1.intersection)(letVariables, parentVariables).length === 0));
    }
    else if ((0, functions_1.isCall)(parent, env, lisp_1.for_)) {
        let parentVariables = parent[1].map(function (x) {
            if ((0, functions_1.listp_)(x)) {
                return x[0];
            }
            else {
                return x;
            }
        });
        wrapBraces = !((parent.length === 3) && ((0, functions_1.intersection)(letVariables, parentVariables).length === 0));
    }
    return wrapBraces;
}
function transpileLet(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        expressionType: undefined,
        wrapBraces: undefined
    }, exp);
    let expressionType = consumedOptions['expressionType'];
    let wrapBraces = consumedOptions['wrapBraces'];
    let indent = inheritedOptions['indent'];
    let language = inheritedOptions['language'];
    let stack = inheritedOptions['stack'];
    inheritedOptions['bindings'] = (0, eval_1.extendEnvironment)(new lisp_1.LispEnvironment(), inheritedOptions['bindings']);
    {
        let bindings = inheritedOptions['bindings'];
        if (expressionType === 'expression') {
            let lambdaCall = [[Symbol.for('lambda'), [], exp]];
            return transpileExpression(lambdaCall, env, inheritedOptions);
        }
        else {
            let letBindings = exp[1];
            let expressions = exp.slice(2);
            if (wrapBraces === undefined) {
                if (stack.length > 1) {
                    let parent = stack[1];
                    let letVariables = letBindings.map(function (x) {
                        if ((0, functions_1.listp_)(x)) {
                            return x[0];
                        }
                        else {
                            return x;
                        }
                    });
                    wrapBraces = shouldWrapBraces(letVariables, parent, env);
                }
                else {
                    wrapBraces = true;
                }
            }
            {
                let transpiledExp = letBindings.map(function (x) {
                    if ((0, functions_1.listp_)(x)) {
                        bindings.setEntry([x[0], [true, 'variable']]);
                        return returnStatementOrExpression('let ' + transpileExpression(x[0], env, inheritedOptions) + ((language === 'TypeScript') ? ': any' : '') + ' = ' + transpileExpression(x[1], env, inheritedOptions), Object.assign(Object.assign({}, inheritedOptions), {
                            expressionType: 'statement',
                            returnStatement: false
                        }));
                    }
                    else {
                        bindings.setEntry([x, [true, 'variable']]);
                        return returnStatementOrExpression('let ' + transpileExpression(x, env, inheritedOptions) + ((language === 'TypeScript') ? ': any' : ''), Object.assign(Object.assign({}, inheritedOptions), {
                            expressionType: 'statement',
                            returnStatement: false
                        }));
                    }
                }).join('\n');
                if (expressions.length > 0) {
                    transpiledExp = transpiledExp + '\n' + transpilePossibleReturnStatement((0, functions_1.prognWrap)(expressions), env, inheritedOptions);
                }
                if (wrapBraces) {
                    transpiledExp = '{\n' + (0, functions_1.indentString)(transpiledExp, indent) + '\n' +
                        '}';
                }
                return transpiledExp;
            }
        }
    }
}
function transpileLetValues(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        wrapBraces: undefined
    }, exp);
    let wrapBraces = consumedOptions['wrapBraces'];
    let indent = inheritedOptions['indent'];
    let language = inheritedOptions['language'];
    let stack = inheritedOptions['stack'];
    let result = '';
    let bindings = exp[1];
    let body = exp.slice(2);
    if (wrapBraces === undefined) {
        if (stack.length > 1) {
            let parent = stack[1];
            let letVariables = (0, functions_1.flatten)(((0, functions_1.dottedListP)(bindings) ? (0, functions_1.flatten)(bindings) : bindings).map(function (x) {
                if ((0, functions_1.listp_)(x)) {
                    return x[0];
                }
                else {
                    return x;
                }
            }));
            wrapBraces = shouldWrapBraces(letVariables, parent, env);
        }
        else {
            wrapBraces = true;
        }
    }
    for (let binding of bindings) {
        let [variables, expression] = binding;
        let regularVars = [];
        let restVar = undefined;
        if ((0, functions_1.dottedListP)(variables)) {
            let varList = (0, functions_1.flatten)(variables);
            regularVars = varList.slice(0, -1);
            restVar = varList[varList.length - 1];
        }
        else {
            regularVars = binding[0];
        }
        result = result + ((result !== '') ? '\n' : '') + 'let [' + regularVars.map(function (x) {
            return transpileExpression(x, env, inheritedOptions);
        }).join(', ') + (restVar ? (', ...' + transpileExpression(restVar, env, inheritedOptions)) : '') + ']' + ((language === 'TypeScript') ? ': any[]' : '') + ' = ' + transpileExpression(expression, env, inheritedOptions) + ';';
    }
    result = result + '\n' + transpilePossibleReturnStatement((0, functions_1.prognWrap)(body), env, inheritedOptions);
    if (wrapBraces) {
        result = '{\n' + (0, functions_1.indentString)(result, indent) + '\n' +
            '}';
    }
    return result;
}
function transpileList(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return returnStatementOrExpression('[' + (0, functions_1.rest_)(exp).map(function (x) {
        return transpileExpression(x, env, inheritedOptions);
    }).join(', ') + ']', inheritedOptions);
}
function transpileMacroCall(exp, env, options = {}) {
    let expansion = (0, lisp_1.macroexpand1)(exp, env);
    return transpileSexp(expansion, env, options);
}
function transpileMethodCall(exp, env, options = {}) {
    let match = (0, symbol_1.symbolToString_)(exp[0]).match(new RegExp('^\\.(.*)$'));
    let method = match[1];
    if (method === '') {
        return transpileDot(exp, env, options);
    }
    else {
        let obj = exp[1];
        if (match = method.match(new RegExp('^-(.*)$'))) {
            let prop = match[1];
            return returnStatementOrExpression(transpileExpression(obj, env, options) + '.' + transpileExpression((0, symbol_1.s_)(prop), env, options), options);
        }
        else {
            return transpileDot([Symbol.for('.'), obj, (0, symbol_1.s_)(method), ...exp.slice(2)], env, options);
        }
    }
}
function transpileMul(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
        monoidIdentity: 1,
        monoidOperator: '*'
    }));
}
function transpileNot(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        wrapParens: undefined
    }, exp);
    let wrapParens = consumedOptions['wrapParens'];
    if (isEqualityExpression(exp[1], env)) {
        return transpileMonoid(exp[1], env, Object.assign(Object.assign({}, options), {
            monoidIdentity: false,
            monoidOperator: '!==',
            wrapParens: wrapParens
        }));
    }
    else {
        let result = '!' + transpileExpression(exp[1], env, Object.assign(Object.assign({}, inheritedOptions), {
            wrapParens: 'smart'
        }));
        if (wrapParens === true) {
            result = wrapInParens(result);
        }
        return returnStatementOrExpression(result, inheritedOptions);
    }
}
function transpileNthMacro(exp, env) {
    return [Symbol.for('aget'), exp[2], exp[1]];
}
function transpileNthcdrMacro(exp, env) {
    return [Symbol.for('drop'), exp[2], exp[1]];
}
function transpileProgn(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptions(options, {
        newlines: undefined,
        returnStatement: undefined
    });
    let newlines = consumedOptions['newlines'];
    let returnStatement = consumedOptions['returnStatement'];
    let bindings = inheritedOptions['bindings'];
    let expressionType = inheritedOptions['expressionType'];
    let body = (0, functions_1.rest_)(exp);
    let transpiledBody = [];
    for (let i = 0; i < body.length; i++) {
        let exp = body[i];
        if ((0, functions_1.isCall)(exp, env, lisp_1.define_)) {
            let sym = ((0, functions_1.listp_)(exp[1]) ? exp[1][0] : exp[1]);
            bindings.setEntry([sym, [true, 'variable']]);
        }
    }
    for (let i = 0; i < body.length; i++) {
        transpiledBody.push(transpileSexp(body[i], env, Object.assign(Object.assign({}, inheritedOptions), {
            returnStatement: returnStatement && (i === (body.length - 1))
        })));
    }
    if (expressionType === 'expression') {
        let result = transpiledBody.join((newlines ? ',\n' : ', '));
        if (exp.length > 2) {
            return result = wrapInParens(result);
        }
    }
    else {
        return transpiledBody.join((newlines ? '\n' +
            '\n' : '\n'));
    }
}
function transpileOr(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        wrapParens: undefined
    }, exp);
    let wrapParens = consumedOptions['wrapParens'];
    let result = transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
        monoidIdentity: false,
        monoidOperator: '||'
    }));
    if (wrapParens) {
        result = wrapInParens(result);
    }
    return result;
}
function transpileProvide(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let indent = inheritedOptions['indent'];
    let expressions = (0, functions_1.rest_)(exp);
    return returnStatementOrExpression('export {\n' + (0, functions_1.indentString)(expressions.map(function (x) {
        if ((0, functions_1.isForm)(x, Symbol.for('rename-out'))) {
            return (0, functions_1.rest_)(x).map(function (x1) {
                return (transpileExpression(x1[0], env, Object.assign(Object.assign({}, inheritedOptions), {
                    literalSymbol: true
                })) + ' as ' + transpileExpression(x1[1], env, Object.assign(Object.assign({}, inheritedOptions), {
                    literalSymbol: true
                })));
            }).join(',\n');
        }
        else {
            return transpileExpression(x, env, Object.assign(Object.assign({}, inheritedOptions), {
                literalSymbol: true
            }));
        }
    }).join(',\n'), indent) + ((expressions.length > 0) ? '\n' : '') + '}', options);
}
function transpileQuote(exp, env, options = {}) {
    let result = '';
    if ((0, functions_1.listp_)(exp[1])) {
        result = transpileExpression([Symbol.for('list'), ...exp[1].map(function (x) {
                return [Symbol.for('quote'), x];
            })], env, options);
    }
    else if ((0, symbol_1.symbolp_)(exp[1])) {
        result = transpileSymbol(exp[1], env, Object.assign(Object.assign({}, options), {
            quotedSymbol: true
        }));
    }
    else {
        result = transpileExpression(exp[1], env, options);
    }
    return returnStatementOrExpression(result, options);
}
function transpileQuasiquote(exp, env, options = {}) {
    return returnStatementOrExpression(quasiquoteHelper(exp[1], env, options), options);
}
function quasiquoteHelper(exp, env, options = {}) {
    if (!(0, functions_1.listp_)(exp)) {
        return transpileExpression([Symbol.for('quote'), exp], env, options);
    }
    else {
        return '[' + exp.map(function (x) {
            if ((0, functions_1.isForm)(x, Symbol.for('unquote'))) {
                return transpileExpression(x[1], env, options);
            }
            else if ((0, functions_1.isForm)(x, Symbol.for('unquote-splicing'))) {
                return ('...' + transpileExpression(x[1], env, options));
            }
            else {
                return quasiquoteHelper(x, env, options);
            }
        }).join(', ') + ']';
    }
}
function transpileRequire(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let esModuleInterop = inheritedOptions['esModuleInterop'];
    let indent = inheritedOptions['indent'];
    let x = exp[1];
    let y = exp[2] || x;
    if ((0, symbol_1.symbolp_)(x)) {
        x = transpileExpression(x, env, Object.assign(Object.assign({}, inheritedOptions), {
            literalSymbol: true
        }));
    }
    if ((0, functions_1.isForm)(x, Symbol.for('only-in'))) {
        y = x[1];
        x = '{\n' + (0, functions_1.indentString)(x.slice(2).map(function (x) {
            if ((0, functions_1.listp_)(x)) {
                return (transpileExpression(x[0], env, Object.assign(Object.assign({}, inheritedOptions), {
                    literalSymbol: true
                })) + ' as ' + transpileExpression(x[1], env, Object.assign(Object.assign({}, inheritedOptions), {
                    literalSymbol: true
                })));
            }
            else {
                return transpileExpression(x, env, Object.assign(Object.assign({}, inheritedOptions), {
                    literalSymbol: true
                }));
            }
        }).join(',\n'), indent) + '\n' +
            '}';
    }
    else {
        x = (esModuleInterop ? '' : '* as ') + x;
    }
    if ((0, symbol_1.symbolp_)(y)) {
        y = (0, symbol_1.symbolToString_)(y);
    }
    return returnStatementOrExpression('import ' + x + ' from \'' + y + '\'', inheritedOptions);
}
function transpileSetq(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        wrapParens: false
    }, exp);
    let wrapParens = consumedOptions['wrapParens'];
    let expressionType = inheritedOptions['expressionType'];
    let returnStatement = inheritedOptions['returnStatement'];
    let sym = exp[1];
    let val = exp[2];
    if ((0, functions_1.isCall)(val, env, functions_1.add) && (((val[1] === sym) && (val[2] === 1)) || ((val[2] === sym) && (val[1] === 1)))) {
        val = [Symbol.for('add1'), sym];
    }
    else if ((0, functions_1.isCall)(val, env, functions_1.sub) && (((val[1] === sym) && (val[2] === 1)) || ((val[2] === sym) && (val[1] === 1)))) {
        val = [Symbol.for('sub1'), sym];
    }
    {
        let result = '';
        if ((0, functions_1.isCall)(val, env, functions_1.add1) && (val[1] === sym)) {
            if ((expressionType === 'statement') && !returnStatement) {
                result = transpileExpression(sym, env, inheritedOptions) + '++';
            }
            else {
                result = '++' + transpileExpression(sym, env, inheritedOptions);
            }
        }
        else if ((0, functions_1.isCall)(val, env, functions_1.sub1) && (val[1] === sym)) {
            if ((expressionType === 'statement') && !returnStatement) {
                result = transpileExpression(sym, env, inheritedOptions) + '--';
            }
            else {
                result = '--' + transpileExpression(sym, env, inheritedOptions);
            }
        }
        else {
            result = transpileExpression(sym, env, inheritedOptions) + ' = ' + transpileExpression(val, env, inheritedOptions);
        }
        if (wrapParens) {
            result = wrapInParens(result);
        }
        return returnStatementOrExpression(result, inheritedOptions);
    }
}
function transpileStatement(exp, env, options = {}) {
    return transpileSexp(exp, env, Object.assign(Object.assign({}, options), {
        expressionType: 'statement',
        returnStatement: false
    }));
}
function transpileReturnStatement(exp, env, options = {}) {
    return transpileSexp(exp, env, Object.assign(Object.assign({}, options), {
        expressionType: 'statement',
        returnStatement: true
    }));
}
function transpilePossibleReturnStatement(exp, env, options = {}) {
    return transpileSexp(exp, env, Object.assign(Object.assign({}, options), {
        expressionType: 'statement'
    }));
}
function transpileString(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptions(options, {});
    let indent = inheritedOptions['indent'];
    let str = exp.replace(new RegExp('\\\\', 'g'), '\\\\').replace(new RegExp('\'', 'g'), '\\\'');
    let lines = str.split('\n');
    if (lines.length <= 1) {
        return returnStatementOrExpression('\'' + str + '\'', options);
    }
    else {
        let penultimateLines = lines.slice(0, -1);
        let lastLine = lines[lines.length - 1];
        let transpiledLines = penultimateLines.map(function (x) {
            return '\'' + x + '\\n\'';
        });
        if (lastLine !== '') {
            transpiledLines.push('\'' + lastLine + '\'');
        }
        for (let i = 1; i < transpiledLines.length; i++) {
            transpiledLines[i] = (0, functions_1.indentString)(transpiledLines[i], indent);
        }
        return returnStatementOrExpression(transpiledLines.join(' +\n'), options);
    }
}
function transpileSub(exp, env, options = {}) {
    if (exp.length === 2) {
        let num = transpileExpression(exp[1], env, options);
        let match;
        if (match = num.match(new RegExp('^-([^]+$)'))) {
            num = match[1];
        }
        else {
            num = '-' + num;
        }
        return returnStatementOrExpression(num, options);
    }
    else {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
            monoidIdentity: 0,
            monoidOperator: '-'
        }));
    }
}
function transpileSymbol(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        literalSymbol: false,
        quotedSymbol: undefined
    }, exp);
    let literalSymbol = consumedOptions['literalSymbol'];
    let quotedSymbol = consumedOptions['quotedSymbol'];
    let camelCaseOption = inheritedOptions['camelCase'];
    let language = inheritedOptions['language'];
    let result = (0, symbol_1.symbolToString_)(exp);
    if (quotedSymbol) {
        result = 'Symbol.for(\'' + result + '\')';
    }
    else if (literalSymbol) {
        if (camelCaseOption) {
            result = (0, functions_1.camelCase)(result);
        }
        else {
            result;
        }
    }
    else if (env.has(exp) && (inheritedOptions.transpilationEnvironment.getBinding(env.get(exp))[1] === 'variable')) {
        result = inheritedOptions.transpilationEnvironment.get(env.get(exp));
    }
    else if (camelCaseOption) {
        result = (0, functions_1.camelCase)(result);
    }
    return returnStatementOrExpression(result, inheritedOptions);
}
function isEqualityExpression(exp, env) {
    return (0, functions_1.isCall)(exp, env, functions_1.eq_) || (0, functions_1.isCall)(exp, env, functions_1.eql_) || (0, functions_1.isCall)(exp, env, functions_1.equal_);
}
function isLetExpression(exp, env) {
    return (0, functions_1.isCall)(exp, env, lisp_1.letStar_);
}
function returnStatementOrExpression(exp, options = {}) {
    let expressionType = options['expressionType'];
    let returnStatement = options['returnStatement'];
    return (returnStatement ? 'return ' : '') + exp + ((expressionType === 'statement') ? ';' : '');
}
function transpileFor(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        returnStatement: false
    }, exp);
    let indent = inheritedOptions['indent'];
    let language = inheritedOptions['language'];
    let decls = exp[1];
    let body = (0, functions_1.prognWrap)(exp.slice(2));
    let decl1 = decls[0];
    let sym = decl1[0];
    let valuesExpr = decl1[1];
    let transpiledBody = (0, functions_1.indentString)(transpilePossibleReturnStatement(body, env, inheritedOptions), indent);
    if ((0, functions_1.isCall)(valuesExpr, env, functions_1.range)) {
        let start = (valuesExpr.length >= 2 ? valuesExpr[1] : undefined);
        let end = (valuesExpr.length >= 3 ? valuesExpr[2] : undefined);
        let step = (4 >= valuesExpr.length ? valuesExpr[3] : undefined);
        start = (end === undefined ? 0 : start);
        end = (end === undefined ? start : end);
        step = step || 1;
        {
            let initialization = '';
            if (language === 'TypeScript') {
                initialization = 'let ' + transpileExpression(sym, env, inheritedOptions) + ': number = ' + transpileExpression(start, env, inheritedOptions);
            }
            else {
                initialization = transpileExpression([Symbol.for('define'), sym, start], env, inheritedOptions);
            }
            {
                let testCondition = '';
                let increment = '';
                if ((0, functions_1.numberp)(step)) {
                    if (step < 0) {
                        testCondition = transpileExpression([Symbol.for('>'), sym, end], env, inheritedOptions);
                        increment = transpileSexp([Symbol.for('set!'), sym, [Symbol.for('-'), sym, Math.abs(step)]], env, Object.assign(Object.assign({}, inheritedOptions), {
                            expressionType: 'statement'
                        })).replace(new RegExp(';$'), '');
                    }
                    else {
                        testCondition = transpileExpression([Symbol.for('<'), sym, end], env, inheritedOptions);
                        increment = transpileSexp([Symbol.for('set!'), sym, [Symbol.for('+'), sym, step]], env, Object.assign(Object.assign({}, inheritedOptions), {
                            expressionType: 'statement'
                        })).replace(new RegExp(';$'), '');
                    }
                }
                else {
                    testCondition = transpileExpression([Symbol.for('if'), [Symbol.for('<'), step, 0], [Symbol.for('>'), sym, end], [Symbol.for('<'), sym, end]], env, inheritedOptions);
                    increment = transpileExpression([Symbol.for('set!'), sym, [Symbol.for('+'), sym, step]], env, inheritedOptions);
                }
                return 'for (' + initialization + '; ' + testCondition + '; ' + increment + ') {\n' + transpiledBody + '\n' +
                    '}';
            }
        }
    }
    else {
        return 'for (let ' + transpileExpression(sym, env, inheritedOptions) + ' of ' + transpileExpression(valuesExpr, env, inheritedOptions) + ') {\n' + transpiledBody + '\n' +
            '}';
    }
}
function transpileBreak(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        returnStatement: undefined
    }, exp);
    return returnStatementOrExpression('break', inheritedOptions);
}
function transpileContinue(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        returnStatement: undefined
    }, exp);
    return returnStatementOrExpression('continue', inheritedOptions);
}
function transpileTypeof(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return returnStatementOrExpression('typeof ' + transpileExpression(exp[1], env, inheritedOptions), inheritedOptions);
}
function transpileInstanceof(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {
        wrapParens: undefined
    }, exp);
    let wrapParens = consumedOptions['wrapParens'];
    let result = transpileExpression(exp[1], env, inheritedOptions) + ' instanceof ' + transpileExpression(exp[2], env, inheritedOptions);
    if (wrapParens) {
        result = wrapInParens(result);
    }
    return returnStatementOrExpression(result, options);
}
function transpileNew(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return returnStatementOrExpression('new ' + transpileFunctionCall(exp.slice(1), env, Object.assign(Object.assign({}, inheritedOptions), {
        expressionType: 'expression',
        returnStatement: false
    })), inheritedOptions);
}
function transpileFirstMacro(exp, env) {
    let lst = exp[1];
    return [Symbol.for('aref'), lst, 0];
}
function transpileLastMacro(exp, env) {
    let lst = exp[1];
    return [Symbol.for('aref'), lst, [Symbol.for('-'), [Symbol.for('length'), lst], 1]];
}
function transpileValuesMacro(exp, env) {
    return [Symbol.for('list'), ...(0, functions_1.rest_)(exp)];
}
function transpileGensymMacro(exp, env) {
    return [Symbol.for('Symbol'), ...(0, functions_1.rest_)(exp)];
}
function transpileWhile(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let indent = inheritedOptions['indent'];
    let stack = inheritedOptions['stack'];
    let condition = exp[1];
    let body = (0, functions_1.prognWrap)(exp.slice(2));
    return 'while (' + transpileExpression(condition, env, inheritedOptions) + ') {\n' + (0, functions_1.indentString)(transpilePossibleReturnStatement(body, env, inheritedOptions), indent) + '\n' +
        '}';
}
function transpileYield(exp, env, options = {}) {
    return returnStatementOrExpression('yield ' + transpileExpression(exp[1], env, options), Object.assign(Object.assign({}, options), {
        expressionType: 'statement',
        returnStatement: false
    }));
}
function transpileThrow(exp, env, options = {}) {
    return returnStatementOrExpression('throw ' + transpileExpression(exp[1], env, options), Object.assign(Object.assign({}, options), {
        expressionType: 'statement',
        returnStatement: false
    }));
}
function transpileReturn(exp, env, options = {}) {
    return returnStatementOrExpression(transpileExpression(exp[1], env, options), Object.assign(Object.assign({}, options), {
        expressionType: 'statement',
        returnStatement: true
    }));
}
function transpileAdd1Macro(exp, env) {
    return [Symbol.for('+'), exp[1], 1];
}
function transpileSub1Macro(exp, env) {
    return [Symbol.for('-'), exp[1], 1];
}
function transpileStringAppend(exp, env, options = {}) {
    if (exp.length <= 0) {
        return '';
    }
    else if (exp.length === 2) {
        return exp[1];
    }
    else {
        let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
        return transpileMonoid(exp, env, Object.assign(Object.assign({}, inheritedOptions), {
            monoidIdentity: '',
            monoidOperator: '+'
        }));
    }
}
function transpileClass(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let indent = inheritedOptions['indent'];
    let className = exp[1];
    let transpiledClassName = ((0, symbol_1.symbolp_)(className) ? transpileExpression(className, env, inheritedOptions) : '');
    let body = (transpiledClassName === '' ? exp.slice(1) : exp.slice(2));
    if ((0, functions_1.listp_)(body[0]) && !(0, functions_1.isCall)(body[0], env, lisp_1.define_)) {
        let superClasses = body[0];
        body = body.slice(1);
        if (superClasses.length > 0) {
            transpiledClassName = transpiledClassName + ' extends ' + transpileExpression(superClasses[0], env, inheritedOptions);
        }
    }
    {
        let transpiledBody = (0, functions_1.indentString)(body.map(function (x) {
            if ((0, functions_1.isCall)(x, env, lisp_1.define_) && (0, functions_1.consp)(x[1]) && (x[1][0] === Symbol.for('constructor'))) {
                return transpileSexp(x, env, Object.assign(Object.assign({}, inheritedOptions), {
                    expressionType: 'statement',
                    returnStatement: false,
                    returnType: 'void'
                })).replace(new RegExp('^function |let '), '').replace(new RegExp(': void {'), ' {');
            }
            else {
                return transpileSexp(x, env, Object.assign(Object.assign({}, inheritedOptions), {
                    expressionType: 'statement',
                    returnStatement: false
                })).replace(new RegExp('^function |let '), '');
            }
        }).join('\n' +
            '\n'), indent);
        let result = 'class ' + ((transpiledClassName === '') ? '' : (transpiledClassName + ' ')) + '{\n' + transpiledBody + ((transpiledBody !== '') ? '\n' : '') + '}';
        return result;
    }
}
function wrapInParens(str) {
    return '(' + str + ')';
}
function transpileJsObj(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let indent = inheritedOptions['indent'];
    let transpiledOptionsArr = [];
    let transpiledOptions = '';
    for (let i = 1; i < exp.length; i = i + 2) {
        let transpiledKey = transpileExpression(exp[i], env, inheritedOptions);
        let match;
        if (match = transpiledKey.match(new RegExp('^\'([a-z]+)\'$', 'i'))) {
            transpiledKey = match[1];
        }
        {
            let transpiledValue = transpileExpression(exp[i + 1], env, inheritedOptions);
            transpiledOptionsArr.push(transpiledKey + ': ' + transpiledValue);
        }
    }
    transpiledOptions = transpiledOptionsArr.join(',\n');
    return returnStatementOrExpression((transpiledOptions === '' ? '{}' : '{\n' + (0, functions_1.indentString)(transpiledOptions, indent) + '\n' +
        '}'), inheritedOptions);
}
function transpileAppend(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    return returnStatementOrExpression('[' + exp.slice(1).map(function (x) {
        return ('...' + transpileExpression(x, env, inheritedOptions));
    }).join(', ') + ']', inheritedOptions);
}
function transpileJsObjAppend(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let result = exp.slice(1).map(function (x) {
        return '...' + transpileExpression(x, env, inheritedOptions);
    }).join(', ');
    result = (result === '' ? '{}' : '{ ' + result + ' }');
    return returnStatementOrExpression(result, inheritedOptions);
}
function parseOptions(options, consumedOptions = {}) {
    let consumedOptionsResult = Object.assign({}, consumedOptions);
    let inheritedOptions = {};
    for (let key of Object.keys(options || {})) {
        if (consumedOptions.hasOwnProperty(key)) {
            consumedOptionsResult[key] = options[key];
        }
        else {
            inheritedOptions[key] = options[key];
        }
    }
    inheritedOptions['stack'] = inheritedOptions['stack'] || [];
    inheritedOptions['bindings'] = inheritedOptions['bindings'] || new lisp_1.LispEnvironment();
    return [consumedOptionsResult, inheritedOptions];
}
function parseOptionsWithStack(options, consumedOptionNames, exp) {
    let [consumedOptions, inheritedOptions] = parseOptions(options, consumedOptionNames);
    inheritedOptions['stack'] = [exp, ...inheritedOptions['stack']];
    return [consumedOptions, inheritedOptions];
}
function transpileTry(exp, env, options = {}) {
    let [consumedOptions, inheritedOptions] = parseOptionsWithStack(options, {}, exp);
    let indent = inheritedOptions['indent'];
    let bodyClauses = [];
    let catchClauses = [];
    let finallyClauses = [];
    let transpiledBody = '';
    let transpiledCatchClauses = '';
    let transpiledFinallyClause = '';
    let result = '';
    for (let x of exp.slice(1)) {
        if ((0, functions_1.isForm)(x, Symbol.for('catch'))) {
            catchClauses.push(x);
        }
        else if ((0, functions_1.isForm)(x, Symbol.for('finally'))) {
            finallyClauses.push(x);
        }
        else {
            bodyClauses.push(x);
        }
    }
    transpiledBody = (0, functions_1.indentString)(transpileStatement((0, functions_1.prognWrap)(bodyClauses), env, inheritedOptions), indent);
    transpiledBody = 'try {\n' + transpiledBody + ((transpiledBody !== '') ? '\n' : '') + '}';
    if (catchClauses.length > 0) {
        let sym = catchClauses[0][2];
        let condExp = [Symbol.for('cond'), ...catchClauses.map(function (x) {
                return [[Symbol.for('instanceof'), sym, x[1]], ...x.slice(3)];
            })];
        transpiledCatchClauses = 'catch (' + transpileExpression(sym, env, inheritedOptions) + ') {\n' + (0, functions_1.indentString)(transpileStatement(condExp, env, inheritedOptions), indent) + '\n' +
            '}';
    }
    if (finallyClauses.length > 0) {
        let finallyClause = finallyClauses[0];
        transpiledFinallyClause = 'finally {\n' + (0, functions_1.indentString)(transpileStatement((0, functions_1.prognWrap)(finallyClause.slice(1)), env, inheritedOptions), indent) + '\n' +
            '}';
    }
    result = transpiledBody + ((transpiledCatchClauses !== '') ? (' ' + transpiledCatchClauses) : '') + ((transpiledFinallyClause !== '') ? (' ' + transpiledFinallyClause) : '');
    return returnStatementOrExpression(result, inheritedOptions);
}
function transpileMapMacro(exp, env) {
    return [Symbol.for('.map'), exp[2], exp[1]];
}
function transpileDisplayMacro(exp, env) {
    return [Symbol.for('.log'), Symbol.for('console'), ...exp.slice(1)];
}
function transpileListpMacro(exp, env) {
    return [Symbol.for('.isArray'), Symbol.for('Array'), exp[1]];
}
function transpileStringpMacro(exp, env) {
    return [Symbol.for('eq'), [Symbol.for('typeof'), exp[1]], 'string'];
}
