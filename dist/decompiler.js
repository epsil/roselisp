"use strict";
// SPDX-License-Identifier: MPL-2.0
/**
 * # Decompiler
 *
 * Decompile from JavaScript/TypeScript to Lisp.
 *
 * ## Description
 *
 * Simple decompiler implementation translating JavaScript or
 * TypeScript code to Lisp code.
 *
 * ### Comments
 *
 * Note that currently, there is no support for comments as the
 * parser we are using, `@typescript-eslint/parser`, throws them
 * away.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.decompileModule = exports.decompileFilesX = exports.decompileFileX = exports.decompile = void 0;
const fs_1 = require("fs");
const path_1 = require("path");
const typescript_estree_1 = require("@typescript-eslint/typescript-estree");
const estree_1 = require("./estree");
const rose_1 = require("./rose");
const printer_1 = require("./printer");
const util_1 = require("./util");
const [lastCdr, cons, findf, listStar] = (() => {
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
    function findf_(proc, lst, notFound = false) {
        const idx = lst.findIndex(proc);
        if (idx >= 0) {
            return lst[idx];
        }
        else {
            return notFound;
        }
    }
    function listStar_(...args) {
        if (args.length === 0) {
            return undefined;
        }
        else if (args.length === 1) {
            return args[0];
        }
        else {
            const tailLst = args[args.length - 1];
            const headLst = args.slice(0, -1);
            if (Array.isArray(tailLst)) {
                return [...headLst, ...tailLst];
            }
            else {
                return [...headLst, Symbol.for('.'), tailLst];
            }
        }
    }
    return [lastCdr_, cons_, findf_, listStar_];
})();
/**
 * Decompile a JavaScript or TypeScript program.
 */
function decompile(x, options = {}) {
    let language = options['language'];
    if (language === 'TypeScript') {
        return decompileTs(x, options);
    }
    else {
        return decompileJs(x, options);
    }
}
exports.decompile = decompile;
/**
 * Read a JavaScript or TypeScript program from disk
 * and decompile it. The result is written to disk.
 */
function decompileFileX(file, options = {}) {
    // TODO: Refactor to `(compile-file in-file out-file options)`?
    const extension = (0, path_1.extname)(file);
    const stem = (0, path_1.basename)(file, extension);
    let language = options['language'];
    const inDir = (0, path_1.dirname)(file);
    const outDir = options['outDir'] || inDir;
    const outExtension = '.scm';
    const outFile = (0, path_1.join)(outDir, stem + outExtension);
    let code;
    let data;
    language = (language.match(new RegExp('^TypeScript$', 'i')) || (extension === '.ts')) ? 'TypeScript' : 'JavaScript';
    options = Object.assign(Object.assign({}, options), { language: language, module: true, noModuleForm: true, pretty: true });
    data = (0, fs_1.readFileSync)(file, {
        encoding: 'utf8'
    });
    code = decompile(data, options);
    (0, fs_1.mkdirSync)(outDir, {
        recursive: true
    });
    (0, fs_1.writeFileSync)(outFile, code, {
        encoding: 'utf8'
    });
    console.log('Decompiled ' + file + ' to ' + outFile);
    return file;
}
exports.decompileFileX = decompileFileX;
/**
 * Read JavaScript or TypeScript programs from disk
 * and decompile them. The results are written to disk.
 */
function decompileFilesX(files, options = {}) {
    for (let file of files) {
        decompileFileX(file, options);
    }
    return files;
}
exports.decompileFilesX = decompileFilesX;
/**
 * Decompile a JavaScript or TypeScript module.
 */
function decompileModule(m, options = {}) {
    // TODO
    return m;
}
exports.decompileModule = decompileModule;
/**
 * Decompile a JavaScript program
 * (i.e., an [ESTree][github:estree]
 * [AST][w:Abstract syntax tree]).
 *
 * [github:estree]: https://github.com/estree/estree
 * [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
 */
function decompileJs(x, options = {}) {
    return decompileTs(x, options);
}
/**
 * Decompile a TypeScript program
 * (i.e., a [TSESTree][npm:typescript-estree]
 * [AST][w:Abstract syntax tree]).
 *
 * [npm:typescript-estree] https://www.npmjs.com/package/@typescript-eslint/typescript-estree
 * [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
 */
function decompileTs(x, options = {}) {
    const ast = (0, typescript_estree_1.parse)(x);
    const resultNode = decompileEstree(ast, options);
    let result = resultNode.getValue();
    if (!options['sexp']) {
        result = (0, printer_1.writeToString)(result, options);
    }
    return result;
}
/**
 * Decompile an [ESTree][github:estree] node
 * (i.e., a JavaScript [AST][w:Abstract syntax tree]).
 *
 * [github:estree]: https://github.com/estree/estree
 * [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
 */
function decompileEstree(node, options = {}) {
    const type = node && (0, estree_1.estreeType)(node);
    const decompiler = decompilerMap.get(type) || defaultDecompiler;
    return decompiler(node, options);
}
/**
 * Decompile an ESTree [`Program`][estree:program] node
 * (i.e., a JavaScript program).
 *
 * [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
 */
function decompileProgram(node, options = {}) {
    const moduleOption = options['module'];
    let result = (0, rose_1.makeRose)([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), ...node.body.map(function (x) {
            return decompileEstree(x, options);
        })]);
    if (!moduleOption) {
        result = (0, rose_1.makeRose)([Symbol.for('begin'), ...result.drop(3)]);
        if (result.getValue().length === 2) {
            result = result.get(1);
        }
    }
    return result;
}
/**
 * Decompile an ESTree [`ExpressionStatement`][estree:expressionstatement] node.
 *
 * [estree:expressionstatement]: https://github.com/estree/estree/blob/master/es5.md#expressionstatement
 */
function decompileExpressionStatement(node, options = {}) {
    return decompileEstree(node.expression, options);
}
/**
 * Decompile an ESTree [`CallExpression`][estree:callexpression] node.
 *
 * [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
 */
function decompileCallExpression(node, options = {}) {
    function isSpreadElement(x) {
        return x && (0, estree_1.estreeTypeP)(x, 'SpreadElement');
    }
    const callee = node.callee;
    const calleeDecompiled = decompileEstree(callee, options);
    const calleeDecompiledExp = calleeDecompiled.getValue();
    const args = node.arguments;
    const spreadIdx = args.findIndex(isSpreadElement);
    const isSpread = Number.isFinite(spreadIdx) && (spreadIdx >= 0);
    const isSpreadLast = Number.isFinite(spreadIdx) && (spreadIdx === (args.length - 1));
    const argsDecompiled = (isSpread && !isSpreadLast && (args.length > 1)) ? [[Symbol.for('append'), ...args.map(function (x) {
                let result = decompileEstree(x, options);
                if (isSpreadElement(x)) {
                    return result;
                }
                else {
                    return [Symbol.for('list'), result];
                }
            })]] : args.map(function (x) {
        return decompileEstree(x, options);
    });
    if ((0, util_1.taggedListP)(calleeDecompiledExp, Symbol.for('get-field'))) {
        return (0, rose_1.makeRose)([isSpread ? Symbol.for('send/apply') : Symbol.for('send'), calleeDecompiled.get(2), calleeDecompiled.get(1), ...argsDecompiled]);
    }
    else {
        return (0, rose_1.makeRose)([...(isSpread ? [Symbol.for('apply')] : []), calleeDecompiled, ...argsDecompiled]);
    }
}
/**
 * Decompile an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
 *
 * [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
 */
function decompileAssignmentExpression(node, options = {}) {
    let op = node.operator;
    let left = node.left;
    const leftDecompiled = decompileEstree(left);
    let leftExp = leftDecompiled.getValue();
    const right = node.right;
    let rightDecompiled = decompileEstree(right);
    if (op === '+=') {
        rightDecompiled = (0, rose_1.makeRose)([Symbol.for('+'), leftDecompiled, rightDecompiled]);
    }
    if ((0, util_1.taggedListP)(leftExp, Symbol.for('get-field'))) {
        return (0, rose_1.makeRose)([Symbol.for('set-field!'), leftDecompiled.get(1), leftDecompiled.get(2), rightDecompiled]);
    }
    else if ((0, util_1.taggedListP)(leftExp, Symbol.for('aget'))) {
        return (0, rose_1.makeRose)([Symbol.for('aset!'), ...leftDecompiled.drop(1), rightDecompiled]);
    }
    else if ((0, util_1.taggedListP)(leftExp, Symbol.for('oget'))) {
        return (0, rose_1.makeRose)([Symbol.for('oset!'), ...leftDecompiled.drop(1), rightDecompiled]);
    }
    else if ((0, estree_1.estreeTypeP)(left, 'ArrayPattern')) {
        return (0, rose_1.makeRose)([Symbol.for('set!-values'), leftDecompiled, rightDecompiled]);
    }
    else if ((0, estree_1.estreeTypeP)(left, 'ObjectPattern')) {
        return (0, rose_1.makeRose)([Symbol.for('set!-js-obj'), leftDecompiled, rightDecompiled]);
    }
    else {
        return (0, rose_1.makeRose)([Symbol.for('set!'), leftDecompiled, rightDecompiled]);
    }
}
/**
 * Decompile an ESTree [`AssignmentPattern`][estree:assignmentpattern] node.
 *
 * [estree:assignmentpattern]: https://github.com/estree/estree/blob/master/es2015.md#assignmentpattern
 */
function decompileAssignmentPattern(node, options = {}) {
    const assignment = decompileAssignmentExpression(node, options);
    let left = node.left;
    const typ = left.typeAnnotation;
    if (typ) {
        return (0, rose_1.makeRose)([assignment.get(1), Symbol.for(':'), decompileEstree(typ, options), assignment.get(2)]);
    }
    else {
        return (0, rose_1.makeRose)(assignment.drop(1));
    }
}
/**
 * Decompile an ESTree [`UnaryExpression`][estree:unaryexpression] node.
 *
 * [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
 */
function decompileUnaryExpression(node, options = {}) {
    let op = node.operator;
    const opDecompiled = (op === 'typeof') ? Symbol.for('type-of') : ((op === 'delete') ? Symbol.for('js/delete') : Symbol.for(op));
    const argument = node.argument;
    const argumentDecompiled = decompileEstree(argument, options);
    const argumentDecompiledExp = argumentDecompiled.getValue();
    if (op === '!') {
        return (0, rose_1.makeRose)([Symbol.for('not'), argumentDecompiled]);
    }
    else if ((op === '-') && Number.isFinite(argumentDecompiledExp)) {
        return (0, rose_1.makeRose)(-argumentDecompiledExp);
    }
    else {
        return (0, rose_1.makeRose)([opDecompiled, argumentDecompiled]);
    }
}
/**
 * Decompile an ESTree [`UpdateExpression`][estree:updateexpression] node.
 *
 * [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
 */
function decompileUpdateExpression(node, options = {}) {
    let op = node.operator;
    const argument = decompileEstree(node.argument, options);
    const prefix = node.prefix;
    let result = false;
    if (op === '++') {
        result = [Symbol.for('set!'), argument, [Symbol.for('+'), argument, 1]];
    }
    else if (op === '--') {
        result = [Symbol.for('set!'), argument, [Symbol.for('-'), argument, 1]];
    }
    if (!prefix) {
        result = [Symbol.for('begin0'), argument, result];
    }
    return (0, rose_1.makeRose)(result);
}
/**
 * Decompile an ESTree [`BinaryExpression`][estree:binaryexpression] node.
 *
 * [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
 */
function decompileBinaryExpression(node, options = {}) {
    function isStringExpression(exp) {
        return (typeof exp === 'string') || (0, util_1.taggedListP)(exp, Symbol.for('string-append'));
    }
    const operator = node.operator;
    let op = ((operator === '===') || (operator === '!==')) ? Symbol.for('eq?') : (((operator === '==') || (operator === '!=')) ? Symbol.for('equal?') : ((operator === '&&') ? Symbol.for('and') : ((operator === '||') ? Symbol.for('or') : ((operator === 'in') ? Symbol.for('js/in') : ((operator === 'instanceof') ? Symbol.for('is-a?') : Symbol.for(operator))))));
    let left = node.left;
    const leftDecompiled = decompileEstree(left);
    const leftDecompiledExp = leftDecompiled.getValue();
    const right = node.right;
    let rightDecompiled = decompileEstree(right);
    const rightDecompiledExp = rightDecompiled.getValue();
    if ((op === Symbol.for('+')) && (isStringExpression(leftDecompiledExp) || isStringExpression(rightDecompiledExp))) {
        op = Symbol.for('string-append');
    }
    const leftOperands = ((0, util_1.taggedListP)(leftDecompiledExp, op) || ((op === Symbol.for('string-append')) && (0, util_1.taggedListP)(leftDecompiledExp, Symbol.for('+')))) ? leftDecompiled.drop(1) : [leftDecompiled];
    const rightOperands = (((0, util_1.taggedListP)(rightDecompiledExp, op) && (op !== Symbol.for('-')) && (op !== Symbol.for('/'))) || ((op === Symbol.for('string-append')) && (0, util_1.taggedListP)(rightDecompiledExp, Symbol.for('+')))) ? rightDecompiled.drop(1) : [rightDecompiled];
    if ((operator === '!==') || (operator === '!=')) {
        return (0, rose_1.makeRose)([Symbol.for('not'), [op, ...leftOperands, ...rightOperands]]);
    }
    else {
        return (0, rose_1.makeRose)([op, ...leftOperands, ...rightOperands]);
    }
}
/**
 * Decompile an ESTree [`LogicalExpression`][estree:logicalexpression] node.
 *
 * [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
 */
function decompileLogicalExpression(node, options = {}) {
    return decompileBinaryExpression(node, options);
}
/**
 * Decompile an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
 *
 * [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
 */
function decompileVariableDeclaration(node, options = {}) {
    const decls = node.declarations.map(function (x) {
        return decompileEstree(x, options);
    });
    let result = (0, rose_1.makeRose)(decls).setValue(new rose_1.RoseSplice());
    if (decls.length === 1) {
        result = result.get(0);
    }
    return result;
}
/**
 * Decompile an ESTree [`VariableDeclarator`][estree:variabledeclarator] node.
 *
 * [estree:variabledeclarator]: https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
 */
function decompileVariableDeclarator(node, options = {}) {
    const id = node.id;
    const idType = (0, estree_1.estreeType)(id);
    const init = node.init;
    const defineSym = (idType === 'ArrayPattern') ? Symbol.for('define-values') : ((idType === 'ObjectPattern') ? Symbol.for('define-js-obj') : Symbol.for('define'));
    return (0, rose_1.makeRose)([defineSym, decompileEstree(id, options), ...(((init === undefined) || (init === null)) ? [] : [decompileEstree(init, options)])]);
}
/**
 * Decompile an ESTree [`Identifier`][estree:identifier] node.
 *
 * [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
 */
function decompileIdentifier(node, options = {}) {
    const name = Symbol.for(node.name);
    return (0, rose_1.makeRose)(name);
}
/**
 * Decompile an ESTree [`Literal`][estree:literal] node.
 *
 * [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
 */
function decompileLiteral(node, options = {}) {
    const value = node.value;
    if (value === null) {
        return (0, rose_1.makeRose)(Symbol.for('js/null'));
    }
    else if (typeof value === 'boolean') {
        return (0, rose_1.makeRose)(value ? Symbol.for('#t') : Symbol.for('#f'));
    }
    else if (value instanceof RegExp) {
        const pattern = node.regex.pattern;
        const flags = node.regex.flags;
        // TODO: We can emit `regexp` instead of `js/regexp`
        // provided it is not the name of a local variable.
        return (0, rose_1.makeRose)([Symbol.for('js/regexp'), pattern, ...(flags ? [flags] : [])]);
    }
    else {
        return (0, rose_1.makeRose)(value);
    }
}
/**
 * Decompile an ESTree [`MemberExpression`][estree:memberexpression] node.
 *
 * [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
 */
function decompileMemberExpression(node, options = {}) {
    const property = decompileEstree(node.property, options);
    const propertyExp = property.getValue();
    const object = decompileEstree(node.object, options);
    const objectExp = object.getValue();
    const computed = node.computed;
    if (computed) {
        if (Number.isFinite(propertyExp)) {
            return (0, rose_1.makeRose)([Symbol.for('aget'), ...((0, util_1.taggedListP)(objectExp, Symbol.for('aget')) ? object.drop(1) : [object]), property]);
        }
        else {
            return (0, rose_1.makeRose)([Symbol.for('oget'), object, property]);
        }
    }
    else {
        return (0, rose_1.makeRose)([Symbol.for('get-field'), property, object]);
    }
}
/**
 * Decompile an ESTree [`ChainExpression`][estree:chainexpression] node.
 *
 * [estree:chainexpression]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
 */
function decompileChainExpression(node, options = {}) {
    const expression = node.expression;
    const expressionDecompiled = decompileEstree(expression, options);
    const expressionDecompiledExp = expressionDecompiled.getValue();
    if ((0, util_1.taggedListP)(expressionDecompiledExp, Symbol.for('get-field'))) {
        if (typeof expressionDecompiledExp[2] === 'symbol') {
            return (0, rose_1.makeRose)([Symbol.for('and'), [Symbol.for('field-bound?'), expressionDecompiled.get(1), expressionDecompiled.get(2)], expressionDecompiled]);
        }
        else {
            return (0, rose_1.makeRose)([Symbol.for('~>'), expressionDecompiled.get(2), [Symbol.for('and'), [Symbol.for('field-bound?'), expressionDecompiled.get(1), Symbol.for('_')], [Symbol.for('get-field'), expressionDecompiled.get(1), Symbol.for('_')]]]);
        }
    }
    else if ((0, util_1.taggedListP)(expressionDecompiledExp, Symbol.for('send'))) {
        if (typeof expressionDecompiledExp[1] === 'symbol') {
            return (0, rose_1.makeRose)([Symbol.for('and'), [Symbol.for('field-bound?'), expressionDecompiled.get(2), expressionDecompiled.get(1)], expressionDecompiled]);
        }
        else {
            return (0, rose_1.makeRose)([Symbol.for('~>'), expressionDecompiled.get(1), [Symbol.for('and'), [Symbol.for('field-bound?'), expressionDecompiled.get(2), Symbol.for('_')], [Symbol.for('send'), Symbol.for('_'), ...expressionDecompiled.drop(2)]]]);
        }
    }
    else {
        return expressionDecompiled;
    }
}
/**
 * Decompile an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
 *
 * [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
 */
function decompileFunctionDeclaration(node, options = {}) {
    return decompileFunction(node, options);
}
/**
 * Decompile an ESTree [`FunctionExpression`][estree:functionexpression] node.
 *
 * [estree:functionexpression]: https://github.com/estree/estree/blob/master/es5.md#functionexpression
 */
function decompileFunctionExpression(node, options = {}) {
    return decompileFunction(node, options);
}
/**
 * Decompile an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
 *
 * [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
 */
function decompileArrowFunctionExpression(node, options = {}) {
    return decompileFunction(node, options);
}
/**
 * Decompile an ESTree [`RestElement`][estree:restelement] node.
 *
 * [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
 */
function decompileRestElement(node, options = {}) {
    return decompileEstree(node.argument, options);
}
/**
 * Decompile an ESTree [`BlockStatement`][estree:blockstatement] node.
 *
 * [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
 */
function decompileBlockStatement(node, options = {}) {
    return (0, rose_1.makeRose)([Symbol.for('begin'), ...node.body.map(function (x) {
            return decompileEstree(x, options);
        })]);
}
/**
 * Decompile an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
 *
 * [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
 */
function decompileSequenceExpression(node, options = {}) {
    return (0, rose_1.makeRose)([Symbol.for('begin'), ...node.expressions.map(function (x) {
            return decompileEstree(x, options);
        })]);
}
/**
 * Decompile an ESTree [`ReturnStatement`][estree:returnstatement] node.
 *
 * [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
 */
function decompileReturnStatement(node, options = {}) {
    const argument = node.argument;
    if (argument) {
        return (0, rose_1.makeRose)([Symbol.for('return'), decompileEstree(argument, options)]);
    }
    else {
        return (0, rose_1.makeRose)([Symbol.for('return')]);
    }
}
/**
 * Decompile an ESTree [`IfStatement`][estree:ifstatement] node.
 *
 * [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
 */
function decompileIfStatement(node, options = {}) {
    const test = decompileEstree(node.test, options);
    const testExp = test.getValue();
    let consequent = decompileEstree(node.consequent, options);
    let consequentExp = consequent.getValue();
    let alternate = node.alternate ? decompileEstree(node.alternate, options) : false;
    let alternateExp = alternate && alternate.getValue();
    if ((0, util_1.taggedListP)(consequentExp, Symbol.for('begin')) && (consequentExp.length === 2)) {
        consequent = consequent.get(1);
        consequentExp = consequent.getValue();
    }
    if ((0, util_1.taggedListP)(alternateExp, Symbol.for('begin')) && (alternateExp.length === 2)) {
        alternate = alternate.get(1);
        alternateExp = alternate.getValue();
    }
    if ((0, util_1.taggedListP)(alternateExp, Symbol.for('when'))) {
        return (0, rose_1.makeRose)([Symbol.for('cond'), [test, ...((0, util_1.taggedListP)(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], [...alternate.drop(1)]]);
    }
    else if ((0, util_1.taggedListP)(alternateExp, Symbol.for('unless'))) {
        return (0, rose_1.makeRose)([Symbol.for('cond'), [test, ...((0, util_1.taggedListP)(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], [[Symbol.for('not'), alternate.get(1)], ...alternate.drop(2)]]);
    }
    else if ((0, util_1.taggedListP)(alternateExp, Symbol.for('if'))) {
        const alternateTest = alternate.get(1);
        const alternateConsequent = alternate.get(2);
        const alternateConsequentExp = alternateConsequent.getValue();
        return (0, rose_1.makeRose)([Symbol.for('cond'), [test, ...((0, util_1.taggedListP)(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], [alternateTest, ...((0, util_1.taggedListP)(alternateConsequentExp, Symbol.for('begin')) ? alternateConsequent.drop(1) : [alternateConsequent])], ...((alternateExp.length > 3) ? [[Symbol.for('else'), ...alternate.drop(3)]] : [])]);
    }
    else if ((0, util_1.taggedListP)(alternateExp, Symbol.for('cond'))) {
        return (0, rose_1.makeRose)([Symbol.for('cond'), [test, ...((0, util_1.taggedListP)(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], ...alternate.drop(1)]);
    }
    else if (!alternate) {
        if ((0, util_1.taggedListP)(testExp, Symbol.for('not'))) {
            return (0, rose_1.makeRose)([Symbol.for('unless'), test.get(1), ...((0, util_1.taggedListP)(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])]);
        }
        else {
            return (0, rose_1.makeRose)([Symbol.for('when'), test, ...((0, util_1.taggedListP)(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])]);
        }
    }
    else if ((0, util_1.taggedListP)(consequentExp, Symbol.for('begin')) || (0, util_1.taggedListP)(alternateExp, Symbol.for('begin'))) {
        return (0, rose_1.makeRose)([Symbol.for('cond'), [test, ...((0, util_1.taggedListP)(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], [Symbol.for('else'), ...((0, util_1.taggedListP)(alternateExp, Symbol.for('begin')) ? alternate.drop(1) : [alternate])]]);
    }
    else {
        return (0, rose_1.makeRose)([Symbol.for('if'), test, consequent, alternate]);
    }
}
/**
 * Decompile an ESTree [`WhileStatement`][estree:whilestatement] node.
 *
 * [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
 */
function decompileWhileStatement(node, options = {}) {
    const test = decompileEstree(node.test, options);
    let body = decompileEstree(node.body, options);
    const bodyExp = body.getValue();
    let result = (0, rose_1.makeRose)([Symbol.for('do'), [], [[Symbol.for('not'), test]], ...((0, util_1.taggedListP)(bodyExp, Symbol.for('begin')) ? body.drop(1) : [body])]);
    return result;
}
/**
 * Decompile an ESTree [`DoWhileStatement`][estree:dowhilestatement] node.
 *
 * [estree:dowhilestatement]: https://github.com/estree/estree/blob/master/es5.md#dowhilestatement
 */
function decompileDoWhileStatement(node, options = {}) {
    const test = decompileEstree(node.test, options);
    let body = decompileEstree(node.body, options);
    const bodyExp = body.getValue();
    if ((0, util_1.taggedListP)(bodyExp, Symbol.for('begin')) && (bodyExp.length === 2)) {
        body = body.get(1);
    }
    let result = (0, rose_1.makeRose)([Symbol.for('js/do-while'), body, test]);
    return result;
}
/**
 * Decompile an ESTree [`ForStatement`][estree:forstatement] node.
 *
 * [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
 */
function decompileForStatement(node, options = {}) {
    const init = decompileEstree(node.init, options);
    const inits = (init.getValue() instanceof rose_1.RoseSplice) ? init.drop(0) : [init];
    const test = decompileEstree(node.test, options);
    const update = decompileEstree(node.update, options);
    const updates = (0, util_1.taggedListP)(update.getValue(), Symbol.for('begin')) ? update.drop(1) : [update];
    const bindings = [];
    const _end = inits.length;
    for (let i = 0; i < _end; i++) {
        let currentInit = inits[i];
        let currentInitExp = currentInit.getValue();
        if ((0, util_1.taggedListP)(currentInitExp, Symbol.for('define')) || (0, util_1.taggedListP)(currentInitExp, Symbol.for('set!'))) {
            currentInit = (0, rose_1.makeRose)([...currentInit.drop(1)], currentInit);
            currentInitExp = currentInit.getValue();
        }
        let currentUpdate = updates[i];
        let currentUpdateExp = currentUpdate.getValue();
        if ((0, util_1.taggedListP)(currentUpdateExp, Symbol.for('begin0'))) {
            currentUpdate = currentUpdate.last();
            currentUpdateExp = currentUpdate.getValue();
        }
        if ((0, util_1.taggedListP)(currentUpdateExp, Symbol.for('set!'))) {
            currentUpdate = currentUpdate.third();
            currentUpdateExp = currentUpdate.getValue();
        }
        bindings.push([...currentInit.drop(0), currentUpdate]);
    }
    let body = decompileEstree(node.body, options);
    if ((bindings.length === 1) && ((0, util_1.taggedListP)(test, Symbol.for('<')) || (0, util_1.taggedListP)(test, Symbol.for('>')))) {
        const binding = bindings[0];
        let i = binding[0];
        const start = (Array.isArray(binding) && (binding.length >= 3) && (binding[binding.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(binding);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = binding;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = binding[binding.length - 1];
                }
                else {
                    result = binding.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : binding[1];
        const end = test.get(2);
        const update = (Array.isArray(binding) && (binding.length >= 3) && (binding[binding.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(binding);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 2;
            let result = binding;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = binding[binding.length - 1];
                }
                else {
                    result = binding.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : binding[2];
        let step = update.get(2).getValue();
        if ((0, util_1.taggedListP)(update, Symbol.for('-'))) {
            step = -step;
        }
        return (0, rose_1.makeRose)([Symbol.for('for'), [[i, [Symbol.for('range'), start, end, ...((step === 1) ? [] : [step])]]], ...body.drop(1)]);
    }
    else {
        return (0, rose_1.makeRose)([Symbol.for('do'), bindings, [[Symbol.for('not'), test]], ...body.drop(1)]);
    }
}
/**
 * Decompile an ESTree [`ForOfStatement`][estree:forofstatement] node.
 *
 * [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
 */
function decompileForOfStatement(node, options = {}) {
    let left = decompileEstree(node.left, options);
    let leftExp = left.getValue();
    if ((0, util_1.taggedListP)(leftExp, Symbol.for('define'))) {
        left = left.get(1);
        leftExp = left.getValue();
    }
    const right = decompileEstree(node.right, options);
    const rightExp = right.getValue();
    let body = decompileEstree(node.body, options);
    const bodyNodes = body.drop(1);
    if ((0, util_1.taggedListP)(leftExp, Symbol.for('define-values'))) {
        const sym = (0, util_1.makeUniqueSymbol)(cons(rightExp, (Array.isArray(leftExp) && (leftExp.length >= 3) && (leftExp[leftExp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(leftExp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = leftExp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = leftExp[leftExp.length - 1];
                }
                else {
                    result = leftExp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : leftExp[1]));
        bodyNodes.unshift((0, rose_1.makeRose)([left.get(0), left.get(1), sym]));
        left = (0, rose_1.makeRose)(sym);
    }
    return (0, rose_1.makeRose)([Symbol.for('for'), [[left, right]], ...bodyNodes]);
}
/**
 * Decompile an ESTree [`ForInStatement`][estree:forinstatement] node.
 *
 * [estree:forinstatement]: https://github.com/estree/estree/blob/master/es5.md#forinstatement
 */
function decompileForInStatement(node, options = {}) {
    let left = decompileEstree(node.left, options);
    let leftExp = left.getValue();
    if ((0, util_1.taggedListP)(leftExp, Symbol.for('define'))) {
        left = left.get(1);
        leftExp = left.getValue();
    }
    const right = decompileEstree(node.right, options);
    let body = decompileEstree(node.body, options);
    return (0, rose_1.makeRose)([Symbol.for('for'), [[left, [Symbol.for('js-keys'), right]]], ...body.drop(1)]);
}
/**
 * Decompile an ESTree [`BreakStatement`][estree:breakstatement] node.
 *
 * [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
 */
function decompileBreakStatement(node, options = {}) {
    return (0, rose_1.makeRose)([Symbol.for('break'), ...(node.label ? [decompileEstree(node.label, options)] : [])]);
}
/**
 * Decompile an ESTree [`ContinueStatement`][estree:continuestatement] node.
 *
 * [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
 */
function decompileContinueStatement(node, options = {}) {
    return (0, rose_1.makeRose)([Symbol.for('continue'), ...(node.label ? [decompileEstree(node.label, options)] : [])]);
}
/**
 * Decompile an ESTree [`ThrowStatement`][estree:throwstatement] node.
 *
 * [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
 */
function decompileThrowStatement(node, options = {}) {
    return (0, rose_1.makeRose)([Symbol.for('throw'), decompileEstree(node.argument, options)]);
}
/**
 * Decompile an ESTree [`TryStatement`][estree:trystatement] node.
 *
 * [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
 */
function decompileTryStatement(node, options = {}) {
    const block = node.block;
    const handler = node.handler;
    const finalizer = node.finalizer;
    let result = (0, rose_1.makeRose)([Symbol.for('try'), ...decompileEstree(block, options).drop(1), ...(handler ? [[Symbol.for('catch'), Symbol.for('Object'), handler.param ? decompileEstree(handler.param, options) : Symbol.for('_'), ...decompileEstree(handler.body, options).drop(1)]] : []), ...(finalizer ? [[Symbol.for('finally'), ...decompileEstree(finalizer, options).drop(1)]] : [])]);
    return result;
}
/**
 * Decompile an ESTree [`YieldExpression`][estree:yieldexpression] node.
 *
 * [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
 */
function decompileYieldExpression(node, options = {}) {
    return (0, rose_1.makeRose)([Symbol.for('yield'), decompileEstree(node.argument, options)]);
}
/**
 * Decompile an ESTree [`NewExpression`][estree:newexpression] node.
 *
 * [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
function decompileNewExpression(node, options = {}) {
    const arguments_ = node.arguments;
    const isSpread = (arguments_.length > 0) && (0, estree_1.estreeTypeP)(arguments_[arguments_.length - 1], 'SpreadElement');
    return (0, rose_1.makeRose)([...(isSpread ? [Symbol.for('apply')] : []), Symbol.for('new'), decompileEstree(node.callee, options), ...arguments_.map(function (x) {
            return decompileEstree(x, options);
        })]);
}
/**
 * Decompile an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
 *
 * [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
 */
function decompileConditionalExpression(node, options = {}) {
    return decompileIfStatement(node, options);
}
/**
 * Decompile an ESTree [`ImportDeclaration`][estree:importdeclaration] node.
 *
 * [estree:importdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importdeclaration
 */
function decompileImportDeclaration(node, options = {}) {
    const source = node.source;
    const sourceDecompiled = decompileEstree(source, options);
    const specifiers = node.specifiers;
    if (specifiers.length === 0) {
        return (0, rose_1.makeRose)([Symbol.for('require'), sourceDecompiled]);
    }
    else if ((specifiers.length === 1) && (0, estree_1.estreeTypeP)(specifiers[0], 'ImportNamespaceSpecifier')) {
        return (0, rose_1.makeRose)([Symbol.for('require'), decompileEstree(specifiers[0].local, options), sourceDecompiled]);
    }
    else {
        const specifiersDecompiled = specifiers.map(function (x) {
            const imported = decompileEstree(x.imported, options);
            const local = decompileEstree(x.local, options);
            if (imported.getValue() === local.getValue()) {
                return imported;
            }
            else {
                return [imported, local];
            }
        });
        return (0, rose_1.makeRose)([Symbol.for('require'), [Symbol.for('only-in'), sourceDecompiled, ...specifiersDecompiled]]);
    }
}
/**
 * Decompile an ESTree [`ExportNamedDeclaration`][estree:exportnameddeclaration] node.
 *
 * [estree:exportnameddeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportnameddeclaration
 */
function decompileExportNamedDeclaration(node, options = {}) {
    const specifiers = node.specifiers;
    const specifiersDecompiled = specifiers.map(function (x) {
        const exported = decompileEstree(x.exported, options);
        const local = decompileEstree(x.local, options);
        if (exported.getValue() === local.getValue()) {
            return exported;
        }
        else {
            return [Symbol.for('rename-out'), [local, exported]];
        }
    });
    return (0, rose_1.makeRose)([Symbol.for('provide'), ...specifiersDecompiled]);
}
/**
 * Decompile an ESTree [`ExportAllDeclaration`][estree:exportalldeclaration] node.
 *
 * [estree:exportalldeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportalldeclaration
 */
function decompileExportAllDeclaration(node, options = {}) {
    const source = node.source;
    const sourceDecompiled = decompileEstree(source, options);
    return (0, rose_1.makeRose)([Symbol.for('provide'), [Symbol.for('all-from-out'), sourceDecompiled]]);
}
/**
 * Decompile an ESTree [`ObjectExpression`][estree:objectexpression] node.
 *
 * [estree:objectexpression]: https://github.com/estree/estree/blob/master/es5.md#objectexpression
 */
function decompileObjectExpression(node, options = {}) {
    const spreads = [];
    const properties = [];
    for (let prop of node.properties) {
        if ((0, estree_1.estreeTypeP)(prop, 'SpreadElement')) {
            spreads.push(decompileEstree(prop.argument, options));
        }
        else {
            properties.push(decompileEstree(prop.key, options).getValue().description);
            properties.push(decompileEstree(prop.value, options));
        }
    }
    if (spreads.length === 0) {
        return (0, rose_1.makeRose)([Symbol.for('js-obj'), ...properties]);
    }
    else {
        return (0, rose_1.makeRose)([Symbol.for('js-obj-append'), ...spreads, ...((properties.length > 0) ? [[Symbol.for('js-obj'), ...properties]] : [])]);
    }
}
/**
 * Decompile an ESTree [`ObjectPattern`][estree:objectpattern] node.
 *
 * [estree:objectpattern]: https://github.com/estree/estree/blob/master/es2015.md#objectpattern
 */
function decompileObjectPattern(node, options = {}) {
    const properties = [];
    for (let prop of node.properties) {
        const key = decompileEstree(prop.key, options);
        const value = decompileEstree(prop.value, options);
        if (key.getValue() === value.getValue()) {
            properties.push(key);
        }
        else {
            properties.push([key, value]);
        }
    }
    return (0, rose_1.makeRose)(properties);
}
/**
 * Decompile an ESTree [`TemplateLiteral`][estree:templateliteral] node.
 *
 * [estree:templateliteral]: https://github.com/estree/estree/blob/master/es2015.md#templateliteral
 */
function decompileTemplateLiteral(node, options = {}) {
    let str = '';
    const quasis = node.quasis;
    if (quasis.length > 0) {
        str = quasis[0].value.cooked;
    }
    return (0, rose_1.makeRose)(str);
}
/**
 * Decompile an ESTree [`TaggedTemplateExpression`][estree:taggedtemplateexpression] node.
 *
 * [estree:taggedtemplateexpression]: https://github.com/estree/estree/blob/master/es2015.md#taggedtemplateexpression
 */
function decompileTaggedTemplateExpression(node, options = {}) {
    const tag = node.tag;
    const tagDecompiled = decompileEstree(tag, options);
    const quasi = node.quasi;
    const quasiDecompiled = decompileEstree(quasi, options);
    return (0, rose_1.makeRose)([Symbol.for('js/tag'), tagDecompiled, quasiDecompiled]);
}
/**
 * Decompile an ESTree [`ArrayExpression`][estree:arrayexpression] node.
 *
 * [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
 */
function decompileArrayExpression(node, options = {}) {
    const elements = node.elements;
    function decompileElement(x) {
        if (x) {
            return decompileEstree(x, options);
        }
        else {
            return Symbol.for('_');
        }
    }
    if ((elements.length > 0) && elements[elements.length - 1] && (0, estree_1.estreeTypeP)(elements[elements.length - 1], 'RestElement')) {
        const regularElements = elements.slice(0, -1).map(function (x) {
            return decompileElement(x);
        });
        const restElement = decompileElement(elements[elements.length - 1]);
        return (0, rose_1.makeRose)(listStar(...[...regularElements, restElement]));
    }
    else if (findf(function (x) {
        return x && (0, estree_1.estreeTypeP)(x, 'SpreadElement');
    }, elements)) {
        const elementsDecompiled = elements.map(function (x) {
            let result = decompileElement(x);
            if (x && (0, estree_1.estreeTypeP)(x, 'SpreadElement')) {
                return result;
            }
            else {
                return (0, rose_1.makeRose)([Symbol.for('list'), result]);
            }
        });
        return (0, rose_1.makeRose)([Symbol.for('append'), ...elementsDecompiled]);
    }
    else {
        const elementsDecompiled = elements.map(function (x) {
            return decompileElement(x);
        });
        return (0, rose_1.makeRose)([Symbol.for('list'), ...elementsDecompiled]);
    }
}
/**
 * Decompile an ESTree [`ArrayPattern`][estree:arraypattern] node.
 *
 * [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
 */
function decompileArrayPattern(node, options = {}) {
    const arrayExpression = decompileArrayExpression(node, options);
    return (0, rose_1.makeRose)((0, util_1.taggedListP)(arrayExpression.getValue(), Symbol.for('list')) ? arrayExpression.drop(1) : arrayExpression);
}
/**
 * Decompile an ESTree [`SpreadElement`][estree:spreadelement] node.
 *
 * [estree:spreadelement]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
function decompileSpreadElement(node, options = {}) {
    const argument = node.argument;
    return decompileEstree(argument, options);
}
/**
 * Decompile an ESTree [`Super`][estree:super] node.
 *
 * [estree:super]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
function decompileSuper(node, options = {}) {
    return (0, rose_1.makeRose)(Symbol.for('super'));
}
/**
 * Decompile an ESTree [`ThisExpression`][estree:thisexpression] node.
 *
 * [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
 */
function decompileThisExpression(node, options = {}) {
    return (0, rose_1.makeRose)(Symbol.for('this'));
}
/**
 * Decompile an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
 *
 * [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
 */
function decompileClassDeclaration(node, options = {}) {
    const id = node.id;
    const idDecompiled = decompileEstree(id, options);
    const superClass = node.superClass;
    const superClassDecompiled = superClass ? decompileEstree(superClass, options) : Symbol.for('object%');
    const superClassDecompiledExp = [Symbol.for('object%'), Symbol.for('object'), Symbol.for('Object')].includes(superClassDecompiled) ? [] : [superClassDecompiled];
    const bodyDecompiled = [];
    let body = node.body;
    for (let x of body.body) {
        bodyDecompiled.push(decompileEstree(x, options));
    }
    return (0, rose_1.makeRose)([Symbol.for('define-class'), idDecompiled, superClassDecompiledExp, ...bodyDecompiled]);
}
/**
 * Decompile an ESTree [`PropertyDefinition`][estree:propertydefinition] node.
 *
 * [estree:propertydefinition]: https://github.com/estree/estree/blob/master/es2022.md#propertydefinition
 */
function decompilePropertyDefinition(node, options = {}) {
    const key = node.key;
    const keyDecompiled = decompileEstree(key, options);
    const value = node.value;
    const valueDecompiled = decompileEstree(value, options);
    const defineSymbol = (node.accessibility === 'private') ? Symbol.for('define') : Symbol.for('define/public');
    return (0, rose_1.makeRose)([defineSymbol, keyDecompiled, ...((value === null) ? [] : [valueDecompiled])]);
}
/**
 * Decompile an ESTree [`MethodDefinition`][estree:methoddefinition] node.
 *
 * [estree:methoddefinition]: https://github.com/estree/estree/blob/master/es2015.md#methoddefinition
 */
function decompileMethodDefinition(node, options = {}) {
    const key = node.key;
    const keyDecompiled = decompileEstree(key, options);
    const keyDecompiledExp = keyDecompiled.getValue();
    const value = node.value;
    const valueDecompiled = decompileEstree(value, options);
    const defineSymbol = value.generator ? Symbol.for('define/generator') : (((node.accessibility === 'private') || (keyDecompiledExp === Symbol.for('constructor'))) ? Symbol.for('define') : Symbol.for('define/public'));
    return (0, rose_1.makeRose)([defineSymbol, cons(keyDecompiledExp, valueDecompiled.getValue()[1]), ...valueDecompiled.drop(2)]);
}
/**
 * Decompile a TSESTree `TSAsExpression` node.
 */
function decompileTsAsExpression(node, options = {}) {
    const expression = node.expression;
    const expressionDecompiled = decompileEstree(expression, options);
    const typeAnnotation = node.typeAnnotation;
    const typeAnnotationDecompiled = decompileEstree(typeAnnotation, options);
    return (0, rose_1.makeRose)([Symbol.for('ann'), expressionDecompiled, typeAnnotationDecompiled]);
}
/**
 * Decompile a TSESTree `TSAnyKeyword` node.
 */
function decompileTsAnyKeyword(node, options = {}) {
    return (0, rose_1.makeRose)(Symbol.for('Any'));
}
/**
 * Decompile a TSESTree `TSBooleanKeyword` node.
 */
function decompileTsBooleanKeyword(node, options = {}) {
    return (0, rose_1.makeRose)(Symbol.for('Boolean'));
}
/**
 * Decompile a TSESTree `TSNumberKeyword` node.
 */
function decompileTsNumberKeyword(node, options = {}) {
    return (0, rose_1.makeRose)(Symbol.for('Number'));
}
/**
 * Decompile a TSESTree `TSStringKeyword` node.
 */
function decompileTsStringKeyword(node, options = {}) {
    return (0, rose_1.makeRose)(Symbol.for('String'));
}
/**
 * Decompile a TSESTree `TSUndefinedKeyword` node.
 */
function decompileTsUndefinedKeyword(node, options = {}) {
    return (0, rose_1.makeRose)(Symbol.for('Undefined'));
}
/**
 * Decompile a TSESTree `TSVoidKeyword` node.
 */
function decompileTsVoidKeyword(node, options = {}) {
    return (0, rose_1.makeRose)(Symbol.for('Void'));
}
/**
 * Decompile a TSESTree `TSLiteralType` node.
 */
function decompileTsLiteralType(node, options = {}) {
    const literal = node.literal;
    const literalDecompiled = literal ? Symbol.for('True') : Symbol.for('False');
    return (0, rose_1.makeRose)(literalDecompiled);
}
/**
 * Decompile a TSESTree `TSArrayType` node.
 */
function decompileTsArrayType(node, options = {}) {
    const elementType = node.elementType;
    const elementTypeDecompiled = decompileEstree(elementType, options);
    return (0, rose_1.makeRose)([Symbol.for('Listof'), elementTypeDecompiled]);
}
/**
 * Decompile a TSESTree `TSTupleType` node.
 */
function decompileTsTupleType(node, options = {}) {
    const elementTypes = node.elementTypes;
    const elementTypesDecompiled = elementTypes.map(function (x) {
        return decompileEstree(x, options);
    });
    return (0, rose_1.makeRose)([Symbol.for('List'), ...elementTypesDecompiled]);
}
/**
 * Decompile a TSESTree `TSNamedTupleMember` node.
 */
function decompileTsNamedTupleMember(node, options = {}) {
    // FIXME: Better decompilation of `TSNamedTupleMember`.
    // The following strips away the identifier.
    const elementType = node.elementType;
    return decompileEstree(elementType, options);
}
/**
 * Decompile a TSESTree `TSUnionType` node.
 */
function decompileTsUnionType(node, options = {}) {
    const types = node.types;
    const typesDecompiled = types.map(function (x) {
        return decompileEstree(x, options);
    });
    return (0, rose_1.makeRose)([Symbol.for('U'), ...typesDecompiled]);
}
/**
 * Decompile a TSESTree `TSFunctionType` node.
 */
function decompileTsFunctionType(node, options = {}) {
    let params = node.params;
    const paramsDecompiled = params.map(function (x) {
        return decompileEstree(x.typeAnnotation, options);
    });
    const returnType = node.returnType;
    const returnTypeDecompiled = decompileEstree(returnType, options);
    return (0, rose_1.makeRose)([Symbol.for('->'), ...paramsDecompiled, returnTypeDecompiled]);
}
/**
 * Decompile a TSESTree `TSTypeReference` node.
 */
function decompileTsTypeReference(node, options = {}) {
    const name = node.typeName;
    let params = node.typeParameters;
    const nameDecompiled = decompileEstree(name, options);
    const paramsDecompiled = params ? decompileEstree(params, options) : [];
    if (paramsDecompiled.length === 0) {
        return (0, rose_1.makeRose)(nameDecompiled);
    }
    else {
        return (0, rose_1.makeRose)([nameDecompiled, ...paramsDecompiled]);
    }
}
/**
 * Decompile a TSESTree `TSTypeParameterInstantiation` node.
 */
function decompileTsTypeParameterInstantiation(node, options = {}) {
    let params = node.params;
    return params.map(function (x) {
        return decompileEstree(x, options);
    });
}
/**
 * Decompile a TSESTree `TSTypeAliasDeclaration` node.
 */
function decompileTsTypeAliasDeclaration(node, options = {}) {
    const id = node.id;
    const idDecompiled = decompileEstree(id, options);
    const typeAnnotation = node.typeAnnotation;
    const typeAnnotationDecompiled = decompileEstree(typeAnnotation, options);
    return (0, rose_1.makeRose)([Symbol.for('define-type'), idDecompiled, typeAnnotationDecompiled]);
}
/**
 * Decompile a TSESTree `TSTypeAnnotation` node.
 */
function decompileTsTypeAnnotation(node, options = {}) {
    return decompileEstree(node.typeAnnotation, options);
}
/**
 * Decompile a function expression or declaration.
 */
function decompileFunction(node, options = {}) {
    const type_ = (0, estree_1.estreeType)(node);
    const id = (type_ === 'FunctionDeclaration') ? decompileEstree(node.id, options) : false;
    const lambdaSym = (type_ === 'ArrowFunctionExpression') ? Symbol.for('js/arrow') : Symbol.for('lambda');
    let params = node.params.map(function (x) {
        return decompileParameter(x, options);
    });
    if ((params.length > 0) && (0, estree_1.estreeTypeP)((() => {
        const arr = node.params;
        return arr[arr.length - 1];
    })(), 'RestElement')) {
        if (params.length === 1) {
            params = params[params.length - 1];
        }
        else {
            params = listStar(...params);
        }
    }
    let body = removeReturnTailCall(decompileEstree(node.body, options));
    const bodyExp = body.getValue();
    const bodyForms = (0, util_1.taggedListP)(bodyExp, Symbol.for('begin')) ? body.drop(1) : [body];
    const asyncField = node.async;
    if (id) {
        if (asyncField) {
            return (0, rose_1.makeRose)([Symbol.for('define'), id, [Symbol.for('async'), [lambdaSym, params, ...bodyForms]]]);
        }
        else {
            return (0, rose_1.makeRose)([Symbol.for('define'), cons(id, params), ...bodyForms]);
        }
    }
    else {
        if (asyncField) {
            return (0, rose_1.makeRose)([Symbol.for('async'), [lambdaSym, params, ...bodyForms]]);
        }
        else {
            return (0, rose_1.makeRose)([lambdaSym, params, ...bodyForms]);
        }
    }
}
/**
 * Decompile a function parameter.
 * Helper function for `decompile-function`.
 */
function decompileParameter(node, options = {}) {
    if ((0, estree_1.estreeTypeP)(node, 'Identifier')) {
        const typeAnnotation = node.typeAnnotation;
        const optional = node.optional;
        const name = Symbol.for(node.name);
        if (optional) {
            return (0, rose_1.makeRose)([name, Symbol.for('undefined')]);
        }
        else if (typeAnnotation) {
            return (0, rose_1.makeRose)([name, Symbol.for(':'), decompileTsTypeAnnotation(typeAnnotation, options)]);
        }
        else {
            return (0, rose_1.makeRose)(name);
        }
    }
    else {
        return decompileEstree(node, options);
    }
}
/**
 * Remove superfluous `(return ...)` forms from
 * a form that occurs in tail call position.
 */
function removeReturnTailCall(node) {
    const exp = node.getValue();
    if ((0, util_1.taggedListP)(exp, Symbol.for('return')) && (exp.length === 2)) {
        return node.get(1);
    }
    else if ((0, util_1.taggedListP)(exp, Symbol.for('begin'))) {
        return (0, rose_1.makeRose)([...node.dropRight(1), removeReturnTailCall(node.get(exp.length - 1))], node);
    }
    else if ((0, util_1.taggedListP)(exp, Symbol.for('if'))) {
        return (0, rose_1.makeRose)([node.get(0), node.get(1), ...node.drop(2).map(function (x) {
                return removeReturnTailCall(x);
            })], node);
    }
    else if ((0, util_1.taggedListP)(exp, Symbol.for('cond'))) {
        return (0, rose_1.makeRose)([node.get(0), ...node.drop(1).map(function (x) {
                return (0, rose_1.makeRose)([...x.dropRight(1), removeReturnTailCall(x.get(x.getValue().length - 1))], node);
            })], node);
    }
    else {
        return node;
    }
}
/**
 * Default decompiler function.
 */
function defaultDecompiler(node, options = {}) {
    return (0, rose_1.makeRose)((node && (0, estree_1.estreeType)(node)) + ' not supported yet');
}
/**
 * Mapping from ESTree node types to decompiler functions.
 */
const decompilerMap = new Map([['ArrayExpression', decompileArrayExpression], ['ArrayPattern', decompileArrayPattern], ['ArrowFunctionExpression', decompileArrowFunctionExpression], ['AssignmentExpression', decompileAssignmentExpression], ['AssignmentPattern', decompileAssignmentPattern], ['BinaryExpression', decompileBinaryExpression], ['BlockStatement', decompileBlockStatement], ['BreakStatement', decompileBreakStatement], ['CallExpression', decompileCallExpression], ['ChainExpression', decompileChainExpression], ['ClassDeclaration', decompileClassDeclaration], ['ConditionalExpression', decompileConditionalExpression], ['ContinueStatement', decompileContinueStatement], ['DoWhileStatement', decompileDoWhileStatement], ['ExportAllDeclaration', decompileExportAllDeclaration], ['ExportNamedDeclaration', decompileExportNamedDeclaration], ['ExpressionStatement', decompileExpressionStatement], ['ForInStatement', decompileForInStatement], ['ForOfStatement', decompileForOfStatement], ['ForStatement', decompileForStatement], ['FunctionDeclaration', decompileFunctionDeclaration], ['FunctionExpression', decompileFunctionExpression], ['Identifier', decompileIdentifier], ['IfStatement', decompileIfStatement], ['ImportDeclaration', decompileImportDeclaration], ['Literal', decompileLiteral], ['LogicalExpression', decompileLogicalExpression], ['MemberExpression', decompileMemberExpression], ['MethodDefinition', decompileMethodDefinition], ['NewExpression', decompileNewExpression], ['ObjectExpression', decompileObjectExpression], ['ObjectPattern', decompileObjectPattern], ['Program', decompileProgram], ['PropertyDefinition', decompilePropertyDefinition], ['RestElement', decompileRestElement], ['ReturnStatement', decompileReturnStatement], ['SequenceExpression', decompileSequenceExpression], ['SpreadElement', decompileSpreadElement], ['Super', decompileSuper], ['TSAnyKeyword', decompileTsAnyKeyword], ['TSArrayType', decompileTsArrayType], ['TSAsExpression', decompileTsAsExpression], ['TSBooleanKeyword', decompileTsBooleanKeyword], ['TSFunctionType', decompileTsFunctionType], ['TSLiteralType', decompileTsLiteralType], ['TSNumberKeyword', decompileTsNumberKeyword], ['TSStringKeyword', decompileTsStringKeyword], ['TSTupleType', decompileTsTupleType], ['TSNamedTupleMember', decompileTsNamedTupleMember], ['TSTypeAliasDeclaration', decompileTsTypeAliasDeclaration], ['TSTypeAnnotation', decompileTsTypeAnnotation], ['TSTypeParameterInstantiation', decompileTsTypeParameterInstantiation], ['TSTypeReference', decompileTsTypeReference], ['TSUndefinedKeyword', decompileTsUndefinedKeyword], ['TSUnionType', decompileTsUnionType], ['TSVoidKeyword', decompileTsVoidKeyword], ['TaggedTemplateExpression', decompileTaggedTemplateExpression], ['TemplateLiteral', decompileTemplateLiteral], ['ThisExpression', decompileThisExpression], ['ThrowStatement', decompileThrowStatement], ['TryStatement', decompileTryStatement], ['UnaryExpression', decompileUnaryExpression], ['UpdateExpression', decompileUpdateExpression], ['VariableDeclaration', decompileVariableDeclaration], ['VariableDeclarator', decompileVariableDeclarator], ['WhileStatement', decompileWhileStatement], ['YieldExpression', decompileYieldExpression]]);
