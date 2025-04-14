"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.or = exports.nop = exports.scmNew = exports.rktMakeObject = exports.newStar = exports.makeObject_ = exports.makeObject = exports.make = exports.jsNew = exports.lisp1Environment = exports.letrecValues = exports.letValues = exports.letstarValues = exports.letrec = exports.let_ = exports.letStar = exports.letJsObj = exports.lambda = exports.fn = exports.compileFunction = exports.js = exports.jsAwait = exports.await_ = exports.await = exports.jsAsync = exports.async_ = exports.async = exports.getField = exports.dot = exports.define = exports.defineValues = exports.defineType = exports.definePublic = exports.defineMacro = exports.defineJsObj = exports.defineGenerator = exports.defineClass = exports.defineAsync = exports.cond = exports.compileLispToJavascript = exports.compileLisp = exports.colon = exports.try_ = exports.try = exports.callCc = exports.callWithCurrentContinuation = exports.block = exports.begin = exports.ann = exports.and = void 0;
exports.getField_ = exports.for_ = exports.findEstree = exports.dot_ = exports.definitionToMacro = exports.define_ = exports.defineValues_ = exports.defineType_ = exports.defineMacro_ = exports.defineMacroToLambdaForm = exports.defineMacroToFunction = exports.defineJsObj_ = exports.defineGenerator_ = exports.defineAsync_ = exports.defineToDefineClass = exports.continue_ = exports.cond_ = exports.compileModules = exports.compileModuleMap = exports.compileFilesX = exports.compileFileX = exports.compile = exports.compilationEnvironment = exports.colon_ = exports.cljTry_ = exports.class_ = exports.break_ = exports.begin_ = exports.applyOptimizations = exports.ann_ = exports.and_ = exports.Module = exports.readFromString = exports.setValues = exports.setXValues = exports.setJsObj = exports.setJsObjX = exports.setXJsObj = exports.setField = exports.setFieldX = exports.setq_ = exports.setq = exports.setX = exports.send = exports.callMethod = exports.sendApply = exports.require = exports.quote = exports.quasiquote = exports.provide = void 0;
exports.send_ = exports.sendMethod = exports.sendApply_ = exports.s = exports.return_ = exports.require_ = exports.readSexp = exports.readRose = exports.read = exports.quote_ = exports.quotep = exports.quasiquote_ = exports.provide_ = exports.or_ = exports.optimizeSexp = exports.optimizeRose = exports.optimizeModule = exports.optimizeEstree = exports.optimizations = exports.nop_ = exports.new_ = exports.module_ = exports.moduleExpressionToModuleObject = exports.mapVisitRose = exports.mapSexp = exports.mapRose = exports.makeModuleMap = exports.makeLisp = exports.macroexpandUntil = exports.macroexpandAllUntil = exports.macroexpandAll = exports.macroexpand1 = exports.macroexpand = exports.lispEnvironment = exports.lisp = exports.letVarsToConstVars = exports.letValues_ = exports.letStar_ = exports.letJsObj_ = exports.langEnvironment = exports.lambda_ = exports.js_ = exports.jsAwait_ = exports.jsAsync_ = exports.iterateRose = exports.isAP_ = exports.interpretationEnvironment = exports.interpretString = exports.interpretFiles = exports.interpret = void 0;
exports.yield_ = exports.typeOf_ = exports.traverseEstree = exports.tokenize = exports.throw_ = exports.splitComments = exports.sourcep = exports.source = exports.sexp = exports.setValues_ = exports.setJsObj_ = exports.setField_ = exports.setX_ = void 0;
const fs_1 = require("fs");
const path_1 = require("path");
const array_1 = require("./array");
const constants_1 = require("./constants");
const curry_1 = require("./curry");
const env_1 = require("./env");
const equal_1 = require("./equal");
const estree_1 = require("./estree");
const eval_1 = require("./eval");
const hash_1 = require("./hash");
const javascript_1 = require("./javascript");
const list_1 = require("./list");
const macros_1 = require("./macros");
Object.defineProperty(exports, "try", { enumerable: true, get: function () { return macros_1.cljTry_; } });
Object.defineProperty(exports, "try_", { enumerable: true, get: function () { return macros_1.cljTry_; } });
Object.defineProperty(exports, "cljTry_", { enumerable: true, get: function () { return macros_1.cljTry_; } });
Object.defineProperty(exports, "definePublic", { enumerable: true, get: function () { return macros_1.definePublic_; } });
const object_1 = require("./object");
const parser_1 = require("./parser");
Object.defineProperty(exports, "read", { enumerable: true, get: function () { return parser_1.read; } });
Object.defineProperty(exports, "readRose", { enumerable: true, get: function () { return parser_1.readRose; } });
Object.defineProperty(exports, "readSexp", { enumerable: true, get: function () { return parser_1.readSexp; } });
Object.defineProperty(exports, "tokenize", { enumerable: true, get: function () { return parser_1.tokenize; } });
const plist_1 = require("./plist");
const printer_1 = require("./printer");
const procedures_1 = require("./procedures");
Object.defineProperty(exports, "isAP_", { enumerable: true, get: function () { return procedures_1.isAP_; } });
Object.defineProperty(exports, "typeOf_", { enumerable: true, get: function () { return procedures_1.typeOf_; } });
const regexp_1 = require("./regexp");
const rose_1 = require("./rose");
const sexp_1 = require("./sexp");
Object.defineProperty(exports, "s", { enumerable: true, get: function () { return sexp_1.s; } });
Object.defineProperty(exports, "readFromString", { enumerable: true, get: function () { return sexp_1.sexp; } });
Object.defineProperty(exports, "sexp", { enumerable: true, get: function () { return sexp_1.sexp; } });
const string_1 = require("./string");
const symbol_1 = require("./symbol");
const thunk_1 = require("./thunk");
const util_1 = require("./util");
Object.defineProperty(exports, "quotep", { enumerable: true, get: function () { return util_1.quotep; } });
const visitor_1 = require("./visitor");
const [lastCdr, cdr, flatten, buildList, listStar, makeList, cons, findf, length] = (() => {
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
    function cdr_(lst) {
        if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
            return lst[2];
        }
        else {
            return lst.slice(1);
        }
    }
    function flatten_(lst) {
        return lst.reduce(function (acc, x) {
            if (Array.isArray(x)) {
                return [...acc, ...flatten_(x)];
            }
            else if (x === Symbol.for('.')) {
                return acc;
            }
            else {
                acc.push(x);
                return acc;
            }
        }, []);
    }
    function buildList_(n, proc) {
        return (() => {
            function range_(start, end = undefined, step = undefined) {
                const startN = (end === undefined) ? 0 : start;
                const endN = (end === undefined) ? start : end;
                const stepN = step || 1;
                let result = [];
                for (let i = startN; (stepN < 0) ? (i > endN) : (i < endN); i = i + stepN) {
                    result.push(i);
                }
                return result;
            }
            return range_;
        })()(0, n).map(function (x) {
            return proc(x);
        });
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
    function makeList_(k, v) {
        let result = [];
        for (let i = 0; i < k; i++) {
            result.push(v);
        }
        return result;
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
    function length_(lst) {
        if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
            return (() => {
                function linkedListLength_(lst) {
                    let len = 0;
                    let current = lst;
                    while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
                        len = len + (lst.length - 2);
                        current = current[current.length - 1];
                    }
                    return len;
                }
                return linkedListLength_;
            })()(lst);
        }
        else {
            return lst.length;
        }
    }
    function range_(start, end = undefined, step = undefined) {
        const startN = (end === undefined) ? 0 : start;
        const endN = (end === undefined) ? start : end;
        const stepN = step || 1;
        let result = [];
        for (let i = startN; (stepN < 0) ? (i > endN) : (i < endN); i = i + stepN) {
            result.push(i);
        }
        return result;
    }
    function linkedListLength_(lst) {
        let len = 0;
        let current = lst;
        while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
            len = len + (lst.length - 2);
            current = current[current.length - 1];
        }
        return len;
    }
    return [lastCdr_, cdr_, flatten_, buildList_, listStar_, makeList_, cons_, findf_, length_];
})();
/**
 * Default options for interpretation and compilation.
 * See also `default-compilation-options`.
 */
const defaultOptions = {
    comments: true,
    expressionType: 'expression',
    eval: false,
    shouldInline: true,
    inlineFunctions: false,
    compileEnvironment: true,
    gensymMap: new Map()
};
/**
 * Add `default-options` to an options object.
 * If `modify` is `#t`, the original object
 * is modified, otherwise a new object is returned.
 */
function addDefaultOptions(options, modify = false) {
    let result = modify ? options : Object.assign({}, options);
    for (let key of Object.keys(defaultOptions)) {
        if (result[key] === undefined) {
            result[key] = defaultOptions[key];
        }
    }
    return result;
}
addDefaultOptions.lispSource = [Symbol.for('define'), [Symbol.for('add-default-options'), Symbol.for('options'), [Symbol.for('modify'), Symbol.for('#f')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('if'), Symbol.for('modify'), Symbol.for('options'), [Symbol.for('js-obj-append'), Symbol.for('options')]]], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('js-keys'), Symbol.for('default-options')]]], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('oget'), Symbol.for('result'), Symbol.for('key')], Symbol.for('undefined')], [Symbol.for('oset!'), Symbol.for('result'), Symbol.for('key'), [Symbol.for('oget'), Symbol.for('default-options'), Symbol.for('key')]]]], Symbol.for('result')];
/**
 * Compilation environment class.
 *
 * A compilation environment is a typed environment mapping
 * Lisp functions to compiled values, compiler procedures
 * or compiler macros.
 */
class CompilationEnvironment extends env_1.TypedEnvironment {
}
/**
 * Compilation variable environment.
 *
 * An environment mapping various Lisp values to their
 * JavaScript equivalents.
 */
const compilationVariablesEnv = new CompilationEnvironment([[Symbol.for('#f'), new estree_1.Literal(false), 'variable'], [Symbol.for('#t'), new estree_1.Literal(true), 'variable'], [Symbol.for('js-null'), new estree_1.Literal(null), 'variable'], [Symbol.for('js-undefined'), new estree_1.Literal(undefined), 'variable'], [Symbol.for('js/arguments'), new estree_1.Identifier('arguments'), 'variable'], [Symbol.for('js/null'), new estree_1.Literal(null), 'variable'], [Symbol.for('js/require'), new estree_1.Identifier('require'), 'variable'], [Symbol.for('js/undefined'), new estree_1.Literal(undefined), 'variable'], [Symbol.for('*cons-dot*'), list_1.consDotCompiled_, 'variable'], [Symbol.for('nil'), new estree_1.Literal(null), 'variable'], [Symbol.for('null'), new estree_1.ArrayExpression(), 'variable'], [Symbol.for('t'), new estree_1.Literal(true), 'variable'], [Symbol.for('undefined'), new estree_1.Literal(undefined), 'variable']]);
/**
 * Compiler procedures mapping environment.
 */
const compilationCompilerMappingEnv = new CompilationEnvironment([[procedures_1.add_, compileAdd, 'compiler'], [and_, compileAnd, 'compiler'], [ann_, compileAnn, 'compiler'], [list_1.append_, compileAppend, 'compiler'], [procedures_1.apply_, compileApply, 'compiler'], [array_1.arrayRef_, compileArrayRef, 'compiler'], [array_1.arraySet_, compileArraySet, 'compiler'], [begin_, compileBegin, 'compiler'], [block_, compileBlock, 'compiler'], [break_, compileBreak, 'compiler'], [class_, compileClass, 'compiler'], [colon_, compileColon, 'compiler'], [cond_, compileCond, 'compiler'], [continue_, compileContinue, 'compiler'], [defineAsync_, compileDefineAsync, 'compiler'], [defineClass_, compileDefineClass, 'compiler'], [defineGenerator_, compileDefineGenerator, 'compiler'], [defineJsObj_, compileDefineJsObj, 'compiler'], [defineMacro_, compileDefineMacro, 'compiler'], [defineType_, compileDefineType, 'compiler'], [defineValues_, compileDefineValues, 'compiler'], [define_, compileDefine, 'compiler'], [procedures_1.div_, compileDiv, 'compiler'], [dot_, compileSend, 'compiler'], [for_, compileFor, 'compiler'], [procedures_1.funcall_, compileFuncall, 'compiler'], [getField_, compileGetField, 'compiler'], [procedures_1.gt_, compileGreaterThan, 'compiler'], [procedures_1.gte_, compileGreaterThanOrEqual, 'compiler'], [jsArrow_, compileJsArrow, 'compiler'], [jsAsync_, compileJsAsync, 'compiler'], [jsAwait_, compileJsAwait, 'compiler'], [javascript_1.jsDelete_, compileJsDelete, 'compiler'], [jsDoWhile_, compileJsDoWhile, 'compiler'], [javascript_1.jsEval_, compileJsEval, 'compiler'], [jsFunction_, compileJsFunction, 'compiler'], [javascript_1.jsIn_, compileJsIn, 'compiler'], [javascript_1.jsInstanceof_, compileJsInstanceof, 'compiler'], [javascript_1.jsIsLooselyEqualP_, compileJsIsLooselyEqual, 'compiler'], [javascript_1.jsIsStrictlyEqualP_, compileJsIsStrictlyEqual, 'compiler'], [object_1.jsObjAppend_, compileJsObjAppend, 'compiler'], [object_1.jsObj_, compileJsObj, 'compiler'], [jsOptionalChaining_, compileJsOptionalChaining, 'compiler'], [javascript_1.jsPlus_, compileAdd, 'compiler'], [jsSwitch_, compileJsSwitch, 'compiler'], [javascript_1.jsTaggedTemplate_, compileJsTaggedTemplate, 'compiler'], [jsTry_, compileJsTry, 'compiler'], [javascript_1.jsTypeof_, compileJsTypeof, 'compiler'], [jsWhile_, compileJsWhile, 'compiler'], [js_, compileJs, 'compiler'], [lambda_, compileLambda, 'compiler'], [letJsObj_, compileLetJsObj, 'compiler'], [letStar_, compileLet, 'compiler'], [letValues_, compileLetValues, 'compiler'], [list_1.list_, compileList, 'compiler'], [procedures_1.lt_, compileLessThan, 'compiler'], [procedures_1.lte_, compileLessThanOrEqual, 'compiler'], [module_, compileModule, 'compiler'], [procedures_1.modulo_, compileModulo, 'compiler'], [procedures_1.mul_, compileMul, 'compiler'], [new_, compileNew, 'compiler'], [procedures_1.not_, compileNot, 'compiler'], [object_1.objectRef_, compileObjectRef, 'compiler'], [object_1.objectSetX_, compileObjectSet, 'compiler'], [or_, compileOr, 'compiler'], [provide_, compileProvide, 'compiler'], [list_1.pushLeftX_, compilePushLeft, 'compiler'], [list_1.pushRightX_, compilePushRight, 'compiler'], [quasiquote_, compileQuasiquote, 'compiler'], [quote_, compileQuote, 'compiler'], [require_, compileRequire, 'compiler'], [return_, compileReturn, 'compiler'], [sendApply_, compileSendApply, 'compiler'], [send_, compileSend, 'compiler'], [setX_, compileSet, 'compiler'], [setField_, compileSetField, 'compiler'], [setJsObj_, compileSetJsObj, 'compiler'], [setValues_, compileSetValues, 'compiler'], [string_1.stringAppend_, compileStringAppend, 'compiler'], [procedures_1.sub_, compileSub, 'compiler'], [throw_, compileThrow, 'compiler'], [yield_, compileYield, 'compiler']]);
/**
 * Compiler macros mapping environment.
 */
const compilationMacroMappingEnv = new CompilationEnvironment([[array_1.arrayDropRight_, compileArrayDropRightMacro, 'macro'], [array_1.arrayDrop_, compileArrayDropMacro, 'macro'], [list_1.arrayListDropRight_, compileArrayListDropRightMacro, 'macro'], [list_1.arrayListDrop_, compileArrayListDropMacro, 'macro'], [procedures_1.assert_, compileAssertMacro, 'macro'], [procedures_1.display_, compileDisplayMacro, 'macro'], [list_1.dropRight_, compileDropRightMacro, 'macro'], [list_1.drop_, compileDropMacro, 'macro'], [procedures_1.foldl_, compileFoldlMacro, 'macro'], [procedures_1.foldr_, compileFoldrMacro, 'macro'], [hash_1.hashClear_, compileHashClearMacro, 'macro'], [hash_1.hashRef_, compileHashRefMacro, 'macro'], [hash_1.hashRemoveX_, compileHashRemoveMacro, 'macro'], [hash_1.hashRemove_, compileHashRemoveMacro, 'macro'], [hash_1.makeHash_, compileMakeHashMacro, 'macro'], [procedures_1.map_, compileMapMacro, 'macro'], [procedures_1.memberp_, compileMemberPMacro, 'macro'], [printer_1.print, compileDisplayMacro, 'macro'], [regexp_1.regexp_, compileRegexpMacro, 'macro'], [string_1.stringTrim_, compileStringTrimMacro, 'macro'], [string_1.stringp_, compileStringpMacro, 'macro'], [string_1.substring_, compileSubstringMacro, 'macro'], [procedures_1.values_, compileValuesMacro, 'macro']]);
/**
 * Compilation mapping environment.
 *
 * An environment mapping Lisp functions to compiler procedures
 * or compiler macros.
 */
const compilationMappingEnv = new env_1.EnvironmentStack(compilationMacroMappingEnv, compilationCompilerMappingEnv);
/**
 * Inlined functions.
 *
 * A list of functions whose definition is so simple
 * that it might be inlined directly into the call site.
 */
const inlinedFunctions = [procedures_1.add1_, array_1.arrayEighth_, array_1.arrayFifth_, array_1.arrayFirst_, array_1.arrayFourth_, array_1.arrayLast_, array_1.arrayLength_, list_1.arrayListCdr_, list_1.arrayListEighth_, list_1.arrayListFifth_, list_1.arrayListFirst_, list_1.arrayListFourth_, list_1.arrayListLast_, list_1.arrayListLength_, list_1.arrayListNinth_, list_1.arrayListNth_, list_1.arrayListNthcdr_, list_1.arrayListRest_, list_1.arrayListReverse_, list_1.arrayListSecond_, list_1.arrayListSeventh_, list_1.arrayListSixth_, list_1.arrayListTake_, list_1.arrayListTenth_, list_1.arrayListThird_, list_1.arrayListP_, array_1.arrayNinth_, array_1.arrayRest_, array_1.arrayReverse_, array_1.arraySecond_, array_1.arraySeventh_, array_1.arraySixth_, array_1.arrayTake_, array_1.arrayTenth_, array_1.arrayThird_, array_1.arrayp_, procedures_1.booleanp_, list_1.consDotF_, list_1.consDotP_, list_1.consp_, procedures_1.const_, list_1.dottedListP_, list_1.dottedPairP_, list_1.eighth_, equal_1.eqp_, equal_1.eqvp_, procedures_1.error_, procedures_1.evenp_, object_1.fieldNames_, list_1.fifth_, procedures_1.filter_, procedures_1.findfIndex_, list_1.first_, list_1.fourth_, symbol_1.gensymp_, symbol_1.gensym_, hash_1.hashToList_, hash_1.hashClearX_, hash_1.hashCopy_, hash_1.hashEntries_, hash_1.hashHasKeyP_, hash_1.hashKeys_, hash_1.hashRemoveX_, hash_1.hashSetX_, hash_1.hashSize_, hash_1.hashValues_, hash_1.hashp_, procedures_1.indexWhere_, procedures_1.isAP_, javascript_1.jsFindIndex_, javascript_1.jsFunctionObjectP_, javascript_1.jsFunctionTypeP_, javascript_1.jsFunctionP_, object_1.jsKeys_, javascript_1.jsNullP_, object_1.jsObjP_, object_1.jsObjectTypeP_, javascript_1.jsSameValueP_, list_1.linkedListCar_, list_1.linkedListCdr_, list_1.linkedListEighth_, list_1.linkedListFifth_, list_1.linkedListFirst_, list_1.linkedListFourth_, list_1.linkedListHead_, list_1.linkedListLinkCar_, list_1.linkedListLinkCdr_, list_1.linkedListLinkP_, list_1.linkedListNinth_, list_1.linkedListNth_, list_1.linkedListNthcdr_, list_1.linkedListSecond_, list_1.linkedListSeventh_, list_1.linkedListSixth_, list_1.linkedListTail_, list_1.linkedListTenth_, list_1.linkedListThird_, list_1.linkedListP_, list_1.linkedPairCar_, list_1.linkedPairCdr_, list_1.linkedPairP_, list_1.listp_, procedures_1.memfp_, procedures_1.memqp_, list_1.ninth_, list_1.nth_, list_1.nullp_, string_1.numberToString_, procedures_1.numberp_, procedures_1.oddp_, procedures_1.onep_, plist_1.plistCopy_, plist_1.plistp_, list_1.popLeftX_, list_1.popRightX_, procedures_1.procedurep_, regexp_1.regexpMatchP_, regexp_1.regexpMatch_, regexp_1.regexpQuote_, regexp_1.regexpReplace_, regexp_1.regexpp_, list_1.rest_, list_1.reverse_, list_1.second_, list_1.seventh_, list_1.sixth_, string_1.stringToNumber_, symbol_1.stringToSymbol_, string_1.stringDowncase_, string_1.stringJoin_, string_1.stringObjectP_, string_1.stringPrimitiveP_, string_1.stringRef_, string_1.stringRepeat_, string_1.stringSplit_, string_1.stringUpcase_, procedures_1.sub1_, symbol_1.symbolToString_, symbol_1.symbolp_, list_1.tenth_, list_1.third_, procedures_1.typeOf_, procedures_1.undefinedp_, procedures_1.zerop_];
/**
 * Compilation map.
 *
 * Map from languages to compilation mapping environments.
 */
const compilationMap = new Map([['JavaScript', compilationMappingEnv], ['TypeScript', compilationMappingEnv]]);
/**
 * Compile a Lisp expression to JavaScript or TypeScript.
 * Returns a string of JavaScript or TypeScript code.
 *
 * `exp` may be an S-expression, an S-expression wrapped in a rose
 * tree, or a module object.
 */
function compile(exp, env = new env_1.LispEnvironment(), options = {}) {
    const languageOption = options['language'] || constants_1.defaultLanguage;
    const langEnv = extendsLispEnvironmentP(env) ? env : new env_1.EnvironmentStack(env, langEnvironment);
    const mappingEnv = compilationMap.get(languageOption) || compilationMappingEnv;
    const compilationOptions = addDefaultOptions(options, true);
    const compiledEnv = new env_1.LispEnvironment();
    let bindingsEnv = new env_1.LispEnvironment();
    const continuationEnv = new env_1.EnvironmentStack(bindingsEnv, env);
    compilationOptions['lispEnvironment'] = langEnv;
    compilationOptions['compilationMappingEnvironment'] = mappingEnv;
    compilationOptions['bindings'] = bindingsEnv;
    compilationOptions['continuationEnv'] = continuationEnv;
    compilationOptions['compiledEnv'] = compiledEnv;
    let ast = (exp instanceof Module) ? compileModule(exp, langEnv, compilationOptions) : ((exp instanceof rose_1.Rose) ? compileRose(exp, langEnv, compilationOptions) : compileSexp(exp, langEnv, compilationOptions));
    ast = optimizeEstree(ast);
    return (0, printer_1.printEstree)(ast, compilationOptions);
}
exports.compileLisp = compile;
exports.compileLispToJavascript = compile;
exports.compile = compile;
compile.lispSource = [Symbol.for('define'), [Symbol.for('compile'), Symbol.for('exp'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language-option'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'language'], Symbol.for('default-language')]], [Symbol.for('define'), Symbol.for('lang-env'), [Symbol.for('if'), [Symbol.for('extends-lisp-environment?'), Symbol.for('env')], Symbol.for('env'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('env'), Symbol.for('lang-environment')]]], [Symbol.for('define'), Symbol.for('mapping-env'), [Symbol.for('or'), [Symbol.for('hash-ref'), Symbol.for('compilation-map'), Symbol.for('language-option')], Symbol.for('compilation-mapping-env')]], [Symbol.for('define'), Symbol.for('compilation-options'), [Symbol.for('add-default-options'), Symbol.for('options'), Symbol.for('#t')]], [Symbol.for('define'), Symbol.for('compiled-env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('define'), Symbol.for('continuation-env'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('bindings-env'), Symbol.for('env')]], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'lispEnvironment', Symbol.for('lang-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'compilationMappingEnvironment', Symbol.for('mapping-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'bindings', Symbol.for('bindings-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'continuationEnv', Symbol.for('continuation-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'compiledEnv', Symbol.for('compiled-env')], [Symbol.for('define'), Symbol.for('ast'), [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Module')], [Symbol.for('compile-module'), Symbol.for('exp'), Symbol.for('lang-env'), Symbol.for('compilation-options')]], [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('compile-rose'), Symbol.for('exp'), Symbol.for('lang-env'), Symbol.for('compilation-options')]], [Symbol.for('else'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('lang-env'), Symbol.for('compilation-options')]]]], [Symbol.for('set!'), Symbol.for('ast'), [Symbol.for('optimize-estree'), Symbol.for('ast')]], [Symbol.for('print-estree'), Symbol.for('ast'), Symbol.for('compilation-options')]];
/**
 * Compile a set of modules together.
 * The modules may reference one another.
 */
function compileModules(modules, env, options = {}) {
    let moduleMap = new Map();
    let compiledModuleMap;
    let moduleName;
    for (let module of modules) {
        if (!(module instanceof rose_1.Rose)) {
            module = (0, rose_1.makeRose)(module);
        }
        moduleName = module.get(1).getValue();
        if (typeof moduleName === 'symbol') {
            moduleName = moduleName.description;
        }
        moduleName = moduleName.replace(new RegExp('^\\./'), '');
        moduleMap.set(moduleName, module);
    }
    compiledModuleMap = compileModuleMap(moduleMap, env, options);
    // `(,@(send compiled-module-map values))
    return [...compiledModuleMap.values()];
}
exports.compileModules = compileModules;
compileModules.lispSource = [Symbol.for('define'), [Symbol.for('compile-modules'), Symbol.for('modules'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('module-map'), [Symbol.for('make-hash')]], [Symbol.for('define'), Symbol.for('compiled-module-map')], [Symbol.for('define'), Symbol.for('module-name')], [Symbol.for('for'), [[Symbol.for('module'), Symbol.for('modules')]], [Symbol.for('unless'), [Symbol.for('is-a?'), Symbol.for('module'), Symbol.for('Rose')], [Symbol.for('set!'), Symbol.for('module'), [Symbol.for('make-rose'), Symbol.for('module')]]], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('module'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('_'), Symbol.for('get-value')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('module-name')], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('symbol->string'), Symbol.for('module-name')]]], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\./'], Symbol.for('module-name'), '']], [Symbol.for('hash-set!'), Symbol.for('module-map'), Symbol.for('module-name'), Symbol.for('module')]], [Symbol.for('set!'), Symbol.for('compiled-module-map'), [Symbol.for('compile-module-map'), Symbol.for('module-map'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('append'), [Symbol.for('send'), Symbol.for('compiled-module-map'), Symbol.for('values')]]];
/**
 * Compile a module map.
 * Returns a new map containing compiled modules.
 */
function compileModuleMap(moduleMap, env, options = {}) {
    let result = new Map();
    const moduleObjectMap = makeModuleMap(moduleMap, env);
    let compiledModule;
    let module;
    for (let key of moduleObjectMap.keys()) {
        module = moduleObjectMap.get(key);
        compiledModule = compile(module, env, options);
        result.set(key, compiledModule);
    }
    return result;
}
exports.compileModuleMap = compileModuleMap;
compileModuleMap.lispSource = [Symbol.for('define'), [Symbol.for('compile-module-map'), Symbol.for('module-map'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('make-hash')]], [Symbol.for('define'), Symbol.for('module-object-map'), [Symbol.for('make-module-map'), Symbol.for('module-map'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('compiled-module')], [Symbol.for('define'), Symbol.for('module')], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('send'), Symbol.for('module-object-map'), Symbol.for('keys')]]], [Symbol.for('set!'), Symbol.for('module'), [Symbol.for('send'), Symbol.for('module-object-map'), Symbol.for('get'), Symbol.for('key')]], [Symbol.for('set!'), Symbol.for('compiled-module'), [Symbol.for('compile'), Symbol.for('module'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('hash-set!'), Symbol.for('result'), Symbol.for('key'), Symbol.for('compiled-module')]], Symbol.for('result')];
/**
 * Compile a module expression or object.
 */
function compileModule(obj, env, options = {}) {
    if (obj instanceof Module) {
        return compileModuleObject(obj, env, options);
    }
    else {
        return compileModuleExpression(obj, env, options);
    }
}
compileModule.lispSource = [Symbol.for('define'), [Symbol.for('compile-module'), Symbol.for('obj'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('Module')], [Symbol.for('compile-module-object'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-module-expression'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(module ...)` expression.
 */
function compileModuleExpression(node, env, options = {}) {
    let module = moduleExpressionToModuleObject(node, env);
    const compilationOptions = Object.assign(Object.assign({}, options), { currentModule: module });
    return compileModuleObject(module, env, compilationOptions);
}
compileModuleExpression.lispSource = [Symbol.for('define'), [Symbol.for('compile-module-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('module'), [Symbol.for('module-expression-to-module-object'), Symbol.for('node'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('compilation-options'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'currentModule', Symbol.for('module')]]], [Symbol.for('compile-module-object'), Symbol.for('module'), Symbol.for('env'), Symbol.for('compilation-options')]];
/**
 * Compile a `Module` object.
 */
function compileModuleObject(module, env, options = {}) {
    const expressions = module.getExpressions();
    let bindings = options['bindings'] || new env_1.LispEnvironment();
    const moduleEnvironment = module.getEnvironment();
    const moduleOptions = Object.assign({ bindings: bindings, currentModule: module, referencedSymbols: [], inlineLispSources: module.getInlineLispSourcesFlag() }, options);
    const headerStatements = compileStatement((0, rose_1.beginWrapRose)(module.headerNodes), moduleEnvironment, moduleOptions);
    const requireStatements = compileStatement((0, rose_1.beginWrapRose)(module.requireNodes), moduleEnvironment, moduleOptions);
    const mainStatements = compileStatement((0, rose_1.beginWrapRose)(module.mainNodes), moduleEnvironment, moduleOptions);
    const provideStatements = compileStatement((0, rose_1.beginWrapRose)(module.provideNodes), moduleEnvironment, moduleOptions);
    const globalEnvironment = buildGlobalEnvironment(moduleOptions['referencedSymbols'], moduleEnvironment, options);
    const program = makeProgram([...headerStatements.body, ...requireStatements.body, ...globalEnvironment.body, ...mainStatements.body, ...provideStatements.body]);
    return program;
}
compileModuleObject.lispSource = [Symbol.for('define'), [Symbol.for('compile-module-object'), Symbol.for('module'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expressions'), [Symbol.for('send'), Symbol.for('module'), Symbol.for('get-expressions')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'bindings'], [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('define'), Symbol.for('module-environment'), [Symbol.for('send'), Symbol.for('module'), Symbol.for('get-environment')]], [Symbol.for('define'), Symbol.for('module-options'), [Symbol.for('js-obj-append'), [Symbol.for('js-obj'), 'bindings', Symbol.for('bindings'), 'currentModule', Symbol.for('module'), 'referencedSymbols', [Symbol.for('quote'), []], 'inlineLispSources', [Symbol.for('send'), Symbol.for('module'), Symbol.for('get-inline-lisp-sources-flag')]], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('header-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('header-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('require-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('require-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('main-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('main-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('provide-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('provide-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('global-environment'), [Symbol.for('build-global-environment'), [Symbol.for('oget'), Symbol.for('module-options'), 'referencedSymbols'], Symbol.for('module-environment'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('program'), [Symbol.for('make-program'), [Symbol.for('append'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('header-statements')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('require-statements')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('global-environment')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('main-statements')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('provide-statements')]]]], Symbol.for('program')];
/**
 * Compile a set of files.
 * This function writes to disk.
 */
function compileFilesX(files, options = {}) {
    const moduleExpressionMap = new thunk_1.ThunkedMap();
    const filenameMap = new thunk_1.ThunkedMap();
    const indentOption = options['indent'];
    const languageOption = (options['language'] || '').match(new RegExp('^TypeScript$', 'i')) ? 'TypeScript' : 'JavaScript';
    const outDirOption = options['outDir'] || '';
    const commentsOption = options['comments'];
    const quickOption = options['quick'];
    const compilationOptions = Object.assign(Object.assign({}, options), { expressionType: 'statement', language: languageOption });
    const extension = (languageOption === 'TypeScript') ? '.ts' : '.js';
    let code;
    let data;
    let module;
    let moduleName;
    const moduleNames = [];
    let moduleMap;
    let node;
    let outFile;
    for (let file of files) {
        moduleName = (0, path_1.basename)(file, (0, path_1.extname)(file));
        filenameMap.set(moduleName, file);
        moduleExpressionMap.set(moduleName, (0, thunk_1.thunk)(function () {
            const data = '(module m scheme\n' +
                (0, fs_1.readFileSync)(file, {
                    encoding: 'utf8'
                }).replace(new RegExp('^#!.*'), '') + '\n' +
                ')';
            let node = (0, parser_1.readRose)(data, {
                comments: commentsOption
            });
            return node;
        }));
        if (quickOption) {
            let shouldCompile = false;
            try {
                const inFile = file;
                const inStats = (0, fs_1.fstatSync)((0, fs_1.openSync)(inFile, 'r'));
                let outFile = (0, path_1.join)(outDirOption, moduleName + extension);
                const outStats = (0, fs_1.fstatSync)((0, fs_1.openSync)(outFile, 'r'));
                if (inStats.mtimeMs > outStats.mtimeMs) {
                    shouldCompile = true;
                }
            }
            catch (err) {
                if (err instanceof Error) {
                    shouldCompile = true;
                }
                else {
                    throw err;
                }
            }
            if (shouldCompile) {
                moduleNames.push(moduleName);
            }
        }
        else {
            moduleNames.push(moduleName);
        }
    }
    moduleMap = makeModuleMap(moduleExpressionMap, compilationEnvironment);
    for (let moduleName of moduleNames) {
        module = moduleMap.get(moduleName);
        code = compile(module, compilationEnvironment, compilationOptions);
        outFile = (0, path_1.join)(outDirOption, moduleName + extension);
        (0, fs_1.mkdirSync)(outDirOption, {
            recursive: true
        });
        (0, fs_1.writeFileSync)(outFile, code, {
            encoding: 'utf8'
        });
        console.log('Compiled ' + filenameMap.get(moduleName) + ' to ' + outFile);
    }
    return moduleMap;
}
exports.compileFilesX = compileFilesX;
compileFilesX.lispSource = [Symbol.for('define'), [Symbol.for('compile-files!'), Symbol.for('files'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('module-expression-map'), [Symbol.for('new'), Symbol.for('ThunkedMap')]], [Symbol.for('define'), Symbol.for('filename-map'), [Symbol.for('new'), Symbol.for('ThunkedMap')]], [Symbol.for('define'), Symbol.for('indent-option'), [Symbol.for('oget'), Symbol.for('options'), 'indent']], [Symbol.for('define'), Symbol.for('language-option'), [Symbol.for('if'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^TypeScript$', 'i'], [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'language'], '']], 'TypeScript', 'JavaScript']], [Symbol.for('define'), Symbol.for('out-dir-option'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'outDir'], '']], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('quick-option'), [Symbol.for('oget'), Symbol.for('options'), 'quick']], [Symbol.for('define'), Symbol.for('compilation-options'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'statement', 'language', Symbol.for('language-option')]]], [Symbol.for('define'), Symbol.for('extension'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language-option'), 'TypeScript'], '.ts', '.js']], [Symbol.for('define'), Symbol.for('code')], [Symbol.for('define'), Symbol.for('data')], [Symbol.for('define'), Symbol.for('module')], [Symbol.for('define'), Symbol.for('module-name')], [Symbol.for('define'), Symbol.for('module-names'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('module-map')], [Symbol.for('define'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('out-file')], [Symbol.for('for'), [[Symbol.for('file'), Symbol.for('files')]], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('basename'), Symbol.for('file'), [Symbol.for('extname'), Symbol.for('file')]]], [Symbol.for('hash-set!'), Symbol.for('filename-map'), Symbol.for('module-name'), Symbol.for('file')], [Symbol.for('hash-set!'), Symbol.for('module-expression-map'), Symbol.for('module-name'), [Symbol.for('thunk'), [Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('data'), [Symbol.for('~>'), Symbol.for('file'), [Symbol.for('readFileSync'), Symbol.for('_'), [Symbol.for('js-obj'), 'encoding', 'utf8']], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^#!.*'], Symbol.for('_'), ''], [Symbol.for('string-append'), '(module m scheme\n', Symbol.for('_'), '\n' +
                                    ')']]], [Symbol.for('define'), Symbol.for('node'), [Symbol.for('read-rose'), Symbol.for('data'), [Symbol.for('js-obj'), 'comments', Symbol.for('comments-option')]]], Symbol.for('node')]]], [Symbol.for('cond'), [Symbol.for('quick-option'), [Symbol.for('define'), Symbol.for('should-compile'), Symbol.for('#f')], [Symbol.for('try'), [Symbol.for('define'), Symbol.for('in-file'), Symbol.for('file')], [Symbol.for('define'), Symbol.for('in-stats'), [Symbol.for('fstatSync'), [Symbol.for('openSync'), Symbol.for('in-file'), 'r']]], [Symbol.for('define'), Symbol.for('out-file'), [Symbol.for('join'), Symbol.for('out-dir-option'), [Symbol.for('string-append'), Symbol.for('module-name'), Symbol.for('extension')]]], [Symbol.for('define'), Symbol.for('out-stats'), [Symbol.for('fstatSync'), [Symbol.for('openSync'), Symbol.for('out-file'), 'r']]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('get-field'), Symbol.for('mtimeMs'), Symbol.for('in-stats')], [Symbol.for('get-field'), Symbol.for('mtimeMs'), Symbol.for('out-stats')]], [Symbol.for('set!'), Symbol.for('should-compile'), Symbol.for('#t')]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('err'), [Symbol.for('set!'), Symbol.for('should-compile'), Symbol.for('#t')]]], [Symbol.for('when'), Symbol.for('should-compile'), [Symbol.for('push-right!'), Symbol.for('module-names'), Symbol.for('module-name')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('module-names'), Symbol.for('module-name')]]]], [Symbol.for('set!'), Symbol.for('module-map'), [Symbol.for('make-module-map'), Symbol.for('module-expression-map'), Symbol.for('compilation-environment')]], [Symbol.for('for'), [[Symbol.for('module-name'), Symbol.for('module-names')]], [Symbol.for('set!'), Symbol.for('module'), [Symbol.for('send'), Symbol.for('module-map'), Symbol.for('get'), Symbol.for('module-name')]], [Symbol.for('set!'), Symbol.for('code'), [Symbol.for('compile'), Symbol.for('module'), Symbol.for('compilation-environment'), Symbol.for('compilation-options')]], [Symbol.for('set!'), Symbol.for('out-file'), [Symbol.for('join'), Symbol.for('out-dir-option'), [Symbol.for('string-append'), Symbol.for('module-name'), Symbol.for('extension')]]], [Symbol.for('mkdirSync'), Symbol.for('out-dir-option'), [Symbol.for('js-obj'), 'recursive', Symbol.for('#t')]], [Symbol.for('writeFileSync'), Symbol.for('out-file'), Symbol.for('code'), [Symbol.for('js-obj'), 'encoding', 'utf8']], [Symbol.for('display'), [Symbol.for('string-append'), 'Compiled ', [Symbol.for('hash-ref'), Symbol.for('filename-map'), Symbol.for('module-name')], ' to ', Symbol.for('out-file')]]], Symbol.for('module-map')];
/**
 * Compile a file.
 * This function writes to disk.
 */
function compileFileX(infile, outfile, options = {}) {
    // TODO: `outfile`. Maybe by adding an
    // `outFileMap` option to `compile-files!`?
    return compileFilesX([infile], options);
}
exports.compileFileX = compileFileX;
compileFileX.lispSource = [Symbol.for('define'), [Symbol.for('compile-file!'), Symbol.for('infile'), Symbol.for('outfile'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-files!'), [Symbol.for('list'), Symbol.for('infile')], Symbol.for('options')]];
/**
 * Compile a S-expression wrapped in a rose tree.
 */
function compileRose(node, env, options = {}) {
    const inheritedOptions = options['bindings'] ? options : Object.assign(Object.assign({}, options), { bindings: new env_1.LispEnvironment() });
    let bindings = inheritedOptions['bindings'];
    const commentsOption = options['comments'];
    const node1 = optimizeRose(node, env);
    let exp = node1.getValue();
    let result;
    if (Array.isArray(exp)) {
        if (exp.length === 0) {
            result = compileList(node1, env, inheritedOptions);
        }
        else {
            const op = exp[0];
            if ((typeof op === 'symbol') && bindings.has(op) && (bindings.getType(op) !== 'macro')) {
                result = compileFunctionCall(node1, env, inheritedOptions);
            }
            else if ((typeof op === 'symbol') && op.description.match(new RegExp('^\\.'))) {
                result = compileDot(node1, env, inheritedOptions);
            }
            else {
                const [f, opType] = env.getTypedValue(op);
                if (opType === 'undefined') {
                    result = compileFunctionCall(node1, env, inheritedOptions);
                }
                else if (inlinedFunctions.includes(f)) {
                    const inlinedExp = definitionToMacro(source(f), exp.slice(1));
                    const inlinedNode = (0, rose_1.makeRose)(inlinedExp, node);
                    result = compileRose(inlinedNode, env, options);
                }
                else {
                    const compilationMappingEnvironment = inheritedOptions['compilationMappingEnvironment'];
                    const [compilationF, compilationType] = compilationMappingEnvironment.getTypedValue(f);
                    if (compilationType === 'compiler') {
                        // Compiler function.
                        result = compilationF(node1, env, inheritedOptions);
                    }
                    else if (compilationType === 'macro') {
                        // Compilation macro.
                        result = compileRose((0, rose_1.insertSexpIntoRose)(compilationF(exp, env), node1, node1), env, inheritedOptions);
                    }
                    else if (opType === 'macro') {
                        // Macro call.
                        result = compileMacroCall(node1, env, inheritedOptions);
                    }
                    else {
                        result = compileFunctionCall(node1, env, inheritedOptions);
                    }
                }
            }
        }
    }
    else if (typeof exp === 'string') {
        result = compileString(node1, env, inheritedOptions);
    }
    else if (typeof exp === 'symbol') {
        result = compileVariable(node1, env, inheritedOptions);
    }
    else if ((0, estree_1.estreep)(exp)) {
        result = exp;
    }
    else {
        result = compileAtom(node1, env, inheritedOptions);
    }
    if (commentsOption && node1.hasProperty('comments')) {
        let comments = node1.getProperty('comments');
        if (comments.length > 0) {
            result.comments = compileComments(comments);
        }
    }
    return result;
}
compileRose.lispSource = [Symbol.for('define'), [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('if'), [Symbol.for('oget'), Symbol.for('options'), 'bindings'], Symbol.for('options'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'bindings', [Symbol.for('new'), Symbol.for('LispEnvironment')]]]]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('node1'), [Symbol.for('optimize-rose'), Symbol.for('node'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node1'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 0], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-list'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('op')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('op')], [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')], 'macro']]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-function-call'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]], [[Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('op')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\.'], [Symbol.for('symbol->string'), Symbol.for('op')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-dot'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('op-type')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-typed-value'), Symbol.for('op')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('op-type'), 'undefined'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-function-call'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]], [[Symbol.for('memq?'), Symbol.for('f'), Symbol.for('inlined-functions')], [Symbol.for('define'), Symbol.for('inlined-exp'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('f')], [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('inlined-node'), [Symbol.for('make-rose'), Symbol.for('inlined-exp'), Symbol.for('node')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-rose'), Symbol.for('inlined-node'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'compilationMappingEnvironment']], [Symbol.for('define-values'), [Symbol.for('compilation-f'), Symbol.for('compilation-type')], [Symbol.for('send'), Symbol.for('compilation-mapping-environment'), Symbol.for('get-typed-value'), Symbol.for('f')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('compilation-type'), 'compiler'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compilation-f'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]], [[Symbol.for('eq?'), Symbol.for('compilation-type'), 'macro'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('insert-sexp-into-rose'), [Symbol.for('compilation-f'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('node1'), Symbol.for('node1')], Symbol.for('env'), Symbol.for('inherited-options')]]], [[Symbol.for('eq?'), Symbol.for('op-type'), 'macro'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-macro-call'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-function-call'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]]]]]]]]]], [[Symbol.for('string?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-string'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]], [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-variable'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]], [[Symbol.for('estree?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('exp')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-atom'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('inherited-options')]]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('comments-option'), [Symbol.for('send'), Symbol.for('node1'), Symbol.for('has-property'), 'comments']], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('send'), Symbol.for('node1'), Symbol.for('get-property'), 'comments']], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('comments')], 0], [Symbol.for('set-field!'), Symbol.for('comments'), Symbol.for('result'), [Symbol.for('compile-comments'), Symbol.for('comments')]]]], Symbol.for('result')];
/**
 * Compile a S-expression.
 */
function compileSexp(exp, env, options = {}) {
    return compileRose((0, rose_1.makeRose)(exp), env, options);
}
compileSexp.lispSource = [Symbol.for('define'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), [Symbol.for('make-rose'), Symbol.for('exp')], [Symbol.for('compile-rose'), Symbol.for('_'), Symbol.for('env'), Symbol.for('options')]]];
/**
 * Compile `node` as an expression.
 */
function compileExpression(node, env, options = {}) {
    return compileRose(node, env, makeExpressionOptions(options));
}
compileExpression.lispSource = [Symbol.for('define'), [Symbol.for('compile-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]]];
/**
 * Compile `node` as a regular statement.
 */
function compileStatement(node, env, options = {}) {
    return compileRose(node, env, makeStatementOptions(options));
}
compileStatement.lispSource = [Symbol.for('define'), [Symbol.for('compile-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('make-statement-options'), Symbol.for('options')]]];
/**
 * Compile `node` as a return statement.
 */
function compileReturnStatement(node, env, options = {}) {
    return compileRose(node, env, makeReturnStatementOptions(options));
}
compileReturnStatement.lispSource = [Symbol.for('define'), [Symbol.for('compile-return-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('make-return-statement-options'), Symbol.for('options')]]];
/**
 * Compile `node` as a regular statement or as a return statement,
 * depending on the value of the `expressionType` option.
 */
function compileStatementOrReturnStatement(node, env, options = {}) {
    if (options['expressionType'] === 'return') {
        return compileReturnStatement(node, env, options);
    }
    else {
        return compileStatement(node, env, options);
    }
}
compileStatementOrReturnStatement.lispSource = [Symbol.for('define'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType'], 'return'], [Symbol.for('compile-return-statement'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-statement'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Helper function for compiling a list of statements.
 * The last statement is compiled as a `return` statement
 * if the `expressionType` option is `"return"`.
 */
function compileStatements(statements, env, options) {
    const expressionType = options['expressionType'];
    let result = [];
    let returnIdx = -1;
    if (expressionType === 'return') {
        const _start = statements.length - 1;
        for (let i = _start; i > -1; i--) {
            const statement = statements[i];
            if (!((0, util_1.formp)(statement, break_, env) || (0, util_1.formp)(statement, continue_, env) || (0, util_1.formp)(statement, yield_, env))) {
                returnIdx = i;
                break;
            }
        }
    }
    const _end = statements.length;
    for (let i = 0; i < _end; i++) {
        const statement = statements[i];
        if (i === returnIdx) {
            result.push(compileReturnStatement(statement, env, options));
        }
        else {
            result.push(compileStatement(statement, env, options));
        }
    }
    // TODO: If the last statement is a `break`/`yield` statement and
    // the penultimate statement is a `return` statement, we can drop
    // the last statement. (Might want a setting to make this behavior
    // toggleable, though.)
    return result;
}
compileStatements.lispSource = [Symbol.for('define'), [Symbol.for('compile-statements'), Symbol.for('statements'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('return-idx'), -1], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('statements')], 1], -1, -1]]], [Symbol.for('define'), Symbol.for('statement'), [Symbol.for('aget'), Symbol.for('statements'), Symbol.for('i')]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('statement'), Symbol.for('break_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('statement'), Symbol.for('continue_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('statement'), Symbol.for('yield_'), Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('return-idx'), Symbol.for('i')], [Symbol.for('break')]]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('statements')]]]], [Symbol.for('define'), Symbol.for('statement'), [Symbol.for('aget'), Symbol.for('statements'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('i'), Symbol.for('return-idx')], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('compile-return-statement'), Symbol.for('statement'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('compile-statement'), Symbol.for('statement'), Symbol.for('env'), Symbol.for('options')]]]]], Symbol.for('result')];
/**
 * Evaluate a Lisp expression `exp` with environment `env`.
 *
 * `env`, if specified, must be a Lisp environment as returned
 * by {@link Environment}. The expression is evaluated in
 * context of a basic Lisp environment defining such constructs
 * as `(if ...)`, `(cond ...)`, and so on.
 */
const interpret = (0, curry_1.dashify)(function (exp, env = (0, env_1.defaultEnvironment)(), options = {}) {
    const evaluator = options['evaluator'] || eval_1.eval_ || eval_1.defaultEvaluator;
    const environment = makeInterpretationEnvironment(env, options);
    return (0, eval_1.callEvaluator)(evaluator, exp, environment, options);
});
exports.interpret = interpret;
/**
 * Interpret a string of Lisp code.
 */
function interpretString(str, env = undefined, options = {}) {
    return interpret((0, parser_1.readSexp)(str), env, options);
}
exports.interpretString = interpretString;
interpretString.lispSource = [Symbol.for('define'), [Symbol.for('interpret-string'), Symbol.for('str'), [Symbol.for('env'), Symbol.for('undefined')], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('interpret'), [Symbol.for('read-sexp'), Symbol.for('str')], Symbol.for('env'), Symbol.for('options')]];
/**
 * Interpret a list of files.
 */
function interpretFiles(files, env = undefined, options = {}) {
    return files.map(function (file) {
        const str = '(begin\n' +
            (0, fs_1.readFileSync)(file, {
                encoding: 'utf8'
            }).replace(new RegExp('^#!.*'), '') + '\n' +
            ')';
        let result = interpretString(str, env, options);
        return result;
    });
}
exports.interpretFiles = interpretFiles;
interpretFiles.lispSource = [Symbol.for('define'), [Symbol.for('interpret-files'), Symbol.for('files'), [Symbol.for('env'), Symbol.for('undefined')], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('file')], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('~>'), Symbol.for('file'), [Symbol.for('readFileSync'), Symbol.for('_'), [Symbol.for('js-obj'), 'encoding', 'utf8']], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^#!.*'], Symbol.for('_'), ''], [Symbol.for('string-append'), '(begin\n', Symbol.for('_'), '\n' +
                            ')']]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('interpret-string'), Symbol.for('str'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('result')], Symbol.for('files')]];
/**
 * Interpret a string of Lisp code.
 * Alias for `interpret-string`.
 */
function lisp(str, env = undefined) {
    return interpretString(str, env);
}
exports.lisp = lisp;
lisp.lispSource = [Symbol.for('define'), [Symbol.for('lisp'), Symbol.for('str'), [Symbol.for('env'), Symbol.for('undefined')]], [Symbol.for('interpret-string'), Symbol.for('str'), Symbol.for('env')]];
/**
 * Make a Lisp environment.
 */
function makeLisp(variables = [], isLisp2 = false) {
    return new env_1.LispEnvironment(variables, lispEnvironment);
}
exports.makeLisp = makeLisp;
makeLisp.lispSource = [Symbol.for('define'), [Symbol.for('make-lisp'), [Symbol.for('variables'), [Symbol.for('quote'), []]], [Symbol.for('is-lisp-2'), Symbol.for('#f')]], [Symbol.for('new'), Symbol.for('LispEnvironment'), Symbol.for('variables'), Symbol.for('lisp-environment')]];
/**
 * Make a Lisp interpretation environment.
 */
function makeInterpretationEnvironment(env, options = {}) {
    let evalOption = options['eval'];
    // TODO: Make `#f` the default.
    if (evalOption === undefined) {
        evalOption = true;
    }
    if ((env === langEnvironment) || ((env instanceof env_1.EnvironmentStack) && env.hasEnvironment(langEnvironment))) {
        return env;
    }
    else {
        return new env_1.EnvironmentStack(env, evalOption ? interpretationEnvironment : interpretationEnvironmentNoEval);
    }
}
makeInterpretationEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('make-interpretation-environment'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('eval-option'), [Symbol.for('oget'), Symbol.for('options'), 'eval']], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('eval-option'), Symbol.for('undefined')], [Symbol.for('set!'), Symbol.for('eval-option'), Symbol.for('#t')]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('env'), Symbol.for('lang-environment')], [Symbol.for('and'), [Symbol.for('is-a?'), Symbol.for('env'), Symbol.for('EnvironmentStack')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('has-environment'), Symbol.for('lang-environment')]]], Symbol.for('env')], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('env'), [Symbol.for('if'), Symbol.for('eval-option'), Symbol.for('interpretation-environment'), Symbol.for('interpretation-environment-no-eval')]]]]];
/**
 * Make an environment suitable for expanding macros
 * and compiler macros.
 */
function makeMacroEnvironment(env) {
    return new env_1.EnvironmentStack(new env_1.EnvironmentPipe(env, compilationMacroMappingEnv), env);
}
makeMacroEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('make-macro-environment'), Symbol.for('env')], [Symbol.for('new'), Symbol.for('EnvironmentStack'), [Symbol.for('new'), Symbol.for('EnvironmentPipe'), Symbol.for('env'), Symbol.for('compilation-macro-mapping-env')], Symbol.for('env')]];
/**
 * Make compilation options for compiling a form as
 * an expression.
 */
function makeExpressionOptions(options) {
    return Object.assign(Object.assign({}, options), { expressionType: 'expression' });
}
makeExpressionOptions.lispSource = [Symbol.for('define'), [Symbol.for('make-expression-options'), Symbol.for('options')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'expression']]];
/**
 * Make compilation options for compiling a form as
 * a statement.
 */
function makeStatementOptions(options) {
    return Object.assign(Object.assign({}, options), { expressionType: 'statement' });
}
makeStatementOptions.lispSource = [Symbol.for('define'), [Symbol.for('make-statement-options'), Symbol.for('options')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'statement']]];
/**
 * Make compilation options for compiling a form as
 * a return statement.
 */
function makeReturnStatementOptions(options) {
    return Object.assign(Object.assign({}, options), { expressionType: 'return' });
}
makeReturnStatementOptions.lispSource = [Symbol.for('define'), [Symbol.for('make-return-statement-options'), Symbol.for('options')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'return']]];
/**
 * Make an expression or statement ESTree node,
 * conditional on options.
 */
function makeExpressionOrStatement(node, options = {}) {
    const expressionType = options['expressionType'];
    if ((expressionType === 'statement') || (expressionType === 'return')) {
        return wrapExpressionInStatement(node, options);
    }
    else {
        return node;
    }
}
makeExpressionOrStatement.lispSource = [Symbol.for('define'), [Symbol.for('make-expression-or-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('wrap-expression-in-statement'), Symbol.for('node'), Symbol.for('options')]], [Symbol.for('else'), Symbol.for('node')]]];
/**
 * Wrap an expression in a statement. An `ExpressionStatement`
 * or `ReturnStatement` node is returned, conditional on options.
 */
function wrapExpressionInStatement(node, options = {}) {
    const expressionType = options['expressionType'];
    if (!(node instanceof estree_1.Expression)) {
        return node;
    }
    else if (expressionType === 'return') {
        return new estree_1.ReturnStatement(node);
    }
    else {
        return new estree_1.ExpressionStatement(node);
    }
}
wrapExpressionInStatement.lispSource = [Symbol.for('define'), [Symbol.for('wrap-expression-in-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Expression')]], Symbol.for('node')], [[Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('new'), Symbol.for('ReturnStatement'), Symbol.for('node')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ExpressionStatement'), Symbol.for('node')]]]];
/**
 * Wraps `node` in a `BlockStatement`.
 */
function wrapInBlockStatement(obj) {
    return makeBlockStatement([obj]);
}
wrapInBlockStatement.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-block-statement'), Symbol.for('obj')], [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('obj')]]];
/**
 * Wraps `node` in a `BlockStatement` unless `node` already is
 * a `BlockStatement`. In other words, avoids double wrapping.
 */
function wrapInBlockStatementSmart(node) {
    if ((0, estree_1.estreeTypeP)(node, 'BlockStatement')) {
        return node;
    }
    else {
        return makeBlockStatement([node]);
    }
}
wrapInBlockStatementSmart.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-block-statement-smart'), Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('node'), 'BlockStatement'], Symbol.for('node')], [Symbol.for('else'), [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('node')]]]]];
/**
 * Wrap `exp` in a `lambda` call.
 */
function wrapInLambdaCall(exp) {
    return (0, rose_1.makeRose)([[Symbol.for('lambda'), [], exp]]);
}
wrapInLambdaCall.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-lambda-call'), Symbol.for('exp')], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [], [Symbol.for('unquote'), Symbol.for('exp')]]]]]];
/**
 * Wrap `exp` in a `js/arrow` call.
 */
function wrapInArrowCall(exp) {
    return (0, rose_1.makeRose)([[Symbol.for('js/arrow'), [], exp]]);
}
wrapInArrowCall.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('js/arrow'), [], [Symbol.for('unquote'), Symbol.for('exp')]]]]]];
/**
 * Make a `BlockStatement`.
 * Handles `Program` fragments.
 */
function makeBlockStatement(body) {
    if (Array.isArray(body)) {
        return new estree_1.BlockStatement(makeBlockStatementHelper(body));
    }
    else {
        return makeBlockStatement([body]);
    }
}
makeBlockStatement.lispSource = [Symbol.for('define'), [Symbol.for('make-block-statement'), Symbol.for('body')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('body')], [Symbol.for('new'), Symbol.for('BlockStatement'), [Symbol.for('make-block-statement-helper'), Symbol.for('body')]]], [Symbol.for('else'), [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('body')]]]]];
/**
 * Helper function for `make-block-statement`.
 */
function makeBlockStatementHelper(body) {
    let statements = [];
    for (let statement of body) {
        if ((0, estree_1.estreeTypeP)(statement, 'Program')) {
            // Program fragments are represented with `Program`.
            // Their contents are spliced into the block statement.
            const fragment = statement;
            const fragmentStatements = fragment.body;
            const fragmentComments = fragment.comments;
            if (fragmentStatements.length > 0) {
                (0, rose_1.transferComments)(fragment, fragmentStatements[0]);
                statements = [...statements, ...fragmentStatements];
            }
            else if (fragmentComments.length > 0) {
                statements.push(statement);
            }
        }
        else {
            statements.push(statement);
        }
    }
    return statements;
}
makeBlockStatementHelper.lispSource = [Symbol.for('define'), [Symbol.for('make-block-statement-helper'), Symbol.for('body')], [Symbol.for('define'), Symbol.for('statements'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('statement'), Symbol.for('body')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('statement'), 'Program'], [Symbol.for('define'), Symbol.for('fragment'), Symbol.for('statement')], [Symbol.for('define'), Symbol.for('fragment-statements'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('fragment')]], [Symbol.for('define'), Symbol.for('fragment-comments'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('fragment')]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('fragment-statements')], 0], [Symbol.for('transfer-comments'), Symbol.for('fragment'), [Symbol.for('first'), Symbol.for('fragment-statements')]], [Symbol.for('set!'), Symbol.for('statements'), [Symbol.for('append'), Symbol.for('statements'), Symbol.for('fragment-statements')]]], [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('fragment-comments')], 0], [Symbol.for('push-right!'), Symbol.for('statements'), Symbol.for('statement')]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('statements'), Symbol.for('statement')]]]], Symbol.for('statements')];
/**
 * Make a `Program`.
 */
function makeProgram(body) {
    return new estree_1.Program(makeBlockStatementHelper(body));
}
makeProgram.lispSource = [Symbol.for('define'), [Symbol.for('make-program'), Symbol.for('body')], [Symbol.for('new'), Symbol.for('Program'), [Symbol.for('make-block-statement-helper'), Symbol.for('body')]]];
/**
 * Make a `Program` fragment (i.e., a program that
 * is to be spliced into the containing program).
 */
function makeProgramFragment(body = []) {
    // `Program` is used to represent programs
    // and program fragments.
    return makeProgram(body);
}
makeProgramFragment.lispSource = [Symbol.for('define'), [Symbol.for('make-program-fragment'), [Symbol.for('body'), [Symbol.for('quote'), []]]], [Symbol.for('make-program'), Symbol.for('body')]];
/**
 * Make an empty program fragment.
 */
function emptyProgram() {
    return makeProgramFragment();
}
emptyProgram.lispSource = [Symbol.for('define'), [Symbol.for('empty-program')], [Symbol.for('make-program-fragment')]];
/**
 * Unwrap a `BlockStatement`, i.e., return the expression it
 * contains. The statement is assumed to contain a single
 * expression.
 */
function unwrapBlockStatement(exp) {
    if (!(0, estree_1.estreeTypeP)(exp, 'BlockStatement')) {
        return exp;
    }
    let unwrappedExp = exp;
    while ((unwrappedExp.body.length === 1) && (0, estree_1.estreeTypeP)(unwrappedExp.body[0], 'BlockStatement')) {
        unwrappedExp = unwrappedExp.body[0];
    }
    return unwrappedExp;
}
unwrapBlockStatement.lispSource = [Symbol.for('define'), [Symbol.for('unwrap-block-statement'), Symbol.for('exp')], [Symbol.for('unless'), [Symbol.for('estree-type?'), Symbol.for('exp'), 'BlockStatement'], [Symbol.for('return'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('unwrapped-exp'), Symbol.for('exp')], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('unwrapped-exp')]], 1], [Symbol.for('estree-type?'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('unwrapped-exp')]], 'BlockStatement']], [Symbol.for('set!'), Symbol.for('unwrapped-exp'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('unwrapped-exp')]]]], Symbol.for('unwrapped-exp')];
/**
 * Remove the comment prefix (`; `, `;; `, `;;; `, etc.)
 * from a comment string.
 */
function removeCommentPrefix(comment) {
    return comment.replace(new RegExp('^[^\\S\\r\\n]*[;]+[^\\S\\r\\n]?', 'gm'), '');
}
removeCommentPrefix.lispSource = [Symbol.for('define'), [Symbol.for('remove-comment-prefix'), Symbol.for('comment')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^[^\\S\\r\\n]*[;]+[^\\S\\r\\n]?', 'gm'], Symbol.for('comment'), '']];
/**
 * Transfer the `comments` property from ESTree `node1` to ESTree `node2`,
 * compiling them in the process.
 */
function transferAndCompileComments(node1, node2, options = {}) {
    const commentsOption = options['comments'];
    let comments = (node1 instanceof rose_1.Rose) ? node1.getProperty('comments') : node1.comments;
    if (commentsOption && comments) {
        if (node2 instanceof rose_1.Rose) {
            node2.setProperty('comments', [...comments, ...(node2.getProperty('comments') || [])]);
        }
        else {
            comments = compileComments(comments);
            node2.comments = [...comments, ...(node2.comments || [])];
        }
    }
    return node2;
}
transferAndCompileComments.lispSource = [Symbol.for('define'), [Symbol.for('transfer-and-compile-comments'), Symbol.for('node1'), Symbol.for('node2'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node1'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node1'), Symbol.for('get-property'), 'comments'], [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node1')]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('comments-option'), Symbol.for('comments')], [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node2'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node2'), Symbol.for('set-property'), 'comments', [Symbol.for('append'), Symbol.for('comments'), [Symbol.for('or'), [Symbol.for('send'), Symbol.for('node2'), Symbol.for('get-property'), 'comments'], [Symbol.for('quote'), []]]]], [Symbol.for('set!'), Symbol.for('comments'), [Symbol.for('compile-comments'), Symbol.for('comments')]], [Symbol.for('set-field!'), Symbol.for('comments'), Symbol.for('node2'), [Symbol.for('append'), Symbol.for('comments'), [Symbol.for('or'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node2')], [Symbol.for('quote'), []]]]]]], Symbol.for('node2')];
/**
 * Compile comments.
 */
function compileComments(comments) {
    const commentsCompiled = [];
    for (let comment of comments) {
        if (comment instanceof parser_1.LeadingCommentToken) {
            const subcomments = splitComments(comment.value);
            for (let subcomment of subcomments) {
                if ((0, parser_1.getCommentLevel)(subcomment) >= 3) {
                    commentsCompiled.push(new estree_1.BlockComment(removeCommentPrefix(subcomment)));
                }
                else {
                    commentsCompiled.push(new estree_1.LeadingComment(removeCommentPrefix(subcomment)));
                }
            }
        }
        else if (comment instanceof parser_1.TrailingCommentToken) {
            commentsCompiled.push(new estree_1.TrailingComment(removeCommentPrefix(comment.value)));
        }
    }
    return commentsCompiled;
}
compileComments.lispSource = [Symbol.for('define'), [Symbol.for('compile-comments'), Symbol.for('comments')], [Symbol.for('define'), Symbol.for('comments-compiled'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('comment'), Symbol.for('comments')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('LeadingCommentToken')], [Symbol.for('define'), Symbol.for('subcomments'), [Symbol.for('split-comments'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('comment')]]], [Symbol.for('for'), [[Symbol.for('subcomment'), Symbol.for('subcomments')]], [Symbol.for('cond'), [[Symbol.for('>='), [Symbol.for('get-comment-level'), Symbol.for('subcomment')], 3], [Symbol.for('push-right!'), Symbol.for('comments-compiled'), [Symbol.for('new'), Symbol.for('BlockComment'), [Symbol.for('remove-comment-prefix'), Symbol.for('subcomment')]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('comments-compiled'), [Symbol.for('new'), Symbol.for('LeadingComment'), [Symbol.for('remove-comment-prefix'), Symbol.for('subcomment')]]]]]]], [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('TrailingCommentToken')], [Symbol.for('push-right!'), Symbol.for('comments-compiled'), [Symbol.for('new'), Symbol.for('TrailingComment'), [Symbol.for('remove-comment-prefix'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('comment')]]]]]]], Symbol.for('comments-compiled')];
/**
 * Split up a string containing multiple comments.
 */
function splitComments(str) {
    let comments = [];
    let comment = '';
    let currentLevel = -1;
    let lines = str.split('\n');
    if (str.match(new RegExp('\\n$'))) {
        lines = lines.slice(0, -1);
    }
    for (let x of lines) {
        if (x === '') {
            if (comment.match(new RegExp('\\n$'))) {
                comment = comment + '\n';
                comments.push(comment);
                comment = '';
            }
            else {
                comment = comment + '\n';
            }
        }
        else {
            const level = (0, parser_1.getCommentLevel)(x);
            if (level !== currentLevel) {
                if (!((comment === '') || (comment === '\n'))) {
                    comments.push(comment);
                    comment = '';
                }
                currentLevel = level;
            }
            comment = comment + x + '\n';
        }
    }
    if (!((comment === '') || (comment === '\n'))) {
        comments.push(comment);
    }
    return comments;
}
exports.splitComments = splitComments;
splitComments.lispSource = [Symbol.for('define'), [Symbol.for('split-comments'), Symbol.for('str')], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('comment'), ''], [Symbol.for('define'), Symbol.for('current-level'), -1], [Symbol.for('define'), Symbol.for('lines'), [Symbol.for('string-split'), Symbol.for('str'), '\n']], [Symbol.for('when'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], Symbol.for('str')], [Symbol.for('set!'), Symbol.for('lines'), [Symbol.for('drop-right'), Symbol.for('lines'), 1]]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('lines')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('x'), ''], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], Symbol.for('comment')], [Symbol.for('set!'), Symbol.for('comment'), [Symbol.for('string-append'), Symbol.for('comment'), '\n']], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('comment')], [Symbol.for('set!'), Symbol.for('comment'), '']], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('comment'), [Symbol.for('string-append'), Symbol.for('comment'), '\n']]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('level'), [Symbol.for('get-comment-level'), Symbol.for('x')]], [Symbol.for('unless'), [Symbol.for('='), Symbol.for('level'), Symbol.for('current-level')], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('comment'), ''], [Symbol.for('eq?'), Symbol.for('comment'), '\n']], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('comment')], [Symbol.for('set!'), Symbol.for('comment'), '']], [Symbol.for('set!'), Symbol.for('current-level'), Symbol.for('level')]], [Symbol.for('set!'), Symbol.for('comment'), [Symbol.for('string-append'), Symbol.for('comment'), Symbol.for('x'), '\n']]]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('comment'), ''], [Symbol.for('eq?'), Symbol.for('comment'), '\n']], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('comment')]], Symbol.for('comments')];
/**
 * Whether `exp` is a function call, given `env`.
 */
function functionCallP(exp, env) {
    if (exp instanceof rose_1.Rose) {
        return macroCallP(exp.getValue(), env);
    }
    else {
        return Array.isArray(exp) && (exp.length > 1) && (typeof exp[0] === 'symbol') && ['function', 'procedure'].includes(env.getType(exp[0]));
    }
}
functionCallP.lispSource = [Symbol.for('define'), [Symbol.for('function-call?'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('macro-call?'), [Symbol.for('send'), Symbol.for('exp'), Symbol.for('get-value')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('symbol?'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('memq?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('quote'), ['function', 'procedure']]]]]]];
/**
 * Whether `exp` is a macro call, given `env`.
 */
function macroCallP(exp, env) {
    if (exp instanceof rose_1.Rose) {
        return macroCallP(exp.getValue(), env);
    }
    else {
        return Array.isArray(exp) && (exp.length > 1) && (typeof exp[0] === 'symbol') && (env.getType(exp[0]) === 'macro');
    }
}
macroCallP.lispSource = [Symbol.for('define'), [Symbol.for('macro-call?'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('macro-call?'), [Symbol.for('send'), Symbol.for('exp'), Symbol.for('get-value')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('symbol?'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), [Symbol.for('first'), Symbol.for('exp')]], 'macro']]]]];
/**
 * Whether `exp` is a special form, given `env`.
 */
function specialFormP(exp, env) {
    if (exp instanceof rose_1.Rose) {
        return macroCallP(exp.getValue(), env);
    }
    else {
        return Array.isArray(exp) && (exp.length > 1) && (typeof exp[0] === 'symbol') && (env.getType(exp[0]) === 'special');
    }
}
specialFormP.lispSource = [Symbol.for('define'), [Symbol.for('special-form?'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('macro-call?'), [Symbol.for('send'), Symbol.for('exp'), Symbol.for('get-value')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('symbol?'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), [Symbol.for('first'), Symbol.for('exp')]], 'special']]]]];
/**
 * Convert a `(define (...) ...)` form to
 * a `(lambda (...) ...)` form.
 */
function defineToLambda(node, options = {}) {
    const curriedOption = options['curried'];
    let exp = node.getValue();
    const nameAndParams = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1];
    let name = nameAndParams[0];
    let params = cdr(nameAndParams);
    const shouldCurry = curriedOption || ((curriedOption === undefined) && Array.isArray(name));
    if (shouldCurry) {
        params = flatten(nameAndParams).slice(1);
        if (Array.isArray(nameAndParams) && (nameAndParams.length >= 3) && (nameAndParams[nameAndParams.length - 2] === Symbol.for('.')) && !(() => {
            const x = lastCdr(nameAndParams);
            return Array.isArray(x) && (x.length === 0);
        })() && (params.length === 1)) {
            params = params[0];
        }
    }
    return (0, rose_1.makeRose)([Symbol.for('lambda'), params, ...node.drop(2)]);
}
defineToLambda.lispSource = [Symbol.for('define'), [Symbol.for('define->lambda'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('curried-option'), [Symbol.for('oget'), Symbol.for('options'), 'curried']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('name-and-params'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-params')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('cdr'), Symbol.for('name-and-params')]], [Symbol.for('define'), Symbol.for('should-curry'), [Symbol.for('or'), Symbol.for('curried-option'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('curried-option'), Symbol.for('undefined')], [Symbol.for('array?'), Symbol.for('name')]]]], [Symbol.for('when'), Symbol.for('should-curry'), [Symbol.for('set!'), Symbol.for('params'), [Symbol.for('rest'), [Symbol.for('flatten'), Symbol.for('name-and-params')]]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('dotted-list?'), Symbol.for('name-and-params')], [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('params')], 1]], [Symbol.for('set!'), Symbol.for('params'), [Symbol.for('first'), Symbol.for('params')]]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('unquote'), Symbol.for('params')], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]]]]];
/**
 * Convert a function to a macro on the basis
 * of its `(define ...)` form.
 */
function definitionToMacro(exp, args) {
    // TODO: Rest arguments.
    //
    // FIXME: When a complex argument is referenced inside of a `lambda`
    // expression, we should store the value in a local variable.
    const paramsList = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1]).slice(1);
    let params = paramsList.map(function (x) {
        if (Array.isArray(x)) {
            return x[0];
        }
        else {
            return x;
        }
    });
    const body = exp.slice(2);
    if (params.length === 0) {
        if (body.length === 1) {
            return body[0];
        }
        else {
            return [Symbol.for('begin'), ...body];
        }
    }
    else {
        const counts = buildList(args.length, function (...args) {
            return 0;
        });
        const shouldMakeLambda = false;
        let shouldMakeLet = false;
        let result = body.map(function (x) {
            return (0, util_1.mapTree)(function (y) {
                const idx = params.findIndex(function (z) {
                    return z === y;
                });
                if (idx >= 0) {
                    counts[idx] = counts[idx] + 1;
                    if (idx < args.length) {
                        return args[idx];
                    }
                    else {
                        const currentParam = paramsList[idx];
                        if (Array.isArray(currentParam)) {
                            if (Array.isArray(currentParam) && (currentParam.length >= 3) && (currentParam[currentParam.length - 2] === Symbol.for('.')) && (() => {
                                let x1 = lastCdr(currentParam);
                                return Array.isArray(x1) && (x1.length === 0);
                            })()) {
                                let i = 1;
                                let result = currentParam;
                                while (i > 0) {
                                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                        result = currentParam[currentParam.length - 1];
                                    }
                                    else {
                                        result = currentParam.slice(1);
                                    }
                                    i--;
                                }
                                if (Array.isArray(result)) {
                                    result = result[0];
                                }
                                return result;
                            }
                            else {
                                return currentParam[1];
                            }
                        }
                        else {
                            return undefined;
                        }
                    }
                }
                else {
                    return y;
                }
            }, x);
        });
        // Determine whether a complex argument is referenced
        // more than once. If so, we need to make a `lambda`
        // expression instead.
        const _end = args.length;
        for (let i = 0; i < _end; i++) {
            const count = counts[i];
            const arg = args[i];
            if ((count > 1) && !((typeof arg === 'symbol') || (typeof arg === 'boolean') || (typeof arg === 'string') || Number.isFinite(arg))) {
                // (set! should-make-lambda #t)
                shouldMakeLet = true;
                break;
            }
        }
        if (shouldMakeLet) {
            const letBindings = [];
            let gensymMap = new Map();
            const _end = params.length;
            for (let i = 0; i < _end; i++) {
                const argExp = (i < args.length) ? args[i] : (() => {
                    const currentParam = paramsList[i];
                    if (Array.isArray(currentParam)) {
                        if (Array.isArray(currentParam) && (currentParam.length >= 3) && (currentParam[currentParam.length - 2] === Symbol.for('.')) && (() => {
                            const x = lastCdr(currentParam);
                            return Array.isArray(x) && (x.length === 0);
                        })()) {
                            let i = 1;
                            let result = currentParam;
                            while (i > 0) {
                                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                    result = currentParam[currentParam.length - 1];
                                }
                                else {
                                    result = currentParam.slice(1);
                                }
                                i--;
                            }
                            if (Array.isArray(result)) {
                                result = result[0];
                            }
                            return result;
                        }
                        else {
                            return currentParam[1];
                        }
                    }
                    else {
                        return undefined;
                    }
                })();
                const paramExp = params[i];
                let param = Array.isArray(paramExp) ? paramExp[0] : paramExp;
                if (typeof argExp === 'symbol') {
                    gensymMap.set(param, argExp);
                }
                else {
                    const paramGensym = Symbol(param.description);
                    gensymMap.set(param, paramGensym);
                    letBindings.push([paramGensym, argExp]);
                }
            }
            const letBody = (0, util_1.mapTree)(function (x) {
                if (gensymMap.has(x)) {
                    return gensymMap.get(x);
                }
                else {
                    return x;
                }
            }, body);
            return [Symbol.for('let*'), letBindings, ...letBody];
        }
        else if (shouldMakeLambda) {
            return [[Symbol.for('lambda'), params, ...body], ...args];
        }
        else {
            if (result.length === 1) {
                return result[0];
            }
            else {
                return [Symbol.for('begin'), ...result];
            }
        }
    }
}
exports.definitionToMacro = definitionToMacro;
definitionToMacro.lispSource = [Symbol.for('define'), [Symbol.for('definition->macro'), Symbol.for('exp'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('params-list'), [Symbol.for('rest'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('first'), Symbol.for('x')], Symbol.for('x')]], Symbol.for('params-list')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('params')], 0], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('body')], 1], [Symbol.for('first'), Symbol.for('body')]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('counts'), [Symbol.for('build-list'), [Symbol.for('array-list-length'), Symbol.for('args')], [Symbol.for('const'), 0]]], [Symbol.for('define'), Symbol.for('should-make-lambda'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('should-make-let'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('y')], [Symbol.for('define'), Symbol.for('idx'), [Symbol.for('js/find-index'), [Symbol.for('lambda'), [Symbol.for('z')], [Symbol.for('eq?'), Symbol.for('z'), Symbol.for('y')]], Symbol.for('params')]], [Symbol.for('cond'), [[Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('list-set!'), Symbol.for('counts'), Symbol.for('idx'), [Symbol.for('+'), [Symbol.for('aget'), Symbol.for('counts'), Symbol.for('idx')], 1]], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('idx'), [Symbol.for('array-list-length'), Symbol.for('args')]], [Symbol.for('aget'), Symbol.for('args'), Symbol.for('idx')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('current-param'), [Symbol.for('aget'), Symbol.for('params-list'), Symbol.for('idx')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('current-param')], [Symbol.for('second'), Symbol.for('current-param')]], [Symbol.for('else'), Symbol.for('undefined')]]]]], [Symbol.for('else'), Symbol.for('y')]]], Symbol.for('x')]], Symbol.for('body')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('define'), Symbol.for('count'), [Symbol.for('aget'), Symbol.for('counts'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('>'), Symbol.for('count'), 1], [Symbol.for('not'), [Symbol.for('or'), [Symbol.for('symbol?'), Symbol.for('arg')], [Symbol.for('boolean?'), Symbol.for('arg')], [Symbol.for('string?'), Symbol.for('arg')], [Symbol.for('number?'), Symbol.for('arg')]]]], [Symbol.for('set!'), Symbol.for('should-make-let'), Symbol.for('#t')], [Symbol.for('break')]]], [Symbol.for('cond'), [Symbol.for('should-make-let'), [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('gensym-map'), [Symbol.for('make-hash')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('params')]]]], [Symbol.for('define'), Symbol.for('arg-exp'), [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), [Symbol.for('array-list-length'), Symbol.for('args')]], [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('current-param'), [Symbol.for('aget'), Symbol.for('params-list'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('current-param')], [Symbol.for('second'), Symbol.for('current-param')]], [Symbol.for('else'), Symbol.for('undefined')]]]]], [Symbol.for('define'), Symbol.for('param-exp'), [Symbol.for('aget'), Symbol.for('params'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('param'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('param-exp')], [Symbol.for('first'), Symbol.for('param-exp')], Symbol.for('param-exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('arg-exp')], [Symbol.for('hash-set!'), Symbol.for('gensym-map'), Symbol.for('param'), Symbol.for('arg-exp')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('param-gensym'), [Symbol.for('gensym'), [Symbol.for('symbol->string'), Symbol.for('param')]]], [Symbol.for('hash-set!'), Symbol.for('gensym-map'), Symbol.for('param'), Symbol.for('param-gensym')], [Symbol.for('push-right!'), Symbol.for('let-bindings'), [Symbol.for('list'), Symbol.for('param-gensym'), Symbol.for('arg-exp')]]]]], [Symbol.for('define'), Symbol.for('let-body'), [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('gensym-map'), Symbol.for('x')], [Symbol.for('hash-ref'), Symbol.for('gensym-map'), Symbol.for('x')]], [Symbol.for('else'), Symbol.for('x')]]], Symbol.for('body')]], [Symbol.for('quasiquote'), [Symbol.for('let*'), [Symbol.for('unquote'), Symbol.for('let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('let-body')]]]], [Symbol.for('should-make-lambda'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('unquote'), Symbol.for('params')], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('unquote-splicing'), Symbol.for('args')]]]], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('result')], 1], [Symbol.for('first'), Symbol.for('result')]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('result')]]]]]]]]]];
/**
 * Create a `(lambda ...)` form for a macro function
 * on the basis of a `(define-macro ...)` expression.
 */
function defineMacroToLambdaForm(exp) {
    const nameAndArgs = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1];
    let name = nameAndArgs[0];
    const args = cdr(nameAndArgs);
    const body = exp.slice(2);
    let expArg = Symbol.for('exp');
    let envArg = Symbol.for('env');
    let macroArgs = [];
    let restArg = undefined;
    if ((() => {
        const x = lastCdr(args);
        return Array.isArray(x) && (x.length === 0);
    })()) {
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
defineMacroToLambdaForm.lispSource = [Symbol.for('define'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('cdr'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('exp-arg'), [Symbol.for('quote'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('env-arg'), [Symbol.for('quote'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('macro-args'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-arg'), Symbol.for('undefined')], [Symbol.for('cond'), [[Symbol.for('list?'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('while'), [Symbol.for('<'), Symbol.for('i'), [Symbol.for('array-list-length'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&rest')]], [Symbol.for('set!'), Symbol.for('rest-arg'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&whole')]], [Symbol.for('set!'), Symbol.for('exp-arg'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&environment')]], [Symbol.for('set!'), Symbol.for('env-arg'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('macro-args'), Symbol.for('arg')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('args')]]], [Symbol.for('when'), Symbol.for('rest-arg'), [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('rest-arg')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), [Symbol.for('apply'), Symbol.for('list*'), [Symbol.for('append'), Symbol.for('macro-args'), [Symbol.for('list'), Symbol.for('rest-arg')]]]]]]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [[Symbol.for('unquote'), Symbol.for('exp-arg')], [Symbol.for('unquote'), Symbol.for('env-arg')]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('quote'), []], [Symbol.for('quasiquote'), [[Symbol.for('define-values'), [Symbol.for('unquote'), Symbol.for('macro-args')], [Symbol.for('rest'), [Symbol.for('unquote'), Symbol.for('exp-arg')]]]]]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
/**
 * Create a macro function on the basis of a
 * `(define-macro ...)` expression.
 */
function defineMacroToFunction(exp, env) {
    const macroFn = defineMacroToLambdaForm(exp);
    return (0, eval_1.eval_)(macroFn, env);
}
exports.defineMacroToFunction = defineMacroToFunction;
defineMacroToFunction.lispSource = [Symbol.for('define'), [Symbol.for('define-macro->function'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('macro-fn'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')]], [Symbol.for('eval_'), Symbol.for('macro-fn'), Symbol.for('env')]];
/**
 * Convert a `(define ... (class ...))` expression to
 * a `(define-class ...)` expression.
 */
function defineToDefineClass(node) {
    if (node instanceof rose_1.Rose) {
        const superclass = node.get(2).get(1);
        const superclassExp = superclass.getValue();
        const superclassList = [Symbol.for('object%'), Symbol.for('object'), Symbol.for('Object')].includes(superclassExp) ? [] : [superclass];
        return (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([Symbol.for('define-class'), node.get(1), (0, rose_1.makeRose)(superclassList), ...node.get(2).drop(2)]));
    }
    else {
        return defineToDefineClass((0, rose_1.makeRose)(node)).getValue();
    }
}
exports.defineToDefineClass = defineToDefineClass;
defineToDefineClass.lispSource = [Symbol.for('define'), [Symbol.for('define->define-class'), Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('define'), Symbol.for('superclass'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('superclass-exp'), [Symbol.for('send'), Symbol.for('superclass'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('superclass-list'), [Symbol.for('if'), [Symbol.for('memq?'), Symbol.for('superclass-exp'), [Symbol.for('quote'), [Symbol.for('object%'), Symbol.for('object'), Symbol.for('Object')]]], [Symbol.for('quote'), []], [Symbol.for('list'), Symbol.for('superclass')]]], [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-class'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('make-rose'), Symbol.for('superclass-list')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('drop'), 2]]]]]]], [Symbol.for('else'), [Symbol.for('send'), [Symbol.for('define->define-class'), [Symbol.for('make-rose'), Symbol.for('node')]], Symbol.for('get-value')]]]];
/**
 * Wrap `f-exp` in a unary function wrapper.
 */
function compileMapMacroHelper(fExp, env) {
    if (typeof fExp === 'symbol') {
        // If `f-exp` is a symbolic expression, then wrap it in a
        // `lambda` expression.
        return [Symbol.for('lambda'), [Symbol.for('x')], [fExp, Symbol.for('x')]];
    }
    else if (((0, util_1.formp)(fExp, lambda_, env) || (0, util_1.formp)(fExp, jsFunction_, env) || (0, util_1.formp)(fExp, jsArrow_, env)) && Array.isArray((Array.isArray(fExp) && (fExp.length >= 3) && (fExp[fExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(fExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = fExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = fExp[fExp.length - 1];
            }
            else {
                result = fExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : fExp[1]) && (((Array.isArray(fExp) && (fExp.length >= 3) && (fExp[fExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(fExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = fExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = fExp[fExp.length - 1];
            }
            else {
                result = fExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : fExp[1]).length === 1)) {
        // If `f-exp` is an anonymous unary function, then there is
        // no need to wrap it.
        return fExp;
    }
    else {
        // Curried function application, i.e., the **A** combinator
        // defined as a curried function. Calling this function with
        // a single argument produces a unary function wrapper that
        // calls a function with a single argument and disregards any
        // additional arguments.
        const AExp = [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]]];
        return [AExp, fExp];
    }
}
compileMapMacroHelper.lispSource = [Symbol.for('define'), [Symbol.for('compile-map-macro-helper'), Symbol.for('f-exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('f-exp')], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('x')], [[Symbol.for('unquote'), Symbol.for('f-exp')], Symbol.for('x')]]]], [[Symbol.for('and'), [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('f-exp'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('f-exp'), Symbol.for('js-function_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('f-exp'), Symbol.for('js-arrow_'), Symbol.for('env')]], [Symbol.for('array?'), [Symbol.for('second'), Symbol.for('f-exp')]], [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('second'), Symbol.for('f-exp')]], 1]], Symbol.for('f-exp')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('A-exp'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]]]]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('A-exp')], [Symbol.for('unquote'), Symbol.for('f-exp')]]]]]];
/**
 * Compile an `(and ...)` expression.
 */
function compileAnd(node, env, options = {}) {
    return compileLogicalExpression(node, env, options, {
        identity: true,
        operator: '&&'
    });
}
compileAnd.lispSource = [Symbol.for('define'), [Symbol.for('compile-and'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-logical-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '&&']]];
/**
 * Compile an `(ann ...)` expression.
 */
function compileAnn(node, env, options = {}) {
    const language = options['language'];
    const e_ = node.get(1);
    if (language === 'TypeScript') {
        const t_ = node.get(2);
        return makeExpressionOrStatement(new estree_1.TSAsExpression(compileExpression(e_, env, options), compileType(t_, env, options)), options);
    }
    else {
        return compileRose(e_, env, options);
    }
}
compileAnn.lispSource = [Symbol.for('define'), [Symbol.for('compile-ann'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('e_'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('define'), Symbol.for('t_'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('TSAsExpression'), [Symbol.for('compile-expression'), Symbol.for('e_'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-type'), Symbol.for('t_'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-rose'), Symbol.for('e_'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(define-type ...)` expression.
 */
function compileDefineType(node, env, options = {}) {
    const language = options['language'];
    if (language === 'TypeScript') {
        let id = compileExpression(node.get(1), env, options);
        let type_ = compileType(node.get(2), env, options);
        return transferAndCompileComments(node, new estree_1.TSTypeAliasDeclaration(id, type_), options);
    }
    else {
        return emptyProgram();
    }
}
compileDefineType.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-type'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('compile-type'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('transfer-and-compile-comments'), Symbol.for('node'), [Symbol.for('new'), Symbol.for('TSTypeAliasDeclaration'), Symbol.for('id'), Symbol.for('type_')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('empty-program')]]]];
/**
 * Compile a type expression.
 */
function compileType(node, env, options = {}) {
    let exp = (node instanceof rose_1.Rose) ? node.getValue() : node;
    return compileTypeExp(exp, env, options);
}
compileType.lispSource = [Symbol.for('define'), [Symbol.for('compile-type'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')], Symbol.for('node')]], [Symbol.for('compile-type-exp'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Helper function for `compile-type`.
 */
function compileTypeExp(exp, env, options = {}) {
    if (typeof exp === 'symbol') {
        if (exp === Symbol.for('Any')) {
            return new estree_1.TSAnyKeyword();
        }
        else if (exp === Symbol.for('Void')) {
            return new estree_1.TSVoidKeyword();
        }
        else if (exp === Symbol.for('Undefined')) {
            return new estree_1.TSUndefinedKeyword();
        }
        else if (exp === Symbol.for('Boolean')) {
            return new estree_1.TSBooleanKeyword();
        }
        else if (exp === Symbol.for('True')) {
            return new estree_1.TSLiteralType(new estree_1.Literal(true));
        }
        else if (exp === Symbol.for('False')) {
            return new estree_1.TSLiteralType(new estree_1.Literal(false));
        }
        else if (exp === Symbol.for('Number')) {
            return new estree_1.TSNumberKeyword();
        }
        else if (exp === Symbol.for('Integer')) {
            return new estree_1.TSNumberKeyword();
        }
        else if (exp === Symbol.for('Natural')) {
            return new estree_1.TSNumberKeyword();
        }
        else if (exp === Symbol.for('Real')) {
            return new estree_1.TSNumberKeyword();
        }
        else if (exp === Symbol.for('String')) {
            return new estree_1.TSStringKeyword();
        }
        else {
            return new estree_1.TSTypeReference(new estree_1.Identifier(exp.description));
        }
    }
    else if ((0, util_1.taggedListP)(exp, Symbol.for('List'))) {
        return new estree_1.TSTupleType(exp.slice(1).map(function (x) {
            return compileTypeExp(x, env, options);
        }));
    }
    else if ((0, util_1.taggedListP)(exp, Symbol.for('Listof'))) {
        return new estree_1.TSArrayType(compileTypeExp((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1], env, options));
    }
    else if ((0, util_1.taggedListP)(exp, Symbol.for('Pairof'))) {
        return compileTypeExp([Symbol.for('Listof'), [Symbol.for('U'), (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(exp);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1], Symbol.for('Symbol')]], env, options);
    }
    else if ((0, util_1.taggedListP)(exp, Symbol.for('U'))) {
        return new estree_1.TSUnionType(exp.slice(1).map(function (x) {
            return compileTypeExp(x, env, options);
        }));
    }
    else if ((0, util_1.taggedListP)(exp, Symbol.for('->')) || (0, util_1.taggedListP)(exp, Symbol.for('->*'))) {
        const mandatoryParams = (0, util_1.taggedListP)(exp, Symbol.for('->*')) ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1]) : exp.slice(0, -1).slice(1);
        const optionalParams = ((0, util_1.taggedListP)(exp, Symbol.for('->*')) && (exp.length > 3)) ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 2;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[2]) : [];
        const returnValue = exp[exp.length - 1];
        let i = 0;
        function compileParam(param, optional) {
            const varName = numberToLetter(i);
            i++;
            return new estree_1.Identifier(varName, optional).setType(compileTypeExp(param, env, options));
        }
        compileParam.lispSource = [Symbol.for('define'), [Symbol.for('compile-param'), Symbol.for('param'), Symbol.for('optional')], [Symbol.for('define'), Symbol.for('var-name'), [Symbol.for('number->letter'), Symbol.for('i')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('~>'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('var-name'), Symbol.for('optional')], [Symbol.for('send'), Symbol.for('set-type'), [Symbol.for('compile-type-exp'), Symbol.for('param'), Symbol.for('env'), Symbol.for('options')]]]];
        return new estree_1.TSFunctionType([...mandatoryParams.map(function (param) {
                return compileParam(param, false);
            }), ...optionalParams.map(function (param) {
                return compileParam(param, true);
            })], compileTypeExp(returnValue, env, options));
    }
    else if (Array.isArray(exp) && (exp.length > 0)) {
        let name = new estree_1.Identifier(exp[0].description);
        let params = exp.slice(1).map(function (x) {
            return x.description;
        });
        if (params.length > 0) {
            return new estree_1.TSTypeReference(name, new estree_1.TSTypeParameterInstantiation(params));
        }
        else {
            return new estree_1.TSTypeReference(name);
        }
    }
    else {
        return new estree_1.TSAnyKeyword();
    }
}
compileTypeExp.lispSource = [Symbol.for('define'), [Symbol.for('compile-type-exp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('new'), Symbol.for('TSAnyKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Void')]], [Symbol.for('new'), Symbol.for('TSVoidKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Undefined')]], [Symbol.for('new'), Symbol.for('TSUndefinedKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Boolean')]], [Symbol.for('new'), Symbol.for('TSBooleanKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('True')]], [Symbol.for('new'), Symbol.for('TSLiteralType'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('#t')]]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('False')]], [Symbol.for('new'), Symbol.for('TSLiteralType'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('#f')]]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Number')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Integer')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Natural')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Real')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('String')]], [Symbol.for('new'), Symbol.for('TSStringKeyword')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('TSTypeReference'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('symbol->string'), Symbol.for('exp')]]]]]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('List')]], [Symbol.for('new'), Symbol.for('TSTupleType'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-type-exp'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('rest'), Symbol.for('exp')]]]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Listof')]], [Symbol.for('new'), Symbol.for('TSArrayType'), [Symbol.for('compile-type-exp'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Pairof')]], [Symbol.for('compile-type-exp'), [Symbol.for('quasiquote'), [Symbol.for('Listof'), [Symbol.for('U'), [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('exp')]], Symbol.for('Symbol')]]], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('U')]], [Symbol.for('new'), Symbol.for('TSUnionType'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-type-exp'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('rest'), Symbol.for('exp')]]]], [[Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->*')]]], [Symbol.for('define'), Symbol.for('mandatory-params'), [Symbol.for('if'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->*')]], [Symbol.for('second'), Symbol.for('exp')], [Symbol.for('drop'), [Symbol.for('drop-right'), Symbol.for('exp'), 1], 1]]], [Symbol.for('define'), Symbol.for('optional-params'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->*')]], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3]], [Symbol.for('third'), Symbol.for('exp')], [Symbol.for('quote'), []]]], [Symbol.for('define'), Symbol.for('return-value'), [Symbol.for('array-list-last'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('define'), [Symbol.for('compile-param'), Symbol.for('param'), Symbol.for('optional')], [Symbol.for('define'), Symbol.for('var-name'), [Symbol.for('number->letter'), Symbol.for('i')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('~>'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('var-name'), Symbol.for('optional')], [Symbol.for('send'), Symbol.for('set-type'), [Symbol.for('compile-type-exp'), Symbol.for('param'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('new'), Symbol.for('TSFunctionType'), [Symbol.for('append'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('param')], [Symbol.for('compile-param'), Symbol.for('param'), Symbol.for('#f')]], Symbol.for('mandatory-params')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('param')], [Symbol.for('compile-param'), Symbol.for('param'), Symbol.for('#t')]], Symbol.for('optional-params')]], [Symbol.for('compile-type-exp'), Symbol.for('return-value'), Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 0]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('symbol->string'), [Symbol.for('first'), Symbol.for('exp')]]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('map'), Symbol.for('symbol->string'), [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('params')], 0], [Symbol.for('new'), Symbol.for('TSTypeReference'), Symbol.for('name'), [Symbol.for('new'), Symbol.for('TSTypeParameterInstantiation'), Symbol.for('params')]]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('TSTypeReference'), Symbol.for('name')]]]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]];
/**
 * Convert a number to a letter.
 * `0` corresponds to `a`, `1` to `b`, etc.
 */
function numberToLetter(n) {
    return String.fromCharCode('a'.charCodeAt(0) + n);
}
numberToLetter.lispSource = [Symbol.for('define'), [Symbol.for('number->letter'), Symbol.for('n')], [Symbol.for('~>'), [Symbol.for('send'), 'a', Symbol.for('charCodeAt'), 0], [Symbol.for('+'), Symbol.for('_'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('String'), Symbol.for('fromCharCode'), Symbol.for('_')]]];
/**
 * "NO-OP" operation.
 */
function nop_(exp, env) {
    return undefined;
}
exports.nop = nop_;
exports.nop_ = nop_;
nop_.lispSource = [Symbol.for('define'), [Symbol.for('nop_'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('undefined')];
/**
 * Compile a `(+ ...)` expression.
 */
function compileAdd(node, env, options = {}) {
    return compileBinaryExpression(node, env, options, {
        identity: 0,
        operator: '+'
    });
}
compileAdd.lispSource = [Symbol.for('define'), [Symbol.for('compile-add'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 0, 'operator', '+']]];
/**
 * Compile an `(apply ...)` expression.
 */
function compileApply(node, env, options = {}) {
    let exp = node.getValue();
    const f = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1];
    const isMakeObject = env.get(f) === new_;
    const callee = isMakeObject ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 2;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[2]) : f;
    const args = isMakeObject ? exp.slice(3) : exp.slice(2);
    const calleeCompiled = compileExpression((0, rose_1.makeRose)(callee), env, options);
    const argsCompiled = [];
    if (args.length > 0) {
        let regularArgs = args.slice(0, -1);
        let restArg = args[args.length - 1];
        for (let arg of regularArgs) {
            argsCompiled.push(compileExpression((0, rose_1.makeRose)(arg), env, options));
        }
        argsCompiled.push(new estree_1.RestElement(compileExpression((0, rose_1.makeRose)(restArg), env, options)));
    }
    if (isMakeObject) {
        return makeExpressionOrStatement(new estree_1.NewExpression(calleeCompiled, argsCompiled), options);
    }
    else {
        return makeExpressionOrStatement(new estree_1.CallExpression(calleeCompiled, argsCompiled), options);
    }
}
compileApply.lispSource = [Symbol.for('define'), [Symbol.for('compile-apply'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('is-make-object'), [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('f')], Symbol.for('new_')]], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('if'), Symbol.for('is-make-object'), [Symbol.for('third'), Symbol.for('exp')], Symbol.for('f')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('if'), Symbol.for('is-make-object'), [Symbol.for('drop'), Symbol.for('exp'), 3], [Symbol.for('drop'), Symbol.for('exp'), 2]]], [Symbol.for('define'), Symbol.for('callee-compiled'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('callee')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('args-compiled'), [Symbol.for('quote'), []]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('args')], 0], [Symbol.for('define'), Symbol.for('regular-args'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('define'), Symbol.for('rest-arg'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('regular-args')]], [Symbol.for('push-right!'), Symbol.for('args-compiled'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('arg')], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('push-right!'), Symbol.for('args-compiled'), [Symbol.for('new'), Symbol.for('RestElement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('rest-arg')], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('cond'), [Symbol.for('is-make-object'), [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('NewExpression'), Symbol.for('callee-compiled'), Symbol.for('args-compiled')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), Symbol.for('callee-compiled'), Symbol.for('args-compiled')], Symbol.for('options')]]]];
/**
 * Compile an `(array-ref ...)` expression.
 */
function compileArrayRef(node, env, options = {}) {
    const variable = node.get(1);
    const variableCompiled = compileExpression(variable, env, options);
    const indices = node.drop(2);
    const indicesCompiled = indices.map(function (x) {
        return compileExpression(x, env, options);
    });
    let result = indicesCompiled.reduce(function (acc, x) {
        return new estree_1.MemberExpression(acc, x, true);
    }, variableCompiled);
    return makeExpressionOrStatement(result, options);
}
compileArrayRef.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-ref'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('variable'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('variable-compiled'), [Symbol.for('compile-expression'), Symbol.for('variable'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('indices-compiled'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('new'), Symbol.for('MemberExpression'), Symbol.for('acc'), Symbol.for('x'), Symbol.for('#t')]], Symbol.for('variable-compiled'), Symbol.for('indices-compiled')]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];
/**
 * Compile an `(array-set! ...)` expression.
 */
function compileArraySet(node, env, options = {}) {
    let exp = node.getValue();
    const arr = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1];
    const indices = exp.slice(2).slice(0, -1);
    let value = exp[exp.length - 1];
    return compileRose((0, rose_1.insertSexpIntoRose)([Symbol.for('set!'), [Symbol.for('array-ref'), arr, ...indices], value], node), env, options);
}
compileArraySet.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-set'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('arr'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), [Symbol.for('drop'), Symbol.for('exp'), 2], 1]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1]]], [Symbol.for('compile-rose'), [Symbol.for('insert-sexp-into-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('array-ref'), [Symbol.for('unquote'), Symbol.for('arr')], [Symbol.for('unquote-splicing'), Symbol.for('indices')]], [Symbol.for('unquote'), Symbol.for('value')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile an `(object-ref ...)` expression.
 */
function compileObjectRef(node, env, options = {}) {
    return compileArrayRef(node, env, options);
}
compileObjectRef.lispSource = [Symbol.for('define'), [Symbol.for('compile-object-ref'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-array-ref'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile an `(object-set! ...)` expression.
 */
function compileObjectSet(node, env, options = {}) {
    return compileArraySet(node, env, options);
}
compileObjectSet.lispSource = [Symbol.for('define'), [Symbol.for('compile-object-set'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-array-set'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile an atomic expression, such as `foo`.
 */
function compileAtom(node, env, options = {}) {
    return makeExpressionOrStatement(new estree_1.Literal(node.getValue()), options);
}
compileAtom.lispSource = [Symbol.for('define'), [Symbol.for('compile-atom'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], Symbol.for('options')]];
/**
 * Compile a `(: ...)` expression.
 */
function compileColon(node, env, options = {}) {
    let bindings = options['bindings'];
    const sym = node.get(1);
    const symExp = sym.getValue();
    let type_ = node.get(2);
    const typeExp = type_.getValue();
    let bindingType = 'variable';
    if (bindings) {
        if (bindings.has(symExp)) {
            bindingType = bindings.getType(symExp);
        }
        // FIXME: This is a kludge. We need a better way
        // of storing types---either a separate environment,
        // or a typed environment, perhaps.
        bindings.setLocal(symExp, typeExp, bindingType);
    }
    return compileNop(node, env, options);
}
compileColon.lispSource = [Symbol.for('define'), [Symbol.for('compile-colon'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('sym-exp'), [Symbol.for('send'), Symbol.for('sym'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('type-exp'), [Symbol.for('send'), Symbol.for('type_'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('binding-type'), 'variable'], [Symbol.for('when'), Symbol.for('bindings'), [Symbol.for('when'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym-exp')], [Symbol.for('set!'), Symbol.for('binding-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('sym-exp')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym-exp'), Symbol.for('type-exp'), Symbol.for('binding-type')]], [Symbol.for('compile-nop'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile a `(cond ...)` expression.
 */
function compileCond(node, env, options = {}) {
    const expressionType = options['expressionType'];
    const condClauses = node.drop(1);
    if (condClauses.length === 0) {
        return makeExpressionOrStatement(new estree_1.Literal(false), options);
    }
    else {
        function reducingF(condClause, compiledExp) {
            const condition = condClause.get(0);
            const thenClauses = (0, rose_1.beginWrapRoseSmart)(condClause.drop(1));
            if ((expressionType === 'statement') || (expressionType === 'return')) {
                return new estree_1.IfStatement(compileExpression(condition, env, options), transferAndCompileComments(condClause, unwrapBlockStatement(wrapInBlockStatementSmart(compileStatementOrReturnStatement(thenClauses, env, options))), options), compiledExp);
            }
            else {
                return new estree_1.ConditionalExpression(compileExpression(condition, env, options), transferAndCompileComments(condClause, compileExpression(thenClauses, env, options), options), compiledExp);
            }
        }
        reducingF.lispSource = [Symbol.for('define'), [Symbol.for('reducing-f'), Symbol.for('cond-clause'), Symbol.for('compiled-exp')], [Symbol.for('define'), Symbol.for('condition'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('then-clauses'), [Symbol.for('begin-wrap-rose-smart'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('drop'), 1]]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('new'), Symbol.for('IfStatement'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('unwrap-block-statement'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')]]], Symbol.for('options')], Symbol.for('compiled-exp')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ConditionalExpression'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('compile-expression'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')], Symbol.for('options')], Symbol.for('compiled-exp')]]]];
        if (condClauses[condClauses.length - 1].get(0).getValue() === Symbol.for('else')) {
            let finalClause = ((expressionType === 'statement') || (expressionType === 'return')) ? transferAndCompileComments(condClauses[condClauses.length - 1], wrapInBlockStatementSmart(compileStatementOrReturnStatement((0, rose_1.beginWrapRoseSmart1)(condClauses[condClauses.length - 1].drop(1)), env, options)), options) : transferAndCompileComments(condClauses[condClauses.length - 1], compileExpression((0, rose_1.beginWrapRose)(condClauses[condClauses.length - 1].drop(1)), env, options), options);
            if (!((expressionType !== 'statement') || (0, estree_1.estreeTypeP)(finalClause, 'BlockStatement'))) {
                finalClause = makeBlockStatement([finalClause]);
            }
            return condClauses.slice(0, -1).reduceRight(function (acc, x) {
                return reducingF(x, acc);
            }, finalClause);
        }
        else {
            return condClauses.reduceRight(function (acc, x) {
                return reducingF(x, acc);
            }, ((expressionType === 'statement') || (expressionType === 'return')) ? null : new estree_1.Identifier('undefined'));
        }
    }
}
compileCond.lispSource = [Symbol.for('define'), [Symbol.for('compile-cond'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('cond-clauses'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('cond-clauses')], 0], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('#f')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), [Symbol.for('reducing-f'), Symbol.for('cond-clause'), Symbol.for('compiled-exp')], [Symbol.for('define'), Symbol.for('condition'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('then-clauses'), [Symbol.for('begin-wrap-rose-smart'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('drop'), 1]]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('new'), Symbol.for('IfStatement'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('unwrap-block-statement'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')]]], Symbol.for('options')], Symbol.for('compiled-exp')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ConditionalExpression'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('compile-expression'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')], Symbol.for('options')], Symbol.for('compiled-exp')]]]], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('~>'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]], [Symbol.for('quote'), Symbol.for('else')]], [Symbol.for('define'), Symbol.for('final-clause'), [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('transfer-and-compile-comments'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('begin-wrap-rose-smart-1'), [Symbol.for('send'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], Symbol.for('drop'), 1]], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('transfer-and-compile-comments'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], [Symbol.for('compile-expression'), [Symbol.for('begin-wrap-rose'), [Symbol.for('send'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], Symbol.for('drop'), 1]], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement']], [Symbol.for('estree-type?'), Symbol.for('final-clause'), 'BlockStatement']], [Symbol.for('set!'), Symbol.for('final-clause'), [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('final-clause')]]]], [Symbol.for('foldr'), Symbol.for('reducing-f'), Symbol.for('final-clause'), [Symbol.for('drop-right'), Symbol.for('cond-clauses'), 1]]], [Symbol.for('else'), [Symbol.for('foldr'), Symbol.for('reducing-f'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], Symbol.for('js/null'), [Symbol.for('new'), Symbol.for('Identifier'), 'undefined']], Symbol.for('cond-clauses')]]]]]];
/**
 * Compile a `(define ...)` expression.
 */
function compileDefine(node, env, options = {}) {
    let bindings = options['bindings'];
    const language = options['language'];
    const inlineLispSourceOption = options['inlineLispSources'];
    let exp = node.getValue();
    let type_ = Symbol.for('Any');
    if (Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1])) {
        // Function definition.
        const sym = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1])[0];
        const shouldCurry = Array.isArray(sym);
        const nameSym = shouldCurry ? flatten(sym)[0] : sym;
        const functionName = (0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(nameSym), env, makeExpressionOptions(options)), options);
        const lambdaExp = defineToLambda(node);
        const returnType = (node.get(2).getValue() === Symbol.for(':')) ? node.get(3).getValue() : Symbol.for('Any');
        let params = lambdaExp.get(1).getValue();
        const declaredType = bindings.get(sym);
        if ((declaredType === undefined) || (declaredType === true) || (declaredType === Symbol.for('Any'))) {
            type_ = [Symbol.for('->'), ...((typeof params === 'symbol') ? [[Symbol.for('Listof'), Symbol.for('Any')]] : ((Array.isArray(params) && (params.length >= 3) && (params[params.length - 2] === Symbol.for('.')) && !(() => {
                    const x = lastCdr(params);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? [...makeList(params.length - 2, Symbol.for('Any')), [Symbol.for('Listof'), Symbol.for('Any')]] : makeList(params.length, Symbol.for('Any')))), returnType];
            bindings.setLocal((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1], type_, 'procedure');
        }
        else {
            type_ = declaredType;
        }
        const compiledType = compileTypeExp(type_, env, options);
        let result;
        if (shouldCurry) {
            result = compileDefine((0, rose_1.makeRose)([Symbol.for('define'), nameSym, lambdaExp], node), env, options);
        }
        else {
            const returnType = ((compiledType instanceof estree_1.TSFunctionType) && (compiledType.returnType instanceof estree_1.TSVoidKeyword)) ? 'void' : undefined;
            result = compileJsFunction(lambdaExp, env, makeExpressionOptions(options), {
                functionName: functionName,
                returnType: returnType
            });
            if (compiledType instanceof estree_1.TSFunctionType) {
                const _end = result.params.length;
                for (let i = 0; i < _end; i++) {
                    let param = result.params[i];
                    const typeParam = compiledType.params[i];
                    const typeParamAnnotation = typeParam ? typeParam.typeAnnotation : new estree_1.TSAnyKeyword();
                    if (!param.hasType()) {
                        param.setType(typeParamAnnotation);
                    }
                }
                result.returnType = compiledType.returnType;
            }
        }
        if (inlineLispSourceOption) {
            const lispCodeExp = compileSexp([Symbol.for('set-field!'), Symbol.for('lispSource'), Symbol.for(functionName), [Symbol.for('quote'), exp]], env, options);
            return new estree_1.Program([result, lispCodeExp]);
        }
        else {
            return result;
        }
    }
    else if (exp.length === 2) {
        // Uninitialized variable.
        bindings.setLocal((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1], true, 'variable');
        return new estree_1.VariableDeclaration([new estree_1.VariableDeclarator(compileExpression(node.get(1), env, options))], 'let');
    }
    else if ((0, util_1.formp)((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 2;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[2], jsAsync_, env) && (0, util_1.formp)((() => {
        const lst = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 2;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[2];
        if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(lst);
            return Array.isArray(x) && (x.length === 0);
        })()) {
            let i = 1;
            let result = lst;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = lst[lst.length - 1];
                }
                else {
                    result = lst.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        }
        else {
            return lst[1];
        }
    })(), lambda_, env)) {
        // Asynchronous function definition.
        const lambdaNode = node.get(2).get(1);
        let name = node.get(1);
        const args = lambdaNode.get(1).drop(0);
        const daForm = (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([Symbol.for('define/async'), [name, ...args], ...lambdaNode.drop(2)]));
        return compileDefineAsync(daForm, env, options);
    }
    else if ((0, util_1.formp)((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 2;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[2], class_, env)) {
        // Class definition.
        return compileDefineClass((0, rose_1.makeRose)(defineToDefineClass(exp), node), env, options);
    }
    else {
        // Initialized variable.
        const sym = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1];
        const symCompiled = compileSymbol(node.get(1), env, options);
        if (bindings.has(sym)) {
            type_ = bindings.get(sym);
        }
        else {
            bindings.setLocal((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1], type_, 'variable');
        }
        return new estree_1.VariableDeclaration([new estree_1.VariableDeclarator(symCompiled.setType(compileType(type_, env, options)), compileExpression(node.get(2), env, options))], 'let');
    }
}
compileDefine.lispSource = [Symbol.for('define'), [Symbol.for('compile-define'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('inline-lisp-source-option'), [Symbol.for('oget'), Symbol.for('options'), 'inlineLispSources']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('cond'), [[Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('should-curry'), [Symbol.for('array?'), Symbol.for('sym')]], [Symbol.for('define'), Symbol.for('name-sym'), [Symbol.for('if'), Symbol.for('should-curry'), [Symbol.for('first'), [Symbol.for('flatten'), Symbol.for('sym')]], Symbol.for('sym')]], [Symbol.for('define'), Symbol.for('function-name'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('name-sym')], Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('lambda-exp'), [Symbol.for('define->lambda'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], [Symbol.for('send'), Symbol.for('get-value')]], [Symbol.for('quote'), Symbol.for(':')]], [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 3], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('else'), [Symbol.for('quote'), Symbol.for('Any')]]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('lambda-exp'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('declared-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get'), Symbol.for('sym')]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('declared-type'), Symbol.for('undefined')], [Symbol.for('eq?'), Symbol.for('declared-type'), Symbol.for('#t')], [Symbol.for('eq?'), Symbol.for('declared-type'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('quasiquote'), [Symbol.for('->'), [Symbol.for('unquote-splicing'), [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params')], [Symbol.for('list'), [Symbol.for('quote'), [Symbol.for('Listof'), Symbol.for('Any')]]]], [[Symbol.for('dotted-list?'), Symbol.for('params')], [Symbol.for('append'), [Symbol.for('make-list'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('params')], 2], [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('list'), [Symbol.for('quote'), [Symbol.for('Listof'), Symbol.for('Any')]]]]], [Symbol.for('else'), [Symbol.for('make-list'), [Symbol.for('array-list-length'), Symbol.for('params')], [Symbol.for('quote'), Symbol.for('Any')]]]]], [Symbol.for('unquote'), Symbol.for('return-type')]]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('type_'), 'procedure']], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('type_'), Symbol.for('declared-type')]]], [Symbol.for('define'), Symbol.for('compiled-type'), [Symbol.for('compile-type-exp'), Symbol.for('type_'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [Symbol.for('should-curry'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-define'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('name-sym')], [Symbol.for('unquote'), Symbol.for('lambda-exp')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('is-a?'), Symbol.for('compiled-type'), Symbol.for('TSFunctionType')], [Symbol.for('is-a?'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('compiled-type')], Symbol.for('TSVoidKeyword')]], 'void', Symbol.for('undefined')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-js-function'), Symbol.for('lambda-exp'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')], [Symbol.for('js-obj'), 'functionName', Symbol.for('function-name'), 'returnType', Symbol.for('return-type')]]], [Symbol.for('when'), [Symbol.for('is-a?'), Symbol.for('compiled-type'), Symbol.for('TSFunctionType')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('result')]]]]], [Symbol.for('define'), Symbol.for('param'), [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('result')], Symbol.for('i')]], [Symbol.for('define'), Symbol.for('type-param'), [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('compiled-type')], Symbol.for('i')]], [Symbol.for('define'), Symbol.for('type-param-annotation'), [Symbol.for('if'), Symbol.for('type-param'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('type-param')], [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]], [Symbol.for('unless'), [Symbol.for('send'), Symbol.for('param'), Symbol.for('has-type')], [Symbol.for('send'), Symbol.for('param'), Symbol.for('set-type'), Symbol.for('type-param-annotation')]]], [Symbol.for('set-field!'), Symbol.for('returnType'), Symbol.for('result'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('compiledType')]]]]], [Symbol.for('cond'), [Symbol.for('inline-lisp-source-option'), [Symbol.for('define'), Symbol.for('lisp-code-exp'), [Symbol.for('compile-sexp'), [Symbol.for('quasiquote'), [Symbol.for('set-field!'), Symbol.for('lispSource'), [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('function-name')]], [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Program'), [Symbol.for('list'), Symbol.for('result'), Symbol.for('lisp-code-exp')]]], [Symbol.for('else'), Symbol.for('result')]]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('#t'), 'variable'], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]], 'let']], [[Symbol.for('and'), [Symbol.for('form?'), [Symbol.for('third'), Symbol.for('exp')], Symbol.for('js-async_'), Symbol.for('env')], [Symbol.for('form?'), [Symbol.for('second'), [Symbol.for('third'), Symbol.for('exp')]], Symbol.for('lambda_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('lambda-node'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('lambda-node'), Symbol.for('get'), 1], Symbol.for('drop'), 0]], [Symbol.for('define'), Symbol.for('da-form'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define/async'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('unquote-splicing'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('lambda-node'), Symbol.for('drop'), 2]]]]]]], [Symbol.for('compile-define-async'), Symbol.for('da-form'), Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('form?'), [Symbol.for('third'), Symbol.for('exp')], Symbol.for('class_'), Symbol.for('env')], [Symbol.for('compile-define-class'), [Symbol.for('make-rose'), [Symbol.for('define->define-class'), Symbol.for('exp')], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('sym-compiled'), [Symbol.for('compile-symbol'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get'), Symbol.for('sym')]]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('type_'), 'variable']]], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('~>'), Symbol.for('sym-compiled'), [Symbol.for('send'), Symbol.for('set-type'), [Symbol.for('compile-type'), Symbol.for('type_'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]]], 'let']]]];
/**
 * Compile a `(define/async ...)` expression.
 */
function compileDefineAsync(node, env, options = {}) {
    const inlineLispSourceOption = options['inlineLispSources'];
    let result = compileDefine(node, env, options);
    const resultF = inlineLispSourceOption ? result.body[0] : result;
    if ((0, estree_1.estreeTypeP)(resultF, 'FunctionDeclaration')) {
        resultF.async = true;
    }
    const returnType = resultF.returnType;
    resultF.returnType = new estree_1.TSTypeReference(new estree_1.Identifier('Promise'), new estree_1.TSTypeParameterInstantiation([new estree_1.TSAnyKeyword()]));
    return result;
}
compileDefineAsync.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-async'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inline-lisp-source-option'), [Symbol.for('oget'), Symbol.for('options'), 'inlineLispSources']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-define'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result-f'), [Symbol.for('if'), Symbol.for('inline-lisp-source-option'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('result')]], Symbol.for('result')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('result-f'), 'FunctionDeclaration'], [Symbol.for('set-field!'), Symbol.for('async'), Symbol.for('result-f'), Symbol.for('#t')]], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('result-f')]], [Symbol.for('set-field!'), Symbol.for('returnType'), Symbol.for('result-f'), [Symbol.for('new'), Symbol.for('TSTypeReference'), [Symbol.for('new'), Symbol.for('Identifier'), 'Promise'], [Symbol.for('new'), Symbol.for('TSTypeParameterInstantiation'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]]], Symbol.for('result')];
/**
 * Compile a `(define/generator ...)` expression.
 */
function compileDefineGenerator(node, env, options = {}) {
    let result = compileDefine(node, env, options);
    result.generator = true;
    return result;
}
compileDefineGenerator.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-generator'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-define'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('generator'), Symbol.for('result'), Symbol.for('#t')], Symbol.for('result')];
/**
 * Compile a `(/ ...)` expression.
 */
function compileDiv(node, env, options = {}) {
    let exp = node.getValue();
    if (exp.length === 2) {
        return compileDiv((0, rose_1.makeRose)([Symbol.for('/'), 1, node.get(1)], node), env, options);
    }
    else {
        return compileBinaryExpression(node, env, options, {
            identity: 1,
            operator: '/'
        });
    }
}
compileDiv.lispSource = [Symbol.for('define'), [Symbol.for('compile-div'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('compile-div'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('/'), 1, [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 1, 'operator', '/']]]]];
/**
 * Compile a `(send ...)` expression.
 */
function compileSend(node, env, options = {}) {
    const obj = node.get(1);
    const method = node.get(2);
    const args = node.drop(3);
    return makeExpressionOrStatement(new estree_1.CallExpression(new estree_1.MemberExpression((typeof obj.getValue() === 'symbol') ? compileSymbol(obj, env, makeExpressionOptions(options)) : compileExpression(obj, env, options), compileSymbol(method, env, options), false), args.map(function (x) {
        return compileExpression(x, env, options);
    })), options);
}
compileSend.lispSource = [Symbol.for('define'), [Symbol.for('compile-send'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), [Symbol.for('new'), Symbol.for('MemberExpression'), [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('obj'), Symbol.for('get-value')]], [Symbol.for('compile-symbol'), Symbol.for('obj'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('compile-symbol'), Symbol.for('method'), Symbol.for('env'), Symbol.for('options')], Symbol.for('#f')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('args')]], Symbol.for('options')]];
/**
 * Compile a `(send/apply ...)` expression.
 */
function compileSendApply(node, env, options = {}) {
    const obj = node.get(1);
    const method = node.get(2);
    const args = node.drop(3);
    return makeExpressionOrStatement(compileExpression((0, rose_1.makeRose)([Symbol.for('apply'), [Symbol.for('get-field'), method, obj], ...args], node), env, options), options);
}
compileSendApply.lispSource = [Symbol.for('define'), [Symbol.for('compile-send-apply'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('apply'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('method')], [Symbol.for('unquote'), Symbol.for('obj')]], [Symbol.for('unquote-splicing'), Symbol.for('args')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]];
/**
 * Compile a `(js/=== ...)` expression.
 */
function compileJsIsStrictlyEqual(node, env, options = {}) {
    return compileBinaryExpression(node, env, options, {
        identity: true,
        operator: '==='
    });
}
compileJsIsStrictlyEqual.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-is-strictly-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '===']]];
/**
 * Compile a `(js/== ...)` expression.
 */
function compileJsIsLooselyEqual(node, env, options = {}) {
    return compileBinaryExpression(node, env, options, {
        identity: true,
        operator: '=='
    });
}
compileJsIsLooselyEqual.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-is-loosely-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '==']]];
/**
 * Compiler macro for `(foldl ...)` expressions.
 */
function compileFoldlMacro(exp, env) {
    const [f, v, lst] = exp.slice(1);
    // `foldl()` and `.reduce()` invoke the reducing function with
    // opposite argument order, and `.reduce()` passes additional
    // arguments to it. We therefore wrap it in a binary function
    // wrapper that reverses the order of the two first arguments
    // and disregards the other arguments.
    return [Symbol.for('send'), lst, Symbol.for('reduce'), flipFunctionExpression(f, env), v];
}
compileFoldlMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-foldl-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('reduce'), [Symbol.for('unquote'), [Symbol.for('flip-function-expression'), Symbol.for('f'), Symbol.for('env')]], [Symbol.for('unquote'), Symbol.for('v')]]]];
compileFoldlMacro.lispMacro = true;
/**
 * Compiler macro for `(foldr ...)` expressions.
 */
function compileFoldrMacro(exp, env) {
    const [f, v, lst] = exp.slice(1);
    // Like `foldl`, but invokes the `reduceRight` method instead.
    return [Symbol.for('send'), lst, Symbol.for('reduceRight'), flipFunctionExpression(f, env), v];
}
compileFoldrMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-foldr-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('reduceRight'), [Symbol.for('unquote'), [Symbol.for('flip-function-expression'), Symbol.for('f'), Symbol.for('env')]], [Symbol.for('unquote'), Symbol.for('v')]]]];
compileFoldrMacro.lispMacro = true;
/**
 * Given an expression that designates a binary function,
 * produce a new expression that flips the argument order.
 * Helper function for `compile-foldl-macro` and
 * `compile-foldr-macro`.
 */
function flipFunctionExpression(exp, env) {
    if (typeof exp === 'symbol') {
        // Function expression is a symbol:
        // wrap it in a `lambda` form that reverses
        // the order of application.
        return [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('x')], [exp, Symbol.for('x'), Symbol.for('acc')]];
    }
    else if ((0, util_1.formp)(exp, lambda_, env) && (((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1]).length >= 2)) {
        // Function expression is a `lambda` form:
        // swap the two first arguments.
        return [Symbol.for('lambda'), [(() => {
                    const lst = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                        const x = lastCdr(exp);
                        return Array.isArray(x) && (x.length === 0);
                    })()) ? (() => {
                        let i = 1;
                        let result = exp;
                        while (i > 0) {
                            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                result = exp[exp.length - 1];
                            }
                            else {
                                result = exp.slice(1);
                            }
                            i--;
                        }
                        if (Array.isArray(result)) {
                            result = result[0];
                        }
                        return result;
                    })() : exp[1];
                    if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && (() => {
                        const x = lastCdr(lst);
                        return Array.isArray(x) && (x.length === 0);
                    })()) {
                        let i = 1;
                        let result = lst;
                        while (i > 0) {
                            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                result = lst[lst.length - 1];
                            }
                            else {
                                result = lst.slice(1);
                            }
                            i--;
                        }
                        if (Array.isArray(result)) {
                            result = result[0];
                        }
                        return result;
                    }
                    else {
                        return lst[1];
                    }
                })(), ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(exp);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1])[0], ...((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(exp);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1]).slice(2)], ...exp.slice(2)];
    }
    else {
        // Function expression is a function call:
        // pass it to a function that will
        // swap the arguments.
        // Curried **C** combinator, also known as `flip`.
        // Only the first argument is curried here, but
        // otherwise, this behaves similarly to Haskell's
        // `flip`.
        const CExp = [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')]]];
        return [CExp, exp];
    }
}
flipFunctionExpression.lispSource = [Symbol.for('define'), [Symbol.for('flip-function-expression'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('x')], [[Symbol.for('unquote'), Symbol.for('exp')], Symbol.for('x'), Symbol.for('acc')]]]], [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('>='), [Symbol.for('array-list-length'), [Symbol.for('second'), Symbol.for('exp')]], 2]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [[Symbol.for('unquote'), [Symbol.for('second'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('unquote'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), [Symbol.for('second'), Symbol.for('exp')], 2]]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('exp'), 2]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('C-exp'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')]]]]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('C-exp')], [Symbol.for('unquote'), Symbol.for('exp')]]]]]];
/**
 * Compile a `(funcall ...)` expression.
 */
function compileFuncall(node, env, options = {}) {
    return compileFunctionCall((0, rose_1.sliceRose)(node, 1), env, options);
}
compileFuncall.lispSource = [Symbol.for('define'), [Symbol.for('compile-funcall'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-function-call'), [Symbol.for('slice-rose'), Symbol.for('node'), 1], Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile a function call.
 */
function compileFunctionCall(node, env, options = {}) {
    let bindings = options['bindings'];
    const referencedSymbols = options['referencedSymbols'];
    const currentModule = options['currentModule'];
    const compilationMappingEnvironment = options['compilationMappingEnvironment'];
    const callee = node.get(0);
    const op = callee.getValue();
    const symbolicOp = typeof op === 'symbol';
    const shouldInlineOp = (symbolicOp && shouldInlineP(op, env, options) &&
        // Do not inline the operator if a
        // compilation macro is defined for it.
        !compilationMappingEnvironment.has(env.get(op)));
    const args = node.drop(1);
    const calleeExp = compileExpression(callee, env, (symbolicOp && !shouldInlineOp) ? // Set the `shouldInline` option to `#f`
     Object.assign(Object.assign({}, options), { shouldInline: false }) : options);
    const argsExps = args.map(function (x) {
        return compileExpression(x, env, options);
    });
    return makeExpressionOrStatement(new estree_1.CallExpression(calleeExp, argsExps), options);
}
compileFunctionCall.lispSource = [Symbol.for('define'), [Symbol.for('compile-function-call'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('referenced-symbols'), [Symbol.for('oget'), Symbol.for('options'), 'referencedSymbols']], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('oget'), Symbol.for('options'), 'currentModule']], [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('send'), Symbol.for('callee'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('symbolic-op'), [Symbol.for('symbol?'), Symbol.for('op')]], [Symbol.for('define'), Symbol.for('should-inline-op'), [Symbol.for('and'), Symbol.for('symbolic-op'), [Symbol.for('should-inline?'), Symbol.for('op'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('not'), [Symbol.for('send'), Symbol.for('compilation-mapping-environment'), Symbol.for('has'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('op')]]]]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('callee-exp'), [Symbol.for('compile-expression'), Symbol.for('callee'), Symbol.for('env'), [Symbol.for('if'), [Symbol.for('and'), Symbol.for('symbolic-op'), [Symbol.for('not'), Symbol.for('should-inline-op')]], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'shouldInline', Symbol.for('#f')]], Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('args-exps'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('args')]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), Symbol.for('callee-exp'), Symbol.for('args-exps')], Symbol.for('options')]];
/**
 * Add symbol `sym` to `referencedSymbols` if it references a value
 * not defined in the current module.
 */
function addReferencedSymbol(sym, env, options = {}) {
    const referencedSymbols = options['referencedSymbols'];
    if (((referencedSymbols &&
        // Do not add if already added.
        !referencedSymbols.includes(sym)) &&
        shouldInlineP(sym, env, options))) {
        referencedSymbols.push(sym);
        return referencedSymbols;
    }
}
addReferencedSymbol.lispSource = [Symbol.for('define'), [Symbol.for('add-referenced-symbol'), Symbol.for('sym'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('referenced-symbols'), [Symbol.for('oget'), Symbol.for('options'), 'referencedSymbols']], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('referenced-symbols'), [Symbol.for('not'), [Symbol.for('memq?'), Symbol.for('sym'), Symbol.for('referenced-symbols')]], [Symbol.for('should-inline?'), Symbol.for('sym'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('push-right!'), Symbol.for('referenced-symbols'), Symbol.for('sym')]]];
/**
 * Whether the language binding for `sym` should be added to
 * the global environment.
 */
function shouldInlineP(sym, env, options = {}) {
    const shouldInlineOption = options['shouldInline'];
    let bindings = options['bindings'];
    const compilationMappingEnvironment = options['compilationMappingEnvironment'];
    const currentModule = options['currentModule'];
    // This may be disabled with the `shouldInline` option.
    if (!shouldInlineOption) {
        return false;
    }
    return ((((((typeof sym === 'symbol') &&
        // Do not inline if the symbol is listed in
        // `send compilation-variables-env`.
        !compilationVariablesEnv.has(sym)) &&
        // Do not inline if there is a local binding for the
        // value (e.g., a `let` variable).
        !(bindings && bindings.has(sym))) &&
        // Do not inline if the current module defines the
        // value.
        !(currentModule && currentModule.hasSymbol(sym))) &&
        // Only inline if the environment binds the symbol.
        env.findFrame(sym, {
            filter: function (x) {
                // Do not inline if the value is a JavaScript
                // value, i.e., if it is provided by the very
                // language compiled to.
                return x !== jsEnvironment;
            }
        })));
}
shouldInlineP.lispSource = [Symbol.for('define'), [Symbol.for('should-inline?'), Symbol.for('sym'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('should-inline-option'), [Symbol.for('oget'), Symbol.for('options'), 'shouldInline']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('oget'), Symbol.for('options'), 'currentModule']], [Symbol.for('unless'), Symbol.for('should-inline-option'), [Symbol.for('return'), Symbol.for('#f')]], [Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('sym')], [Symbol.for('not'), [Symbol.for('send'), Symbol.for('compilation-variables-env'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('not'), [Symbol.for('and'), Symbol.for('bindings'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]]], [Symbol.for('not'), [Symbol.for('and'), Symbol.for('current-module'), [Symbol.for('send'), Symbol.for('current-module'), Symbol.for('has-symbol'), Symbol.for('sym')]]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('find-frame'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('js-environment')]]]]]]];
/**
 * Compile a `(> ...)` expression.
 */
function compileGreaterThan(node, env, options = {}) {
    let exp = node.getValue();
    if (exp.length < 3) {
        return compileRose((0, rose_1.makeRose)(true), env, options);
    }
    else if (exp.length === 3) {
        return compileBinaryExpression(node, env, options, {
            identity: true,
            operator: '>'
        });
    }
    else {
        // Create `(and ...)` expression.
        const andExp = [Symbol.for('and')];
        const _end = exp.length;
        for (let i = 2; i < _end; i++) {
            andExp.push([Symbol.for('>'), exp[i - 1], exp[i]]);
        }
        return compileRose((0, rose_1.makeRose)(andExp), env, options);
    }
}
compileGreaterThan.lispSource = [Symbol.for('define'), [Symbol.for('compile-greater-than'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('#t')], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '>']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('>'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(>= ...)` expression.
 */
function compileGreaterThanOrEqual(node, env, options = {}) {
    let exp = node.getValue();
    if (exp.length < 3) {
        return compileRose((0, rose_1.makeRose)(true), env, options);
    }
    else if (exp.length === 3) {
        return compileBinaryExpression(node, env, options, {
            identity: true,
            operator: '>='
        });
    }
    else {
        // Create `(and ...)` expression.
        const andExp = [Symbol.for('and')];
        const _end = exp.length;
        for (let i = 2; i < _end; i++) {
            andExp.push([Symbol.for('>='), exp[i - 1], exp[i]]);
        }
        return compileRose((0, rose_1.makeRose)(andExp), env, options);
    }
}
compileGreaterThanOrEqual.lispSource = [Symbol.for('define'), [Symbol.for('compile-greater-than-or-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('#t')], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '>=']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('>='), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a binary expression.
 * Returns a `BinaryExpression`.
 */
function compileBinaryExpression(node, env, options = {}, settings = {}) {
    const operator = settings['operator'];
    const logical = settings['logical'];
    const operands = node.drop(1);
    if (operands.length === 0) {
        const identity = settings['identity'];
        return makeExpressionOrStatement(compileRose((0, rose_1.makeRose)(identity), env, options), options);
    }
    else if (operands.length === 1) {
        return makeExpressionOrStatement(compileRose(operands[0], env, options), options);
    }
    else {
        const compiledOperands = operands.map(function (arg) {
            return compileExpression(arg, env, options);
        });
        return makeExpressionOrStatement(compiledOperands.slice(1).reduce(function (acc, x) {
            if (logical) {
                return new estree_1.LogicalExpression(operator, acc, x);
            }
            else {
                return new estree_1.BinaryExpression(operator, acc, x);
            }
        }, compiledOperands[0]), options);
    }
}
compileBinaryExpression.lispSource = [Symbol.for('define'), [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('oget'), Symbol.for('settings'), 'operator']], [Symbol.for('define'), Symbol.for('logical'), [Symbol.for('oget'), Symbol.for('settings'), 'logical']], [Symbol.for('define'), Symbol.for('operands'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('operands')], 0], [Symbol.for('define'), Symbol.for('identity'), [Symbol.for('oget'), Symbol.for('settings'), 'identity']], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('identity')], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('operands')], 1], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-rose'), [Symbol.for('first'), Symbol.for('operands')], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('compiled-operands'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('arg')], [Symbol.for('compile-expression'), Symbol.for('arg'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('operands')]], [Symbol.for('make-expression-or-statement'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('if'), Symbol.for('logical'), [Symbol.for('new'), Symbol.for('LogicalExpression'), Symbol.for('operator'), Symbol.for('acc'), Symbol.for('x')], [Symbol.for('new'), Symbol.for('BinaryExpression'), Symbol.for('operator'), Symbol.for('acc'), Symbol.for('x')]]], [Symbol.for('first'), Symbol.for('compiled-operands')], [Symbol.for('rest'), Symbol.for('compiled-operands')]], Symbol.for('options')]]]];
/**
 * Compile a logical expression.
 * Like `compile-binary-expression`, but
 * returns a `LogicalExpression` instead.
 */
function compileLogicalExpression(node, env, options = {}, settings = {}) {
    return compileBinaryExpression(node, env, options, Object.assign(Object.assign({}, settings), { logical: true }));
}
compileLogicalExpression.lispSource = [Symbol.for('define'), [Symbol.for('compile-logical-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj-append'), Symbol.for('settings'), [Symbol.for('js-obj'), 'logical', Symbol.for('#t')]]]];
/**
 * Compile a `(lambda ...)` expression.
 */
function compileLambda(node, env, options = {}) {
    // (compile-js-arrow node env options)
    return compileJsFunction(node, env, options);
}
compileLambda.lispSource = [Symbol.for('define'), [Symbol.for('compile-lambda'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-js-function'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile a `(js/function ...)` expression.
 */
function compileJsFunction(node, env, options = {}, settings = {}) {
    const inheritedOptions = Object.assign({}, options);
    let exp = node.getValue();
    const functionName = settings['functionName'];
    const generator = settings['generator'];
    const returnType = settings['returnType'];
    const language = inheritedOptions['language'];
    let params = [];
    let bindings = inheritedOptions['bindings'];
    let argsList;
    let regularArgs;
    let restArg;
    bindings = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
    inheritedOptions['bindings'] = bindings;
    // Parse the parameter list: sort the regular parameters
    // from the rest parameter, if any.
    if (typeof ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1]) === 'symbol') {
        restArg = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1];
    }
    else if ((() => {
        const x = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            let x1 = lastCdr(exp);
            return Array.isArray(x1) && (x1.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1];
        return Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && !(() => {
            let x1 = lastCdr(x);
            return Array.isArray(x1) && (x1.length === 0);
        })();
    })()) {
        argsList = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1];
        regularArgs = (0, list_1.linkedListDropRight_)(argsList, 1);
        restArg =
            // (linked-list-last_ args-list)
            argsList[argsList.length - 1];
    }
    else {
        regularArgs = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1];
    }
    if (regularArgs) {
        for (let arg of regularArgs) {
            if ((0, util_1.colonFormP)(arg)) {
                const sym = arg[0];
                const typ = (Array.isArray(arg) && (arg.length >= 3) && (arg[arg.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(arg);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 2;
                    let result = arg;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = arg[arg.length - 1];
                        }
                        else {
                            result = arg.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : arg[2];
                bindings.setLocal(sym, true, 'variable');
                let result = ((arg.length === 4) ? new estree_1.AssignmentPattern(new estree_1.Identifier((0, printer_1.printEstree)(compileExpression((0, rose_1.makeRose)(sym), env, inheritedOptions), inheritedOptions)), compileExpression((0, rose_1.makeRose)((Array.isArray(arg) && (arg.length >= 3) && (arg[arg.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(arg);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 3;
                    let result = arg;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = arg[arg.length - 1];
                        }
                        else {
                            result = arg.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : arg[3]), env, inheritedOptions)) : new estree_1.Identifier((0, printer_1.printEstree)(compileExpression((0, rose_1.makeRose)(sym), env, inheritedOptions), inheritedOptions))).setType(compileType(typ, env, options));
                params.push(result);
            }
            else if (Array.isArray(arg)) {
                bindings.setLocal(arg[0], true, 'variable');
                params.push(new estree_1.AssignmentPattern(new estree_1.Identifier((0, printer_1.printEstree)(compileExpression((0, rose_1.makeRose)(arg[0]), env, inheritedOptions), inheritedOptions)), compileExpression((0, rose_1.makeRose)((Array.isArray(arg) && (arg.length >= 3) && (arg[arg.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(arg);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = arg;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = arg[arg.length - 1];
                        }
                        else {
                            result = arg.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : arg[1]), env, inheritedOptions)));
            }
            else {
                bindings.setLocal(arg, true, 'variable');
                params.push(new estree_1.Identifier((0, printer_1.printEstree)(compileExpression((0, rose_1.makeRose)(arg), env, inheritedOptions), inheritedOptions)));
            }
        }
    }
    if (restArg) {
        bindings.setLocal(restArg, true, 'variable');
        params.push(new estree_1.RestElement(compileExpression((0, rose_1.makeRose)(restArg), env, inheritedOptions)));
    }
    let bodyStatements = node.drop(2);
    if ((bodyStatements.length > 0) && (bodyStatements[0].getValue() === Symbol.for(':'))) {
        bodyStatements = bodyStatements.slice(2);
    }
    const body = wrapInBlockStatement(// wrap-in-block-statement-smart
    compileStatementOrReturnStatement((0, rose_1.beginWrapRoseSmart1)(bodyStatements).setParent(node), env, Object.assign(Object.assign({}, inheritedOptions), { expressionType: (returnType === 'void') ? 'statement' : 'return' })));
    let result;
    if (functionName && (functionName !== '')) {
        result = new estree_1.FunctionDeclaration(new estree_1.Identifier(functionName), params, body);
    }
    else {
        result = new estree_1.FunctionExpression(params, body);
    }
    if (generator) {
        result.generator = true;
    }
    return makeExpressionOrStatement(result, inheritedOptions);
}
compileJsFunction.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-function'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('function-name'), [Symbol.for('oget'), Symbol.for('settings'), 'functionName']], [Symbol.for('define'), Symbol.for('generator'), [Symbol.for('oget'), Symbol.for('settings'), 'generator']], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('oget'), Symbol.for('settings'), 'returnType']], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'language']], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('args-list')], [Symbol.for('define'), Symbol.for('regular-args')], [Symbol.for('define'), Symbol.for('rest-arg')], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('symbol?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('rest-arg'), [Symbol.for('second'), Symbol.for('exp')]]], [[Symbol.for('dotted-list?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('args-list'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('regular-args'), [Symbol.for('linked-list-drop-right_'), Symbol.for('args-list'), 1]], [Symbol.for('set!'), Symbol.for('rest-arg'), [Symbol.for('dotted-list-tail'), Symbol.for('args-list')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('regular-args'), [Symbol.for('second'), Symbol.for('exp')]]]], [Symbol.for('when'), Symbol.for('regular-args'), [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('regular-args')]], [Symbol.for('cond'), [[Symbol.for('colon-form?'), Symbol.for('arg')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), Symbol.for('arg')]], [Symbol.for('define'), Symbol.for('typ'), [Symbol.for('third'), Symbol.for('arg')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('#t'), 'variable'], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('~>'), [Symbol.for('if'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('arg')], 4], [Symbol.for('new'), Symbol.for('AssignmentPattern'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('sym')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]], [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('fourth'), Symbol.for('arg')]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('sym')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]], [Symbol.for('send'), Symbol.for('set-type'), [Symbol.for('compile-type'), Symbol.for('typ'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('push-right!'), Symbol.for('params'), Symbol.for('result')]], [[Symbol.for('array?'), Symbol.for('arg')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), [Symbol.for('first'), Symbol.for('arg')], Symbol.for('#t'), 'variable'], [Symbol.for('push-right!'), Symbol.for('params'), [Symbol.for('new'), Symbol.for('AssignmentPattern'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('first'), Symbol.for('arg')]], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]], [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('second'), Symbol.for('arg')]], Symbol.for('env'), Symbol.for('inherited-options')]]]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('arg'), Symbol.for('#t'), 'variable'], [Symbol.for('push-right!'), Symbol.for('params'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('arg')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]]]], [Symbol.for('when'), Symbol.for('rest-arg'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('rest-arg'), Symbol.for('#t'), 'variable'], [Symbol.for('push-right!'), Symbol.for('params'), [Symbol.for('new'), Symbol.for('RestElement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('rest-arg')], Symbol.for('env'), Symbol.for('inherited-options')]]]], [Symbol.for('define'), Symbol.for('body-statements'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('body-statements')], 0], [Symbol.for('eq?'), [Symbol.for('send'), [Symbol.for('first'), Symbol.for('body-statements')], Symbol.for('get-value')], [Symbol.for('quote'), Symbol.for(':')]]], [Symbol.for('set!'), Symbol.for('body-statements'), [Symbol.for('drop'), Symbol.for('body-statements'), 2]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('~>'), [Symbol.for('begin-wrap-rose-smart-1'), Symbol.for('body-statements')], [Symbol.for('send'), Symbol.for('set-parent'), Symbol.for('node')]], Symbol.for('env'), [Symbol.for('js-obj-append'), Symbol.for('inherited-options'), [Symbol.for('js-obj'), 'expressionType', [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('return-type'), 'void'], 'statement', 'return']]]]]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [[Symbol.for('and'), Symbol.for('function-name'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('function-name'), '']]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('FunctionDeclaration'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('function-name')], Symbol.for('params'), Symbol.for('body')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('FunctionExpression'), Symbol.for('params'), Symbol.for('body')]]]], [Symbol.for('when'), Symbol.for('generator'), [Symbol.for('set-field!'), Symbol.for('generator'), Symbol.for('result'), Symbol.for('#t')]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('inherited-options')]];
/**
 * Compile a `(js/arrow ...)` expression.
 */
function compileJsArrow(node, env, options = {}) {
    const f = compileJsFunction(node, env, options);
    if (f instanceof estree_1.FunctionExpression) {
        return new estree_1.ArrowFunctionExpression(f.params, f.body);
    }
    else {
        return f;
    }
}
compileJsArrow.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-arrow'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('compile-js-function'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('f'), Symbol.for('FunctionExpression')], [Symbol.for('new'), Symbol.for('ArrowFunctionExpression'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('f')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('f')]]], [Symbol.for('else'), Symbol.for('f')]]];
/**
 * Compile a `(< ...)` expression.
 */
function compileLessThan(node, env, options = {}) {
    let exp = node.getValue();
    if (exp.length < 3) {
        return compileRose((0, rose_1.makeRose)(true), env, options);
    }
    else if (exp.length === 3) {
        return compileBinaryExpression(node, env, options, {
            identity: true,
            operator: '<'
        });
    }
    else {
        // Create `(and ...)` expression.
        const andExp = [Symbol.for('and')];
        const _end = exp.length;
        for (let i = 2; i < _end; i++) {
            andExp.push([Symbol.for('<'), exp[i - 1], exp[i]]);
        }
        return compileRose((0, rose_1.makeRose)(andExp), env, options);
    }
}
compileLessThan.lispSource = [Symbol.for('define'), [Symbol.for('compile-less-than'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('#t')], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '<']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('<'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(<= ...)` expression.
 */
function compileLessThanOrEqual(node, env, options = {}) {
    let exp = node.getValue();
    if (exp.length < 3) {
        return compileRose((0, rose_1.makeRose)(true), env, options);
    }
    else if (exp.length === 3) {
        return compileBinaryExpression(node, env, options, {
            identity: true,
            operator: '<='
        });
    }
    else {
        // Create `(and ...)` expression.
        const andExp = [Symbol.for('and')];
        const _end = exp.length;
        for (let i = 2; i < _end; i++) {
            andExp.push([Symbol.for('<='), exp[i - 1], exp[i]]);
        }
        return compileRose((0, rose_1.makeRose)(andExp), env, options);
    }
}
compileLessThanOrEqual.lispSource = [Symbol.for('define'), [Symbol.for('compile-less-than-or-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('#t')], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '<=']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('<='), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(let ...)` expression.
 */
function compileLet(node, env, options = {}) {
    // There is no distinction between `(let ...)` and `(let* ...)`
    // expressions---they are compiled in the same way.
    return compileLetStar(node, env, options);
}
compileLet.lispSource = [Symbol.for('define'), [Symbol.for('compile-let'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-let-star'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile a `(let* ...)` expression.
 */
function compileLetStar(node, env, options = {}) {
    const inheritedOptions = Object.assign({}, options);
    let exp = node.getValue();
    const expressionType = inheritedOptions['expressionType'];
    let bindings = inheritedOptions['bindings'];
    let makeBlock = false;
    bindings = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
    inheritedOptions['bindings'] = bindings;
    if ((expressionType === 'statement') || (expressionType === 'return')) {
        const letNodes = node.get(1).getNodes();
        const bodyNodes = node.drop(2);
        const defineNodes = letNodes.map(function (x) {
            let exp = x.getValue();
            if (Array.isArray(exp)) {
                const sym = exp[0];
                if (!makeBlock && bindings.has(sym)) {
                    makeBlock = true;
                }
                return (0, rose_1.makeRose)([Symbol.for('define'), x.get(0), x.get(1)], x);
            }
            else {
                const sym = exp;
                if (!makeBlock && bindings.has(sym)) {
                    makeBlock = true;
                }
                return (0, rose_1.makeRose)([Symbol.for('define'), x], x);
            }
        });
        let result = compileRose((0, rose_1.makeRose)([makeBlock ? Symbol.for('block') : Symbol.for('begin'), ...defineNodes, ...bodyNodes], node), env, inheritedOptions);
        return result;
    }
    else {
        return compileExpression(wrapInArrowCall(exp), env, inheritedOptions);
    }
}
compileLetStar.lispSource = [Symbol.for('define'), [Symbol.for('compile-let-star'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#f')], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('let-nodes'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('define-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]]]], Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('sym'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('x')]]], Symbol.for('x')]]]], Symbol.for('let-nodes')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('make-block'), [Symbol.for('quote'), Symbol.for('block')], [Symbol.for('quote'), Symbol.for('begin')]]], [Symbol.for('unquote-splicing'), Symbol.for('define-nodes')], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('inherited-options')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('inherited-options')]]]];
/**
 * Compile a `(let-values ...)` expression.
 */
function compileLetValues(node, env, options = {}) {
    const inheritedOptions = Object.assign({}, options);
    let exp = node.getValue();
    const expressionType = inheritedOptions['expressionType'];
    let bindings = inheritedOptions['bindings'];
    let makeBlock = false;
    bindings = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
    inheritedOptions['bindings'] = bindings;
    if ((expressionType === 'statement') || (expressionType === 'return')) {
        const letNodes = node.get(1).getNodes();
        const bodyNodes = node.drop(2);
        const defineNodes = letNodes.map(function (x) {
            let exp = x.getValue();
            if (typeof exp === 'symbol') {
                const sym = exp;
                if (!makeBlock && bindings.has(sym)) {
                    makeBlock = true;
                }
                return (0, rose_1.makeRose)([Symbol.for('define'), x]);
            }
            else {
                const variables = x.get(0).getValue();
                if (typeof variables === 'symbol') {
                    const sym = variables;
                    if (!makeBlock && bindings.has(sym)) {
                        makeBlock = true;
                    }
                }
                else {
                    const syms = flatten(variables);
                    if (!makeBlock) {
                        for (let sym of flatten(variables)) {
                            if (bindings.has(sym)) {
                                makeBlock = true;
                                break;
                            }
                        }
                    }
                }
                let expression = x.get(1);
                return (0, rose_1.makeRose)([Symbol.for('define-values'), x.get(0), x.get(1)], x);
            }
        });
        let result = compileRose((0, rose_1.makeRose)([makeBlock ? Symbol.for('block') : Symbol.for('begin'), ...defineNodes, ...bodyNodes], node), env, inheritedOptions);
        return result;
    }
    else {
        return compileExpression(wrapInArrowCall(exp), env, inheritedOptions);
    }
}
compileLetValues.lispSource = [Symbol.for('define'), [Symbol.for('compile-let-values'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#f')], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('let-nodes'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('define-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('sym'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('x')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('variables'), [Symbol.for('~>'), Symbol.for('x'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('variables')], [Symbol.for('define'), Symbol.for('sym'), Symbol.for('variables')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('syms'), [Symbol.for('flatten'), Symbol.for('variables')]], [Symbol.for('unless'), Symbol.for('make-block'), [Symbol.for('for'), [[Symbol.for('sym'), [Symbol.for('flatten'), Symbol.for('variables')]]], [Symbol.for('when'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')], [Symbol.for('break')]]]]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]]]], Symbol.for('x')]]]], Symbol.for('let-nodes')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('make-block'), [Symbol.for('quote'), Symbol.for('block')], [Symbol.for('quote'), Symbol.for('begin')]]], [Symbol.for('unquote-splicing'), Symbol.for('define-nodes')], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('inherited-options')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('inherited-options')]]]];
/**
 * Compile a `(define-values ...)` expression.
 */
function compileDefineValues(node, env, options = {}) {
    let exp = node.getValue();
    const inheritedOptions = Object.assign({}, options);
    const expressionType = inheritedOptions['expressionType'];
    let bindings = inheritedOptions['bindings'];
    let makeBlock = true;
    let holeMarker = Symbol.for('_');
    bindings = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
    inheritedOptions['bindings'] = bindings;
    const variables = node.get(1).getValue();
    let expression = node.get(2);
    let regularVars = [];
    let restVar = undefined;
    let varDecls = [];
    let declaratorId;
    let declaratorInit;
    if (expression.getValue() === Symbol.for(':hole-marker')) {
        holeMarker = node.get(3).getValue();
        expression = node.get(4);
    }
    if (typeof variables === 'symbol') {
        declaratorId = new estree_1.Identifier((0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(variables), env, inheritedOptions), inheritedOptions));
        bindings.setLocal(variables, true, 'variable');
    }
    else {
        if (Array.isArray(variables) && (variables.length >= 3) && (variables[variables.length - 2] === Symbol.for('.')) && !(() => {
            const x = lastCdr(variables);
            return Array.isArray(x) && (x.length === 0);
        })()) {
            const varList = flatten(variables);
            regularVars = varList.slice(0, -1);
            restVar = varList[varList.length - 1];
        }
        else {
            regularVars = variables;
        }
        varDecls = regularVars.map(function (x) {
            if (x === holeMarker) {
                return null;
            }
            else {
                bindings.setLocal(x, true, 'variable');
                return new estree_1.Identifier((0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(x), env, inheritedOptions), inheritedOptions));
            }
        });
        if (restVar) {
            bindings.setLocal(restVar, true, 'variable');
            varDecls.push(new estree_1.RestElement(new estree_1.Identifier((0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(restVar), env, inheritedOptions), inheritedOptions))));
        }
        declaratorId = new estree_1.ArrayPattern(varDecls);
    }
    declaratorInit = compileExpression(expression, env, inheritedOptions);
    return new estree_1.VariableDeclaration([new estree_1.VariableDeclarator(declaratorId, declaratorInit)], 'let');
}
compileDefineValues.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-values'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#t')], [Symbol.for('define'), Symbol.for('hole-marker'), [Symbol.for('quote'), Symbol.for('_')]], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('variables'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 2]]], [Symbol.for('define'), Symbol.for('regular-vars'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-var'), Symbol.for('undefined')], [Symbol.for('define'), Symbol.for('var-decls'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('declarator-id')], [Symbol.for('define'), Symbol.for('declarator-init')], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('expression'), Symbol.for('get-value')], [Symbol.for('quote'), Symbol.for(':hole-marker')]], [Symbol.for('set!'), Symbol.for('hole-marker'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 3], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('set!'), Symbol.for('expression'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 4]]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('variables')], [Symbol.for('set!'), Symbol.for('declarator-id'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('variables')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('variables'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('variables')], [Symbol.for('define'), Symbol.for('var-list'), [Symbol.for('flatten'), Symbol.for('variables')]], [Symbol.for('set!'), Symbol.for('regular-vars'), [Symbol.for('drop-right'), Symbol.for('var-list'), 1]], [Symbol.for('set!'), Symbol.for('rest-var'), [Symbol.for('array-list-last'), Symbol.for('var-list')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('regular-vars'), Symbol.for('variables')]]], [Symbol.for('set!'), Symbol.for('var-decls'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')], Symbol.for('js/null')], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('x'), Symbol.for('#t'), 'variable'], [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]], Symbol.for('regular-vars')]], [Symbol.for('when'), Symbol.for('rest-var'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('rest-var'), Symbol.for('#t'), 'variable'], [Symbol.for('push-right!'), Symbol.for('var-decls'), [Symbol.for('new'), Symbol.for('RestElement'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('rest-var')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]], [Symbol.for('set!'), Symbol.for('declarator-id'), [Symbol.for('new'), Symbol.for('ArrayPattern'), Symbol.for('var-decls')]]]], [Symbol.for('set!'), Symbol.for('declarator-init'), [Symbol.for('compile-expression'), Symbol.for('expression'), Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), Symbol.for('declarator-id'), Symbol.for('declarator-init')]], 'let']];
/**
 * Compile a `(set!-values ...)` expression.
 */
function compileSetValues(node, env, options = {}) {
    let exp = node.getValue();
    const inheritedOptions = Object.assign({}, options);
    const expressionType = inheritedOptions['expressionType'];
    let makeBlock = true;
    let bindings = inheritedOptions['bindings'];
    let declaration;
    let declarator;
    let left;
    let right;
    bindings = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
    inheritedOptions['bindings'] = bindings;
    declaration = compileDefineValues((0, rose_1.makeRose)([Symbol.for('define-values'), ...node.drop(1)], node), env, inheritedOptions);
    declarator = declaration.declarations[0];
    left = declarator.id;
    right = declarator.init;
    return wrapExpressionInStatement(new estree_1.AssignmentExpression('=', left, right), inheritedOptions);
}
compileSetValues.lispSource = [Symbol.for('define'), [Symbol.for('compile-set-values'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#t')], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('declaration')], [Symbol.for('define'), Symbol.for('declarator')], [Symbol.for('define'), Symbol.for('left')], [Symbol.for('define'), Symbol.for('right')], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('set!'), Symbol.for('declaration'), [Symbol.for('compile-define-values'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('declarator'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('declaration')]]], [Symbol.for('set!'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('declarator')]], [Symbol.for('set!'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('declarator')]], [Symbol.for('wrap-expression-in-statement'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', Symbol.for('left'), Symbol.for('right')], Symbol.for('inherited-options')]];
/**
 * Compile a `(let-js-obj ...)` expression.
 */
function compileLetJsObj(node, env, options = {}) {
    const inheritedOptions = Object.assign({}, options);
    const expressionType = options['expressionType'];
    let bindings = inheritedOptions['bindings'];
    let makeBlock = false;
    bindings = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
    inheritedOptions['bindings'] = bindings;
    if ((expressionType === 'statement') || (expressionType === 'return')) {
        const letNodes = node.get(1).getNodes();
        const bodyNodes = node.drop(2);
        const defineNodes = letNodes.map(function (x) {
            const fields = x.get(0);
            const fieldsExp = fields.getValue();
            const obj = x.get(1);
            for (let f of fieldsExp) {
                const sym = Array.isArray(f) ? ((Array.isArray(f) && (f.length >= 3) && (f[f.length - 2] === Symbol.for('.')) && (() => {
                    let x1 = lastCdr(f);
                    return Array.isArray(x1) && (x1.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = f;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = f[f.length - 1];
                        }
                        else {
                            result = f.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : f[1]) : f;
                if (!makeBlock && bindings.has(sym)) {
                    makeBlock = true;
                }
            }
            return (0, rose_1.makeRose)([Symbol.for('define-js-obj'), fields, obj], x);
        });
        let result = compileRose((0, rose_1.makeRose)([makeBlock ? Symbol.for('block') : Symbol.for('begin'), ...defineNodes, ...bodyNodes], node), env, inheritedOptions);
        return result;
    }
    else {
        let exp = node.getValue();
        return compileExpression(wrapInArrowCall(exp), env, options);
    }
}
compileLetJsObj.lispSource = [Symbol.for('define'), [Symbol.for('compile-let-js-obj'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#f')], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('let-nodes'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('define-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('fields'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('fields-exp'), [Symbol.for('send'), Symbol.for('fields'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('for'), [[Symbol.for('f'), Symbol.for('fields-exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('f')], [Symbol.for('second'), Symbol.for('f')], Symbol.for('f')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-js-obj'), [Symbol.for('unquote'), Symbol.for('fields')], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('x')]], Symbol.for('let-nodes')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('make-block'), [Symbol.for('quote'), Symbol.for('block')], [Symbol.for('quote'), Symbol.for('begin')]]], [Symbol.for('unquote-splicing'), Symbol.for('define-nodes')], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('inherited-options')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(define-js-obj ...)` expression.
 */
function compileDefineJsObj(node, env, options = {}) {
    const expressionType = options['expressionType'];
    let bindings = options['bindings'];
    const fields = node.get(1);
    const fieldsExp = fields.getValue();
    const obj = node.get(2);
    for (let f of fieldsExp) {
        const sym = Array.isArray(f) ? ((Array.isArray(f) && (f.length >= 3) && (f[f.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(f);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = f;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = f[f.length - 1];
                }
                else {
                    result = f.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : f[1]) : f;
        bindings.setLocal(sym, true, 'variable');
    }
    const expressionStatement = compileSetJsObj((0, rose_1.makeRose)([Symbol.for('set!-js-obj'), fields, obj], node), env, options);
    const assignmentExpression = expressionStatement.expression;
    let left = assignmentExpression.left;
    let right = assignmentExpression.right;
    return new estree_1.VariableDeclaration([new estree_1.VariableDeclarator(left, right)], 'let');
}
compileDefineJsObj.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-js-obj'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('fields'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('fields-exp'), [Symbol.for('send'), Symbol.for('fields'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('for'), [[Symbol.for('f'), Symbol.for('fields-exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('f')], [Symbol.for('second'), Symbol.for('f')], Symbol.for('f')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('#t'), 'variable']], [Symbol.for('define'), Symbol.for('expression-statement'), [Symbol.for('compile-set-js-obj'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!-js-obj'), [Symbol.for('unquote'), Symbol.for('fields')], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('assignment-expression'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('expression-statement')]], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('assignment-expression')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('assignment-expression')]], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), Symbol.for('left'), Symbol.for('right')]], 'let']];
/**
 * Compile a `(set!-js-obj! ...)` expression.
 */
function compileSetJsObj(node, env, options = {}) {
    const expressionType = options['expressionType'];
    return wrapExpressionInStatement(new estree_1.AssignmentExpression('=', new estree_1.ObjectPattern(node.get(1).drop(0).map(function (x) {
        let exp = x.getValue();
        if (Array.isArray(exp)) {
            return new estree_1.Property(compileExpression(x.get(0), env, options), compileExpression(x.get(1), env, options));
        }
        else {
            const key = compileExpression(x, env, options);
            return new estree_1.Property(key, key);
        }
    })), compileExpression(node.get(2), env, options)), options);
}
compileSetJsObj.lispSource = [Symbol.for('define'), [Symbol.for('compile-set-js-obj'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('wrap-expression-in-statement'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', [Symbol.for('new'), Symbol.for('ObjectPattern'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('new'), Symbol.for('Property'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0], Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('key'), [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Property'), Symbol.for('key'), Symbol.for('key')]]]], [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('drop'), 0]]], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];
/**
 * Compile a `(list ...)` expression.
 */
function compileList(node, env, options = {}) {
    return makeExpressionOrStatement(new estree_1.ArrayExpression(node.drop(1).map(function (x) {
        return compileExpression(x, env, options);
    })), options);
}
compileList.lispSource = [Symbol.for('define'), [Symbol.for('compile-list'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ArrayExpression'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]], Symbol.for('options')]];
/**
 * Compile a macro call.
 */
function compileMacroCall(node, env, options = {}) {
    // Only expand the macro a single step, as there might be
    // compilers defined for the immediate expansion.
    const [expansion] = macroexpand1(node, env);
    return compileRose(expansion, env, options);
}
compileMacroCall.lispSource = [Symbol.for('define'), [Symbol.for('compile-macro-call'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define-values'), [Symbol.for('expansion')], [Symbol.for('macroexpand-1'), Symbol.for('node'), Symbol.for('env')]], [Symbol.for('compile-rose'), Symbol.for('expansion'), Symbol.for('env'), Symbol.for('options')]];
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
function macroexpand(exp, env) {
    let result = exp;
    let expanded = false;
    let expanded1 = true;
    while (expanded1) {
        [result, expanded1] = macroexpand1(result, env);
        expanded = expanded || expanded1;
    }
    return [result, expanded];
}
exports.macroexpand = macroexpand;
macroexpand.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('expanded'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('expanded1'), Symbol.for('#t')], [Symbol.for('while'), Symbol.for('expanded1'), [Symbol.for('set!-values'), [Symbol.for('result'), Symbol.for('expanded1')], [Symbol.for('macroexpand-1'), Symbol.for('result'), Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('expanded'), [Symbol.for('or'), Symbol.for('expanded'), Symbol.for('expanded1')]]], [Symbol.for('values'), Symbol.for('result'), Symbol.for('expanded')]];
/**
 * Expand the macro call `exp` in `env`.
 *
 * Similar to [`macroexpand-1` in Common Lisp][cl:macroexpand-1]
 * and [`macroexpand-1` in Emacs Lisp][el:macroexpand-1].
 *
 * [cl:macroexpand-1]: http://clhs.lisp.se/Body/f_mexp_.htm#macroexpand-1
 * [el:macroexpand-1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand_002d1
 */
function macroexpand1(exp, env) {
    let node = exp;
    let result = exp;
    let expanded = false;
    exp = (node instanceof rose_1.Rose) ? node.getValue() : node;
    env = env || (0, env_1.currentEnvironment_)() || (0, env_1.emptyEnvironment)();
    if (!(() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) {
        result = exp;
    }
    else if (Array.isArray(exp) && (exp.length === 0)) {
        result = exp;
    }
    else if ((0, util_1.quotep)(exp)) {
        result = (0, util_1.textOfQuotation)(exp);
    }
    else {
        const op = exp[0];
        const [macroF, typ] = env.getTypedValue(op);
        if (typ === 'macro') {
            result = macroF(exp, env);
            expanded = true;
        }
    }
    return [(node instanceof rose_1.Rose) ? (0, rose_1.makeRose)(result, node) : result, expanded];
}
exports.macroexpand1 = macroexpand1;
macroexpand1.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-1'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('node'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('expanded'), Symbol.for('#f')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')], Symbol.for('node')]], [Symbol.for('set!'), Symbol.for('env'), [Symbol.for('or'), Symbol.for('env'), [Symbol.for('current-environment_')], [Symbol.for('empty-environment')]]], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('list?'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('exp')]], [[Symbol.for('null?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('exp')]], [[Symbol.for('quote?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('text-of-quotation'), Symbol.for('exp')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('define-values'), [Symbol.for('macro-f'), Symbol.for('typ')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-typed-value'), Symbol.for('op')]], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('typ'), 'macro'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('macro-f'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('expanded'), Symbol.for('#t')]]]], [Symbol.for('values'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('make-rose'), Symbol.for('result'), Symbol.for('node')], Symbol.for('result')], Symbol.for('expanded')]];
/**
 * Expand the macro call `exp` in `env`, and keep
 * expanding the result for a total number of `n`
 * expansions, or until something that is not a
 * macro call is obtained.
 */
function macroexpandN(exp, env, n = 1) {
    let i = n;
    let result = exp;
    let expanded = false;
    let expanded1 = true;
    while (expanded1 && (i > 0)) {
        [result, expanded1] = macroexpand1(result, env);
        expanded = expanded || expanded1;
        i--;
    }
    return [result, expanded];
}
macroexpandN.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-n'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('n'), 1]], [Symbol.for('define'), Symbol.for('i'), Symbol.for('n')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('expanded'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('expanded1'), Symbol.for('#t')], [Symbol.for('while'), [Symbol.for('and'), Symbol.for('expanded1'), [Symbol.for('>'), Symbol.for('i'), 0]], [Symbol.for('set!-values'), [Symbol.for('result'), Symbol.for('expanded1')], [Symbol.for('macroexpand-1'), Symbol.for('result'), Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('expanded'), [Symbol.for('or'), Symbol.for('expanded'), Symbol.for('expanded1')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('values'), Symbol.for('result'), Symbol.for('expanded')]];
/**
 * Expand the macro call `exp` in `env`, and keep
 * expanding the result until `pred` returns `#f`,
 * or until something that is not a macro call
 * is obtained.
 */
function macroexpandUntil(exp, env, pred) {
    let result = exp;
    while (macroCallP(result, env) && pred(result)) {
        [result] = macroexpand1(result, env);
    }
    return result;
}
exports.macroexpandUntil = macroexpandUntil;
macroexpandUntil.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-until'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('pred')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('macro-call?'), Symbol.for('result'), Symbol.for('env')], [Symbol.for('pred'), Symbol.for('result')]], [Symbol.for('set!-values'), [Symbol.for('result')], [Symbol.for('macroexpand-1'), Symbol.for('result'), Symbol.for('env')]]], Symbol.for('result')];
/**
 * Expand all macro calls in `exp` in `env`.
 *
 * Similar to [`macroexpand-all` in Emacs Lisp][el:macroexpand-all].
 *
 * [el:macroexpand-all]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand_002dall
 */
function macroexpandAll(exp, env) {
    return macroexpandAllUntil(exp, env, function (...args) {
        return true;
    });
}
exports.macroexpandAll = macroexpandAll;
macroexpandAll.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-all'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('macroexpand-all-until'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('const'), Symbol.for('#t')]]];
/**
 * Expand the macro calls in `exp` in `env`, and keep
 * expanding until `pred` returns `#f`, or until
 * something that is not a macro call is obtained.
 */
function macroexpandAllUntil(exp, env, pred = undefined, stack = [], bindings = new env_1.LispEnvironment()) {
    function f(x, stack, bindings) {
        // Wrap `pred` in a function that checks
        // whether the operator symbol is locally
        // bound to something else than a macro.
        const predF = pred || (function (...args) {
            return true;
        });
        function predF1(x) {
            const op = x[0];
            const bType = bindings.getType(op);
            return ((bType === 'macro') || (bType === 'undefined')) && predF(x);
        }
        predF1.lispSource = [Symbol.for('define'), [Symbol.for('pred-f-1'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define-values'), Symbol.for('b-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')]], [Symbol.for('and'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('b-type'), 'macro'], [Symbol.for('eq?'), Symbol.for('b-type'), 'undefined']], [Symbol.for('pred-f'), Symbol.for('x')]]];
        if (macroCallP(x, env)) {
            let result = macroexpandUntil(x, env, predF1);
            if (!macroCallP(result, env)) {
                result = mapSexp(f, result, env, stack, bindings);
            }
            return result;
        }
        else {
            return x;
        }
    }
    f.lispSource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('pred-f'), [Symbol.for('or'), Symbol.for('pred'), [Symbol.for('const'), Symbol.for('#t')]]], [Symbol.for('define'), [Symbol.for('pred-f-1'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define-values'), Symbol.for('b-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')]], [Symbol.for('and'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('b-type'), 'macro'], [Symbol.for('eq?'), Symbol.for('b-type'), 'undefined']], [Symbol.for('pred-f'), Symbol.for('x')]]], [Symbol.for('cond'), [[Symbol.for('macro-call?'), Symbol.for('x'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('macroexpand-until'), Symbol.for('x'), Symbol.for('env'), Symbol.for('pred-f-1')]], [Symbol.for('unless'), [Symbol.for('macro-call?'), Symbol.for('result'), Symbol.for('env')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('result'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]], Symbol.for('result')], [Symbol.for('else'), Symbol.for('x')]]];
    return mapSexp(f, exp, env, stack, bindings);
}
exports.macroexpandAllUntil = macroexpandAllUntil;
macroexpandAllUntil.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-all-until'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('pred'), Symbol.for('undefined')], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('pred-f'), [Symbol.for('or'), Symbol.for('pred'), [Symbol.for('const'), Symbol.for('#t')]]], [Symbol.for('define'), [Symbol.for('pred-f-1'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define-values'), Symbol.for('b-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')]], [Symbol.for('and'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('b-type'), 'macro'], [Symbol.for('eq?'), Symbol.for('b-type'), 'undefined']], [Symbol.for('pred-f'), Symbol.for('x')]]], [Symbol.for('cond'), [[Symbol.for('macro-call?'), Symbol.for('x'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('macroexpand-until'), Symbol.for('x'), Symbol.for('env'), Symbol.for('pred-f-1')]], [Symbol.for('unless'), [Symbol.for('macro-call?'), Symbol.for('result'), Symbol.for('env')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('result'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]], Symbol.for('result')], [Symbol.for('else'), Symbol.for('x')]]], [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]];
/**
 * Macroexpand all compiler macros.
 * This expands regular macros as well.
 */
function macroexpandCompilerMacros(exp, env) {
    const compilerMacroEnv = makeMacroEnvironment(env);
    let result = macroexpandAll(exp, compilerMacroEnv);
    return result;
}
macroexpandCompilerMacros.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-compiler-macros'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('compiler-macro-env'), [Symbol.for('make-macro-environment'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('macroexpand-all'), Symbol.for('exp'), Symbol.for('compiler-macro-env')]], Symbol.for('result')];
/**
 * Compile a `(. ...)` expression.
 * Also handles `(.method obj ...)` calls.
 */
function compileDot(node, env, options = {}) {
    let exp = node.getValue();
    let match = exp[0].description.match(new RegExp('^\\.(.*)$'));
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
    if (method === '') {
        // Method call:
        // `(. foo bar ...)` = `(send foo bar ...)`.
        if ((match = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 2;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[2]).description.match(new RegExp('^-(.*)$')))) {
            const field = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && (() => {
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
            const fieldSym = Symbol.for(field);
            const obj = node.get(1);
            return compileGetField((0, rose_1.makeRose)([Symbol.for('get-field'), fieldSym, obj]), env, options);
        }
        else {
            return compileSend(node, env, options);
        }
    }
    else {
        const obj = node.get(1);
        if ((match = method.match(new RegExp('^-(.*)$')))) {
            // Member expression:
            // `(.-foo bar)` = `(get-field foo bar)`.
            const field = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && (() => {
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
            return compileGetField((0, rose_1.makeRose)([Symbol.for('get-field'), Symbol.for(field), obj], node), env, options);
        }
        else {
            // Method call:
            // `(.foo bar ...)` = `(send bar foo ...)`.
            return compileSend((0, rose_1.makeRose)([Symbol.for('send'), obj, Symbol.for(method), ...node.drop(2)], node), env, options);
        }
    }
}
compileDot.lispSource = [Symbol.for('define'), [Symbol.for('compile-dot'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\.(.*)$'], [Symbol.for('symbol->string'), [Symbol.for('first'), Symbol.for('exp')]]]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('method'), ''], [Symbol.for('cond'), [[Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^-(.*)$'], [Symbol.for('symbol->string'), [Symbol.for('third'), Symbol.for('exp')]]]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('define'), Symbol.for('field-sym'), [Symbol.for('string->symbol'), Symbol.for('field')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('compile-get-field'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('field-sym')], [Symbol.for('unquote'), Symbol.for('obj')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-send'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('cond'), [[Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^-(.*)$'], Symbol.for('method')]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('compile-get-field'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('field')]], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-send'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('obj')], [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('method')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]]]]];
/**
 * Compile a `(get-field ...)` expression.
 */
function compileGetField(node, env, options = {}) {
    const field = node.get(1);
    const obj = node.get(2);
    const computed = typeof field.getValue() !== 'symbol';
    return makeExpressionOrStatement(new estree_1.MemberExpression((typeof obj.getValue() === 'symbol') ? compileSymbol(obj, env, makeExpressionOptions(options)) : compileExpression(obj, env, options), computed ? compileExpression(field, env, options) : compileSymbol(field, env, options), computed), options);
}
compileGetField.lispSource = [Symbol.for('define'), [Symbol.for('compile-get-field'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('not'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('field'), Symbol.for('get-value')]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('MemberExpression'), [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('obj'), Symbol.for('get-value')]], [Symbol.for('compile-symbol'), Symbol.for('obj'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('if'), Symbol.for('computed'), [Symbol.for('compile-expression'), Symbol.for('field'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-symbol'), Symbol.for('field'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('computed')], Symbol.for('options')]];
/**
 * Compile a `(js/optional-chaining ...)` expression.
 */
function compileJsOptionalChaining(node, env, options = {}) {
    const obj = node.get(1);
    const field = node.get(2);
    const fieldExp = field.getValue();
    if (Array.isArray(fieldExp)) {
        let result = compileRose((0, rose_1.makeRose)([obj, ...field.drop(0)], node), env, options);
        result.optional = true;
        return result;
    }
    else {
        let result = compileRose((0, rose_1.makeRose)([Symbol.for('get-field'), field, obj], node), env, options);
        result.optional = true;
        return result;
    }
}
compileJsOptionalChaining.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-optional-chaining'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('field-exp'), [Symbol.for('send'), Symbol.for('field'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('field-exp')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('obj')], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('field'), Symbol.for('drop'), 0]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('optional'), Symbol.for('result'), Symbol.for('#t')], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('field')], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('optional'), Symbol.for('result'), Symbol.for('#t')], Symbol.for('result')]]];
/**
 * Compile a `(set-field! ...)` expression.
 */
function compileSetField(node, env, options = {}) {
    const field = node.get(1);
    const obj = node.get(2);
    let val = node.get(3);
    return compileRose((0, rose_1.makeRose)([Symbol.for('set!'), [Symbol.for('get-field'), field, obj], val], node), env, options);
}
compileSetField.lispSource = [Symbol.for('define'), [Symbol.for('compile-set-field'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 3]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('field')], [Symbol.for('unquote'), Symbol.for('obj')]], [Symbol.for('unquote'), Symbol.for('val')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile a `(modulo ...)` expression.
 */
function compileModulo(node, env, options = {}) {
    return compileBinaryExpression(node, env, options, {
        identity: 1,
        operator: '%'
    });
}
compileModulo.lispSource = [Symbol.for('define'), [Symbol.for('compile-modulo'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 1, 'operator', '%']]];
/**
 * Compile a `(* ...)` expression.
 */
function compileMul(node, env, options = {}) {
    return compileBinaryExpression(node, env, options, {
        identity: 1,
        operator: '*'
    });
}
compileMul.lispSource = [Symbol.for('define'), [Symbol.for('compile-mul'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 1, 'operator', '*']]];
/**
 * "NO-OP" compilation operation.
 * Creates an empty program fragment and does nothing else.
 */
function compileNop(node, env, options = {}) {
    return makeProgramFragment();
}
compileNop.lispSource = [Symbol.for('define'), [Symbol.for('compile-nop'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-program-fragment')]];
/**
 * Compile a `(not ...)` expression.
 */
function compileNot(node, env, options = {}) {
    function isNotExpressionP(x) {
        return (0, estree_1.estreeTypeP)(x, 'UnaryExpression') && (x.operator === '!');
    }
    isNotExpressionP.lispSource = [Symbol.for('define'), [Symbol.for('is-not-expression?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('x'), 'UnaryExpression'], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('x')], '!']]];
    const operand = node.get(1);
    const operandCompiled = compileExpression(operand, env, options);
    let result = undefined;
    if ((0, estree_1.estreeTypeP)(operandCompiled, 'BinaryExpression') && (operandCompiled.operator === '===')) {
        operandCompiled.operator = '!==';
        result = operandCompiled;
    }
    else if ((0, estree_1.estreeTypeP)(operandCompiled, 'BinaryExpression') && (operandCompiled.operator === '==')) {
        operandCompiled.operator = '!=';
        result = operandCompiled;
    }
    else {
        let notExpression = new estree_1.UnaryExpression('!', true, operandCompiled);
        // Cancel out double negation. Not sure this really
        // belongs---perhaps we do want it in some cases, as a way
        // to force boolean values (e.g., `!!undefined` = `false`).
        while (isNotExpressionP(notExpression) && isNotExpressionP(notExpression.argument)) {
            notExpression = notExpression.argument.argument;
        }
        result = notExpression;
    }
    return makeExpressionOrStatement(result, options);
}
compileNot.lispSource = [Symbol.for('define'), [Symbol.for('compile-not'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), [Symbol.for('is-not-expression?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('x'), 'UnaryExpression'], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('x')], '!']]], [Symbol.for('define'), Symbol.for('operand'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('operand-compiled'), [Symbol.for('compile-expression'), Symbol.for('operand'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('operand-compiled'), 'BinaryExpression'], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('operand-compiled')], '===']], [Symbol.for('set-field!'), Symbol.for('operator'), Symbol.for('operand-compiled'), '!=='], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('operand-compiled')]], [[Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('operand-compiled'), 'BinaryExpression'], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('operand-compiled')], '==']], [Symbol.for('set-field!'), Symbol.for('operator'), Symbol.for('operand-compiled'), '!='], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('operand-compiled')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('not-expression'), [Symbol.for('new'), Symbol.for('UnaryExpression'), '!', Symbol.for('#t'), Symbol.for('operand-compiled')]], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('is-not-expression?'), Symbol.for('not-expression')], [Symbol.for('is-not-expression?'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('not-expression')]]], [Symbol.for('set!'), Symbol.for('not-expression'), [Symbol.for('~>'), Symbol.for('not-expression'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('_')], [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('_')]]]], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('not-expression')]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];
/**
 * Compile a `(begin ...)` expression.
 */
function compileBegin(node, env, options = {}) {
    let bindings = options['bindings'];
    const expressionType = options['expressionType'];
    let exp = node.getValue();
    const body = node.drop(1);
    const compiledBody = [];
    // Add defined variables to `bindings` environment.
    // We have to handle them here since they may refer
    // to each other.
    const _end = body.length;
    for (let i = 0; i < _end; i++) {
        let exp = body[i].getValue();
        if ((0, util_1.formp)(exp, define_, env)) {
            const sym = Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1]) ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1])[0] : ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1]);
            bindings.setLocal(sym, true, 'variable');
        }
        else if ((0, util_1.formp)(exp, defineMacro_, env)) {
            const sym = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1])[0];
            bindings.setLocal(sym, true, 'macro');
        }
        else if ((0, util_1.formp)(exp, macros_1.defmacro_, env)) {
            const sym = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1];
            bindings.setLocal(sym, true, 'macro');
        }
    }
    if ((expressionType === 'statement') || (expressionType === 'return')) {
        let bodyStatements = compileStatements(body, env, options);
        // Note that this returns a `Program` node, but in
        // some contexts, a `BlockStatement` node is wanted.
        // One can convert a `Program` node to a
        // `BlockStatement` node with
        // `wrap-in-block-statement`.
        return makeProgramFragment(bodyStatements);
    }
    else {
        // Wrap in an arrow function.
        if (exp.length === 2) {
            return compileExpression(node.get(1), env, options);
        }
        else {
            return compileExpression(wrapInArrowCall(exp), env, options);
        }
    }
}
compileBegin.lispSource = [Symbol.for('define'), [Symbol.for('compile-begin'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('compiled-body'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('body')]]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), [Symbol.for('aget'), Symbol.for('body'), Symbol.for('i')], Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('form?'), Symbol.for('exp'), Symbol.for('define_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('if'), [Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('#t'), 'variable']], [[Symbol.for('form?'), Symbol.for('exp'), Symbol.for('define-macro_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('#t'), 'macro']], [[Symbol.for('form?'), Symbol.for('exp'), Symbol.for('defmacro_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('#t'), 'macro']]]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('body-statements'), [Symbol.for('compile-statements'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('make-program-fragment'), Symbol.for('body-statements')]], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('options')]]]]]];
/**
 * Compile a `(block ...)` expression.
 */
function compileBlock(node, env, options = {}) {
    const expressionType = options['expressionType'];
    if ((expressionType === 'statement') || (expressionType === 'return')) {
        return wrapInBlockStatement(compileBegin(node, env, options));
    }
    else {
        return compileBegin(node, env, options);
    }
}
compileBlock.lispSource = [Symbol.for('define'), [Symbol.for('compile-block'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-begin'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('compile-begin'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Make and compile a `(require ...)` or `(define-values ...)` form
 * that defines referenced values from the language environment.
 * `symbols` is a list of symbols bound in the language environment.
 */
function buildGlobalEnvironment(symbols, env, options = {}) {
    let exp = makeGlobalEnvironmentExp(symbols, env, options);
    return compileGlobalEnvironment(exp, env, options);
}
buildGlobalEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('build-global-environment'), Symbol.for('symbols'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('make-global-environment-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('compile-global-environment'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Make a form that defines referenced values
 * from the language environment. Returns `#f`
 * if there are no symbols.
 */
function makeGlobalEnvironmentExp(symbols, env, options) {
    if (symbols.length === 0) {
        return false;
    }
    else if (options['inlineFunctions']) {
        return makeDefineValuesExp(symbols, env, options);
    }
    else {
        return makeRequireExp(symbols, env, options);
    }
}
makeGlobalEnvironmentExp.lispSource = [Symbol.for('define'), [Symbol.for('make-global-environment-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('symbols')], 0], Symbol.for('#f')], [[Symbol.for('oget'), Symbol.for('options'), 'inlineFunctions'], [Symbol.for('make-define-values-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('make-require-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Make a `(define-values ...)` form for the global environment.
 */
function makeDefineValuesExp(symbols, env, options) {
    const inlineFunctionsOption = options['inlineFunctions'];
    let definitions = false;
    const defineForms = [];
    const internalSymbols = [];
    const externalSymbols = [];
    const referencedSymbols = [...symbols];
    const currentModule = new Module();
    let bindings = options['bindings'];
    const seen = [];
    let exp;
    let internalSymbol;
    let symbol;
    let value;
    while (referencedSymbols.length > 0) {
        symbol = referencedSymbols.shift();
        seen.push(symbol);
        if (!externalSymbols.includes(symbol) && env.has(symbol)) {
            value = env.get(symbol);
            if (sourcep(value)) {
                exp = source(value);
                if ((0, util_1.taggedListP)(exp, Symbol.for('define'))) {
                    internalSymbol = Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                        const x = lastCdr(exp);
                        return Array.isArray(x) && (x.length === 0);
                    })()) ? (() => {
                        let i = 1;
                        let result = exp;
                        while (i > 0) {
                            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                result = exp[exp.length - 1];
                            }
                            else {
                                result = exp.slice(1);
                            }
                            i--;
                        }
                        if (Array.isArray(result)) {
                            result = result[0];
                        }
                        return result;
                    })() : exp[1]) ? exp[1][0] : exp[1];
                    const referencedSymbols1 = [];
                    const bindings1 = bindings ? bindings.clone() : undefined;
                    const compiledExpression = compileRose((0, rose_1.makeRose)(exp), env, Object.assign(Object.assign({}, options), { bindings: bindings1, currentModule: currentModule, referencedSymbols: referencedSymbols1 }));
                    for (let symbol1 of referencedSymbols1) {
                        if (!(seen.includes(symbol1) || referencedSymbols.includes(symbol1))) {
                            referencedSymbols.push(symbol1);
                        }
                    }
                }
            }
            else {
                // Deal with the case when the value has no Lisp source.
                if (value instanceof Function) {
                    const jsString = value + '';
                    let match;
                    match = jsString.match(new RegExp('^function ([^( ]+)'));
                    if (match) {
                        internalSymbol = Symbol.for((Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && (() => {
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
                        })() : match[1]);
                        exp = [Symbol.for('js'), jsString];
                    }
                    else {
                        internalSymbol = symbol;
                        exp = [Symbol.for('define'), internalSymbol, [Symbol.for('js'), jsString]];
                    }
                }
                else if ((value !== null) && (typeof value === 'object')) {
                    const jsString = JSON.stringify(value, null, 2);
                    internalSymbol = symbol;
                    exp = [Symbol.for('define'), internalSymbol, [Symbol.for('js'), jsString]];
                }
                else {
                    const jsString = value + '';
                    internalSymbol = symbol;
                    exp = [Symbol.for('define'), internalSymbol, [Symbol.for('js'), jsString]];
                }
            }
            if (!internalSymbols.includes(internalSymbol)) {
                // Do not push the same `define` form more than once.
                defineForms.push(exp);
            }
            if (symbols.includes(symbol)) {
                internalSymbols.push(internalSymbol);
                externalSymbols.push(symbol);
            }
        }
    }
    if (externalSymbols.length > 0) {
        definitions = [Symbol.for('define-values'), externalSymbols, [[Symbol.for('js/arrow'), [], ...defineForms, [Symbol.for('values'), ...internalSymbols]]]];
    }
    return definitions;
}
makeDefineValuesExp.lispSource = [Symbol.for('define'), [Symbol.for('make-define-values-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('inline-functions-option'), [Symbol.for('oget'), Symbol.for('options'), 'inlineFunctions']], [Symbol.for('define'), Symbol.for('definitions'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('define-forms'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('internal-symbols'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('external-symbols'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('referenced-symbols'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('symbols')]]]], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('new'), Symbol.for('Module')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('seen'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('internal-symbol')], [Symbol.for('define'), Symbol.for('symbol')], [Symbol.for('define'), Symbol.for('value')], [Symbol.for('while'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('referenced-symbols')], 0], [Symbol.for('set!'), Symbol.for('symbol'), [Symbol.for('pop!'), Symbol.for('referenced-symbols')]], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('symbol')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('memq?'), Symbol.for('symbol'), Symbol.for('external-symbols')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('symbol')]], [Symbol.for('set!'), Symbol.for('value'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('symbol')]], [Symbol.for('cond'), [[Symbol.for('source?'), Symbol.for('value')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('source'), Symbol.for('value')]], [Symbol.for('when'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define')]], [Symbol.for('set!'), Symbol.for('internal-symbol'), [Symbol.for('if'), [Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('array-first'), [Symbol.for('array-second'), Symbol.for('exp')]], [Symbol.for('array-second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('referenced-symbols-1'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('bindings-1'), [Symbol.for('if'), Symbol.for('bindings'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('clone')], Symbol.for('undefined')]], [Symbol.for('define'), Symbol.for('compiled-expression'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('exp')], Symbol.for('env'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'bindings', Symbol.for('bindings-1'), 'currentModule', Symbol.for('current-module'), 'referencedSymbols', Symbol.for('referenced-symbols-1')]]]], [Symbol.for('for'), [[Symbol.for('symbol-1'), Symbol.for('referenced-symbols-1')]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('memq?'), Symbol.for('symbol-1'), Symbol.for('seen')], [Symbol.for('memq?'), Symbol.for('symbol-1'), Symbol.for('referenced-symbols')]], [Symbol.for('push-right!'), Symbol.for('referenced-symbols'), Symbol.for('symbol-1')]]]]], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('procedure?'), Symbol.for('value')], [Symbol.for('define'), Symbol.for('js-string'), [Symbol.for('string-append'), Symbol.for('value'), '']], [Symbol.for('define'), Symbol.for('match')], [Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^function ([^( ]+)'], Symbol.for('js-string')]], [Symbol.for('cond'), [Symbol.for('match'), [Symbol.for('set!'), Symbol.for('internal-symbol'), [Symbol.for('string->symbol'), [Symbol.for('second'), Symbol.for('match')]]], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quasiquote'), [Symbol.for('js'), [Symbol.for('unquote'), Symbol.for('js-string')]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('internal-symbol'), Symbol.for('symbol')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('internal-symbol')], [Symbol.for('js'), [Symbol.for('unquote'), Symbol.for('js-string')]]]]]]]], [[Symbol.for('js-obj?'), Symbol.for('value')], [Symbol.for('define'), Symbol.for('js-string'), [Symbol.for('send'), Symbol.for('JSON'), Symbol.for('stringify'), Symbol.for('value'), Symbol.for('js/null'), 2]], [Symbol.for('set!'), Symbol.for('internal-symbol'), Symbol.for('symbol')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('internal-symbol')], [Symbol.for('js'), [Symbol.for('unquote'), Symbol.for('js-string')]]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('js-string'), [Symbol.for('string-append'), Symbol.for('value'), '']], [Symbol.for('set!'), Symbol.for('internal-symbol'), Symbol.for('symbol')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('internal-symbol')], [Symbol.for('js'), [Symbol.for('unquote'), Symbol.for('js-string')]]]]]]]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('internal-symbol'), Symbol.for('internal-symbols')], [Symbol.for('push-right!'), Symbol.for('define-forms'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('memq?'), Symbol.for('symbol'), Symbol.for('symbols')], [Symbol.for('push-right!'), Symbol.for('internal-symbols'), Symbol.for('internal-symbol')], [Symbol.for('push-right!'), Symbol.for('external-symbols'), Symbol.for('symbol')]]]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('external-symbols')], 0], [Symbol.for('set!'), Symbol.for('definitions'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote'), Symbol.for('external-symbols')], [[Symbol.for('js/arrow'), [], [Symbol.for('unquote-splicing'), Symbol.for('define-forms')], [Symbol.for('values'), [Symbol.for('unquote-splicing'), Symbol.for('internal-symbols')]]]]]]]], Symbol.for('definitions')];
/**
 * Make a `(require ...)` form for the global environment.
 */
function makeRequireExp(symbols, env, options) {
    return [Symbol.for('require'), [Symbol.for('only-in'), constants_1.packageName, ...symbols]];
}
makeRequireExp.lispSource = [Symbol.for('define'), [Symbol.for('make-require-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('quasiquote'), [Symbol.for('require'), [Symbol.for('only-in'), [Symbol.for('unquote'), Symbol.for('package-name')], [Symbol.for('unquote-splicing'), Symbol.for('symbols')]]]]];
/**
 * Compile a `(define-values ...)` form that defines referenced values
 * from the language environment.
 */
function compileGlobalEnvironment(exp, env, options = {}) {
    if (!exp) {
        return makeProgramFragment();
    }
    else if ((0, util_1.taggedListP)(exp, Symbol.for('define-values'))) {
        const defineValuesForm = [exp[0], (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1], [Symbol.for('list')]];
        const body = exp[2];
        // Compile the body in a sandboxed environment.
        const bodyCompiled = compileSexp(body, env, Object.assign(Object.assign({}, options), { bindings: new env_1.LispEnvironment(), expressionType: 'expression' }));
        const varDecl = compileSexp(defineValuesForm, env, options);
        varDecl.declarations[0].init = bodyCompiled;
        let result = makeProgramFragment([varDecl]);
        return result;
    }
    else {
        return makeProgramFragment([compileSexp(exp, env, options)]);
    }
}
compileGlobalEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('compile-global-environment'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('exp')], [Symbol.for('make-program-fragment')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define-values')]], [Symbol.for('define'), Symbol.for('define-values-form'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('list')]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('aget'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('body-compiled'), [Symbol.for('compile-sexp'), Symbol.for('body'), Symbol.for('env'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'bindings', [Symbol.for('new'), Symbol.for('LispEnvironment')], 'expressionType', 'expression']]]], [Symbol.for('define'), Symbol.for('var-decl'), [Symbol.for('compile-sexp'), Symbol.for('define-values-form'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('init'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('var-decl')]], Symbol.for('body-compiled')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('make-program-fragment'), [Symbol.for('list'), Symbol.for('var-decl')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('make-program-fragment'), [Symbol.for('list'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]]]]]];
/**
 * Make a `((lambda () ...))` expression that evaluates to a single
 * value from the language environment. `symbol` is a symbol bound in
 * the language environment.
 */
function makeInlinedValue(symbol, env, options) {
    // We take the output of a call to `make-global-environment-exp`
    // and massage it into a simpler expression.
    const globalEnvironmentExp = makeGlobalEnvironmentExp([symbol], env, Object.assign(Object.assign({}, options), { inlineFunctions: true }));
    if (globalEnvironmentExp.length > 1) {
        const lambdaCall = globalEnvironmentExp[2];
        const lambdaExp = lambdaCall[0];
        const valuesExp = lambdaExp[lambdaExp.length - 1];
        const sym = (Array.isArray(valuesExp) && (valuesExp.length >= 3) && (valuesExp[valuesExp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(valuesExp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = valuesExp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = valuesExp[valuesExp.length - 1];
                }
                else {
                    result = valuesExp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : valuesExp[1];
        let result = lambdaCall;
        if ((lambdaExp.length === 4) && (typeof (() => {
            const lst = (Array.isArray(lambdaExp) && (lambdaExp.length >= 3) && (lambdaExp[lambdaExp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(lambdaExp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 2;
                let result = lambdaExp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = lambdaExp[lambdaExp.length - 1];
                    }
                    else {
                        result = lambdaExp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : lambdaExp[2];
            if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(lst);
                return Array.isArray(x) && (x.length === 0);
            })()) {
                let i = 1;
                let result = lst;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = lst[lst.length - 1];
                    }
                    else {
                        result = lst.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            }
            else {
                return lst[1];
            }
        })() === 'symbol')) {
            // In simple cases, where there is only a single
            // `(define sym ...)` form, no `lambda` expression
            // is necessary.
            result = (() => {
                const lst = (Array.isArray(lambdaExp) && (lambdaExp.length >= 3) && (lambdaExp[lambdaExp.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(lambdaExp);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 2;
                    let result = lambdaExp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = lambdaExp[lambdaExp.length - 1];
                        }
                        else {
                            result = lambdaExp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : lambdaExp[2];
                if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(lst);
                    return Array.isArray(x) && (x.length === 0);
                })()) {
                    let i = 2;
                    let result = lst;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = lst[lst.length - 1];
                        }
                        else {
                            result = lst.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                }
                else {
                    return lst[2];
                }
            })();
        }
        else {
            // Change the return value of the `lambda` function
            // from a `(values ...)` form to a single value.
            lambdaExp[lambdaExp.length - 1] = sym;
        }
        return result;
    }
    else {
        return globalEnvironmentExp;
    }
}
makeInlinedValue.lispSource = [Symbol.for('define'), [Symbol.for('make-inlined-value'), Symbol.for('symbol'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('global-environment-exp'), [Symbol.for('make-global-environment-exp'), [Symbol.for('list'), Symbol.for('symbol')], Symbol.for('env'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'inlineFunctions', Symbol.for('#t')]]]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('global-environment-exp')], 1], [Symbol.for('define'), Symbol.for('lambda-call'), [Symbol.for('aget'), Symbol.for('global-environment-exp'), 2]], [Symbol.for('define'), Symbol.for('lambda-exp'), [Symbol.for('aget'), Symbol.for('lambda-call'), 0]], [Symbol.for('define'), Symbol.for('values-exp'), [Symbol.for('array-list-last'), Symbol.for('lambda-exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('second'), Symbol.for('values-exp')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lambda-call')], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('lambda-exp')], 4], [Symbol.for('symbol?'), [Symbol.for('second'), [Symbol.for('third'), Symbol.for('lambda-exp')]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('third'), [Symbol.for('third'), Symbol.for('lambda-exp')]]]], [Symbol.for('else'), [Symbol.for('list-set!'), Symbol.for('lambda-exp'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('lambda-exp')], 1], Symbol.for('sym')]]], Symbol.for('result')], [Symbol.for('else'), Symbol.for('global-environment-exp')]]];
/**
 * Compile an `(or ...)` expression.
 */
function compileOr(node, env, options = {}) {
    return compileLogicalExpression(node, env, options, {
        identity: false,
        operator: '||'
    });
}
compileOr.lispSource = [Symbol.for('define'), [Symbol.for('compile-or'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-logical-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#f'), 'operator', '||']]];
/**
 * Compile a `(provide ...)` expression.
 */
function compileProvide(node, env, options = {}) {
    const expressions = node.drop(1);
    // Sort `all-from-out` expressions from the rest.
    const allFromOutExpressions = [];
    const otherExpressions = [];
    for (let x of expressions) {
        if ((0, util_1.taggedListP)(x.getValue(), Symbol.for('all-from-out'))) {
            allFromOutExpressions.push(x);
        }
        else {
            otherExpressions.push(x);
        }
    }
    // Compile `all-from-out` expressions.
    const results = [];
    for (let x of allFromOutExpressions) {
        const source = x.get(1);
        let result = new estree_1.ExportAllDeclaration(compileExpression(source, env, options));
        results.push(result);
    }
    // Compile other expressions.
    if (otherExpressions.length > 0) {
        let specifiers = [];
        const seen = [];
        for (let x of otherExpressions) {
            let exp = x.getValue();
            if ((0, util_1.taggedListP)(exp, Symbol.for('rename-out'))) {
                for (let pair of exp.slice(1)) {
                    let x1 = pair[0];
                    let x2 = (Array.isArray(pair) && (pair.length >= 3) && (pair[pair.length - 2] === Symbol.for('.')) && (() => {
                        const x3 = lastCdr(pair);
                        return Array.isArray(x3) && (x3.length === 0);
                    })()) ? (() => {
                        let i = 1;
                        let result = pair;
                        while (i > 0) {
                            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                result = pair[pair.length - 1];
                            }
                            else {
                                result = pair.slice(1);
                            }
                            i--;
                        }
                        if (Array.isArray(result)) {
                            result = result[0];
                        }
                        return result;
                    })() : pair[1];
                    if (typeof x1 === 'symbol') {
                        x1 = (0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(x1), env, options, {
                            literalSymbol: true
                        }), options);
                    }
                    if (typeof x2 === 'symbol') {
                        x2 = (0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(x2), env, options, {
                            literalSymbol: true
                        }), options);
                    }
                    if (!seen.includes(x2)) {
                        seen.push(x2);
                        specifiers.push(new estree_1.ExportSpecifier(new estree_1.Identifier(x1), new estree_1.Identifier(x2)));
                    }
                }
            }
            else {
                let x1 = exp;
                if (typeof x1 === 'symbol') {
                    x1 = (0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(x1), env, options, {
                        literalSymbol: true
                    }), options);
                }
                if (!seen.includes(x1)) {
                    seen.push(x1);
                    specifiers.push(new estree_1.ExportSpecifier(new estree_1.Identifier(x1)));
                }
            }
        }
        let result = new estree_1.ExportNamedDeclaration(null, specifiers);
        results.push(result);
    }
    if (results.length === 1) {
        return results[0];
    }
    else {
        return makeProgramFragment(results);
    }
}
compileProvide.lispSource = [Symbol.for('define'), [Symbol.for('compile-provide'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expressions'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('all-from-out-expressions'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('other-expressions'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('expressions')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')], [Symbol.for('quote'), Symbol.for('all-from-out')]], [Symbol.for('push-right!'), Symbol.for('all-from-out-expressions'), Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('other-expressions'), Symbol.for('x')]]]], [Symbol.for('define'), Symbol.for('results'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('all-from-out-expressions')]], [Symbol.for('define'), Symbol.for('source'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('ExportAllDeclaration'), [Symbol.for('compile-expression'), Symbol.for('source'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('push-right!'), Symbol.for('results'), Symbol.for('result')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('other-expressions')], 0], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('seen'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('other-expressions')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('rename-out')]], [Symbol.for('for'), [[Symbol.for('pair'), [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('first'), Symbol.for('pair')]], [Symbol.for('define'), Symbol.for('x2'), [Symbol.for('second'), Symbol.for('pair')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x2')], [Symbol.for('set!'), Symbol.for('x2'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x2')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x2'), Symbol.for('seen')], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x2')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ExportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x2')]]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x1'), Symbol.for('seen')], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x1')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ExportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('ExportNamedDeclaration'), Symbol.for('js/null'), Symbol.for('specifiers')]], [Symbol.for('push-right!'), Symbol.for('results'), Symbol.for('result')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('results')], 1], [Symbol.for('first'), Symbol.for('results')]], [Symbol.for('else'), [Symbol.for('make-program-fragment'), Symbol.for('results')]]]];
/**
 * Compile a `(quote ...)` expression.
 */
function compileQuote(node, env, options = {}) {
    let exp = node.getValue();
    let result;
    if (Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1])) {
        result = compileExpression((0, rose_1.makeRose)([Symbol.for('list'), ...((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1]).map(function (x) {
                return [Symbol.for('quote'), x];
            })]), env, options);
    }
    else if (typeof ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1]) === 'symbol') {
        result = compileSymbol(node.get(1), env, options, {
            quotedSymbol: true
        });
    }
    else {
        result = compileExpression(node.get(1), env, options);
    }
    return makeExpressionOrStatement(result, options);
}
compileQuote.lispSource = [Symbol.for('define'), [Symbol.for('compile-quote'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [[Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('list'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('quasiquote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('x')]]]]]]]]], Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('symbol?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-symbol'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'quotedSymbol', Symbol.for('#t')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];
/**
 * Compile a `(quasiquote ...)` expression.
 */
function compileQuasiquote(node, env, options = {}) {
    return makeExpressionOrStatement(compileQuasiquoteHelper(node.get(1), env, options), options);
}
compileQuasiquote.lispSource = [Symbol.for('define'), [Symbol.for('compile-quasiquote'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-quasiquote-helper'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]];
/**
 * Helper function for `compile-quasiquote`.
 */
function compileQuasiquoteHelper(node, env, options = {}) {
    let exp = node.getValue();
    if (!Array.isArray(exp)) {
        return compileExpression((0, rose_1.makeRose)([Symbol.for('quote'), exp]), env, options);
    }
    else {
        return new estree_1.ArrayExpression(node.getNodes().map(function (x) {
            let exp = x.getValue();
            if ((0, util_1.taggedListP)(exp, Symbol.for('quasiquote'))) {
                return compileQuote((0, rose_1.makeRose)([Symbol.for('quote'), exp]), env, options);
            }
            else if ((0, util_1.taggedListP)(exp, Symbol.for('unquote'))) {
                return compileExpression(x.get(1), env, options);
            }
            else if ((0, util_1.taggedListP)(exp, Symbol.for('unquote-splicing'))) {
                return new estree_1.RestElement(compileExpression(x.get(1), env, options));
            }
            else {
                return compileQuasiquoteHelper(x, env, options);
            }
        }));
    }
}
compileQuasiquoteHelper.lispSource = [Symbol.for('define'), [Symbol.for('compile-quasiquote-helper'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('array?'), Symbol.for('exp')]], [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ArrayExpression'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('quasiquote')]], [Symbol.for('compile-quote'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('unquote')]], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]], [Symbol.for('new'), Symbol.for('RestElement'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('compile-quasiquote-helper'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-nodes')]]]]]];
/**
 * Compile a `(require ...)` expression.
 */
function compileRequire(node, env, options = {}) {
    const esModuleInterop = options['esModuleInterop'];
    let bindings = options['bindings'];
    const xNode = node.get(1);
    let xExp = xNode.getValue();
    const yNode = node.get(2) || xNode;
    let yExp = yNode.getValue();
    let specifiers = [];
    const seen = [];
    let src = null;
    if ((0, util_1.taggedListP)(xExp, Symbol.for('only-in'))) {
        for (let x of xNode.drop(2)) {
            let exp = x.getValue();
            if (Array.isArray(exp)) {
                let x1 = exp[0];
                let x2 = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    const x3 = lastCdr(exp);
                    return Array.isArray(x3) && (x3.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1];
                if (typeof x1 === 'symbol') {
                    x1 = (0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(x1), env, options, {
                        literalSymbol: true
                    }), options);
                }
                if (typeof x2 === 'symbol') {
                    x2 = (0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(x2), env, options, {
                        literalSymbol: true
                    }), options);
                }
                if (!seen.includes(x2)) {
                    if (bindings) {
                        bindings.setLocal(x2, true, 'variable');
                    }
                    seen.push(x2);
                    specifiers.push(new estree_1.ImportSpecifier(new estree_1.Identifier(x1), new estree_1.Identifier(x2)));
                }
            }
            else {
                let x1 = exp;
                if (typeof x1 === 'symbol') {
                    x1 = (0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(x1), env, options, {
                        literalSymbol: true
                    }), options);
                }
                if (!seen.includes(x1)) {
                    if (bindings) {
                        bindings.setLocal(x1, true, 'variable');
                    }
                    seen.push(x1);
                    specifiers.push(new estree_1.ImportSpecifier(new estree_1.Identifier(x1)));
                }
            }
        }
        yExp = (Array.isArray(xExp) && (xExp.length >= 3) && (xExp[xExp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(xExp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = xExp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = xExp[xExp.length - 1];
                }
                else {
                    result = xExp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : xExp[1];
    }
    else {
        if (typeof xExp === 'symbol') {
            xExp = (0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(xExp), env, options, {
                literalSymbol: true
            }), options);
        }
        specifiers = [esModuleInterop ? new estree_1.ImportDefaultSpecifier(new estree_1.Identifier(xExp)) : new estree_1.ImportNamespaceSpecifier(new estree_1.Identifier(xExp))];
    }
    if (typeof yExp === 'symbol') {
        yExp = (0, printer_1.printEstree)(compileSymbol((0, rose_1.makeRose)(yExp), env, options, {
            literalSymbol: true
        }), options);
    }
    src = new estree_1.Literal(yExp);
    if (bindings && (typeof xExp === 'symbol')) {
        bindings.setLocal(xExp, true, 'variable');
    }
    if (Array.isArray(specifiers) && (specifiers.length === 0)) {
        return emptyProgram();
    }
    else {
        return new estree_1.ImportDeclaration(specifiers, src);
    }
}
compileRequire.lispSource = [Symbol.for('define'), [Symbol.for('compile-require'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('es-module-interop'), [Symbol.for('oget'), Symbol.for('options'), 'esModuleInterop']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('x-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('x-exp'), [Symbol.for('send'), Symbol.for('x-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('y-node'), [Symbol.for('or'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('x-node')]], [Symbol.for('define'), Symbol.for('y-exp'), [Symbol.for('send'), Symbol.for('y-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('seen'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('src'), Symbol.for('js/null')], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('x-exp'), [Symbol.for('quote'), Symbol.for('only-in')]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('x-node'), Symbol.for('drop'), 2]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('x2'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x2')], [Symbol.for('set!'), Symbol.for('x2'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x2')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x2'), Symbol.for('seen')], [Symbol.for('when'), Symbol.for('bindings'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('x2'), Symbol.for('#t'), 'variable']], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x2')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ImportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x2')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x1'), Symbol.for('seen')], [Symbol.for('when'), Symbol.for('bindings'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('x1'), Symbol.for('#t'), 'variable']], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x1')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ImportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')]]]]]]], [Symbol.for('set!'), Symbol.for('y-exp'), [Symbol.for('second'), Symbol.for('x-exp')]]], [Symbol.for('else'), [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x-exp')], [Symbol.for('set!'), Symbol.for('x-exp'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x-exp')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('specifiers'), [Symbol.for('list'), [Symbol.for('if'), Symbol.for('es-module-interop'), [Symbol.for('new'), Symbol.for('ImportDefaultSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x-exp')]], [Symbol.for('new'), Symbol.for('ImportNamespaceSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x-exp')]]]]]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('y-exp')], [Symbol.for('set!'), Symbol.for('y-exp'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('y-exp')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('src'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('y-exp')]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('bindings'), [Symbol.for('symbol?'), Symbol.for('x-exp')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('x-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('specifiers')], [Symbol.for('empty-program')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ImportDeclaration'), Symbol.for('specifiers'), Symbol.for('src')]]]];
/**
 * Compile a `(set! ...)` expression.
 */
function compileSet(node, env, options = {}) {
    const expressionType = options['expressionType'];
    const symNode = node.get(1);
    const symExp = symNode.getValue();
    let valNode = node.get(2);
    let valExp = valNode.getValue();
    if ((0, util_1.formp)(valExp, procedures_1.add_, env) && (((((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[1]) === symExp) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 2;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[2]) === 1)) || ((((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 2;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[2]) === symExp) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[1]) === 1)))) {
        valExp = [Symbol.for('add1'), symExp];
        valNode = (0, rose_1.makeRose)(valExp);
    }
    else if ((0, util_1.formp)(valExp, procedures_1.sub_, env) && (((((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[1]) === symExp) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 2;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[2]) === 1)) || ((((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 2;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[2]) === symExp) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[1]) === 1)))) {
        valExp = [Symbol.for('sub1'), symExp];
        valNode = (0, rose_1.makeRose)(valExp);
    }
    let result = '';
    if ((0, util_1.formp)(valExp, procedures_1.add1_, env) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[1]) === symExp)) {
        result = new estree_1.UpdateExpression('++', compileExpression(symNode, env, options), (expressionType === 'return') || (expressionType !== 'statement'));
    }
    else if ((0, util_1.formp)(valExp, procedures_1.sub1_, env) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(valExp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = valExp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = valExp[valExp.length - 1];
            }
            else {
                result = valExp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : valExp[1]) === symExp)) {
        result = new estree_1.UpdateExpression('--', compileExpression(symNode, env, options), (expressionType === 'return') || (expressionType !== 'statement'));
    }
    else {
        result = new estree_1.AssignmentExpression('=', (typeof symNode.getValue() === 'symbol') ? compileSymbol(symNode, env, makeExpressionOptions(options)) : compileExpression(symNode, env, options), compileExpression(valNode, env, options));
    }
    return makeExpressionOrStatement(result, options);
}
compileSet.lispSource = [Symbol.for('define'), [Symbol.for('compile-set'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('sym-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('sym-exp'), [Symbol.for('send'), Symbol.for('sym-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('val-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('val-exp'), [Symbol.for('send'), Symbol.for('val-node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('add_'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], 1]], [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], 1]]]], [Symbol.for('set!'), Symbol.for('val-exp'), [Symbol.for('quasiquote'), [Symbol.for('add1'), [Symbol.for('unquote'), Symbol.for('sym-exp')]]]], [Symbol.for('set!'), Symbol.for('val-node'), [Symbol.for('make-rose'), Symbol.for('val-exp')]]], [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('sub_'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], 1]], [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], 1]]]], [Symbol.for('set!'), Symbol.for('val-exp'), [Symbol.for('quasiquote'), [Symbol.for('sub1'), [Symbol.for('unquote'), Symbol.for('sym-exp')]]]], [Symbol.for('set!'), Symbol.for('val-node'), [Symbol.for('make-rose'), Symbol.for('val-exp')]]]], [Symbol.for('define'), Symbol.for('result'), ''], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('add1_'), Symbol.for('env')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('UpdateExpression'), '++', [Symbol.for('compile-expression'), Symbol.for('sym-node'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement']]]]]], [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('sub1_'), Symbol.for('env')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('UpdateExpression'), '--', [Symbol.for('compile-expression'), Symbol.for('sym-node'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement']]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('sym-node'), Symbol.for('get-value')]], [Symbol.for('compile-symbol'), Symbol.for('sym-node'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('sym-node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('val-node'), Symbol.for('env'), Symbol.for('options')]]]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];
/**
 * Compile a string expression.
 */
function compileString(node, env, options = {}) {
    const str = node.getValue();
    if (str.match(new RegExp('\\n'))) {
        let lines = str.split(new RegExp('^', 'gm'));
        if (lines.length <= 1) {
            return compileAtom(node, env, options);
        }
        else {
            // TODO: We could compile to a template literal instead.
            // We just have to take care to escape it properly.
            return compileRose((0, rose_1.transferComments)(node, (0, rose_1.makeRose)([Symbol.for('string-append'), ...lines], node)), env, options);
        }
    }
    else {
        return compileAtom(node, env, options);
    }
}
compileString.lispSource = [Symbol.for('define'), [Symbol.for('compile-string'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n'], Symbol.for('str')], [Symbol.for('define'), Symbol.for('lines'), [Symbol.for('string-split'), Symbol.for('str'), [Symbol.for('regexp'), '^', 'gm']]], [Symbol.for('cond'), [[Symbol.for('<='), [Symbol.for('array-list-length'), Symbol.for('lines')], 1], [Symbol.for('compile-atom'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-rose'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('string-append'), [Symbol.for('unquote-splicing'), Symbol.for('lines')]]], Symbol.for('node')]], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('compile-atom'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(- ...)` expression.
 */
function compileSub(node, env, options = {}) {
    let exp = node.getValue();
    if (exp.length === 2) {
        const num = node.get(1);
        const numCompiled = compileExpression(num, env, options);
        return makeExpressionOrStatement(new estree_1.UnaryExpression('-', true, numCompiled), options);
    }
    else {
        return compileBinaryExpression(node, env, options, {
            identity: 0,
            operator: '-'
        });
    }
}
compileSub.lispSource = [Symbol.for('define'), [Symbol.for('compile-sub'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('define'), Symbol.for('num'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('num-compiled'), [Symbol.for('compile-expression'), Symbol.for('num'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('UnaryExpression'), '-', Symbol.for('#t'), Symbol.for('num-compiled')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 0, 'operator', '-']]]]];
/**
 * Compile a variable expression.
 */
function compileVariable(node, env, options = {}) {
    const compilationMappingEnvironment = options['compilationMappingEnvironment'];
    const literalSymbol = options['literalSymbol'];
    const quotedSymbol = options['quotedSymbol'];
    const currentModule = options['currentModule'];
    let exp = node.getValue();
    if (!(quotedSymbol || literalSymbol || (env.has(exp) && (compilationMappingEnvironment.getType(env.get(exp)) === 'variable')))) {
        if (shouldInlineP(exp, env, options)) {
            if (currentModule) {
                addReferencedSymbol(exp, env, options);
            }
            else {
                // Inlined expression. The symbol references a value
                // that is defined in the language environment.
                // Create an expression that will evaluate to this
                // value and compile that.
                return makeExpressionOrStatement(compileExpression((0, rose_1.makeRose)(makeInlinedValue(exp, env, options)), env, options), options);
            }
        }
    }
    return makeExpressionOrStatement(compileSymbol(node, env, options), options);
}
compileVariable.lispSource = [Symbol.for('define'), [Symbol.for('compile-variable'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define'), Symbol.for('literal-symbol'), [Symbol.for('oget'), Symbol.for('options'), 'literalSymbol']], [Symbol.for('define'), Symbol.for('quoted-symbol'), [Symbol.for('oget'), Symbol.for('options'), 'quotedSymbol']], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('oget'), Symbol.for('options'), 'currentModule']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('unless'), [Symbol.for('or'), Symbol.for('quoted-symbol'), Symbol.for('literal-symbol'), [Symbol.for('and'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('exp')], [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('compilation-mapping-environment'), Symbol.for('get-type'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('exp')]], 'variable']]], [Symbol.for('when'), [Symbol.for('should-inline?'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('cond'), [Symbol.for('current-module'), [Symbol.for('add-referenced-symbol'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('return'), [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('make-inlined-value'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]]]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-symbol'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]];
/**
 * Compile a symbol expression.
 */
function compileSymbol(node, env, options = {}, settings = {}) {
    // TODO: Better handling of gensym'ed symbols.
    const literalSymbolOption = settings['literalSymbol'] || false;
    let quotedSymbolOption = settings['quotedSymbol'];
    const compileEnvironmentOption = options['compileEnvironment'];
    const camelCaseOption = options['camelCase'];
    let bindings = options['bindings'];
    let exp = node.getValue();
    const gensymedSymbol = (typeof exp === 'symbol') && (exp !== Symbol.for(exp.description));
    const str = exp.description;
    // Keyword symbols (i.e., symbols beginning with `:`,
    // e.g., `:foo`) are auto-quoted.
    if (str.match(new RegExp('^:'))) {
        quotedSymbolOption = true;
    }
    if (quotedSymbolOption) {
        return compileExpression((0, rose_1.makeRose)([Symbol.for('string->symbol'), str]), env, options);
    }
    else if (literalSymbolOption) {
        let name = makeJsIdentifierString(str, options);
        return new estree_1.Identifier(name);
    }
    else if (compilationVariablesEnv.has(exp)) {
        return compilationVariablesEnv.get(exp);
    }
    else if (str === 'this') {
        return new estree_1.ThisExpression();
    }
    else if (gensymedSymbol) {
        let gensymMap = options['gensymMap'];
        if (!gensymMap) {
            gensymMap = new Map();
            options['gensymMap'] = gensymMap;
        }
        if (gensymMap.has(exp)) {
            let [gensymName, name, i] = gensymMap.get(exp);
            const identifier = new estree_1.Identifier(gensymName);
            return identifier;
        }
        else {
            let name = makeJsIdentifierString(str, options);
            let gensymName = name;
            let i = 1;
            let regularSym = Symbol.for(gensymName);
            while (bindings.has(regularSym)) {
                gensymName = name + i + '';
                regularSym = Symbol.for(gensymName);
                i++;
            }
            const identifier = new estree_1.Identifier(gensymName);
            const entry = [gensymName, name, i];
            gensymMap.set(exp, entry);
            bindings.setLocal(regularSym, true);
            return identifier;
        }
    }
    else {
        let name = makeJsIdentifierString(str, options);
        return new estree_1.Identifier(name);
    }
}
compileSymbol.lispSource = [Symbol.for('define'), [Symbol.for('compile-symbol'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('literal-symbol-option'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('settings'), 'literalSymbol'], Symbol.for('#f')]], [Symbol.for('define'), Symbol.for('quoted-symbol-option'), [Symbol.for('oget'), Symbol.for('settings'), 'quotedSymbol']], [Symbol.for('define'), Symbol.for('compile-environment-option'), [Symbol.for('oget'), Symbol.for('options'), 'compileEnvironment']], [Symbol.for('define'), Symbol.for('camel-case-option'), [Symbol.for('oget'), Symbol.for('options'), 'camelCase']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('gensymed-symbol'), [Symbol.for('gensym?'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('symbol->string'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^:'], Symbol.for('str')], [Symbol.for('set!'), Symbol.for('quoted-symbol-option'), Symbol.for('#t')]], [Symbol.for('cond'), [Symbol.for('quoted-symbol-option'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('string->symbol'), [Symbol.for('unquote'), Symbol.for('str')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('literal-symbol-option'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('name')]], [[Symbol.for('send'), Symbol.for('compilation-variables-env'), Symbol.for('has'), Symbol.for('exp')], [Symbol.for('send'), Symbol.for('compilation-variables-env'), Symbol.for('get'), Symbol.for('exp')]], [[Symbol.for('eq?'), Symbol.for('str'), 'this'], [Symbol.for('new'), Symbol.for('ThisExpression')]], [Symbol.for('gensymed-symbol'), [Symbol.for('define'), Symbol.for('gensym-map'), [Symbol.for('oget'), Symbol.for('options'), 'gensymMap']], [Symbol.for('unless'), Symbol.for('gensym-map'), [Symbol.for('set!'), Symbol.for('gensym-map'), [Symbol.for('make-hash')]], [Symbol.for('oset!'), Symbol.for('options'), 'gensymMap', Symbol.for('gensym-map')]], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('gensym-map'), Symbol.for('exp')], [Symbol.for('define-values'), [Symbol.for('gensym-name'), Symbol.for('name'), Symbol.for('i')], [Symbol.for('hash-ref'), Symbol.for('gensym-map'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('gensym-name')]], Symbol.for('identifier')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('gensym-name'), Symbol.for('name')], [Symbol.for('define'), Symbol.for('i'), 1], [Symbol.for('define'), Symbol.for('regular-sym'), [Symbol.for('string->symbol'), Symbol.for('gensym-name')]], [Symbol.for('while'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('regular-sym')], [Symbol.for('set!'), Symbol.for('gensym-name'), [Symbol.for('string-append'), Symbol.for('name'), [Symbol.for('number->string'), Symbol.for('i')]]], [Symbol.for('set!'), Symbol.for('regular-sym'), [Symbol.for('string->symbol'), Symbol.for('gensym-name')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('define'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('gensym-name')]], [Symbol.for('define'), Symbol.for('entry'), [Symbol.for('list'), Symbol.for('gensym-name'), Symbol.for('name'), Symbol.for('i')]], [Symbol.for('hash-set!'), Symbol.for('gensym-map'), Symbol.for('exp'), Symbol.for('entry')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('regular-sym'), Symbol.for('#t')], Symbol.for('identifier')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('name')]]]];
/**
 * Transform a string to a valid JavaScript identifier string.
 */
function makeJsIdentifierString(str, options = {}) {
    // TODO: This function could benefit from memoization.
    // Need to use a custom memoization map to handle `options`,
    // though... an equality map using `equal?` should suffice.
    const compileEnvironmentOption = options['compileEnvironment'];
    // FIXME: Kludge.
    if (!compileEnvironmentOption) {
        return str;
    }
    const camelCaseOption = options['camelCase'];
    let result = makeJsIdentifierStringHelper(str);
    if (camelCaseOption === false) {
        return (0, util_1.kebabCaseToSnakeCase)(result);
    }
    else {
        return (0, util_1.kebabCaseToCamelCase)(result);
    }
}
makeJsIdentifierString.lispSource = [Symbol.for('define'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('compile-environment-option'), [Symbol.for('oget'), Symbol.for('options'), 'compileEnvironment']], [Symbol.for('unless'), Symbol.for('compile-environment-option'), [Symbol.for('return'), Symbol.for('str')]], [Symbol.for('define'), Symbol.for('camel-case-option'), [Symbol.for('oget'), Symbol.for('options'), 'camelCase']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('make-js-identifier-string-helper'), Symbol.for('str')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('camel-case-option'), Symbol.for('#f')], [Symbol.for('kebab-case->snake-case'), Symbol.for('result')]], [Symbol.for('else'), [Symbol.for('kebab-case->camel-case'), Symbol.for('result')]]]];
/**
 * Helper function for `make-js-identifier-string`.
 */
function makeJsIdentifierStringHelper(str) {
    let result = str.replace(new RegExp('^\\+$', 'g'), '_add').replace(new RegExp('^-$', 'g'), '_sub').replace(new RegExp('^\\*$', 'g'), '_mul').replace(new RegExp('^/$', 'g'), '_div').replace(new RegExp('%', 'g'), '').replace(new RegExp('/', 'g'), '-').replace(new RegExp('!', 'g'), '-x').replace(new RegExp(':', 'g'), '-').replace(new RegExp('->', 'g'), '-to-').replace(new RegExp('\\+', 'g'), '_').replace(new RegExp('\\*$', 'g'), '-star').replace(new RegExp('\\*', 'g'), 'star-');
    if (result.match(new RegExp('-', 'g'))) {
        result = result.replace(new RegExp('\\?', 'g'), '-p');
    }
    else {
        result = result.replace(new RegExp('\\?', 'g'), 'p');
    }
    return result;
}
makeJsIdentifierStringHelper.lispSource = [Symbol.for('define'), [Symbol.for('make-js-identifier-string-helper'), Symbol.for('str')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\+$', 'g'], Symbol.for('_'), '_add'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^-$', 'g'], Symbol.for('_'), '_sub'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\*$', 'g'], Symbol.for('_'), '_mul'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^/$', 'g'], Symbol.for('_'), '_div'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '%', 'g'], Symbol.for('_'), ''], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '/', 'g'], Symbol.for('_'), '-'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '!', 'g'], Symbol.for('_'), '-x'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ':', 'g'], Symbol.for('_'), '-'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '->', 'g'], Symbol.for('_'), '-to-'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\+', 'g'], Symbol.for('_'), '_'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\*$', 'g'], Symbol.for('_'), '-star'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\*', 'g'], Symbol.for('_'), 'star-']]], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '-', 'g'], Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\?', 'g'], Symbol.for('result'), '-p']]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\?', 'g'], Symbol.for('result'), 'p']]]], Symbol.for('result')];
/**
 * Whether something is an equality expression.
 */
function isEqualityExpression(exp, env) {
    return (0, util_1.formp)(exp, equal_1.eqp_, env) || (0, util_1.formp)(exp, equal_1.eqvp_, env) || (0, util_1.formp)(exp, equal_1.equalp_, env);
}
isEqualityExpression.lispSource = [Symbol.for('define'), [Symbol.for('is-equality-expression'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('eq?_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('eqv?_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('equal?_'), Symbol.for('env')]]];
/**
 * Whether something is a `let` or `let*` expression.
 */
function isLetExpression(exp, env) {
    return (0, util_1.formp)(exp, letStar_, env);
}
isLetExpression.lispSource = [Symbol.for('define'), [Symbol.for('is-let-expression'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('let-star_'), Symbol.for('env')]];
/**
 * Compile a `(for ...)` expression.
 */
function compileFor(node, env, options = {}) {
    // TODO: Implement `compile-js-for` and implement this
    // in terms of that?
    const inheritedOptions = Object.assign({}, options);
    let bindings = inheritedOptions['bindings'];
    bindings = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
    inheritedOptions['bindings'] = bindings;
    const language = options['language'];
    const declsNode = node.get(1);
    const decls = declsNode.getValue();
    const bodyNodes = node.drop(2);
    let bodyNode = (0, rose_1.beginWrapRoseSmart1)(bodyNodes);
    const decl1Node = declsNode.get(0);
    const decl1 = decl1Node.getValue();
    const symNode = decl1Node.get(0);
    const symExp = symNode.getValue();
    const valsNode = decl1Node.get(1);
    const valsExp = valsNode.getValue();
    if ((0, util_1.formp)(valsExp, procedures_1.range_, env)) {
        let start = (valsExp.length >= 2) ? ((Array.isArray(valsExp) && (valsExp.length >= 3) && (valsExp[valsExp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(valsExp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = valsExp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = valsExp[valsExp.length - 1];
                }
                else {
                    result = valsExp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : valsExp[1]) : undefined;
        let end = (valsExp.length >= 3) ? ((Array.isArray(valsExp) && (valsExp.length >= 3) && (valsExp[valsExp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(valsExp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 2;
            let result = valsExp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = valsExp[valsExp.length - 1];
                }
                else {
                    result = valsExp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : valsExp[2]) : undefined;
        let step = (4 >= valsExp.length) ? ((Array.isArray(valsExp) && (valsExp.length >= 3) && (valsExp[valsExp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(valsExp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 3;
            let result = valsExp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = valsExp[valsExp.length - 1];
                }
                else {
                    result = valsExp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : valsExp[3]) : undefined;
        start = (end === undefined) ? 0 : start;
        end = (end === undefined) ? start : end;
        // If `start`, `end` or `step` is a function call,
        // then rewrite the expression to a `let` expression,
        // storing the values in local variables so that
        // the function is only called once.
        if (Array.isArray(start) || Array.isArray(end) || Array.isArray(step)) {
            const startVar = Symbol('_start');
            const endVar = Symbol('_end');
            const stepVar = Symbol('_step');
            return compileRose((0, rose_1.makeRose)([Symbol.for('let'), [...(Array.isArray(start) ? [[startVar, start]] : []), ...(Array.isArray(end) ? [[endVar, end]] : []), ...(Array.isArray(step) ? [[stepVar, step]] : [])], [Symbol.for('for'), [[symExp, [Symbol.for('range'), Array.isArray(start) ? startVar : start, Array.isArray(end) ? endVar : end, ...(step ? (Array.isArray(step) ? [stepVar] : [step]) : [])]]], ...bodyNodes]], node), env, options);
        }
        // Otherwise, proceed to create a `for` loop.
        step = step || 1;
        const init = compileStatement((0, rose_1.makeRose)([Symbol.for('define'), symExp, start]), env, inheritedOptions);
        let test;
        let update;
        if (Number.isFinite(step)) {
            if (step < 0) {
                test = compileExpression((0, rose_1.makeRose)([Symbol.for('>'), symExp, end]), env, inheritedOptions);
                update = compileStatement((0, rose_1.makeRose)([Symbol.for('set!'), symExp, [Symbol.for('-'), symExp, Math.abs(step)]]), env, inheritedOptions);
            }
            else {
                test = compileExpression((0, rose_1.makeRose)([Symbol.for('<'), symExp, end]), env, inheritedOptions);
                update = compileStatement((0, rose_1.makeRose)([Symbol.for('set!'), symExp, [Symbol.for('+'), symExp, step]]), env, inheritedOptions);
            }
        }
        else {
            test = compileExpression((0, rose_1.makeRose)([Symbol.for('if'), [Symbol.for('<'), step, 0], [Symbol.for('>'), symExp, end], [Symbol.for('<'), symExp, end]]), env, inheritedOptions);
            update = compileStatement((0, rose_1.makeRose)([Symbol.for('set!'), symExp, [Symbol.for('+'), symExp, step]]), env, inheritedOptions);
        }
        if ((0, estree_1.estreeTypeP)(update, 'ExpressionStatement')) {
            update = update.expression;
        }
        const body = wrapInBlockStatement(compileStatement(bodyNode, env, inheritedOptions));
        return new estree_1.ForStatement(init, test, update, body);
    }
    else {
        let left = compileExpression((0, rose_1.makeRose)([Symbol.for('define'), symExp]), env, inheritedOptions);
        let right = compileExpression(valsNode, env, inheritedOptions);
        const body = wrapInBlockStatement(compileStatement(bodyNode, env, inheritedOptions));
        return new estree_1.ForOfStatement(left, right, body);
    }
}
compileFor.lispSource = [Symbol.for('define'), [Symbol.for('compile-for'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('decls-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('decls'), [Symbol.for('send'), Symbol.for('decls-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('body-node'), [Symbol.for('begin-wrap-rose-smart-1'), Symbol.for('body-nodes')]], [Symbol.for('define'), Symbol.for('decl1-node'), [Symbol.for('send'), Symbol.for('decls-node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('decl1'), [Symbol.for('send'), Symbol.for('decl1-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('sym-node'), [Symbol.for('send'), Symbol.for('decl1-node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('sym-exp'), [Symbol.for('send'), Symbol.for('sym-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('vals-node'), [Symbol.for('send'), Symbol.for('decl1-node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('vals-exp'), [Symbol.for('send'), Symbol.for('vals-node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('form?'), Symbol.for('vals-exp'), Symbol.for('range_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('start'), [Symbol.for('if'), [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('vals-exp')], 2], [Symbol.for('second'), Symbol.for('vals-exp')], Symbol.for('undefined')]], [Symbol.for('define'), Symbol.for('end'), [Symbol.for('if'), [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('vals-exp')], 3], [Symbol.for('third'), Symbol.for('vals-exp')], Symbol.for('undefined')]], [Symbol.for('define'), Symbol.for('step'), [Symbol.for('if'), [Symbol.for('>='), 4, [Symbol.for('array-list-length'), Symbol.for('vals-exp')]], [Symbol.for('fourth'), Symbol.for('vals-exp')], Symbol.for('undefined')]], [Symbol.for('set!'), Symbol.for('start'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('end'), Symbol.for('undefined')], 0, Symbol.for('start')]], [Symbol.for('set!'), Symbol.for('end'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('end'), Symbol.for('undefined')], Symbol.for('start'), Symbol.for('end')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('array?'), Symbol.for('start')], [Symbol.for('array?'), Symbol.for('end')], [Symbol.for('array?'), Symbol.for('step')]], [Symbol.for('define'), Symbol.for('start-var'), [Symbol.for('gensym'), '_start']], [Symbol.for('define'), Symbol.for('end-var'), [Symbol.for('gensym'), '_end']], [Symbol.for('define'), Symbol.for('step-var'), [Symbol.for('gensym'), '_step']], [Symbol.for('return'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('start')], [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('start-var')], [Symbol.for('unquote'), Symbol.for('start')]]]], [Symbol.for('quote'), []]]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('end')], [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('end-var')], [Symbol.for('unquote'), Symbol.for('end')]]]], [Symbol.for('quote'), []]]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('step')], [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('step-var')], [Symbol.for('unquote'), Symbol.for('step')]]]], [Symbol.for('quote'), []]]]], [Symbol.for('for'), [[[Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('range'), [Symbol.for('unquote'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('start')], Symbol.for('start-var'), Symbol.for('start')]], [Symbol.for('unquote'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('end')], Symbol.for('end-var'), Symbol.for('end')]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), Symbol.for('step'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('step')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('step-var')]]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('step')]]]], [Symbol.for('quote'), []]]]]]], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('step'), [Symbol.for('or'), Symbol.for('step'), 1]], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('start')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('define'), Symbol.for('test')], [Symbol.for('define'), Symbol.for('update')], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('step')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('step'), 0], [Symbol.for('set!'), Symbol.for('test'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('>'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('Math'), Symbol.for('abs'), Symbol.for('step')]]]]]], Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('test'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('step')]]]]], Symbol.for('env'), Symbol.for('inherited-options')]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('test'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('if'), [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('step')], 0], [Symbol.for('>'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]], [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('step')]]]]], Symbol.for('env'), Symbol.for('inherited-options')]]]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('update'), 'ExpressionStatement'], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('update')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-statement'), Symbol.for('body-node'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('new'), Symbol.for('ForStatement'), Symbol.for('init'), Symbol.for('test'), Symbol.for('update'), Symbol.for('body')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('left'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('sym-exp')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('compile-expression'), Symbol.for('vals-node'), Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-statement'), Symbol.for('body-node'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('new'), Symbol.for('ForOfStatement'), Symbol.for('left'), Symbol.for('right'), Symbol.for('body')]]]];
/**
 * Compile a `(break)` expression.
 */
function compileBreak(node, env, options = {}) {
    return new estree_1.BreakStatement((node.size() > 1) ? compileExpression(node.get(1), env, options) : null);
}
compileBreak.lispSource = [Symbol.for('define'), [Symbol.for('compile-break'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('BreakStatement'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('js/null')]]];
/**
 * Compile a `(continue)` expression.
 */
function compileContinue(node, env, options = {}) {
    return new estree_1.ContinueStatement((node.size() > 1) ? compileExpression(node.get(1), env, options) : null);
}
compileContinue.lispSource = [Symbol.for('define'), [Symbol.for('compile-continue'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('ContinueStatement'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('js/null')]]];
/**
 * Compile a `(js/typeof ...)` expression.
 */
function compileJsTypeof(node, env, options = {}) {
    return makeExpressionOrStatement(new estree_1.UnaryExpression('typeof', true, compileExpression(node.get(1), env, options)), options);
}
compileJsTypeof.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-typeof'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('UnaryExpression'), 'typeof', Symbol.for('#t'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];
/**
 * Compile a `(js/instanceof? ...)` expression.
 */
function compileJsInstanceof(node, env, options = {}) {
    return makeExpressionOrStatement(new estree_1.BinaryExpression('instanceof', compileExpression(node.get(1), env, options), compileExpression(node.get(2), env, options)), options);
}
compileJsInstanceof.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-instanceof'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('BinaryExpression'), 'instanceof', [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];
/**
 * Compile a `(js/in ...)` expression.
 */
function compileJsIn(node, env, options = {}) {
    return makeExpressionOrStatement(new estree_1.BinaryExpression('in', compileExpression(node.get(1), env, options), compileExpression(node.get(2), env, options)), options);
}
compileJsIn.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-in'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('BinaryExpression'), 'in', [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];
/**
 * Compile a `(new ...)` expression.
 */
function compileNew(node, env, options = {}) {
    return makeExpressionOrStatement(new estree_1.NewExpression(compileExpression(node.get(1), env, options), node.drop(2).map(function (x) {
        return compileExpression(x, env, options);
    })), options);
}
compileNew.lispSource = [Symbol.for('define'), [Symbol.for('compile-new'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('NewExpression'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]], Symbol.for('options')]];
/**
 * Compile a `(js/do-while ...)` expression.
 */
function compileJsDoWhile(node, env, options = {}) {
    const body = node.get(1);
    let test = node.get(2);
    return new estree_1.DoWhileStatement(compileExpression(test, env, options), wrapInBlockStatementSmart(compileStatementOrReturnStatement(body, env, options)));
}
compileJsDoWhile.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-do-while'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('new'), Symbol.for('DoWhileStatement'), [Symbol.for('compile-expression'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(js/while ...)` expression.
 */
function compileJsWhile(node, env, options = {}) {
    let test = node.get(1);
    const body = (0, rose_1.beginWrapRose)(node.drop(2));
    return new estree_1.WhileStatement(compileExpression(test, env, options), wrapInBlockStatementSmart(compileStatementOrReturnStatement(body, env, options)));
}
compileJsWhile.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-while'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('begin-wrap-rose'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]], [Symbol.for('new'), Symbol.for('WhileStatement'), [Symbol.for('compile-expression'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(yield ...)` expression.
 */
function compileYield(node, env, options = {}) {
    return makeExpressionOrStatement(new estree_1.YieldExpression((node.size() > 1) ? compileExpression(node.get(1), env, options) : null), options);
}
compileYield.lispSource = [Symbol.for('define'), [Symbol.for('compile-yield'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('YieldExpression'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('js/null')]], Symbol.for('options')]];
/**
 * Compile a `(throw ...)` expression.
 */
function compileThrow(node, env, options = {}) {
    return new estree_1.ThrowStatement(compileExpression(node.get(1), env, options));
}
compileThrow.lispSource = [Symbol.for('define'), [Symbol.for('compile-throw'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('ThrowStatement'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]];
/**
 * Compile a `(js/delete ...)` expression.
 */
function compileJsDelete(node, env, options = {}) {
    return new estree_1.UnaryExpression('delete', true, compileExpression(node.get(1), env, options));
}
compileJsDelete.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-delete'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('UnaryExpression'), 'delete', Symbol.for('#t'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]];
/**
 * Compile a `(return ...)` expression.
 */
function compileReturn(node, env, options = {}) {
    return new estree_1.ReturnStatement((node.size() > 1) ? compileExpression(node.get(1), env, options) : null);
}
compileReturn.lispSource = [Symbol.for('define'), [Symbol.for('compile-return'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('ReturnStatement'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('js/null')]]];
/**
 * Compile a `(js/async ...)` expression.
 */
function compileJsAsync(node, env, options = {}) {
    let result = compileExpression(node.get(1), env, options);
    if ((0, estree_1.estreeTypeP)(result, 'FunctionDeclaration') || (0, estree_1.estreeTypeP)(result, 'FunctionExpression') || (0, estree_1.estreeTypeP)(result, 'ArrowFunctionExpression')) {
        result.async = true;
        result.returnType = new estree_1.TSTypeReference(new estree_1.Identifier('Promise'), new estree_1.TSTypeParameterInstantiation([new estree_1.TSAnyKeyword()]));
    }
    return makeExpressionOrStatement(result, options);
}
compileJsAsync.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-async'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('estree-type?'), Symbol.for('result'), 'FunctionDeclaration'], [Symbol.for('estree-type?'), Symbol.for('result'), 'FunctionExpression'], [Symbol.for('estree-type?'), Symbol.for('result'), 'ArrowFunctionExpression']], [Symbol.for('set-field!'), Symbol.for('async'), Symbol.for('result'), Symbol.for('#t')], [Symbol.for('set-field!'), Symbol.for('returnType'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('TSTypeReference'), [Symbol.for('new'), Symbol.for('Identifier'), 'Promise'], [Symbol.for('new'), Symbol.for('TSTypeParameterInstantiation'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];
/**
 * Compile a `(js/await ...)` expression.
 */
function compileJsAwait(node, env, options = {}) {
    return makeExpressionOrStatement(new estree_1.AwaitExpression(compileExpression(node.get(1), env, options)), options);
}
compileJsAwait.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-await'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('AwaitExpression'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];
/**
 * Compile a `(string-append ...)` expression.
 */
function compileStringAppend(node, env, options = {}) {
    let exp = node.getValue();
    if (exp.length <= 0) {
        return compileRose('', env, options);
    }
    else if (exp.length === 2) {
        return compileRose(node.get(1), env, options);
    }
    else {
        return compileBinaryExpression(node, env, options, {
            identity: '',
            operator: '+'
        });
    }
}
compileStringAppend.lispSource = [Symbol.for('define'), [Symbol.for('compile-string-append'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<='), [Symbol.for('array-list-length'), Symbol.for('exp')], 0], [Symbol.for('compile-rose'), '', Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('compile-rose'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', '', 'operator', '+']]]]];
/**
 * Compile a `(define-class ...)` expression.
 */
function compileDefineClass(node, env, options = {}) {
    return compileClass(node, env, options);
}
compileDefineClass.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-class'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-class'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile a `(class ...)` expression.
 */
function compileClass(node, env, options = {}) {
    const inheritedOptions = Object.assign({}, options);
    const classNameNode = node.get(1);
    const className = classNameNode.getValue();
    let superClass = null;
    let id = (typeof className === 'symbol') ? new estree_1.Identifier((0, printer_1.printEstree)(compileExpression(classNameNode, env, inheritedOptions), inheritedOptions)) : null;
    let bodyNode = (id === null) ? (0, rose_1.sliceRose)(node, 1) : (0, rose_1.sliceRose)(node, 2);
    let bodyExp = bodyNode.getValue();
    let bindings = inheritedOptions['bindings'];
    bindings = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
    inheritedOptions['bindings'] = bindings;
    bindings.setLocal(Symbol.for('super'), true, 'variable');
    if (Array.isArray(bodyExp[0]) && !(0, util_1.formp)(bodyExp[0], define_, env)) {
        const superClassesNode = bodyNode.get(0);
        const superClasses = superClassesNode.getValue();
        bodyNode = (0, rose_1.sliceRose)(bodyNode, 1);
        bodyExp = bodyNode.getValue();
        if (superClasses.length > 0) {
            superClass = new estree_1.Identifier((0, printer_1.printEstree)(compileExpression((0, rose_1.makeRose)(superClasses[0]), env, inheritedOptions), inheritedOptions));
        }
    }
    const bodyDeclarations = [];
    const accessibilities = new Map();
    for (let x of bodyNode.getNodes()) {
        let exp = x.getValue();
        if ((0, util_1.taggedListP)(exp, Symbol.for('public'))) {
            accessibilities.set((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                let x1 = lastCdr(exp);
                return Array.isArray(x1) && (x1.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1], 'public');
        }
        else if ((0, util_1.taggedListP)(exp, Symbol.for('private'))) {
            accessibilities.set((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                let x1 = lastCdr(exp);
                return Array.isArray(x1) && (x1.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1], 'private');
        }
        else {
            const isInitialized = exp.length >= 3;
            let id = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                let x1 = lastCdr(exp);
                return Array.isArray(x1) && (x1.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1];
            const isMethod = Array.isArray(id);
            if (isMethod) {
                id = id[0];
            }
            const idNode = isMethod ? x.get(1).get(0) : x.get(1);
            let accessibility = accessibilities.has(id) ? accessibilities.get(id) : ((0, util_1.taggedListP)(exp, Symbol.for('define/public')) ? 'public' : 'private');
            const isGenerator = (0, util_1.taggedListP)(exp, Symbol.for('define/generator'));
            const isConstructor = isMethod && (((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                let x1 = lastCdr(exp);
                return Array.isArray(x1) && (x1.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1]).length > 0) && (id === Symbol.for('constructor'));
            if (isConstructor) {
                accessibility = 'public';
            }
            const returnType = isConstructor ? 'void' : undefined;
            const isComputed = typeof id !== 'symbol';
            const idCompiled = isComputed ? compileExpression(idNode, env, inheritedOptions) : compileSymbol(idNode, env, makeExpressionOptions(inheritedOptions));
            const initCompiled = !isInitialized ? undefined : (isMethod ? compileJsFunction(defineToLambda(x, {
                curried: false
            }), env, makeExpressionOptions(inheritedOptions), {
                generator: isGenerator,
                returnType: returnType
            }) : compileExpression(x.get(2), env, makeExpressionOptions(inheritedOptions)));
            if (isMethod) {
                const kind = isConstructor ? 'constructor' : 'method';
                let methodDefinition = new estree_1.MethodDefinition(idCompiled, initCompiled, kind, false, false, isComputed, accessibility);
                methodDefinition = transferAndCompileComments(x, methodDefinition, inheritedOptions);
                bodyDeclarations.push(methodDefinition);
            }
            else {
                let propertyDefinition = new estree_1.PropertyDefinition(idCompiled, initCompiled, false, accessibility);
                propertyDefinition = transferAndCompileComments(x, propertyDefinition, inheritedOptions);
                bodyDeclarations.push(propertyDefinition);
            }
        }
    }
    const body = new estree_1.ClassBody(bodyDeclarations);
    if (id === null) {
        return new estree_1.ClassExpression(body, superClass);
    }
    else {
        return new estree_1.ClassDeclaration(id, body, superClass);
    }
}
compileClass.lispSource = [Symbol.for('define'), [Symbol.for('compile-class'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('class-name-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('class-name'), [Symbol.for('send'), Symbol.for('class-name-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('super-class'), Symbol.for('js/null')], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('if'), [Symbol.for('symbol?'), Symbol.for('class-name')], [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), Symbol.for('class-name-node'), Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]], Symbol.for('js/null')]], [Symbol.for('define'), Symbol.for('body-node'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('js/null')], [Symbol.for('slice-rose'), Symbol.for('node'), 1], [Symbol.for('slice-rose'), Symbol.for('node'), 2]]], [Symbol.for('define'), Symbol.for('body-exp'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), [Symbol.for('quote'), Symbol.for('super')], Symbol.for('#t'), 'variable'], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('array?'), [Symbol.for('first'), Symbol.for('body-exp')]], [Symbol.for('not'), [Symbol.for('form?'), [Symbol.for('first'), Symbol.for('body-exp')], Symbol.for('define_'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('super-classes-node'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('super-classes'), [Symbol.for('send'), Symbol.for('super-classes-node'), Symbol.for('get-value')]], [Symbol.for('set!'), Symbol.for('body-node'), [Symbol.for('slice-rose'), Symbol.for('body-node'), 1]], [Symbol.for('set!'), Symbol.for('body-exp'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get-value')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('super-classes')], 0], [Symbol.for('set!'), Symbol.for('super-class'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('first'), Symbol.for('super-classes')]], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]], [Symbol.for('define'), Symbol.for('body-declarations'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('accessibilities'), [Symbol.for('make-hash')]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('public')]], [Symbol.for('hash-set!'), Symbol.for('accessibilities'), [Symbol.for('second'), Symbol.for('exp')], 'public']], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('private')]], [Symbol.for('hash-set!'), Symbol.for('accessibilities'), [Symbol.for('second'), Symbol.for('exp')], 'private']], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('is-initialized'), [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('is-method'), [Symbol.for('array?'), Symbol.for('id')]], [Symbol.for('when'), Symbol.for('is-method'), [Symbol.for('set!'), Symbol.for('id'), [Symbol.for('first'), Symbol.for('id')]]], [Symbol.for('define'), Symbol.for('id-node'), [Symbol.for('if'), Symbol.for('is-method'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]]], [Symbol.for('define'), Symbol.for('accessibility'), [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('accessibilities'), Symbol.for('id')], [Symbol.for('hash-ref'), Symbol.for('accessibilities'), Symbol.for('id')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define/public')]], 'public'], [Symbol.for('else'), 'private']]], [Symbol.for('define'), Symbol.for('is-generator'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define/generator')]]], [Symbol.for('define'), Symbol.for('is-constructor'), [Symbol.for('and'), Symbol.for('is-method'), [Symbol.for('>'), [Symbol.for('array-list-length'), [Symbol.for('second'), Symbol.for('exp')]], 0], [Symbol.for('eq?'), Symbol.for('id'), [Symbol.for('quote'), Symbol.for('constructor')]]]], [Symbol.for('when'), Symbol.for('is-constructor'), [Symbol.for('set!'), Symbol.for('accessibility'), 'public']], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('if'), Symbol.for('is-constructor'), 'void', Symbol.for('undefined')]], [Symbol.for('define'), Symbol.for('is-computed'), [Symbol.for('not'), [Symbol.for('symbol?'), Symbol.for('id')]]], [Symbol.for('define'), Symbol.for('id-compiled'), [Symbol.for('if'), Symbol.for('is-computed'), [Symbol.for('compile-expression'), Symbol.for('id-node'), Symbol.for('env'), Symbol.for('inherited-options')], [Symbol.for('compile-symbol'), Symbol.for('id-node'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('inherited-options')]]]], [Symbol.for('define'), Symbol.for('init-compiled'), [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('is-initialized')], Symbol.for('undefined')], [Symbol.for('is-method'), [Symbol.for('compile-js-function'), [Symbol.for('define->lambda'), Symbol.for('x'), [Symbol.for('js-obj'), 'curried', Symbol.for('#f')]], Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('inherited-options')], [Symbol.for('js-obj'), 'generator', Symbol.for('is-generator'), 'returnType', Symbol.for('return-type')]]], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 2], Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('inherited-options')]]]]], [Symbol.for('cond'), [Symbol.for('is-method'), [Symbol.for('define'), Symbol.for('kind'), [Symbol.for('if'), Symbol.for('is-constructor'), 'constructor', 'method']], [Symbol.for('define'), Symbol.for('method-definition'), [Symbol.for('new'), Symbol.for('MethodDefinition'), Symbol.for('id-compiled'), Symbol.for('init-compiled'), Symbol.for('kind'), Symbol.for('#f'), Symbol.for('#f'), Symbol.for('is-computed'), Symbol.for('accessibility')]], [Symbol.for('set!'), Symbol.for('method-definition'), [Symbol.for('transfer-and-compile-comments'), Symbol.for('x'), Symbol.for('method-definition'), Symbol.for('inherited-options')]], [Symbol.for('push-right!'), Symbol.for('body-declarations'), Symbol.for('method-definition')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('property-definition'), [Symbol.for('new'), Symbol.for('PropertyDefinition'), Symbol.for('id-compiled'), Symbol.for('init-compiled'), Symbol.for('#f'), Symbol.for('accessibility')]], [Symbol.for('set!'), Symbol.for('property-definition'), [Symbol.for('transfer-and-compile-comments'), Symbol.for('x'), Symbol.for('property-definition'), Symbol.for('inherited-options')]], [Symbol.for('push-right!'), Symbol.for('body-declarations'), Symbol.for('property-definition')]]]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('new'), Symbol.for('ClassBody'), Symbol.for('body-declarations')]], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('js/null')], [Symbol.for('new'), Symbol.for('ClassExpression'), Symbol.for('body'), Symbol.for('super-class')], [Symbol.for('new'), Symbol.for('ClassDeclaration'), Symbol.for('id'), Symbol.for('body'), Symbol.for('super-class')]]];
/**
 * Compile a `(js-obj ...)` expression.
 */
function compileJsObj(node, env, options = {}) {
    let exp = node.getValue();
    const properties = [];
    const _end = exp.length;
    for (let i = 1; i < _end; i = i + 2) {
        const keyNode = node.get(i);
        const keyValue = keyNode.getValue();
        let compiledKey = compileExpression(keyNode, env, options);
        const compiledValue = compileExpression(node.get(i + 1), env, options);
        const computed = typeof keyNode.getValue() !== 'string';
        let match;
        if ((typeof keyValue === 'string') && keyValue.match(new RegExp('^[a-z]+$', 'i'))) {
            compiledKey = new estree_1.Identifier(keyValue);
        }
        properties.push(new estree_1.Property(compiledKey, compiledValue, computed));
    }
    return makeExpressionOrStatement(new estree_1.ObjectExpression(properties), options);
}
compileJsObj.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-obj'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('exp')], 2]]], [Symbol.for('define'), Symbol.for('key-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('key-value'), [Symbol.for('send'), Symbol.for('key-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('compiled-key'), [Symbol.for('compile-expression'), Symbol.for('key-node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('compiled-value'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('not'), [Symbol.for('string?'), [Symbol.for('send'), Symbol.for('key-node'), Symbol.for('get-value')]]]], [Symbol.for('define'), Symbol.for('match')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('string?'), Symbol.for('key-value')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^[a-z]+$', 'i'], Symbol.for('key-value')]], [Symbol.for('set!'), Symbol.for('compiled-key'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('key-value')]]], [Symbol.for('push-right!'), Symbol.for('properties'), [Symbol.for('new'), Symbol.for('Property'), Symbol.for('compiled-key'), Symbol.for('compiled-value'), Symbol.for('computed')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ObjectExpression'), Symbol.for('properties')], Symbol.for('options')]];
/**
 * Compile a `(js-obj-append ...)` expression.
 */
function compileJsObjAppend(node, env, options = {}) {
    const args = node.drop(1);
    const properties = [];
    for (let arg of args) {
        let exp = compileExpression(arg, env, options);
        if (exp instanceof estree_1.ObjectExpression) {
            for (let prop of exp.properties) {
                properties.push(prop);
            }
        }
        else {
            properties.push(new estree_1.SpreadElement(exp));
        }
    }
    return makeExpressionOrStatement(new estree_1.ObjectExpression(properties), options);
}
compileJsObjAppend.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-obj-append'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('compile-expression'), Symbol.for('arg'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('ObjectExpression')], [Symbol.for('for'), [[Symbol.for('prop'), [Symbol.for('get-field'), Symbol.for('properties'), Symbol.for('exp')]]], [Symbol.for('push-right!'), Symbol.for('properties'), Symbol.for('prop')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('properties'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('exp')]]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ObjectExpression'), Symbol.for('properties')], Symbol.for('options')]];
/**
 * Compile a `(js/tag ...)` expression.
 */
function compileJsTaggedTemplate(node, env, options = {}) {
    const tag = node.get(1);
    const tagCompiled = compileRose(tag, env, options);
    const str = node.get(2);
    const strExp = str.getValue();
    return new estree_1.TaggedTemplateExpression(tagCompiled, new estree_1.TemplateLiteral([new estree_1.TemplateElement(true, strExp)]));
}
compileJsTaggedTemplate.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-tagged-template'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('tag'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('tag-compiled'), [Symbol.for('compile-rose'), Symbol.for('tag'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('str-exp'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('get-value')]], [Symbol.for('new'), Symbol.for('TaggedTemplateExpression'), Symbol.for('tag-compiled'), [Symbol.for('new'), Symbol.for('TemplateLiteral'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('TemplateElement'), Symbol.for('#t'), Symbol.for('str-exp')]]]]];
/**
 * Compile an `(append ...)` expression.
 */
function compileAppend(node, env, options = {}) {
    const elements = [];
    for (let x of node.drop(1)) {
        let el = compileExpression(x, env, options);
        if ((0, estree_1.estreeTypeP)(el, 'ArrayExpression')) {
            if (el.elements.length === 0) {
            }
            else if (el.elements.length === 1) {
                // Ignore empty arrays.
                // Unwrap singleton arrays.
                elements.push(el.elements[0]);
            }
            else {
                elements.push(new estree_1.SpreadElement(el));
            }
        }
        else {
            elements.push(new estree_1.SpreadElement(el));
        }
    }
    return makeExpressionOrStatement(new estree_1.ArrayExpression(elements), options);
}
compileAppend.lispSource = [Symbol.for('define'), [Symbol.for('compile-append'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('elements'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]], [Symbol.for('define'), Symbol.for('el'), [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('el'), 'ArrayExpression'], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('el')]], 0]], [[Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('el')]], 1], [Symbol.for('push-right!'), Symbol.for('elements'), [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('el')], 0]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('elements'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('el')]]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('elements'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('el')]]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ArrayExpression'), Symbol.for('elements')], Symbol.for('options')]];
/**
 * Compile a `(js/try ...)` expression.
 */
function compileJsTry(node, env, options = {}) {
    const bodyExps = [];
    let catchClause = null;
    let finallyClause = null;
    for (let x of node.drop(1)) {
        if ((0, util_1.taggedListP)(x, Symbol.for('catch'))) {
            catchClause = x;
        }
        else if ((0, util_1.taggedListP)(x, Symbol.for('finally'))) {
            finallyClause = x;
        }
        else {
            bodyExps.push(x);
        }
    }
    const block = wrapInBlockStatementSmart(compileStatementOrReturnStatement((0, rose_1.makeRose)([Symbol.for('begin'), ...bodyExps]), env, options));
    let handler = null;
    if (catchClause) {
        // TODO: Permit destructuring.
        let param = catchClause.get(1);
        const paramExp = param.getValue();
        const paramCompiled = (paramExp === Symbol.for('_')) ? null : compileExpression(param, env, options);
        const body = (0, rose_1.makeRose)([Symbol.for('begin'), ...catchClause.drop(2)]);
        const bodyCompiled = compileStatementOrReturnStatement(body, env, options);
        handler = new estree_1.CatchClause(paramCompiled, bodyCompiled);
    }
    const finalizer = finallyClause ? wrapInBlockStatementSmart(compileStatementOrReturnStatement((0, rose_1.makeRose)([Symbol.for('begin'), ...finallyClause.drop(1)]), env, options)) : null;
    return makeExpressionOrStatement(new estree_1.TryStatement(block, handler, finalizer), options);
}
compileJsTry.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-try'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('body-exps'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('catch-clause'), Symbol.for('js/null')], [Symbol.for('define'), Symbol.for('finally-clause'), Symbol.for('js/null')], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('catch')]], [Symbol.for('set!'), Symbol.for('catch-clause'), Symbol.for('x')]], [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('finally')]], [Symbol.for('set!'), Symbol.for('finally-clause'), Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('body-exps'), Symbol.for('x')]]]], [Symbol.for('define'), Symbol.for('block'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body-exps')]]]], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('handler'), Symbol.for('js/null')], [Symbol.for('when'), Symbol.for('catch-clause'), [Symbol.for('define'), Symbol.for('param'), [Symbol.for('send'), Symbol.for('catch-clause'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('param-exp'), [Symbol.for('send'), Symbol.for('param'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('param-compiled'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('param-exp'), [Symbol.for('quote'), Symbol.for('_')]], Symbol.for('js/null'), [Symbol.for('compile-expression'), Symbol.for('param'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('catch-clause'), Symbol.for('drop'), 2]]]]]], [Symbol.for('define'), Symbol.for('body-compiled'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('handler'), [Symbol.for('new'), Symbol.for('CatchClause'), Symbol.for('param-compiled'), Symbol.for('body-compiled')]]], [Symbol.for('define'), Symbol.for('finalizer'), [Symbol.for('if'), Symbol.for('finally-clause'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('finally-clause'), Symbol.for('drop'), 1]]]]], Symbol.for('env'), Symbol.for('options')]], Symbol.for('js/null')]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('TryStatement'), Symbol.for('block'), Symbol.for('handler'), Symbol.for('finalizer')], Symbol.for('options')]];
/**
 * Compile a `(push-left! ...)` expression.
 */
function compilePushLeft(node, env, options = {}) {
    // `.unshift()` returns the length of the array, while `push!()`
    // returns the list.
    return compilePushHelper((0, rose_1.makeRose)([Symbol.for('send'), node.get(1), Symbol.for('unshift'), node.get(2)]), (0, rose_1.makeRose)([[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('lst')], node.get(1), node.get(2)]), node, env, options);
}
compilePushLeft.lispSource = [Symbol.for('define'), [Symbol.for('compile-push-left'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-push-helper'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], Symbol.for('unshift'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('lst')], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Compile a `(push-right! ...)` expression.
 */
function compilePushRight(node, env, options = {}) {
    // `.push()` returns the length of the array, while `push-right!()`
    // returns the list.
    return compilePushHelper((0, rose_1.makeRose)([Symbol.for('send'), node.get(1), Symbol.for('push'), node.get(2)]), (0, rose_1.makeRose)([[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('push'), Symbol.for('x')], Symbol.for('lst')], node.get(1), node.get(2)]), node, env, options);
}
compilePushRight.lispSource = [Symbol.for('define'), [Symbol.for('compile-push-right'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-push-helper'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], Symbol.for('push'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('push'), Symbol.for('x')], Symbol.for('lst')], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];
/**
 * Helper function for `compile-push-left`
 * and `compile-push-right`.
 */
function compilePushHelper(statementExp, expressionExp, node, env, options = {}) {
    const expressionType = options['expressionType'];
    if (expressionType === 'return') {
        // When compiled as a return statement, create a program fragment
        // if the list expression is a symbol. Otherwise, reuse the
        // expression logic and wrap in `(return ...)`.
        if (typeof node.get(1).getValue() === 'symbol') {
            return new estree_1.Program([compileStatement(statementExp, env, options), compileReturnStatement(node.get(1), env, options)]);
        }
        else {
            return compileRose((0, rose_1.makeRose)([Symbol.for('return'), node]), env, options);
        }
    }
    else if (expressionType === 'statement') {
        // When compiled as a statement, the return
        // type does not matter.
        return compileStatementOrReturnStatement(statementExp, env, options);
    }
    else if (typeof node.get(1).getValue() === 'symbol') {
        // When compiled as an expression, we can use the comma
        // operator if the list expression is a symbol.
        return new estree_1.SequenceExpression([compileExpression(statementExp, env, options), compileExpression(node.get(1), env, options)]);
    }
    else {
        // In more complicated cases, we compile to
        // a lambda expression.
        return compileExpression(expressionExp, env, options);
    }
}
compilePushHelper.lispSource = [Symbol.for('define'), [Symbol.for('compile-push-helper'), Symbol.for('statement-exp'), Symbol.for('expression-exp'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('cond'), [[Symbol.for('symbol?'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('get-value')]], [Symbol.for('new'), Symbol.for('Program'), [Symbol.for('list'), [Symbol.for('compile-statement'), Symbol.for('statement-exp'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-return-statement'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('return'), [Symbol.for('unquote'), Symbol.for('node')]]]], Symbol.for('env'), Symbol.for('options')]]]], [[Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('compile-statement-or-return-statement'), Symbol.for('statement-exp'), Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('symbol?'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('get-value')]], [Symbol.for('new'), Symbol.for('SequenceExpression'), [Symbol.for('list'), [Symbol.for('compile-expression'), Symbol.for('statement-exp'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('compile-expression'), Symbol.for('expression-exp'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(define-macro ...)` expression.
 */
function compileDefineMacro(node, env, options = {}) {
    let exp = node.getValue();
    const nameAndArgs = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1];
    let name = nameAndArgs[0];
    const macroFnForm = defineMacroToLambdaForm(exp);
    const args = (Array.isArray(macroFnForm) && (macroFnForm.length >= 3) && (macroFnForm[macroFnForm.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(macroFnForm);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = macroFnForm;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = macroFnForm[macroFnForm.length - 1];
            }
            else {
                result = macroFnForm.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : macroFnForm[1];
    const body = macroFnForm.slice(2);
    let bindings = options['bindings'];
    let result = compileRose((0, rose_1.transferComments)(node, (0, rose_1.makeRose)([Symbol.for('begin'), [Symbol.for('define'), [name, ...args], ...body], [Symbol.for('set-field!'), Symbol.for('lispMacro'), name, Symbol.for('#t')]], node)), env, options);
    if (bindings) {
        bindings.setLocal(name, true, 'macro');
    }
    return result;
}
compileDefineMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-macro'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('macro-fn-form'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('second'), Symbol.for('macro-fn-form')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('macro-fn-form'), 2]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('unquote-splicing'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('set-field!'), Symbol.for('lispMacro'), [Symbol.for('unquote'), Symbol.for('name')], Symbol.for('#t')]]], Symbol.for('node')]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('when'), Symbol.for('bindings'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('name'), Symbol.for('#t'), 'macro']], Symbol.for('result')];
/**
 * Compiler macro for `(make-hash ...)` expressions.
 */
function compileMakeHashMacro(exp, env) {
    const [assocs] = exp.slice(1);
    if (assocs) {
        if (((0, util_1.taggedListP)(assocs, Symbol.for('quasiquote')) || (0, util_1.taggedListP)(assocs, Symbol.for('quote'))) && (() => {
            const x = lastCdr((Array.isArray(assocs) && (assocs.length >= 3) && (assocs[assocs.length - 2] === Symbol.for('.')) && (() => {
                let x1 = lastCdr(assocs);
                return Array.isArray(x1) && (x1.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = assocs;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = assocs[assocs.length - 1];
                    }
                    else {
                        result = assocs.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : assocs[1]);
            return Array.isArray(x) && (x.length === 0);
        })() && (((Array.isArray(assocs) && (assocs.length >= 3) && (assocs[assocs.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(assocs);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = assocs;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = assocs[assocs.length - 1];
                }
                else {
                    result = assocs.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : assocs[1]).filter(function (x) {
            return !Array.isArray(x) || ((x.length === 2) && ((0, util_1.taggedListP)(x, Symbol.for('unquote')) || ((0, util_1.taggedListP)(x, Symbol.for('unquote-splicing')) && !(0, util_1.taggedListP)((Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && (() => {
                let x1 = lastCdr(x);
                return Array.isArray(x1) && (x1.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = x;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = x[x.length - 1];
                    }
                    else {
                        result = x.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : x[1], Symbol.for('hash->list')))));
        }).length === 0)) {
            // If we have a quoted list of pairs, rewrite it to a simpler
            // expression that does not call `flatten`.
            return [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('ann'), [assocs[0], ((Array.isArray(assocs) && (assocs.length >= 3) && (assocs[assocs.length - 2] === Symbol.for('.')) && (() => {
                            const x = lastCdr(assocs);
                            return Array.isArray(x) && (x.length === 0);
                        })()) ? (() => {
                            let i = 1;
                            let result = assocs;
                            while (i > 0) {
                                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                    result = assocs[assocs.length - 1];
                                }
                                else {
                                    result = assocs.slice(1);
                                }
                                i--;
                            }
                            if (Array.isArray(result)) {
                                result = result[0];
                            }
                            return result;
                        })() : assocs[1]).map(function (x) {
                            if ((0, util_1.taggedListP)(x, Symbol.for('unquote-splicing')) && (0, util_1.taggedListP)((Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && (() => {
                                let x1 = lastCdr(x);
                                return Array.isArray(x1) && (x1.length === 0);
                            })()) ? (() => {
                                let i = 1;
                                let result = x;
                                while (i > 0) {
                                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                        result = x[x.length - 1];
                                    }
                                    else {
                                        result = x.slice(1);
                                    }
                                    i--;
                                }
                                if (Array.isArray(result)) {
                                    result = result[0];
                                }
                                return result;
                            })() : x[1], Symbol.for('hash->list'))) {
                                return cons(x[0], [[Symbol.for('send'), (() => {
                                            const lst = (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && (() => {
                                                let x1 = lastCdr(x);
                                                return Array.isArray(x1) && (x1.length === 0);
                                            })()) ? (() => {
                                                let i = 1;
                                                let result = x;
                                                while (i > 0) {
                                                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                                        result = x[x.length - 1];
                                                    }
                                                    else {
                                                        result = x.slice(1);
                                                    }
                                                    i--;
                                                }
                                                if (Array.isArray(result)) {
                                                    result = result[0];
                                                }
                                                return result;
                                            })() : x[1];
                                            if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && (() => {
                                                let x1 = lastCdr(lst);
                                                return Array.isArray(x1) && (x1.length === 0);
                                            })()) {
                                                let i = 1;
                                                let result = lst;
                                                while (i > 0) {
                                                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                                        result = lst[lst.length - 1];
                                                    }
                                                    else {
                                                        result = lst.slice(1);
                                                    }
                                                    i--;
                                                }
                                                if (Array.isArray(result)) {
                                                    result = result[0];
                                                }
                                                return result;
                                            }
                                            else {
                                                return lst[1];
                                            }
                                        })(), Symbol.for('entries')]]);
                            }
                            else {
                                return [x[0], cdr(x)];
                            }
                        })], Symbol.for('Any')]];
        }
        else {
            // If the `assocs` form is not simple, then we have map
            // `flatten` over it in order to convert a list of pairs to a
            // list of lists.
            return [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('map'), Symbol.for('flatten'), assocs]];
        }
    }
    else {
        return [Symbol.for('new'), Symbol.for('Map')];
    }
}
compileMakeHashMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-make-hash-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('assocs')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [Symbol.for('assocs'), [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('assocs'), [Symbol.for('quote'), Symbol.for('quasiquote')]], [Symbol.for('tagged-list?'), Symbol.for('assocs'), [Symbol.for('quote'), Symbol.for('quote')]]], [Symbol.for('list?'), [Symbol.for('second'), Symbol.for('assocs')]], [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('filter'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('array?'), Symbol.for('x')]], [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('x')], 2], [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('unquote')]], [Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]], [Symbol.for('not'), [Symbol.for('tagged-list?'), [Symbol.for('second'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('hash->list')]]]]]]]], [Symbol.for('second'), Symbol.for('assocs')]]], 0]], [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('ann'), [[Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('assocs')]], [Symbol.for('unquote'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]], [Symbol.for('tagged-list?'), [Symbol.for('second'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('hash->list')]]], [Symbol.for('cons'), [Symbol.for('first'), Symbol.for('x')], [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('second'), [Symbol.for('second'), Symbol.for('x')]]], Symbol.for('entries')]]]]], [Symbol.for('else'), [Symbol.for('list'), [Symbol.for('car'), Symbol.for('x')], [Symbol.for('cdr'), Symbol.for('x')]]]]], [Symbol.for('second'), Symbol.for('assocs')]]]], Symbol.for('Any')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('map'), Symbol.for('flatten'), [Symbol.for('unquote'), Symbol.for('assocs')]]]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('Map')]]]]];
compileMakeHashMacro.lispMacro = true;
/**
 * Compiler macro for `(hash-clear ...)` expressions.
 */
function compileHashClearMacro(exp, env) {
    const [ht] = exp.slice(1);
    if (typeof ht === 'symbol') {
        return [Symbol.for('begin'), [Symbol.for('send'), ht, Symbol.for('clear')], ht];
    }
    else {
        return [[Symbol.for('lambda'), [Symbol.for('ht')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('clear')], Symbol.for('ht')], ht];
    }
}
compileHashClearMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-hash-clear-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('ht')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ht')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('ht')], Symbol.for('clear')], [Symbol.for('unquote'), Symbol.for('ht')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('ht')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('clear')], Symbol.for('ht')], [Symbol.for('unquote'), Symbol.for('ht')]]]]]];
compileHashClearMacro.lispMacro = true;
/**
 * Compiler macro for `(hash-remove! ...)` expressions.
 */
function compileHashRemoveMacro(exp, env) {
    const [ht, key] = exp.slice(1);
    if (typeof ht === 'symbol') {
        return [Symbol.for('begin'), [Symbol.for('send'), ht, Symbol.for('delete'), key], ht];
    }
    else {
        return [[Symbol.for('lambda'), [Symbol.for('ht'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('delete'), Symbol.for('key')], Symbol.for('ht')], ht, key];
    }
}
compileHashRemoveMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-hash-remove-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('ht'), Symbol.for('key')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ht')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('ht')], Symbol.for('delete'), [Symbol.for('unquote'), Symbol.for('key')]], [Symbol.for('unquote'), Symbol.for('ht')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('ht'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('delete'), Symbol.for('key')], Symbol.for('ht')], [Symbol.for('unquote'), Symbol.for('ht')], [Symbol.for('unquote'), Symbol.for('key')]]]]]];
compileHashRemoveMacro.lispMacro = true;
/**
 * Compiler macro for `(hash-ref ...)` expressions.
 */
function compileHashRefMacro(exp, env) {
    const [ht, key, failureResult] = exp.slice(1);
    if (failureResult) {
        return definitionToMacro([Symbol.for('define'), [Symbol.for('hash-ref'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')], [Symbol.for('if'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('has'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('get'), Symbol.for('key')], Symbol.for('failure-result')]], [ht, key, failureResult]);
    }
    else {
        return [Symbol.for('send'), ht, Symbol.for('get'), key];
    }
}
compileHashRefMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-hash-ref-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [Symbol.for('failure-result'), [Symbol.for('definition->macro'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('hash-ref'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')], [Symbol.for('if'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('has'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('get'), Symbol.for('key')], Symbol.for('failure-result')]]], [Symbol.for('list'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('ht')], Symbol.for('get'), [Symbol.for('unquote'), Symbol.for('key')]]]]]];
compileHashRefMacro.lispMacro = true;
/**
 * Compiler macro for `(map ...)` expressions.
 */
function compileMapMacro(exp, env) {
    const [f, x] = exp.slice(1);
    // Note that `` `(send ,x map ,f) `` is too simple, as JavaScript's
    // `.map()` method calls the function with multiple arguments. This
    // can lead to unintuitive bugs in cases where the function has an
    // optional second parameter. To avoid this, we enclose `f` in a
    // unary function wrapper.
    const fExp = compileMapMacroHelper(f, env);
    return [Symbol.for('send'), x, Symbol.for('map'), fExp];
}
compileMapMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-map-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('f-exp'), [Symbol.for('compile-map-macro-helper'), Symbol.for('f'), Symbol.for('env')]], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('x')], Symbol.for('map'), [Symbol.for('unquote'), Symbol.for('f-exp')]]]];
compileMapMacro.lispMacro = true;
/**
 * Compiler macro for `(values ...)` expressions.
 */
function compileValuesMacro(exp, env) {
    const args = exp.slice(1);
    return [Symbol.for('list'), ...args];
}
compileValuesMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-values-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('list'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];
compileValuesMacro.lispMacro = true;
/**
 * Compiler macro for `(string? ...)` expressions.
 */
function compileStringpMacro(exp, env) {
    const [x] = exp.slice(1);
    return [Symbol.for('eq?'), [Symbol.for('type-of'), x], 'string'];
}
compileStringpMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-stringp-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('eq?'), [Symbol.for('type-of'), [Symbol.for('unquote'), Symbol.for('x')]], 'string']]];
compileStringpMacro.lispMacro = true;
/**
 * Compiler macro for `(string-trim ...)` expressions.
 */
function compileStringTrimMacro(exp, env) {
    const args = exp.slice(1);
    if (args.length === 1) {
        return [Symbol.for('send'), args[0], Symbol.for('trim')];
    }
    else {
        return definitionToMacro(source(string_1.stringTrim_), args);
    }
}
compileStringTrimMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-string-trim-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-length'), Symbol.for('args')], 1], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('array-first'), Symbol.for('args')]], Symbol.for('trim')]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('string-trim_')], Symbol.for('args')]]]];
compileStringTrimMacro.lispMacro = true;
/**
 * Compiler macro for `(member? ...)` expressions.
 */
function compileMemberPMacro(exp, env) {
    const [v, lst, isEqual] = exp.slice(1);
    if (!isEqual) {
        return definitionToMacro([Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]], [v, lst]);
    }
    else {
        return definitionToMacro([Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]], [v, lst, isEqual]);
    }
}
compileMemberPMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-member-p-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('is-equal')], [Symbol.for('definition->macro'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]]], [Symbol.for('list'), Symbol.for('v'), Symbol.for('lst')]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]]], [Symbol.for('list'), Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')]]]]];
compileMemberPMacro.lispMacro = true;
/**
 * Compiler macro for `(substring ...)` expressions.
 */
function compileSubstringMacro(exp, env) {
    const [str, ...args] = exp.slice(1);
    return [Symbol.for('send'), str, Symbol.for('substring'), ...args];
}
compileSubstringMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-substring-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('str'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('str')], Symbol.for('substring'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];
compileSubstringMacro.lispMacro = true;
/**
 * Compiler macro for `(array-drop ...)` expressions.
 */
function compileArrayDropMacro(exp, env) {
    const [arr, n] = exp.slice(1);
    if (Number.isFinite(n)) {
        if (n === 0) {
            return arr;
        }
        else {
            return [Symbol.for('send'), arr, Symbol.for('slice'), n];
        }
    }
    else {
        return definitionToMacro(source(array_1.arrayDrop_), [arr, n]);
    }
}
compileArrayDropMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-drop-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('arr'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('arr')], Symbol.for('slice'), [Symbol.for('unquote'), Symbol.for('n')]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-drop_')], [Symbol.for('list'), Symbol.for('arr'), Symbol.for('n')]]]]];
compileArrayDropMacro.lispMacro = true;
/**
 * Compiler macro for `(drop-right ...)` expressions.
 */
function compileArrayDropRightMacro(exp, env) {
    const [arr, n] = exp.slice(1);
    if (Number.isFinite(n)) {
        if (n === 0) {
            return arr;
        }
        else {
            return [Symbol.for('send'), arr, Symbol.for('slice'), 0, [Symbol.for('-'), n]];
        }
    }
    else {
        return definitionToMacro(source(array_1.arrayDropRight_), [arr, n]);
    }
}
compileArrayDropRightMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-drop-right-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('arr'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('arr')], Symbol.for('slice'), 0, [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('n')]]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-drop-right_')], [Symbol.for('list'), Symbol.for('arr'), Symbol.for('n')]]]]];
compileArrayDropRightMacro.lispMacro = true;
/**
 * Compiler macro for `(drop ...)` expressions.
 */
function compileDropMacro(exp, env) {
    const [lst, pos] = exp.slice(1);
    if (Number.isFinite(pos)) {
        if (pos === 0) {
            return lst;
        }
        else {
            return [Symbol.for('send'), lst, Symbol.for('slice'), pos];
        }
    }
    else {
        return definitionToMacro(source(list_1.drop_), [lst, pos]);
    }
}
compileDropMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-drop-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('pos')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('pos')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('pos'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), [Symbol.for('unquote'), Symbol.for('pos')]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('drop_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('pos')]]]]];
compileDropMacro.lispMacro = true;
/**
 * Compiler macro for `(drop-right ...)` expressions.
 */
function compileDropRightMacro(exp, env) {
    const [lst, n] = exp.slice(1);
    if (Number.isFinite(n)) {
        if (n === 0) {
            return lst;
        }
        else {
            return [Symbol.for('send'), lst, Symbol.for('slice'), 0, [Symbol.for('-'), n]];
        }
    }
    else {
        return definitionToMacro(source(list_1.dropRight_), [lst, n]);
    }
}
compileDropRightMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-drop-right-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), 0, [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('n')]]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('drop-right_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('n')]]]]];
compileDropRightMacro.lispMacro = true;
/**
 * Compiler macro for `(array-list-drop ...)` expressions.
 */
function compileArrayListDropMacro(exp, env) {
    const [lst, n] = exp.slice(1);
    if (Number.isFinite(n)) {
        if (n === 0) {
            return lst;
        }
        else {
            return [Symbol.for('send'), lst, Symbol.for('slice'), n];
        }
    }
    else {
        return definitionToMacro(source(list_1.arrayListDrop_), [lst, n]);
    }
}
compileArrayListDropMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-list-drop-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), [Symbol.for('unquote'), Symbol.for('n')]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-list-drop_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('n')]]]]];
compileArrayListDropMacro.lispMacro = true;
/**
 * Compiler macro for `(array-list-drop-right ...)` expressions.
 */
function compileArrayListDropRightMacro(exp, env) {
    const [lst, n] = exp.slice(1);
    if (Number.isFinite(n)) {
        if (n === 0) {
            return lst;
        }
        else {
            return [Symbol.for('send'), lst, Symbol.for('slice'), 0, [Symbol.for('-'), n]];
        }
    }
    else {
        return definitionToMacro(source(list_1.arrayListDropRight_), [lst, n]);
    }
}
compileArrayListDropRightMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-list-drop-right-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), 0, [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('n')]]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-list-drop-right_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('n')]]]]];
compileArrayListDropRightMacro.lispMacro = true;
/**
 * Compiler macro for `(regexp ...)` expressions.
 */
function compileRegexpMacro(exp, env) {
    const args = exp.slice(1);
    return [Symbol.for('new'), Symbol.for('RegExp'), ...args];
}
compileRegexpMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-regexp-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('RegExp'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];
compileRegexpMacro.lispMacro = true;
/**
 * Compiler macro for `(assert ...)` expressions.
 */
function compileAssertMacro(exp, env) {
    const args = exp.slice(1);
    return [Symbol.for('send'), Symbol.for('console'), Symbol.for('assert'), ...args];
}
compileAssertMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-assert-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), Symbol.for('console'), Symbol.for('assert'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];
compileAssertMacro.lispMacro = true;
/**
 * Compiler macro for `(display ...)` expressions.
 */
function compileDisplayMacro(exp, env) {
    const args = exp.slice(1);
    return [Symbol.for('send'), Symbol.for('console'), Symbol.for('log'), ...args];
}
compileDisplayMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-display-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), Symbol.for('console'), Symbol.for('log'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];
compileDisplayMacro.lispMacro = true;
/**
 * Compiler macro for `(current-environment)` expressions.
 */
function compileCurrentEnvironmentMacro(exp, env) {
    const argSym = Symbol('_arg');
    const strSym = Symbol('_str');
    const identifierRegexp = [Symbol.for('regexp'), '^\\w+$'];
    return [Symbol.for('js-obj'), 'get', [Symbol.for('js/arrow'), [argSym], [Symbol.for('try'), [Symbol.for('define'), strSym, [Symbol.for('symbol->string'), argSym]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), identifierRegexp, strSym], [Symbol.for('return'), [Symbol.for('js/eval'), strSym]]], [Symbol.for('else'), [Symbol.for('return'), Symbol.for('undefined')]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), Symbol.for('undefined')]]]], 'has', [Symbol.for('js/arrow'), [argSym], [Symbol.for('try'), [Symbol.for('define'), strSym, [Symbol.for('symbol->string'), argSym]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), identifierRegexp, strSym], [Symbol.for('js/eval'), strSym], [Symbol.for('return'), Symbol.for('#t')]], [Symbol.for('else'), [Symbol.for('return'), Symbol.for('#f')]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), Symbol.for('#f')]]]]];
}
compileCurrentEnvironmentMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-current-environment-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('arg-sym'), [Symbol.for('gensym'), '_arg']], [Symbol.for('define'), Symbol.for('str-sym'), [Symbol.for('gensym'), '_str']], [Symbol.for('define'), Symbol.for('identifier-regexp'), [Symbol.for('quote'), [Symbol.for('regexp'), '^\\w+$']]], [Symbol.for('quasiquote'), [Symbol.for('js-obj'), 'get', [Symbol.for('js/arrow'), [[Symbol.for('unquote'), Symbol.for('arg-sym')]], [Symbol.for('try'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('str-sym')], [Symbol.for('symbol->string'), [Symbol.for('unquote'), Symbol.for('arg-sym')]]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), [Symbol.for('unquote'), Symbol.for('identifier-regexp')], [Symbol.for('unquote'), Symbol.for('str-sym')]], [Symbol.for('return'), [Symbol.for('js/eval'), [Symbol.for('unquote'), Symbol.for('str-sym')]]]], [Symbol.for('else'), [Symbol.for('return'), Symbol.for('undefined')]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), Symbol.for('undefined')]]]], 'has', [Symbol.for('js/arrow'), [[Symbol.for('unquote'), Symbol.for('arg-sym')]], [Symbol.for('try'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('str-sym')], [Symbol.for('symbol->string'), [Symbol.for('unquote'), Symbol.for('arg-sym')]]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), [Symbol.for('unquote'), Symbol.for('identifier-regexp')], [Symbol.for('unquote'), Symbol.for('str-sym')]], [Symbol.for('js/eval'), [Symbol.for('unquote'), Symbol.for('str-sym')]], [Symbol.for('return'), Symbol.for('#t')]], [Symbol.for('else'), [Symbol.for('return'), Symbol.for('#f')]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), Symbol.for('#f')]]]]]]];
compileCurrentEnvironmentMacro.lispMacro = true;
/**
 * Compile a `(js ...)` expression.
 */
function compileJs(node, env, options = {}) {
    let evalOption = options['eval'];
    evalOption = true;
    const str = node.get(1);
    const strExp = str.getValue();
    if (!evalOption) {
        return makeExpressionOrStatement(new estree_1.Literal(undefined), options);
    }
    else if (typeof strExp === 'string') {
        return makeExpressionOrStatement(new estree_1.XRawJavaScript(strExp), options);
    }
    else {
        return compileJsEval(node, env, options);
    }
}
compileJs.lispSource = [Symbol.for('define'), [Symbol.for('compile-js'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('eval-option'), [Symbol.for('oget'), Symbol.for('options'), 'eval']], [Symbol.for('set!'), Symbol.for('eval-option'), Symbol.for('#t')], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('str-exp'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('eval-option')], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('undefined')], Symbol.for('options')]], [[Symbol.for('string?'), Symbol.for('str-exp')], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('XRawJavaScript'), Symbol.for('str-exp')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-js-eval'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Compile a `(js/eval ...)` expression.
 */
function compileJsEval(node, env, options = {}) {
    // TODO: Disable if `eval-option` is `#f`.
    let evalOption = options['eval'];
    // TODO: Make `#f` the default.
    evalOption = true;
    const str = node.get(1);
    const strExp = str.getValue();
    if (!evalOption) {
        return makeExpressionOrStatement(new estree_1.Literal(undefined), options);
    }
    else {
        return makeExpressionOrStatement(new estree_1.CallExpression(new estree_1.Identifier('eval'), [compileExpression(str, env, options)]), options);
    }
}
compileJsEval.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-eval'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('eval-option'), [Symbol.for('oget'), Symbol.for('options'), 'eval']], [Symbol.for('set!'), Symbol.for('eval-option'), Symbol.for('#t')], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('str-exp'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('eval-option')], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('undefined')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), [Symbol.for('new'), Symbol.for('Identifier'), 'eval'], [Symbol.for('list'), [Symbol.for('compile-expression'), Symbol.for('str'), Symbol.for('env'), Symbol.for('options')]]], Symbol.for('options')]]]];
/**
 * Expand a `(quote ...)` expression.
 *
 * Similar to [`quote` in Racket][rkt:quote] and
 * [`quote` in Common Lisp][cl:quote].
 *
 * [rkt:quote]: https://docs.racket-lang.org/reference/quote.html
 * [cl:quote]: http://clhs.lisp.se/Body/s_quote.htm#quote
 */
function quote_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.quote = quote_;
exports.quote_ = quote_;
quote_.lispSource = [Symbol.for('define'), [Symbol.for('quote_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
quote_.lispMacro = true;
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
function quasiquote_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.quasiquote = quasiquote_;
exports.quasiquote_ = quasiquote_;
quasiquote_.lispSource = [Symbol.for('define'), [Symbol.for('quasiquote_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
quasiquote_.lispMacro = true;
/**
 * Expand a `(set! ...)` expression.
 *
 * Similar to [`set!` in Racket][rkt:setx] and
 * [`setq` in Common Lisp][cl:setq].
 *
 * [rkt:setx]: https://docs.racket-lang.org/reference/set_.html#%28form._%28%28quote._~23~25kernel%29._set%21%29%29
 * [cl:setq]: http://clhs.lisp.se/Body/s_setq.htm#setq
 */
function setX_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.setX = setX_;
exports.setq = setX_;
exports.setq_ = setX_;
exports.setX_ = setX_;
setX_.lispSource = [Symbol.for('define'), [Symbol.for('set!_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
setX_.lispMacro = true;
/**
 * Expand a `(module ...)` expression.
 */
function module_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.module_ = module_;
module_.lispSource = [Symbol.for('define'), [Symbol.for('module_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
module_.lispMacro = true;
/**
 * Expand a `(begin ...)` expression.
 */
function begin_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.begin = begin_;
exports.begin_ = begin_;
begin_.lispSource = [Symbol.for('define'), [Symbol.for('begin_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
begin_.lispMacro = true;
/**
 * Expand a `(block ...)` expression.
 */
function block_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.block = block_;
block_.lispSource = [Symbol.for('define'), [Symbol.for('block_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
block_.lispMacro = true;
/**
 * Expand a `(let* ...)` expression.
 */
function letStar_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.letStar = letStar_;
exports.let_ = letStar_;
exports.letrec = letStar_;
exports.letStar_ = letStar_;
letStar_.lispSource = [Symbol.for('define'), [Symbol.for('let-star_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
letStar_.lispMacro = true;
/**
 * Expand a `(let-values ...)` expression.
 */
function letValues_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.letstarValues = letValues_;
exports.letValues = letValues_;
exports.letrecValues = letValues_;
exports.letValues_ = letValues_;
letValues_.lispSource = [Symbol.for('define'), [Symbol.for('let-values_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
letValues_.lispMacro = true;
/**
 * Expand a `(define-values ...)` expression.
 */
function defineValues_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.defineValues = defineValues_;
exports.defineValues_ = defineValues_;
defineValues_.lispSource = [Symbol.for('define'), [Symbol.for('define-values_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
defineValues_.lispMacro = true;
/**
 * Expand a `(set!-values ...)` expression.
 */
function setValues_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.setXValues = setValues_;
exports.setValues = setValues_;
exports.setValues_ = setValues_;
setValues_.lispSource = [Symbol.for('define'), [Symbol.for('set-values_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
setValues_.lispMacro = true;
/**
 * Expand a `(define ...)` expression.
 */
function define_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.define = define_;
exports.define_ = define_;
define_.lispSource = [Symbol.for('define'), [Symbol.for('define_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
define_.lispMacro = true;
/**
 * Expand a `(define/generator ...)` expression.
 */
function defineGenerator_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.defineGenerator = defineGenerator_;
exports.defineGenerator_ = defineGenerator_;
defineGenerator_.lispSource = [Symbol.for('define'), [Symbol.for('define-generator_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
defineGenerator_.lispMacro = true;
/**
 * Expand a `(define/async ...)` expression.
 */
function defineAsync_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.defineAsync = defineAsync_;
exports.defineAsync_ = defineAsync_;
defineAsync_.lispSource = [Symbol.for('define'), [Symbol.for('define-async_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
defineAsync_.lispMacro = true;
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
    let name = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 1;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[1])[0];
    const fExp = compileSexp(exp, env, currentCompilationOptions());
    const f = (0, eval_1.eval_)(fExp, env);
    env.set(name, f, 'macro');
    return fExp;
}
exports.defineMacro = defineMacro_;
exports.defineMacro_ = defineMacro_;
defineMacro_.lispSource = [Symbol.for('define'), [Symbol.for('define-macro_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('f-exp'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('eval_'), Symbol.for('f-exp'), Symbol.for('env')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('name'), Symbol.for('f'), 'macro'], Symbol.for('f-exp')];
defineMacro_.lispMacro = true;
/**
 * Expand a `(for ...)` expression.
 */
function for_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.for_ = for_;
for_.lispSource = [Symbol.for('define'), [Symbol.for('for_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
for_.lispMacro = true;
/**
 * Expand a `(js/while ...)` expression.
 */
function jsWhile_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
jsWhile_.lispSource = [Symbol.for('define'), [Symbol.for('js-while_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
jsWhile_.lispMacro = true;
/**
 * Expand a `(js/do-while ...)` expression.
 */
function jsDoWhile_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
jsDoWhile_.lispSource = [Symbol.for('define'), [Symbol.for('js-do-while_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
jsDoWhile_.lispMacro = true;
/**
 * Expand a `(break)` expression.
 */
function break_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.break_ = break_;
break_.lispSource = [Symbol.for('define'), [Symbol.for('break_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
break_.lispMacro = true;
/**
 * Expand a `(continue)` expression.
 */
function continue_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.continue_ = continue_;
continue_.lispSource = [Symbol.for('define'), [Symbol.for('continue_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
continue_.lispMacro = true;
/**
 * Expand a `(yield ...)` expression.
 */
function yield_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.yield_ = yield_;
yield_.lispSource = [Symbol.for('define'), [Symbol.for('yield_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
yield_.lispMacro = true;
/**
 * Expand a `(return ...)` expression.
 */
function return_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.return_ = return_;
return_.lispSource = [Symbol.for('define'), [Symbol.for('return_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
return_.lispMacro = true;
/**
 * Expand a `(throw ...)` expression.
 *
 * Similar to the [`throw`][clj:throw] special form in Clojure.
 *
 * [clj:throw]: https://clojuredocs.org/clojure.core/throw
 */
function throw_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.throw_ = throw_;
throw_.lispSource = [Symbol.for('define'), [Symbol.for('throw_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
throw_.lispMacro = true;
/**
 * Expand a `(js/async ...)` expression.
 */
function jsAsync_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.async = jsAsync_;
exports.async_ = jsAsync_;
exports.jsAsync = jsAsync_;
exports.jsAsync_ = jsAsync_;
jsAsync_.lispSource = [Symbol.for('define'), [Symbol.for('js-async_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
jsAsync_.lispMacro = true;
/**
 * Expand a `(js/await ...)` expression.
 */
function jsAwait_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.await = jsAwait_;
exports.await_ = jsAwait_;
exports.jsAwait = jsAwait_;
exports.jsAwait_ = jsAwait_;
jsAwait_.lispSource = [Symbol.for('define'), [Symbol.for('js-await_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
jsAwait_.lispMacro = true;
/**
 * Expand a `(lambda ...)` expression.
 *
 * Returns an anonymous function. The name `lambda` is
 * a reference to [lambda calculus][w:Lambda calculus].
 *
 * [w:Lambda calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
 */
function lambda_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.compileFunction = lambda_;
exports.fn = lambda_;
exports.lambda = lambda_;
exports.lambda_ = lambda_;
lambda_.lispSource = [Symbol.for('define'), [Symbol.for('lambda_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
lambda_.lispMacro = true;
/**
 * Expand a `(js/function ...)` expression.
 *
 * Creates an anonymous JavaScript function.
 */
function jsFunction_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
jsFunction_.lispSource = [Symbol.for('define'), [Symbol.for('js-function_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
jsFunction_.lispMacro = true;
/**
 * Expand a `(js/arrow ...)` expression.
 *
 * Creates a JavaScript arrow function.
 */
function jsArrow_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
jsArrow_.lispSource = [Symbol.for('define'), [Symbol.for('js-arrow_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
jsArrow_.lispMacro = true;
/**
 * Expand a `(cond ...)` expression.
 */
function cond_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.cond = cond_;
exports.cond_ = cond_;
cond_.lispSource = [Symbol.for('define'), [Symbol.for('cond_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
cond_.lispMacro = true;
/**
 * Expand an `(and ...)` expression.
 */
function and_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.and = and_;
exports.and_ = and_;
and_.lispSource = [Symbol.for('define'), [Symbol.for('and_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
and_.lispMacro = true;
/**
 * Expand an `(or ...)` expression.
 */
function or_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.or = or_;
exports.or_ = or_;
or_.lispSource = [Symbol.for('define'), [Symbol.for('or_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
or_.lispMacro = true;
/**
 * Call a method on an object.
 */
function sendMethod(...args) {
    const [obj, method, ...restArgs] = args;
    if (typeof method === 'symbol') {
        return sendMethod(obj, method.description, ...restArgs);
    }
    else if (typeof method === 'string') {
        return sendMethod(obj, obj[method], ...restArgs);
    }
    else if (method instanceof Function) {
        return method.call(obj, ...restArgs);
    }
    else {
        throw new Error('Not a method: ' + method);
    }
}
exports.sendMethod = sendMethod;
sendMethod.lispSource = [Symbol.for('define'), [Symbol.for('send-method'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define-values'), [Symbol.for('obj'), Symbol.for('method'), Symbol.for('.'), Symbol.for('rest-args')], Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('method')], 'symbol'], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), [Symbol.for('symbol->string'), Symbol.for('method')], Symbol.for('rest-args')]], [[Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('method')], 'string'], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), [Symbol.for('oget'), Symbol.for('obj'), Symbol.for('method')], Symbol.for('rest-args')]], [[Symbol.for('is-a?'), Symbol.for('method'), Symbol.for('Function')], [Symbol.for('send/apply'), Symbol.for('method'), Symbol.for('call'), Symbol.for('obj'), Symbol.for('rest-args')]], [Symbol.for('else'), [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), [Symbol.for('string-append'), 'Not a method: ', Symbol.for('method')]]]]]];
/**
 * Expand a `(send ...)` expression.
 *
 * Similar to [`send`][rkt:send] in Racket.
 *
 * [rkt:send]: https://docs.racket-lang.org/guide/classes.html#(part._methods)
 */
function send_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.callMethod = send_;
exports.send = send_;
exports.send_ = send_;
send_.lispSource = [Symbol.for('define'), [Symbol.for('send_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
send_.lispMacro = true;
/**
 * Expand a `(send/apply ...)` expression.
 */
function sendApply_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.sendApply = sendApply_;
exports.sendApply_ = sendApply_;
sendApply_.lispSource = [Symbol.for('define'), [Symbol.for('send-apply_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
sendApply_.lispMacro = true;
/**
 * Expand a `(. ...)` expression.
 *
 * Similar to the [`.` special form][clj:dot] in Clojure and
 * [ClojureScript][cljs:dot].
 *
 * [clj:dot]: https://clojure.org/reference/java_interop#dot
 * [cljs:dot]: https://cljs.github.io/api/syntax/dot
 */
function dot_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.dot = dot_;
exports.dot_ = dot_;
dot_.lispSource = [Symbol.for('define'), [Symbol.for('dot_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
dot_.lispMacro = true;
/**
 * Expand a `(get-field ...)` expression.
 */
function getField_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.getField = getField_;
exports.getField_ = getField_;
getField_.lispSource = [Symbol.for('define'), [Symbol.for('get-field_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
getField_.lispMacro = true;
/**
 * Expand a `(js/optional-chaining ...)` expression.
 */
function jsOptionalChaining_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
jsOptionalChaining_.lispSource = [Symbol.for('define'), [Symbol.for('js-optional-chaining_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
jsOptionalChaining_.lispMacro = true;
/**
 * Expand a `(set-field! ...)` expression.
 */
function setField_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.setFieldX = setField_;
exports.setField = setField_;
exports.setField_ = setField_;
setField_.lispSource = [Symbol.for('define'), [Symbol.for('set-field_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
setField_.lispMacro = true;
/**
 * Evaluate an `(make-object ...)` expression.
 */
function new_(constructor, ...args) {
    // TODO: Rename to `new_`.
    // TODO: Express as `(apply new ...)`.
    return new constructor(...args);
}
exports.jsNew = new_;
exports.make = new_;
exports.makeObject = new_;
exports.makeObject_ = new_;
exports.newStar = new_;
exports.rktMakeObject = new_;
exports.scmNew = new_;
exports.new_ = new_;
new_.lispSource = [Symbol.for('define'), [Symbol.for('new_'), Symbol.for('constructor'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('apply'), Symbol.for('make-object'), Symbol.for('constructor'), Symbol.for('args')]];
/**
 * Expand a `(class ...)` expression.
 *
 * Loosely based on [`class` in Racket][rkt:class] and
 * [`define-class` in CLOS][cl:define-class].
 *
 * [rkt:class]: https://docs.racket-lang.org/guide/classes.html
 * [cl:define-class]: http://clhs.lisp.se/Body/07_.htm
 */
function class_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.class_ = class_;
class_.lispSource = [Symbol.for('define'), [Symbol.for('class_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
class_.lispMacro = true;
/**
 * Expand a `(define-class ...)` expression.
 *
 * Loosely based on [`class` in Racket][rkt:class] and
 * [`defclass` in CLOS][cl:defclass].
 *
 * [rkt:class]: https://docs.racket-lang.org/guide/classes.html
 * [cl:defclass]: http://clhs.lisp.se/Body/m_defcla.htm#defclass
 */
function defineClass_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.defineClass = defineClass_;
defineClass_.lispSource = [Symbol.for('define'), [Symbol.for('define-class_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
defineClass_.lispMacro = true;
/**
 * Expand a `(js/try ...)` expression.
 */
function jsTry_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
jsTry_.lispSource = [Symbol.for('define'), [Symbol.for('js-try_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
jsTry_.lispMacro = true;
/**
 * Expand a `(provide ...)` expression.
 */
function provide_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.provide = provide_;
exports.provide_ = provide_;
provide_.lispSource = [Symbol.for('define'), [Symbol.for('provide_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
provide_.lispMacro = true;
/**
 * Expand a `(require ...)` expression.
 */
function require_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.require = require_;
exports.require_ = require_;
require_.lispSource = [Symbol.for('define'), [Symbol.for('require_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
require_.lispMacro = true;
/**
 * Evaluate a JavaScript string.
 */
function js_(str) {
    return eval(str);
}
exports.js = js_;
exports.js_ = js_;
js_.lispSource = [Symbol.for('define'), [Symbol.for('js_'), Symbol.for('str')], [Symbol.for('js/eval'), Symbol.for('str')]];
/**
 * Get the Lisp source of a function.
 */
function source(x) {
    return x.lispSource;
}
exports.source = source;
source.lispSource = [Symbol.for('define'), [Symbol.for('source'), Symbol.for('x')], [Symbol.for('get-field'), Symbol.for('lispSource'), Symbol.for('x')]];
/**
 * Whether a function has Lisp source.
 */
function sourcep(x) {
    return (x !== undefined) && (x.lispSource !== undefined);
}
exports.sourcep = sourcep;
sourcep.lispSource = [Symbol.for('define'), [Symbol.for('source?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('undefined')]], [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('lispSource'), Symbol.for('x')], Symbol.for('undefined')]]]];
/**
 * Map the function `f` over the rose tree-wrapped
 * S-expression `node`. The S-expression is processed
 * in bottom-up order.
 */
function mapRose(f, node, env = new env_1.LispEnvironment(), stack = [], bindings = new env_1.LispEnvironment()) {
    if (!(node instanceof rose_1.Rose)) {
        return mapSexp(f, node, env, stack, bindings);
    }
    else {
        return mapVisitRose(f, node, env, stack, bindings);
    }
}
exports.mapRose = mapRose;
mapRose.lispSource = [Symbol.for('define'), [Symbol.for('map-rose'), Symbol.for('f'), Symbol.for('node'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')]], [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('node'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('else'), [Symbol.for('map-visit-rose'), Symbol.for('f'), Symbol.for('node'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]]];
/**
 * Map a function `f` over a rose tree using the Visitor pattern.
 */
function mapVisitRose(f, node, env = new env_1.LispEnvironment(), stack = [], bindings = new env_1.LispEnvironment()) {
    function skipNode(node, stack, bindings) {
        return node;
    }
    skipNode.lispSource = [Symbol.for('define'), [Symbol.for('skip-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], Symbol.for('node')];
    function visitNode(node, stack, bindings) {
        return f(node, stack, bindings);
    }
    visitNode.lispSource = [Symbol.for('define'), [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('f'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
    // Nonatomic value (i.e., a list form some sort).
    function visitNonatomic(node, stack, bindings, skip = 0) {
        let result = visitFormsNode(node, [...stack, node], bindings, skip);
        return f(result, stack, bindings);
    }
    visitNonatomic.lispSource = [Symbol.for('define'), [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('visit-forms-node'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
    // Macro call.
    function visitMacroCallP(node) {
        let exp = node.getValue();
        return macroCallP(exp, env);
    }
    visitMacroCallP.lispSource = [Symbol.for('define'), [Symbol.for('visit-macro-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('macro-call?'), Symbol.for('exp'), Symbol.for('env')]]];
    const visitMacroCall = visitNode;
    // Special form.
    function visitSpecialFormP(node) {
        let exp = node.getValue();
        return specialFormP(exp, env);
    }
    visitSpecialFormP.lispSource = [Symbol.for('define'), [Symbol.for('visit-special-form-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('special-form?'), Symbol.for('exp'), Symbol.for('env')]]];
    const visitSpecialForm = visitNode;
    // Function call.
    function visitFunctionCallP(node) {
        let exp = node.getValue();
        return functionCallP(exp, env);
    }
    visitFunctionCallP.lispSource = [Symbol.for('define'), [Symbol.for('visit-function-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('function-call?'), Symbol.for('exp'), Symbol.for('env')]]];
    const visitFunctionCall = visitNonatomic;
    function visitElseP(node) {
        return true;
    }
    visitElseP.lispSource = [Symbol.for('define'), [Symbol.for('visit-else-p'), Symbol.for('node')], Symbol.for('#t')];
    function visitFormsNodeWith(visitor, node, stack, bindings, skip = 0) {
        let exp = node.getValue();
        if (!Array.isArray(exp)) {
            // `node` is not a list expression; early return.
            return (0, visitor_1.visit)(visitor, node, stack, bindings);
        }
        const nodes = node.getNodes();
        const resultNodes = visitFormsListWith(visitor, nodes, stack, bindings, skip);
        if (resultNodes === nodes) {
            return node;
        }
        else {
            let exp = [];
            let result = (0, rose_1.transferComments)(node, new rose_1.Rose(exp));
            for (let node of resultNodes) {
                exp.push(node.getValue());
                result.insert(node);
            }
            return result;
        }
    }
    visitFormsNodeWith.lispSource = [Symbol.for('define'), [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-nodes')]], [Symbol.for('define'), Symbol.for('result-nodes'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('result-nodes'), Symbol.for('nodes')], Symbol.for('node')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('new'), Symbol.for('Rose'), Symbol.for('exp')]]], [Symbol.for('for'), [[Symbol.for('node'), Symbol.for('result-nodes')]], [Symbol.for('push-right!'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('send'), Symbol.for('result'), Symbol.for('insert'), Symbol.for('node')]], Symbol.for('result')]]];
    function visitFormsListWith(visitor, nodes, stack, bindings, skip = 0) {
        if (!Array.isArray(nodes)) {
            // `nodes` is not a list; early return.
            return (0, visitor_1.visit)(visitor, nodes, stack, bindings);
        }
        // Keep track of whether any of the expressions are modified
        // by visitation. If none of them are, return the original list.
        let isModified = false;
        let i = 0;
        let result = nodes.map(function (x) {
            if (i < skip) {
                i++;
                return x;
            }
            else {
                let x1 = (0, visitor_1.visit)(visitor, x, stack, bindings);
                if (x !== x1) {
                    isModified = true;
                }
                i++;
                return x1;
            }
        });
        // Return the original list if none of the sub-expressions
        // were modified.
        if (!isModified) {
            result = nodes;
        }
        return result;
    }
    visitFormsListWith.lispSource = [Symbol.for('define'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('nodes')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('is-modified'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), Symbol.for('skip')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('is-modified'), Symbol.for('#t')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x1')]]], Symbol.for('nodes')]], [Symbol.for('unless'), Symbol.for('is-modified'), [Symbol.for('set!'), Symbol.for('result'), Symbol.for('nodes')]], Symbol.for('result')];
    function visitFormsNode(node, stack, bindings, skip = 0) {
        return visitFormsNodeWith(visitor, node, stack, bindings, skip);
    }
    visitFormsNode.lispSource = [Symbol.for('define'), [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
    function visitFormsList(nodes, stack, bindings, skip = 0) {
        return visitFormsListWith(visitor, nodes, stack, bindings, skip);
    }
    visitFormsList.lispSource = [Symbol.for('define'), [Symbol.for('visit-forms-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
    function visitClausesNode(node, stack, bindings, skip = 0) {
        return visitFormsNodeWith(visitFormsNode, node, stack, bindings, skip);
    }
    visitClausesNode.lispSource = [Symbol.for('define'), [Symbol.for('visit-clauses-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
    function visitClausesList(nodes, stack, bindings, skip = 0) {
        return visitFormsListWith(visitFormsNode, nodes, stack, bindings, skip);
    }
    visitClausesList.lispSource = [Symbol.for('define'), [Symbol.for('visit-clauses-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visit-forms-node'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
    // `(module ...)` form.
    function visitModuleP(node) {
        return (0, util_1.formp)(node, module_, env);
    }
    visitModuleP.lispSource = [Symbol.for('define'), [Symbol.for('visit-module-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('module_'), Symbol.for('env')]];
    function visitModule(node, stack, bindings) {
        return visitNonatomic(node, stack, bindings, 3);
    }
    visitModule.lispSource = [Symbol.for('define'), [Symbol.for('visit-module'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 3]];
    // `(begin ...)` form.
    function visitBeginP(node) {
        return (0, util_1.formp)(node, begin_, env);
    }
    visitBeginP.lispSource = [Symbol.for('define'), [Symbol.for('visit-begin-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin_'), Symbol.for('env')]];
    function visitBegin(node, stack, bindings) {
        return visitNonatomic(node, stack, bindings, 1);
    }
    visitBegin.lispSource = [Symbol.for('define'), [Symbol.for('visit-begin'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
    // `(begin0 ...)` form.
    function visitBegin0P(node) {
        return (0, util_1.formp)(node, macros_1.begin0_, env);
    }
    visitBegin0P.lispSource = [Symbol.for('define'), [Symbol.for('visit-begin0-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin0_'), Symbol.for('env')]];
    const visitBegin0 = visitBegin;
    // `(let ...)` form.
    function visitLetP(node) {
        return (0, util_1.formp)(node, letStar_, env);
    }
    visitLetP.lispSource = [Symbol.for('define'), [Symbol.for('visit-let-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-star_'), Symbol.for('env')]];
    function visitLet(node, stack, bindings) {
        let result = node;
        let bindingsEnv = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
        const sym = node.get(0).getValue();
        const letBindings = node.get(1);
        const body = node.drop(2);
        for (let letBinding of letBindings.getValue()) {
            const bindingSym = Array.isArray(letBinding) ? letBinding[0] : letBinding;
            bindingsEnv.setLocal(bindingSym, true, 'variable');
        }
        const visitedLetBindings = visitClausesNode(letBindings, [...stack, node], bindingsEnv);
        const visitedBody = visitFormsList(body, [...stack, node], bindingsEnv);
        if (!((letBindings === visitedLetBindings) && (body === visitedBody))) {
            result = (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([sym, visitedLetBindings, ...visitedBody]));
        }
        return f(result, stack, bindings);
    }
    visitLet.lispSource = [Symbol.for('define'), [Symbol.for('visit-let'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('for'), [[Symbol.for('let-binding'), [Symbol.for('send'), Symbol.for('let-bindings'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('binding-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('let-binding')], [Symbol.for('first'), Symbol.for('let-binding')], Symbol.for('let-binding')]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('binding-sym'), Symbol.for('#t'), 'variable']], [Symbol.for('define'), Symbol.for('visited-let-bindings'), [Symbol.for('visit-clauses-node'), Symbol.for('let-bindings'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings'), Symbol.for('visited-let-bindings')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
    function visitLetValuesP(node) {
        return (0, util_1.formp)(node, letValues_, env);
    }
    visitLetValuesP.lispSource = [Symbol.for('define'), [Symbol.for('visit-let-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-values_'), Symbol.for('env')]];
    function visitLetValues(node, stack, bindings) {
        let result = node;
        let bindingsEnv = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
        const sym = node.get(0).getValue();
        const letBindings = node.get(1);
        const body = node.drop(2);
        const visitedLetBindings = visitFormsNodeWith(function (x) {
            let xResult = x;
            const ids = x.get(0);
            let val = x.get(1);
            const idsExp = ids.getValue();
            if (typeof idsExp === 'symbol') {
                bindingsEnv.setLocal(idsExp, true, 'variable');
            }
            else {
                for (let letBinding of idsExp) {
                    if (typeof letBinding === 'symbol') {
                        bindingsEnv.set(letBinding, true, 'variable');
                    }
                }
            }
            const visitedIds = visitFormsNode(ids, [...stack, node], bindingsEnv);
            const visitedVal = (0, visitor_1.visit)(visitor, val, [...stack, node], bindingsEnv);
            if (!((visitedIds === ids) && (visitedVal === val))) {
                xResult = (0, rose_1.transferComments)(x, (0, rose_1.makeRose)([visitedIds, visitedVal]));
            }
            return xResult;
        }, letBindings, [...stack, node], bindingsEnv);
        const visitedBody = visitFormsList(body, [...stack, node], bindingsEnv);
        if (!((letBindings === visitedLetBindings) && (body === visitedBody))) {
            result = (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([sym, visitedLetBindings, ...visitedBody]));
        }
        return f(result, stack, bindings);
    }
    visitLetValues.lispSource = [Symbol.for('define'), [Symbol.for('visit-let-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-let-bindings'), [Symbol.for('visit-forms-node-with'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('x-result'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('ids'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('ids-exp'), [Symbol.for('send'), Symbol.for('ids'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ids-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('ids-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('let-binding'), Symbol.for('ids-exp')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('let-binding')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set'), Symbol.for('let-binding'), Symbol.for('#t'), 'variable']]]]], [Symbol.for('define'), Symbol.for('visited-ids'), [Symbol.for('visit-forms-node'), Symbol.for('ids'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('val'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('visited-ids'), Symbol.for('ids')], [Symbol.for('eq?'), Symbol.for('visited-val'), Symbol.for('val')]], [Symbol.for('set!'), Symbol.for('x-result'), [Symbol.for('transfer-comments'), Symbol.for('x'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('visited-ids')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], Symbol.for('x-result')], Symbol.for('let-bindings'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings'), Symbol.for('visited-let-bindings')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
    // `(for ...)` form.
    function visitForP(node) {
        return (0, util_1.formp)(node, for_, env);
    }
    visitForP.lispSource = [Symbol.for('define'), [Symbol.for('visit-for-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('for_'), Symbol.for('env')]];
    const visitFor = visitLet;
    // `(while ...)` form.
    function visitWhileP(node) {
        return (0, util_1.formp)(node, jsWhile_, env);
    }
    visitWhileP.lispSource = [Symbol.for('define'), [Symbol.for('visit-while-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-while_'), Symbol.for('env')]];
    const visitWhile = visitFunctionCall;
    // `(cond ...)` form.
    function visitCondP(node) {
        return (0, util_1.formp)(node, cond_, env);
    }
    visitCondP.lispSource = [Symbol.for('define'), [Symbol.for('visit-cond-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('cond_'), Symbol.for('env')]];
    function visitCond(node, stack, bindings) {
        let result = node;
        const sym = node.get(0).getValue();
        const clauses = node.drop(1);
        const visitedClauses = visitClausesList(clauses, [...stack, node], bindings);
        if (visitedClauses !== clauses) {
            result = (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([sym, ...visitedClauses]));
        }
        return f(result, stack, bindings);
    }
    visitCond.lispSource = [Symbol.for('define'), [Symbol.for('visit-cond'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('clauses'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('visited-clauses'), [Symbol.for('visit-clauses-list'), Symbol.for('clauses'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('visited-clauses'), Symbol.for('clauses')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote-splicing'), Symbol.for('visited-clauses')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
    // `(lambda ...)` form.
    function visitLambdaP(node) {
        return (0, util_1.formp)(node, lambda_, env) || (0, util_1.formp)(node, jsFunction_, env) || (0, util_1.formp)(node, jsArrow_, env);
    }
    visitLambdaP.lispSource = [Symbol.for('define'), [Symbol.for('visit-lambda-p'), Symbol.for('node')], [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('node'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-function_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-arrow_'), Symbol.for('env')]]];
    function visitLambda(node, stack, bindings) {
        let result = node;
        let bindingsEnv = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
        const sym = node.get(0).getValue();
        let params = node.get(1);
        const paramsExp = params.getValue();
        const body = node.drop(2);
        if (typeof paramsExp === 'symbol') {
            bindingsEnv.setLocal(paramsExp, true, 'variable');
        }
        else {
            for (let param of paramsExp) {
                if (Array.isArray(param)) {
                    param = param[0];
                }
                bindingsEnv.setLocal(param, true, 'variable');
            }
        }
        const visitedParams = visitClausesNode(params, [...stack, node], bindingsEnv);
        const visitedBody = visitFormsList(body, [...stack, node], bindingsEnv);
        if (!((params === visitedParams) && (body === visitedBody))) {
            result = (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([sym, visitedParams, ...visitedBody]));
        }
        return f(result, stack, bindings);
    }
    visitLambda.lispSource = [Symbol.for('define'), [Symbol.for('visit-lambda'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), Symbol.for('params-exp')]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('param'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-clauses-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
    // `(define ...)` form.
    function visitDefineP(node) {
        return (0, util_1.formp)(node, define_, env);
    }
    visitDefineP.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define_'), Symbol.for('env')]];
    function visitDefine(node, stack, bindings) {
        let result = node;
        const defineSym = node.get(0).getValue();
        let id = node.get(1);
        const idExp = id.getValue();
        const idSym = Array.isArray(idExp) ? idExp[0] : idExp;
        let bindingsEnv = bindings;
        if (Array.isArray(idExp)) {
            bindings.setLocal(idSym, true, 'procedure');
            for (let param of idExp.slice(1)) {
                if (Array.isArray(param)) {
                    param = param[0];
                }
                bindings.setLocal(param, true, 'variable');
            }
            bindingsEnv = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
        }
        else {
            bindings.setLocal(idSym, true, 'variable');
        }
        const body = node.drop(2);
        const visitedId = Array.isArray(idExp) ? visitClausesNode(id, [...stack, node], bindingsEnv) : visitNode(id, [...stack, node], bindingsEnv);
        const visitedBody = visitFormsList(body, [...stack, node], bindingsEnv);
        if (!((id === visitedId) && (body === visitedBody))) {
            result = (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([defineSym, visitedId, ...visitedBody]));
        }
        return f(result, stack, bindings);
    }
    visitDefine.lispSource = [Symbol.for('define'), [Symbol.for('visit-define'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-exp'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('first'), Symbol.for('id-exp')], Symbol.for('id-exp')]], [Symbol.for('define'), Symbol.for('bindings-env'), Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'procedure'], [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('rest'), Symbol.for('id-exp')]]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('param'), Symbol.for('#t'), 'variable']], [Symbol.for('set!'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'variable']]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('visit-clauses-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')], [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
    // `(define-values ...)` form.
    function visitDefineValuesP(node) {
        return (0, util_1.formp)(node, defineValues_, env);
    }
    visitDefineValuesP.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-values_'), Symbol.for('env')]];
    function visitDefineValues(node, stack, bindings) {
        return visitFormsNode(node, stack, bindings, 2);
    }
    visitDefineValues.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 2]];
    // `(defmacro ...)` form.
    function visitDefmacroP(node) {
        return (0, util_1.formp)(node, macros_1.defmacro_, env);
    }
    visitDefmacroP.lispSource = [Symbol.for('define'), [Symbol.for('visit-defmacro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('defmacro_'), Symbol.for('env')]];
    function visitDefmacro(node, stack, bindings) {
        let result = node;
        const defmacroSym = node.get(0).getValue();
        let id = node.get(1);
        const idSym = id.getValue();
        let params = node.get(2);
        const paramsExp = params.getValue();
        const body = node.drop(3);
        bindings.setLocal(idSym, true, 'macro');
        let bindingsEnv = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
        if (typeof paramsExp === 'symbol') {
            bindingsEnv.setLocal(paramsExp, true, 'variable');
        }
        else {
            for (let param of (0, list_1.flatten_)(paramsExp)) {
                bindingsEnv.setLocal(params, true, 'variable');
            }
        }
        const visitedId = visitNode(id, [...stack, node], bindingsEnv);
        const visitedParams = visitFormsNode(params, [...stack, node], bindingsEnv);
        const visitedBody = visitFormsList(body, [...stack, node], bindingsEnv);
        if (!((id === visitedId) && (params === visitedParams) && (body === visitedBody))) {
            result = (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([defmacroSym, visitedId, visitedParams, ...visitedBody]));
        }
        result = f(result, stack, bindings);
        bindings.setLocal(idSym, true, 'macro');
        return result;
    }
    visitDefmacro.lispSource = [Symbol.for('define'), [Symbol.for('visit-defmacro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('defmacro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('defmacro-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], Symbol.for('result')];
    // `(define-macro ...)` form.
    function visitDefineMacroP(node) {
        return (0, util_1.formp)(node, defineMacro_, env);
    }
    visitDefineMacroP.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-macro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-macro_'), Symbol.for('env')]];
    function visitDefineMacro(node, stack, bindings) {
        let result = node;
        const defineMacroSym = node.get(0).getValue();
        const nameAndArgs = node.get(1);
        const nameAndArgsExp = nameAndArgs.getValue();
        const idSym = nameAndArgsExp[0];
        let id = (0, rose_1.makeRose)(idSym, nameAndArgs);
        const paramsExp = cdr(nameAndArgsExp);
        let params = (0, rose_1.makeRose)(paramsExp, nameAndArgs);
        const body = node.drop(2);
        bindings.setLocal(idSym, true, 'macro');
        let bindingsEnv = (0, env_1.extendEnvironment)(new env_1.LispEnvironment(), bindings);
        if (typeof paramsExp === 'symbol') {
            bindingsEnv.setLocal(paramsExp, true, 'variable');
        }
        else {
            for (let param of (0, list_1.flatten_)(paramsExp)) {
                bindingsEnv.setLocal(params, true, 'variable');
            }
        }
        const visitedId = visitNode(id, [...stack, node], bindingsEnv);
        const visitedParams = visitFormsNode(params, [...stack, node], bindingsEnv);
        const visitedBody = visitFormsList(body, [...stack, node], bindingsEnv);
        if (!((id === visitedId) && (params === visitedParams) && (body === visitedBody))) {
            result = (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([defineMacroSym, cons(visitedId, visitedParams), ...visitedBody]));
        }
        result = f(result, stack, bindings);
        bindings.setLocal(idSym, true, 'macro');
        return result;
    }
    visitDefineMacro.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-macro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-macro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('name-and-args-exp'), [Symbol.for('send'), Symbol.for('name-and-args'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('car'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('make-rose'), Symbol.for('id-sym'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('cdr'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('make-rose'), Symbol.for('params-exp'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-macro-sym')], [Symbol.for('unquote'), [Symbol.for('cons'), Symbol.for('visited-id'), Symbol.for('visited-params')]], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], Symbol.for('result')];
    // `(define-class ...)` form.
    function visitDefineClassP(node) {
        return (0, util_1.formp)(node, class_, env);
    }
    visitDefineClassP.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-class-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('class_'), Symbol.for('env')]];
    const visitDefineClass = visitFunctionCall;
    // `(ann ...)` form.
    function visitAnnP(node) {
        return (0, util_1.formp)(node, ann_, env);
    }
    visitAnnP.lispSource = [Symbol.for('define'), [Symbol.for('visit-ann-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('ann_'), Symbol.for('env')]];
    function visitAnn(node, stack, bindings) {
        return visitNode(node, stack, bindings);
    }
    visitAnn.lispSource = [Symbol.for('define'), [Symbol.for('visit-ann'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
    // `(and ...)` form.
    function visitAndP(node) {
        return (0, util_1.formp)(node, and_, env);
    }
    visitAndP.lispSource = [Symbol.for('define'), [Symbol.for('visit-and-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('and_'), Symbol.for('env')]];
    const visitAnd = visitFunctionCall;
    // `(or ...)` form.
    function visitOrP(node) {
        return (0, util_1.formp)(node, or_, env);
    }
    visitOrP.lispSource = [Symbol.for('define'), [Symbol.for('visit-or-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('or_'), Symbol.for('env')]];
    const visitOr = visitFunctionCall;
    // `(when ...)` form.
    function visitWhenP(node) {
        return (0, util_1.formp)(node, macros_1.when_, env);
    }
    visitWhenP.lispSource = [Symbol.for('define'), [Symbol.for('visit-when-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('when_'), Symbol.for('env')]];
    function visitWhen(node, stack, bindings) {
        return visitNonatomic(node, stack, bindings, 1);
    }
    visitWhen.lispSource = [Symbol.for('define'), [Symbol.for('visit-when'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
    // `(unless ...)` form.
    function visitUnlessP(node) {
        return (0, util_1.formp)(node, macros_1.unless_, env);
    }
    visitUnlessP.lispSource = [Symbol.for('define'), [Symbol.for('visit-unless-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('unless_'), Symbol.for('env')]];
    function visitUnless(node, stack, bindings) {
        return visitNonatomic(node, stack, bindings, 1);
    }
    visitUnless.lispSource = [Symbol.for('define'), [Symbol.for('visit-unless'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
    // `(make-object ...)` form.
    function visitMakeObjectP(node) {
        return (0, util_1.formp)(node, new_, env);
    }
    visitMakeObjectP.lispSource = [Symbol.for('define'), [Symbol.for('visit-make-object-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('new_'), Symbol.for('env')]];
    const visitMakeObject = visitFunctionCall;
    // `(return ...)` form.
    function visitReturnP(node) {
        return (0, util_1.formp)(node, return_, env);
    }
    visitReturnP.lispSource = [Symbol.for('define'), [Symbol.for('visit-return-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('return_'), Symbol.for('env')]];
    const visitReturn = visitFunctionCall;
    // `(send ...)` form.
    function visitSendP(node) {
        return (0, util_1.formp)(node, send_, env);
    }
    visitSendP.lispSource = [Symbol.for('define'), [Symbol.for('visit-send-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('send_'), Symbol.for('env')]];
    const visitSend = visitFunctionCall;
    // `(set! ...)` form.
    function visitSetqP(node) {
        return (0, util_1.formp)(node, setX_, env);
    }
    visitSetqP.lispSource = [Symbol.for('define'), [Symbol.for('visit-setq-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set!_'), Symbol.for('env')]];
    const visitSetq = visitFunctionCall;
    // `(set-field! ...)` form.
    function visitSetFieldP(node) {
        return (0, util_1.formp)(node, setField_, env);
    }
    visitSetFieldP.lispSource = [Symbol.for('define'), [Symbol.for('visit-set-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set-field_'), Symbol.for('env')]];
    const visitSetField = visitFunctionCall;
    // `(get-field ...)` form.
    function visitGetFieldP(node) {
        return (0, util_1.formp)(node, getField_, env);
    }
    visitGetFieldP.lispSource = [Symbol.for('define'), [Symbol.for('visit-get-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('get-field_'), Symbol.for('env')]];
    const visitGetField = visitFunctionCall;
    // Quoted value.
    function visitQuoteP(node) {
        return (0, util_1.formp)(node, quote_, env);
    }
    visitQuoteP.lispSource = [Symbol.for('define'), [Symbol.for('visit-quote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quote_'), Symbol.for('env')]];
    const visitQuote = visitNode;
    // Quasiquoted value.
    function visitQuasiquoteP(node) {
        return (0, util_1.formp)(node, quasiquote_, env);
    }
    visitQuasiquoteP.lispSource = [Symbol.for('define'), [Symbol.for('visit-quasiquote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quasiquote_'), Symbol.for('env')]];
    function visitQuasiquote(node, stack, bindings) {
        function visitQuasiquoteForm(node, stack, bindings) {
            let result = node;
            const sym = node.get(0);
            let val = node.get(1);
            // Visit `unquote` and `unquote-splicing` expressions, if any.
            const visitedVal = (0, visitor_1.visit)(quasiquoteVisitor, val, stack, bindings);
            if (val !== visitedVal) {
                result = (0, rose_1.transferComments)(node, (0, rose_1.makeRose)([sym, visitedVal]));
            }
            // Visit the `unquote` expression.
            return f(result, stack, bindings);
        }
        visitQuasiquoteForm.lispSource = [Symbol.for('define'), [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('quasiquote-visitor'), Symbol.for('val'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('visited-val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
        function visitUnquoteP(node) {
            return (0, util_1.taggedListP)(node, Symbol.for('unquote'));
        }
        visitUnquoteP.lispSource = [Symbol.for('define'), [Symbol.for('visit-unquote-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote')]]];
        function visitUnquote(node, stack) {
            // When visiting unquoted expressions,
            // use the regular visitor.
            return visitFormsNodeWith(visitor, node, stack, bindings, 1);
        }
        visitUnquote.lispSource = [Symbol.for('define'), [Symbol.for('visit-unquote'), Symbol.for('node'), Symbol.for('stack')], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
        function visitUnquoteSplicingP(node) {
            return (0, util_1.taggedListP)(node, Symbol.for('unquote-splicing'));
        }
        visitUnquoteSplicingP.lispSource = [Symbol.for('define'), [Symbol.for('visit-unquote-splicing-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]]];
        const visitUnquoteSplicing = visitUnquote;
        function visitQuotedList(node, stack, bindings) {
            return visitFormsNodeWith(quasiquoteVisitor, node, stack, bindings);
        }
        visitQuotedList.lispSource = [Symbol.for('define'), [Symbol.for('visit-quoted-list'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node-with'), Symbol.for('quasiquote-visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
        const quasiquoteVisitor = (0, visitor_1.makeVisitor)([[visitUnquoteP, visitUnquote], [visitUnquoteSplicingP, visitUnquoteSplicing], [visitNonatomicP, visitQuotedList], [visitElseP, skipNode]]);
        return visitQuasiquoteForm(node, [...stack, node], bindings);
    }
    visitQuasiquote.lispSource = [Symbol.for('define'), [Symbol.for('visit-quasiquote'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('quasiquote-visitor'), Symbol.for('val'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('visited-val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-unquote-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote')]]], [Symbol.for('define'), [Symbol.for('visit-unquote'), Symbol.for('node'), Symbol.for('stack')], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-unquote-splicing-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]]], [Symbol.for('define'), Symbol.for('visit-unquote-splicing'), Symbol.for('visit-unquote')], [Symbol.for('define'), [Symbol.for('visit-quoted-list'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node-with'), Symbol.for('quasiquote-visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('quasiquote-visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('visit-unquote-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote')]], [[Symbol.for('unquote'), Symbol.for('visit-unquote-splicing-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote-splicing')]], [[Symbol.for('unquote'), Symbol.for('visit-nonatomic-p')], [Symbol.for('unquote'), Symbol.for('visit-quoted-list')]], [[Symbol.for('unquote'), Symbol.for('visit-else-p')], [Symbol.for('unquote'), Symbol.for('skip-node')]]]]]], [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]];
    // List.
    function visitNonatomicP(node) {
        let exp = node.getValue();
        return Array.isArray(exp);
    }
    visitNonatomicP.lispSource = [Symbol.for('define'), [Symbol.for('visit-nonatomic-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('array?'), Symbol.for('exp')]]];
    // Atomic value.
    const visitAtomP = visitElseP;
    const visitAtom = visitNode;
    // Rename this to `map-visitor` to distinguish it from
    // the `visitor` parameter of many functions.
    const visitor = (0, visitor_1.makeVisitor)([[visitModuleP, visitModule], [visitBeginP, visitBegin], [visitBegin0P, visitBegin0], [visitLetP, visitLet], [visitLetValuesP, visitLetValues], [visitCondP, visitCond], [visitLambdaP, visitLambda], [visitDefineP, visitDefine], [visitDefineValuesP, visitDefineValues], [visitDefineMacroP, visitDefineMacro], [visitDefmacroP, visitDefmacro], [visitAnnP, visitAnn], [visitAndP, visitAnd], [visitOrP, visitOr], [visitForP, visitFor], [visitWhileP, visitWhile], [visitWhenP, visitWhen], [visitSendP, visitSend], [visitSetqP, visitSetq], [visitSetFieldP, visitSetField], [visitGetFieldP, visitGetField], [visitUnlessP, visitUnless], [visitDefineClassP, visitDefineClass], [visitMakeObjectP, visitMakeObject], [visitReturnP, visitReturn], [visitQuoteP, visitQuote], [visitQuasiquoteP, visitQuasiquote], [visitMacroCallP, visitMacroCall], [visitSpecialFormP, visitSpecialForm], [visitFunctionCallP, visitFunctionCall], [visitNonatomicP, visitNonatomic], [visitElseP, visitAtom]]);
    return (0, visitor_1.visit)(visitor, node, stack, bindings);
}
exports.mapVisitRose = mapVisitRose;
mapVisitRose.lispSource = [Symbol.for('define'), [Symbol.for('map-visit-rose'), Symbol.for('f'), Symbol.for('node'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('define'), [Symbol.for('skip-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], Symbol.for('node')], [Symbol.for('define'), [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('f'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('visit-forms-node'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-macro-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('macro-call?'), Symbol.for('exp'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('visit-macro-call'), Symbol.for('visit-node')], [Symbol.for('define'), [Symbol.for('visit-special-form-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('special-form?'), Symbol.for('exp'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('visit-special-form'), Symbol.for('visit-node')], [Symbol.for('define'), [Symbol.for('visit-function-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('function-call?'), Symbol.for('exp'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('visit-function-call'), Symbol.for('visit-nonatomic')], [Symbol.for('define'), [Symbol.for('visit-else-p'), Symbol.for('node')], Symbol.for('#t')], [Symbol.for('define'), [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-nodes')]], [Symbol.for('define'), Symbol.for('result-nodes'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('result-nodes'), Symbol.for('nodes')], Symbol.for('node')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('new'), Symbol.for('Rose'), Symbol.for('exp')]]], [Symbol.for('for'), [[Symbol.for('node'), Symbol.for('result-nodes')]], [Symbol.for('push-right!'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('send'), Symbol.for('result'), Symbol.for('insert'), Symbol.for('node')]], Symbol.for('result')]]], [Symbol.for('define'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('nodes')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('is-modified'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), Symbol.for('skip')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('is-modified'), Symbol.for('#t')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x1')]]], Symbol.for('nodes')]], [Symbol.for('unless'), Symbol.for('is-modified'), [Symbol.for('set!'), Symbol.for('result'), Symbol.for('nodes')]], Symbol.for('result')], [Symbol.for('define'), [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-forms-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-clauses-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-clauses-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visit-forms-node'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-module-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('module_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-module'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 3]], [Symbol.for('define'), [Symbol.for('visit-begin-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-begin'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-begin0-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin0_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-begin0'), Symbol.for('visit-begin')], [Symbol.for('define'), [Symbol.for('visit-let-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-star_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-let'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('for'), [[Symbol.for('let-binding'), [Symbol.for('send'), Symbol.for('let-bindings'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('binding-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('let-binding')], [Symbol.for('first'), Symbol.for('let-binding')], Symbol.for('let-binding')]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('binding-sym'), Symbol.for('#t'), 'variable']], [Symbol.for('define'), Symbol.for('visited-let-bindings'), [Symbol.for('visit-clauses-node'), Symbol.for('let-bindings'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings'), Symbol.for('visited-let-bindings')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-let-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-values_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-let-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-let-bindings'), [Symbol.for('visit-forms-node-with'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('x-result'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('ids'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('ids-exp'), [Symbol.for('send'), Symbol.for('ids'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ids-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('ids-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('let-binding'), Symbol.for('ids-exp')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('let-binding')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set'), Symbol.for('let-binding'), Symbol.for('#t'), 'variable']]]]], [Symbol.for('define'), Symbol.for('visited-ids'), [Symbol.for('visit-forms-node'), Symbol.for('ids'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('val'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('visited-ids'), Symbol.for('ids')], [Symbol.for('eq?'), Symbol.for('visited-val'), Symbol.for('val')]], [Symbol.for('set!'), Symbol.for('x-result'), [Symbol.for('transfer-comments'), Symbol.for('x'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('visited-ids')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], Symbol.for('x-result')], Symbol.for('let-bindings'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings'), Symbol.for('visited-let-bindings')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-for-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('for_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-for'), Symbol.for('visit-let')], [Symbol.for('define'), [Symbol.for('visit-while-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-while_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-while'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-cond-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('cond_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-cond'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('clauses'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('visited-clauses'), [Symbol.for('visit-clauses-list'), Symbol.for('clauses'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('visited-clauses'), Symbol.for('clauses')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote-splicing'), Symbol.for('visited-clauses')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-lambda-p'), Symbol.for('node')], [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('node'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-function_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-arrow_'), Symbol.for('env')]]], [Symbol.for('define'), [Symbol.for('visit-lambda'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), Symbol.for('params-exp')]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('param'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-clauses-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-define-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-define'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-exp'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('first'), Symbol.for('id-exp')], Symbol.for('id-exp')]], [Symbol.for('define'), Symbol.for('bindings-env'), Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'procedure'], [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('rest'), Symbol.for('id-exp')]]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('param'), Symbol.for('#t'), 'variable']], [Symbol.for('set!'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'variable']]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('visit-clauses-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')], [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-define-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-values_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-define-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 2]], [Symbol.for('define'), [Symbol.for('visit-defmacro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('defmacro_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-defmacro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('defmacro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('defmacro-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], Symbol.for('result')], [Symbol.for('define'), [Symbol.for('visit-define-macro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-macro_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-define-macro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-macro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('name-and-args-exp'), [Symbol.for('send'), Symbol.for('name-and-args'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('car'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('make-rose'), Symbol.for('id-sym'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('cdr'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('make-rose'), Symbol.for('params-exp'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-macro-sym')], [Symbol.for('unquote'), [Symbol.for('cons'), Symbol.for('visited-id'), Symbol.for('visited-params')]], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], Symbol.for('result')], [Symbol.for('define'), [Symbol.for('visit-define-class-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('class_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-define-class'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-ann-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('ann_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-ann'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-and-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('and_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-and'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-or-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('or_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-or'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-when-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('when_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-when'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-unless-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('unless_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-unless'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-make-object-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('new_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-make-object'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-return-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('return_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-return'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-send-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('send_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-send'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-setq-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set!_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-setq'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-set-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set-field_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-set-field'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-get-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('get-field_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-get-field'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-quote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quote_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-quote'), Symbol.for('visit-node')], [Symbol.for('define'), [Symbol.for('visit-quasiquote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quasiquote_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-quasiquote'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('quasiquote-visitor'), Symbol.for('val'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('visited-val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-unquote-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote')]]], [Symbol.for('define'), [Symbol.for('visit-unquote'), Symbol.for('node'), Symbol.for('stack')], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-unquote-splicing-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]]], [Symbol.for('define'), Symbol.for('visit-unquote-splicing'), Symbol.for('visit-unquote')], [Symbol.for('define'), [Symbol.for('visit-quoted-list'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node-with'), Symbol.for('quasiquote-visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('quasiquote-visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('visit-unquote-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote')]], [[Symbol.for('unquote'), Symbol.for('visit-unquote-splicing-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote-splicing')]], [[Symbol.for('unquote'), Symbol.for('visit-nonatomic-p')], [Symbol.for('unquote'), Symbol.for('visit-quoted-list')]], [[Symbol.for('unquote'), Symbol.for('visit-else-p')], [Symbol.for('unquote'), Symbol.for('skip-node')]]]]]], [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-nonatomic-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('array?'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('visit-atom-p'), Symbol.for('visit-else-p')], [Symbol.for('define'), Symbol.for('visit-atom'), Symbol.for('visit-node')], [Symbol.for('define'), Symbol.for('visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('visit-module-p')], [Symbol.for('unquote'), Symbol.for('visit-module')]], [[Symbol.for('unquote'), Symbol.for('visit-begin-p')], [Symbol.for('unquote'), Symbol.for('visit-begin')]], [[Symbol.for('unquote'), Symbol.for('visit-begin0-p')], [Symbol.for('unquote'), Symbol.for('visit-begin0')]], [[Symbol.for('unquote'), Symbol.for('visit-let-p')], [Symbol.for('unquote'), Symbol.for('visit-let')]], [[Symbol.for('unquote'), Symbol.for('visit-let-values-p')], [Symbol.for('unquote'), Symbol.for('visit-let-values')]], [[Symbol.for('unquote'), Symbol.for('visit-cond-p')], [Symbol.for('unquote'), Symbol.for('visit-cond')]], [[Symbol.for('unquote'), Symbol.for('visit-lambda-p')], [Symbol.for('unquote'), Symbol.for('visit-lambda')]], [[Symbol.for('unquote'), Symbol.for('visit-define-p')], [Symbol.for('unquote'), Symbol.for('visit-define')]], [[Symbol.for('unquote'), Symbol.for('visit-define-values-p')], [Symbol.for('unquote'), Symbol.for('visit-define-values')]], [[Symbol.for('unquote'), Symbol.for('visit-define-macro-p')], [Symbol.for('unquote'), Symbol.for('visit-define-macro')]], [[Symbol.for('unquote'), Symbol.for('visit-defmacro-p')], [Symbol.for('unquote'), Symbol.for('visit-defmacro')]], [[Symbol.for('unquote'), Symbol.for('visit-ann-p')], [Symbol.for('unquote'), Symbol.for('visit-ann')]], [[Symbol.for('unquote'), Symbol.for('visit-and-p')], [Symbol.for('unquote'), Symbol.for('visit-and')]], [[Symbol.for('unquote'), Symbol.for('visit-or-p')], [Symbol.for('unquote'), Symbol.for('visit-or')]], [[Symbol.for('unquote'), Symbol.for('visit-for-p')], [Symbol.for('unquote'), Symbol.for('visit-for')]], [[Symbol.for('unquote'), Symbol.for('visit-while-p')], [Symbol.for('unquote'), Symbol.for('visit-while')]], [[Symbol.for('unquote'), Symbol.for('visit-when-p')], [Symbol.for('unquote'), Symbol.for('visit-when')]], [[Symbol.for('unquote'), Symbol.for('visit-send-p')], [Symbol.for('unquote'), Symbol.for('visit-send')]], [[Symbol.for('unquote'), Symbol.for('visit-setq-p')], [Symbol.for('unquote'), Symbol.for('visit-setq')]], [[Symbol.for('unquote'), Symbol.for('visit-set-field-p')], [Symbol.for('unquote'), Symbol.for('visit-set-field')]], [[Symbol.for('unquote'), Symbol.for('visit-get-field-p')], [Symbol.for('unquote'), Symbol.for('visit-get-field')]], [[Symbol.for('unquote'), Symbol.for('visit-unless-p')], [Symbol.for('unquote'), Symbol.for('visit-unless')]], [[Symbol.for('unquote'), Symbol.for('visit-define-class-p')], [Symbol.for('unquote'), Symbol.for('visit-define-class')]], [[Symbol.for('unquote'), Symbol.for('visit-make-object-p')], [Symbol.for('unquote'), Symbol.for('visit-make-object')]], [[Symbol.for('unquote'), Symbol.for('visit-return-p')], [Symbol.for('unquote'), Symbol.for('visit-return')]], [[Symbol.for('unquote'), Symbol.for('visit-quote-p')], [Symbol.for('unquote'), Symbol.for('visit-quote')]], [[Symbol.for('unquote'), Symbol.for('visit-quasiquote-p')], [Symbol.for('unquote'), Symbol.for('visit-quasiquote')]], [[Symbol.for('unquote'), Symbol.for('visit-macro-call-p')], [Symbol.for('unquote'), Symbol.for('visit-macro-call')]], [[Symbol.for('unquote'), Symbol.for('visit-special-form-p')], [Symbol.for('unquote'), Symbol.for('visit-special-form')]], [[Symbol.for('unquote'), Symbol.for('visit-function-call-p')], [Symbol.for('unquote'), Symbol.for('visit-function-call')]], [[Symbol.for('unquote'), Symbol.for('visit-nonatomic-p')], [Symbol.for('unquote'), Symbol.for('visit-nonatomic')]], [[Symbol.for('unquote'), Symbol.for('visit-else-p')], [Symbol.for('unquote'), Symbol.for('visit-atom')]]]]]], [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
/**
 * Map the function `f` over the S-expression `exp`.
 * The S-expression is processed in bottom-up order.
 */
function mapSexp(f, exp, env = new env_1.LispEnvironment(), stack = [], bindings = new env_1.LispEnvironment()) {
    const f1 = function (x, stack, bindings) {
        {
            let exp = x.getValue();
            const stack1 = stack.map(function (x) {
                if (x instanceof rose_1.Rose) {
                    return x.getValue();
                }
                else {
                    return x;
                }
            });
            let result = f(exp, stack1, bindings);
            if (result === exp) {
                return x;
            }
            else {
                return (0, rose_1.makeRose)(result, x);
            }
        }
    };
    const isRose = exp instanceof rose_1.Rose;
    let node = isRose ? exp : (0, rose_1.makeRose)(exp);
    let result = mapRose(f1, node, env, stack, bindings);
    // If the input is a rose tree node,
    // return a rose tree node as output too.
    if (isRose) {
        return result;
    }
    else {
        return result.getValue();
    }
}
exports.mapSexp = mapSexp;
mapSexp.lispSource = [Symbol.for('define'), [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('exp'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('let*'), [[Symbol.for('f1'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('let*'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('stack1'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')], Symbol.for('x')]], Symbol.for('stack')]], [Symbol.for('result'), [Symbol.for('f'), Symbol.for('exp'), Symbol.for('stack1'), Symbol.for('bindings')]]], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('result'), Symbol.for('exp')], Symbol.for('x'), [Symbol.for('make-rose'), Symbol.for('result'), Symbol.for('x')]]]]], [Symbol.for('is-rose'), [Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')]], [Symbol.for('node'), [Symbol.for('if'), Symbol.for('is-rose'), Symbol.for('exp'), [Symbol.for('make-rose'), Symbol.for('exp')]]], [Symbol.for('result'), [Symbol.for('map-rose'), Symbol.for('f1'), Symbol.for('node'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('if'), Symbol.for('is-rose'), Symbol.for('result'), [Symbol.for('send'), Symbol.for('result'), Symbol.for('get-value')]]]];
/**
 * Call the function `f` on each node of a rose tree,
 * but do not create a new rose tree in the process.
 */
function iterateRose(f, node, env = new env_1.LispEnvironment()) {
    return mapRose(function (x, stack) {
        f(x, stack);
        return x;
    }, node, env);
}
exports.iterateRose = iterateRose;
iterateRose.lispSource = [Symbol.for('define'), [Symbol.for('iterate-rose'), Symbol.for('f'), Symbol.for('node'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('map-rose'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('stack')], [Symbol.for('f'), Symbol.for('x'), Symbol.for('stack')], Symbol.for('x')], Symbol.for('node'), Symbol.for('env')]];
/**
 * Expand an `(ann ...)` expression.
 */
function ann_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.ann = ann_;
exports.ann_ = ann_;
ann_.lispSource = [Symbol.for('define'), [Symbol.for('ann_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
ann_.lispMacro = true;
/**
 * Expand a `(: ...)` expression.
 */
function colon_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.colon = colon_;
exports.colon_ = colon_;
colon_.lispSource = [Symbol.for('define'), [Symbol.for('colon_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
colon_.lispMacro = true;
/**
 * Expand a `(define-type ...)` expression.
 */
function defineType_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.defineType = defineType_;
exports.defineType_ = defineType_;
defineType_.lispSource = [Symbol.for('define'), [Symbol.for('define-type_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
defineType_.lispMacro = true;
/**
 * Expand a `(let-js-obj ...)` expression.
 */
function letJsObj_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.letJsObj = letJsObj_;
exports.letJsObj_ = letJsObj_;
letJsObj_.lispSource = [Symbol.for('define'), [Symbol.for('let-js-obj_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
letJsObj_.lispMacro = true;
/**
 * Expand a `(define-js-obj ...)` expression.
 */
function defineJsObj_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.defineJsObj = defineJsObj_;
exports.defineJsObj_ = defineJsObj_;
defineJsObj_.lispSource = [Symbol.for('define'), [Symbol.for('define-js-obj_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
defineJsObj_.lispMacro = true;
/**
 * Expand a `(set!-js-obj ...)` expression.
 */
function setJsObj_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
exports.setXJsObj = setJsObj_;
exports.setJsObjX = setJsObj_;
exports.setJsObj = setJsObj_;
exports.setJsObj_ = setJsObj_;
setJsObj_.lispSource = [Symbol.for('define'), [Symbol.for('set-js-obj_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
setJsObj_.lispMacro = true;
/**
 * Compile a `(js/switch ...)` form.
 */
function compileJsSwitch(node, env, options = {}) {
    const expressionType = options['expressionType'];
    if ((expressionType === 'statement') || (expressionType === 'return')) {
        const discriminant = node.get(1);
        const discriminantCompiled = compileExpression(discriminant, env, options);
        const cases = node.drop(2);
        const casesCompiled = cases.map(function (x) {
            const op = x.get(0).getValue();
            let testCompiled;
            let consequentCompiled;
            if (op === Symbol.for('case')) {
                let test = x.get(1);
                testCompiled = compileExpression(test, env, options);
                const consequent = x.drop(2);
                // It is advisable to wrap cases in a block statement.
                // <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/switch#lexical_scoping>
                consequentCompiled = [compileStatementOrReturnStatement((0, rose_1.makeRose)([Symbol.for('block'), ...consequent]), env, options)];
            }
            else {
                testCompiled = null;
                const consequent = x.drop(1);
                consequentCompiled = [compileStatementOrReturnStatement((0, rose_1.makeRose)([Symbol.for('block'), ...consequent]), env, options)];
            }
            return new estree_1.SwitchCase(testCompiled, consequentCompiled);
        });
        return new estree_1.SwitchStatement(discriminantCompiled, casesCompiled);
    }
    else {
        return compileExpression(wrapInArrowCall(node), env, options);
    }
}
compileJsSwitch.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-switch'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('discriminant'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('discriminant-compiled'), [Symbol.for('compile-expression'), Symbol.for('discriminant'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('cases'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('cases-compiled'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('~>'), Symbol.for('x'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('test-compiled')], [Symbol.for('define'), Symbol.for('consequent-compiled')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('op'), [Symbol.for('quote'), Symbol.for('case')]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('set!'), Symbol.for('test-compiled'), [Symbol.for('compile-expression'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('drop'), 2]], [Symbol.for('set!'), Symbol.for('consequent-compiled'), [Symbol.for('list'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('block'), [Symbol.for('unquote-splicing'), Symbol.for('consequent')]]]], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('test-compiled'), Symbol.for('js/null')], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('drop'), 1]], [Symbol.for('set!'), Symbol.for('consequent-compiled'), [Symbol.for('list'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('block'), [Symbol.for('unquote-splicing'), Symbol.for('consequent')]]]], Symbol.for('env'), Symbol.for('options')]]]]], [Symbol.for('new'), Symbol.for('SwitchCase'), Symbol.for('test-compiled'), Symbol.for('consequent-compiled')]], Symbol.for('cases')]], [Symbol.for('new'), Symbol.for('SwitchStatement'), Symbol.for('discriminant-compiled'), Symbol.for('cases-compiled')]], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]]];
/**
 * Expand a `(js/switch ...)` expression.
 */
function jsSwitch_(exp, env) {
    return compileSexp(exp, env, currentCompilationOptions());
}
jsSwitch_.lispSource = [Symbol.for('define'), [Symbol.for('js-switch_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];
jsSwitch_.lispMacro = true;
/**
 * Simple `call-with-current-continuation` implementation.
 * Also known as `call/cc`.
 *
 * Similar to
 * [`call-with-current-continuation` in Racket][rkt:call-with-current-continuation].
 *
 * [rkt:call-with-current-continuation]: https://docs.racket-lang.org/reference/cont.html#%28def._%28%28quote._~23~25kernel%29._call-with-current-continuation%29%29
 */
function callWithCurrentContinuation_(proc, promptTag = undefined) {
    class CallCCWrapper {
        constructor(value) {
            this.value = value;
        }
    }
    try {
        return proc((value) => {
            throw new CallCCWrapper(value);
        });
    }
    catch (e) {
        if (e instanceof CallCCWrapper) {
            return e.value;
        }
        else {
            throw e;
        }
    }
}
exports.callWithCurrentContinuation = callWithCurrentContinuation_;
exports.callCc = callWithCurrentContinuation_;
callWithCurrentContinuation_.lispSource = [Symbol.for('define'), [Symbol.for('call-with-current-continuation_'), Symbol.for('proc'), [Symbol.for('prompt-tag'), Symbol.for('undefined')]], [Symbol.for('define-class'), Symbol.for('CallCCWrapper'), [], [Symbol.for('define/public'), Symbol.for('value')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('value')], [Symbol.for('set-field!'), Symbol.for('value'), Symbol.for('this'), Symbol.for('value')]]], [Symbol.for('try'), [Symbol.for('return'), [Symbol.for('proc'), [Symbol.for('js/arrow'), [Symbol.for('value')], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('CallCCWrapper'), Symbol.for('value')]]]]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e'), [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('e'), Symbol.for('CallCCWrapper')], [Symbol.for('return'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('e')]]], [Symbol.for('else'), [Symbol.for('throw'), Symbol.for('e')]]]]]];
/**
 * Traverse an ESTree tree.
 */
function traverseEstree(node, enter = undefined, leave = undefined, replace = undefined) {
    let result = node;
    let el;
    let el1;
    let val;
    let val1;
    if (!(node instanceof estree_1.Node)) {
        return result;
    }
    if (enter) {
        enter(node);
    }
    for (let key of Object.keys(node)) {
        val = node[key];
        if (Array.isArray(val)) {
            const _end = val.length;
            for (let i = 0; i < _end; i++) {
                el = val[i];
                el1 = traverseEstree(el, enter, leave, replace);
                if (el !== el1) {
                    val[i] = el1;
                }
            }
        }
        else {
            val1 = traverseEstree(val, enter, leave, replace);
            if (val !== val1) {
                node[key] = val1;
            }
        }
    }
    if (leave) {
        leave(node);
    }
    if (replace) {
        result = replace(node);
    }
    return result;
}
exports.traverseEstree = traverseEstree;
traverseEstree.lispSource = [Symbol.for('define'), [Symbol.for('traverse-estree'), Symbol.for('node'), [Symbol.for('enter'), Symbol.for('undefined')], [Symbol.for('leave'), Symbol.for('undefined')], [Symbol.for('replace'), Symbol.for('undefined')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('el')], [Symbol.for('define'), Symbol.for('el1')], [Symbol.for('define'), Symbol.for('val')], [Symbol.for('define'), Symbol.for('val1')], [Symbol.for('unless'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Node')], [Symbol.for('return'), Symbol.for('result')]], [Symbol.for('when'), Symbol.for('enter'), [Symbol.for('enter'), Symbol.for('node')]], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('js-keys'), Symbol.for('node')]]], [Symbol.for('set!'), Symbol.for('val'), [Symbol.for('oget'), Symbol.for('node'), Symbol.for('key')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('val')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('val')]]]], [Symbol.for('set!'), Symbol.for('el'), [Symbol.for('aget'), Symbol.for('val'), Symbol.for('i')]], [Symbol.for('set!'), Symbol.for('el1'), [Symbol.for('traverse-estree'), Symbol.for('el'), Symbol.for('enter'), Symbol.for('leave'), Symbol.for('replace')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('el'), Symbol.for('el1')], [Symbol.for('list-set!'), Symbol.for('val'), Symbol.for('i'), Symbol.for('el1')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('val1'), [Symbol.for('traverse-estree'), Symbol.for('val'), Symbol.for('enter'), Symbol.for('leave'), Symbol.for('replace')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('val1')], [Symbol.for('oset!'), Symbol.for('node'), Symbol.for('key'), Symbol.for('val1')]]]]], [Symbol.for('when'), Symbol.for('leave'), [Symbol.for('leave'), Symbol.for('node')]], [Symbol.for('when'), Symbol.for('replace'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('replace'), Symbol.for('node')]]], Symbol.for('result')];
/**
 * Find ESTree nodes matching a predicate.
 */
function findEstree(pred, node) {
    const nodes = [];
    traverseEstree(node, function (x) {
        if (pred(x)) {
            nodes.push(x);
            return nodes;
        }
    });
    return nodes;
}
exports.findEstree = findEstree;
findEstree.lispSource = [Symbol.for('define'), [Symbol.for('find-estree'), Symbol.for('pred'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('nodes'), [Symbol.for('quote'), []]], [Symbol.for('traverse-estree'), Symbol.for('node'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('when'), [Symbol.for('pred'), Symbol.for('x')], [Symbol.for('push-right!'), Symbol.for('nodes'), Symbol.for('x')]]]], Symbol.for('nodes')];
/**
 * Optimize an S-expression.
 */
function optimizeSexp(exp, env) {
    if ((0, rose_1.rosep)(exp)) {
        return optimizeRose(exp, env);
    }
    else {
        return optimizeRose((0, rose_1.makeRose)(exp), env).getValue();
    }
}
exports.optimizeSexp = optimizeSexp;
optimizeSexp.lispSource = [Symbol.for('define'), [Symbol.for('optimize-sexp'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('rose?'), Symbol.for('exp')], [Symbol.for('optimize-rose'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('~>'), [Symbol.for('make-rose'), Symbol.for('exp')], [Symbol.for('optimize-rose'), Symbol.for('_'), Symbol.for('env')], [Symbol.for('send'), Symbol.for('_'), Symbol.for('get-value')]]]]];
/**
 * Optimize a rose tree-wrapped S-expression.
 */
function optimizeRose(exp, env) {
    return applyOptimizations(exp, env);
}
exports.optimizeRose = optimizeRose;
optimizeRose.lispSource = [Symbol.for('define'), [Symbol.for('optimize-rose'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('apply-optimizations'), Symbol.for('exp'), Symbol.for('env')]];
/**
 * Optimize a module.
 */
function optimizeModule(m, env) {
    return m.setNodes(m.mainNodes.map(function (x) {
        return optimizeSexp(x, env);
    }));
}
exports.optimizeModule = optimizeModule;
optimizeModule.lispSource = [Symbol.for('define'), [Symbol.for('optimize-module'), Symbol.for('m'), Symbol.for('env')], [Symbol.for('send'), Symbol.for('m'), Symbol.for('set-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('optimize-sexp'), Symbol.for('x'), Symbol.for('env')]], [Symbol.for('get-field'), Symbol.for('main-nodes'), Symbol.for('m')]]]];
/**
 * Optimize an ESTree tree.
 */
function optimizeEstree(exp) {
    return letVarsToConstVars(exp);
}
exports.optimizeEstree = optimizeEstree;
optimizeEstree.lispSource = [Symbol.for('define'), [Symbol.for('optimize-estree'), Symbol.for('exp')], [Symbol.for('~>'), Symbol.for('exp'), [Symbol.for('let-vars-to-const-vars')]]];
function letVarsToConstVars(program) {
    const variables = [];
    traverseEstree(program, function (node) {
        const varNames = [];
        if ((0, estree_1.estreeTypeP)(node, 'AssignmentExpression')) {
            if ((0, estree_1.estreeTypeP)(node.left, 'Identifier')) {
                varNames.unshift(node.left.name);
            }
            else if ((0, estree_1.estreeTypeP)(node.left, 'ArrayPattern')) {
                for (let element of node.left.elements) {
                    if (element && (0, estree_1.estreeTypeP)(element, 'Identifier')) {
                        varNames.unshift(element.name);
                    }
                }
            }
        }
        else if ((0, estree_1.estreeTypeP)(node, 'UpdateExpression')) {
            if ((0, estree_1.estreeTypeP)(node.argument, 'Identifier')) {
                varNames.unshift(node.argument.name);
            }
        }
        for (let varName of varNames) {
            if (!variables.includes(varName)) {
                variables.unshift(varName);
            }
        }
    });
    return traverseEstree(program, undefined, undefined, function (node) {
        if ((0, estree_1.estreeTypeP)(node, 'VariableDeclaration')) {
            if (!findf(function (x) {
                return !x.init || (findEstree(function (y) {
                    return (0, estree_1.estreeTypeP)(y, 'Identifier') && variables.includes(y.name);
                }, x.id).length !== 0);
            }, node.declarations)) {
                node.kind = 'const';
            }
            return node;
        }
        else {
            return node;
        }
    });
}
exports.letVarsToConstVars = letVarsToConstVars;
letVarsToConstVars.lispSource = [Symbol.for('define'), [Symbol.for('let-vars-to-const-vars'), Symbol.for('program')], [Symbol.for('define'), Symbol.for('variables'), [Symbol.for('quote'), []]], [Symbol.for('traverse-estree'), Symbol.for('program'), [Symbol.for('lambda'), [Symbol.for('node')], [Symbol.for('define'), Symbol.for('var-names'), [Symbol.for('quote'), []]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('node'), 'AssignmentExpression'], [Symbol.for('cond'), [[Symbol.for('estree-type?'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')], 'Identifier'], [Symbol.for('push!'), Symbol.for('var-names'), [Symbol.for('get-field'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]]]], [[Symbol.for('estree-type?'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')], 'ArrayPattern'], [Symbol.for('for'), [[Symbol.for('element'), [Symbol.for('get-field'), Symbol.for('elements'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('element'), [Symbol.for('estree-type?'), Symbol.for('element'), 'Identifier']], [Symbol.for('push!'), Symbol.for('var-names'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('element')]]]]]]], [[Symbol.for('estree-type?'), Symbol.for('node'), 'UpdateExpression'], [Symbol.for('when'), [Symbol.for('estree-type?'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], 'Identifier'], [Symbol.for('push!'), Symbol.for('var-names'), [Symbol.for('get-field'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]]]]]], [Symbol.for('for'), [[Symbol.for('var-name'), Symbol.for('var-names')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('var-name'), Symbol.for('variables')], [Symbol.for('push!'), Symbol.for('variables'), Symbol.for('var-name')]]]]], [Symbol.for('traverse-estree'), Symbol.for('program'), Symbol.for('undefined'), Symbol.for('undefined'), [Symbol.for('lambda'), [Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('node'), 'VariableDeclaration'], [Symbol.for('unless'), [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('x')]], [Symbol.for('not'), [Symbol.for('zero?'), [Symbol.for('array-list-length'), [Symbol.for('find-estree'), [Symbol.for('lambda'), [Symbol.for('y')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('y'), 'Identifier'], [Symbol.for('memq?'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('y')], Symbol.for('variables')]]], [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('x')]]]]]]], [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('node')]], [Symbol.for('set-field!'), Symbol.for('kind'), Symbol.for('node'), 'const']], Symbol.for('node')], [Symbol.for('else'), Symbol.for('node')]]]]];
/**
 * Find a optimization rule matching `node`.
 */
function findOptimization(node, env, rules = optimizations) {
    for (let rule of rules) {
        const [predicate] = rule;
        if (predicate(node, env)) {
            return rule;
        }
    }
    return false;
}
findOptimization.lispSource = [Symbol.for('define'), [Symbol.for('find-optimization'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('rules'), Symbol.for('optimizations')]], [Symbol.for('for'), [[Symbol.for('rule'), Symbol.for('rules')]], [Symbol.for('define-values'), [Symbol.for('predicate')], Symbol.for('rule')], [Symbol.for('when'), [Symbol.for('predicate'), Symbol.for('node'), Symbol.for('env')], [Symbol.for('return'), Symbol.for('rule')]]], Symbol.for('#f')];
/**
 * Apply optimizations to `node`.
 */
function applyOptimizations(node, env, rules = optimizations) {
    let result = node;
    let rule = false;
    while ((rule = findOptimization(result, env, rules))) {
        const [predicate, optimizer] = rule;
        result = optimizer(result, env);
    }
    return result;
}
exports.applyOptimizations = applyOptimizations;
applyOptimizations.lispSource = [Symbol.for('define'), [Symbol.for('apply-optimizations'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('rules'), Symbol.for('optimizations')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('rule'), Symbol.for('#f')], [Symbol.for('while'), [Symbol.for('set!'), Symbol.for('rule'), [Symbol.for('find-optimization'), Symbol.for('result'), Symbol.for('env'), Symbol.for('rules')]], [Symbol.for('define-values'), [Symbol.for('predicate'), Symbol.for('optimizer')], Symbol.for('rule')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('optimizer'), Symbol.for('result'), Symbol.for('env')]]], Symbol.for('result')];
/**
 * List of `(predicate optimizer)` tuples.
 */
const optimizations = [];
exports.optimizations = optimizations;
/**
 * Module class.
 */
class Module {
    constructor(nodes = [], parent = langEnvironment, name = '') {
        this.name = '';
        this.headerExpressions = [];
        this.headerNodes = [];
        this.requireExpressions = [];
        this.requireNodes = [];
        this.provideExpressions = [];
        this.provideNodes = [];
        this.mainExpressions = [];
        this.mainNodes = [];
        this.expressions = [];
        this.nodes = [];
        this.inlineLispSourcesFlag = false;
        this.seenModules = [];
        this.symbolMap = new Map();
        this.parentEnvironment = parent;
        this.name = name;
        this.initializeNodes(nodes);
    }
    getExpressions() {
        return this.expressions;
    }
    getEnvironment() {
        if (this.environment) {
            return this.environment;
        }
        else {
            return this.makeEnvironment(this.parentEnvironment);
        }
    }
    getModuleMap() {
        return this.moduleMap;
    }
    getName() {
        return this.name;
    }
    /**
     * Whether a particular symbol is bound in this module's scope
     * (i.e., whether the module imports or defines the symbol).
     */
    hasSymbol(sym) {
        const key = (typeof sym === 'string') ? Symbol.for(sym) : sym;
        return this.symbolMap.has(key);
    }
    makeHeaderNode(nodes = []) {
        // Create header node if there is more than one comment, or if
        // there is a single comment ending in a blank line.
        if (length(nodes) > 0) {
            const initialNode = nodes[0];
            let comments = initialNode.getProperty('comments');
            let initialNodeComments = [];
            let initialNodeCommentString = undefined;
            let headerComments = [];
            let headerCommentStrings = [];
            if (comments) {
                this.findInlineLispSourcesComment(comments);
                let commentStrings = [];
                for (let comment of comments) {
                    commentStrings = [...commentStrings, ...splitComments(comment.value)];
                }
                if (length(commentStrings) > 0) {
                    headerCommentStrings = commentStrings.slice(0, -1);
                    initialNodeCommentString = commentStrings[commentStrings.length - 1];
                    if (initialNodeCommentString.match(new RegExp('\\n\\n$'))) {
                        headerCommentStrings.push(initialNodeCommentString);
                        initialNodeCommentString = undefined;
                    }
                    if (length(headerCommentStrings) > 0) {
                        headerCommentStrings[headerCommentStrings.length - 1] = headerCommentStrings[headerCommentStrings.length - 1].replace(new RegExp('\\n*$'), '');
                    }
                }
            }
            if (length(headerCommentStrings) > 0) {
                const headerExp = [Symbol.for('begin')];
                const headerNode = (0, rose_1.makeRose)(headerExp);
                headerComments = headerCommentStrings.map(function (x) {
                    return new parser_1.LeadingCommentToken(x);
                });
                headerNode.setProperty('comments', headerComments);
                this.headerNodes.push(headerNode);
                this.headerExpressions.push(headerExp);
                if (initialNodeCommentString) {
                    initialNodeComments = [new parser_1.LeadingCommentToken(initialNodeCommentString)];
                }
                return initialNode.setProperty('comments', initialNodeComments);
            }
        }
    }
    findInlineLispSourcesComment(comments = []) {
        if (!this.getInlineLispSourcesFlag()) {
            const pattern = new RegExp('; inline-lisp-sources: t');
            for (let comment of comments) {
                const text = comment.value;
                if (text.match(pattern)) {
                    this.setInlineLispSourcesFlag(true);
                    break;
                }
            }
        }
    }
    initializeNodes(nodes = []) {
        let exp;
        let match;
        let node;
        this.makeHeaderNode(nodes);
        // Sort the expressions into `require` expressions, `provide`
        // expressions and main expressions.
        for (let node of nodes) {
            // Handle both S-expressions and rose tree values---for now.
            // In the future, we might want to simplify this to only
            // rose tree values.
            if (node instanceof rose_1.Rose) {
                exp = node.getValue();
                let comments = node.getProperty('comments');
                if (comments) {
                    // Look for `inline-lisp-sources: true` magic comment.
                    this.findInlineLispSourcesComment(comments);
                }
            }
            else {
                exp = node;
                node = (0, rose_1.makeRose)(exp);
            }
            if ((0, util_1.taggedListP)(exp, Symbol.for('require'))) {
                this.requireExpressions.push(exp);
                this.requireNodes.push(node);
            }
            else if ((0, util_1.taggedListP)(exp, Symbol.for('provide'))) {
                this.provideExpressions.push(exp);
                this.provideNodes.push(node);
            }
            else {
                this.mainExpressions.push(exp);
                this.mainNodes.push(node);
            }
        }
        // Iterate over `require-expressions`.
        for (let node of this.requireNodes) {
            exp = node.getValue();
            if ((0, util_1.taggedListP)(exp, Symbol.for('require')) && (exp.length > 1) && (0, util_1.taggedListP)((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1], Symbol.for('only-in'))) {
                let moduleName = (() => {
                    const lst = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                        const x = lastCdr(exp);
                        return Array.isArray(x) && (x.length === 0);
                    })()) ? (() => {
                        let i = 1;
                        let result = exp;
                        while (i > 0) {
                            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                result = exp[exp.length - 1];
                            }
                            else {
                                result = exp.slice(1);
                            }
                            i--;
                        }
                        if (Array.isArray(result)) {
                            result = result[0];
                        }
                        return result;
                    })() : exp[1];
                    if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && (() => {
                        const x = lastCdr(lst);
                        return Array.isArray(x) && (x.length === 0);
                    })()) {
                        let i = 1;
                        let result = lst;
                        while (i > 0) {
                            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                result = lst[lst.length - 1];
                            }
                            else {
                                result = lst.slice(1);
                            }
                            i--;
                        }
                        if (Array.isArray(result)) {
                            result = result[0];
                        }
                        return result;
                    }
                    else {
                        return lst[1];
                    }
                })();
                if (typeof moduleName === 'symbol') {
                    moduleName = moduleName.description;
                }
                if ((match = moduleName.match(new RegExp('^\\./(.*)$')))) {
                    moduleName = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && (() => {
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
                }
                if (!(!match || this.seenModules.includes(moduleName))) {
                    this.seenModules.push(moduleName);
                }
                // Add imported symbols to `.symbol-map`.
                for (let x of ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    let x1 = lastCdr(exp);
                    return Array.isArray(x1) && (x1.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1]).slice(1)) {
                    if (Array.isArray(x)) {
                        this.symbolMap.set((Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && (() => {
                            let x1 = lastCdr(x);
                            return Array.isArray(x1) && (x1.length === 0);
                        })()) ? (() => {
                            let i = 1;
                            let result = x;
                            while (i > 0) {
                                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                    result = x[x.length - 1];
                                }
                                else {
                                    result = x.slice(1);
                                }
                                i--;
                            }
                            if (Array.isArray(result)) {
                                result = result[0];
                            }
                            return result;
                        })() : x[1], true);
                    }
                    else {
                        this.symbolMap.set(x, true);
                    }
                }
            }
            else if ((0, util_1.taggedListP)(exp, Symbol.for('require')) && (exp.length > 1)) {
                let moduleNameSymbol = exp[exp.length - 1];
                let moduleName = moduleNameSymbol;
                if (typeof moduleNameSymbol === 'symbol') {
                    moduleName = moduleNameSymbol.description;
                }
                else {
                    moduleNameSymbol = Symbol.for(moduleName);
                }
                moduleName = getModuleName(moduleName);
                if (!this.seenModules.includes(moduleName)) {
                    this.seenModules.push(moduleName);
                }
                // Add module symbol to `symbol-map`.
                this.symbolMap.set(moduleNameSymbol, true);
            }
        }
        // Iterate over `main-expressions`.
        for (let node of this.mainNodes) {
            exp = node.getValue();
            if ((0, util_1.taggedListP)(exp, Symbol.for('define')) || (0, util_1.taggedListP)(exp, Symbol.for('define-class'))) {
                let name = Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(exp);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1]) ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(exp);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1])[0] : ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(exp);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1]);
                this.symbolMap.set(name, true);
            }
        }
        this.nodes = [...this.requireNodes, ...this.mainNodes, ...this.provideNodes];
        this.setExpressions([...this.requireExpressions, ...this.mainExpressions, ...this.provideExpressions]);
        return this;
    }
    makeEnvironment(parent = undefined) {
        const moduleEnv = new env_1.LispEnvironment([], parent);
        let imported;
        let local;
        let module;
        let env;
        let moduleName;
        this.parentEnvironment = parent;
        this.environment = moduleEnv;
        // Iterate over `require-nodes`, importing definitions
        // from other modules.
        for (let node of this.requireNodes) {
            let exp = node.getValue();
            if ((0, util_1.taggedListP)(exp, Symbol.for('require')) && (exp.length > 1) && (0, util_1.taggedListP)((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                const x = lastCdr(exp);
                return Array.isArray(x) && (x.length === 0);
            })()) ? (() => {
                let i = 1;
                let result = exp;
                while (i > 0) {
                    if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                        result = exp[exp.length - 1];
                    }
                    else {
                        result = exp.slice(1);
                    }
                    i--;
                }
                if (Array.isArray(result)) {
                    result = result[0];
                }
                return result;
            })() : exp[1], Symbol.for('only-in'))) {
                moduleName = (() => {
                    const lst = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                        const x = lastCdr(exp);
                        return Array.isArray(x) && (x.length === 0);
                    })()) ? (() => {
                        let i = 1;
                        let result = exp;
                        while (i > 0) {
                            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                result = exp[exp.length - 1];
                            }
                            else {
                                result = exp.slice(1);
                            }
                            i--;
                        }
                        if (Array.isArray(result)) {
                            result = result[0];
                        }
                        return result;
                    })() : exp[1];
                    if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && (() => {
                        const x = lastCdr(lst);
                        return Array.isArray(x) && (x.length === 0);
                    })()) {
                        let i = 1;
                        let result = lst;
                        while (i > 0) {
                            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                result = lst[lst.length - 1];
                            }
                            else {
                                result = lst.slice(1);
                            }
                            i--;
                        }
                        if (Array.isArray(result)) {
                            result = result[0];
                        }
                        return result;
                    }
                    else {
                        return lst[1];
                    }
                })();
                if (typeof moduleName === 'symbol') {
                    moduleName = moduleName.description;
                }
                moduleName = moduleName.replace(new RegExp('^\\./'), '');
                if (this.moduleMap && this.moduleMap.has(moduleName)) {
                    module = this.moduleMap.get(moduleName);
                    env = module.getEnvironment();
                }
                else {
                    env = undefined;
                }
                for (let exp1 of ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(exp);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1]).slice(2)) {
                    if (Array.isArray(exp1)) {
                        local = exp1[0];
                        imported = (Array.isArray(exp1) && (exp1.length >= 3) && (exp1[exp1.length - 2] === Symbol.for('.')) && (() => {
                            const x = lastCdr(exp1);
                            return Array.isArray(x) && (x.length === 0);
                        })()) ? (() => {
                            let i = 1;
                            let result = exp1;
                            while (i > 0) {
                                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                                    result = exp1[exp1.length - 1];
                                }
                                else {
                                    result = exp1.slice(1);
                                }
                                i--;
                            }
                            if (Array.isArray(result)) {
                                result = result[0];
                            }
                            return result;
                        })() : exp1[1];
                    }
                    else {
                        local = exp1;
                        imported = exp1;
                    }
                    moduleEnv.setLocal(imported, true, 'variable');
                    if (env) {
                        const [f, fType] = env.getTypedValue(local);
                        if (fType !== 'undefined') {
                            moduleEnv.setLocal(imported, f, fType);
                        }
                    }
                }
            }
        }
        // Iterate over `main-nodes`, evaluating definition forms
        // in the module environment.
        for (let node of this.mainNodes) {
            let exp = node.getValue();
            if (definitionp(exp) || macroDefinitionP(exp)) {
                // Evaluate `define` and `defmacro` forms in the module
                // environment. Be error-tolerant since the module
                // environment is not needed in many cases.
                let name = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
                    const x = lastCdr(exp);
                    return Array.isArray(x) && (x.length === 0);
                })()) ? (() => {
                    let i = 1;
                    let result = exp;
                    while (i > 0) {
                        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                            result = exp[exp.length - 1];
                        }
                        else {
                            result = exp.slice(1);
                        }
                        i--;
                    }
                    if (Array.isArray(result)) {
                        result = result[0];
                    }
                    return result;
                })() : exp[1];
                if (Array.isArray(name)) {
                    name = name[0];
                }
                const typ = macroDefinitionP(exp) ? 'macro' : 'procedure';
                moduleEnv.setLocal(name, (0, thunk_1.thunk)(function () {
                    let result = undefined;
                    try {
                        result = (0, eval_1.eval_)(exp, moduleEnv);
                    }
                    catch (e) {
                        if (e instanceof Error) {
                        }
                        else {
                            throw e;
                        }
                    }
                    // Do nothing
                    return result;
                }), typ);
            }
        }
        return moduleEnv;
    }
    setModuleMap(moduleMap) {
        this.moduleMap = moduleMap;
        return this;
    }
    setNodes(nodes) {
        this.mainNodes = nodes;
        this.mainExpressions = nodes.map(function (x) {
            return x.getValue();
        });
        return this;
    }
    setExpressions(expressions = []) {
        return this.expressions = expressions;
    }
    setInlineLispSourcesFlag(val) {
        return this.inlineLispSourcesFlag = val;
    }
    getInlineLispSourcesFlag() {
        return this.inlineLispSourcesFlag;
    }
}
exports.Module = Module;
/**
 * Convert a map of `module` forms to a map of `Module` objects,
 * interlinking them in the process.
 */
function makeModuleMap(moduleExpressionMap, env) {
    let moduleMap = new thunk_1.ThunkedMap();
    for (let key of moduleExpressionMap.keys()) {
        moduleMap.set(key, (0, thunk_1.thunk)(function () {
            let val = moduleExpressionMap.get(key);
            const m = (val instanceof Module) ? val : moduleExpressionToModuleObject(val, env);
            m.setModuleMap(moduleMap);
            return m;
        }));
    }
    return moduleMap;
}
exports.makeModuleMap = makeModuleMap;
makeModuleMap.lispSource = [Symbol.for('define'), [Symbol.for('make-module-map'), Symbol.for('module-expression-map'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('module-map'), [Symbol.for('new'), Symbol.for('ThunkedMap')]], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('send'), Symbol.for('module-expression-map'), Symbol.for('keys')]]], [Symbol.for('send'), Symbol.for('module-map'), Symbol.for('set'), Symbol.for('key'), [Symbol.for('thunk'), [Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('module-expression-map'), Symbol.for('get'), Symbol.for('key')]], [Symbol.for('define'), Symbol.for('m'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('val'), Symbol.for('Module')], Symbol.for('val'), [Symbol.for('module-expression-to-module-object'), Symbol.for('val'), Symbol.for('env')]]], [Symbol.for('send'), Symbol.for('m'), Symbol.for('set-module-map'), Symbol.for('module-map')], Symbol.for('m')]]]], Symbol.for('module-map')];
/**
 * Convert a `(module ...)` expression to a
 * `Module` object.
 */
function moduleExpressionToModuleObject(node, env) {
    let name = node.get(1).getValue();
    if (typeof name === 'symbol') {
        name = name.description;
    }
    return new Module(node.drop(3), env, name);
}
exports.moduleExpressionToModuleObject = moduleExpressionToModuleObject;
moduleExpressionToModuleObject.lispSource = [Symbol.for('define'), [Symbol.for('module-expression-to-module-object'), Symbol.for('node'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('name')], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('symbol->string'), Symbol.for('name')]]], [Symbol.for('new'), Symbol.for('Module'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3], Symbol.for('env'), Symbol.for('name')]];
/**
 * Whether `env` extends the Lisp environment.
 */
function extendsLispEnvironmentP(env) {
    // TODO: Check `parent`.
    return (env === lispEnvironment) || ((env instanceof env_1.EnvironmentStack) && env.hasEnvironment(lispEnvironment));
}
extendsLispEnvironmentP.lispSource = [Symbol.for('define'), [Symbol.for('extends-lisp-environment?'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('env'), Symbol.for('lisp-environment')], [Symbol.for('and'), [Symbol.for('is-a?'), Symbol.for('env'), Symbol.for('EnvironmentStack')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('has-environment'), Symbol.for('lisp-environment')]]]];
/**
 * Extract the module name from a `(require ...)` expression.
 */
function getModuleName(nameObj) {
    let name = nameObj;
    if (typeof name === 'symbol') {
        name = name.description;
    }
    name = name.replace(new RegExp('^\\./'), '');
    return name;
}
getModuleName.lispSource = [Symbol.for('define'), [Symbol.for('get-module-name'), Symbol.for('name-obj')], [Symbol.for('define'), Symbol.for('name'), Symbol.for('name-obj')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('name')], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('symbol->string'), Symbol.for('name')]]], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\./'], Symbol.for('name'), '']], Symbol.for('name')];
/**
 * Return the current environment.
 */
function currentCompilationOptions() {
    return currentCompilationOptionsPointer;
}
currentCompilationOptions.lispSource = [Symbol.for('define'), [Symbol.for('current-compilation-options')], Symbol.for('current-compilation-options-pointer')];
/**
 * Run `f` with `current-compilation-options-pointer` bound to `options`.
 * The return value is the result of invoking `f`.
 */
function withCompilationOptions(options, f) {
    let result = undefined;
    const tmp = currentCompilationOptionsPointer;
    try {
        currentCompilationOptionsPointer = options;
        result = f();
    }
    finally {
        currentCompilationOptionsPointer = tmp;
    }
    return result;
}
withCompilationOptions.lispSource = [Symbol.for('define'), [Symbol.for('with-compilation-options'), Symbol.for('options'), Symbol.for('f')], [Symbol.for('let'), [[Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('tmp'), Symbol.for('current-compilation-options-pointer')]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('current-compilation-options-pointer'), Symbol.for('options')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f')]], [Symbol.for('finally'), [Symbol.for('set!'), Symbol.for('current-compilation-options-pointer'), Symbol.for('tmp')]]], Symbol.for('result')]];
/**
 * Whether an expression is a definition.
 */
function definitionp(exp) {
    return (0, util_1.taggedListP)(exp, Symbol.for('define'));
}
definitionp.lispSource = [Symbol.for('define'), [Symbol.for('definition?'), Symbol.for('exp')], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define')]]];
/**
 * Whether an expression is a function definition.
 */
function functionDefinitionP(exp) {
    return definitionp(exp) && ((() => {
        const obj = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = exp;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = exp[exp.length - 1];
                }
                else {
                    result = exp.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : exp[1];
        return Array.isArray(obj) && !(Array.isArray(obj) && (obj.length === 0));
    })() || functionExpressionP((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && (() => {
        const x = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
    })()) ? (() => {
        let i = 2;
        let result = exp;
        while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
            }
            else {
                result = exp.slice(1);
            }
            i--;
        }
        if (Array.isArray(result)) {
            result = result[0];
        }
        return result;
    })() : exp[2]));
}
functionDefinitionP.lispSource = [Symbol.for('define'), [Symbol.for('function-definition?'), Symbol.for('exp')], [Symbol.for('and'), [Symbol.for('definition?'), Symbol.for('exp')], [Symbol.for('or'), [Symbol.for('cons?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('function-expression?'), [Symbol.for('third'), Symbol.for('exp')]]]]];
/**
 * Whether an expression is a function expression.
 */
function functionExpressionP(exp) {
    return (0, util_1.taggedListP)(exp, Symbol.for('lambda')) || (0, util_1.taggedListP)(exp, Symbol.for('js/function')) || (0, util_1.taggedListP)(exp, Symbol.for('js/arrow'));
}
functionExpressionP.lispSource = [Symbol.for('define'), [Symbol.for('function-expression?'), Symbol.for('exp')], [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('lambda')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('js/function')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('js/arrow')]]]];
/**
 * Whether an expression is a macro definition.
 */
function macroDefinitionP(exp) {
    return (0, util_1.taggedListP)(exp, Symbol.for('define-macro')) || (0, util_1.taggedListP)(exp, Symbol.for('defmacro'));
}
macroDefinitionP.lispSource = [Symbol.for('define'), [Symbol.for('macro-definition?'), Symbol.for('exp')], [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define-macro')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('defmacro')]]]];
/**
 * Lisp environment.
 */
const lispEnvironment = new env_1.LispEnvironment([[Symbol.for('_'), curry_1.__, 'variable'], [Symbol.for('__'), curry_1.__, 'variable'], [Symbol.for('#f'), constants_1.false_, 'variable'], [Symbol.for('#t'), constants_1.true_, 'variable'], [Symbol.for('#u'), constants_1.undefined_, 'variable'], [Symbol.for('false'), constants_1.false_, 'variable'], [Symbol.for('nil'), constants_1.null_, 'variable'], [Symbol.for('null'), constants_1.null_, 'variable'], [Symbol.for('js/null'), constants_1.jsNull_, 'variable'], [Symbol.for('js-null'), constants_1.jsNull_, 'variable'], [Symbol.for('t'), constants_1.true_, 'variable'], [Symbol.for('true'), constants_1.true_, 'variable'], [Symbol.for('js-undefined'), constants_1.undefined_, 'variable'], [Symbol.for('js/undefined'), constants_1.undefined_, 'variable'], [Symbol.for('undefined'), constants_1.undefined_, 'variable'], [Symbol.for('*cons-dot*'), list_1.consDot_, 'variable'], [Symbol.for('$'), procedures_1.funcall_, 'procedure'], [Symbol.for('%'), procedures_1.modulo_, 'procedure'], [Symbol.for('*'), procedures_1.mul_, 'procedure'], [Symbol.for('+'), procedures_1.add_, 'procedure'], [Symbol.for('-'), procedures_1.sub_, 'procedure'], [Symbol.for('/'), procedures_1.div_, 'procedure'], [Symbol.for('<'), procedures_1.lt_, 'procedure'], [Symbol.for('<='), procedures_1.lte_, 'procedure'], [Symbol.for('='), equal_1.eqp_, 'procedure'], [Symbol.for('=?'), equal_1.eqp_, 'procedure'], [Symbol.for('>'), procedures_1.gt_, 'procedure'], [Symbol.for('>='), procedures_1.gte_, 'procedure'], [Symbol.for('add'), procedures_1.add_, 'procedure'], [Symbol.for('add1'), procedures_1.add1_, 'procedure'], [Symbol.for('aget'), array_1.arrayRef_, 'procedure'], [Symbol.for('append'), list_1.append_, 'procedure'], [Symbol.for('apply'), procedures_1.apply_, 'procedure'], [Symbol.for('aref'), array_1.arrayRef_, 'procedure'], [Symbol.for('array-drop'), array_1.arrayDrop_, 'procedure'], [Symbol.for('array-drop-right'), array_1.arrayDropRight_, 'procedure'], [Symbol.for('array-eighth'), array_1.arrayEighth_, 'procedure'], [Symbol.for('array-fifth'), array_1.arrayFifth_, 'procedure'], [Symbol.for('array-first'), array_1.arrayFirst_, 'procedure'], [Symbol.for('array-fourth'), array_1.arrayFourth_, 'procedure'], [Symbol.for('array-get'), array_1.arrayRef_, 'procedure'], [Symbol.for('array-last'), array_1.arrayLast_, 'procedure'], [Symbol.for('array-length'), array_1.arrayLength_, 'procedure'], [Symbol.for('array-list->linked-list'), list_1.arrayListToLinkedList_, 'procedure'], [Symbol.for('array-list-car'), list_1.car_, 'procedure'], [Symbol.for('array-list-cdr'), list_1.arrayListCdr_, 'procedure'], [Symbol.for('array-list-drop'), list_1.arrayListDrop_, 'procedure'], [Symbol.for('array-list-drop-right'), list_1.arrayListDropRight_, 'procedure'], [Symbol.for('array-list-eighth'), list_1.arrayListEighth_, 'procedure'], [Symbol.for('array-list-fifth'), list_1.arrayListFifth_, 'procedure'], [Symbol.for('array-list-first'), list_1.arrayListFirst_, 'procedure'], [Symbol.for('array-list-fourth'), list_1.arrayListFourth_, 'procedure'], [Symbol.for('array-list-last'), list_1.arrayListLast_, 'procedure'], [Symbol.for('array-list-length'), list_1.arrayListLength_, 'procedure'], [Symbol.for('array-list-ninth'), list_1.arrayListNinth_, 'procedure'], [Symbol.for('array-list-nth'), list_1.arrayListNth_, 'procedure'], [Symbol.for('array-list-nthcdr'), list_1.arrayListNthcdr_, 'procedure'], [Symbol.for('array-list-rest'), list_1.arrayListRest_, 'procedure'], [Symbol.for('array-list-reverse'), list_1.arrayListReverse_, 'procedure'], [Symbol.for('array-list-second'), list_1.arrayListSecond_, 'procedure'], [Symbol.for('array-list-seventh'), list_1.arrayListSeventh_, 'procedure'], [Symbol.for('array-list-sixth'), list_1.arrayListSixth_, 'procedure'], [Symbol.for('array-list-take'), list_1.arrayListTake_, 'procedure'], [Symbol.for('array-list-tenth'), list_1.arrayListTenth_, 'procedure'], [Symbol.for('array-list-third'), list_1.arrayListThird_, 'procedure'], [Symbol.for('array-list?'), list_1.arrayListP_, 'procedure'], [Symbol.for('array-ninth'), array_1.arrayNinth_, 'procedure'], [Symbol.for('array-ref'), array_1.arrayRef_, 'procedure'], [Symbol.for('array-rest'), array_1.arrayRest_, 'procedure'], [Symbol.for('array-reverse'), array_1.arrayReverse_, 'procedure'], [Symbol.for('array-second'), array_1.arraySecond_, 'procedure'], [Symbol.for('array-set'), array_1.arraySet_, 'procedure'], [Symbol.for('array-set!'), array_1.arraySet_, 'procedure'], [Symbol.for('array-seventh'), array_1.arraySeventh_, 'procedure'], [Symbol.for('array-sixth'), array_1.arraySixth_, 'procedure'], [Symbol.for('array-take'), array_1.arrayTake_, 'procedure'], [Symbol.for('array-tenth'), array_1.arrayTenth_, 'procedure'], [Symbol.for('array-third'), array_1.arrayThird_, 'procedure'], [Symbol.for('array?'), array_1.arrayp_, 'procedure'], [Symbol.for('aset'), array_1.arraySet_, 'procedure'], [Symbol.for('aset!'), array_1.arraySet_, 'procedure'], [Symbol.for('assert'), procedures_1.assert_, 'procedure'], [Symbol.for('boolean?'), procedures_1.booleanp_, 'procedure'], [Symbol.for('booleanp'), procedures_1.booleanp_, 'procedure'], [Symbol.for('build-list'), list_1.buildList_, 'procedure'], [Symbol.for('cadr'), list_1.cadr_, 'procedure'], [Symbol.for('call-cc'), callWithCurrentContinuation_, 'procedure'], [Symbol.for('call-with-current-continuation'), callWithCurrentContinuation_, 'procedure'], [Symbol.for('call/cc'), callWithCurrentContinuation_, 'procedure'], [Symbol.for('car'), list_1.car_, 'procedure'], [Symbol.for('cdr'), list_1.cdr_, 'procedure'], [Symbol.for('circular-list-p'), list_1.circularListP_, 'procedure'], [Symbol.for('circular-list?'), list_1.circularListP_, 'procedure'], [Symbol.for('cons'), list_1.cons_, 'procedure'], [Symbol.for('cons*'), list_1.listStar_, 'procedure'], [Symbol.for('cons-dot'), list_1.consDotF_, 'procedure'], [Symbol.for('cons-dot?'), list_1.consDotP_, 'procedure'], [Symbol.for('cons?'), list_1.consp_, 'procedure'], [Symbol.for('console.log'), console.log, 'procedure'], [Symbol.for('consp'), list_1.consp_, 'procedure'], [Symbol.for('const'), procedures_1.const_, 'procedure'], [Symbol.for('constantly'), procedures_1.const_, 'procedure'], [Symbol.for('current-environment'), env_1.currentEnvironment_, 'procedure'], [Symbol.for('curry'), curry_1.curry, 'procedure'], [Symbol.for('curry-n'), curry_1.curryN, 'procedure'], [Symbol.for('delete'), javascript_1.jsDelete_, 'procedure'], [Symbol.for('display'), procedures_1.display_, 'procedure'], [Symbol.for('div'), procedures_1.div_, 'procedure'], [Symbol.for('dotted-list->proper-list'), list_1.linkedListToArrayList_, 'procedure'], [Symbol.for('dotted-list-car'), list_1.linkedListCar_, 'procedure'], [Symbol.for('dotted-list-cdr'), list_1.linkedListCdr_, 'procedure'], [Symbol.for('dotted-list-head'), list_1.linkedListHead_, 'procedure'], [Symbol.for('dotted-list-last'), list_1.linkedListLast_, 'procedure'], [Symbol.for('dotted-list-last-cdr'), list_1.linkedListLastCdr_, 'procedure'], [Symbol.for('dotted-list-length'), list_1.linkedListLength_, 'procedure'], [Symbol.for('dotted-list-nth'), list_1.linkedListNth_, 'procedure'], [Symbol.for('dotted-list-nthcdr'), list_1.linkedListNthcdr_, 'procedure'], [Symbol.for('dotted-list-p'), list_1.dottedListP_, 'procedure'], [Symbol.for('dotted-list-tail'), list_1.linkedListTail_, 'procedure'], [Symbol.for('dotted-list?'), list_1.dottedListP_, 'procedure'], [Symbol.for('dotted-pair-cdr'), list_1.linkedPairCdr_, 'procedure'], [Symbol.for('dotted-pair-p'), list_1.dottedPairP_, 'procedure'], [Symbol.for('dotted-pair?'), list_1.dottedPairP_, 'procedure'], [Symbol.for('drop'), list_1.listTail_, 'procedure'], [Symbol.for('drop-right'), list_1.dropRight_, 'procedure'], [Symbol.for('eighth'), list_1.eighth_, 'procedure'], [Symbol.for('eq'), equal_1.eqp_, 'procedure'], [Symbol.for('eq?'), equal_1.eqp_, 'procedure'], [Symbol.for('eql'), equal_1.eqvp_, 'procedure'], [Symbol.for('eql?'), equal_1.eqvp_, 'procedure'], [Symbol.for('equal'), equal_1.equalp_, 'procedure'], [Symbol.for('equal?'), equal_1.equalp_, 'procedure'], [Symbol.for('eqv'), equal_1.eqvp_, 'procedure'], [Symbol.for('eqv?'), equal_1.eqvp_, 'procedure'], [Symbol.for('error'), procedures_1.error_, 'procedure'], [Symbol.for('even?'), procedures_1.evenp_, 'procedure'], [Symbol.for('extend-environment'), env_1.extendEnvironment, 'procedure'], [Symbol.for('false?'), procedures_1.falsep_, 'procedure'], [Symbol.for('falsep'), procedures_1.falsep_, 'procedure'], [Symbol.for('fexpr?'), procedures_1.fexprp_, 'procedure'], [Symbol.for('fexprp'), procedures_1.fexprp_, 'procedure'], [Symbol.for('field-names'), object_1.fieldNames_, 'procedure'], [Symbol.for('fifth'), list_1.fifth_, 'procedure'], [Symbol.for('filter'), procedures_1.filter_, 'procedure'], [Symbol.for('findf'), procedures_1.findf_, 'procedure'], [Symbol.for('findf-index'), procedures_1.findfIndex_, 'procedure'], [Symbol.for('first'), list_1.first_, 'procedure'], [Symbol.for('flatten'), list_1.flatten_, 'procedure'], [Symbol.for('foldl'), procedures_1.foldl_, 'procedure'], [Symbol.for('foldr'), procedures_1.foldr_, 'procedure'], [Symbol.for('fourth'), list_1.fourth_, 'procedure'], [Symbol.for('funcall'), procedures_1.funcall_, 'procedure'], [Symbol.for('function-object?'), javascript_1.jsFunctionObjectP_, 'procedure'], [Symbol.for('function-type?'), javascript_1.jsFunctionTypeP_, 'procedure'], [Symbol.for('function?'), procedures_1.procedurep_, 'procedure'], [Symbol.for('functionp'), procedures_1.procedurep_, 'procedure'], [Symbol.for('gensym'), symbol_1.gensym_, 'procedure'], [Symbol.for('gensym?'), symbol_1.gensymp_, 'procedure'], [Symbol.for('get'), array_1.arrayRef_, 'procedure'], [Symbol.for('hash'), hash_1.makeHash_, 'procedure'], [Symbol.for('hash->list'), hash_1.hashToList_, 'procedure'], [Symbol.for('hash-clear'), hash_1.hashClear_, 'procedure'], [Symbol.for('hash-clear!'), hash_1.hashClearX_, 'procedure'], [Symbol.for('hash-copy'), hash_1.hashCopy_, 'procedure'], [Symbol.for('hash-entries'), hash_1.hashEntries_, 'procedure'], [Symbol.for('hash-has-key?'), hash_1.hashHasKeyP_, 'procedure'], [Symbol.for('hash-keys'), hash_1.hashKeys_, 'procedure'], [Symbol.for('hash-ref'), hash_1.hashRef_, 'procedure'], [Symbol.for('hash-remove'), hash_1.hashRemove_, 'procedure'], [Symbol.for('hash-remove!'), hash_1.hashRemoveX_, 'procedure'], [Symbol.for('hash-set'), hash_1.hashSetX_, 'procedure'], [Symbol.for('hash-set!'), hash_1.hashSetX_, 'procedure'], [Symbol.for('hash-size'), hash_1.hashSize_, 'procedure'], [Symbol.for('hash-values'), hash_1.hashValues_, 'procedure'], [Symbol.for('hash?'), hash_1.hashp_, 'procedure'], [Symbol.for('head'), list_1.car_, 'procedure'], [Symbol.for('id'), procedures_1.identity_, 'procedure'], [Symbol.for('identity'), procedures_1.identity_, 'procedure'], [Symbol.for('improper-list-p'), list_1.improperListP_, 'procedure'], [Symbol.for('improper-list?'), list_1.improperListP_, 'procedure'], [Symbol.for('in-range'), procedures_1.range_, 'procedure'], [Symbol.for('index-of'), procedures_1.indexOf_, 'procedure'], [Symbol.for('index-where'), procedures_1.indexWhere_, 'procedure'], [Symbol.for('instance-of'), procedures_1.isAP_, 'procedure'], [Symbol.for('instance-of?'), procedures_1.isAP_, 'procedure'], [Symbol.for('instanceof'), procedures_1.isAP_, 'procedure'], [Symbol.for('instanceof?'), procedures_1.isAP_, 'procedure'], [Symbol.for('intern'), symbol_1.stringToSymbol_, 'procedure'], [Symbol.for('intersection'), procedures_1.intersection_, 'procedure'], [Symbol.for('is-a?'), procedures_1.isAP_, 'procedure'], [Symbol.for('js'), js_, 'procedure'], [Symbol.for('js-field'), array_1.arrayRef_, 'procedure'], [Symbol.for('js-keys'), object_1.jsKeys_, 'procedure'], [Symbol.for('js-obj'), object_1.jsObj_, 'procedure'], [Symbol.for('js-obj-append'), object_1.jsObjAppend_, 'procedure'], [Symbol.for('js-obj-keys'), object_1.jsKeys_, 'procedure'], [Symbol.for('js-obj?'), object_1.jsObjP_, 'procedure'], [Symbol.for('js/+'), javascript_1.jsPlus_, 'procedure'], [Symbol.for('js/=='), javascript_1.jsIsLooselyEqualP_, 'procedure'], [Symbol.for('js/==='), javascript_1.jsIsStrictlyEqualP_, 'procedure'], [Symbol.for('js/===?'), javascript_1.jsIsStrictlyEqualP_, 'procedure'], [Symbol.for('js/==?'), javascript_1.jsIsLooselyEqualP_, 'procedure'], [Symbol.for('js/console.log'), console.log, 'procedure'], [Symbol.for('js/delete'), javascript_1.jsDelete_, 'procedure'], [Symbol.for('js/find-index'), javascript_1.jsFindIndex_, 'procedure'], [Symbol.for('js/findf-index'), javascript_1.jsFindIndex_, 'procedure'], [Symbol.for('js/function-object?'), javascript_1.jsFunctionObjectP_, 'procedure'], [Symbol.for('js/function-type?'), javascript_1.jsFunctionTypeP_, 'procedure'], [Symbol.for('js/function?'), javascript_1.jsFunctionP_, 'procedure'], [Symbol.for('js/in'), javascript_1.jsIn_, 'procedure'], [Symbol.for('js/instanceof'), javascript_1.jsInstanceof_, 'procedure'], [Symbol.for('js/instanceof?'), javascript_1.jsInstanceof_, 'procedure'], [Symbol.for('js/is-loosely-equal?'), javascript_1.jsIsLooselyEqualP_, 'procedure'], [Symbol.for('js/is-strictly-equal?'), javascript_1.jsIsStrictlyEqualP_, 'procedure'], [Symbol.for('js/js-obj'), object_1.jsObj_, 'procedure'], [Symbol.for('js/js-obj-append'), object_1.jsObjAppend_, 'procedure'], [Symbol.for('js/js-obj?'), object_1.jsObjP_, 'procedure'], [Symbol.for('js/new'), new_, 'procedure'], [Symbol.for('js/null?'), javascript_1.jsNullP_, 'procedure'], [Symbol.for('js/obj'), object_1.jsObj_, 'procedure'], [Symbol.for('js/obj-append'), object_1.jsObjAppend_, 'procedure'], [Symbol.for('js/obj?'), object_1.jsObjP_, 'procedure'], [Symbol.for('js/object-type?'), object_1.jsObjectTypeP_, 'procedure'], [Symbol.for('js/object?'), object_1.jsObjectTypeP_, 'procedure'], [Symbol.for('js/regexp'), regexp_1.regexp_, 'procedure'], [Symbol.for('js/regexp-quote'), regexp_1.regexpQuote_, 'procedure'], [Symbol.for('js/regexp?'), regexp_1.regexpp_, 'procedure'], [Symbol.for('js/same-value-zero?'), javascript_1.jsSameValueZeroP_, 'procedure'], [Symbol.for('js/same-value?'), javascript_1.jsSameValueP_, 'procedure'], [Symbol.for('js/tag'), javascript_1.jsTaggedTemplate_, 'procedure'], [Symbol.for('js/tagged-template'), javascript_1.jsTaggedTemplate_, 'procedure'], [Symbol.for('js/typeof'), javascript_1.jsTypeof_, 'procedure'], [Symbol.for('keyword?'), procedures_1.keywordp_, 'procedure'], [Symbol.for('keywordp'), procedures_1.keywordp_, 'procedure'], [Symbol.for('last'), list_1.last_, 'procedure'], [Symbol.for('last-cdr'), list_1.lastCdr_, 'procedure'], [Symbol.for('last-cons'), list_1.lastPair_, 'procedure'], [Symbol.for('last-pair'), list_1.lastPair_, 'procedure'], [Symbol.for('length'), list_1.length_, 'procedure'], [Symbol.for('length*'), list_1.length_, 'procedure'], [Symbol.for('linked-list-car'), list_1.linkedListCar_, 'procedure'], [Symbol.for('linked-list-cdr'), list_1.linkedListCdr_, 'procedure'], [Symbol.for('linked-list-eighth'), list_1.linkedListEighth_, 'procedure'], [Symbol.for('linked-list-fifth'), list_1.linkedListFifth_, 'procedure'], [Symbol.for('linked-list-first'), list_1.linkedListFirst_, 'procedure'], [Symbol.for('linked-list-fourth'), list_1.linkedListFourth_, 'procedure'], [Symbol.for('linked-list-head'), list_1.linkedListHead_, 'procedure'], [Symbol.for('linked-list-last'), list_1.linkedListLast_, 'procedure'], [Symbol.for('linked-list-last-cdr'), list_1.linkedListLastCdr_, 'procedure'], [Symbol.for('linked-list-length'), list_1.linkedListLength_, 'procedure'], [Symbol.for('linked-list-link-car'), list_1.linkedListLinkCar_, 'procedure'], [Symbol.for('linked-list-link-cdr'), list_1.linkedListLinkCdr_, 'procedure'], [Symbol.for('linked-list-link-p'), list_1.linkedListLinkP_, 'procedure'], [Symbol.for('linked-list-link?'), list_1.linkedListLinkP_, 'procedure'], [Symbol.for('linked-list-ninth'), list_1.linkedListNinth_, 'procedure'], [Symbol.for('linked-list-nth'), list_1.linkedListNth_, 'procedure'], [Symbol.for('linked-list-nthcdr'), list_1.linkedListNthcdr_, 'procedure'], [Symbol.for('linked-list-p'), list_1.linkedListP_, 'procedure'], [Symbol.for('linked-list-second'), list_1.linkedListSecond_, 'procedure'], [Symbol.for('linked-list-seventh'), list_1.linkedListSeventh_, 'procedure'], [Symbol.for('linked-list-sixth'), list_1.linkedListSixth_, 'procedure'], [Symbol.for('linked-list-tail'), list_1.linkedListTail_, 'procedure'], [Symbol.for('linked-list-tenth'), list_1.linkedListTenth_, 'procedure'], [Symbol.for('linked-list-third'), list_1.linkedListThird_, 'procedure'], [Symbol.for('linked-list?'), list_1.linkedListP_, 'procedure'], [Symbol.for('linked-pair-car'), list_1.linkedPairCar_, 'procedure'], [Symbol.for('linked-pair-cdr'), list_1.linkedPairCdr_, 'procedure'], [Symbol.for('linked-pair?'), list_1.linkedPairP_, 'procedure'], [Symbol.for('list'), list_1.list_, 'procedure'], [Symbol.for('list*'), list_1.listStar_, 'procedure'], [Symbol.for('list-ref'), list_1.nth_, 'procedure'], [Symbol.for('list-set'), array_1.arraySet_, 'procedure'], [Symbol.for('list-set!'), array_1.arraySet_, 'procedure'], [Symbol.for('list-star'), list_1.listStar_, 'procedure'], [Symbol.for('list-tail'), list_1.listTail_, 'procedure'], [Symbol.for('list?'), list_1.listp_, 'procedure'], [Symbol.for('listp'), list_1.listp_, 'procedure'], [Symbol.for('log'), console.log, 'procedure'], [Symbol.for('make'), new_, 'procedure'], [Symbol.for('make-hash'), hash_1.makeHash_, 'procedure'], [Symbol.for('make-list'), list_1.makeList_, 'procedure'], [Symbol.for('make-object'), new_, 'procedure'], [Symbol.for('map'), procedures_1.map_, 'procedure'], [Symbol.for('mapcar'), procedures_1.map_, 'procedure'], [Symbol.for('member'), procedures_1.member_, 'procedure'], [Symbol.for('member-p'), procedures_1.memberp_, 'procedure'], [Symbol.for('member?'), procedures_1.memberp_, 'procedure'], [Symbol.for('memberp'), procedures_1.memberp_, 'procedure'], [Symbol.for('memf'), procedures_1.memf_, 'procedure'], [Symbol.for('memf?'), procedures_1.memfp_, 'procedure'], [Symbol.for('memq'), procedures_1.memq_, 'procedure'], [Symbol.for('memq?'), procedures_1.memqp_, 'procedure'], [Symbol.for('mod'), procedures_1.modulo_, 'procedure'], [Symbol.for('modulo'), procedures_1.modulo_, 'procedure'], [Symbol.for('mul'), procedures_1.mul_, 'procedure'], [Symbol.for('new'), new_, 'procedure'], [Symbol.for('new*'), new_, 'procedure'], [Symbol.for('ninth'), list_1.ninth_, 'procedure'], [Symbol.for('not'), procedures_1.not_, 'procedure'], [Symbol.for('nth'), list_1.nth_, 'procedure'], [Symbol.for('nthcdr'), list_1.nthcdr_, 'procedure'], [Symbol.for('null?'), list_1.nullp_, 'procedure'], [Symbol.for('nullp'), list_1.nullp_, 'procedure'], [Symbol.for('number->string'), string_1.numberToString_, 'procedure'], [Symbol.for('number?'), procedures_1.numberp_, 'procedure'], [Symbol.for('numberp'), procedures_1.numberp_, 'procedure'], [Symbol.for('object?'), object_1.jsObjP_, 'procedure'], [Symbol.for('objectp'), object_1.jsObjP_, 'procedure'], [Symbol.for('odd?'), procedures_1.oddp_, 'procedure'], [Symbol.for('oget'), object_1.objectRef_, 'procedure'], [Symbol.for('one?'), procedures_1.onep_, 'procedure'], [Symbol.for('onep'), procedures_1.onep_, 'procedure'], [Symbol.for('oref'), array_1.arrayRef_, 'procedure'], [Symbol.for('oset'), object_1.objectSetX_, 'procedure'], [Symbol.for('oset!'), object_1.objectSetX_, 'procedure'], [Symbol.for('plist->alist'), plist_1.plistToAlist_, 'procedure'], [Symbol.for('plist-copy'), plist_1.plistCopy_, 'procedure'], [Symbol.for('plist-get'), plist_1.plistGet_, 'procedure'], [Symbol.for('plist-has'), plist_1.plistHasP_, 'procedure'], [Symbol.for('plist-has?'), plist_1.plistHasP_, 'procedure'], [Symbol.for('plist-ref'), plist_1.plistGet_, 'procedure'], [Symbol.for('plist-set'), plist_1.plistSetX_, 'procedure'], [Symbol.for('plist-set!'), plist_1.plistSetX_, 'procedure'], [Symbol.for('plist?'), plist_1.plistp_, 'procedure'], [Symbol.for('pop'), list_1.popLeftX_, 'procedure'], [Symbol.for('pop!'), list_1.popLeftX_, 'procedure'], [Symbol.for('pop-left'), list_1.popLeftX_, 'procedure'], [Symbol.for('pop-left!'), list_1.popLeftX_, 'procedure'], [Symbol.for('pop-right'), list_1.popRightX_, 'procedure'], [Symbol.for('pop-right!'), list_1.popRightX_, 'procedure'], [Symbol.for('print'), printer_1.print, 'procedure'], [Symbol.for('print-estree'), printer_1.printEstree, 'procedure'], [Symbol.for('procedure?'), procedures_1.procedurep_, 'procedure'], [Symbol.for('proper-list->dotted-list'), list_1.arrayListToLinkedList_, 'procedure'], [Symbol.for('proper-list-p'), list_1.properListP_, 'procedure'], [Symbol.for('proper-list?'), list_1.properListP_, 'procedure'], [Symbol.for('push'), list_1.pushLeftX_, 'procedure'], [Symbol.for('push!'), list_1.pushLeftX_, 'procedure'], [Symbol.for('push-left'), list_1.pushLeftX_, 'procedure'], [Symbol.for('push-left!'), list_1.pushLeftX_, 'procedure'], [Symbol.for('push-right'), list_1.pushRightX_, 'procedure'], [Symbol.for('push-right!'), list_1.pushRightX_, 'procedure'], [Symbol.for('range'), procedures_1.range_, 'procedure'], [Symbol.for('re'), regexp_1.regexp_, 'procedure'], [Symbol.for('re-pattern'), regexp_1.regexp_, 'procedure'], [Symbol.for('regexp'), regexp_1.regexp_, 'procedure'], [Symbol.for('regexp-match'), regexp_1.regexpMatch_, 'procedure'], [Symbol.for('regexp-match?'), regexp_1.regexpMatchP_, 'procedure'], [Symbol.for('regexp-quote'), regexp_1.regexpQuote_, 'procedure'], [Symbol.for('regexp-replace'), regexp_1.regexpReplace_, 'procedure'], [Symbol.for('regexp?'), regexp_1.regexpp_, 'procedure'], [Symbol.for('rest'), list_1.rest_, 'procedure'], [Symbol.for('reverse'), list_1.reverse_, 'procedure'], [Symbol.for('rx'), regexp_1.regexp_, 'procedure'], [Symbol.for('scm/new'), new_, 'procedure'], [Symbol.for('second'), list_1.second_, 'procedure'], [Symbol.for('self-evaluating?'), procedures_1.selfEvaluatingP_, 'procedure'], [Symbol.for('set-car!'), list_1.setCarX_, 'procedure'], [Symbol.for('set-cdr!'), list_1.setCdrX_, 'procedure'], [Symbol.for('set-mcar!'), list_1.setCarX_, 'procedure'], [Symbol.for('set-mcdr!'), list_1.setCdrX_, 'procedure'], [Symbol.for('set-nth'), array_1.arraySet_, 'procedure'], [Symbol.for('set-nth!'), array_1.arraySet_, 'procedure'], [Symbol.for('seventh'), list_1.seventh_, 'procedure'], [Symbol.for('sixth'), list_1.sixth_, 'procedure'], [Symbol.for('source'), source, 'procedure'], [Symbol.for('string->number'), string_1.stringToNumber_, 'procedure'], [Symbol.for('string->symbol'), symbol_1.stringToSymbol_, 'procedure'], [Symbol.for('string-append'), string_1.stringAppend_, 'procedure'], [Symbol.for('string-downcase'), string_1.stringDowncase_, 'procedure'], [Symbol.for('string-join'), string_1.stringJoin_, 'procedure'], [Symbol.for('string-length'), list_1.length_, 'procedure'], [Symbol.for('string-object?'), string_1.stringObjectP_, 'procedure'], [Symbol.for('string-primitive?'), string_1.stringPrimitiveP_, 'procedure'], [Symbol.for('string-ref'), string_1.stringRef_, 'procedure'], [Symbol.for('string-repeat'), string_1.stringRepeat_, 'procedure'], [Symbol.for('string-replace'), string_1.stringReplace_, 'procedure'], [Symbol.for('string-split'), string_1.stringSplit_, 'procedure'], [Symbol.for('string-to-symbol'), symbol_1.stringToSymbol_, 'procedure'], [Symbol.for('string-trim'), string_1.stringTrim_, 'procedure'], [Symbol.for('string-upcase'), string_1.stringUpcase_, 'procedure'], [Symbol.for('string?'), string_1.stringp_, 'procedure'], [Symbol.for('stringp'), string_1.stringp_, 'procedure'], [Symbol.for('sub'), procedures_1.sub_, 'procedure'], [Symbol.for('sub1'), procedures_1.sub1_, 'procedure'], [Symbol.for('substring'), string_1.substring_, 'procedure'], [Symbol.for('symbol->string'), symbol_1.symbolToString_, 'procedure'], [Symbol.for('symbol-to-string'), symbol_1.symbolToString_, 'procedure'], [Symbol.for('symbol?'), symbol_1.symbolp_, 'procedure'], [Symbol.for('symbolp'), symbol_1.symbolp_, 'procedure'], [Symbol.for('tail'), list_1.cdr_, 'procedure'], [Symbol.for('take'), list_1.take_, 'procedure'], [Symbol.for('tenth'), list_1.tenth_, 'procedure'], [Symbol.for('third'), list_1.third_, 'procedure'], [Symbol.for('true?'), procedures_1.truep_, 'procedure'], [Symbol.for('truep'), procedures_1.truep_, 'procedure'], [Symbol.for('type-of'), procedures_1.typeOf_, 'procedure'], [Symbol.for('typeof'), procedures_1.typeOf_, 'procedure'], [Symbol.for('undefined?'), procedures_1.undefinedp_, 'procedure'], [Symbol.for('union'), procedures_1.union_, 'procedure'], [Symbol.for('values'), procedures_1.values_, 'procedure'], [Symbol.for('vector'), list_1.list_, 'procedure'], [Symbol.for('vector-ref'), list_1.nth_, 'procedure'], [Symbol.for('vector-set'), array_1.arraySet_, 'procedure'], [Symbol.for('vector-set!'), array_1.arraySet_, 'procedure'], [Symbol.for('vector?'), array_1.arrayp_, 'procedure'], [Symbol.for('zero?'), procedures_1.zerop_, 'procedure'], [Symbol.for('zerop'), procedures_1.zerop_, 'procedure'], [Symbol.for('.'), dot_, 'macro'], [Symbol.for(':'), colon_, 'macro'], [constants_1.quasiquoteSym_, quasiquote_, 'macro'], [constants_1.quoteSym_, quote_, 'macro'], [Symbol.for('->'), macros_1.threadFirst_, 'macro'], [Symbol.for('->>'), macros_1.threadLast_, 'macro'], [Symbol.for('and'), and_, 'macro'], [Symbol.for('ann'), ann_, 'macro'], [Symbol.for('as->'), macros_1.threadAs_, 'macro'], [Symbol.for('async'), jsAsync_, 'macro'], [Symbol.for('as~>'), macros_1.threadAs_, 'macro'], [Symbol.for('await'), jsAwait_, 'macro'], [Symbol.for('begin'), begin_, 'macro'], [Symbol.for('begin0'), macros_1.begin0_, 'macro'], [Symbol.for('block'), block_, 'macro'], [Symbol.for('break'), break_, 'macro'], [Symbol.for('call-method'), send_, 'macro'], [Symbol.for('case'), macros_1.case_, 'macro'], [Symbol.for('case/eq'), macros_1.caseEq_, 'macro'], [Symbol.for('class'), class_, 'macro'], [Symbol.for('clj/try'), macros_1.cljTry_, 'macro'], [Symbol.for('cond'), cond_, 'macro'], [Symbol.for('continue'), continue_, 'macro'], [Symbol.for('defclass'), macros_1.defclass_, 'macro'], [Symbol.for('define'), define_, 'macro'], [Symbol.for('define-class'), defineClass_, 'macro'], [Symbol.for('define-js-obj'), defineJsObj_, 'macro'], [Symbol.for('define-macro'), defineMacro_, 'macro'], [Symbol.for('define-type'), defineType_, 'macro'], [Symbol.for('define-values'), defineValues_, 'macro'], [Symbol.for('define/async'), defineAsync_, 'macro'], [Symbol.for('define/generator'), defineGenerator_, 'macro'], [Symbol.for('define/private'), macros_1.definePrivate_, 'macro'], [Symbol.for('define/public'), macros_1.definePublic_, 'macro'], [Symbol.for('defmacro'), macros_1.defmacro_, 'macro'], [Symbol.for('defun'), macros_1.defun_, 'macro'], [Symbol.for('destructuring-bind'), macros_1.multipleValueBind_, 'macro'], [Symbol.for('do'), macros_1.do_, 'macro'], [Symbol.for('field-bound?'), macros_1.fieldBoundP_, 'macro'], [Symbol.for('fn'), lambda_, 'macro'], [Symbol.for('for'), for_, 'macro'], [Symbol.for('fset'), macros_1.set_, 'macro'], [Symbol.for('get-field'), getField_, 'macro'], [Symbol.for('if'), macros_1.if_, 'macro'], [Symbol.for('js/?.'), jsOptionalChaining_, 'macro'], [Symbol.for('js/arrow'), jsArrow_, 'macro'], [Symbol.for('js/async'), jsAsync_, 'macro'], [Symbol.for('js/await'), jsAwait_, 'macro'], [Symbol.for('js/do-while'), jsDoWhile_, 'macro'], [Symbol.for('js/for'), macros_1.jsFor_, 'macro'], [Symbol.for('js/for-in'), macros_1.jsForIn_, 'macro'], [Symbol.for('js/for-of'), macros_1.jsForOf_, 'macro'], [Symbol.for('js/function'), jsFunction_, 'macro'], [Symbol.for('js/switch'), jsSwitch_, 'macro'], [Symbol.for('js/try'), jsTry_, 'macro'], [Symbol.for('js/while'), jsWhile_, 'macro'], [Symbol.for('lambda'), lambda_, 'macro'], [Symbol.for('let'), letStar_, 'macro'], [Symbol.for('let*'), letStar_, 'macro'], [Symbol.for('let*-values'), letValues_, 'macro'], [Symbol.for('let-env'), macros_1.letEnv_, 'macro'], [Symbol.for('let-js-obj'), letJsObj_, 'macro'], [Symbol.for('let-values'), letValues_, 'macro'], [Symbol.for('letrec'), letStar_, 'macro'], [Symbol.for('letrec-values'), letValues_, 'macro'], [Symbol.for('module'), module_, 'macro'], [Symbol.for('multiple-value-bind'), macros_1.multipleValueBind_, 'macro'], [Symbol.for('multiple-values-bind'), macros_1.multipleValueBind_, 'macro'], [Symbol.for('new/apply'), macros_1.newApply_, 'macro'], [Symbol.for('or'), or_, 'macro'], [Symbol.for('prog1'), macros_1.begin0_, 'macro'], [Symbol.for('progn'), begin_, 'macro'], [Symbol.for('provide'), provide_, 'macro'], [Symbol.for('require'), require_, 'macro'], [Symbol.for('return'), return_, 'macro'], [Symbol.for('rkt/new'), macros_1.rktNew_, 'macro'], [Symbol.for('send'), send_, 'macro'], [Symbol.for('send/apply'), sendApply_, 'macro'], [Symbol.for('set'), macros_1.set_, 'macro'], [Symbol.for('set!'), setX_, 'macro'], [Symbol.for('set!-js-obj'), setJsObj_, 'macro'], [Symbol.for('set!-values'), setValues_, 'macro'], [Symbol.for('set-field!'), setField_, 'macro'], [Symbol.for('setq'), setX_, 'macro'], [Symbol.for('throw'), throw_, 'macro'], [Symbol.for('try'), macros_1.cljTry_, 'macro'], [Symbol.for('unless'), macros_1.unless_, 'macro'], [Symbol.for('unwind-protect'), macros_1.unwindProtect_, 'macro'], [Symbol.for('when'), macros_1.when_, 'macro'], [Symbol.for('while'), macros_1.while_, 'macro'], [Symbol.for('yield'), yield_, 'macro'], [Symbol.for('~>'), macros_1.threadFirst_, 'macro'], [Symbol.for('~>>'), macros_1.threadLast_, 'macro'], [Symbol.for('λ'), lambda_, 'macro']]);
exports.lisp1Environment = lispEnvironment;
exports.lispEnvironment = lispEnvironment;
// (define/public ,define_ "macro")
// (new ,rkt-new_ "macro")
// (set!-field ,set-field_ "macro")
// (set-js-obj! ,set-js-obj_ "macro")
// (set-values! ,set-values_ "macro")
// Special forms, expressed as macros.
/**
 * Evaluation environment.
 */
const evalEnvironment = new env_1.LispEnvironment([[Symbol.for('eval'), interpret, 'procedure'], [Symbol.for('js/eval'), javascript_1.jsEval_, 'procedure'], [Symbol.for('scm/eval'), interpret, 'procedure'], [Symbol.for('seval'), eval_1.eval_, 'procedure']]);
/**
 * JavaScript environment.
 */
const jsEnvironment = new env_1.JavaScriptEnvironment();
/**
 * Interpretation environment.
 * Includes `eval`.
 */
const interpretationEnvironment = new env_1.EnvironmentStack(lispEnvironment, evalEnvironment, jsEnvironment);
exports.interpretationEnvironment = interpretationEnvironment;
/**
 * Interpretation environment.
 * No `eval`.
 */
const interpretationEnvironmentNoEval = new env_1.EnvironmentStack(lispEnvironment, jsEnvironment);
/**
 * Compilation environment.
 */
const compilationEnvironment = new env_1.EnvironmentStack(lispEnvironment, evalEnvironment);
exports.compilationEnvironment = compilationEnvironment;
/**
 * Language environment.
 */
const langEnvironment = interpretationEnvironment;
exports.langEnvironment = langEnvironment;
/**
 * Default options used when compiling.
 */
const defaultCompilationOptions = {
    lispEnvironment: langEnvironment,
    compilationMappingEnvironment: compilationMappingEnv,
    inlineFunctions: true,
    gensymMap: new Map()
};
/**
 * Pointer to the current compilation options.
 */
let currentCompilationOptionsPointer = defaultCompilationOptions;
__exportStar(require("./array"), exports);
__exportStar(require("./constants"), exports);
__exportStar(require("./curry"), exports);
__exportStar(require("./env"), exports);
__exportStar(require("./equal"), exports);
__exportStar(require("./eval"), exports);
__exportStar(require("./hash"), exports);
__exportStar(require("./javascript"), exports);
__exportStar(require("./list"), exports);
__exportStar(require("./macros"), exports);
__exportStar(require("./object"), exports);
__exportStar(require("./plist"), exports);
__exportStar(require("./printer"), exports);
__exportStar(require("./procedures"), exports);
__exportStar(require("./regexp"), exports);
__exportStar(require("./rose"), exports);
__exportStar(require("./string"), exports);
__exportStar(require("./symbol"), exports);
