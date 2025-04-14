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

import {
  fstatSync,
  mkdirSync,
  openSync,
  readFileSync,
  writeFileSync
} from 'fs';

import {
  basename,
  extname,
  join
} from 'path';

import {
  arrayDropRight_,
  arrayDrop_,
  arrayEighth_,
  arrayFifth_,
  arrayFirst_,
  arrayFourth_,
  arrayLast_,
  arrayLength_,
  arrayNinth_,
  arrayRef_,
  arrayRest_,
  arrayReverse_,
  arraySecond_,
  arraySet_,
  arraySeventh_,
  arraySixth_,
  arrayTake_,
  arrayTenth_,
  arrayThird_,
  arrayp_
} from './array';

import {
  defaultLanguage,
  false_,
  jsNull_,
  null_,
  packageName,
  quasiquoteSym_,
  quoteSym_,
  true_,
  undefined_
} from './constants';

import {
  __,
  curry,
  curryN,
  dashify
} from './curry';

import {
  Environment,
  EnvironmentPipe,
  EnvironmentStack,
  JavaScriptEnvironment,
  LispEnvironment,
  TypedEnvironment,
  currentEnvironment_,
  defaultEnvironment,
  emptyEnvironment,
  extendEnvironment,
  makeEnvironment,
  withEnvironment
} from './env';

import {
  eqp_,
  equalp_,
  eqvp_
} from './equal';

import {
  ArrayExpression,
  ArrayPattern,
  ArrowFunctionExpression,
  AssignmentExpression,
  AssignmentPattern,
  AwaitExpression,
  BinaryExpression,
  BlockComment,
  BlockStatement,
  BreakStatement,
  CallExpression,
  CatchClause,
  ClassBody,
  ClassDeclaration,
  ClassExpression,
  ConditionalExpression,
  ContinueStatement,
  DoWhileStatement,
  ESTreeNode,
  ExportAllDeclaration,
  ExportNamedDeclaration,
  ExportSpecifier,
  Expression,
  ExpressionStatement,
  ForOfStatement,
  ForStatement,
  FunctionDeclaration,
  FunctionExpression,
  Identifier,
  IfStatement,
  ImportDeclaration,
  ImportDefaultSpecifier,
  ImportNamespaceSpecifier,
  ImportSpecifier,
  LeadingComment,
  Literal,
  LogicalExpression,
  MemberExpression,
  MethodDefinition,
  NewExpression,
  Node,
  ObjectExpression,
  ObjectPattern,
  Program,
  Property,
  PropertyDefinition,
  RestElement,
  ReturnStatement,
  SequenceExpression,
  SpreadElement,
  SwitchCase,
  SwitchStatement,
  TSAnyKeyword,
  TSArrayType,
  TSAsExpression,
  TSBooleanKeyword,
  TSFunctionType,
  TSLiteralType,
  TSNumberKeyword,
  TSStringKeyword,
  TSTupleType,
  TSTypeAliasDeclaration,
  TSTypeParameterInstantiation,
  TSTypeReference,
  TSUndefinedKeyword,
  TSUnionType,
  TSVoidKeyword,
  TaggedTemplateExpression,
  TemplateElement,
  TemplateLiteral,
  ThisExpression,
  ThrowStatement,
  TrailingComment,
  TryStatement,
  UnaryExpression,
  UpdateExpression,
  VariableDeclaration,
  VariableDeclarator,
  WhileStatement,
  XRawJavaScript,
  YieldExpression,
  estreeTypeP,
  estreep
} from './estree';

import {
  callEvaluator,
  defaultEvaluator,
  eval_
} from './eval';

import {
  hashToList_,
  hashClearX_,
  hashClear_,
  hashCopy_,
  hashEntries_,
  hashHasKeyP_,
  hashKeys_,
  hashRef_,
  hashRemoveX_,
  hashRemove_,
  hashSetX_,
  hashSize_,
  hashValues_,
  hashp_,
  makeHash_
} from './hash';

import {
  jsDelete_,
  jsEval_,
  jsFindIndex_,
  jsFunctionObjectP_,
  jsFunctionTypeP_,
  jsFunctionP_,
  jsIn_,
  jsInstanceof_,
  jsIsLooselyEqualP_,
  jsIsStrictlyEqualP_,
  jsNullP_,
  jsPlus_,
  jsSameValueZeroP_,
  jsSameValueP_,
  jsTaggedTemplate_,
  jsTypeof_
} from './javascript';

import {
  append_,
  arrayListCdr_,
  arrayListDropRight_,
  arrayListDrop_,
  arrayListEighth_,
  arrayListFifth_,
  arrayListFirst_,
  arrayListFourth_,
  arrayListLast_,
  arrayListLength_,
  arrayListNinth_,
  arrayListNth_,
  arrayListNthcdr_,
  arrayListRest_,
  arrayListReverse_,
  arrayListSecond_,
  arrayListSeventh_,
  arrayListSixth_,
  arrayListTake_,
  arrayListTenth_,
  arrayListThird_,
  arrayListToLinkedList_,
  arrayListP_,
  buildList_,
  cadr_,
  car_,
  cdr_,
  circularListP_,
  consDotCompiled_,
  consDotF_,
  consDotP_,
  consDot_,
  consp_,
  cons_,
  dottedListP_,
  dottedPairP_,
  dropRight_,
  drop_,
  eighth_,
  fifth_,
  first_,
  flatten_,
  fourth_,
  improperListP_,
  lastCdr_,
  lastPair_,
  last_,
  length_,
  linkedListCar_,
  linkedListCdr_,
  linkedListDropRight_,
  linkedListEighth_,
  linkedListFifth_,
  linkedListFirst_,
  linkedListFourth_,
  linkedListHead_,
  linkedListLastCdr_,
  linkedListLast_,
  linkedListLength_,
  linkedListLinkCar_,
  linkedListLinkCdr_,
  linkedListLinkP_,
  linkedListNinth_,
  linkedListNth_,
  linkedListNthcdr_,
  linkedListSecond_,
  linkedListSeventh_,
  linkedListSixth_,
  linkedListTail_,
  linkedListTenth_,
  linkedListThird_,
  linkedListToArrayList_,
  linkedListP_,
  linkedPairCar_,
  linkedPairCdr_,
  linkedPairP_,
  listStar_,
  listTail_,
  listp_,
  list_,
  makeList_,
  ninth_,
  nth_,
  nthcdr_,
  nullp_,
  popLeftX_,
  popRightX_,
  properListP_,
  pushLeftX_,
  pushRightX_,
  rest_,
  reverse_,
  second_,
  setCarX_,
  setCdrX_,
  seventh_,
  sixth_,
  take_,
  tenth_,
  third_
} from './list';

import {
  begin0_,
  caseEq_,
  case_,
  cljTry_,
  defclass_,
  definePrivate_,
  definePublic_,
  defmacro_,
  defun_,
  do_,
  fieldBoundP_,
  if_,
  jsForIn_,
  jsForOf_,
  jsFor_,
  letEnv_,
  multipleValueBind_,
  newApply_,
  rktNew_,
  set_,
  threadAs_,
  threadFirst_,
  threadLast_,
  unless_,
  unwindProtect_,
  when_,
  while_
} from './macros';

import {
  fieldNames_,
  jsKeys_,
  jsObjAppend_,
  jsObjP_,
  jsObj_,
  jsObjectTypeP_,
  objectRef_,
  objectSetX_
} from './object';

import {
  LeadingCommentToken,
  TrailingCommentToken,
  getCommentLevel,
  read,
  readRose,
  readSexp,
  tokenize
} from './parser';

import {
  plistToAlist_,
  plistCopy_,
  plistGet_,
  plistHasP_,
  plistSetX_,
  plistp_
} from './plist';

import {
  print,
  printEstree,
  printSexp,
  printSexpAsExpression,
  writeToString
} from './printer';

import {
  add1_,
  add_,
  apply_,
  assert_,
  booleanp_,
  const_,
  display_,
  div_,
  error_,
  evenp_,
  falsep_,
  fexprp_,
  filter_,
  findfIndex_,
  findf_,
  foldl_,
  foldr_,
  funcall_,
  gt_,
  gte_,
  identity_,
  indexOf_,
  indexWhere_,
  isAP_,
  intersection_,
  keywordp_,
  lt_,
  lte_,
  map_,
  memberp_,
  member_,
  memfp_,
  memf_,
  memqp_,
  memq_,
  modulo_,
  mul_,
  not_,
  numberp_,
  oddp_,
  onep_,
  procedurep_,
  range_,
  selfEvaluatingP_,
  sub1_,
  sub_,
  truep_,
  typeOf_,
  undefinedp_,
  union_,
  values_,
  zerop_
} from './procedures';

import {
  regexpMatchP_,
  regexpMatch_,
  regexpQuote_,
  regexpReplace_,
  regexpp_,
  regexp_
} from './regexp';

import {
  Rose,
  beginWrapRose,
  beginWrapRoseSmart,
  beginWrapRoseSmart1,
  insertSexpIntoRose,
  makeRose,
  rosep,
  sliceRose,
  transferComments
} from './rose';

import {
  s,
  sexp
} from './sexp';

import {
  numberToString_,
  stringToNumber_,
  stringAppend_,
  stringDowncase_,
  stringJoin_,
  stringObjectP_,
  stringPrimitiveP_,
  stringRef_,
  stringRepeat_,
  stringReplace_,
  stringSplit_,
  stringTrim_,
  stringUpcase_,
  stringp_,
  substring_
} from './string';

import {
  gensym_,
  gensymp_,
  stringToSymbol_,
  symbolToString_,
  symbolp_
} from './symbol';

import {
  thunk,
  ThunkedMap
} from './thunk';

import {
  beginWrap,
  colonFormP,
  formp,
  kebabCaseToCamelCase,
  kebabCaseToSnakeCase,
  lambdaToLet,
  mapTree,
  quotep,
  taggedListP,
  textOfQuotation
} from './util';

import {
  makeVisitor,
  visit
} from './visitor';

const [lastCdr, cdr, flatten, buildList, listStar, makeList, cons, findf, length]: any[] = ((): any => {
  function lastCdr_(lst: any): any {
    if (!Array.isArray(lst)) {
      return undefined;
    } else if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
      let result: any = lst;
      while (Array.isArray(result) && (result.length >= 3) && (result[result.length - 2] === Symbol.for('.'))) {
        result = result[result.length - 1];
      }
      return result;
    } else {
      return [];
    }
  }
  function cdr_(lst: any): any {
    if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
      return lst[2];
    } else {
      return lst.slice(1);
    }
  }
  function flatten_(lst: any): any {
    return lst.reduce(function (acc: any, x: any): any {
      if (Array.isArray(x)) {
        return [...acc, ...flatten_(x)];
      } else if (x === Symbol.for('.')) {
        return acc;
      } else {
        acc.push(x);
        return acc;
      }
    }, []);
  }
  function buildList_(n: any, proc: any): any {
    return ((): any => {
      function range_(start: any, end: any = undefined, step: any = undefined): any {
        const startN: any = (end === undefined) ? 0 : start;
        const endN: any = (end === undefined) ? start : end;
        const stepN: any = step || 1;
        let result: any = [];
        for (let i: any = startN; (stepN < 0) ? (i > endN) : (i < endN); i = i + stepN) {
          result.push(i);
        }
        return result;
      }
      return range_;
    })()(0, n).map(function (x: any): any {
      return proc(x);
    });
  }
  function listStar_(...args: any[]): any {
    if (args.length === 0) {
      return undefined;
    } else if (args.length === 1) {
      return args[0];
    } else {
      const tailLst: any = args[args.length - 1];
      const headLst: any = args.slice(0, -1);
      if (Array.isArray(tailLst)) {
        return [...headLst, ...tailLst];
      } else {
        return [...headLst, Symbol.for('.'), tailLst];
      }
    }
  }
  function makeList_(k: any, v: any): any {
    let result: any = [];
    for (let i: any = 0; i < k; i++) {
      result.push(v);
    }
    return result;
  }
  function cons_(x: any, y: any): any {
    if (Array.isArray(y)) {
      return [x, ...y];
    } else {
      return [x, Symbol.for('.'), y];
    }
  }
  function findf_(proc: any, lst: any, notFound: any = false): any {
    const idx: any = lst.findIndex(proc);
    if (idx >= 0) {
      return (lst as any)[idx];
    } else {
      return notFound;
    }
  }
  function length_(lst: any): any {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
      return ((): any => {
        function linkedListLength_(lst: any): any {
          let len: any = 0;
          let current: any = lst;
          while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
            len = len + (lst.length - 2);
            current = current[current.length - 1];
          }
          return len;
        }
        return linkedListLength_;
      })()(lst);
    } else {
      return lst.length;
    }
  }
  function range_(start: any, end: any = undefined, step: any = undefined): any {
    const startN: any = (end === undefined) ? 0 : start;
    const endN: any = (end === undefined) ? start : end;
    const stepN: any = step || 1;
    let result: any = [];
    for (let i: any = startN; (stepN < 0) ? (i > endN) : (i < endN); i = i + stepN) {
      result.push(i);
    }
    return result;
  }
  function linkedListLength_(lst: any): any {
    let len: any = 0;
    let current: any = lst;
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
const defaultOptions: any = {
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
function addDefaultOptions(options: any, modify: any = false): any {
  let result: any = modify ? options : {
    ...options
  };
  for (let key of Object.keys(defaultOptions)) {
    if ((result as any)[key] === undefined) {
      (result as any)[key] = (defaultOptions as any)[key];
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
class CompilationEnvironment extends TypedEnvironment {
}

/**
 * Compilation variable environment.
 *
 * An environment mapping various Lisp values to their
 * JavaScript equivalents.
 */
const compilationVariablesEnv: any = new CompilationEnvironment([[Symbol.for('#f'), new Literal(false), 'variable'], [Symbol.for('#t'), new Literal(true), 'variable'], [Symbol.for('js-null'), new Literal(null), 'variable'], [Symbol.for('js-undefined'), new Literal(undefined), 'variable'], [Symbol.for('js/arguments'), new Identifier('arguments'), 'variable'], [Symbol.for('js/null'), new Literal(null), 'variable'], [Symbol.for('js/require'), new Identifier('require'), 'variable'], [Symbol.for('js/undefined'), new Literal(undefined), 'variable'], [Symbol.for('*cons-dot*'), consDotCompiled_, 'variable'], [Symbol.for('nil'), new Literal(null), 'variable'], [Symbol.for('null'), new ArrayExpression(), 'variable'], [Symbol.for('t'), new Literal(true), 'variable'], [Symbol.for('undefined'), new Literal(undefined), 'variable']]);

/**
 * Compiler procedures mapping environment.
 */
const compilationCompilerMappingEnv: any = new CompilationEnvironment([[add_, compileAdd, 'compiler'], [and_, compileAnd, 'compiler'], [ann_, compileAnn, 'compiler'], [append_, compileAppend, 'compiler'], [apply_, compileApply, 'compiler'], [arrayRef_, compileArrayRef, 'compiler'], [arraySet_, compileArraySet, 'compiler'], [begin_, compileBegin, 'compiler'], [block_, compileBlock, 'compiler'], [break_, compileBreak, 'compiler'], [class_, compileClass, 'compiler'], [colon_, compileColon, 'compiler'], [cond_, compileCond, 'compiler'], [continue_, compileContinue, 'compiler'], [defineAsync_, compileDefineAsync, 'compiler'], [defineClass_, compileDefineClass, 'compiler'], [defineGenerator_, compileDefineGenerator, 'compiler'], [defineJsObj_, compileDefineJsObj, 'compiler'], [defineMacro_, compileDefineMacro, 'compiler'], [defineType_, compileDefineType, 'compiler'], [defineValues_, compileDefineValues, 'compiler'], [define_, compileDefine, 'compiler'], [div_, compileDiv, 'compiler'], [dot_, compileSend, 'compiler'], [for_, compileFor, 'compiler'], [funcall_, compileFuncall, 'compiler'], [getField_, compileGetField, 'compiler'], [gt_, compileGreaterThan, 'compiler'], [gte_, compileGreaterThanOrEqual, 'compiler'], [jsArrow_, compileJsArrow, 'compiler'], [jsAsync_, compileJsAsync, 'compiler'], [jsAwait_, compileJsAwait, 'compiler'], [jsDelete_, compileJsDelete, 'compiler'], [jsDoWhile_, compileJsDoWhile, 'compiler'], [jsEval_, compileJsEval, 'compiler'], [jsFunction_, compileJsFunction, 'compiler'], [jsIn_, compileJsIn, 'compiler'], [jsInstanceof_, compileJsInstanceof, 'compiler'], [jsIsLooselyEqualP_, compileJsIsLooselyEqual, 'compiler'], [jsIsStrictlyEqualP_, compileJsIsStrictlyEqual, 'compiler'], [jsObjAppend_, compileJsObjAppend, 'compiler'], [jsObj_, compileJsObj, 'compiler'], [jsOptionalChaining_, compileJsOptionalChaining, 'compiler'], [jsPlus_, compileAdd, 'compiler'], [jsSwitch_, compileJsSwitch, 'compiler'], [jsTaggedTemplate_, compileJsTaggedTemplate, 'compiler'], [jsTry_, compileJsTry, 'compiler'], [jsTypeof_, compileJsTypeof, 'compiler'], [jsWhile_, compileJsWhile, 'compiler'], [js_, compileJs, 'compiler'], [lambda_, compileLambda, 'compiler'], [letJsObj_, compileLetJsObj, 'compiler'], [letStar_, compileLet, 'compiler'], [letValues_, compileLetValues, 'compiler'], [list_, compileList, 'compiler'], [lt_, compileLessThan, 'compiler'], [lte_, compileLessThanOrEqual, 'compiler'], [module_, compileModule, 'compiler'], [modulo_, compileModulo, 'compiler'], [mul_, compileMul, 'compiler'], [new_, compileNew, 'compiler'], [not_, compileNot, 'compiler'], [objectRef_, compileObjectRef, 'compiler'], [objectSetX_, compileObjectSet, 'compiler'], [or_, compileOr, 'compiler'], [provide_, compileProvide, 'compiler'], [pushLeftX_, compilePushLeft, 'compiler'], [pushRightX_, compilePushRight, 'compiler'], [quasiquote_, compileQuasiquote, 'compiler'], [quote_, compileQuote, 'compiler'], [require_, compileRequire, 'compiler'], [return_, compileReturn, 'compiler'], [sendApply_, compileSendApply, 'compiler'], [send_, compileSend, 'compiler'], [setX_, compileSet, 'compiler'], [setField_, compileSetField, 'compiler'], [setJsObj_, compileSetJsObj, 'compiler'], [setValues_, compileSetValues, 'compiler'], [stringAppend_, compileStringAppend, 'compiler'], [sub_, compileSub, 'compiler'], [throw_, compileThrow, 'compiler'], [yield_, compileYield, 'compiler']]);

/**
 * Compiler macros mapping environment.
 */
const compilationMacroMappingEnv: any = new CompilationEnvironment([[arrayDropRight_, compileArrayDropRightMacro, 'macro'], [arrayDrop_, compileArrayDropMacro, 'macro'], [arrayListDropRight_, compileArrayListDropRightMacro, 'macro'], [arrayListDrop_, compileArrayListDropMacro, 'macro'], [assert_, compileAssertMacro, 'macro'], [display_, compileDisplayMacro, 'macro'], [dropRight_, compileDropRightMacro, 'macro'], [drop_, compileDropMacro, 'macro'], [foldl_, compileFoldlMacro, 'macro'], [foldr_, compileFoldrMacro, 'macro'], [hashClear_, compileHashClearMacro, 'macro'], [hashRef_, compileHashRefMacro, 'macro'], [hashRemoveX_, compileHashRemoveMacro, 'macro'], [hashRemove_, compileHashRemoveMacro, 'macro'], [makeHash_, compileMakeHashMacro, 'macro'], [map_, compileMapMacro, 'macro'], [memberp_, compileMemberPMacro, 'macro'], [print, compileDisplayMacro, 'macro'], [regexp_, compileRegexpMacro, 'macro'], [stringTrim_, compileStringTrimMacro, 'macro'], [stringp_, compileStringpMacro, 'macro'], [substring_, compileSubstringMacro, 'macro'], [values_, compileValuesMacro, 'macro']]);

/**
 * Compilation mapping environment.
 *
 * An environment mapping Lisp functions to compiler procedures
 * or compiler macros.
 */
const compilationMappingEnv: any = new EnvironmentStack(compilationMacroMappingEnv, compilationCompilerMappingEnv);

/**
 * Inlined functions.
 *
 * A list of functions whose definition is so simple
 * that it might be inlined directly into the call site.
 */
const inlinedFunctions: any = [add1_, arrayEighth_, arrayFifth_, arrayFirst_, arrayFourth_, arrayLast_, arrayLength_, arrayListCdr_, arrayListEighth_, arrayListFifth_, arrayListFirst_, arrayListFourth_, arrayListLast_, arrayListLength_, arrayListNinth_, arrayListNth_, arrayListNthcdr_, arrayListRest_, arrayListReverse_, arrayListSecond_, arrayListSeventh_, arrayListSixth_, arrayListTake_, arrayListTenth_, arrayListThird_, arrayListP_, arrayNinth_, arrayRest_, arrayReverse_, arraySecond_, arraySeventh_, arraySixth_, arrayTake_, arrayTenth_, arrayThird_, arrayp_, booleanp_, consDotF_, consDotP_, consp_, const_, dottedListP_, dottedPairP_, eighth_, eqp_, eqvp_, error_, evenp_, fieldNames_, fifth_, filter_, findfIndex_, first_, fourth_, gensymp_, gensym_, hashToList_, hashClearX_, hashCopy_, hashEntries_, hashHasKeyP_, hashKeys_, hashRemoveX_, hashSetX_, hashSize_, hashValues_, hashp_, indexWhere_, isAP_, jsFindIndex_, jsFunctionObjectP_, jsFunctionTypeP_, jsFunctionP_, jsKeys_, jsNullP_, jsObjP_, jsObjectTypeP_, jsSameValueP_, linkedListCar_, linkedListCdr_, linkedListEighth_, linkedListFifth_, linkedListFirst_, linkedListFourth_, linkedListHead_, linkedListLinkCar_, linkedListLinkCdr_, linkedListLinkP_, linkedListNinth_, linkedListNth_, linkedListNthcdr_, linkedListSecond_, linkedListSeventh_, linkedListSixth_, linkedListTail_, linkedListTenth_, linkedListThird_, linkedListP_, linkedPairCar_, linkedPairCdr_, linkedPairP_, listp_, memfp_, memqp_, ninth_, nth_, nullp_, numberToString_, numberp_, oddp_, onep_, plistCopy_, plistp_, popLeftX_, popRightX_, procedurep_, regexpMatchP_, regexpMatch_, regexpQuote_, regexpReplace_, regexpp_, rest_, reverse_, second_, seventh_, sixth_, stringToNumber_, stringToSymbol_, stringDowncase_, stringJoin_, stringObjectP_, stringPrimitiveP_, stringRef_, stringRepeat_, stringSplit_, stringUpcase_, sub1_, symbolToString_, symbolp_, tenth_, third_, typeOf_, undefinedp_, zerop_];

/**
 * Compilation map.
 *
 * Map from languages to compilation mapping environments.
 */
const compilationMap: any = new Map([['JavaScript', compilationMappingEnv], ['TypeScript', compilationMappingEnv]] as any);

/**
 * Compile a Lisp expression to JavaScript or TypeScript.
 * Returns a string of JavaScript or TypeScript code.
 *
 * `exp` may be an S-expression, an S-expression wrapped in a rose
 * tree, or a module object.
 */
function compile(exp: any, env: any = new LispEnvironment(), options: any = {}): any {
  const languageOption: any = options['language'] || defaultLanguage;
  const langEnv: any = extendsLispEnvironmentP(env) ? env : new EnvironmentStack(env, langEnvironment);
  const mappingEnv: any = compilationMap.get(languageOption) || compilationMappingEnv;
  const compilationOptions: any = addDefaultOptions(options, true);
  const compiledEnv: any = new LispEnvironment();
  let bindingsEnv: any = new LispEnvironment();
  const continuationEnv: any = new EnvironmentStack(bindingsEnv, env);
  compilationOptions['lispEnvironment'] = langEnv;
  compilationOptions['compilationMappingEnvironment'] = mappingEnv;
  compilationOptions['bindings'] = bindingsEnv;
  compilationOptions['continuationEnv'] = continuationEnv;
  compilationOptions['compiledEnv'] = compiledEnv;
  let ast: any = (exp instanceof Module) ? compileModule(exp, langEnv, compilationOptions) : ((exp instanceof Rose) ? compileRose(exp, langEnv, compilationOptions) : compileSexp(exp, langEnv, compilationOptions));
  ast = optimizeEstree(ast);
  return printEstree(ast, compilationOptions);
}

compile.lispSource = [Symbol.for('define'), [Symbol.for('compile'), Symbol.for('exp'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language-option'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'language'], Symbol.for('default-language')]], [Symbol.for('define'), Symbol.for('lang-env'), [Symbol.for('if'), [Symbol.for('extends-lisp-environment?'), Symbol.for('env')], Symbol.for('env'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('env'), Symbol.for('lang-environment')]]], [Symbol.for('define'), Symbol.for('mapping-env'), [Symbol.for('or'), [Symbol.for('hash-ref'), Symbol.for('compilation-map'), Symbol.for('language-option')], Symbol.for('compilation-mapping-env')]], [Symbol.for('define'), Symbol.for('compilation-options'), [Symbol.for('add-default-options'), Symbol.for('options'), Symbol.for('#t')]], [Symbol.for('define'), Symbol.for('compiled-env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('define'), Symbol.for('continuation-env'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('bindings-env'), Symbol.for('env')]], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'lispEnvironment', Symbol.for('lang-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'compilationMappingEnvironment', Symbol.for('mapping-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'bindings', Symbol.for('bindings-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'continuationEnv', Symbol.for('continuation-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'compiledEnv', Symbol.for('compiled-env')], [Symbol.for('define'), Symbol.for('ast'), [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Module')], [Symbol.for('compile-module'), Symbol.for('exp'), Symbol.for('lang-env'), Symbol.for('compilation-options')]], [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('compile-rose'), Symbol.for('exp'), Symbol.for('lang-env'), Symbol.for('compilation-options')]], [Symbol.for('else'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('lang-env'), Symbol.for('compilation-options')]]]], [Symbol.for('set!'), Symbol.for('ast'), [Symbol.for('optimize-estree'), Symbol.for('ast')]], [Symbol.for('print-estree'), Symbol.for('ast'), Symbol.for('compilation-options')]];

/**
 * Compile a set of modules together.
 * The modules may reference one another.
 */
function compileModules(modules: any, env: any, options: any = {}): any {
  let moduleMap: any = new Map();
  let compiledModuleMap: any;
  let moduleName: any;
  for (let module of modules) {
    if (!(module instanceof Rose)) {
      module = makeRose(module);
    }
    moduleName = module.get(1).getValue();
    if (typeof moduleName === 'symbol') {
      moduleName = moduleName.description as string;
    }
    moduleName = moduleName.replace(new RegExp('^\\./'), '');
    moduleMap.set(moduleName, module);
  }
  compiledModuleMap = compileModuleMap(moduleMap, env, options);
  // `(,@(send compiled-module-map values))
  return [...compiledModuleMap.values()];
}

compileModules.lispSource = [Symbol.for('define'), [Symbol.for('compile-modules'), Symbol.for('modules'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('module-map'), [Symbol.for('make-hash')]], [Symbol.for('define'), Symbol.for('compiled-module-map')], [Symbol.for('define'), Symbol.for('module-name')], [Symbol.for('for'), [[Symbol.for('module'), Symbol.for('modules')]], [Symbol.for('unless'), [Symbol.for('is-a?'), Symbol.for('module'), Symbol.for('Rose')], [Symbol.for('set!'), Symbol.for('module'), [Symbol.for('make-rose'), Symbol.for('module')]]], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('module'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('_'), Symbol.for('get-value')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('module-name')], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('symbol->string'), Symbol.for('module-name')]]], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\./'], Symbol.for('module-name'), '']], [Symbol.for('hash-set!'), Symbol.for('module-map'), Symbol.for('module-name'), Symbol.for('module')]], [Symbol.for('set!'), Symbol.for('compiled-module-map'), [Symbol.for('compile-module-map'), Symbol.for('module-map'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('append'), [Symbol.for('send'), Symbol.for('compiled-module-map'), Symbol.for('values')]]];

/**
 * Compile a module map.
 * Returns a new map containing compiled modules.
 */
function compileModuleMap(moduleMap: any, env: any, options: any = {}): any {
  let result: any = new Map();
  const moduleObjectMap: any = makeModuleMap(moduleMap, env);
  let compiledModule: any;
  let module: any;
  for (let key of moduleObjectMap.keys()) {
    module = moduleObjectMap.get(key);
    compiledModule = compile(module, env, options);
    result.set(key, compiledModule);
  }
  return result;
}

compileModuleMap.lispSource = [Symbol.for('define'), [Symbol.for('compile-module-map'), Symbol.for('module-map'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('make-hash')]], [Symbol.for('define'), Symbol.for('module-object-map'), [Symbol.for('make-module-map'), Symbol.for('module-map'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('compiled-module')], [Symbol.for('define'), Symbol.for('module')], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('send'), Symbol.for('module-object-map'), Symbol.for('keys')]]], [Symbol.for('set!'), Symbol.for('module'), [Symbol.for('send'), Symbol.for('module-object-map'), Symbol.for('get'), Symbol.for('key')]], [Symbol.for('set!'), Symbol.for('compiled-module'), [Symbol.for('compile'), Symbol.for('module'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('hash-set!'), Symbol.for('result'), Symbol.for('key'), Symbol.for('compiled-module')]], Symbol.for('result')];

/**
 * Compile a module expression or object.
 */
function compileModule(obj: any, env: any, options: any = {}): any {
  if (obj instanceof Module) {
    return compileModuleObject(obj, env, options);
  } else {
    return compileModuleExpression(obj, env, options);
  }
}

compileModule.lispSource = [Symbol.for('define'), [Symbol.for('compile-module'), Symbol.for('obj'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('Module')], [Symbol.for('compile-module-object'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-module-expression'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(module ...)` expression.
 */
function compileModuleExpression(node: any, env: any, options: any = {}): any {
  let module: any = moduleExpressionToModuleObject(node, env);
  const compilationOptions: any = {
    ...options,
    currentModule: module
  };
  return compileModuleObject(module, env, compilationOptions);
}

compileModuleExpression.lispSource = [Symbol.for('define'), [Symbol.for('compile-module-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('module'), [Symbol.for('module-expression-to-module-object'), Symbol.for('node'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('compilation-options'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'currentModule', Symbol.for('module')]]], [Symbol.for('compile-module-object'), Symbol.for('module'), Symbol.for('env'), Symbol.for('compilation-options')]];

/**
 * Compile a `Module` object.
 */
function compileModuleObject(module: any, env: any, options: any = {}): any {
  const expressions: any = module.getExpressions();
  let bindings: any = options['bindings'] || new LispEnvironment();
  const moduleEnvironment: any = module.getEnvironment();
  const moduleOptions: any = {
    bindings: bindings,
    currentModule: module,
    referencedSymbols: [],
    inlineLispSources: module.getInlineLispSourcesFlag(),
    ...options
  };
  const headerStatements: any = compileStatement(beginWrapRose(module.headerNodes), moduleEnvironment, moduleOptions);
  const requireStatements: any = compileStatement(beginWrapRose(module.requireNodes), moduleEnvironment, moduleOptions);
  const mainStatements: any = compileStatement(beginWrapRose(module.mainNodes), moduleEnvironment, moduleOptions);
  const provideStatements: any = compileStatement(beginWrapRose(module.provideNodes), moduleEnvironment, moduleOptions);
  const globalEnvironment: any = buildGlobalEnvironment(moduleOptions['referencedSymbols'], moduleEnvironment, options);
  const program: any = makeProgram([...headerStatements.body, ...requireStatements.body, ...globalEnvironment.body, ...mainStatements.body, ...provideStatements.body]);
  return program;
}

compileModuleObject.lispSource = [Symbol.for('define'), [Symbol.for('compile-module-object'), Symbol.for('module'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expressions'), [Symbol.for('send'), Symbol.for('module'), Symbol.for('get-expressions')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'bindings'], [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('define'), Symbol.for('module-environment'), [Symbol.for('send'), Symbol.for('module'), Symbol.for('get-environment')]], [Symbol.for('define'), Symbol.for('module-options'), [Symbol.for('js-obj-append'), [Symbol.for('js-obj'), 'bindings', Symbol.for('bindings'), 'currentModule', Symbol.for('module'), 'referencedSymbols', [Symbol.for('quote'), []], 'inlineLispSources', [Symbol.for('send'), Symbol.for('module'), Symbol.for('get-inline-lisp-sources-flag')]], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('header-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('header-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('require-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('require-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('main-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('main-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('provide-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('provide-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('global-environment'), [Symbol.for('build-global-environment'), [Symbol.for('oget'), Symbol.for('module-options'), 'referencedSymbols'], Symbol.for('module-environment'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('program'), [Symbol.for('make-program'), [Symbol.for('append'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('header-statements')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('require-statements')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('global-environment')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('main-statements')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('provide-statements')]]]], Symbol.for('program')];

/**
 * Compile a set of files.
 * This function writes to disk.
 */
function compileFilesX(files: any, options: any = {}): any {
  const moduleExpressionMap: any = new ThunkedMap();
  const filenameMap: any = new ThunkedMap();
  const indentOption: any = options['indent'];
  const languageOption: any = (options['language'] || '').match(new RegExp('^TypeScript$', 'i')) ? 'TypeScript' : 'JavaScript';
  const outDirOption: any = options['outDir'] || '';
  const commentsOption: any = options['comments'];
  const quickOption: any = options['quick'];
  const compilationOptions: any = {
    ...options,
    expressionType: 'statement',
    language: languageOption
  };
  const extension: any = (languageOption === 'TypeScript') ? '.ts' : '.js';
  let code: any;
  let data: any;
  let module: any;
  let moduleName: any;
  const moduleNames: any = [];
  let moduleMap: any;
  let node: any;
  let outFile: any;
  for (let file of files) {
    moduleName = basename(file, extname(file));
    filenameMap.set(moduleName, file);
    moduleExpressionMap.set(moduleName, thunk(function (): any {
      const data: any = '(module m scheme\n' +
        readFileSync(file, {
          encoding: 'utf8'
        }).replace(new RegExp('^#!.*'), '') + '\n' +
        ')';
      let node: any = readRose(data, {
        comments: commentsOption
      });
      return node;
    }));
    if (quickOption) {
      let shouldCompile: any = false;
      try {
        const inFile: any = file;
        const inStats: any = fstatSync(openSync(inFile, 'r'));
        let outFile: any = join(outDirOption, moduleName + extension);
        const outStats: any = fstatSync(openSync(outFile, 'r'));
        if (inStats.mtimeMs > outStats.mtimeMs) {
          shouldCompile = true;
        }
      } catch (err) {
        if (err instanceof Error) {
          shouldCompile = true;
        } else {
          throw err;
        }
      }
      if (shouldCompile) {
        moduleNames.push(moduleName);
      }
    } else {
      moduleNames.push(moduleName);
    }
  }
  moduleMap = makeModuleMap(moduleExpressionMap, compilationEnvironment);
  for (let moduleName of moduleNames) {
    module = moduleMap.get(moduleName);
    code = compile(module, compilationEnvironment, compilationOptions);
    outFile = join(outDirOption, moduleName + extension);
    mkdirSync(outDirOption, {
      recursive: true
    });
    writeFileSync(outFile, code, {
      encoding: 'utf8'
    });
    console.log('Compiled ' + filenameMap.get(moduleName) + ' to ' + outFile);
  }
  return moduleMap;
}

compileFilesX.lispSource = [Symbol.for('define'), [Symbol.for('compile-files!'), Symbol.for('files'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('module-expression-map'), [Symbol.for('new'), Symbol.for('ThunkedMap')]], [Symbol.for('define'), Symbol.for('filename-map'), [Symbol.for('new'), Symbol.for('ThunkedMap')]], [Symbol.for('define'), Symbol.for('indent-option'), [Symbol.for('oget'), Symbol.for('options'), 'indent']], [Symbol.for('define'), Symbol.for('language-option'), [Symbol.for('if'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^TypeScript$', 'i'], [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'language'], '']], 'TypeScript', 'JavaScript']], [Symbol.for('define'), Symbol.for('out-dir-option'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'outDir'], '']], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('quick-option'), [Symbol.for('oget'), Symbol.for('options'), 'quick']], [Symbol.for('define'), Symbol.for('compilation-options'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'statement', 'language', Symbol.for('language-option')]]], [Symbol.for('define'), Symbol.for('extension'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language-option'), 'TypeScript'], '.ts', '.js']], [Symbol.for('define'), Symbol.for('code')], [Symbol.for('define'), Symbol.for('data')], [Symbol.for('define'), Symbol.for('module')], [Symbol.for('define'), Symbol.for('module-name')], [Symbol.for('define'), Symbol.for('module-names'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('module-map')], [Symbol.for('define'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('out-file')], [Symbol.for('for'), [[Symbol.for('file'), Symbol.for('files')]], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('basename'), Symbol.for('file'), [Symbol.for('extname'), Symbol.for('file')]]], [Symbol.for('hash-set!'), Symbol.for('filename-map'), Symbol.for('module-name'), Symbol.for('file')], [Symbol.for('hash-set!'), Symbol.for('module-expression-map'), Symbol.for('module-name'), [Symbol.for('thunk'), [Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('data'), [Symbol.for('~>'), Symbol.for('file'), [Symbol.for('readFileSync'), Symbol.for('_'), [Symbol.for('js-obj'), 'encoding', 'utf8']], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^#!.*'], Symbol.for('_'), ''], [Symbol.for('string-append'), '(module m scheme\n', Symbol.for('_'), '\n' +
  ')']]], [Symbol.for('define'), Symbol.for('node'), [Symbol.for('read-rose'), Symbol.for('data'), [Symbol.for('js-obj'), 'comments', Symbol.for('comments-option')]]], Symbol.for('node')]]], [Symbol.for('cond'), [Symbol.for('quick-option'), [Symbol.for('define'), Symbol.for('should-compile'), Symbol.for('#f')], [Symbol.for('try'), [Symbol.for('define'), Symbol.for('in-file'), Symbol.for('file')], [Symbol.for('define'), Symbol.for('in-stats'), [Symbol.for('fstatSync'), [Symbol.for('openSync'), Symbol.for('in-file'), 'r']]], [Symbol.for('define'), Symbol.for('out-file'), [Symbol.for('join'), Symbol.for('out-dir-option'), [Symbol.for('string-append'), Symbol.for('module-name'), Symbol.for('extension')]]], [Symbol.for('define'), Symbol.for('out-stats'), [Symbol.for('fstatSync'), [Symbol.for('openSync'), Symbol.for('out-file'), 'r']]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('get-field'), Symbol.for('mtimeMs'), Symbol.for('in-stats')], [Symbol.for('get-field'), Symbol.for('mtimeMs'), Symbol.for('out-stats')]], [Symbol.for('set!'), Symbol.for('should-compile'), Symbol.for('#t')]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('err'), [Symbol.for('set!'), Symbol.for('should-compile'), Symbol.for('#t')]]], [Symbol.for('when'), Symbol.for('should-compile'), [Symbol.for('push-right!'), Symbol.for('module-names'), Symbol.for('module-name')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('module-names'), Symbol.for('module-name')]]]], [Symbol.for('set!'), Symbol.for('module-map'), [Symbol.for('make-module-map'), Symbol.for('module-expression-map'), Symbol.for('compilation-environment')]], [Symbol.for('for'), [[Symbol.for('module-name'), Symbol.for('module-names')]], [Symbol.for('set!'), Symbol.for('module'), [Symbol.for('send'), Symbol.for('module-map'), Symbol.for('get'), Symbol.for('module-name')]], [Symbol.for('set!'), Symbol.for('code'), [Symbol.for('compile'), Symbol.for('module'), Symbol.for('compilation-environment'), Symbol.for('compilation-options')]], [Symbol.for('set!'), Symbol.for('out-file'), [Symbol.for('join'), Symbol.for('out-dir-option'), [Symbol.for('string-append'), Symbol.for('module-name'), Symbol.for('extension')]]], [Symbol.for('mkdirSync'), Symbol.for('out-dir-option'), [Symbol.for('js-obj'), 'recursive', Symbol.for('#t')]], [Symbol.for('writeFileSync'), Symbol.for('out-file'), Symbol.for('code'), [Symbol.for('js-obj'), 'encoding', 'utf8']], [Symbol.for('display'), [Symbol.for('string-append'), 'Compiled ', [Symbol.for('hash-ref'), Symbol.for('filename-map'), Symbol.for('module-name')], ' to ', Symbol.for('out-file')]]], Symbol.for('module-map')];

/**
 * Compile a file.
 * This function writes to disk.
 */
function compileFileX(infile: any, outfile: any, options: any = {}): any {
  // TODO: `outfile`. Maybe by adding an
  // `outFileMap` option to `compile-files!`?
  return compileFilesX([infile], options);
}

compileFileX.lispSource = [Symbol.for('define'), [Symbol.for('compile-file!'), Symbol.for('infile'), Symbol.for('outfile'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-files!'), [Symbol.for('list'), Symbol.for('infile')], Symbol.for('options')]];

/**
 * Compile a S-expression wrapped in a rose tree.
 */
function compileRose(node: any, env: any, options: any = {}): any {
  const inheritedOptions: any = options['bindings'] ? options : {
    ...options,
    bindings: new LispEnvironment()
  };
  let bindings: any = inheritedOptions['bindings'];
  const commentsOption: any = options['comments'];
  const node1: any = optimizeRose(node, env);
  let exp: any = node1.getValue();
  let result: any;
  if (Array.isArray(exp)) {
    if (exp.length === 0) {
      result = compileList(node1, env, inheritedOptions);
    } else {
      const op: any = exp[0];
      if ((typeof op === 'symbol') && bindings.has(op) && (bindings.getType(op) !== 'macro')) {
        result = compileFunctionCall(node1, env, inheritedOptions);
      } else if ((typeof op === 'symbol') && (op.description as string).match(new RegExp('^\\.'))) {
        result = compileDot(node1, env, inheritedOptions);
      } else {
        const [f, opType]: any[] = env.getTypedValue(op);
        if (opType === 'undefined') {
          result = compileFunctionCall(node1, env, inheritedOptions);
        } else if (inlinedFunctions.includes(f)) {
          const inlinedExp: any = definitionToMacro(source(f), exp.slice(1));
          const inlinedNode: any = makeRose(inlinedExp, node);
          result = compileRose(inlinedNode, env, options);
        } else {
          const compilationMappingEnvironment: any = inheritedOptions['compilationMappingEnvironment'];
          const [compilationF, compilationType]: any[] = compilationMappingEnvironment.getTypedValue(f);
          if (compilationType === 'compiler') {
            // Compiler function.
            result = compilationF(node1, env, inheritedOptions);
          } else if (compilationType === 'macro') {
            // Compilation macro.
            result = compileRose(insertSexpIntoRose(compilationF(exp, env), node1, node1), env, inheritedOptions);
          } else if (opType === 'macro') {
            // Macro call.
            result = compileMacroCall(node1, env, inheritedOptions);
          } else {
            result = compileFunctionCall(node1, env, inheritedOptions);
          }
        }
      }
    }
  } else if (typeof exp === 'string') {
    result = compileString(node1, env, inheritedOptions);
  } else if (typeof exp === 'symbol') {
    result = compileVariable(node1, env, inheritedOptions);
  } else if (estreep(exp)) {
    result = exp;
  } else {
    result = compileAtom(node1, env, inheritedOptions);
  }
  if (commentsOption && node1.hasProperty('comments')) {
    let comments: any = node1.getProperty('comments');
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
function compileSexp(exp: any, env: any, options: any = {}): any {
  return compileRose(makeRose(exp), env, options);
}

compileSexp.lispSource = [Symbol.for('define'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), [Symbol.for('make-rose'), Symbol.for('exp')], [Symbol.for('compile-rose'), Symbol.for('_'), Symbol.for('env'), Symbol.for('options')]]];

/**
 * Compile `node` as an expression.
 */
function compileExpression(node: any, env: any, options: any = {}): any {
  return compileRose(node, env, makeExpressionOptions(options));
}

compileExpression.lispSource = [Symbol.for('define'), [Symbol.for('compile-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]]];

/**
 * Compile `node` as a regular statement.
 */
function compileStatement(node: any, env: any, options: any = {}): any {
  return compileRose(node, env, makeStatementOptions(options));
}

compileStatement.lispSource = [Symbol.for('define'), [Symbol.for('compile-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('make-statement-options'), Symbol.for('options')]]];

/**
 * Compile `node` as a return statement.
 */
function compileReturnStatement(node: any, env: any, options: any = {}): any {
  return compileRose(node, env, makeReturnStatementOptions(options));
}

compileReturnStatement.lispSource = [Symbol.for('define'), [Symbol.for('compile-return-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('make-return-statement-options'), Symbol.for('options')]]];

/**
 * Compile `node` as a regular statement or as a return statement,
 * depending on the value of the `expressionType` option.
 */
function compileStatementOrReturnStatement(node: any, env: any, options: any = {}): any {
  if (options['expressionType'] === 'return') {
    return compileReturnStatement(node, env, options);
  } else {
    return compileStatement(node, env, options);
  }
}

compileStatementOrReturnStatement.lispSource = [Symbol.for('define'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType'], 'return'], [Symbol.for('compile-return-statement'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-statement'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Helper function for compiling a list of statements.
 * The last statement is compiled as a `return` statement
 * if the `expressionType` option is `"return"`.
 */
function compileStatements(statements: any, env: any, options: any): any {
  const expressionType: any = options['expressionType'];
  let result: any = [];
  let returnIdx: any = -1;
  if (expressionType === 'return') {
    const _start: any = statements.length - 1;
    for (let i: any = _start; i > -1; i--) {
      const statement: any = (statements as any)[i];
      if (!(formp(statement, break_, env) || formp(statement, continue_, env) || formp(statement, yield_, env))) {
        returnIdx = i;
        break;
      }
    }
  }
  const _end: any = statements.length;
  for (let i: any = 0; i < _end; i++) {
    const statement: any = (statements as any)[i];
    if (i === returnIdx) {
      result.push(compileReturnStatement(statement, env, options));
    } else {
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
const interpret: any = dashify(function (exp: any, env: any = defaultEnvironment(), options: any = {}): any {
  const evaluator: any = options['evaluator'] || eval_ || defaultEvaluator;
  const environment: any = makeInterpretationEnvironment(env, options);
  return callEvaluator(evaluator, exp, environment, options);
});

/**
 * Interpret a string of Lisp code.
 */
function interpretString(str: any, env: any = undefined, options: any = {}): any {
  return interpret(readSexp(str), env, options);
}

interpretString.lispSource = [Symbol.for('define'), [Symbol.for('interpret-string'), Symbol.for('str'), [Symbol.for('env'), Symbol.for('undefined')], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('interpret'), [Symbol.for('read-sexp'), Symbol.for('str')], Symbol.for('env'), Symbol.for('options')]];

/**
 * Interpret a list of files.
 */
function interpretFiles(files: any, env: any = undefined, options: any = {}): any {
  return files.map(function (file: any): any {
    const str: any = '(begin\n' +
      readFileSync(file, {
        encoding: 'utf8'
      }).replace(new RegExp('^#!.*'), '') + '\n' +
      ')';
    let result: any = interpretString(str, env, options);
    return result;
  });
}

interpretFiles.lispSource = [Symbol.for('define'), [Symbol.for('interpret-files'), Symbol.for('files'), [Symbol.for('env'), Symbol.for('undefined')], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('file')], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('~>'), Symbol.for('file'), [Symbol.for('readFileSync'), Symbol.for('_'), [Symbol.for('js-obj'), 'encoding', 'utf8']], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^#!.*'], Symbol.for('_'), ''], [Symbol.for('string-append'), '(begin\n', Symbol.for('_'), '\n' +
  ')']]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('interpret-string'), Symbol.for('str'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('result')], Symbol.for('files')]];

/**
 * Interpret a string of Lisp code.
 * Alias for `interpret-string`.
 */
function lisp(str: any, env: any = undefined): any {
  return interpretString(str, env);
}

lisp.lispSource = [Symbol.for('define'), [Symbol.for('lisp'), Symbol.for('str'), [Symbol.for('env'), Symbol.for('undefined')]], [Symbol.for('interpret-string'), Symbol.for('str'), Symbol.for('env')]];

/**
 * Make a Lisp environment.
 */
function makeLisp(variables: any = [], isLisp2: any = false): any {
  return new LispEnvironment(variables, lispEnvironment);
}

makeLisp.lispSource = [Symbol.for('define'), [Symbol.for('make-lisp'), [Symbol.for('variables'), [Symbol.for('quote'), []]], [Symbol.for('is-lisp-2'), Symbol.for('#f')]], [Symbol.for('new'), Symbol.for('LispEnvironment'), Symbol.for('variables'), Symbol.for('lisp-environment')]];

/**
 * Make a Lisp interpretation environment.
 */
function makeInterpretationEnvironment(env: any, options: any = {}): any {
  let evalOption: any = options['eval'];
  // TODO: Make `#f` the default.
  if (evalOption === undefined) {
    evalOption = true;
  }
  if ((env === langEnvironment) || ((env instanceof EnvironmentStack) && env.hasEnvironment(langEnvironment))) {
    return env;
  } else {
    return new EnvironmentStack(env, evalOption ? interpretationEnvironment : interpretationEnvironmentNoEval);
  }
}

makeInterpretationEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('make-interpretation-environment'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('eval-option'), [Symbol.for('oget'), Symbol.for('options'), 'eval']], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('eval-option'), Symbol.for('undefined')], [Symbol.for('set!'), Symbol.for('eval-option'), Symbol.for('#t')]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('env'), Symbol.for('lang-environment')], [Symbol.for('and'), [Symbol.for('is-a?'), Symbol.for('env'), Symbol.for('EnvironmentStack')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('has-environment'), Symbol.for('lang-environment')]]], Symbol.for('env')], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('env'), [Symbol.for('if'), Symbol.for('eval-option'), Symbol.for('interpretation-environment'), Symbol.for('interpretation-environment-no-eval')]]]]];

/**
 * Make an environment suitable for expanding macros
 * and compiler macros.
 */
function makeMacroEnvironment(env: any): any {
  return new EnvironmentStack(new EnvironmentPipe(env, compilationMacroMappingEnv), env);
}

makeMacroEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('make-macro-environment'), Symbol.for('env')], [Symbol.for('new'), Symbol.for('EnvironmentStack'), [Symbol.for('new'), Symbol.for('EnvironmentPipe'), Symbol.for('env'), Symbol.for('compilation-macro-mapping-env')], Symbol.for('env')]];

/**
 * Make compilation options for compiling a form as
 * an expression.
 */
function makeExpressionOptions(options: any): any {
  return {
    ...options,
    expressionType: 'expression'
  };
}

makeExpressionOptions.lispSource = [Symbol.for('define'), [Symbol.for('make-expression-options'), Symbol.for('options')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'expression']]];

/**
 * Make compilation options for compiling a form as
 * a statement.
 */
function makeStatementOptions(options: any): any {
  return {
    ...options,
    expressionType: 'statement'
  };
}

makeStatementOptions.lispSource = [Symbol.for('define'), [Symbol.for('make-statement-options'), Symbol.for('options')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'statement']]];

/**
 * Make compilation options for compiling a form as
 * a return statement.
 */
function makeReturnStatementOptions(options: any): any {
  return {
    ...options,
    expressionType: 'return'
  };
}

makeReturnStatementOptions.lispSource = [Symbol.for('define'), [Symbol.for('make-return-statement-options'), Symbol.for('options')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'return']]];

/**
 * Make an expression or statement ESTree node,
 * conditional on options.
 */
function makeExpressionOrStatement(node: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    return wrapExpressionInStatement(node, options);
  } else {
    return node;
  }
}

makeExpressionOrStatement.lispSource = [Symbol.for('define'), [Symbol.for('make-expression-or-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('wrap-expression-in-statement'), Symbol.for('node'), Symbol.for('options')]], [Symbol.for('else'), Symbol.for('node')]]];

/**
 * Wrap an expression in a statement. An `ExpressionStatement`
 * or `ReturnStatement` node is returned, conditional on options.
 */
function wrapExpressionInStatement(node: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  if (!(node instanceof Expression)) {
    return node;
  } else if (expressionType === 'return') {
    return new ReturnStatement(node);
  } else {
    return new ExpressionStatement(node);
  }
}

wrapExpressionInStatement.lispSource = [Symbol.for('define'), [Symbol.for('wrap-expression-in-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Expression')]], Symbol.for('node')], [[Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('new'), Symbol.for('ReturnStatement'), Symbol.for('node')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ExpressionStatement'), Symbol.for('node')]]]];

/**
 * Wraps `node` in a `BlockStatement`.
 */
function wrapInBlockStatement(obj: any): any {
  return makeBlockStatement([obj]);
}

wrapInBlockStatement.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-block-statement'), Symbol.for('obj')], [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('obj')]]];

/**
 * Wraps `node` in a `BlockStatement` unless `node` already is
 * a `BlockStatement`. In other words, avoids double wrapping.
 */
function wrapInBlockStatementSmart(node: any): any {
  if (estreeTypeP(node, 'BlockStatement')) {
    return node;
  } else {
    return makeBlockStatement([node]);
  }
}

wrapInBlockStatementSmart.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-block-statement-smart'), Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('node'), 'BlockStatement'], Symbol.for('node')], [Symbol.for('else'), [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('node')]]]]];

/**
 * Wrap `exp` in a `lambda` call.
 */
function wrapInLambdaCall(exp: any): any {
  return makeRose([[Symbol.for('lambda'), [], exp]]);
}

wrapInLambdaCall.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-lambda-call'), Symbol.for('exp')], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [], [Symbol.for('unquote'), Symbol.for('exp')]]]]]];

/**
 * Wrap `exp` in a `js/arrow` call.
 */
function wrapInArrowCall(exp: any): any {
  return makeRose([[Symbol.for('js/arrow'), [], exp]]);
}

wrapInArrowCall.lispSource = [Symbol.for('define'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('js/arrow'), [], [Symbol.for('unquote'), Symbol.for('exp')]]]]]];

/**
 * Make a `BlockStatement`.
 * Handles `Program` fragments.
 */
function makeBlockStatement(body: any): any {
  if (Array.isArray(body)) {
    return new BlockStatement(makeBlockStatementHelper(body));
  } else {
    return makeBlockStatement([body]);
  }
}

makeBlockStatement.lispSource = [Symbol.for('define'), [Symbol.for('make-block-statement'), Symbol.for('body')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('body')], [Symbol.for('new'), Symbol.for('BlockStatement'), [Symbol.for('make-block-statement-helper'), Symbol.for('body')]]], [Symbol.for('else'), [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('body')]]]]];

/**
 * Helper function for `make-block-statement`.
 */
function makeBlockStatementHelper(body: any): any {
  let statements: any = [];
  for (let statement of body) {
    if (estreeTypeP(statement, 'Program')) {
      // Program fragments are represented with `Program`.
      // Their contents are spliced into the block statement.
      const fragment: any = statement;
      const fragmentStatements: any = fragment.body;
      const fragmentComments: any = fragment.comments;
      if (fragmentStatements.length > 0) {
        transferComments(fragment, fragmentStatements[0]);
        statements = [...statements, ...fragmentStatements];
      } else if (fragmentComments.length > 0) {
        statements.push(statement);
      }
    } else {
      statements.push(statement);
    }
  }
  return statements;
}

makeBlockStatementHelper.lispSource = [Symbol.for('define'), [Symbol.for('make-block-statement-helper'), Symbol.for('body')], [Symbol.for('define'), Symbol.for('statements'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('statement'), Symbol.for('body')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('statement'), 'Program'], [Symbol.for('define'), Symbol.for('fragment'), Symbol.for('statement')], [Symbol.for('define'), Symbol.for('fragment-statements'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('fragment')]], [Symbol.for('define'), Symbol.for('fragment-comments'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('fragment')]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('fragment-statements')], 0], [Symbol.for('transfer-comments'), Symbol.for('fragment'), [Symbol.for('first'), Symbol.for('fragment-statements')]], [Symbol.for('set!'), Symbol.for('statements'), [Symbol.for('append'), Symbol.for('statements'), Symbol.for('fragment-statements')]]], [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('fragment-comments')], 0], [Symbol.for('push-right!'), Symbol.for('statements'), Symbol.for('statement')]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('statements'), Symbol.for('statement')]]]], Symbol.for('statements')];

/**
 * Make a `Program`.
 */
function makeProgram(body: any): any {
  return new Program(makeBlockStatementHelper(body));
}

makeProgram.lispSource = [Symbol.for('define'), [Symbol.for('make-program'), Symbol.for('body')], [Symbol.for('new'), Symbol.for('Program'), [Symbol.for('make-block-statement-helper'), Symbol.for('body')]]];

/**
 * Make a `Program` fragment (i.e., a program that
 * is to be spliced into the containing program).
 */
function makeProgramFragment(body: any = []): any {
  // `Program` is used to represent programs
  // and program fragments.
  return makeProgram(body);
}

makeProgramFragment.lispSource = [Symbol.for('define'), [Symbol.for('make-program-fragment'), [Symbol.for('body'), [Symbol.for('quote'), []]]], [Symbol.for('make-program'), Symbol.for('body')]];

/**
 * Make an empty program fragment.
 */
function emptyProgram(): any {
  return makeProgramFragment();
}

emptyProgram.lispSource = [Symbol.for('define'), [Symbol.for('empty-program')], [Symbol.for('make-program-fragment')]];

/**
 * Unwrap a `BlockStatement`, i.e., return the expression it
 * contains. The statement is assumed to contain a single
 * expression.
 */
function unwrapBlockStatement(exp: any): any {
  if (!estreeTypeP(exp, 'BlockStatement')) {
    return exp;
  }
  let unwrappedExp: any = exp;
  while ((unwrappedExp.body.length === 1) && estreeTypeP(unwrappedExp.body[0], 'BlockStatement')) {
    unwrappedExp = unwrappedExp.body[0];
  }
  return unwrappedExp;
}

unwrapBlockStatement.lispSource = [Symbol.for('define'), [Symbol.for('unwrap-block-statement'), Symbol.for('exp')], [Symbol.for('unless'), [Symbol.for('estree-type?'), Symbol.for('exp'), 'BlockStatement'], [Symbol.for('return'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('unwrapped-exp'), Symbol.for('exp')], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('unwrapped-exp')]], 1], [Symbol.for('estree-type?'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('unwrapped-exp')]], 'BlockStatement']], [Symbol.for('set!'), Symbol.for('unwrapped-exp'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('unwrapped-exp')]]]], Symbol.for('unwrapped-exp')];

/**
 * Remove the comment prefix (`; `, `;; `, `;;; `, etc.)
 * from a comment string.
 */
function removeCommentPrefix(comment: any): any {
  return comment.replace(new RegExp('^[^\\S\\r\\n]*[;]+[^\\S\\r\\n]?', 'gm'), '');
}

removeCommentPrefix.lispSource = [Symbol.for('define'), [Symbol.for('remove-comment-prefix'), Symbol.for('comment')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^[^\\S\\r\\n]*[;]+[^\\S\\r\\n]?', 'gm'], Symbol.for('comment'), '']];

/**
 * Transfer the `comments` property from ESTree `node1` to ESTree `node2`,
 * compiling them in the process.
 */
function transferAndCompileComments(node1: any, node2: any, options: any = {}): any {
  const commentsOption: any = options['comments'];
  let comments: any = (node1 instanceof Rose) ? node1.getProperty('comments') : node1.comments;
  if (commentsOption && comments) {
    if (node2 instanceof Rose) {
      node2.setProperty('comments', [...comments, ...(node2.getProperty('comments') || [])]);
    } else {
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
function compileComments(comments: any): any {
  const commentsCompiled: any = [];
  for (let comment of comments) {
    if (comment instanceof LeadingCommentToken) {
      const subcomments: any = splitComments(comment.value);
      for (let subcomment of subcomments) {
        if (getCommentLevel(subcomment) >= 3) {
          commentsCompiled.push(new BlockComment(removeCommentPrefix(subcomment)));
        } else {
          commentsCompiled.push(new LeadingComment(removeCommentPrefix(subcomment)));
        }
      }
    } else if (comment instanceof TrailingCommentToken) {
      commentsCompiled.push(new TrailingComment(removeCommentPrefix(comment.value)));
    }
  }
  return commentsCompiled;
}

compileComments.lispSource = [Symbol.for('define'), [Symbol.for('compile-comments'), Symbol.for('comments')], [Symbol.for('define'), Symbol.for('comments-compiled'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('comment'), Symbol.for('comments')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('LeadingCommentToken')], [Symbol.for('define'), Symbol.for('subcomments'), [Symbol.for('split-comments'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('comment')]]], [Symbol.for('for'), [[Symbol.for('subcomment'), Symbol.for('subcomments')]], [Symbol.for('cond'), [[Symbol.for('>='), [Symbol.for('get-comment-level'), Symbol.for('subcomment')], 3], [Symbol.for('push-right!'), Symbol.for('comments-compiled'), [Symbol.for('new'), Symbol.for('BlockComment'), [Symbol.for('remove-comment-prefix'), Symbol.for('subcomment')]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('comments-compiled'), [Symbol.for('new'), Symbol.for('LeadingComment'), [Symbol.for('remove-comment-prefix'), Symbol.for('subcomment')]]]]]]], [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('TrailingCommentToken')], [Symbol.for('push-right!'), Symbol.for('comments-compiled'), [Symbol.for('new'), Symbol.for('TrailingComment'), [Symbol.for('remove-comment-prefix'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('comment')]]]]]]], Symbol.for('comments-compiled')];

/**
 * Split up a string containing multiple comments.
 */
function splitComments(str: any): any {
  let comments: any = [];
  let comment: any = '';
  let currentLevel: any = -1;
  let lines: any = str.split('\n');
  if (str.match(new RegExp('\\n$'))) {
    lines = lines.slice(0, -1);
  }
  for (let x of lines) {
    if (x === '') {
      if (comment.match(new RegExp('\\n$'))) {
        comment = comment + '\n';
        comments.push(comment);
        comment = '';
      } else {
        comment = comment + '\n';
      }
    } else {
      const level: any = getCommentLevel(x);
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

splitComments.lispSource = [Symbol.for('define'), [Symbol.for('split-comments'), Symbol.for('str')], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('comment'), ''], [Symbol.for('define'), Symbol.for('current-level'), -1], [Symbol.for('define'), Symbol.for('lines'), [Symbol.for('string-split'), Symbol.for('str'), '\n']], [Symbol.for('when'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], Symbol.for('str')], [Symbol.for('set!'), Symbol.for('lines'), [Symbol.for('drop-right'), Symbol.for('lines'), 1]]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('lines')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('x'), ''], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], Symbol.for('comment')], [Symbol.for('set!'), Symbol.for('comment'), [Symbol.for('string-append'), Symbol.for('comment'), '\n']], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('comment')], [Symbol.for('set!'), Symbol.for('comment'), '']], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('comment'), [Symbol.for('string-append'), Symbol.for('comment'), '\n']]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('level'), [Symbol.for('get-comment-level'), Symbol.for('x')]], [Symbol.for('unless'), [Symbol.for('='), Symbol.for('level'), Symbol.for('current-level')], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('comment'), ''], [Symbol.for('eq?'), Symbol.for('comment'), '\n']], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('comment')], [Symbol.for('set!'), Symbol.for('comment'), '']], [Symbol.for('set!'), Symbol.for('current-level'), Symbol.for('level')]], [Symbol.for('set!'), Symbol.for('comment'), [Symbol.for('string-append'), Symbol.for('comment'), Symbol.for('x'), '\n']]]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('comment'), ''], [Symbol.for('eq?'), Symbol.for('comment'), '\n']], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('comment')]], Symbol.for('comments')];

/**
 * Whether `exp` is a function call, given `env`.
 */
function functionCallP(exp: any, env: any): any {
  if (exp instanceof Rose) {
    return macroCallP(exp.getValue(), env);
  } else {
    return Array.isArray(exp) && (exp.length > 1) && (typeof exp[0] === 'symbol') && ['function', 'procedure'].includes(env.getType(exp[0]));
  }
}

functionCallP.lispSource = [Symbol.for('define'), [Symbol.for('function-call?'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('macro-call?'), [Symbol.for('send'), Symbol.for('exp'), Symbol.for('get-value')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('symbol?'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('memq?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('quote'), ['function', 'procedure']]]]]]];

/**
 * Whether `exp` is a macro call, given `env`.
 */
function macroCallP(exp: any, env: any): any {
  if (exp instanceof Rose) {
    return macroCallP(exp.getValue(), env);
  } else {
    return Array.isArray(exp) && (exp.length > 1) && (typeof exp[0] === 'symbol') && (env.getType(exp[0]) === 'macro');
  }
}

macroCallP.lispSource = [Symbol.for('define'), [Symbol.for('macro-call?'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('macro-call?'), [Symbol.for('send'), Symbol.for('exp'), Symbol.for('get-value')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('symbol?'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), [Symbol.for('first'), Symbol.for('exp')]], 'macro']]]]];

/**
 * Whether `exp` is a special form, given `env`.
 */
function specialFormP(exp: any, env: any): any {
  if (exp instanceof Rose) {
    return macroCallP(exp.getValue(), env);
  } else {
    return Array.isArray(exp) && (exp.length > 1) && (typeof exp[0] === 'symbol') && (env.getType(exp[0]) === 'special');
  }
}

specialFormP.lispSource = [Symbol.for('define'), [Symbol.for('special-form?'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('macro-call?'), [Symbol.for('send'), Symbol.for('exp'), Symbol.for('get-value')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('symbol?'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), [Symbol.for('first'), Symbol.for('exp')]], 'special']]]]];

/**
 * Convert a `(define (...) ...)` form to
 * a `(lambda (...) ...)` form.
 */
function defineToLambda(node: any, options: any = {}): any {
  const curriedOption: any = options['curried'];
  let exp: any = node.getValue();
  const nameAndParams: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let name: any = nameAndParams[0];
  let params: any = cdr(nameAndParams);
  const shouldCurry: any = curriedOption || ((curriedOption === undefined) && Array.isArray(name));
  if (shouldCurry) {
    params = flatten(nameAndParams).slice(1);
    if (Array.isArray(nameAndParams) && (nameAndParams.length >= 3) && (nameAndParams[nameAndParams.length - 2] === Symbol.for('.')) && !((): any => {
      const x: any = lastCdr(nameAndParams);
      return Array.isArray(x) && (x.length === 0);
    })() && (params.length === 1)) {
      params = params[0];
    }
  }
  return makeRose([Symbol.for('lambda'), params, ...node.drop(2)]);
}

defineToLambda.lispSource = [Symbol.for('define'), [Symbol.for('define->lambda'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('curried-option'), [Symbol.for('oget'), Symbol.for('options'), 'curried']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('name-and-params'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-params')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('cdr'), Symbol.for('name-and-params')]], [Symbol.for('define'), Symbol.for('should-curry'), [Symbol.for('or'), Symbol.for('curried-option'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('curried-option'), Symbol.for('undefined')], [Symbol.for('array?'), Symbol.for('name')]]]], [Symbol.for('when'), Symbol.for('should-curry'), [Symbol.for('set!'), Symbol.for('params'), [Symbol.for('rest'), [Symbol.for('flatten'), Symbol.for('name-and-params')]]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('dotted-list?'), Symbol.for('name-and-params')], [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('params')], 1]], [Symbol.for('set!'), Symbol.for('params'), [Symbol.for('first'), Symbol.for('params')]]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('unquote'), Symbol.for('params')], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]]]]];

/**
 * Convert a function to a macro on the basis
 * of its `(define ...)` form.
 */
function definitionToMacro(exp: any, args: any): any {
  // TODO: Rest arguments.
  //
  // FIXME: When a complex argument is referenced inside of a `lambda`
  // expression, we should store the value in a local variable.
  const paramsList: any = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1]).slice(1);
  let params: any = paramsList.map(function (x: any): any {
    if (Array.isArray(x)) {
      return x[0];
    } else {
      return x;
    }
  });
  const body: any = exp.slice(2);
  if (params.length === 0) {
    if (body.length === 1) {
      return body[0];
    } else {
      return [Symbol.for('begin'), ...body];
    }
  } else {
    const counts: any = buildList(args.length, function (...args: any[]): any {
      return 0;
    });
    const shouldMakeLambda: any = false;
    let shouldMakeLet: any = false;
    let result: any = body.map(function (x: any): any {
      return mapTree(function (y: any): any {
        const idx: any = params.findIndex(function (z: any): any {
          return z === y;
        });
        if (idx >= 0) {
          (counts as any)[idx] = (counts as any)[idx] + 1;
          if (idx < args.length) {
            return (args as any)[idx];
          } else {
            const currentParam: any = (paramsList as any)[idx];
            if (Array.isArray(currentParam)) {
              if (Array.isArray(currentParam) && (currentParam.length >= 3) && (currentParam[currentParam.length - 2] === Symbol.for('.')) && ((): any => {
                let x1: any = lastCdr(currentParam);
                return Array.isArray(x1) && (x1.length === 0);
              })()) {
                let i: any = 1;
                let result: any = currentParam;
                while (i > 0) {
                  if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = currentParam[currentParam.length - 1];
                  } else {
                    result = currentParam.slice(1);
                  }
                  i--;
                }
                if (Array.isArray(result)) {
                  result = result[0];
                }
                return result;
              } else {
                return currentParam[1];
              }
            } else {
              return undefined;
            }
          }
        } else {
          return y;
        }
      }, x);
    });
    // Determine whether a complex argument is referenced
    // more than once. If so, we need to make a `lambda`
    // expression instead.
    const _end: any = args.length;
    for (let i: any = 0; i < _end; i++) {
      const count: any = (counts as any)[i];
      const arg: any = (args as any)[i];
      if ((count > 1) && !((typeof arg === 'symbol') || (typeof arg === 'boolean') || (typeof arg === 'string') || Number.isFinite(arg))) {
        // (set! should-make-lambda #t)
        shouldMakeLet = true;
        break;
      }
    }
    if (shouldMakeLet) {
      const letBindings: any = [];
      let gensymMap: any = new Map();
      const _end: any = params.length;
      for (let i: any = 0; i < _end; i++) {
        const argExp: any = (i < args.length) ? (args as any)[i] : ((): any => {
          const currentParam: any = (paramsList as any)[i];
          if (Array.isArray(currentParam)) {
            if (Array.isArray(currentParam) && (currentParam.length >= 3) && (currentParam[currentParam.length - 2] === Symbol.for('.')) && ((): any => {
              const x: any = lastCdr(currentParam);
              return Array.isArray(x) && (x.length === 0);
            })()) {
              let i: any = 1;
              let result: any = currentParam;
              while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                  result = currentParam[currentParam.length - 1];
                } else {
                  result = currentParam.slice(1);
                }
                i--;
              }
              if (Array.isArray(result)) {
                result = result[0];
              }
              return result;
            } else {
              return currentParam[1];
            }
          } else {
            return undefined;
          }
        })();
        const paramExp: any = (params as any)[i];
        let param: any = Array.isArray(paramExp) ? paramExp[0] : paramExp;
        if (typeof argExp === 'symbol') {
          gensymMap.set(param, argExp);
        } else {
          const paramGensym: any = Symbol(param.description as string);
          gensymMap.set(param, paramGensym);
          letBindings.push([paramGensym, argExp]);
        }
      }
      const letBody: any = mapTree(function (x: any): any {
        if (gensymMap.has(x)) {
          return gensymMap.get(x);
        } else {
          return x;
        }
      }, body);
      return [Symbol.for('let*'), letBindings, ...letBody];
    } else if (shouldMakeLambda) {
      return [[Symbol.for('lambda'), params, ...body], ...args];
    } else {
      if (result.length === 1) {
        return result[0];
      } else {
        return [Symbol.for('begin'), ...result];
      }
    }
  }
}

definitionToMacro.lispSource = [Symbol.for('define'), [Symbol.for('definition->macro'), Symbol.for('exp'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('params-list'), [Symbol.for('rest'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('first'), Symbol.for('x')], Symbol.for('x')]], Symbol.for('params-list')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('params')], 0], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('body')], 1], [Symbol.for('first'), Symbol.for('body')]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('counts'), [Symbol.for('build-list'), [Symbol.for('array-list-length'), Symbol.for('args')], [Symbol.for('const'), 0]]], [Symbol.for('define'), Symbol.for('should-make-lambda'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('should-make-let'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('y')], [Symbol.for('define'), Symbol.for('idx'), [Symbol.for('js/find-index'), [Symbol.for('lambda'), [Symbol.for('z')], [Symbol.for('eq?'), Symbol.for('z'), Symbol.for('y')]], Symbol.for('params')]], [Symbol.for('cond'), [[Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('list-set!'), Symbol.for('counts'), Symbol.for('idx'), [Symbol.for('+'), [Symbol.for('aget'), Symbol.for('counts'), Symbol.for('idx')], 1]], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('idx'), [Symbol.for('array-list-length'), Symbol.for('args')]], [Symbol.for('aget'), Symbol.for('args'), Symbol.for('idx')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('current-param'), [Symbol.for('aget'), Symbol.for('params-list'), Symbol.for('idx')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('current-param')], [Symbol.for('second'), Symbol.for('current-param')]], [Symbol.for('else'), Symbol.for('undefined')]]]]], [Symbol.for('else'), Symbol.for('y')]]], Symbol.for('x')]], Symbol.for('body')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('define'), Symbol.for('count'), [Symbol.for('aget'), Symbol.for('counts'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('>'), Symbol.for('count'), 1], [Symbol.for('not'), [Symbol.for('or'), [Symbol.for('symbol?'), Symbol.for('arg')], [Symbol.for('boolean?'), Symbol.for('arg')], [Symbol.for('string?'), Symbol.for('arg')], [Symbol.for('number?'), Symbol.for('arg')]]]], [Symbol.for('set!'), Symbol.for('should-make-let'), Symbol.for('#t')], [Symbol.for('break')]]], [Symbol.for('cond'), [Symbol.for('should-make-let'), [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('gensym-map'), [Symbol.for('make-hash')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('params')]]]], [Symbol.for('define'), Symbol.for('arg-exp'), [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), [Symbol.for('array-list-length'), Symbol.for('args')]], [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('current-param'), [Symbol.for('aget'), Symbol.for('params-list'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('current-param')], [Symbol.for('second'), Symbol.for('current-param')]], [Symbol.for('else'), Symbol.for('undefined')]]]]], [Symbol.for('define'), Symbol.for('param-exp'), [Symbol.for('aget'), Symbol.for('params'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('param'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('param-exp')], [Symbol.for('first'), Symbol.for('param-exp')], Symbol.for('param-exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('arg-exp')], [Symbol.for('hash-set!'), Symbol.for('gensym-map'), Symbol.for('param'), Symbol.for('arg-exp')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('param-gensym'), [Symbol.for('gensym'), [Symbol.for('symbol->string'), Symbol.for('param')]]], [Symbol.for('hash-set!'), Symbol.for('gensym-map'), Symbol.for('param'), Symbol.for('param-gensym')], [Symbol.for('push-right!'), Symbol.for('let-bindings'), [Symbol.for('list'), Symbol.for('param-gensym'), Symbol.for('arg-exp')]]]]], [Symbol.for('define'), Symbol.for('let-body'), [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('gensym-map'), Symbol.for('x')], [Symbol.for('hash-ref'), Symbol.for('gensym-map'), Symbol.for('x')]], [Symbol.for('else'), Symbol.for('x')]]], Symbol.for('body')]], [Symbol.for('quasiquote'), [Symbol.for('let*'), [Symbol.for('unquote'), Symbol.for('let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('let-body')]]]], [Symbol.for('should-make-lambda'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('unquote'), Symbol.for('params')], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('unquote-splicing'), Symbol.for('args')]]]], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('result')], 1], [Symbol.for('first'), Symbol.for('result')]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('result')]]]]]]]]]];

/**
 * Create a `(lambda ...)` form for a macro function
 * on the basis of a `(define-macro ...)` expression.
 */
function defineMacroToLambdaForm(exp: any): any {
  const nameAndArgs: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let name: any = nameAndArgs[0];
  const args: any = cdr(nameAndArgs);
  const body: any = exp.slice(2);
  let expArg: any = Symbol.for('exp');
  let envArg: any = Symbol.for('env');
  let macroArgs: any = [];
  let restArg: any = undefined;
  if (((): any => {
    const x: any = lastCdr(args);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    let i: any = 0;
    while (i < args.length) {
      const arg: any = (args as any)[i];
      if (arg === Symbol.for('&rest')) {
        restArg = args[i + 1];
        i = i + 2;
      } else if (arg === Symbol.for('&whole')) {
        expArg = args[i + 1];
        i = i + 2;
      } else if (arg === Symbol.for('&environment')) {
        envArg = args[i + 1];
        i = i + 2;
      } else {
        macroArgs.push(arg);
        i++;
      }
    }
  } else {
    macroArgs = args;
  }
  if (restArg) {
    if (Array.isArray(macroArgs) && (macroArgs.length === 0)) {
      macroArgs = restArg;
    } else {
      macroArgs = listStar(...[...macroArgs, restArg]);
    }
  }
  return [Symbol.for('lambda'), [expArg, envArg], ...((Array.isArray(macroArgs) && (macroArgs.length === 0)) ? [] : [[Symbol.for('define-values'), macroArgs, [Symbol.for('rest'), expArg]]]), ...body];
}

defineMacroToLambdaForm.lispSource = [Symbol.for('define'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('cdr'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('exp-arg'), [Symbol.for('quote'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('env-arg'), [Symbol.for('quote'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('macro-args'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-arg'), Symbol.for('undefined')], [Symbol.for('cond'), [[Symbol.for('list?'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('while'), [Symbol.for('<'), Symbol.for('i'), [Symbol.for('array-list-length'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&rest')]], [Symbol.for('set!'), Symbol.for('rest-arg'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&whole')]], [Symbol.for('set!'), Symbol.for('exp-arg'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&environment')]], [Symbol.for('set!'), Symbol.for('env-arg'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('macro-args'), Symbol.for('arg')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('args')]]], [Symbol.for('when'), Symbol.for('rest-arg'), [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('rest-arg')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), [Symbol.for('apply'), Symbol.for('list*'), [Symbol.for('append'), Symbol.for('macro-args'), [Symbol.for('list'), Symbol.for('rest-arg')]]]]]]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [[Symbol.for('unquote'), Symbol.for('exp-arg')], [Symbol.for('unquote'), Symbol.for('env-arg')]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('quote'), []], [Symbol.for('quasiquote'), [[Symbol.for('define-values'), [Symbol.for('unquote'), Symbol.for('macro-args')], [Symbol.for('rest'), [Symbol.for('unquote'), Symbol.for('exp-arg')]]]]]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

/**
 * Create a macro function on the basis of a
 * `(define-macro ...)` expression.
 */
function defineMacroToFunction(exp: any, env: any): any {
  const macroFn: any = defineMacroToLambdaForm(exp);
  return eval_(macroFn, env);
}

defineMacroToFunction.lispSource = [Symbol.for('define'), [Symbol.for('define-macro->function'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('macro-fn'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')]], [Symbol.for('eval_'), Symbol.for('macro-fn'), Symbol.for('env')]];

/**
 * Convert a `(define ... (class ...))` expression to
 * a `(define-class ...)` expression.
 */
function defineToDefineClass(node: any): any {
  if (node instanceof Rose) {
    const superclass: any = node.get(2).get(1);
    const superclassExp: any = superclass.getValue();
    const superclassList: any = [Symbol.for('object%'), Symbol.for('object'), Symbol.for('Object')].includes(superclassExp) ? [] : [superclass];
    return transferComments(node, makeRose([Symbol.for('define-class'), node.get(1), makeRose(superclassList), ...node.get(2).drop(2)]));
  } else {
    return defineToDefineClass(makeRose(node)).getValue();
  }
}

defineToDefineClass.lispSource = [Symbol.for('define'), [Symbol.for('define->define-class'), Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('define'), Symbol.for('superclass'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('superclass-exp'), [Symbol.for('send'), Symbol.for('superclass'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('superclass-list'), [Symbol.for('if'), [Symbol.for('memq?'), Symbol.for('superclass-exp'), [Symbol.for('quote'), [Symbol.for('object%'), Symbol.for('object'), Symbol.for('Object')]]], [Symbol.for('quote'), []], [Symbol.for('list'), Symbol.for('superclass')]]], [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-class'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('make-rose'), Symbol.for('superclass-list')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('drop'), 2]]]]]]], [Symbol.for('else'), [Symbol.for('send'), [Symbol.for('define->define-class'), [Symbol.for('make-rose'), Symbol.for('node')]], Symbol.for('get-value')]]]];

/**
 * Wrap `f-exp` in a unary function wrapper.
 */
function compileMapMacroHelper(fExp: any, env: any): any {
  if (typeof fExp === 'symbol') {
    // If `f-exp` is a symbolic expression, then wrap it in a
    // `lambda` expression.
    return [Symbol.for('lambda'), [Symbol.for('x')], [fExp, Symbol.for('x')]];
  } else if ((formp(fExp, lambda_, env) || formp(fExp, jsFunction_, env) || formp(fExp, jsArrow_, env)) && Array.isArray((Array.isArray(fExp) && (fExp.length >= 3) && (fExp[fExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(fExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = fExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = fExp[fExp.length - 1];
      } else {
        result = fExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : fExp[1]) && (((Array.isArray(fExp) && (fExp.length >= 3) && (fExp[fExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(fExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = fExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = fExp[fExp.length - 1];
      } else {
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
  } else {
    // Curried function application, i.e., the **A** combinator
    // defined as a curried function. Calling this function with
    // a single argument produces a unary function wrapper that
    // calls a function with a single argument and disregards any
    // additional arguments.
    const AExp: any = [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]]];
    return [AExp, fExp];
  }
}

compileMapMacroHelper.lispSource = [Symbol.for('define'), [Symbol.for('compile-map-macro-helper'), Symbol.for('f-exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('f-exp')], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('x')], [[Symbol.for('unquote'), Symbol.for('f-exp')], Symbol.for('x')]]]], [[Symbol.for('and'), [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('f-exp'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('f-exp'), Symbol.for('js-function_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('f-exp'), Symbol.for('js-arrow_'), Symbol.for('env')]], [Symbol.for('array?'), [Symbol.for('second'), Symbol.for('f-exp')]], [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('second'), Symbol.for('f-exp')]], 1]], Symbol.for('f-exp')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('A-exp'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]]]]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('A-exp')], [Symbol.for('unquote'), Symbol.for('f-exp')]]]]]];

/**
 * Compile an `(and ...)` expression.
 */
function compileAnd(node: any, env: any, options: any = {}): any {
  return compileLogicalExpression(node, env, options, {
    identity: true,
    operator: '&&'
  });
}

compileAnd.lispSource = [Symbol.for('define'), [Symbol.for('compile-and'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-logical-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '&&']]];

/**
 * Compile an `(ann ...)` expression.
 */
function compileAnn(node: any, env: any, options: any = {}): any {
  const language: any = options['language'];
  const e_: any = node.get(1);
  if (language === 'TypeScript') {
    const t_: any = node.get(2);
    return makeExpressionOrStatement(new TSAsExpression(compileExpression(e_, env, options), compileType(t_, env, options)), options);
  } else {
    return compileRose(e_, env, options);
  }
}

compileAnn.lispSource = [Symbol.for('define'), [Symbol.for('compile-ann'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('e_'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('define'), Symbol.for('t_'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('TSAsExpression'), [Symbol.for('compile-expression'), Symbol.for('e_'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-type'), Symbol.for('t_'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-rose'), Symbol.for('e_'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(define-type ...)` expression.
 */
function compileDefineType(node: any, env: any, options: any = {}): any {
  const language: any = options['language'];
  if (language === 'TypeScript') {
    let id: any = compileExpression(node.get(1), env, options);
    let type_: any = compileType(node.get(2), env, options);
    return transferAndCompileComments(node, new TSTypeAliasDeclaration(id, type_), options);
  } else {
    return emptyProgram();
  }
}

compileDefineType.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-type'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('compile-type'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('transfer-and-compile-comments'), Symbol.for('node'), [Symbol.for('new'), Symbol.for('TSTypeAliasDeclaration'), Symbol.for('id'), Symbol.for('type_')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('empty-program')]]]];

/**
 * Compile a type expression.
 */
function compileType(node: any, env: any, options: any = {}): any {
  let exp: any = (node instanceof Rose) ? node.getValue() : node;
  return compileTypeExp(exp, env, options);
}

compileType.lispSource = [Symbol.for('define'), [Symbol.for('compile-type'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')], Symbol.for('node')]], [Symbol.for('compile-type-exp'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Helper function for `compile-type`.
 */
function compileTypeExp(exp: any, env: any, options: any = {}): any {
  if (typeof exp === 'symbol') {
    if (exp === Symbol.for('Any')) {
      return new TSAnyKeyword();
    } else if (exp === Symbol.for('Void')) {
      return new TSVoidKeyword();
    } else if (exp === Symbol.for('Undefined')) {
      return new TSUndefinedKeyword();
    } else if (exp === Symbol.for('Boolean')) {
      return new TSBooleanKeyword();
    } else if (exp === Symbol.for('True')) {
      return new TSLiteralType(new Literal(true));
    } else if (exp === Symbol.for('False')) {
      return new TSLiteralType(new Literal(false));
    } else if (exp === Symbol.for('Number')) {
      return new TSNumberKeyword();
    } else if (exp === Symbol.for('Integer')) {
      return new TSNumberKeyword();
    } else if (exp === Symbol.for('Natural')) {
      return new TSNumberKeyword();
    } else if (exp === Symbol.for('Real')) {
      return new TSNumberKeyword();
    } else if (exp === Symbol.for('String')) {
      return new TSStringKeyword();
    } else {
      return new TSTypeReference(new Identifier(exp.description as string));
    }
  } else if (taggedListP(exp, Symbol.for('List'))) {
    return new TSTupleType(exp.slice(1).map(function (x: any): any {
      return compileTypeExp(x, env, options);
    }));
  } else if (taggedListP(exp, Symbol.for('Listof'))) {
    return new TSArrayType(compileTypeExp((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1], env, options));
  } else if (taggedListP(exp, Symbol.for('Pairof'))) {
    return compileTypeExp([Symbol.for('Listof'), [Symbol.for('U'), (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1], Symbol.for('Symbol')]], env, options);
  } else if (taggedListP(exp, Symbol.for('U'))) {
    return new TSUnionType(exp.slice(1).map(function (x: any): any {
      return compileTypeExp(x, env, options);
    }));
  } else if (taggedListP(exp, Symbol.for('->')) || taggedListP(exp, Symbol.for('->*'))) {
    const mandatoryParams: any = taggedListP(exp, Symbol.for('->*')) ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1]) : exp.slice(0, -1).slice(1);
    const optionalParams: any = (taggedListP(exp, Symbol.for('->*')) && (exp.length > 3)) ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 2;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[2]) : [];
    const returnValue: any = exp[exp.length - 1];
    let i: any = 0;
    function compileParam(param: any, optional: any): any {
      const varName: any = numberToLetter(i);
      i++;
      return new Identifier(varName, optional).setType(compileTypeExp(param, env, options));
    }
    compileParam.lispSource = [Symbol.for('define'), [Symbol.for('compile-param'), Symbol.for('param'), Symbol.for('optional')], [Symbol.for('define'), Symbol.for('var-name'), [Symbol.for('number->letter'), Symbol.for('i')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('~>'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('var-name'), Symbol.for('optional')], [Symbol.for('send'), Symbol.for('set-type'), [Symbol.for('compile-type-exp'), Symbol.for('param'), Symbol.for('env'), Symbol.for('options')]]]];
    return new TSFunctionType([...mandatoryParams.map(function (param: any): any {
      return compileParam(param, false);
    }), ...optionalParams.map(function (param: any): any {
      return compileParam(param, true);
    })], compileTypeExp(returnValue, env, options));
  } else if (Array.isArray(exp) && (exp.length > 0)) {
    let name: any = new Identifier(exp[0].description as string);
    let params: any = exp.slice(1).map(function (x: any): any {
      return x.description as string;
    });
    if (params.length > 0) {
      return new TSTypeReference(name, new TSTypeParameterInstantiation(params));
    } else {
      return new TSTypeReference(name);
    }
  } else {
    return new TSAnyKeyword();
  }
}

compileTypeExp.lispSource = [Symbol.for('define'), [Symbol.for('compile-type-exp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('new'), Symbol.for('TSAnyKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Void')]], [Symbol.for('new'), Symbol.for('TSVoidKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Undefined')]], [Symbol.for('new'), Symbol.for('TSUndefinedKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Boolean')]], [Symbol.for('new'), Symbol.for('TSBooleanKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('True')]], [Symbol.for('new'), Symbol.for('TSLiteralType'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('#t')]]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('False')]], [Symbol.for('new'), Symbol.for('TSLiteralType'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('#f')]]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Number')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Integer')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Natural')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Real')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('String')]], [Symbol.for('new'), Symbol.for('TSStringKeyword')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('TSTypeReference'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('symbol->string'), Symbol.for('exp')]]]]]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('List')]], [Symbol.for('new'), Symbol.for('TSTupleType'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-type-exp'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('rest'), Symbol.for('exp')]]]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Listof')]], [Symbol.for('new'), Symbol.for('TSArrayType'), [Symbol.for('compile-type-exp'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Pairof')]], [Symbol.for('compile-type-exp'), [Symbol.for('quasiquote'), [Symbol.for('Listof'), [Symbol.for('U'), [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('exp')]], Symbol.for('Symbol')]]], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('U')]], [Symbol.for('new'), Symbol.for('TSUnionType'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-type-exp'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('rest'), Symbol.for('exp')]]]], [[Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->*')]]], [Symbol.for('define'), Symbol.for('mandatory-params'), [Symbol.for('if'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->*')]], [Symbol.for('second'), Symbol.for('exp')], [Symbol.for('drop'), [Symbol.for('drop-right'), Symbol.for('exp'), 1], 1]]], [Symbol.for('define'), Symbol.for('optional-params'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->*')]], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3]], [Symbol.for('third'), Symbol.for('exp')], [Symbol.for('quote'), []]]], [Symbol.for('define'), Symbol.for('return-value'), [Symbol.for('array-list-last'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('define'), [Symbol.for('compile-param'), Symbol.for('param'), Symbol.for('optional')], [Symbol.for('define'), Symbol.for('var-name'), [Symbol.for('number->letter'), Symbol.for('i')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('~>'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('var-name'), Symbol.for('optional')], [Symbol.for('send'), Symbol.for('set-type'), [Symbol.for('compile-type-exp'), Symbol.for('param'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('new'), Symbol.for('TSFunctionType'), [Symbol.for('append'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('param')], [Symbol.for('compile-param'), Symbol.for('param'), Symbol.for('#f')]], Symbol.for('mandatory-params')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('param')], [Symbol.for('compile-param'), Symbol.for('param'), Symbol.for('#t')]], Symbol.for('optional-params')]], [Symbol.for('compile-type-exp'), Symbol.for('return-value'), Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 0]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('symbol->string'), [Symbol.for('first'), Symbol.for('exp')]]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('map'), Symbol.for('symbol->string'), [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('params')], 0], [Symbol.for('new'), Symbol.for('TSTypeReference'), Symbol.for('name'), [Symbol.for('new'), Symbol.for('TSTypeParameterInstantiation'), Symbol.for('params')]]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('TSTypeReference'), Symbol.for('name')]]]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]];

/**
 * Convert a number to a letter.
 * `0` corresponds to `a`, `1` to `b`, etc.
 */
function numberToLetter(n: any): any {
  return String.fromCharCode('a'.charCodeAt(0) + n);
}

numberToLetter.lispSource = [Symbol.for('define'), [Symbol.for('number->letter'), Symbol.for('n')], [Symbol.for('~>'), [Symbol.for('send'), 'a', Symbol.for('charCodeAt'), 0], [Symbol.for('+'), Symbol.for('_'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('String'), Symbol.for('fromCharCode'), Symbol.for('_')]]];

/**
 * "NO-OP" operation.
 */
function nop_(exp: any, env: any): any {
  return undefined;
}

nop_.lispSource = [Symbol.for('define'), [Symbol.for('nop_'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('undefined')];

/**
 * Compile a `(+ ...)` expression.
 */
function compileAdd(node: any, env: any, options: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    identity: 0,
    operator: '+'
  });
}

compileAdd.lispSource = [Symbol.for('define'), [Symbol.for('compile-add'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 0, 'operator', '+']]];

/**
 * Compile an `(apply ...)` expression.
 */
function compileApply(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  const f: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  const isMakeObject: any = env.get(f) === new_;
  const callee: any = isMakeObject ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2]) : f;
  const args: any = isMakeObject ? exp.slice(3) : exp.slice(2);
  const calleeCompiled: any = compileExpression(makeRose(callee), env, options);
  const argsCompiled: any = [];
  if (args.length > 0) {
    let regularArgs: any = args.slice(0, -1);
    let restArg: any = args[args.length - 1];
    for (let arg of regularArgs) {
      argsCompiled.push(compileExpression(makeRose(arg), env, options));
    }
    argsCompiled.push(new RestElement(compileExpression(makeRose(restArg), env, options)));
  }
  if (isMakeObject) {
    return makeExpressionOrStatement(new NewExpression(calleeCompiled, argsCompiled), options);
  } else {
    return makeExpressionOrStatement(new CallExpression(calleeCompiled, argsCompiled), options);
  }
}

compileApply.lispSource = [Symbol.for('define'), [Symbol.for('compile-apply'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('is-make-object'), [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('f')], Symbol.for('new_')]], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('if'), Symbol.for('is-make-object'), [Symbol.for('third'), Symbol.for('exp')], Symbol.for('f')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('if'), Symbol.for('is-make-object'), [Symbol.for('drop'), Symbol.for('exp'), 3], [Symbol.for('drop'), Symbol.for('exp'), 2]]], [Symbol.for('define'), Symbol.for('callee-compiled'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('callee')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('args-compiled'), [Symbol.for('quote'), []]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('args')], 0], [Symbol.for('define'), Symbol.for('regular-args'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('define'), Symbol.for('rest-arg'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('regular-args')]], [Symbol.for('push-right!'), Symbol.for('args-compiled'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('arg')], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('push-right!'), Symbol.for('args-compiled'), [Symbol.for('new'), Symbol.for('RestElement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('rest-arg')], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('cond'), [Symbol.for('is-make-object'), [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('NewExpression'), Symbol.for('callee-compiled'), Symbol.for('args-compiled')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), Symbol.for('callee-compiled'), Symbol.for('args-compiled')], Symbol.for('options')]]]];

/**
 * Compile an `(array-ref ...)` expression.
 */
function compileArrayRef(node: any, env: any, options: any = {}): any {
  const variable: any = node.get(1);
  const variableCompiled: any = compileExpression(variable, env, options);
  const indices: any = node.drop(2);
  const indicesCompiled: any = indices.map(function (x: any): any {
    return compileExpression(x, env, options);
  });
  let result: any = indicesCompiled.reduce(function (acc: any, x: any): any {
    return new MemberExpression(acc, x, true);
  }, variableCompiled);
  return makeExpressionOrStatement(result, options);
}

compileArrayRef.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-ref'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('variable'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('variable-compiled'), [Symbol.for('compile-expression'), Symbol.for('variable'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('indices-compiled'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('new'), Symbol.for('MemberExpression'), Symbol.for('acc'), Symbol.for('x'), Symbol.for('#t')]], Symbol.for('variable-compiled'), Symbol.for('indices-compiled')]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];

/**
 * Compile an `(array-set! ...)` expression.
 */
function compileArraySet(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  const arr: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  const indices: any = exp.slice(2).slice(0, -1);
  let value: any = exp[exp.length - 1];
  return compileRose(insertSexpIntoRose([Symbol.for('set!'), [Symbol.for('array-ref'), arr, ...indices], value], node), env, options);
}

compileArraySet.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-set'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('arr'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), [Symbol.for('drop'), Symbol.for('exp'), 2], 1]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1]]], [Symbol.for('compile-rose'), [Symbol.for('insert-sexp-into-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('array-ref'), [Symbol.for('unquote'), Symbol.for('arr')], [Symbol.for('unquote-splicing'), Symbol.for('indices')]], [Symbol.for('unquote'), Symbol.for('value')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile an `(object-ref ...)` expression.
 */
function compileObjectRef(node: any, env: any, options: any = {}): any {
  return compileArrayRef(node, env, options);
}

compileObjectRef.lispSource = [Symbol.for('define'), [Symbol.for('compile-object-ref'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-array-ref'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile an `(object-set! ...)` expression.
 */
function compileObjectSet(node: any, env: any, options: any = {}): any {
  return compileArraySet(node, env, options);
}

compileObjectSet.lispSource = [Symbol.for('define'), [Symbol.for('compile-object-set'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-array-set'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile an atomic expression, such as `foo`.
 */
function compileAtom(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new Literal(node.getValue()), options);
}

compileAtom.lispSource = [Symbol.for('define'), [Symbol.for('compile-atom'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], Symbol.for('options')]];

/**
 * Compile a `(: ...)` expression.
 */
function compileColon(node: any, env: any, options: any = {}): any {
  let bindings: any = options['bindings'];
  const sym: any = node.get(1);
  const symExp: any = sym.getValue();
  let type_: any = node.get(2);
  const typeExp: any = type_.getValue();
  let bindingType: any = 'variable';
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
function compileCond(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  const condClauses: any = node.drop(1);
  if (condClauses.length === 0) {
    return makeExpressionOrStatement(new Literal(false), options);
  } else {
    function reducingF(condClause: any, compiledExp: any): any {
      const condition: any = condClause.get(0);
      const thenClauses: any = beginWrapRoseSmart(condClause.drop(1));
      if ((expressionType === 'statement') || (expressionType === 'return')) {
        return new IfStatement(compileExpression(condition, env, options), transferAndCompileComments(condClause, unwrapBlockStatement(wrapInBlockStatementSmart(compileStatementOrReturnStatement(thenClauses, env, options))), options), compiledExp);
      } else {
        return new ConditionalExpression(compileExpression(condition, env, options), transferAndCompileComments(condClause, compileExpression(thenClauses, env, options), options), compiledExp);
      }
    }
    reducingF.lispSource = [Symbol.for('define'), [Symbol.for('reducing-f'), Symbol.for('cond-clause'), Symbol.for('compiled-exp')], [Symbol.for('define'), Symbol.for('condition'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('then-clauses'), [Symbol.for('begin-wrap-rose-smart'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('drop'), 1]]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('new'), Symbol.for('IfStatement'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('unwrap-block-statement'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')]]], Symbol.for('options')], Symbol.for('compiled-exp')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ConditionalExpression'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('compile-expression'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')], Symbol.for('options')], Symbol.for('compiled-exp')]]]];
    if (condClauses[condClauses.length - 1].get(0).getValue() === Symbol.for('else')) {
      let finalClause: any = ((expressionType === 'statement') || (expressionType === 'return')) ? transferAndCompileComments(condClauses[condClauses.length - 1], wrapInBlockStatementSmart(compileStatementOrReturnStatement(beginWrapRoseSmart1(condClauses[condClauses.length - 1].drop(1)), env, options)), options) : transferAndCompileComments(condClauses[condClauses.length - 1], compileExpression(beginWrapRose(condClauses[condClauses.length - 1].drop(1)), env, options), options);
      if (!((expressionType !== 'statement') || estreeTypeP(finalClause, 'BlockStatement'))) {
        finalClause = makeBlockStatement([finalClause]);
      }
      return condClauses.slice(0, -1).reduceRight(function (acc: any, x: any): any {
        return reducingF(x, acc);
      }, finalClause);
    } else {
      return condClauses.reduceRight(function (acc: any, x: any): any {
        return reducingF(x, acc);
      }, ((expressionType === 'statement') || (expressionType === 'return')) ? null : new Identifier('undefined'));
    }
  }
}

compileCond.lispSource = [Symbol.for('define'), [Symbol.for('compile-cond'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('cond-clauses'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('cond-clauses')], 0], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('#f')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), [Symbol.for('reducing-f'), Symbol.for('cond-clause'), Symbol.for('compiled-exp')], [Symbol.for('define'), Symbol.for('condition'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('then-clauses'), [Symbol.for('begin-wrap-rose-smart'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('drop'), 1]]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('new'), Symbol.for('IfStatement'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('unwrap-block-statement'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')]]], Symbol.for('options')], Symbol.for('compiled-exp')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ConditionalExpression'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('compile-expression'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')], Symbol.for('options')], Symbol.for('compiled-exp')]]]], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('~>'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]], [Symbol.for('quote'), Symbol.for('else')]], [Symbol.for('define'), Symbol.for('final-clause'), [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('transfer-and-compile-comments'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('begin-wrap-rose-smart-1'), [Symbol.for('send'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], Symbol.for('drop'), 1]], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('transfer-and-compile-comments'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], [Symbol.for('compile-expression'), [Symbol.for('begin-wrap-rose'), [Symbol.for('send'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], Symbol.for('drop'), 1]], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement']], [Symbol.for('estree-type?'), Symbol.for('final-clause'), 'BlockStatement']], [Symbol.for('set!'), Symbol.for('final-clause'), [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('final-clause')]]]], [Symbol.for('foldr'), Symbol.for('reducing-f'), Symbol.for('final-clause'), [Symbol.for('drop-right'), Symbol.for('cond-clauses'), 1]]], [Symbol.for('else'), [Symbol.for('foldr'), Symbol.for('reducing-f'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], Symbol.for('js/null'), [Symbol.for('new'), Symbol.for('Identifier'), 'undefined']], Symbol.for('cond-clauses')]]]]]];

/**
 * Compile a `(define ...)` expression.
 */
function compileDefine(node: any, env: any, options: any = {}): any {
  let bindings: any = options['bindings'];
  const language: any = options['language'];
  const inlineLispSourceOption: any = options['inlineLispSources'];
  let exp: any = node.getValue();
  let type_: any = Symbol.for('Any');
  if (Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
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
    const sym: any = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1])[0];
    const shouldCurry: any = Array.isArray(sym);
    const nameSym: any = shouldCurry ? flatten(sym)[0] : sym;
    const functionName: any = printEstree(compileSymbol(makeRose(nameSym), env, makeExpressionOptions(options)), options);
    const lambdaExp: any = defineToLambda(node);
    const returnType: any = (node.get(2).getValue() === Symbol.for(':')) ? node.get(3).getValue() : Symbol.for('Any');
    let params: any = lambdaExp.get(1).getValue();
    const declaredType: any = bindings.get(sym);
    if ((declaredType === undefined) || (declaredType === true) || (declaredType === Symbol.for('Any'))) {
      type_ = [Symbol.for('->'), ...((typeof params === 'symbol') ? [[Symbol.for('Listof'), Symbol.for('Any')]] : ((Array.isArray(params) && (params.length >= 3) && (params[params.length - 2] === Symbol.for('.')) && !((): any => {
        const x: any = lastCdr(params);
        return Array.isArray(x) && (x.length === 0);
      })()) ? [...makeList(params.length - 2, Symbol.for('Any')), [Symbol.for('Listof'), Symbol.for('Any')]] : makeList(params.length, Symbol.for('Any')))), returnType];
      bindings.setLocal((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
            result = exp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : exp[1], type_, 'procedure');
    } else {
      type_ = declaredType;
    }
    const compiledType: any = compileTypeExp(type_, env, options);
    let result: any;
    if (shouldCurry) {
      result = compileDefine(makeRose([Symbol.for('define'), nameSym, lambdaExp], node), env, options);
    } else {
      const returnType: any = ((compiledType instanceof TSFunctionType) && (compiledType.returnType instanceof TSVoidKeyword)) ? 'void' : undefined;
      result = compileJsFunction(lambdaExp, env, makeExpressionOptions(options), {
        functionName: functionName,
        returnType: returnType
      });
      if (compiledType instanceof TSFunctionType) {
        const _end: any = result.params.length;
        for (let i: any = 0; i < _end; i++) {
          let param: any = (result.params as any)[i];
          const typeParam: any = (compiledType.params as any)[i];
          const typeParamAnnotation: any = typeParam ? typeParam.typeAnnotation : new TSAnyKeyword();
          if (!param.hasType()) {
            param.setType(typeParamAnnotation);
          }
        }
        result.returnType = compiledType.returnType;
      }
    }
    if (inlineLispSourceOption) {
      const lispCodeExp: any = compileSexp([Symbol.for('set-field!'), Symbol.for('lispSource'), Symbol.for(functionName), [Symbol.for('quote'), exp]], env, options);
      return new Program([result, lispCodeExp]);
    } else {
      return result;
    }
  } else if (exp.length === 2) {
    // Uninitialized variable.
    bindings.setLocal((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1], true, 'variable');
    return new VariableDeclaration([new VariableDeclarator(compileExpression(node.get(1), env, options))], 'let');
  } else if (formp((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[2], jsAsync_, env) && formp(((): any => {
    const lst: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 2;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[2];
    if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(lst);
      return Array.isArray(x) && (x.length === 0);
    })()) {
      let i: any = 1;
      let result: any = lst;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = lst[lst.length - 1];
        } else {
          result = lst.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    } else {
      return lst[1];
    }
  })(), lambda_, env)) {
    // Asynchronous function definition.
    const lambdaNode: any = node.get(2).get(1);
    let name: any = node.get(1);
    const args: any = lambdaNode.get(1).drop(0);
    const daForm: any = transferComments(node, makeRose([Symbol.for('define/async'), [name, ...args], ...lambdaNode.drop(2)]));
    return compileDefineAsync(daForm, env, options);
  } else if (formp((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
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
    return compileDefineClass(makeRose(defineToDefineClass(exp), node), env, options);
  } else {
    // Initialized variable.
    const sym: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1];
    const symCompiled: any = compileSymbol(node.get(1), env, options);
    if (bindings.has(sym)) {
      type_ = bindings.get(sym);
    } else {
      bindings.setLocal((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
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
    return new VariableDeclaration([new VariableDeclarator(symCompiled.setType(compileType(type_, env, options)), compileExpression(node.get(2), env, options))], 'let');
  }
}

compileDefine.lispSource = [Symbol.for('define'), [Symbol.for('compile-define'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('inline-lisp-source-option'), [Symbol.for('oget'), Symbol.for('options'), 'inlineLispSources']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('cond'), [[Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('should-curry'), [Symbol.for('array?'), Symbol.for('sym')]], [Symbol.for('define'), Symbol.for('name-sym'), [Symbol.for('if'), Symbol.for('should-curry'), [Symbol.for('first'), [Symbol.for('flatten'), Symbol.for('sym')]], Symbol.for('sym')]], [Symbol.for('define'), Symbol.for('function-name'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('name-sym')], Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('lambda-exp'), [Symbol.for('define->lambda'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], [Symbol.for('send'), Symbol.for('get-value')]], [Symbol.for('quote'), Symbol.for(':')]], [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 3], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('else'), [Symbol.for('quote'), Symbol.for('Any')]]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('lambda-exp'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('declared-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get'), Symbol.for('sym')]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('declared-type'), Symbol.for('undefined')], [Symbol.for('eq?'), Symbol.for('declared-type'), Symbol.for('#t')], [Symbol.for('eq?'), Symbol.for('declared-type'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('quasiquote'), [Symbol.for('->'), [Symbol.for('unquote-splicing'), [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params')], [Symbol.for('list'), [Symbol.for('quote'), [Symbol.for('Listof'), Symbol.for('Any')]]]], [[Symbol.for('dotted-list?'), Symbol.for('params')], [Symbol.for('append'), [Symbol.for('make-list'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('params')], 2], [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('list'), [Symbol.for('quote'), [Symbol.for('Listof'), Symbol.for('Any')]]]]], [Symbol.for('else'), [Symbol.for('make-list'), [Symbol.for('array-list-length'), Symbol.for('params')], [Symbol.for('quote'), Symbol.for('Any')]]]]], [Symbol.for('unquote'), Symbol.for('return-type')]]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('type_'), 'procedure']], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('type_'), Symbol.for('declared-type')]]], [Symbol.for('define'), Symbol.for('compiled-type'), [Symbol.for('compile-type-exp'), Symbol.for('type_'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [Symbol.for('should-curry'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-define'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('name-sym')], [Symbol.for('unquote'), Symbol.for('lambda-exp')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('is-a?'), Symbol.for('compiled-type'), Symbol.for('TSFunctionType')], [Symbol.for('is-a?'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('compiled-type')], Symbol.for('TSVoidKeyword')]], 'void', Symbol.for('undefined')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-js-function'), Symbol.for('lambda-exp'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')], [Symbol.for('js-obj'), 'functionName', Symbol.for('function-name'), 'returnType', Symbol.for('return-type')]]], [Symbol.for('when'), [Symbol.for('is-a?'), Symbol.for('compiled-type'), Symbol.for('TSFunctionType')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('result')]]]]], [Symbol.for('define'), Symbol.for('param'), [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('result')], Symbol.for('i')]], [Symbol.for('define'), Symbol.for('type-param'), [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('compiled-type')], Symbol.for('i')]], [Symbol.for('define'), Symbol.for('type-param-annotation'), [Symbol.for('if'), Symbol.for('type-param'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('type-param')], [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]], [Symbol.for('unless'), [Symbol.for('send'), Symbol.for('param'), Symbol.for('has-type')], [Symbol.for('send'), Symbol.for('param'), Symbol.for('set-type'), Symbol.for('type-param-annotation')]]], [Symbol.for('set-field!'), Symbol.for('returnType'), Symbol.for('result'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('compiledType')]]]]], [Symbol.for('cond'), [Symbol.for('inline-lisp-source-option'), [Symbol.for('define'), Symbol.for('lisp-code-exp'), [Symbol.for('compile-sexp'), [Symbol.for('quasiquote'), [Symbol.for('set-field!'), Symbol.for('lispSource'), [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('function-name')]], [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Program'), [Symbol.for('list'), Symbol.for('result'), Symbol.for('lisp-code-exp')]]], [Symbol.for('else'), Symbol.for('result')]]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('#t'), 'variable'], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]], 'let']], [[Symbol.for('and'), [Symbol.for('form?'), [Symbol.for('third'), Symbol.for('exp')], Symbol.for('js-async_'), Symbol.for('env')], [Symbol.for('form?'), [Symbol.for('second'), [Symbol.for('third'), Symbol.for('exp')]], Symbol.for('lambda_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('lambda-node'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('lambda-node'), Symbol.for('get'), 1], Symbol.for('drop'), 0]], [Symbol.for('define'), Symbol.for('da-form'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define/async'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('unquote-splicing'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('lambda-node'), Symbol.for('drop'), 2]]]]]]], [Symbol.for('compile-define-async'), Symbol.for('da-form'), Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('form?'), [Symbol.for('third'), Symbol.for('exp')], Symbol.for('class_'), Symbol.for('env')], [Symbol.for('compile-define-class'), [Symbol.for('make-rose'), [Symbol.for('define->define-class'), Symbol.for('exp')], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('sym-compiled'), [Symbol.for('compile-symbol'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get'), Symbol.for('sym')]]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('type_'), 'variable']]], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('~>'), Symbol.for('sym-compiled'), [Symbol.for('send'), Symbol.for('set-type'), [Symbol.for('compile-type'), Symbol.for('type_'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]]], 'let']]]];

/**
 * Compile a `(define/async ...)` expression.
 */
function compileDefineAsync(node: any, env: any, options: any = {}): any {
  const inlineLispSourceOption: any = options['inlineLispSources'];
  let result: any = compileDefine(node, env, options);
  const resultF: any = inlineLispSourceOption ? result.body[0] : result;
  if (estreeTypeP(resultF, 'FunctionDeclaration')) {
    resultF.async = true;
  }
  const returnType: any = resultF.returnType;
  resultF.returnType = new TSTypeReference(new Identifier('Promise'), new TSTypeParameterInstantiation([new TSAnyKeyword()]));
  return result;
}

compileDefineAsync.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-async'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inline-lisp-source-option'), [Symbol.for('oget'), Symbol.for('options'), 'inlineLispSources']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-define'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result-f'), [Symbol.for('if'), Symbol.for('inline-lisp-source-option'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('result')]], Symbol.for('result')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('result-f'), 'FunctionDeclaration'], [Symbol.for('set-field!'), Symbol.for('async'), Symbol.for('result-f'), Symbol.for('#t')]], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('result-f')]], [Symbol.for('set-field!'), Symbol.for('returnType'), Symbol.for('result-f'), [Symbol.for('new'), Symbol.for('TSTypeReference'), [Symbol.for('new'), Symbol.for('Identifier'), 'Promise'], [Symbol.for('new'), Symbol.for('TSTypeParameterInstantiation'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]]], Symbol.for('result')];

/**
 * Compile a `(define/generator ...)` expression.
 */
function compileDefineGenerator(node: any, env: any, options: any = {}): any {
  let result: any = compileDefine(node, env, options);
  result.generator = true;
  return result;
}

compileDefineGenerator.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-generator'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-define'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('generator'), Symbol.for('result'), Symbol.for('#t')], Symbol.for('result')];

/**
 * Compile a `(/ ...)` expression.
 */
function compileDiv(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  if (exp.length === 2) {
    return compileDiv(makeRose([Symbol.for('/'), 1, node.get(1)], node), env, options);
  } else {
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
function compileSend(node: any, env: any, options: any = {}): any {
  const obj: any = node.get(1);
  const method: any = node.get(2);
  const args: any = node.drop(3);
  return makeExpressionOrStatement(new CallExpression(new MemberExpression((typeof obj.getValue() === 'symbol') ? compileSymbol(obj, env, makeExpressionOptions(options)) : compileExpression(obj, env, options), compileSymbol(method, env, options), false), args.map(function (x: any): any {
    return compileExpression(x, env, options);
  })), options);
}

compileSend.lispSource = [Symbol.for('define'), [Symbol.for('compile-send'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), [Symbol.for('new'), Symbol.for('MemberExpression'), [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('obj'), Symbol.for('get-value')]], [Symbol.for('compile-symbol'), Symbol.for('obj'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('compile-symbol'), Symbol.for('method'), Symbol.for('env'), Symbol.for('options')], Symbol.for('#f')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('args')]], Symbol.for('options')]];

/**
 * Compile a `(send/apply ...)` expression.
 */
function compileSendApply(node: any, env: any, options: any = {}): any {
  const obj: any = node.get(1);
  const method: any = node.get(2);
  const args: any = node.drop(3);
  return makeExpressionOrStatement(compileExpression(makeRose([Symbol.for('apply'), [Symbol.for('get-field'), method, obj], ...args], node), env, options), options);
}

compileSendApply.lispSource = [Symbol.for('define'), [Symbol.for('compile-send-apply'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('apply'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('method')], [Symbol.for('unquote'), Symbol.for('obj')]], [Symbol.for('unquote-splicing'), Symbol.for('args')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]];

/**
 * Compile a `(js/=== ...)` expression.
 */
function compileJsIsStrictlyEqual(node: any, env: any, options: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    identity: true,
    operator: '==='
  });
}

compileJsIsStrictlyEqual.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-is-strictly-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '===']]];

/**
 * Compile a `(js/== ...)` expression.
 */
function compileJsIsLooselyEqual(node: any, env: any, options: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    identity: true,
    operator: '=='
  });
}

compileJsIsLooselyEqual.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-is-loosely-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '==']]];

/**
 * Compiler macro for `(foldl ...)` expressions.
 */
function compileFoldlMacro(exp: any, env: any): any {
  const [f, v, lst]: any[] = exp.slice(1);
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
function compileFoldrMacro(exp: any, env: any): any {
  const [f, v, lst]: any[] = exp.slice(1);
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
function flipFunctionExpression(exp: any, env: any): any {
  if (typeof exp === 'symbol') {
    // Function expression is a symbol:
    // wrap it in a `lambda` form that reverses
    // the order of application.
    return [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('x')], [exp, Symbol.for('x'), Symbol.for('acc')]];
  } else if (formp(exp, lambda_, env) && (((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
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
    return [Symbol.for('lambda'), [((): any => {
      const lst: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
            result = exp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : exp[1];
      if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(lst);
        return Array.isArray(x) && (x.length === 0);
      })()) {
        let i: any = 1;
        let result: any = lst;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = lst[lst.length - 1];
          } else {
            result = lst.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      } else {
        return lst[1];
      }
    })(), ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1])[0], ...((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1]).slice(2)], ...exp.slice(2)];
  } else {
    // Function expression is a function call:
    // pass it to a function that will
    // swap the arguments.
    // Curried **C** combinator, also known as `flip`.
    // Only the first argument is curried here, but
    // otherwise, this behaves similarly to Haskell's
    // `flip`.
    const CExp: any = [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')]]];
    return [CExp, exp];
  }
}

flipFunctionExpression.lispSource = [Symbol.for('define'), [Symbol.for('flip-function-expression'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('x')], [[Symbol.for('unquote'), Symbol.for('exp')], Symbol.for('x'), Symbol.for('acc')]]]], [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('>='), [Symbol.for('array-list-length'), [Symbol.for('second'), Symbol.for('exp')]], 2]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [[Symbol.for('unquote'), [Symbol.for('second'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('unquote'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), [Symbol.for('second'), Symbol.for('exp')], 2]]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('exp'), 2]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('C-exp'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')]]]]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('C-exp')], [Symbol.for('unquote'), Symbol.for('exp')]]]]]];

/**
 * Compile a `(funcall ...)` expression.
 */
function compileFuncall(node: any, env: any, options: any = {}): any {
  return compileFunctionCall(sliceRose(node, 1), env, options);
}

compileFuncall.lispSource = [Symbol.for('define'), [Symbol.for('compile-funcall'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-function-call'), [Symbol.for('slice-rose'), Symbol.for('node'), 1], Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a function call.
 */
function compileFunctionCall(node: any, env: any, options: any = {}): any {
  let bindings: any = options['bindings'];
  const referencedSymbols: any = options['referencedSymbols'];
  const currentModule: any = options['currentModule'];
  const compilationMappingEnvironment: any = options['compilationMappingEnvironment'];
  const callee: any = node.get(0);
  const op: any = callee.getValue();
  const symbolicOp: any = typeof op === 'symbol';
  const shouldInlineOp: any = (
   symbolicOp && shouldInlineP(op, env, options) &&
   // Do not inline the operator if a
   // compilation macro is defined for it.
   !compilationMappingEnvironment.has(env.get(op))
  );
  const args: any = node.drop(1);
  const calleeExp: any = compileExpression(callee, env, (symbolicOp && !shouldInlineOp) ? // Set the `shouldInline` option to `#f`
  // if `op` is a symbol and there is a
  // compilation macro defined for it.
  // options
  {
    ...options,
    shouldInline: false
  } : options);
  const argsExps: any = args.map(function (x: any): any {
    return compileExpression(x, env, options);
  });
  return makeExpressionOrStatement(new CallExpression(calleeExp, argsExps), options);
}

compileFunctionCall.lispSource = [Symbol.for('define'), [Symbol.for('compile-function-call'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('referenced-symbols'), [Symbol.for('oget'), Symbol.for('options'), 'referencedSymbols']], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('oget'), Symbol.for('options'), 'currentModule']], [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('send'), Symbol.for('callee'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('symbolic-op'), [Symbol.for('symbol?'), Symbol.for('op')]], [Symbol.for('define'), Symbol.for('should-inline-op'), [Symbol.for('and'), Symbol.for('symbolic-op'), [Symbol.for('should-inline?'), Symbol.for('op'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('not'), [Symbol.for('send'), Symbol.for('compilation-mapping-environment'), Symbol.for('has'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('op')]]]]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('callee-exp'), [Symbol.for('compile-expression'), Symbol.for('callee'), Symbol.for('env'), [Symbol.for('if'), [Symbol.for('and'), Symbol.for('symbolic-op'), [Symbol.for('not'), Symbol.for('should-inline-op')]], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'shouldInline', Symbol.for('#f')]], Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('args-exps'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('args')]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), Symbol.for('callee-exp'), Symbol.for('args-exps')], Symbol.for('options')]];

/**
 * Add symbol `sym` to `referencedSymbols` if it references a value
 * not defined in the current module.
 */
function addReferencedSymbol(sym: any, env: any, options: any = {}): any {
  const referencedSymbols: any = options['referencedSymbols'];
  if ((
   (
    referencedSymbols &&
    // Do not add if already added.
    !referencedSymbols.includes(sym)
   ) &&
   shouldInlineP(sym, env, options)
  )) {
    referencedSymbols.push(sym);
    return referencedSymbols;
  }
}

addReferencedSymbol.lispSource = [Symbol.for('define'), [Symbol.for('add-referenced-symbol'), Symbol.for('sym'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('referenced-symbols'), [Symbol.for('oget'), Symbol.for('options'), 'referencedSymbols']], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('referenced-symbols'), [Symbol.for('not'), [Symbol.for('memq?'), Symbol.for('sym'), Symbol.for('referenced-symbols')]], [Symbol.for('should-inline?'), Symbol.for('sym'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('push-right!'), Symbol.for('referenced-symbols'), Symbol.for('sym')]]];

/**
 * Whether the language binding for `sym` should be added to
 * the global environment.
 */
function shouldInlineP(sym: any, env: any, options: any = {}): any {
  const shouldInlineOption: any = options['shouldInline'];
  let bindings: any = options['bindings'];
  const compilationMappingEnvironment: any = options['compilationMappingEnvironment'];
  const currentModule: any = options['currentModule'];
  // This may be disabled with the `shouldInline` option.
  if (!shouldInlineOption) {
    return false;
  }
  return (
    (
     (
      (
       (
        (typeof sym === 'symbol') &&
        // Do not inline if the symbol is listed in
        // `send compilation-variables-env`.
        !compilationVariablesEnv.has(sym)
       ) &&
       // Do not inline if there is a local binding for the
       // value (e.g., a `let` variable).
       !(bindings && bindings.has(sym))
      ) &&
      // Do not inline if the current module defines the
      // value.
      !(currentModule && currentModule.hasSymbol(sym))
     ) &&
     // Only inline if the environment binds the symbol.
     env.findFrame(sym, {
       filter: function (x: any): any {
         // Do not inline if the value is a JavaScript
         // value, i.e., if it is provided by the very
         // language compiled to.
         return x !== jsEnvironment;
       }
     })
    )
  );
}

shouldInlineP.lispSource = [Symbol.for('define'), [Symbol.for('should-inline?'), Symbol.for('sym'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('should-inline-option'), [Symbol.for('oget'), Symbol.for('options'), 'shouldInline']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('oget'), Symbol.for('options'), 'currentModule']], [Symbol.for('unless'), Symbol.for('should-inline-option'), [Symbol.for('return'), Symbol.for('#f')]], [Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('sym')], [Symbol.for('not'), [Symbol.for('send'), Symbol.for('compilation-variables-env'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('not'), [Symbol.for('and'), Symbol.for('bindings'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]]], [Symbol.for('not'), [Symbol.for('and'), Symbol.for('current-module'), [Symbol.for('send'), Symbol.for('current-module'), Symbol.for('has-symbol'), Symbol.for('sym')]]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('find-frame'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('js-environment')]]]]]]];

/**
 * Compile a `(> ...)` expression.
 */
function compileGreaterThan(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  if (exp.length < 3) {
    return compileRose(makeRose(true), env, options);
  } else if (exp.length === 3) {
    return compileBinaryExpression(node, env, options, {
      identity: true,
      operator: '>'
    });
  } else {
    // Create `(and ...)` expression.
    const andExp: any = [Symbol.for('and')];
    const _end: any = exp.length;
    for (let i: any = 2; i < _end; i++) {
      andExp.push([Symbol.for('>'), exp[i - 1], (exp as any)[i]]);
    }
    return compileRose(makeRose(andExp), env, options);
  }
}

compileGreaterThan.lispSource = [Symbol.for('define'), [Symbol.for('compile-greater-than'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('#t')], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '>']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('>'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(>= ...)` expression.
 */
function compileGreaterThanOrEqual(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  if (exp.length < 3) {
    return compileRose(makeRose(true), env, options);
  } else if (exp.length === 3) {
    return compileBinaryExpression(node, env, options, {
      identity: true,
      operator: '>='
    });
  } else {
    // Create `(and ...)` expression.
    const andExp: any = [Symbol.for('and')];
    const _end: any = exp.length;
    for (let i: any = 2; i < _end; i++) {
      andExp.push([Symbol.for('>='), exp[i - 1], (exp as any)[i]]);
    }
    return compileRose(makeRose(andExp), env, options);
  }
}

compileGreaterThanOrEqual.lispSource = [Symbol.for('define'), [Symbol.for('compile-greater-than-or-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('#t')], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '>=']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('>='), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a binary expression.
 * Returns a `BinaryExpression`.
 */
function compileBinaryExpression(node: any, env: any, options: any = {}, settings: any = {}): any {
  const operator: any = settings['operator'];
  const logical: any = settings['logical'];
  const operands: any = node.drop(1);
  if (operands.length === 0) {
    const identity: any = settings['identity'];
    return makeExpressionOrStatement(compileRose(makeRose(identity), env, options), options);
  } else if (operands.length === 1) {
    return makeExpressionOrStatement(compileRose(operands[0], env, options), options);
  } else {
    const compiledOperands: any = operands.map(function (arg: any): any {
      return compileExpression(arg, env, options);
    });
    return makeExpressionOrStatement(compiledOperands.slice(1).reduce(function (acc: any, x: any): any {
      if (logical) {
        return new LogicalExpression(operator, acc, x);
      } else {
        return new BinaryExpression(operator, acc, x);
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
function compileLogicalExpression(node: any, env: any, options: any = {}, settings: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    ...settings,
    logical: true
  });
}

compileLogicalExpression.lispSource = [Symbol.for('define'), [Symbol.for('compile-logical-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj-append'), Symbol.for('settings'), [Symbol.for('js-obj'), 'logical', Symbol.for('#t')]]]];

/**
 * Compile a `(lambda ...)` expression.
 */
function compileLambda(node: any, env: any, options: any = {}): any {
  // (compile-js-arrow node env options)
  return compileJsFunction(node, env, options);
}

compileLambda.lispSource = [Symbol.for('define'), [Symbol.for('compile-lambda'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-js-function'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a `(js/function ...)` expression.
 */
function compileJsFunction(node: any, env: any, options: any = {}, settings: any = {}): any {
  const inheritedOptions: any = {
    ...options
  };
  let exp: any = node.getValue();
  const functionName: any = settings['functionName'];
  const generator: any = settings['generator'];
  const returnType: any = settings['returnType'];
  const language: any = inheritedOptions['language'];
  let params: any = [];
  let bindings: any = inheritedOptions['bindings'];
  let argsList: any;
  let regularArgs: any;
  let restArg: any;
  bindings = extendEnvironment(new LispEnvironment(), bindings);
  inheritedOptions['bindings'] = bindings;
  // Parse the parameter list: sort the regular parameters
  // from the rest parameter, if any.
  if (typeof ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1]) === 'symbol') {
    restArg = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1];
  } else if (((): any => {
    const x: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      let x1: any = lastCdr(exp);
      return Array.isArray(x1) && (x1.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1];
    return Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && !((): any => {
      let x1: any = lastCdr(x);
      return Array.isArray(x1) && (x1.length === 0);
    })();
  })()) {
    argsList = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1];
    regularArgs = linkedListDropRight_(argsList, 1);
    restArg =
      // (linked-list-last_ args-list)
      argsList[argsList.length - 1];
  } else {
    regularArgs = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
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
      if (colonFormP(arg)) {
        const sym: any = arg[0];
        const typ: any = (Array.isArray(arg) && (arg.length >= 3) && (arg[arg.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(arg);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 2;
          let result: any = arg;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = arg[arg.length - 1];
            } else {
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
        let result: any = ((arg.length === 4) ? new AssignmentPattern(new Identifier(printEstree(compileExpression(makeRose(sym), env, inheritedOptions), inheritedOptions)), compileExpression(makeRose((Array.isArray(arg) && (arg.length >= 3) && (arg[arg.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(arg);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 3;
          let result: any = arg;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = arg[arg.length - 1];
            } else {
              result = arg.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : arg[3]), env, inheritedOptions)) : new Identifier(printEstree(compileExpression(makeRose(sym), env, inheritedOptions), inheritedOptions))).setType(compileType(typ, env, options));
        params.push(result);
      } else if (Array.isArray(arg)) {
        bindings.setLocal(arg[0], true, 'variable');
        params.push(new AssignmentPattern(new Identifier(printEstree(compileExpression(makeRose(arg[0]), env, inheritedOptions), inheritedOptions)), compileExpression(makeRose((Array.isArray(arg) && (arg.length >= 3) && (arg[arg.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(arg);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = arg;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = arg[arg.length - 1];
            } else {
              result = arg.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : arg[1]), env, inheritedOptions)));
      } else {
        bindings.setLocal(arg, true, 'variable');
        params.push(new Identifier(printEstree(compileExpression(makeRose(arg), env, inheritedOptions), inheritedOptions)));
      }
    }
  }
  if (restArg) {
    bindings.setLocal(restArg, true, 'variable');
    params.push(new RestElement(compileExpression(makeRose(restArg), env, inheritedOptions)));
  }
  let bodyStatements: any = node.drop(2);
  if ((bodyStatements.length > 0) && (bodyStatements[0].getValue() === Symbol.for(':'))) {
    bodyStatements = bodyStatements.slice(2);
  }
  const body: any = wrapInBlockStatement(// wrap-in-block-statement-smart
  compileStatementOrReturnStatement(beginWrapRoseSmart1(bodyStatements).setParent(node), env, {
    ...inheritedOptions,
    expressionType: (returnType === 'void') ? 'statement' : 'return'
  }));
  let result: any;
  if (functionName && (functionName !== '')) {
    result = new FunctionDeclaration(new Identifier(functionName), params, body);
  } else {
    result = new FunctionExpression(params, body);
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
function compileJsArrow(node: any, env: any, options: any = {}): any {
  const f: any = compileJsFunction(node, env, options);
  if (f instanceof FunctionExpression) {
    return new ArrowFunctionExpression(f.params, f.body);
  } else {
    return f;
  }
}

compileJsArrow.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-arrow'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('compile-js-function'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('f'), Symbol.for('FunctionExpression')], [Symbol.for('new'), Symbol.for('ArrowFunctionExpression'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('f')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('f')]]], [Symbol.for('else'), Symbol.for('f')]]];

/**
 * Compile a `(< ...)` expression.
 */
function compileLessThan(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  if (exp.length < 3) {
    return compileRose(makeRose(true), env, options);
  } else if (exp.length === 3) {
    return compileBinaryExpression(node, env, options, {
      identity: true,
      operator: '<'
    });
  } else {
    // Create `(and ...)` expression.
    const andExp: any = [Symbol.for('and')];
    const _end: any = exp.length;
    for (let i: any = 2; i < _end; i++) {
      andExp.push([Symbol.for('<'), exp[i - 1], (exp as any)[i]]);
    }
    return compileRose(makeRose(andExp), env, options);
  }
}

compileLessThan.lispSource = [Symbol.for('define'), [Symbol.for('compile-less-than'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('#t')], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '<']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('<'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(<= ...)` expression.
 */
function compileLessThanOrEqual(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  if (exp.length < 3) {
    return compileRose(makeRose(true), env, options);
  } else if (exp.length === 3) {
    return compileBinaryExpression(node, env, options, {
      identity: true,
      operator: '<='
    });
  } else {
    // Create `(and ...)` expression.
    const andExp: any = [Symbol.for('and')];
    const _end: any = exp.length;
    for (let i: any = 2; i < _end; i++) {
      andExp.push([Symbol.for('<='), exp[i - 1], (exp as any)[i]]);
    }
    return compileRose(makeRose(andExp), env, options);
  }
}

compileLessThanOrEqual.lispSource = [Symbol.for('define'), [Symbol.for('compile-less-than-or-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('#t')], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#t'), 'operator', '<=']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('<='), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(let ...)` expression.
 */
function compileLet(node: any, env: any, options: any = {}): any {
  // There is no distinction between `(let ...)` and `(let* ...)`
  // expressions---they are compiled in the same way.
  return compileLetStar(node, env, options);
}

compileLet.lispSource = [Symbol.for('define'), [Symbol.for('compile-let'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-let-star'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a `(let* ...)` expression.
 */
function compileLetStar(node: any, env: any, options: any = {}): any {
  const inheritedOptions: any = {
    ...options
  };
  let exp: any = node.getValue();
  const expressionType: any = inheritedOptions['expressionType'];
  let bindings: any = inheritedOptions['bindings'];
  let makeBlock: any = false;
  bindings = extendEnvironment(new LispEnvironment(), bindings);
  inheritedOptions['bindings'] = bindings;
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    const letNodes: any = node.get(1).getNodes();
    const bodyNodes: any = node.drop(2);
    const defineNodes: any = letNodes.map(function (x: any): any {
      let exp: any = x.getValue();
      if (Array.isArray(exp)) {
        const sym: any = exp[0];
        if (!makeBlock && bindings.has(sym)) {
          makeBlock = true;
        }
        return makeRose([Symbol.for('define'), x.get(0), x.get(1)], x);
      } else {
        const sym: any = exp;
        if (!makeBlock && bindings.has(sym)) {
          makeBlock = true;
        }
        return makeRose([Symbol.for('define'), x], x);
      }
    });
    let result: any = compileRose(makeRose([makeBlock ? Symbol.for('block') : Symbol.for('begin'), ...defineNodes, ...bodyNodes], node), env, inheritedOptions);
    return result;
  } else {
    return compileExpression(wrapInArrowCall(exp), env, inheritedOptions);
  }
}

compileLetStar.lispSource = [Symbol.for('define'), [Symbol.for('compile-let-star'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#f')], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('let-nodes'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('define-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]]]], Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('sym'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('x')]]], Symbol.for('x')]]]], Symbol.for('let-nodes')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('make-block'), [Symbol.for('quote'), Symbol.for('block')], [Symbol.for('quote'), Symbol.for('begin')]]], [Symbol.for('unquote-splicing'), Symbol.for('define-nodes')], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('inherited-options')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('inherited-options')]]]];

/**
 * Compile a `(let-values ...)` expression.
 */
function compileLetValues(node: any, env: any, options: any = {}): any {
  const inheritedOptions: any = {
    ...options
  };
  let exp: any = node.getValue();
  const expressionType: any = inheritedOptions['expressionType'];
  let bindings: any = inheritedOptions['bindings'];
  let makeBlock: any = false;
  bindings = extendEnvironment(new LispEnvironment(), bindings);
  inheritedOptions['bindings'] = bindings;
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    const letNodes: any = node.get(1).getNodes();
    const bodyNodes: any = node.drop(2);
    const defineNodes: any = letNodes.map(function (x: any): any {
      let exp: any = x.getValue();
      if (typeof exp === 'symbol') {
        const sym: any = exp;
        if (!makeBlock && bindings.has(sym)) {
          makeBlock = true;
        }
        return makeRose([Symbol.for('define'), x]);
      } else {
        const variables: any = x.get(0).getValue();
        if (typeof variables === 'symbol') {
          const sym: any = variables;
          if (!makeBlock && bindings.has(sym)) {
            makeBlock = true;
          }
        } else {
          const syms: any = flatten(variables);
          if (!makeBlock) {
            for (let sym of flatten(variables)) {
              if (bindings.has(sym)) {
                makeBlock = true;
                break;
              }
            }
          }
        }
        let expression: any = x.get(1);
        return makeRose([Symbol.for('define-values'), x.get(0), x.get(1)], x);
      }
    });
    let result: any = compileRose(makeRose([makeBlock ? Symbol.for('block') : Symbol.for('begin'), ...defineNodes, ...bodyNodes], node), env, inheritedOptions);
    return result;
  } else {
    return compileExpression(wrapInArrowCall(exp), env, inheritedOptions);
  }
}

compileLetValues.lispSource = [Symbol.for('define'), [Symbol.for('compile-let-values'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#f')], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('let-nodes'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('define-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('sym'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('x')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('variables'), [Symbol.for('~>'), Symbol.for('x'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('variables')], [Symbol.for('define'), Symbol.for('sym'), Symbol.for('variables')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('syms'), [Symbol.for('flatten'), Symbol.for('variables')]], [Symbol.for('unless'), Symbol.for('make-block'), [Symbol.for('for'), [[Symbol.for('sym'), [Symbol.for('flatten'), Symbol.for('variables')]]], [Symbol.for('when'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')], [Symbol.for('break')]]]]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]]]], Symbol.for('x')]]]], Symbol.for('let-nodes')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('make-block'), [Symbol.for('quote'), Symbol.for('block')], [Symbol.for('quote'), Symbol.for('begin')]]], [Symbol.for('unquote-splicing'), Symbol.for('define-nodes')], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('inherited-options')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('inherited-options')]]]];

/**
 * Compile a `(define-values ...)` expression.
 */
function compileDefineValues(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  const inheritedOptions: any = {
    ...options
  };
  const expressionType: any = inheritedOptions['expressionType'];
  let bindings: any = inheritedOptions['bindings'];
  let makeBlock: any = true;
  let holeMarker: any = Symbol.for('_');
  bindings = extendEnvironment(new LispEnvironment(), bindings);
  inheritedOptions['bindings'] = bindings;
  const variables: any = node.get(1).getValue();
  let expression: any = node.get(2);
  let regularVars: any = [];
  let restVar: any = undefined;
  let varDecls: any = [];
  let declaratorId: any;
  let declaratorInit: any;
  if (expression.getValue() === Symbol.for(':hole-marker')) {
    holeMarker = node.get(3).getValue();
    expression = node.get(4);
  }
  if (typeof variables === 'symbol') {
    declaratorId = new Identifier(printEstree(compileSymbol(makeRose(variables), env, inheritedOptions), inheritedOptions));
    bindings.setLocal(variables, true, 'variable');
  } else {
    if (Array.isArray(variables) && (variables.length >= 3) && (variables[variables.length - 2] === Symbol.for('.')) && !((): any => {
      const x: any = lastCdr(variables);
      return Array.isArray(x) && (x.length === 0);
    })()) {
      const varList: any = flatten(variables);
      regularVars = varList.slice(0, -1);
      restVar = varList[varList.length - 1];
    } else {
      regularVars = variables;
    }
    varDecls = regularVars.map(function (x: any): any {
      if (x === holeMarker) {
        return null;
      } else {
        bindings.setLocal(x, true, 'variable');
        return new Identifier(printEstree(compileSymbol(makeRose(x), env, inheritedOptions), inheritedOptions));
      }
    });
    if (restVar) {
      bindings.setLocal(restVar, true, 'variable');
      varDecls.push(new RestElement(new Identifier(printEstree(compileSymbol(makeRose(restVar), env, inheritedOptions), inheritedOptions))));
    }
    declaratorId = new ArrayPattern(varDecls);
  }
  declaratorInit = compileExpression(expression, env, inheritedOptions);
  return new VariableDeclaration([new VariableDeclarator(declaratorId, declaratorInit)], 'let');
}

compileDefineValues.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-values'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#t')], [Symbol.for('define'), Symbol.for('hole-marker'), [Symbol.for('quote'), Symbol.for('_')]], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('variables'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 2]]], [Symbol.for('define'), Symbol.for('regular-vars'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-var'), Symbol.for('undefined')], [Symbol.for('define'), Symbol.for('var-decls'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('declarator-id')], [Symbol.for('define'), Symbol.for('declarator-init')], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('expression'), Symbol.for('get-value')], [Symbol.for('quote'), Symbol.for(':hole-marker')]], [Symbol.for('set!'), Symbol.for('hole-marker'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 3], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('set!'), Symbol.for('expression'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 4]]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('variables')], [Symbol.for('set!'), Symbol.for('declarator-id'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('variables')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('variables'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('variables')], [Symbol.for('define'), Symbol.for('var-list'), [Symbol.for('flatten'), Symbol.for('variables')]], [Symbol.for('set!'), Symbol.for('regular-vars'), [Symbol.for('drop-right'), Symbol.for('var-list'), 1]], [Symbol.for('set!'), Symbol.for('rest-var'), [Symbol.for('array-list-last'), Symbol.for('var-list')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('regular-vars'), Symbol.for('variables')]]], [Symbol.for('set!'), Symbol.for('var-decls'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')], Symbol.for('js/null')], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('x'), Symbol.for('#t'), 'variable'], [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]], Symbol.for('regular-vars')]], [Symbol.for('when'), Symbol.for('rest-var'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('rest-var'), Symbol.for('#t'), 'variable'], [Symbol.for('push-right!'), Symbol.for('var-decls'), [Symbol.for('new'), Symbol.for('RestElement'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('rest-var')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]], [Symbol.for('set!'), Symbol.for('declarator-id'), [Symbol.for('new'), Symbol.for('ArrayPattern'), Symbol.for('var-decls')]]]], [Symbol.for('set!'), Symbol.for('declarator-init'), [Symbol.for('compile-expression'), Symbol.for('expression'), Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), Symbol.for('declarator-id'), Symbol.for('declarator-init')]], 'let']];

/**
 * Compile a `(set!-values ...)` expression.
 */
function compileSetValues(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  const inheritedOptions: any = {
    ...options
  };
  const expressionType: any = inheritedOptions['expressionType'];
  let makeBlock: any = true;
  let bindings: any = inheritedOptions['bindings'];
  let declaration: any;
  let declarator: any;
  let left: any;
  let right: any;
  bindings = extendEnvironment(new LispEnvironment(), bindings);
  inheritedOptions['bindings'] = bindings;
  declaration = compileDefineValues(makeRose([Symbol.for('define-values'), ...node.drop(1)], node), env, inheritedOptions);
  declarator = declaration.declarations[0];
  left = declarator.id;
  right = declarator.init;
  return wrapExpressionInStatement(new AssignmentExpression('=', left, right), inheritedOptions);
}

compileSetValues.lispSource = [Symbol.for('define'), [Symbol.for('compile-set-values'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#t')], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('declaration')], [Symbol.for('define'), Symbol.for('declarator')], [Symbol.for('define'), Symbol.for('left')], [Symbol.for('define'), Symbol.for('right')], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('set!'), Symbol.for('declaration'), [Symbol.for('compile-define-values'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('declarator'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('declaration')]]], [Symbol.for('set!'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('declarator')]], [Symbol.for('set!'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('declarator')]], [Symbol.for('wrap-expression-in-statement'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', Symbol.for('left'), Symbol.for('right')], Symbol.for('inherited-options')]];

/**
 * Compile a `(let-js-obj ...)` expression.
 */
function compileLetJsObj(node: any, env: any, options: any = {}): any {
  const inheritedOptions: any = {
    ...options
  };
  const expressionType: any = options['expressionType'];
  let bindings: any = inheritedOptions['bindings'];
  let makeBlock: any = false;
  bindings = extendEnvironment(new LispEnvironment(), bindings);
  inheritedOptions['bindings'] = bindings;
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    const letNodes: any = node.get(1).getNodes();
    const bodyNodes: any = node.drop(2);
    const defineNodes: any = letNodes.map(function (x: any): any {
      const fields: any = x.get(0);
      const fieldsExp: any = fields.getValue();
      const obj: any = x.get(1);
      for (let f of fieldsExp) {
        const sym: any = Array.isArray(f) ? ((Array.isArray(f) && (f.length >= 3) && (f[f.length - 2] === Symbol.for('.')) && ((): any => {
          let x1: any = lastCdr(f);
          return Array.isArray(x1) && (x1.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = f;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = f[f.length - 1];
            } else {
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
      return makeRose([Symbol.for('define-js-obj'), fields, obj], x);
    });
    let result: any = compileRose(makeRose([makeBlock ? Symbol.for('block') : Symbol.for('begin'), ...defineNodes, ...bodyNodes], node), env, inheritedOptions);
    return result;
  } else {
    let exp: any = node.getValue();
    return compileExpression(wrapInArrowCall(exp), env, options);
  }
}

compileLetJsObj.lispSource = [Symbol.for('define'), [Symbol.for('compile-let-js-obj'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('define'), Symbol.for('make-block'), Symbol.for('#f')], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('let-nodes'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('define-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('fields'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('fields-exp'), [Symbol.for('send'), Symbol.for('fields'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('for'), [[Symbol.for('f'), Symbol.for('fields-exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('f')], [Symbol.for('second'), Symbol.for('f')], Symbol.for('f')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('set!'), Symbol.for('make-block'), Symbol.for('#t')]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-js-obj'), [Symbol.for('unquote'), Symbol.for('fields')], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('x')]], Symbol.for('let-nodes')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('make-block'), [Symbol.for('quote'), Symbol.for('block')], [Symbol.for('quote'), Symbol.for('begin')]]], [Symbol.for('unquote-splicing'), Symbol.for('define-nodes')], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('inherited-options')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(define-js-obj ...)` expression.
 */
function compileDefineJsObj(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  let bindings: any = options['bindings'];
  const fields: any = node.get(1);
  const fieldsExp: any = fields.getValue();
  const obj: any = node.get(2);
  for (let f of fieldsExp) {
    const sym: any = Array.isArray(f) ? ((Array.isArray(f) && (f.length >= 3) && (f[f.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(f);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = f;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = f[f.length - 1];
        } else {
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
  const expressionStatement: any = compileSetJsObj(makeRose([Symbol.for('set!-js-obj'), fields, obj], node), env, options);
  const assignmentExpression: any = expressionStatement.expression;
  let left: any = assignmentExpression.left;
  let right: any = assignmentExpression.right;
  return new VariableDeclaration([new VariableDeclarator(left, right)], 'let');
}

compileDefineJsObj.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-js-obj'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('fields'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('fields-exp'), [Symbol.for('send'), Symbol.for('fields'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('for'), [[Symbol.for('f'), Symbol.for('fields-exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('f')], [Symbol.for('second'), Symbol.for('f')], Symbol.for('f')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('#t'), 'variable']], [Symbol.for('define'), Symbol.for('expression-statement'), [Symbol.for('compile-set-js-obj'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!-js-obj'), [Symbol.for('unquote'), Symbol.for('fields')], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('assignment-expression'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('expression-statement')]], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('assignment-expression')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('assignment-expression')]], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), Symbol.for('left'), Symbol.for('right')]], 'let']];

/**
 * Compile a `(set!-js-obj! ...)` expression.
 */
function compileSetJsObj(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  return wrapExpressionInStatement(new AssignmentExpression('=', new ObjectPattern(node.get(1).drop(0).map(function (x: any): any {
    let exp: any = x.getValue();
    if (Array.isArray(exp)) {
      return new Property(compileExpression(x.get(0), env, options), compileExpression(x.get(1), env, options));
    } else {
      const key: any = compileExpression(x, env, options);
      return new Property(key, key);
    }
  })), compileExpression(node.get(2), env, options)), options);
}

compileSetJsObj.lispSource = [Symbol.for('define'), [Symbol.for('compile-set-js-obj'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('wrap-expression-in-statement'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', [Symbol.for('new'), Symbol.for('ObjectPattern'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('new'), Symbol.for('Property'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0], Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('key'), [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Property'), Symbol.for('key'), Symbol.for('key')]]]], [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('drop'), 0]]], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

/**
 * Compile a `(list ...)` expression.
 */
function compileList(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new ArrayExpression(node.drop(1).map(function (x: any): any {
    return compileExpression(x, env, options);
  })), options);
}

compileList.lispSource = [Symbol.for('define'), [Symbol.for('compile-list'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ArrayExpression'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]], Symbol.for('options')]];

/**
 * Compile a macro call.
 */
function compileMacroCall(node: any, env: any, options: any = {}): any {
  // Only expand the macro a single step, as there might be
  // compilers defined for the immediate expansion.
  const [expansion]: any[] = macroexpand1(node, env);
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
function macroexpand(exp: any, env: any): any {
  let result: any = exp;
  let expanded: any = false;
  let expanded1: any = true;
  while (expanded1) {
    [result, expanded1] = macroexpand1(result, env);
    expanded = expanded || expanded1;
  }
  return [result, expanded];
}

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
function macroexpand1(exp: any, env: any): any {
  let node: any = exp;
  let result: any = exp;
  let expanded: any = false;
  exp = (node instanceof Rose) ? node.getValue() : node;
  env = env || currentEnvironment_() || emptyEnvironment();
  if (!((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    result = exp;
  } else if (Array.isArray(exp) && (exp.length === 0)) {
    result = exp;
  } else if (quotep(exp)) {
    result = textOfQuotation(exp);
  } else {
    const op: any = exp[0];
    const [macroF, typ]: any[] = env.getTypedValue(op);
    if (typ === 'macro') {
      result = macroF(exp, env);
      expanded = true;
    }
  }
  return [(node instanceof Rose) ? makeRose(result, node) : result, expanded];
}

macroexpand1.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-1'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('node'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('expanded'), Symbol.for('#f')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')], Symbol.for('node')]], [Symbol.for('set!'), Symbol.for('env'), [Symbol.for('or'), Symbol.for('env'), [Symbol.for('current-environment_')], [Symbol.for('empty-environment')]]], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('list?'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('exp')]], [[Symbol.for('null?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('exp')]], [[Symbol.for('quote?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('text-of-quotation'), Symbol.for('exp')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('define-values'), [Symbol.for('macro-f'), Symbol.for('typ')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-typed-value'), Symbol.for('op')]], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('typ'), 'macro'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('macro-f'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('expanded'), Symbol.for('#t')]]]], [Symbol.for('values'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('make-rose'), Symbol.for('result'), Symbol.for('node')], Symbol.for('result')], Symbol.for('expanded')]];

/**
 * Expand the macro call `exp` in `env`, and keep
 * expanding the result for a total number of `n`
 * expansions, or until something that is not a
 * macro call is obtained.
 */
function macroexpandN(exp: any, env: any, n: any = 1): any {
  let i: any = n;
  let result: any = exp;
  let expanded: any = false;
  let expanded1: any = true;
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
function macroexpandUntil(exp: any, env: any, pred: any): any {
  let result: any = exp;
  while (macroCallP(result, env) && pred(result)) {
    [result] = macroexpand1(result, env);
  }
  return result;
}

macroexpandUntil.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-until'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('pred')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('macro-call?'), Symbol.for('result'), Symbol.for('env')], [Symbol.for('pred'), Symbol.for('result')]], [Symbol.for('set!-values'), [Symbol.for('result')], [Symbol.for('macroexpand-1'), Symbol.for('result'), Symbol.for('env')]]], Symbol.for('result')];

/**
 * Expand all macro calls in `exp` in `env`.
 *
 * Similar to [`macroexpand-all` in Emacs Lisp][el:macroexpand-all].
 *
 * [el:macroexpand-all]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand_002dall
 */
function macroexpandAll(exp: any, env: any): any {
  return macroexpandAllUntil(exp, env, function (...args: any[]): any {
    return true;
  });
}

macroexpandAll.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-all'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('macroexpand-all-until'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('const'), Symbol.for('#t')]]];

/**
 * Expand the macro calls in `exp` in `env`, and keep
 * expanding until `pred` returns `#f`, or until
 * something that is not a macro call is obtained.
 */
function macroexpandAllUntil(exp: any, env: any, pred: any = undefined, stack: any = [], bindings: any = new LispEnvironment()): any {
  function f(x: any, stack: any, bindings: any): any {
    // Wrap `pred` in a function that checks
    // whether the operator symbol is locally
    // bound to something else than a macro.
    const predF: any = pred || (function (...args: any[]): any {
      return true;
    });
    function predF1(x: any): any {
      const op: any = x[0];
      const bType: any = bindings.getType(op);
      return ((bType === 'macro') || (bType === 'undefined')) && predF(x);
    }
    predF1.lispSource = [Symbol.for('define'), [Symbol.for('pred-f-1'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define-values'), Symbol.for('b-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')]], [Symbol.for('and'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('b-type'), 'macro'], [Symbol.for('eq?'), Symbol.for('b-type'), 'undefined']], [Symbol.for('pred-f'), Symbol.for('x')]]];
    if (macroCallP(x, env)) {
      let result: any = macroexpandUntil(x, env, predF1);
      if (!macroCallP(result, env)) {
        result = mapSexp(f, result, env, stack, bindings);
      }
      return result;
    } else {
      return x;
    }
  }
  f.lispSource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('pred-f'), [Symbol.for('or'), Symbol.for('pred'), [Symbol.for('const'), Symbol.for('#t')]]], [Symbol.for('define'), [Symbol.for('pred-f-1'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define-values'), Symbol.for('b-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')]], [Symbol.for('and'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('b-type'), 'macro'], [Symbol.for('eq?'), Symbol.for('b-type'), 'undefined']], [Symbol.for('pred-f'), Symbol.for('x')]]], [Symbol.for('cond'), [[Symbol.for('macro-call?'), Symbol.for('x'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('macroexpand-until'), Symbol.for('x'), Symbol.for('env'), Symbol.for('pred-f-1')]], [Symbol.for('unless'), [Symbol.for('macro-call?'), Symbol.for('result'), Symbol.for('env')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('result'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]], Symbol.for('result')], [Symbol.for('else'), Symbol.for('x')]]];
  return mapSexp(f, exp, env, stack, bindings);
}

macroexpandAllUntil.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-all-until'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('pred'), Symbol.for('undefined')], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('pred-f'), [Symbol.for('or'), Symbol.for('pred'), [Symbol.for('const'), Symbol.for('#t')]]], [Symbol.for('define'), [Symbol.for('pred-f-1'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define-values'), Symbol.for('b-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')]], [Symbol.for('and'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('b-type'), 'macro'], [Symbol.for('eq?'), Symbol.for('b-type'), 'undefined']], [Symbol.for('pred-f'), Symbol.for('x')]]], [Symbol.for('cond'), [[Symbol.for('macro-call?'), Symbol.for('x'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('macroexpand-until'), Symbol.for('x'), Symbol.for('env'), Symbol.for('pred-f-1')]], [Symbol.for('unless'), [Symbol.for('macro-call?'), Symbol.for('result'), Symbol.for('env')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('result'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]], Symbol.for('result')], [Symbol.for('else'), Symbol.for('x')]]], [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]];

/**
 * Macroexpand all compiler macros.
 * This expands regular macros as well.
 */
function macroexpandCompilerMacros(exp: any, env: any): any {
  const compilerMacroEnv: any = makeMacroEnvironment(env);
  let result: any = macroexpandAll(exp, compilerMacroEnv);
  return result;
}

macroexpandCompilerMacros.lispSource = [Symbol.for('define'), [Symbol.for('macroexpand-compiler-macros'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('compiler-macro-env'), [Symbol.for('make-macro-environment'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('macroexpand-all'), Symbol.for('exp'), Symbol.for('compiler-macro-env')]], Symbol.for('result')];

/**
 * Compile a `(. ...)` expression.
 * Also handles `(.method obj ...)` calls.
 */
function compileDot(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  let match: any = (exp[0].description as string).match(new RegExp('^\\.(.*)$'));
  const method: any = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(match);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = match;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = match[match.length - 1];
      } else {
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
    if ((match = (((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 2;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[2]).description as string).match(new RegExp('^-(.*)$')))) {
      const field: any = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(match);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = match;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = match[match.length - 1];
          } else {
            result = match.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : match[1];
      const fieldSym: any = Symbol.for(field);
      const obj: any = node.get(1);
      return compileGetField(makeRose([Symbol.for('get-field'), fieldSym, obj]), env, options);
    } else {
      return compileSend(node, env, options);
    }
  } else {
    const obj: any = node.get(1);
    if ((match = method.match(new RegExp('^-(.*)$')))) {
      // Member expression:
      // `(.-foo bar)` = `(get-field foo bar)`.
      const field: any = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(match);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = match;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = match[match.length - 1];
          } else {
            result = match.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : match[1];
      return compileGetField(makeRose([Symbol.for('get-field'), Symbol.for(field), obj], node), env, options);
    } else {
      // Method call:
      // `(.foo bar ...)` = `(send bar foo ...)`.
      return compileSend(makeRose([Symbol.for('send'), obj, Symbol.for(method), ...node.drop(2)], node), env, options);
    }
  }
}

compileDot.lispSource = [Symbol.for('define'), [Symbol.for('compile-dot'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\.(.*)$'], [Symbol.for('symbol->string'), [Symbol.for('first'), Symbol.for('exp')]]]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('method'), ''], [Symbol.for('cond'), [[Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^-(.*)$'], [Symbol.for('symbol->string'), [Symbol.for('third'), Symbol.for('exp')]]]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('define'), Symbol.for('field-sym'), [Symbol.for('string->symbol'), Symbol.for('field')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('compile-get-field'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('field-sym')], [Symbol.for('unquote'), Symbol.for('obj')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-send'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('cond'), [[Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^-(.*)$'], Symbol.for('method')]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('compile-get-field'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('field')]], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-send'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('obj')], [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('method')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]]]]];

/**
 * Compile a `(get-field ...)` expression.
 */
function compileGetField(node: any, env: any, options: any = {}): any {
  const field: any = node.get(1);
  const obj: any = node.get(2);
  const computed: any = typeof field.getValue() !== 'symbol';
  return makeExpressionOrStatement(new MemberExpression((typeof obj.getValue() === 'symbol') ? compileSymbol(obj, env, makeExpressionOptions(options)) : compileExpression(obj, env, options), computed ? compileExpression(field, env, options) : compileSymbol(field, env, options), computed), options);
}

compileGetField.lispSource = [Symbol.for('define'), [Symbol.for('compile-get-field'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('not'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('field'), Symbol.for('get-value')]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('MemberExpression'), [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('obj'), Symbol.for('get-value')]], [Symbol.for('compile-symbol'), Symbol.for('obj'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('if'), Symbol.for('computed'), [Symbol.for('compile-expression'), Symbol.for('field'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-symbol'), Symbol.for('field'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('computed')], Symbol.for('options')]];

/**
 * Compile a `(js/optional-chaining ...)` expression.
 */
function compileJsOptionalChaining(node: any, env: any, options: any = {}): any {
  const obj: any = node.get(1);
  const field: any = node.get(2);
  const fieldExp: any = field.getValue();
  if (Array.isArray(fieldExp)) {
    let result: any = compileRose(makeRose([obj, ...field.drop(0)], node), env, options);
    result.optional = true;
    return result;
  } else {
    let result: any = compileRose(makeRose([Symbol.for('get-field'), field, obj], node), env, options);
    result.optional = true;
    return result;
  }
}

compileJsOptionalChaining.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-optional-chaining'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('field-exp'), [Symbol.for('send'), Symbol.for('field'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('field-exp')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('obj')], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('field'), Symbol.for('drop'), 0]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('optional'), Symbol.for('result'), Symbol.for('#t')], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('field')], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('optional'), Symbol.for('result'), Symbol.for('#t')], Symbol.for('result')]]];

/**
 * Compile a `(set-field! ...)` expression.
 */
function compileSetField(node: any, env: any, options: any = {}): any {
  const field: any = node.get(1);
  const obj: any = node.get(2);
  let val: any = node.get(3);
  return compileRose(makeRose([Symbol.for('set!'), [Symbol.for('get-field'), field, obj], val], node), env, options);
}

compileSetField.lispSource = [Symbol.for('define'), [Symbol.for('compile-set-field'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 3]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('field')], [Symbol.for('unquote'), Symbol.for('obj')]], [Symbol.for('unquote'), Symbol.for('val')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a `(modulo ...)` expression.
 */
function compileModulo(node: any, env: any, options: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    identity: 1,
    operator: '%'
  });
}

compileModulo.lispSource = [Symbol.for('define'), [Symbol.for('compile-modulo'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 1, 'operator', '%']]];

/**
 * Compile a `(* ...)` expression.
 */
function compileMul(node: any, env: any, options: any = {}): any {
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
function compileNop(node: any, env: any, options: any = {}): any {
  return makeProgramFragment();
}

compileNop.lispSource = [Symbol.for('define'), [Symbol.for('compile-nop'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-program-fragment')]];

/**
 * Compile a `(not ...)` expression.
 */
function compileNot(node: any, env: any, options: any = {}): any {
  function isNotExpressionP(x: any): any {
    return estreeTypeP(x, 'UnaryExpression') && (x.operator === '!');
  }
  isNotExpressionP.lispSource = [Symbol.for('define'), [Symbol.for('is-not-expression?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('x'), 'UnaryExpression'], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('x')], '!']]];
  const operand: any = node.get(1);
  const operandCompiled: any = compileExpression(operand, env, options);
  let result: any = undefined;
  if (estreeTypeP(operandCompiled, 'BinaryExpression') && (operandCompiled.operator === '===')) {
    operandCompiled.operator = '!==';
    result = operandCompiled;
  } else if (estreeTypeP(operandCompiled, 'BinaryExpression') && (operandCompiled.operator === '==')) {
    operandCompiled.operator = '!=';
    result = operandCompiled;
  } else {
    let notExpression: any = new UnaryExpression('!', true, operandCompiled);
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
function compileBegin(node: any, env: any, options: any = {}): any {
  let bindings: any = options['bindings'];
  const expressionType: any = options['expressionType'];
  let exp: any = node.getValue();
  const body: any = node.drop(1);
  const compiledBody: any = [];
  // Add defined variables to `bindings` environment.
  // We have to handle them here since they may refer
  // to each other.
  const _end: any = body.length;
  for (let i: any = 0; i < _end; i++) {
    let exp: any = (body as any)[i].getValue();
    if (formp(exp, define_, env)) {
      const sym: any = Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
            result = exp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : exp[1]) ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
            result = exp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : exp[1])[0] : ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
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
    } else if (formp(exp, defineMacro_, env)) {
      const sym: any = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
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
    } else if (formp(exp, defmacro_, env)) {
      const sym: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
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
    let bodyStatements: any = compileStatements(body, env, options);
    // Note that this returns a `Program` node, but in
    // some contexts, a `BlockStatement` node is wanted.
    // One can convert a `Program` node to a
    // `BlockStatement` node with
    // `wrap-in-block-statement`.
    return makeProgramFragment(bodyStatements);
  } else {
    // Wrap in an arrow function.
    if (exp.length === 2) {
      return compileExpression(node.get(1), env, options);
    } else {
      return compileExpression(wrapInArrowCall(exp), env, options);
    }
  }
}

compileBegin.lispSource = [Symbol.for('define'), [Symbol.for('compile-begin'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('compiled-body'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('body')]]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), [Symbol.for('aget'), Symbol.for('body'), Symbol.for('i')], Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('form?'), Symbol.for('exp'), Symbol.for('define_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('if'), [Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('#t'), 'variable']], [[Symbol.for('form?'), Symbol.for('exp'), Symbol.for('define-macro_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('#t'), 'macro']], [[Symbol.for('form?'), Symbol.for('exp'), Symbol.for('defmacro_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('sym'), Symbol.for('#t'), 'macro']]]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('body-statements'), [Symbol.for('compile-statements'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('make-program-fragment'), Symbol.for('body-statements')]], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('options')]]]]]];

/**
 * Compile a `(block ...)` expression.
 */
function compileBlock(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    return wrapInBlockStatement(compileBegin(node, env, options));
  } else {
    return compileBegin(node, env, options);
  }
}

compileBlock.lispSource = [Symbol.for('define'), [Symbol.for('compile-block'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-begin'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('compile-begin'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Make and compile a `(require ...)` or `(define-values ...)` form
 * that defines referenced values from the language environment.
 * `symbols` is a list of symbols bound in the language environment.
 */
function buildGlobalEnvironment(symbols: any, env: any, options: any = {}): any {
  let exp: any = makeGlobalEnvironmentExp(symbols, env, options);
  return compileGlobalEnvironment(exp, env, options);
}

buildGlobalEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('build-global-environment'), Symbol.for('symbols'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('make-global-environment-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('compile-global-environment'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Make a form that defines referenced values
 * from the language environment. Returns `#f`
 * if there are no symbols.
 */
function makeGlobalEnvironmentExp(symbols: any, env: any, options: any): any {
  if (symbols.length === 0) {
    return false;
  } else if (options['inlineFunctions']) {
    return makeDefineValuesExp(symbols, env, options);
  } else {
    return makeRequireExp(symbols, env, options);
  }
}

makeGlobalEnvironmentExp.lispSource = [Symbol.for('define'), [Symbol.for('make-global-environment-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('symbols')], 0], Symbol.for('#f')], [[Symbol.for('oget'), Symbol.for('options'), 'inlineFunctions'], [Symbol.for('make-define-values-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('make-require-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Make a `(define-values ...)` form for the global environment.
 */
function makeDefineValuesExp(symbols: any, env: any, options: any): any {
  const inlineFunctionsOption: any = options['inlineFunctions'];
  let definitions: any = false;
  const defineForms: any = [];
  const internalSymbols: any = [];
  const externalSymbols: any = [];
  const referencedSymbols: any = [...symbols];
  const currentModule: any = new Module();
  let bindings: any = options['bindings'];
  const seen: any = [];
  let exp: any;
  let internalSymbol: any;
  let symbol: any;
  let value: any;
  while (referencedSymbols.length > 0) {
    symbol = referencedSymbols.shift();
    seen.push(symbol);
    if (!externalSymbols.includes(symbol) && env.has(symbol)) {
      value = env.get(symbol);
      if (sourcep(value)) {
        exp = source(value);
        if (taggedListP(exp, Symbol.for('define'))) {
          internalSymbol = Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
            const x: any = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
          })()) ? ((): any => {
            let i: any = 1;
            let result: any = exp;
            while (i > 0) {
              if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
              } else {
                result = exp.slice(1);
              }
              i--;
            }
            if (Array.isArray(result)) {
              result = result[0];
            }
            return result;
          })() : exp[1]) ? exp[1][0] : exp[1];
          const referencedSymbols1: any = [];
          const bindings1: any = bindings ? bindings.clone() : undefined;
          const compiledExpression: any = compileRose(makeRose(exp), env, {
            ...options,
            bindings: bindings1,
            currentModule: currentModule,
            referencedSymbols: referencedSymbols1
          });
          for (let symbol1 of referencedSymbols1) {
            if (!(seen.includes(symbol1) || referencedSymbols.includes(symbol1))) {
              referencedSymbols.push(symbol1);
            }
          }
        }
      } else {
        // Deal with the case when the value has no Lisp source.
        if (value instanceof Function) {
          const jsString: any = value + '';
          let match: any;
          match = jsString.match(new RegExp('^function ([^( ]+)'));
          if (match) {
            internalSymbol = Symbol.for((Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && ((): any => {
              const x: any = lastCdr(match);
              return Array.isArray(x) && (x.length === 0);
            })()) ? ((): any => {
              let i: any = 1;
              let result: any = match;
              while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                  result = match[match.length - 1];
                } else {
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
          } else {
            internalSymbol = symbol;
            exp = [Symbol.for('define'), internalSymbol, [Symbol.for('js'), jsString]];
          }
        } else if ((value !== null) && (typeof value === 'object')) {
          const jsString: any = JSON.stringify(value, null, 2);
          internalSymbol = symbol;
          exp = [Symbol.for('define'), internalSymbol, [Symbol.for('js'), jsString]];
        } else {
          const jsString: any = value + '';
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
function makeRequireExp(symbols: any, env: any, options: any): any {
  return [Symbol.for('require'), [Symbol.for('only-in'), packageName, ...symbols]];
}

makeRequireExp.lispSource = [Symbol.for('define'), [Symbol.for('make-require-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('quasiquote'), [Symbol.for('require'), [Symbol.for('only-in'), [Symbol.for('unquote'), Symbol.for('package-name')], [Symbol.for('unquote-splicing'), Symbol.for('symbols')]]]]];

/**
 * Compile a `(define-values ...)` form that defines referenced values
 * from the language environment.
 */
function compileGlobalEnvironment(exp: any, env: any, options: any = {}): any {
  if (!exp) {
    return makeProgramFragment();
  } else if (taggedListP(exp, Symbol.for('define-values'))) {
    const defineValuesForm: any = [exp[0], (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1], [Symbol.for('list')]];
    const body: any = exp[2];
    // Compile the body in a sandboxed environment.
    const bodyCompiled: any = compileSexp(body, env, {
      ...options,
      bindings: new LispEnvironment(),
      expressionType: 'expression'
    });
    const varDecl: any = compileSexp(defineValuesForm, env, options);
    varDecl.declarations[0].init = bodyCompiled;
    let result: any = makeProgramFragment([varDecl]);
    return result;
  } else {
    return makeProgramFragment([compileSexp(exp, env, options)]);
  }
}

compileGlobalEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('compile-global-environment'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('exp')], [Symbol.for('make-program-fragment')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define-values')]], [Symbol.for('define'), Symbol.for('define-values-form'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('list')]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('aget'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('body-compiled'), [Symbol.for('compile-sexp'), Symbol.for('body'), Symbol.for('env'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'bindings', [Symbol.for('new'), Symbol.for('LispEnvironment')], 'expressionType', 'expression']]]], [Symbol.for('define'), Symbol.for('var-decl'), [Symbol.for('compile-sexp'), Symbol.for('define-values-form'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('init'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('var-decl')]], Symbol.for('body-compiled')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('make-program-fragment'), [Symbol.for('list'), Symbol.for('var-decl')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('make-program-fragment'), [Symbol.for('list'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]]]]]];

/**
 * Make a `((lambda () ...))` expression that evaluates to a single
 * value from the language environment. `symbol` is a symbol bound in
 * the language environment.
 */
function makeInlinedValue(symbol: any, env: any, options: any): any {
  // We take the output of a call to `make-global-environment-exp`
  // and massage it into a simpler expression.
  const globalEnvironmentExp: any = makeGlobalEnvironmentExp([symbol], env, {
    ...options,
    inlineFunctions: true
  });
  if (globalEnvironmentExp.length > 1) {
    const lambdaCall: any = globalEnvironmentExp[2];
    const lambdaExp: any = lambdaCall[0];
    const valuesExp: any = lambdaExp[lambdaExp.length - 1];
    const sym: any = (Array.isArray(valuesExp) && (valuesExp.length >= 3) && (valuesExp[valuesExp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(valuesExp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = valuesExp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = valuesExp[valuesExp.length - 1];
        } else {
          result = valuesExp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : valuesExp[1];
    let result: any = lambdaCall;
    if ((lambdaExp.length === 4) && (typeof ((): any => {
      const lst: any = (Array.isArray(lambdaExp) && (lambdaExp.length >= 3) && (lambdaExp[lambdaExp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(lambdaExp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 2;
        let result: any = lambdaExp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = lambdaExp[lambdaExp.length - 1];
          } else {
            result = lambdaExp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : lambdaExp[2];
      if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(lst);
        return Array.isArray(x) && (x.length === 0);
      })()) {
        let i: any = 1;
        let result: any = lst;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = lst[lst.length - 1];
          } else {
            result = lst.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      } else {
        return lst[1];
      }
    })() === 'symbol')) {
      // In simple cases, where there is only a single
      // `(define sym ...)` form, no `lambda` expression
      // is necessary.
      result = ((): any => {
        const lst: any = (Array.isArray(lambdaExp) && (lambdaExp.length >= 3) && (lambdaExp[lambdaExp.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(lambdaExp);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 2;
          let result: any = lambdaExp;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = lambdaExp[lambdaExp.length - 1];
            } else {
              result = lambdaExp.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : lambdaExp[2];
        if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(lst);
          return Array.isArray(x) && (x.length === 0);
        })()) {
          let i: any = 2;
          let result: any = lst;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = lst[lst.length - 1];
            } else {
              result = lst.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        } else {
          return lst[2];
        }
      })();
    } else {
      // Change the return value of the `lambda` function
      // from a `(values ...)` form to a single value.
      lambdaExp[lambdaExp.length - 1] = sym;
    }
    return result;
  } else {
    return globalEnvironmentExp;
  }
}

makeInlinedValue.lispSource = [Symbol.for('define'), [Symbol.for('make-inlined-value'), Symbol.for('symbol'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('global-environment-exp'), [Symbol.for('make-global-environment-exp'), [Symbol.for('list'), Symbol.for('symbol')], Symbol.for('env'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'inlineFunctions', Symbol.for('#t')]]]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('global-environment-exp')], 1], [Symbol.for('define'), Symbol.for('lambda-call'), [Symbol.for('aget'), Symbol.for('global-environment-exp'), 2]], [Symbol.for('define'), Symbol.for('lambda-exp'), [Symbol.for('aget'), Symbol.for('lambda-call'), 0]], [Symbol.for('define'), Symbol.for('values-exp'), [Symbol.for('array-list-last'), Symbol.for('lambda-exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('second'), Symbol.for('values-exp')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lambda-call')], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('lambda-exp')], 4], [Symbol.for('symbol?'), [Symbol.for('second'), [Symbol.for('third'), Symbol.for('lambda-exp')]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('third'), [Symbol.for('third'), Symbol.for('lambda-exp')]]]], [Symbol.for('else'), [Symbol.for('list-set!'), Symbol.for('lambda-exp'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('lambda-exp')], 1], Symbol.for('sym')]]], Symbol.for('result')], [Symbol.for('else'), Symbol.for('global-environment-exp')]]];

/**
 * Compile an `(or ...)` expression.
 */
function compileOr(node: any, env: any, options: any = {}): any {
  return compileLogicalExpression(node, env, options, {
    identity: false,
    operator: '||'
  });
}

compileOr.lispSource = [Symbol.for('define'), [Symbol.for('compile-or'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-logical-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', Symbol.for('#f'), 'operator', '||']]];

/**
 * Compile a `(provide ...)` expression.
 */
function compileProvide(node: any, env: any, options: any = {}): any {
  const expressions: any = node.drop(1);
  // Sort `all-from-out` expressions from the rest.
  const allFromOutExpressions: any = [];
  const otherExpressions: any = [];
  for (let x of expressions) {
    if (taggedListP(x.getValue(), Symbol.for('all-from-out'))) {
      allFromOutExpressions.push(x);
    } else {
      otherExpressions.push(x);
    }
  }
  // Compile `all-from-out` expressions.
  const results: any = [];
  for (let x of allFromOutExpressions) {
    const source: any = x.get(1);
    let result: any = new ExportAllDeclaration(compileExpression(source, env, options));
    results.push(result);
  }
  // Compile other expressions.
  if (otherExpressions.length > 0) {
    let specifiers: any = [];
    const seen: any = [];
    for (let x of otherExpressions) {
      let exp: any = x.getValue();
      if (taggedListP(exp, Symbol.for('rename-out'))) {
        for (let pair of exp.slice(1)) {
          let x1: any = pair[0];
          let x2: any = (Array.isArray(pair) && (pair.length >= 3) && (pair[pair.length - 2] === Symbol.for('.')) && ((): any => {
            const x3: any = lastCdr(pair);
            return Array.isArray(x3) && (x3.length === 0);
          })()) ? ((): any => {
            let i: any = 1;
            let result: any = pair;
            while (i > 0) {
              if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = pair[pair.length - 1];
              } else {
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
            x1 = printEstree(compileSymbol(makeRose(x1), env, options, {
              literalSymbol: true
            }), options);
          }
          if (typeof x2 === 'symbol') {
            x2 = printEstree(compileSymbol(makeRose(x2), env, options, {
              literalSymbol: true
            }), options);
          }
          if (!seen.includes(x2)) {
            seen.push(x2);
            specifiers.push(new ExportSpecifier(new Identifier(x1), new Identifier(x2)));
          }
        }
      } else {
        let x1: any = exp;
        if (typeof x1 === 'symbol') {
          x1 = printEstree(compileSymbol(makeRose(x1), env, options, {
            literalSymbol: true
          }), options);
        }
        if (!seen.includes(x1)) {
          seen.push(x1);
          specifiers.push(new ExportSpecifier(new Identifier(x1)));
        }
      }
    }
    let result: any = new ExportNamedDeclaration(null, specifiers);
    results.push(result);
  }
  if (results.length === 1) {
    return results[0];
  } else {
    return makeProgramFragment(results);
  }
}

compileProvide.lispSource = [Symbol.for('define'), [Symbol.for('compile-provide'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expressions'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('all-from-out-expressions'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('other-expressions'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('expressions')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')], [Symbol.for('quote'), Symbol.for('all-from-out')]], [Symbol.for('push-right!'), Symbol.for('all-from-out-expressions'), Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('other-expressions'), Symbol.for('x')]]]], [Symbol.for('define'), Symbol.for('results'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('all-from-out-expressions')]], [Symbol.for('define'), Symbol.for('source'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('ExportAllDeclaration'), [Symbol.for('compile-expression'), Symbol.for('source'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('push-right!'), Symbol.for('results'), Symbol.for('result')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('other-expressions')], 0], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('seen'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('other-expressions')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('rename-out')]], [Symbol.for('for'), [[Symbol.for('pair'), [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('first'), Symbol.for('pair')]], [Symbol.for('define'), Symbol.for('x2'), [Symbol.for('second'), Symbol.for('pair')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x2')], [Symbol.for('set!'), Symbol.for('x2'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x2')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x2'), Symbol.for('seen')], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x2')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ExportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x2')]]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x1'), Symbol.for('seen')], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x1')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ExportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('ExportNamedDeclaration'), Symbol.for('js/null'), Symbol.for('specifiers')]], [Symbol.for('push-right!'), Symbol.for('results'), Symbol.for('result')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('results')], 1], [Symbol.for('first'), Symbol.for('results')]], [Symbol.for('else'), [Symbol.for('make-program-fragment'), Symbol.for('results')]]]];

/**
 * Compile a `(quote ...)` expression.
 */
function compileQuote(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  let result: any;
  if (Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1])) {
    result = compileExpression(makeRose([Symbol.for('list'), ...((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
          result = exp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : exp[1]).map(function (x: any): any {
      return [Symbol.for('quote'), x];
    })]), env, options);
  } else if (typeof ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
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
  } else {
    result = compileExpression(node.get(1), env, options);
  }
  return makeExpressionOrStatement(result, options);
}

compileQuote.lispSource = [Symbol.for('define'), [Symbol.for('compile-quote'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [[Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('list'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('quasiquote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('x')]]]]]]]]], Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('symbol?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-symbol'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'quotedSymbol', Symbol.for('#t')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];

/**
 * Compile a `(quasiquote ...)` expression.
 */
function compileQuasiquote(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(compileQuasiquoteHelper(node.get(1), env, options), options);
}

compileQuasiquote.lispSource = [Symbol.for('define'), [Symbol.for('compile-quasiquote'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-quasiquote-helper'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]];

/**
 * Helper function for `compile-quasiquote`.
 */
function compileQuasiquoteHelper(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  if (!Array.isArray(exp)) {
    return compileExpression(makeRose([Symbol.for('quote'), exp]), env, options);
  } else {
    return new ArrayExpression(node.getNodes().map(function (x: any): any {
      let exp: any = x.getValue();
      if (taggedListP(exp, Symbol.for('quasiquote'))) {
        return compileQuote(makeRose([Symbol.for('quote'), exp]), env, options);
      } else if (taggedListP(exp, Symbol.for('unquote'))) {
        return compileExpression(x.get(1), env, options);
      } else if (taggedListP(exp, Symbol.for('unquote-splicing'))) {
        return new RestElement(compileExpression(x.get(1), env, options));
      } else {
        return compileQuasiquoteHelper(x, env, options);
      }
    }));
  }
}

compileQuasiquoteHelper.lispSource = [Symbol.for('define'), [Symbol.for('compile-quasiquote-helper'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('array?'), Symbol.for('exp')]], [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ArrayExpression'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('quasiquote')]], [Symbol.for('compile-quote'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('unquote')]], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]], [Symbol.for('new'), Symbol.for('RestElement'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('compile-quasiquote-helper'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-nodes')]]]]]];

/**
 * Compile a `(require ...)` expression.
 */
function compileRequire(node: any, env: any, options: any = {}): any {
  const esModuleInterop: any = options['esModuleInterop'];
  let bindings: any = options['bindings'];
  const xNode: any = node.get(1);
  let xExp: any = xNode.getValue();
  const yNode: any = node.get(2) || xNode;
  let yExp: any = yNode.getValue();
  let specifiers: any = [];
  const seen: any = [];
  let src: any = null;
  if (taggedListP(xExp, Symbol.for('only-in'))) {
    for (let x of xNode.drop(2)) {
      let exp: any = x.getValue();
      if (Array.isArray(exp)) {
        let x1: any = exp[0];
        let x2: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
          const x3: any = lastCdr(exp);
          return Array.isArray(x3) && (x3.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = exp;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = exp[exp.length - 1];
            } else {
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
          x1 = printEstree(compileSymbol(makeRose(x1), env, options, {
            literalSymbol: true
          }), options);
        }
        if (typeof x2 === 'symbol') {
          x2 = printEstree(compileSymbol(makeRose(x2), env, options, {
            literalSymbol: true
          }), options);
        }
        if (!seen.includes(x2)) {
          if (bindings) {
            bindings.setLocal(x2, true, 'variable');
          }
          seen.push(x2);
          specifiers.push(new ImportSpecifier(new Identifier(x1), new Identifier(x2)));
        }
      } else {
        let x1: any = exp;
        if (typeof x1 === 'symbol') {
          x1 = printEstree(compileSymbol(makeRose(x1), env, options, {
            literalSymbol: true
          }), options);
        }
        if (!seen.includes(x1)) {
          if (bindings) {
            bindings.setLocal(x1, true, 'variable');
          }
          seen.push(x1);
          specifiers.push(new ImportSpecifier(new Identifier(x1)));
        }
      }
    }
    yExp = (Array.isArray(xExp) && (xExp.length >= 3) && (xExp[xExp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(xExp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = xExp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = xExp[xExp.length - 1];
        } else {
          result = xExp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : xExp[1];
  } else {
    if (typeof xExp === 'symbol') {
      xExp = printEstree(compileSymbol(makeRose(xExp), env, options, {
        literalSymbol: true
      }), options);
    }
    specifiers = [esModuleInterop ? new ImportDefaultSpecifier(new Identifier(xExp)) : new ImportNamespaceSpecifier(new Identifier(xExp))];
  }
  if (typeof yExp === 'symbol') {
    yExp = printEstree(compileSymbol(makeRose(yExp), env, options, {
      literalSymbol: true
    }), options);
  }
  src = new Literal(yExp);
  if (bindings && (typeof xExp === 'symbol')) {
    bindings.setLocal(xExp, true, 'variable');
  }
  if (Array.isArray(specifiers) && (specifiers.length === 0)) {
    return emptyProgram();
  } else {
    return new ImportDeclaration(specifiers, src);
  }
}

compileRequire.lispSource = [Symbol.for('define'), [Symbol.for('compile-require'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('es-module-interop'), [Symbol.for('oget'), Symbol.for('options'), 'esModuleInterop']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('x-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('x-exp'), [Symbol.for('send'), Symbol.for('x-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('y-node'), [Symbol.for('or'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('x-node')]], [Symbol.for('define'), Symbol.for('y-exp'), [Symbol.for('send'), Symbol.for('y-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('seen'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('src'), Symbol.for('js/null')], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('x-exp'), [Symbol.for('quote'), Symbol.for('only-in')]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('x-node'), Symbol.for('drop'), 2]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('x2'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x2')], [Symbol.for('set!'), Symbol.for('x2'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x2')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x2'), Symbol.for('seen')], [Symbol.for('when'), Symbol.for('bindings'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('x2'), Symbol.for('#t'), 'variable']], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x2')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ImportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x2')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x1'), Symbol.for('seen')], [Symbol.for('when'), Symbol.for('bindings'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('x1'), Symbol.for('#t'), 'variable']], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x1')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ImportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')]]]]]]], [Symbol.for('set!'), Symbol.for('y-exp'), [Symbol.for('second'), Symbol.for('x-exp')]]], [Symbol.for('else'), [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x-exp')], [Symbol.for('set!'), Symbol.for('x-exp'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x-exp')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('specifiers'), [Symbol.for('list'), [Symbol.for('if'), Symbol.for('es-module-interop'), [Symbol.for('new'), Symbol.for('ImportDefaultSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x-exp')]], [Symbol.for('new'), Symbol.for('ImportNamespaceSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x-exp')]]]]]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('y-exp')], [Symbol.for('set!'), Symbol.for('y-exp'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('y-exp')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', Symbol.for('#t')]], Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('src'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('y-exp')]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('bindings'), [Symbol.for('symbol?'), Symbol.for('x-exp')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('x-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('specifiers')], [Symbol.for('empty-program')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ImportDeclaration'), Symbol.for('specifiers'), Symbol.for('src')]]]];

/**
 * Compile a `(set! ...)` expression.
 */
function compileSet(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  const symNode: any = node.get(1);
  const symExp: any = symNode.getValue();
  let valNode: any = node.get(2);
  let valExp: any = valNode.getValue();
  if (formp(valExp, add_, env) && (((((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
        result = valExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : valExp[1]) === symExp) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
        result = valExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : valExp[2]) === 1)) || ((((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
        result = valExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : valExp[2]) === symExp) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
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
    valNode = makeRose(valExp);
  } else if (formp(valExp, sub_, env) && (((((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
        result = valExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : valExp[1]) === symExp) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
        result = valExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : valExp[2]) === 1)) || ((((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
        result = valExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : valExp[2]) === symExp) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
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
    valNode = makeRose(valExp);
  }
  let result: any = '';
  if (formp(valExp, add1_, env) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
        result = valExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : valExp[1]) === symExp)) {
    result = new UpdateExpression('++', compileExpression(symNode, env, options), (expressionType === 'return') || (expressionType !== 'statement'));
  } else if (formp(valExp, sub1_, env) && (((Array.isArray(valExp) && (valExp.length >= 3) && (valExp[valExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(valExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = valExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = valExp[valExp.length - 1];
      } else {
        result = valExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : valExp[1]) === symExp)) {
    result = new UpdateExpression('--', compileExpression(symNode, env, options), (expressionType === 'return') || (expressionType !== 'statement'));
  } else {
    result = new AssignmentExpression('=', (typeof symNode.getValue() === 'symbol') ? compileSymbol(symNode, env, makeExpressionOptions(options)) : compileExpression(symNode, env, options), compileExpression(valNode, env, options));
  }
  return makeExpressionOrStatement(result, options);
}

compileSet.lispSource = [Symbol.for('define'), [Symbol.for('compile-set'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('sym-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('sym-exp'), [Symbol.for('send'), Symbol.for('sym-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('val-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('val-exp'), [Symbol.for('send'), Symbol.for('val-node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('add_'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], 1]], [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], 1]]]], [Symbol.for('set!'), Symbol.for('val-exp'), [Symbol.for('quasiquote'), [Symbol.for('add1'), [Symbol.for('unquote'), Symbol.for('sym-exp')]]]], [Symbol.for('set!'), Symbol.for('val-node'), [Symbol.for('make-rose'), Symbol.for('val-exp')]]], [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('sub_'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], 1]], [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], 1]]]], [Symbol.for('set!'), Symbol.for('val-exp'), [Symbol.for('quasiquote'), [Symbol.for('sub1'), [Symbol.for('unquote'), Symbol.for('sym-exp')]]]], [Symbol.for('set!'), Symbol.for('val-node'), [Symbol.for('make-rose'), Symbol.for('val-exp')]]]], [Symbol.for('define'), Symbol.for('result'), ''], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('add1_'), Symbol.for('env')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('UpdateExpression'), '++', [Symbol.for('compile-expression'), Symbol.for('sym-node'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement']]]]]], [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('sub1_'), Symbol.for('env')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('UpdateExpression'), '--', [Symbol.for('compile-expression'), Symbol.for('sym-node'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement']]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('sym-node'), Symbol.for('get-value')]], [Symbol.for('compile-symbol'), Symbol.for('sym-node'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('sym-node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('val-node'), Symbol.for('env'), Symbol.for('options')]]]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];

/**
 * Compile a string expression.
 */
function compileString(node: any, env: any, options: any = {}): any {
  const str: any = node.getValue();
  if (str.match(new RegExp('\\n'))) {
    let lines: any = str.split(new RegExp('^', 'gm'));
    if (lines.length <= 1) {
      return compileAtom(node, env, options);
    } else {
      // TODO: We could compile to a template literal instead.
      // We just have to take care to escape it properly.
      return compileRose(transferComments(node, makeRose([Symbol.for('string-append'), ...lines], node)), env, options);
    }
  } else {
    return compileAtom(node, env, options);
  }
}

compileString.lispSource = [Symbol.for('define'), [Symbol.for('compile-string'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n'], Symbol.for('str')], [Symbol.for('define'), Symbol.for('lines'), [Symbol.for('string-split'), Symbol.for('str'), [Symbol.for('regexp'), '^', 'gm']]], [Symbol.for('cond'), [[Symbol.for('<='), [Symbol.for('array-list-length'), Symbol.for('lines')], 1], [Symbol.for('compile-atom'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-rose'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('string-append'), [Symbol.for('unquote-splicing'), Symbol.for('lines')]]], Symbol.for('node')]], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('compile-atom'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(- ...)` expression.
 */
function compileSub(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  if (exp.length === 2) {
    const num: any = node.get(1);
    const numCompiled: any = compileExpression(num, env, options);
    return makeExpressionOrStatement(new UnaryExpression('-', true, numCompiled), options);
  } else {
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
function compileVariable(node: any, env: any, options: any = {}): any {
  const compilationMappingEnvironment: any = options['compilationMappingEnvironment'];
  const literalSymbol: any = options['literalSymbol'];
  const quotedSymbol: any = options['quotedSymbol'];
  const currentModule: any = options['currentModule'];
  let exp: any = node.getValue();
  if (!(quotedSymbol || literalSymbol || (env.has(exp) && (compilationMappingEnvironment.getType(env.get(exp)) === 'variable')))) {
    if (shouldInlineP(exp, env, options)) {
      if (currentModule) {
        addReferencedSymbol(exp, env, options);
      } else {
        // Inlined expression. The symbol references a value
        // that is defined in the language environment.
        // Create an expression that will evaluate to this
        // value and compile that.
        return makeExpressionOrStatement(compileExpression(makeRose(makeInlinedValue(exp, env, options)), env, options), options);
      }
    }
  }
  return makeExpressionOrStatement(compileSymbol(node, env, options), options);
}

compileVariable.lispSource = [Symbol.for('define'), [Symbol.for('compile-variable'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define'), Symbol.for('literal-symbol'), [Symbol.for('oget'), Symbol.for('options'), 'literalSymbol']], [Symbol.for('define'), Symbol.for('quoted-symbol'), [Symbol.for('oget'), Symbol.for('options'), 'quotedSymbol']], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('oget'), Symbol.for('options'), 'currentModule']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('unless'), [Symbol.for('or'), Symbol.for('quoted-symbol'), Symbol.for('literal-symbol'), [Symbol.for('and'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('exp')], [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('compilation-mapping-environment'), Symbol.for('get-type'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('exp')]], 'variable']]], [Symbol.for('when'), [Symbol.for('should-inline?'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('cond'), [Symbol.for('current-module'), [Symbol.for('add-referenced-symbol'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('return'), [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('make-inlined-value'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]]]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-symbol'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]];

/**
 * Compile a symbol expression.
 */
function compileSymbol(node: any, env: any, options: any = {}, settings: any = {}): any {
  // TODO: Better handling of gensym'ed symbols.
  const literalSymbolOption: any = settings['literalSymbol'] || false;
  let quotedSymbolOption: any = settings['quotedSymbol'];
  const compileEnvironmentOption: any = options['compileEnvironment'];
  const camelCaseOption: any = options['camelCase'];
  let bindings: any = options['bindings'];
  let exp: any = node.getValue();
  const gensymedSymbol: any = (typeof exp === 'symbol') && (exp !== Symbol.for(exp.description as string));
  const str: any = exp.description as string;
  // Keyword symbols (i.e., symbols beginning with `:`,
  // e.g., `:foo`) are auto-quoted.
  if (str.match(new RegExp('^:'))) {
    quotedSymbolOption = true;
  }
  if (quotedSymbolOption) {
    return compileExpression(makeRose([Symbol.for('string->symbol'), str]), env, options);
  } else if (literalSymbolOption) {
    let name: any = makeJsIdentifierString(str, options);
    return new Identifier(name);
  } else if (compilationVariablesEnv.has(exp)) {
    return compilationVariablesEnv.get(exp);
  } else if (str === 'this') {
    return new ThisExpression();
  } else if (gensymedSymbol) {
    let gensymMap: any = options['gensymMap'];
    if (!gensymMap) {
      gensymMap = new Map();
      options['gensymMap'] = gensymMap;
    }
    if (gensymMap.has(exp)) {
      let [gensymName, name, i]: any[] = gensymMap.get(exp);
      const identifier: any = new Identifier(gensymName);
      return identifier;
    } else {
      let name: any = makeJsIdentifierString(str, options);
      let gensymName: any = name;
      let i: any = 1;
      let regularSym: any = Symbol.for(gensymName);
      while (bindings.has(regularSym)) {
        gensymName = name + i + '';
        regularSym = Symbol.for(gensymName);
        i++;
      }
      const identifier: any = new Identifier(gensymName);
      const entry: any = [gensymName, name, i];
      gensymMap.set(exp, entry);
      bindings.setLocal(regularSym, true);
      return identifier;
    }
  } else {
    let name: any = makeJsIdentifierString(str, options);
    return new Identifier(name);
  }
}

compileSymbol.lispSource = [Symbol.for('define'), [Symbol.for('compile-symbol'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('literal-symbol-option'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('settings'), 'literalSymbol'], Symbol.for('#f')]], [Symbol.for('define'), Symbol.for('quoted-symbol-option'), [Symbol.for('oget'), Symbol.for('settings'), 'quotedSymbol']], [Symbol.for('define'), Symbol.for('compile-environment-option'), [Symbol.for('oget'), Symbol.for('options'), 'compileEnvironment']], [Symbol.for('define'), Symbol.for('camel-case-option'), [Symbol.for('oget'), Symbol.for('options'), 'camelCase']], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('gensymed-symbol'), [Symbol.for('gensym?'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('symbol->string'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^:'], Symbol.for('str')], [Symbol.for('set!'), Symbol.for('quoted-symbol-option'), Symbol.for('#t')]], [Symbol.for('cond'), [Symbol.for('quoted-symbol-option'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('string->symbol'), [Symbol.for('unquote'), Symbol.for('str')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('literal-symbol-option'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('name')]], [[Symbol.for('send'), Symbol.for('compilation-variables-env'), Symbol.for('has'), Symbol.for('exp')], [Symbol.for('send'), Symbol.for('compilation-variables-env'), Symbol.for('get'), Symbol.for('exp')]], [[Symbol.for('eq?'), Symbol.for('str'), 'this'], [Symbol.for('new'), Symbol.for('ThisExpression')]], [Symbol.for('gensymed-symbol'), [Symbol.for('define'), Symbol.for('gensym-map'), [Symbol.for('oget'), Symbol.for('options'), 'gensymMap']], [Symbol.for('unless'), Symbol.for('gensym-map'), [Symbol.for('set!'), Symbol.for('gensym-map'), [Symbol.for('make-hash')]], [Symbol.for('oset!'), Symbol.for('options'), 'gensymMap', Symbol.for('gensym-map')]], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('gensym-map'), Symbol.for('exp')], [Symbol.for('define-values'), [Symbol.for('gensym-name'), Symbol.for('name'), Symbol.for('i')], [Symbol.for('hash-ref'), Symbol.for('gensym-map'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('gensym-name')]], Symbol.for('identifier')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('gensym-name'), Symbol.for('name')], [Symbol.for('define'), Symbol.for('i'), 1], [Symbol.for('define'), Symbol.for('regular-sym'), [Symbol.for('string->symbol'), Symbol.for('gensym-name')]], [Symbol.for('while'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('has'), Symbol.for('regular-sym')], [Symbol.for('set!'), Symbol.for('gensym-name'), [Symbol.for('string-append'), Symbol.for('name'), [Symbol.for('number->string'), Symbol.for('i')]]], [Symbol.for('set!'), Symbol.for('regular-sym'), [Symbol.for('string->symbol'), Symbol.for('gensym-name')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('define'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('gensym-name')]], [Symbol.for('define'), Symbol.for('entry'), [Symbol.for('list'), Symbol.for('gensym-name'), Symbol.for('name'), Symbol.for('i')]], [Symbol.for('hash-set!'), Symbol.for('gensym-map'), Symbol.for('exp'), Symbol.for('entry')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('regular-sym'), Symbol.for('#t')], Symbol.for('identifier')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('name')]]]];

/**
 * Transform a string to a valid JavaScript identifier string.
 */
function makeJsIdentifierString(str: any, options: any = {}): any {
  // TODO: This function could benefit from memoization.
  // Need to use a custom memoization map to handle `options`,
  // though... an equality map using `equal?` should suffice.
  const compileEnvironmentOption: any = options['compileEnvironment'];
  // FIXME: Kludge.
  if (!compileEnvironmentOption) {
    return str;
  }
  const camelCaseOption: any = options['camelCase'];
  let result: any = makeJsIdentifierStringHelper(str);
  if (camelCaseOption === false) {
    return kebabCaseToSnakeCase(result);
  } else {
    return kebabCaseToCamelCase(result);
  }
}

makeJsIdentifierString.lispSource = [Symbol.for('define'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('compile-environment-option'), [Symbol.for('oget'), Symbol.for('options'), 'compileEnvironment']], [Symbol.for('unless'), Symbol.for('compile-environment-option'), [Symbol.for('return'), Symbol.for('str')]], [Symbol.for('define'), Symbol.for('camel-case-option'), [Symbol.for('oget'), Symbol.for('options'), 'camelCase']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('make-js-identifier-string-helper'), Symbol.for('str')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('camel-case-option'), Symbol.for('#f')], [Symbol.for('kebab-case->snake-case'), Symbol.for('result')]], [Symbol.for('else'), [Symbol.for('kebab-case->camel-case'), Symbol.for('result')]]]];

/**
 * Helper function for `make-js-identifier-string`.
 */
function makeJsIdentifierStringHelper(str: any): any {
  let result: any = str.replace(new RegExp('^\\+$', 'g'), '_add').replace(new RegExp('^-$', 'g'), '_sub').replace(new RegExp('^\\*$', 'g'), '_mul').replace(new RegExp('^/$', 'g'), '_div').replace(new RegExp('%', 'g'), '').replace(new RegExp('/', 'g'), '-').replace(new RegExp('!', 'g'), '-x').replace(new RegExp(':', 'g'), '-').replace(new RegExp('->', 'g'), '-to-').replace(new RegExp('\\+', 'g'), '_').replace(new RegExp('\\*$', 'g'), '-star').replace(new RegExp('\\*', 'g'), 'star-');
  if (result.match(new RegExp('-', 'g'))) {
    result = result.replace(new RegExp('\\?', 'g'), '-p');
  } else {
    result = result.replace(new RegExp('\\?', 'g'), 'p');
  }
  return result;
}

makeJsIdentifierStringHelper.lispSource = [Symbol.for('define'), [Symbol.for('make-js-identifier-string-helper'), Symbol.for('str')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\+$', 'g'], Symbol.for('_'), '_add'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^-$', 'g'], Symbol.for('_'), '_sub'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\*$', 'g'], Symbol.for('_'), '_mul'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^/$', 'g'], Symbol.for('_'), '_div'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '%', 'g'], Symbol.for('_'), ''], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '/', 'g'], Symbol.for('_'), '-'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '!', 'g'], Symbol.for('_'), '-x'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ':', 'g'], Symbol.for('_'), '-'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '->', 'g'], Symbol.for('_'), '-to-'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\+', 'g'], Symbol.for('_'), '_'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\*$', 'g'], Symbol.for('_'), '-star'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\*', 'g'], Symbol.for('_'), 'star-']]], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '-', 'g'], Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\?', 'g'], Symbol.for('result'), '-p']]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\?', 'g'], Symbol.for('result'), 'p']]]], Symbol.for('result')];

/**
 * Whether something is an equality expression.
 */
function isEqualityExpression(exp: any, env: any): any {
  return formp(exp, eqp_, env) || formp(exp, eqvp_, env) || formp(exp, equalp_, env);
}

isEqualityExpression.lispSource = [Symbol.for('define'), [Symbol.for('is-equality-expression'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('eq?_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('eqv?_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('equal?_'), Symbol.for('env')]]];

/**
 * Whether something is a `let` or `let*` expression.
 */
function isLetExpression(exp: any, env: any): any {
  return formp(exp, letStar_, env);
}

isLetExpression.lispSource = [Symbol.for('define'), [Symbol.for('is-let-expression'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('let-star_'), Symbol.for('env')]];

/**
 * Compile a `(for ...)` expression.
 */
function compileFor(node: any, env: any, options: any = {}): any {
  // TODO: Implement `compile-js-for` and implement this
  // in terms of that?
  const inheritedOptions: any = {
    ...options
  };
  let bindings: any = inheritedOptions['bindings'];
  bindings = extendEnvironment(new LispEnvironment(), bindings);
  inheritedOptions['bindings'] = bindings;
  const language: any = options['language'];
  const declsNode: any = node.get(1);
  const decls: any = declsNode.getValue();
  const bodyNodes: any = node.drop(2);
  let bodyNode: any = beginWrapRoseSmart1(bodyNodes);
  const decl1Node: any = declsNode.get(0);
  const decl1: any = decl1Node.getValue();
  const symNode: any = decl1Node.get(0);
  const symExp: any = symNode.getValue();
  const valsNode: any = decl1Node.get(1);
  const valsExp: any = valsNode.getValue();
  if (formp(valsExp, range_, env)) {
    let start: any = (valsExp.length >= 2) ? ((Array.isArray(valsExp) && (valsExp.length >= 3) && (valsExp[valsExp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(valsExp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = valsExp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = valsExp[valsExp.length - 1];
        } else {
          result = valsExp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : valsExp[1]) : undefined;
    let end: any = (valsExp.length >= 3) ? ((Array.isArray(valsExp) && (valsExp.length >= 3) && (valsExp[valsExp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(valsExp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 2;
      let result: any = valsExp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = valsExp[valsExp.length - 1];
        } else {
          result = valsExp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : valsExp[2]) : undefined;
    let step: any = (4 >= valsExp.length) ? ((Array.isArray(valsExp) && (valsExp.length >= 3) && (valsExp[valsExp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(valsExp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 3;
      let result: any = valsExp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = valsExp[valsExp.length - 1];
        } else {
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
      const startVar: any = Symbol('_start');
      const endVar: any = Symbol('_end');
      const stepVar: any = Symbol('_step');
      return compileRose(makeRose([Symbol.for('let'), [...(Array.isArray(start) ? [[startVar, start]] : []), ...(Array.isArray(end) ? [[endVar, end]] : []), ...(Array.isArray(step) ? [[stepVar, step]] : [])], [Symbol.for('for'), [[symExp, [Symbol.for('range'), Array.isArray(start) ? startVar : start, Array.isArray(end) ? endVar : end, ...(step ? (Array.isArray(step) ? [stepVar] : [step]) : [])]]], ...bodyNodes]], node), env, options);
    }
    // Otherwise, proceed to create a `for` loop.
    step = step || 1;
    const init: any = compileStatement(makeRose([Symbol.for('define'), symExp, start]), env, inheritedOptions);
    let test: any;
    let update: any;
    if (Number.isFinite(step)) {
      if (step < 0) {
        test = compileExpression(makeRose([Symbol.for('>'), symExp, end]), env, inheritedOptions);
        update = compileStatement(makeRose([Symbol.for('set!'), symExp, [Symbol.for('-'), symExp, Math.abs(step)]]), env, inheritedOptions);
      } else {
        test = compileExpression(makeRose([Symbol.for('<'), symExp, end]), env, inheritedOptions);
        update = compileStatement(makeRose([Symbol.for('set!'), symExp, [Symbol.for('+'), symExp, step]]), env, inheritedOptions);
      }
    } else {
      test = compileExpression(makeRose([Symbol.for('if'), [Symbol.for('<'), step, 0], [Symbol.for('>'), symExp, end], [Symbol.for('<'), symExp, end]]), env, inheritedOptions);
      update = compileStatement(makeRose([Symbol.for('set!'), symExp, [Symbol.for('+'), symExp, step]]), env, inheritedOptions);
    }
    if (estreeTypeP(update, 'ExpressionStatement')) {
      update = update.expression;
    }
    const body: any = wrapInBlockStatement(compileStatement(bodyNode, env, inheritedOptions));
    return new ForStatement(init, test, update, body);
  } else {
    let left: any = compileExpression(makeRose([Symbol.for('define'), symExp]), env, inheritedOptions);
    let right: any = compileExpression(valsNode, env, inheritedOptions);
    const body: any = wrapInBlockStatement(compileStatement(bodyNode, env, inheritedOptions));
    return new ForOfStatement(left, right, body);
  }
}

compileFor.lispSource = [Symbol.for('define'), [Symbol.for('compile-for'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('decls-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('decls'), [Symbol.for('send'), Symbol.for('decls-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('body-node'), [Symbol.for('begin-wrap-rose-smart-1'), Symbol.for('body-nodes')]], [Symbol.for('define'), Symbol.for('decl1-node'), [Symbol.for('send'), Symbol.for('decls-node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('decl1'), [Symbol.for('send'), Symbol.for('decl1-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('sym-node'), [Symbol.for('send'), Symbol.for('decl1-node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('sym-exp'), [Symbol.for('send'), Symbol.for('sym-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('vals-node'), [Symbol.for('send'), Symbol.for('decl1-node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('vals-exp'), [Symbol.for('send'), Symbol.for('vals-node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('form?'), Symbol.for('vals-exp'), Symbol.for('range_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('start'), [Symbol.for('if'), [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('vals-exp')], 2], [Symbol.for('second'), Symbol.for('vals-exp')], Symbol.for('undefined')]], [Symbol.for('define'), Symbol.for('end'), [Symbol.for('if'), [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('vals-exp')], 3], [Symbol.for('third'), Symbol.for('vals-exp')], Symbol.for('undefined')]], [Symbol.for('define'), Symbol.for('step'), [Symbol.for('if'), [Symbol.for('>='), 4, [Symbol.for('array-list-length'), Symbol.for('vals-exp')]], [Symbol.for('fourth'), Symbol.for('vals-exp')], Symbol.for('undefined')]], [Symbol.for('set!'), Symbol.for('start'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('end'), Symbol.for('undefined')], 0, Symbol.for('start')]], [Symbol.for('set!'), Symbol.for('end'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('end'), Symbol.for('undefined')], Symbol.for('start'), Symbol.for('end')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('array?'), Symbol.for('start')], [Symbol.for('array?'), Symbol.for('end')], [Symbol.for('array?'), Symbol.for('step')]], [Symbol.for('define'), Symbol.for('start-var'), [Symbol.for('gensym'), '_start']], [Symbol.for('define'), Symbol.for('end-var'), [Symbol.for('gensym'), '_end']], [Symbol.for('define'), Symbol.for('step-var'), [Symbol.for('gensym'), '_step']], [Symbol.for('return'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('start')], [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('start-var')], [Symbol.for('unquote'), Symbol.for('start')]]]], [Symbol.for('quote'), []]]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('end')], [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('end-var')], [Symbol.for('unquote'), Symbol.for('end')]]]], [Symbol.for('quote'), []]]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('step')], [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('step-var')], [Symbol.for('unquote'), Symbol.for('step')]]]], [Symbol.for('quote'), []]]]], [Symbol.for('for'), [[[Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('range'), [Symbol.for('unquote'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('start')], Symbol.for('start-var'), Symbol.for('start')]], [Symbol.for('unquote'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('end')], Symbol.for('end-var'), Symbol.for('end')]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), Symbol.for('step'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('step')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('step-var')]]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('step')]]]], [Symbol.for('quote'), []]]]]]], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('step'), [Symbol.for('or'), Symbol.for('step'), 1]], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('start')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('define'), Symbol.for('test')], [Symbol.for('define'), Symbol.for('update')], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('step')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('step'), 0], [Symbol.for('set!'), Symbol.for('test'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('>'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('Math'), Symbol.for('abs'), Symbol.for('step')]]]]]], Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('test'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('step')]]]]], Symbol.for('env'), Symbol.for('inherited-options')]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('test'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('if'), [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('step')], 0], [Symbol.for('>'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]], [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('step')]]]]], Symbol.for('env'), Symbol.for('inherited-options')]]]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('update'), 'ExpressionStatement'], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('update')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-statement'), Symbol.for('body-node'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('new'), Symbol.for('ForStatement'), Symbol.for('init'), Symbol.for('test'), Symbol.for('update'), Symbol.for('body')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('left'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('sym-exp')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('compile-expression'), Symbol.for('vals-node'), Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-statement'), Symbol.for('body-node'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('new'), Symbol.for('ForOfStatement'), Symbol.for('left'), Symbol.for('right'), Symbol.for('body')]]]];

/**
 * Compile a `(break)` expression.
 */
function compileBreak(node: any, env: any, options: any = {}): any {
  return new BreakStatement((node.size() > 1) ? compileExpression(node.get(1), env, options) : null);
}

compileBreak.lispSource = [Symbol.for('define'), [Symbol.for('compile-break'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('BreakStatement'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('js/null')]]];

/**
 * Compile a `(continue)` expression.
 */
function compileContinue(node: any, env: any, options: any = {}): any {
  return new ContinueStatement((node.size() > 1) ? compileExpression(node.get(1), env, options) : null);
}

compileContinue.lispSource = [Symbol.for('define'), [Symbol.for('compile-continue'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('ContinueStatement'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('js/null')]]];

/**
 * Compile a `(js/typeof ...)` expression.
 */
function compileJsTypeof(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new UnaryExpression('typeof', true, compileExpression(node.get(1), env, options)), options);
}

compileJsTypeof.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-typeof'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('UnaryExpression'), 'typeof', Symbol.for('#t'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

/**
 * Compile a `(js/instanceof? ...)` expression.
 */
function compileJsInstanceof(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new BinaryExpression('instanceof', compileExpression(node.get(1), env, options), compileExpression(node.get(2), env, options)), options);
}

compileJsInstanceof.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-instanceof'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('BinaryExpression'), 'instanceof', [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

/**
 * Compile a `(js/in ...)` expression.
 */
function compileJsIn(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new BinaryExpression('in', compileExpression(node.get(1), env, options), compileExpression(node.get(2), env, options)), options);
}

compileJsIn.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-in'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('BinaryExpression'), 'in', [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

/**
 * Compile a `(new ...)` expression.
 */
function compileNew(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new NewExpression(compileExpression(node.get(1), env, options), node.drop(2).map(function (x: any): any {
    return compileExpression(x, env, options);
  })), options);
}

compileNew.lispSource = [Symbol.for('define'), [Symbol.for('compile-new'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('NewExpression'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]], Symbol.for('options')]];

/**
 * Compile a `(js/do-while ...)` expression.
 */
function compileJsDoWhile(node: any, env: any, options: any = {}): any {
  const body: any = node.get(1);
  let test: any = node.get(2);
  return new DoWhileStatement(compileExpression(test, env, options), wrapInBlockStatementSmart(compileStatementOrReturnStatement(body, env, options)));
}

compileJsDoWhile.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-do-while'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('new'), Symbol.for('DoWhileStatement'), [Symbol.for('compile-expression'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(js/while ...)` expression.
 */
function compileJsWhile(node: any, env: any, options: any = {}): any {
  let test: any = node.get(1);
  const body: any = beginWrapRose(node.drop(2));
  return new WhileStatement(compileExpression(test, env, options), wrapInBlockStatementSmart(compileStatementOrReturnStatement(body, env, options)));
}

compileJsWhile.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-while'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('begin-wrap-rose'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]], [Symbol.for('new'), Symbol.for('WhileStatement'), [Symbol.for('compile-expression'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(yield ...)` expression.
 */
function compileYield(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new YieldExpression((node.size() > 1) ? compileExpression(node.get(1), env, options) : null), options);
}

compileYield.lispSource = [Symbol.for('define'), [Symbol.for('compile-yield'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('YieldExpression'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('js/null')]], Symbol.for('options')]];

/**
 * Compile a `(throw ...)` expression.
 */
function compileThrow(node: any, env: any, options: any = {}): any {
  return new ThrowStatement(compileExpression(node.get(1), env, options));
}

compileThrow.lispSource = [Symbol.for('define'), [Symbol.for('compile-throw'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('ThrowStatement'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]];

/**
 * Compile a `(js/delete ...)` expression.
 */
function compileJsDelete(node: any, env: any, options: any = {}): any {
  return new UnaryExpression('delete', true, compileExpression(node.get(1), env, options));
}

compileJsDelete.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-delete'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('UnaryExpression'), 'delete', Symbol.for('#t'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]];

/**
 * Compile a `(return ...)` expression.
 */
function compileReturn(node: any, env: any, options: any = {}): any {
  return new ReturnStatement((node.size() > 1) ? compileExpression(node.get(1), env, options) : null);
}

compileReturn.lispSource = [Symbol.for('define'), [Symbol.for('compile-return'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('ReturnStatement'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('js/null')]]];

/**
 * Compile a `(js/async ...)` expression.
 */
function compileJsAsync(node: any, env: any, options: any = {}): any {
  let result: any = compileExpression(node.get(1), env, options);
  if (estreeTypeP(result, 'FunctionDeclaration') || estreeTypeP(result, 'FunctionExpression') || estreeTypeP(result, 'ArrowFunctionExpression')) {
    result.async = true;
    result.returnType = new TSTypeReference(new Identifier('Promise'), new TSTypeParameterInstantiation([new TSAnyKeyword()]));
  }
  return makeExpressionOrStatement(result, options);
}

compileJsAsync.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-async'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('estree-type?'), Symbol.for('result'), 'FunctionDeclaration'], [Symbol.for('estree-type?'), Symbol.for('result'), 'FunctionExpression'], [Symbol.for('estree-type?'), Symbol.for('result'), 'ArrowFunctionExpression']], [Symbol.for('set-field!'), Symbol.for('async'), Symbol.for('result'), Symbol.for('#t')], [Symbol.for('set-field!'), Symbol.for('returnType'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('TSTypeReference'), [Symbol.for('new'), Symbol.for('Identifier'), 'Promise'], [Symbol.for('new'), Symbol.for('TSTypeParameterInstantiation'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];

/**
 * Compile a `(js/await ...)` expression.
 */
function compileJsAwait(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new AwaitExpression(compileExpression(node.get(1), env, options)), options);
}

compileJsAwait.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-await'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('AwaitExpression'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

/**
 * Compile a `(string-append ...)` expression.
 */
function compileStringAppend(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  if (exp.length <= 0) {
    return compileRose('', env, options);
  } else if (exp.length === 2) {
    return compileRose(node.get(1), env, options);
  } else {
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
function compileDefineClass(node: any, env: any, options: any = {}): any {
  return compileClass(node, env, options);
}

compileDefineClass.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-class'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-class'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a `(class ...)` expression.
 */
function compileClass(node: any, env: any, options: any = {}): any {
  const inheritedOptions: any = {
    ...options
  };
  const classNameNode: any = node.get(1);
  const className: any = classNameNode.getValue();
  let superClass: any = null;
  let id: any = (typeof className === 'symbol') ? new Identifier(printEstree(compileExpression(classNameNode, env, inheritedOptions), inheritedOptions)) : null;
  let bodyNode: any = (id === null) ? sliceRose(node, 1) : sliceRose(node, 2);
  let bodyExp: any = bodyNode.getValue();
  let bindings: any = inheritedOptions['bindings'];
  bindings = extendEnvironment(new LispEnvironment(), bindings);
  inheritedOptions['bindings'] = bindings;
  bindings.setLocal(Symbol.for('super'), true, 'variable');
  if (Array.isArray(bodyExp[0]) && !formp(bodyExp[0], define_, env)) {
    const superClassesNode: any = bodyNode.get(0);
    const superClasses: any = superClassesNode.getValue();
    bodyNode = sliceRose(bodyNode, 1);
    bodyExp = bodyNode.getValue();
    if (superClasses.length > 0) {
      superClass = new Identifier(printEstree(compileExpression(makeRose(superClasses[0]), env, inheritedOptions), inheritedOptions));
    }
  }
  const bodyDeclarations: any = [];
  const accessibilities: any = new Map();
  for (let x of bodyNode.getNodes()) {
    let exp: any = x.getValue();
    if (taggedListP(exp, Symbol.for('public'))) {
      accessibilities.set((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        let x1: any = lastCdr(exp);
        return Array.isArray(x1) && (x1.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
            result = exp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : exp[1], 'public');
    } else if (taggedListP(exp, Symbol.for('private'))) {
      accessibilities.set((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        let x1: any = lastCdr(exp);
        return Array.isArray(x1) && (x1.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
            result = exp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : exp[1], 'private');
    } else {
      const isInitialized: any = exp.length >= 3;
      let id: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        let x1: any = lastCdr(exp);
        return Array.isArray(x1) && (x1.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
            result = exp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : exp[1];
      const isMethod: any = Array.isArray(id);
      if (isMethod) {
        id = id[0];
      }
      const idNode: any = isMethod ? x.get(1).get(0) : x.get(1);
      let accessibility: any = accessibilities.has(id) ? accessibilities.get(id) : (taggedListP(exp, Symbol.for('define/public')) ? 'public' : 'private');
      const isGenerator: any = taggedListP(exp, Symbol.for('define/generator'));
      const isConstructor: any = isMethod && (((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        let x1: any = lastCdr(exp);
        return Array.isArray(x1) && (x1.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
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
      const returnType: any = isConstructor ? 'void' : undefined;
      const isComputed: any = typeof id !== 'symbol';
      const idCompiled: any = isComputed ? compileExpression(idNode, env, inheritedOptions) : compileSymbol(idNode, env, makeExpressionOptions(inheritedOptions));
      const initCompiled: any = !isInitialized ? undefined : (isMethod ? compileJsFunction(defineToLambda(x, {
        curried: false
      }), env, makeExpressionOptions(inheritedOptions), {
        generator: isGenerator,
        returnType: returnType
      }) : compileExpression(x.get(2), env, makeExpressionOptions(inheritedOptions)));
      if (isMethod) {
        const kind: any = isConstructor ? 'constructor' : 'method';
        let methodDefinition: any = new MethodDefinition(idCompiled, initCompiled, kind, false, false, isComputed, accessibility);
        methodDefinition = transferAndCompileComments(x, methodDefinition, inheritedOptions);
        bodyDeclarations.push(methodDefinition);
      } else {
        let propertyDefinition: any = new PropertyDefinition(idCompiled, initCompiled, false, accessibility);
        propertyDefinition = transferAndCompileComments(x, propertyDefinition, inheritedOptions);
        bodyDeclarations.push(propertyDefinition);
      }
    }
  }
  const body: any = new ClassBody(bodyDeclarations);
  if (id === null) {
    return new ClassExpression(body, superClass);
  } else {
    return new ClassDeclaration(id, body, superClass);
  }
}

compileClass.lispSource = [Symbol.for('define'), [Symbol.for('compile-class'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('class-name-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('class-name'), [Symbol.for('send'), Symbol.for('class-name-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('super-class'), Symbol.for('js/null')], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('if'), [Symbol.for('symbol?'), Symbol.for('class-name')], [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), Symbol.for('class-name-node'), Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]], Symbol.for('js/null')]], [Symbol.for('define'), Symbol.for('body-node'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('js/null')], [Symbol.for('slice-rose'), Symbol.for('node'), 1], [Symbol.for('slice-rose'), Symbol.for('node'), 2]]], [Symbol.for('define'), Symbol.for('body-exp'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'bindings']], [Symbol.for('set!'), Symbol.for('bindings'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('oset!'), Symbol.for('inherited-options'), 'bindings', Symbol.for('bindings')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), [Symbol.for('quote'), Symbol.for('super')], Symbol.for('#t'), 'variable'], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('array?'), [Symbol.for('first'), Symbol.for('body-exp')]], [Symbol.for('not'), [Symbol.for('form?'), [Symbol.for('first'), Symbol.for('body-exp')], Symbol.for('define_'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('super-classes-node'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('super-classes'), [Symbol.for('send'), Symbol.for('super-classes-node'), Symbol.for('get-value')]], [Symbol.for('set!'), Symbol.for('body-node'), [Symbol.for('slice-rose'), Symbol.for('body-node'), 1]], [Symbol.for('set!'), Symbol.for('body-exp'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get-value')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('super-classes')], 0], [Symbol.for('set!'), Symbol.for('super-class'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('first'), Symbol.for('super-classes')]], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]], [Symbol.for('define'), Symbol.for('body-declarations'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('accessibilities'), [Symbol.for('make-hash')]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('public')]], [Symbol.for('hash-set!'), Symbol.for('accessibilities'), [Symbol.for('second'), Symbol.for('exp')], 'public']], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('private')]], [Symbol.for('hash-set!'), Symbol.for('accessibilities'), [Symbol.for('second'), Symbol.for('exp')], 'private']], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('is-initialized'), [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('is-method'), [Symbol.for('array?'), Symbol.for('id')]], [Symbol.for('when'), Symbol.for('is-method'), [Symbol.for('set!'), Symbol.for('id'), [Symbol.for('first'), Symbol.for('id')]]], [Symbol.for('define'), Symbol.for('id-node'), [Symbol.for('if'), Symbol.for('is-method'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]]], [Symbol.for('define'), Symbol.for('accessibility'), [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('accessibilities'), Symbol.for('id')], [Symbol.for('hash-ref'), Symbol.for('accessibilities'), Symbol.for('id')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define/public')]], 'public'], [Symbol.for('else'), 'private']]], [Symbol.for('define'), Symbol.for('is-generator'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define/generator')]]], [Symbol.for('define'), Symbol.for('is-constructor'), [Symbol.for('and'), Symbol.for('is-method'), [Symbol.for('>'), [Symbol.for('array-list-length'), [Symbol.for('second'), Symbol.for('exp')]], 0], [Symbol.for('eq?'), Symbol.for('id'), [Symbol.for('quote'), Symbol.for('constructor')]]]], [Symbol.for('when'), Symbol.for('is-constructor'), [Symbol.for('set!'), Symbol.for('accessibility'), 'public']], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('if'), Symbol.for('is-constructor'), 'void', Symbol.for('undefined')]], [Symbol.for('define'), Symbol.for('is-computed'), [Symbol.for('not'), [Symbol.for('symbol?'), Symbol.for('id')]]], [Symbol.for('define'), Symbol.for('id-compiled'), [Symbol.for('if'), Symbol.for('is-computed'), [Symbol.for('compile-expression'), Symbol.for('id-node'), Symbol.for('env'), Symbol.for('inherited-options')], [Symbol.for('compile-symbol'), Symbol.for('id-node'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('inherited-options')]]]], [Symbol.for('define'), Symbol.for('init-compiled'), [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('is-initialized')], Symbol.for('undefined')], [Symbol.for('is-method'), [Symbol.for('compile-js-function'), [Symbol.for('define->lambda'), Symbol.for('x'), [Symbol.for('js-obj'), 'curried', Symbol.for('#f')]], Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('inherited-options')], [Symbol.for('js-obj'), 'generator', Symbol.for('is-generator'), 'returnType', Symbol.for('return-type')]]], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 2], Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('inherited-options')]]]]], [Symbol.for('cond'), [Symbol.for('is-method'), [Symbol.for('define'), Symbol.for('kind'), [Symbol.for('if'), Symbol.for('is-constructor'), 'constructor', 'method']], [Symbol.for('define'), Symbol.for('method-definition'), [Symbol.for('new'), Symbol.for('MethodDefinition'), Symbol.for('id-compiled'), Symbol.for('init-compiled'), Symbol.for('kind'), Symbol.for('#f'), Symbol.for('#f'), Symbol.for('is-computed'), Symbol.for('accessibility')]], [Symbol.for('set!'), Symbol.for('method-definition'), [Symbol.for('transfer-and-compile-comments'), Symbol.for('x'), Symbol.for('method-definition'), Symbol.for('inherited-options')]], [Symbol.for('push-right!'), Symbol.for('body-declarations'), Symbol.for('method-definition')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('property-definition'), [Symbol.for('new'), Symbol.for('PropertyDefinition'), Symbol.for('id-compiled'), Symbol.for('init-compiled'), Symbol.for('#f'), Symbol.for('accessibility')]], [Symbol.for('set!'), Symbol.for('property-definition'), [Symbol.for('transfer-and-compile-comments'), Symbol.for('x'), Symbol.for('property-definition'), Symbol.for('inherited-options')]], [Symbol.for('push-right!'), Symbol.for('body-declarations'), Symbol.for('property-definition')]]]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('new'), Symbol.for('ClassBody'), Symbol.for('body-declarations')]], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('js/null')], [Symbol.for('new'), Symbol.for('ClassExpression'), Symbol.for('body'), Symbol.for('super-class')], [Symbol.for('new'), Symbol.for('ClassDeclaration'), Symbol.for('id'), Symbol.for('body'), Symbol.for('super-class')]]];

/**
 * Compile a `(js-obj ...)` expression.
 */
function compileJsObj(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  const properties: any = [];
  const _end: any = exp.length;
  for (let i: any = 1; i < _end; i = i + 2) {
    const keyNode: any = node.get(i);
    const keyValue: any = keyNode.getValue();
    let compiledKey: any = compileExpression(keyNode, env, options);
    const compiledValue: any = compileExpression(node.get(i + 1), env, options);
    const computed: any = typeof keyNode.getValue() !== 'string';
    let match: any;
    if ((typeof keyValue === 'string') && keyValue.match(new RegExp('^[a-z]+$', 'i'))) {
      compiledKey = new Identifier(keyValue);
    }
    properties.push(new Property(compiledKey, compiledValue, computed));
  }
  return makeExpressionOrStatement(new ObjectExpression(properties), options);
}

compileJsObj.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-obj'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('exp')], 2]]], [Symbol.for('define'), Symbol.for('key-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('key-value'), [Symbol.for('send'), Symbol.for('key-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('compiled-key'), [Symbol.for('compile-expression'), Symbol.for('key-node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('compiled-value'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('not'), [Symbol.for('string?'), [Symbol.for('send'), Symbol.for('key-node'), Symbol.for('get-value')]]]], [Symbol.for('define'), Symbol.for('match')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('string?'), Symbol.for('key-value')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^[a-z]+$', 'i'], Symbol.for('key-value')]], [Symbol.for('set!'), Symbol.for('compiled-key'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('key-value')]]], [Symbol.for('push-right!'), Symbol.for('properties'), [Symbol.for('new'), Symbol.for('Property'), Symbol.for('compiled-key'), Symbol.for('compiled-value'), Symbol.for('computed')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ObjectExpression'), Symbol.for('properties')], Symbol.for('options')]];

/**
 * Compile a `(js-obj-append ...)` expression.
 */
function compileJsObjAppend(node: any, env: any, options: any = {}): any {
  const args: any = node.drop(1);
  const properties: any = [];
  for (let arg of args) {
    let exp: any = compileExpression(arg, env, options);
    if (exp instanceof ObjectExpression) {
      for (let prop of exp.properties) {
        properties.push(prop);
      }
    } else {
      properties.push(new SpreadElement(exp));
    }
  }
  return makeExpressionOrStatement(new ObjectExpression(properties), options);
}

compileJsObjAppend.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-obj-append'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('compile-expression'), Symbol.for('arg'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('ObjectExpression')], [Symbol.for('for'), [[Symbol.for('prop'), [Symbol.for('get-field'), Symbol.for('properties'), Symbol.for('exp')]]], [Symbol.for('push-right!'), Symbol.for('properties'), Symbol.for('prop')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('properties'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('exp')]]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ObjectExpression'), Symbol.for('properties')], Symbol.for('options')]];

/**
 * Compile a `(js/tag ...)` expression.
 */
function compileJsTaggedTemplate(node: any, env: any, options: any = {}): any {
  const tag: any = node.get(1);
  const tagCompiled: any = compileRose(tag, env, options);
  const str: any = node.get(2);
  const strExp: any = str.getValue();
  return new TaggedTemplateExpression(tagCompiled, new TemplateLiteral([new TemplateElement(true, strExp)]));
}

compileJsTaggedTemplate.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-tagged-template'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('tag'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('tag-compiled'), [Symbol.for('compile-rose'), Symbol.for('tag'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('str-exp'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('get-value')]], [Symbol.for('new'), Symbol.for('TaggedTemplateExpression'), Symbol.for('tag-compiled'), [Symbol.for('new'), Symbol.for('TemplateLiteral'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('TemplateElement'), Symbol.for('#t'), Symbol.for('str-exp')]]]]];

/**
 * Compile an `(append ...)` expression.
 */
function compileAppend(node: any, env: any, options: any = {}): any {
  const elements: any = [];
  for (let x of node.drop(1)) {
    let el: any = compileExpression(x, env, options);
    if (estreeTypeP(el, 'ArrayExpression')) {
      if (el.elements.length === 0) {
      } else if (el.elements.length === 1) {
        // Ignore empty arrays.
        // Unwrap singleton arrays.
        elements.push(el.elements[0]);
      } else {
        elements.push(new SpreadElement(el));
      }
    } else {
      elements.push(new SpreadElement(el));
    }
  }
  return makeExpressionOrStatement(new ArrayExpression(elements), options);
}

compileAppend.lispSource = [Symbol.for('define'), [Symbol.for('compile-append'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('elements'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]], [Symbol.for('define'), Symbol.for('el'), [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('el'), 'ArrayExpression'], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('el')]], 0]], [[Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('el')]], 1], [Symbol.for('push-right!'), Symbol.for('elements'), [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('el')], 0]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('elements'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('el')]]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('elements'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('el')]]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ArrayExpression'), Symbol.for('elements')], Symbol.for('options')]];

/**
 * Compile a `(js/try ...)` expression.
 */
function compileJsTry(node: any, env: any, options: any = {}): any {
  const bodyExps: any = [];
  let catchClause: any = null;
  let finallyClause: any = null;
  for (let x of node.drop(1)) {
    if (taggedListP(x, Symbol.for('catch'))) {
      catchClause = x;
    } else if (taggedListP(x, Symbol.for('finally'))) {
      finallyClause = x;
    } else {
      bodyExps.push(x);
    }
  }
  const block: any = wrapInBlockStatementSmart(compileStatementOrReturnStatement(makeRose([Symbol.for('begin'), ...bodyExps]), env, options));
  let handler: any = null;
  if (catchClause) {
    // TODO: Permit destructuring.
    let param: any = catchClause.get(1);
    const paramExp: any = param.getValue();
    const paramCompiled: any = (paramExp === Symbol.for('_')) ? null : compileExpression(param, env, options);
    const body: any = makeRose([Symbol.for('begin'), ...catchClause.drop(2)]);
    const bodyCompiled: any = compileStatementOrReturnStatement(body, env, options);
    handler = new CatchClause(paramCompiled, bodyCompiled);
  }
  const finalizer: any = finallyClause ? wrapInBlockStatementSmart(compileStatementOrReturnStatement(makeRose([Symbol.for('begin'), ...finallyClause.drop(1)]), env, options)) : null;
  return makeExpressionOrStatement(new TryStatement(block, handler, finalizer), options);
}

compileJsTry.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-try'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('body-exps'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('catch-clause'), Symbol.for('js/null')], [Symbol.for('define'), Symbol.for('finally-clause'), Symbol.for('js/null')], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('catch')]], [Symbol.for('set!'), Symbol.for('catch-clause'), Symbol.for('x')]], [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('finally')]], [Symbol.for('set!'), Symbol.for('finally-clause'), Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('body-exps'), Symbol.for('x')]]]], [Symbol.for('define'), Symbol.for('block'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body-exps')]]]], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('handler'), Symbol.for('js/null')], [Symbol.for('when'), Symbol.for('catch-clause'), [Symbol.for('define'), Symbol.for('param'), [Symbol.for('send'), Symbol.for('catch-clause'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('param-exp'), [Symbol.for('send'), Symbol.for('param'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('param-compiled'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('param-exp'), [Symbol.for('quote'), Symbol.for('_')]], Symbol.for('js/null'), [Symbol.for('compile-expression'), Symbol.for('param'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('catch-clause'), Symbol.for('drop'), 2]]]]]], [Symbol.for('define'), Symbol.for('body-compiled'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('handler'), [Symbol.for('new'), Symbol.for('CatchClause'), Symbol.for('param-compiled'), Symbol.for('body-compiled')]]], [Symbol.for('define'), Symbol.for('finalizer'), [Symbol.for('if'), Symbol.for('finally-clause'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('finally-clause'), Symbol.for('drop'), 1]]]]], Symbol.for('env'), Symbol.for('options')]], Symbol.for('js/null')]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('TryStatement'), Symbol.for('block'), Symbol.for('handler'), Symbol.for('finalizer')], Symbol.for('options')]];

/**
 * Compile a `(push-left! ...)` expression.
 */
function compilePushLeft(node: any, env: any, options: any = {}): any {
  // `.unshift()` returns the length of the array, while `push!()`
  // returns the list.
  return compilePushHelper(makeRose([Symbol.for('send'), node.get(1), Symbol.for('unshift'), node.get(2)]), makeRose([[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('lst')], node.get(1), node.get(2)]), node, env, options);
}

compilePushLeft.lispSource = [Symbol.for('define'), [Symbol.for('compile-push-left'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-push-helper'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], Symbol.for('unshift'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('lst')], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a `(push-right! ...)` expression.
 */
function compilePushRight(node: any, env: any, options: any = {}): any {
  // `.push()` returns the length of the array, while `push-right!()`
  // returns the list.
  return compilePushHelper(makeRose([Symbol.for('send'), node.get(1), Symbol.for('push'), node.get(2)]), makeRose([[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('push'), Symbol.for('x')], Symbol.for('lst')], node.get(1), node.get(2)]), node, env, options);
}

compilePushRight.lispSource = [Symbol.for('define'), [Symbol.for('compile-push-right'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-push-helper'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], Symbol.for('push'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('push'), Symbol.for('x')], Symbol.for('lst')], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Helper function for `compile-push-left`
 * and `compile-push-right`.
 */
function compilePushHelper(statementExp: any, expressionExp: any, node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  if (expressionType === 'return') {
    // When compiled as a return statement, create a program fragment
    // if the list expression is a symbol. Otherwise, reuse the
    // expression logic and wrap in `(return ...)`.
    if (typeof node.get(1).getValue() === 'symbol') {
      return new Program([compileStatement(statementExp, env, options), compileReturnStatement(node.get(1), env, options)]);
    } else {
      return compileRose(makeRose([Symbol.for('return'), node]), env, options);
    }
  } else if (expressionType === 'statement') {
    // When compiled as a statement, the return
    // type does not matter.
    return compileStatementOrReturnStatement(statementExp, env, options);
  } else if (typeof node.get(1).getValue() === 'symbol') {
    // When compiled as an expression, we can use the comma
    // operator if the list expression is a symbol.
    return new SequenceExpression([compileExpression(statementExp, env, options), compileExpression(node.get(1), env, options)]);
  } else {
    // In more complicated cases, we compile to
    // a lambda expression.
    return compileExpression(expressionExp, env, options);
  }
}

compilePushHelper.lispSource = [Symbol.for('define'), [Symbol.for('compile-push-helper'), Symbol.for('statement-exp'), Symbol.for('expression-exp'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('cond'), [[Symbol.for('symbol?'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('get-value')]], [Symbol.for('new'), Symbol.for('Program'), [Symbol.for('list'), [Symbol.for('compile-statement'), Symbol.for('statement-exp'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-return-statement'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('return'), [Symbol.for('unquote'), Symbol.for('node')]]]], Symbol.for('env'), Symbol.for('options')]]]], [[Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('compile-statement-or-return-statement'), Symbol.for('statement-exp'), Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('symbol?'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('get-value')]], [Symbol.for('new'), Symbol.for('SequenceExpression'), [Symbol.for('list'), [Symbol.for('compile-expression'), Symbol.for('statement-exp'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('compile-expression'), Symbol.for('expression-exp'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(define-macro ...)` expression.
 */
function compileDefineMacro(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  const nameAndArgs: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1];
  let name: any = nameAndArgs[0];
  const macroFnForm: any = defineMacroToLambdaForm(exp);
  const args: any = (Array.isArray(macroFnForm) && (macroFnForm.length >= 3) && (macroFnForm[macroFnForm.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(macroFnForm);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = macroFnForm;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = macroFnForm[macroFnForm.length - 1];
      } else {
        result = macroFnForm.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : macroFnForm[1];
  const body: any = macroFnForm.slice(2);
  let bindings: any = options['bindings'];
  let result: any = compileRose(transferComments(node, makeRose([Symbol.for('begin'), [Symbol.for('define'), [name, ...args], ...body], [Symbol.for('set-field!'), Symbol.for('lispMacro'), name, Symbol.for('#t')]], node)), env, options);
  if (bindings) {
    bindings.setLocal(name, true, 'macro');
  }
  return result;
}

compileDefineMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-define-macro'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('macro-fn-form'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('second'), Symbol.for('macro-fn-form')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('macro-fn-form'), 2]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('oget'), Symbol.for('options'), 'bindings']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('unquote-splicing'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('set-field!'), Symbol.for('lispMacro'), [Symbol.for('unquote'), Symbol.for('name')], Symbol.for('#t')]]], Symbol.for('node')]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('when'), Symbol.for('bindings'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('name'), Symbol.for('#t'), 'macro']], Symbol.for('result')];

/**
 * Compiler macro for `(make-hash ...)` expressions.
 */
function compileMakeHashMacro(exp: any, env: any): any {
  const [assocs]: any[] = exp.slice(1);
  if (assocs) {
    if ((taggedListP(assocs, Symbol.for('quasiquote')) || taggedListP(assocs, Symbol.for('quote'))) && ((): any => {
      const x: any = lastCdr((Array.isArray(assocs) && (assocs.length >= 3) && (assocs[assocs.length - 2] === Symbol.for('.')) && ((): any => {
        let x1: any = lastCdr(assocs);
        return Array.isArray(x1) && (x1.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = assocs;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = assocs[assocs.length - 1];
          } else {
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
    })() && (((Array.isArray(assocs) && (assocs.length >= 3) && (assocs[assocs.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(assocs);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = assocs;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = assocs[assocs.length - 1];
        } else {
          result = assocs.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : assocs[1]).filter(function (x: any): any {
      return !Array.isArray(x) || ((x.length === 2) && (taggedListP(x, Symbol.for('unquote')) || (taggedListP(x, Symbol.for('unquote-splicing')) && !taggedListP((Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && ((): any => {
        let x1: any = lastCdr(x);
        return Array.isArray(x1) && (x1.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = x;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = x[x.length - 1];
          } else {
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
      return [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('ann'), [assocs[0], ((Array.isArray(assocs) && (assocs.length >= 3) && (assocs[assocs.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(assocs);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = assocs;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = assocs[assocs.length - 1];
          } else {
            result = assocs.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : assocs[1]).map(function (x: any): any {
        if (taggedListP(x, Symbol.for('unquote-splicing')) && taggedListP((Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && ((): any => {
          let x1: any = lastCdr(x);
          return Array.isArray(x1) && (x1.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = x;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = x[x.length - 1];
            } else {
              result = x.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : x[1], Symbol.for('hash->list'))) {
          return cons(x[0], [[Symbol.for('send'), ((): any => {
            const lst: any = (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && ((): any => {
              let x1: any = lastCdr(x);
              return Array.isArray(x1) && (x1.length === 0);
            })()) ? ((): any => {
              let i: any = 1;
              let result: any = x;
              while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                  result = x[x.length - 1];
                } else {
                  result = x.slice(1);
                }
                i--;
              }
              if (Array.isArray(result)) {
                result = result[0];
              }
              return result;
            })() : x[1];
            if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
              let x1: any = lastCdr(lst);
              return Array.isArray(x1) && (x1.length === 0);
            })()) {
              let i: any = 1;
              let result: any = lst;
              while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                  result = lst[lst.length - 1];
                } else {
                  result = lst.slice(1);
                }
                i--;
              }
              if (Array.isArray(result)) {
                result = result[0];
              }
              return result;
            } else {
              return lst[1];
            }
          })(), Symbol.for('entries')]]);
        } else {
          return [x[0], cdr(x)];
        }
      })], Symbol.for('Any')]];
    } else {
      // If the `assocs` form is not simple, then we have map
      // `flatten` over it in order to convert a list of pairs to a
      // list of lists.
      return [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('map'), Symbol.for('flatten'), assocs]];
    }
  } else {
    return [Symbol.for('new'), Symbol.for('Map')];
  }
}

compileMakeHashMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-make-hash-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('assocs')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [Symbol.for('assocs'), [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('assocs'), [Symbol.for('quote'), Symbol.for('quasiquote')]], [Symbol.for('tagged-list?'), Symbol.for('assocs'), [Symbol.for('quote'), Symbol.for('quote')]]], [Symbol.for('list?'), [Symbol.for('second'), Symbol.for('assocs')]], [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('filter'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('array?'), Symbol.for('x')]], [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('x')], 2], [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('unquote')]], [Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]], [Symbol.for('not'), [Symbol.for('tagged-list?'), [Symbol.for('second'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('hash->list')]]]]]]]], [Symbol.for('second'), Symbol.for('assocs')]]], 0]], [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('ann'), [[Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('assocs')]], [Symbol.for('unquote'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]], [Symbol.for('tagged-list?'), [Symbol.for('second'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('hash->list')]]], [Symbol.for('cons'), [Symbol.for('first'), Symbol.for('x')], [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('second'), [Symbol.for('second'), Symbol.for('x')]]], Symbol.for('entries')]]]]], [Symbol.for('else'), [Symbol.for('list'), [Symbol.for('car'), Symbol.for('x')], [Symbol.for('cdr'), Symbol.for('x')]]]]], [Symbol.for('second'), Symbol.for('assocs')]]]], Symbol.for('Any')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('map'), Symbol.for('flatten'), [Symbol.for('unquote'), Symbol.for('assocs')]]]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('Map')]]]]];

compileMakeHashMacro.lispMacro = true;

/**
 * Compiler macro for `(hash-clear ...)` expressions.
 */
function compileHashClearMacro(exp: any, env: any): any {
  const [ht]: any[] = exp.slice(1);
  if (typeof ht === 'symbol') {
    return [Symbol.for('begin'), [Symbol.for('send'), ht, Symbol.for('clear')], ht];
  } else {
    return [[Symbol.for('lambda'), [Symbol.for('ht')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('clear')], Symbol.for('ht')], ht];
  }
}

compileHashClearMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-hash-clear-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('ht')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ht')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('ht')], Symbol.for('clear')], [Symbol.for('unquote'), Symbol.for('ht')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('ht')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('clear')], Symbol.for('ht')], [Symbol.for('unquote'), Symbol.for('ht')]]]]]];

compileHashClearMacro.lispMacro = true;

/**
 * Compiler macro for `(hash-remove! ...)` expressions.
 */
function compileHashRemoveMacro(exp: any, env: any): any {
  const [ht, key]: any[] = exp.slice(1);
  if (typeof ht === 'symbol') {
    return [Symbol.for('begin'), [Symbol.for('send'), ht, Symbol.for('delete'), key], ht];
  } else {
    return [[Symbol.for('lambda'), [Symbol.for('ht'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('delete'), Symbol.for('key')], Symbol.for('ht')], ht, key];
  }
}

compileHashRemoveMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-hash-remove-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('ht'), Symbol.for('key')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ht')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('ht')], Symbol.for('delete'), [Symbol.for('unquote'), Symbol.for('key')]], [Symbol.for('unquote'), Symbol.for('ht')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('ht'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('delete'), Symbol.for('key')], Symbol.for('ht')], [Symbol.for('unquote'), Symbol.for('ht')], [Symbol.for('unquote'), Symbol.for('key')]]]]]];

compileHashRemoveMacro.lispMacro = true;

/**
 * Compiler macro for `(hash-ref ...)` expressions.
 */
function compileHashRefMacro(exp: any, env: any): any {
  const [ht, key, failureResult]: any[] = exp.slice(1);
  if (failureResult) {
    return definitionToMacro([Symbol.for('define'), [Symbol.for('hash-ref'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')], [Symbol.for('if'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('has'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('get'), Symbol.for('key')], Symbol.for('failure-result')]], [ht, key, failureResult]);
  } else {
    return [Symbol.for('send'), ht, Symbol.for('get'), key];
  }
}

compileHashRefMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-hash-ref-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [Symbol.for('failure-result'), [Symbol.for('definition->macro'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('hash-ref'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')], [Symbol.for('if'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('has'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('get'), Symbol.for('key')], Symbol.for('failure-result')]]], [Symbol.for('list'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('ht')], Symbol.for('get'), [Symbol.for('unquote'), Symbol.for('key')]]]]]];

compileHashRefMacro.lispMacro = true;

/**
 * Compiler macro for `(map ...)` expressions.
 */
function compileMapMacro(exp: any, env: any): any {
  const [f, x]: any[] = exp.slice(1);
  // Note that `` `(send ,x map ,f) `` is too simple, as JavaScript's
  // `.map()` method calls the function with multiple arguments. This
  // can lead to unintuitive bugs in cases where the function has an
  // optional second parameter. To avoid this, we enclose `f` in a
  // unary function wrapper.
  const fExp: any = compileMapMacroHelper(f, env);
  return [Symbol.for('send'), x, Symbol.for('map'), fExp];
}

compileMapMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-map-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('f-exp'), [Symbol.for('compile-map-macro-helper'), Symbol.for('f'), Symbol.for('env')]], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('x')], Symbol.for('map'), [Symbol.for('unquote'), Symbol.for('f-exp')]]]];

compileMapMacro.lispMacro = true;

/**
 * Compiler macro for `(values ...)` expressions.
 */
function compileValuesMacro(exp: any, env: any): any {
  const args: any = exp.slice(1);
  return [Symbol.for('list'), ...args];
}

compileValuesMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-values-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('list'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileValuesMacro.lispMacro = true;

/**
 * Compiler macro for `(string? ...)` expressions.
 */
function compileStringpMacro(exp: any, env: any): any {
  const [x]: any[] = exp.slice(1);
  return [Symbol.for('eq?'), [Symbol.for('type-of'), x], 'string'];
}

compileStringpMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-stringp-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('eq?'), [Symbol.for('type-of'), [Symbol.for('unquote'), Symbol.for('x')]], 'string']]];

compileStringpMacro.lispMacro = true;

/**
 * Compiler macro for `(string-trim ...)` expressions.
 */
function compileStringTrimMacro(exp: any, env: any): any {
  const args: any = exp.slice(1);
  if (args.length === 1) {
    return [Symbol.for('send'), args[0], Symbol.for('trim')];
  } else {
    return definitionToMacro(source(stringTrim_), args);
  }
}

compileStringTrimMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-string-trim-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-length'), Symbol.for('args')], 1], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('array-first'), Symbol.for('args')]], Symbol.for('trim')]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('string-trim_')], Symbol.for('args')]]]];

compileStringTrimMacro.lispMacro = true;

/**
 * Compiler macro for `(member? ...)` expressions.
 */
function compileMemberPMacro(exp: any, env: any): any {
  const [v, lst, isEqual]: any[] = exp.slice(1);
  if (!isEqual) {
    return definitionToMacro([Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]], [v, lst]);
  } else {
    return definitionToMacro([Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]], [v, lst, isEqual]);
  }
}

compileMemberPMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-member-p-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('is-equal')], [Symbol.for('definition->macro'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]]], [Symbol.for('list'), Symbol.for('v'), Symbol.for('lst')]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]]], [Symbol.for('list'), Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')]]]]];

compileMemberPMacro.lispMacro = true;

/**
 * Compiler macro for `(substring ...)` expressions.
 */
function compileSubstringMacro(exp: any, env: any): any {
  const [str, ...args]: any[] = exp.slice(1);
  return [Symbol.for('send'), str, Symbol.for('substring'), ...args];
}

compileSubstringMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-substring-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('str'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('str')], Symbol.for('substring'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileSubstringMacro.lispMacro = true;

/**
 * Compiler macro for `(array-drop ...)` expressions.
 */
function compileArrayDropMacro(exp: any, env: any): any {
  const [arr, n]: any[] = exp.slice(1);
  if (Number.isFinite(n)) {
    if (n === 0) {
      return arr;
    } else {
      return [Symbol.for('send'), arr, Symbol.for('slice'), n];
    }
  } else {
    return definitionToMacro(source(arrayDrop_), [arr, n]);
  }
}

compileArrayDropMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-drop-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('arr'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('arr')], Symbol.for('slice'), [Symbol.for('unquote'), Symbol.for('n')]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-drop_')], [Symbol.for('list'), Symbol.for('arr'), Symbol.for('n')]]]]];

compileArrayDropMacro.lispMacro = true;

/**
 * Compiler macro for `(drop-right ...)` expressions.
 */
function compileArrayDropRightMacro(exp: any, env: any): any {
  const [arr, n]: any[] = exp.slice(1);
  if (Number.isFinite(n)) {
    if (n === 0) {
      return arr;
    } else {
      return [Symbol.for('send'), arr, Symbol.for('slice'), 0, [Symbol.for('-'), n]];
    }
  } else {
    return definitionToMacro(source(arrayDropRight_), [arr, n]);
  }
}

compileArrayDropRightMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-drop-right-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('arr'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('arr')], Symbol.for('slice'), 0, [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('n')]]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-drop-right_')], [Symbol.for('list'), Symbol.for('arr'), Symbol.for('n')]]]]];

compileArrayDropRightMacro.lispMacro = true;

/**
 * Compiler macro for `(drop ...)` expressions.
 */
function compileDropMacro(exp: any, env: any): any {
  const [lst, pos]: any[] = exp.slice(1);
  if (Number.isFinite(pos)) {
    if (pos === 0) {
      return lst;
    } else {
      return [Symbol.for('send'), lst, Symbol.for('slice'), pos];
    }
  } else {
    return definitionToMacro(source(drop_), [lst, pos]);
  }
}

compileDropMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-drop-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('pos')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('pos')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('pos'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), [Symbol.for('unquote'), Symbol.for('pos')]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('drop_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('pos')]]]]];

compileDropMacro.lispMacro = true;

/**
 * Compiler macro for `(drop-right ...)` expressions.
 */
function compileDropRightMacro(exp: any, env: any): any {
  const [lst, n]: any[] = exp.slice(1);
  if (Number.isFinite(n)) {
    if (n === 0) {
      return lst;
    } else {
      return [Symbol.for('send'), lst, Symbol.for('slice'), 0, [Symbol.for('-'), n]];
    }
  } else {
    return definitionToMacro(source(dropRight_), [lst, n]);
  }
}

compileDropRightMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-drop-right-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), 0, [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('n')]]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('drop-right_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('n')]]]]];

compileDropRightMacro.lispMacro = true;

/**
 * Compiler macro for `(array-list-drop ...)` expressions.
 */
function compileArrayListDropMacro(exp: any, env: any): any {
  const [lst, n]: any[] = exp.slice(1);
  if (Number.isFinite(n)) {
    if (n === 0) {
      return lst;
    } else {
      return [Symbol.for('send'), lst, Symbol.for('slice'), n];
    }
  } else {
    return definitionToMacro(source(arrayListDrop_), [lst, n]);
  }
}

compileArrayListDropMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-list-drop-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), [Symbol.for('unquote'), Symbol.for('n')]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-list-drop_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('n')]]]]];

compileArrayListDropMacro.lispMacro = true;

/**
 * Compiler macro for `(array-list-drop-right ...)` expressions.
 */
function compileArrayListDropRightMacro(exp: any, env: any): any {
  const [lst, n]: any[] = exp.slice(1);
  if (Number.isFinite(n)) {
    if (n === 0) {
      return lst;
    } else {
      return [Symbol.for('send'), lst, Symbol.for('slice'), 0, [Symbol.for('-'), n]];
    }
  } else {
    return definitionToMacro(source(arrayListDropRight_), [lst, n]);
  }
}

compileArrayListDropRightMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-array-list-drop-right-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), 0, [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('n')]]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-list-drop-right_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('n')]]]]];

compileArrayListDropRightMacro.lispMacro = true;

/**
 * Compiler macro for `(regexp ...)` expressions.
 */
function compileRegexpMacro(exp: any, env: any): any {
  const args: any = exp.slice(1);
  return [Symbol.for('new'), Symbol.for('RegExp'), ...args];
}

compileRegexpMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-regexp-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('RegExp'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileRegexpMacro.lispMacro = true;

/**
 * Compiler macro for `(assert ...)` expressions.
 */
function compileAssertMacro(exp: any, env: any): any {
  const args: any = exp.slice(1);
  return [Symbol.for('send'), Symbol.for('console'), Symbol.for('assert'), ...args];
}

compileAssertMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-assert-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), Symbol.for('console'), Symbol.for('assert'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileAssertMacro.lispMacro = true;

/**
 * Compiler macro for `(display ...)` expressions.
 */
function compileDisplayMacro(exp: any, env: any): any {
  const args: any = exp.slice(1);
  return [Symbol.for('send'), Symbol.for('console'), Symbol.for('log'), ...args];
}

compileDisplayMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-display-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), Symbol.for('console'), Symbol.for('log'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileDisplayMacro.lispMacro = true;

/**
 * Compiler macro for `(current-environment)` expressions.
 */
function compileCurrentEnvironmentMacro(exp: any, env: any): any {
  const argSym: any = Symbol('_arg');
  const strSym: any = Symbol('_str');
  const identifierRegexp: any = [Symbol.for('regexp'), '^\\w+$'];
  return [Symbol.for('js-obj'), 'get', [Symbol.for('js/arrow'), [argSym], [Symbol.for('try'), [Symbol.for('define'), strSym, [Symbol.for('symbol->string'), argSym]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), identifierRegexp, strSym], [Symbol.for('return'), [Symbol.for('js/eval'), strSym]]], [Symbol.for('else'), [Symbol.for('return'), Symbol.for('undefined')]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), Symbol.for('undefined')]]]], 'has', [Symbol.for('js/arrow'), [argSym], [Symbol.for('try'), [Symbol.for('define'), strSym, [Symbol.for('symbol->string'), argSym]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), identifierRegexp, strSym], [Symbol.for('js/eval'), strSym], [Symbol.for('return'), Symbol.for('#t')]], [Symbol.for('else'), [Symbol.for('return'), Symbol.for('#f')]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), Symbol.for('#f')]]]]];
}

compileCurrentEnvironmentMacro.lispSource = [Symbol.for('define'), [Symbol.for('compile-current-environment-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('arg-sym'), [Symbol.for('gensym'), '_arg']], [Symbol.for('define'), Symbol.for('str-sym'), [Symbol.for('gensym'), '_str']], [Symbol.for('define'), Symbol.for('identifier-regexp'), [Symbol.for('quote'), [Symbol.for('regexp'), '^\\w+$']]], [Symbol.for('quasiquote'), [Symbol.for('js-obj'), 'get', [Symbol.for('js/arrow'), [[Symbol.for('unquote'), Symbol.for('arg-sym')]], [Symbol.for('try'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('str-sym')], [Symbol.for('symbol->string'), [Symbol.for('unquote'), Symbol.for('arg-sym')]]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), [Symbol.for('unquote'), Symbol.for('identifier-regexp')], [Symbol.for('unquote'), Symbol.for('str-sym')]], [Symbol.for('return'), [Symbol.for('js/eval'), [Symbol.for('unquote'), Symbol.for('str-sym')]]]], [Symbol.for('else'), [Symbol.for('return'), Symbol.for('undefined')]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), Symbol.for('undefined')]]]], 'has', [Symbol.for('js/arrow'), [[Symbol.for('unquote'), Symbol.for('arg-sym')]], [Symbol.for('try'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('str-sym')], [Symbol.for('symbol->string'), [Symbol.for('unquote'), Symbol.for('arg-sym')]]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), [Symbol.for('unquote'), Symbol.for('identifier-regexp')], [Symbol.for('unquote'), Symbol.for('str-sym')]], [Symbol.for('js/eval'), [Symbol.for('unquote'), Symbol.for('str-sym')]], [Symbol.for('return'), Symbol.for('#t')]], [Symbol.for('else'), [Symbol.for('return'), Symbol.for('#f')]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), Symbol.for('#f')]]]]]]];

compileCurrentEnvironmentMacro.lispMacro = true;

/**
 * Compile a `(js ...)` expression.
 */
function compileJs(node: any, env: any, options: any = {}): any {
  let evalOption: any = options['eval'];
  evalOption = true;
  const str: any = node.get(1);
  const strExp: any = str.getValue();
  if (!evalOption) {
    return makeExpressionOrStatement(new Literal(undefined), options);
  } else if (typeof strExp === 'string') {
    return makeExpressionOrStatement(new XRawJavaScript(strExp), options);
  } else {
    return compileJsEval(node, env, options);
  }
}

compileJs.lispSource = [Symbol.for('define'), [Symbol.for('compile-js'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('eval-option'), [Symbol.for('oget'), Symbol.for('options'), 'eval']], [Symbol.for('set!'), Symbol.for('eval-option'), Symbol.for('#t')], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('str-exp'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('eval-option')], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('undefined')], Symbol.for('options')]], [[Symbol.for('string?'), Symbol.for('str-exp')], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('XRawJavaScript'), Symbol.for('str-exp')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-js-eval'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(js/eval ...)` expression.
 */
function compileJsEval(node: any, env: any, options: any = {}): any {
  // TODO: Disable if `eval-option` is `#f`.
  let evalOption: any = options['eval'];
  // TODO: Make `#f` the default.
  evalOption = true;
  const str: any = node.get(1);
  const strExp: any = str.getValue();
  if (!evalOption) {
    return makeExpressionOrStatement(new Literal(undefined), options);
  } else {
    return makeExpressionOrStatement(new CallExpression(new Identifier('eval'), [compileExpression(str, env, options)]), options);
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
function quote_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

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
function quasiquote_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

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
function setX_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

setX_.lispSource = [Symbol.for('define'), [Symbol.for('set!_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

setX_.lispMacro = true;

/**
 * Expand a `(module ...)` expression.
 */
function module_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

module_.lispSource = [Symbol.for('define'), [Symbol.for('module_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

module_.lispMacro = true;

/**
 * Expand a `(begin ...)` expression.
 */
function begin_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

begin_.lispSource = [Symbol.for('define'), [Symbol.for('begin_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

begin_.lispMacro = true;

/**
 * Expand a `(block ...)` expression.
 */
function block_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

block_.lispSource = [Symbol.for('define'), [Symbol.for('block_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

block_.lispMacro = true;

/**
 * Expand a `(let* ...)` expression.
 */
function letStar_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

letStar_.lispSource = [Symbol.for('define'), [Symbol.for('let-star_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

letStar_.lispMacro = true;

/**
 * Expand a `(let-values ...)` expression.
 */
function letValues_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

letValues_.lispSource = [Symbol.for('define'), [Symbol.for('let-values_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

letValues_.lispMacro = true;

/**
 * Expand a `(define-values ...)` expression.
 */
function defineValues_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineValues_.lispSource = [Symbol.for('define'), [Symbol.for('define-values_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineValues_.lispMacro = true;

/**
 * Expand a `(set!-values ...)` expression.
 */
function setValues_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

setValues_.lispSource = [Symbol.for('define'), [Symbol.for('set-values_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

setValues_.lispMacro = true;

/**
 * Expand a `(define ...)` expression.
 */
function define_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

define_.lispSource = [Symbol.for('define'), [Symbol.for('define_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

define_.lispMacro = true;

/**
 * Expand a `(define/generator ...)` expression.
 */
function defineGenerator_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineGenerator_.lispSource = [Symbol.for('define'), [Symbol.for('define-generator_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineGenerator_.lispMacro = true;

/**
 * Expand a `(define/async ...)` expression.
 */
function defineAsync_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

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
function defineMacro_(exp: any, env: any): any {
  let name: any = ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : exp[1])[0];
  const fExp: any = compileSexp(exp, env, currentCompilationOptions());
  const f: any = eval_(fExp, env);
  env.set(name, f, 'macro');
  return fExp;
}

defineMacro_.lispSource = [Symbol.for('define'), [Symbol.for('define-macro_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('f-exp'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('eval_'), Symbol.for('f-exp'), Symbol.for('env')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('name'), Symbol.for('f'), 'macro'], Symbol.for('f-exp')];

defineMacro_.lispMacro = true;

/**
 * Expand a `(for ...)` expression.
 */
function for_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

for_.lispSource = [Symbol.for('define'), [Symbol.for('for_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

for_.lispMacro = true;

/**
 * Expand a `(js/while ...)` expression.
 */
function jsWhile_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsWhile_.lispSource = [Symbol.for('define'), [Symbol.for('js-while_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsWhile_.lispMacro = true;

/**
 * Expand a `(js/do-while ...)` expression.
 */
function jsDoWhile_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsDoWhile_.lispSource = [Symbol.for('define'), [Symbol.for('js-do-while_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsDoWhile_.lispMacro = true;

/**
 * Expand a `(break)` expression.
 */
function break_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

break_.lispSource = [Symbol.for('define'), [Symbol.for('break_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

break_.lispMacro = true;

/**
 * Expand a `(continue)` expression.
 */
function continue_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

continue_.lispSource = [Symbol.for('define'), [Symbol.for('continue_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

continue_.lispMacro = true;

/**
 * Expand a `(yield ...)` expression.
 */
function yield_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

yield_.lispSource = [Symbol.for('define'), [Symbol.for('yield_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

yield_.lispMacro = true;

/**
 * Expand a `(return ...)` expression.
 */
function return_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

return_.lispSource = [Symbol.for('define'), [Symbol.for('return_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

return_.lispMacro = true;

/**
 * Expand a `(throw ...)` expression.
 *
 * Similar to the [`throw`][clj:throw] special form in Clojure.
 *
 * [clj:throw]: https://clojuredocs.org/clojure.core/throw
 */
function throw_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

throw_.lispSource = [Symbol.for('define'), [Symbol.for('throw_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

throw_.lispMacro = true;

/**
 * Expand a `(js/async ...)` expression.
 */
function jsAsync_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsAsync_.lispSource = [Symbol.for('define'), [Symbol.for('js-async_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsAsync_.lispMacro = true;

/**
 * Expand a `(js/await ...)` expression.
 */
function jsAwait_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

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
function lambda_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

lambda_.lispSource = [Symbol.for('define'), [Symbol.for('lambda_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

lambda_.lispMacro = true;

/**
 * Expand a `(js/function ...)` expression.
 *
 * Creates an anonymous JavaScript function.
 */
function jsFunction_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsFunction_.lispSource = [Symbol.for('define'), [Symbol.for('js-function_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsFunction_.lispMacro = true;

/**
 * Expand a `(js/arrow ...)` expression.
 *
 * Creates a JavaScript arrow function.
 */
function jsArrow_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsArrow_.lispSource = [Symbol.for('define'), [Symbol.for('js-arrow_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsArrow_.lispMacro = true;

/**
 * Expand a `(cond ...)` expression.
 */
function cond_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

cond_.lispSource = [Symbol.for('define'), [Symbol.for('cond_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

cond_.lispMacro = true;

/**
 * Expand an `(and ...)` expression.
 */
function and_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

and_.lispSource = [Symbol.for('define'), [Symbol.for('and_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

and_.lispMacro = true;

/**
 * Expand an `(or ...)` expression.
 */
function or_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

or_.lispSource = [Symbol.for('define'), [Symbol.for('or_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

or_.lispMacro = true;

/**
 * Call a method on an object.
 */
function sendMethod(...args: any[]): any {
  const [obj, method, ...restArgs]: any[] = args;
  if (typeof method === 'symbol') {
    return sendMethod(obj, method.description as string, ...restArgs);
  } else if (typeof method === 'string') {
    return sendMethod(obj, (obj as any)[method], ...restArgs);
  } else if (method instanceof Function) {
    return method.call(obj, ...restArgs);
  } else {
    throw new Error('Not a method: ' + method);
  }
}

sendMethod.lispSource = [Symbol.for('define'), [Symbol.for('send-method'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define-values'), [Symbol.for('obj'), Symbol.for('method'), Symbol.for('.'), Symbol.for('rest-args')], Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('method')], 'symbol'], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), [Symbol.for('symbol->string'), Symbol.for('method')], Symbol.for('rest-args')]], [[Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('method')], 'string'], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), [Symbol.for('oget'), Symbol.for('obj'), Symbol.for('method')], Symbol.for('rest-args')]], [[Symbol.for('is-a?'), Symbol.for('method'), Symbol.for('Function')], [Symbol.for('send/apply'), Symbol.for('method'), Symbol.for('call'), Symbol.for('obj'), Symbol.for('rest-args')]], [Symbol.for('else'), [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), [Symbol.for('string-append'), 'Not a method: ', Symbol.for('method')]]]]]];

/**
 * Expand a `(send ...)` expression.
 *
 * Similar to [`send`][rkt:send] in Racket.
 *
 * [rkt:send]: https://docs.racket-lang.org/guide/classes.html#(part._methods)
 */
function send_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

send_.lispSource = [Symbol.for('define'), [Symbol.for('send_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

send_.lispMacro = true;

/**
 * Expand a `(send/apply ...)` expression.
 */
function sendApply_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

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
function dot_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

dot_.lispSource = [Symbol.for('define'), [Symbol.for('dot_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

dot_.lispMacro = true;

/**
 * Expand a `(get-field ...)` expression.
 */
function getField_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

getField_.lispSource = [Symbol.for('define'), [Symbol.for('get-field_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

getField_.lispMacro = true;

/**
 * Expand a `(js/optional-chaining ...)` expression.
 */
function jsOptionalChaining_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsOptionalChaining_.lispSource = [Symbol.for('define'), [Symbol.for('js-optional-chaining_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsOptionalChaining_.lispMacro = true;

/**
 * Expand a `(set-field! ...)` expression.
 */
function setField_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

setField_.lispSource = [Symbol.for('define'), [Symbol.for('set-field_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

setField_.lispMacro = true;

/**
 * Evaluate an `(make-object ...)` expression.
 */
function new_(constructor: any, ...args: any[]): any {
  // TODO: Rename to `new_`.
  // TODO: Express as `(apply new ...)`.
  return new constructor(...args);
}

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
function class_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

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
function defineClass_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineClass_.lispSource = [Symbol.for('define'), [Symbol.for('define-class_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineClass_.lispMacro = true;

/**
 * Expand a `(js/try ...)` expression.
 */
function jsTry_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsTry_.lispSource = [Symbol.for('define'), [Symbol.for('js-try_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsTry_.lispMacro = true;

/**
 * Expand a `(provide ...)` expression.
 */
function provide_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

provide_.lispSource = [Symbol.for('define'), [Symbol.for('provide_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

provide_.lispMacro = true;

/**
 * Expand a `(require ...)` expression.
 */
function require_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

require_.lispSource = [Symbol.for('define'), [Symbol.for('require_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

require_.lispMacro = true;

/**
 * Evaluate a JavaScript string.
 */
function js_(str: any): any {
  return eval(str);
}

js_.lispSource = [Symbol.for('define'), [Symbol.for('js_'), Symbol.for('str')], [Symbol.for('js/eval'), Symbol.for('str')]];

/**
 * Get the Lisp source of a function.
 */
function source(x: any): any {
  return x.lispSource;
}

source.lispSource = [Symbol.for('define'), [Symbol.for('source'), Symbol.for('x')], [Symbol.for('get-field'), Symbol.for('lispSource'), Symbol.for('x')]];

/**
 * Whether a function has Lisp source.
 */
function sourcep(x: any): any {
  return (x !== undefined) && (x.lispSource !== undefined);
}

sourcep.lispSource = [Symbol.for('define'), [Symbol.for('source?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('undefined')]], [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('lispSource'), Symbol.for('x')], Symbol.for('undefined')]]]];

/**
 * Map the function `f` over the rose tree-wrapped
 * S-expression `node`. The S-expression is processed
 * in bottom-up order.
 */
function mapRose(f: any, node: any, env: any = new LispEnvironment(), stack: any = [], bindings: any = new LispEnvironment()): any {
  if (!(node instanceof Rose)) {
    return mapSexp(f, node, env, stack, bindings);
  } else {
    return mapVisitRose(f, node, env, stack, bindings);
  }
}

mapRose.lispSource = [Symbol.for('define'), [Symbol.for('map-rose'), Symbol.for('f'), Symbol.for('node'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')]], [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('node'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('else'), [Symbol.for('map-visit-rose'), Symbol.for('f'), Symbol.for('node'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]]];

/**
 * Map a function `f` over a rose tree using the Visitor pattern.
 */
function mapVisitRose(f: any, node: any, env: any = new LispEnvironment(), stack: any = [], bindings: any = new LispEnvironment()): any {
  function skipNode(node: any, stack: any, bindings: any): any {
    return node;
  }
  skipNode.lispSource = [Symbol.for('define'), [Symbol.for('skip-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], Symbol.for('node')];
  function visitNode(node: any, stack: any, bindings: any): any {
    return f(node, stack, bindings);
  }
  visitNode.lispSource = [Symbol.for('define'), [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('f'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
  // Nonatomic value (i.e., a list form some sort).
  function visitNonatomic(node: any, stack: any, bindings: any, skip: any = 0): any {
    let result: any = visitFormsNode(node, [...stack, node], bindings, skip);
    return f(result, stack, bindings);
  }
  visitNonatomic.lispSource = [Symbol.for('define'), [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('visit-forms-node'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // Macro call.
  function visitMacroCallP(node: any): any {
    let exp: any = node.getValue();
    return macroCallP(exp, env);
  }
  visitMacroCallP.lispSource = [Symbol.for('define'), [Symbol.for('visit-macro-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('macro-call?'), Symbol.for('exp'), Symbol.for('env')]]];
  const visitMacroCall: any = visitNode;
  // Special form.
  function visitSpecialFormP(node: any): any {
    let exp: any = node.getValue();
    return specialFormP(exp, env);
  }
  visitSpecialFormP.lispSource = [Symbol.for('define'), [Symbol.for('visit-special-form-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('special-form?'), Symbol.for('exp'), Symbol.for('env')]]];
  const visitSpecialForm: any = visitNode;
  // Function call.
  function visitFunctionCallP(node: any): any {
    let exp: any = node.getValue();
    return functionCallP(exp, env);
  }
  visitFunctionCallP.lispSource = [Symbol.for('define'), [Symbol.for('visit-function-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('function-call?'), Symbol.for('exp'), Symbol.for('env')]]];
  const visitFunctionCall: any = visitNonatomic;
  function visitElseP(node: any): any {
    return true;
  }
  visitElseP.lispSource = [Symbol.for('define'), [Symbol.for('visit-else-p'), Symbol.for('node')], Symbol.for('#t')];
  function visitFormsNodeWith(visitor: any, node: any, stack: any, bindings: any, skip: any = 0): any {
    let exp: any = node.getValue();
    if (!Array.isArray(exp)) {
      // `node` is not a list expression; early return.
      return visit(visitor, node, stack, bindings);
    }
    const nodes: any = node.getNodes();
    const resultNodes: any = visitFormsListWith(visitor, nodes, stack, bindings, skip);
    if (resultNodes === nodes) {
      return node;
    } else {
      let exp: any = [];
      let result: any = transferComments(node, new Rose(exp));
      for (let node of resultNodes) {
        exp.push(node.getValue());
        result.insert(node);
      }
      return result;
    }
  }
  visitFormsNodeWith.lispSource = [Symbol.for('define'), [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-nodes')]], [Symbol.for('define'), Symbol.for('result-nodes'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('result-nodes'), Symbol.for('nodes')], Symbol.for('node')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('new'), Symbol.for('Rose'), Symbol.for('exp')]]], [Symbol.for('for'), [[Symbol.for('node'), Symbol.for('result-nodes')]], [Symbol.for('push-right!'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('send'), Symbol.for('result'), Symbol.for('insert'), Symbol.for('node')]], Symbol.for('result')]]];
  function visitFormsListWith(visitor: any, nodes: any, stack: any, bindings: any, skip: any = 0): any {
    if (!Array.isArray(nodes)) {
      // `nodes` is not a list; early return.
      return visit(visitor, nodes, stack, bindings);
    }
    // Keep track of whether any of the expressions are modified
    // by visitation. If none of them are, return the original list.
    let isModified: any = false;
    let i: any = 0;
    let result: any = nodes.map(function (x: any): any {
      if (i < skip) {
        i++;
        return x;
      } else {
        let x1: any = visit(visitor, x, stack, bindings);
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
  function visitFormsNode(node: any, stack: any, bindings: any, skip: any = 0): any {
    return visitFormsNodeWith(visitor, node, stack, bindings, skip);
  }
  visitFormsNode.lispSource = [Symbol.for('define'), [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
  function visitFormsList(nodes: any, stack: any, bindings: any, skip: any = 0): any {
    return visitFormsListWith(visitor, nodes, stack, bindings, skip);
  }
  visitFormsList.lispSource = [Symbol.for('define'), [Symbol.for('visit-forms-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
  function visitClausesNode(node: any, stack: any, bindings: any, skip: any = 0): any {
    return visitFormsNodeWith(visitFormsNode, node, stack, bindings, skip);
  }
  visitClausesNode.lispSource = [Symbol.for('define'), [Symbol.for('visit-clauses-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
  function visitClausesList(nodes: any, stack: any, bindings: any, skip: any = 0): any {
    return visitFormsListWith(visitFormsNode, nodes, stack, bindings, skip);
  }
  visitClausesList.lispSource = [Symbol.for('define'), [Symbol.for('visit-clauses-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visit-forms-node'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
  // `(module ...)` form.
  function visitModuleP(node: any): any {
    return formp(node, module_, env);
  }
  visitModuleP.lispSource = [Symbol.for('define'), [Symbol.for('visit-module-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('module_'), Symbol.for('env')]];
  function visitModule(node: any, stack: any, bindings: any): any {
    return visitNonatomic(node, stack, bindings, 3);
  }
  visitModule.lispSource = [Symbol.for('define'), [Symbol.for('visit-module'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 3]];
  // `(begin ...)` form.
  function visitBeginP(node: any): any {
    return formp(node, begin_, env);
  }
  visitBeginP.lispSource = [Symbol.for('define'), [Symbol.for('visit-begin-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin_'), Symbol.for('env')]];
  function visitBegin(node: any, stack: any, bindings: any): any {
    return visitNonatomic(node, stack, bindings, 1);
  }
  visitBegin.lispSource = [Symbol.for('define'), [Symbol.for('visit-begin'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
  // `(begin0 ...)` form.
  function visitBegin0P(node: any): any {
    return formp(node, begin0_, env);
  }
  visitBegin0P.lispSource = [Symbol.for('define'), [Symbol.for('visit-begin0-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin0_'), Symbol.for('env')]];
  const visitBegin0: any = visitBegin;
  // `(let ...)` form.
  function visitLetP(node: any): any {
    return formp(node, letStar_, env);
  }
  visitLetP.lispSource = [Symbol.for('define'), [Symbol.for('visit-let-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-star_'), Symbol.for('env')]];
  function visitLet(node: any, stack: any, bindings: any): any {
    let result: any = node;
    let bindingsEnv: any = extendEnvironment(new LispEnvironment(), bindings);
    const sym: any = node.get(0).getValue();
    const letBindings: any = node.get(1);
    const body: any = node.drop(2);
    for (let letBinding of letBindings.getValue()) {
      const bindingSym: any = Array.isArray(letBinding) ? letBinding[0] : letBinding;
      bindingsEnv.setLocal(bindingSym, true, 'variable');
    }
    const visitedLetBindings: any = visitClausesNode(letBindings, [...stack, node], bindingsEnv);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindingsEnv);
    if (!((letBindings === visitedLetBindings) && (body === visitedBody))) {
      result = transferComments(node, makeRose([sym, visitedLetBindings, ...visitedBody]));
    }
    return f(result, stack, bindings);
  }
  visitLet.lispSource = [Symbol.for('define'), [Symbol.for('visit-let'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('for'), [[Symbol.for('let-binding'), [Symbol.for('send'), Symbol.for('let-bindings'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('binding-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('let-binding')], [Symbol.for('first'), Symbol.for('let-binding')], Symbol.for('let-binding')]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('binding-sym'), Symbol.for('#t'), 'variable']], [Symbol.for('define'), Symbol.for('visited-let-bindings'), [Symbol.for('visit-clauses-node'), Symbol.for('let-bindings'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings'), Symbol.for('visited-let-bindings')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  function visitLetValuesP(node: any): any {
    return formp(node, letValues_, env);
  }
  visitLetValuesP.lispSource = [Symbol.for('define'), [Symbol.for('visit-let-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-values_'), Symbol.for('env')]];
  function visitLetValues(node: any, stack: any, bindings: any): any {
    let result: any = node;
    let bindingsEnv: any = extendEnvironment(new LispEnvironment(), bindings);
    const sym: any = node.get(0).getValue();
    const letBindings: any = node.get(1);
    const body: any = node.drop(2);
    const visitedLetBindings: any = visitFormsNodeWith(function (x: any): any {
      let xResult: any = x;
      const ids: any = x.get(0);
      let val: any = x.get(1);
      const idsExp: any = ids.getValue();
      if (typeof idsExp === 'symbol') {
        bindingsEnv.setLocal(idsExp, true, 'variable');
      } else {
        for (let letBinding of idsExp) {
          if (typeof letBinding === 'symbol') {
            bindingsEnv.set(letBinding, true, 'variable');
          }
        }
      }
      const visitedIds: any = visitFormsNode(ids, [...stack, node], bindingsEnv);
      const visitedVal: any = visit(visitor, val, [...stack, node], bindingsEnv);
      if (!((visitedIds === ids) && (visitedVal === val))) {
        xResult = transferComments(x, makeRose([visitedIds, visitedVal]));
      }
      return xResult;
    }, letBindings, [...stack, node], bindingsEnv);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindingsEnv);
    if (!((letBindings === visitedLetBindings) && (body === visitedBody))) {
      result = transferComments(node, makeRose([sym, visitedLetBindings, ...visitedBody]));
    }
    return f(result, stack, bindings);
  }
  visitLetValues.lispSource = [Symbol.for('define'), [Symbol.for('visit-let-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-let-bindings'), [Symbol.for('visit-forms-node-with'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('x-result'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('ids'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('ids-exp'), [Symbol.for('send'), Symbol.for('ids'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ids-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('ids-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('let-binding'), Symbol.for('ids-exp')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('let-binding')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set'), Symbol.for('let-binding'), Symbol.for('#t'), 'variable']]]]], [Symbol.for('define'), Symbol.for('visited-ids'), [Symbol.for('visit-forms-node'), Symbol.for('ids'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('val'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('visited-ids'), Symbol.for('ids')], [Symbol.for('eq?'), Symbol.for('visited-val'), Symbol.for('val')]], [Symbol.for('set!'), Symbol.for('x-result'), [Symbol.for('transfer-comments'), Symbol.for('x'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('visited-ids')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], Symbol.for('x-result')], Symbol.for('let-bindings'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings'), Symbol.for('visited-let-bindings')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(for ...)` form.
  function visitForP(node: any): any {
    return formp(node, for_, env);
  }
  visitForP.lispSource = [Symbol.for('define'), [Symbol.for('visit-for-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('for_'), Symbol.for('env')]];
  const visitFor: any = visitLet;
  // `(while ...)` form.
  function visitWhileP(node: any): any {
    return formp(node, jsWhile_, env);
  }
  visitWhileP.lispSource = [Symbol.for('define'), [Symbol.for('visit-while-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-while_'), Symbol.for('env')]];
  const visitWhile: any = visitFunctionCall;
  // `(cond ...)` form.
  function visitCondP(node: any): any {
    return formp(node, cond_, env);
  }
  visitCondP.lispSource = [Symbol.for('define'), [Symbol.for('visit-cond-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('cond_'), Symbol.for('env')]];
  function visitCond(node: any, stack: any, bindings: any): any {
    let result: any = node;
    const sym: any = node.get(0).getValue();
    const clauses: any = node.drop(1);
    const visitedClauses: any = visitClausesList(clauses, [...stack, node], bindings);
    if (visitedClauses !== clauses) {
      result = transferComments(node, makeRose([sym, ...visitedClauses]));
    }
    return f(result, stack, bindings);
  }
  visitCond.lispSource = [Symbol.for('define'), [Symbol.for('visit-cond'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('clauses'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('visited-clauses'), [Symbol.for('visit-clauses-list'), Symbol.for('clauses'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('visited-clauses'), Symbol.for('clauses')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote-splicing'), Symbol.for('visited-clauses')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(lambda ...)` form.
  function visitLambdaP(node: any): any {
    return formp(node, lambda_, env) || formp(node, jsFunction_, env) || formp(node, jsArrow_, env);
  }
  visitLambdaP.lispSource = [Symbol.for('define'), [Symbol.for('visit-lambda-p'), Symbol.for('node')], [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('node'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-function_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-arrow_'), Symbol.for('env')]]];
  function visitLambda(node: any, stack: any, bindings: any): any {
    let result: any = node;
    let bindingsEnv: any = extendEnvironment(new LispEnvironment(), bindings);
    const sym: any = node.get(0).getValue();
    let params: any = node.get(1);
    const paramsExp: any = params.getValue();
    const body: any = node.drop(2);
    if (typeof paramsExp === 'symbol') {
      bindingsEnv.setLocal(paramsExp, true, 'variable');
    } else {
      for (let param of paramsExp) {
        if (Array.isArray(param)) {
          param = param[0];
        }
        bindingsEnv.setLocal(param, true, 'variable');
      }
    }
    const visitedParams: any = visitClausesNode(params, [...stack, node], bindingsEnv);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindingsEnv);
    if (!((params === visitedParams) && (body === visitedBody))) {
      result = transferComments(node, makeRose([sym, visitedParams, ...visitedBody]));
    }
    return f(result, stack, bindings);
  }
  visitLambda.lispSource = [Symbol.for('define'), [Symbol.for('visit-lambda'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), Symbol.for('params-exp')]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('param'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-clauses-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(define ...)` form.
  function visitDefineP(node: any): any {
    return formp(node, define_, env);
  }
  visitDefineP.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define_'), Symbol.for('env')]];
  function visitDefine(node: any, stack: any, bindings: any): any {
    let result: any = node;
    const defineSym: any = node.get(0).getValue();
    let id: any = node.get(1);
    const idExp: any = id.getValue();
    const idSym: any = Array.isArray(idExp) ? idExp[0] : idExp;
    let bindingsEnv: any = bindings;
    if (Array.isArray(idExp)) {
      bindings.setLocal(idSym, true, 'procedure');
      for (let param of idExp.slice(1)) {
        if (Array.isArray(param)) {
          param = param[0];
        }
        bindings.setLocal(param, true, 'variable');
      }
      bindingsEnv = extendEnvironment(new LispEnvironment(), bindings);
    } else {
      bindings.setLocal(idSym, true, 'variable');
    }
    const body: any = node.drop(2);
    const visitedId: any = Array.isArray(idExp) ? visitClausesNode(id, [...stack, node], bindingsEnv) : visitNode(id, [...stack, node], bindingsEnv);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindingsEnv);
    if (!((id === visitedId) && (body === visitedBody))) {
      result = transferComments(node, makeRose([defineSym, visitedId, ...visitedBody]));
    }
    return f(result, stack, bindings);
  }
  visitDefine.lispSource = [Symbol.for('define'), [Symbol.for('visit-define'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-exp'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('first'), Symbol.for('id-exp')], Symbol.for('id-exp')]], [Symbol.for('define'), Symbol.for('bindings-env'), Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'procedure'], [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('rest'), Symbol.for('id-exp')]]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('param'), Symbol.for('#t'), 'variable']], [Symbol.for('set!'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'variable']]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('visit-clauses-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')], [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(define-values ...)` form.
  function visitDefineValuesP(node: any): any {
    return formp(node, defineValues_, env);
  }
  visitDefineValuesP.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-values_'), Symbol.for('env')]];
  function visitDefineValues(node: any, stack: any, bindings: any): any {
    return visitFormsNode(node, stack, bindings, 2);
  }
  visitDefineValues.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 2]];
  // `(defmacro ...)` form.
  function visitDefmacroP(node: any): any {
    return formp(node, defmacro_, env);
  }
  visitDefmacroP.lispSource = [Symbol.for('define'), [Symbol.for('visit-defmacro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('defmacro_'), Symbol.for('env')]];
  function visitDefmacro(node: any, stack: any, bindings: any): any {
    let result: any = node;
    const defmacroSym: any = node.get(0).getValue();
    let id: any = node.get(1);
    const idSym: any = id.getValue();
    let params: any = node.get(2);
    const paramsExp: any = params.getValue();
    const body: any = node.drop(3);
    bindings.setLocal(idSym, true, 'macro');
    let bindingsEnv: any = extendEnvironment(new LispEnvironment(), bindings);
    if (typeof paramsExp === 'symbol') {
      bindingsEnv.setLocal(paramsExp, true, 'variable');
    } else {
      for (let param of flatten_(paramsExp)) {
        bindingsEnv.setLocal(params, true, 'variable');
      }
    }
    const visitedId: any = visitNode(id, [...stack, node], bindingsEnv);
    const visitedParams: any = visitFormsNode(params, [...stack, node], bindingsEnv);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindingsEnv);
    if (!((id === visitedId) && (params === visitedParams) && (body === visitedBody))) {
      result = transferComments(node, makeRose([defmacroSym, visitedId, visitedParams, ...visitedBody]));
    }
    result = f(result, stack, bindings);
    bindings.setLocal(idSym, true, 'macro');
    return result;
  }
  visitDefmacro.lispSource = [Symbol.for('define'), [Symbol.for('visit-defmacro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('defmacro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('defmacro-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], Symbol.for('result')];
  // `(define-macro ...)` form.
  function visitDefineMacroP(node: any): any {
    return formp(node, defineMacro_, env);
  }
  visitDefineMacroP.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-macro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-macro_'), Symbol.for('env')]];
  function visitDefineMacro(node: any, stack: any, bindings: any): any {
    let result: any = node;
    const defineMacroSym: any = node.get(0).getValue();
    const nameAndArgs: any = node.get(1);
    const nameAndArgsExp: any = nameAndArgs.getValue();
    const idSym: any = nameAndArgsExp[0];
    let id: any = makeRose(idSym, nameAndArgs);
    const paramsExp: any = cdr(nameAndArgsExp);
    let params: any = makeRose(paramsExp, nameAndArgs);
    const body: any = node.drop(2);
    bindings.setLocal(idSym, true, 'macro');
    let bindingsEnv: any = extendEnvironment(new LispEnvironment(), bindings);
    if (typeof paramsExp === 'symbol') {
      bindingsEnv.setLocal(paramsExp, true, 'variable');
    } else {
      for (let param of flatten_(paramsExp)) {
        bindingsEnv.setLocal(params, true, 'variable');
      }
    }
    const visitedId: any = visitNode(id, [...stack, node], bindingsEnv);
    const visitedParams: any = visitFormsNode(params, [...stack, node], bindingsEnv);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindingsEnv);
    if (!((id === visitedId) && (params === visitedParams) && (body === visitedBody))) {
      result = transferComments(node, makeRose([defineMacroSym, cons(visitedId, visitedParams), ...visitedBody]));
    }
    result = f(result, stack, bindings);
    bindings.setLocal(idSym, true, 'macro');
    return result;
  }
  visitDefineMacro.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-macro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-macro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('name-and-args-exp'), [Symbol.for('send'), Symbol.for('name-and-args'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('car'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('make-rose'), Symbol.for('id-sym'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('cdr'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('make-rose'), Symbol.for('params-exp'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-macro-sym')], [Symbol.for('unquote'), [Symbol.for('cons'), Symbol.for('visited-id'), Symbol.for('visited-params')]], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], Symbol.for('result')];
  // `(define-class ...)` form.
  function visitDefineClassP(node: any): any {
    return formp(node, class_, env);
  }
  visitDefineClassP.lispSource = [Symbol.for('define'), [Symbol.for('visit-define-class-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('class_'), Symbol.for('env')]];
  const visitDefineClass: any = visitFunctionCall;
  // `(ann ...)` form.
  function visitAnnP(node: any): any {
    return formp(node, ann_, env);
  }
  visitAnnP.lispSource = [Symbol.for('define'), [Symbol.for('visit-ann-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('ann_'), Symbol.for('env')]];
  function visitAnn(node: any, stack: any, bindings: any): any {
    return visitNode(node, stack, bindings);
  }
  visitAnn.lispSource = [Symbol.for('define'), [Symbol.for('visit-ann'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(and ...)` form.
  function visitAndP(node: any): any {
    return formp(node, and_, env);
  }
  visitAndP.lispSource = [Symbol.for('define'), [Symbol.for('visit-and-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('and_'), Symbol.for('env')]];
  const visitAnd: any = visitFunctionCall;
  // `(or ...)` form.
  function visitOrP(node: any): any {
    return formp(node, or_, env);
  }
  visitOrP.lispSource = [Symbol.for('define'), [Symbol.for('visit-or-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('or_'), Symbol.for('env')]];
  const visitOr: any = visitFunctionCall;
  // `(when ...)` form.
  function visitWhenP(node: any): any {
    return formp(node, when_, env);
  }
  visitWhenP.lispSource = [Symbol.for('define'), [Symbol.for('visit-when-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('when_'), Symbol.for('env')]];
  function visitWhen(node: any, stack: any, bindings: any): any {
    return visitNonatomic(node, stack, bindings, 1);
  }
  visitWhen.lispSource = [Symbol.for('define'), [Symbol.for('visit-when'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
  // `(unless ...)` form.
  function visitUnlessP(node: any): any {
    return formp(node, unless_, env);
  }
  visitUnlessP.lispSource = [Symbol.for('define'), [Symbol.for('visit-unless-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('unless_'), Symbol.for('env')]];
  function visitUnless(node: any, stack: any, bindings: any): any {
    return visitNonatomic(node, stack, bindings, 1);
  }
  visitUnless.lispSource = [Symbol.for('define'), [Symbol.for('visit-unless'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
  // `(make-object ...)` form.
  function visitMakeObjectP(node: any): any {
    return formp(node, new_, env);
  }
  visitMakeObjectP.lispSource = [Symbol.for('define'), [Symbol.for('visit-make-object-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('new_'), Symbol.for('env')]];
  const visitMakeObject: any = visitFunctionCall;
  // `(return ...)` form.
  function visitReturnP(node: any): any {
    return formp(node, return_, env);
  }
  visitReturnP.lispSource = [Symbol.for('define'), [Symbol.for('visit-return-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('return_'), Symbol.for('env')]];
  const visitReturn: any = visitFunctionCall;
  // `(send ...)` form.
  function visitSendP(node: any): any {
    return formp(node, send_, env);
  }
  visitSendP.lispSource = [Symbol.for('define'), [Symbol.for('visit-send-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('send_'), Symbol.for('env')]];
  const visitSend: any = visitFunctionCall;
  // `(set! ...)` form.
  function visitSetqP(node: any): any {
    return formp(node, setX_, env);
  }
  visitSetqP.lispSource = [Symbol.for('define'), [Symbol.for('visit-setq-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set!_'), Symbol.for('env')]];
  const visitSetq: any = visitFunctionCall;
  // `(set-field! ...)` form.
  function visitSetFieldP(node: any): any {
    return formp(node, setField_, env);
  }
  visitSetFieldP.lispSource = [Symbol.for('define'), [Symbol.for('visit-set-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set-field_'), Symbol.for('env')]];
  const visitSetField: any = visitFunctionCall;
  // `(get-field ...)` form.
  function visitGetFieldP(node: any): any {
    return formp(node, getField_, env);
  }
  visitGetFieldP.lispSource = [Symbol.for('define'), [Symbol.for('visit-get-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('get-field_'), Symbol.for('env')]];
  const visitGetField: any = visitFunctionCall;
  // Quoted value.
  function visitQuoteP(node: any): any {
    return formp(node, quote_, env);
  }
  visitQuoteP.lispSource = [Symbol.for('define'), [Symbol.for('visit-quote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quote_'), Symbol.for('env')]];
  const visitQuote: any = visitNode;
  // Quasiquoted value.
  function visitQuasiquoteP(node: any): any {
    return formp(node, quasiquote_, env);
  }
  visitQuasiquoteP.lispSource = [Symbol.for('define'), [Symbol.for('visit-quasiquote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quasiquote_'), Symbol.for('env')]];
  function visitQuasiquote(node: any, stack: any, bindings: any): any {
    function visitQuasiquoteForm(node: any, stack: any, bindings: any): any {
      let result: any = node;
      const sym: any = node.get(0);
      let val: any = node.get(1);
      // Visit `unquote` and `unquote-splicing` expressions, if any.
      const visitedVal: any = visit(quasiquoteVisitor, val, stack, bindings);
      if (val !== visitedVal) {
        result = transferComments(node, makeRose([sym, visitedVal]));
      }
      // Visit the `unquote` expression.
      return f(result, stack, bindings);
    }
    visitQuasiquoteForm.lispSource = [Symbol.for('define'), [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('quasiquote-visitor'), Symbol.for('val'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('visited-val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
    function visitUnquoteP(node: any): any {
      return taggedListP(node, Symbol.for('unquote'));
    }
    visitUnquoteP.lispSource = [Symbol.for('define'), [Symbol.for('visit-unquote-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote')]]];
    function visitUnquote(node: any, stack: any): any {
      // When visiting unquoted expressions,
      // use the regular visitor.
      return visitFormsNodeWith(visitor, node, stack, bindings, 1);
    }
    visitUnquote.lispSource = [Symbol.for('define'), [Symbol.for('visit-unquote'), Symbol.for('node'), Symbol.for('stack')], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
    function visitUnquoteSplicingP(node: any): any {
      return taggedListP(node, Symbol.for('unquote-splicing'));
    }
    visitUnquoteSplicingP.lispSource = [Symbol.for('define'), [Symbol.for('visit-unquote-splicing-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]]];
    const visitUnquoteSplicing: any = visitUnquote;
    function visitQuotedList(node: any, stack: any, bindings: any): any {
      return visitFormsNodeWith(quasiquoteVisitor, node, stack, bindings);
    }
    visitQuotedList.lispSource = [Symbol.for('define'), [Symbol.for('visit-quoted-list'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node-with'), Symbol.for('quasiquote-visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
    const quasiquoteVisitor: any = makeVisitor([[visitUnquoteP, visitUnquote], [visitUnquoteSplicingP, visitUnquoteSplicing], [visitNonatomicP, visitQuotedList], [visitElseP, skipNode]]);
    return visitQuasiquoteForm(node, [...stack, node], bindings);
  }
  visitQuasiquote.lispSource = [Symbol.for('define'), [Symbol.for('visit-quasiquote'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('quasiquote-visitor'), Symbol.for('val'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('visited-val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-unquote-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote')]]], [Symbol.for('define'), [Symbol.for('visit-unquote'), Symbol.for('node'), Symbol.for('stack')], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-unquote-splicing-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]]], [Symbol.for('define'), Symbol.for('visit-unquote-splicing'), Symbol.for('visit-unquote')], [Symbol.for('define'), [Symbol.for('visit-quoted-list'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node-with'), Symbol.for('quasiquote-visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('quasiquote-visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('visit-unquote-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote')]], [[Symbol.for('unquote'), Symbol.for('visit-unquote-splicing-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote-splicing')]], [[Symbol.for('unquote'), Symbol.for('visit-nonatomic-p')], [Symbol.for('unquote'), Symbol.for('visit-quoted-list')]], [[Symbol.for('unquote'), Symbol.for('visit-else-p')], [Symbol.for('unquote'), Symbol.for('skip-node')]]]]]], [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]];
  // List.
  function visitNonatomicP(node: any): any {
    let exp: any = node.getValue();
    return Array.isArray(exp);
  }
  visitNonatomicP.lispSource = [Symbol.for('define'), [Symbol.for('visit-nonatomic-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('array?'), Symbol.for('exp')]]];
  // Atomic value.
  const visitAtomP: any = visitElseP;
  const visitAtom: any = visitNode;
  // Rename this to `map-visitor` to distinguish it from
  // the `visitor` parameter of many functions.
  const visitor: any = makeVisitor([[visitModuleP, visitModule], [visitBeginP, visitBegin], [visitBegin0P, visitBegin0], [visitLetP, visitLet], [visitLetValuesP, visitLetValues], [visitCondP, visitCond], [visitLambdaP, visitLambda], [visitDefineP, visitDefine], [visitDefineValuesP, visitDefineValues], [visitDefineMacroP, visitDefineMacro], [visitDefmacroP, visitDefmacro], [visitAnnP, visitAnn], [visitAndP, visitAnd], [visitOrP, visitOr], [visitForP, visitFor], [visitWhileP, visitWhile], [visitWhenP, visitWhen], [visitSendP, visitSend], [visitSetqP, visitSetq], [visitSetFieldP, visitSetField], [visitGetFieldP, visitGetField], [visitUnlessP, visitUnless], [visitDefineClassP, visitDefineClass], [visitMakeObjectP, visitMakeObject], [visitReturnP, visitReturn], [visitQuoteP, visitQuote], [visitQuasiquoteP, visitQuasiquote], [visitMacroCallP, visitMacroCall], [visitSpecialFormP, visitSpecialForm], [visitFunctionCallP, visitFunctionCall], [visitNonatomicP, visitNonatomic], [visitElseP, visitAtom]]);
  return visit(visitor, node, stack, bindings);
}

mapVisitRose.lispSource = [Symbol.for('define'), [Symbol.for('map-visit-rose'), Symbol.for('f'), Symbol.for('node'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('define'), [Symbol.for('skip-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], Symbol.for('node')], [Symbol.for('define'), [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('f'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('visit-forms-node'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-macro-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('macro-call?'), Symbol.for('exp'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('visit-macro-call'), Symbol.for('visit-node')], [Symbol.for('define'), [Symbol.for('visit-special-form-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('special-form?'), Symbol.for('exp'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('visit-special-form'), Symbol.for('visit-node')], [Symbol.for('define'), [Symbol.for('visit-function-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('function-call?'), Symbol.for('exp'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('visit-function-call'), Symbol.for('visit-nonatomic')], [Symbol.for('define'), [Symbol.for('visit-else-p'), Symbol.for('node')], Symbol.for('#t')], [Symbol.for('define'), [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-nodes')]], [Symbol.for('define'), Symbol.for('result-nodes'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('result-nodes'), Symbol.for('nodes')], Symbol.for('node')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('new'), Symbol.for('Rose'), Symbol.for('exp')]]], [Symbol.for('for'), [[Symbol.for('node'), Symbol.for('result-nodes')]], [Symbol.for('push-right!'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('send'), Symbol.for('result'), Symbol.for('insert'), Symbol.for('node')]], Symbol.for('result')]]], [Symbol.for('define'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('nodes')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('is-modified'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), Symbol.for('skip')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('is-modified'), Symbol.for('#t')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x1')]]], Symbol.for('nodes')]], [Symbol.for('unless'), Symbol.for('is-modified'), [Symbol.for('set!'), Symbol.for('result'), Symbol.for('nodes')]], Symbol.for('result')], [Symbol.for('define'), [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-forms-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-clauses-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-clauses-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visit-forms-node'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-module-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('module_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-module'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 3]], [Symbol.for('define'), [Symbol.for('visit-begin-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-begin'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-begin0-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin0_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-begin0'), Symbol.for('visit-begin')], [Symbol.for('define'), [Symbol.for('visit-let-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-star_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-let'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('for'), [[Symbol.for('let-binding'), [Symbol.for('send'), Symbol.for('let-bindings'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('binding-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('let-binding')], [Symbol.for('first'), Symbol.for('let-binding')], Symbol.for('let-binding')]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('binding-sym'), Symbol.for('#t'), 'variable']], [Symbol.for('define'), Symbol.for('visited-let-bindings'), [Symbol.for('visit-clauses-node'), Symbol.for('let-bindings'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings'), Symbol.for('visited-let-bindings')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-let-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-values_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-let-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-let-bindings'), [Symbol.for('visit-forms-node-with'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('x-result'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('ids'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('ids-exp'), [Symbol.for('send'), Symbol.for('ids'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ids-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('ids-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('let-binding'), Symbol.for('ids-exp')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('let-binding')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set'), Symbol.for('let-binding'), Symbol.for('#t'), 'variable']]]]], [Symbol.for('define'), Symbol.for('visited-ids'), [Symbol.for('visit-forms-node'), Symbol.for('ids'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('val'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('visited-ids'), Symbol.for('ids')], [Symbol.for('eq?'), Symbol.for('visited-val'), Symbol.for('val')]], [Symbol.for('set!'), Symbol.for('x-result'), [Symbol.for('transfer-comments'), Symbol.for('x'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('visited-ids')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], Symbol.for('x-result')], Symbol.for('let-bindings'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings'), Symbol.for('visited-let-bindings')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-for-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('for_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-for'), Symbol.for('visit-let')], [Symbol.for('define'), [Symbol.for('visit-while-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-while_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-while'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-cond-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('cond_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-cond'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('clauses'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('visited-clauses'), [Symbol.for('visit-clauses-list'), Symbol.for('clauses'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('visited-clauses'), Symbol.for('clauses')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote-splicing'), Symbol.for('visited-clauses')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-lambda-p'), Symbol.for('node')], [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('node'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-function_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-arrow_'), Symbol.for('env')]]], [Symbol.for('define'), [Symbol.for('visit-lambda'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), Symbol.for('params-exp')]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('param'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-clauses-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-define-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-define'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-exp'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('first'), Symbol.for('id-exp')], Symbol.for('id-exp')]], [Symbol.for('define'), Symbol.for('bindings-env'), Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'procedure'], [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('rest'), Symbol.for('id-exp')]]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('param'), Symbol.for('#t'), 'variable']], [Symbol.for('set!'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'variable']]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('visit-clauses-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')], [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-define-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-values_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-define-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 2]], [Symbol.for('define'), [Symbol.for('visit-defmacro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('defmacro_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-defmacro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('defmacro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('defmacro-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], Symbol.for('result')], [Symbol.for('define'), [Symbol.for('visit-define-macro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-macro_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-define-macro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-macro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('name-and-args-exp'), [Symbol.for('send'), Symbol.for('name-and-args'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('car'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('make-rose'), Symbol.for('id-sym'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('cdr'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('make-rose'), Symbol.for('params-exp'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], [Symbol.for('define'), Symbol.for('bindings-env'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params-exp'), Symbol.for('#t'), 'variable']], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('send'), Symbol.for('bindings-env'), Symbol.for('set-local'), Symbol.for('params'), Symbol.for('#t'), 'variable']]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-env')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-macro-sym')], [Symbol.for('unquote'), [Symbol.for('cons'), Symbol.for('visited-id'), Symbol.for('visited-params')]], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('set-local'), Symbol.for('id-sym'), Symbol.for('#t'), 'macro'], Symbol.for('result')], [Symbol.for('define'), [Symbol.for('visit-define-class-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('class_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-define-class'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-ann-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('ann_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-ann'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-and-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('and_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-and'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-or-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('or_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-or'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-when-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('when_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-when'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-unless-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('unless_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-unless'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-make-object-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('new_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-make-object'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-return-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('return_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-return'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-send-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('send_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-send'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-setq-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set!_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-setq'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-set-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set-field_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-set-field'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-get-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('get-field_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-get-field'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-quote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quote_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-quote'), Symbol.for('visit-node')], [Symbol.for('define'), [Symbol.for('visit-quasiquote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quasiquote_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-quasiquote'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('quasiquote-visitor'), Symbol.for('val'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('visited-val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-unquote-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote')]]], [Symbol.for('define'), [Symbol.for('visit-unquote'), Symbol.for('node'), Symbol.for('stack')], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-unquote-splicing-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]]], [Symbol.for('define'), Symbol.for('visit-unquote-splicing'), Symbol.for('visit-unquote')], [Symbol.for('define'), [Symbol.for('visit-quoted-list'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node-with'), Symbol.for('quasiquote-visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('quasiquote-visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('visit-unquote-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote')]], [[Symbol.for('unquote'), Symbol.for('visit-unquote-splicing-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote-splicing')]], [[Symbol.for('unquote'), Symbol.for('visit-nonatomic-p')], [Symbol.for('unquote'), Symbol.for('visit-quoted-list')]], [[Symbol.for('unquote'), Symbol.for('visit-else-p')], [Symbol.for('unquote'), Symbol.for('skip-node')]]]]]], [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-nonatomic-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('array?'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('visit-atom-p'), Symbol.for('visit-else-p')], [Symbol.for('define'), Symbol.for('visit-atom'), Symbol.for('visit-node')], [Symbol.for('define'), Symbol.for('visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('visit-module-p')], [Symbol.for('unquote'), Symbol.for('visit-module')]], [[Symbol.for('unquote'), Symbol.for('visit-begin-p')], [Symbol.for('unquote'), Symbol.for('visit-begin')]], [[Symbol.for('unquote'), Symbol.for('visit-begin0-p')], [Symbol.for('unquote'), Symbol.for('visit-begin0')]], [[Symbol.for('unquote'), Symbol.for('visit-let-p')], [Symbol.for('unquote'), Symbol.for('visit-let')]], [[Symbol.for('unquote'), Symbol.for('visit-let-values-p')], [Symbol.for('unquote'), Symbol.for('visit-let-values')]], [[Symbol.for('unquote'), Symbol.for('visit-cond-p')], [Symbol.for('unquote'), Symbol.for('visit-cond')]], [[Symbol.for('unquote'), Symbol.for('visit-lambda-p')], [Symbol.for('unquote'), Symbol.for('visit-lambda')]], [[Symbol.for('unquote'), Symbol.for('visit-define-p')], [Symbol.for('unquote'), Symbol.for('visit-define')]], [[Symbol.for('unquote'), Symbol.for('visit-define-values-p')], [Symbol.for('unquote'), Symbol.for('visit-define-values')]], [[Symbol.for('unquote'), Symbol.for('visit-define-macro-p')], [Symbol.for('unquote'), Symbol.for('visit-define-macro')]], [[Symbol.for('unquote'), Symbol.for('visit-defmacro-p')], [Symbol.for('unquote'), Symbol.for('visit-defmacro')]], [[Symbol.for('unquote'), Symbol.for('visit-ann-p')], [Symbol.for('unquote'), Symbol.for('visit-ann')]], [[Symbol.for('unquote'), Symbol.for('visit-and-p')], [Symbol.for('unquote'), Symbol.for('visit-and')]], [[Symbol.for('unquote'), Symbol.for('visit-or-p')], [Symbol.for('unquote'), Symbol.for('visit-or')]], [[Symbol.for('unquote'), Symbol.for('visit-for-p')], [Symbol.for('unquote'), Symbol.for('visit-for')]], [[Symbol.for('unquote'), Symbol.for('visit-while-p')], [Symbol.for('unquote'), Symbol.for('visit-while')]], [[Symbol.for('unquote'), Symbol.for('visit-when-p')], [Symbol.for('unquote'), Symbol.for('visit-when')]], [[Symbol.for('unquote'), Symbol.for('visit-send-p')], [Symbol.for('unquote'), Symbol.for('visit-send')]], [[Symbol.for('unquote'), Symbol.for('visit-setq-p')], [Symbol.for('unquote'), Symbol.for('visit-setq')]], [[Symbol.for('unquote'), Symbol.for('visit-set-field-p')], [Symbol.for('unquote'), Symbol.for('visit-set-field')]], [[Symbol.for('unquote'), Symbol.for('visit-get-field-p')], [Symbol.for('unquote'), Symbol.for('visit-get-field')]], [[Symbol.for('unquote'), Symbol.for('visit-unless-p')], [Symbol.for('unquote'), Symbol.for('visit-unless')]], [[Symbol.for('unquote'), Symbol.for('visit-define-class-p')], [Symbol.for('unquote'), Symbol.for('visit-define-class')]], [[Symbol.for('unquote'), Symbol.for('visit-make-object-p')], [Symbol.for('unquote'), Symbol.for('visit-make-object')]], [[Symbol.for('unquote'), Symbol.for('visit-return-p')], [Symbol.for('unquote'), Symbol.for('visit-return')]], [[Symbol.for('unquote'), Symbol.for('visit-quote-p')], [Symbol.for('unquote'), Symbol.for('visit-quote')]], [[Symbol.for('unquote'), Symbol.for('visit-quasiquote-p')], [Symbol.for('unquote'), Symbol.for('visit-quasiquote')]], [[Symbol.for('unquote'), Symbol.for('visit-macro-call-p')], [Symbol.for('unquote'), Symbol.for('visit-macro-call')]], [[Symbol.for('unquote'), Symbol.for('visit-special-form-p')], [Symbol.for('unquote'), Symbol.for('visit-special-form')]], [[Symbol.for('unquote'), Symbol.for('visit-function-call-p')], [Symbol.for('unquote'), Symbol.for('visit-function-call')]], [[Symbol.for('unquote'), Symbol.for('visit-nonatomic-p')], [Symbol.for('unquote'), Symbol.for('visit-nonatomic')]], [[Symbol.for('unquote'), Symbol.for('visit-else-p')], [Symbol.for('unquote'), Symbol.for('visit-atom')]]]]]], [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];

/**
 * Map the function `f` over the S-expression `exp`.
 * The S-expression is processed in bottom-up order.
 */
function mapSexp(f: any, exp: any, env: any = new LispEnvironment(), stack: any = [], bindings: any = new LispEnvironment()): any {
  const f1: any = function (x: any, stack: any, bindings: any): any {
    {
      let exp: any = x.getValue();
      const stack1: any = stack.map(function (x: any): any {
        if (x instanceof Rose) {
          return x.getValue();
        } else {
          return x;
        }
      });
      let result: any = f(exp, stack1, bindings);
      if (result === exp) {
        return x;
      } else {
        return makeRose(result, x);
      }
    }
  };
  const isRose: any = exp instanceof Rose;
  let node: any = isRose ? exp : makeRose(exp);
  let result: any = mapRose(f1, node, env, stack, bindings);
  // If the input is a rose tree node,
  // return a rose tree node as output too.
  if (isRose) {
    return result;
  } else {
    return result.getValue();
  }
}

mapSexp.lispSource = [Symbol.for('define'), [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('exp'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('let*'), [[Symbol.for('f1'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('let*'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('stack1'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')], Symbol.for('x')]], Symbol.for('stack')]], [Symbol.for('result'), [Symbol.for('f'), Symbol.for('exp'), Symbol.for('stack1'), Symbol.for('bindings')]]], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('result'), Symbol.for('exp')], Symbol.for('x'), [Symbol.for('make-rose'), Symbol.for('result'), Symbol.for('x')]]]]], [Symbol.for('is-rose'), [Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')]], [Symbol.for('node'), [Symbol.for('if'), Symbol.for('is-rose'), Symbol.for('exp'), [Symbol.for('make-rose'), Symbol.for('exp')]]], [Symbol.for('result'), [Symbol.for('map-rose'), Symbol.for('f1'), Symbol.for('node'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('if'), Symbol.for('is-rose'), Symbol.for('result'), [Symbol.for('send'), Symbol.for('result'), Symbol.for('get-value')]]]];

/**
 * Call the function `f` on each node of a rose tree,
 * but do not create a new rose tree in the process.
 */
function iterateRose(f: any, node: any, env: any = new LispEnvironment()): any {
  return mapRose(function (x: any, stack: any): any {
    f(x, stack);
    return x;
  }, node, env);
}

iterateRose.lispSource = [Symbol.for('define'), [Symbol.for('iterate-rose'), Symbol.for('f'), Symbol.for('node'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('map-rose'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('stack')], [Symbol.for('f'), Symbol.for('x'), Symbol.for('stack')], Symbol.for('x')], Symbol.for('node'), Symbol.for('env')]];

/**
 * Expand an `(ann ...)` expression.
 */
function ann_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

ann_.lispSource = [Symbol.for('define'), [Symbol.for('ann_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

ann_.lispMacro = true;

/**
 * Expand a `(: ...)` expression.
 */
function colon_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

colon_.lispSource = [Symbol.for('define'), [Symbol.for('colon_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

colon_.lispMacro = true;

/**
 * Expand a `(define-type ...)` expression.
 */
function defineType_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineType_.lispSource = [Symbol.for('define'), [Symbol.for('define-type_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineType_.lispMacro = true;

/**
 * Expand a `(let-js-obj ...)` expression.
 */
function letJsObj_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

letJsObj_.lispSource = [Symbol.for('define'), [Symbol.for('let-js-obj_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

letJsObj_.lispMacro = true;

/**
 * Expand a `(define-js-obj ...)` expression.
 */
function defineJsObj_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineJsObj_.lispSource = [Symbol.for('define'), [Symbol.for('define-js-obj_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineJsObj_.lispMacro = true;

/**
 * Expand a `(set!-js-obj ...)` expression.
 */
function setJsObj_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

setJsObj_.lispSource = [Symbol.for('define'), [Symbol.for('set-js-obj_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

setJsObj_.lispMacro = true;

/**
 * Compile a `(js/switch ...)` form.
 */
function compileJsSwitch(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    const discriminant: any = node.get(1);
    const discriminantCompiled: any = compileExpression(discriminant, env, options);
    const cases: any = node.drop(2);
    const casesCompiled: any = cases.map(function (x: any): any {
      const op: any = x.get(0).getValue();
      let testCompiled: any;
      let consequentCompiled: any;
      if (op === Symbol.for('case')) {
        let test: any = x.get(1);
        testCompiled = compileExpression(test, env, options);
        const consequent: any = x.drop(2);
        // It is advisable to wrap cases in a block statement.
        // <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/switch#lexical_scoping>
        consequentCompiled = [compileStatementOrReturnStatement(makeRose([Symbol.for('block'), ...consequent]), env, options)];
      } else {
        testCompiled = null;
        const consequent: any = x.drop(1);
        consequentCompiled = [compileStatementOrReturnStatement(makeRose([Symbol.for('block'), ...consequent]), env, options)];
      }
      return new SwitchCase(testCompiled, consequentCompiled);
    });
    return new SwitchStatement(discriminantCompiled, casesCompiled);
  } else {
    return compileExpression(wrapInArrowCall(node), env, options);
  }
}

compileJsSwitch.lispSource = [Symbol.for('define'), [Symbol.for('compile-js-switch'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('discriminant'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('discriminant-compiled'), [Symbol.for('compile-expression'), Symbol.for('discriminant'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('cases'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('cases-compiled'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('~>'), Symbol.for('x'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('test-compiled')], [Symbol.for('define'), Symbol.for('consequent-compiled')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('op'), [Symbol.for('quote'), Symbol.for('case')]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('set!'), Symbol.for('test-compiled'), [Symbol.for('compile-expression'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('drop'), 2]], [Symbol.for('set!'), Symbol.for('consequent-compiled'), [Symbol.for('list'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('block'), [Symbol.for('unquote-splicing'), Symbol.for('consequent')]]]], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('test-compiled'), Symbol.for('js/null')], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('drop'), 1]], [Symbol.for('set!'), Symbol.for('consequent-compiled'), [Symbol.for('list'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('block'), [Symbol.for('unquote-splicing'), Symbol.for('consequent')]]]], Symbol.for('env'), Symbol.for('options')]]]]], [Symbol.for('new'), Symbol.for('SwitchCase'), Symbol.for('test-compiled'), Symbol.for('consequent-compiled')]], Symbol.for('cases')]], [Symbol.for('new'), Symbol.for('SwitchStatement'), Symbol.for('discriminant-compiled'), Symbol.for('cases-compiled')]], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Expand a `(js/switch ...)` expression.
 */
function jsSwitch_(exp: any, env: any): any {
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
function callWithCurrentContinuation_(proc: any, promptTag: any = undefined): any {
  class CallCCWrapper {
    value: any;

    constructor(value: any) {
      this.value = value;
    }
  }
  try {
    return proc((value: any): any => {
      throw new CallCCWrapper(value);
    });
  } catch (e) {
    if (e instanceof CallCCWrapper) {
      return e.value;
    } else {
      throw e;
    }
  }
}

callWithCurrentContinuation_.lispSource = [Symbol.for('define'), [Symbol.for('call-with-current-continuation_'), Symbol.for('proc'), [Symbol.for('prompt-tag'), Symbol.for('undefined')]], [Symbol.for('define-class'), Symbol.for('CallCCWrapper'), [], [Symbol.for('define/public'), Symbol.for('value')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('value')], [Symbol.for('set-field!'), Symbol.for('value'), Symbol.for('this'), Symbol.for('value')]]], [Symbol.for('try'), [Symbol.for('return'), [Symbol.for('proc'), [Symbol.for('js/arrow'), [Symbol.for('value')], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('CallCCWrapper'), Symbol.for('value')]]]]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e'), [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('e'), Symbol.for('CallCCWrapper')], [Symbol.for('return'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('e')]]], [Symbol.for('else'), [Symbol.for('throw'), Symbol.for('e')]]]]]];

/**
 * Traverse an ESTree tree.
 */
function traverseEstree(node: any, enter: any = undefined, leave: any = undefined, replace: any = undefined): any {
  let result: any = node;
  let el: any;
  let el1: any;
  let val: any;
  let val1: any;
  if (!(node instanceof Node)) {
    return result;
  }
  if (enter) {
    enter(node);
  }
  for (let key of Object.keys(node)) {
    val = (node as any)[key];
    if (Array.isArray(val)) {
      const _end: any = val.length;
      for (let i: any = 0; i < _end; i++) {
        el = (val as any)[i];
        el1 = traverseEstree(el, enter, leave, replace);
        if (el !== el1) {
          (val as any)[i] = el1;
        }
      }
    } else {
      val1 = traverseEstree(val, enter, leave, replace);
      if (val !== val1) {
        (node as any)[key] = val1;
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

traverseEstree.lispSource = [Symbol.for('define'), [Symbol.for('traverse-estree'), Symbol.for('node'), [Symbol.for('enter'), Symbol.for('undefined')], [Symbol.for('leave'), Symbol.for('undefined')], [Symbol.for('replace'), Symbol.for('undefined')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('el')], [Symbol.for('define'), Symbol.for('el1')], [Symbol.for('define'), Symbol.for('val')], [Symbol.for('define'), Symbol.for('val1')], [Symbol.for('unless'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Node')], [Symbol.for('return'), Symbol.for('result')]], [Symbol.for('when'), Symbol.for('enter'), [Symbol.for('enter'), Symbol.for('node')]], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('js-keys'), Symbol.for('node')]]], [Symbol.for('set!'), Symbol.for('val'), [Symbol.for('oget'), Symbol.for('node'), Symbol.for('key')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('val')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('val')]]]], [Symbol.for('set!'), Symbol.for('el'), [Symbol.for('aget'), Symbol.for('val'), Symbol.for('i')]], [Symbol.for('set!'), Symbol.for('el1'), [Symbol.for('traverse-estree'), Symbol.for('el'), Symbol.for('enter'), Symbol.for('leave'), Symbol.for('replace')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('el'), Symbol.for('el1')], [Symbol.for('list-set!'), Symbol.for('val'), Symbol.for('i'), Symbol.for('el1')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('val1'), [Symbol.for('traverse-estree'), Symbol.for('val'), Symbol.for('enter'), Symbol.for('leave'), Symbol.for('replace')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('val1')], [Symbol.for('oset!'), Symbol.for('node'), Symbol.for('key'), Symbol.for('val1')]]]]], [Symbol.for('when'), Symbol.for('leave'), [Symbol.for('leave'), Symbol.for('node')]], [Symbol.for('when'), Symbol.for('replace'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('replace'), Symbol.for('node')]]], Symbol.for('result')];

/**
 * Find ESTree nodes matching a predicate.
 */
function findEstree(pred: any, node: any): any {
  const nodes: any = [];
  traverseEstree(node, function (x: any): any {
    if (pred(x)) {
      nodes.push(x);
      return nodes;
    }
  });
  return nodes;
}

findEstree.lispSource = [Symbol.for('define'), [Symbol.for('find-estree'), Symbol.for('pred'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('nodes'), [Symbol.for('quote'), []]], [Symbol.for('traverse-estree'), Symbol.for('node'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('when'), [Symbol.for('pred'), Symbol.for('x')], [Symbol.for('push-right!'), Symbol.for('nodes'), Symbol.for('x')]]]], Symbol.for('nodes')];

/**
 * Optimize an S-expression.
 */
function optimizeSexp(exp: any, env: any): any {
  if (rosep(exp)) {
    return optimizeRose(exp, env);
  } else {
    return optimizeRose(makeRose(exp), env).getValue();
  }
}

optimizeSexp.lispSource = [Symbol.for('define'), [Symbol.for('optimize-sexp'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('rose?'), Symbol.for('exp')], [Symbol.for('optimize-rose'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('~>'), [Symbol.for('make-rose'), Symbol.for('exp')], [Symbol.for('optimize-rose'), Symbol.for('_'), Symbol.for('env')], [Symbol.for('send'), Symbol.for('_'), Symbol.for('get-value')]]]]];

/**
 * Optimize a rose tree-wrapped S-expression.
 */
function optimizeRose(exp: any, env: any): any {
  return applyOptimizations(exp, env);
}

optimizeRose.lispSource = [Symbol.for('define'), [Symbol.for('optimize-rose'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('apply-optimizations'), Symbol.for('exp'), Symbol.for('env')]];

/**
 * Optimize a module.
 */
function optimizeModule(m: any, env: any): any {
  return m.setNodes(m.mainNodes.map(function (x: any): any {
    return optimizeSexp(x, env);
  }));
}

optimizeModule.lispSource = [Symbol.for('define'), [Symbol.for('optimize-module'), Symbol.for('m'), Symbol.for('env')], [Symbol.for('send'), Symbol.for('m'), Symbol.for('set-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('optimize-sexp'), Symbol.for('x'), Symbol.for('env')]], [Symbol.for('get-field'), Symbol.for('main-nodes'), Symbol.for('m')]]]];

/**
 * Optimize an ESTree tree.
 */
function optimizeEstree(exp: any): any {
  return letVarsToConstVars(exp);
}

optimizeEstree.lispSource = [Symbol.for('define'), [Symbol.for('optimize-estree'), Symbol.for('exp')], [Symbol.for('~>'), Symbol.for('exp'), [Symbol.for('let-vars-to-const-vars')]]];

function letVarsToConstVars(program: any): any {
  const variables: any = [];
  traverseEstree(program, function (node: any): any {
    const varNames: any = [];
    if (estreeTypeP(node, 'AssignmentExpression')) {
      if (estreeTypeP(node.left, 'Identifier')) {
        varNames.unshift(node.left.name);
      } else if (estreeTypeP(node.left, 'ArrayPattern')) {
        for (let element of node.left.elements) {
          if (element && estreeTypeP(element, 'Identifier')) {
            varNames.unshift(element.name);
          }
        }
      }
    } else if (estreeTypeP(node, 'UpdateExpression')) {
      if (estreeTypeP(node.argument, 'Identifier')) {
        varNames.unshift(node.argument.name);
      }
    }
    for (let varName of varNames) {
      if (!variables.includes(varName)) {
        variables.unshift(varName);
      }
    }
  });
  return traverseEstree(program, undefined, undefined, function (node: any): any {
    if (estreeTypeP(node, 'VariableDeclaration')) {
      if (!findf(function (x: any): any {
        return !x.init || (findEstree(function (y: any): any {
          return estreeTypeP(y, 'Identifier') && variables.includes(y.name);
        }, x.id).length !== 0);
      }, node.declarations)) {
        node.kind = 'const';
      }
      return node;
    } else {
      return node;
    }
  });
}

letVarsToConstVars.lispSource = [Symbol.for('define'), [Symbol.for('let-vars-to-const-vars'), Symbol.for('program')], [Symbol.for('define'), Symbol.for('variables'), [Symbol.for('quote'), []]], [Symbol.for('traverse-estree'), Symbol.for('program'), [Symbol.for('lambda'), [Symbol.for('node')], [Symbol.for('define'), Symbol.for('var-names'), [Symbol.for('quote'), []]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('node'), 'AssignmentExpression'], [Symbol.for('cond'), [[Symbol.for('estree-type?'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')], 'Identifier'], [Symbol.for('push!'), Symbol.for('var-names'), [Symbol.for('get-field'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]]]], [[Symbol.for('estree-type?'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')], 'ArrayPattern'], [Symbol.for('for'), [[Symbol.for('element'), [Symbol.for('get-field'), Symbol.for('elements'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('element'), [Symbol.for('estree-type?'), Symbol.for('element'), 'Identifier']], [Symbol.for('push!'), Symbol.for('var-names'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('element')]]]]]]], [[Symbol.for('estree-type?'), Symbol.for('node'), 'UpdateExpression'], [Symbol.for('when'), [Symbol.for('estree-type?'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], 'Identifier'], [Symbol.for('push!'), Symbol.for('var-names'), [Symbol.for('get-field'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]]]]]], [Symbol.for('for'), [[Symbol.for('var-name'), Symbol.for('var-names')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('var-name'), Symbol.for('variables')], [Symbol.for('push!'), Symbol.for('variables'), Symbol.for('var-name')]]]]], [Symbol.for('traverse-estree'), Symbol.for('program'), Symbol.for('undefined'), Symbol.for('undefined'), [Symbol.for('lambda'), [Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('node'), 'VariableDeclaration'], [Symbol.for('unless'), [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('x')]], [Symbol.for('not'), [Symbol.for('zero?'), [Symbol.for('array-list-length'), [Symbol.for('find-estree'), [Symbol.for('lambda'), [Symbol.for('y')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('y'), 'Identifier'], [Symbol.for('memq?'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('y')], Symbol.for('variables')]]], [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('x')]]]]]]], [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('node')]], [Symbol.for('set-field!'), Symbol.for('kind'), Symbol.for('node'), 'const']], Symbol.for('node')], [Symbol.for('else'), Symbol.for('node')]]]]];

/**
 * Find a optimization rule matching `node`.
 */
function findOptimization(node: any, env: any, rules: any = optimizations): any {
  for (let rule of rules) {
    const [predicate]: any[] = rule;
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
function applyOptimizations(node: any, env: any, rules: any = optimizations): any {
  let result: any = node;
  let rule: any = false;
  while ((rule = findOptimization(result, env, rules))) {
    const [predicate, optimizer]: any[] = rule;
    result = optimizer(result, env);
  }
  return result;
}

applyOptimizations.lispSource = [Symbol.for('define'), [Symbol.for('apply-optimizations'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('rules'), Symbol.for('optimizations')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('rule'), Symbol.for('#f')], [Symbol.for('while'), [Symbol.for('set!'), Symbol.for('rule'), [Symbol.for('find-optimization'), Symbol.for('result'), Symbol.for('env'), Symbol.for('rules')]], [Symbol.for('define-values'), [Symbol.for('predicate'), Symbol.for('optimizer')], Symbol.for('rule')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('optimizer'), Symbol.for('result'), Symbol.for('env')]]], Symbol.for('result')];

/**
 * List of `(predicate optimizer)` tuples.
 */
const optimizations: any = [];

/**
 * Module class.
 */
class Module {
  name: any = '';

  headerExpressions: any = [];

  headerNodes: any = [];

  requireExpressions: any = [];

  requireNodes: any = [];

  provideExpressions: any = [];

  provideNodes: any = [];

  mainExpressions: any = [];

  mainNodes: any = [];

  expressions: any = [];

  nodes: any = [];

  inlineLispSourcesFlag: any = false;

  seenModules: any = [];

  environment: any;

  parentEnvironment: any;

  moduleMap: any;

  symbolMap: any = new Map();

  constructor(nodes: any = [], parent: any = langEnvironment, name: any = '') {
    this.parentEnvironment = parent;
    this.name = name;
    this.initializeNodes(nodes);
  }

  getExpressions(): any {
    return this.expressions;
  }

  getEnvironment(): any {
    if (this.environment) {
      return this.environment;
    } else {
      return this.makeEnvironment(this.parentEnvironment);
    }
  }

  getModuleMap(): any {
    return this.moduleMap;
  }

  getName(): any {
    return this.name;
  }

  /**
   * Whether a particular symbol is bound in this module's scope
   * (i.e., whether the module imports or defines the symbol).
   */
  hasSymbol(sym: any): any {
    const key: any = (typeof sym === 'string') ? Symbol.for(sym) : sym;
    return this.symbolMap.has(key);
  }

  makeHeaderNode(nodes: any = []): any {
    // Create header node if there is more than one comment, or if
    // there is a single comment ending in a blank line.
    if (length(nodes) > 0) {
      const initialNode: any = nodes[0];
      let comments: any = initialNode.getProperty('comments');
      let initialNodeComments: any = [];
      let initialNodeCommentString: any = undefined;
      let headerComments: any = [];
      let headerCommentStrings: any = [];
      if (comments) {
        this.findInlineLispSourcesComment(comments);
        let commentStrings: any = [];
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
        const headerExp: any = [Symbol.for('begin')];
        const headerNode: any = makeRose(headerExp);
        headerComments = headerCommentStrings.map(function (x: any): any {
          return new LeadingCommentToken(x);
        });
        headerNode.setProperty('comments', headerComments);
        this.headerNodes.push(headerNode);
        this.headerExpressions.push(headerExp);
        if (initialNodeCommentString) {
          initialNodeComments = [new LeadingCommentToken(initialNodeCommentString)];
        }
        return initialNode.setProperty('comments', initialNodeComments);
      }
    }
  }

  findInlineLispSourcesComment(comments: any = []): any {
    if (!this.getInlineLispSourcesFlag()) {
      const pattern: any = new RegExp('; inline-lisp-sources: t');
      for (let comment of comments) {
        const text: any = comment.value;
        if (text.match(pattern)) {
          this.setInlineLispSourcesFlag(true);
          break;
        }
      }
    }
  }

  initializeNodes(nodes: any = []): any {
    let exp: any;
    let match: any;
    let node: any;
    this.makeHeaderNode(nodes);
    // Sort the expressions into `require` expressions, `provide`
    // expressions and main expressions.
    for (let node of nodes) {
      // Handle both S-expressions and rose tree values---for now.
      // In the future, we might want to simplify this to only
      // rose tree values.
      if (node instanceof Rose) {
        exp = node.getValue();
        let comments: any = node.getProperty('comments');
        if (comments) {
          // Look for `inline-lisp-sources: true` magic comment.
          this.findInlineLispSourcesComment(comments);
        }
      } else {
        exp = node;
        node = makeRose(exp);
      }
      if (taggedListP(exp, Symbol.for('require'))) {
        this.requireExpressions.push(exp);
        this.requireNodes.push(node);
      } else if (taggedListP(exp, Symbol.for('provide'))) {
        this.provideExpressions.push(exp);
        this.provideNodes.push(node);
      } else {
        this.mainExpressions.push(exp);
        this.mainNodes.push(node);
      }
    }
    // Iterate over `require-expressions`.
    for (let node of this.requireNodes) {
      exp = node.getValue();
      if (taggedListP(exp, Symbol.for('require')) && (exp.length > 1) && taggedListP((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
            result = exp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : exp[1], Symbol.for('only-in'))) {
        let moduleName: any = ((): any => {
          const lst: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
            const x: any = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
          })()) ? ((): any => {
            let i: any = 1;
            let result: any = exp;
            while (i > 0) {
              if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
              } else {
                result = exp.slice(1);
              }
              i--;
            }
            if (Array.isArray(result)) {
              result = result[0];
            }
            return result;
          })() : exp[1];
          if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
            const x: any = lastCdr(lst);
            return Array.isArray(x) && (x.length === 0);
          })()) {
            let i: any = 1;
            let result: any = lst;
            while (i > 0) {
              if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = lst[lst.length - 1];
              } else {
                result = lst.slice(1);
              }
              i--;
            }
            if (Array.isArray(result)) {
              result = result[0];
            }
            return result;
          } else {
            return lst[1];
          }
        })();
        if (typeof moduleName === 'symbol') {
          moduleName = moduleName.description as string;
        }
        if ((match = moduleName.match(new RegExp('^\\./(.*)$')))) {
          moduleName = (Array.isArray(match) && (match.length >= 3) && (match[match.length - 2] === Symbol.for('.')) && ((): any => {
            const x: any = lastCdr(match);
            return Array.isArray(x) && (x.length === 0);
          })()) ? ((): any => {
            let i: any = 1;
            let result: any = match;
            while (i > 0) {
              if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = match[match.length - 1];
              } else {
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
        for (let x of ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
          let x1: any = lastCdr(exp);
          return Array.isArray(x1) && (x1.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = exp;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = exp[exp.length - 1];
            } else {
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
            this.symbolMap.set((Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && ((): any => {
              let x1: any = lastCdr(x);
              return Array.isArray(x1) && (x1.length === 0);
            })()) ? ((): any => {
              let i: any = 1;
              let result: any = x;
              while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                  result = x[x.length - 1];
                } else {
                  result = x.slice(1);
                }
                i--;
              }
              if (Array.isArray(result)) {
                result = result[0];
              }
              return result;
            })() : x[1], true);
          } else {
            this.symbolMap.set(x, true);
          }
        }
      } else if (taggedListP(exp, Symbol.for('require')) && (exp.length > 1)) {
        let moduleNameSymbol: any = exp[exp.length - 1];
        let moduleName: any = moduleNameSymbol;
        if (typeof moduleNameSymbol === 'symbol') {
          moduleName = moduleNameSymbol.description as string;
        } else {
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
      if (taggedListP(exp, Symbol.for('define')) || taggedListP(exp, Symbol.for('define-class'))) {
        let name: any = Array.isArray((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(exp);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = exp;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = exp[exp.length - 1];
            } else {
              result = exp.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : exp[1]) ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(exp);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = exp;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = exp[exp.length - 1];
            } else {
              result = exp.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : exp[1])[0] : ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(exp);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = exp;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = exp[exp.length - 1];
            } else {
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

  makeEnvironment(parent: any = undefined): any {
    const moduleEnv: any = new LispEnvironment([], parent);
    let imported: any;
    let local: any;
    let module: any;
    let env: any;
    let moduleName: any;
    this.parentEnvironment = parent;
    this.environment = moduleEnv;
    // Iterate over `require-nodes`, importing definitions
    // from other modules.
    for (let node of this.requireNodes) {
      let exp: any = node.getValue();
      if (taggedListP(exp, Symbol.for('require')) && (exp.length > 1) && taggedListP((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = exp;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = exp[exp.length - 1];
          } else {
            result = exp.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : exp[1], Symbol.for('only-in'))) {
        moduleName = ((): any => {
          const lst: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
            const x: any = lastCdr(exp);
            return Array.isArray(x) && (x.length === 0);
          })()) ? ((): any => {
            let i: any = 1;
            let result: any = exp;
            while (i > 0) {
              if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = exp[exp.length - 1];
              } else {
                result = exp.slice(1);
              }
              i--;
            }
            if (Array.isArray(result)) {
              result = result[0];
            }
            return result;
          })() : exp[1];
          if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
            const x: any = lastCdr(lst);
            return Array.isArray(x) && (x.length === 0);
          })()) {
            let i: any = 1;
            let result: any = lst;
            while (i > 0) {
              if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                result = lst[lst.length - 1];
              } else {
                result = lst.slice(1);
              }
              i--;
            }
            if (Array.isArray(result)) {
              result = result[0];
            }
            return result;
          } else {
            return lst[1];
          }
        })();
        if (typeof moduleName === 'symbol') {
          moduleName = moduleName.description as string;
        }
        moduleName = moduleName.replace(new RegExp('^\\./'), '');
        if (this.moduleMap && this.moduleMap.has(moduleName)) {
          module = this.moduleMap.get(moduleName);
          env = module.getEnvironment();
        } else {
          env = undefined;
        }
        for (let exp1 of ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(exp);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = exp;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = exp[exp.length - 1];
            } else {
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
            imported = (Array.isArray(exp1) && (exp1.length >= 3) && (exp1[exp1.length - 2] === Symbol.for('.')) && ((): any => {
              const x: any = lastCdr(exp1);
              return Array.isArray(x) && (x.length === 0);
            })()) ? ((): any => {
              let i: any = 1;
              let result: any = exp1;
              while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                  result = exp1[exp1.length - 1];
                } else {
                  result = exp1.slice(1);
                }
                i--;
              }
              if (Array.isArray(result)) {
                result = result[0];
              }
              return result;
            })() : exp1[1];
          } else {
            local = exp1;
            imported = exp1;
          }
          moduleEnv.setLocal(imported, true, 'variable');
          if (env) {
            const [f, fType]: any[] = env.getTypedValue(local);
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
      let exp: any = node.getValue();
      if (definitionp(exp) || macroDefinitionP(exp)) {
        // Evaluate `define` and `defmacro` forms in the module
        // environment. Be error-tolerant since the module
        // environment is not needed in many cases.
        let name: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
          const x: any = lastCdr(exp);
          return Array.isArray(x) && (x.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = exp;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = exp[exp.length - 1];
            } else {
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
        const typ: any = macroDefinitionP(exp) ? 'macro' : 'procedure';
        moduleEnv.setLocal(name, thunk(function (): any {
          let result: any = undefined;
          try {
            result = eval_(exp, moduleEnv);
          } catch (e) {
            if (e instanceof Error) {
            } else {
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

  setModuleMap(moduleMap: any): any {
    this.moduleMap = moduleMap;
    return this;
  }

  setNodes(nodes: any): any {
    this.mainNodes = nodes;
    this.mainExpressions = nodes.map(function (x: any): any {
      return x.getValue();
    });
    return this;
  }

  setExpressions(expressions: any = []): any {
    return this.expressions = expressions;
  }

  setInlineLispSourcesFlag(val: any): any {
    return this.inlineLispSourcesFlag = val;
  }

  getInlineLispSourcesFlag(): any {
    return this.inlineLispSourcesFlag;
  }
}

/**
 * Convert a map of `module` forms to a map of `Module` objects,
 * interlinking them in the process.
 */
function makeModuleMap(moduleExpressionMap: any, env: any): any {
  let moduleMap: any = new ThunkedMap();
  for (let key of moduleExpressionMap.keys()) {
    moduleMap.set(key, thunk(function (): any {
      let val: any = moduleExpressionMap.get(key);
      const m: any = (val instanceof Module) ? val : moduleExpressionToModuleObject(val, env);
      m.setModuleMap(moduleMap);
      return m;
    }));
  }
  return moduleMap;
}

makeModuleMap.lispSource = [Symbol.for('define'), [Symbol.for('make-module-map'), Symbol.for('module-expression-map'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('module-map'), [Symbol.for('new'), Symbol.for('ThunkedMap')]], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('send'), Symbol.for('module-expression-map'), Symbol.for('keys')]]], [Symbol.for('send'), Symbol.for('module-map'), Symbol.for('set'), Symbol.for('key'), [Symbol.for('thunk'), [Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('module-expression-map'), Symbol.for('get'), Symbol.for('key')]], [Symbol.for('define'), Symbol.for('m'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('val'), Symbol.for('Module')], Symbol.for('val'), [Symbol.for('module-expression-to-module-object'), Symbol.for('val'), Symbol.for('env')]]], [Symbol.for('send'), Symbol.for('m'), Symbol.for('set-module-map'), Symbol.for('module-map')], Symbol.for('m')]]]], Symbol.for('module-map')];

/**
 * Convert a `(module ...)` expression to a
 * `Module` object.
 */
function moduleExpressionToModuleObject(node: any, env: any): any {
  let name: any = node.get(1).getValue();
  if (typeof name === 'symbol') {
    name = name.description as string;
  }
  return new Module(node.drop(3), env, name);
}

moduleExpressionToModuleObject.lispSource = [Symbol.for('define'), [Symbol.for('module-expression-to-module-object'), Symbol.for('node'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('name')], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('symbol->string'), Symbol.for('name')]]], [Symbol.for('new'), Symbol.for('Module'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3], Symbol.for('env'), Symbol.for('name')]];

/**
 * Whether `env` extends the Lisp environment.
 */
function extendsLispEnvironmentP(env: any): any {
  // TODO: Check `parent`.
  return (env === lispEnvironment) || ((env instanceof EnvironmentStack) && env.hasEnvironment(lispEnvironment));
}

extendsLispEnvironmentP.lispSource = [Symbol.for('define'), [Symbol.for('extends-lisp-environment?'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('env'), Symbol.for('lisp-environment')], [Symbol.for('and'), [Symbol.for('is-a?'), Symbol.for('env'), Symbol.for('EnvironmentStack')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('has-environment'), Symbol.for('lisp-environment')]]]];

/**
 * Extract the module name from a `(require ...)` expression.
 */
function getModuleName(nameObj: any): any {
  let name: any = nameObj;
  if (typeof name === 'symbol') {
    name = name.description as string;
  }
  name = name.replace(new RegExp('^\\./'), '');
  return name;
}

getModuleName.lispSource = [Symbol.for('define'), [Symbol.for('get-module-name'), Symbol.for('name-obj')], [Symbol.for('define'), Symbol.for('name'), Symbol.for('name-obj')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('name')], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('symbol->string'), Symbol.for('name')]]], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\./'], Symbol.for('name'), '']], Symbol.for('name')];

/**
 * Return the current environment.
 */
function currentCompilationOptions(): any {
  return currentCompilationOptionsPointer;
}

currentCompilationOptions.lispSource = [Symbol.for('define'), [Symbol.for('current-compilation-options')], Symbol.for('current-compilation-options-pointer')];

/**
 * Run `f` with `current-compilation-options-pointer` bound to `options`.
 * The return value is the result of invoking `f`.
 */
function withCompilationOptions(options: any, f: any): any {
  let result: any = undefined;
  const tmp: any = currentCompilationOptionsPointer;
  try {
    currentCompilationOptionsPointer = options;
    result = f();
  } finally {
    currentCompilationOptionsPointer = tmp;
  }
  return result;
}

withCompilationOptions.lispSource = [Symbol.for('define'), [Symbol.for('with-compilation-options'), Symbol.for('options'), Symbol.for('f')], [Symbol.for('let'), [[Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('tmp'), Symbol.for('current-compilation-options-pointer')]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('current-compilation-options-pointer'), Symbol.for('options')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f')]], [Symbol.for('finally'), [Symbol.for('set!'), Symbol.for('current-compilation-options-pointer'), Symbol.for('tmp')]]], Symbol.for('result')]];

/**
 * Whether an expression is a definition.
 */
function definitionp(exp: any): any {
  return taggedListP(exp, Symbol.for('define'));
}

definitionp.lispSource = [Symbol.for('define'), [Symbol.for('definition?'), Symbol.for('exp')], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define')]]];

/**
 * Whether an expression is a function definition.
 */
function functionDefinitionP(exp: any): any {
  return definitionp(exp) && (((): any => {
    const obj: any = (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(exp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = exp[exp.length - 1];
        } else {
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
  })() || functionExpressionP((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 2;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
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
function functionExpressionP(exp: any): any {
  return taggedListP(exp, Symbol.for('lambda')) || taggedListP(exp, Symbol.for('js/function')) || taggedListP(exp, Symbol.for('js/arrow'));
}

functionExpressionP.lispSource = [Symbol.for('define'), [Symbol.for('function-expression?'), Symbol.for('exp')], [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('lambda')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('js/function')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('js/arrow')]]]];

/**
 * Whether an expression is a macro definition.
 */
function macroDefinitionP(exp: any): any {
  return taggedListP(exp, Symbol.for('define-macro')) || taggedListP(exp, Symbol.for('defmacro'));
}

macroDefinitionP.lispSource = [Symbol.for('define'), [Symbol.for('macro-definition?'), Symbol.for('exp')], [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define-macro')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('defmacro')]]]];

/**
 * Lisp environment.
 */
const lispEnvironment: any = new LispEnvironment([[Symbol.for('_'), __, 'variable'], [Symbol.for('__'), __, 'variable'], [Symbol.for('#f'), false_, 'variable'], [Symbol.for('#t'), true_, 'variable'], [Symbol.for('#u'), undefined_, 'variable'], [Symbol.for('false'), false_, 'variable'], [Symbol.for('nil'), null_, 'variable'], [Symbol.for('null'), null_, 'variable'], [Symbol.for('js/null'), jsNull_, 'variable'], [Symbol.for('js-null'), jsNull_, 'variable'], [Symbol.for('t'), true_, 'variable'], [Symbol.for('true'), true_, 'variable'], [Symbol.for('js-undefined'), undefined_, 'variable'], [Symbol.for('js/undefined'), undefined_, 'variable'], [Symbol.for('undefined'), undefined_, 'variable'], [Symbol.for('*cons-dot*'), consDot_, 'variable'], [Symbol.for('$'), funcall_, 'procedure'], [Symbol.for('%'), modulo_, 'procedure'], [Symbol.for('*'), mul_, 'procedure'], [Symbol.for('+'), add_, 'procedure'], [Symbol.for('-'), sub_, 'procedure'], [Symbol.for('/'), div_, 'procedure'], [Symbol.for('<'), lt_, 'procedure'], [Symbol.for('<='), lte_, 'procedure'], [Symbol.for('='), eqp_, 'procedure'], [Symbol.for('=?'), eqp_, 'procedure'], [Symbol.for('>'), gt_, 'procedure'], [Symbol.for('>='), gte_, 'procedure'], [Symbol.for('add'), add_, 'procedure'], [Symbol.for('add1'), add1_, 'procedure'], [Symbol.for('aget'), arrayRef_, 'procedure'], [Symbol.for('append'), append_, 'procedure'], [Symbol.for('apply'), apply_, 'procedure'], [Symbol.for('aref'), arrayRef_, 'procedure'], [Symbol.for('array-drop'), arrayDrop_, 'procedure'], [Symbol.for('array-drop-right'), arrayDropRight_, 'procedure'], [Symbol.for('array-eighth'), arrayEighth_, 'procedure'], [Symbol.for('array-fifth'), arrayFifth_, 'procedure'], [Symbol.for('array-first'), arrayFirst_, 'procedure'], [Symbol.for('array-fourth'), arrayFourth_, 'procedure'], [Symbol.for('array-get'), arrayRef_, 'procedure'], [Symbol.for('array-last'), arrayLast_, 'procedure'], [Symbol.for('array-length'), arrayLength_, 'procedure'], [Symbol.for('array-list->linked-list'), arrayListToLinkedList_, 'procedure'], [Symbol.for('array-list-car'), car_, 'procedure'], [Symbol.for('array-list-cdr'), arrayListCdr_, 'procedure'], [Symbol.for('array-list-drop'), arrayListDrop_, 'procedure'], [Symbol.for('array-list-drop-right'), arrayListDropRight_, 'procedure'], [Symbol.for('array-list-eighth'), arrayListEighth_, 'procedure'], [Symbol.for('array-list-fifth'), arrayListFifth_, 'procedure'], [Symbol.for('array-list-first'), arrayListFirst_, 'procedure'], [Symbol.for('array-list-fourth'), arrayListFourth_, 'procedure'], [Symbol.for('array-list-last'), arrayListLast_, 'procedure'], [Symbol.for('array-list-length'), arrayListLength_, 'procedure'], [Symbol.for('array-list-ninth'), arrayListNinth_, 'procedure'], [Symbol.for('array-list-nth'), arrayListNth_, 'procedure'], [Symbol.for('array-list-nthcdr'), arrayListNthcdr_, 'procedure'], [Symbol.for('array-list-rest'), arrayListRest_, 'procedure'], [Symbol.for('array-list-reverse'), arrayListReverse_, 'procedure'], [Symbol.for('array-list-second'), arrayListSecond_, 'procedure'], [Symbol.for('array-list-seventh'), arrayListSeventh_, 'procedure'], [Symbol.for('array-list-sixth'), arrayListSixth_, 'procedure'], [Symbol.for('array-list-take'), arrayListTake_, 'procedure'], [Symbol.for('array-list-tenth'), arrayListTenth_, 'procedure'], [Symbol.for('array-list-third'), arrayListThird_, 'procedure'], [Symbol.for('array-list?'), arrayListP_, 'procedure'], [Symbol.for('array-ninth'), arrayNinth_, 'procedure'], [Symbol.for('array-ref'), arrayRef_, 'procedure'], [Symbol.for('array-rest'), arrayRest_, 'procedure'], [Symbol.for('array-reverse'), arrayReverse_, 'procedure'], [Symbol.for('array-second'), arraySecond_, 'procedure'], [Symbol.for('array-set'), arraySet_, 'procedure'], [Symbol.for('array-set!'), arraySet_, 'procedure'], [Symbol.for('array-seventh'), arraySeventh_, 'procedure'], [Symbol.for('array-sixth'), arraySixth_, 'procedure'], [Symbol.for('array-take'), arrayTake_, 'procedure'], [Symbol.for('array-tenth'), arrayTenth_, 'procedure'], [Symbol.for('array-third'), arrayThird_, 'procedure'], [Symbol.for('array?'), arrayp_, 'procedure'], [Symbol.for('aset'), arraySet_, 'procedure'], [Symbol.for('aset!'), arraySet_, 'procedure'], [Symbol.for('assert'), assert_, 'procedure'], [Symbol.for('boolean?'), booleanp_, 'procedure'], [Symbol.for('booleanp'), booleanp_, 'procedure'], [Symbol.for('build-list'), buildList_, 'procedure'], [Symbol.for('cadr'), cadr_, 'procedure'], [Symbol.for('call-cc'), callWithCurrentContinuation_, 'procedure'], [Symbol.for('call-with-current-continuation'), callWithCurrentContinuation_, 'procedure'], [Symbol.for('call/cc'), callWithCurrentContinuation_, 'procedure'], [Symbol.for('car'), car_, 'procedure'], [Symbol.for('cdr'), cdr_, 'procedure'], [Symbol.for('circular-list-p'), circularListP_, 'procedure'], [Symbol.for('circular-list?'), circularListP_, 'procedure'], [Symbol.for('cons'), cons_, 'procedure'], [Symbol.for('cons*'), listStar_, 'procedure'], [Symbol.for('cons-dot'), consDotF_, 'procedure'], [Symbol.for('cons-dot?'), consDotP_, 'procedure'], [Symbol.for('cons?'), consp_, 'procedure'], [Symbol.for('console.log'), console.log, 'procedure'], [Symbol.for('consp'), consp_, 'procedure'], [Symbol.for('const'), const_, 'procedure'], [Symbol.for('constantly'), const_, 'procedure'], [Symbol.for('current-environment'), currentEnvironment_, 'procedure'], [Symbol.for('curry'), curry, 'procedure'], [Symbol.for('curry-n'), curryN, 'procedure'], [Symbol.for('delete'), jsDelete_, 'procedure'], [Symbol.for('display'), display_, 'procedure'], [Symbol.for('div'), div_, 'procedure'], [Symbol.for('dotted-list->proper-list'), linkedListToArrayList_, 'procedure'], [Symbol.for('dotted-list-car'), linkedListCar_, 'procedure'], [Symbol.for('dotted-list-cdr'), linkedListCdr_, 'procedure'], [Symbol.for('dotted-list-head'), linkedListHead_, 'procedure'], [Symbol.for('dotted-list-last'), linkedListLast_, 'procedure'], [Symbol.for('dotted-list-last-cdr'), linkedListLastCdr_, 'procedure'], [Symbol.for('dotted-list-length'), linkedListLength_, 'procedure'], [Symbol.for('dotted-list-nth'), linkedListNth_, 'procedure'], [Symbol.for('dotted-list-nthcdr'), linkedListNthcdr_, 'procedure'], [Symbol.for('dotted-list-p'), dottedListP_, 'procedure'], [Symbol.for('dotted-list-tail'), linkedListTail_, 'procedure'], [Symbol.for('dotted-list?'), dottedListP_, 'procedure'], [Symbol.for('dotted-pair-cdr'), linkedPairCdr_, 'procedure'], [Symbol.for('dotted-pair-p'), dottedPairP_, 'procedure'], [Symbol.for('dotted-pair?'), dottedPairP_, 'procedure'], [Symbol.for('drop'), listTail_, 'procedure'], [Symbol.for('drop-right'), dropRight_, 'procedure'], [Symbol.for('eighth'), eighth_, 'procedure'], [Symbol.for('eq'), eqp_, 'procedure'], [Symbol.for('eq?'), eqp_, 'procedure'], [Symbol.for('eql'), eqvp_, 'procedure'], [Symbol.for('eql?'), eqvp_, 'procedure'], [Symbol.for('equal'), equalp_, 'procedure'], [Symbol.for('equal?'), equalp_, 'procedure'], [Symbol.for('eqv'), eqvp_, 'procedure'], [Symbol.for('eqv?'), eqvp_, 'procedure'], [Symbol.for('error'), error_, 'procedure'], [Symbol.for('even?'), evenp_, 'procedure'], [Symbol.for('extend-environment'), extendEnvironment, 'procedure'], [Symbol.for('false?'), falsep_, 'procedure'], [Symbol.for('falsep'), falsep_, 'procedure'], [Symbol.for('fexpr?'), fexprp_, 'procedure'], [Symbol.for('fexprp'), fexprp_, 'procedure'], [Symbol.for('field-names'), fieldNames_, 'procedure'], [Symbol.for('fifth'), fifth_, 'procedure'], [Symbol.for('filter'), filter_, 'procedure'], [Symbol.for('findf'), findf_, 'procedure'], [Symbol.for('findf-index'), findfIndex_, 'procedure'], [Symbol.for('first'), first_, 'procedure'], [Symbol.for('flatten'), flatten_, 'procedure'], [Symbol.for('foldl'), foldl_, 'procedure'], [Symbol.for('foldr'), foldr_, 'procedure'], [Symbol.for('fourth'), fourth_, 'procedure'], [Symbol.for('funcall'), funcall_, 'procedure'], [Symbol.for('function-object?'), jsFunctionObjectP_, 'procedure'], [Symbol.for('function-type?'), jsFunctionTypeP_, 'procedure'], [Symbol.for('function?'), procedurep_, 'procedure'], [Symbol.for('functionp'), procedurep_, 'procedure'], [Symbol.for('gensym'), gensym_, 'procedure'], [Symbol.for('gensym?'), gensymp_, 'procedure'], [Symbol.for('get'), arrayRef_, 'procedure'], [Symbol.for('hash'), makeHash_, 'procedure'], [Symbol.for('hash->list'), hashToList_, 'procedure'], [Symbol.for('hash-clear'), hashClear_, 'procedure'], [Symbol.for('hash-clear!'), hashClearX_, 'procedure'], [Symbol.for('hash-copy'), hashCopy_, 'procedure'], [Symbol.for('hash-entries'), hashEntries_, 'procedure'], [Symbol.for('hash-has-key?'), hashHasKeyP_, 'procedure'], [Symbol.for('hash-keys'), hashKeys_, 'procedure'], [Symbol.for('hash-ref'), hashRef_, 'procedure'], [Symbol.for('hash-remove'), hashRemove_, 'procedure'], [Symbol.for('hash-remove!'), hashRemoveX_, 'procedure'], [Symbol.for('hash-set'), hashSetX_, 'procedure'], [Symbol.for('hash-set!'), hashSetX_, 'procedure'], [Symbol.for('hash-size'), hashSize_, 'procedure'], [Symbol.for('hash-values'), hashValues_, 'procedure'], [Symbol.for('hash?'), hashp_, 'procedure'], [Symbol.for('head'), car_, 'procedure'], [Symbol.for('id'), identity_, 'procedure'], [Symbol.for('identity'), identity_, 'procedure'], [Symbol.for('improper-list-p'), improperListP_, 'procedure'], [Symbol.for('improper-list?'), improperListP_, 'procedure'], [Symbol.for('in-range'), range_, 'procedure'], [Symbol.for('index-of'), indexOf_, 'procedure'], [Symbol.for('index-where'), indexWhere_, 'procedure'], [Symbol.for('instance-of'), isAP_, 'procedure'], [Symbol.for('instance-of?'), isAP_, 'procedure'], [Symbol.for('instanceof'), isAP_, 'procedure'], [Symbol.for('instanceof?'), isAP_, 'procedure'], [Symbol.for('intern'), stringToSymbol_, 'procedure'], [Symbol.for('intersection'), intersection_, 'procedure'], [Symbol.for('is-a?'), isAP_, 'procedure'], [Symbol.for('js'), js_, 'procedure'], [Symbol.for('js-field'), arrayRef_, 'procedure'], [Symbol.for('js-keys'), jsKeys_, 'procedure'], [Symbol.for('js-obj'), jsObj_, 'procedure'], [Symbol.for('js-obj-append'), jsObjAppend_, 'procedure'], [Symbol.for('js-obj-keys'), jsKeys_, 'procedure'], [Symbol.for('js-obj?'), jsObjP_, 'procedure'], [Symbol.for('js/+'), jsPlus_, 'procedure'], [Symbol.for('js/=='), jsIsLooselyEqualP_, 'procedure'], [Symbol.for('js/==='), jsIsStrictlyEqualP_, 'procedure'], [Symbol.for('js/===?'), jsIsStrictlyEqualP_, 'procedure'], [Symbol.for('js/==?'), jsIsLooselyEqualP_, 'procedure'], [Symbol.for('js/console.log'), console.log, 'procedure'], [Symbol.for('js/delete'), jsDelete_, 'procedure'], [Symbol.for('js/find-index'), jsFindIndex_, 'procedure'], [Symbol.for('js/findf-index'), jsFindIndex_, 'procedure'], [Symbol.for('js/function-object?'), jsFunctionObjectP_, 'procedure'], [Symbol.for('js/function-type?'), jsFunctionTypeP_, 'procedure'], [Symbol.for('js/function?'), jsFunctionP_, 'procedure'], [Symbol.for('js/in'), jsIn_, 'procedure'], [Symbol.for('js/instanceof'), jsInstanceof_, 'procedure'], [Symbol.for('js/instanceof?'), jsInstanceof_, 'procedure'], [Symbol.for('js/is-loosely-equal?'), jsIsLooselyEqualP_, 'procedure'], [Symbol.for('js/is-strictly-equal?'), jsIsStrictlyEqualP_, 'procedure'], [Symbol.for('js/js-obj'), jsObj_, 'procedure'], [Symbol.for('js/js-obj-append'), jsObjAppend_, 'procedure'], [Symbol.for('js/js-obj?'), jsObjP_, 'procedure'], [Symbol.for('js/new'), new_, 'procedure'], [Symbol.for('js/null?'), jsNullP_, 'procedure'], [Symbol.for('js/obj'), jsObj_, 'procedure'], [Symbol.for('js/obj-append'), jsObjAppend_, 'procedure'], [Symbol.for('js/obj?'), jsObjP_, 'procedure'], [Symbol.for('js/object-type?'), jsObjectTypeP_, 'procedure'], [Symbol.for('js/object?'), jsObjectTypeP_, 'procedure'], [Symbol.for('js/regexp'), regexp_, 'procedure'], [Symbol.for('js/regexp-quote'), regexpQuote_, 'procedure'], [Symbol.for('js/regexp?'), regexpp_, 'procedure'], [Symbol.for('js/same-value-zero?'), jsSameValueZeroP_, 'procedure'], [Symbol.for('js/same-value?'), jsSameValueP_, 'procedure'], [Symbol.for('js/tag'), jsTaggedTemplate_, 'procedure'], [Symbol.for('js/tagged-template'), jsTaggedTemplate_, 'procedure'], [Symbol.for('js/typeof'), jsTypeof_, 'procedure'], [Symbol.for('keyword?'), keywordp_, 'procedure'], [Symbol.for('keywordp'), keywordp_, 'procedure'], [Symbol.for('last'), last_, 'procedure'], [Symbol.for('last-cdr'), lastCdr_, 'procedure'], [Symbol.for('last-cons'), lastPair_, 'procedure'], [Symbol.for('last-pair'), lastPair_, 'procedure'], [Symbol.for('length'), length_, 'procedure'], [Symbol.for('length*'), length_, 'procedure'], [Symbol.for('linked-list-car'), linkedListCar_, 'procedure'], [Symbol.for('linked-list-cdr'), linkedListCdr_, 'procedure'], [Symbol.for('linked-list-eighth'), linkedListEighth_, 'procedure'], [Symbol.for('linked-list-fifth'), linkedListFifth_, 'procedure'], [Symbol.for('linked-list-first'), linkedListFirst_, 'procedure'], [Symbol.for('linked-list-fourth'), linkedListFourth_, 'procedure'], [Symbol.for('linked-list-head'), linkedListHead_, 'procedure'], [Symbol.for('linked-list-last'), linkedListLast_, 'procedure'], [Symbol.for('linked-list-last-cdr'), linkedListLastCdr_, 'procedure'], [Symbol.for('linked-list-length'), linkedListLength_, 'procedure'], [Symbol.for('linked-list-link-car'), linkedListLinkCar_, 'procedure'], [Symbol.for('linked-list-link-cdr'), linkedListLinkCdr_, 'procedure'], [Symbol.for('linked-list-link-p'), linkedListLinkP_, 'procedure'], [Symbol.for('linked-list-link?'), linkedListLinkP_, 'procedure'], [Symbol.for('linked-list-ninth'), linkedListNinth_, 'procedure'], [Symbol.for('linked-list-nth'), linkedListNth_, 'procedure'], [Symbol.for('linked-list-nthcdr'), linkedListNthcdr_, 'procedure'], [Symbol.for('linked-list-p'), linkedListP_, 'procedure'], [Symbol.for('linked-list-second'), linkedListSecond_, 'procedure'], [Symbol.for('linked-list-seventh'), linkedListSeventh_, 'procedure'], [Symbol.for('linked-list-sixth'), linkedListSixth_, 'procedure'], [Symbol.for('linked-list-tail'), linkedListTail_, 'procedure'], [Symbol.for('linked-list-tenth'), linkedListTenth_, 'procedure'], [Symbol.for('linked-list-third'), linkedListThird_, 'procedure'], [Symbol.for('linked-list?'), linkedListP_, 'procedure'], [Symbol.for('linked-pair-car'), linkedPairCar_, 'procedure'], [Symbol.for('linked-pair-cdr'), linkedPairCdr_, 'procedure'], [Symbol.for('linked-pair?'), linkedPairP_, 'procedure'], [Symbol.for('list'), list_, 'procedure'], [Symbol.for('list*'), listStar_, 'procedure'], [Symbol.for('list-ref'), nth_, 'procedure'], [Symbol.for('list-set'), arraySet_, 'procedure'], [Symbol.for('list-set!'), arraySet_, 'procedure'], [Symbol.for('list-star'), listStar_, 'procedure'], [Symbol.for('list-tail'), listTail_, 'procedure'], [Symbol.for('list?'), listp_, 'procedure'], [Symbol.for('listp'), listp_, 'procedure'], [Symbol.for('log'), console.log, 'procedure'], [Symbol.for('make'), new_, 'procedure'], [Symbol.for('make-hash'), makeHash_, 'procedure'], [Symbol.for('make-list'), makeList_, 'procedure'], [Symbol.for('make-object'), new_, 'procedure'], [Symbol.for('map'), map_, 'procedure'], [Symbol.for('mapcar'), map_, 'procedure'], [Symbol.for('member'), member_, 'procedure'], [Symbol.for('member-p'), memberp_, 'procedure'], [Symbol.for('member?'), memberp_, 'procedure'], [Symbol.for('memberp'), memberp_, 'procedure'], [Symbol.for('memf'), memf_, 'procedure'], [Symbol.for('memf?'), memfp_, 'procedure'], [Symbol.for('memq'), memq_, 'procedure'], [Symbol.for('memq?'), memqp_, 'procedure'], [Symbol.for('mod'), modulo_, 'procedure'], [Symbol.for('modulo'), modulo_, 'procedure'], [Symbol.for('mul'), mul_, 'procedure'], [Symbol.for('new'), new_, 'procedure'], [Symbol.for('new*'), new_, 'procedure'], [Symbol.for('ninth'), ninth_, 'procedure'], [Symbol.for('not'), not_, 'procedure'], [Symbol.for('nth'), nth_, 'procedure'], [Symbol.for('nthcdr'), nthcdr_, 'procedure'], [Symbol.for('null?'), nullp_, 'procedure'], [Symbol.for('nullp'), nullp_, 'procedure'], [Symbol.for('number->string'), numberToString_, 'procedure'], [Symbol.for('number?'), numberp_, 'procedure'], [Symbol.for('numberp'), numberp_, 'procedure'], [Symbol.for('object?'), jsObjP_, 'procedure'], [Symbol.for('objectp'), jsObjP_, 'procedure'], [Symbol.for('odd?'), oddp_, 'procedure'], [Symbol.for('oget'), objectRef_, 'procedure'], [Symbol.for('one?'), onep_, 'procedure'], [Symbol.for('onep'), onep_, 'procedure'], [Symbol.for('oref'), arrayRef_, 'procedure'], [Symbol.for('oset'), objectSetX_, 'procedure'], [Symbol.for('oset!'), objectSetX_, 'procedure'], [Symbol.for('plist->alist'), plistToAlist_, 'procedure'], [Symbol.for('plist-copy'), plistCopy_, 'procedure'], [Symbol.for('plist-get'), plistGet_, 'procedure'], [Symbol.for('plist-has'), plistHasP_, 'procedure'], [Symbol.for('plist-has?'), plistHasP_, 'procedure'], [Symbol.for('plist-ref'), plistGet_, 'procedure'], [Symbol.for('plist-set'), plistSetX_, 'procedure'], [Symbol.for('plist-set!'), plistSetX_, 'procedure'], [Symbol.for('plist?'), plistp_, 'procedure'], [Symbol.for('pop'), popLeftX_, 'procedure'], [Symbol.for('pop!'), popLeftX_, 'procedure'], [Symbol.for('pop-left'), popLeftX_, 'procedure'], [Symbol.for('pop-left!'), popLeftX_, 'procedure'], [Symbol.for('pop-right'), popRightX_, 'procedure'], [Symbol.for('pop-right!'), popRightX_, 'procedure'], [Symbol.for('print'), print, 'procedure'], [Symbol.for('print-estree'), printEstree, 'procedure'], [Symbol.for('procedure?'), procedurep_, 'procedure'], [Symbol.for('proper-list->dotted-list'), arrayListToLinkedList_, 'procedure'], [Symbol.for('proper-list-p'), properListP_, 'procedure'], [Symbol.for('proper-list?'), properListP_, 'procedure'], [Symbol.for('push'), pushLeftX_, 'procedure'], [Symbol.for('push!'), pushLeftX_, 'procedure'], [Symbol.for('push-left'), pushLeftX_, 'procedure'], [Symbol.for('push-left!'), pushLeftX_, 'procedure'], [Symbol.for('push-right'), pushRightX_, 'procedure'], [Symbol.for('push-right!'), pushRightX_, 'procedure'], [Symbol.for('range'), range_, 'procedure'], [Symbol.for('re'), regexp_, 'procedure'], [Symbol.for('re-pattern'), regexp_, 'procedure'], [Symbol.for('regexp'), regexp_, 'procedure'], [Symbol.for('regexp-match'), regexpMatch_, 'procedure'], [Symbol.for('regexp-match?'), regexpMatchP_, 'procedure'], [Symbol.for('regexp-quote'), regexpQuote_, 'procedure'], [Symbol.for('regexp-replace'), regexpReplace_, 'procedure'], [Symbol.for('regexp?'), regexpp_, 'procedure'], [Symbol.for('rest'), rest_, 'procedure'], [Symbol.for('reverse'), reverse_, 'procedure'], [Symbol.for('rx'), regexp_, 'procedure'], [Symbol.for('scm/new'), new_, 'procedure'], [Symbol.for('second'), second_, 'procedure'], [Symbol.for('self-evaluating?'), selfEvaluatingP_, 'procedure'], [Symbol.for('set-car!'), setCarX_, 'procedure'], [Symbol.for('set-cdr!'), setCdrX_, 'procedure'], [Symbol.for('set-mcar!'), setCarX_, 'procedure'], [Symbol.for('set-mcdr!'), setCdrX_, 'procedure'], [Symbol.for('set-nth'), arraySet_, 'procedure'], [Symbol.for('set-nth!'), arraySet_, 'procedure'], [Symbol.for('seventh'), seventh_, 'procedure'], [Symbol.for('sixth'), sixth_, 'procedure'], [Symbol.for('source'), source, 'procedure'], [Symbol.for('string->number'), stringToNumber_, 'procedure'], [Symbol.for('string->symbol'), stringToSymbol_, 'procedure'], [Symbol.for('string-append'), stringAppend_, 'procedure'], [Symbol.for('string-downcase'), stringDowncase_, 'procedure'], [Symbol.for('string-join'), stringJoin_, 'procedure'], [Symbol.for('string-length'), length_, 'procedure'], [Symbol.for('string-object?'), stringObjectP_, 'procedure'], [Symbol.for('string-primitive?'), stringPrimitiveP_, 'procedure'], [Symbol.for('string-ref'), stringRef_, 'procedure'], [Symbol.for('string-repeat'), stringRepeat_, 'procedure'], [Symbol.for('string-replace'), stringReplace_, 'procedure'], [Symbol.for('string-split'), stringSplit_, 'procedure'], [Symbol.for('string-to-symbol'), stringToSymbol_, 'procedure'], [Symbol.for('string-trim'), stringTrim_, 'procedure'], [Symbol.for('string-upcase'), stringUpcase_, 'procedure'], [Symbol.for('string?'), stringp_, 'procedure'], [Symbol.for('stringp'), stringp_, 'procedure'], [Symbol.for('sub'), sub_, 'procedure'], [Symbol.for('sub1'), sub1_, 'procedure'], [Symbol.for('substring'), substring_, 'procedure'], [Symbol.for('symbol->string'), symbolToString_, 'procedure'], [Symbol.for('symbol-to-string'), symbolToString_, 'procedure'], [Symbol.for('symbol?'), symbolp_, 'procedure'], [Symbol.for('symbolp'), symbolp_, 'procedure'], [Symbol.for('tail'), cdr_, 'procedure'], [Symbol.for('take'), take_, 'procedure'], [Symbol.for('tenth'), tenth_, 'procedure'], [Symbol.for('third'), third_, 'procedure'], [Symbol.for('true?'), truep_, 'procedure'], [Symbol.for('truep'), truep_, 'procedure'], [Symbol.for('type-of'), typeOf_, 'procedure'], [Symbol.for('typeof'), typeOf_, 'procedure'], [Symbol.for('undefined?'), undefinedp_, 'procedure'], [Symbol.for('union'), union_, 'procedure'], [Symbol.for('values'), values_, 'procedure'], [Symbol.for('vector'), list_, 'procedure'], [Symbol.for('vector-ref'), nth_, 'procedure'], [Symbol.for('vector-set'), arraySet_, 'procedure'], [Symbol.for('vector-set!'), arraySet_, 'procedure'], [Symbol.for('vector?'), arrayp_, 'procedure'], [Symbol.for('zero?'), zerop_, 'procedure'], [Symbol.for('zerop'), zerop_, 'procedure'], [Symbol.for('.'), dot_, 'macro'], [Symbol.for(':'), colon_, 'macro'], [quasiquoteSym_, quasiquote_, 'macro'], [quoteSym_, quote_, 'macro'], [Symbol.for('->'), threadFirst_, 'macro'], [Symbol.for('->>'), threadLast_, 'macro'], [Symbol.for('and'), and_, 'macro'], [Symbol.for('ann'), ann_, 'macro'], [Symbol.for('as->'), threadAs_, 'macro'], [Symbol.for('async'), jsAsync_, 'macro'], [Symbol.for('as~>'), threadAs_, 'macro'], [Symbol.for('await'), jsAwait_, 'macro'], [Symbol.for('begin'), begin_, 'macro'], [Symbol.for('begin0'), begin0_, 'macro'], [Symbol.for('block'), block_, 'macro'], [Symbol.for('break'), break_, 'macro'], [Symbol.for('call-method'), send_, 'macro'], [Symbol.for('case'), case_, 'macro'], [Symbol.for('case/eq'), caseEq_, 'macro'], [Symbol.for('class'), class_, 'macro'], [Symbol.for('clj/try'), cljTry_, 'macro'], [Symbol.for('cond'), cond_, 'macro'], [Symbol.for('continue'), continue_, 'macro'], [Symbol.for('defclass'), defclass_, 'macro'], [Symbol.for('define'), define_, 'macro'], [Symbol.for('define-class'), defineClass_, 'macro'], [Symbol.for('define-js-obj'), defineJsObj_, 'macro'], [Symbol.for('define-macro'), defineMacro_, 'macro'], [Symbol.for('define-type'), defineType_, 'macro'], [Symbol.for('define-values'), defineValues_, 'macro'], [Symbol.for('define/async'), defineAsync_, 'macro'], [Symbol.for('define/generator'), defineGenerator_, 'macro'], [Symbol.for('define/private'), definePrivate_, 'macro'], [Symbol.for('define/public'), definePublic_, 'macro'], [Symbol.for('defmacro'), defmacro_, 'macro'], [Symbol.for('defun'), defun_, 'macro'], [Symbol.for('destructuring-bind'), multipleValueBind_, 'macro'], [Symbol.for('do'), do_, 'macro'], [Symbol.for('field-bound?'), fieldBoundP_, 'macro'], [Symbol.for('fn'), lambda_, 'macro'], [Symbol.for('for'), for_, 'macro'], [Symbol.for('fset'), set_, 'macro'], [Symbol.for('get-field'), getField_, 'macro'], [Symbol.for('if'), if_, 'macro'], [Symbol.for('js/?.'), jsOptionalChaining_, 'macro'], [Symbol.for('js/arrow'), jsArrow_, 'macro'], [Symbol.for('js/async'), jsAsync_, 'macro'], [Symbol.for('js/await'), jsAwait_, 'macro'], [Symbol.for('js/do-while'), jsDoWhile_, 'macro'], [Symbol.for('js/for'), jsFor_, 'macro'], [Symbol.for('js/for-in'), jsForIn_, 'macro'], [Symbol.for('js/for-of'), jsForOf_, 'macro'], [Symbol.for('js/function'), jsFunction_, 'macro'], [Symbol.for('js/switch'), jsSwitch_, 'macro'], [Symbol.for('js/try'), jsTry_, 'macro'], [Symbol.for('js/while'), jsWhile_, 'macro'], [Symbol.for('lambda'), lambda_, 'macro'], [Symbol.for('let'), letStar_, 'macro'], [Symbol.for('let*'), letStar_, 'macro'], [Symbol.for('let*-values'), letValues_, 'macro'], [Symbol.for('let-env'), letEnv_, 'macro'], [Symbol.for('let-js-obj'), letJsObj_, 'macro'], [Symbol.for('let-values'), letValues_, 'macro'], [Symbol.for('letrec'), letStar_, 'macro'], [Symbol.for('letrec-values'), letValues_, 'macro'], [Symbol.for('module'), module_, 'macro'], [Symbol.for('multiple-value-bind'), multipleValueBind_, 'macro'], [Symbol.for('multiple-values-bind'), multipleValueBind_, 'macro'], [Symbol.for('new/apply'), newApply_, 'macro'], [Symbol.for('or'), or_, 'macro'], [Symbol.for('prog1'), begin0_, 'macro'], [Symbol.for('progn'), begin_, 'macro'], [Symbol.for('provide'), provide_, 'macro'], [Symbol.for('require'), require_, 'macro'], [Symbol.for('return'), return_, 'macro'], [Symbol.for('rkt/new'), rktNew_, 'macro'], [Symbol.for('send'), send_, 'macro'], [Symbol.for('send/apply'), sendApply_, 'macro'], [Symbol.for('set'), set_, 'macro'], [Symbol.for('set!'), setX_, 'macro'], [Symbol.for('set!-js-obj'), setJsObj_, 'macro'], [Symbol.for('set!-values'), setValues_, 'macro'], [Symbol.for('set-field!'), setField_, 'macro'], [Symbol.for('setq'), setX_, 'macro'], [Symbol.for('throw'), throw_, 'macro'], [Symbol.for('try'), cljTry_, 'macro'], [Symbol.for('unless'), unless_, 'macro'], [Symbol.for('unwind-protect'), unwindProtect_, 'macro'], [Symbol.for('when'), when_, 'macro'], [Symbol.for('while'), while_, 'macro'], [Symbol.for('yield'), yield_, 'macro'], [Symbol.for('~>'), threadFirst_, 'macro'], [Symbol.for('~>>'), threadLast_, 'macro'], [Symbol.for('λ'), lambda_, 'macro']]);

// (define/public ,define_ "macro")
// (new ,rkt-new_ "macro")
// (set!-field ,set-field_ "macro")
// (set-js-obj! ,set-js-obj_ "macro")
// (set-values! ,set-values_ "macro")
// Special forms, expressed as macros.
/**
 * Evaluation environment.
 */
const evalEnvironment: any = new LispEnvironment([[Symbol.for('eval'), interpret, 'procedure'], [Symbol.for('js/eval'), jsEval_, 'procedure'], [Symbol.for('scm/eval'), interpret, 'procedure'], [Symbol.for('seval'), eval_, 'procedure']]);

/**
 * JavaScript environment.
 */
const jsEnvironment: any = new JavaScriptEnvironment();

/**
 * Interpretation environment.
 * Includes `eval`.
 */
const interpretationEnvironment: any = new EnvironmentStack(lispEnvironment, evalEnvironment, jsEnvironment);

/**
 * Interpretation environment.
 * No `eval`.
 */
const interpretationEnvironmentNoEval: any = new EnvironmentStack(lispEnvironment, jsEnvironment);

/**
 * Compilation environment.
 */
const compilationEnvironment: any = new EnvironmentStack(lispEnvironment, evalEnvironment);

/**
 * Language environment.
 */
const langEnvironment: any = interpretationEnvironment;

/**
 * Default options used when compiling.
 */
const defaultCompilationOptions: any = {
  lispEnvironment: langEnvironment,
  compilationMappingEnvironment: compilationMappingEnv,
  inlineFunctions: true,
  gensymMap: new Map()
};

/**
 * Pointer to the current compilation options.
 */
let currentCompilationOptionsPointer: any = defaultCompilationOptions;

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

export {
  and_ as and,
  ann_ as ann,
  begin_ as begin,
  block_ as block,
  callWithCurrentContinuation_ as callWithCurrentContinuation,
  callWithCurrentContinuation_ as callCc,
  cljTry_ as try,
  cljTry_ as try_,
  colon_ as colon,
  compile as compileLisp,
  compile as compileLispToJavascript,
  cond_ as cond,
  defineAsync_ as defineAsync,
  defineClass_ as defineClass,
  defineGenerator_ as defineGenerator,
  defineJsObj_ as defineJsObj,
  defineMacro_ as defineMacro,
  definePublic_ as definePublic,
  defineType_ as defineType,
  defineValues_ as defineValues,
  define_ as define,
  dot_ as dot,
  getField_ as getField,
  jsAsync_ as async,
  jsAsync_ as async_,
  jsAsync_ as jsAsync,
  jsAwait_ as await,
  jsAwait_ as await_,
  jsAwait_ as jsAwait,
  js_ as js,
  lambda_ as compileFunction,
  lambda_ as fn,
  lambda_ as lambda,
  letJsObj_ as letJsObj,
  letStar_ as letStar,
  letStar_ as let_,
  letStar_ as letrec,
  letValues_ as letstarValues,
  letValues_ as letValues,
  letValues_ as letrecValues,
  lispEnvironment as lisp1Environment,
  new_ as jsNew,
  new_ as make,
  new_ as makeObject,
  new_ as makeObject_,
  new_ as newStar,
  new_ as rktMakeObject,
  new_ as scmNew,
  nop_ as nop,
  or_ as or,
  provide_ as provide,
  quasiquote_ as quasiquote,
  quote_ as quote,
  require_ as require,
  sendApply_ as sendApply,
  send_ as callMethod,
  send_ as send,
  setX_ as setX,
  setX_ as setq,
  setX_ as setq_,
  setField_ as setFieldX,
  setField_ as setField,
  setJsObj_ as setXJsObj,
  setJsObj_ as setJsObjX,
  setJsObj_ as setJsObj,
  setValues_ as setXValues,
  setValues_ as setValues,
  sexp as readFromString,
  Module,
  and_,
  ann_,
  applyOptimizations,
  begin_,
  break_,
  class_,
  cljTry_,
  colon_,
  compilationEnvironment,
  compile,
  compileFileX,
  compileFilesX,
  compileModuleMap,
  compileModules,
  cond_,
  continue_,
  defineToDefineClass,
  defineAsync_,
  defineGenerator_,
  defineJsObj_,
  defineMacroToFunction,
  defineMacroToLambdaForm,
  defineMacro_,
  defineType_,
  defineValues_,
  define_,
  definitionToMacro,
  dot_,
  findEstree,
  for_,
  getField_,
  interpret,
  interpretFiles,
  interpretString,
  interpretationEnvironment,
  isAP_,
  iterateRose,
  jsAsync_,
  jsAwait_,
  js_,
  lambda_,
  langEnvironment,
  letJsObj_,
  letStar_,
  letValues_,
  letVarsToConstVars,
  lisp,
  lispEnvironment,
  macroexpand,
  macroexpand1,
  macroexpandAll,
  macroexpandAllUntil,
  macroexpandUntil,
  makeLisp,
  makeModuleMap,
  mapRose,
  mapSexp,
  mapVisitRose,
  moduleExpressionToModuleObject,
  module_,
  new_,
  nop_,
  optimizations,
  optimizeEstree,
  optimizeModule,
  optimizeRose,
  optimizeSexp,
  or_,
  provide_,
  quasiquote_,
  quotep,
  quote_,
  read,
  readRose,
  readSexp,
  require_,
  return_,
  s,
  sendApply_,
  sendMethod,
  send_,
  setX_,
  setField_,
  setJsObj_,
  setValues_,
  sexp,
  source,
  sourcep,
  splitComments,
  throw_,
  tokenize,
  traverseEstree,
  typeOf_,
  yield_
};