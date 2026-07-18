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
  license,
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
  jsNew_ as new_,
  jsBlock_,
  jsDelete_,
  jsEighth_,
  jsEval_,
  jsFifth_,
  jsFindIndex_,
  jsFirst_,
  jsFourth_,
  jsFunctionObjectP_,
  jsFunctionTypeP_,
  jsFunctionP_,
  jsGet_,
  jsIn_,
  jsInstanceOfP_,
  jsLast_,
  jsLength_,
  jsLooselyEqualP_,
  jsNanP_,
  jsNew_,
  jsNinth_,
  jsNullP_,
  jsPlus_,
  jsReduceRight_,
  jsReduce_,
  jsRegexpMatch_,
  jsRegexpReplace_,
  jsRegexpP_,
  jsRegexp_,
  jsRest_,
  jsReturn_,
  jsReverse_,
  jsSameValueZeroP_,
  jsSameValueP_,
  jsSecond_,
  jsSeventh_,
  jsSixth_,
  jsSlice_,
  jsStrictlyEqualP_,
  jsTaggedTemplate_,
  jsTake_,
  jsTenth_,
  jsThird_,
  jsTypeOf_,
  jsYield_
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
  declare_,
  defclass_,
  definePrivate_,
  definePublic_,
  defmacro_,
  defun_,
  do_,
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
  compilerTypeP,
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
  intersection_,
  isAP_,
  keywordp_,
  lt_,
  lte_,
  macroTypeP,
  macrop_,
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
  procedureTypeP,
  procedurep_,
  range_,
  selfEvaluatingP_,
  specialTypeP,
  sub1_,
  sub_,
  truep_,
  typeOf_,
  undefinedTypeP,
  undefinedp_,
  union_,
  values_,
  variableTypeP,
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

const [lastCdr, cdr, flatten, buildList, listStar, keywordp, makeList, cons, findf, length]: any[] = ((): any => {
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
  function keywordp_(obj: any): any {
    return (typeof obj === 'symbol') && (obj.description as string).match(new RegExp('^:'));
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
  return [lastCdr_, cdr_, flatten_, buildList_, listStar_, keywordp_, makeList_, cons_, findf_, length_];
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

addDefaultOptions.fsource = [Symbol.for('define'), [Symbol.for('add-default-options'), Symbol.for('options'), [Symbol.for('modify'), false]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('if'), Symbol.for('modify'), Symbol.for('options'), [Symbol.for('js-obj-append'), Symbol.for('options')]]], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('js-keys'), Symbol.for('default-options')]]], [Symbol.for('when'), [Symbol.for('undefined?'), [Symbol.for('oget'), Symbol.for('result'), Symbol.for('key')]], [Symbol.for('oset!'), Symbol.for('result'), Symbol.for('key'), [Symbol.for('oget'), Symbol.for('default-options'), Symbol.for('key')]]]], Symbol.for('result')];

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
const compilationVariablesEnv: any = new CompilationEnvironment([[Symbol.for('#f'), new Literal(false), Symbol.for('Any')], [Symbol.for('#t'), new Literal(true), Symbol.for('Any')], [Symbol.for('#n'), new Literal(null), Symbol.for('Any')], [Symbol.for('#u'), new Identifier('undefined'), Symbol.for('Any')], [Symbol.for('js-null'), new Literal(null), Symbol.for('Any')], [Symbol.for('js-undefined'), new Identifier('undefined'), Symbol.for('Any')], [Symbol.for('js/arguments'), new Identifier('arguments'), Symbol.for('Any')], [Symbol.for('js/null'), new Literal(null), Symbol.for('Any')], [Symbol.for('js/require'), new Identifier('require'), Symbol.for('Any')], [Symbol.for('js/undefined'), new Identifier('undefined'), Symbol.for('Any')], [Symbol.for('*cons-dot*'), consDotCompiled_, Symbol.for('Any')], [Symbol.for('nil'), new ArrayExpression(), Symbol.for('Any')], [Symbol.for('null'), new ArrayExpression(), Symbol.for('Any')], [Symbol.for('t'), new Literal(true), Symbol.for('Any')], [Symbol.for('undefined'), new Identifier('undefined'), Symbol.for('Any')]]);

/**
 * Compiler procedures mapping environment.
 */
const compilationCompilerMappingEnv: any = new CompilationEnvironment([[add_, compileAdd, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [and_, compileAnd, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [ann_, compileAnn, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [append_, compileAppend, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [apply_, compileApply, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [arrayRef_, compileArrayRef, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [arraySet_, compileArraySet, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [begin_, compileBegin, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [break_, compileBreak, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [class_, compileClass, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [colon_, compileColon, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [cond_, compileCond, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [continue_, compileContinue, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [declareMacro_, compileDeclareMacro, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [defineAsync_, compileDefineAsync, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [defineClass_, compileDefineClass, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [defineFields_, compileDefineFields, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [defineGenerator_, compileDefineGenerator, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [defineMacro_, compileDefineMacro, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [defineType_, compileDefineType, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [defineValues_, compileDefineValues, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [define_, compileDefine, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [div_, compileDiv, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [dot_, compileSend, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [for_, compileFor, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [funcall_, compileFuncall, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [getField_, compileGetField, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [gt_, compileGreaterThan, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [gte_, compileGreaterThanOrEqual, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsArrow_, compileJsArrow, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsAsync_, compileJsAsync, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsAwait_, compileJsAwait, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsBlock_, compileJsBlock, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsDelete_, compileJsDelete, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsDoWhile_, compileJsDoWhile, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsEval_, compileJsEval, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsFunction_, compileJsFunction, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsGet_, compileJsGet, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsIn_, compileJsIn, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsInstanceOfP_, compileJsInstanceOf, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsNew_, compileJsNew, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsLooselyEqualP_, compileJsLooselyEqual, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsObjAppend_, compileJsObjAppend, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsObj_, compileJsObj, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsOptionalChaining_, compileJsOptionalChaining, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsPlus_, compileAdd, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsReturn_, compileReturn, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsStrictlyEqualP_, compileJsStrictlyEqual, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsSwitch_, compileJsSwitch, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsTaggedTemplate_, compileJsTaggedTemplate, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsTry_, compileJsTry, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsTypeOf_, compileJsTypeOf, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsWhile_, compileJsWhile, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsYield_, compileYield, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [js_, compileJs, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [lambda_, compileLambda, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [letFields_, compileLetFields, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [letStar_, compileLet, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [letValues_, compileLetValues, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [list_, compileList, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [lt_, compileLessThan, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [lte_, compileLessThanOrEqual, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [module_, compileModule, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [modulo_, compileModulo, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [mul_, compileMul, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [not_, compileNot, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [objectSetX_, compileObjectSet, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [or_, compileOr, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [provide_, compileProvide, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [pushLeftX_, compilePushLeft, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [pushRightX_, compilePushRight, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [quasiquote_, compileQuasiquote, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [quote_, compileQuote, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [require_, compileRequire, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [return_, compileReturn, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [sendApply_, compileSendApply, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [send_, compileSend, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [setX_, compileSet, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [setField_, compileSetField, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [setFields_, compileSetFields, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [setValues_, compileSetValues, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [stringAppend_, compileStringAppend, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [sub_, compileSub, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [throw_, compileThrow, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [yield_, compileYield, [Symbol.for('compiler->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]]);

/**
 * Compiler macros mapping environment.
 */
const compilationMacroMappingEnv: any = new CompilationEnvironment([[arrayDropRight_, compileArrayDropRightMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [arrayDrop_, compileArrayDropMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [arrayListDropRight_, compileArrayListDropRightMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [arrayListDrop_, compileArrayListDropMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [assert_, compileAssertMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [display_, compileDisplayMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [dropRight_, compileDropRightMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [drop_, compileDropMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [foldl_, compileFoldlMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [foldr_, compileFoldrMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [hashClear_, compileHashClearMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [hashRef_, compileHashRefMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [hashRemoveX_, compileHashRemoveMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [hashRemove_, compileHashRemoveMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [jsRegexp_, compileJsRegexpMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [makeHash_, compileMakeHashMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [map_, compileMapMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [memberp_, compileMemberPMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [print, compileDisplayMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [stringTrim_, compileStringTrimMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [stringp_, compileStringpMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [substring_, compileSubstringMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [values_, compileValuesMacro, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]]);

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
const inlinedFunctions: any = [add1_, arrayEighth_, arrayFifth_, arrayFirst_, arrayFourth_, arrayLast_, arrayLength_, arrayListCdr_, arrayListEighth_, arrayListFifth_, arrayListFirst_, arrayListFourth_, arrayListLast_, arrayListLength_, arrayListNinth_, arrayListNth_, arrayListNthcdr_, arrayListRest_, arrayListReverse_, arrayListSecond_, arrayListSeventh_, arrayListSixth_, arrayListTake_, arrayListTenth_, arrayListThird_, arrayListP_, arrayNinth_, arrayRest_, arrayReverse_, arraySecond_, arraySeventh_, arraySixth_, arrayTake_, arrayTenth_, arrayThird_, arrayp_, booleanp_, consDotF_, consDotP_, consp_, const_, dottedListP_, dottedPairP_, eighth_, eqp_, eqvp_, error_, evenp_, fieldNames_, fifth_, filter_, findfIndex_, first_, fourth_, gensymp_, gensym_, hashToList_, hashClearX_, hashCopy_, hashEntries_, hashHasKeyP_, hashKeys_, hashRemoveX_, hashSetX_, hashSize_, hashValues_, hashp_, indexWhere_, isAP_, jsEighth_, jsFifth_, jsFindIndex_, jsFirst_, jsFourth_, jsFunctionObjectP_, jsFunctionTypeP_, jsFunctionP_, jsKeys_, jsLast_, jsLength_, jsNanP_, jsNinth_, jsNullP_, jsObjP_, jsObjectTypeP_, jsReduceRight_, jsReduce_, jsRegexpMatch_, jsRegexpReplace_, jsRegexpP_, jsRest_, jsReverse_, jsSameValueP_, jsSecond_, jsSeventh_, jsSixth_, jsSlice_, jsTake_, jsTenth_, jsThird_, linkedListCar_, linkedListCdr_, linkedListEighth_, linkedListFifth_, linkedListFirst_, linkedListFourth_, linkedListHead_, linkedListLinkCar_, linkedListLinkCdr_, linkedListLinkP_, linkedListNinth_, linkedListNth_, linkedListNthcdr_, linkedListSecond_, linkedListSeventh_, linkedListSixth_, linkedListTail_, linkedListTenth_, linkedListThird_, linkedListP_, linkedPairCar_, linkedPairCdr_, linkedPairP_, listp_, memfp_, memqp_, ninth_, nth_, nullp_, numberToString_, numberp_, objectRef_, oddp_, onep_, plistCopy_, plistp_, popLeftX_, popRightX_, procedurep_, regexpMatchP_, regexpMatch_, regexpQuote_, regexpReplace_, regexpp_, rest_, reverse_, second_, seventh_, sixth_, stringToNumber_, stringToSymbol_, stringDowncase_, stringJoin_, stringObjectP_, stringPrimitiveP_, stringRef_, stringRepeat_, stringSplit_, stringUpcase_, sub1_, symbolToString_, symbolp_, tenth_, third_, typeOf_, undefinedp_, zerop_];

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
  let compilationOptions: any = addDefaultOptions(options, true);
  const compiledEnv: any = new LispEnvironment();
  const continuationEnv: any = new LispEnvironment([], langEnv);
  compilationOptions['languageEnvironment'] = langEnv;
  compilationOptions['compilationMappingEnvironment'] = mappingEnv;
  compilationOptions['compiledEnvironment'] = compiledEnv;
  compilationOptions = {
    ...defaultCompilationOptions,
    ...compilationOptions
  };
  return withCompilationOptions(compilationOptions, function (): any {
    let ast: any = (exp instanceof Module) ? compileModule(exp, continuationEnv, compilationOptions) : ((exp instanceof Rose) ? compileRose(exp, continuationEnv, compilationOptions) : compileSexp(exp, continuationEnv, compilationOptions));
    ast = optimizeEstree(ast);
    return printEstree(ast, compilationOptions);
  });
}

compile.fsource = [Symbol.for('define'), [Symbol.for('compile'), Symbol.for('exp'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language-option'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'language'], Symbol.for('default-language')]], [Symbol.for('define'), Symbol.for('lang-env'), [Symbol.for('if'), [Symbol.for('extends-lisp-environment?'), Symbol.for('env')], Symbol.for('env'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('env'), Symbol.for('lang-environment')]]], [Symbol.for('define'), Symbol.for('mapping-env'), [Symbol.for('or'), [Symbol.for('hash-ref'), Symbol.for('compilation-map'), Symbol.for('language-option')], Symbol.for('compilation-mapping-env')]], [Symbol.for('define'), Symbol.for('compilation-options'), [Symbol.for('add-default-options'), Symbol.for('options'), true]], [Symbol.for('define'), Symbol.for('compiled-env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('define'), Symbol.for('continuation-env'), [Symbol.for('new'), Symbol.for('LispEnvironment'), [Symbol.for('quote'), []], Symbol.for('lang-env')]], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'languageEnvironment', Symbol.for('lang-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'compilationMappingEnvironment', Symbol.for('mapping-env')], [Symbol.for('oset!'), Symbol.for('compilation-options'), 'compiledEnvironment', Symbol.for('compiled-env')], [Symbol.for('set!'), Symbol.for('compilation-options'), [Symbol.for('js-obj-append'), Symbol.for('default-compilation-options'), Symbol.for('compilation-options')]], [Symbol.for('with-compilation-options'), Symbol.for('compilation-options'), [Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('ast'), [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Module')], [Symbol.for('compile-module'), Symbol.for('exp'), Symbol.for('continuation-env'), Symbol.for('compilation-options')]], [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('compile-rose'), Symbol.for('exp'), Symbol.for('continuation-env'), Symbol.for('compilation-options')]], [Symbol.for('else'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('continuation-env'), Symbol.for('compilation-options')]]]], [Symbol.for('set!'), Symbol.for('ast'), [Symbol.for('optimize-estree'), Symbol.for('ast')]], [Symbol.for('print-estree'), Symbol.for('ast'), Symbol.for('compilation-options')]]]];

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
  return [...compiledModuleMap.values()];
}

compileModules.fsource = [Symbol.for('define'), [Symbol.for('compile-modules'), Symbol.for('modules'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('module-map'), [Symbol.for('make-hash')]], [Symbol.for('define'), Symbol.for('compiled-module-map')], [Symbol.for('define'), Symbol.for('module-name')], [Symbol.for('for'), [[Symbol.for('module'), Symbol.for('modules')]], [Symbol.for('unless'), [Symbol.for('is-a?'), Symbol.for('module'), Symbol.for('Rose')], [Symbol.for('set!'), Symbol.for('module'), [Symbol.for('make-rose'), Symbol.for('module')]]], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('module'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('_'), Symbol.for('get-value')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('module-name')], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('symbol->string'), Symbol.for('module-name')]]], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\./'], Symbol.for('module-name'), '']], [Symbol.for('hash-set!'), Symbol.for('module-map'), Symbol.for('module-name'), Symbol.for('module')]], [Symbol.for('set!'), Symbol.for('compiled-module-map'), [Symbol.for('compile-module-map'), Symbol.for('module-map'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('append'), [Symbol.for('send'), Symbol.for('compiled-module-map'), Symbol.for('values')]]];

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

compileModuleMap.fsource = [Symbol.for('define'), [Symbol.for('compile-module-map'), Symbol.for('module-map'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('make-hash')]], [Symbol.for('define'), Symbol.for('module-object-map'), [Symbol.for('make-module-map'), Symbol.for('module-map'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('compiled-module')], [Symbol.for('define'), Symbol.for('module')], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('send'), Symbol.for('module-object-map'), Symbol.for('keys')]]], [Symbol.for('set!'), Symbol.for('module'), [Symbol.for('send'), Symbol.for('module-object-map'), Symbol.for('get'), Symbol.for('key')]], [Symbol.for('set!'), Symbol.for('compiled-module'), [Symbol.for('compile'), Symbol.for('module'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('hash-set!'), Symbol.for('result'), Symbol.for('key'), Symbol.for('compiled-module')]], Symbol.for('result')];

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

compileModule.fsource = [Symbol.for('define'), [Symbol.for('compile-module'), Symbol.for('obj'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('obj'), Symbol.for('Module')], [Symbol.for('compile-module-object'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-module-expression'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(module ...)` expression.
 */
function compileModuleExpression(node: any, env: any, options: any = {}): any {
  let module: any = moduleExpressionToModuleObject(node, env);
  let compilationOptions: any = {
    ...options,
    currentModule: module
  };
  return compileModuleObject(module, env, compilationOptions);
}

compileModuleExpression.fsource = [Symbol.for('define'), [Symbol.for('compile-module-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('module'), [Symbol.for('module-expression-to-module-object'), Symbol.for('node'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('compilation-options'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'currentModule', Symbol.for('module')]]], [Symbol.for('compile-module-object'), Symbol.for('module'), Symbol.for('env'), Symbol.for('compilation-options')]];

/**
 * Compile a `Module` object.
 */
function compileModuleObject(module: any, env: any, options: any = {}): any {
  const expressions: any = module.getExpressions();
  const moduleEnvironment: any = module.getEnvironment();
  const moduleOptions: any = {
    currentModule: module,
    referencedSymbols: [],
    inlineLispSources: module.getInlineLispSourcesFlag(),
    ...options
  };
  const headerStatements: any = compileStatement(beginWrapRose(module.headerNodes), moduleEnvironment, moduleOptions);
  const requireStatements: any = compileStatement(beginWrapRose(module.requireNodes), moduleEnvironment, moduleOptions);
  const mainStatements: any = compileStatementOrReturnStatement(beginWrapRose(module.mainNodes), moduleEnvironment, moduleOptions);
  const provideStatements: any = compileStatement(beginWrapRose(module.provideNodes), moduleEnvironment, moduleOptions);
  const globalEnvironment: any = buildGlobalEnvironment(moduleOptions['referencedSymbols'], moduleEnvironment, options);
  const program: any = makeProgram([...headerStatements.body, ...requireStatements.body, ...globalEnvironment.body, ...mainStatements.body, ...provideStatements.body]);
  return program;
}

compileModuleObject.fsource = [Symbol.for('define'), [Symbol.for('compile-module-object'), Symbol.for('module'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expressions'), [Symbol.for('send'), Symbol.for('module'), Symbol.for('get-expressions')]], [Symbol.for('define'), Symbol.for('module-environment'), [Symbol.for('send'), Symbol.for('module'), Symbol.for('get-environment')]], [Symbol.for('define'), Symbol.for('module-options'), [Symbol.for('js-obj-append'), [Symbol.for('js-obj'), 'currentModule', Symbol.for('module'), 'referencedSymbols', [Symbol.for('quote'), []], 'inlineLispSources', [Symbol.for('send'), Symbol.for('module'), Symbol.for('get-inline-lisp-sources-flag')]], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('header-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('header-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('require-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('require-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('main-statements'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('main-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('provide-statements'), [Symbol.for('compile-statement'), [Symbol.for('begin-wrap-rose'), [Symbol.for('get-field'), Symbol.for('provide-nodes'), Symbol.for('module')]], Symbol.for('module-environment'), Symbol.for('module-options')]], [Symbol.for('define'), Symbol.for('global-environment'), [Symbol.for('build-global-environment'), [Symbol.for('oget'), Symbol.for('module-options'), 'referencedSymbols'], Symbol.for('module-environment'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('program'), [Symbol.for('make-program'), [Symbol.for('append'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('header-statements')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('require-statements')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('global-environment')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('main-statements')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('provide-statements')]]]], Symbol.for('program')];

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
  let compilationOptions: any = {
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

compileFilesX.fsource = [Symbol.for('define'), [Symbol.for('compile-files!'), Symbol.for('files'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('module-expression-map'), [Symbol.for('new'), Symbol.for('ThunkedMap')]], [Symbol.for('define'), Symbol.for('filename-map'), [Symbol.for('new'), Symbol.for('ThunkedMap')]], [Symbol.for('define'), Symbol.for('indent-option'), [Symbol.for('oget'), Symbol.for('options'), 'indent']], [Symbol.for('define'), Symbol.for('language-option'), [Symbol.for('if'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^TypeScript$', 'i'], [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'language'], '']], 'TypeScript', 'JavaScript']], [Symbol.for('define'), Symbol.for('out-dir-option'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'outDir'], '']], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('quick-option'), [Symbol.for('oget'), Symbol.for('options'), 'quick']], [Symbol.for('define'), Symbol.for('compilation-options'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'statement', 'language', Symbol.for('language-option')]]], [Symbol.for('define'), Symbol.for('extension'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language-option'), 'TypeScript'], '.ts', '.js']], [Symbol.for('define'), Symbol.for('code')], [Symbol.for('define'), Symbol.for('data')], [Symbol.for('define'), Symbol.for('module')], [Symbol.for('define'), Symbol.for('module-name')], [Symbol.for('define'), Symbol.for('module-names'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('module-map')], [Symbol.for('define'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('out-file')], [Symbol.for('for'), [[Symbol.for('file'), Symbol.for('files')]], [Symbol.for('set!'), Symbol.for('module-name'), [Symbol.for('basename'), Symbol.for('file'), [Symbol.for('extname'), Symbol.for('file')]]], [Symbol.for('hash-set!'), Symbol.for('filename-map'), Symbol.for('module-name'), Symbol.for('file')], [Symbol.for('hash-set!'), Symbol.for('module-expression-map'), Symbol.for('module-name'), [Symbol.for('thunk'), [Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('data'), [Symbol.for('~>'), Symbol.for('file'), [Symbol.for('readFileSync'), Symbol.for('_'), [Symbol.for('js-obj'), 'encoding', 'utf8']], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^#!.*'], Symbol.for('_'), ''], [Symbol.for('string-append'), '(module m scheme\n', Symbol.for('_'), '\n' +
  ')']]], [Symbol.for('define'), Symbol.for('node'), [Symbol.for('read-rose'), Symbol.for('data'), [Symbol.for('js-obj'), 'comments', Symbol.for('comments-option')]]], Symbol.for('node')]]], [Symbol.for('cond'), [Symbol.for('quick-option'), [Symbol.for('define'), Symbol.for('should-compile'), false], [Symbol.for('try'), [Symbol.for('define'), Symbol.for('in-file'), Symbol.for('file')], [Symbol.for('define'), Symbol.for('in-stats'), [Symbol.for('fstatSync'), [Symbol.for('openSync'), Symbol.for('in-file'), 'r']]], [Symbol.for('define'), Symbol.for('out-file'), [Symbol.for('join'), Symbol.for('out-dir-option'), [Symbol.for('string-append'), Symbol.for('module-name'), Symbol.for('extension')]]], [Symbol.for('define'), Symbol.for('out-stats'), [Symbol.for('fstatSync'), [Symbol.for('openSync'), Symbol.for('out-file'), 'r']]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('get-field'), Symbol.for('mtimeMs'), Symbol.for('in-stats')], [Symbol.for('get-field'), Symbol.for('mtimeMs'), Symbol.for('out-stats')]], [Symbol.for('set!'), Symbol.for('should-compile'), true]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('err'), [Symbol.for('set!'), Symbol.for('should-compile'), true]]], [Symbol.for('when'), Symbol.for('should-compile'), [Symbol.for('push-right!'), Symbol.for('module-names'), Symbol.for('module-name')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('module-names'), Symbol.for('module-name')]]]], [Symbol.for('set!'), Symbol.for('module-map'), [Symbol.for('make-module-map'), Symbol.for('module-expression-map'), Symbol.for('compilation-environment')]], [Symbol.for('for'), [[Symbol.for('module-name'), Symbol.for('module-names')]], [Symbol.for('set!'), Symbol.for('module'), [Symbol.for('send'), Symbol.for('module-map'), Symbol.for('get'), Symbol.for('module-name')]], [Symbol.for('set!'), Symbol.for('code'), [Symbol.for('compile'), Symbol.for('module'), Symbol.for('compilation-environment'), Symbol.for('compilation-options')]], [Symbol.for('set!'), Symbol.for('out-file'), [Symbol.for('join'), Symbol.for('out-dir-option'), [Symbol.for('string-append'), Symbol.for('module-name'), Symbol.for('extension')]]], [Symbol.for('mkdirSync'), Symbol.for('out-dir-option'), [Symbol.for('js-obj'), 'recursive', true]], [Symbol.for('writeFileSync'), Symbol.for('out-file'), Symbol.for('code'), [Symbol.for('js-obj'), 'encoding', 'utf8']], [Symbol.for('display'), [Symbol.for('string-append'), 'Compiled ', [Symbol.for('hash-ref'), Symbol.for('filename-map'), Symbol.for('module-name')], ' to ', Symbol.for('out-file')]]], Symbol.for('module-map')];

/**
 * Compile a file.
 * This function writes to disk.
 */
function compileFileX(infile: any, outfile: any, options: any = {}): any {
  // TODO: `outfile`. Maybe by adding an
  // `outFileMap` option to `compile-files!`?
  return compileFilesX([infile], options);
}

compileFileX.fsource = [Symbol.for('define'), [Symbol.for('compile-file!'), Symbol.for('infile'), Symbol.for('outfile'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-files!'), [Symbol.for('list'), Symbol.for('infile')], Symbol.for('options')]];

/**
 * Compile a S-expression wrapped in a rose tree.
 */
function compileRose(node: any, env: any, options: any = {}): any {
  const languageEnv: any = options['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
  const commentsOption: any = options['comments'];
  const node1: any = optimizeRose(node, env);
  let exp: any = node1.getValue();
  let result: any;
  if (Array.isArray(exp)) {
    if (exp.length === 0) {
      result = compileList(node1, env, options);
    } else {
      const op: any = exp[0];
      if ((typeof op === 'symbol') && env.has(op, {
        filter: langFilter
      }) && !macroTypeP(env.getType(op, {
        filter: langFilter
      }))) {
        result = compileFunctionCall(node1, env, options);
      } else if ((typeof op === 'symbol') && (op.description as string).match(new RegExp('^\\.'))) {
        result = compileDot(node1, env, options);
      } else {
        const [f, opType]: any[] = env.getTypedValue(op);
        if (undefinedTypeP(opType)) {
          result = compileFunctionCall(node1, env, options);
        } else if (inlinedFunctions.includes(f)) {
          const inlinedExp: any = definitionToMacro(source(f), exp.slice(1));
          const inlinedNode: any = makeRose(inlinedExp, node);
          result = compileRose(inlinedNode, env, options);
        } else {
          const compilationMappingEnvironment: any = options['compilationMappingEnvironment'];
          const [compilationF, compilationType]: any[] = compilationMappingEnvironment.getTypedValue(f);
          if (compilerTypeP(compilationType)) {
            // Compiler function.
            result = compilationF(node1, env, options);
          } else if (macroTypeP(compilationType)) {
            // Compilation macro.
            result = compileRose(insertSexpIntoRose(compilationF(exp, env), node1, node1), env, options);
          } else if (macroTypeP(opType)) {
            // Macro call.
            result = compileMacroCall(node1, env, options);
          } else {
            result = compileFunctionCall(node1, env, options);
          }
        }
      }
    }
  } else if (typeof exp === 'string') {
    result = compileString(node1, env, options);
  } else if (typeof exp === 'symbol') {
    result = compileVariable(node1, env, options);
  } else if (estreep(exp)) {
    result = exp;
  } else {
    result = compileAtom(node1, env, options);
  }
  if (commentsOption && node1.hasProperty('comments')) {
    let comments: any = node1.getProperty('comments');
    if (comments.length > 0) {
      result.comments = compileComments(comments);
    }
  }
  return result;
}

compileRose.fsource = [Symbol.for('define'), [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('node1'), [Symbol.for('optimize-rose'), Symbol.for('node'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node1'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 0], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-list'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('op')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('op'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]], [Symbol.for('not'), [Symbol.for('macro-type?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), Symbol.for('op'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-function-call'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('op')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\.'], [Symbol.for('symbol->string'), Symbol.for('op')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-dot'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('op-type')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-typed-value'), Symbol.for('op')]], [Symbol.for('cond'), [[Symbol.for('undefined-type?'), Symbol.for('op-type')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-function-call'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('memq?'), Symbol.for('f'), Symbol.for('inlined-functions')], [Symbol.for('define'), Symbol.for('inlined-exp'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('f')], [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('inlined-node'), [Symbol.for('make-rose'), Symbol.for('inlined-exp'), Symbol.for('node')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-rose'), Symbol.for('inlined-node'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define-values'), [Symbol.for('compilation-f'), Symbol.for('compilation-type')], [Symbol.for('send'), Symbol.for('compilation-mapping-environment'), Symbol.for('get-typed-value'), Symbol.for('f')]], [Symbol.for('cond'), [[Symbol.for('compiler-type?'), Symbol.for('compilation-type')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compilation-f'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('macro-type?'), Symbol.for('compilation-type')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('insert-sexp-into-rose'), [Symbol.for('compilation-f'), Symbol.for('exp'), Symbol.for('env')], Symbol.for('node1'), Symbol.for('node1')], Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('macro-type?'), Symbol.for('op-type')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-macro-call'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-function-call'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]]]]]]]]]], [[Symbol.for('string?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-string'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-variable'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('estree?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('exp')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-atom'), Symbol.for('node1'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('comments-option'), [Symbol.for('send'), Symbol.for('node1'), Symbol.for('has-property'), 'comments']], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('send'), Symbol.for('node1'), Symbol.for('get-property'), 'comments']], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('comments')], 0], [Symbol.for('set-field!'), Symbol.for('comments'), Symbol.for('result'), [Symbol.for('compile-comments'), Symbol.for('comments')]]]], Symbol.for('result')];

/**
 * Compile a S-expression.
 */
function compileSexp(exp: any, env: any, options: any = {}): any {
  return compileRose(makeRose(exp), env, options);
}

compileSexp.fsource = [Symbol.for('define'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), [Symbol.for('make-rose'), Symbol.for('exp')], [Symbol.for('compile-rose'), Symbol.for('_'), Symbol.for('env'), Symbol.for('options')]]];

/**
 * Compile `node` as an expression.
 */
function compileExpression(node: any, env: any, options: any = {}): any {
  return compileRose(node, env, makeExpressionOptions(options));
}

compileExpression.fsource = [Symbol.for('define'), [Symbol.for('compile-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]]];

/**
 * Compile `node` as a regular statement.
 */
function compileStatement(node: any, env: any, options: any = {}): any {
  return compileRose(node, env, makeStatementOptions(options));
}

compileStatement.fsource = [Symbol.for('define'), [Symbol.for('compile-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('make-statement-options'), Symbol.for('options')]]];

/**
 * Compile `node` as a return statement.
 */
function compileReturnStatement(node: any, env: any, options: any = {}): any {
  return compileRose(node, env, makeReturnStatementOptions(options));
}

compileReturnStatement.fsource = [Symbol.for('define'), [Symbol.for('compile-return-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-rose'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('make-return-statement-options'), Symbol.for('options')]]];

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

compileStatementOrReturnStatement.fsource = [Symbol.for('define'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType'], 'return'], [Symbol.for('compile-return-statement'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-statement'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];

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

compileStatements.fsource = [Symbol.for('define'), [Symbol.for('compile-statements'), Symbol.for('statements'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('return-idx'), -1], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('statements')], 1], -1, -1]]], [Symbol.for('define'), Symbol.for('statement'), [Symbol.for('aget'), Symbol.for('statements'), Symbol.for('i')]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('statement'), Symbol.for('break_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('statement'), Symbol.for('continue_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('statement'), Symbol.for('yield_'), Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('return-idx'), Symbol.for('i')], [Symbol.for('break')]]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('statements')]]]], [Symbol.for('define'), Symbol.for('statement'), [Symbol.for('aget'), Symbol.for('statements'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('i'), Symbol.for('return-idx')], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('compile-return-statement'), Symbol.for('statement'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('compile-statement'), Symbol.for('statement'), Symbol.for('env'), Symbol.for('options')]]]]], Symbol.for('result')];

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

interpretString.fsource = [Symbol.for('define'), [Symbol.for('interpret-string'), Symbol.for('str'), [Symbol.for('env'), undefined], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('interpret'), [Symbol.for('read-sexp'), Symbol.for('str')], Symbol.for('env'), Symbol.for('options')]];

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

interpretFiles.fsource = [Symbol.for('define'), [Symbol.for('interpret-files'), Symbol.for('files'), [Symbol.for('env'), undefined], [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('file')], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('~>'), Symbol.for('file'), [Symbol.for('readFileSync'), Symbol.for('_'), [Symbol.for('js-obj'), 'encoding', 'utf8']], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^#!.*'], Symbol.for('_'), ''], [Symbol.for('string-append'), '(begin\n', Symbol.for('_'), '\n' +
  ')']]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('interpret-string'), Symbol.for('str'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('result')], Symbol.for('files')]];

/**
 * Interpret a string of Lisp code.
 * Alias for `interpret-string`.
 */
function lisp(str: any, env: any = undefined): any {
  return interpretString(str, env);
}

lisp.fsource = [Symbol.for('define'), [Symbol.for('lisp'), Symbol.for('str'), [Symbol.for('env'), undefined]], [Symbol.for('interpret-string'), Symbol.for('str'), Symbol.for('env')]];

/**
 * Make a Lisp environment.
 */
function makeLisp(variables: any = [], isLisp2: any = false): any {
  return new LispEnvironment(variables, lispEnvironment);
}

makeLisp.fsource = [Symbol.for('define'), [Symbol.for('make-lisp'), [Symbol.for('variables'), [Symbol.for('quote'), []]], [Symbol.for('is-lisp-2'), false]], [Symbol.for('new'), Symbol.for('LispEnvironment'), Symbol.for('variables'), Symbol.for('lisp-environment')]];

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

makeInterpretationEnvironment.fsource = [Symbol.for('define'), [Symbol.for('make-interpretation-environment'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('eval-option'), [Symbol.for('oget'), Symbol.for('options'), 'eval']], [Symbol.for('when'), [Symbol.for('undefined?'), Symbol.for('eval-option')], [Symbol.for('set!'), Symbol.for('eval-option'), true]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('env'), Symbol.for('lang-environment')], [Symbol.for('and'), [Symbol.for('is-a?'), Symbol.for('env'), Symbol.for('EnvironmentStack')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('has-environment'), Symbol.for('lang-environment')]]], Symbol.for('env')], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('EnvironmentStack'), Symbol.for('env'), [Symbol.for('if'), Symbol.for('eval-option'), Symbol.for('interpretation-environment'), Symbol.for('interpretation-environment-no-eval')]]]]];

/**
 * Make an environment suitable for expanding macros
 * and compiler macros.
 */
function makeMacroEnvironment(env: any): any {
  return new EnvironmentStack(new EnvironmentPipe(env, compilationMacroMappingEnv), env);
}

makeMacroEnvironment.fsource = [Symbol.for('define'), [Symbol.for('make-macro-environment'), Symbol.for('env')], [Symbol.for('new'), Symbol.for('EnvironmentStack'), [Symbol.for('new'), Symbol.for('EnvironmentPipe'), Symbol.for('env'), Symbol.for('compilation-macro-mapping-env')], Symbol.for('env')]];

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

makeExpressionOptions.fsource = [Symbol.for('define'), [Symbol.for('make-expression-options'), Symbol.for('options')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'expression']]];

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

makeStatementOptions.fsource = [Symbol.for('define'), [Symbol.for('make-statement-options'), Symbol.for('options')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'statement']]];

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

makeReturnStatementOptions.fsource = [Symbol.for('define'), [Symbol.for('make-return-statement-options'), Symbol.for('options')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'expressionType', 'return']]];

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

makeExpressionOrStatement.fsource = [Symbol.for('define'), [Symbol.for('make-expression-or-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('wrap-expression-in-statement'), Symbol.for('node'), Symbol.for('options')]], [Symbol.for('else'), Symbol.for('node')]]];

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

wrapExpressionInStatement.fsource = [Symbol.for('define'), [Symbol.for('wrap-expression-in-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Expression')]], Symbol.for('node')], [[Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('new'), Symbol.for('ReturnStatement'), Symbol.for('node')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ExpressionStatement'), Symbol.for('node')]]]];

/**
 * Wraps `node` in a `BlockStatement`.
 */
function wrapInBlockStatement(obj: any): any {
  return makeBlockStatement([obj]);
}

wrapInBlockStatement.fsource = [Symbol.for('define'), [Symbol.for('wrap-in-block-statement'), Symbol.for('obj')], [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('obj')]]];

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

wrapInBlockStatementSmart.fsource = [Symbol.for('define'), [Symbol.for('wrap-in-block-statement-smart'), Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('node'), 'BlockStatement'], Symbol.for('node')], [Symbol.for('else'), [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('node')]]]]];

/**
 * Wrap `exp` in a `lambda` call.
 */
function wrapInLambdaCall(exp: any): any {
  return makeRose([[Symbol.for('lambda'), [], exp]]);
}

wrapInLambdaCall.fsource = [Symbol.for('define'), [Symbol.for('wrap-in-lambda-call'), Symbol.for('exp')], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [], [Symbol.for('unquote'), Symbol.for('exp')]]]]]];

/**
 * Wrap `exp` in a `js/arrow` call.
 */
function wrapInArrowCall(exp: any): any {
  return makeRose([[Symbol.for('js/arrow'), [], exp]]);
}

wrapInArrowCall.fsource = [Symbol.for('define'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('js/arrow'), [], [Symbol.for('unquote'), Symbol.for('exp')]]]]]];

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

makeBlockStatement.fsource = [Symbol.for('define'), [Symbol.for('make-block-statement'), Symbol.for('body')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('body')], [Symbol.for('new'), Symbol.for('BlockStatement'), [Symbol.for('make-block-statement-helper'), Symbol.for('body')]]], [Symbol.for('else'), [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('body')]]]]];

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

makeBlockStatementHelper.fsource = [Symbol.for('define'), [Symbol.for('make-block-statement-helper'), Symbol.for('body')], [Symbol.for('define'), Symbol.for('statements'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('statement'), Symbol.for('body')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('statement'), 'Program'], [Symbol.for('define'), Symbol.for('fragment'), Symbol.for('statement')], [Symbol.for('define'), Symbol.for('fragment-statements'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('fragment')]], [Symbol.for('define'), Symbol.for('fragment-comments'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('fragment')]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('fragment-statements')], 0], [Symbol.for('transfer-comments'), Symbol.for('fragment'), [Symbol.for('first'), Symbol.for('fragment-statements')]], [Symbol.for('set!'), Symbol.for('statements'), [Symbol.for('append'), Symbol.for('statements'), Symbol.for('fragment-statements')]]], [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('fragment-comments')], 0], [Symbol.for('push-right!'), Symbol.for('statements'), Symbol.for('statement')]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('statements'), Symbol.for('statement')]]]], Symbol.for('statements')];

/**
 * Make a `Program`.
 */
function makeProgram(body: any): any {
  return new Program(makeBlockStatementHelper(body));
}

makeProgram.fsource = [Symbol.for('define'), [Symbol.for('make-program'), Symbol.for('body')], [Symbol.for('new'), Symbol.for('Program'), [Symbol.for('make-block-statement-helper'), Symbol.for('body')]]];

/**
 * Make a `Program` fragment (i.e., a program that
 * is to be spliced into the containing program).
 */
function makeProgramFragment(body: any = []): any {
  // `Program` is used to represent programs
  // and program fragments.
  return makeProgram(body);
}

makeProgramFragment.fsource = [Symbol.for('define'), [Symbol.for('make-program-fragment'), [Symbol.for('body'), [Symbol.for('quote'), []]]], [Symbol.for('make-program'), Symbol.for('body')]];

/**
 * Make an empty program fragment.
 */
function emptyProgram(): any {
  return makeProgramFragment();
}

emptyProgram.fsource = [Symbol.for('define'), [Symbol.for('empty-program')], [Symbol.for('make-program-fragment')]];

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

unwrapBlockStatement.fsource = [Symbol.for('define'), [Symbol.for('unwrap-block-statement'), Symbol.for('exp')], [Symbol.for('unless'), [Symbol.for('estree-type?'), Symbol.for('exp'), 'BlockStatement'], [Symbol.for('return'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('unwrapped-exp'), Symbol.for('exp')], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('unwrapped-exp')]], 1], [Symbol.for('estree-type?'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('unwrapped-exp')]], 'BlockStatement']], [Symbol.for('set!'), Symbol.for('unwrapped-exp'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('unwrapped-exp')]]]], Symbol.for('unwrapped-exp')];

/**
 * Remove the comment prefix (`; `, `;; `, `;;; `, etc.)
 * from a comment string.
 */
function removeCommentPrefix(comment: any): any {
  return comment.replace(new RegExp('^[^\\S\\r\\n]*[;]+[^\\S\\r\\n]?', 'gm'), '');
}

removeCommentPrefix.fsource = [Symbol.for('define'), [Symbol.for('remove-comment-prefix'), Symbol.for('comment')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^[^\\S\\r\\n]*[;]+[^\\S\\r\\n]?', 'gm'], Symbol.for('comment'), '']];

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

transferAndCompileComments.fsource = [Symbol.for('define'), [Symbol.for('transfer-and-compile-comments'), Symbol.for('node1'), Symbol.for('node2'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node1'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node1'), Symbol.for('get-property'), 'comments'], [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node1')]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('comments-option'), Symbol.for('comments')], [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node2'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node2'), Symbol.for('set-property'), 'comments', [Symbol.for('append'), Symbol.for('comments'), [Symbol.for('or'), [Symbol.for('send'), Symbol.for('node2'), Symbol.for('get-property'), 'comments'], [Symbol.for('quote'), []]]]], [Symbol.for('set!'), Symbol.for('comments'), [Symbol.for('compile-comments'), Symbol.for('comments')]], [Symbol.for('set-field!'), Symbol.for('comments'), Symbol.for('node2'), [Symbol.for('append'), Symbol.for('comments'), [Symbol.for('or'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node2')], [Symbol.for('quote'), []]]]]]], Symbol.for('node2')];

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

compileComments.fsource = [Symbol.for('define'), [Symbol.for('compile-comments'), Symbol.for('comments')], [Symbol.for('define'), Symbol.for('comments-compiled'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('comment'), Symbol.for('comments')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('LeadingCommentToken')], [Symbol.for('define'), Symbol.for('subcomments'), [Symbol.for('split-comments'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('comment')]]], [Symbol.for('for'), [[Symbol.for('subcomment'), Symbol.for('subcomments')]], [Symbol.for('cond'), [[Symbol.for('>='), [Symbol.for('get-comment-level'), Symbol.for('subcomment')], 3], [Symbol.for('push-right!'), Symbol.for('comments-compiled'), [Symbol.for('new'), Symbol.for('BlockComment'), [Symbol.for('remove-comment-prefix'), Symbol.for('subcomment')]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('comments-compiled'), [Symbol.for('new'), Symbol.for('LeadingComment'), [Symbol.for('remove-comment-prefix'), Symbol.for('subcomment')]]]]]]], [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('TrailingCommentToken')], [Symbol.for('push-right!'), Symbol.for('comments-compiled'), [Symbol.for('new'), Symbol.for('TrailingComment'), [Symbol.for('remove-comment-prefix'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('comment')]]]]]]], Symbol.for('comments-compiled')];

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

splitComments.fsource = [Symbol.for('define'), [Symbol.for('split-comments'), Symbol.for('str')], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('comment'), ''], [Symbol.for('define'), Symbol.for('current-level'), -1], [Symbol.for('define'), Symbol.for('lines'), [Symbol.for('string-split'), Symbol.for('str'), '\n']], [Symbol.for('when'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], Symbol.for('str')], [Symbol.for('set!'), Symbol.for('lines'), [Symbol.for('drop-right'), Symbol.for('lines'), 1]]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('lines')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('x'), ''], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], Symbol.for('comment')], [Symbol.for('set!'), Symbol.for('comment'), [Symbol.for('string-append'), Symbol.for('comment'), '\n']], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('comment')], [Symbol.for('set!'), Symbol.for('comment'), '']], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('comment'), [Symbol.for('string-append'), Symbol.for('comment'), '\n']]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('level'), [Symbol.for('get-comment-level'), Symbol.for('x')]], [Symbol.for('unless'), [Symbol.for('='), Symbol.for('level'), Symbol.for('current-level')], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('comment'), ''], [Symbol.for('eq?'), Symbol.for('comment'), '\n']], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('comment')], [Symbol.for('set!'), Symbol.for('comment'), '']], [Symbol.for('set!'), Symbol.for('current-level'), Symbol.for('level')]], [Symbol.for('set!'), Symbol.for('comment'), [Symbol.for('string-append'), Symbol.for('comment'), Symbol.for('x'), '\n']]]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('comment'), ''], [Symbol.for('eq?'), Symbol.for('comment'), '\n']], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('comment')]], Symbol.for('comments')];

/**
 * Whether `exp` is a function call, given `env`.
 */
function functionCallP(exp: any, env: any): any {
  if (exp instanceof Rose) {
    return macroCallP(exp.getValue(), env);
  } else {
    return Array.isArray(exp) && (exp.length > 1) && (typeof exp[0] === 'symbol') && procedureTypeP(env.getType(exp[0]));
  }
}

functionCallP.fsource = [Symbol.for('define'), [Symbol.for('function-call?'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('macro-call?'), [Symbol.for('send'), Symbol.for('exp'), Symbol.for('get-value')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('symbol?'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('procedure-type?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), [Symbol.for('first'), Symbol.for('exp')]]]]]]];

/**
 * Whether `exp` is a macro call, given `env`.
 */
function macroCallP(exp: any, env: any): any {
  if (exp instanceof Rose) {
    return macroCallP(exp.getValue(), env);
  } else {
    return Array.isArray(exp) && (exp.length > 1) && (typeof exp[0] === 'symbol') && macroTypeP(env.getType(exp[0]));
  }
}

macroCallP.fsource = [Symbol.for('define'), [Symbol.for('macro-call?'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('macro-call?'), [Symbol.for('send'), Symbol.for('exp'), Symbol.for('get-value')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('symbol?'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('macro-type?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), [Symbol.for('first'), Symbol.for('exp')]]]]]]];

/**
 * Whether `exp` is a special form, given `env`.
 */
function specialFormP(exp: any, env: any): any {
  if (exp instanceof Rose) {
    return macroCallP(exp.getValue(), env);
  } else {
    return Array.isArray(exp) && (exp.length > 1) && (typeof exp[0] === 'symbol') && specialTypeP(env.getType(exp[0]));
  }
}

specialFormP.fsource = [Symbol.for('define'), [Symbol.for('special-form?'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')], [Symbol.for('macro-call?'), [Symbol.for('send'), Symbol.for('exp'), Symbol.for('get-value')], Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('symbol?'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('special-type?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), [Symbol.for('first'), Symbol.for('exp')]]]]]]];

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

defineToLambda.fsource = [Symbol.for('define'), [Symbol.for('define->lambda'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('curried-option'), [Symbol.for('oget'), Symbol.for('options'), 'curried']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('name-and-params'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-params')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('cdr'), Symbol.for('name-and-params')]], [Symbol.for('define'), Symbol.for('should-curry'), [Symbol.for('or'), Symbol.for('curried-option'), [Symbol.for('and'), [Symbol.for('undefined?'), Symbol.for('curried-option')], [Symbol.for('array?'), Symbol.for('name')]]]], [Symbol.for('when'), Symbol.for('should-curry'), [Symbol.for('set!'), Symbol.for('params'), [Symbol.for('rest'), [Symbol.for('flatten'), Symbol.for('name-and-params')]]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('dotted-list?'), Symbol.for('name-and-params')], [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('params')], 1]], [Symbol.for('set!'), Symbol.for('params'), [Symbol.for('first'), Symbol.for('params')]]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('unquote'), Symbol.for('params')], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]]]]];

/**
 * Convert a function to a macro on the basis
 * of its `(define ...)` form.
 */
function definitionToMacro(exp: any, args: any): any {
  // FIXME: When a complex argument is referenced inside of a `lambda`
  // expression, we should store the value in a local variable.
  let params: any = cdr(exp[1]);
  let [regularParams, restParam]: any[] = parseParamsList(params);
  if (restParam) {
    params = [...regularParams, restParam];
  }
  const paramsList: any = params.map(function (x: any): any {
    if (Array.isArray(x)) {
      return x[0];
    } else {
      return x;
    }
  });
  let regularArgs: any = [];
  let restArg: any = [Symbol.for('list')];
  const _end: any = args.length;
  for (let i: any = 0; i < _end; i++) {
    const arg: any = (args as any)[i];
    if (i < regularParams.length) {
      regularArgs.push(arg);
    } else if (restParam) {
      restArg.push(arg);
    }
  }
  let argsList: any = [...regularArgs, ...((restParam && true) ? [restArg] : [])];
  const body: any = exp.slice(2);
  if (paramsList.length === 0) {
    if (body.length === 1) {
      return body[0];
    } else {
      return [Symbol.for('begin'), ...body];
    }
  } else {
    const counts: any = buildList(argsList.length, function (...args: any[]): any {
      return 0;
    });
    const shouldMakeLambda: any = false;
    let shouldMakeLet: any = false;
    let result: any = body.map(function (x: any): any {
      return mapTree(function (y: any): any {
        const idx: any = paramsList.findIndex(function (z: any): any {
          return z === y;
        });
        if (idx >= 0) {
          (counts as any)[idx] = (counts as any)[idx] + 1;
          if (idx < argsList.length) {
            return (argsList as any)[idx];
          } else {
            const currentParam: any = (params as any)[idx];
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
    const _end: any = argsList.length;
    for (let i: any = 0; i < _end; i++) {
      const count: any = (counts as any)[i];
      const arg: any = (argsList as any)[i];
      if ((count > 1) && !((typeof arg === 'symbol') || (typeof arg === 'boolean') || (typeof arg === 'string') || Number.isFinite(arg))) {
        shouldMakeLet = true;
        break;
      }
    }
    if (shouldMakeLet) {
      const letBindingsEnv: any = [];
      let gensymMap: any = new Map();
      const _end: any = paramsList.length;
      for (let i: any = 0; i < _end; i++) {
        const argExp: any = (i < argsList.length) ? (argsList as any)[i] : ((): any => {
          const currentParam: any = (params as any)[i];
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
        const paramExp: any = (paramsList as any)[i];
        let param: any = Array.isArray(paramExp) ? paramExp[0] : paramExp;
        if (typeof argExp === 'symbol') {
          gensymMap.set(param, argExp);
        } else {
          const paramGensym: any = Symbol(param.description as string);
          gensymMap.set(param, paramGensym);
          letBindingsEnv.push([paramGensym, argExp]);
        }
      }
      const letBody: any = mapTree(function (x: any): any {
        if (gensymMap.has(x)) {
          return gensymMap.get(x);
        } else {
          return x;
        }
      }, body);
      return [Symbol.for('let*'), letBindingsEnv, ...letBody];
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

definitionToMacro.fsource = [Symbol.for('define'), [Symbol.for('definition->macro'), Symbol.for('exp'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('cdr'), [Symbol.for('array-list-second'), Symbol.for('exp')]]], [Symbol.for('define-values'), [Symbol.for('regular-params'), Symbol.for('rest-param')], [Symbol.for('parse-params-list'), Symbol.for('params')]], [Symbol.for('when'), Symbol.for('rest-param'), [Symbol.for('set!'), Symbol.for('params'), [Symbol.for('append'), Symbol.for('regular-params'), [Symbol.for('list'), Symbol.for('rest-param')]]]], [Symbol.for('define'), Symbol.for('params-list'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('array-first'), Symbol.for('x')], Symbol.for('x')]], Symbol.for('params')]], [Symbol.for('define'), Symbol.for('regular-args'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-arg'), [Symbol.for('quote'), [Symbol.for('list')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), [Symbol.for('array-list-length'), Symbol.for('regular-params')]], [Symbol.for('push-right!'), Symbol.for('regular-args'), Symbol.for('arg')]], [Symbol.for('rest-param'), [Symbol.for('push-right!'), Symbol.for('rest-arg'), Symbol.for('arg')]]]], [Symbol.for('define'), Symbol.for('args-list'), [Symbol.for('append'), Symbol.for('regular-args'), [Symbol.for('if'), [Symbol.for('and'), Symbol.for('rest-param'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('rest-arg'), 1]]], [Symbol.for('list'), Symbol.for('rest-arg')], [Symbol.for('quote'), []]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('params-list')], 0], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('body')], 1], [Symbol.for('first'), Symbol.for('body')]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('counts'), [Symbol.for('build-list'), [Symbol.for('array-list-length'), Symbol.for('args-list')], [Symbol.for('const'), 0]]], [Symbol.for('define'), Symbol.for('should-make-lambda'), false], [Symbol.for('define'), Symbol.for('should-make-let'), false], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('y')], [Symbol.for('define'), Symbol.for('idx'), [Symbol.for('js/find-index'), [Symbol.for('lambda'), [Symbol.for('z')], [Symbol.for('eq?'), Symbol.for('z'), Symbol.for('y')]], Symbol.for('params-list')]], [Symbol.for('cond'), [[Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('list-set!'), Symbol.for('counts'), Symbol.for('idx'), [Symbol.for('+'), [Symbol.for('aget'), Symbol.for('counts'), Symbol.for('idx')], 1]], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('idx'), [Symbol.for('array-list-length'), Symbol.for('args-list')]], [Symbol.for('aget'), Symbol.for('args-list'), Symbol.for('idx')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('current-param'), [Symbol.for('aget'), Symbol.for('params'), Symbol.for('idx')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('current-param')], [Symbol.for('second'), Symbol.for('current-param')]], [Symbol.for('else'), undefined]]]]], [Symbol.for('else'), Symbol.for('y')]]], Symbol.for('x')]], Symbol.for('body')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args-list')]]]], [Symbol.for('define'), Symbol.for('count'), [Symbol.for('aget'), Symbol.for('counts'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args-list'), Symbol.for('i')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('>'), Symbol.for('count'), 1], [Symbol.for('not'), [Symbol.for('or'), [Symbol.for('symbol?'), Symbol.for('arg')], [Symbol.for('boolean?'), Symbol.for('arg')], [Symbol.for('string?'), Symbol.for('arg')], [Symbol.for('number?'), Symbol.for('arg')]]]], [Symbol.for('set!'), Symbol.for('should-make-let'), true], [Symbol.for('break')]]], [Symbol.for('cond'), [Symbol.for('should-make-let'), [Symbol.for('define'), Symbol.for('let-bindings-env'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('gensym-map'), [Symbol.for('make-hash')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('params-list')]]]], [Symbol.for('define'), Symbol.for('arg-exp'), [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), [Symbol.for('array-list-length'), Symbol.for('args-list')]], [Symbol.for('aget'), Symbol.for('args-list'), Symbol.for('i')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('current-param'), [Symbol.for('aget'), Symbol.for('params'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('current-param')], [Symbol.for('second'), Symbol.for('current-param')]], [Symbol.for('else'), undefined]]]]], [Symbol.for('define'), Symbol.for('param-exp'), [Symbol.for('aget'), Symbol.for('params-list'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('param'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('param-exp')], [Symbol.for('first'), Symbol.for('param-exp')], Symbol.for('param-exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('arg-exp')], [Symbol.for('hash-set!'), Symbol.for('gensym-map'), Symbol.for('param'), Symbol.for('arg-exp')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('param-gensym'), [Symbol.for('gensym'), [Symbol.for('symbol->string'), Symbol.for('param')]]], [Symbol.for('hash-set!'), Symbol.for('gensym-map'), Symbol.for('param'), Symbol.for('param-gensym')], [Symbol.for('push-right!'), Symbol.for('let-bindings-env'), [Symbol.for('list'), Symbol.for('param-gensym'), Symbol.for('arg-exp')]]]]], [Symbol.for('define'), Symbol.for('let-body'), [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('gensym-map'), Symbol.for('x')], [Symbol.for('hash-ref'), Symbol.for('gensym-map'), Symbol.for('x')]], [Symbol.for('else'), Symbol.for('x')]]], Symbol.for('body')]], [Symbol.for('quasiquote'), [Symbol.for('let*'), [Symbol.for('unquote'), Symbol.for('let-bindings-env')], [Symbol.for('unquote-splicing'), Symbol.for('let-body')]]]], [Symbol.for('should-make-lambda'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('unquote'), Symbol.for('params')], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('unquote-splicing'), Symbol.for('args')]]]], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('result')], 1], [Symbol.for('first'), Symbol.for('result')]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('result')]]]]]]]]]];

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

defineMacroToLambdaForm.fsource = [Symbol.for('define'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('cdr'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('exp-arg'), [Symbol.for('quote'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('env-arg'), [Symbol.for('quote'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('macro-args'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-arg'), undefined], [Symbol.for('cond'), [[Symbol.for('list?'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('while'), [Symbol.for('<'), Symbol.for('i'), [Symbol.for('array-list-length'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('arg'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&rest')]], [Symbol.for('set!'), Symbol.for('rest-arg'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&whole')]], [Symbol.for('set!'), Symbol.for('exp-arg'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [[Symbol.for('eq?'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('&environment')]], [Symbol.for('set!'), Symbol.for('env-arg'), [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 2]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('macro-args'), Symbol.for('arg')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('args')]]], [Symbol.for('when'), Symbol.for('rest-arg'), [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('set!'), Symbol.for('macro-args'), Symbol.for('rest-arg')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('macro-args'), [Symbol.for('apply'), Symbol.for('list*'), [Symbol.for('append'), Symbol.for('macro-args'), [Symbol.for('list'), Symbol.for('rest-arg')]]]]]]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [[Symbol.for('unquote'), Symbol.for('exp-arg')], [Symbol.for('unquote'), Symbol.for('env-arg')]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('null?'), Symbol.for('macro-args')], [Symbol.for('quote'), []], [Symbol.for('quasiquote'), [[Symbol.for('define-values'), [Symbol.for('unquote'), Symbol.for('macro-args')], [Symbol.for('rest'), [Symbol.for('unquote'), Symbol.for('exp-arg')]]]]]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

/**
 * Create a macro function on the basis of a
 * `(define-macro ...)` expression.
 */
function defineMacroToFunction(exp: any, env: any): any {
  const macroFn: any = defineMacroToLambdaForm(exp);
  return eval_(macroFn, env);
}

defineMacroToFunction.fsource = [Symbol.for('define'), [Symbol.for('define-macro->function'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('macro-fn'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')]], [Symbol.for('eval_'), Symbol.for('macro-fn'), Symbol.for('env')]];

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

defineToDefineClass.fsource = [Symbol.for('define'), [Symbol.for('define->define-class'), Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('define'), Symbol.for('superclass'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('superclass-exp'), [Symbol.for('send'), Symbol.for('superclass'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('superclass-list'), [Symbol.for('if'), [Symbol.for('memq?'), Symbol.for('superclass-exp'), [Symbol.for('quote'), [Symbol.for('object%'), Symbol.for('object'), Symbol.for('Object')]]], [Symbol.for('quote'), []], [Symbol.for('list'), Symbol.for('superclass')]]], [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-class'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('make-rose'), Symbol.for('superclass-list')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('drop'), 2]]]]]]], [Symbol.for('else'), [Symbol.for('send'), [Symbol.for('define->define-class'), [Symbol.for('make-rose'), Symbol.for('node')]], Symbol.for('get-value')]]]];

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

compileMapMacroHelper.fsource = [Symbol.for('define'), [Symbol.for('compile-map-macro-helper'), Symbol.for('f-exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('f-exp')], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('x')], [[Symbol.for('unquote'), Symbol.for('f-exp')], Symbol.for('x')]]]], [[Symbol.for('and'), [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('f-exp'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('f-exp'), Symbol.for('js-function_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('f-exp'), Symbol.for('js-arrow_'), Symbol.for('env')]], [Symbol.for('array?'), [Symbol.for('second'), Symbol.for('f-exp')]], [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('second'), Symbol.for('f-exp')]], 1]], Symbol.for('f-exp')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('A-exp'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]]]]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('A-exp')], [Symbol.for('unquote'), Symbol.for('f-exp')]]]]]];

/**
 * Compile an `(and ...)` expression.
 */
function compileAnd(node: any, env: any, options: any = {}): any {
  return compileLogicalExpression(node, env, options, {
    identity: true,
    operator: '&&'
  });
}

compileAnd.fsource = [Symbol.for('define'), [Symbol.for('compile-and'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-logical-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', true, 'operator', '&&']]];

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

compileAnn.fsource = [Symbol.for('define'), [Symbol.for('compile-ann'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('e_'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('define'), Symbol.for('t_'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('TSAsExpression'), [Symbol.for('compile-expression'), Symbol.for('e_'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-type'), Symbol.for('t_'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-rose'), Symbol.for('e_'), Symbol.for('env'), Symbol.for('options')]]]];

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

compileDefineType.fsource = [Symbol.for('define'), [Symbol.for('compile-define-type'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('compile-type'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('transfer-and-compile-comments'), Symbol.for('node'), [Symbol.for('new'), Symbol.for('TSTypeAliasDeclaration'), Symbol.for('id'), Symbol.for('type_')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('empty-program')]]]];

/**
 * Compile a type expression.
 */
function compileType(node: any, env: any, options: any = {}): any {
  let exp: any = (node instanceof Rose) ? node.getValue() : node;
  return compileTypeExp(exp, env, options);
}

compileType.fsource = [Symbol.for('define'), [Symbol.for('compile-type'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')], Symbol.for('node')]], [Symbol.for('compile-type-exp'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]];

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
    let params: any = exp.slice(1);
    const returnValue: any = params[params.length - 1];
    params = params.slice(0, -1);
    let plist: any = [];
    const _end: any = params.length;
    for (let i: any = 0; i < _end; i++) {
      if (keywordp((params as any)[i])) {
        plist = (i === 0) ? params : params.slice(i);
        params = ((): any => {
          const n: any = params.length - i;
          if (n === 0) {
            return params;
          } else {
            return params.slice(0, -n);
          }
        })();
        break;
      }
    }
    let restParam: any = undefined;
    if (params[params.length - 1] === Symbol.for('*')) {
      params.pop();
      restParam = params.pop();
    } else {
      restParam = plistGet_(plist, Symbol.for(':rest'));
    }
    const mandatoryParams: any = (taggedListP(exp, Symbol.for('->*')) && (params.length >= 1)) ? params[0] : params;
    const optionalParams: any = (taggedListP(exp, Symbol.for('->*')) && (params.length >= 2)) ? params[1] : [];
    let pos: any = 0;
    function compileParam(param: any, options: any = {
      optional: false,
      rest: false
    }): any {
      const {optional, rest} = options;
      const varName: any = numberToLetter(pos);
      pos++;
      let identifier: any = new Identifier(varName, optional);
      if (rest) {
        identifier = new RestElement(identifier);
      }
      let type_: any = compileTypeExp(param, env, options);
      return identifier.setType(type_);
    }
    compileParam.fsource = [Symbol.for('define'), [Symbol.for('compile-param'), Symbol.for('param'), [Symbol.for('options'), [Symbol.for('js-obj'), 'optional', false, 'rest', false]]], [Symbol.for('define-fields'), [Symbol.for('optional'), Symbol.for('rest')], Symbol.for('options')], [Symbol.for('define'), Symbol.for('var-name'), [Symbol.for('number->letter'), Symbol.for('pos')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]], [Symbol.for('define'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('var-name'), Symbol.for('optional')]], [Symbol.for('when'), Symbol.for('rest'), [Symbol.for('set!'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('RestElement'), Symbol.for('identifier')]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('compile-type-exp'), Symbol.for('param'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('identifier'), Symbol.for('set-type'), Symbol.for('type_')]];
    const mandatoryParamsCompiled: any = mandatoryParams.map(function (param: any): any {
      return compileParam(param);
    });
    const optionalParamsCompiled: any = optionalParams.map(function (param: any): any {
      return compileParam(param, {
        optional: true
      });
    });
    const restParamsCompiled: any = restParam ? [compileParam(restParam, {
      rest: true
    })] : [];
    const returnValueCompiled: any = compileTypeExp(returnValue, env, options);
    return new TSFunctionType([...mandatoryParamsCompiled, ...optionalParamsCompiled, ...restParamsCompiled], returnValueCompiled);
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

compileTypeExp.fsource = [Symbol.for('define'), [Symbol.for('compile-type-exp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('new'), Symbol.for('TSAnyKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Void')]], [Symbol.for('new'), Symbol.for('TSVoidKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Undefined')]], [Symbol.for('new'), Symbol.for('TSUndefinedKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Boolean')]], [Symbol.for('new'), Symbol.for('TSBooleanKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('True')]], [Symbol.for('new'), Symbol.for('TSLiteralType'), [Symbol.for('new'), Symbol.for('Literal'), true]]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('False')]], [Symbol.for('new'), Symbol.for('TSLiteralType'), [Symbol.for('new'), Symbol.for('Literal'), false]]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Number')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Integer')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Natural')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Real')]], [Symbol.for('new'), Symbol.for('TSNumberKeyword')]], [[Symbol.for('eq?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('String')]], [Symbol.for('new'), Symbol.for('TSStringKeyword')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('TSTypeReference'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('symbol->string'), Symbol.for('exp')]]]]]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('List')]], [Symbol.for('new'), Symbol.for('TSTupleType'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-type-exp'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('rest'), Symbol.for('exp')]]]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Listof')]], [Symbol.for('new'), Symbol.for('TSArrayType'), [Symbol.for('compile-type-exp'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('Pairof')]], [Symbol.for('compile-type-exp'), [Symbol.for('quasiquote'), [Symbol.for('Listof'), [Symbol.for('U'), [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('exp')]], Symbol.for('Symbol')]]], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('U')]], [Symbol.for('new'), Symbol.for('TSUnionType'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-type-exp'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('rest'), Symbol.for('exp')]]]], [[Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->*')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('drop'), Symbol.for('exp'), 1]], [Symbol.for('define'), Symbol.for('return-value'), [Symbol.for('array-list-last'), Symbol.for('params')]], [Symbol.for('set!'), Symbol.for('params'), [Symbol.for('drop-right'), Symbol.for('params'), 1]], [Symbol.for('define'), Symbol.for('plist'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('params')]]]], [Symbol.for('when'), [Symbol.for('keyword?'), [Symbol.for('aget'), Symbol.for('params'), Symbol.for('i')]], [Symbol.for('set!'), Symbol.for('plist'), [Symbol.for('drop'), Symbol.for('params'), Symbol.for('i')]], [Symbol.for('set!'), Symbol.for('params'), [Symbol.for('drop-right'), Symbol.for('params'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('params')], Symbol.for('i')]]], [Symbol.for('break')]]], [Symbol.for('define'), Symbol.for('rest-param'), undefined], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('array-list-last'), Symbol.for('params')], [Symbol.for('quote'), Symbol.for('*')]], [Symbol.for('pop-right!'), Symbol.for('params')], [Symbol.for('set!'), Symbol.for('rest-param'), [Symbol.for('pop-right!'), Symbol.for('params')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('rest-param'), [Symbol.for('plist-get_'), Symbol.for('plist'), [Symbol.for('quote'), Symbol.for(':rest')]]]]], [Symbol.for('define'), Symbol.for('mandatory-params'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->*')]], [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('params')], 1]], [Symbol.for('array-list-first'), Symbol.for('params')], Symbol.for('params')]], [Symbol.for('define'), Symbol.for('optional-params'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('->*')]], [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('params')], 2]], [Symbol.for('array-list-second'), Symbol.for('params')], [Symbol.for('quote'), []]]], [Symbol.for('define'), Symbol.for('pos'), 0], [Symbol.for('define'), [Symbol.for('compile-param'), Symbol.for('param'), [Symbol.for('options'), [Symbol.for('js-obj'), 'optional', false, 'rest', false]]], [Symbol.for('define-fields'), [Symbol.for('optional'), Symbol.for('rest')], Symbol.for('options')], [Symbol.for('define'), Symbol.for('var-name'), [Symbol.for('number->letter'), Symbol.for('pos')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]], [Symbol.for('define'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('var-name'), Symbol.for('optional')]], [Symbol.for('when'), Symbol.for('rest'), [Symbol.for('set!'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('RestElement'), Symbol.for('identifier')]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('compile-type-exp'), Symbol.for('param'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('identifier'), Symbol.for('set-type'), Symbol.for('type_')]], [Symbol.for('define'), Symbol.for('mandatory-params-compiled'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('param')], [Symbol.for('compile-param'), Symbol.for('param')]], Symbol.for('mandatory-params')]], [Symbol.for('define'), Symbol.for('optional-params-compiled'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('param')], [Symbol.for('compile-param'), Symbol.for('param'), [Symbol.for('js-obj'), 'optional', true]]], Symbol.for('optional-params')]], [Symbol.for('define'), Symbol.for('rest-params-compiled'), [Symbol.for('if'), Symbol.for('rest-param'), [Symbol.for('list'), [Symbol.for('compile-param'), Symbol.for('rest-param'), [Symbol.for('js-obj'), 'rest', true]]], [Symbol.for('quote'), []]]], [Symbol.for('define'), Symbol.for('return-value-compiled'), [Symbol.for('compile-type-exp'), Symbol.for('return-value'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('TSFunctionType'), [Symbol.for('append'), Symbol.for('mandatory-params-compiled'), Symbol.for('optional-params-compiled'), Symbol.for('rest-params-compiled')], Symbol.for('return-value-compiled')]], [[Symbol.for('and'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('exp')], 0]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('symbol->string'), [Symbol.for('first'), Symbol.for('exp')]]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('map'), Symbol.for('symbol->string'), [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('params')], 0], [Symbol.for('new'), Symbol.for('TSTypeReference'), Symbol.for('name'), [Symbol.for('new'), Symbol.for('TSTypeParameterInstantiation'), Symbol.for('params')]]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('TSTypeReference'), Symbol.for('name')]]]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]];

/**
 * Convert a number to a letter.
 * `0` corresponds to `a`, `1` to `b`, etc.
 */
function numberToLetter(n: any): any {
  return String.fromCharCode('a'.charCodeAt(0) + n);
}

numberToLetter.fsource = [Symbol.for('define'), [Symbol.for('number->letter'), Symbol.for('n')], [Symbol.for('~>'), [Symbol.for('send'), 'a', Symbol.for('charCodeAt'), 0], [Symbol.for('+'), Symbol.for('_'), Symbol.for('n')], [Symbol.for('send'), Symbol.for('String'), Symbol.for('fromCharCode'), Symbol.for('_')]]];

/**
 * "NO-OP" operation.
 */
function nop_(exp: any, env: any): any {
  return undefined;
}

nop_.fsource = [Symbol.for('define'), [Symbol.for('nop_'), Symbol.for('exp'), Symbol.for('env')], undefined];

/**
 * Compile a `(+ ...)` expression.
 */
function compileAdd(node: any, env: any, options: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    identity: 0,
    operator: '+'
  });
}

compileAdd.fsource = [Symbol.for('define'), [Symbol.for('compile-add'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 0, 'operator', '+']]];

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
  const isNew: any = env.get(f) === new_;
  const callee: any = isNew ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
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
  const args: any = isNew ? exp.slice(3) : exp.slice(2);
  const calleeCompiled: any = compileExpression(makeRose(callee), env, options);
  const argsCompiled: any = [];
  if (args.length > 0) {
    let regularArgs: any = args.slice(0, -1);
    for (let arg of regularArgs) {
      argsCompiled.push(compileExpression(makeRose(arg), env, options));
    }
    let restArg: any = args[args.length - 1];
    const restArgCompiled: any = compileExpression(makeRose(restArg), env, options);
    const spreadElement: any = new SpreadElement(restArgCompiled);
    // Simplify the expression if the rest argument
    // is nothing more than a simple list.
    if (estreeTypeP(restArgCompiled, 'ArrayExpression')) {
      const elements: any = restArgCompiled.elements;
      let isSimpleList: any = true;
      for (let x of elements) {
        if (estreeTypeP(x, 'SpreadElement')) {
          isSimpleList = false;
          break;
        }
      }
      if (isSimpleList) {
        for (let x of elements) {
          argsCompiled.push(x);
        }
      } else {
        argsCompiled.push(spreadElement);
      }
    } else {
      argsCompiled.push(spreadElement);
    }
  }
  if (isNew) {
    return makeExpressionOrStatement(new NewExpression(calleeCompiled, argsCompiled), options);
  } else {
    return makeExpressionOrStatement(new CallExpression(calleeCompiled, argsCompiled), options);
  }
}

compileApply.fsource = [Symbol.for('define'), [Symbol.for('compile-apply'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('is-new'), [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('f')], Symbol.for('new_')]], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('if'), Symbol.for('is-new'), [Symbol.for('third'), Symbol.for('exp')], Symbol.for('f')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('if'), Symbol.for('is-new'), [Symbol.for('drop'), Symbol.for('exp'), 3], [Symbol.for('drop'), Symbol.for('exp'), 2]]], [Symbol.for('define'), Symbol.for('callee-compiled'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('callee')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('args-compiled'), [Symbol.for('quote'), []]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('args')], 0], [Symbol.for('define'), Symbol.for('regular-args'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('regular-args')]], [Symbol.for('push-right!'), Symbol.for('args-compiled'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('arg')], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('rest-arg'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('rest-arg-compiled'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('rest-arg')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('spread-element'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('rest-arg-compiled')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('rest-arg-compiled'), 'ArrayExpression'], [Symbol.for('define'), Symbol.for('elements'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('rest-arg-compiled')]], [Symbol.for('define'), Symbol.for('is-simple-list'), true], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('elements')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('x'), 'SpreadElement'], [Symbol.for('set!'), Symbol.for('is-simple-list'), false], [Symbol.for('break')]]], [Symbol.for('cond'), [Symbol.for('is-simple-list'), [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('elements')]], [Symbol.for('push-right!'), Symbol.for('args-compiled'), Symbol.for('x')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('args-compiled'), Symbol.for('spread-element')]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('args-compiled'), Symbol.for('spread-element')]]]], [Symbol.for('cond'), [Symbol.for('is-new'), [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('NewExpression'), Symbol.for('callee-compiled'), Symbol.for('args-compiled')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), Symbol.for('callee-compiled'), Symbol.for('args-compiled')], Symbol.for('options')]]]];

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

compileArrayRef.fsource = [Symbol.for('define'), [Symbol.for('compile-array-ref'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('variable'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('variable-compiled'), [Symbol.for('compile-expression'), Symbol.for('variable'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('indices-compiled'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('new'), Symbol.for('MemberExpression'), Symbol.for('acc'), Symbol.for('x'), true]], Symbol.for('variable-compiled'), Symbol.for('indices-compiled')]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];

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

compileArraySet.fsource = [Symbol.for('define'), [Symbol.for('compile-array-set'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('arr'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), [Symbol.for('drop'), Symbol.for('exp'), 2], 1]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('exp')], 1]]], [Symbol.for('compile-rose'), [Symbol.for('insert-sexp-into-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('array-ref'), [Symbol.for('unquote'), Symbol.for('arr')], [Symbol.for('unquote-splicing'), Symbol.for('indices')]], [Symbol.for('unquote'), Symbol.for('value')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a `(js/get ...)` expression.
 */
function compileJsGet(node: any, env: any, options: any = {}): any {
  return compileArrayRef(node, env, options);
}

compileJsGet.fsource = [Symbol.for('define'), [Symbol.for('compile-js-get'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-array-ref'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile an `(object-ref ...)` expression.
 */
function compileObjectRef(node: any, env: any, options: any = {}): any {
  return compileArrayRef(node, env, options);
}

compileObjectRef.fsource = [Symbol.for('define'), [Symbol.for('compile-object-ref'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-array-ref'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile an `(object-set! ...)` expression.
 */
function compileObjectSet(node: any, env: any, options: any = {}): any {
  return compileArraySet(node, env, options);
}

compileObjectSet.fsource = [Symbol.for('define'), [Symbol.for('compile-object-set'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-array-set'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile an atomic expression, such as `foo`.
 */
function compileAtom(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new Literal(node.getValue()), options);
}

compileAtom.fsource = [Symbol.for('define'), [Symbol.for('compile-atom'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], Symbol.for('options')]];

/**
 * Compile a `(: ...)` expression.
 */
function compileColon(node: any, env: any, options: any = {}): any {
  const sym: any = node.get(1);
  const symExp: any = sym.getValue();
  let type_: any = node.get(2);
  const typeExp: any = type_.getValue();
  env.setLocalType(symExp, typeExp);
  return compileNop(node, env, options);
}

compileColon.fsource = [Symbol.for('define'), [Symbol.for('compile-colon'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('sym-exp'), [Symbol.for('send'), Symbol.for('sym'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('type-exp'), [Symbol.for('send'), Symbol.for('type_'), Symbol.for('get-value')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local-type'), Symbol.for('sym-exp'), Symbol.for('type-exp')], [Symbol.for('compile-nop'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

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
    reducingF.fsource = [Symbol.for('define'), [Symbol.for('reducing-f'), Symbol.for('cond-clause'), Symbol.for('compiled-exp')], [Symbol.for('define'), Symbol.for('condition'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('then-clauses'), [Symbol.for('begin-wrap-rose-smart'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('drop'), 1]]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('new'), Symbol.for('IfStatement'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('unwrap-block-statement'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')]]], Symbol.for('options')], Symbol.for('compiled-exp')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ConditionalExpression'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('compile-expression'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')], Symbol.for('options')], Symbol.for('compiled-exp')]]]];
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

compileCond.fsource = [Symbol.for('define'), [Symbol.for('compile-cond'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('cond-clauses'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('cond-clauses')], 0], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), false], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), [Symbol.for('reducing-f'), Symbol.for('cond-clause'), Symbol.for('compiled-exp')], [Symbol.for('define'), Symbol.for('condition'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('then-clauses'), [Symbol.for('begin-wrap-rose-smart'), [Symbol.for('send'), Symbol.for('cond-clause'), Symbol.for('drop'), 1]]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('new'), Symbol.for('IfStatement'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('unwrap-block-statement'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')]]], Symbol.for('options')], Symbol.for('compiled-exp')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ConditionalExpression'), [Symbol.for('compile-expression'), Symbol.for('condition'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('transfer-and-compile-comments'), Symbol.for('cond-clause'), [Symbol.for('compile-expression'), Symbol.for('then-clauses'), Symbol.for('env'), Symbol.for('options')], Symbol.for('options')], Symbol.for('compiled-exp')]]]], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('~>'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]], [Symbol.for('quote'), Symbol.for('else')]], [Symbol.for('define'), Symbol.for('final-clause'), [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('transfer-and-compile-comments'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('begin-wrap-rose-smart-1'), [Symbol.for('send'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], Symbol.for('drop'), 1]], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('transfer-and-compile-comments'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], [Symbol.for('compile-expression'), [Symbol.for('begin-wrap-rose'), [Symbol.for('send'), [Symbol.for('array-list-last'), Symbol.for('cond-clauses')], Symbol.for('drop'), 1]], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement']], [Symbol.for('estree-type?'), Symbol.for('final-clause'), 'BlockStatement']], [Symbol.for('set!'), Symbol.for('final-clause'), [Symbol.for('make-block-statement'), [Symbol.for('list'), Symbol.for('final-clause')]]]], [Symbol.for('foldr'), Symbol.for('reducing-f'), Symbol.for('final-clause'), [Symbol.for('drop-right'), Symbol.for('cond-clauses'), 1]]], [Symbol.for('else'), [Symbol.for('foldr'), Symbol.for('reducing-f'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], null, [Symbol.for('new'), Symbol.for('Identifier'), 'undefined']], Symbol.for('cond-clauses')]]]]]];

/**
 * Compile a `(define ...)` expression.
 */
function compileDefine(node: any, env: any, options: any = {}): any {
  const languageEnv: any = options['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
  const language: any = options['language'];
  const inlineLispSources: any = options['inlineLispSources'];
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
    const declaredType: any = env.getLocalType(sym);
    if ((declaredType === Symbol.for('Any')) || (declaredType === Symbol.for('Undefined'))) {
      type_ = [Symbol.for('->'), ...((typeof params === 'symbol') ? [[Symbol.for('Listof'), Symbol.for('Any')]] : ((Array.isArray(params) && (params.length >= 3) && (params[params.length - 2] === Symbol.for('.')) && !((): any => {
        const x: any = lastCdr(params);
        return Array.isArray(x) && (x.length === 0);
      })()) ? [...makeList(params.length - 2, Symbol.for('Any')), [Symbol.for('Listof'), Symbol.for('Any')]] : makeList(params.length, Symbol.for('Any')))), returnType];
      makeTypeBinding(env, nameSym, type_, langFilter);
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
    if (inlineLispSources) {
      const sym: any = Symbol.for(functionName);
      const lispCodeExp: any = compileSexp([Symbol.for('declare'), sym, [Symbol.for('fsource'), [Symbol.for('quote'), exp]]], env, options);
      return new Program([result, lispCodeExp]);
    } else {
      return result;
    }
  } else if (exp.length === 2) {
    // Uninitialized variable.
    makeTypeBinding(env, (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
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
    })() : exp[1], Symbol.for('Any'), langFilter);
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
    if (env.has(sym, {
      filter: langFilter
    })) {
      type_ = env.getType(sym);
    } else {
      makeTypeBinding(env, (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
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
      })() : exp[1], type_, langFilter);
    }
    return new VariableDeclaration([new VariableDeclarator(symCompiled.setType(compileType(type_, env, options)), compileExpression(node.get(2), env, options))], 'let');
  }
}

compileDefine.fsource = [Symbol.for('define'), [Symbol.for('compile-define'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('inline-lisp-sources'), [Symbol.for('oget'), Symbol.for('options'), 'inlineLispSources']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('cond'), [[Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('should-curry'), [Symbol.for('array?'), Symbol.for('sym')]], [Symbol.for('define'), Symbol.for('name-sym'), [Symbol.for('if'), Symbol.for('should-curry'), [Symbol.for('first'), [Symbol.for('flatten'), Symbol.for('sym')]], Symbol.for('sym')]], [Symbol.for('define'), Symbol.for('function-name'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('name-sym')], Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('lambda-exp'), [Symbol.for('define->lambda'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], [Symbol.for('send'), Symbol.for('get-value')]], [Symbol.for('quote'), Symbol.for(':')]], [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 3], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('else'), [Symbol.for('quote'), Symbol.for('Any')]]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('~>'), [Symbol.for('send'), Symbol.for('lambda-exp'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('declared-type'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-local-type'), Symbol.for('sym')]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('declared-type'), [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('eq?'), Symbol.for('declared-type'), [Symbol.for('quote'), Symbol.for('Undefined')]]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('quasiquote'), [Symbol.for('->'), [Symbol.for('unquote-splicing'), [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params')], [Symbol.for('list'), [Symbol.for('quote'), [Symbol.for('Listof'), Symbol.for('Any')]]]], [[Symbol.for('dotted-list?'), Symbol.for('params')], [Symbol.for('append'), [Symbol.for('make-list'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('params')], 2], [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('list'), [Symbol.for('quote'), [Symbol.for('Listof'), Symbol.for('Any')]]]]], [Symbol.for('else'), [Symbol.for('make-list'), [Symbol.for('array-list-length'), Symbol.for('params')], [Symbol.for('quote'), Symbol.for('Any')]]]]], [Symbol.for('unquote'), Symbol.for('return-type')]]]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('name-sym'), Symbol.for('type_'), Symbol.for('lang-filter')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('type_'), Symbol.for('declared-type')]]], [Symbol.for('define'), Symbol.for('compiled-type'), [Symbol.for('compile-type-exp'), Symbol.for('type_'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [Symbol.for('should-curry'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-define'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('name-sym')], [Symbol.for('unquote'), Symbol.for('lambda-exp')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('is-a?'), Symbol.for('compiled-type'), Symbol.for('TSFunctionType')], [Symbol.for('is-a?'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('compiled-type')], Symbol.for('TSVoidKeyword')]], 'void', undefined]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-js-function'), Symbol.for('lambda-exp'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')], [Symbol.for('js-obj'), 'functionName', Symbol.for('function-name'), 'returnType', Symbol.for('return-type')]]], [Symbol.for('when'), [Symbol.for('is-a?'), Symbol.for('compiled-type'), Symbol.for('TSFunctionType')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('result')]]]]], [Symbol.for('define'), Symbol.for('param'), [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('result')], Symbol.for('i')]], [Symbol.for('define'), Symbol.for('type-param'), [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('compiled-type')], Symbol.for('i')]], [Symbol.for('define'), Symbol.for('type-param-annotation'), [Symbol.for('if'), Symbol.for('type-param'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('type-param')], [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]], [Symbol.for('unless'), [Symbol.for('send'), Symbol.for('param'), Symbol.for('has-type')], [Symbol.for('send'), Symbol.for('param'), Symbol.for('set-type'), Symbol.for('type-param-annotation')]]], [Symbol.for('set-field!'), Symbol.for('returnType'), Symbol.for('result'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('compiledType')]]]]], [Symbol.for('cond'), [Symbol.for('inline-lisp-sources'), [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('string->symbol'), Symbol.for('function-name')]], [Symbol.for('define'), Symbol.for('lisp-code-exp'), [Symbol.for('compile-sexp'), [Symbol.for('quasiquote'), [Symbol.for('declare'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('fsource'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Program'), [Symbol.for('list'), Symbol.for('result'), Symbol.for('lisp-code-exp')]]], [Symbol.for('else'), Symbol.for('result')]]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('make-type-binding'), Symbol.for('env'), [Symbol.for('second'), Symbol.for('exp')], [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]], 'let']], [[Symbol.for('and'), [Symbol.for('form?'), [Symbol.for('third'), Symbol.for('exp')], Symbol.for('js-async_'), Symbol.for('env')], [Symbol.for('form?'), [Symbol.for('second'), [Symbol.for('third'), Symbol.for('exp')]], Symbol.for('lambda_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('lambda-node'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('lambda-node'), Symbol.for('get'), 1], Symbol.for('drop'), 0]], [Symbol.for('define'), Symbol.for('da-form'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define/async'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('unquote-splicing'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('lambda-node'), Symbol.for('drop'), 2]]]]]]], [Symbol.for('compile-define-async'), Symbol.for('da-form'), Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('form?'), [Symbol.for('third'), Symbol.for('exp')], Symbol.for('class_'), Symbol.for('env')], [Symbol.for('compile-define-class'), [Symbol.for('make-rose'), [Symbol.for('define->define-class'), Symbol.for('exp')], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('sym-compiled'), [Symbol.for('compile-symbol'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-type'), Symbol.for('sym')]]], [Symbol.for('else'), [Symbol.for('make-type-binding'), Symbol.for('env'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('type_'), Symbol.for('lang-filter')]]], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('~>'), Symbol.for('sym-compiled'), [Symbol.for('send'), Symbol.for('set-type'), [Symbol.for('compile-type'), Symbol.for('type_'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]]], 'let']]]];

/**
 * Compile a `(define/async ...)` expression.
 */
function compileDefineAsync(node: any, env: any, options: any = {}): any {
  const inlineLispSources: any = options['inlineLispSources'];
  let result: any = compileDefine(node, env, options);
  const resultF: any = inlineLispSources ? result.body[0] : result;
  if (estreeTypeP(resultF, 'FunctionDeclaration')) {
    resultF.async = true;
  }
  const returnType: any = resultF.returnType;
  resultF.returnType = new TSTypeReference(new Identifier('Promise'), new TSTypeParameterInstantiation([new TSAnyKeyword()]));
  return result;
}

compileDefineAsync.fsource = [Symbol.for('define'), [Symbol.for('compile-define-async'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inline-lisp-sources'), [Symbol.for('oget'), Symbol.for('options'), 'inlineLispSources']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-define'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result-f'), [Symbol.for('if'), Symbol.for('inline-lisp-sources'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('result')]], Symbol.for('result')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('result-f'), 'FunctionDeclaration'], [Symbol.for('set-field!'), Symbol.for('async'), Symbol.for('result-f'), true]], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('result-f')]], [Symbol.for('set-field!'), Symbol.for('returnType'), Symbol.for('result-f'), [Symbol.for('new'), Symbol.for('TSTypeReference'), [Symbol.for('new'), Symbol.for('Identifier'), 'Promise'], [Symbol.for('new'), Symbol.for('TSTypeParameterInstantiation'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]]], Symbol.for('result')];

/**
 * Compile a `(define/generator ...)` expression.
 */
function compileDefineGenerator(node: any, env: any, options: any = {}): any {
  let result: any = compileDefine(node, env, options);
  result.generator = true;
  return result;
}

compileDefineGenerator.fsource = [Symbol.for('define'), [Symbol.for('compile-define-generator'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-define'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('generator'), Symbol.for('result'), true], Symbol.for('result')];

/**
 * Compile a `(/ ...)` expression.
 */
function compileDiv(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  if (exp.length === 1) {
    return compileExpression(makeRose(undefined, node), env, options);
  } else if (exp.length === 2) {
    return compileDiv(makeRose([Symbol.for('/'), 1, node.get(1)], node), env, options);
  } else {
    return compileBinaryExpression(node, env, options, {
      identity: 1,
      operator: '/'
    });
  }
}

compileDiv.fsource = [Symbol.for('define'), [Symbol.for('compile-div'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 1], [Symbol.for('compile-expression'), [Symbol.for('make-rose'), undefined, Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('compile-div'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('/'), 1, [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 1, 'operator', '/']]]]];

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

compileSend.fsource = [Symbol.for('define'), [Symbol.for('compile-send'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), [Symbol.for('new'), Symbol.for('MemberExpression'), [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('obj'), Symbol.for('get-value')]], [Symbol.for('compile-symbol'), Symbol.for('obj'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('compile-symbol'), Symbol.for('method'), Symbol.for('env'), Symbol.for('options')], false], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('args')]], Symbol.for('options')]];

/**
 * Compile a `(send/apply ...)` expression.
 */
function compileSendApply(node: any, env: any, options: any = {}): any {
  const obj: any = node.get(1);
  const method: any = node.get(2);
  const args: any = node.drop(3);
  return makeExpressionOrStatement(compileExpression(makeRose([Symbol.for('apply'), [Symbol.for('get-field'), method, obj], ...args], node), env, options), options);
}

compileSendApply.fsource = [Symbol.for('define'), [Symbol.for('compile-send-apply'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('apply'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('method')], [Symbol.for('unquote'), Symbol.for('obj')]], [Symbol.for('unquote-splicing'), Symbol.for('args')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]];

/**
 * Compile a `(js/=== ...)` expression.
 */
function compileJsStrictlyEqual(node: any, env: any, options: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    identity: true,
    operator: '==='
  });
}

compileJsStrictlyEqual.fsource = [Symbol.for('define'), [Symbol.for('compile-js-strictly-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', true, 'operator', '===']]];

/**
 * Compile a `(js/== ...)` expression.
 */
function compileJsLooselyEqual(node: any, env: any, options: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    identity: true,
    operator: '=='
  });
}

compileJsLooselyEqual.fsource = [Symbol.for('define'), [Symbol.for('compile-js-loosely-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', true, 'operator', '==']]];

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
  return [Symbol.for('js/reduce'), lst, flipFunctionExpression(f, env), v];
}

compileFoldlMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-foldl-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('js/reduce'), [Symbol.for('unquote'), Symbol.for('lst')], [Symbol.for('unquote'), [Symbol.for('flip-function-expression'), Symbol.for('f'), Symbol.for('env')]], [Symbol.for('unquote'), Symbol.for('v')]]]];

compileFoldlMacro.ftype = 'macro';

/**
 * Compiler macro for `(foldr ...)` expressions.
 */
function compileFoldrMacro(exp: any, env: any): any {
  const [f, v, lst]: any[] = exp.slice(1);
  // Like `foldl`, but invokes the `reduceRight` method instead.
  return [Symbol.for('js/reduce-right'), lst, flipFunctionExpression(f, env), v];
}

compileFoldrMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-foldr-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('js/reduce-right'), [Symbol.for('unquote'), Symbol.for('lst')], [Symbol.for('unquote'), [Symbol.for('flip-function-expression'), Symbol.for('f'), Symbol.for('env')]], [Symbol.for('unquote'), Symbol.for('v')]]]];

compileFoldrMacro.ftype = 'macro';

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

flipFunctionExpression.fsource = [Symbol.for('define'), [Symbol.for('flip-function-expression'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('x')], [[Symbol.for('unquote'), Symbol.for('exp')], Symbol.for('x'), Symbol.for('acc')]]]], [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('>='), [Symbol.for('array-list-length'), [Symbol.for('second'), Symbol.for('exp')]], 2]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [[Symbol.for('unquote'), [Symbol.for('second'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('unquote'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), [Symbol.for('second'), Symbol.for('exp')], 2]]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('exp'), 2]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('C-exp'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')]]]]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('C-exp')], [Symbol.for('unquote'), Symbol.for('exp')]]]]]];

/**
 * Compile a `(funcall ...)` expression.
 */
function compileFuncall(node: any, env: any, options: any = {}): any {
  return compileFunctionCall(sliceRose(node, 1), env, options);
}

compileFuncall.fsource = [Symbol.for('define'), [Symbol.for('compile-funcall'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-function-call'), [Symbol.for('slice-rose'), Symbol.for('node'), 1], Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a function call.
 */
function compileFunctionCall(node: any, env: any, options: any = {}): any {
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
  {
    ...options,
    shouldInline: false
  } : options);
  const argsExps: any = args.map(function (x: any): any {
    return compileExpression(x, env, options);
  });
  return makeExpressionOrStatement(new CallExpression(calleeExp, argsExps), options);
}

compileFunctionCall.fsource = [Symbol.for('define'), [Symbol.for('compile-function-call'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('referenced-symbols'), [Symbol.for('oget'), Symbol.for('options'), 'referencedSymbols']], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('oget'), Symbol.for('options'), 'currentModule']], [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('send'), Symbol.for('callee'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('symbolic-op'), [Symbol.for('symbol?'), Symbol.for('op')]], [Symbol.for('define'), Symbol.for('should-inline-op'), [Symbol.for('and'), Symbol.for('symbolic-op'), [Symbol.for('should-inline?'), Symbol.for('op'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('not'), [Symbol.for('send'), Symbol.for('compilation-mapping-environment'), Symbol.for('has'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('op')]]]]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('callee-exp'), [Symbol.for('compile-expression'), Symbol.for('callee'), Symbol.for('env'), [Symbol.for('if'), [Symbol.for('and'), Symbol.for('symbolic-op'), [Symbol.for('not'), Symbol.for('should-inline-op')]], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'shouldInline', false]], Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('args-exps'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('args')]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), Symbol.for('callee-exp'), Symbol.for('args-exps')], Symbol.for('options')]];

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

addReferencedSymbol.fsource = [Symbol.for('define'), [Symbol.for('add-referenced-symbol'), Symbol.for('sym'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('referenced-symbols'), [Symbol.for('oget'), Symbol.for('options'), 'referencedSymbols']], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('referenced-symbols'), [Symbol.for('not'), [Symbol.for('memq?'), Symbol.for('sym'), Symbol.for('referenced-symbols')]], [Symbol.for('should-inline?'), Symbol.for('sym'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('push-right!'), Symbol.for('referenced-symbols'), Symbol.for('sym')]]];

/**
 * Whether the language binding for `sym` should be added to
 * the global environment.
 */
function shouldInlineP(sym: any, env: any, options: any = {}): any {
  // This may be disabled with the `shouldInline` option.
  const shouldInlineOption: any = options['shouldInline'];
  if (!shouldInlineOption) {
    return false;
  }
  const languageEnv: any = options['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
  function jsFilter(x: any): any {
    return x !== jsEnvironment;
  }
  jsFilter.fsource = [Symbol.for('define'), [Symbol.for('js-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('js-environment')]]];
  const compilationMappingEnvironment: any = options['compilationMappingEnvironment'];
  const currentModule: any = options['currentModule'];
  return (
    (
     (
      (
       (
        (typeof sym === 'symbol') &&
        // Do not inline if the symbol is listed in
        // `compilation-variables-env`.
        !compilationVariablesEnv.has(sym)
       ) &&
       // Do not inline if there is a local binding for the
       // value (e.g., a `let` variable).
       !env.has(sym, {
         filter: langFilter
       })
      ) &&
      // Do not inline if the current module defines the
      // value.
      !(currentModule && currentModule.hasSymbol(sym))
     ) &&
     // Only inline if the language environment binds the symbol.
     // However, do not inline if the value is a JavaScript
     // value, i.e., if it is provided by the very language
     // compiled to.
     languageEnv.has(sym, {
       filter: jsFilter
     })
    )
  );
}

shouldInlineP.fsource = [Symbol.for('define'), [Symbol.for('should-inline?'), Symbol.for('sym'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('should-inline-option'), [Symbol.for('oget'), Symbol.for('options'), 'shouldInline']], [Symbol.for('unless'), Symbol.for('should-inline-option'), [Symbol.for('return'), false]], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), [Symbol.for('js-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('js-environment')]]], [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('oget'), Symbol.for('options'), 'currentModule']], [Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('sym')], [Symbol.for('not'), [Symbol.for('send'), Symbol.for('compilation-variables-env'), Symbol.for('has'), Symbol.for('sym')]], [Symbol.for('not'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]]], [Symbol.for('not'), [Symbol.for('and'), Symbol.for('current-module'), [Symbol.for('send'), Symbol.for('current-module'), Symbol.for('has-symbol'), Symbol.for('sym')]]], [Symbol.for('send'), Symbol.for('language-env'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('js-filter')]]]];

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

compileGreaterThan.fsource = [Symbol.for('define'), [Symbol.for('compile-greater-than'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), true], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', true, 'operator', '>']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('>'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];

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

compileGreaterThanOrEqual.fsource = [Symbol.for('define'), [Symbol.for('compile-greater-than-or-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), true], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', true, 'operator', '>=']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('>='), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];

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

compileBinaryExpression.fsource = [Symbol.for('define'), [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('oget'), Symbol.for('settings'), 'operator']], [Symbol.for('define'), Symbol.for('logical'), [Symbol.for('oget'), Symbol.for('settings'), 'logical']], [Symbol.for('define'), Symbol.for('operands'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('operands')], 0], [Symbol.for('define'), Symbol.for('identity'), [Symbol.for('oget'), Symbol.for('settings'), 'identity']], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('identity')], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('operands')], 1], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-rose'), [Symbol.for('first'), Symbol.for('operands')], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('compiled-operands'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('arg')], [Symbol.for('compile-expression'), Symbol.for('arg'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('operands')]], [Symbol.for('make-expression-or-statement'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('if'), Symbol.for('logical'), [Symbol.for('new'), Symbol.for('LogicalExpression'), Symbol.for('operator'), Symbol.for('acc'), Symbol.for('x')], [Symbol.for('new'), Symbol.for('BinaryExpression'), Symbol.for('operator'), Symbol.for('acc'), Symbol.for('x')]]], [Symbol.for('first'), Symbol.for('compiled-operands')], [Symbol.for('rest'), Symbol.for('compiled-operands')]], Symbol.for('options')]]]];

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

compileLogicalExpression.fsource = [Symbol.for('define'), [Symbol.for('compile-logical-expression'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj-append'), Symbol.for('settings'), [Symbol.for('js-obj'), 'logical', true]]]];

/**
 * Compile a `(lambda ...)` expression.
 */
function compileLambda(node: any, env: any, options: any = {}): any {
  return compileJsFunction(node, env, options);
}

compileLambda.fsource = [Symbol.for('define'), [Symbol.for('compile-lambda'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-js-function'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

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
  const languageEnv: any = inheritedOptions['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
  const env1: any = extendEnvironment(new LispEnvironment(), env);
  let argsList: any;
  let regularArgs: any;
  let restArg: any;
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
    restArg = argsList[argsList.length - 1];
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
        makeTypeBinding(env1, sym, Symbol.for('Any'), langFilter);
        let result: any = ((arg.length === 4) ? new AssignmentPattern(new Identifier(printEstree(compileExpression(makeRose(sym), env1, inheritedOptions), inheritedOptions)), compileExpression(makeRose((Array.isArray(arg) && (arg.length >= 3) && (arg[arg.length - 2] === Symbol.for('.')) && ((): any => {
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
        })() : arg[3]), env1, inheritedOptions)) : new Identifier(printEstree(compileExpression(makeRose(sym), env1, inheritedOptions), inheritedOptions))).setType(compileType(typ, env1, options));
        params.push(result);
      } else if (Array.isArray(arg)) {
        makeTypeBinding(env1, arg[0], Symbol.for('Any'), langFilter);
        params.push(new AssignmentPattern(new Identifier(printEstree(compileExpression(makeRose(arg[0]), env1, inheritedOptions), inheritedOptions)), compileExpression(makeRose((Array.isArray(arg) && (arg.length >= 3) && (arg[arg.length - 2] === Symbol.for('.')) && ((): any => {
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
        })() : arg[1]), env1, inheritedOptions)));
      } else {
        makeTypeBinding(env1, arg, Symbol.for('Any'), langFilter);
        params.push(new Identifier(printEstree(compileExpression(makeRose(arg), env1, inheritedOptions), inheritedOptions)));
      }
    }
  }
  if (restArg) {
    makeTypeBinding(env1, restArg, Symbol.for('Any'), langFilter);
    params.push(new RestElement(compileExpression(makeRose(restArg), env1, inheritedOptions)));
  }
  let bodyStatements: any = node.drop(2);
  if ((bodyStatements.length > 0) && (bodyStatements[0].getValue() === Symbol.for(':'))) {
    bodyStatements = bodyStatements.slice(2);
  }
  const body: any = wrapInBlockStatement(// wrap-in-block-statement-smart
  compileStatementOrReturnStatement(beginWrapRoseSmart1(bodyStatements).setParent(node), env1, {
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

compileJsFunction.fsource = [Symbol.for('define'), [Symbol.for('compile-js-function'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('function-name'), [Symbol.for('oget'), Symbol.for('settings'), 'functionName']], [Symbol.for('define'), Symbol.for('generator'), [Symbol.for('oget'), Symbol.for('settings'), 'generator']], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('oget'), Symbol.for('settings'), 'returnType']], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'language']], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('env1'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('env')]], [Symbol.for('define'), Symbol.for('args-list')], [Symbol.for('define'), Symbol.for('regular-args')], [Symbol.for('define'), Symbol.for('rest-arg')], [Symbol.for('cond'), [[Symbol.for('symbol?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('rest-arg'), [Symbol.for('second'), Symbol.for('exp')]]], [[Symbol.for('dotted-list?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('args-list'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('regular-args'), [Symbol.for('linked-list-drop-right_'), Symbol.for('args-list'), 1]], [Symbol.for('set!'), Symbol.for('rest-arg'), [Symbol.for('dotted-list-tail'), Symbol.for('args-list')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('regular-args'), [Symbol.for('second'), Symbol.for('exp')]]]], [Symbol.for('when'), Symbol.for('regular-args'), [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('regular-args')]], [Symbol.for('cond'), [[Symbol.for('colon-form?'), Symbol.for('arg')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), Symbol.for('arg')]], [Symbol.for('define'), Symbol.for('typ'), [Symbol.for('third'), Symbol.for('arg')]], [Symbol.for('make-type-binding'), Symbol.for('env1'), Symbol.for('sym'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('~>'), [Symbol.for('if'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('arg')], 4], [Symbol.for('new'), Symbol.for('AssignmentPattern'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('sym')], Symbol.for('env1'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]], [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('fourth'), Symbol.for('arg')]], Symbol.for('env1'), Symbol.for('inherited-options')]], [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('sym')], Symbol.for('env1'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]], [Symbol.for('send'), Symbol.for('set-type'), [Symbol.for('compile-type'), Symbol.for('typ'), Symbol.for('env1'), Symbol.for('options')]]]], [Symbol.for('push-right!'), Symbol.for('params'), Symbol.for('result')]], [[Symbol.for('array?'), Symbol.for('arg')], [Symbol.for('make-type-binding'), Symbol.for('env1'), [Symbol.for('first'), Symbol.for('arg')], [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')], [Symbol.for('push-right!'), Symbol.for('params'), [Symbol.for('new'), Symbol.for('AssignmentPattern'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('first'), Symbol.for('arg')]], Symbol.for('env1'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]], [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('second'), Symbol.for('arg')]], Symbol.for('env1'), Symbol.for('inherited-options')]]]], [Symbol.for('else'), [Symbol.for('make-type-binding'), Symbol.for('env1'), Symbol.for('arg'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')], [Symbol.for('push-right!'), Symbol.for('params'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('arg')], Symbol.for('env1'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]]]], [Symbol.for('when'), Symbol.for('rest-arg'), [Symbol.for('make-type-binding'), Symbol.for('env1'), Symbol.for('rest-arg'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')], [Symbol.for('push-right!'), Symbol.for('params'), [Symbol.for('new'), Symbol.for('RestElement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), Symbol.for('rest-arg')], Symbol.for('env1'), Symbol.for('inherited-options')]]]], [Symbol.for('define'), Symbol.for('body-statements'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('body-statements')], 0], [Symbol.for('eq?'), [Symbol.for('send'), [Symbol.for('first'), Symbol.for('body-statements')], Symbol.for('get-value')], [Symbol.for('quote'), Symbol.for(':')]]], [Symbol.for('set!'), Symbol.for('body-statements'), [Symbol.for('drop'), Symbol.for('body-statements'), 2]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('~>'), [Symbol.for('begin-wrap-rose-smart-1'), Symbol.for('body-statements')], [Symbol.for('send'), Symbol.for('set-parent'), Symbol.for('node')]], Symbol.for('env1'), [Symbol.for('js-obj-append'), Symbol.for('inherited-options'), [Symbol.for('js-obj'), 'expressionType', [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('return-type'), 'void'], 'statement', 'return']]]]]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [[Symbol.for('and'), Symbol.for('function-name'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('function-name'), '']]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('FunctionDeclaration'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('function-name')], Symbol.for('params'), Symbol.for('body')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('FunctionExpression'), Symbol.for('params'), Symbol.for('body')]]]], [Symbol.for('when'), Symbol.for('generator'), [Symbol.for('set-field!'), Symbol.for('generator'), Symbol.for('result'), true]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('inherited-options')]];

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

compileJsArrow.fsource = [Symbol.for('define'), [Symbol.for('compile-js-arrow'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('compile-js-function'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('f'), Symbol.for('FunctionExpression')], [Symbol.for('new'), Symbol.for('ArrowFunctionExpression'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('f')], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('f')]]], [Symbol.for('else'), Symbol.for('f')]]];

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

compileLessThan.fsource = [Symbol.for('define'), [Symbol.for('compile-less-than'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), true], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', true, 'operator', '<']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('<'), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];

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

compileLessThanOrEqual.fsource = [Symbol.for('define'), [Symbol.for('compile-less-than-or-equal'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), true], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', true, 'operator', '<=']]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('and')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 2, [Symbol.for('array-list-length'), Symbol.for('exp')]]]], [Symbol.for('push-right!'), Symbol.for('and-exp'), [Symbol.for('quasiquote'), [Symbol.for('<='), [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('unquote'), [Symbol.for('aget'), Symbol.for('exp'), Symbol.for('i')]]]]]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('and-exp')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(let ...)` expression.
 */
function compileLet(node: any, env: any, options: any = {}): any {
  // There is no distinction between `(let ...)` and `(let* ...)`
  // expressions---they are compiled in the same way.
  return compileLetStar(node, env, options);
}

compileLet.fsource = [Symbol.for('define'), [Symbol.for('compile-let'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-let-star'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a `(let* ...)` expression.
 */
function compileLetStar(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  let makeBlock: any = false;
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    const languageEnv: any = options['languageEnvironment'];
    function langFilter(x: any): any {
      return x !== languageEnv;
    }
    langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
    const env1: any = extendEnvironment(new LispEnvironment(), env);
    const inheritedOptions: any = {
      ...options
    };
    const letNodes: any = node.get(1).getNodes();
    const bodyNodes: any = node.drop(2);
    const defineNodes: any = letNodes.map(function (x: any): any {
      let exp: any = x.getValue();
      if (Array.isArray(exp)) {
        const sym: any = exp[0];
        if (!makeBlock && env1.has(sym, {
          filter: langFilter
        })) {
          makeBlock = true;
        }
        return makeRose([Symbol.for('define'), x.get(0), x.get(1)], x);
      } else {
        const sym: any = exp;
        if (!makeBlock && env1.has(sym, {
          filter: langFilter
        })) {
          makeBlock = true;
        }
        return makeRose([Symbol.for('define'), x], x);
      }
    });
    let result: any = compileRose(makeRose([makeBlock ? Symbol.for('js/block') : Symbol.for('begin'), ...defineNodes, ...bodyNodes], node), env1, inheritedOptions);
    return result;
  } else {
    return compileExpression(wrapInArrowCall(node), env, options);
  }
}

compileLetStar.fsource = [Symbol.for('define'), [Symbol.for('compile-let-star'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('make-block'), false], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('env1'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('env')]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('let-nodes'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('define-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('env1'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]]], [Symbol.for('set!'), Symbol.for('make-block'), true]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]]]], Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('sym'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('env1'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]]], [Symbol.for('set!'), Symbol.for('make-block'), true]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('x')]]], Symbol.for('x')]]]], Symbol.for('let-nodes')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('make-block'), [Symbol.for('quote'), Symbol.for('js/block')], [Symbol.for('quote'), Symbol.for('begin')]]], [Symbol.for('unquote-splicing'), Symbol.for('define-nodes')], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]], Symbol.for('node')], Symbol.for('env1'), Symbol.for('inherited-options')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(let-values ...)` expression.
 */
function compileLetValues(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    const languageEnv: any = options['languageEnvironment'];
    function langFilter(x: any): any {
      return x !== languageEnv;
    }
    langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
    const env1: any = extendEnvironment(new LispEnvironment(), env);
    const inheritedOptions: any = {
      ...options
    };
    let makeBlock: any = false;
    const letNodes: any = node.get(1).getNodes();
    const bodyNodes: any = node.drop(2);
    const defineNodes: any = letNodes.map(function (x: any): any {
      let exp: any = x.getValue();
      if (typeof exp === 'symbol') {
        const sym: any = exp;
        if (!makeBlock && env1.has(sym, {
          filter: langFilter
        })) {
          makeBlock = true;
        }
        return makeRose([Symbol.for('define'), x]);
      } else {
        const variables: any = x.get(0).getValue();
        if (typeof variables === 'symbol') {
          const sym: any = variables;
          if (!makeBlock && env1.has(sym, {
            filter: langFilter
          })) {
            makeBlock = true;
          }
        } else {
          const syms: any = flatten(variables);
          if (!makeBlock) {
            for (let sym of flatten(variables)) {
              if (env1.has(sym, {
                filter: langFilter
              })) {
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
    let result: any = compileRose(makeRose([makeBlock ? Symbol.for('js/block') : Symbol.for('begin'), ...defineNodes, ...bodyNodes], node), env1, inheritedOptions);
    return result;
  } else {
    return compileExpression(wrapInArrowCall(node), env, options);
  }
}

compileLetValues.fsource = [Symbol.for('define'), [Symbol.for('compile-let-values'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('env1'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('env')]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('make-block'), false], [Symbol.for('define'), Symbol.for('let-nodes'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('define-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('sym'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('env1'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]]], [Symbol.for('set!'), Symbol.for('make-block'), true]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('x')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('variables'), [Symbol.for('~>'), Symbol.for('x'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('variables')], [Symbol.for('define'), Symbol.for('sym'), Symbol.for('variables')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('env1'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]]], [Symbol.for('set!'), Symbol.for('make-block'), true]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('syms'), [Symbol.for('flatten'), Symbol.for('variables')]], [Symbol.for('unless'), Symbol.for('make-block'), [Symbol.for('for'), [[Symbol.for('sym'), [Symbol.for('flatten'), Symbol.for('variables')]]], [Symbol.for('when'), [Symbol.for('send'), Symbol.for('env1'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]], [Symbol.for('set!'), Symbol.for('make-block'), true], [Symbol.for('break')]]]]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]]]], Symbol.for('x')]]]], Symbol.for('let-nodes')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('make-block'), [Symbol.for('quote'), Symbol.for('js/block')], [Symbol.for('quote'), Symbol.for('begin')]]], [Symbol.for('unquote-splicing'), Symbol.for('define-nodes')], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]], Symbol.for('node')], Symbol.for('env1'), Symbol.for('inherited-options')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(define-values ...)` expression.
 */
function compileDefineValues(node: any, env: any, options: any = {}): any {
  let exp: any = node.getValue();
  const inheritedOptions: any = {
    ...options
  };
  const expressionType: any = inheritedOptions['expressionType'];
  const languageEnv: any = inheritedOptions['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
  let makeBlock: any = true;
  let holeMarker: any = Symbol.for('_');
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
    makeTypeBinding(env, variables, Symbol.for('Any'), langFilter);
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
        makeTypeBinding(env, x, Symbol.for('Any'), langFilter);
        return new Identifier(printEstree(compileSymbol(makeRose(x), env, inheritedOptions), inheritedOptions));
      }
    });
    if (restVar) {
      makeTypeBinding(env, restVar, Symbol.for('Any'), langFilter);
      varDecls.push(new RestElement(new Identifier(printEstree(compileSymbol(makeRose(restVar), env, inheritedOptions), inheritedOptions))));
    }
    declaratorId = new ArrayPattern(varDecls);
  }
  declaratorInit = compileExpression(expression, env, inheritedOptions);
  return new VariableDeclaration([new VariableDeclarator(declaratorId, declaratorInit)], 'let');
}

compileDefineValues.fsource = [Symbol.for('define'), [Symbol.for('compile-define-values'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('make-block'), true], [Symbol.for('define'), Symbol.for('hole-marker'), [Symbol.for('quote'), Symbol.for('_')]], [Symbol.for('define'), Symbol.for('variables'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 2]]], [Symbol.for('define'), Symbol.for('regular-vars'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-var'), undefined], [Symbol.for('define'), Symbol.for('var-decls'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('declarator-id')], [Symbol.for('define'), Symbol.for('declarator-init')], [Symbol.for('when'), [Symbol.for('eq?'), [Symbol.for('send'), Symbol.for('expression'), Symbol.for('get-value')], [Symbol.for('quote'), Symbol.for(':hole-marker')]], [Symbol.for('set!'), Symbol.for('hole-marker'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 3], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('set!'), Symbol.for('expression'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 4]]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('variables')], [Symbol.for('set!'), Symbol.for('declarator-id'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('variables')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('variables'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')]], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('variables')], [Symbol.for('define'), Symbol.for('var-list'), [Symbol.for('flatten'), Symbol.for('variables')]], [Symbol.for('set!'), Symbol.for('regular-vars'), [Symbol.for('drop-right'), Symbol.for('var-list'), 1]], [Symbol.for('set!'), Symbol.for('rest-var'), [Symbol.for('array-list-last'), Symbol.for('var-list')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('regular-vars'), Symbol.for('variables')]]], [Symbol.for('set!'), Symbol.for('var-decls'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')], null], [Symbol.for('else'), [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')], [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]], Symbol.for('regular-vars')]], [Symbol.for('when'), Symbol.for('rest-var'), [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('rest-var'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')], [Symbol.for('push-right!'), Symbol.for('var-decls'), [Symbol.for('new'), Symbol.for('RestElement'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('rest-var')], Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]], [Symbol.for('set!'), Symbol.for('declarator-id'), [Symbol.for('new'), Symbol.for('ArrayPattern'), Symbol.for('var-decls')]]]], [Symbol.for('set!'), Symbol.for('declarator-init'), [Symbol.for('compile-expression'), Symbol.for('expression'), Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), Symbol.for('declarator-id'), Symbol.for('declarator-init')]], 'let']];

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
  let declaration: any;
  let declarator: any;
  let left: any;
  let right: any;
  declaration = compileDefineValues(makeRose([Symbol.for('define-values'), ...node.drop(1)], node), env, inheritedOptions);
  declarator = declaration.declarations[0];
  left = declarator.id;
  right = declarator.init;
  return wrapExpressionInStatement(new AssignmentExpression('=', left, right), inheritedOptions);
}

compileSetValues.fsource = [Symbol.for('define'), [Symbol.for('compile-set-values'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('inherited-options'), 'expressionType']], [Symbol.for('define'), Symbol.for('make-block'), true], [Symbol.for('define'), Symbol.for('declaration')], [Symbol.for('define'), Symbol.for('declarator')], [Symbol.for('define'), Symbol.for('left')], [Symbol.for('define'), Symbol.for('right')], [Symbol.for('set!'), Symbol.for('declaration'), [Symbol.for('compile-define-values'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('declarator'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('declaration')]]], [Symbol.for('set!'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('declarator')]], [Symbol.for('set!'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('declarator')]], [Symbol.for('wrap-expression-in-statement'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', Symbol.for('left'), Symbol.for('right')], Symbol.for('inherited-options')]];

/**
 * Compile a `(let-fields ...)` expression.
 */
function compileLetFields(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    const languageEnv: any = options['languageEnvironment'];
    function langFilter(x: any): any {
      return x !== languageEnv;
    }
    langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
    const env1: any = extendEnvironment(new LispEnvironment(), env);
    const inheritedOptions: any = {
      ...options
    };
    let makeBlock: any = false;
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
        if (!makeBlock && env1.has(sym, {
          filter: langFilter
        })) {
          makeBlock = true;
        }
      }
      return makeRose([Symbol.for('define-fields'), fields, obj], x);
    });
    let result: any = compileRose(makeRose([makeBlock ? Symbol.for('js/block') : Symbol.for('begin'), ...defineNodes, ...bodyNodes], node), env1, inheritedOptions);
    return result;
  } else {
    return compileExpression(wrapInArrowCall(node), env, options);
  }
}

compileLetFields.fsource = [Symbol.for('define'), [Symbol.for('compile-let-fields'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('env1'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('env')]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('make-block'), false], [Symbol.for('define'), Symbol.for('let-nodes'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('define-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('fields'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('fields-exp'), [Symbol.for('send'), Symbol.for('fields'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('for'), [[Symbol.for('f'), Symbol.for('fields-exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('f')], [Symbol.for('second'), Symbol.for('f')], Symbol.for('f')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('make-block')], [Symbol.for('send'), Symbol.for('env1'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]]], [Symbol.for('set!'), Symbol.for('make-block'), true]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define-fields'), [Symbol.for('unquote'), Symbol.for('fields')], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('x')]], Symbol.for('let-nodes')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('if'), Symbol.for('make-block'), [Symbol.for('quote'), Symbol.for('js/block')], [Symbol.for('quote'), Symbol.for('begin')]]], [Symbol.for('unquote-splicing'), Symbol.for('define-nodes')], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]], Symbol.for('node')], Symbol.for('env1'), Symbol.for('inherited-options')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(define-fields ...)` expression.
 */
function compileDefineFields(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  const languageEnv: any = options['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
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
    makeTypeBinding(env, sym, Symbol.for('Any'), langFilter);
  }
  const expressionStatement: any = compileSetFields(makeRose([Symbol.for('set!-fields'), fields, obj], node), env, options);
  const assignmentExpression: any = expressionStatement.expression;
  let left: any = assignmentExpression.left;
  let right: any = assignmentExpression.right;
  return new VariableDeclaration([new VariableDeclarator(left, right)], 'let');
}

compileDefineFields.fsource = [Symbol.for('define'), [Symbol.for('compile-define-fields'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('fields'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('fields-exp'), [Symbol.for('send'), Symbol.for('fields'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('for'), [[Symbol.for('f'), Symbol.for('fields-exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('f')], [Symbol.for('second'), Symbol.for('f')], Symbol.for('f')]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('sym'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')]], [Symbol.for('define'), Symbol.for('expression-statement'), [Symbol.for('compile-set-fields'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!-fields'), [Symbol.for('unquote'), Symbol.for('fields')], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('assignment-expression'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('expression-statement')]], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('assignment-expression')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('assignment-expression')]], [Symbol.for('new'), Symbol.for('VariableDeclaration'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), Symbol.for('left'), Symbol.for('right')]], 'let']];

/**
 * Compile a `(set!-fields! ...)` expression.
 */
function compileSetFields(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  return wrapExpressionInStatement(new AssignmentExpression('=', new ObjectPattern(node.get(1).drop(0).map(function (x: any): any {
    let exp: any = x.getValue();
    if (Array.isArray(exp)) {
      return new Property(compileSymbol(x.get(0), env, options), compileSymbol(x.get(1), env, options));
    } else {
      const key: any = compileSymbol(x, env, options);
      return new Property(key, key);
    }
  })), compileExpression(node.get(2), env, options)), options);
}

compileSetFields.fsource = [Symbol.for('define'), [Symbol.for('compile-set-fields'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('wrap-expression-in-statement'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', [Symbol.for('new'), Symbol.for('ObjectPattern'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('new'), Symbol.for('Property'), [Symbol.for('compile-symbol'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0], Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-symbol'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('key'), [Symbol.for('compile-symbol'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Property'), Symbol.for('key'), Symbol.for('key')]]]], [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('drop'), 0]]], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

/**
 * Compile a `(list ...)` expression.
 */
function compileList(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new ArrayExpression(node.drop(1).map(function (x: any): any {
    return compileExpression(x, env, options);
  })), options);
}

compileList.fsource = [Symbol.for('define'), [Symbol.for('compile-list'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ArrayExpression'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]], Symbol.for('options')]];

/**
 * Compile a macro call.
 */
function compileMacroCall(node: any, env: any, options: any = {}): any {
  // Only expand the macro a single step, as there might be
  // compilers defined for the immediate expansion.
  const [expansion]: any[] = macroexpand1(node, env);
  return compileRose(expansion, env, options);
}

compileMacroCall.fsource = [Symbol.for('define'), [Symbol.for('compile-macro-call'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define-values'), [Symbol.for('expansion')], [Symbol.for('macroexpand-1'), Symbol.for('node'), Symbol.for('env')]], [Symbol.for('compile-rose'), Symbol.for('expansion'), Symbol.for('env'), Symbol.for('options')]];

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

macroexpand.fsource = [Symbol.for('define'), [Symbol.for('macroexpand'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('expanded'), false], [Symbol.for('define'), Symbol.for('expanded1'), true], [Symbol.for('while'), Symbol.for('expanded1'), [Symbol.for('set!-values'), [Symbol.for('result'), Symbol.for('expanded1')], [Symbol.for('macroexpand-1'), Symbol.for('result'), Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('expanded'), [Symbol.for('or'), Symbol.for('expanded'), Symbol.for('expanded1')]]], [Symbol.for('values'), Symbol.for('result'), Symbol.for('expanded')]];

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
    if (macroTypeP(typ)) {
      result = macroF(exp, env);
      expanded = true;
    }
  }
  return [(node instanceof Rose) ? makeRose(result, node) : result, expanded];
}

macroexpand1.fsource = [Symbol.for('define'), [Symbol.for('macroexpand-1'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('node'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('expanded'), false], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')], Symbol.for('node')]], [Symbol.for('set!'), Symbol.for('env'), [Symbol.for('or'), Symbol.for('env'), [Symbol.for('current-environment_')], [Symbol.for('empty-environment')]]], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('list?'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('exp')]], [[Symbol.for('null?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('exp')]], [[Symbol.for('quote?'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('text-of-quotation'), Symbol.for('exp')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('define-values'), [Symbol.for('macro-f'), Symbol.for('typ')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('get-typed-value'), Symbol.for('op')]], [Symbol.for('when'), [Symbol.for('macro-type?'), Symbol.for('typ')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('macro-f'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('expanded'), true]]]], [Symbol.for('values'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], [Symbol.for('make-rose'), Symbol.for('result'), Symbol.for('node')], Symbol.for('result')], Symbol.for('expanded')]];

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

macroexpandN.fsource = [Symbol.for('define'), [Symbol.for('macroexpand-n'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('n'), 1]], [Symbol.for('define'), Symbol.for('i'), Symbol.for('n')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('expanded'), false], [Symbol.for('define'), Symbol.for('expanded1'), true], [Symbol.for('while'), [Symbol.for('and'), Symbol.for('expanded1'), [Symbol.for('>'), Symbol.for('i'), 0]], [Symbol.for('set!-values'), [Symbol.for('result'), Symbol.for('expanded1')], [Symbol.for('macroexpand-1'), Symbol.for('result'), Symbol.for('env')]], [Symbol.for('set!'), Symbol.for('expanded'), [Symbol.for('or'), Symbol.for('expanded'), Symbol.for('expanded1')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('values'), Symbol.for('result'), Symbol.for('expanded')]];

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

macroexpandUntil.fsource = [Symbol.for('define'), [Symbol.for('macroexpand-until'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('pred')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('exp')], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('macro-call?'), Symbol.for('result'), Symbol.for('env')], [Symbol.for('pred'), Symbol.for('result')]], [Symbol.for('set!-values'), [Symbol.for('result')], [Symbol.for('macroexpand-1'), Symbol.for('result'), Symbol.for('env')]]], Symbol.for('result')];

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

macroexpandAll.fsource = [Symbol.for('define'), [Symbol.for('macroexpand-all'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('macroexpand-all-until'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('const'), true]]];

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
      return (macroTypeP(bType) || undefinedTypeP(bType)) && predF(x);
    }
    predF1.fsource = [Symbol.for('define'), [Symbol.for('pred-f-1'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define-values'), Symbol.for('b-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')]], [Symbol.for('and'), [Symbol.for('or'), [Symbol.for('macro-type?'), Symbol.for('b-type')], [Symbol.for('undefined-type?'), Symbol.for('b-type')]], [Symbol.for('pred-f'), Symbol.for('x')]]];
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
  f.fsource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('pred-f'), [Symbol.for('or'), Symbol.for('pred'), [Symbol.for('const'), true]]], [Symbol.for('define'), [Symbol.for('pred-f-1'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define-values'), Symbol.for('b-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')]], [Symbol.for('and'), [Symbol.for('or'), [Symbol.for('macro-type?'), Symbol.for('b-type')], [Symbol.for('undefined-type?'), Symbol.for('b-type')]], [Symbol.for('pred-f'), Symbol.for('x')]]], [Symbol.for('cond'), [[Symbol.for('macro-call?'), Symbol.for('x'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('macroexpand-until'), Symbol.for('x'), Symbol.for('env'), Symbol.for('pred-f-1')]], [Symbol.for('unless'), [Symbol.for('macro-call?'), Symbol.for('result'), Symbol.for('env')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('result'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]], Symbol.for('result')], [Symbol.for('else'), Symbol.for('x')]]];
  return mapSexp(f, exp, env, stack, bindings);
}

macroexpandAllUntil.fsource = [Symbol.for('define'), [Symbol.for('macroexpand-all-until'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('pred'), undefined], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('pred-f'), [Symbol.for('or'), Symbol.for('pred'), [Symbol.for('const'), true]]], [Symbol.for('define'), [Symbol.for('pred-f-1'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('x')]], [Symbol.for('define-values'), Symbol.for('b-type'), [Symbol.for('send'), Symbol.for('bindings'), Symbol.for('get-type'), Symbol.for('op')]], [Symbol.for('and'), [Symbol.for('or'), [Symbol.for('macro-type?'), Symbol.for('b-type')], [Symbol.for('undefined-type?'), Symbol.for('b-type')]], [Symbol.for('pred-f'), Symbol.for('x')]]], [Symbol.for('cond'), [[Symbol.for('macro-call?'), Symbol.for('x'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('macroexpand-until'), Symbol.for('x'), Symbol.for('env'), Symbol.for('pred-f-1')]], [Symbol.for('unless'), [Symbol.for('macro-call?'), Symbol.for('result'), Symbol.for('env')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('result'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]], Symbol.for('result')], [Symbol.for('else'), Symbol.for('x')]]], [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]];

/**
 * Macroexpand all compiler macros.
 * This expands regular macros as well.
 */
function macroexpandCompilerMacros(exp: any, env: any): any {
  const compilerMacroEnv: any = makeMacroEnvironment(env);
  let result: any = macroexpandAll(exp, compilerMacroEnv);
  return result;
}

macroexpandCompilerMacros.fsource = [Symbol.for('define'), [Symbol.for('macroexpand-compiler-macros'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('compiler-macro-env'), [Symbol.for('make-macro-environment'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('macroexpand-all'), Symbol.for('exp'), Symbol.for('compiler-macro-env')]], Symbol.for('result')];

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

compileDot.fsource = [Symbol.for('define'), [Symbol.for('compile-dot'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\.(.*)$'], [Symbol.for('symbol->string'), [Symbol.for('first'), Symbol.for('exp')]]]], [Symbol.for('define'), Symbol.for('method'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('method'), ''], [Symbol.for('cond'), [[Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^-(.*)$'], [Symbol.for('symbol->string'), [Symbol.for('third'), Symbol.for('exp')]]]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('define'), Symbol.for('field-sym'), [Symbol.for('string->symbol'), Symbol.for('field')]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('compile-get-field'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('field-sym')], [Symbol.for('unquote'), Symbol.for('obj')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-send'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('cond'), [[Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^-(.*)$'], Symbol.for('method')]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('second'), Symbol.for('match')]], [Symbol.for('compile-get-field'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('field')]], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-send'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('obj')], [Symbol.for('unquote'), [Symbol.for('string->symbol'), Symbol.for('method')]], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]]]]];

/**
 * Compile a `(get-field ...)` expression.
 */
function compileGetField(node: any, env: any, options: any = {}): any {
  const field: any = node.get(1);
  const obj: any = node.get(2);
  const computed: any = typeof field.getValue() !== 'symbol';
  return makeExpressionOrStatement(new MemberExpression((typeof obj.getValue() === 'symbol') ? compileSymbol(obj, env, makeExpressionOptions(options)) : compileExpression(obj, env, options), computed ? compileExpression(field, env, options) : compileSymbol(field, env, options), computed), options);
}

compileGetField.fsource = [Symbol.for('define'), [Symbol.for('compile-get-field'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('not'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('field'), Symbol.for('get-value')]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('MemberExpression'), [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('obj'), Symbol.for('get-value')]], [Symbol.for('compile-symbol'), Symbol.for('obj'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('obj'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('if'), Symbol.for('computed'), [Symbol.for('compile-expression'), Symbol.for('field'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-symbol'), Symbol.for('field'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('computed')], Symbol.for('options')]];

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

compileJsOptionalChaining.fsource = [Symbol.for('define'), [Symbol.for('compile-js-optional-chaining'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('field-exp'), [Symbol.for('send'), Symbol.for('field'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('field-exp')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('obj')], [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('field'), Symbol.for('drop'), 0]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('optional'), Symbol.for('result'), true], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('field')], [Symbol.for('unquote'), Symbol.for('obj')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('optional'), Symbol.for('result'), true], Symbol.for('result')]]];

/**
 * Compile a `(set-field! ...)` expression.
 */
function compileSetField(node: any, env: any, options: any = {}): any {
  const field: any = node.get(1);
  const obj: any = node.get(2);
  let val: any = node.get(3);
  return compileRose(makeRose([Symbol.for('set!'), [Symbol.for('get-field'), field, obj], val], node), env, options);
}

compileSetField.fsource = [Symbol.for('define'), [Symbol.for('compile-set-field'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('field'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 3]], [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('get-field'), [Symbol.for('unquote'), Symbol.for('field')], [Symbol.for('unquote'), Symbol.for('obj')]], [Symbol.for('unquote'), Symbol.for('val')]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a `(modulo ...)` expression.
 */
function compileModulo(node: any, env: any, options: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    identity: 1,
    operator: '%'
  });
}

compileModulo.fsource = [Symbol.for('define'), [Symbol.for('compile-modulo'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 1, 'operator', '%']]];

/**
 * Compile a `(* ...)` expression.
 */
function compileMul(node: any, env: any, options: any = {}): any {
  return compileBinaryExpression(node, env, options, {
    identity: 1,
    operator: '*'
  });
}

compileMul.fsource = [Symbol.for('define'), [Symbol.for('compile-mul'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 1, 'operator', '*']]];

/**
 * "NO-OP" compilation operation.
 * Creates an empty program fragment and does nothing else.
 */
function compileNop(node: any, env: any, options: any = {}): any {
  return makeProgramFragment();
}

compileNop.fsource = [Symbol.for('define'), [Symbol.for('compile-nop'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-program-fragment')]];

/**
 * Compile a `(not ...)` expression.
 */
function compileNot(node: any, env: any, options: any = {}): any {
  function isNotExpressionP(x: any): any {
    return estreeTypeP(x, 'UnaryExpression') && (x.operator === '!');
  }
  isNotExpressionP.fsource = [Symbol.for('define'), [Symbol.for('is-not-expression?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('x'), 'UnaryExpression'], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('x')], '!']]];
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

compileNot.fsource = [Symbol.for('define'), [Symbol.for('compile-not'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), [Symbol.for('is-not-expression?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('x'), 'UnaryExpression'], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('x')], '!']]], [Symbol.for('define'), Symbol.for('operand'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('operand-compiled'), [Symbol.for('compile-expression'), Symbol.for('operand'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result'), undefined], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('operand-compiled'), 'BinaryExpression'], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('operand-compiled')], '===']], [Symbol.for('set-field!'), Symbol.for('operator'), Symbol.for('operand-compiled'), '!=='], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('operand-compiled')]], [[Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('operand-compiled'), 'BinaryExpression'], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('operand-compiled')], '==']], [Symbol.for('set-field!'), Symbol.for('operator'), Symbol.for('operand-compiled'), '!='], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('operand-compiled')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('not-expression'), [Symbol.for('new'), Symbol.for('UnaryExpression'), '!', true, Symbol.for('operand-compiled')]], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('is-not-expression?'), Symbol.for('not-expression')], [Symbol.for('is-not-expression?'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('not-expression')]]], [Symbol.for('set!'), Symbol.for('not-expression'), [Symbol.for('~>'), Symbol.for('not-expression'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('_')], [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('_')]]]], [Symbol.for('set!'), Symbol.for('result'), Symbol.for('not-expression')]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];

/**
 * Compile a `(begin ...)` expression.
 */
function compileBegin(node: any, env: any, options: any = {}): any {
  const languageEnv: any = options['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
  const expressionType: any = options['expressionType'];
  let exp: any = node.getValue();
  const body: any = node.drop(1);
  const compiledBody: any = [];
  // Add defined variables to environment. We have to
  // handle them here since they may refer to each other.
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
      makeTypeBinding(env, sym, Symbol.for('Any'), langFilter);
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
      makeTypeBinding(env, sym, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')], langFilter);
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
      makeTypeBinding(env, sym, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')], langFilter);
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

compileBegin.fsource = [Symbol.for('define'), [Symbol.for('compile-begin'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('compiled-body'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('body')]]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), [Symbol.for('aget'), Symbol.for('body'), Symbol.for('i')], Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('form?'), Symbol.for('exp'), Symbol.for('define_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('if'), [Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('sym'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')]], [[Symbol.for('form?'), Symbol.for('exp'), Symbol.for('define-macro_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], Symbol.for('lang-filter')]], [[Symbol.for('form?'), Symbol.for('exp'), Symbol.for('defmacro_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], Symbol.for('lang-filter')]]]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('body-statements'), [Symbol.for('compile-statements'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('make-program-fragment'), Symbol.for('body-statements')]], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('exp')], Symbol.for('env'), Symbol.for('options')]]]]]];

/**
 * Compile a `(js/block ...)` expression.
 */
function compileJsBlock(node: any, env: any, options: any = {}): any {
  const expressionType: any = options['expressionType'];
  if ((expressionType === 'statement') || (expressionType === 'return')) {
    return wrapInBlockStatement(compileBegin(node, env, options));
  } else {
    return compileBegin(node, env, options);
  }
}

compileJsBlock.fsource = [Symbol.for('define'), [Symbol.for('compile-js-block'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-begin'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('compile-begin'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Make and compile a `(require ...)` or `(define-values ...)` form
 * that defines referenced values from the language environment.
 * `symbols` is a list of symbols bound in the language environment.
 */
function buildGlobalEnvironment(symbols: any, env: any, options: any = {}): any {
  let exp: any = makeGlobalEnvironmentExp(symbols, env, options);
  return compileGlobalEnvironment(exp, env, options);
}

buildGlobalEnvironment.fsource = [Symbol.for('define'), [Symbol.for('build-global-environment'), Symbol.for('symbols'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('make-global-environment-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('compile-global-environment'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]];

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

makeGlobalEnvironmentExp.fsource = [Symbol.for('define'), [Symbol.for('make-global-environment-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('symbols')], 0], false], [[Symbol.for('oget'), Symbol.for('options'), 'inlineFunctions'], [Symbol.for('make-define-values-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('make-require-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')]]]];

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
          const env1: any = env.clone();
          const compiledExpression: any = compileRose(makeRose(exp), env1, {
            ...options,
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
        } else if (typeof value === 'symbol') {
          const str: any = value.description as string;
          internalSymbol = symbol;
          exp = [Symbol.for('define'), internalSymbol, [Symbol.for('send'), Symbol.for('Symbol'), Symbol.for('for'), str]];
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

makeDefineValuesExp.fsource = [Symbol.for('define'), [Symbol.for('make-define-values-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('inline-functions-option'), [Symbol.for('oget'), Symbol.for('options'), 'inlineFunctions']], [Symbol.for('define'), Symbol.for('definitions'), false], [Symbol.for('define'), Symbol.for('define-forms'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('internal-symbols'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('external-symbols'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('referenced-symbols'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('symbols')]]]], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('new'), Symbol.for('Module')]], [Symbol.for('define'), Symbol.for('seen'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('internal-symbol')], [Symbol.for('define'), Symbol.for('symbol')], [Symbol.for('define'), Symbol.for('value')], [Symbol.for('while'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('referenced-symbols')], 0], [Symbol.for('set!'), Symbol.for('symbol'), [Symbol.for('pop!'), Symbol.for('referenced-symbols')]], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('symbol')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('memq?'), Symbol.for('symbol'), Symbol.for('external-symbols')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('symbol')]], [Symbol.for('set!'), Symbol.for('value'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('symbol')]], [Symbol.for('cond'), [[Symbol.for('source?'), Symbol.for('value')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('source'), Symbol.for('value')]], [Symbol.for('when'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define')]], [Symbol.for('set!'), Symbol.for('internal-symbol'), [Symbol.for('if'), [Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('array-first'), [Symbol.for('array-second'), Symbol.for('exp')]], [Symbol.for('array-second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('referenced-symbols-1'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('env1'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('clone')]], [Symbol.for('define'), Symbol.for('compiled-expression'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), Symbol.for('exp')], Symbol.for('env1'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'currentModule', Symbol.for('current-module'), 'referencedSymbols', Symbol.for('referenced-symbols-1')]]]], [Symbol.for('for'), [[Symbol.for('symbol-1'), Symbol.for('referenced-symbols-1')]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('memq?'), Symbol.for('symbol-1'), Symbol.for('seen')], [Symbol.for('memq?'), Symbol.for('symbol-1'), Symbol.for('referenced-symbols')]], [Symbol.for('push-right!'), Symbol.for('referenced-symbols'), Symbol.for('symbol-1')]]]]], [Symbol.for('else'), [Symbol.for('cond'), [[Symbol.for('procedure?'), Symbol.for('value')], [Symbol.for('define'), Symbol.for('js-string'), [Symbol.for('string-append'), Symbol.for('value'), '']], [Symbol.for('define'), Symbol.for('match')], [Symbol.for('set!'), Symbol.for('match'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^function ([^( ]+)'], Symbol.for('js-string')]], [Symbol.for('cond'), [Symbol.for('match'), [Symbol.for('set!'), Symbol.for('internal-symbol'), [Symbol.for('string->symbol'), [Symbol.for('second'), Symbol.for('match')]]], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quasiquote'), [Symbol.for('js'), [Symbol.for('unquote'), Symbol.for('js-string')]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('internal-symbol'), Symbol.for('symbol')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('internal-symbol')], [Symbol.for('js'), [Symbol.for('unquote'), Symbol.for('js-string')]]]]]]]], [[Symbol.for('js-obj?'), Symbol.for('value')], [Symbol.for('define'), Symbol.for('js-string'), [Symbol.for('send'), Symbol.for('JSON'), Symbol.for('stringify'), Symbol.for('value'), null, 2]], [Symbol.for('set!'), Symbol.for('internal-symbol'), Symbol.for('symbol')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('internal-symbol')], [Symbol.for('js'), [Symbol.for('unquote'), Symbol.for('js-string')]]]]]], [[Symbol.for('symbol?'), Symbol.for('value')], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('symbol->string'), Symbol.for('value')]], [Symbol.for('set!'), Symbol.for('internal-symbol'), Symbol.for('symbol')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('internal-symbol')], [Symbol.for('send'), Symbol.for('Symbol'), Symbol.for('for'), [Symbol.for('unquote'), Symbol.for('str')]]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('js-string'), [Symbol.for('string-append'), Symbol.for('value'), '']], [Symbol.for('set!'), Symbol.for('internal-symbol'), Symbol.for('symbol')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('internal-symbol')], [Symbol.for('js'), [Symbol.for('unquote'), Symbol.for('js-string')]]]]]]]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('internal-symbol'), Symbol.for('internal-symbols')], [Symbol.for('push-right!'), Symbol.for('define-forms'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('memq?'), Symbol.for('symbol'), Symbol.for('symbols')], [Symbol.for('push-right!'), Symbol.for('internal-symbols'), Symbol.for('internal-symbol')], [Symbol.for('push-right!'), Symbol.for('external-symbols'), Symbol.for('symbol')]]]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('external-symbols')], 0], [Symbol.for('set!'), Symbol.for('definitions'), [Symbol.for('quasiquote'), [Symbol.for('define-values'), [Symbol.for('unquote'), Symbol.for('external-symbols')], [[Symbol.for('js/arrow'), [], [Symbol.for('unquote-splicing'), Symbol.for('define-forms')], [Symbol.for('values'), [Symbol.for('unquote-splicing'), Symbol.for('internal-symbols')]]]]]]]], Symbol.for('definitions')];

/**
 * Make a `(require ...)` form for the global environment.
 */
function makeRequireExp(symbols: any, env: any, options: any): any {
  return [Symbol.for('require'), [Symbol.for('only-in'), packageName, ...symbols]];
}

makeRequireExp.fsource = [Symbol.for('define'), [Symbol.for('make-require-exp'), Symbol.for('symbols'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('quasiquote'), [Symbol.for('require'), [Symbol.for('only-in'), [Symbol.for('unquote'), Symbol.for('package-name')], [Symbol.for('unquote-splicing'), Symbol.for('symbols')]]]]];

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
      continuationEnvironment: new LispEnvironment(),
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

compileGlobalEnvironment.fsource = [Symbol.for('define'), [Symbol.for('compile-global-environment'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('exp')], [Symbol.for('make-program-fragment')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define-values')]], [Symbol.for('define'), Symbol.for('define-values-form'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('list')]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('aget'), Symbol.for('exp'), 2]], [Symbol.for('define'), Symbol.for('body-compiled'), [Symbol.for('compile-sexp'), Symbol.for('body'), Symbol.for('env'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'continuationEnvironment', [Symbol.for('new'), Symbol.for('LispEnvironment')], 'expressionType', 'expression']]]], [Symbol.for('define'), Symbol.for('var-decl'), [Symbol.for('compile-sexp'), Symbol.for('define-values-form'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('set-field!'), Symbol.for('init'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('var-decl')]], Symbol.for('body-compiled')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('make-program-fragment'), [Symbol.for('list'), Symbol.for('var-decl')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('make-program-fragment'), [Symbol.for('list'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]]]]]];

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

makeInlinedValue.fsource = [Symbol.for('define'), [Symbol.for('make-inlined-value'), Symbol.for('symbol'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('global-environment-exp'), [Symbol.for('make-global-environment-exp'), [Symbol.for('list'), Symbol.for('symbol')], Symbol.for('env'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'inlineFunctions', true]]]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('global-environment-exp')], 1], [Symbol.for('define'), Symbol.for('lambda-call'), [Symbol.for('aget'), Symbol.for('global-environment-exp'), 2]], [Symbol.for('define'), Symbol.for('lambda-exp'), [Symbol.for('aget'), Symbol.for('lambda-call'), 0]], [Symbol.for('define'), Symbol.for('values-exp'), [Symbol.for('array-list-last'), Symbol.for('lambda-exp')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('second'), Symbol.for('values-exp')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lambda-call')], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('lambda-exp')], 4], [Symbol.for('symbol?'), [Symbol.for('second'), [Symbol.for('third'), Symbol.for('lambda-exp')]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('third'), [Symbol.for('third'), Symbol.for('lambda-exp')]]]], [Symbol.for('else'), [Symbol.for('list-set!'), Symbol.for('lambda-exp'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('lambda-exp')], 1], Symbol.for('sym')]]], Symbol.for('result')], [Symbol.for('else'), Symbol.for('global-environment-exp')]]];

/**
 * Compile an `(or ...)` expression.
 */
function compileOr(node: any, env: any, options: any = {}): any {
  return compileLogicalExpression(node, env, options, {
    identity: false,
    operator: '||'
  });
}

compileOr.fsource = [Symbol.for('define'), [Symbol.for('compile-or'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-logical-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', false, 'operator', '||']]];

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

compileProvide.fsource = [Symbol.for('define'), [Symbol.for('compile-provide'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expressions'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('all-from-out-expressions'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('other-expressions'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('expressions')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')], [Symbol.for('quote'), Symbol.for('all-from-out')]], [Symbol.for('push-right!'), Symbol.for('all-from-out-expressions'), Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('other-expressions'), Symbol.for('x')]]]], [Symbol.for('define'), Symbol.for('results'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('all-from-out-expressions')]], [Symbol.for('define'), Symbol.for('source'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('ExportAllDeclaration'), [Symbol.for('compile-expression'), Symbol.for('source'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('push-right!'), Symbol.for('results'), Symbol.for('result')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('other-expressions')], 0], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('seen'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('other-expressions')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('rename-out')]], [Symbol.for('for'), [[Symbol.for('pair'), [Symbol.for('rest'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('first'), Symbol.for('pair')]], [Symbol.for('define'), Symbol.for('x2'), [Symbol.for('second'), Symbol.for('pair')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', true]], Symbol.for('options')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x2')], [Symbol.for('set!'), Symbol.for('x2'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x2')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', true]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x2'), Symbol.for('seen')], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x2')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ExportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x2')]]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), Symbol.for('exp')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', true]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x1'), Symbol.for('seen')], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x1')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ExportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1')]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('ExportNamedDeclaration'), null, Symbol.for('specifiers')]], [Symbol.for('push-right!'), Symbol.for('results'), Symbol.for('result')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('results')], 1], [Symbol.for('first'), Symbol.for('results')]], [Symbol.for('else'), [Symbol.for('make-program-fragment'), Symbol.for('results')]]]];

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

compileQuote.fsource = [Symbol.for('define'), [Symbol.for('compile-quote'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('cond'), [[Symbol.for('array?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('list'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), [Symbol.for('second'), Symbol.for('exp')], Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('quasiquote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('x')]]]]]]]]], Symbol.for('env'), Symbol.for('options')]]], [[Symbol.for('symbol?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-symbol'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'quotedSymbol', true]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];

/**
 * Compile a `(quasiquote ...)` expression.
 */
function compileQuasiquote(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(compileQuasiquoteHelper(node.get(1), env, options), options);
}

compileQuasiquote.fsource = [Symbol.for('define'), [Symbol.for('compile-quasiquote'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-quasiquote-helper'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]];

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
        return new SpreadElement(compileExpression(x.get(1), env, options));
      } else {
        return compileQuasiquoteHelper(x, env, options);
      }
    }));
  }
}

compileQuasiquoteHelper.fsource = [Symbol.for('define'), [Symbol.for('compile-quasiquote-helper'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('array?'), Symbol.for('exp')]], [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ArrayExpression'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('quasiquote')]], [Symbol.for('compile-quote'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('unquote')]], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]], [Symbol.for('new'), Symbol.for('SpreadElement'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('else'), [Symbol.for('compile-quasiquote-helper'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-nodes')]]]]]];

/**
 * Compile a `(require ...)` expression.
 */
function compileRequire(node: any, env: any, options: any = {}): any {
  const esModuleInterop: any = options['esModuleInterop'];
  const languageEnv: any = options['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
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
        let x1Str: any = x1;
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
        let x2Str: any = x2;
        if (typeof x1 === 'symbol') {
          x1Str = printEstree(compileSymbol(makeRose(x1), env, options, {
            literalSymbol: true
          }), options);
        }
        if (typeof x2 === 'symbol') {
          x2Str = printEstree(compileSymbol(makeRose(x2), env, options, {
            literalSymbol: true
          }), options);
        }
        if (!seen.includes(x2Str)) {
          if (!env.has(x2, {
            filter: langFilter
          })) {
            makeTypeBinding(env, x2, Symbol.for('Any'), langFilter);
          }
          seen.push(x2);
          specifiers.push(new ImportSpecifier(new Identifier(x1Str), new Identifier(x2Str)));
        }
      } else {
        let x1: any = exp;
        let x1Str: any = x1;
        if (typeof x1 === 'symbol') {
          x1Str = printEstree(compileSymbol(makeRose(x1), env, options, {
            literalSymbol: true
          }), options);
        }
        if (!seen.includes(x1Str)) {
          if (!env.has(x1, {
            filter: langFilter
          })) {
            makeTypeBinding(env, x1, Symbol.for('Any'), langFilter);
          }
          seen.push(x1Str);
          specifiers.push(new ImportSpecifier(new Identifier(x1Str)));
        }
      }
    }
    yExp = (Array.isArray(xExp) && (xExp.length >= 3) && (xExp[xExp.length - 2] === Symbol.for('.')) && ((): any => {
      const x3: any = lastCdr(xExp);
      return Array.isArray(x3) && (x3.length === 0);
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
  if (typeof xExp === 'symbol') {
    if (!env.has(xExp, {
      filter: langFilter
    })) {
      makeTypeBinding(env, xExp, Symbol.for('Any'), langFilter);
    }
  }
  if (Array.isArray(specifiers) && (specifiers.length === 0)) {
    return emptyProgram();
  } else {
    return new ImportDeclaration(specifiers, src);
  }
}

compileRequire.fsource = [Symbol.for('define'), [Symbol.for('compile-require'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('es-module-interop'), [Symbol.for('oget'), Symbol.for('options'), 'esModuleInterop']], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('x-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('x-exp'), [Symbol.for('send'), Symbol.for('x-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('y-node'), [Symbol.for('or'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('x-node')]], [Symbol.for('define'), Symbol.for('y-exp'), [Symbol.for('send'), Symbol.for('y-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('seen'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('src'), null], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('x-exp'), [Symbol.for('quote'), Symbol.for('only-in')]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('x-node'), Symbol.for('drop'), 2]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('first'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('x1-str'), Symbol.for('x1')], [Symbol.for('define'), Symbol.for('x2'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('x2-str'), Symbol.for('x2')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1-str'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', true]], Symbol.for('options')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x2')], [Symbol.for('set!'), Symbol.for('x2-str'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x2')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', true]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x2-str'), Symbol.for('seen')], [Symbol.for('unless'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('x2'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('x2'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')]], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x2')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ImportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1-str')], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x2-str')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), Symbol.for('exp')], [Symbol.for('define'), Symbol.for('x1-str'), Symbol.for('x1')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('x1-str'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x1')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', true]], Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('x1-str'), Symbol.for('seen')], [Symbol.for('unless'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('x1'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('x1'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')]], [Symbol.for('push-right!'), Symbol.for('seen'), Symbol.for('x1-str')], [Symbol.for('push-right!'), Symbol.for('specifiers'), [Symbol.for('new'), Symbol.for('ImportSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x1-str')]]]]]]], [Symbol.for('set!'), Symbol.for('y-exp'), [Symbol.for('second'), Symbol.for('x-exp')]]], [Symbol.for('else'), [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x-exp')], [Symbol.for('set!'), Symbol.for('x-exp'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('x-exp')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', true]], Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('specifiers'), [Symbol.for('list'), [Symbol.for('if'), Symbol.for('es-module-interop'), [Symbol.for('new'), Symbol.for('ImportDefaultSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x-exp')]], [Symbol.for('new'), Symbol.for('ImportNamespaceSpecifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('x-exp')]]]]]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('y-exp')], [Symbol.for('set!'), Symbol.for('y-exp'), [Symbol.for('print-estree'), [Symbol.for('compile-symbol'), [Symbol.for('make-rose'), Symbol.for('y-exp')], Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'literalSymbol', true]], Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('src'), [Symbol.for('new'), Symbol.for('Literal'), Symbol.for('y-exp')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('x-exp')], [Symbol.for('unless'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('x-exp'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('x-exp'), [Symbol.for('quote'), Symbol.for('Any')], Symbol.for('lang-filter')]]], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('specifiers')], [Symbol.for('empty-program')]], [Symbol.for('else'), [Symbol.for('new'), Symbol.for('ImportDeclaration'), Symbol.for('specifiers'), Symbol.for('src')]]]];

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

compileSet.fsource = [Symbol.for('define'), [Symbol.for('compile-set'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('define'), Symbol.for('sym-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('sym-exp'), [Symbol.for('send'), Symbol.for('sym-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('val-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('val-exp'), [Symbol.for('send'), Symbol.for('val-node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('add_'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], 1]], [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], 1]]]], [Symbol.for('set!'), Symbol.for('val-exp'), [Symbol.for('quasiquote'), [Symbol.for('add1'), [Symbol.for('unquote'), Symbol.for('sym-exp')]]]], [Symbol.for('set!'), Symbol.for('val-node'), [Symbol.for('make-rose'), Symbol.for('val-exp')]]], [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('sub_'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], 1]], [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('third'), Symbol.for('val-exp')], Symbol.for('sym-exp')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], 1]]]], [Symbol.for('set!'), Symbol.for('val-exp'), [Symbol.for('quasiquote'), [Symbol.for('sub1'), [Symbol.for('unquote'), Symbol.for('sym-exp')]]]], [Symbol.for('set!'), Symbol.for('val-node'), [Symbol.for('make-rose'), Symbol.for('val-exp')]]]], [Symbol.for('define'), Symbol.for('result'), ''], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('add1_'), Symbol.for('env')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('UpdateExpression'), '++', [Symbol.for('compile-expression'), Symbol.for('sym-node'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement']]]]]], [[Symbol.for('and'), [Symbol.for('form?'), Symbol.for('val-exp'), Symbol.for('sub1_'), Symbol.for('env')], [Symbol.for('eq?'), [Symbol.for('second'), Symbol.for('val-exp')], Symbol.for('sym-exp')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('UpdateExpression'), '--', [Symbol.for('compile-expression'), Symbol.for('sym-node'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement']]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('AssignmentExpression'), '=', [Symbol.for('if'), [Symbol.for('symbol?'), [Symbol.for('send'), Symbol.for('sym-node'), Symbol.for('get-value')]], [Symbol.for('compile-symbol'), Symbol.for('sym-node'), Symbol.for('env'), [Symbol.for('make-expression-options'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('sym-node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('compile-expression'), Symbol.for('val-node'), Symbol.for('env'), Symbol.for('options')]]]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];

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

compileString.fsource = [Symbol.for('define'), [Symbol.for('compile-string'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n'], Symbol.for('str')], [Symbol.for('define'), Symbol.for('lines'), [Symbol.for('string-split'), Symbol.for('str'), [Symbol.for('regexp'), '^', 'gm']]], [Symbol.for('cond'), [[Symbol.for('<='), [Symbol.for('array-list-length'), Symbol.for('lines')], 1], [Symbol.for('compile-atom'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-rose'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('string-append'), [Symbol.for('unquote-splicing'), Symbol.for('lines')]]], Symbol.for('node')]], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('compile-atom'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];

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

compileSub.fsource = [Symbol.for('define'), [Symbol.for('compile-sub'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('define'), Symbol.for('num'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('num-compiled'), [Symbol.for('compile-expression'), Symbol.for('num'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('UnaryExpression'), '-', true, Symbol.for('num-compiled')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', 0, 'operator', '-']]]]];

/**
 * Compile a variable expression.
 */
function compileVariable(node: any, env: any, options: any = {}): any {
  const compilationMappingEnvironment: any = options['compilationMappingEnvironment'];
  const literalSymbol: any = options['literalSymbol'];
  const quotedSymbol: any = options['quotedSymbol'];
  const currentModule: any = options['currentModule'];
  let exp: any = node.getValue();
  if (!(quotedSymbol || literalSymbol || (env.has(exp) && variableTypeP(compilationMappingEnvironment.getType(env.get(exp)))))) {
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

compileVariable.fsource = [Symbol.for('define'), [Symbol.for('compile-variable'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('compilation-mapping-environment'), [Symbol.for('oget'), Symbol.for('options'), 'compilationMappingEnvironment']], [Symbol.for('define'), Symbol.for('literal-symbol'), [Symbol.for('oget'), Symbol.for('options'), 'literalSymbol']], [Symbol.for('define'), Symbol.for('quoted-symbol'), [Symbol.for('oget'), Symbol.for('options'), 'quotedSymbol']], [Symbol.for('define'), Symbol.for('current-module'), [Symbol.for('oget'), Symbol.for('options'), 'currentModule']], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('unless'), [Symbol.for('or'), Symbol.for('quoted-symbol'), Symbol.for('literal-symbol'), [Symbol.for('and'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('exp')], [Symbol.for('variable-type?'), [Symbol.for('send'), Symbol.for('compilation-mapping-environment'), Symbol.for('get-type'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('get'), Symbol.for('exp')]]]]], [Symbol.for('when'), [Symbol.for('should-inline?'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('cond'), [Symbol.for('current-module'), [Symbol.for('add-referenced-symbol'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('return'), [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('make-inlined-value'), Symbol.for('exp'), Symbol.for('env'), Symbol.for('options')]], Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]]]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('compile-symbol'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')], Symbol.for('options')]];

/**
 * Compile a symbol expression.
 */
function compileSymbol(node: any, env: any, options: any = {}, settings: any = {}): any {
  // TODO: Better handling of gensym'ed symbols.
  const literalSymbolOption: any = settings['literalSymbol'] || false;
  let quotedSymbolOption: any = settings['quotedSymbol'];
  const compileEnvironmentOption: any = options['compileEnvironment'];
  const camelCaseOption: any = options['camelCase'];
  const languageEnv: any = options['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
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
      let identifier: any = new Identifier(gensymName);
      return identifier;
    } else {
      let name: any = makeJsIdentifierString(str, options);
      let gensymName: any = name;
      let i: any = 1;
      let regularSym: any = Symbol.for(gensymName);
      while (env.has(regularSym, {
        filter: langFilter
      })) {
        gensymName = name + i + '';
        regularSym = Symbol.for(gensymName);
        i++;
      }
      let identifier: any = new Identifier(gensymName);
      const entry: any = [gensymName, name, i];
      gensymMap.set(exp, entry);
      env.setLocal(regularSym, undefined, Symbol.for('Any'));
      return identifier;
    }
  } else {
    let name: any = makeJsIdentifierString(str, options);
    return new Identifier(name);
  }
}

compileSymbol.fsource = [Symbol.for('define'), [Symbol.for('compile-symbol'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('literal-symbol-option'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('settings'), 'literalSymbol'], false]], [Symbol.for('define'), Symbol.for('quoted-symbol-option'), [Symbol.for('oget'), Symbol.for('settings'), 'quotedSymbol']], [Symbol.for('define'), Symbol.for('compile-environment-option'), [Symbol.for('oget'), Symbol.for('options'), 'compileEnvironment']], [Symbol.for('define'), Symbol.for('camel-case-option'), [Symbol.for('oget'), Symbol.for('options'), 'camelCase']], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('gensymed-symbol'), [Symbol.for('gensym?'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('symbol->string'), Symbol.for('exp')]], [Symbol.for('when'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^:'], Symbol.for('str')], [Symbol.for('set!'), Symbol.for('quoted-symbol-option'), true]], [Symbol.for('cond'), [Symbol.for('quoted-symbol-option'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('string->symbol'), [Symbol.for('unquote'), Symbol.for('str')]]]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('literal-symbol-option'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('name')]], [[Symbol.for('send'), Symbol.for('compilation-variables-env'), Symbol.for('has'), Symbol.for('exp')], [Symbol.for('send'), Symbol.for('compilation-variables-env'), Symbol.for('get'), Symbol.for('exp')]], [[Symbol.for('eq?'), Symbol.for('str'), 'this'], [Symbol.for('new'), Symbol.for('ThisExpression')]], [Symbol.for('gensymed-symbol'), [Symbol.for('define'), Symbol.for('gensym-map'), [Symbol.for('oget'), Symbol.for('options'), 'gensymMap']], [Symbol.for('unless'), Symbol.for('gensym-map'), [Symbol.for('set!'), Symbol.for('gensym-map'), [Symbol.for('make-hash')]], [Symbol.for('oset!'), Symbol.for('options'), 'gensymMap', Symbol.for('gensym-map')]], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('gensym-map'), Symbol.for('exp')], [Symbol.for('define-values'), [Symbol.for('gensym-name'), Symbol.for('name'), Symbol.for('i')], [Symbol.for('hash-ref'), Symbol.for('gensym-map'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('gensym-name')]], Symbol.for('identifier')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('gensym-name'), Symbol.for('name')], [Symbol.for('define'), Symbol.for('i'), 1], [Symbol.for('define'), Symbol.for('regular-sym'), [Symbol.for('string->symbol'), Symbol.for('gensym-name')]], [Symbol.for('while'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('regular-sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('lang-filter')]], [Symbol.for('set!'), Symbol.for('gensym-name'), [Symbol.for('string-append'), Symbol.for('name'), [Symbol.for('number->string'), Symbol.for('i')]]], [Symbol.for('set!'), Symbol.for('regular-sym'), [Symbol.for('string->symbol'), Symbol.for('gensym-name')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]]], [Symbol.for('define'), Symbol.for('identifier'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('gensym-name')]], [Symbol.for('define'), Symbol.for('entry'), [Symbol.for('list'), Symbol.for('gensym-name'), Symbol.for('name'), Symbol.for('i')]], [Symbol.for('hash-set!'), Symbol.for('gensym-map'), Symbol.for('exp'), Symbol.for('entry')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('regular-sym'), undefined, [Symbol.for('quote'), Symbol.for('Any')]], Symbol.for('identifier')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('name'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), Symbol.for('options')]], [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('name')]]]];

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

makeJsIdentifierString.fsource = [Symbol.for('define'), [Symbol.for('make-js-identifier-string'), Symbol.for('str'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('compile-environment-option'), [Symbol.for('oget'), Symbol.for('options'), 'compileEnvironment']], [Symbol.for('unless'), Symbol.for('compile-environment-option'), [Symbol.for('return'), Symbol.for('str')]], [Symbol.for('define'), Symbol.for('camel-case-option'), [Symbol.for('oget'), Symbol.for('options'), 'camelCase']], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('make-js-identifier-string-helper'), Symbol.for('str')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('camel-case-option'), false], [Symbol.for('kebab-case->snake-case'), Symbol.for('result')]], [Symbol.for('else'), [Symbol.for('kebab-case->camel-case'), Symbol.for('result')]]]];

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

makeJsIdentifierStringHelper.fsource = [Symbol.for('define'), [Symbol.for('make-js-identifier-string-helper'), Symbol.for('str')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\+$', 'g'], Symbol.for('_'), '_add'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^-$', 'g'], Symbol.for('_'), '_sub'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\*$', 'g'], Symbol.for('_'), '_mul'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^/$', 'g'], Symbol.for('_'), '_div'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '%', 'g'], Symbol.for('_'), ''], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '/', 'g'], Symbol.for('_'), '-'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '!', 'g'], Symbol.for('_'), '-x'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ':', 'g'], Symbol.for('_'), '-'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '->', 'g'], Symbol.for('_'), '-to-'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\+', 'g'], Symbol.for('_'), '_'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\*$', 'g'], Symbol.for('_'), '-star'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\*', 'g'], Symbol.for('_'), 'star-']]], [Symbol.for('cond'), [[Symbol.for('regexp-match'), [Symbol.for('regexp'), '-', 'g'], Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\?', 'g'], Symbol.for('result'), '-p']]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\?', 'g'], Symbol.for('result'), 'p']]]], Symbol.for('result')];

/**
 * Whether something is an equality expression.
 */
function isEqualityExpression(exp: any, env: any): any {
  return formp(exp, eqp_, env) || formp(exp, eqvp_, env) || formp(exp, equalp_, env);
}

isEqualityExpression.fsource = [Symbol.for('define'), [Symbol.for('is-equality-expression'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('eq?_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('eqv?_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('equal?_'), Symbol.for('env')]]];

/**
 * Whether something is a `let` or `let*` expression.
 */
function isLetExpression(exp: any, env: any): any {
  return formp(exp, letStar_, env);
}

isLetExpression.fsource = [Symbol.for('define'), [Symbol.for('is-let-expression'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('exp'), Symbol.for('let-star_'), Symbol.for('env')]];

/**
 * Compile a `(for ...)` expression.
 */
function compileFor(node: any, env: any, options: any = {}): any {
  // TODO: Implement `compile-js-for` and implement this
  // in terms of that?
  const inheritedOptions: any = {
    ...options
  };
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

compileFor.fsource = [Symbol.for('define'), [Symbol.for('compile-for'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('decls-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('decls'), [Symbol.for('send'), Symbol.for('decls-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body-nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('body-node'), [Symbol.for('begin-wrap-rose-smart-1'), Symbol.for('body-nodes')]], [Symbol.for('define'), Symbol.for('decl1-node'), [Symbol.for('send'), Symbol.for('decls-node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('decl1'), [Symbol.for('send'), Symbol.for('decl1-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('sym-node'), [Symbol.for('send'), Symbol.for('decl1-node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('sym-exp'), [Symbol.for('send'), Symbol.for('sym-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('vals-node'), [Symbol.for('send'), Symbol.for('decl1-node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('vals-exp'), [Symbol.for('send'), Symbol.for('vals-node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('form?'), Symbol.for('vals-exp'), Symbol.for('range_'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('start'), [Symbol.for('if'), [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('vals-exp')], 2], [Symbol.for('second'), Symbol.for('vals-exp')], undefined]], [Symbol.for('define'), Symbol.for('end'), [Symbol.for('if'), [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('vals-exp')], 3], [Symbol.for('third'), Symbol.for('vals-exp')], undefined]], [Symbol.for('define'), Symbol.for('step'), [Symbol.for('if'), [Symbol.for('>='), 4, [Symbol.for('array-list-length'), Symbol.for('vals-exp')]], [Symbol.for('fourth'), Symbol.for('vals-exp')], undefined]], [Symbol.for('set!'), Symbol.for('start'), [Symbol.for('if'), [Symbol.for('undefined?'), Symbol.for('end')], 0, Symbol.for('start')]], [Symbol.for('set!'), Symbol.for('end'), [Symbol.for('if'), [Symbol.for('undefined?'), Symbol.for('end')], Symbol.for('start'), Symbol.for('end')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('array?'), Symbol.for('start')], [Symbol.for('array?'), Symbol.for('end')], [Symbol.for('array?'), Symbol.for('step')]], [Symbol.for('define'), Symbol.for('start-var'), [Symbol.for('gensym'), '_start']], [Symbol.for('define'), Symbol.for('end-var'), [Symbol.for('gensym'), '_end']], [Symbol.for('define'), Symbol.for('step-var'), [Symbol.for('gensym'), '_step']], [Symbol.for('return'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('start')], [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('start-var')], [Symbol.for('unquote'), Symbol.for('start')]]]], [Symbol.for('quote'), []]]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('end')], [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('end-var')], [Symbol.for('unquote'), Symbol.for('end')]]]], [Symbol.for('quote'), []]]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('step')], [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('step-var')], [Symbol.for('unquote'), Symbol.for('step')]]]], [Symbol.for('quote'), []]]]], [Symbol.for('for'), [[[Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('range'), [Symbol.for('unquote'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('start')], Symbol.for('start-var'), Symbol.for('start')]], [Symbol.for('unquote'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('end')], Symbol.for('end-var'), Symbol.for('end')]], [Symbol.for('unquote-splicing'), [Symbol.for('if'), Symbol.for('step'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('step')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('step-var')]]], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('step')]]]], [Symbol.for('quote'), []]]]]]], [Symbol.for('unquote-splicing'), Symbol.for('body-nodes')]]]], Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('step'), [Symbol.for('or'), Symbol.for('step'), 1]], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('start')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('define'), Symbol.for('test')], [Symbol.for('define'), Symbol.for('update')], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('step')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('step'), 0], [Symbol.for('set!'), Symbol.for('test'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('>'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('Math'), Symbol.for('abs'), Symbol.for('step')]]]]]], Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('test'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('step')]]]]], Symbol.for('env'), Symbol.for('inherited-options')]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('test'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('if'), [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('step')], 0], [Symbol.for('>'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]], [Symbol.for('<'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('end')]]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('sym-exp')], [Symbol.for('unquote'), Symbol.for('step')]]]]], Symbol.for('env'), Symbol.for('inherited-options')]]]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('update'), 'ExpressionStatement'], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('update')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-statement'), Symbol.for('body-node'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('new'), Symbol.for('ForStatement'), Symbol.for('init'), Symbol.for('test'), Symbol.for('update'), Symbol.for('body')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('left'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('sym-exp')]]]], Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('compile-expression'), Symbol.for('vals-node'), Symbol.for('env'), Symbol.for('inherited-options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('wrap-in-block-statement'), [Symbol.for('compile-statement'), Symbol.for('body-node'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('new'), Symbol.for('ForOfStatement'), Symbol.for('left'), Symbol.for('right'), Symbol.for('body')]]]];

/**
 * Compile a `(break)` expression.
 */
function compileBreak(node: any, env: any, options: any = {}): any {
  return new BreakStatement((node.size() > 1) ? compileExpression(node.get(1), env, options) : null);
}

compileBreak.fsource = [Symbol.for('define'), [Symbol.for('compile-break'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('BreakStatement'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], null]]];

/**
 * Compile a `(continue)` expression.
 */
function compileContinue(node: any, env: any, options: any = {}): any {
  return new ContinueStatement((node.size() > 1) ? compileExpression(node.get(1), env, options) : null);
}

compileContinue.fsource = [Symbol.for('define'), [Symbol.for('compile-continue'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('ContinueStatement'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], null]]];

/**
 * Compile a `(js/type-of ...)` expression.
 */
function compileJsTypeOf(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new UnaryExpression('typeof', true, compileExpression(node.get(1), env, options)), options);
}

compileJsTypeOf.fsource = [Symbol.for('define'), [Symbol.for('compile-js-type-of'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('UnaryExpression'), 'typeof', true, [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

/**
 * Compile a `(js/instance-of? ...)` expression.
 */
function compileJsInstanceOf(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new BinaryExpression('instanceof', compileExpression(node.get(1), env, options), compileExpression(node.get(2), env, options)), options);
}

compileJsInstanceOf.fsource = [Symbol.for('define'), [Symbol.for('compile-js-instance-of'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('BinaryExpression'), 'instanceof', [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

/**
 * Compile a `(js/in ...)` expression.
 */
function compileJsIn(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new BinaryExpression('in', compileExpression(node.get(1), env, options), compileExpression(node.get(2), env, options)), options);
}

compileJsIn.fsource = [Symbol.for('define'), [Symbol.for('compile-js-in'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('BinaryExpression'), 'in', [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

/**
 * Compile a `(js/new ...)` expression.
 */
function compileJsNew(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new NewExpression(compileExpression(node.get(1), env, options), node.drop(2).map(function (x: any): any {
    return compileExpression(x, env, options);
  })), options);
}

compileJsNew.fsource = [Symbol.for('define'), [Symbol.for('compile-js-new'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('NewExpression'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]], Symbol.for('options')]];

/**
 * Compile a `(js/do-while ...)` expression.
 */
function compileJsDoWhile(node: any, env: any, options: any = {}): any {
  const body: any = node.get(1);
  let test: any = node.get(2);
  return new DoWhileStatement(compileExpression(test, env, options), wrapInBlockStatementSmart(compileStatementOrReturnStatement(body, env, options)));
}

compileJsDoWhile.fsource = [Symbol.for('define'), [Symbol.for('compile-js-do-while'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('new'), Symbol.for('DoWhileStatement'), [Symbol.for('compile-expression'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(js/while ...)` expression.
 */
function compileJsWhile(node: any, env: any, options: any = {}): any {
  let test: any = node.get(1);
  const body: any = beginWrapRose(node.drop(2));
  return new WhileStatement(compileExpression(test, env, options), wrapInBlockStatementSmart(compileStatementOrReturnStatement(body, env, options)));
}

compileJsWhile.fsource = [Symbol.for('define'), [Symbol.for('compile-js-while'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('begin-wrap-rose'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]]], [Symbol.for('new'), Symbol.for('WhileStatement'), [Symbol.for('compile-expression'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(yield ...)` expression.
 */
function compileYield(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new YieldExpression((node.size() > 1) ? compileExpression(node.get(1), env, options) : null), options);
}

compileYield.fsource = [Symbol.for('define'), [Symbol.for('compile-yield'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('YieldExpression'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], null]], Symbol.for('options')]];

/**
 * Compile a `(throw ...)` expression.
 */
function compileThrow(node: any, env: any, options: any = {}): any {
  return new ThrowStatement(compileExpression(node.get(1), env, options));
}

compileThrow.fsource = [Symbol.for('define'), [Symbol.for('compile-throw'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('ThrowStatement'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]];

/**
 * Compile a `(js/delete ...)` expression.
 */
function compileJsDelete(node: any, env: any, options: any = {}): any {
  return new UnaryExpression('delete', true, compileExpression(node.get(1), env, options));
}

compileJsDelete.fsource = [Symbol.for('define'), [Symbol.for('compile-js-delete'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('UnaryExpression'), 'delete', true, [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]];

/**
 * Compile a `(return ...)` expression.
 */
function compileReturn(node: any, env: any, options: any = {}): any {
  return new ReturnStatement((node.size() > 1) ? compileExpression(node.get(1), env, options) : null);
}

compileReturn.fsource = [Symbol.for('define'), [Symbol.for('compile-return'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('ReturnStatement'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('size')], 1], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')], null]]];

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

compileJsAsync.fsource = [Symbol.for('define'), [Symbol.for('compile-js-async'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('estree-type?'), Symbol.for('result'), 'FunctionDeclaration'], [Symbol.for('estree-type?'), Symbol.for('result'), 'FunctionExpression'], [Symbol.for('estree-type?'), Symbol.for('result'), 'ArrowFunctionExpression']], [Symbol.for('set-field!'), Symbol.for('async'), Symbol.for('result'), true], [Symbol.for('set-field!'), Symbol.for('returnType'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('TSTypeReference'), [Symbol.for('new'), Symbol.for('Identifier'), 'Promise'], [Symbol.for('new'), Symbol.for('TSTypeParameterInstantiation'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]]]], [Symbol.for('make-expression-or-statement'), Symbol.for('result'), Symbol.for('options')]];

/**
 * Compile a `(js/await ...)` expression.
 */
function compileJsAwait(node: any, env: any, options: any = {}): any {
  return makeExpressionOrStatement(new AwaitExpression(compileExpression(node.get(1), env, options)), options);
}

compileJsAwait.fsource = [Symbol.for('define'), [Symbol.for('compile-js-await'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('AwaitExpression'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], Symbol.for('options')]];

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

compileStringAppend.fsource = [Symbol.for('define'), [Symbol.for('compile-string-append'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('<='), [Symbol.for('array-list-length'), Symbol.for('exp')], 0], [Symbol.for('compile-rose'), '', Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('exp')], 2], [Symbol.for('compile-rose'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-binary-expression'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options'), [Symbol.for('js-obj'), 'identity', '', 'operator', '+']]]]];

/**
 * Compile a `(define-class ...)` expression.
 */
function compileDefineClass(node: any, env: any, options: any = {}): any {
  return compileClass(node, env, options);
}

compileDefineClass.fsource = [Symbol.for('define'), [Symbol.for('compile-define-class'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-class'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

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
  const env1: any = extendEnvironment(new LispEnvironment(), env);
  makeTypeBinding(env1, Symbol.for('super'), Symbol.for('Any'));
  if (Array.isArray(bodyExp[0]) && !formp(bodyExp[0], define_, env1)) {
    const superClassesNode: any = bodyNode.get(0);
    const superClasses: any = superClassesNode.getValue();
    bodyNode = sliceRose(bodyNode, 1);
    bodyExp = bodyNode.getValue();
    if (superClasses.length > 0) {
      superClass = new Identifier(printEstree(compileExpression(makeRose(superClasses[0]), env1, inheritedOptions), inheritedOptions));
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
      if (isConstructor || isGenerator) {
        accessibility = 'public';
      }
      const returnType: any = isConstructor ? 'void' : undefined;
      const isComputed: any = typeof id !== 'symbol';
      const idCompiled: any = isComputed ? compileExpression(idNode, env1, inheritedOptions) : compileSymbol(idNode, env1, makeExpressionOptions(inheritedOptions));
      const initCompiled: any = !isInitialized ? undefined : (isMethod ? compileJsFunction(defineToLambda(x, {
        curried: false
      }), env1, makeExpressionOptions(inheritedOptions), {
        generator: isGenerator,
        returnType: returnType
      }) : compileExpression(x.get(2), env1, makeExpressionOptions(inheritedOptions)));
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

compileClass.fsource = [Symbol.for('define'), [Symbol.for('compile-class'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('inherited-options'), [Symbol.for('js-obj-append'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('class-name-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('class-name'), [Symbol.for('send'), Symbol.for('class-name-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('super-class'), null], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('if'), [Symbol.for('symbol?'), Symbol.for('class-name')], [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), Symbol.for('class-name-node'), Symbol.for('env'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]], null]], [Symbol.for('define'), Symbol.for('body-node'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('id'), null], [Symbol.for('slice-rose'), Symbol.for('node'), 1], [Symbol.for('slice-rose'), Symbol.for('node'), 2]]], [Symbol.for('define'), Symbol.for('body-exp'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('env1'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('env')]], [Symbol.for('make-type-binding'), Symbol.for('env1'), [Symbol.for('quote'), Symbol.for('super')], [Symbol.for('quote'), Symbol.for('Any')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('array?'), [Symbol.for('first'), Symbol.for('body-exp')]], [Symbol.for('not'), [Symbol.for('form?'), [Symbol.for('first'), Symbol.for('body-exp')], Symbol.for('define_'), Symbol.for('env1')]]], [Symbol.for('define'), Symbol.for('super-classes-node'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('super-classes'), [Symbol.for('send'), Symbol.for('super-classes-node'), Symbol.for('get-value')]], [Symbol.for('set!'), Symbol.for('body-node'), [Symbol.for('slice-rose'), Symbol.for('body-node'), 1]], [Symbol.for('set!'), Symbol.for('body-exp'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get-value')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('super-classes')], 0], [Symbol.for('set!'), Symbol.for('super-class'), [Symbol.for('new'), Symbol.for('Identifier'), [Symbol.for('print-estree'), [Symbol.for('compile-expression'), [Symbol.for('make-rose'), [Symbol.for('first'), Symbol.for('super-classes')]], Symbol.for('env1'), Symbol.for('inherited-options')], Symbol.for('inherited-options')]]]]], [Symbol.for('define'), Symbol.for('body-declarations'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('accessibilities'), [Symbol.for('make-hash')]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('body-node'), Symbol.for('get-nodes')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('public')]], [Symbol.for('hash-set!'), Symbol.for('accessibilities'), [Symbol.for('second'), Symbol.for('exp')], 'public']], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('private')]], [Symbol.for('hash-set!'), Symbol.for('accessibilities'), [Symbol.for('second'), Symbol.for('exp')], 'private']], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('is-initialized'), [Symbol.for('>='), [Symbol.for('array-list-length'), Symbol.for('exp')], 3]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('is-method'), [Symbol.for('array?'), Symbol.for('id')]], [Symbol.for('when'), Symbol.for('is-method'), [Symbol.for('set!'), Symbol.for('id'), [Symbol.for('first'), Symbol.for('id')]]], [Symbol.for('define'), Symbol.for('id-node'), [Symbol.for('if'), Symbol.for('is-method'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1], Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]]], [Symbol.for('define'), Symbol.for('accessibility'), [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('accessibilities'), Symbol.for('id')], [Symbol.for('hash-ref'), Symbol.for('accessibilities'), Symbol.for('id')]], [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define/public')]], 'public'], [Symbol.for('else'), 'private']]], [Symbol.for('define'), Symbol.for('is-generator'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define/generator')]]], [Symbol.for('define'), Symbol.for('is-constructor'), [Symbol.for('and'), Symbol.for('is-method'), [Symbol.for('>'), [Symbol.for('array-list-length'), [Symbol.for('second'), Symbol.for('exp')]], 0], [Symbol.for('eq?'), Symbol.for('id'), [Symbol.for('quote'), Symbol.for('constructor')]]]], [Symbol.for('when'), [Symbol.for('or'), Symbol.for('is-constructor'), Symbol.for('is-generator')], [Symbol.for('set!'), Symbol.for('accessibility'), 'public']], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('if'), Symbol.for('is-constructor'), 'void', undefined]], [Symbol.for('define'), Symbol.for('is-computed'), [Symbol.for('not'), [Symbol.for('symbol?'), Symbol.for('id')]]], [Symbol.for('define'), Symbol.for('id-compiled'), [Symbol.for('if'), Symbol.for('is-computed'), [Symbol.for('compile-expression'), Symbol.for('id-node'), Symbol.for('env1'), Symbol.for('inherited-options')], [Symbol.for('compile-symbol'), Symbol.for('id-node'), Symbol.for('env1'), [Symbol.for('make-expression-options'), Symbol.for('inherited-options')]]]], [Symbol.for('define'), Symbol.for('init-compiled'), [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('is-initialized')], undefined], [Symbol.for('is-method'), [Symbol.for('compile-js-function'), [Symbol.for('define->lambda'), Symbol.for('x'), [Symbol.for('js-obj'), 'curried', false]], Symbol.for('env1'), [Symbol.for('make-expression-options'), Symbol.for('inherited-options')], [Symbol.for('js-obj'), 'generator', Symbol.for('is-generator'), 'returnType', Symbol.for('return-type')]]], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 2], Symbol.for('env1'), [Symbol.for('make-expression-options'), Symbol.for('inherited-options')]]]]], [Symbol.for('cond'), [Symbol.for('is-method'), [Symbol.for('define'), Symbol.for('kind'), [Symbol.for('if'), Symbol.for('is-constructor'), 'constructor', 'method']], [Symbol.for('define'), Symbol.for('method-definition'), [Symbol.for('new'), Symbol.for('MethodDefinition'), Symbol.for('id-compiled'), Symbol.for('init-compiled'), Symbol.for('kind'), false, false, Symbol.for('is-computed'), Symbol.for('accessibility')]], [Symbol.for('set!'), Symbol.for('method-definition'), [Symbol.for('transfer-and-compile-comments'), Symbol.for('x'), Symbol.for('method-definition'), Symbol.for('inherited-options')]], [Symbol.for('push-right!'), Symbol.for('body-declarations'), Symbol.for('method-definition')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('property-definition'), [Symbol.for('new'), Symbol.for('PropertyDefinition'), Symbol.for('id-compiled'), Symbol.for('init-compiled'), false, Symbol.for('accessibility')]], [Symbol.for('set!'), Symbol.for('property-definition'), [Symbol.for('transfer-and-compile-comments'), Symbol.for('x'), Symbol.for('property-definition'), Symbol.for('inherited-options')]], [Symbol.for('push-right!'), Symbol.for('body-declarations'), Symbol.for('property-definition')]]]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('new'), Symbol.for('ClassBody'), Symbol.for('body-declarations')]], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('id'), null], [Symbol.for('new'), Symbol.for('ClassExpression'), Symbol.for('body'), Symbol.for('super-class')], [Symbol.for('new'), Symbol.for('ClassDeclaration'), Symbol.for('id'), Symbol.for('body'), Symbol.for('super-class')]]];

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

compileJsObj.fsource = [Symbol.for('define'), [Symbol.for('compile-js-obj'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('exp')], 2]]], [Symbol.for('define'), Symbol.for('key-node'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('key-value'), [Symbol.for('send'), Symbol.for('key-node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('compiled-key'), [Symbol.for('compile-expression'), Symbol.for('key-node'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('compiled-value'), [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('not'), [Symbol.for('string?'), [Symbol.for('send'), Symbol.for('key-node'), Symbol.for('get-value')]]]], [Symbol.for('define'), Symbol.for('match')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('string?'), Symbol.for('key-value')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^[a-z]+$', 'i'], Symbol.for('key-value')]], [Symbol.for('set!'), Symbol.for('compiled-key'), [Symbol.for('new'), Symbol.for('Identifier'), Symbol.for('key-value')]]], [Symbol.for('push-right!'), Symbol.for('properties'), [Symbol.for('new'), Symbol.for('Property'), Symbol.for('compiled-key'), Symbol.for('compiled-value'), Symbol.for('computed')]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ObjectExpression'), Symbol.for('properties')], Symbol.for('options')]];

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

compileJsObjAppend.fsource = [Symbol.for('define'), [Symbol.for('compile-js-obj-append'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('compile-expression'), Symbol.for('arg'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('ObjectExpression')], [Symbol.for('for'), [[Symbol.for('prop'), [Symbol.for('get-field'), Symbol.for('properties'), Symbol.for('exp')]]], [Symbol.for('push-right!'), Symbol.for('properties'), Symbol.for('prop')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('properties'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('exp')]]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ObjectExpression'), Symbol.for('properties')], Symbol.for('options')]];

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

compileJsTaggedTemplate.fsource = [Symbol.for('define'), [Symbol.for('compile-js-tagged-template'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('tag'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('tag-compiled'), [Symbol.for('compile-rose'), Symbol.for('tag'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('str-exp'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('get-value')]], [Symbol.for('new'), Symbol.for('TaggedTemplateExpression'), Symbol.for('tag-compiled'), [Symbol.for('new'), Symbol.for('TemplateLiteral'), [Symbol.for('list'), [Symbol.for('new'), Symbol.for('TemplateElement'), true, Symbol.for('str-exp')]]]]];

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

compileAppend.fsource = [Symbol.for('define'), [Symbol.for('compile-append'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('elements'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]], [Symbol.for('define'), Symbol.for('el'), [Symbol.for('compile-expression'), Symbol.for('x'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('el'), 'ArrayExpression'], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('el')]], 0]], [[Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('el')]], 1], [Symbol.for('push-right!'), Symbol.for('elements'), [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('el')], 0]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('elements'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('el')]]]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('elements'), [Symbol.for('new'), Symbol.for('SpreadElement'), Symbol.for('el')]]]]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('ArrayExpression'), Symbol.for('elements')], Symbol.for('options')]];

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
    const bodyCompiled: any = wrapInBlockStatementSmart(compileStatement(body, env, options));
    handler = new CatchClause(paramCompiled, bodyCompiled);
  }
  const finalizer: any = finallyClause ? wrapInBlockStatementSmart(compileStatement(makeRose([Symbol.for('begin'), ...finallyClause.drop(1)]), env, options)) : null;
  return makeExpressionOrStatement(new TryStatement(block, handler, finalizer), options);
}

compileJsTry.fsource = [Symbol.for('define'), [Symbol.for('compile-js-try'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('body-exps'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('catch-clause'), null], [Symbol.for('define'), Symbol.for('finally-clause'), null], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('catch')]], [Symbol.for('set!'), Symbol.for('catch-clause'), Symbol.for('x')]], [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('finally')]], [Symbol.for('set!'), Symbol.for('finally-clause'), Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('body-exps'), Symbol.for('x')]]]], [Symbol.for('define'), Symbol.for('block'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body-exps')]]]], Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('handler'), null], [Symbol.for('when'), Symbol.for('catch-clause'), [Symbol.for('define'), Symbol.for('param'), [Symbol.for('send'), Symbol.for('catch-clause'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('param-exp'), [Symbol.for('send'), Symbol.for('param'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('param-compiled'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('param-exp'), [Symbol.for('quote'), Symbol.for('_')]], null, [Symbol.for('compile-expression'), Symbol.for('param'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('catch-clause'), Symbol.for('drop'), 2]]]]]], [Symbol.for('define'), Symbol.for('body-compiled'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement'), Symbol.for('body'), Symbol.for('env'), Symbol.for('options')]]], [Symbol.for('set!'), Symbol.for('handler'), [Symbol.for('new'), Symbol.for('CatchClause'), Symbol.for('param-compiled'), Symbol.for('body-compiled')]]], [Symbol.for('define'), Symbol.for('finalizer'), [Symbol.for('if'), Symbol.for('finally-clause'), [Symbol.for('wrap-in-block-statement-smart'), [Symbol.for('compile-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('finally-clause'), Symbol.for('drop'), 1]]]]], Symbol.for('env'), Symbol.for('options')]], null]], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('TryStatement'), Symbol.for('block'), Symbol.for('handler'), Symbol.for('finalizer')], Symbol.for('options')]];

/**
 * Compile a `(push-left! ...)` expression.
 */
function compilePushLeft(node: any, env: any, options: any = {}): any {
  // `.unshift()` returns the length of the array, while `push!()`
  // returns the list.
  return compilePushHelper(makeRose([Symbol.for('send'), node.get(1), Symbol.for('unshift'), node.get(2)]), makeRose([[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('lst')], node.get(1), node.get(2)]), node, env, options);
}

compilePushLeft.fsource = [Symbol.for('define'), [Symbol.for('compile-push-left'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-push-helper'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], Symbol.for('unshift'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('lst')], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

/**
 * Compile a `(push-right! ...)` expression.
 */
function compilePushRight(node: any, env: any, options: any = {}): any {
  // `.push()` returns the length of the array, while `push-right!()`
  // returns the list.
  return compilePushHelper(makeRose([Symbol.for('send'), node.get(1), Symbol.for('push'), node.get(2)]), makeRose([[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('push'), Symbol.for('x')], Symbol.for('lst')], node.get(1), node.get(2)]), node, env, options);
}

compilePushRight.fsource = [Symbol.for('define'), [Symbol.for('compile-push-right'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('compile-push-helper'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], Symbol.for('push'), [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('push'), Symbol.for('x')], Symbol.for('lst')], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('unquote'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]]]]], Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]];

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

compilePushHelper.fsource = [Symbol.for('define'), [Symbol.for('compile-push-helper'), Symbol.for('statement-exp'), Symbol.for('expression-exp'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('expression-type'), 'return'], [Symbol.for('cond'), [[Symbol.for('symbol?'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('get-value')]], [Symbol.for('new'), Symbol.for('Program'), [Symbol.for('list'), [Symbol.for('compile-statement'), Symbol.for('statement-exp'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-return-statement'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('compile-rose'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('return'), [Symbol.for('unquote'), Symbol.for('node')]]]], Symbol.for('env'), Symbol.for('options')]]]], [[Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('compile-statement-or-return-statement'), Symbol.for('statement-exp'), Symbol.for('env'), Symbol.for('options')]], [[Symbol.for('symbol?'), [Symbol.for('send'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('get-value')]], [Symbol.for('new'), Symbol.for('SequenceExpression'), [Symbol.for('list'), [Symbol.for('compile-expression'), Symbol.for('statement-exp'), Symbol.for('env'), Symbol.for('options')], [Symbol.for('compile-expression'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('compile-expression'), Symbol.for('expression-exp'), Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Compile a `(define-macro ...)` expression.
 */
function compileDefineMacro(node: any, env: any, options: any = {}): any {
  const languageEnv: any = options['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
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
  let result: any = compileRose(transferComments(node, makeRose([Symbol.for('begin'), [Symbol.for('define'), [name, ...args], ...body], [Symbol.for('declare'), name, [Symbol.for('ftype'), 'macro']]], node)), env, options);
  makeTypeBinding(env, name, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')], langFilter);
  return result;
}

compileDefineMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-define-macro'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('car'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('macro-fn-form'), [Symbol.for('define-macro->lambda-form'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('second'), Symbol.for('macro-fn-form')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('drop'), Symbol.for('macro-fn-form'), 2]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('compile-rose'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('unquote-splicing'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('declare'), [Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('ftype'), 'macro']]]], Symbol.for('node')]], Symbol.for('env'), Symbol.for('options')]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('name'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], Symbol.for('lang-filter')], Symbol.for('result')];

/**
 * Compile a `(declare-macro ...)` expression.
 */
function compileDeclareMacro(node: any, env: any, options: any = {}): any {
  const languageEnv: any = options['languageEnvironment'];
  function langFilter(x: any): any {
    return x !== languageEnv;
  }
  langFilter.fsource = [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]];
  let name: any = node.get(1).getValue();
  makeTypeBinding(env, name, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')], langFilter);
  return emptyProgram();
}

compileDeclareMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-declare-macro'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language-env'), [Symbol.for('oget'), Symbol.for('options'), 'languageEnvironment']], [Symbol.for('define'), [Symbol.for('lang-filter'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('language-env')]]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('_'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('_'), Symbol.for('get-value')]]], [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('name'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], Symbol.for('lang-filter')], [Symbol.for('empty-program')]];

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

compileMakeHashMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-make-hash-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('assocs')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [Symbol.for('assocs'), [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('assocs'), [Symbol.for('quote'), Symbol.for('quasiquote')]], [Symbol.for('tagged-list?'), Symbol.for('assocs'), [Symbol.for('quote'), Symbol.for('quote')]]], [Symbol.for('list?'), [Symbol.for('second'), Symbol.for('assocs')]], [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('filter'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('array?'), Symbol.for('x')]], [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('x')], 2], [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('unquote')]], [Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]], [Symbol.for('not'), [Symbol.for('tagged-list?'), [Symbol.for('second'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('hash->list')]]]]]]]], [Symbol.for('second'), Symbol.for('assocs')]]], 0]], [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('ann'), [[Symbol.for('unquote'), [Symbol.for('first'), Symbol.for('assocs')]], [Symbol.for('unquote'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]], [Symbol.for('tagged-list?'), [Symbol.for('second'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('hash->list')]]], [Symbol.for('cons'), [Symbol.for('first'), Symbol.for('x')], [Symbol.for('list'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('second'), [Symbol.for('second'), Symbol.for('x')]]], Symbol.for('entries')]]]]], [Symbol.for('else'), [Symbol.for('list'), [Symbol.for('car'), Symbol.for('x')], [Symbol.for('cdr'), Symbol.for('x')]]]]], [Symbol.for('second'), Symbol.for('assocs')]]]], Symbol.for('Any')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('map'), Symbol.for('flatten'), [Symbol.for('unquote'), Symbol.for('assocs')]]]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('Map')]]]]];

compileMakeHashMacro.ftype = 'macro';

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

compileHashClearMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-hash-clear-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('ht')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ht')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('ht')], Symbol.for('clear')], [Symbol.for('unquote'), Symbol.for('ht')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('ht')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('clear')], Symbol.for('ht')], [Symbol.for('unquote'), Symbol.for('ht')]]]]]];

compileHashClearMacro.ftype = 'macro';

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

compileHashRemoveMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-hash-remove-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('ht'), Symbol.for('key')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ht')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('ht')], Symbol.for('delete'), [Symbol.for('unquote'), Symbol.for('key')]], [Symbol.for('unquote'), Symbol.for('ht')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('lambda'), [Symbol.for('ht'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('delete'), Symbol.for('key')], Symbol.for('ht')], [Symbol.for('unquote'), Symbol.for('ht')], [Symbol.for('unquote'), Symbol.for('key')]]]]]];

compileHashRemoveMacro.ftype = 'macro';

/**
 * Compiler macro for `(hash-ref ...)` expressions.
 */
function compileHashRefMacro(exp: any, env: any): any {
  const [ht, key, failureResult]: any[] = exp.slice(1);
  if (failureResult === undefined) {
    return [Symbol.for('send'), ht, Symbol.for('get'), key];
  } else {
    return definitionToMacro([Symbol.for('define'), [Symbol.for('hash-ref'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')], [Symbol.for('if'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('has'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('get'), Symbol.for('key')], Symbol.for('failure-result')]], [ht, key, failureResult]);
  }
}

compileHashRefMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-hash-ref-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('undefined?'), Symbol.for('failure-result')], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('ht')], Symbol.for('get'), [Symbol.for('unquote'), Symbol.for('key')]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('hash-ref'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')], [Symbol.for('if'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('has'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('get'), Symbol.for('key')], Symbol.for('failure-result')]]], [Symbol.for('list'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('failure-result')]]]]];

compileHashRefMacro.ftype = 'macro';

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

compileMapMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-map-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('f'), Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('f-exp'), [Symbol.for('compile-map-macro-helper'), Symbol.for('f'), Symbol.for('env')]], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('x')], Symbol.for('map'), [Symbol.for('unquote'), Symbol.for('f-exp')]]]];

compileMapMacro.ftype = 'macro';

/**
 * Compiler macro for `(values ...)` expressions.
 */
function compileValuesMacro(exp: any, env: any): any {
  const args: any = exp.slice(1);
  return [Symbol.for('list'), ...args];
}

compileValuesMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-values-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('list'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileValuesMacro.ftype = 'macro';

/**
 * Compiler macro for `(string? ...)` expressions.
 */
function compileStringpMacro(exp: any, env: any): any {
  const [x]: any[] = exp.slice(1);
  return [Symbol.for('eq?'), [Symbol.for('type-of'), x], 'string'];
}

compileStringpMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-stringp-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('eq?'), [Symbol.for('type-of'), [Symbol.for('unquote'), Symbol.for('x')]], 'string']]];

compileStringpMacro.ftype = 'macro';

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

compileStringTrimMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-string-trim-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-length'), Symbol.for('args')], 1], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), [Symbol.for('array-first'), Symbol.for('args')]], Symbol.for('trim')]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('string-trim_')], Symbol.for('args')]]]];

compileStringTrimMacro.ftype = 'macro';

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

compileMemberPMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-member-p-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('is-equal')], [Symbol.for('definition->macro'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]]], [Symbol.for('list'), Symbol.for('v'), Symbol.for('lst')]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')], [Symbol.for('memf?'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]]], [Symbol.for('list'), Symbol.for('v'), Symbol.for('lst'), Symbol.for('is-equal')]]]]];

compileMemberPMacro.ftype = 'macro';

/**
 * Compiler macro for `(substring ...)` expressions.
 */
function compileSubstringMacro(exp: any, env: any): any {
  const [str, ...args]: any[] = exp.slice(1);
  return [Symbol.for('send'), str, Symbol.for('substring'), ...args];
}

compileSubstringMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-substring-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('str'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('str')], Symbol.for('substring'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileSubstringMacro.ftype = 'macro';

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

compileArrayDropMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-array-drop-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('arr'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('arr')], Symbol.for('slice'), [Symbol.for('unquote'), Symbol.for('n')]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-drop_')], [Symbol.for('list'), Symbol.for('arr'), Symbol.for('n')]]]]];

compileArrayDropMacro.ftype = 'macro';

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

compileArrayDropRightMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-array-drop-right-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('arr'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('arr')], Symbol.for('slice'), 0, [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('n')]]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-drop-right_')], [Symbol.for('list'), Symbol.for('arr'), Symbol.for('n')]]]]];

compileArrayDropRightMacro.ftype = 'macro';

/**
 * Compiler macro for `(drop ...)` expressions.
 */
function compileDropMacro(exp: any, env: any): any {
  let [lst, pos]: any[] = exp.slice(1);
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

compileDropMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-drop-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('pos')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('pos')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('pos'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), [Symbol.for('unquote'), Symbol.for('pos')]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('drop_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('pos')]]]]];

compileDropMacro.ftype = 'macro';

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

compileDropRightMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-drop-right-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), 0, [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('n')]]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('drop-right_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('n')]]]]];

compileDropRightMacro.ftype = 'macro';

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

compileArrayListDropMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-array-list-drop-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), [Symbol.for('unquote'), Symbol.for('n')]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-list-drop_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('n')]]]]];

compileArrayListDropMacro.ftype = 'macro';

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

compileArrayListDropRightMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-array-list-drop-right-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('lst'), Symbol.for('n')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('number?'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('lst')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('send'), [Symbol.for('unquote'), Symbol.for('lst')], Symbol.for('slice'), 0, [Symbol.for('-'), [Symbol.for('unquote'), Symbol.for('n')]]]]]]], [Symbol.for('else'), [Symbol.for('definition->macro'), [Symbol.for('source'), Symbol.for('array-list-drop-right_')], [Symbol.for('list'), Symbol.for('lst'), Symbol.for('n')]]]]];

compileArrayListDropRightMacro.ftype = 'macro';

/**
 * Compiler macro for `(js/regexp ...)` expressions.
 */
function compileJsRegexpMacro(exp: any, env: any): any {
  const args: any = exp.slice(1);
  return [Symbol.for('new'), Symbol.for('RegExp'), ...args];
}

compileJsRegexpMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-js-regexp-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('new'), Symbol.for('RegExp'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileJsRegexpMacro.ftype = 'macro';

/**
 * Compiler macro for `(assert ...)` expressions.
 */
function compileAssertMacro(exp: any, env: any): any {
  const args: any = exp.slice(1);
  return [Symbol.for('send'), Symbol.for('console'), Symbol.for('assert'), ...args];
}

compileAssertMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-assert-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), Symbol.for('console'), Symbol.for('assert'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileAssertMacro.ftype = 'macro';

/**
 * Compiler macro for `(display ...)` expressions.
 */
function compileDisplayMacro(exp: any, env: any): any {
  const args: any = exp.slice(1);
  return [Symbol.for('send'), Symbol.for('console'), Symbol.for('log'), ...args];
}

compileDisplayMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-display-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('send'), Symbol.for('console'), Symbol.for('log'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

compileDisplayMacro.ftype = 'macro';

/**
 * Compiler macro for `(current-environment)` expressions.
 */
function compileCurrentEnvironmentMacro(exp: any, env: any): any {
  const argSym: any = Symbol('_arg');
  const strSym: any = Symbol('_str');
  const identifierRegexp: any = [Symbol.for('regexp'), '^\\w+$'];
  return [Symbol.for('js-obj'), 'get', [Symbol.for('js/arrow'), [argSym], [Symbol.for('try'), [Symbol.for('define'), strSym, [Symbol.for('symbol->string'), argSym]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), identifierRegexp, strSym], [Symbol.for('return'), [Symbol.for('js/eval'), strSym]]], [Symbol.for('else'), [Symbol.for('return'), undefined]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), undefined]]]], 'has', [Symbol.for('js/arrow'), [argSym], [Symbol.for('try'), [Symbol.for('define'), strSym, [Symbol.for('symbol->string'), argSym]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), identifierRegexp, strSym], [Symbol.for('js/eval'), strSym], [Symbol.for('return'), true]], [Symbol.for('else'), [Symbol.for('return'), false]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), false]]]]];
}

compileCurrentEnvironmentMacro.fsource = [Symbol.for('define'), [Symbol.for('compile-current-environment-macro'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('arg-sym'), [Symbol.for('gensym'), '_arg']], [Symbol.for('define'), Symbol.for('str-sym'), [Symbol.for('gensym'), '_str']], [Symbol.for('define'), Symbol.for('identifier-regexp'), [Symbol.for('quote'), [Symbol.for('regexp'), '^\\w+$']]], [Symbol.for('quasiquote'), [Symbol.for('js-obj'), 'get', [Symbol.for('js/arrow'), [[Symbol.for('unquote'), Symbol.for('arg-sym')]], [Symbol.for('try'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('str-sym')], [Symbol.for('symbol->string'), [Symbol.for('unquote'), Symbol.for('arg-sym')]]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), [Symbol.for('unquote'), Symbol.for('identifier-regexp')], [Symbol.for('unquote'), Symbol.for('str-sym')]], [Symbol.for('return'), [Symbol.for('js/eval'), [Symbol.for('unquote'), Symbol.for('str-sym')]]]], [Symbol.for('else'), [Symbol.for('return'), undefined]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), undefined]]]], 'has', [Symbol.for('js/arrow'), [[Symbol.for('unquote'), Symbol.for('arg-sym')]], [Symbol.for('try'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('str-sym')], [Symbol.for('symbol->string'), [Symbol.for('unquote'), Symbol.for('arg-sym')]]], [Symbol.for('cond'), [[Symbol.for('regexp-match?'), [Symbol.for('unquote'), Symbol.for('identifier-regexp')], [Symbol.for('unquote'), Symbol.for('str-sym')]], [Symbol.for('js/eval'), [Symbol.for('unquote'), Symbol.for('str-sym')]], [Symbol.for('return'), true]], [Symbol.for('else'), [Symbol.for('return'), false]]], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), [Symbol.for('return'), false]]]]]]];

compileCurrentEnvironmentMacro.ftype = 'macro';

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

compileJs.fsource = [Symbol.for('define'), [Symbol.for('compile-js'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('eval-option'), [Symbol.for('oget'), Symbol.for('options'), 'eval']], [Symbol.for('set!'), Symbol.for('eval-option'), true], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('str-exp'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('eval-option')], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), undefined], Symbol.for('options')]], [[Symbol.for('string?'), Symbol.for('str-exp')], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('XRawJavaScript'), Symbol.for('str-exp')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('compile-js-eval'), Symbol.for('node'), Symbol.for('env'), Symbol.for('options')]]]];

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

compileJsEval.fsource = [Symbol.for('define'), [Symbol.for('compile-js-eval'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('eval-option'), [Symbol.for('oget'), Symbol.for('options'), 'eval']], [Symbol.for('set!'), Symbol.for('eval-option'), true], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('str-exp'), [Symbol.for('send'), Symbol.for('str'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('eval-option')], [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('Literal'), undefined], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('make-expression-or-statement'), [Symbol.for('new'), Symbol.for('CallExpression'), [Symbol.for('new'), Symbol.for('Identifier'), 'eval'], [Symbol.for('list'), [Symbol.for('compile-expression'), Symbol.for('str'), Symbol.for('env'), Symbol.for('options')]]], Symbol.for('options')]]]];

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

quote_.fsource = [Symbol.for('define'), [Symbol.for('quote_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

quote_.ftype = 'macro';

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

quasiquote_.fsource = [Symbol.for('define'), [Symbol.for('quasiquote_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

quasiquote_.ftype = 'macro';

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

setX_.fsource = [Symbol.for('define'), [Symbol.for('set!_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

setX_.ftype = 'macro';

/**
 * Expand a `(module ...)` expression.
 */
function module_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

module_.fsource = [Symbol.for('define'), [Symbol.for('module_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

module_.ftype = 'macro';

/**
 * Evaluate a `(begin ...)` expression.
 */
function begin_(...args: any[]): any {
  if (args.length === 0) {
    return undefined;
  } else {
    return args[args.length - 1];
  }
}

begin_.fsource = [Symbol.for('define'), [Symbol.for('begin_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('if'), [Symbol.for('zero?'), [Symbol.for('js/length'), Symbol.for('args')]], undefined, [Symbol.for('js/last'), Symbol.for('args')]]];

/**
 * Expand a `(let* ...)` expression.
 */
function letStar_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

letStar_.fsource = [Symbol.for('define'), [Symbol.for('let-star_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

letStar_.ftype = 'macro';

/**
 * Expand a `(let-values ...)` expression.
 */
function letValues_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

letValues_.fsource = [Symbol.for('define'), [Symbol.for('let-values_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

letValues_.ftype = 'macro';

/**
 * Expand a `(define-values ...)` expression.
 */
function defineValues_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineValues_.fsource = [Symbol.for('define'), [Symbol.for('define-values_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineValues_.ftype = 'macro';

/**
 * Expand a `(set!-values ...)` expression.
 */
function setValues_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

setValues_.fsource = [Symbol.for('define'), [Symbol.for('set-values_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

setValues_.ftype = 'macro';

/**
 * Expand a `(define ...)` expression.
 */
function define_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

define_.fsource = [Symbol.for('define'), [Symbol.for('define_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

define_.ftype = 'macro';

/**
 * Expand a `(define/generator ...)` expression.
 */
function defineGenerator_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineGenerator_.fsource = [Symbol.for('define'), [Symbol.for('define-generator_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineGenerator_.ftype = 'macro';

/**
 * Expand a `(define/async ...)` expression.
 */
function defineAsync_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineAsync_.fsource = [Symbol.for('define'), [Symbol.for('define-async_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineAsync_.ftype = 'macro';

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
  env.set(name, f, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]);
  return fExp;
}

defineMacro_.fsource = [Symbol.for('define'), [Symbol.for('define-macro_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('first'), [Symbol.for('second'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('f-exp'), [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('eval_'), Symbol.for('f-exp'), Symbol.for('env')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set'), Symbol.for('name'), Symbol.for('f'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], Symbol.for('f-exp')];

defineMacro_.ftype = 'macro';

/**
 * Expand a `(declare-macro ...)` expression.
 */
function declareMacro_(exp: any, env: any): any {
  const [x]: any[] = exp.slice(1);
  return compileSexp(exp, env, currentCompilationOptions());
}

declareMacro_.fsource = [Symbol.for('define'), [Symbol.for('declare-macro_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

declareMacro_.ftype = 'macro';

/**
 * Expand a `(for ...)` expression.
 */
function for_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

for_.fsource = [Symbol.for('define'), [Symbol.for('for_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

for_.ftype = 'macro';

/**
 * Expand a `(js/while ...)` expression.
 */
function jsWhile_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsWhile_.fsource = [Symbol.for('define'), [Symbol.for('js-while_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsWhile_.ftype = 'macro';

/**
 * Expand a `(js/do-while ...)` expression.
 */
function jsDoWhile_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsDoWhile_.fsource = [Symbol.for('define'), [Symbol.for('js-do-while_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsDoWhile_.ftype = 'macro';

/**
 * Expand a `(break)` expression.
 */
function break_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

break_.fsource = [Symbol.for('define'), [Symbol.for('break_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

break_.ftype = 'macro';

/**
 * Expand a `(continue)` expression.
 */
function continue_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

continue_.fsource = [Symbol.for('define'), [Symbol.for('continue_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

continue_.ftype = 'macro';

/**
 * Expand a `(yield ...)` expression.
 */
function yield_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

yield_.fsource = [Symbol.for('define'), [Symbol.for('yield_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

yield_.ftype = 'macro';

/**
 * Expand a `(return ...)` expression.
 */
function return_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

return_.fsource = [Symbol.for('define'), [Symbol.for('return_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

return_.ftype = 'macro';

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

throw_.fsource = [Symbol.for('define'), [Symbol.for('throw_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

throw_.ftype = 'macro';

/**
 * Expand a `(js/async ...)` expression.
 */
function jsAsync_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsAsync_.fsource = [Symbol.for('define'), [Symbol.for('js-async_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsAsync_.ftype = 'macro';

/**
 * Expand a `(js/await ...)` expression.
 */
function jsAwait_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsAwait_.fsource = [Symbol.for('define'), [Symbol.for('js-await_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsAwait_.ftype = 'macro';

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

lambda_.fsource = [Symbol.for('define'), [Symbol.for('lambda_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

lambda_.ftype = 'macro';

/**
 * Expand a `(js/function ...)` expression.
 *
 * Creates an anonymous JavaScript function.
 */
function jsFunction_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsFunction_.fsource = [Symbol.for('define'), [Symbol.for('js-function_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsFunction_.ftype = 'macro';

/**
 * Expand a `(js/arrow ...)` expression.
 *
 * Creates a JavaScript arrow function.
 */
function jsArrow_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsArrow_.fsource = [Symbol.for('define'), [Symbol.for('js-arrow_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsArrow_.ftype = 'macro';

/**
 * Expand a `(cond ...)` expression.
 */
function cond_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

cond_.fsource = [Symbol.for('define'), [Symbol.for('cond_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

cond_.ftype = 'macro';

/**
 * Expand an `(and ...)` expression.
 */
function and_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

and_.fsource = [Symbol.for('define'), [Symbol.for('and_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

and_.ftype = 'macro';

/**
 * Expand an `(or ...)` expression.
 */
function or_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

or_.fsource = [Symbol.for('define'), [Symbol.for('or_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

or_.ftype = 'macro';

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

sendMethod.fsource = [Symbol.for('define'), [Symbol.for('send-method'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define-values'), [Symbol.for('obj'), Symbol.for('method'), Symbol.for('.'), Symbol.for('rest-args')], Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('method')], 'symbol'], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), [Symbol.for('symbol->string'), Symbol.for('method')], Symbol.for('rest-args')]], [[Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('method')], 'string'], [Symbol.for('apply'), Symbol.for('send-method'), Symbol.for('obj'), [Symbol.for('oget'), Symbol.for('obj'), Symbol.for('method')], Symbol.for('rest-args')]], [[Symbol.for('is-a?'), Symbol.for('method'), Symbol.for('Function')], [Symbol.for('send/apply'), Symbol.for('method'), Symbol.for('call'), Symbol.for('obj'), Symbol.for('rest-args')]], [Symbol.for('else'), [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), [Symbol.for('string-append'), 'Not a method: ', Symbol.for('method')]]]]]];

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

send_.fsource = [Symbol.for('define'), [Symbol.for('send_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

send_.ftype = 'macro';

/**
 * Expand a `(send/apply ...)` expression.
 */
function sendApply_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

sendApply_.fsource = [Symbol.for('define'), [Symbol.for('send-apply_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

sendApply_.ftype = 'macro';

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

dot_.fsource = [Symbol.for('define'), [Symbol.for('dot_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

dot_.ftype = 'macro';

/**
 * Expand a `(get-field ...)` expression.
 */
function getField_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

getField_.fsource = [Symbol.for('define'), [Symbol.for('get-field_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

getField_.ftype = 'macro';

/**
 * Expand a `(js/optional-chaining ...)` expression.
 */
function jsOptionalChaining_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsOptionalChaining_.fsource = [Symbol.for('define'), [Symbol.for('js-optional-chaining_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsOptionalChaining_.ftype = 'macro';

/**
 * Expand a `(set-field! ...)` expression.
 */
function setField_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

setField_.fsource = [Symbol.for('define'), [Symbol.for('set-field_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

setField_.ftype = 'macro';

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

class_.fsource = [Symbol.for('define'), [Symbol.for('class_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

class_.ftype = 'macro';

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

defineClass_.fsource = [Symbol.for('define'), [Symbol.for('define-class_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineClass_.ftype = 'macro';

/**
 * Expand a `(js/try ...)` expression.
 */
function jsTry_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsTry_.fsource = [Symbol.for('define'), [Symbol.for('js-try_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsTry_.ftype = 'macro';

/**
 * Expand a `(provide ...)` expression.
 */
function provide_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

provide_.fsource = [Symbol.for('define'), [Symbol.for('provide_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

provide_.ftype = 'macro';

/**
 * Expand a `(require ...)` expression.
 */
function require_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

require_.fsource = [Symbol.for('define'), [Symbol.for('require_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

require_.ftype = 'macro';

/**
 * Evaluate a JavaScript string.
 */
function js_(str: any): any {
  return eval(str);
}

js_.fsource = [Symbol.for('define'), [Symbol.for('js_'), Symbol.for('str')], [Symbol.for('js/eval'), Symbol.for('str')]];

/**
 * Get the Lisp source of a function.
 */
function source(x: any): any {
  return x.fsource;
}

source.fsource = [Symbol.for('define'), [Symbol.for('source'), Symbol.for('x')], [Symbol.for('get-field'), Symbol.for('fsource'), Symbol.for('x')]];

/**
 * Whether a function has Lisp source.
 */
function sourcep(x: any): any {
  return (x !== undefined) && (x.fsource !== undefined);
}

sourcep.fsource = [Symbol.for('define'), [Symbol.for('source?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('undefined?'), Symbol.for('x')]], [Symbol.for('not'), [Symbol.for('undefined?'), [Symbol.for('get-field'), Symbol.for('fsource'), Symbol.for('x')]]]]];

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

mapRose.fsource = [Symbol.for('define'), [Symbol.for('map-rose'), Symbol.for('f'), Symbol.for('node'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')]], [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('node'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('else'), [Symbol.for('map-visit-rose'), Symbol.for('f'), Symbol.for('node'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]]];

/**
 * Map a function `f` over a rose tree using the Visitor pattern.
 */
function mapVisitRose(f: any, node: any, env: any = new LispEnvironment(), stack: any = [], bindings: any = new LispEnvironment()): any {
  function skipNode(node: any, stack: any, bindings: any): any {
    return node;
  }
  skipNode.fsource = [Symbol.for('define'), [Symbol.for('skip-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], Symbol.for('node')];
  function visitNode(node: any, stack: any, bindings: any): any {
    return f(node, stack, bindings);
  }
  visitNode.fsource = [Symbol.for('define'), [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('f'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
  // Nonatomic value (i.e., a list form some sort).
  function visitNonatomic(node: any, stack: any, bindings: any, skip: any = 0): any {
    let result: any = visitFormsNode(node, [...stack, node], bindings, skip);
    return f(result, stack, bindings);
  }
  visitNonatomic.fsource = [Symbol.for('define'), [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('visit-forms-node'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // Macro call.
  function visitMacroCallP(node: any): any {
    let exp: any = node.getValue();
    return macroCallP(exp, env);
  }
  visitMacroCallP.fsource = [Symbol.for('define'), [Symbol.for('visit-macro-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('macro-call?'), Symbol.for('exp'), Symbol.for('env')]]];
  const visitMacroCall: any = visitNode;
  // Special form.
  function visitSpecialFormP(node: any): any {
    let exp: any = node.getValue();
    return specialFormP(exp, env);
  }
  visitSpecialFormP.fsource = [Symbol.for('define'), [Symbol.for('visit-special-form-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('special-form?'), Symbol.for('exp'), Symbol.for('env')]]];
  const visitSpecialForm: any = visitNode;
  // Function call.
  function visitFunctionCallP(node: any): any {
    let exp: any = node.getValue();
    return functionCallP(exp, env);
  }
  visitFunctionCallP.fsource = [Symbol.for('define'), [Symbol.for('visit-function-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('function-call?'), Symbol.for('exp'), Symbol.for('env')]]];
  const visitFunctionCall: any = visitNonatomic;
  function visitElseP(node: any): any {
    return true;
  }
  visitElseP.fsource = [Symbol.for('define'), [Symbol.for('visit-else-p'), Symbol.for('node')], true];
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
  visitFormsNodeWith.fsource = [Symbol.for('define'), [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-nodes')]], [Symbol.for('define'), Symbol.for('result-nodes'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('result-nodes'), Symbol.for('nodes')], Symbol.for('node')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('new'), Symbol.for('Rose'), Symbol.for('exp')]]], [Symbol.for('for'), [[Symbol.for('node'), Symbol.for('result-nodes')]], [Symbol.for('push-right!'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('send'), Symbol.for('result'), Symbol.for('insert'), Symbol.for('node')]], Symbol.for('result')]]];
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
  visitFormsListWith.fsource = [Symbol.for('define'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('nodes')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('is-modified'), false], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), Symbol.for('skip')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('is-modified'), true]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x1')]]], Symbol.for('nodes')]], [Symbol.for('unless'), Symbol.for('is-modified'), [Symbol.for('set!'), Symbol.for('result'), Symbol.for('nodes')]], Symbol.for('result')];
  function visitFormsNode(node: any, stack: any, bindings: any, skip: any = 0): any {
    return visitFormsNodeWith(visitor, node, stack, bindings, skip);
  }
  visitFormsNode.fsource = [Symbol.for('define'), [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
  function visitFormsList(nodes: any, stack: any, bindings: any, skip: any = 0): any {
    return visitFormsListWith(visitor, nodes, stack, bindings, skip);
  }
  visitFormsList.fsource = [Symbol.for('define'), [Symbol.for('visit-forms-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
  function visitClausesNode(node: any, stack: any, bindings: any, skip: any = 0): any {
    return visitFormsNodeWith(visitFormsNode, node, stack, bindings, skip);
  }
  visitClausesNode.fsource = [Symbol.for('define'), [Symbol.for('visit-clauses-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
  function visitClausesList(nodes: any, stack: any, bindings: any, skip: any = 0): any {
    return visitFormsListWith(visitFormsNode, nodes, stack, bindings, skip);
  }
  visitClausesList.fsource = [Symbol.for('define'), [Symbol.for('visit-clauses-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visit-forms-node'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]];
  // `(module ...)` form.
  function visitModuleP(node: any): any {
    return formp(node, module_, env);
  }
  visitModuleP.fsource = [Symbol.for('define'), [Symbol.for('visit-module-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('module_'), Symbol.for('env')]];
  function visitModule(node: any, stack: any, bindings: any): any {
    return visitNonatomic(node, stack, bindings, 3);
  }
  visitModule.fsource = [Symbol.for('define'), [Symbol.for('visit-module'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 3]];
  // `(begin ...)` form.
  function visitBeginP(node: any): any {
    return formp(node, begin_, env);
  }
  visitBeginP.fsource = [Symbol.for('define'), [Symbol.for('visit-begin-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin_'), Symbol.for('env')]];
  function visitBegin(node: any, stack: any, bindings: any): any {
    return visitNonatomic(node, stack, bindings, 1);
  }
  visitBegin.fsource = [Symbol.for('define'), [Symbol.for('visit-begin'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
  // `(begin0 ...)` form.
  function visitBegin0P(node: any): any {
    return formp(node, begin0_, env);
  }
  visitBegin0P.fsource = [Symbol.for('define'), [Symbol.for('visit-begin0-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin0_'), Symbol.for('env')]];
  const visitBegin0: any = visitBegin;
  // `(let ...)` form.
  function visitLetP(node: any): any {
    return formp(node, letStar_, env);
  }
  visitLetP.fsource = [Symbol.for('define'), [Symbol.for('visit-let-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-star_'), Symbol.for('env')]];
  function visitLet(node: any, stack: any, bindings: any): any {
    let result: any = node;
    let bindings2: any = extendEnvironment(new LispEnvironment(), bindings);
    const sym: any = node.get(0).getValue();
    const letBindingsEnv: any = node.get(1);
    const body: any = node.drop(2);
    for (let letBinding of letBindingsEnv.getValue()) {
      const bindingSym: any = Array.isArray(letBinding) ? letBinding[0] : letBinding;
      makeTypeBinding(bindings2, bindingSym, Symbol.for('Any'));
    }
    const visitedLetBindingsEnv: any = visitClausesNode(letBindingsEnv, [...stack, node], bindings2);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindings2);
    if (!((letBindingsEnv === visitedLetBindingsEnv) && (body === visitedBody))) {
      result = transferComments(node, makeRose([sym, visitedLetBindingsEnv, ...visitedBody]));
    }
    return f(result, stack, bindings);
  }
  visitLet.fsource = [Symbol.for('define'), [Symbol.for('visit-let'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings-env'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('for'), [[Symbol.for('let-binding'), [Symbol.for('send'), Symbol.for('let-bindings-env'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('binding-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('let-binding')], [Symbol.for('first'), Symbol.for('let-binding')], Symbol.for('let-binding')]], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('binding-sym'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('define'), Symbol.for('visited-let-bindings-env'), [Symbol.for('visit-clauses-node'), Symbol.for('let-bindings-env'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings-env'), Symbol.for('visited-let-bindings-env')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings-env')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  function visitLetValuesP(node: any): any {
    return formp(node, letValues_, env);
  }
  visitLetValuesP.fsource = [Symbol.for('define'), [Symbol.for('visit-let-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-values_'), Symbol.for('env')]];
  function visitLetValues(node: any, stack: any, bindings: any): any {
    let result: any = node;
    let bindings2: any = extendEnvironment(new LispEnvironment(), bindings);
    const sym: any = node.get(0).getValue();
    const letBindingsEnv: any = node.get(1);
    const body: any = node.drop(2);
    const visitedLetBindingsEnv: any = visitFormsNodeWith(function (x: any): any {
      let xResult: any = x;
      const ids: any = x.get(0);
      let val: any = x.get(1);
      const idsExp: any = ids.getValue();
      if (typeof idsExp === 'symbol') {
        makeTypeBinding(bindings2, idsExp, Symbol.for('Any'));
      } else {
        for (let letBinding of idsExp) {
          if (typeof letBinding === 'symbol') {
            makeTypeBinding(bindings2, letBinding, Symbol.for('Any'));
          }
        }
      }
      const visitedIds: any = visitFormsNode(ids, [...stack, node], bindings2);
      const visitedVal: any = visit(visitor, val, [...stack, node], bindings2);
      if (!((visitedIds === ids) && (visitedVal === val))) {
        xResult = transferComments(x, makeRose([visitedIds, visitedVal]));
      }
      return xResult;
    }, letBindingsEnv, [...stack, node], bindings2);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindings2);
    if (!((letBindingsEnv === visitedLetBindingsEnv) && (body === visitedBody))) {
      result = transferComments(node, makeRose([sym, visitedLetBindingsEnv, ...visitedBody]));
    }
    return f(result, stack, bindings);
  }
  visitLetValues.fsource = [Symbol.for('define'), [Symbol.for('visit-let-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings-env'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-let-bindings-env'), [Symbol.for('visit-forms-node-with'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('x-result'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('ids'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('ids-exp'), [Symbol.for('send'), Symbol.for('ids'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ids-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('ids-exp'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('let-binding'), Symbol.for('ids-exp')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('let-binding')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('let-binding'), [Symbol.for('quote'), Symbol.for('Any')]]]]]], [Symbol.for('define'), Symbol.for('visited-ids'), [Symbol.for('visit-forms-node'), Symbol.for('ids'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('val'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('visited-ids'), Symbol.for('ids')], [Symbol.for('eq?'), Symbol.for('visited-val'), Symbol.for('val')]], [Symbol.for('set!'), Symbol.for('x-result'), [Symbol.for('transfer-comments'), Symbol.for('x'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('visited-ids')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], Symbol.for('x-result')], Symbol.for('let-bindings-env'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings-env'), Symbol.for('visited-let-bindings-env')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings-env')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(for ...)` form.
  function visitForP(node: any): any {
    return formp(node, for_, env);
  }
  visitForP.fsource = [Symbol.for('define'), [Symbol.for('visit-for-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('for_'), Symbol.for('env')]];
  const visitFor: any = visitLet;
  // `(while ...)` form.
  function visitWhileP(node: any): any {
    return formp(node, jsWhile_, env);
  }
  visitWhileP.fsource = [Symbol.for('define'), [Symbol.for('visit-while-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-while_'), Symbol.for('env')]];
  const visitWhile: any = visitFunctionCall;
  // `(cond ...)` form.
  function visitCondP(node: any): any {
    return formp(node, cond_, env);
  }
  visitCondP.fsource = [Symbol.for('define'), [Symbol.for('visit-cond-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('cond_'), Symbol.for('env')]];
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
  visitCond.fsource = [Symbol.for('define'), [Symbol.for('visit-cond'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('clauses'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('visited-clauses'), [Symbol.for('visit-clauses-list'), Symbol.for('clauses'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('visited-clauses'), Symbol.for('clauses')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote-splicing'), Symbol.for('visited-clauses')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(lambda ...)` form.
  function visitLambdaP(node: any): any {
    return formp(node, lambda_, env) || formp(node, jsFunction_, env) || formp(node, jsArrow_, env);
  }
  visitLambdaP.fsource = [Symbol.for('define'), [Symbol.for('visit-lambda-p'), Symbol.for('node')], [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('node'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-function_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-arrow_'), Symbol.for('env')]]];
  function visitLambda(node: any, stack: any, bindings: any): any {
    let result: any = node;
    let bindings2: any = extendEnvironment(new LispEnvironment(), bindings);
    const sym: any = node.get(0).getValue();
    let params: any = node.get(1);
    const paramsExp: any = params.getValue();
    const body: any = node.drop(2);
    if (typeof paramsExp === 'symbol') {
      makeTypeBinding(bindings2, paramsExp, Symbol.for('Any'));
    } else {
      for (let param of paramsExp) {
        if (Array.isArray(param)) {
          param = param[0];
        }
        makeTypeBinding(bindings2, param, Symbol.for('Any'));
      }
    }
    const visitedParams: any = visitClausesNode(params, [...stack, node], bindings2);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindings2);
    if (!((params === visitedParams) && (body === visitedBody))) {
      result = transferComments(node, makeRose([sym, visitedParams, ...visitedBody]));
    }
    return f(result, stack, bindings);
  }
  visitLambda.fsource = [Symbol.for('define'), [Symbol.for('visit-lambda'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params-exp'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), Symbol.for('params-exp')]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('param'), [Symbol.for('quote'), Symbol.for('Any')]]]]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-clauses-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(define ...)` form.
  function visitDefineP(node: any): any {
    return formp(node, define_, env);
  }
  visitDefineP.fsource = [Symbol.for('define'), [Symbol.for('visit-define-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define_'), Symbol.for('env')]];
  function visitDefine(node: any, stack: any, bindings: any): any {
    let result: any = node;
    const defineSym: any = node.get(0).getValue();
    let id: any = node.get(1);
    const idExp: any = id.getValue();
    const idSym: any = Array.isArray(idExp) ? idExp[0] : idExp;
    let bindings2: any = bindings;
    if (Array.isArray(idExp)) {
      makeTypeBinding(bindings, idSym, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]);
      for (let param of idExp.slice(1)) {
        if (Array.isArray(param)) {
          param = param[0];
        }
        makeTypeBinding(bindings, param, Symbol.for('Any'));
      }
      bindings2 = extendEnvironment(new LispEnvironment(), bindings);
    } else {
      makeTypeBinding(bindings, idSym, Symbol.for('Any'));
    }
    const body: any = node.drop(2);
    const visitedId: any = Array.isArray(idExp) ? visitClausesNode(id, [...stack, node], bindings2) : visitNode(id, [...stack, node], bindings2);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindings2);
    if (!((id === visitedId) && (body === visitedBody))) {
      result = transferComments(node, makeRose([defineSym, visitedId, ...visitedBody]));
    }
    return f(result, stack, bindings);
  }
  visitDefine.fsource = [Symbol.for('define'), [Symbol.for('visit-define'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-exp'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('first'), Symbol.for('id-exp')], Symbol.for('id-exp')]], [Symbol.for('define'), Symbol.for('bindings-2'), Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('rest'), Symbol.for('id-exp')]]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('param'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('set!'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]]], [Symbol.for('else'), [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), Symbol.for('Any')]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('visit-clauses-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')], [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(define-values ...)` form.
  function visitDefineValuesP(node: any): any {
    return formp(node, defineValues_, env);
  }
  visitDefineValuesP.fsource = [Symbol.for('define'), [Symbol.for('visit-define-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-values_'), Symbol.for('env')]];
  function visitDefineValues(node: any, stack: any, bindings: any): any {
    return visitFormsNode(node, stack, bindings, 2);
  }
  visitDefineValues.fsource = [Symbol.for('define'), [Symbol.for('visit-define-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 2]];
  // `(defmacro ...)` form.
  function visitDefmacroP(node: any): any {
    return formp(node, defmacro_, env);
  }
  visitDefmacroP.fsource = [Symbol.for('define'), [Symbol.for('visit-defmacro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('defmacro_'), Symbol.for('env')]];
  function visitDefmacro(node: any, stack: any, bindings: any): any {
    let result: any = node;
    const defmacroSym: any = node.get(0).getValue();
    let id: any = node.get(1);
    const idSym: any = id.getValue();
    let params: any = node.get(2);
    const paramsExp: any = params.getValue();
    const body: any = node.drop(3);
    makeTypeBinding(bindings, idSym, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]);
    let bindings2: any = extendEnvironment(new LispEnvironment(), bindings);
    if (typeof paramsExp === 'symbol') {
      makeTypeBinding(bindings2, paramsExp, Symbol.for('Any'));
    } else {
      for (let param of flatten_(paramsExp)) {
        makeTypeBinding(bindings2, params, Symbol.for('Any'));
      }
    }
    const visitedId: any = visitNode(id, [...stack, node], bindings2);
    const visitedParams: any = visitFormsNode(params, [...stack, node], bindings2);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindings2);
    if (!((id === visitedId) && (params === visitedParams) && (body === visitedBody))) {
      result = transferComments(node, makeRose([defmacroSym, visitedId, visitedParams, ...visitedBody]));
    }
    result = f(result, stack, bindings);
    makeTypeBinding(bindings, idSym, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]);
    return result;
  }
  visitDefmacro.fsource = [Symbol.for('define'), [Symbol.for('visit-defmacro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('defmacro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params-exp'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params'), [Symbol.for('quote'), Symbol.for('Any')]]]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('defmacro-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], Symbol.for('result')];
  // `(define-macro ...)` form.
  function visitDefineMacroP(node: any): any {
    return formp(node, defineMacro_, env);
  }
  visitDefineMacroP.fsource = [Symbol.for('define'), [Symbol.for('visit-define-macro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-macro_'), Symbol.for('env')]];
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
    makeTypeBinding(bindings, idSym, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]);
    let bindings2: any = extendEnvironment(new LispEnvironment(), bindings);
    if (typeof paramsExp === 'symbol') {
      makeTypeBinding(bindings2, paramsExp, Symbol.for('Any'));
    } else {
      for (let param of flatten_(paramsExp)) {
        makeTypeBinding(bindings2, params, Symbol.for('Any'));
      }
    }
    const visitedId: any = visitNode(id, [...stack, node], bindings2);
    const visitedParams: any = visitFormsNode(params, [...stack, node], bindings2);
    const visitedBody: any = visitFormsList(body, [...stack, node], bindings2);
    if (!((id === visitedId) && (params === visitedParams) && (body === visitedBody))) {
      result = transferComments(node, makeRose([defineMacroSym, cons(visitedId, visitedParams), ...visitedBody]));
    }
    result = f(result, stack, bindings);
    makeTypeBinding(bindings, idSym, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]);
    return result;
  }
  visitDefineMacro.fsource = [Symbol.for('define'), [Symbol.for('visit-define-macro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-macro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('name-and-args-exp'), [Symbol.for('send'), Symbol.for('name-and-args'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('car'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('make-rose'), Symbol.for('id-sym'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('cdr'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('make-rose'), Symbol.for('params-exp'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params-exp'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params'), [Symbol.for('quote'), Symbol.for('Any')]]]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-macro-sym')], [Symbol.for('unquote'), [Symbol.for('cons'), Symbol.for('visited-id'), Symbol.for('visited-params')]], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], Symbol.for('result')];
  // `(define-class ...)` form.
  function visitDefineClassP(node: any): any {
    return formp(node, class_, env);
  }
  visitDefineClassP.fsource = [Symbol.for('define'), [Symbol.for('visit-define-class-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('class_'), Symbol.for('env')]];
  const visitDefineClass: any = visitFunctionCall;
  // `(ann ...)` form.
  function visitAnnP(node: any): any {
    return formp(node, ann_, env);
  }
  visitAnnP.fsource = [Symbol.for('define'), [Symbol.for('visit-ann-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('ann_'), Symbol.for('env')]];
  function visitAnn(node: any, stack: any, bindings: any): any {
    return visitNode(node, stack, bindings);
  }
  visitAnn.fsource = [Symbol.for('define'), [Symbol.for('visit-ann'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
  // `(and ...)` form.
  function visitAndP(node: any): any {
    return formp(node, and_, env);
  }
  visitAndP.fsource = [Symbol.for('define'), [Symbol.for('visit-and-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('and_'), Symbol.for('env')]];
  const visitAnd: any = visitFunctionCall;
  // `(or ...)` form.
  function visitOrP(node: any): any {
    return formp(node, or_, env);
  }
  visitOrP.fsource = [Symbol.for('define'), [Symbol.for('visit-or-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('or_'), Symbol.for('env')]];
  const visitOr: any = visitFunctionCall;
  // `(when ...)` form.
  function visitWhenP(node: any): any {
    return formp(node, when_, env);
  }
  visitWhenP.fsource = [Symbol.for('define'), [Symbol.for('visit-when-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('when_'), Symbol.for('env')]];
  function visitWhen(node: any, stack: any, bindings: any): any {
    return visitNonatomic(node, stack, bindings, 1);
  }
  visitWhen.fsource = [Symbol.for('define'), [Symbol.for('visit-when'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
  // `(unless ...)` form.
  function visitUnlessP(node: any): any {
    return formp(node, unless_, env);
  }
  visitUnlessP.fsource = [Symbol.for('define'), [Symbol.for('visit-unless-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('unless_'), Symbol.for('env')]];
  function visitUnless(node: any, stack: any, bindings: any): any {
    return visitNonatomic(node, stack, bindings, 1);
  }
  visitUnless.fsource = [Symbol.for('define'), [Symbol.for('visit-unless'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
  // `(new ...)` form.
  function visitNewP(node: any): any {
    return formp(node, new_, env);
  }
  visitNewP.fsource = [Symbol.for('define'), [Symbol.for('visit-new-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('new_'), Symbol.for('env')]];
  const visitNew: any = visitFunctionCall;
  // `(return ...)` form.
  function visitReturnP(node: any): any {
    return formp(node, return_, env);
  }
  visitReturnP.fsource = [Symbol.for('define'), [Symbol.for('visit-return-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('return_'), Symbol.for('env')]];
  const visitReturn: any = visitFunctionCall;
  // `(send ...)` form.
  function visitSendP(node: any): any {
    return formp(node, send_, env);
  }
  visitSendP.fsource = [Symbol.for('define'), [Symbol.for('visit-send-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('send_'), Symbol.for('env')]];
  const visitSend: any = visitFunctionCall;
  // `(set! ...)` form.
  function visitSetqP(node: any): any {
    return formp(node, setX_, env);
  }
  visitSetqP.fsource = [Symbol.for('define'), [Symbol.for('visit-setq-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set!_'), Symbol.for('env')]];
  const visitSetq: any = visitFunctionCall;
  // `(set-field! ...)` form.
  function visitSetFieldP(node: any): any {
    return formp(node, setField_, env);
  }
  visitSetFieldP.fsource = [Symbol.for('define'), [Symbol.for('visit-set-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set-field_'), Symbol.for('env')]];
  const visitSetField: any = visitFunctionCall;
  // `(get-field ...)` form.
  function visitGetFieldP(node: any): any {
    return formp(node, getField_, env);
  }
  visitGetFieldP.fsource = [Symbol.for('define'), [Symbol.for('visit-get-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('get-field_'), Symbol.for('env')]];
  const visitGetField: any = visitFunctionCall;
  // Quoted value.
  function visitQuoteP(node: any): any {
    return formp(node, quote_, env);
  }
  visitQuoteP.fsource = [Symbol.for('define'), [Symbol.for('visit-quote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quote_'), Symbol.for('env')]];
  const visitQuote: any = visitNode;
  // Quasiquoted value.
  function visitQuasiquoteP(node: any): any {
    return formp(node, quasiquote_, env);
  }
  visitQuasiquoteP.fsource = [Symbol.for('define'), [Symbol.for('visit-quasiquote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quasiquote_'), Symbol.for('env')]];
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
    visitQuasiquoteForm.fsource = [Symbol.for('define'), [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('quasiquote-visitor'), Symbol.for('val'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('visited-val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]];
    function visitUnquoteP(node: any): any {
      return taggedListP(node, Symbol.for('unquote'));
    }
    visitUnquoteP.fsource = [Symbol.for('define'), [Symbol.for('visit-unquote-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote')]]];
    function visitUnquote(node: any, stack: any): any {
      // When visiting unquoted expressions,
      // use the regular visitor.
      return visitFormsNodeWith(visitor, node, stack, bindings, 1);
    }
    visitUnquote.fsource = [Symbol.for('define'), [Symbol.for('visit-unquote'), Symbol.for('node'), Symbol.for('stack')], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]];
    function visitUnquoteSplicingP(node: any): any {
      return taggedListP(node, Symbol.for('unquote-splicing'));
    }
    visitUnquoteSplicingP.fsource = [Symbol.for('define'), [Symbol.for('visit-unquote-splicing-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]]];
    const visitUnquoteSplicing: any = visitUnquote;
    function visitQuotedList(node: any, stack: any, bindings: any): any {
      return visitFormsNodeWith(quasiquoteVisitor, node, stack, bindings);
    }
    visitQuotedList.fsource = [Symbol.for('define'), [Symbol.for('visit-quoted-list'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node-with'), Symbol.for('quasiquote-visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];
    const quasiquoteVisitor: any = makeVisitor([[visitUnquoteP, visitUnquote], [visitUnquoteSplicingP, visitUnquoteSplicing], [visitNonatomicP, visitQuotedList], [visitElseP, skipNode]]);
    return visitQuasiquoteForm(node, [...stack, node], bindings);
  }
  visitQuasiquote.fsource = [Symbol.for('define'), [Symbol.for('visit-quasiquote'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('quasiquote-visitor'), Symbol.for('val'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('visited-val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-unquote-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote')]]], [Symbol.for('define'), [Symbol.for('visit-unquote'), Symbol.for('node'), Symbol.for('stack')], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-unquote-splicing-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]]], [Symbol.for('define'), Symbol.for('visit-unquote-splicing'), Symbol.for('visit-unquote')], [Symbol.for('define'), [Symbol.for('visit-quoted-list'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node-with'), Symbol.for('quasiquote-visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('quasiquote-visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('visit-unquote-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote')]], [[Symbol.for('unquote'), Symbol.for('visit-unquote-splicing-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote-splicing')]], [[Symbol.for('unquote'), Symbol.for('visit-nonatomic-p')], [Symbol.for('unquote'), Symbol.for('visit-quoted-list')]], [[Symbol.for('unquote'), Symbol.for('visit-else-p')], [Symbol.for('unquote'), Symbol.for('skip-node')]]]]]], [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]];
  // List.
  function visitNonatomicP(node: any): any {
    let exp: any = node.getValue();
    return Array.isArray(exp);
  }
  visitNonatomicP.fsource = [Symbol.for('define'), [Symbol.for('visit-nonatomic-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('array?'), Symbol.for('exp')]]];
  // Atomic value.
  const visitAtomP: any = visitElseP;
  const visitAtom: any = visitNode;
  // Rename this to `map-visitor` to distinguish it from
  // the `visitor` parameter of many functions.
  const visitor: any = makeVisitor([[visitModuleP, visitModule], [visitBeginP, visitBegin], [visitBegin0P, visitBegin0], [visitLetP, visitLet], [visitLetValuesP, visitLetValues], [visitCondP, visitCond], [visitLambdaP, visitLambda], [visitDefineP, visitDefine], [visitDefineValuesP, visitDefineValues], [visitDefineMacroP, visitDefineMacro], [visitDefmacroP, visitDefmacro], [visitAnnP, visitAnn], [visitAndP, visitAnd], [visitOrP, visitOr], [visitForP, visitFor], [visitWhileP, visitWhile], [visitWhenP, visitWhen], [visitSendP, visitSend], [visitSetqP, visitSetq], [visitSetFieldP, visitSetField], [visitGetFieldP, visitGetField], [visitUnlessP, visitUnless], [visitDefineClassP, visitDefineClass], [visitNewP, visitNew], [visitReturnP, visitReturn], [visitQuoteP, visitQuote], [visitQuasiquoteP, visitQuasiquote], [visitMacroCallP, visitMacroCall], [visitSpecialFormP, visitSpecialForm], [visitFunctionCallP, visitFunctionCall], [visitNonatomicP, visitNonatomic], [visitElseP, visitAtom]]);
  return visit(visitor, node, stack, bindings);
}

mapVisitRose.fsource = [Symbol.for('define'), [Symbol.for('map-visit-rose'), Symbol.for('f'), Symbol.for('node'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('define'), [Symbol.for('skip-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], Symbol.for('node')], [Symbol.for('define'), [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('f'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('visit-forms-node'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-macro-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('macro-call?'), Symbol.for('exp'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('visit-macro-call'), Symbol.for('visit-node')], [Symbol.for('define'), [Symbol.for('visit-special-form-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('special-form?'), Symbol.for('exp'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('visit-special-form'), Symbol.for('visit-node')], [Symbol.for('define'), [Symbol.for('visit-function-call-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('function-call?'), Symbol.for('exp'), Symbol.for('env')]]], [Symbol.for('define'), Symbol.for('visit-function-call'), Symbol.for('visit-nonatomic')], [Symbol.for('define'), [Symbol.for('visit-else-p'), Symbol.for('node')], true], [Symbol.for('define'), [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('exp')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('nodes'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-nodes')]], [Symbol.for('define'), Symbol.for('result-nodes'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('result-nodes'), Symbol.for('nodes')], Symbol.for('node')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('new'), Symbol.for('Rose'), Symbol.for('exp')]]], [Symbol.for('for'), [[Symbol.for('node'), Symbol.for('result-nodes')]], [Symbol.for('push-right!'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]], [Symbol.for('send'), Symbol.for('result'), Symbol.for('insert'), Symbol.for('node')]], Symbol.for('result')]]], [Symbol.for('define'), [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('nodes')], [Symbol.for('return'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('define'), Symbol.for('is-modified'), false], [Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), Symbol.for('skip')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('x1'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('x1')], [Symbol.for('set!'), Symbol.for('is-modified'), true]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('x1')]]], Symbol.for('nodes')]], [Symbol.for('unless'), Symbol.for('is-modified'), [Symbol.for('set!'), Symbol.for('result'), Symbol.for('nodes')]], Symbol.for('result')], [Symbol.for('define'), [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-forms-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visitor'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-clauses-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-node-with'), Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-clauses-list'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), [Symbol.for('skip'), 0]], [Symbol.for('visit-forms-list-with'), Symbol.for('visit-forms-node'), Symbol.for('nodes'), Symbol.for('stack'), Symbol.for('bindings'), Symbol.for('skip')]], [Symbol.for('define'), [Symbol.for('visit-module-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('module_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-module'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 3]], [Symbol.for('define'), [Symbol.for('visit-begin-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-begin'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-begin0-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('begin0_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-begin0'), Symbol.for('visit-begin')], [Symbol.for('define'), [Symbol.for('visit-let-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-star_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-let'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings-env'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('for'), [[Symbol.for('let-binding'), [Symbol.for('send'), Symbol.for('let-bindings-env'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('binding-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('let-binding')], [Symbol.for('first'), Symbol.for('let-binding')], Symbol.for('let-binding')]], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('binding-sym'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('define'), Symbol.for('visited-let-bindings-env'), [Symbol.for('visit-clauses-node'), Symbol.for('let-bindings-env'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings-env'), Symbol.for('visited-let-bindings-env')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings-env')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-let-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('let-values_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-let-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('let-bindings-env'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-let-bindings-env'), [Symbol.for('visit-forms-node-with'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('x-result'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('ids'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('ids-exp'), [Symbol.for('send'), Symbol.for('ids'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('ids-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('ids-exp'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('let-binding'), Symbol.for('ids-exp')]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('let-binding')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('let-binding'), [Symbol.for('quote'), Symbol.for('Any')]]]]]], [Symbol.for('define'), Symbol.for('visited-ids'), [Symbol.for('visit-forms-node'), Symbol.for('ids'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('val'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('visited-ids'), Symbol.for('ids')], [Symbol.for('eq?'), Symbol.for('visited-val'), Symbol.for('val')]], [Symbol.for('set!'), Symbol.for('x-result'), [Symbol.for('transfer-comments'), Symbol.for('x'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('visited-ids')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], Symbol.for('x-result')], Symbol.for('let-bindings-env'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('let-bindings-env'), Symbol.for('visited-let-bindings-env')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-let-bindings-env')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-for-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('for_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-for'), Symbol.for('visit-let')], [Symbol.for('define'), [Symbol.for('visit-while-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-while_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-while'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-cond-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('cond_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-cond'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('clauses'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 1]], [Symbol.for('define'), Symbol.for('visited-clauses'), [Symbol.for('visit-clauses-list'), Symbol.for('clauses'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('visited-clauses'), Symbol.for('clauses')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote-splicing'), Symbol.for('visited-clauses')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-lambda-p'), Symbol.for('node')], [Symbol.for('or'), [Symbol.for('form?'), Symbol.for('node'), Symbol.for('lambda_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-function_'), Symbol.for('env')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('js-arrow_'), Symbol.for('env')]]], [Symbol.for('define'), [Symbol.for('visit-lambda'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params-exp'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), Symbol.for('params-exp')]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('param'), [Symbol.for('quote'), Symbol.for('Any')]]]]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-clauses-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-define-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-define'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-exp'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('first'), Symbol.for('id-exp')], Symbol.for('id-exp')]], [Symbol.for('define'), Symbol.for('bindings-2'), Symbol.for('bindings')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('rest'), Symbol.for('id-exp')]]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('param')], [Symbol.for('set!'), Symbol.for('param'), [Symbol.for('first'), Symbol.for('param')]]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('param'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('set!'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]]], [Symbol.for('else'), [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), Symbol.for('Any')]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('id-exp')], [Symbol.for('visit-clauses-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')], [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-define-values-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-values_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-define-values'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 2]], [Symbol.for('define'), [Symbol.for('visit-defmacro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('defmacro_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-defmacro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('defmacro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('send'), Symbol.for('id'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 2]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('send'), Symbol.for('params'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params-exp'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params'), [Symbol.for('quote'), Symbol.for('Any')]]]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('defmacro-sym')], [Symbol.for('unquote'), Symbol.for('visited-id')], [Symbol.for('unquote'), Symbol.for('visited-params')], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], Symbol.for('result')], [Symbol.for('define'), [Symbol.for('visit-define-macro-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('define-macro_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-define-macro'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('define-macro-sym'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('name-and-args'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('name-and-args-exp'), [Symbol.for('send'), Symbol.for('name-and-args'), Symbol.for('get-value')]], [Symbol.for('define'), Symbol.for('id-sym'), [Symbol.for('car'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('make-rose'), Symbol.for('id-sym'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('params-exp'), [Symbol.for('cdr'), Symbol.for('name-and-args-exp')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('make-rose'), Symbol.for('params-exp'), Symbol.for('name-and-args')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('define'), Symbol.for('bindings-2'), [Symbol.for('extend-environment'), [Symbol.for('new'), Symbol.for('LispEnvironment')], Symbol.for('bindings')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params-exp')], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params-exp'), [Symbol.for('quote'), Symbol.for('Any')]]], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('param'), [Symbol.for('flatten_'), Symbol.for('params-exp')]]], [Symbol.for('make-type-binding'), Symbol.for('bindings-2'), Symbol.for('params'), [Symbol.for('quote'), Symbol.for('Any')]]]]], [Symbol.for('define'), Symbol.for('visited-id'), [Symbol.for('visit-node'), Symbol.for('id'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-params'), [Symbol.for('visit-forms-node'), Symbol.for('params'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('define'), Symbol.for('visited-body'), [Symbol.for('visit-forms-list'), Symbol.for('body'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings-2')]], [Symbol.for('unless'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('id'), Symbol.for('visited-id')], [Symbol.for('eq?'), Symbol.for('params'), Symbol.for('visited-params')], [Symbol.for('eq?'), Symbol.for('body'), Symbol.for('visited-body')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('define-macro-sym')], [Symbol.for('unquote'), [Symbol.for('cons'), Symbol.for('visited-id'), Symbol.for('visited-params')]], [Symbol.for('unquote-splicing'), Symbol.for('visited-body')]]]]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('make-type-binding'), Symbol.for('bindings'), Symbol.for('id-sym'), [Symbol.for('quote'), [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], Symbol.for('result')], [Symbol.for('define'), [Symbol.for('visit-define-class-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('class_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-define-class'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-ann-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('ann_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-ann'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-node'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-and-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('and_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-and'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-or-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('or_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-or'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-when-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('when_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-when'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-unless-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('unless_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-unless'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-nonatomic'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-new-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('new_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-new'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-return-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('return_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-return'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-send-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('send_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-send'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-setq-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set!_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-setq'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-set-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('set-field_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-set-field'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-get-field-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('get-field_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-get-field'), Symbol.for('visit-function-call')], [Symbol.for('define'), [Symbol.for('visit-quote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quote_'), Symbol.for('env')]], [Symbol.for('define'), Symbol.for('visit-quote'), Symbol.for('visit-node')], [Symbol.for('define'), [Symbol.for('visit-quasiquote-p'), Symbol.for('node')], [Symbol.for('form?'), Symbol.for('node'), Symbol.for('quasiquote_'), Symbol.for('env')]], [Symbol.for('define'), [Symbol.for('visit-quasiquote'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 0]], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('visited-val'), [Symbol.for('visit'), Symbol.for('quasiquote-visitor'), Symbol.for('val'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('visited-val')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('transfer-comments'), Symbol.for('node'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('visited-val')]]]]]]], [Symbol.for('f'), Symbol.for('result'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-unquote-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote')]]], [Symbol.for('define'), [Symbol.for('visit-unquote'), Symbol.for('node'), Symbol.for('stack')], [Symbol.for('visit-forms-node-with'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings'), 1]], [Symbol.for('define'), [Symbol.for('visit-unquote-splicing-p'), Symbol.for('node')], [Symbol.for('tagged-list?'), Symbol.for('node'), [Symbol.for('quote'), Symbol.for('unquote-splicing')]]], [Symbol.for('define'), Symbol.for('visit-unquote-splicing'), Symbol.for('visit-unquote')], [Symbol.for('define'), [Symbol.for('visit-quoted-list'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('visit-forms-node-with'), Symbol.for('quasiquote-visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]], [Symbol.for('define'), Symbol.for('quasiquote-visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('visit-unquote-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote')]], [[Symbol.for('unquote'), Symbol.for('visit-unquote-splicing-p')], [Symbol.for('unquote'), Symbol.for('visit-unquote-splicing')]], [[Symbol.for('unquote'), Symbol.for('visit-nonatomic-p')], [Symbol.for('unquote'), Symbol.for('visit-quoted-list')]], [[Symbol.for('unquote'), Symbol.for('visit-else-p')], [Symbol.for('unquote'), Symbol.for('skip-node')]]]]]], [Symbol.for('visit-quasiquote-form'), Symbol.for('node'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('stack')], [Symbol.for('unquote'), Symbol.for('node')]]], Symbol.for('bindings')]], [Symbol.for('define'), [Symbol.for('visit-nonatomic-p'), Symbol.for('node')], [Symbol.for('let'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')]]], [Symbol.for('array?'), Symbol.for('exp')]]], [Symbol.for('define'), Symbol.for('visit-atom-p'), Symbol.for('visit-else-p')], [Symbol.for('define'), Symbol.for('visit-atom'), Symbol.for('visit-node')], [Symbol.for('define'), Symbol.for('visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('visit-module-p')], [Symbol.for('unquote'), Symbol.for('visit-module')]], [[Symbol.for('unquote'), Symbol.for('visit-begin-p')], [Symbol.for('unquote'), Symbol.for('visit-begin')]], [[Symbol.for('unquote'), Symbol.for('visit-begin0-p')], [Symbol.for('unquote'), Symbol.for('visit-begin0')]], [[Symbol.for('unquote'), Symbol.for('visit-let-p')], [Symbol.for('unquote'), Symbol.for('visit-let')]], [[Symbol.for('unquote'), Symbol.for('visit-let-values-p')], [Symbol.for('unquote'), Symbol.for('visit-let-values')]], [[Symbol.for('unquote'), Symbol.for('visit-cond-p')], [Symbol.for('unquote'), Symbol.for('visit-cond')]], [[Symbol.for('unquote'), Symbol.for('visit-lambda-p')], [Symbol.for('unquote'), Symbol.for('visit-lambda')]], [[Symbol.for('unquote'), Symbol.for('visit-define-p')], [Symbol.for('unquote'), Symbol.for('visit-define')]], [[Symbol.for('unquote'), Symbol.for('visit-define-values-p')], [Symbol.for('unquote'), Symbol.for('visit-define-values')]], [[Symbol.for('unquote'), Symbol.for('visit-define-macro-p')], [Symbol.for('unquote'), Symbol.for('visit-define-macro')]], [[Symbol.for('unquote'), Symbol.for('visit-defmacro-p')], [Symbol.for('unquote'), Symbol.for('visit-defmacro')]], [[Symbol.for('unquote'), Symbol.for('visit-ann-p')], [Symbol.for('unquote'), Symbol.for('visit-ann')]], [[Symbol.for('unquote'), Symbol.for('visit-and-p')], [Symbol.for('unquote'), Symbol.for('visit-and')]], [[Symbol.for('unquote'), Symbol.for('visit-or-p')], [Symbol.for('unquote'), Symbol.for('visit-or')]], [[Symbol.for('unquote'), Symbol.for('visit-for-p')], [Symbol.for('unquote'), Symbol.for('visit-for')]], [[Symbol.for('unquote'), Symbol.for('visit-while-p')], [Symbol.for('unquote'), Symbol.for('visit-while')]], [[Symbol.for('unquote'), Symbol.for('visit-when-p')], [Symbol.for('unquote'), Symbol.for('visit-when')]], [[Symbol.for('unquote'), Symbol.for('visit-send-p')], [Symbol.for('unquote'), Symbol.for('visit-send')]], [[Symbol.for('unquote'), Symbol.for('visit-setq-p')], [Symbol.for('unquote'), Symbol.for('visit-setq')]], [[Symbol.for('unquote'), Symbol.for('visit-set-field-p')], [Symbol.for('unquote'), Symbol.for('visit-set-field')]], [[Symbol.for('unquote'), Symbol.for('visit-get-field-p')], [Symbol.for('unquote'), Symbol.for('visit-get-field')]], [[Symbol.for('unquote'), Symbol.for('visit-unless-p')], [Symbol.for('unquote'), Symbol.for('visit-unless')]], [[Symbol.for('unquote'), Symbol.for('visit-define-class-p')], [Symbol.for('unquote'), Symbol.for('visit-define-class')]], [[Symbol.for('unquote'), Symbol.for('visit-new-p')], [Symbol.for('unquote'), Symbol.for('visit-new')]], [[Symbol.for('unquote'), Symbol.for('visit-return-p')], [Symbol.for('unquote'), Symbol.for('visit-return')]], [[Symbol.for('unquote'), Symbol.for('visit-quote-p')], [Symbol.for('unquote'), Symbol.for('visit-quote')]], [[Symbol.for('unquote'), Symbol.for('visit-quasiquote-p')], [Symbol.for('unquote'), Symbol.for('visit-quasiquote')]], [[Symbol.for('unquote'), Symbol.for('visit-macro-call-p')], [Symbol.for('unquote'), Symbol.for('visit-macro-call')]], [[Symbol.for('unquote'), Symbol.for('visit-special-form-p')], [Symbol.for('unquote'), Symbol.for('visit-special-form')]], [[Symbol.for('unquote'), Symbol.for('visit-function-call-p')], [Symbol.for('unquote'), Symbol.for('visit-function-call')]], [[Symbol.for('unquote'), Symbol.for('visit-nonatomic-p')], [Symbol.for('unquote'), Symbol.for('visit-nonatomic')]], [[Symbol.for('unquote'), Symbol.for('visit-else-p')], [Symbol.for('unquote'), Symbol.for('visit-atom')]]]]]], [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('stack'), Symbol.for('bindings')]];

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

mapSexp.fsource = [Symbol.for('define'), [Symbol.for('map-sexp'), Symbol.for('f'), Symbol.for('exp'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]], [Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('bindings'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('let*'), [[Symbol.for('f1'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('stack'), Symbol.for('bindings')], [Symbol.for('let*'), [[Symbol.for('exp'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')]], [Symbol.for('stack1'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Rose')], [Symbol.for('send'), Symbol.for('x'), Symbol.for('get-value')], Symbol.for('x')]], Symbol.for('stack')]], [Symbol.for('result'), [Symbol.for('f'), Symbol.for('exp'), Symbol.for('stack1'), Symbol.for('bindings')]]], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('result'), Symbol.for('exp')], Symbol.for('x'), [Symbol.for('make-rose'), Symbol.for('result'), Symbol.for('x')]]]]], [Symbol.for('is-rose'), [Symbol.for('is-a?'), Symbol.for('exp'), Symbol.for('Rose')]], [Symbol.for('node'), [Symbol.for('if'), Symbol.for('is-rose'), Symbol.for('exp'), [Symbol.for('make-rose'), Symbol.for('exp')]]], [Symbol.for('result'), [Symbol.for('map-rose'), Symbol.for('f1'), Symbol.for('node'), Symbol.for('env'), Symbol.for('stack'), Symbol.for('bindings')]]], [Symbol.for('if'), Symbol.for('is-rose'), Symbol.for('result'), [Symbol.for('send'), Symbol.for('result'), Symbol.for('get-value')]]]];

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

iterateRose.fsource = [Symbol.for('define'), [Symbol.for('iterate-rose'), Symbol.for('f'), Symbol.for('node'), [Symbol.for('env'), [Symbol.for('new'), Symbol.for('LispEnvironment')]]], [Symbol.for('map-rose'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('stack')], [Symbol.for('f'), Symbol.for('x'), Symbol.for('stack')], Symbol.for('x')], Symbol.for('node'), Symbol.for('env')]];

/**
 * Expand an `(ann ...)` expression.
 */
function ann_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

ann_.fsource = [Symbol.for('define'), [Symbol.for('ann_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

ann_.ftype = 'macro';

/**
 * Expand a `(: ...)` expression.
 */
function colon_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

colon_.fsource = [Symbol.for('define'), [Symbol.for('colon_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

colon_.ftype = 'macro';

/**
 * Expand a `(define-type ...)` expression.
 */
function defineType_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineType_.fsource = [Symbol.for('define'), [Symbol.for('define-type_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineType_.ftype = 'macro';

/**
 * Expand a `(let-fields ...)` expression.
 */
function letFields_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

letFields_.fsource = [Symbol.for('define'), [Symbol.for('let-fields_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

letFields_.ftype = 'macro';

/**
 * Expand a `(define-fields ...)` expression.
 */
function defineFields_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

defineFields_.fsource = [Symbol.for('define'), [Symbol.for('define-fields_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

defineFields_.ftype = 'macro';

/**
 * Expand a `(set!-fields ...)` expression.
 */
function setFields_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

setFields_.fsource = [Symbol.for('define'), [Symbol.for('set-fields_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

setFields_.ftype = 'macro';

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
        consequentCompiled = [compileStatementOrReturnStatement(makeRose([Symbol.for('js/block'), ...consequent]), env, options)];
      } else {
        testCompiled = null;
        const consequent: any = x.drop(1);
        consequentCompiled = [compileStatementOrReturnStatement(makeRose([Symbol.for('js/block'), ...consequent]), env, options)];
      }
      return new SwitchCase(testCompiled, consequentCompiled);
    });
    return new SwitchStatement(discriminantCompiled, casesCompiled);
  } else {
    return compileExpression(wrapInArrowCall(node), env, options);
  }
}

compileJsSwitch.fsource = [Symbol.for('define'), [Symbol.for('compile-js-switch'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression-type'), [Symbol.for('oget'), Symbol.for('options'), 'expressionType']], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('expression-type'), 'statement'], [Symbol.for('eq?'), Symbol.for('expression-type'), 'return']], [Symbol.for('define'), Symbol.for('discriminant'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get'), 1]], [Symbol.for('define'), Symbol.for('discriminant-compiled'), [Symbol.for('compile-expression'), Symbol.for('discriminant'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('cases'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 2]], [Symbol.for('define'), Symbol.for('cases-compiled'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('~>'), Symbol.for('x'), [Symbol.for('send'), Symbol.for('get'), 0], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('define'), Symbol.for('test-compiled')], [Symbol.for('define'), Symbol.for('consequent-compiled')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('op'), [Symbol.for('quote'), Symbol.for('case')]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('get'), 1]], [Symbol.for('set!'), Symbol.for('test-compiled'), [Symbol.for('compile-expression'), Symbol.for('test'), Symbol.for('env'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('drop'), 2]], [Symbol.for('set!'), Symbol.for('consequent-compiled'), [Symbol.for('list'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('js/block'), [Symbol.for('unquote-splicing'), Symbol.for('consequent')]]]], Symbol.for('env'), Symbol.for('options')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('test-compiled'), null], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('drop'), 1]], [Symbol.for('set!'), Symbol.for('consequent-compiled'), [Symbol.for('list'), [Symbol.for('compile-statement-or-return-statement'), [Symbol.for('make-rose'), [Symbol.for('quasiquote'), [Symbol.for('js/block'), [Symbol.for('unquote-splicing'), Symbol.for('consequent')]]]], Symbol.for('env'), Symbol.for('options')]]]]], [Symbol.for('new'), Symbol.for('SwitchCase'), Symbol.for('test-compiled'), Symbol.for('consequent-compiled')]], Symbol.for('cases')]], [Symbol.for('new'), Symbol.for('SwitchStatement'), Symbol.for('discriminant-compiled'), Symbol.for('cases-compiled')]], [Symbol.for('else'), [Symbol.for('compile-expression'), [Symbol.for('wrap-in-arrow-call'), Symbol.for('node')], Symbol.for('env'), Symbol.for('options')]]]];

/**
 * Expand a `(js/switch ...)` expression.
 */
function jsSwitch_(exp: any, env: any): any {
  return compileSexp(exp, env, currentCompilationOptions());
}

jsSwitch_.fsource = [Symbol.for('define'), [Symbol.for('js-switch_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('compile-sexp'), Symbol.for('exp'), Symbol.for('env'), [Symbol.for('current-compilation-options')]]];

jsSwitch_.ftype = 'macro';

/**
 * Expand a `(field-bound? ...)` expression.
 */
function fieldBoundP_(exp: any, env: any): any {
  let [id, obj]: any[] = exp.slice(1);
  const prop: any = makeJsIdentifierString(id.description as string, currentCompilationOptions());
  if (typeof obj === 'symbol') {
    return [Symbol.for('and'), obj, [Symbol.for('js/in'), prop, obj]];
  } else {
    const objSym: any = Symbol('obj');
    return [Symbol.for('let'), [[objSym, obj]], [Symbol.for('and'), objSym, [Symbol.for('js/in'), prop, objSym]]];
  }
}

fieldBoundP_.fsource = [Symbol.for('define'), [Symbol.for('field-bound?_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('id'), Symbol.for('obj')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('prop'), [Symbol.for('make-js-identifier-string'), [Symbol.for('symbol->string'), Symbol.for('id')], [Symbol.for('current-compilation-options')]]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('obj')], [Symbol.for('quasiquote'), [Symbol.for('and'), [Symbol.for('unquote'), Symbol.for('obj')], [Symbol.for('js/in'), [Symbol.for('unquote'), Symbol.for('prop')], [Symbol.for('unquote'), Symbol.for('obj')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('obj-sym'), [Symbol.for('gensym'), 'obj']], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('obj-sym')], [Symbol.for('unquote'), Symbol.for('obj')]]], [Symbol.for('and'), [Symbol.for('unquote'), Symbol.for('obj-sym')], [Symbol.for('js/in'), [Symbol.for('unquote'), Symbol.for('prop')], [Symbol.for('unquote'), Symbol.for('obj-sym')]]]]]]]];

fieldBoundP_.ftype = 'macro';

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

callWithCurrentContinuation_.fsource = [Symbol.for('define'), [Symbol.for('call-with-current-continuation_'), Symbol.for('proc'), [Symbol.for('prompt-tag'), undefined]], [Symbol.for('define-class'), Symbol.for('CallCCWrapper'), [], [Symbol.for('define/public'), Symbol.for('value')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('value')], [Symbol.for('set-field!'), Symbol.for('value'), Symbol.for('this'), Symbol.for('value')]]], [Symbol.for('try'), [Symbol.for('return'), [Symbol.for('proc'), [Symbol.for('js/arrow'), [Symbol.for('value')], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('CallCCWrapper'), Symbol.for('value')]]]]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e'), [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('e'), Symbol.for('CallCCWrapper')], [Symbol.for('return'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('e')]]], [Symbol.for('else'), [Symbol.for('throw'), Symbol.for('e')]]]]]];

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

traverseEstree.fsource = [Symbol.for('define'), [Symbol.for('traverse-estree'), Symbol.for('node'), [Symbol.for('enter'), undefined], [Symbol.for('leave'), undefined], [Symbol.for('replace'), undefined]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('el')], [Symbol.for('define'), Symbol.for('el1')], [Symbol.for('define'), Symbol.for('val')], [Symbol.for('define'), Symbol.for('val1')], [Symbol.for('unless'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Node')], [Symbol.for('return'), Symbol.for('result')]], [Symbol.for('when'), Symbol.for('enter'), [Symbol.for('enter'), Symbol.for('node')]], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('js-keys'), Symbol.for('node')]]], [Symbol.for('set!'), Symbol.for('val'), [Symbol.for('oget'), Symbol.for('node'), Symbol.for('key')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('val')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('val')]]]], [Symbol.for('set!'), Symbol.for('el'), [Symbol.for('aget'), Symbol.for('val'), Symbol.for('i')]], [Symbol.for('set!'), Symbol.for('el1'), [Symbol.for('traverse-estree'), Symbol.for('el'), Symbol.for('enter'), Symbol.for('leave'), Symbol.for('replace')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('el'), Symbol.for('el1')], [Symbol.for('list-set!'), Symbol.for('val'), Symbol.for('i'), Symbol.for('el1')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('val1'), [Symbol.for('traverse-estree'), Symbol.for('val'), Symbol.for('enter'), Symbol.for('leave'), Symbol.for('replace')]], [Symbol.for('unless'), [Symbol.for('eq?'), Symbol.for('val'), Symbol.for('val1')], [Symbol.for('oset!'), Symbol.for('node'), Symbol.for('key'), Symbol.for('val1')]]]]], [Symbol.for('when'), Symbol.for('leave'), [Symbol.for('leave'), Symbol.for('node')]], [Symbol.for('when'), Symbol.for('replace'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('replace'), Symbol.for('node')]]], Symbol.for('result')];

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

findEstree.fsource = [Symbol.for('define'), [Symbol.for('find-estree'), Symbol.for('pred'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('nodes'), [Symbol.for('quote'), []]], [Symbol.for('traverse-estree'), Symbol.for('node'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('when'), [Symbol.for('pred'), Symbol.for('x')], [Symbol.for('push-right!'), Symbol.for('nodes'), Symbol.for('x')]]]], Symbol.for('nodes')];

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

optimizeSexp.fsource = [Symbol.for('define'), [Symbol.for('optimize-sexp'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('cond'), [[Symbol.for('rose?'), Symbol.for('exp')], [Symbol.for('optimize-rose'), Symbol.for('exp'), Symbol.for('env')]], [Symbol.for('else'), [Symbol.for('~>'), [Symbol.for('make-rose'), Symbol.for('exp')], [Symbol.for('optimize-rose'), Symbol.for('_'), Symbol.for('env')], [Symbol.for('send'), Symbol.for('_'), Symbol.for('get-value')]]]]];

/**
 * Optimize a rose tree-wrapped S-expression.
 */
function optimizeRose(exp: any, env: any): any {
  return applyOptimizations(exp, env);
}

optimizeRose.fsource = [Symbol.for('define'), [Symbol.for('optimize-rose'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('apply-optimizations'), Symbol.for('exp'), Symbol.for('env')]];

/**
 * Optimize a module.
 */
function optimizeModule(m: any, env: any): any {
  return m.setNodes(m.mainNodes.map(function (x: any): any {
    return optimizeSexp(x, env);
  }));
}

optimizeModule.fsource = [Symbol.for('define'), [Symbol.for('optimize-module'), Symbol.for('m'), Symbol.for('env')], [Symbol.for('send'), Symbol.for('m'), Symbol.for('set-nodes'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('optimize-sexp'), Symbol.for('x'), Symbol.for('env')]], [Symbol.for('get-field'), Symbol.for('main-nodes'), Symbol.for('m')]]]];

/**
 * Optimize an ESTree tree.
 */
function optimizeEstree(exp: any): any {
  return letVarsToConstVars(exp);
}

optimizeEstree.fsource = [Symbol.for('define'), [Symbol.for('optimize-estree'), Symbol.for('exp')], [Symbol.for('~>'), Symbol.for('exp'), [Symbol.for('let-vars-to-const-vars')]]];

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

letVarsToConstVars.fsource = [Symbol.for('define'), [Symbol.for('let-vars-to-const-vars'), Symbol.for('program')], [Symbol.for('define'), Symbol.for('variables'), [Symbol.for('quote'), []]], [Symbol.for('traverse-estree'), Symbol.for('program'), [Symbol.for('lambda'), [Symbol.for('node')], [Symbol.for('define'), Symbol.for('var-names'), [Symbol.for('quote'), []]], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('node'), 'AssignmentExpression'], [Symbol.for('cond'), [[Symbol.for('estree-type?'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')], 'Identifier'], [Symbol.for('push!'), Symbol.for('var-names'), [Symbol.for('get-field'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]]]], [[Symbol.for('estree-type?'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')], 'ArrayPattern'], [Symbol.for('for'), [[Symbol.for('element'), [Symbol.for('get-field'), Symbol.for('elements'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('element'), [Symbol.for('estree-type?'), Symbol.for('element'), 'Identifier']], [Symbol.for('push!'), Symbol.for('var-names'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('element')]]]]]]], [[Symbol.for('estree-type?'), Symbol.for('node'), 'UpdateExpression'], [Symbol.for('when'), [Symbol.for('estree-type?'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], 'Identifier'], [Symbol.for('push!'), Symbol.for('var-names'), [Symbol.for('get-field'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]]]]]], [Symbol.for('for'), [[Symbol.for('var-name'), Symbol.for('var-names')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('var-name'), Symbol.for('variables')], [Symbol.for('push!'), Symbol.for('variables'), Symbol.for('var-name')]]]]], [Symbol.for('traverse-estree'), Symbol.for('program'), undefined, undefined, [Symbol.for('lambda'), [Symbol.for('node')], [Symbol.for('cond'), [[Symbol.for('estree-type?'), Symbol.for('node'), 'VariableDeclaration'], [Symbol.for('unless'), [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('x')]], [Symbol.for('not'), [Symbol.for('zero?'), [Symbol.for('array-list-length'), [Symbol.for('find-estree'), [Symbol.for('lambda'), [Symbol.for('y')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('y'), 'Identifier'], [Symbol.for('memq?'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('y')], Symbol.for('variables')]]], [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('x')]]]]]]], [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('node')]], [Symbol.for('set-field!'), Symbol.for('kind'), Symbol.for('node'), 'const']], Symbol.for('node')], [Symbol.for('else'), Symbol.for('node')]]]]];

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

findOptimization.fsource = [Symbol.for('define'), [Symbol.for('find-optimization'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('rules'), Symbol.for('optimizations')]], [Symbol.for('for'), [[Symbol.for('rule'), Symbol.for('rules')]], [Symbol.for('define-values'), [Symbol.for('predicate')], Symbol.for('rule')], [Symbol.for('when'), [Symbol.for('predicate'), Symbol.for('node'), Symbol.for('env')], [Symbol.for('return'), Symbol.for('rule')]]], false];

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

applyOptimizations.fsource = [Symbol.for('define'), [Symbol.for('apply-optimizations'), Symbol.for('node'), Symbol.for('env'), [Symbol.for('rules'), Symbol.for('optimizations')]], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('rule'), false], [Symbol.for('while'), [Symbol.for('set!'), Symbol.for('rule'), [Symbol.for('find-optimization'), Symbol.for('result'), Symbol.for('env'), Symbol.for('rules')]], [Symbol.for('define-values'), [Symbol.for('predicate'), Symbol.for('optimizer')], Symbol.for('rule')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('optimizer'), Symbol.for('result'), Symbol.for('env')]]], Symbol.for('result')];

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

  interpretationEnvironment: any;

  moduleMap: any;

  symbolMap: any = new Map();

  constructor(nodes: any = [], parent: any = langEnvironment, name: any = '') {
    this.parentEnvironment = parent;
    this.name = name;
    this.initializeNodes(nodes);
  }

  getContinuationEnv(): any {
    return new LispEnvironment([], this.getEnvironment());
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
        })() : exp[1]) ? ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
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
        })() : exp[1])[0] : ((Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
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
    const moduleInterpretationEnv: any = new EnvironmentStack(moduleEnv, jsEnvironment);
    let imported: any;
    let local: any;
    let module: any;
    let env: any;
    let moduleName: any;
    this.parentEnvironment = parent;
    this.environment = moduleEnv;
    this.interpretationEnvironment = moduleInterpretationEnv;
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
          makeTypeBinding(moduleEnv, imported, Symbol.for('Any'));
          if (env) {
            const [f, fType]: any[] = env.getTypedValue(local);
            if (!undefinedTypeP(fType)) {
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
        const typ: any = macroDefinitionP(exp) ? [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')] : [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')];
        moduleEnv.setLocal(name, thunk(function (): any {
          let result: any = undefined;
          try {
            result = eval_([Symbol.for('begin'), exp, name], moduleInterpretationEnv);
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

makeModuleMap.fsource = [Symbol.for('define'), [Symbol.for('make-module-map'), Symbol.for('module-expression-map'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('module-map'), [Symbol.for('new'), Symbol.for('ThunkedMap')]], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('send'), Symbol.for('module-expression-map'), Symbol.for('keys')]]], [Symbol.for('send'), Symbol.for('module-map'), Symbol.for('set'), Symbol.for('key'), [Symbol.for('thunk'), [Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('val'), [Symbol.for('send'), Symbol.for('module-expression-map'), Symbol.for('get'), Symbol.for('key')]], [Symbol.for('define'), Symbol.for('m'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('val'), Symbol.for('Module')], Symbol.for('val'), [Symbol.for('module-expression-to-module-object'), Symbol.for('val'), Symbol.for('env')]]], [Symbol.for('send'), Symbol.for('m'), Symbol.for('set-module-map'), Symbol.for('module-map')], Symbol.for('m')]]]], Symbol.for('module-map')];

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

moduleExpressionToModuleObject.fsource = [Symbol.for('define'), [Symbol.for('module-expression-to-module-object'), Symbol.for('node'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('send'), Symbol.for('get'), 1], [Symbol.for('send'), Symbol.for('get-value')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('name')], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('symbol->string'), Symbol.for('name')]]], [Symbol.for('new'), Symbol.for('Module'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('drop'), 3], Symbol.for('env'), Symbol.for('name')]];

/**
 * Whether `env` extends the Lisp environment.
 */
function extendsLispEnvironmentP(env: any): any {
  // TODO: Check `parent`.
  return (env === lispEnvironment) || ((env instanceof EnvironmentStack) && env.hasEnvironment(lispEnvironment));
}

extendsLispEnvironmentP.fsource = [Symbol.for('define'), [Symbol.for('extends-lisp-environment?'), Symbol.for('env')], [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('env'), Symbol.for('lisp-environment')], [Symbol.for('and'), [Symbol.for('is-a?'), Symbol.for('env'), Symbol.for('EnvironmentStack')], [Symbol.for('send'), Symbol.for('env'), Symbol.for('has-environment'), Symbol.for('lisp-environment')]]]];

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

getModuleName.fsource = [Symbol.for('define'), [Symbol.for('get-module-name'), Symbol.for('name-obj')], [Symbol.for('define'), Symbol.for('name'), Symbol.for('name-obj')], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('name')], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('symbol->string'), Symbol.for('name')]]], [Symbol.for('set!'), Symbol.for('name'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^\\./'], Symbol.for('name'), '']], Symbol.for('name')];

/**
 * Return the current environment.
 */
function currentCompilationOptions(): any {
  return currentCompilationOptionsPointer;
}

currentCompilationOptions.fsource = [Symbol.for('define'), [Symbol.for('current-compilation-options')], Symbol.for('current-compilation-options-pointer')];

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

withCompilationOptions.fsource = [Symbol.for('define'), [Symbol.for('with-compilation-options'), Symbol.for('options'), Symbol.for('f')], [Symbol.for('let'), [[Symbol.for('result'), undefined], [Symbol.for('tmp'), Symbol.for('current-compilation-options-pointer')]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('current-compilation-options-pointer'), Symbol.for('options')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f')]], [Symbol.for('finally'), [Symbol.for('set!'), Symbol.for('current-compilation-options-pointer'), Symbol.for('tmp')]]], Symbol.for('result')]];

/**
 * Whether an expression is a definition.
 */
function definitionp(exp: any): any {
  return taggedListP(exp, Symbol.for('define'));
}

definitionp.fsource = [Symbol.for('define'), [Symbol.for('definition?'), Symbol.for('exp')], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define')]]];

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

functionDefinitionP.fsource = [Symbol.for('define'), [Symbol.for('function-definition?'), Symbol.for('exp')], [Symbol.for('and'), [Symbol.for('definition?'), Symbol.for('exp')], [Symbol.for('or'), [Symbol.for('cons?'), [Symbol.for('second'), Symbol.for('exp')]], [Symbol.for('function-expression?'), [Symbol.for('third'), Symbol.for('exp')]]]]];

/**
 * Whether an expression is a function expression.
 */
function functionExpressionP(exp: any): any {
  return taggedListP(exp, Symbol.for('lambda')) || taggedListP(exp, Symbol.for('js/function')) || taggedListP(exp, Symbol.for('js/arrow'));
}

functionExpressionP.fsource = [Symbol.for('define'), [Symbol.for('function-expression?'), Symbol.for('exp')], [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('lambda')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('js/function')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('js/arrow')]]]];

/**
 * Whether an expression is a macro definition.
 */
function macroDefinitionP(exp: any): any {
  return taggedListP(exp, Symbol.for('define-macro')) || taggedListP(exp, Symbol.for('defmacro'));
}

macroDefinitionP.fsource = [Symbol.for('define'), [Symbol.for('macro-definition?'), Symbol.for('exp')], [Symbol.for('or'), [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('define-macro')]], [Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('defmacro')]]]];

/**
 * Parse a parameter list into regular parameters
 * and rest parameter, if any.
 */
function parseParamsList(params: any): any {
  let regularParams: any = [];
  let restParam: any = undefined;
  if (typeof params === 'symbol') {
    restParam = params;
  } else if (Array.isArray(params) && (params.length >= 3) && (params[params.length - 2] === Symbol.for('.')) && !((): any => {
    const x: any = lastCdr(params);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    regularParams = linkedListDropRight_(params, 1);
    restParam = params[params.length - 1];
  } else {
    regularParams = params;
  }
  return [regularParams, restParam];
}

parseParamsList.fsource = [Symbol.for('define'), [Symbol.for('parse-params-list'), Symbol.for('params')], [Symbol.for('define'), Symbol.for('regular-params'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('rest-param'), undefined], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('params')], [Symbol.for('set!'), Symbol.for('rest-param'), Symbol.for('params')]], [[Symbol.for('dotted-list?'), Symbol.for('params')], [Symbol.for('set!'), Symbol.for('regular-params'), [Symbol.for('linked-list-drop-right_'), Symbol.for('params'), 1]], [Symbol.for('set!'), Symbol.for('rest-param'), [Symbol.for('dotted-list-tail'), Symbol.for('params')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('regular-params'), Symbol.for('params')]]], [Symbol.for('values'), Symbol.for('regular-params'), Symbol.for('rest-param')]];

/**
 * Make a type binding for `sym` in `env`,
 * which should be a typed environment.
 */
function makeTypeBinding(env: any, sym: any, typ: any, filter: any = undefined): any {
  if (env.has(sym, {
    filter: filter
  })) {
    return env.setType(sym, typ);
  } else {
    return env.setLocal(sym, undefined, typ);
  }
}

makeTypeBinding.fsource = [Symbol.for('define'), [Symbol.for('make-type-binding'), Symbol.for('env'), Symbol.for('sym'), Symbol.for('typ'), [Symbol.for('filter'), undefined]], [Symbol.for('cond'), [[Symbol.for('send'), Symbol.for('env'), Symbol.for('has'), Symbol.for('sym'), [Symbol.for('js-obj'), 'filter', Symbol.for('filter')]], [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-type'), Symbol.for('sym'), Symbol.for('typ')]], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('env'), Symbol.for('set-local'), Symbol.for('sym'), undefined, Symbol.for('typ')]]]];

/**
 * Lisp environment.
 */
const lispEnvironment: any = new LispEnvironment([[Symbol.for('_'), __, Symbol.for('Any')], [Symbol.for('__'), __, Symbol.for('Any')], [Symbol.for('#f'), false_, Symbol.for('Any')], [Symbol.for('#t'), true_, Symbol.for('Any')], [Symbol.for('#n'), jsNull_, Symbol.for('Any')], [Symbol.for('#u'), undefined_, Symbol.for('Any')], [Symbol.for('false'), false_, Symbol.for('Any')], [Symbol.for('nil'), null_, Symbol.for('Any')], [Symbol.for('null'), null_, Symbol.for('Any')], [Symbol.for('js/null'), jsNull_, Symbol.for('Any')], [Symbol.for('js-null'), jsNull_, Symbol.for('Any')], [Symbol.for('t'), true_, Symbol.for('Any')], [Symbol.for('true'), true_, Symbol.for('Any')], [Symbol.for('js-undefined'), undefined_, Symbol.for('Any')], [Symbol.for('js/undefined'), undefined_, Symbol.for('Any')], [Symbol.for('undefined'), undefined_, Symbol.for('Any')], [Symbol.for('*cons-dot*'), consDot_, Symbol.for('Any')], [Symbol.for('license'), license, Symbol.for('Any')], [Symbol.for('$'), funcall_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('%'), modulo_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('*'), mul_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('+'), add_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('-'), sub_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('/'), div_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('<'), lt_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('<='), lte_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('='), eqp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('=?'), eqp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('>'), gt_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('>='), gte_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('add'), add_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('add1'), add1_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('aget'), arrayRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('append'), append_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('apply'), apply_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('aref'), arrayRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-drop'), arrayDrop_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-drop-right'), arrayDropRight_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-eighth'), arrayEighth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-fifth'), arrayFifth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-first'), arrayFirst_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-fourth'), arrayFourth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-get'), arrayRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-last'), arrayLast_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-length'), arrayLength_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list->linked-list'), arrayListToLinkedList_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-car'), car_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-cdr'), arrayListCdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-drop'), arrayListDrop_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-drop-right'), arrayListDropRight_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-eighth'), arrayListEighth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-fifth'), arrayListFifth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-first'), arrayListFirst_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-fourth'), arrayListFourth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-last'), arrayListLast_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-length'), arrayListLength_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-ninth'), arrayListNinth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-nth'), arrayListNth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-nthcdr'), arrayListNthcdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-rest'), arrayListRest_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-reverse'), arrayListReverse_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-second'), arrayListSecond_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-seventh'), arrayListSeventh_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-sixth'), arrayListSixth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-take'), arrayListTake_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-tenth'), arrayListTenth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list-third'), arrayListThird_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-list?'), arrayListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-ninth'), arrayNinth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-ref'), arrayRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-rest'), arrayRest_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-reverse'), arrayReverse_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-second'), arraySecond_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-set'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-set!'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-seventh'), arraySeventh_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-sixth'), arraySixth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-take'), arrayTake_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-tenth'), arrayTenth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array-third'), arrayThird_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('array?'), arrayp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('aset'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('aset!'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('assert'), assert_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('begin'), begin_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('block'), jsBlock_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('boolean?'), booleanp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('booleanp'), booleanp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('build-list'), buildList_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('cadr'), cadr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('call-cc'), callWithCurrentContinuation_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('call-with-current-continuation'), callWithCurrentContinuation_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('call/cc'), callWithCurrentContinuation_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('car'), car_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('cdr'), cdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('circular-list-p'), circularListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('circular-list?'), circularListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('cons'), cons_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('cons*'), listStar_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('cons-dot'), consDotF_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('cons-dot?'), consDotP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('cons?'), consp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('console.log'), console.log, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('consp'), consp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('const'), const_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('constantly'), const_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('current-environment'), currentEnvironment_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('curry'), curry, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('curry-n'), curryN, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('delete'), jsDelete_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('display'), display_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('div'), div_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list->proper-list'), linkedListToArrayList_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-car'), linkedListCar_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-cdr'), linkedListCdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-head'), linkedListHead_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-last'), linkedListLast_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-last-cdr'), linkedListLastCdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-length'), linkedListLength_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-nth'), linkedListNth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-nthcdr'), linkedListNthcdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-p'), dottedListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list-tail'), linkedListTail_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-list?'), dottedListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-pair-cdr'), linkedPairCdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-pair-p'), dottedPairP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('dotted-pair?'), dottedPairP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('drop'), listTail_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('drop-right'), dropRight_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('eighth'), eighth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('eq'), eqp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('eq?'), eqp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('eql'), eqvp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('eql?'), eqvp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('equal'), equalp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('equal?'), equalp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('eqv'), eqvp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('eqv?'), eqvp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('error'), error_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('even?'), evenp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('extend-environment'), extendEnvironment, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('false?'), falsep_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('falsep'), falsep_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('fexpr?'), fexprp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('fexprp'), fexprp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('field-names'), fieldNames_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('fifth'), fifth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('filter'), filter_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('findf'), findf_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('findf-index'), findfIndex_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('first'), first_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('flatten'), flatten_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('foldl'), foldl_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('foldr'), foldr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('fourth'), fourth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('funcall'), funcall_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('function-object?'), jsFunctionObjectP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('function-type?'), jsFunctionTypeP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('function?'), procedurep_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('functionp'), procedurep_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('gensym'), gensym_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('gensym?'), gensymp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('get'), arrayRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash'), makeHash_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash->list'), hashToList_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-clear'), hashClear_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-clear!'), hashClearX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-copy'), hashCopy_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-entries'), hashEntries_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-has-key?'), hashHasKeyP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-keys'), hashKeys_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-ref'), hashRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-remove'), hashRemove_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-remove!'), hashRemoveX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-set'), hashSetX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-set!'), hashSetX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-size'), hashSize_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash-values'), hashValues_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('hash?'), hashp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('head'), car_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('id'), identity_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('identity'), identity_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('improper-list-p'), improperListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('improper-list?'), improperListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('in-range'), range_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('index-of'), indexOf_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('index-where'), indexWhere_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('instance-of'), isAP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('instance-of?'), isAP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('instanceof'), isAP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('instanceof?'), isAP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('intern'), stringToSymbol_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('intersection'), intersection_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('is-a?'), isAP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js'), js_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js-field'), arrayRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js-keys'), jsKeys_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js-obj'), jsObj_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js-obj-append'), jsObjAppend_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js-obj-keys'), jsKeys_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js-obj?'), jsObjP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/+'), jsPlus_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/=='), jsLooselyEqualP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/==='), jsStrictlyEqualP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/===?'), jsStrictlyEqualP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/==?'), jsLooselyEqualP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/append'), jsPlus_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/block'), jsBlock_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/console.log'), console.log, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/delete'), jsDelete_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/eighth'), jsEighth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/field'), arrayRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/fifth'), jsFifth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/find-index'), jsFindIndex_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/findf-index'), jsFindIndex_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/first'), jsFirst_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/fourth'), jsFourth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/function-object?'), jsFunctionObjectP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/function-type?'), jsFunctionTypeP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/function?'), jsFunctionP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/get'), jsGet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/in'), jsIn_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/instance-of'), jsInstanceOfP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/instance-of?'), jsInstanceOfP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/instanceof'), jsInstanceOfP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/instanceof?'), jsInstanceOfP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/is-loosely-equal?'), jsLooselyEqualP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/is-strictly-equal?'), jsStrictlyEqualP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/new'), jsNew_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/js-obj'), jsObj_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/js-obj-append'), jsObjAppend_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/js-obj?'), jsObjP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/keys'), jsKeys_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/last'), jsLast_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/length'), jsLength_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/nan?'), jsNanP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/ninth'), jsNinth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/null?'), jsNullP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/obj'), jsObj_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/obj-append'), jsObjAppend_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/obj-keys'), jsKeys_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/obj?'), jsObjP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/object-type?'), jsObjectTypeP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/object?'), jsObjectTypeP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/reduce'), jsReduce_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/reduce-right'), jsReduceRight_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/regexp'), jsRegexp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/regexp-match'), jsRegexpMatch_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/regexp-quote'), regexpQuote_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/regexp-replace'), jsRegexpReplace_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/regexp?'), jsRegexpP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/rest'), jsRest_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/return'), jsReturn_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/reverse'), jsReverse_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/same-value-zero?'), jsSameValueZeroP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/same-value?'), jsSameValueP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/second'), jsSecond_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/seventh'), jsSeventh_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/sixth'), jsSixth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/slice'), jsSlice_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/tag'), jsTaggedTemplate_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/tagged-template'), jsTaggedTemplate_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/take'), jsTake_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/tenth'), jsTenth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/third'), jsThird_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/type-of'), jsTypeOf_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/yield'), yield_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/typeof'), jsTypeOf_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('keyword?'), keywordp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('keywordp'), keywordp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('last'), last_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('last-cdr'), lastCdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('last-cons'), lastPair_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('last-pair'), lastPair_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('length'), length_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('length*'), length_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-car'), linkedListCar_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-cdr'), linkedListCdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-eighth'), linkedListEighth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-fifth'), linkedListFifth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-first'), linkedListFirst_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-fourth'), linkedListFourth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-head'), linkedListHead_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-last'), linkedListLast_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-last-cdr'), linkedListLastCdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-length'), linkedListLength_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-link-car'), linkedListLinkCar_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-link-cdr'), linkedListLinkCdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-link-p'), linkedListLinkP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-link?'), linkedListLinkP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-ninth'), linkedListNinth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-nth'), linkedListNth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-nthcdr'), linkedListNthcdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-p'), linkedListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-second'), linkedListSecond_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-seventh'), linkedListSeventh_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-sixth'), linkedListSixth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-tail'), linkedListTail_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-tenth'), linkedListTenth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list-third'), linkedListThird_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-list?'), linkedListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-pair-car'), linkedPairCar_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-pair-cdr'), linkedPairCdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('linked-pair?'), linkedPairP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('list'), list_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('list*'), listStar_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('list-ref'), nth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('list-set'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('list-set!'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('list-star'), listStar_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('list-tail'), listTail_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('list?'), listp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('listp'), listp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('log'), console.log, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('macro?'), macrop_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('make'), jsNew_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('make-hash'), makeHash_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('make-list'), makeList_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('make-object'), jsNew_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('map'), map_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('mapcar'), map_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('member'), member_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('member-p'), memberp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('member?'), memberp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('memberp'), memberp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('memf'), memf_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('memf?'), memfp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('memq'), memq_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('memq?'), memqp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('mod'), modulo_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('modulo'), modulo_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('mul'), mul_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('new'), jsNew_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('new*'), jsNew_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('ninth'), ninth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('not'), not_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('nth'), nth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('nthcdr'), nthcdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('null?'), nullp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('nullp'), nullp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('number->string'), numberToString_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('number?'), numberp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('numberp'), numberp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('object?'), jsObjP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('objectp'), jsObjP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('odd?'), oddp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('oget'), objectRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('one?'), onep_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('onep'), onep_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('oref'), arrayRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('oset'), objectSetX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('oset!'), objectSetX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('plist->alist'), plistToAlist_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('plist-copy'), plistCopy_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('plist-get'), plistGet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('plist-has'), plistHasP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('plist-has?'), plistHasP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('plist-ref'), plistGet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('plist-set'), plistSetX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('plist-set!'), plistSetX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('plist?'), plistp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('pop'), popLeftX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('pop!'), popLeftX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('pop-left'), popLeftX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('pop-left!'), popLeftX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('pop-right'), popRightX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('pop-right!'), popRightX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('print'), print, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('print-estree'), printEstree, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('procedure?'), procedurep_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('progn'), begin_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('proper-list->dotted-list'), arrayListToLinkedList_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('proper-list-p'), properListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('proper-list?'), properListP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('push'), pushLeftX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('push!'), pushLeftX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('push-left'), pushLeftX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('push-left!'), pushLeftX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('push-right'), pushRightX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('push-right!'), pushRightX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('range'), range_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('re'), jsRegexp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('re-pattern'), jsRegexp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('regexp'), jsRegexp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('regexp-match'), regexpMatch_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('regexp-match?'), regexpMatchP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('regexp-quote'), regexpQuote_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('regexp-replace'), regexpReplace_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('regexp?'), regexpp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('rest'), rest_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('reverse'), reverse_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('rx'), jsRegexp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('scm/new'), jsNew_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('second'), second_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('self-evaluating?'), selfEvaluatingP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set-car!'), setCarX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set-cdr!'), setCdrX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set-mcar!'), setCarX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set-mcdr!'), setCdrX_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set-nth'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set-nth!'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('seventh'), seventh_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('sixth'), sixth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('source'), source, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string->number'), stringToNumber_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string->symbol'), stringToSymbol_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-append'), stringAppend_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-downcase'), stringDowncase_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-join'), stringJoin_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-length'), length_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-object?'), stringObjectP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-primitive?'), stringPrimitiveP_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-ref'), stringRef_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-repeat'), stringRepeat_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-replace'), stringReplace_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-split'), stringSplit_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-to-symbol'), stringToSymbol_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-trim'), stringTrim_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string-upcase'), stringUpcase_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('string?'), stringp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('stringp'), stringp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('sub'), sub_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('sub1'), sub1_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('substring'), substring_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('symbol->string'), symbolToString_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('symbol-to-string'), symbolToString_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('symbol?'), symbolp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('symbolp'), symbolp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('tail'), cdr_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('take'), take_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('tenth'), tenth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('third'), third_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('true?'), truep_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('truep'), truep_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('type-of'), typeOf_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('typeof'), typeOf_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('undefined?'), undefinedp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('union'), union_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('values'), values_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('vector'), list_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('vector-ref'), nth_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('vector-set'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('vector-set!'), arraySet_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('vector?'), arrayp_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('zero?'), zerop_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('zerop'), zerop_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('.'), dot_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for(':'), colon_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [quasiquoteSym_, quasiquote_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [quoteSym_, quote_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('->'), threadFirst_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('->>'), threadLast_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('~>'), threadFirst_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('~>>'), threadLast_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('and'), and_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('ann'), ann_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('as->'), threadAs_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('async'), jsAsync_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('as~>'), threadAs_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('await'), jsAwait_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('begin0'), begin0_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('break'), break_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('call-method'), send_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('case'), case_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('case/eq'), caseEq_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('class'), class_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('clj/try'), cljTry_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('cond'), cond_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('continue'), continue_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('declare'), declare_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('declare-macro'), declareMacro_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('defclass'), defclass_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define'), define_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define-class'), defineClass_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define-fields'), defineFields_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define-js-obj'), defineFields_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define-macro'), defineMacro_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define-type'), defineType_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define-values'), defineValues_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define/async'), defineAsync_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define/generator'), defineGenerator_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define/private'), definePrivate_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define/public'), definePublic_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('defmacro'), defmacro_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('defun'), defun_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('destructuring-bind'), multipleValueBind_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('do'), do_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('field-bound?'), fieldBoundP_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('fn'), lambda_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('for'), for_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('fset'), set_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('get-field'), getField_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('if'), if_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/?.'), jsOptionalChaining_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/arrow'), jsArrow_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/async'), jsAsync_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/await'), jsAwait_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/do-while'), jsDoWhile_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/for'), jsFor_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/for-in'), jsForIn_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/for-of'), jsForOf_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/function'), jsFunction_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/switch'), jsSwitch_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/try'), jsTry_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/while'), jsWhile_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('λ'), lambda_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('lambda'), lambda_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('let'), letStar_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('let*'), letStar_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('let*-values'), letValues_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('let-env'), letEnv_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('let-fields'), letFields_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('let-js-obj'), letFields_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('let-values'), letValues_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('letrec'), letStar_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('letrec-values'), letValues_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('module'), module_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('multiple-value-bind'), multipleValueBind_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('multiple-values-bind'), multipleValueBind_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('new/apply'), newApply_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('or'), or_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('prog1'), begin0_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('provide'), provide_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('require'), require_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('return'), return_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('rkt/new'), rktNew_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('send'), send_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('send/apply'), sendApply_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set'), set_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set!'), setX_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set!-fields'), setFields_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set!-js-obj'), setFields_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set!-values'), setValues_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('set-field!'), setField_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('setq'), setX_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('throw'), throw_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('try'), cljTry_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('unless'), unless_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('unwind-protect'), unwindProtect_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('when'), when_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('while'), while_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('yield'), yield_, [Symbol.for('macro->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]]);

/**
 * Evaluation environment.
 */
const evalEnvironment: any = new LispEnvironment([[Symbol.for('eval'), interpret, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('js/eval'), jsEval_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('scm/eval'), interpret, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('seval'), eval_, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]]);

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
  languageEnvironment: langEnvironment,
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
  jsBlock_ as block,
  jsBlock_ as block_,
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
  defineFields_ as defineFields,
  defineFields_ as defineJsObj,
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
  letFields_ as letFields,
  letFields_ as letJsObj,
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
  setFields_ as setXFields,
  setFields_ as setXJsObj,
  setFields_ as setFieldsX,
  setFields_ as setFields,
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
  defineFields_,
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
  letFields_,
  letStar_,
  letValues_,
  letVarsToConstVars,
  lisp,
  lispEnvironment,
  macroexpand,
  macroexpand1,
  macroexpandAll,
  macroexpandAllUntil,
  macroexpandN,
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
  setFields_,
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