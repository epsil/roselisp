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

import {
  mkdirSync,
  readFileSync,
  writeFileSync
} from 'fs';

import {
  basename,
  dirname,
  join,
  extname
} from 'path';

import {
  parse as parseTs
} from '@typescript-eslint/typescript-estree';

import {
  estreeTypeP,
  estreeType
} from './estree';

import {
  RoseSplice,
  makeRose
} from './rose';

import {
  writeToString
} from './printer';

import {
  makeUniqueSymbol,
  taggedListP
} from './util';

const [lastCdr, cons, findf, listStar]: any[] = ((): any => {
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
  return [lastCdr_, cons_, findf_, listStar_];
})();

/**
 * Decompile a JavaScript or TypeScript program.
 */
function decompile(x: any, options: any = {}): any {
  let language: any = options['language'];
  if (language === 'TypeScript') {
    return decompileTs(x, options);
  } else {
    return decompileJs(x, options);
  }
}

/**
 * Read a JavaScript or TypeScript program from disk
 * and decompile it. The result is written to disk.
 */
function decompileFileX(file: any, options: any = {}): any {
  // TODO: Refactor to `(compile-file in-file out-file options)`?
  const extension: any = extname(file);
  const stem: any = basename(file, extension);
  let language: any = options['language'];
  const inDir: any = dirname(file);
  const outDir: any = options['outDir'] || inDir;
  const outExtension: any = '.scm';
  const outFile: any = join(outDir, stem + outExtension);
  let code: any;
  let data: any;
  language = (language.match(new RegExp('^TypeScript$', 'i')) || (extension === '.ts')) ? 'TypeScript' : 'JavaScript';
  options = {
    ...options,
    language: language,
    module: true,
    noModuleForm: true,
    pretty: true
  };
  data = readFileSync(file, {
    encoding: 'utf8'
  });
  code = decompile(data, options);
  mkdirSync(outDir, {
    recursive: true
  });
  writeFileSync(outFile, code, {
    encoding: 'utf8'
  });
  console.log('Decompiled ' + file + ' to ' + outFile);
  return file;
}

/**
 * Read JavaScript or TypeScript programs from disk
 * and decompile them. The results are written to disk.
 */
function decompileFilesX(files: any, options: any = {}): any {
  for (let file of files) {
    decompileFileX(file, options);
  }
  return files;
}

/**
 * Decompile a JavaScript or TypeScript module.
 */
function decompileModule(m: any, options: any = {}): any {
  // TODO
  return m;
}

/**
 * Decompile a JavaScript program
 * (i.e., an [ESTree][github:estree]
 * [AST][w:Abstract syntax tree]).
 *
 * [github:estree]: https://github.com/estree/estree
 * [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
 */
function decompileJs(x: any, options: any = {}): any {
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
function decompileTs(x: any, options: any = {}): any {
  const ast: any = parseTs(x);
  const resultNode: any = decompileEstree(ast, options);
  let result: any = resultNode.getValue();
  if (!options['sexp']) {
    result = writeToString(result, options);
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
function decompileEstree(node: any, options: any = {}): any {
  const type: any = node && estreeType(node);
  const decompiler: any = decompilerMap.get(type) || defaultDecompiler;
  return decompiler(node, options);
}

/**
 * Decompile an ESTree [`Program`][estree:program] node
 * (i.e., a JavaScript program).
 *
 * [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
 */
function decompileProgram(node: any, options: any = {}): any {
  const moduleOption: any = options['module'];
  let result: any = makeRose([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), ...node.body.map(function (x: any): any {
    return decompileEstree(x, options);
  })]);
  if (!moduleOption) {
    result = makeRose([Symbol.for('begin'), ...result.drop(3)]);
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
function decompileExpressionStatement(node: any, options: any = {}): any {
  return decompileEstree(node.expression, options);
}

/**
 * Decompile an ESTree [`CallExpression`][estree:callexpression] node.
 *
 * [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
 */
function decompileCallExpression(node: any, options: any = {}): any {
  function isSpreadElement(x: any): any {
    return x && estreeTypeP(x, 'SpreadElement');
  }
  const callee: any = node.callee;
  const calleeDecompiled: any = decompileEstree(callee, options);
  const calleeDecompiledExp: any = calleeDecompiled.getValue();
  const args: any = node.arguments;
  const spreadIdx: any = args.findIndex(isSpreadElement);
  const isSpread: any = Number.isFinite(spreadIdx) && (spreadIdx >= 0);
  const isSpreadLast: any = Number.isFinite(spreadIdx) && (spreadIdx === (args.length - 1));
  const argsDecompiled: any = (isSpread && !isSpreadLast && (args.length > 1)) ? [[Symbol.for('append'), ...args.map(function (x: any): any {
    let result: any = decompileEstree(x, options);
    if (isSpreadElement(x)) {
      return result;
    } else {
      return [Symbol.for('list'), result];
    }
  })]] : args.map(function (x: any): any {
    return decompileEstree(x, options);
  });
  if (taggedListP(calleeDecompiledExp, Symbol.for('get-field'))) {
    return makeRose([isSpread ? Symbol.for('send/apply') : Symbol.for('send'), calleeDecompiled.get(2), calleeDecompiled.get(1), ...argsDecompiled]);
  } else {
    return makeRose([...(isSpread ? [Symbol.for('apply')] : []), calleeDecompiled, ...argsDecompiled]);
  }
}

/**
 * Decompile an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
 *
 * [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
 */
function decompileAssignmentExpression(node: any, options: any = {}): any {
  let op: any = node.operator;
  let left: any = node.left;
  const leftDecompiled: any = decompileEstree(left);
  let leftExp: any = leftDecompiled.getValue();
  const right: any = node.right;
  let rightDecompiled: any = decompileEstree(right);
  if (op === '+=') {
    rightDecompiled = makeRose([Symbol.for('+'), leftDecompiled, rightDecompiled]);
  }
  if (taggedListP(leftExp, Symbol.for('get-field'))) {
    return makeRose([Symbol.for('set-field!'), leftDecompiled.get(1), leftDecompiled.get(2), rightDecompiled]);
  } else if (taggedListP(leftExp, Symbol.for('aget'))) {
    return makeRose([Symbol.for('aset!'), ...leftDecompiled.drop(1), rightDecompiled]);
  } else if (taggedListP(leftExp, Symbol.for('oget'))) {
    return makeRose([Symbol.for('oset!'), ...leftDecompiled.drop(1), rightDecompiled]);
  } else if (estreeTypeP(left, 'ArrayPattern')) {
    return makeRose([Symbol.for('set!-values'), leftDecompiled, rightDecompiled]);
  } else if (estreeTypeP(left, 'ObjectPattern')) {
    return makeRose([Symbol.for('set!-js-obj'), leftDecompiled, rightDecompiled]);
  } else {
    return makeRose([Symbol.for('set!'), leftDecompiled, rightDecompiled]);
  }
}

/**
 * Decompile an ESTree [`AssignmentPattern`][estree:assignmentpattern] node.
 *
 * [estree:assignmentpattern]: https://github.com/estree/estree/blob/master/es2015.md#assignmentpattern
 */
function decompileAssignmentPattern(node: any, options: any = {}): any {
  const assignment: any = decompileAssignmentExpression(node, options);
  let left: any = node.left;
  const typ: any = left.typeAnnotation;
  if (typ) {
    return makeRose([assignment.get(1), Symbol.for(':'), decompileEstree(typ, options), assignment.get(2)]);
  } else {
    return makeRose(assignment.drop(1));
  }
}

/**
 * Decompile an ESTree [`UnaryExpression`][estree:unaryexpression] node.
 *
 * [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
 */
function decompileUnaryExpression(node: any, options: any = {}): any {
  let op: any = node.operator;
  const opDecompiled: any = (op === 'typeof') ? Symbol.for('type-of') : ((op === 'delete') ? Symbol.for('js/delete') : Symbol.for(op));
  const argument: any = node.argument;
  const argumentDecompiled: any = decompileEstree(argument, options);
  const argumentDecompiledExp: any = argumentDecompiled.getValue();
  if (op === '!') {
    return makeRose([Symbol.for('not'), argumentDecompiled]);
  } else if ((op === '-') && Number.isFinite(argumentDecompiledExp)) {
    return makeRose(-argumentDecompiledExp);
  } else {
    return makeRose([opDecompiled, argumentDecompiled]);
  }
}

/**
 * Decompile an ESTree [`UpdateExpression`][estree:updateexpression] node.
 *
 * [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
 */
function decompileUpdateExpression(node: any, options: any = {}): any {
  let op: any = node.operator;
  const argument: any = decompileEstree(node.argument, options);
  const prefix: any = node.prefix;
  let result: any = false;
  if (op === '++') {
    result = [Symbol.for('set!'), argument, [Symbol.for('+'), argument, 1]];
  } else if (op === '--') {
    result = [Symbol.for('set!'), argument, [Symbol.for('-'), argument, 1]];
  }
  if (!prefix) {
    result = [Symbol.for('begin0'), argument, result];
  }
  return makeRose(result);
}

/**
 * Decompile an ESTree [`BinaryExpression`][estree:binaryexpression] node.
 *
 * [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
 */
function decompileBinaryExpression(node: any, options: any = {}): any {
  function isStringExpression(exp: any): any {
    return (typeof exp === 'string') || taggedListP(exp, Symbol.for('string-append'));
  }
  const operator: any = node.operator;
  let op: any = ((operator === '===') || (operator === '!==')) ? Symbol.for('eq?') : (((operator === '==') || (operator === '!=')) ? Symbol.for('equal?') : ((operator === '&&') ? Symbol.for('and') : ((operator === '||') ? Symbol.for('or') : ((operator === 'in') ? Symbol.for('js/in') : ((operator === 'instanceof') ? Symbol.for('is-a?') : Symbol.for(operator))))));
  let left: any = node.left;
  const leftDecompiled: any = decompileEstree(left);
  const leftDecompiledExp: any = leftDecompiled.getValue();
  const right: any = node.right;
  let rightDecompiled: any = decompileEstree(right);
  const rightDecompiledExp: any = rightDecompiled.getValue();
  if ((op === Symbol.for('+')) && (isStringExpression(leftDecompiledExp) || isStringExpression(rightDecompiledExp))) {
    op = Symbol.for('string-append');
  }
  const leftOperands: any = (taggedListP(leftDecompiledExp, op) || ((op === Symbol.for('string-append')) && taggedListP(leftDecompiledExp, Symbol.for('+')))) ? leftDecompiled.drop(1) : [leftDecompiled];
  const rightOperands: any = ((taggedListP(rightDecompiledExp, op) && (op !== Symbol.for('-')) && (op !== Symbol.for('/'))) || ((op === Symbol.for('string-append')) && taggedListP(rightDecompiledExp, Symbol.for('+')))) ? rightDecompiled.drop(1) : [rightDecompiled];
  if ((operator === '!==') || (operator === '!=')) {
    return makeRose([Symbol.for('not'), [op, ...leftOperands, ...rightOperands]]);
  } else {
    return makeRose([op, ...leftOperands, ...rightOperands]);
  }
}

/**
 * Decompile an ESTree [`LogicalExpression`][estree:logicalexpression] node.
 *
 * [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
 */
function decompileLogicalExpression(node: any, options: any = {}): any {
  return decompileBinaryExpression(node, options);
}

/**
 * Decompile an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
 *
 * [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
 */
function decompileVariableDeclaration(node: any, options: any = {}): any {
  const decls: any = node.declarations.map(function (x: any): any {
    return decompileEstree(x, options);
  });
  let result: any = makeRose(decls).setValue(new RoseSplice());
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
function decompileVariableDeclarator(node: any, options: any = {}): any {
  const id: any = node.id;
  const idType: any = estreeType(id);
  const init: any = node.init;
  const defineSym: any = (idType === 'ArrayPattern') ? Symbol.for('define-values') : ((idType === 'ObjectPattern') ? Symbol.for('define-js-obj') : Symbol.for('define'));
  return makeRose([defineSym, decompileEstree(id, options), ...(((init === undefined) || (init === null)) ? [] : [decompileEstree(init, options)])]);
}

/**
 * Decompile an ESTree [`Identifier`][estree:identifier] node.
 *
 * [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
 */
function decompileIdentifier(node: any, options: any = {}): any {
  const name: any = Symbol.for(node.name);
  return makeRose(name);
}

/**
 * Decompile an ESTree [`Literal`][estree:literal] node.
 *
 * [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
 */
function decompileLiteral(node: any, options: any = {}): any {
  const value: any = node.value;
  if (value === null) {
    return makeRose(Symbol.for('js/null'));
  } else if (typeof value === 'boolean') {
    return makeRose(value ? Symbol.for('#t') : Symbol.for('#f'));
  } else if (value instanceof RegExp) {
    const pattern: any = node.regex.pattern;
    const flags: any = node.regex.flags;
    // TODO: We can emit `regexp` instead of `js/regexp`
    // provided it is not the name of a local variable.
    return makeRose([Symbol.for('js/regexp'), pattern, ...(flags ? [flags] : [])]);
  } else {
    return makeRose(value);
  }
}

/**
 * Decompile an ESTree [`MemberExpression`][estree:memberexpression] node.
 *
 * [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
 */
function decompileMemberExpression(node: any, options: any = {}): any {
  const property: any = decompileEstree(node.property, options);
  const propertyExp: any = property.getValue();
  const object: any = decompileEstree(node.object, options);
  const objectExp: any = object.getValue();
  const computed: any = node.computed;
  if (computed) {
    if (Number.isFinite(propertyExp)) {
      return makeRose([Symbol.for('aget'), ...(taggedListP(objectExp, Symbol.for('aget')) ? object.drop(1) : [object]), property]);
    } else {
      return makeRose([Symbol.for('oget'), object, property]);
    }
  } else {
    return makeRose([Symbol.for('get-field'), property, object]);
  }
}

/**
 * Decompile an ESTree [`ChainExpression`][estree:chainexpression] node.
 *
 * [estree:chainexpression]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
 */
function decompileChainExpression(node: any, options: any = {}): any {
  const expression: any = node.expression;
  const expressionDecompiled: any = decompileEstree(expression, options);
  const expressionDecompiledExp: any = expressionDecompiled.getValue();
  if (taggedListP(expressionDecompiledExp, Symbol.for('get-field'))) {
    if (typeof expressionDecompiledExp[2] === 'symbol') {
      return makeRose([Symbol.for('and'), [Symbol.for('field-bound?'), expressionDecompiled.get(1), expressionDecompiled.get(2)], expressionDecompiled]);
    } else {
      return makeRose([Symbol.for('~>'), expressionDecompiled.get(2), [Symbol.for('and'), [Symbol.for('field-bound?'), expressionDecompiled.get(1), Symbol.for('_')], [Symbol.for('get-field'), expressionDecompiled.get(1), Symbol.for('_')]]]);
    }
  } else if (taggedListP(expressionDecompiledExp, Symbol.for('send'))) {
    if (typeof expressionDecompiledExp[1] === 'symbol') {
      return makeRose([Symbol.for('and'), [Symbol.for('field-bound?'), expressionDecompiled.get(2), expressionDecompiled.get(1)], expressionDecompiled]);
    } else {
      return makeRose([Symbol.for('~>'), expressionDecompiled.get(1), [Symbol.for('and'), [Symbol.for('field-bound?'), expressionDecompiled.get(2), Symbol.for('_')], [Symbol.for('send'), Symbol.for('_'), ...expressionDecompiled.drop(2)]]]);
    }
  } else {
    return expressionDecompiled;
  }
}

/**
 * Decompile an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
 *
 * [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
 */
function decompileFunctionDeclaration(node: any, options: any = {}): any {
  return decompileFunction(node, options);
}

/**
 * Decompile an ESTree [`FunctionExpression`][estree:functionexpression] node.
 *
 * [estree:functionexpression]: https://github.com/estree/estree/blob/master/es5.md#functionexpression
 */
function decompileFunctionExpression(node: any, options: any = {}): any {
  return decompileFunction(node, options);
}

/**
 * Decompile an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
 *
 * [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
 */
function decompileArrowFunctionExpression(node: any, options: any = {}): any {
  return decompileFunction(node, options);
}

/**
 * Decompile an ESTree [`RestElement`][estree:restelement] node.
 *
 * [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
 */
function decompileRestElement(node: any, options: any = {}): any {
  return decompileEstree(node.argument, options);
}

/**
 * Decompile an ESTree [`BlockStatement`][estree:blockstatement] node.
 *
 * [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
 */
function decompileBlockStatement(node: any, options: any = {}): any {
  return makeRose([Symbol.for('begin'), ...node.body.map(function (x: any): any {
    return decompileEstree(x, options);
  })]);
}

/**
 * Decompile an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
 *
 * [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
 */
function decompileSequenceExpression(node: any, options: any = {}): any {
  return makeRose([Symbol.for('begin'), ...node.expressions.map(function (x: any): any {
    return decompileEstree(x, options);
  })]);
}

/**
 * Decompile an ESTree [`ReturnStatement`][estree:returnstatement] node.
 *
 * [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
 */
function decompileReturnStatement(node: any, options: any = {}): any {
  const argument: any = node.argument;
  if (argument) {
    return makeRose([Symbol.for('return'), decompileEstree(argument, options)]);
  } else {
    return makeRose([Symbol.for('return')]);
  }
}

/**
 * Decompile an ESTree [`IfStatement`][estree:ifstatement] node.
 *
 * [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
 */
function decompileIfStatement(node: any, options: any = {}): any {
  const test: any = decompileEstree(node.test, options);
  const testExp: any = test.getValue();
  let consequent: any = decompileEstree(node.consequent, options);
  let consequentExp: any = consequent.getValue();
  let alternate: any = node.alternate ? decompileEstree(node.alternate, options) : false;
  let alternateExp: any = alternate && alternate.getValue();
  if (taggedListP(consequentExp, Symbol.for('begin')) && (consequentExp.length === 2)) {
    consequent = consequent.get(1);
    consequentExp = consequent.getValue();
  }
  if (taggedListP(alternateExp, Symbol.for('begin')) && (alternateExp.length === 2)) {
    alternate = alternate.get(1);
    alternateExp = alternate.getValue();
  }
  if (taggedListP(alternateExp, Symbol.for('when'))) {
    return makeRose([Symbol.for('cond'), [test, ...(taggedListP(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], [...alternate.drop(1)]]);
  } else if (taggedListP(alternateExp, Symbol.for('unless'))) {
    return makeRose([Symbol.for('cond'), [test, ...(taggedListP(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], [[Symbol.for('not'), alternate.get(1)], ...alternate.drop(2)]]);
  } else if (taggedListP(alternateExp, Symbol.for('if'))) {
    const alternateTest: any = alternate.get(1);
    const alternateConsequent: any = alternate.get(2);
    const alternateConsequentExp: any = alternateConsequent.getValue();
    return makeRose([Symbol.for('cond'), [test, ...(taggedListP(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], [alternateTest, ...(taggedListP(alternateConsequentExp, Symbol.for('begin')) ? alternateConsequent.drop(1) : [alternateConsequent])], ...((alternateExp.length > 3) ? [[Symbol.for('else'), ...alternate.drop(3)]] : [])]);
  } else if (taggedListP(alternateExp, Symbol.for('cond'))) {
    return makeRose([Symbol.for('cond'), [test, ...(taggedListP(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], ...alternate.drop(1)]);
  } else if (!alternate) {
    if (taggedListP(testExp, Symbol.for('not'))) {
      return makeRose([Symbol.for('unless'), test.get(1), ...(taggedListP(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])]);
    } else {
      return makeRose([Symbol.for('when'), test, ...(taggedListP(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])]);
    }
  } else if (taggedListP(consequentExp, Symbol.for('begin')) || taggedListP(alternateExp, Symbol.for('begin'))) {
    return makeRose([Symbol.for('cond'), [test, ...(taggedListP(consequentExp, Symbol.for('begin')) ? consequent.drop(1) : [consequent])], [Symbol.for('else'), ...(taggedListP(alternateExp, Symbol.for('begin')) ? alternate.drop(1) : [alternate])]]);
  } else {
    return makeRose([Symbol.for('if'), test, consequent, alternate]);
  }
}

/**
 * Decompile an ESTree [`WhileStatement`][estree:whilestatement] node.
 *
 * [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
 */
function decompileWhileStatement(node: any, options: any = {}): any {
  const test: any = decompileEstree(node.test, options);
  let body: any = decompileEstree(node.body, options);
  const bodyExp: any = body.getValue();
  let result: any = makeRose([Symbol.for('do'), [], [[Symbol.for('not'), test]], ...(taggedListP(bodyExp, Symbol.for('begin')) ? body.drop(1) : [body])]);
  return result;
}

/**
 * Decompile an ESTree [`DoWhileStatement`][estree:dowhilestatement] node.
 *
 * [estree:dowhilestatement]: https://github.com/estree/estree/blob/master/es5.md#dowhilestatement
 */
function decompileDoWhileStatement(node: any, options: any = {}): any {
  const test: any = decompileEstree(node.test, options);
  let body: any = decompileEstree(node.body, options);
  const bodyExp: any = body.getValue();
  if (taggedListP(bodyExp, Symbol.for('begin')) && (bodyExp.length === 2)) {
    body = body.get(1);
  }
  let result: any = makeRose([Symbol.for('js/do-while'), body, test]);
  return result;
}

/**
 * Decompile an ESTree [`ForStatement`][estree:forstatement] node.
 *
 * [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
 */
function decompileForStatement(node: any, options: any = {}): any {
  const init: any = decompileEstree(node.init, options);
  const inits: any = (init.getValue() instanceof RoseSplice) ? init.drop(0) : [init];
  const test: any = decompileEstree(node.test, options);
  const update: any = decompileEstree(node.update, options);
  const updates: any = taggedListP(update.getValue(), Symbol.for('begin')) ? update.drop(1) : [update];
  const bindings: any = [];
  const _end: any = inits.length;
  for (let i: any = 0; i < _end; i++) {
    let currentInit: any = (inits as any)[i];
    let currentInitExp: any = currentInit.getValue();
    if (taggedListP(currentInitExp, Symbol.for('define')) || taggedListP(currentInitExp, Symbol.for('set!'))) {
      currentInit = makeRose([...currentInit.drop(1)], currentInit);
      currentInitExp = currentInit.getValue();
    }
    let currentUpdate: any = (updates as any)[i];
    let currentUpdateExp: any = currentUpdate.getValue();
    if (taggedListP(currentUpdateExp, Symbol.for('begin0'))) {
      currentUpdate = currentUpdate.last();
      currentUpdateExp = currentUpdate.getValue();
    }
    if (taggedListP(currentUpdateExp, Symbol.for('set!'))) {
      currentUpdate = currentUpdate.third();
      currentUpdateExp = currentUpdate.getValue();
    }
    bindings.push([...currentInit.drop(0), currentUpdate]);
  }
  let body: any = decompileEstree(node.body, options);
  if ((bindings.length === 1) && (taggedListP(test, Symbol.for('<')) || taggedListP(test, Symbol.for('>')))) {
    const binding: any = bindings[0];
    let i: any = binding[0];
    const start: any = (Array.isArray(binding) && (binding.length >= 3) && (binding[binding.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(binding);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = binding;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = binding[binding.length - 1];
        } else {
          result = binding.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : binding[1];
    const end: any = test.get(2);
    const update: any = (Array.isArray(binding) && (binding.length >= 3) && (binding[binding.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(binding);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 2;
      let result: any = binding;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = binding[binding.length - 1];
        } else {
          result = binding.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : binding[2];
    let step: any = update.get(2).getValue();
    if (taggedListP(update, Symbol.for('-'))) {
      step = -step;
    }
    return makeRose([Symbol.for('for'), [[i, [Symbol.for('range'), start, end, ...((step === 1) ? [] : [step])]]], ...body.drop(1)]);
  } else {
    return makeRose([Symbol.for('do'), bindings, [[Symbol.for('not'), test]], ...body.drop(1)]);
  }
}

/**
 * Decompile an ESTree [`ForOfStatement`][estree:forofstatement] node.
 *
 * [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
 */
function decompileForOfStatement(node: any, options: any = {}): any {
  let left: any = decompileEstree(node.left, options);
  let leftExp: any = left.getValue();
  if (taggedListP(leftExp, Symbol.for('define'))) {
    left = left.get(1);
    leftExp = left.getValue();
  }
  const right: any = decompileEstree(node.right, options);
  const rightExp: any = right.getValue();
  let body: any = decompileEstree(node.body, options);
  const bodyNodes: any = body.drop(1);
  if (taggedListP(leftExp, Symbol.for('define-values'))) {
    const sym: any = makeUniqueSymbol(cons(rightExp, (Array.isArray(leftExp) && (leftExp.length >= 3) && (leftExp[leftExp.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(leftExp);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = leftExp;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = leftExp[leftExp.length - 1];
        } else {
          result = leftExp.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : leftExp[1]));
    bodyNodes.unshift(makeRose([left.get(0), left.get(1), sym]));
    left = makeRose(sym);
  }
  return makeRose([Symbol.for('for'), [[left, right]], ...bodyNodes]);
}

/**
 * Decompile an ESTree [`ForInStatement`][estree:forinstatement] node.
 *
 * [estree:forinstatement]: https://github.com/estree/estree/blob/master/es5.md#forinstatement
 */
function decompileForInStatement(node: any, options: any = {}): any {
  let left: any = decompileEstree(node.left, options);
  let leftExp: any = left.getValue();
  if (taggedListP(leftExp, Symbol.for('define'))) {
    left = left.get(1);
    leftExp = left.getValue();
  }
  const right: any = decompileEstree(node.right, options);
  let body: any = decompileEstree(node.body, options);
  return makeRose([Symbol.for('for'), [[left, [Symbol.for('js-keys'), right]]], ...body.drop(1)]);
}

/**
 * Decompile an ESTree [`BreakStatement`][estree:breakstatement] node.
 *
 * [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
 */
function decompileBreakStatement(node: any, options: any = {}): any {
  return makeRose([Symbol.for('break'), ...(node.label ? [decompileEstree(node.label, options)] : [])]);
}

/**
 * Decompile an ESTree [`ContinueStatement`][estree:continuestatement] node.
 *
 * [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
 */
function decompileContinueStatement(node: any, options: any = {}): any {
  return makeRose([Symbol.for('continue'), ...(node.label ? [decompileEstree(node.label, options)] : [])]);
}

/**
 * Decompile an ESTree [`ThrowStatement`][estree:throwstatement] node.
 *
 * [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
 */
function decompileThrowStatement(node: any, options: any = {}): any {
  return makeRose([Symbol.for('throw'), decompileEstree(node.argument, options)]);
}

/**
 * Decompile an ESTree [`TryStatement`][estree:trystatement] node.
 *
 * [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
 */
function decompileTryStatement(node: any, options: any = {}): any {
  const block: any = node.block;
  const handler: any = node.handler;
  const finalizer: any = node.finalizer;
  let result: any = makeRose([Symbol.for('try'), ...decompileEstree(block, options).drop(1), ...(handler ? [[Symbol.for('catch'), Symbol.for('Object'), handler.param ? decompileEstree(handler.param, options) : Symbol.for('_'), ...decompileEstree(handler.body, options).drop(1)]] : []), ...(finalizer ? [[Symbol.for('finally'), ...decompileEstree(finalizer, options).drop(1)]] : [])]);
  return result;
}

/**
 * Decompile an ESTree [`YieldExpression`][estree:yieldexpression] node.
 *
 * [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
 */
function decompileYieldExpression(node: any, options: any = {}): any {
  return makeRose([Symbol.for('yield'), decompileEstree(node.argument, options)]);
}

/**
 * Decompile an ESTree [`NewExpression`][estree:newexpression] node.
 *
 * [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
function decompileNewExpression(node: any, options: any = {}): any {
  const arguments_: any = node.arguments;
  const isSpread: any = (arguments_.length > 0) && estreeTypeP(arguments_[arguments_.length - 1], 'SpreadElement');
  return makeRose([...(isSpread ? [Symbol.for('apply')] : []), Symbol.for('new'), decompileEstree(node.callee, options), ...arguments_.map(function (x: any): any {
    return decompileEstree(x, options);
  })]);
}

/**
 * Decompile an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
 *
 * [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
 */
function decompileConditionalExpression(node: any, options: any = {}): any {
  return decompileIfStatement(node, options);
}

/**
 * Decompile an ESTree [`ImportDeclaration`][estree:importdeclaration] node.
 *
 * [estree:importdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importdeclaration
 */
function decompileImportDeclaration(node: any, options: any = {}): any {
  const source: any = node.source;
  const sourceDecompiled: any = decompileEstree(source, options);
  const specifiers: any = node.specifiers;
  if (specifiers.length === 0) {
    return makeRose([Symbol.for('require'), sourceDecompiled]);
  } else if ((specifiers.length === 1) && estreeTypeP(specifiers[0], 'ImportNamespaceSpecifier')) {
    return makeRose([Symbol.for('require'), decompileEstree(specifiers[0].local, options), sourceDecompiled]);
  } else {
    const specifiersDecompiled: any = specifiers.map(function (x: any): any {
      const imported: any = decompileEstree(x.imported, options);
      const local: any = decompileEstree(x.local, options);
      if (imported.getValue() === local.getValue()) {
        return imported;
      } else {
        return [imported, local];
      }
    });
    return makeRose([Symbol.for('require'), [Symbol.for('only-in'), sourceDecompiled, ...specifiersDecompiled]]);
  }
}

/**
 * Decompile an ESTree [`ExportNamedDeclaration`][estree:exportnameddeclaration] node.
 *
 * [estree:exportnameddeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportnameddeclaration
 */
function decompileExportNamedDeclaration(node: any, options: any = {}): any {
  const specifiers: any = node.specifiers;
  const specifiersDecompiled: any = specifiers.map(function (x: any): any {
    const exported: any = decompileEstree(x.exported, options);
    const local: any = decompileEstree(x.local, options);
    if (exported.getValue() === local.getValue()) {
      return exported;
    } else {
      return [Symbol.for('rename-out'), [local, exported]];
    }
  });
  return makeRose([Symbol.for('provide'), ...specifiersDecompiled]);
}

/**
 * Decompile an ESTree [`ExportAllDeclaration`][estree:exportalldeclaration] node.
 *
 * [estree:exportalldeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportalldeclaration
 */
function decompileExportAllDeclaration(node: any, options: any = {}): any {
  const source: any = node.source;
  const sourceDecompiled: any = decompileEstree(source, options);
  return makeRose([Symbol.for('provide'), [Symbol.for('all-from-out'), sourceDecompiled]]);
}

/**
 * Decompile an ESTree [`ObjectExpression`][estree:objectexpression] node.
 *
 * [estree:objectexpression]: https://github.com/estree/estree/blob/master/es5.md#objectexpression
 */
function decompileObjectExpression(node: any, options: any = {}): any {
  const spreads: any = [];
  const properties: any = [];
  for (let prop of node.properties) {
    if (estreeTypeP(prop, 'SpreadElement')) {
      spreads.push(decompileEstree(prop.argument, options));
    } else {
      properties.push(decompileEstree(prop.key, options).getValue().description as string);
      properties.push(decompileEstree(prop.value, options));
    }
  }
  if (spreads.length === 0) {
    return makeRose([Symbol.for('js-obj'), ...properties]);
  } else {
    return makeRose([Symbol.for('js-obj-append'), ...spreads, ...((properties.length > 0) ? [[Symbol.for('js-obj'), ...properties]] : [])]);
  }
}

/**
 * Decompile an ESTree [`ObjectPattern`][estree:objectpattern] node.
 *
 * [estree:objectpattern]: https://github.com/estree/estree/blob/master/es2015.md#objectpattern
 */
function decompileObjectPattern(node: any, options: any = {}): any {
  const properties: any = [];
  for (let prop of node.properties) {
    const key: any = decompileEstree(prop.key, options);
    const value: any = decompileEstree(prop.value, options);
    if (key.getValue() === value.getValue()) {
      properties.push(key);
    } else {
      properties.push([key, value]);
    }
  }
  return makeRose(properties);
}

/**
 * Decompile an ESTree [`TemplateLiteral`][estree:templateliteral] node.
 *
 * [estree:templateliteral]: https://github.com/estree/estree/blob/master/es2015.md#templateliteral
 */
function decompileTemplateLiteral(node: any, options: any = {}): any {
  let str: any = '';
  const quasis: any = node.quasis;
  if (quasis.length > 0) {
    str = quasis[0].value.cooked;
  }
  return makeRose(str);
}

/**
 * Decompile an ESTree [`TaggedTemplateExpression`][estree:taggedtemplateexpression] node.
 *
 * [estree:taggedtemplateexpression]: https://github.com/estree/estree/blob/master/es2015.md#taggedtemplateexpression
 */
function decompileTaggedTemplateExpression(node: any, options: any = {}): any {
  const tag: any = node.tag;
  const tagDecompiled: any = decompileEstree(tag, options);
  const quasi: any = node.quasi;
  const quasiDecompiled: any = decompileEstree(quasi, options);
  return makeRose([Symbol.for('js/tag'), tagDecompiled, quasiDecompiled]);
}

/**
 * Decompile an ESTree [`ArrayExpression`][estree:arrayexpression] node.
 *
 * [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
 */
function decompileArrayExpression(node: any, options: any = {}): any {
  const elements: any = node.elements;
  function decompileElement(x: any): any {
    if (x) {
      return decompileEstree(x, options);
    } else {
      return Symbol.for('_');
    }
  }
  if ((elements.length > 0) && elements[elements.length - 1] && estreeTypeP(elements[elements.length - 1], 'RestElement')) {
    const regularElements: any = elements.slice(0, -1).map(function (x: any): any {
      return decompileElement(x);
    });
    const restElement: any = decompileElement(elements[elements.length - 1]);
    return makeRose(listStar(...[...regularElements, restElement]));
  } else if (findf(function (x: any): any {
    return x && estreeTypeP(x, 'SpreadElement');
  }, elements)) {
    const elementsDecompiled: any = elements.map(function (x: any): any {
      let result: any = decompileElement(x);
      if (x && estreeTypeP(x, 'SpreadElement')) {
        return result;
      } else {
        return makeRose([Symbol.for('list'), result]);
      }
    });
    return makeRose([Symbol.for('append'), ...elementsDecompiled]);
  } else {
    const elementsDecompiled: any = elements.map(function (x: any): any {
      return decompileElement(x);
    });
    return makeRose([Symbol.for('list'), ...elementsDecompiled]);
  }
}

/**
 * Decompile an ESTree [`ArrayPattern`][estree:arraypattern] node.
 *
 * [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
 */
function decompileArrayPattern(node: any, options: any = {}): any {
  const arrayExpression: any = decompileArrayExpression(node, options);
  return makeRose(taggedListP(arrayExpression.getValue(), Symbol.for('list')) ? arrayExpression.drop(1) : arrayExpression);
}

/**
 * Decompile an ESTree [`SpreadElement`][estree:spreadelement] node.
 *
 * [estree:spreadelement]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
function decompileSpreadElement(node: any, options: any = {}): any {
  const argument: any = node.argument;
  return decompileEstree(argument, options);
}

/**
 * Decompile an ESTree [`Super`][estree:super] node.
 *
 * [estree:super]: https://github.com/estree/estree/blob/master/es2015.md#expressions
 */
function decompileSuper(node: any, options: any = {}): any {
  return makeRose(Symbol.for('super'));
}

/**
 * Decompile an ESTree [`ThisExpression`][estree:thisexpression] node.
 *
 * [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
 */
function decompileThisExpression(node: any, options: any = {}): any {
  return makeRose(Symbol.for('this'));
}

/**
 * Decompile an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
 *
 * [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
 */
function decompileClassDeclaration(node: any, options: any = {}): any {
  const id: any = node.id;
  const idDecompiled: any = decompileEstree(id, options);
  const superClass: any = node.superClass;
  const superClassDecompiled: any = superClass ? decompileEstree(superClass, options) : Symbol.for('object%');
  const superClassDecompiledExp: any = [Symbol.for('object%'), Symbol.for('object'), Symbol.for('Object')].includes(superClassDecompiled) ? [] : [superClassDecompiled];
  const bodyDecompiled: any = [];
  let body: any = node.body;
  for (let x of body.body) {
    bodyDecompiled.push(decompileEstree(x, options));
  }
  return makeRose([Symbol.for('define-class'), idDecompiled, superClassDecompiledExp, ...bodyDecompiled]);
}

/**
 * Decompile an ESTree [`PropertyDefinition`][estree:propertydefinition] node.
 *
 * [estree:propertydefinition]: https://github.com/estree/estree/blob/master/es2022.md#propertydefinition
 */
function decompilePropertyDefinition(node: any, options: any = {}): any {
  const key: any = node.key;
  const keyDecompiled: any = decompileEstree(key, options);
  const value: any = node.value;
  const valueDecompiled: any = decompileEstree(value, options);
  const defineSymbol: any = (node.accessibility === 'private') ? Symbol.for('define') : Symbol.for('define/public');
  return makeRose([defineSymbol, keyDecompiled, ...((value === null) ? [] : [valueDecompiled])]);
}

/**
 * Decompile an ESTree [`MethodDefinition`][estree:methoddefinition] node.
 *
 * [estree:methoddefinition]: https://github.com/estree/estree/blob/master/es2015.md#methoddefinition
 */
function decompileMethodDefinition(node: any, options: any = {}): any {
  const key: any = node.key;
  const keyDecompiled: any = decompileEstree(key, options);
  const keyDecompiledExp: any = keyDecompiled.getValue();
  const value: any = node.value;
  const valueDecompiled: any = decompileEstree(value, options);
  const defineSymbol: any = value.generator ? Symbol.for('define/generator') : (((node.accessibility === 'private') || (keyDecompiledExp === Symbol.for('constructor'))) ? Symbol.for('define') : Symbol.for('define/public'));
  return makeRose([defineSymbol, cons(keyDecompiledExp, valueDecompiled.getValue()[1]), ...valueDecompiled.drop(2)]);
}

/**
 * Decompile a TSESTree `TSAsExpression` node.
 */
function decompileTsAsExpression(node: any, options: any = {}): any {
  const expression: any = node.expression;
  const expressionDecompiled: any = decompileEstree(expression, options);
  const typeAnnotation: any = node.typeAnnotation;
  const typeAnnotationDecompiled: any = decompileEstree(typeAnnotation, options);
  return makeRose([Symbol.for('ann'), expressionDecompiled, typeAnnotationDecompiled]);
}

/**
 * Decompile a TSESTree `TSAnyKeyword` node.
 */
function decompileTsAnyKeyword(node: any, options: any = {}): any {
  return makeRose(Symbol.for('Any'));
}

/**
 * Decompile a TSESTree `TSBooleanKeyword` node.
 */
function decompileTsBooleanKeyword(node: any, options: any = {}): any {
  return makeRose(Symbol.for('Boolean'));
}

/**
 * Decompile a TSESTree `TSNumberKeyword` node.
 */
function decompileTsNumberKeyword(node: any, options: any = {}): any {
  return makeRose(Symbol.for('Number'));
}

/**
 * Decompile a TSESTree `TSStringKeyword` node.
 */
function decompileTsStringKeyword(node: any, options: any = {}): any {
  return makeRose(Symbol.for('String'));
}

/**
 * Decompile a TSESTree `TSUndefinedKeyword` node.
 */
function decompileTsUndefinedKeyword(node: any, options: any = {}): any {
  return makeRose(Symbol.for('Undefined'));
}

/**
 * Decompile a TSESTree `TSVoidKeyword` node.
 */
function decompileTsVoidKeyword(node: any, options: any = {}): any {
  return makeRose(Symbol.for('Void'));
}

/**
 * Decompile a TSESTree `TSLiteralType` node.
 */
function decompileTsLiteralType(node: any, options: any = {}): any {
  const literal: any = node.literal;
  const literalDecompiled: any = literal ? Symbol.for('True') : Symbol.for('False');
  return makeRose(literalDecompiled);
}

/**
 * Decompile a TSESTree `TSArrayType` node.
 */
function decompileTsArrayType(node: any, options: any = {}): any {
  const elementType: any = node.elementType;
  const elementTypeDecompiled: any = decompileEstree(elementType, options);
  return makeRose([Symbol.for('Listof'), elementTypeDecompiled]);
}

/**
 * Decompile a TSESTree `TSTupleType` node.
 */
function decompileTsTupleType(node: any, options: any = {}): any {
  const elementTypes: any = node.elementTypes;
  const elementTypesDecompiled: any = elementTypes.map(function (x: any): any {
    return decompileEstree(x, options);
  });
  return makeRose([Symbol.for('List'), ...elementTypesDecompiled]);
}

/**
 * Decompile a TSESTree `TSNamedTupleMember` node.
 */
function decompileTsNamedTupleMember(node: any, options: any = {}): any {
  // FIXME: Better decompilation of `TSNamedTupleMember`.
  // The following strips away the identifier.
  const elementType: any = node.elementType;
  return decompileEstree(elementType, options);
}

/**
 * Decompile a TSESTree `TSUnionType` node.
 */
function decompileTsUnionType(node: any, options: any = {}): any {
  const types: any = node.types;
  const typesDecompiled: any = types.map(function (x: any): any {
    return decompileEstree(x, options);
  });
  return makeRose([Symbol.for('U'), ...typesDecompiled]);
}

/**
 * Decompile a TSESTree `TSFunctionType` node.
 */
function decompileTsFunctionType(node: any, options: any = {}): any {
  let params: any = node.params;
  const paramsDecompiled: any = params.map(function (x: any): any {
    return decompileEstree(x.typeAnnotation, options);
  });
  const returnType: any = node.returnType;
  const returnTypeDecompiled: any = decompileEstree(returnType, options);
  return makeRose([Symbol.for('->'), ...paramsDecompiled, returnTypeDecompiled]);
}

/**
 * Decompile a TSESTree `TSTypeReference` node.
 */
function decompileTsTypeReference(node: any, options: any = {}): any {
  const name: any = node.typeName;
  let params: any = node.typeParameters;
  const nameDecompiled: any = decompileEstree(name, options);
  const paramsDecompiled: any = params ? decompileEstree(params, options) : [];
  if (paramsDecompiled.length === 0) {
    return makeRose(nameDecompiled);
  } else {
    return makeRose([nameDecompiled, ...paramsDecompiled]);
  }
}

/**
 * Decompile a TSESTree `TSTypeParameterInstantiation` node.
 */
function decompileTsTypeParameterInstantiation(node: any, options: any = {}): any {
  let params: any = node.params;
  return params.map(function (x: any): any {
    return decompileEstree(x, options);
  });
}

/**
 * Decompile a TSESTree `TSTypeAliasDeclaration` node.
 */
function decompileTsTypeAliasDeclaration(node: any, options: any = {}): any {
  const id: any = node.id;
  const idDecompiled: any = decompileEstree(id, options);
  const typeAnnotation: any = node.typeAnnotation;
  const typeAnnotationDecompiled: any = decompileEstree(typeAnnotation, options);
  return makeRose([Symbol.for('define-type'), idDecompiled, typeAnnotationDecompiled]);
}

/**
 * Decompile a TSESTree `TSTypeAnnotation` node.
 */
function decompileTsTypeAnnotation(node: any, options: any = {}): any {
  return decompileEstree(node.typeAnnotation, options);
}

/**
 * Decompile a function expression or declaration.
 */
function decompileFunction(node: any, options: any = {}): any {
  const type_: any = estreeType(node);
  const id: any = (type_ === 'FunctionDeclaration') ? decompileEstree(node.id, options) : false;
  const lambdaSym: any = (type_ === 'ArrowFunctionExpression') ? Symbol.for('js/arrow') : Symbol.for('lambda');
  let params: any = node.params.map(function (x: any): any {
    return decompileParameter(x, options);
  });
  if ((params.length > 0) && estreeTypeP(((): any => {
    const arr: any = node.params;
    return arr[arr.length - 1];
  })(), 'RestElement')) {
    if (params.length === 1) {
      params = params[params.length - 1];
    } else {
      params = listStar(...params);
    }
  }
  let body: any = removeReturnTailCall(decompileEstree(node.body, options));
  const bodyExp: any = body.getValue();
  const bodyForms: any = taggedListP(bodyExp, Symbol.for('begin')) ? body.drop(1) : [body];
  const asyncField: any = node.async;
  if (id) {
    if (asyncField) {
      return makeRose([Symbol.for('define'), id, [Symbol.for('async'), [lambdaSym, params, ...bodyForms]]]);
    } else {
      return makeRose([Symbol.for('define'), cons(id, params), ...bodyForms]);
    }
  } else {
    if (asyncField) {
      return makeRose([Symbol.for('async'), [lambdaSym, params, ...bodyForms]]);
    } else {
      return makeRose([lambdaSym, params, ...bodyForms]);
    }
  }
}

/**
 * Decompile a function parameter.
 * Helper function for `decompile-function`.
 */
function decompileParameter(node: any, options: any = {}): any {
  if (estreeTypeP(node, 'Identifier')) {
    const typeAnnotation: any = node.typeAnnotation;
    const optional: any = node.optional;
    const name: any = Symbol.for(node.name);
    if (optional) {
      return makeRose([name, Symbol.for('undefined')]);
    } else if (typeAnnotation) {
      return makeRose([name, Symbol.for(':'), decompileTsTypeAnnotation(typeAnnotation, options)]);
    } else {
      return makeRose(name);
    }
  } else {
    return decompileEstree(node, options);
  }
}

/**
 * Remove superfluous `(return ...)` forms from
 * a form that occurs in tail call position.
 */
function removeReturnTailCall(node: any): any {
  const exp: any = node.getValue();
  if (taggedListP(exp, Symbol.for('return')) && (exp.length === 2)) {
    return node.get(1);
  } else if (taggedListP(exp, Symbol.for('begin'))) {
    return makeRose([...node.dropRight(1), removeReturnTailCall(node.get(exp.length - 1))], node);
  } else if (taggedListP(exp, Symbol.for('if'))) {
    return makeRose([node.get(0), node.get(1), ...node.drop(2).map(function (x: any): any {
      return removeReturnTailCall(x);
    })], node);
  } else if (taggedListP(exp, Symbol.for('cond'))) {
    return makeRose([node.get(0), ...node.drop(1).map(function (x: any): any {
      return makeRose([...x.dropRight(1), removeReturnTailCall(x.get(x.getValue().length - 1))], node);
    })], node);
  } else {
    return node;
  }
}

/**
 * Default decompiler function.
 */
function defaultDecompiler(node: any, options: any = {}): any {
  return makeRose((node && estreeType(node)) + ' not supported yet');
}

/**
 * Mapping from ESTree node types to decompiler functions.
 */
const decompilerMap: any = new Map([['ArrayExpression', decompileArrayExpression], ['ArrayPattern', decompileArrayPattern], ['ArrowFunctionExpression', decompileArrowFunctionExpression], ['AssignmentExpression', decompileAssignmentExpression], ['AssignmentPattern', decompileAssignmentPattern], ['BinaryExpression', decompileBinaryExpression], ['BlockStatement', decompileBlockStatement], ['BreakStatement', decompileBreakStatement], ['CallExpression', decompileCallExpression], ['ChainExpression', decompileChainExpression], ['ClassDeclaration', decompileClassDeclaration], ['ConditionalExpression', decompileConditionalExpression], ['ContinueStatement', decompileContinueStatement], ['DoWhileStatement', decompileDoWhileStatement], ['ExportAllDeclaration', decompileExportAllDeclaration], ['ExportNamedDeclaration', decompileExportNamedDeclaration], ['ExpressionStatement', decompileExpressionStatement], ['ForInStatement', decompileForInStatement], ['ForOfStatement', decompileForOfStatement], ['ForStatement', decompileForStatement], ['FunctionDeclaration', decompileFunctionDeclaration], ['FunctionExpression', decompileFunctionExpression], ['Identifier', decompileIdentifier], ['IfStatement', decompileIfStatement], ['ImportDeclaration', decompileImportDeclaration], ['Literal', decompileLiteral], ['LogicalExpression', decompileLogicalExpression], ['MemberExpression', decompileMemberExpression], ['MethodDefinition', decompileMethodDefinition], ['NewExpression', decompileNewExpression], ['ObjectExpression', decompileObjectExpression], ['ObjectPattern', decompileObjectPattern], ['Program', decompileProgram], ['PropertyDefinition', decompilePropertyDefinition], ['RestElement', decompileRestElement], ['ReturnStatement', decompileReturnStatement], ['SequenceExpression', decompileSequenceExpression], ['SpreadElement', decompileSpreadElement], ['Super', decompileSuper], ['TSAnyKeyword', decompileTsAnyKeyword], ['TSArrayType', decompileTsArrayType], ['TSAsExpression', decompileTsAsExpression], ['TSBooleanKeyword', decompileTsBooleanKeyword], ['TSFunctionType', decompileTsFunctionType], ['TSLiteralType', decompileTsLiteralType], ['TSNumberKeyword', decompileTsNumberKeyword], ['TSStringKeyword', decompileTsStringKeyword], ['TSTupleType', decompileTsTupleType], ['TSNamedTupleMember', decompileTsNamedTupleMember], ['TSTypeAliasDeclaration', decompileTsTypeAliasDeclaration], ['TSTypeAnnotation', decompileTsTypeAnnotation], ['TSTypeParameterInstantiation', decompileTsTypeParameterInstantiation], ['TSTypeReference', decompileTsTypeReference], ['TSUndefinedKeyword', decompileTsUndefinedKeyword], ['TSUnionType', decompileTsUnionType], ['TSVoidKeyword', decompileTsVoidKeyword], ['TaggedTemplateExpression', decompileTaggedTemplateExpression], ['TemplateLiteral', decompileTemplateLiteral], ['ThisExpression', decompileThisExpression], ['ThrowStatement', decompileThrowStatement], ['TryStatement', decompileTryStatement], ['UnaryExpression', decompileUnaryExpression], ['UpdateExpression', decompileUpdateExpression], ['VariableDeclaration', decompileVariableDeclaration], ['VariableDeclarator', decompileVariableDeclarator], ['WhileStatement', decompileWhileStatement], ['YieldExpression', decompileYieldExpression]] as any);

export {
  decompile,
  decompileFileX,
  decompileFilesX,
  decompileModule
};