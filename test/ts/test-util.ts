/**
 * # Test utilities
 *
 * Various helper functions for testing Lisp code.
 */

import * as chai from 'chai';

import {
  compileWithEnvironment as compile,
  LispEnvironment,
  compileLisp,
  extendEnvironment,
  interpret,
  printSexp,
  writeToString
} from '../../src/ts/language';

const [equalp, keywordp]: any[] = ((): any => {
  function equalp_(x: any, y: any): any {
    if (x === y) {
      return true;
    } else if (Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.')) && Array.isArray(y)) {
      const cdrX: any = ((x.length === 3) && (x[1] === Symbol.for('.'))) ? x[2] : x.slice(1);
      if (Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.')) && (x.length === 3) && !Array.isArray(cdrX) && !(Array.isArray(cdrX) && (cdrX.length >= 3) && (cdrX.at(-2) === Symbol.for('.')))) {
        return false;
      } else if (equalp_(x[0], y[0])) {
        return equalp_(cdrX, ((y.length === 3) && (y[1] === Symbol.for('.'))) ? y[2] : y.slice(1));
      } else {
        return false;
      }
    } else if (Array.isArray(x) && Array.isArray(y) && (y.length >= 3) && (y.at(-2) === Symbol.for('.'))) {
      return equalp_(y, x);
    } else if (Array.isArray(x) && Array.isArray(y)) {
      if (x.length !== y.length) {
        return false;
      }
      const _end: any = x.length;
      for (let i: any = 0; i < _end; i++) {
        if (!equalp_((x as any)[i], (y as any)[i])) {
          return false;
        }
      }
      return true;
    } else if ((x instanceof Map) && (y instanceof Map)) {
      if (x.size !== y.size) {
        return false;
      }
      for (let entry of x.entries()) {
        const [key1, value1]: any[] = entry;
        const value2: any = y.get(key1);
        if (!equalp_(value1, value2)) {
          return false;
        }
      }
      return true;
    } else if ((x !== null) && (typeof x === 'object') && (y !== null) && (typeof y === 'object')) {
      if (Object.keys(x).length !== Object.keys(y).length) {
        return false;
      }
      for (let key of Object.keys(x)) {
        if (!equalp_((x as any)[key], (y as any)[key])) {
          return false;
        }
      }
      return true;
    } else {
      return false;
    }
  }
  function keywordp_(obj: any): any {
    return (typeof obj === 'symbol') && ((obj.description as string).match(new RegExp('^:')) ? true : false);
  }
  return [equalp_, keywordp_];
})();

const assertEqual: any = chai.assert.deepEqual;

const assertNotEqual: any = chai.assert.notDeepEqual;

const assertThrows: any = chai.assert.throws;

/**
 * Test helper function that verifies that interpretation and
 * compilation amounts to the same:
 *
 *     evalLisp(x) = evalJavaScript(compile-lisp(x))
 *
 * Using `.` to denote right-to-left function composition, this can
 * also be expressed as:
 *
 *     evalLisp = evalJavaScript . compile-lisp
 *
 * Alternatively, using `;` to denote left-to-right function
 * composition (as is done in some texts):
 *
 *     evalLisp = compile-lisp ; evalJavaScript
 *
 * All of which is to say that the following diagram [commutes][1]:
 *
 *            compile-lisp
 *     SExp --------------> string
 *       \                     |
 *         \                   |
 *           \                 |
 *             \               |
 *               \             |
 *       evalLisp  \           | evalJavaScript
 *                   \         |
 *                     \       |
 *                       \     |
 *                         \   |
 *                           \ |
 *                            VV
 *                            any
 *
 * Here, `SExp` is an S-expression (atomic or non-atomic), `string` is
 * a JavaScript code string, and `any` is any kind of value. Note that
 * `evalJavaScript` is simply called [`eval`][2] in JavaScript, just
 * like `evalLisp` is called [`eval`][3] in Lisp; they are here named
 * `evalJavaScript` and `evalLisp` to avoid confusion.
 *
 * A prettier and more general version of this diagram can be found in
 * the introductory chapter of the book [*Essentials of Compilation:
 * An Incremental Approach in Racket*][4] by Jeremy G. Siek.
 *
 * [1]: http://en.wikipedia.org/wiki/Commutative_diagram
 * [2]: http://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
 * [3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm
 * [4]: https://github.com/IUCompilerCourse/Essentials-of-Compilation
 */
function testLisp(exp: any = undefined, val: any = undefined, options: any = {}): any {
  // FIXME: `exp` *might* be modified by side-effect. If so, the
  // compilation test will receive a different value. We should
  // clone the value to avoid this.
  let {compile, env, interpret: interpretFlag, interpretValue: interpretValueOption, verbose, wrapParens} = options;
  env = env || new LispEnvironment();
  const expectedValue: any = interpretValueOption ? interpret(val, env) : val;
  const evaluationOptions: any = {
    ...options
  };
  delete evaluationOptions.compile;
  const compilationOptions: any = {
    ...evaluationOptions,
    case: 'camelcase'
  };
  interpretFlag = (interpretFlag === undefined) ? true : compile;
  compile = (compile === undefined) ? true : compile;
  let interpretedValue: any = undefined;
  let compiledValue: any = undefined;
  if (verbose) {
    console.log('expression:', writeToString(exp));
  }
  if (interpretFlag) {
    const interpretationEnv: any = extendEnvironment(new LispEnvironment(), env);
    interpretedValue = interpret(exp, interpretationEnv);
    if (verbose) {
      console.log('interpreted value:', interpretedValue);
    }
    if ((interpretedValue === null) || (interpretedValue === undefined)) {
      assertEqual(interpretedValue === expectedValue, true);
    } else {
      assertEqual(interpretedValue, expectedValue);
    }
  }
  if (compile) {
    const compilationEnv: any = extendEnvironment(new LispEnvironment(), env);
    let compiledJs: any = compileLisp(exp, compilationEnv, compilationOptions);
    if (wrapParens) {
      compiledJs = '(' + compiledJs + ')';
    }
    if (verbose) {
      console.log('compiled expression:', compiledJs);
    }
    compiledValue = eval(compiledJs);
    if (verbose) {
      console.log('compiled value:', compiledValue);
    }
    if ((compiledValue === null) || (compiledValue === undefined)) {
      assertEqual(compiledValue === expectedValue, true);
    } else {
      assertEqual(compiledValue, expectedValue);
    }
  }
  if (interpretFlag) {
    return interpretedValue;
  } else {
    return compiledValue;
  }
}

/**
 * Test a REPL form.
 */
function testRepl(exp: any, options: any = {}): any {
  let {env} = options;
  env = env || new LispEnvironment();
  const _value: any = getReplFormType(exp);
  if ([Symbol.for('javascript'), Symbol.for('js'), Symbol.for('node')].findIndex(function (x: any): any {
    return equalp(_value, x);
  }) >= 0) {
    return testNodeRepl(exp);
  } else {
    return testRoselispRepl(exp, options);
  }
}

/**
 * Test a Node REPL form.
 */
function testNodeRepl(exp: any): any {
  const clauses: any = parseReplForm(exp);
  for (let clause of clauses) {
    const expected: any = eval(clause[0]);
    if (clause[1] !== '_') {
      let actual: any = eval(clause[1]);
      assertEqual(expected, actual);
    }
  }
}

/**
 * Test a Roselisp REPL form.
 */
function testRoselispRepl(exp: any, options: any = {}): any {
  let {compile: compileOption, verbose: verboseOption, env} = {
    compile: false,
    ...// #t
    options
  };
  if (verboseOption) {
    console.log('Roselisp REPL form: ', exp);
  }
  const testEnv: any = extendEnvironment(new LispEnvironment(), env || new LispEnvironment());
  const clauses: any = parseReplForm(exp);
  for (let clause of clauses) {
    const expected: any = interpret(clause[0], testEnv);
    if (clause[1] !== Symbol.for('_')) {
      let actual: any = interpret(clause[1], testEnv);
      assertEqual(expected, actual);
    }
  }
  if (compileOption) {
    const nodeReplForm: any = compileReplForm(simplifyReplForm(exp), {
      from: 'roselisp',
      to: 'node'
    });
    if (verboseOption) {
      console.log('Node REPL form: ', nodeReplForm);
    }
    return testRepl(nodeReplForm, testEnv, options);
  }
}

/**
 * Parse a REPL form, i.e., an expression on the form:
 *
 *     (repl
 *      > (+ 1 1)
 *      2
 *      > (+ 2 2)
 *      4)
 *
 * Returns a list of `(input output)` tuples.
 */
function parseReplForm(exp: any): any {
  const form: any = (exp.length === 0) ? exp : ((exp[0] === Symbol.for('>')) ? exp : ((exp[0] === Symbol.for('$')) ? exp.slice(2) : exp.slice(1)));
  const result: any = [];
  const _end: any = form.length;
  for (let i: any = 0; i < _end; i = i + 3) {
    result.push([form[i + 1], form[i + 2]]);
  }
  return result;
}

function getReplFormType(exp: any): any {
  if (exp.length === 0) {
    return Symbol.for('roselisp');
  } else if (exp[0] === Symbol.for('>')) {
    return Symbol.for('roselisp');
  } else if ((exp.length >= 2) && (exp[0] === Symbol.for('$'))) {
    return exp[1];
  } else {
    return exp[0];
  }
}

/**
 * Simplify a REPL form of multiple clauses
 * to a single-clause form.
 */
function simplifyReplForm(exp: any): any {
  const clauses: any = parseReplForm(exp);
  if (clauses.length <= 1) {
    return exp;
  } else {
    return [getReplFormType(exp), Symbol.for('>'), [Symbol.for('begin'), ...clauses.map(function (x: any): any {
      return x[0];
    })], clauses.at(-1)[1]];
  }
}

/**
 * Compile a REPL form from one language
 * to another.
 */
function compileReplForm(exp: any, options: any = {}): any {
  const fromOption: any = options['from'];
  const toOption: any = options['to'];
  if (fromOption === 'roselisp') {
    if (toOption === 'node') {
      const clauses: any = parseReplForm(exp);
      const result: any = [Symbol.for('node')];
      for (let clause of clauses) {
        result.push(Symbol.for('>'));
        result.push(compile([[Symbol.for('lambda'), [], clause[0]]]));
        result.push((clause[1] === Symbol.for('_')) ? '_' : compile([[Symbol.for('lambda'), [], clause[1]]]));
      }
      return result;
    } else {
      return exp;
    }
  } else {
    return exp;
  }
}

/**
 * Whether `exp` is a list whose first element is `tag`.
 */
function taggedListP(exp: any, tag: any): any {
  return Array.isArray(exp) && (exp.length >= 1) && (exp[0] === tag);
}

function printSexp(exp: any): any {
  if (exp === undefined) {
    return '#u';
  } else if (exp === null) {
    return '#n';
  } else if (typeof exp === 'boolean') {
    if (exp) {
      return '#t';
    } else {
      return '#f';
    }
  } else if (taggedListP(exp, Symbol.for('quote'))) {
    return '\'' + printSexp(exp[1]);
  } else if (taggedListP(exp, Symbol.for('quasiquote'))) {
    return '`' + printSexp(exp[1]);
  } else if (taggedListP(exp, Symbol.for('unquote'))) {
    return ',' + printSexp(exp[1]);
  } else if (taggedListP(exp, Symbol.for('unquote-splicing'))) {
    return ',@' + printSexp(exp[1]);
  } else if (Array.isArray(exp)) {
    return '(' + exp.map(function (x: any): any {
      return printSexp(x);
    }).join(' ') + ')';
  } else if (typeof exp === 'string') {
    return '"' + exp.replace(new RegExp('\\\\', 'g'), '\\\\').replace(new RegExp('"', 'g'), '\\"') + '"';
  } else if (typeof exp === 'symbol') {
    return exp.description as string;
  } else {
    return exp + '';
  }
}

/**
 * Macro for expanding tests written in "REPL style"
 * to Mocha tests.
 */
function testMacro(exp: any, env: any): any {
  const body: any = exp.slice(1);
  // Parse options.
  const options: any = {};
  let bodyExps: any = [];
  const _end: any = body.length;
  for (let i: any = 0; i < _end; i = i + 2) {
    const exp: any = (body as any)[i];
    if (keywordp(exp)) {
      const key: any = (exp.description as string).replace(new RegExp('^:'), '');
      const val: any = body[i + 1];
      (options as any)[key] = val;
    } else {
      bodyExps = body.slice(i);
      break;
    }
  }
  const replOption: any = options['repl'];
  // Create tests.
  let group: any = [];
  const groups: any = [];
  let only: any = false;
  const _end1: any = bodyExps.length;
  for (let i: any = 0; i < _end1; i = i + 3) {
    const prompt: any = (bodyExps as any)[i];
    const exp: any = bodyExps[i + 1];
    const expected: any = bodyExps[i + 2];
    if (Array.isArray(exp) && (exp.length >= 2) && (exp[0] === Symbol.for('describe'))) {
      if (group.length > 0) {
        groups.push(group);
        group = [];
      }
      let description: any = exp[1];
      group.push(description);
    } else if (Array.isArray(exp) && (exp.length >= 1) && (exp[0] === Symbol.for('only'))) {
      only = true;
    } else {
      const f: any = (prompt === Symbol.for('xit>')) ? [Symbol.for('xit')] : ((only || [Symbol.for('it.only>'), Symbol.for('only>')].includes(prompt)) ? [Symbol.for('send'), Symbol.for('it'), Symbol.for('only')] : [Symbol.for('it')]);
      let description: any = '';
      let actual: any = undefined;
      if (taggedListP(exp, Symbol.for('it'))) {
        description = exp[1];
        actual = (exp.length > 3) ? [Symbol.for('begin'), ...exp.slice(2)] : exp[2];
      } else {
        description = printSexp(exp);
        actual = exp;
      }
      const test: any = ((expected === Symbol.for('_')) && !taggedListP(exp, Symbol.for('it'))) ? actual : [...f, description, [Symbol.for('fn'), [], ...(replOption ? [[Symbol.for('test-repl'), [Symbol.for('quote'), [Symbol.for('roselisp'), prompt, actual, expected]]]] : ((expected === Symbol.for('_')) ? (taggedListP(actual, Symbol.for('begin')) ? actual.slice(1) : [actual]) : [[Symbol.for('assert-equal'), actual, expected]]))]];
      group.push(test);
      only = false;
    }
  }
  if (group.length > 0) {
    groups.push(group);
  }
  const tests: any = groups.map(function (group: any): any {
    return [Symbol.for('describe'), group[0], [Symbol.for('fn'), [], ...group.slice(1)]];
  });
  return [Symbol.for('begin'), ...tests];
}

testMacro.ftype = 'macro';

export {
  testRepl as testReplForm,
  testRepl as testShellForm,
  assertEqual,
  assertNotEqual,
  assertThrows,
  compileReplForm,
  parseReplForm,
  printSexp,
  simplifyReplForm,
  testLisp,
  testMacro,
  testRepl
};