/**
 * # Test utilities
 *
 * Various helper functions for testing Lisp code.
 */

import * as chai from 'chai';

import {
  LispEnvironment,
  compile,
  compileLisp,
  extendEnvironment,
  interpret,
  printSexp,
  writeToString,
} from '../../src/ts/language';

const [equalp, lastCdr, length, last]: any[] = ((): any => {
  function equalp_(x: any, y: any): any {
    if (x === y) {
      return true;
    } else if (
      Array.isArray(x) &&
      x.length >= 3 &&
      x[x.length - 2] === Symbol.for('.') &&
      Array.isArray(y)
    ) {
      const cdrX: any =
        Array.isArray(x) && x.length === 3 && x[1] === Symbol.for('.')
          ? x[2]
          : x.slice(1);
      if (
        Array.isArray(x) &&
        x.length >= 3 &&
        x[x.length - 2] === Symbol.for('.') &&
        x.length === 3 &&
        !Array.isArray(cdrX) &&
        !(
          Array.isArray(cdrX) &&
          cdrX.length >= 3 &&
          cdrX[cdrX.length - 2] === Symbol.for('.')
        )
      ) {
        return false;
      } else if (equalp_(x[0], y[0])) {
        return equalp_(
          cdrX,
          ((): any => {
            function cdr_(lst: any): any {
              if (
                Array.isArray(lst) &&
                lst.length === 3 &&
                lst[1] === Symbol.for('.')
              ) {
                return lst[2];
              } else {
                return lst.slice(1);
              }
            }
            return cdr_;
          })()(y)
        );
      } else {
        return false;
      }
    } else if (
      Array.isArray(x) &&
      Array.isArray(y) &&
      y.length >= 3 &&
      y[y.length - 2] === Symbol.for('.')
    ) {
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
    } else if (x instanceof Map && y instanceof Map) {
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
    } else if (
      x !== null &&
      typeof x === 'object' &&
      y !== null &&
      typeof y === 'object'
    ) {
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
  function lastCdr_(lst: any): any {
    if (!Array.isArray(lst)) {
      return undefined;
    } else if (
      Array.isArray(lst) &&
      lst.length >= 3 &&
      lst[lst.length - 2] === Symbol.for('.')
    ) {
      let result: any = lst;
      while (
        Array.isArray(result) &&
        result.length >= 3 &&
        result[result.length - 2] === Symbol.for('.')
      ) {
        result = result[result.length - 1];
      }
      return result;
    } else {
      return [];
    }
  }
  function length_(lst: any): any {
    if (
      Array.isArray(lst) &&
      lst.length >= 3 &&
      lst[lst.length - 2] === Symbol.for('.')
    ) {
      return ((): any => {
        function linkedListLength_(lst: any): any {
          let len: any = 0;
          let current: any = lst;
          while (
            Array.isArray(current) &&
            current.length >= 3 &&
            current[current.length - 2] === Symbol.for('.')
          ) {
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
  function last_(lst: any): any {
    if (
      Array.isArray(lst) &&
      lst.length >= 3 &&
      lst[lst.length - 2] === Symbol.for('.')
    ) {
      return ((): any => {
        function linkedListLast_(lst: any): any {
          let current: any = lst;
          let result: any = undefined;
          while (
            Array.isArray(current) &&
            current.length >= 3 &&
            current[current.length - 2] === Symbol.for('.') &&
            !((): any => {
              const x: any = current[current.length - 1];
              return Array.isArray(x) && x.length === 0;
            })()
          ) {
            current = current[current.length - 1];
          }
          if (
            Array.isArray(current) &&
            current.length >= 3 &&
            current[current.length - 2] === Symbol.for('.')
          ) {
            result = current[current.length - 3];
          }
          return result;
        }
        return linkedListLast_;
      })()(lst);
    } else {
      return lst[lst.length - 1];
    }
  }
  function cdr_(lst: any): any {
    if (Array.isArray(lst) && lst.length === 3 && lst[1] === Symbol.for('.')) {
      return lst[2];
    } else {
      return lst.slice(1);
    }
  }
  function linkedListLength_(lst: any): any {
    let len: any = 0;
    let current: any = lst;
    while (
      Array.isArray(current) &&
      current.length >= 3 &&
      current[current.length - 2] === Symbol.for('.')
    ) {
      len = len + (lst.length - 2);
      current = current[current.length - 1];
    }
    return len;
  }
  function linkedListLast_(lst: any): any {
    let current: any = lst;
    let result: any = undefined;
    while (
      Array.isArray(current) &&
      current.length >= 3 &&
      current[current.length - 2] === Symbol.for('.') &&
      !((): any => {
        const x: any = current[current.length - 1];
        return Array.isArray(x) && x.length === 0;
      })()
    ) {
      current = current[current.length - 1];
    }
    if (
      Array.isArray(current) &&
      current.length >= 3 &&
      current[current.length - 2] === Symbol.for('.')
    ) {
      result = current[current.length - 3];
    }
    return result;
  }
  return [equalp_, lastCdr_, length_, last_];
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
function testLisp(
  exp: any = undefined,
  val: any = undefined,
  options: any = {}
): any {
  // FIXME: `exp` *might* be modified by side-effect. If so, the
  // compilation test will receive a different value. We should
  // clone the value to avoid this.
  let {
    compile,
    env,
    interpret: interpretFlag,
    interpretValue: interpretValueOption,
    verbose,
    wrapParens,
  } = options;
  env = env || new LispEnvironment();
  const expectedValue: any = interpretValueOption ? interpret(val, env) : val;
  const evaluationOptions: any = {
    ...options,
  };
  delete evaluationOptions.compile;
  const compilationOptions: any = evaluationOptions;
  interpretFlag = interpretFlag === undefined ? true : compile;
  compile = compile === undefined ? true : compile;
  let interpretedValue: any = undefined;
  let compiledValue: any = undefined;
  if (verbose) {
    console.log('expression:', writeToString(exp));
  }
  if (interpretFlag) {
    const interpretationEnv: any = extendEnvironment(
      new LispEnvironment(),
      env
    );
    interpretedValue = interpret(exp, interpretationEnv);
    if (verbose) {
      console.log('interpreted value:', interpretedValue);
    }
    if (interpretedValue === null || interpretedValue === undefined) {
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
    if (compiledValue === null || compiledValue === undefined) {
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
  let { env } = options;
  env = env || new LispEnvironment();
  const _value: any = getReplFormType(exp);
  if (
    [Symbol.for('javascript'), Symbol.for('js'), Symbol.for('node')].findIndex(
      function (x: any): any {
        return equalp(_value, x);
      }
    ) >= 0
  ) {
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
    const actual: any = eval(clause[0]);
    if (
      (Array.isArray(clause) &&
      clause.length >= 3 &&
      clause[clause.length - 2] === Symbol.for('.') &&
      ((): any => {
        const x: any = lastCdr(clause);
        return Array.isArray(x) && x.length === 0;
      })()
        ? ((): any => {
            let i: any = 1;
            let result: any = clause;
            while (i > 0) {
              if (
                Array.isArray(result) &&
                result.length === 3 &&
                result[1] === Symbol.for('.')
              ) {
                result = clause[clause.length - 1];
              } else {
                result = clause.slice(1);
              }
              i--;
            }
            if (Array.isArray(result)) {
              result = result[0];
            }
            return result;
          })()
        : clause[1]) !== '_'
    ) {
      const expected: any = eval(
        Array.isArray(clause) &&
          clause.length >= 3 &&
          clause[clause.length - 2] === Symbol.for('.') &&
          ((): any => {
            const x: any = lastCdr(clause);
            return Array.isArray(x) && x.length === 0;
          })()
          ? ((): any => {
              let i: any = 1;
              let result: any = clause;
              while (i > 0) {
                if (
                  Array.isArray(result) &&
                  result.length === 3 &&
                  result[1] === Symbol.for('.')
                ) {
                  result = clause[clause.length - 1];
                } else {
                  result = clause.slice(1);
                }
                i--;
              }
              if (Array.isArray(result)) {
                result = result[0];
              }
              return result;
            })()
          : clause[1]
      );
      assertEqual(actual, expected);
    }
  }
}

/**
 * Test a Roselisp REPL form.
 */
function testRoselispRepl(exp: any, options: any = {}): any {
  let {
    compile: compileOption,
    verbose: verboseOption,
    env,
  } = {
    compile: true,
    ...options,
  };
  if (verboseOption) {
    console.log('Roselisp REPL form: ', exp);
  }
  const testEnv: any = extendEnvironment(
    new LispEnvironment(),
    env || new LispEnvironment()
  );
  const clauses: any = parseReplForm(exp);
  for (let clause of clauses) {
    const actual: any = interpret(clause[0], testEnv);
    if (
      (Array.isArray(clause) &&
      clause.length >= 3 &&
      clause[clause.length - 2] === Symbol.for('.') &&
      ((): any => {
        const x: any = lastCdr(clause);
        return Array.isArray(x) && x.length === 0;
      })()
        ? ((): any => {
            let i: any = 1;
            let result: any = clause;
            while (i > 0) {
              if (
                Array.isArray(result) &&
                result.length === 3 &&
                result[1] === Symbol.for('.')
              ) {
                result = clause[clause.length - 1];
              } else {
                result = clause.slice(1);
              }
              i--;
            }
            if (Array.isArray(result)) {
              result = result[0];
            }
            return result;
          })()
        : clause[1]) !== Symbol.for('_')
    ) {
      const expected: any = interpret(
        Array.isArray(clause) &&
          clause.length >= 3 &&
          clause[clause.length - 2] === Symbol.for('.') &&
          ((): any => {
            const x: any = lastCdr(clause);
            return Array.isArray(x) && x.length === 0;
          })()
          ? ((): any => {
              let i: any = 1;
              let result: any = clause;
              while (i > 0) {
                if (
                  Array.isArray(result) &&
                  result.length === 3 &&
                  result[1] === Symbol.for('.')
                ) {
                  result = clause[clause.length - 1];
                } else {
                  result = clause.slice(1);
                }
                i--;
              }
              if (Array.isArray(result)) {
                result = result[0];
              }
              return result;
            })()
          : clause[1],
        testEnv
      );
      assertEqual(actual, expected);
    }
  }
  if (compileOption) {
    const nodeReplForm: any = compileReplForm(simplifyReplForm(exp), {
      from: 'roselisp',
      to: 'node',
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
  const form: any =
    length(exp) === 0
      ? exp
      : exp[0] === Symbol.for('>')
      ? exp
      : exp[0] === Symbol.for('$')
      ? exp.slice(2)
      : exp.slice(1);
  let result: any = [];
  const _end: any = length(form);
  for (let i: any = 0; i < _end; i = i + 3) {
    result.push([
      ((): any => {
        const n: any = i + 1;
        if (
          Array.isArray(form) &&
          form.length >= 3 &&
          form[form.length - 2] === Symbol.for('.')
        ) {
          let i: any = n;
          let result: any = form;
          while (i > 0) {
            if (
              Array.isArray(result) &&
              result.length === 3 &&
              result[1] === Symbol.for('.')
            ) {
              result = form[form.length - 1];
            } else {
              result = form.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        } else {
          return (form as any)[n];
        }
      })(),
      ((): any => {
        const n: any = i + 2;
        if (
          Array.isArray(form) &&
          form.length >= 3 &&
          form[form.length - 2] === Symbol.for('.')
        ) {
          let i: any = n;
          let result: any = form;
          while (i > 0) {
            if (
              Array.isArray(result) &&
              result.length === 3 &&
              result[1] === Symbol.for('.')
            ) {
              result = form[form.length - 1];
            } else {
              result = form.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        } else {
          return (form as any)[n];
        }
      })(),
    ]);
  }
  return result;
}

function getReplFormType(exp: any): any {
  if (length(exp) === 0) {
    return Symbol.for('roselisp');
  } else if (exp[0] === Symbol.for('>')) {
    return Symbol.for('roselisp');
  } else if (length(exp) >= 2 && exp[0] === Symbol.for('$')) {
    if (
      Array.isArray(exp) &&
      exp.length >= 3 &&
      exp[exp.length - 2] === Symbol.for('.') &&
      ((): any => {
        const x: any = lastCdr(exp);
        return Array.isArray(x) && x.length === 0;
      })()
    ) {
      let i: any = 1;
      let result: any = exp;
      while (i > 0) {
        if (
          Array.isArray(result) &&
          result.length === 3 &&
          result[1] === Symbol.for('.')
        ) {
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
    } else {
      return exp[1];
    }
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
  if (length(clauses) <= 1) {
    return exp;
  } else {
    return [
      getReplFormType(exp),
      Symbol.for('>'),
      [
        Symbol.for('begin'),
        ...clauses.map(function (x: any): any {
          return x[0];
        }),
      ],
      ((): any => {
        const lst: any = last(clauses);
        if (
          Array.isArray(lst) &&
          lst.length >= 3 &&
          lst[lst.length - 2] === Symbol.for('.') &&
          ((): any => {
            const x: any = lastCdr(lst);
            return Array.isArray(x) && x.length === 0;
          })()
        ) {
          let i: any = 1;
          let result: any = lst;
          while (i > 0) {
            if (
              Array.isArray(result) &&
              result.length === 3 &&
              result[1] === Symbol.for('.')
            ) {
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
      })(),
    ];
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
      let result: any = [Symbol.for('node')];
      for (let clause of clauses) {
        result.push(Symbol.for('>'));
        result.push(compile(clause[0]));
        result.push(
          (Array.isArray(clause) &&
          clause.length >= 3 &&
          clause[clause.length - 2] === Symbol.for('.') &&
          ((): any => {
            const x: any = lastCdr(clause);
            return Array.isArray(x) && x.length === 0;
          })()
            ? ((): any => {
                let i: any = 1;
                let result: any = clause;
                while (i > 0) {
                  if (
                    Array.isArray(result) &&
                    result.length === 3 &&
                    result[1] === Symbol.for('.')
                  ) {
                    result = clause[clause.length - 1];
                  } else {
                    result = clause.slice(1);
                  }
                  i--;
                }
                if (Array.isArray(result)) {
                  result = result[0];
                }
                return result;
              })()
            : clause[1]) === Symbol.for('_')
            ? '_'
            : compile(
                Array.isArray(clause) &&
                  clause.length >= 3 &&
                  clause[clause.length - 2] === Symbol.for('.') &&
                  ((): any => {
                    const x: any = lastCdr(clause);
                    return Array.isArray(x) && x.length === 0;
                  })()
                  ? ((): any => {
                      let i: any = 1;
                      let result: any = clause;
                      while (i > 0) {
                        if (
                          Array.isArray(result) &&
                          result.length === 3 &&
                          result[1] === Symbol.for('.')
                        ) {
                          result = clause[clause.length - 1];
                        } else {
                          result = clause.slice(1);
                        }
                        i--;
                      }
                      if (Array.isArray(result)) {
                        result = result[0];
                      }
                      return result;
                    })()
                  : clause[1]
              )
        );
      }
      return result;
    } else {
      return exp;
    }
  } else {
    return exp;
  }
}

export {
  assertEqual,
  assertNotEqual,
  assertThrows,
  compileReplForm,
  parseReplForm,
  simplifyReplForm,
  testLisp,
  testRepl,
  testRepl as testReplForm,
  testRepl as testShellForm,
};
