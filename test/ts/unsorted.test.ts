/**
 * # Unsorted tests
 *
 * Tests that have not been sorted yet.
 */

import { EnvironmentStack, LispEnvironment } from '../../src/ts/env';

import { compile, compilationEnvironment } from '../../src/ts/language';

import { assertEqual, testRepl } from './test-util';

const [callCc]: any[] = ((): any => {
  function callWithCurrentContinuation_(
    proc: any,
    promptTag: any = undefined
  ): any {
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
  return [callWithCurrentContinuation_];
})();

/**
 * Test inbox
 */
describe('Unsorted tests', function (): any {
  describe('call/cc', function (): any {
    return it('(try ... (+ 5 (call/cc (lambda (x) (error "error")))) ...)', function (): any {
      let result: any = 0;
      try {
        result =
          5 +
          callCc(function (x: any): any {
            throw new Error('error');
          });
      } catch (e) {}
      return assertEqual(result, 0);
    });
  });
  describe('string-split', function (): any {
    return xit('(string-split "  foo bar  baz \\r\\n\\t")', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('string-split'), '  foo bar  baz \n' + '\n' + '	'],
        [Symbol.for('quote'), ['foo', 'bar', 'baz']],
      ]);
    });
  });
  describe('Y combinator', function (): any {
    return it('6!', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('define'),
          [Symbol.for('Y'), Symbol.for('f')],
          [
            [
              Symbol.for('lambda'),
              [Symbol.for('future')],
              [
                Symbol.for('f'),
                [
                  Symbol.for('lambda'),
                  [Symbol.for('arg')],
                  [
                    [Symbol.for('future'), Symbol.for('future')],
                    Symbol.for('arg'),
                  ],
                ],
              ],
            ],
            [
              Symbol.for('lambda'),
              [Symbol.for('future')],
              [
                Symbol.for('f'),
                [
                  Symbol.for('lambda'),
                  [Symbol.for('arg')],
                  [
                    [Symbol.for('future'), Symbol.for('future')],
                    Symbol.for('arg'),
                  ],
                ],
              ],
            ],
          ],
        ],
        undefined,
        Symbol.for('>'),
        [
          [
            Symbol.for('Y'),
            [
              Symbol.for('lambda'),
              [Symbol.for('f')],
              [
                Symbol.for('lambda'),
                [Symbol.for('x')],
                [
                  Symbol.for('if'),
                  [Symbol.for('zero?'), Symbol.for('x')],
                  1,
                  [
                    Symbol.for('*'),
                    Symbol.for('x'),
                    [Symbol.for('f'), [Symbol.for('-'), Symbol.for('x'), 1]],
                  ],
                ],
              ],
            ],
          ],
          6,
        ],
        720,
      ]);
    });
  });
  describe('define-macro', function (): any {
    it('(define-macro (foo x) x)', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('define-macro'),
            [Symbol.for('foo'), Symbol.for('x')],
            Symbol.for('x'),
          ],
          Symbol.for('_'),
          Symbol.for('>'),
          [Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), 1]]],
          [Symbol.for('quote'), [Symbol.for('foo'), 1]],
        ],
        {
          compile: false,
        }
      );
    });
    return it('(define-macro (foo x) `(+ ,x ,x))', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('define-macro'),
            [Symbol.for('foo'), Symbol.for('x')],
            [
              Symbol.for('quasiquote'),
              [
                Symbol.for('+'),
                [Symbol.for('unquote'), Symbol.for('x')],
                [Symbol.for('unquote'), Symbol.for('x')],
              ],
            ],
          ],
          Symbol.for('_'),
          Symbol.for('>'),
          [Symbol.for('foo'), 1],
          2,
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('defmacro', function (): any {
    return it('(defmacro foo (x) x)', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('defmacro'),
            Symbol.for('foo'),
            [Symbol.for('x')],
            Symbol.for('x'),
          ],
          Symbol.for('_'),
          Symbol.for('>'),
          [Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), 1]]],
          [Symbol.for('quote'), [Symbol.for('foo'), 1]],
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('compiled environment', function (): any {
    return it('compiledEnv', function (): any {
      const options: any = {};
      compile(Symbol.for('foo'), undefined, options);
      const compiledEnv: any = options['compiledEnv'];
      return assertEqual(compiledEnv instanceof LispEnvironment, true);
    });
  });
  describe('continuation environment', function (): any {
    it('has', function (): any {
      const options: any = {};
      compile([Symbol.for('define'), Symbol.for('foo'), 1], undefined, options);
      const continuationEnv: any = options['continuationEnv'];
      return assertEqual(continuationEnv.has(Symbol.for('foo')), true);
    });
    return xit('EnvironmentStack', function (): any {
      const options: any = {};
      compile(Symbol.for('foo'), undefined, options);
      const continuationEnv: any = options['continuationEnv'];
      return assertEqual(continuationEnv instanceof EnvironmentStack, true);
    });
  });
  describe('empty list', function (): any {
    it('cons?', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('cons?'), [Symbol.for('quote'), []]],
          false,
        ],
        {
          compile: false,
        }
      );
    });
    return it('list?', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('list?'), [Symbol.for('quote'), []]],
          true,
        ],
        {
          compile: false,
        }
      );
    });
  });
  return describe('js/try', function (): any {
    return it('compile (js/try ... (catch ...) (finally ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js/try'),
            [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
            [
              Symbol.for('catch'),
              Symbol.for('e'),
              [Symbol.for('display'), 'there was an error'],
            ],
            [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'try {\n' +
          '  x = 2 / 1;\n' +
          '} catch (e) {\n' +
          "  console.log('there was an error');\n" +
          '} finally {\n' +
          "  console.log('cleanup');\n" +
          '}'
      );
    });
  });
});
