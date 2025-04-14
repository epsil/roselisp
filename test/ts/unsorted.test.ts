/**
 * # Unsorted tests
 *
 * Tests that have not been sorted yet.
 */

import * as chai from 'chai';

import { EnvironmentStack, LispEnvironment } from '../../src/ts/env';

import {
  compile,
  compilationEnvironment,
  writeToString,
} from '../../src/ts/language';

import { assertEqual, testLisp, testRepl } from './test-util';

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
  it('unsorted', function (): any {
    return testRepl(
      // Move test code from here into their own tests.
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('flatten'), [Symbol.for('quote'), [[[[4]]]]]],
        [Symbol.for('quote'), [4]],
      ]
    );
  });
  it('2 + 2', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+'), 2, 2],
      4,
    ]);
  });
  it('((define (foo . args) args))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        [Symbol.for('foo'), Symbol.for('.'), Symbol.for('args')],
        Symbol.for('args'),
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [Symbol.for('foo')],
      [Symbol.for('quote'), []],
    ]);
  });
  describe('equal?', function (): any {
    it('(equal? 1 1)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('equal?'), 1, 1],
        Symbol.for('#t'),
      ]);
    });
    return it("(equal? '() '())", function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('equal?'),
          [Symbol.for('quote'), []],
          [Symbol.for('quote'), []],
        ],
        Symbol.for('#t'),
      ]);
    });
  });
  describe('define-js-obj', function (): any {
    it('(define-js-obj (foo) ...)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('define-js-obj'),
          [Symbol.for('foo')],
          [Symbol.for('js-obj'), 'foo', 'bar'],
        ],
        Symbol.for('undefined'),
        Symbol.for('>'),
        Symbol.for('foo'),
        'bar',
      ]);
    });
    return it('(define-js-obj ((foo bar)) ...)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('define-js-obj'),
          [[Symbol.for('foo'), Symbol.for('bar')]],
          [Symbol.for('js-obj'), 'foo', 'bar'],
        ],
        Symbol.for('undefined'),
        Symbol.for('>'),
        Symbol.for('bar'),
        'bar',
      ]);
    });
  });
  describe('call/cc', function (): any {
    it('(+ 5 (call/cc (lambda (x) (* 10 3))))', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('+'),
          5,
          [
            Symbol.for('call/cc'),
            [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('*'), 10, 3]],
          ],
        ],
        35,
      ]);
    });
    it('(+ 5 (call/cc (lambda (x) (* 10 (x 3)))))', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('+'),
          5,
          [
            Symbol.for('call/cc'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x')],
              [Symbol.for('*'), 10, [Symbol.for('x'), 3]],
            ],
          ],
        ],
        8,
      ]);
    });
    it('(+ 5 (call/cc (lambda (x) (x 10) 3)))', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('+'),
          5,
          [
            Symbol.for('call/cc'),
            [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('x'), 10], 3],
          ],
        ],
        15,
      ]);
    });
    it('(+ 5 (call/cc (lambda (x) (x 10) (error "error"))))', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('+'),
          5,
          [
            Symbol.for('call/cc'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x')],
              [Symbol.for('x'), 10],
              [Symbol.for('error'), 'error'],
            ],
          ],
        ],
        15,
      ]);
    });
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
    it('(string-split "foo bar  baz")', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('string-split'), 'foo bar  baz'],
        [Symbol.for('quote'), ['foo', 'bar', 'baz']],
      ]);
    });
    it('(string-split "foo,bar,baz" ",")', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('string-split'), 'foo,bar,baz', ','],
        [Symbol.for('quote'), ['foo', 'bar', 'baz']],
      ]);
    });
    it('(string-split "foo, bar, baz" ", ")', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('string-split'), 'foo, bar, baz', ', '],
        [Symbol.for('quote'), ['foo', 'bar', 'baz']],
      ]);
    });
    it('(string-split "foo\\nbar\\nbaz" "\\n")', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('string-split'), 'foo\n' + 'bar\n' + 'baz', '\n'],
        [Symbol.for('quote'), ['foo', 'bar', 'baz']],
      ]);
    });
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
        Symbol.for('undefined'),
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
  describe('cons dot', function (): any {
    it('*cons-dot*', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        Symbol.for('*cons-dot*'),
        [Symbol.for('quote'), Symbol.for('.')],
      ]);
    });
    it('cons-dot', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('cons-dot')],
        [Symbol.for('quote'), Symbol.for('.')],
      ]);
    });
    return it('cons-dot?', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('cons-dot?'), Symbol.for('*cons-dot*')],
        Symbol.for('#t'),
      ]);
    });
  });
  describe('define-class', function (): any {
    return it('(define-class Foo () ...)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
        ],
        Symbol.for('undefined'),
        Symbol.for('>'),
        [
          Symbol.for('define'),
          Symbol.for('foo'),
          [Symbol.for('new'), Symbol.for('Foo')],
        ],
        Symbol.for('undefined'),
        Symbol.for('>'),
        [Symbol.for('send'), Symbol.for('foo'), Symbol.for('bar')],
        'bar',
      ]);
    });
  });
  describe('defclass', function (): any {
    return it('(defclass Foo () ...)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('defclass'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
        ],
        Symbol.for('undefined'),
        Symbol.for('>'),
        [
          Symbol.for('define'),
          Symbol.for('foo'),
          [Symbol.for('new'), Symbol.for('Foo')],
        ],
        Symbol.for('undefined'),
        Symbol.for('>'),
        [Symbol.for('send'), Symbol.for('foo'), Symbol.for('bar')],
        'bar',
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
    it('EnvironmentStack', function (): any {
      const options: any = {};
      compile(Symbol.for('foo'), undefined, options);
      const continuationEnv: any = options['continuationEnv'];
      return assertEqual(continuationEnv instanceof EnvironmentStack, true);
    });
    return it('has', function (): any {
      const options: any = {};
      compile([Symbol.for('define'), Symbol.for('foo'), 1], undefined, options);
      const continuationEnv: any = options['continuationEnv'];
      return assertEqual(continuationEnv.has(Symbol.for('foo')), true);
    });
  });
  describe('dotted lists', function (): any {
    return it('equal?', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('equal?'),
            [Symbol.for('quote'), [1, 2]],
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('empty list', function (): any {
    it('cons?', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('cons?'), [Symbol.for('quote'), []]],
          Symbol.for('#f'),
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
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('list?', function (): any {
    it('empty list', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('list?'), [Symbol.for('quote'), []]],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it('dotted pair', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
          Symbol.for('#f'),
        ],
        {
          compile: false,
        }
      );
    });
    it('dotted list', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('list?'),
            [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
          ],
          Symbol.for('#f'),
        ],
        {
          compile: false,
        }
      );
    });
    it("dotted list '(1 . ())", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('list?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), []]],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    return it("dotted list '(1 . (2 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('list?'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('vector?', function (): any {
    it('empty list', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('vector?'), [Symbol.for('quote'), []]],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it('dotted pair', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('vector?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it('dotted list', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('vector?'),
            [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it("dotted list '(1 . ())", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('vector?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), []]],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    return it("dotted list '(1 . (2 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('vector?'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('dotted-list-length', function (): any {
    it('()', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('dotted-list-length'), [Symbol.for('quote'), []]],
          0,
        ],
        {
          compile: false,
        }
      );
    });
    it("'(1 . ())", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list-length'),
            [Symbol.for('quote'), [1, Symbol.for('.'), []]],
          ],
          1,
        ],
        {
          compile: false,
        }
      );
    });
    return it("'(1 . (2 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list-length'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          2,
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('dotted-list-last', function (): any {
    it('()', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('dotted-list-last'), [Symbol.for('quote'), []]],
          Symbol.for('undefined'),
        ],
        {
          compile: false,
        }
      );
    });
    it("'(1 . ())", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list-last'),
            [Symbol.for('quote'), [1, Symbol.for('.'), []]],
          ],
          1,
        ],
        {
          compile: false,
        }
      );
    });
    return it("'(1 . (2 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list-last'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          2,
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('dotted-list-last-cdr', function (): any {
    it('()', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('dotted-list-last-cdr'), [Symbol.for('quote'), []]],
          [Symbol.for('quote'), []],
        ],
        {
          compile: false,
        }
      );
    });
    it("'(1 . ())", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list-last-cdr'),
            [Symbol.for('quote'), [1, Symbol.for('.'), []]],
          ],
          [Symbol.for('quote'), []],
        ],
        {
          compile: false,
        }
      );
    });
    return it("'(1 . (2 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list-last-cdr'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          [Symbol.for('quote'), []],
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('nth', function (): any {
    it("(nth 1 '(1 . (2 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('nth'),
            1,
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          2,
        ],
        {
          compile: false,
        }
      );
    });
    return it("(nth 1 '(1 . (2 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('nth'),
            1,
            [
              Symbol.for('quote'),
              [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]],
            ],
          ],
          2,
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('cdr', function (): any {
    it("(cdr '(1 . (2 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('cdr'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          [Symbol.for('quote'), [2, Symbol.for('.'), []]],
        ],
        {
          compile: false,
        }
      );
    });
    return it("(cdr '(1 . (2 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('cdr'),
            [
              Symbol.for('quote'),
              [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]],
            ],
          ],
          [Symbol.for('quote'), [2, Symbol.for('.'), [3, Symbol.for('.'), []]]],
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('array-list?', function (): any {
    it("(array-list? '())", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('array-list?'), [Symbol.for('quote'), []]],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(array-list? '(1 . 2))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('array-list?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(array-list? '(1 2))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('array-list?'), [Symbol.for('quote'), [1, 2]]],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    return it("(array-list? '(1 2 3))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('array-list?'), [Symbol.for('quote'), [1, 2, 3]]],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('linked-list?', function (): any {
    it("(linked-list? '())", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('linked-list?'), [Symbol.for('quote'), []]],
          Symbol.for('#f'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(linked-list? '(1 . 2))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('linked-list?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
          ],
          Symbol.for('#f'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(linked-list? '(1 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('linked-list?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), []]],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(linked-list? '(1 . (2 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('linked-list?'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    return it("(linked-list? '(1 2 . (3 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('linked-list?'),
            [
              Symbol.for('quote'),
              [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]],
            ],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('linked-list-link?', function (): any {
    it("(linked-list-link? '())", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('linked-list-link?'), [Symbol.for('quote'), []]],
          Symbol.for('#f'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(linked-list-link? '(1 . 2))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('linked-list-link?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(linked-list-link? '(1 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('linked-list-link?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), []]],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(linked-list-link? '(1 . (2 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('linked-list-link?'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    return it("(linked-list-link? '(1 2 . (3 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('linked-list-link?'),
            [
              Symbol.for('quote'),
              [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]],
            ],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('dotted-list?', function (): any {
    it("(dotted-list? '())", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('dotted-list?'), [Symbol.for('quote'), []]],
          Symbol.for('#f'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(dotted-list? '(1 . 2))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(dotted-list? '(1 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list?'),
            [Symbol.for('quote'), [1, Symbol.for('.'), []]],
          ],
          Symbol.for('#f'),
        ],
        {
          compile: false,
        }
      );
    });
    it("(dotted-list? '(1 . (2 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list?'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          Symbol.for('#f'),
        ],
        {
          compile: false,
        }
      );
    });
    return it("(dotted-list? '(1 . (2 . 3)))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('dotted-list?'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), 3]],
            ],
          ],
          Symbol.for('#t'),
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('length', function (): any {
    it("(length '(1 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('length'),
            [Symbol.for('quote'), [1, Symbol.for('.'), []]],
          ],
          1,
        ],
        {
          compile: false,
        }
      );
    });
    it("(length '(1 . (2 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('length'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          2,
        ],
        {
          compile: false,
        }
      );
    });
    return it("(length '(1 2 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('length'),
            [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]],
          ],
          2,
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('last', function (): any {
    it("(last '(1 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('last'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
          1,
        ],
        {
          compile: false,
        }
      );
    });
    it("(last '(1 . (2 . ())))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('last'),
            [
              Symbol.for('quote'),
              [1, Symbol.for('.'), [2, Symbol.for('.'), []]],
            ],
          ],
          2,
        ],
        {
          compile: false,
        }
      );
    });
    return it("(last '(1 2 . ()))", function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('last'),
            [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]],
          ],
          2,
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('js-keys', function (): any {
    it('(js-keys (js-obj))', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('js-keys'), [Symbol.for('js-obj')]],
          [Symbol.for('quote'), []],
        ],
        {
          compile: false,
        }
      );
    });
    it('(js-keys (js-obj "foo" "bar"))', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('js-keys'), [Symbol.for('js-obj'), 'foo', 'bar']],
          [Symbol.for('quote'), ['foo']],
        ],
        {
          compile: false,
        }
      );
    });
    return it('(js-keys (js-obj "foo" "bar" "baz" "quux"))', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('js-keys'),
            [Symbol.for('js-obj'), 'foo', 'bar', 'baz', 'quux'],
          ],
          [Symbol.for('quote'), ['foo', 'baz']],
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('string-join', function (): any {
    it('(string-join \'("foo" "bar"))', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('string-join'), [Symbol.for('quote'), ['foo', 'bar']]],
          'foo bar',
        ],
        {
          compile: false,
        }
      );
    });
    return it('(string-join \'("foo" "bar"))', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('string-join'),
            [Symbol.for('quote'), ['foo', 'bar']],
            ',',
          ],
          'foo,bar',
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('string-upcase', function (): any {
    return it('(string-upcase "foo")', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('string-upcase'), 'foo'],
          'FOO',
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('string-downcase', function (): any {
    return it('(string-downcase "FOO")', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('string-downcase'), 'FOO'],
          'foo',
        ],
        {
          compile: false,
        }
      );
    });
  });
  describe('plist->alist', function (): any {
    it("(plist->alist '())", function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('plist->alist'), [Symbol.for('quote'), []]],
        [Symbol.for('quote'), []],
      ]);
    });
    it("(plist->alist '(foo bar))", function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('plist->alist'),
          [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
        ],
        [
          Symbol.for('quote'),
          [[Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]],
        ],
      ]);
    });
    return it("(plist->alist '(foo bar baz quux))", function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('plist->alist'),
          [
            Symbol.for('quote'),
            [
              Symbol.for('foo'),
              Symbol.for('bar'),
              Symbol.for('baz'),
              Symbol.for('quux'),
            ],
          ],
        ],
        [
          Symbol.for('quote'),
          [
            [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
            [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')],
          ],
        ],
      ]);
    });
  });
  describe('js/try', function (): any {
    it('compile (js/try ... (catch ...) (finally ...))', function (): any {
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
    it('(js/try (/ 1 2) ...)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('js/try'),
          [Symbol.for('/'), 1, 2],
          [
            Symbol.for('catch'),
            Symbol.for('e'),
            [Symbol.for('display'), 'there was an error'],
          ],
          [Symbol.for('finally'), [Symbol.for('display'), 'finally']],
        ],
        0.5,
      ]);
    });
    return it('(js/try (/ 1 3) (/ 1 2) ...)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('js/try'),
          [Symbol.for('/'), 1, 3],
          [Symbol.for('/'), 1, 2],
          [
            Symbol.for('catch'),
            Symbol.for('e'),
            [Symbol.for('display'), 'there was an error'],
          ],
          [Symbol.for('finally'), [Symbol.for('display'), 'finally']],
        ],
        0.5,
      ]);
    });
  });
  describe('instance-of?', function (): any {
    return it('(instance-of? (new Map) Map)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('instance-of?'),
          [Symbol.for('new'), Symbol.for('Map')],
          Symbol.for('Map'),
        ],
        Symbol.for('#t'),
      ]);
    });
  });
  return describe('is-a?', function (): any {
    return it('(is-a? (new Map) Map)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('is-a?'),
          [Symbol.for('new'), Symbol.for('Map')],
          Symbol.for('Map'),
        ],
        Symbol.for('#t'),
      ]);
    });
  });
});
