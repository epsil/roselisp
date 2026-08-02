/**
 * # Various unsorted tests
 *
 * This file functions as an "inbox" for incoming tests.
 */

import { assertEqual, testRepl, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('To do', function (): any {});

describe('require', function (): any {
  xit('(compile \'(require "foo") :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('require'), 'foo']],
        Symbol.for(':fcommonjs'),
        true,
      ],
      "let foo = require('foo');",
    ]);
  });
  xit('(compile \'(require foo "bar") :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('require'), Symbol.for('foo'), 'bar'],
        ],
        Symbol.for(':fcommonjs'),
        true,
      ],
      "let foo = require('bar');",
    ]);
  });
  xit('(compile \'(require "foo" "bar") :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('require'), 'foo', 'bar']],
        Symbol.for(':fcommonjs'),
        true,
      ],
      "let foo = require('bar');",
    ]);
  });
  xit('(compile \'(require (only-in "foo" bar)) :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('require'),
            [Symbol.for('only-in'), 'foo', Symbol.for('bar')],
          ],
        ],
        Symbol.for(':fcommonjs'),
        true,
      ],
      "let {bar} = require('foo');",
    ]);
  });
  return xit('(compile \'(require (only-in "foo" (bar baz))) :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('require'),
            [
              Symbol.for('only-in'),
              'foo',
              [Symbol.for('bar'), Symbol.for('baz')],
            ],
          ],
        ],
        Symbol.for(':fcommonjs'),
        true,
      ],
      "let {bar: baz} = require('foo');",
    ]);
  });
});

describe('provide', function (): any {
  xit("(compile '(provide x) :fcommonjs #t)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x')]],
        Symbol.for(':fcommonjs'),
        true,
      ],
      'module.exports = {\n' + '  x\n' + '};',
    ]);
  });
  xit("(compile '(provide x y) :fcommonjs #t)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('provide'), Symbol.for('x'), Symbol.for('y')],
        ],
        Symbol.for(':fcommonjs'),
        true,
      ],
      'export {\n' + '  x,\n' + '  y\n' + '};',
    ]);
  });
  return xit("(compile '(provide (rename-out (x y))) :fcommonjs #t)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('provide'),
            [Symbol.for('rename-out'), [Symbol.for('x'), Symbol.for('y')]],
          ],
        ],
        Symbol.for(':fcommonjs'),
        true,
      ],
      'export {\n' + '  x: y\n' + '};',
    ]);
  });
});

describe('gensym', function (): any {
  xit('(compile `(begin (define x 1) (define ,(gensym "x") 2) (define x1 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quasiquote'),
          [
            Symbol.for('begin'),
            [Symbol.for('define'), Symbol.for('x'), 1],
            [
              Symbol.for('define'),
              [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']],
              2,
            ],
            [Symbol.for('define'), Symbol.for('x1'), 3],
          ],
        ],
      ],
      'let x = 1;\n' + '\n' + 'let x2 = 2;\n' + '\n' + 'let x1 = 3;',
    ]);
  });
  xit('(compile `(begin (define x 1) (define ,(gensym "x") 2) (define-values (x1) (list 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quasiquote'),
          [
            Symbol.for('begin'),
            [Symbol.for('define'), Symbol.for('x'), 1],
            [
              Symbol.for('define'),
              [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']],
              2,
            ],
            [
              Symbol.for('define-values'),
              [Symbol.for('x1')],
              [Symbol.for('list'), 3],
            ],
          ],
        ],
      ],
      'let x = 1;\n' + '\n' + 'let x2 = 2;\n' + '\n' + 'let [x1] = [3];',
    ]);
  });
  return xit('(compile `(begin (define x 1) (define ,(gensym "x") 2) (define ,(gensym "x") 3) (define x1 4) (define x2 5)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quasiquote'),
          [
            Symbol.for('begin'),
            [Symbol.for('define'), Symbol.for('x'), 1],
            [
              Symbol.for('define'),
              [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']],
              2,
            ],
            [
              Symbol.for('define'),
              [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']],
              3,
            ],
            [Symbol.for('define'), Symbol.for('x1'), 4],
            [Symbol.for('define'), Symbol.for('x2'), 5],
          ],
        ],
      ],
      'let x = 1;\n' +
        '\n' +
        'let x3 = 2;\n' +
        '\n' +
        'let x4 = 3;\n' +
        '\n' +
        'let x1 = 4;\n' +
        '\n' +
        'let x2 = 5;',
    ]);
  });
});

describe('Fundamental operators', function (): any {
  xit("(compile '(js/= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x = y;',
    ]);
  });
  xit("(compile '(js/, x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/,'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x, y;',
    ]);
  });
  return xit("(compile '(js/; x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/;'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x;\n' + 'y;',
    ]);
  });
});

describe('Assignment operators', function (): any {
  xit("(compile '(js/+= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/+='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x += y;',
    ]);
  });
  xit("(compile '(js/-= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/-='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x -= y;',
    ]);
  });
  xit("(compile '(js/*= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/*='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x *= y;',
    ]);
  });
  xit("(compile '(js//= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js//='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x /= y;',
    ]);
  });
  xit("(compile '(js/^= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/^='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x ^= y;',
    ]);
  });
  xit("(compile '(js/&= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/&='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x &= y;',
    ]);
  });
  xit("(compile '(js/|= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/|='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x |= y;',
    ]);
  });
  xit("(compile '(js/<<= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/<<='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x <<= y;',
    ]);
  });
  xit("(compile '(js/>>= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/>>='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x >>= y;',
    ]);
  });
  return xit("(compile '(js/>>>= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/>>>='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x >>>= y;',
    ]);
  });
});
