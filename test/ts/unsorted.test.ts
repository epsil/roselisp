/**
 * # Various unsorted tests
 *
 * This file functions as an "inbox" for incoming tests.
 */

import { assertEqual, testRepl, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('To do', function (): any {});

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
  xit("(compile '(js/; x y))", function (): any {
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
  xit("(compile '(js/() x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/()'), Symbol.for('x')]],
      ],
      '(x)',
    ]);
  });
  xit("(compile '(js/[] x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/[]'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x[y]',
    ]);
  });
  return xit("(compile '(js/{} x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/{}'), Symbol.for('x')]],
      ],
      '{\n' + '  x;\n' + '}',
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

describe('define-syntax', function (): any {
  return xit("(compile '(module m scheme (define x 1) (define-syntax (foo x) (syntax (begin (define x 2) x))) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('scheme'),
            [Symbol.for('define'), Symbol.for('x'), 1],
            [
              Symbol.for('define-syntax'),
              [Symbol.for('foo'), Symbol.for('x')],
              [
                Symbol.for('syntax'),
                [
                  Symbol.for('begin'),
                  [Symbol.for('define'), Symbol.for('x'), 2],
                  Symbol.for('x'),
                ],
              ],
            ],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'import {\n' +
        '  datumToSyntax\n' +
        "} from 'roselisp';\n" +
        '\n' +
        'let x = 1;\n' +
        '\n' +
        'function foo(x) {\n' +
        "  return datumToSyntax(false, [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 2], Symbol.for('x')]);\n" +
        '}\n' +
        '\n' +
        "foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];\n" +
        '\n' +
        'let x1 = 2;\n' +
        '\n' +
        'x1;',
    ]);
  });
});
