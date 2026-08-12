/**
 * # Various unsorted tests
 *
 * This file functions as an "inbox" for incoming tests.
 */

import { assertEqual, testRepl, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('To do', function (): any {});

describe('js/=', function (): any {
  it("(compile '(js/= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(js/= (aget x i) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('aget'), Symbol.for('x'), Symbol.for('i')],
            Symbol.for('y'),
          ],
        ],
      ],
      'x[i] = y;',
    ]);
  });
  it("(compile '(js/= (list x y) z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
            Symbol.for('z'),
          ],
        ],
      ],
      '[x, y] = z;',
    ]);
  });
  it("(compile '(js/= (list #f y) z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('list'), false, Symbol.for('y')],
            Symbol.for('z'),
          ],
        ],
      ],
      '[, y] = z;',
    ]);
  });
  it("(compile '(js/= (list* x) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('list*'), Symbol.for('x')],
            Symbol.for('y'),
          ],
        ],
      ],
      'x = y;',
    ]);
  });
  it("(compile '(js/= (list* x y) z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('list*'), Symbol.for('x'), Symbol.for('y')],
            Symbol.for('z'),
          ],
        ],
      ],
      '[x, ...y] = z;',
    ]);
  });
  it("(compile '(js/= (list* #f x y) z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('list*'), false, Symbol.for('x'), Symbol.for('y')],
            Symbol.for('z'),
          ],
        ],
      ],
      '[, x, ...y] = z;',
    ]);
  });
  it("(compile '(js/= (values x y) z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('values'), Symbol.for('x'), Symbol.for('y')],
            Symbol.for('z'),
          ],
        ],
      ],
      '[x, y] = z;',
    ]);
  });
  it("(compile '(js/= (js/obj x x) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('js/obj'), Symbol.for('x'), Symbol.for('x')],
            Symbol.for('y'),
          ],
        ],
      ],
      '({x} = y);',
    ]);
  });
  it("(compile '(js/= (js/obj x y) z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('js/obj'), Symbol.for('x'), Symbol.for('y')],
            Symbol.for('z'),
          ],
        ],
      ],
      '({x: y} = z);',
    ]);
  });
  it("(compile '(js/= (set! x) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('set!'), Symbol.for('x')],
            Symbol.for('y'),
          ],
        ],
      ],
      'x = y;',
    ]);
  });
  it("(compile '(js/= (aset! x i) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('aset!'), Symbol.for('x'), Symbol.for('i')],
            Symbol.for('y'),
          ],
        ],
      ],
      'x[i] = y;',
    ]);
  });
  it('(compile \'(js/= (oset! x "y") z))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('oset!'), Symbol.for('x'), 'y'],
            Symbol.for('z'),
          ],
        ],
      ],
      "x['y'] = z;",
    ]);
  });
  it("(compile '(js/= (set!-values (x)) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('set!-values'), [Symbol.for('x')]],
            Symbol.for('y'),
          ],
        ],
      ],
      '[x] = y;',
    ]);
  });
  it("(compile '(js/= (set!-fields (x)) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('set!-fields'), [Symbol.for('x')]],
            Symbol.for('y'),
          ],
        ],
      ],
      '({x} = y);',
    ]);
  });
  it("(compile '(js/= (define x) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('define'), Symbol.for('x')],
            Symbol.for('y'),
          ],
        ],
      ],
      'let x = y;',
    ]);
  });
  it("(compile '(js/= (define-values (x)) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('define-values'), [Symbol.for('x')]],
            Symbol.for('y'),
          ],
        ],
      ],
      'let [x] = y;',
    ]);
  });
  it("(compile '(js/= (define-values (_ x)) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('x')]],
            Symbol.for('y'),
          ],
        ],
      ],
      'let [, x] = y;',
    ]);
  });
  it("(compile '(js/= (define-values (_ __ x) :hole-marker __) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [
              Symbol.for('define-values'),
              [Symbol.for('_'), Symbol.for('__'), Symbol.for('x')],
              Symbol.for(':hole-marker'),
              Symbol.for('__'),
            ],
            Symbol.for('y'),
          ],
        ],
      ],
      'let [_, , x] = y;',
    ]);
  });
  return it("(compile '(js/= (define-fields (x)) y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/='),
            [Symbol.for('define-fields'), [Symbol.for('x')]],
            Symbol.for('y'),
          ],
        ],
      ],
      'let {x} = y;',
    ]);
  });
});

describe('Fundamental operators', function (): any {
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
      'x()',
    ]);
  });
  xit("(compile '(js/() x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/()'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x(y)',
    ]);
  });
  xit("(compile '(js/() x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/()'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      'x(y, z)',
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
