/**
 * # Unsorted tests
 *
 * Tests that have not been sorted yet.
 */

import { assertEqual, testRepl, testMacro } from './test-util';

testMacro.ftype = 'macro';

/**
 * Test inbox
 */
describe('js/for', function (): any {
  return xit("(compile '(js/for ((i 0) (< i 10) (+ i 1)) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/for'),
            [
              [Symbol.for('i'), 0],
              [Symbol.for('<'), Symbol.for('i'), 10],
              [Symbol.for('+'), Symbol.for('i'), 1],
            ],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (let i = 0; i < 10; i++) {\n' + '  foo();\n' + '}',
    ]);
  });
});

describe('To do', function (): any {
  xit("(compile '(js/? x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/?'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      'x ? y : z;',
    ]);
  });
  xit("(compile '(js/if x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/if'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      'if (x) {\n' + '  y;\n' + '} else {\n' + '  z;\n' + '}',
    ]);
  });
  xit("(compile '(js/< x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/<'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x < y;',
    ]);
  });
  xit("(compile '(js/> x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/>'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x > y;',
    ]);
  });
  xit("(compile '(js/% x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/%'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x % y;',
    ]);
  });
  xit("(compile '(js/and x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/and'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x and y;',
    ]);
  });
  xit("(compile '(js/or x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/or'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x or y;',
    ]);
  });
  return xit("(compile '(js/! x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/!'), Symbol.for('x')]],
      ],
      '!x;',
    ]);
  });
});

describe('Bitwise operators', function (): any {
  xit("(compile '(bitwise-and x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bitwise-and'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x & y;',
    ]);
  });
  xit("(compile '(bit-and x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bit-and'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x & y;',
    ]);
  });
  xit("(compile '(js/& x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/&'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x & y;',
    ]);
  });
  xit("(compile '(bitwise-or x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bitwise-or'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x | y;',
    ]);
  });
  xit("(compile '(bit-or x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bit-or'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x | y;',
    ]);
  });
  xit("(compile '(js/| x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/|'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x | y;',
    ]);
  });
  xit("(compile '(bitwise-xor x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bitwise-xor'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x ^ y;',
    ]);
  });
  xit("(compile '(bit-xor x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bit-xor'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x ^ y;',
    ]);
  });
  xit("(compile '(js/^ x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/^'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x ^ y;',
    ]);
  });
  xit("(compile '(bitwise-negation x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bitwise-negation'), Symbol.for('x')],
        ],
      ],
      '~x;',
    ]);
  });
  xit("(compile '(bitwise-not x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('bitwise-not'), Symbol.for('x')]],
      ],
      '~x;',
    ]);
  });
  xit("(compile '(bit-not x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('bit-not'), Symbol.for('x')]],
      ],
      '~x;',
    ]);
  });
  xit("(compile '(js/~ x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/~'), Symbol.for('x')]],
      ],
      '~x;',
    ]);
  });
  xit("(compile '(bitwise-shift-left x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bitwise-shift-left'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x << y;',
    ]);
  });
  xit("(compile '(bit-shift-left x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bit-shift-left'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x << y;',
    ]);
  });
  xit("(compile '(js/<< x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/<<'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x << y;',
    ]);
  });
  xit("(compile '(bitwise-shift-right x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bitwise-shift-right'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x >> y;',
    ]);
  });
  xit("(compile '(bit-shift-right x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('bit-shift-right'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x >> y;',
    ]);
  });
  xit("(compile '(js/>> x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/>>'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x >> y;',
    ]);
  });
  xit("(compile '(unsigned-bit-shift-right x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('unsigned-bit-shift-right'),
            Symbol.for('x'),
            Symbol.for('y'),
          ],
        ],
      ],
      'x >> y;',
    ]);
  });
  xit("(compile '(js/>>> x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/>>>'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x >>> y;',
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
  xit("(compile '(js/>>>= x y))", function (): any {
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
  return xit("(compile '(js/^= x y))", function (): any {
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
});
