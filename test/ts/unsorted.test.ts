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
