/**
 * # Unsorted tests
 *
 * Tests that have not been sorted yet.
 */

import * as chai from 'chai';

import {
  assertEqual,
  compileReplForm,
  simplifyReplForm,
  testRepl,
} from './test-util';

describe('test-repl', function (): any {
  describe('roselisp REPL', function (): any {
    it('(> ...)', function (): any {
      return testRepl([Symbol.for('>'), [Symbol.for('+'), 1, 1], 2]);
    });
    it('(_ > ...)', function (): any {
      return testRepl([
        Symbol.for('_'),
        Symbol.for('>'),
        [Symbol.for('+'), 1, 1],
        2,
      ]);
    });
    it('(repl > ...)', function (): any {
      return testRepl([
        Symbol.for('repl'),
        Symbol.for('>'),
        [Symbol.for('+'), 1, 1],
        2,
      ]);
    });
    it('(shell > ...)', function (): any {
      return testRepl([
        Symbol.for('shell'),
        Symbol.for('>'),
        [Symbol.for('+'), 1, 1],
        2,
      ]);
    });
    it('(roselisp > ...)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('+'), 1, 1],
        2,
      ]);
    });
    it('($ roselisp > ...)', function (): any {
      return testRepl([
        Symbol.for('$'),
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('+'), 1, 1],
        2,
      ]);
    });
    it('(+ 2 2)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('+'), 2, 2],
        4,
      ]);
    });
    return it('(list 1 2 3 4)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('list'), 1, 2, 3, 4],
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ]);
    });
  });
  return describe('node REPL', function (): any {
    it('1 + 1', function (): any {
      return testRepl([Symbol.for('node'), Symbol.for('>'), '1 + 1', '2']);
    });
    return xit('const n = 1', function (): any {
      return testRepl([
        Symbol.for('node'),
        Symbol.for('>'),
        'const n = 1',
        'undefined',
        Symbol.for('>'),
        'n + 1',
        '2',
      ]);
    });
  });
});

describe('compile-repl-form', function (): any {
  it('(roselisp > (+ 1 1) 2)', function (): any {
    return assertEqual(
      compileReplForm(
        [Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 1, 1], 2],
        {
          from: 'roselisp',
          to: 'node',
        }
      ),
      [Symbol.for('node'), Symbol.for('>'), '1 + 1', '2']
    );
  });
  it('(roselisp > (+ 1 1) _)', function (): any {
    return assertEqual(
      compileReplForm(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('+'), 1, 1],
          Symbol.for('_'),
        ],
        {
          from: 'roselisp',
          to: 'node',
        }
      ),
      [Symbol.for('node'), Symbol.for('>'), '1 + 1', '_']
    );
  });
  return xit('(roselisp > (+ 1 1) 2), plist', function (): any {
    return assertEqual(
      compileReplForm(
        [Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 1, 1], 2],
        Symbol.for(':from'),
        'roselisp',
        Symbol.for(':to'),
        'node'
      ),
      [Symbol.for('node'), Symbol.for('>'), '1 + 1', '2']
    );
  });
});

describe('simplify-repl-form', function (): any {
  return it('(roselisp > (+ 1 1) 2)', function (): any {
    return assertEqual(
      simplifyReplForm([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('+'), 1, 1],
        2,
        Symbol.for('>'),
        [Symbol.for('+'), 2, 2],
        4,
      ]),
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('begin'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 2, 2]],
        4,
      ]
    );
  });
});
