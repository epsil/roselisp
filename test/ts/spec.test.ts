/**
 * # Test specification
 *
 * Tests expressed as a Roselisp REPL session.
 */

import { testRepl, testMacro } from './test-util';

describe('eq?', function (): any {
  it('(eq? #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('eq?'), true, true],
      true,
    ]);
  });
  it('(eq? #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('eq?'), false, false],
      true,
    ]);
  });
  return it('(eq? #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('eq?'), true, false],
      false,
    ]);
  });
});

describe('equal?', function (): any {
  it('(equal? #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('equal?'), true, true],
      true,
    ]);
  });
  it('(equal? #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('equal?'), false, false],
      true,
    ]);
  });
  return it('(equal? #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('equal?'), true, false],
      false,
    ]);
  });
});
