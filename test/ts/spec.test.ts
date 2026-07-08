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

describe('+', function (): any {
  it('(+)', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, [Symbol.for('+')], 0]);
  });
  it('(+ 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('+'), 1],
      1,
    ]);
  });
  it('(+ 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('+'), 1, 2],
      3,
    ]);
  });
  return it('(+ 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('+'), 1, 2, 4],
      7,
    ]);
  });
});

describe('-', function (): any {
  it('(-)', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, [Symbol.for('-')], 0]);
  });
  it('(- 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('-'), 1],
      -1,
    ]);
  });
  it('(- 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('-'), 1, 2],
      -1,
    ]);
  });
  return it('(- 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('-'), 1, 2, 4],
      -5,
    ]);
  });
});

describe('*', function (): any {
  it('(*)', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, [Symbol.for('*')], 1]);
  });
  it('(* 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('*'), 1],
      1,
    ]);
  });
  it('(* 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('*'), 1, 2],
      2,
    ]);
  });
  return it('(* 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('*'), 1, 2, 4],
      8,
    ]);
  });
});

describe('/', function (): any {
  it('(/)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('/')],
      undefined,
    ]);
  });
  it('(/ 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('/'), 1],
      1,
    ]);
  });
  it('(/ 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('/'), 1, 2],
      0.5,
    ]);
  });
  return it('(/ 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('/'), 1, 2, 4],
      0.125,
    ]);
  });
});
