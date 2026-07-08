/**
 * # Test specification
 *
 * Tests expressed as a Roselisp REPL session.
 */

import { testRepl, testMacro } from './test-util';

describe('license', function (): any {
  return it('license', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      Symbol.for('license'),
      [Symbol.for('quote'), Symbol.for('MPL-2.0')],
    ]);
  });
});

describe('#t', function (): any {
  it('#t', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, true, true]);
  });
  return it('(quote #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quote'), true],
      true,
    ]);
  });
});

describe('#f', function (): any {
  it('#f', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, false, false]);
  });
  return it('(quote #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quote'), false],
      false,
    ]);
  });
});

describe('#u', function (): any {
  it('#u', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, undefined, undefined]);
  });
  it('(quote #u)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quote'), undefined],
      undefined,
    ]);
  });
  return it('undefined', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      Symbol.for('undefined'),
      undefined,
    ]);
  });
});

describe('#n', function (): any {
  it('#n', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, null, null]);
  });
  it('(quote #n)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quote'), null],
      null,
    ]);
  });
  it('js-null', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      Symbol.for('js-null'),
      null,
    ]);
  });
  return it('js/null', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      Symbol.for('js/null'),
      null,
    ]);
  });
});

describe('true?', function (): any {
  it('(true? #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('true?'), true],
      true,
    ]);
  });
  it('(true? #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('true?'), false],
      false,
    ]);
  });
  it('(true? #u)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('true?'), undefined],
      false,
    ]);
  });
  it('(true? #n)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('true?'), null],
      false,
    ]);
  });
  return it('(true? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('true?'), [Symbol.for('quote'), []]],
      true,
    ]);
  });
});

describe('false?', function (): any {
  it('(false? #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('false?'), true],
      false,
    ]);
  });
  it('(false? #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('false?'), false],
      true,
    ]);
  });
  it('(false? #u)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('false?'), undefined],
      true,
    ]);
  });
  it('(false? #n)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('false?'), null],
      true,
    ]);
  });
  return it('(false? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('false?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
});

describe('nil', function (): any {
  it('nil', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      Symbol.for('nil'),
      [Symbol.for('quote'), []],
    ]);
  });
  it('(list? nil)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list?'), Symbol.for('nil')],
      true,
    ]);
  });
  return it('(length nil)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('length'), Symbol.for('nil')],
      0,
    ]);
  });
});

describe('null', function (): any {
  it('null', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      Symbol.for('null'),
      [Symbol.for('quote'), []],
    ]);
  });
  it('(listp null)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('listp'), Symbol.for('null')],
      true,
    ]);
  });
  return it('(length null)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('length'), Symbol.for('null')],
      0,
    ]);
  });
});

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
