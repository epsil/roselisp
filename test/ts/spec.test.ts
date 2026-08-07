/**
 * # Test specification
 *
 * Tests expressed as a Roselisp REPL session.
 */

import { testRepl, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('#t', function (): any {
  it('#t', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), true, true]);
  });
  it("'#t", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quote'), true],
      true,
    ]);
  });
  return it('(compile #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), true],
      'true;',
    ]);
  });
});

describe('#f', function (): any {
  it('#f', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), false, false]);
  });
  it("'#f", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quote'), false],
      false,
    ]);
  });
  return it('(compile #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), false],
      'false;',
    ]);
  });
});

describe('#u', function (): any {
  it('#u', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      undefined,
      undefined,
    ]);
  });
  it("'#u", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quote'), undefined],
      undefined,
    ]);
  });
  it('undefined', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      Symbol.for('undefined'),
      undefined,
    ]);
  });
  return it('(compile #u)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), undefined],
      'undefined;',
    ]);
  });
});

describe('#n', function (): any {
  it('#n', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), null, null]);
  });
  it("'#n", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quote'), null],
      null,
    ]);
  });
  it('js-null', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      Symbol.for('js-null'),
      null,
    ]);
  });
  it('js/null', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      Symbol.for('js/null'),
      null,
    ]);
  });
  return it('(compile #n)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), null],
      'null;',
    ]);
  });
});

describe('true?', function (): any {
  it('(true? #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('true?'), true],
      true,
    ]);
  });
  it('(true? #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('true?'), false],
      false,
    ]);
  });
  it('(true? #u)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('true?'), undefined],
      false,
    ]);
  });
  it('(true? #n)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('true?'), null],
      false,
    ]);
  });
  return it("(true? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('true?'), [Symbol.for('quote'), []]],
      true,
    ]);
  });
});

describe('false?', function (): any {
  it('(false? #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('false?'), true],
      false,
    ]);
  });
  it('(false? #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('false?'), false],
      true,
    ]);
  });
  it('(false? #u)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('false?'), undefined],
      true,
    ]);
  });
  it('(false? #n)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('false?'), null],
      true,
    ]);
  });
  return it("(false? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('false?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
});

describe('nil', function (): any {
  it('nil', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      Symbol.for('nil'),
      [Symbol.for('quote'), []],
    ]);
  });
  it('(list? nil)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list?'), Symbol.for('nil')],
      true,
    ]);
  });
  return it('(length nil)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('length'), Symbol.for('nil')],
      0,
    ]);
  });
});

describe('null', function (): any {
  it('null', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      Symbol.for('null'),
      [Symbol.for('quote'), []],
    ]);
  });
  it('(listp null)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('listp'), Symbol.for('null')],
      true,
    ]);
  });
  return it('(length null)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('length'), Symbol.for('null')],
      0,
    ]);
  });
});

describe('Numbers', function (): any {
  it('0', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), 0, 0]);
  });
  it('1', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), 1, 1]);
  });
  it('2', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), 2, 2]);
  });
  it('(compile 0)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), 0],
      '0;',
    ]);
  });
  it('(compile 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), 1],
      '1;',
    ]);
  });
  return it('(compile 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), 2],
      '2;',
    ]);
  });
});

describe('Strings', function (): any {
  it('""', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), '', '']);
  });
  it('"foo"', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), 'foo', 'foo']);
  });
  it('"\\"foo\\""', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      '"foo"',
      '"foo"',
    ]);
  });
  it('(eq? "	" "	")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('eq?'), '	', '	'],
      true,
    ]);
  });
  it('(compile "")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), ''],
      "'';",
    ]);
  });
  return it('(compile "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), 'foo'],
      "'foo';",
    ]);
  });
});

describe('Symbols', function (): any {
  it("'foo", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quote'), Symbol.for('foo')],
      [Symbol.for('quote'), Symbol.for('foo')],
    ]);
  });
  it("(compile 'foo)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo')]],
      'foo;',
    ]);
  });
  it("(compile 'foo-bar)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo-bar')]],
      'fooBar;',
    ]);
  });
  it("(compile ''foo)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quote'), Symbol.for('foo')]],
      ],
      "Symbol.for('foo');",
    ]);
  });
  return it("(compile ''foo-bar)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quote'), Symbol.for('foo-bar')]],
      ],
      "Symbol.for('foo-bar');",
    ]);
  });
});

describe('Keywords', function (): any {
  it(':foo', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      Symbol.for(':foo'),
      [Symbol.for('quote'), Symbol.for(':foo')],
    ]);
  });
  it("':foo", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quote'), Symbol.for(':foo')],
      [Symbol.for('quote'), Symbol.for(':foo')],
    ]);
  });
  it("(keyword? ':foo)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('keyword?'), [Symbol.for('quote'), Symbol.for(':foo')]],
      true,
    ]);
  });
  it("(keyword? 'foo)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('keyword?'), [Symbol.for('quote'), Symbol.for('foo')]],
      false,
    ]);
  });
  return it("(compile ':foo)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for(':foo')]],
      "Symbol.for(':foo');",
    ]);
  });
});

describe('symbol?', function (): any {
  it("(symbol? 'foo)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('symbol?'), [Symbol.for('quote'), Symbol.for('foo')]],
      true,
    ]);
  });
  it('(symbol? 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('symbol?'), 1],
      false,
    ]);
  });
  it('(symbol? "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('symbol?'), 'foo'],
      false,
    ]);
  });
  it('(symbol? (js/obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('symbol?'), [Symbol.for('js/obj')]],
      false,
    ]);
  });
  return it("(symbol? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('symbol?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
});

describe('symbol->string', function (): any {
  return it("(symbol->string 'foo)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('symbol->string'), [Symbol.for('quote'), Symbol.for('foo')]],
      'foo',
    ]);
  });
});

describe('intern', function (): any {
  return it('(intern "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('intern'), 'foo'],
      [Symbol.for('quote'), Symbol.for('foo')],
    ]);
  });
});

describe('gensym', function (): any {
  it('(symbol? (gensym "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('symbol?'), [Symbol.for('gensym'), 'foo']],
      true,
    ]);
  });
  return it('(compile \'(gensym "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('gensym'), 'foo']],
      ],
      "Symbol('foo');",
    ]);
  });
});

describe('Cons cells', function (): any {
  it('(cons 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cons'), 1, 2],
      [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
    ]);
  });
  it('(cons 1 (cons 2 3))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cons'), 1, [Symbol.for('cons'), 2, 3]],
      [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
    ]);
  });
  it("(cons 1 '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cons'), 1, [Symbol.for('quote'), []]],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it("(cons 1 '(2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cons'), 1, [Symbol.for('quote'), [2]]],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  it("(car '(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('car'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      1,
    ]);
  });
  it("(cdr '(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      2,
    ]);
  });
  it('(car (cons 1 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('car'), [Symbol.for('cons'), 1, 2]],
      1,
    ]);
  });
  it('(cdr (cons 1 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cdr'), [Symbol.for('cons'), 1, 2]],
      2,
    ]);
  });
  it("(compile ''(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
      ],
      "[1, Symbol.for('.'), []];",
    ]);
  });
  it("(compile ''(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      ],
      "[1, Symbol.for('.'), 2];",
    ]);
  });
  return it("(compile ''(1 2 . 3))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
        ],
      ],
      "[1, 2, Symbol.for('.'), 3];",
    ]);
  });
});

describe('Lists', function (): any {
  it('(list 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list'), 1, 2],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  it("(aget '(1 2) 0)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('aget'), [Symbol.for('quote'), [1, 2]], 0],
      1,
    ]);
  });
  it("(aget '((1 2) (3 4)) 0 1)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('aget'),
        [
          Symbol.for('quote'),
          [
            [1, 2],
            [3, 4],
          ],
        ],
        0,
        1,
      ],
      2,
    ]);
  });
  it("(aref '(1 2) 0)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('aref'), [Symbol.for('quote'), [1, 2]], 0],
      1,
    ]);
  });
  it("(aset! '(1 2) 0 3)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('aset!'), [Symbol.for('quote'), [1, 2]], 0, 3],
      3,
    ]);
  });
  it("(let ((lst '(1 2))) (aset! lst 0 3) lst)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2]]]],
        [Symbol.for('aset!'), Symbol.for('lst'), 0, 3],
        Symbol.for('lst'),
      ],
      [Symbol.for('quote'), [3, 2]],
    ]);
  });
  it("(let ((lst '(1 2)) (i 0)) (aget lst i))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [Symbol.for('lst'), [Symbol.for('quote'), [1, 2]]],
          [Symbol.for('i'), 0],
        ],
        [Symbol.for('aget'), Symbol.for('lst'), Symbol.for('i')],
      ],
      1,
    ]);
  });
  it("(let ((lst '(1 2)) (i 0)) (aget lst (+ i 1)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [Symbol.for('lst'), [Symbol.for('quote'), [1, 2]]],
          [Symbol.for('i'), 0],
        ],
        [
          Symbol.for('aget'),
          Symbol.for('lst'),
          [Symbol.for('+'), Symbol.for('i'), 1],
        ],
      ],
      2,
    ]);
  });
  it("(let ((lst '(1 2))) (set! (aref lst 0) 3) lst)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2]]]],
        [Symbol.for('set!'), [Symbol.for('aref'), Symbol.for('lst'), 0], 3],
        Symbol.for('lst'),
      ],
      [Symbol.for('quote'), [3, 2]],
    ]);
  });
  it("(compile ''())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), []]]],
      '[];',
    ]);
  });
  it("(compile '(list))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list')]]],
      '[];',
    ]);
  });
  it("(compile ''(1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quote'), [1]]],
      ],
      '[1];',
    ]);
  });
  it("(compile '(list 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list'), 1]]],
      '[1];',
    ]);
  });
  it("(compile ''(1 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quote'), [1, 2]]],
      ],
      '[1, 2];',
    ]);
  });
  it("(compile '(list 1 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('list'), 1, 2]],
      ],
      '[1, 2];',
    ]);
  });
  it("(compile '(aget x 0))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('aget'), Symbol.for('x'), 0]],
      ],
      'x[0];',
    ]);
  });
  it("(compile '(let ((length 0)) (aget x length)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('let'),
            [[Symbol.for('length'), 0]],
            [Symbol.for('aget'), Symbol.for('x'), Symbol.for('length')],
          ],
        ],
      ],
      'let length = 0;\n' + '\n' + 'x[length];',
    ]);
  });
  it("(compile '(aget x 'length))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('aget'),
            Symbol.for('x'),
            [Symbol.for('quote'), Symbol.for('length')],
          ],
        ],
      ],
      "x['length'];",
    ]);
  });
  it("(compile '(aget x :length))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('aget'), Symbol.for('x'), Symbol.for(':length')],
        ],
      ],
      "x['length'];",
    ]);
  });
  return it("(compile '(aget (js/?. x) 0))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('aget'), [Symbol.for('js/?.'), Symbol.for('x')], 0],
        ],
      ],
      'x?.[0];',
    ]);
  });
});

describe('quote', function (): any {
  it("'foo", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quote'), Symbol.for('foo')],
      [Symbol.for('quote'), Symbol.for('foo')],
    ]);
  });
  it("'(1)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quote'), [1]],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it("'(1 2)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quote'), [1, 2]],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  it("'((1 2) (3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('quote'),
        [
          [1, 2],
          [3, 4],
        ],
      ],
      [
        Symbol.for('quote'),
        [
          [1, 2],
          [3, 4],
        ],
      ],
    ]);
  });
  it("(compile ''foo)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quote'), Symbol.for('foo')]],
      ],
      "Symbol.for('foo');",
    ]);
  });
  it("(compile ''(1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quote'), [1]]],
      ],
      '[1];',
    ]);
  });
  it("(compile ''(1 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quote'), [1, 2]]],
      ],
      '[1, 2];',
    ]);
  });
  return it("(compile ''((1 2) (3 4)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('quote'),
            [
              [1, 2],
              [3, 4],
            ],
          ],
        ],
      ],
      '[[1, 2], [3, 4]];',
    ]);
  });
});

describe('quasiquote', function (): any {
  it('`foo', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quasiquote'), Symbol.for('foo')],
      [Symbol.for('quote'), Symbol.for('foo')],
    ]);
  });
  it('`foo', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quasiquote'), Symbol.for('foo')],
      [Symbol.for('quote'), Symbol.for('foo')],
    ]);
  });
  it('`(,1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quasiquote'), [[Symbol.for('unquote'), 1]]],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it('`((,1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), 1]]]],
      [Symbol.for('quote'), [[1]]],
    ]);
  });
  it('`(,@(list 1 2 3))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('quasiquote'),
        [[Symbol.for('unquote-splicing'), [Symbol.for('list'), 1, 2, 3]]],
      ],
      [Symbol.for('quote'), [1, 2, 3]],
    ]);
  });
  it("(compile '`foo)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('quasiquote'), Symbol.for('foo')]],
      ],
      "Symbol.for('foo');",
    ]);
  });
  it("(compile '`(,1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('quasiquote'), [[Symbol.for('unquote'), 1]]],
        ],
      ],
      '[1];',
    ]);
  });
  it("(compile '`((,1)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), 1]]]],
        ],
      ],
      '[[1]];',
    ]);
  });
  return it("(compile '`(,@(list 1 2 3)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('quasiquote'),
            [[Symbol.for('unquote-splicing'), [Symbol.for('list'), 1, 2, 3]]],
          ],
        ],
      ],
      '[...[1, 2, 3]];',
    ]);
  });
});

describe('Variables', function (): any {
  it('(let ((x 2)) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('let'), [[Symbol.for('x'), 2]], Symbol.for('x')],
      2,
    ]);
  });
  it('(let ((x 2) y) y)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('x'), 2], Symbol.for('y')],
        Symbol.for('y'),
      ],
      undefined,
    ]);
  });
  it('(let (x) (set! x 2) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('x')],
        [Symbol.for('set!'), Symbol.for('x'), 2],
        Symbol.for('x'),
      ],
      2,
    ]);
  });
  it('((lambda () (define x 2) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [Symbol.for('define'), Symbol.for('x'), 2],
          Symbol.for('x'),
        ],
      ],
      2,
    ]);
  });
  it('((lambda () (define x) (set! x 2) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [Symbol.for('define'), Symbol.for('x')],
          [Symbol.for('set!'), Symbol.for('x'), 2],
          Symbol.for('x'),
        ],
      ],
      2,
    ]);
  });
  return it('(let (x y) (set! x 2) (set! y 3) (+ x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('x'), Symbol.for('y')],
        [Symbol.for('set!'), Symbol.for('x'), 2],
        [Symbol.for('set!'), Symbol.for('y'), 3],
        [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
      ],
      5,
    ]);
  });
});

describe('Function calls', function (): any {
  it('(let ((identity (lambda (x) x))) (identity "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('identity'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        [Symbol.for('identity'), 'foo'],
      ],
      'foo',
    ]);
  });
  it('(let ((my-add (lambda (x y) (+ x y)))) (my-add 1 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('my-add'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('y')],
              [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
            ],
          ],
        ],
        [Symbol.for('my-add'), 1, 2],
      ],
      3,
    ]);
  });
  return it('(let ((my-add (lambda (x y z) (+ x y z)))) (my-add 1 2 3))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('my-add'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
              [
                Symbol.for('+'),
                Symbol.for('x'),
                Symbol.for('y'),
                Symbol.for('z'),
              ],
            ],
          ],
        ],
        [Symbol.for('my-add'), 1, 2, 3],
      ],
      6,
    ]);
  });
});

describe('define', function (): any {
  it('((lambda () (define x 1) 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [Symbol.for('define'), Symbol.for('x'), 1],
          1,
        ],
      ],
      1,
    ]);
  });
  it('((lambda () (define (foo . args) args) (foo)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            [Symbol.for('foo'), Symbol.for('.'), Symbol.for('args')],
            Symbol.for('args'),
          ],
          [Symbol.for('foo')],
        ],
      ],
      [Symbol.for('quote'), []],
    ]);
  });
  it('((lambda () (define (my-add x y) (+ x y)) (my-add 2 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            [Symbol.for('my-add'), Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
          ],
          [Symbol.for('my-add'), 2, 3],
        ],
      ],
      5,
    ]);
  });
  it('(let ((my-add (lambda (x y) (+ x y)))) ((lambda () (define (my-add-2 x y) (my-add x y)) (my-add-2 2 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('my-add'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('y')],
              [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
            ],
          ],
        ],
        [
          [
            Symbol.for('lambda'),
            [],
            [
              Symbol.for('define'),
              [Symbol.for('my-add-2'), Symbol.for('x'), Symbol.for('y')],
              [Symbol.for('my-add'), Symbol.for('x'), Symbol.for('y')],
            ],
            [Symbol.for('my-add-2'), 2, 3],
          ],
        ],
      ],
      5,
    ]);
  });
  it('(let ((my-add (lambda (x y z) (+ x y z)))) ((lambda () (define (my-add-2 x y z) (my-add x y z)) (my-add-2 1 2 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('my-add'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
              [
                Symbol.for('+'),
                Symbol.for('x'),
                Symbol.for('y'),
                Symbol.for('z'),
              ],
            ],
          ],
        ],
        [
          [
            Symbol.for('lambda'),
            [],
            [
              Symbol.for('define'),
              [
                Symbol.for('my-add-2'),
                Symbol.for('x'),
                Symbol.for('y'),
                Symbol.for('z'),
              ],
              [
                Symbol.for('my-add'),
                Symbol.for('x'),
                Symbol.for('y'),
                Symbol.for('z'),
              ],
            ],
            [Symbol.for('my-add-2'), 1, 2, 3],
          ],
        ],
      ],
      6,
    ]);
  });
  it("(compile '(define x 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), 1]],
      ],
      'let x = 1;',
    ]);
  });
  it("(compile '(define (foo x) x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('define'),
            [Symbol.for('foo'), Symbol.for('x')],
            Symbol.for('x'),
          ],
        ],
      ],
      'function foo(x) {\n' + '  return x;\n' + '}',
    ]);
  });
  return it("(compile '(define foo (lambda (x) x)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
      ],
      'let foo = function (x) {\n' + '  return x;\n' + '};',
    ]);
  });
});

describe('defun', function (): any {
  it('((lambda () (defun my-add (x y) (+ x y)) (my-add 2 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('defun'),
            Symbol.for('my-add'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
          ],
          [Symbol.for('my-add'), 2, 3],
        ],
      ],
      5,
    ]);
  });
  it('(let ((my-add (lambda (x y) (+ x y)))) ((lambda () (defun my-add-2 (x y) (my-add x y)) (my-add-2 2 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('my-add'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('y')],
              [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
            ],
          ],
        ],
        [
          [
            Symbol.for('lambda'),
            [],
            [
              Symbol.for('defun'),
              Symbol.for('my-add-2'),
              [Symbol.for('x'), Symbol.for('y')],
              [Symbol.for('my-add'), Symbol.for('x'), Symbol.for('y')],
            ],
            [Symbol.for('my-add-2'), 2, 3],
          ],
        ],
      ],
      5,
    ]);
  });
  return it('(let ((my-add (lambda (x y z) (+ x y z)))) ((lambda () (defun my-add-2 (x y z) (my-add x y z)) (my-add-2 1 2 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('my-add'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
              [
                Symbol.for('+'),
                Symbol.for('x'),
                Symbol.for('y'),
                Symbol.for('z'),
              ],
            ],
          ],
        ],
        [
          [
            Symbol.for('lambda'),
            [],
            [
              Symbol.for('defun'),
              Symbol.for('my-add-2'),
              [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
              [
                Symbol.for('my-add'),
                Symbol.for('x'),
                Symbol.for('y'),
                Symbol.for('z'),
              ],
            ],
            [Symbol.for('my-add-2'), 1, 2, 3],
          ],
        ],
      ],
      6,
    ]);
  });
});

describe('define-macro', function (): any {
  return it('((lambda () (define-macro (my-macro x) x) (my-macro 1)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-macro'),
            [Symbol.for('my-macro'), Symbol.for('x')],
            Symbol.for('x'),
          ],
          [Symbol.for('my-macro'), 1],
        ],
      ],
      1,
    ]);
  });
});

describe('defmacro', function (): any {
  return it('((lambda () (defmacro my-macro (x) x) (my-macro 1)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('defmacro'),
            Symbol.for('my-macro'),
            [Symbol.for('x')],
            Symbol.for('x'),
          ],
          [Symbol.for('my-macro'), 1],
        ],
      ],
      1,
    ]);
  });
});

describe('let', function (): any {
  it('(let ((x 0)) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('let'), [[Symbol.for('x'), 0]], Symbol.for('x')],
      0,
    ]);
  });
  it('(let ((x 1)) (let ((y 2)) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('x'), 1]],
        [Symbol.for('let'), [[Symbol.for('y'), 2]], Symbol.for('x')],
      ],
      1,
    ]);
  });
  it("(let ((x '((1 2) (3 4)))) x)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('x'),
            [
              Symbol.for('quote'),
              [
                [1, 2],
                [3, 4],
              ],
            ],
          ],
        ],
        Symbol.for('x'),
      ],
      [
        Symbol.for('quote'),
        [
          [1, 2],
          [3, 4],
        ],
      ],
    ]);
  });
  it('(let (x) (set! x 1) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('x')],
        [Symbol.for('set!'), Symbol.for('x'), 1],
        Symbol.for('x'),
      ],
      1,
    ]);
  });
  it('(let (x) (set! x 1) (set! x 2) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('x')],
        [Symbol.for('set!'), Symbol.for('x'), 1],
        [Symbol.for('set!'), Symbol.for('x'), 2],
        Symbol.for('x'),
      ],
      2,
    ]);
  });
  it('(let (x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('let'), [Symbol.for('x')]],
      undefined,
    ]);
  });
  it('(let ((a 1)) (+ (let ((a 2)) a) a))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('a'), 1]],
        [
          Symbol.for('+'),
          [Symbol.for('let'), [[Symbol.for('a'), 2]], Symbol.for('a')],
          Symbol.for('a'),
        ],
      ],
      3,
    ]);
  });
  return it('(let ((compose (lambda (f g) (lambda (x) (f (g x))))) (square (lambda (x) (* x x))) (add1 (lambda (x) (+ x 1)))) ((compose square add1) (add1 4)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('compose'),
            [
              Symbol.for('lambda'),
              [Symbol.for('f'), Symbol.for('g')],
              [
                Symbol.for('lambda'),
                [Symbol.for('x')],
                [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')]],
              ],
            ],
          ],
          [
            Symbol.for('square'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x')],
              [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')],
            ],
          ],
          [
            Symbol.for('add1'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x')],
              [Symbol.for('+'), Symbol.for('x'), 1],
            ],
          ],
        ],
        [
          [Symbol.for('compose'), Symbol.for('square'), Symbol.for('add1')],
          [Symbol.for('add1'), 4],
        ],
      ],
      36,
    ]);
  });
});

describe('let*', function (): any {
  return it('(let* ((x 1)) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('let*'), [[Symbol.for('x'), 1]], Symbol.for('x')],
      1,
    ]);
  });
});

describe('lambda', function (): any {
  it('((lambda (x) x) 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 1],
      1,
    ]);
  });
  it('((lambda (x) x) "Lisp")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 'Lisp'],
      'Lisp',
    ]);
  });
  it('((lambda x x) "Lisp")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [[Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')], 'Lisp'],
      [Symbol.for('quote'), ['Lisp']],
    ]);
  });
  it('((fn (x) x) 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [[Symbol.for('fn'), [Symbol.for('x')], Symbol.for('x')], 1],
      1,
    ]);
  });
  return it('((λ (x) x) 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [[Symbol.for('λ'), [Symbol.for('x')], Symbol.for('x')], 1],
      1,
    ]);
  });
});

describe('lexical scope', function (): any {
  it('((lambda () (define (K x) (lambda () x)) ((K 42))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            [Symbol.for('K'), Symbol.for('x')],
            [Symbol.for('lambda'), [], Symbol.for('x')],
          ],
          [[Symbol.for('K'), 42]],
        ],
      ],
      42,
    ]);
  });
  it('((lambda () (define incrementer #u) (let ((x 1)) (set! incrementer (lambda () (set! x (+ x 1)) x))) (incrementer)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [Symbol.for('define'), Symbol.for('incrementer'), undefined],
          [
            Symbol.for('let'),
            [[Symbol.for('x'), 1]],
            [
              Symbol.for('set!'),
              Symbol.for('incrementer'),
              [
                Symbol.for('lambda'),
                [],
                [
                  Symbol.for('set!'),
                  Symbol.for('x'),
                  [Symbol.for('+'), Symbol.for('x'), 1],
                ],
                Symbol.for('x'),
              ],
            ],
          ],
          [Symbol.for('incrementer')],
        ],
      ],
      2,
    ]);
  });
  return it('(let ((x 100) incrementer) (let ((x 1)) (set! incrementer (lambda () (set! x (+ x 1)) x))) (incrementer) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('x'), 100], Symbol.for('incrementer')],
        [
          Symbol.for('let'),
          [[Symbol.for('x'), 1]],
          [
            Symbol.for('set!'),
            Symbol.for('incrementer'),
            [
              Symbol.for('lambda'),
              [],
              [
                Symbol.for('set!'),
                Symbol.for('x'),
                [Symbol.for('+'), Symbol.for('x'), 1],
              ],
              Symbol.for('x'),
            ],
          ],
        ],
        [Symbol.for('incrementer')],
        Symbol.for('x'),
      ],
      100,
    ]);
  });
});

describe('begin', function (): any {
  return it('(begin)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('begin')],
      undefined,
    ]);
  });
});

describe('begin0', function (): any {
  return it('(begin0 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('begin0'), 1, 2],
      1,
    ]);
  });
});

describe('if', function (): any {
  it('(if #t 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('if'), true, 1, 2],
      1,
    ]);
  });
  it('(if #f 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('if'), false, 1, 2],
      2,
    ]);
  });
  it('(if (< 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('if'), [Symbol.for('<'), 1, 2], 1, 2],
      1,
    ]);
  });
  it('(if (> 2 1) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('if'), [Symbol.for('>'), 2, 1], 1, 2],
      1,
    ]);
  });
  it("(compile '(if #t (foo) (bar)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('if'), true, [Symbol.for('foo')], [Symbol.for('bar')]],
        ],
      ],
      'if (true) {\n' + '  foo();\n' + '} else {\n' + '  bar();\n' + '}',
    ]);
  });
  it("(compile '(if #t (foo) (bar)) :as 'statement)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('if'), true, [Symbol.for('foo')], [Symbol.for('bar')]],
        ],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('statement')],
      ],
      'if (true) {\n' + '  foo();\n' + '} else {\n' + '  bar();\n' + '}',
    ]);
  });
  it("(compile '(if #t (foo) (bar)) :as 'return)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('if'), true, [Symbol.for('foo')], [Symbol.for('bar')]],
        ],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('return')],
      ],
      'if (true) {\n' +
        '  return foo();\n' +
        '} else {\n' +
        '  return bar();\n' +
        '}',
    ]);
  });
  it("(compile '(if #t (foo) (bar)) :as 'expression)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('if'), true, [Symbol.for('foo')], [Symbol.for('bar')]],
        ],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('expression')],
      ],
      'true ? foo() : bar()',
    ]);
  });
  return it("(compile '(if #t (foo) (bar) (baz)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('if'),
            true,
            [Symbol.for('foo')],
            [Symbol.for('bar')],
            [Symbol.for('baz')],
          ],
        ],
      ],
      'if (true) {\n' + '  foo();\n' + '} else {\n' + '  bar();\n' + '}',
    ]);
  });
});

describe('when', function (): any {
  it('(when (< 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('when'), [Symbol.for('<'), 1, 2], 1, 2],
      2,
    ]);
  });
  it('(when (> 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('when'), [Symbol.for('>'), 1, 2], 1, 2],
      undefined,
    ]);
  });
  return it("(compile '(when (< 1 2) (foo) (bar)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('when'),
            [Symbol.for('<'), 1, 2],
            [Symbol.for('foo')],
            [Symbol.for('bar')],
          ],
        ],
      ],
      'if (1 < 2) {\n' + '  foo();\n' + '  bar();\n' + '}',
    ]);
  });
});

describe('unless', function (): any {
  it('(unless (< 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('unless'), [Symbol.for('<'), 1, 2], 1, 2],
      undefined,
    ]);
  });
  it('(unless (> 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('unless'), [Symbol.for('>'), 1, 2], 1, 2],
      2,
    ]);
  });
  return it("(compile '(unless (> 1 2) (foo) (bar)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('unless'),
            [Symbol.for('>'), 1, 2],
            [Symbol.for('foo')],
            [Symbol.for('bar')],
          ],
        ],
      ],
      'if (!(1 > 2)) {\n' + '  foo();\n' + '  bar();\n' + '}',
    ]);
  });
});

describe('cond', function (): any {
  it('(cond (#f 1) (else 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cond'), [false, 1], [Symbol.for('else'), 2]],
      2,
    ]);
  });
  it('(cond (#t 1) (#f 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cond'), [true, 1], [false, 2]],
      1,
    ]);
  });
  it('(cond (#f 1) (#t 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cond'), [false, 1], [true, 2]],
      2,
    ]);
  });
  it('(cond (#f 1) (#t 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cond'), [false, 1], [true, 2]],
      2,
    ]);
  });
  it("(macroexpand-1 '(cond (#f (foo)) (else (bar))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('macroexpand-1'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('cond'),
            [false, [Symbol.for('foo')]],
            [Symbol.for('else'), [Symbol.for('bar')]],
          ],
        ],
      ],
      [
        Symbol.for('quote'),
        [Symbol.for('if'), false, [Symbol.for('foo')], [Symbol.for('bar')]],
      ],
    ]);
  });
  it("(macroexpand-1 '(cond (#f (foo) (bar)) (else (baz))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('macroexpand-1'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('cond'),
            [false, [Symbol.for('foo')], [Symbol.for('bar')]],
            [Symbol.for('else'), [Symbol.for('baz')]],
          ],
        ],
      ],
      [
        Symbol.for('quote'),
        [
          Symbol.for('if'),
          false,
          [Symbol.for('begin'), [Symbol.for('foo')], [Symbol.for('bar')]],
          [Symbol.for('baz')],
        ],
      ],
    ]);
  });
  it("(macroexpand-1 '(cond (#f (foo) (bar)) (else (baz) (quux))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('macroexpand-1'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('cond'),
            [false, [Symbol.for('foo')], [Symbol.for('bar')]],
            [Symbol.for('else'), [Symbol.for('baz')], [Symbol.for('quux')]],
          ],
        ],
      ],
      [
        Symbol.for('quote'),
        [
          Symbol.for('if'),
          false,
          [Symbol.for('begin'), [Symbol.for('foo')], [Symbol.for('bar')]],
          [Symbol.for('begin'), [Symbol.for('baz')], [Symbol.for('quux')]],
        ],
      ],
    ]);
  });
  it("(macroexpand-1 '(cond (x (foo)) (y (bar)) (else (baz))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('macroexpand-1'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('cond'),
            [Symbol.for('x'), [Symbol.for('foo')]],
            [Symbol.for('y'), [Symbol.for('bar')]],
            [Symbol.for('else'), [Symbol.for('baz')]],
          ],
        ],
      ],
      [
        Symbol.for('quote'),
        [
          Symbol.for('if'),
          Symbol.for('x'),
          [Symbol.for('foo')],
          [
            Symbol.for('if'),
            Symbol.for('y'),
            [Symbol.for('bar')],
            [Symbol.for('baz')],
          ],
        ],
      ],
    ]);
  });
  it("(compile '(cond (#f (foo)) (else (bar))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('cond'),
            [false, [Symbol.for('foo')]],
            [Symbol.for('else'), [Symbol.for('bar')]],
          ],
        ],
      ],
      'if (false) {\n' + '  foo();\n' + '} else {\n' + '  bar();\n' + '}',
    ]);
  });
  it("(compile '(cond (x (foo)) (y (bar)) (else (baz))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('cond'),
            [Symbol.for('x'), [Symbol.for('foo')]],
            [Symbol.for('y'), [Symbol.for('bar')]],
            [Symbol.for('else'), [Symbol.for('baz')]],
          ],
        ],
      ],
      'if (x) {\n' +
        '  foo();\n' +
        '} else if (y) {\n' +
        '  bar();\n' +
        '} else {\n' +
        '  baz();\n' +
        '}',
    ]);
  });
  it("(compile '(cond (#f (foo)) (else (bar))) :as 'statement)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('cond'),
            [false, [Symbol.for('foo')]],
            [Symbol.for('else'), [Symbol.for('bar')]],
          ],
        ],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('statement')],
      ],
      'if (false) {\n' + '  foo();\n' + '} else {\n' + '  bar();\n' + '}',
    ]);
  });
  it("(compile '(cond (#f (foo)) (else (bar))) :as 'return)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('cond'),
            [false, [Symbol.for('foo')]],
            [Symbol.for('else'), [Symbol.for('bar')]],
          ],
        ],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('return')],
      ],
      'if (false) {\n' +
        '  return foo();\n' +
        '} else {\n' +
        '  return bar();\n' +
        '}',
    ]);
  });
  return it("(compile '(cond (#f (foo)) (else (bar))) :as 'expression)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('cond'),
            [false, [Symbol.for('foo')]],
            [Symbol.for('else'), [Symbol.for('bar')]],
          ],
        ],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('expression')],
      ],
      'false ? foo() : bar()',
    ]);
  });
});

describe('js/?', function (): any {
  it("(compile '(js/? x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x ? y : undefined;',
    ]);
  });
  it("(compile '(js/? x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(js/? x y (js/? z w)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/?'),
            Symbol.for('x'),
            Symbol.for('y'),
            [Symbol.for('js/?'), Symbol.for('z'), Symbol.for('w')],
          ],
        ],
      ],
      'x ? y : (z ? w : undefined);',
    ]);
  });
  it("(compile '(js/? x y z) :as 'statement)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('statement')],
      ],
      'x ? y : z;',
    ]);
  });
  it("(compile '(js/? x y z) :as 'return)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('return')],
      ],
      'return x ? y : z;',
    ]);
  });
  return it("(compile '(js/? x y z) :as 'expression)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('expression')],
      ],
      'x ? y : z',
    ]);
  });
});

describe('js/if', function (): any {
  it("(compile '(js/if x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'if (x) {\n' + '  y;\n' + '}',
    ]);
  });
  it("(compile '(js/if x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(js/if x y (js/if z w)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/if'),
            Symbol.for('x'),
            Symbol.for('y'),
            [Symbol.for('js/if'), Symbol.for('z'), Symbol.for('w')],
          ],
        ],
      ],
      'if (x) {\n' + '  y;\n' + '} else if (z) {\n' + '  w;\n' + '}',
    ]);
  });
  it("(compile '(js/if x y z) :as 'statement)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('statement')],
      ],
      'if (x) {\n' + '  y;\n' + '} else {\n' + '  z;\n' + '}',
    ]);
  });
  it("(compile '(js/if x y z) :as 'return)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('return')],
      ],
      'if (x) {\n' + '  return y;\n' + '} else {\n' + '  return z;\n' + '}',
    ]);
  });
  return it("(compile '(js/if x y z) :as 'expression)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('it>'),
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
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('expression')],
      ],
      '(() => {\n' +
        '  if (x) {\n' +
        '    return y;\n' +
        '  } else {\n' +
        '    return z;\n' +
        '  }\n' +
        '})()',
    ]);
  });
});

describe('js/switch', function (): any {
  it('(let* ((x "foo") (y "bar")) (js/switch x (case "foo" (set! y "baz") (break)) (default (set! y "quux"))) y)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let*'),
        [
          [Symbol.for('x'), 'foo'],
          [Symbol.for('y'), 'bar'],
        ],
        [
          Symbol.for('js/switch'),
          Symbol.for('x'),
          [
            Symbol.for('case'),
            'foo',
            [Symbol.for('set!'), Symbol.for('y'), 'baz'],
            [Symbol.for('break')],
          ],
          [
            Symbol.for('default'),
            [Symbol.for('set!'), Symbol.for('y'), 'quux'],
          ],
        ],
        Symbol.for('y'),
      ],
      'baz',
    ]);
  });
  it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/switch'),
            Symbol.for('x'),
            [
              Symbol.for('case'),
              'foo',
              [Symbol.for('display'), 'foo'],
              [Symbol.for('break')],
            ],
            [Symbol.for('default'), [Symbol.for('display'), 'bar']],
          ],
        ],
      ],
      'switch (x) {\n' +
        "  case 'foo': {\n" +
        "    console.log('foo');\n" +
        '    break;\n' +
        '  }\n' +
        '  default: {\n' +
        "    console.log('bar');\n" +
        '  }\n' +
        '}',
    ]);
  });
  it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))) :as \'return)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/switch'),
            Symbol.for('x'),
            [
              Symbol.for('case'),
              'foo',
              [Symbol.for('display'), 'foo'],
              [Symbol.for('break')],
            ],
            [Symbol.for('default'), [Symbol.for('display'), 'bar']],
          ],
        ],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('return')],
      ],
      'switch (x) {\n' +
        "  case 'foo': {\n" +
        "    return console.log('foo');\n" +
        '    break;\n' +
        '  }\n' +
        '  default: {\n' +
        "    return console.log('bar');\n" +
        '  }\n' +
        '}',
    ]);
  });
  it('(compile \'(js/switch x (case "foo" (display "foo")) (default (display "bar"))) :as \'return)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/switch'),
            Symbol.for('x'),
            [Symbol.for('case'), 'foo', [Symbol.for('display'), 'foo']],
            [Symbol.for('default'), [Symbol.for('display'), 'bar']],
          ],
        ],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('return')],
      ],
      'switch (x) {\n' +
        "  case 'foo': {\n" +
        "    console.log('foo');\n" +
        '  }\n' +
        '  default: {\n' +
        "    return console.log('bar');\n" +
        '  }\n' +
        '}',
    ]);
  });
  return it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))) :as \'expression)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/switch'),
            Symbol.for('x'),
            [
              Symbol.for('case'),
              'foo',
              [Symbol.for('display'), 'foo'],
              [Symbol.for('break')],
            ],
            [Symbol.for('default'), [Symbol.for('display'), 'bar']],
          ],
        ],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('expression')],
      ],
      '(() => {\n' +
        '  switch (x) {\n' +
        "    case 'foo': {\n" +
        "      return console.log('foo');\n" +
        '      break;\n' +
        '    }\n' +
        '    default: {\n' +
        "      return console.log('bar');\n" +
        '    }\n' +
        '  }\n' +
        '})()',
    ]);
  });
});

describe('eq?', function (): any {
  it('(eq? #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('eq?'), true, true],
      true,
    ]);
  });
  it('(eq? #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('eq?'), false, false],
      true,
    ]);
  });
  it('(eq? #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('eq?'), true, false],
      false,
    ]);
  });
  it("(eq '_ '_)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('eq'),
        [Symbol.for('quote'), Symbol.for('_')],
        [Symbol.for('quote'), Symbol.for('_')],
      ],
      true,
    ]);
  });
  it("(eq _ '_)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('eq'),
        Symbol.for('_'),
        [Symbol.for('quote'), Symbol.for('_')],
      ],
      false,
    ]);
  });
  return it("(compile '(eq? #t #t))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('eq?'), true, true]],
      ],
      'true === true;',
    ]);
  });
});

describe('equal?', function (): any {
  it('(equal? #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('equal?'), true, true],
      true,
    ]);
  });
  it('(equal? #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('equal?'), false, false],
      true,
    ]);
  });
  it('(equal? #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('equal?'), true, false],
      false,
    ]);
  });
  it("(equal? _ '_)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('equal?'),
        Symbol.for('_'),
        [Symbol.for('quote'), Symbol.for('_')],
      ],
      false,
    ]);
  });
  it('(equal? 1 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('equal?'), 1, 1],
      true,
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
      true,
    ]);
  });
});

describe('not', function (): any {
  it('(not #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('not'), false],
      true,
    ]);
  });
  it('(not #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('not'), true],
      false,
    ]);
  });
  return it("(compile '(not x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('not'), Symbol.for('x')]],
      ],
      '!x;',
    ]);
  });
});

describe('js/!', function (): any {
  it('(js/! #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/!'), false],
      true,
    ]);
  });
  it('(js/! #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/!'), true],
      false,
    ]);
  });
  return it("(compile '(js/! x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/!'), Symbol.for('x')]],
      ],
      '!x;',
    ]);
  });
});

describe('and', function (): any {
  it('(and)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('and')],
      true,
    ]);
  });
  it('(and #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('and'), true],
      true,
    ]);
  });
  it('(and #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('and'), true, true],
      true,
    ]);
  });
  it('(and #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('and'), false, false],
      false,
    ]);
  });
  it('(and #f #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('and'), false, true],
      false,
    ]);
  });
  it('(and #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('and'), true, false],
      false,
    ]);
  });
  return it("(compile '(and #t #t))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('and'), true, true]],
      ],
      'true && true;',
    ]);
  });
});

describe('js/&&', function (): any {
  it('(js/&&)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/&&')],
      true,
    ]);
  });
  it('(js/&& #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/&&'), true],
      true,
    ]);
  });
  it('(js/&& #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/&&'), true, true],
      true,
    ]);
  });
  it('(js/&& #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/&&'), true, false],
      false,
    ]);
  });
  it('(funcall js/&&)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/&&')],
      true,
    ]);
  });
  it('(funcall js/&& #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/&&'), true],
      true,
    ]);
  });
  it('(funcall js/&& #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/&&'), true, true],
      true,
    ]);
  });
  it('(funcall js/&& #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/&&'), true, false],
      false,
    ]);
  });
  it("(compile '(js/&& x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/&&'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x && y;',
    ]);
  });
  return it("(compile '(js/&& x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/&&'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      'x && y && z;',
    ]);
  });
});

describe('or', function (): any {
  it('(or)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('or')],
      false,
    ]);
  });
  it('(or #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('or'), true],
      true,
    ]);
  });
  it('(or #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('or'), true, true],
      true,
    ]);
  });
  it('(or #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('or'), false, false],
      false,
    ]);
  });
  it('(or #f #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('or'), false, true],
      true,
    ]);
  });
  it('(or #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('or'), true, false],
      true,
    ]);
  });
  it('(or 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('or'), 1, 2],
      1,
    ]);
  });
  it('(or #u 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('or'), undefined, 2],
      2,
    ]);
  });
  return it("(compile '(or #t #t))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('or'), true, true]],
      ],
      'true || true;',
    ]);
  });
});

describe('js/||', function (): any {
  it('(js/||)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/||')],
      false,
    ]);
  });
  it('(js/|| #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/||'), true],
      true,
    ]);
  });
  it('(js/|| #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/||'), true, true],
      true,
    ]);
  });
  it('(js/|| #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/||'), true, false],
      true,
    ]);
  });
  it('(funcall js/||)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/||')],
      false,
    ]);
  });
  it('(funcall js/|| #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/||'), true],
      true,
    ]);
  });
  it('(funcall js/|| #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/||'), true, true],
      true,
    ]);
  });
  it('(funcall js/|| #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/||'), true, false],
      true,
    ]);
  });
  it("(compile '(js/|| x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/||'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x || y;',
    ]);
  });
  return it("(compile '(js/|| x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/||'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      'x || y || z;',
    ]);
  });
});

describe('bitwise-and', function (): any {
  it("(compile '(bitwise-and x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(bit-and x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(js/& x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  return it("(compile '(js/& x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/&'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      'x & y & z;',
    ]);
  });
});

describe('bitwise-or', function (): any {
  it("(compile '(bitwise-or x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(bit-or x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(js/| x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  return it("(compile '(js/| x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/|'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      'x | y | z;',
    ]);
  });
});

describe('bitwise-xor', function (): any {
  it("(compile '(bitwise-xor x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(bit-xor x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  return it("(compile '(js/^ x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
});

describe('bitwise-not', function (): any {
  it("(compile '(bitwise-negation x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(bitwise-not x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('bitwise-not'), Symbol.for('x')]],
      ],
      '~x;',
    ]);
  });
  it("(compile '(bit-not x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('bit-not'), Symbol.for('x')]],
      ],
      '~x;',
    ]);
  });
  return it("(compile '(js/~ x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/~'), Symbol.for('x')]],
      ],
      '~x;',
    ]);
  });
});

describe('bitwise-shift-left', function (): any {
  it("(compile '(bitwise-shift-left x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(bit-shift-left x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  return it("(compile '(js/<< x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
});

describe('bitwise-shift-right', function (): any {
  it("(compile '(bitwise-shift-right x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(bit-shift-right x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  return it("(compile '(js/>> x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
});

describe('unsigned-bitwise-shift-right', function (): any {
  it("(compile '(unsigned-bitwise-shift-right x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('unsigned-bitwise-shift-right'),
            Symbol.for('x'),
            Symbol.for('y'),
          ],
        ],
      ],
      'x >>> y;',
    ]);
  });
  it("(compile '(unsigned-bit-shift-right x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
      'x >>> y;',
    ]);
  });
  return it("(compile '(js/>>> x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
});

describe('js/op', function (): any {
  it('(js/op ! #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/op'), Symbol.for('!'), true],
      false,
    ]);
  });
  it('(js/op && #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/op'), Symbol.for('&&'), true, false],
      false,
    ]);
  });
  it('(js/op || #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/op'), Symbol.for('||'), true, false],
      true,
    ]);
  });
  it("(compile '(js/op ! x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/op'), Symbol.for('!'), Symbol.for('x')],
        ],
      ],
      '!x;',
    ]);
  });
  it("(compile '(js/op ~ x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/op'), Symbol.for('~'), Symbol.for('x')],
        ],
      ],
      '~x;',
    ]);
  });
  it("(compile '(js/op & x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/op'),
            Symbol.for('&'),
            Symbol.for('x'),
            Symbol.for('y'),
          ],
        ],
      ],
      'x & y;',
    ]);
  });
  it("(compile '(js/op && x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/op'),
            Symbol.for('&&'),
            Symbol.for('x'),
            Symbol.for('y'),
          ],
        ],
      ],
      'x && y;',
    ]);
  });
  return it("(compile '(js/op || x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/op'),
            Symbol.for('||'),
            Symbol.for('x'),
            Symbol.for('y'),
          ],
        ],
      ],
      'x || y;',
    ]);
  });
});

describe('while', function (): any {
  it("(let ((result '())) (while (< (length result) 3) (set! result (cons 1 result))) result)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('result'), [Symbol.for('quote'), []]]],
        [
          Symbol.for('while'),
          [Symbol.for('<'), [Symbol.for('length'), Symbol.for('result')], 3],
          [
            Symbol.for('set!'),
            Symbol.for('result'),
            [Symbol.for('cons'), 1, Symbol.for('result')],
          ],
        ],
        Symbol.for('result'),
      ],
      [Symbol.for('quote'), [1, 1, 1]],
    ]);
  });
  return it("(compile '(while (> x 0) (set! x (- x 1))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('while'),
            [Symbol.for('>'), Symbol.for('x'), 0],
            [
              Symbol.for('set!'),
              Symbol.for('x'),
              [Symbol.for('-'), Symbol.for('x'), 1],
            ],
          ],
        ],
      ],
      'while (x > 0) {\n' + '  x--;\n' + '}',
    ]);
  });
});

describe('for', function (): any {
  it('(let (result) (js/for (() () ()) (set! result 1) (break)) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('result')],
        [
          Symbol.for('js/for'),
          [[], [], []],
          [Symbol.for('set!'), Symbol.for('result'), 1],
          [Symbol.for('break')],
        ],
        Symbol.for('result'),
      ],
      1,
    ]);
  });
  it('(let (result) (js/for (#u #u #u) (set! result 1) (break)) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('result')],
        [
          Symbol.for('js/for'),
          [undefined, undefined, undefined],
          [Symbol.for('set!'), Symbol.for('result'), 1],
          [Symbol.for('break')],
        ],
        Symbol.for('result'),
      ],
      1,
    ]);
  });
  it("(let ((result '())) (for ((x '(1 2 3))) (set! result (cons x result))) result)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('result'), [Symbol.for('quote'), []]]],
        [
          Symbol.for('for'),
          [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
          [
            Symbol.for('set!'),
            Symbol.for('result'),
            [Symbol.for('cons'), Symbol.for('x'), Symbol.for('result')],
          ],
        ],
        Symbol.for('result'),
      ],
      [Symbol.for('quote'), [3, 2, 1]],
    ]);
  });
  it("((lambda () (define foo '(1 2 3 4)) (define len (length foo)) (for ((i (range 0 len))) (pop-right! foo)) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [1, 2, 3, 4]],
          ],
          [
            Symbol.for('define'),
            Symbol.for('len'),
            [Symbol.for('length'), Symbol.for('foo')],
          ],
          [
            Symbol.for('for'),
            [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('len')]]],
            [Symbol.for('pop-right!'), Symbol.for('foo')],
          ],
          Symbol.for('foo'),
        ],
      ],
      [Symbol.for('quote'), []],
    ]);
  });
  it("((lambda () (define foo '(1 2 3 4)) (for ((i (range 0 (length foo)))) (pop-right! foo)) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [1, 2, 3, 4]],
          ],
          [
            Symbol.for('for'),
            [
              [
                Symbol.for('i'),
                [
                  Symbol.for('range'),
                  0,
                  [Symbol.for('length'), Symbol.for('foo')],
                ],
              ],
            ],
            [Symbol.for('pop-right!'), Symbol.for('foo')],
          ],
          Symbol.for('foo'),
        ],
      ],
      [Symbol.for('quote'), []],
    ]);
  });
  it("(compile '(js/for (() () ()) (break)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/for'), [[], [], []], [Symbol.for('break')]],
        ],
      ],
      'for (;;) {\n' + '  break;\n' + '}',
    ]);
  });
  it("(compile '(js/for (#u #u #u) (break)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/for'),
            [undefined, undefined, undefined],
            [Symbol.for('break')],
          ],
        ],
      ],
      'for (;;) {\n' + '  break;\n' + '}',
    ]);
  });
  it("(compile '(for ((i (range 0 10))) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('for'),
            [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (let i = 0; i < 10; i++) {\n' + '  foo();\n' + '}',
    ]);
  });
  it("(compile '(for ((i (range 0 10 2))) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('for'),
            [[Symbol.for('i'), [Symbol.for('range'), 0, 10, 2]]],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (let i = 0; i < 10; i = i + 2) {\n' + '  foo();\n' + '}',
    ]);
  });
  it("(compile '(for ((x lst)) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('for'),
            [[Symbol.for('x'), Symbol.for('lst')]],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (let x of lst) {\n' + '  foo();\n' + '}',
    ]);
  });
  it("(compile '(for ((x '(1 2 3))) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('for'),
            [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (let x of [1, 2, 3]) {\n' + '  foo();\n' + '}',
    ]);
  });
  it("(compile '(for ((i (range 0 len))) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('for'),
            [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('len')]]],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (let i = 0; i < len; i++) {\n' + '  foo();\n' + '}',
    ]);
  });
  it("(compile '(for ((i (range 0 (js/length foo)))) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('for'),
            [
              [
                Symbol.for('i'),
                [
                  Symbol.for('range'),
                  0,
                  [Symbol.for('js/length'), Symbol.for('foo')],
                ],
              ],
            ],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'let _end = foo.length;\n' +
        '\n' +
        'for (let i = 0; i < _end; i++) {\n' +
        '  foo();\n' +
        '}',
    ]);
  });
  return xit("(compile '(for ((i (range 0 10)) (j (range 0 10))) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('for'),
            [
              [Symbol.for('i'), [Symbol.for('range'), 0, 10]],
              [Symbol.for('j'), [Symbol.for('range'), 0, 10]],
            ],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {\n' +
        '  foo();\n' +
        '}',
    ]);
  });
});

describe('js/for', function (): any {
  it('(let ((result 0)) (js/for ((i 0) (< i 10) (+ i 1)) (set! result (+ result 2))) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('result'), 0]],
        [
          Symbol.for('js/for'),
          [
            [Symbol.for('i'), 0],
            [Symbol.for('<'), Symbol.for('i'), 10],
            [Symbol.for('+'), Symbol.for('i'), 1],
          ],
          [
            Symbol.for('set!'),
            Symbol.for('result'),
            [Symbol.for('+'), Symbol.for('result'), 2],
          ],
        ],
        Symbol.for('result'),
      ],
      20,
    ]);
  });
  it("(compile '(js/for ((i 0) (< i 10) (+ i 1)) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it("(compile '(js/for ((set! i 0) (< i 10) (+ i 1)) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/for'),
            [
              [Symbol.for('set!'), Symbol.for('i'), 0],
              [Symbol.for('<'), Symbol.for('i'), 10],
              [Symbol.for('+'), Symbol.for('i'), 1],
            ],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (i = 0; i < 10; i++) {\n' + '  foo();\n' + '}',
    ]);
  });
  it("(compile '(js/for ((define i 0) (< i 10) (+ i 1)) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/for'),
            [
              [Symbol.for('define'), Symbol.for('i'), 0],
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
  return it("(compile '(js/for ((begin (set! i 0) (set! j 0)) (and (< i 10) (< j 10)) (begin (set! i (+ i 1)) (set! j (+ j 1)))) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/for'),
            [
              [
                Symbol.for('begin'),
                [Symbol.for('set!'), Symbol.for('i'), 0],
                [Symbol.for('set!'), Symbol.for('j'), 0],
              ],
              [
                Symbol.for('and'),
                [Symbol.for('<'), Symbol.for('i'), 10],
                [Symbol.for('<'), Symbol.for('j'), 10],
              ],
              [
                Symbol.for('begin'),
                [
                  Symbol.for('set!'),
                  Symbol.for('i'),
                  [Symbol.for('+'), Symbol.for('i'), 1],
                ],
                [
                  Symbol.for('set!'),
                  Symbol.for('j'),
                  [Symbol.for('+'), Symbol.for('j'), 1],
                ],
              ],
            ],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {\n' +
        '  foo();\n' +
        '}',
    ]);
  });
});

describe('js/for-in', function (): any {
  return it("(compile '(js/for-in ((i obj)) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/for-in'),
            [[Symbol.for('i'), Symbol.for('obj')]],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (let i in obj) {\n' + '  foo();\n' + '}',
    ]);
  });
});

describe('js/for-of', function (): any {
  return it("(compile '(js/for-of ((i lst)) (foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/for-of'),
            [[Symbol.for('i'), Symbol.for('lst')]],
            [Symbol.for('foo')],
          ],
        ],
      ],
      'for (let i of lst) {\n' + '  foo();\n' + '}',
    ]);
  });
});

describe('break', function (): any {
  it('((lambda () (while #t (break)) 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [Symbol.for('while'), true, [Symbol.for('break')]],
          1,
        ],
      ],
      1,
    ]);
  });
  it('(let ((result (list))) (for ((i (range 0 10))) (break) (push-right! result i)) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('result'), [Symbol.for('list')]]],
        [
          Symbol.for('for'),
          [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]],
          [Symbol.for('break')],
          [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')],
        ],
        Symbol.for('result'),
      ],
      [Symbol.for('quote'), []],
    ]);
  });
  return it("(compile '(while #t (break)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('while'), true, [Symbol.for('break')]],
        ],
      ],
      'while (true) {\n' + '  break;\n' + '}',
    ]);
  });
});

describe('continue', function (): any {
  it('(let ((result (list)) (i 0)) (while (< i 10) (set! i (+ i 1)) (when (< i 5) (continue)) (push-right! result i)) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [Symbol.for('result'), [Symbol.for('list')]],
          [Symbol.for('i'), 0],
        ],
        [
          Symbol.for('while'),
          [Symbol.for('<'), Symbol.for('i'), 10],
          [
            Symbol.for('set!'),
            Symbol.for('i'),
            [Symbol.for('+'), Symbol.for('i'), 1],
          ],
          [
            Symbol.for('when'),
            [Symbol.for('<'), Symbol.for('i'), 5],
            [Symbol.for('continue')],
          ],
          [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')],
        ],
        Symbol.for('result'),
      ],
      [Symbol.for('quote'), [5, 6, 7, 8, 9, 10]],
    ]);
  });
  it('(let ((result (list))) (for ((i (range 0 11))) (when (< i 5) (continue)) (push-right! result i)) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('result'), [Symbol.for('list')]]],
        [
          Symbol.for('for'),
          [[Symbol.for('i'), [Symbol.for('range'), 0, 11]]],
          [
            Symbol.for('when'),
            [Symbol.for('<'), Symbol.for('i'), 5],
            [Symbol.for('continue')],
          ],
          [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')],
        ],
        Symbol.for('result'),
      ],
      [Symbol.for('quote'), [5, 6, 7, 8, 9, 10]],
    ]);
  });
  return it("(compile '(while #f (continue)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('while'), false, [Symbol.for('continue')]],
        ],
      ],
      'while (false) {\n' + '  continue;\n' + '}',
    ]);
  });
});

describe('return', function (): any {
  it('((lambda () (return 1) 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [[Symbol.for('lambda'), [], [Symbol.for('return'), 1], 2]],
      1,
    ]);
  });
  it('((js/function () (return 1) 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [[Symbol.for('js/function'), [], [Symbol.for('return'), 1], 2]],
      1,
    ]);
  });
  it('((js/arrow () (return 1) 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [[Symbol.for('js/arrow'), [], [Symbol.for('return'), 1], 2]],
      1,
    ]);
  });
  return it("(compile '(while #t (return 0)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('while'), true, [Symbol.for('return'), 0]],
        ],
      ],
      'while (true) {\n' + '  return 0;\n' + '}',
    ]);
  });
});

describe('js/.', function (): any {
  it('(let ((obj (js/obj "foo" "bar"))) (js/. obj foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('foo')],
      ],
      'bar',
    ]);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/. obj "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [Symbol.for('js/.'), Symbol.for('obj'), 'foo'],
      ],
      'bar',
    ]);
  });
  it('(let ((obj (js/obj "foo" (js/obj "bar" "baz")))) (js/. obj foo bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('obj'),
            [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'bar', 'baz']],
          ],
        ],
        [
          Symbol.for('js/.'),
          Symbol.for('obj'),
          Symbol.for('foo'),
          Symbol.for('bar'),
        ],
      ],
      'baz',
    ]);
  });
  it('(let ((obj (js/obj "foo" (js/obj "bar" "baz")))) (js/. (js/. obj foo) bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('obj'),
            [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'bar', 'baz']],
          ],
        ],
        [
          Symbol.for('js/.'),
          [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('foo')],
          Symbol.for('bar'),
        ],
      ],
      'baz',
    ]);
  });
  it("(compile '(js/. obj prop))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop')],
        ],
      ],
      'obj.prop;',
    ]);
  });
  it('(compile \'(js/. obj "prop"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), 'prop']],
      ],
      "obj['prop'];",
    ]);
  });
  it("(compile '(js/. obj :foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for(':foo')],
        ],
      ],
      'obj.foo;',
    ]);
  });
  it("(compile '(js/. obj :foo-bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for(':foo-bar')],
        ],
      ],
      'obj.fooBar;',
    ]);
  });
  it("(compile '(js/. obj 'foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/.'),
            Symbol.for('obj'),
            [Symbol.for('quote'), Symbol.for('foo')],
          ],
        ],
      ],
      'obj.foo;',
    ]);
  });
  it("(compile '(js/. obj 'foo-bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/.'),
            Symbol.for('obj'),
            [Symbol.for('quote'), Symbol.for('foo-bar')],
          ],
        ],
      ],
      'obj.fooBar;',
    ]);
  });
  it("(compile '(js/. obj prop1 prop2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/.'),
            Symbol.for('obj'),
            Symbol.for('prop1'),
            Symbol.for('prop2'),
          ],
        ],
      ],
      'obj.prop1.prop2;',
    ]);
  });
  return it("(compile '(js/. (js/. obj prop1) prop2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/.'),
            [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop1')],
            Symbol.for('prop2'),
          ],
        ],
      ],
      'obj.prop1.prop2;',
    ]);
  });
});

describe('js/?.', function (): any {
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('foo')],
      ],
      'bar',
    ]);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [Symbol.for('js/?.'), Symbol.for('obj'), 'foo'],
      ],
      'bar',
    ]);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj quux))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux')],
      ],
      undefined,
    ]);
  });
  it('(let ((obj (js/obj "foo" "bar"))) ((js/?. obj quux)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [[Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux')]],
      ],
      undefined,
    ]);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj quux wobble))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [
          Symbol.for('js/?.'),
          Symbol.for('obj'),
          Symbol.for('quux'),
          Symbol.for('wobble'),
        ],
      ],
      undefined,
    ]);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. (js/?. obj quux) wobble))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [
          Symbol.for('js/?.'),
          [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux')],
          Symbol.for('wobble'),
        ],
      ],
      undefined,
    ]);
  });
  it("(compile '(js/?. obj prop))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('prop')],
        ],
      ],
      'obj?.prop;',
    ]);
  });
  it("(compile '(js/?. obj prop1 prop2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/?.'),
            Symbol.for('obj'),
            Symbol.for('prop1'),
            Symbol.for('prop2'),
          ],
        ],
      ],
      'obj?.prop1?.prop2;',
    ]);
  });
  it("(compile '(js/?. (js/?. obj prop1) prop2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/?.'),
            [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('prop1')],
            Symbol.for('prop2'),
          ],
        ],
      ],
      'obj?.prop1?.prop2;',
    ]);
  });
  return it("(compile '(js/?. (js/?. (js/?. obj) prop1) prop2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/?.'),
            [
              Symbol.for('js/?.'),
              [Symbol.for('js/?.'), Symbol.for('obj')],
              Symbol.for('prop1'),
            ],
            Symbol.for('prop2'),
          ],
        ],
      ],
      'obj?.prop1?.prop2;',
    ]);
  });
});

describe('get-field', function (): any {
  it('(let ((obj (js/obj "foo" "bar"))) (get-field foo obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [Symbol.for('get-field'), Symbol.for('foo'), Symbol.for('obj')],
      ],
      'bar',
    ]);
  });
  it("(compile '(get-field foo obj))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('get-field'), Symbol.for('foo'), Symbol.for('obj')],
        ],
      ],
      'obj.foo;',
    ]);
  });
  it("(compile '(get-field foo-bar obj))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('get-field'), Symbol.for('foo-bar'), Symbol.for('obj')],
        ],
      ],
      'obj.fooBar;',
    ]);
  });
  it('(compile \'(get-field "foo" obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('get-field'), 'foo', Symbol.for('obj')],
        ],
      ],
      "obj['foo'];",
    ]);
  });
  return it('(compile \'(get-field "foo-bar" obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('get-field'), 'foo-bar', Symbol.for('obj')],
        ],
      ],
      "obj['foo-bar'];",
    ]);
  });
});

describe('set-field!', function (): any {
  it('(let ((obj (js/obj))) (set-field! foo-bar obj "baz") (get-field foo-bar obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj')]]],
        [
          Symbol.for('set-field!'),
          Symbol.for('foo-bar'),
          Symbol.for('obj'),
          'baz',
        ],
        [Symbol.for('get-field'), Symbol.for('foo-bar'), Symbol.for('obj')],
      ],
      'baz',
    ]);
  });
  it('(compile \'(set-field! foo-bar obj "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('set-field!'),
            Symbol.for('foo-bar'),
            Symbol.for('obj'),
            'baz',
          ],
        ],
      ],
      "obj.fooBar = 'baz';",
    ]);
  });
  it('(compile \'(set-field! \'foo-bar obj "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('set-field!'),
            [Symbol.for('quote'), Symbol.for('foo-bar')],
            Symbol.for('obj'),
            'baz',
          ],
        ],
      ],
      "obj.fooBar = 'baz';",
    ]);
  });
  it('(compile \'(set-field! :foo-bar obj "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('set-field!'),
            Symbol.for(':foo-bar'),
            Symbol.for('obj'),
            'baz',
          ],
        ],
      ],
      "obj.fooBar = 'baz';",
    ]);
  });
  it('(compile \'(set-field! "foo-bar" obj "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('set-field!'), 'foo-bar', Symbol.for('obj'), 'baz'],
        ],
      ],
      "obj['foo-bar'] = 'baz';",
    ]);
  });
  return it('(compile \'(set-field! (foo-bar) obj "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('set-field!'),
            [Symbol.for('foo-bar')],
            Symbol.for('obj'),
            'baz',
          ],
        ],
      ],
      "obj[fooBar()] = 'baz';",
    ]);
  });
});

describe('field-bound?', function (): any {
  return it('(let ((obj (js/obj "foo" "bar"))) (field-bound? foo obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [Symbol.for('field-bound?'), Symbol.for('foo'), Symbol.for('obj')],
      ],
      true,
    ]);
  });
});

describe('oget', function (): any {
  it('(let ((obj (js/obj "prop" "foo"))) (oget obj "prop"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'prop', 'foo']]],
        [Symbol.for('oget'), Symbol.for('obj'), 'prop'],
      ],
      'foo',
    ]);
  });
  it('(oget _ "@@functional/placeholder")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('oget'), Symbol.for('_'), '@@functional/placeholder'],
      true,
    ]);
  });
  it("(compile '(oget obj foo-bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('oget'), Symbol.for('obj'), Symbol.for('foo-bar')],
        ],
      ],
      'obj[fooBar];',
    ]);
  });
  it("(compile '(oget obj 'foo-bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('oget'),
            Symbol.for('obj'),
            [Symbol.for('quote'), Symbol.for('foo-bar')],
          ],
        ],
      ],
      "obj['fooBar'];",
    ]);
  });
  it("(compile '(oget obj :foo-bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('oget'), Symbol.for('obj'), Symbol.for(':foo-bar')],
        ],
      ],
      "obj['fooBar'];",
    ]);
  });
  it('(compile \'(oget obj "foo-bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('oget'), Symbol.for('obj'), 'foo-bar'],
        ],
      ],
      "obj['foo-bar'];",
    ]);
  });
  return it("(compile '(oget obj (foo-bar)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('oget'), Symbol.for('obj'), [Symbol.for('foo-bar')]],
        ],
      ],
      'obj[fooBar()];',
    ]);
  });
});

describe('oset!', function (): any {
  it('(let ((obj (js/obj))) (oset! obj \'foo-bar "baz") (oget obj \'foo-bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj')]]],
        [
          Symbol.for('oset!'),
          Symbol.for('obj'),
          [Symbol.for('quote'), Symbol.for('foo-bar')],
          'baz',
        ],
        [
          Symbol.for('oget'),
          Symbol.for('obj'),
          [Symbol.for('quote'), Symbol.for('foo-bar')],
        ],
      ],
      'baz',
    ]);
  });
  it('(let ((obj (js/obj))) (oset! obj :foo-bar "baz") (oget obj :foo-bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj')]]],
        [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for(':foo-bar'), 'baz'],
        [Symbol.for('oget'), Symbol.for('obj'), Symbol.for(':foo-bar')],
      ],
      'baz',
    ]);
  });
  it('(let ((obj (js/obj))) (oset! obj "foo-bar" "baz") (oget obj "foo-bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj')]]],
        [Symbol.for('oset!'), Symbol.for('obj'), 'foo-bar', 'baz'],
        [Symbol.for('oget'), Symbol.for('obj'), 'foo-bar'],
      ],
      'baz',
    ]);
  });
  it('(compile \'(oset! obj foo-bar "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('oset!'),
            Symbol.for('obj'),
            Symbol.for('foo-bar'),
            'baz',
          ],
        ],
      ],
      "obj[fooBar] = 'baz';",
    ]);
  });
  it('(compile \'(oset! obj \'foo-bar "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('oset!'),
            Symbol.for('obj'),
            [Symbol.for('quote'), Symbol.for('foo-bar')],
            'baz',
          ],
        ],
      ],
      "obj['fooBar'] = 'baz';",
    ]);
  });
  it('(compile \'(oset! obj :foo-bar "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('oset!'),
            Symbol.for('obj'),
            Symbol.for(':foo-bar'),
            'baz',
          ],
        ],
      ],
      "obj['fooBar'] = 'baz';",
    ]);
  });
  it('(compile \'(oset! obj "foo-bar" "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('oset!'), Symbol.for('obj'), 'foo-bar', 'baz'],
        ],
      ],
      "obj['foo-bar'] = 'baz';",
    ]);
  });
  return it('(compile \'(oset! obj (foo-bar) "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('oset!'),
            Symbol.for('obj'),
            [Symbol.for('foo-bar')],
            'baz',
          ],
        ],
      ],
      "obj[fooBar()] = 'baz';",
    ]);
  });
});

describe('send', function (): any {
  it('(let ((obj (js/obj "add" (lambda (x y) (+ x y))))) (send obj add 1 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('obj'),
            [
              Symbol.for('js/obj'),
              'add',
              [
                Symbol.for('lambda'),
                [Symbol.for('x'), Symbol.for('y')],
                [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
              ],
            ],
          ],
        ],
        [Symbol.for('send'), Symbol.for('obj'), Symbol.for('add'), 1, 1],
      ],
      2,
    ]);
  });
  it('(let ((obj (make-hash \'(("foo" . "foo"))))) (send obj has "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('obj'),
            [
              Symbol.for('make-hash'),
              [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]],
            ],
          ],
        ],
        [Symbol.for('send'), Symbol.for('obj'), Symbol.for('has'), 'foo'],
      ],
      true,
    ]);
  });
  it('(let ((obj (make-hash \'(("foo" . "foo"))))) (send obj has "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('obj'),
            [
              Symbol.for('make-hash'),
              [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]],
            ],
          ],
        ],
        [Symbol.for('send'), Symbol.for('obj'), Symbol.for('has'), 'bar'],
      ],
      false,
    ]);
  });
  return it("(compile '(send obj m arg))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('send'),
            Symbol.for('obj'),
            Symbol.for('m'),
            Symbol.for('arg'),
          ],
        ],
      ],
      'obj.m(arg);',
    ]);
  });
});

describe('send/apply', function (): any {
  it('(let ((obj (make-hash \'(("foo" . "foo"))))) (send/apply obj has \'("foo")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('obj'),
            [
              Symbol.for('make-hash'),
              [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]],
            ],
          ],
        ],
        [
          Symbol.for('send/apply'),
          Symbol.for('obj'),
          Symbol.for('has'),
          [Symbol.for('quote'), ['foo']],
        ],
      ],
      true,
    ]);
  });
  return it("(compile '(send/apply obj m args))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('send/apply'),
            Symbol.for('obj'),
            Symbol.for('m'),
            Symbol.for('args'),
          ],
        ],
      ],
      'obj.m(...args);',
    ]);
  });
});

describe('new', function (): any {
  it('(let (quux) (set! quux (new (class () (define/public (bar) "baz")))) (send quux bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('quux')],
        [
          Symbol.for('set!'),
          Symbol.for('quux'),
          [
            Symbol.for('new'),
            [
              Symbol.for('class'),
              [],
              [Symbol.for('define/public'), [Symbol.for('bar')], 'baz'],
            ],
          ],
        ],
        [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')],
      ],
      'baz',
    ]);
  });
  it('(let (quux) (set! quux (new (class () (define/public val 1) (define (constructor x) (set-field! val this x)) (define/public (bar) (get-field val this))) 2)) (send quux bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('quux')],
        [
          Symbol.for('set!'),
          Symbol.for('quux'),
          [
            Symbol.for('new'),
            [
              Symbol.for('class'),
              [],
              [Symbol.for('define/public'), Symbol.for('val'), 1],
              [
                Symbol.for('define'),
                [Symbol.for('constructor'), Symbol.for('x')],
                [
                  Symbol.for('set-field!'),
                  Symbol.for('val'),
                  Symbol.for('this'),
                  Symbol.for('x'),
                ],
              ],
              [
                Symbol.for('define/public'),
                [Symbol.for('bar')],
                [
                  Symbol.for('get-field'),
                  Symbol.for('val'),
                  Symbol.for('this'),
                ],
              ],
            ],
            2,
          ],
        ],
        [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')],
      ],
      2,
    ]);
  });
  it("(compile '(new Foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('new'), Symbol.for('Foo')]],
      ],
      'new Foo();',
    ]);
  });
  return it("(compile '(new Foo x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('new'), Symbol.for('Foo'), Symbol.for('x')],
        ],
      ],
      'new Foo(x);',
    ]);
  });
});

describe('new/apply', function (): any {
  return it("(compile '(new/apply Foo args))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('new/apply'), Symbol.for('Foo'), Symbol.for('args')],
        ],
      ],
      'new Foo(...args);',
    ]);
  });
});

describe('class', function (): any {
  it('((lambda () (define Foo (class object% (define/public (bar) "baz"))) (define quux (new Foo)) (send quux bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('Foo'),
            [
              Symbol.for('class'),
              Symbol.for('object%'),
              [Symbol.for('define/public'), [Symbol.for('bar')], 'baz'],
            ],
          ],
          [
            Symbol.for('define'),
            Symbol.for('quux'),
            [Symbol.for('new'), Symbol.for('Foo')],
          ],
          [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')],
        ],
      ],
      'baz',
    ]);
  });
  it('((lambda () (defclass Foo () (define/public (bar) "baz")) (define quux (new Foo)) (send quux bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('defclass'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define/public'), [Symbol.for('bar')], 'baz'],
          ],
          [
            Symbol.for('define'),
            Symbol.for('quux'),
            [Symbol.for('new'), Symbol.for('Foo')],
          ],
          [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')],
        ],
      ],
      'baz',
    ]);
  });
  it('((lambda () (defclass Foo () (define bar "baz")) (define quux (new Foo)) (get-field bar quux)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('defclass'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define'), Symbol.for('bar'), 'baz'],
          ],
          [
            Symbol.for('define'),
            Symbol.for('quux'),
            [Symbol.for('new'), Symbol.for('Foo')],
          ],
          [Symbol.for('get-field'), Symbol.for('bar'), Symbol.for('quux')],
        ],
      ],
      'baz',
    ]);
  });
  it('((lambda () (defclass Foo () (define x) (define (constructor x) (set-field! x this x)) (define (bar) (get-field x this))) (define quux (new Foo "xyzzy")) (send quux bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('defclass'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define'), Symbol.for('x')],
            [
              Symbol.for('define'),
              [Symbol.for('constructor'), Symbol.for('x')],
              [
                Symbol.for('set-field!'),
                Symbol.for('x'),
                Symbol.for('this'),
                Symbol.for('x'),
              ],
            ],
            [
              Symbol.for('define'),
              [Symbol.for('bar')],
              [Symbol.for('get-field'), Symbol.for('x'), Symbol.for('this')],
            ],
          ],
          [
            Symbol.for('define'),
            Symbol.for('quux'),
            [Symbol.for('new'), Symbol.for('Foo'), 'xyzzy'],
          ],
          [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')],
        ],
      ],
      'xyzzy',
    ]);
  });
  it('((lambda () (defclass Foo (Object) (define (bar) "baz")) (define quux (new Foo)) (send quux bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('defclass'),
            Symbol.for('Foo'),
            [Symbol.for('Object')],
            [Symbol.for('define'), [Symbol.for('bar')], 'baz'],
          ],
          [
            Symbol.for('define'),
            Symbol.for('quux'),
            [Symbol.for('new'), Symbol.for('Foo')],
          ],
          [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')],
        ],
      ],
      'baz',
    ]);
  });
  return it('(compile \'(class () (define/public (bar) "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('class'),
            [],
            [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
          ],
        ],
      ],
      'class {\n' + '  bar() {\n' + "    return 'bar';\n" + '  }\n' + '}',
    ]);
  });
});

describe('define-class', function (): any {
  it('((lambda () (define-class Foo () (define/public (bar) "bar")) (define foo (new Foo)) (send foo bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-class'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
          ],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('new'), Symbol.for('Foo')],
          ],
          [Symbol.for('send'), Symbol.for('foo'), Symbol.for('bar')],
        ],
      ],
      'bar',
    ]);
  });
  return it('(compile \'(define-class Foo () (define/public (bar) "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('define-class'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
          ],
        ],
      ],
      'class Foo {\n' + '  bar() {\n' + "    return 'bar';\n" + '  }\n' + '}',
    ]);
  });
});

describe('defclass', function (): any {
  it('((lambda () (defclass Foo () (define/public (bar) "bar")) (define foo (new Foo)) (send foo bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('defclass'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
          ],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('new'), Symbol.for('Foo')],
          ],
          [Symbol.for('send'), Symbol.for('foo'), Symbol.for('bar')],
        ],
      ],
      'bar',
    ]);
  });
  return it('(compile \'(defclass Foo () (define/public (bar) "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('defclass'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
          ],
        ],
      ],
      'class Foo {\n' + '  bar() {\n' + "    return 'bar';\n" + '  }\n' + '}',
    ]);
  });
});

describe('instance-of?', function (): any {
  it('(instance-of? (new Map) Map)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('instance-of?'),
        [Symbol.for('new'), Symbol.for('Map')],
        Symbol.for('Map'),
      ],
      true,
    ]);
  });
  return it("(compile '(instance-of? x Foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('instance-of?'), Symbol.for('x'), Symbol.for('Foo')],
        ],
      ],
      'x instanceof Foo;',
    ]);
  });
});

describe('is-a?', function (): any {
  it('(is-a? (new Map) Map)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('is-a?'),
        [Symbol.for('new'), Symbol.for('Map')],
        Symbol.for('Map'),
      ],
      true,
    ]);
  });
  return it("(compile '(is-a? x Foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Foo')],
        ],
      ],
      'x instanceof Foo;',
    ]);
  });
});

describe('js/obj', function (): any {
  it('(js/obj)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/obj')],
      [Symbol.for('js/obj')],
    ]);
  });
  it('(js/obj "foo" "bar")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/obj'), 'foo', 'bar'],
      [Symbol.for('js/obj'), 'foo', 'bar'],
    ]);
  });
  it('(js/obj "foo" 1 "bar" 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/obj'), 'foo', 1, 'bar', 2],
      [Symbol.for('js/obj'), 'foo', 1, 'bar', 2],
    ]);
  });
  it("(compile '(js/obj))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj')]]],
      '({});',
    ]);
  });
  it('(compile \'(js/obj "foo" foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', Symbol.for('foo')]],
      ],
      '({\n' + '  foo\n' + '});',
    ]);
  });
  it('(compile \'(js/obj "foo" "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 'bar']],
      ],
      '({\n' + "  foo: 'bar'\n" + '});',
    ]);
  });
  it("(compile '(js/obj foo foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/obj'), Symbol.for('foo'), Symbol.for('foo')],
        ],
      ],
      '({\n' + '  [foo]: foo\n' + '});',
    ]);
  });
  it('(compile \'(js/obj foo "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for('foo'), 'bar']],
      ],
      '({\n' + "  [foo]: 'bar'\n" + '});',
    ]);
  });
  it('(compile \'(js/obj \'foo "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/obj'),
            [Symbol.for('quote'), Symbol.for('foo')],
            'bar',
          ],
        ],
      ],
      '({\n' + "  foo: 'bar'\n" + '});',
    ]);
  });
  it('(compile \'(js/obj :foo "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/obj'), Symbol.for(':foo'), 'bar'],
        ],
      ],
      '({\n' + "  foo: 'bar'\n" + '});',
    ]);
  });
  it('(compile \'(js/obj "foo-bar" "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo-bar', 'baz']],
      ],
      '({\n' + "  'foo-bar': 'baz'\n" + '});',
    ]);
  });
  it('(compile \'(js/obj foo-bar "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/obj'), Symbol.for('foo-bar'), 'baz'],
        ],
      ],
      '({\n' + "  [fooBar]: 'baz'\n" + '});',
    ]);
  });
  it('(compile \'(js/obj \'foo-bar "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/obj'),
            [Symbol.for('quote'), Symbol.for('foo-bar')],
            'baz',
          ],
        ],
      ],
      '({\n' + "  fooBar: 'baz'\n" + '});',
    ]);
  });
  it('(compile \'(js/obj :foo-bar "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/obj'), Symbol.for(':foo-bar'), 'baz'],
        ],
      ],
      '({\n' + "  fooBar: 'baz'\n" + '});',
    ]);
  });
  it('(compile \'(js/obj "foo bar" "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo bar', 'baz']],
      ],
      '({\n' + "  'foo bar': 'baz'\n" + '});',
    ]);
  });
  it('(compile \'(js/obj "foo bar" baz))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/obj'), 'foo bar', Symbol.for('baz')],
        ],
      ],
      '({\n' + "  'foo bar': baz\n" + '});',
    ]);
  });
  it('(compile \'(js/obj "foo bar" \'baz))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/obj'),
            'foo bar',
            [Symbol.for('quote'), Symbol.for('baz')],
          ],
        ],
      ],
      '({\n' + "  'foo bar': Symbol.for('baz')\n" + '});',
    ]);
  });
  it('(compile \'(js/obj "foo bar" :baz))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/obj'), 'foo bar', Symbol.for(':baz')],
        ],
      ],
      '({\n' + "  'foo bar': Symbol.for(':baz')\n" + '});',
    ]);
  });
  it('(compile \'(js/obj "foo" 1 "bar" 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]],
      ],
      '({\n' + '  foo: 1,\n' + '  bar: 2\n' + '});',
    ]);
  });
  it('(compile \'(js/obj "foo" foo "bar" bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/obj'),
            'foo',
            Symbol.for('foo'),
            'bar',
            Symbol.for('bar'),
          ],
        ],
      ],
      '({\n' + '  foo,\n' + '  bar\n' + '});',
    ]);
  });
  it('(compile \'(js/obj "foo" (js/obj "bar" "baz")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'bar', 'baz']],
        ],
      ],
      '({\n' + '  foo: {\n' + "    bar: 'baz'\n" + '  }\n' + '});',
    ]);
  });
  it('(compile \'(js/obj "foo" (js/obj "foo" "foo") "bar" (js/obj "bar" "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/obj'),
            'foo',
            [Symbol.for('js/obj'), 'foo', 'foo'],
            'bar',
            [Symbol.for('js/obj'), 'bar', 'bar'],
          ],
        ],
      ],
      '({\n' +
        '  foo: {\n' +
        "    foo: 'foo'\n" +
        '  },\n' +
        '  bar: {\n' +
        "    bar: 'bar'\n" +
        '  }\n' +
        '});',
    ]);
  });
  it('(compile \'(js/obj "foo" (js/obj) "bar" (js/obj "bar" "bar") "baz" (js/obj "baz" "baz")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/obj'),
            'foo',
            [Symbol.for('js/obj')],
            'bar',
            [Symbol.for('js/obj'), 'bar', 'bar'],
            'baz',
            [Symbol.for('js/obj'), 'baz', 'baz'],
          ],
        ],
      ],
      '({\n' +
        '  foo: {},\n' +
        '  bar: {\n' +
        "    bar: 'bar'\n" +
        '  },\n' +
        '  baz: {\n' +
        "    baz: 'baz'\n" +
        '  }\n' +
        '});',
    ]);
  });
  it("(compile '(js/obj) :as 'expression)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj')]],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('expression')],
      ],
      '{}',
    ]);
  });
  it('(compile \'(js/obj "foo" "bar") :as \'expression)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 'bar']],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('expression')],
      ],
      '{\n' + "  foo: 'bar'\n" + '}',
    ]);
  });
  it('(compile \'(js/obj "foo" 1 "bar" 2) :as \'expression)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('expression')],
      ],
      '{\n' + '  foo: 1,\n' + '  bar: 2\n' + '}',
    ]);
  });
  it("(compile '(js/obj) :as 'return)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj')]],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('return')],
      ],
      'return {};',
    ]);
  });
  it('(compile \'(js/obj "foo" "bar") :as \'return)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 'bar']],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('return')],
      ],
      'return {\n' + "  foo: 'bar'\n" + '};',
    ]);
  });
  return it('(compile \'(js/obj "foo" 1 "bar" 2) :as \'return)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]],
        Symbol.for(':as'),
        [Symbol.for('quote'), Symbol.for('return')],
      ],
      'return {\n' + '  foo: 1,\n' + '  bar: 2\n' + '};',
    ]);
  });
});

describe('js/obj?', function (): any {
  return it("(compile '(js/obj? x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/obj?'), Symbol.for('x')]],
      ],
      "(x !== null) && (typeof x === 'object');",
    ]);
  });
});

describe('js/obj-append', function (): any {
  return it('(compile \'(js/obj-append obj (js/obj "foo" "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/obj-append'),
            Symbol.for('obj'),
            [Symbol.for('js/obj'), 'foo', 'bar'],
          ],
        ],
      ],
      '({\n' + '  ...obj,\n' + "  foo: 'bar'\n" + '});',
    ]);
  });
});

describe('js/keys', function (): any {
  it('(js/keys (js/obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/keys'), [Symbol.for('js/obj')]],
      [Symbol.for('quote'), []],
    ]);
  });
  it('(js/keys (js/obj "foo" "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/keys'), [Symbol.for('js/obj'), 'foo', 'bar']],
      [Symbol.for('quote'), ['foo']],
    ]);
  });
  it('(js/keys (js/obj "foo" "bar" "baz" "quux"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('js/keys'),
        [Symbol.for('js/obj'), 'foo', 'bar', 'baz', 'quux'],
      ],
      [Symbol.for('quote'), ['foo', 'baz']],
    ]);
  });
  return it("(compile '(js/keys x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/keys'), Symbol.for('x')]],
      ],
      'Object.keys(x);',
    ]);
  });
});

describe('js/in', function (): any {
  it('(let ((obj (js/obj "foo" "bar"))) (js/in "foo" obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]],
        [Symbol.for('js/in'), 'foo', Symbol.for('obj')],
      ],
      true,
    ]);
  });
  return it('(compile \'(js/in "foo" obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/in'), 'foo', Symbol.for('obj')]],
      ],
      "'foo' in obj;",
    ]);
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

describe('plist->object', function (): any {
  it("(plist->object '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('plist->object'), [Symbol.for('quote'), []]],
      [Symbol.for('js/obj')],
    ]);
  });
  it("(plist->object '(foo bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('plist->object'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      [Symbol.for('js/obj'), 'foo', [Symbol.for('quote'), Symbol.for('bar')]],
    ]);
  });
  return it("(plist->object '(foo bar baz quux))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('plist->object'),
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
        Symbol.for('js/obj'),
        'foo',
        [Symbol.for('quote'), Symbol.for('bar')],
        'baz',
        [Symbol.for('quote'), Symbol.for('quux')],
      ],
    ]);
  });
});

describe('module', function (): any {
  return it('(module foo bar (+ 1 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('module'),
        Symbol.for('foo'),
        Symbol.for('bar'),
        [Symbol.for('+'), 1, 1],
      ],
      2,
    ]);
  });
});

describe('js/try', function (): any {
  it('(js/try (/ 1 2) (catch e (display "there was an error")) (finally (display "finally")))', function (): any {
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
  it('(js/try (/ 1 3) (/ 1 2) (catch e (display "there was an error")) (finally (display "finally")))', function (): any {
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
  return it('(compile \'(js/try (/ 1 2) (catch e (display "there was an error")) (finally (display "finally"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
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
        ],
      ],
      'try {\n' +
        '  1 / 2;\n' +
        '} catch (e) {\n' +
        "  console.log('there was an error');\n" +
        '} finally {\n' +
        "  console.log('finally');\n" +
        '}',
    ]);
  });
});

describe('clj/try', function (): any {
  it('(clj/try (/ 1 2) (catch Exception e "there was an error") (finally (display "finally")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('clj/try'),
        [Symbol.for('/'), 1, 2],
        [
          Symbol.for('catch'),
          Symbol.for('Exception'),
          Symbol.for('e'),
          'there was an error',
        ],
        [Symbol.for('finally'), [Symbol.for('display'), 'finally']],
      ],
      0.5,
    ]);
  });
  it('(clj/try (/ 1 3) (/ 1 2) (catch Exception e "there was an error") (finally (display "finally")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('clj/try'),
        [Symbol.for('/'), 1, 3],
        [Symbol.for('/'), 1, 2],
        [
          Symbol.for('catch'),
          Symbol.for('Exception'),
          Symbol.for('e'),
          'there was an error',
        ],
        [Symbol.for('finally'), [Symbol.for('display'), 'finally']],
      ],
      0.5,
    ]);
  });
  it('(compile \'(clj/try (/ 1 2) (catch Object e (display "there was an error")) (finally (display "finally"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('clj/try'),
            [Symbol.for('/'), 1, 2],
            [
              Symbol.for('catch'),
              Symbol.for('Object'),
              Symbol.for('e'),
              [Symbol.for('display'), 'there was an error'],
            ],
            [Symbol.for('finally'), [Symbol.for('display'), 'finally']],
          ],
        ],
      ],
      'try {\n' +
        '  1 / 2;\n' +
        '} catch (e) {\n' +
        "  console.log('there was an error');\n" +
        '} finally {\n' +
        "  console.log('finally');\n" +
        '}',
    ]);
  });
  return it('(compile \'(clj/try (/ 1 2) (catch Exception e (display "there was an error")) (finally (display "finally"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('clj/try'),
            [Symbol.for('/'), 1, 2],
            [
              Symbol.for('catch'),
              Symbol.for('Exception'),
              Symbol.for('e'),
              [Symbol.for('display'), 'there was an error'],
            ],
            [Symbol.for('finally'), [Symbol.for('display'), 'finally']],
          ],
        ],
      ],
      'try {\n' +
        '  1 / 2;\n' +
        '} catch (e) {\n' +
        '  if (e instanceof Exception) {\n' +
        "    console.log('there was an error');\n" +
        '  } else {\n' +
        '    throw e;\n' +
        '  }\n' +
        '} finally {\n' +
        "  console.log('finally');\n" +
        '}',
    ]);
  });
});

describe('unwind-protect', function (): any {
  it('(unwind-protect 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('unwind-protect'), 1, 2, 3],
      1,
    ]);
  });
  return it("(compile '(unwind-protect (foo) (bar)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('unwind-protect'),
            [Symbol.for('foo')],
            [Symbol.for('bar')],
          ],
        ],
      ],
      'try {\n' + '  foo();\n' + '} finally {\n' + '  bar();\n' + '}',
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
  return it('(try ... (+ 5 (call/cc (lambda (x) (error ...)))) ...)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('result'), 0]],
        [
          Symbol.for('try'),
          [
            Symbol.for('set!'),
            Symbol.for('result'),
            [
              Symbol.for('+'),
              5,
              [
                Symbol.for('call/cc'),
                [
                  Symbol.for('lambda'),
                  [Symbol.for('x')],
                  [Symbol.for('error'), 'error'],
                ],
              ],
            ],
          ],
          [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e')],
        ],
        Symbol.for('result'),
      ],
      0,
    ]);
  });
});

describe('define-values', function (): any {
  it('((lambda () (define-values (x y) (values 1 2))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-values'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('values'), 1, 2],
          ],
        ],
      ],
      undefined,
    ]);
  });
  it('((lambda () (define-values (x y) (values 1 2)) (list x y)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-values'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('values'), 1, 2],
          ],
          [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  return it('((lambda () (define-values (x y) (values 1 2)) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-values'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('values'), 1, 2],
          ],
          Symbol.for('x'),
        ],
      ],
      1,
    ]);
  });
});

describe('set!-values', function (): any {
  it('(let (x y) (set!-values (x y) (values 1 2)) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('x'), Symbol.for('y')],
        [
          Symbol.for('set!-values'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('values'), 1, 2],
        ],
        Symbol.for('x'),
      ],
      1,
    ]);
  });
  it('(let (x y) (set!-values (x y) (values 1 2)) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [Symbol.for('x'), Symbol.for('y')],
        [
          Symbol.for('set!-values'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('values'), 1, 2],
        ],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  return it("(compile '(set!-values (x y) (values 1 2)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('set!-values'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('values'), 1, 2],
          ],
        ],
      ],
      '[x, y] = [1, 2];',
    ]);
  });
});

describe('let-values', function (): any {
  it('(let-values (((x y) (values 1 2))) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let-values'),
        [
          [
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('values'), 1, 2],
          ],
        ],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  it('(let-values (((x . y) (values 1 2))) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let-values'),
        [
          [
            [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')],
            [Symbol.for('values'), 1, 2],
          ],
        ],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [Symbol.for('quote'), [1, [2]]],
    ]);
  });
  return it("(compile '(let-values (((x y) (values 1 2))) (define z (+ x y))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('let-values'),
            [
              [
                [Symbol.for('x'), Symbol.for('y')],
                [Symbol.for('values'), 1, 2],
              ],
            ],
            [
              Symbol.for('define'),
              Symbol.for('z'),
              [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
            ],
          ],
        ],
      ],
      'let [x, y] = [1, 2];\n' + '\n' + 'let z = x + y;',
    ]);
  });
});

describe('let*-values', function (): any {
  it('(let*-values (((x y) (values 1 2)) ((w z) (values 3 4))) (list x y w z))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let*-values'),
        [
          [
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('values'), 1, 2],
          ],
          [
            [Symbol.for('w'), Symbol.for('z')],
            [Symbol.for('values'), 3, 4],
          ],
        ],
        [
          Symbol.for('list'),
          Symbol.for('x'),
          Symbol.for('y'),
          Symbol.for('w'),
          Symbol.for('z'),
        ],
      ],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  return it("(compile '(let*-values (((x y) (values 1 2)) ((w z) (values 3 4))) (define z (+ x y w z))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('let*-values'),
            [
              [
                [Symbol.for('x'), Symbol.for('y')],
                [Symbol.for('values'), 1, 2],
              ],
              [
                [Symbol.for('w'), Symbol.for('z')],
                [Symbol.for('values'), 3, 4],
              ],
            ],
            [
              Symbol.for('define'),
              Symbol.for('z'),
              [
                Symbol.for('+'),
                Symbol.for('x'),
                Symbol.for('y'),
                Symbol.for('w'),
                Symbol.for('z'),
              ],
            ],
          ],
        ],
      ],
      'let [x, y] = [1, 2];\n' +
        '\n' +
        'let [w, z] = [3, 4];\n' +
        '\n' +
        'let z = x + y + w + z;',
    ]);
  });
});

describe('define-fields', function (): any {
  it('((lambda () (define-fields (x) (js/obj "x" 1)) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-fields'),
            [Symbol.for('x')],
            [Symbol.for('js/obj'), 'x', 1],
          ],
          Symbol.for('x'),
        ],
      ],
      1,
    ]);
  });
  it('((lambda () (define-fields (foo) (js/obj "foo" "bar")) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-fields'),
            [Symbol.for('foo')],
            [Symbol.for('js/obj'), 'foo', 'bar'],
          ],
          Symbol.for('foo'),
        ],
      ],
      'bar',
    ]);
  });
  it('((lambda () (define-fields ((foo bar)) (js/obj "foo" "bar")) bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-fields'),
            [[Symbol.for('foo'), Symbol.for('bar')]],
            [Symbol.for('js/obj'), 'foo', 'bar'],
          ],
          Symbol.for('bar'),
        ],
      ],
      'bar',
    ]);
  });
  it('(compile \'(define-fields (foo) (js/obj "foo" "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('define-fields'),
            [Symbol.for('foo')],
            [Symbol.for('js/obj'), 'foo', 'bar'],
          ],
        ],
      ],
      'let {foo} = {\n' + "  foo: 'bar'\n" + '};',
    ]);
  });
  return it('(compile \'(define-fields ((foo bar)) (js/obj "foo" "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('define-fields'),
            [[Symbol.for('foo'), Symbol.for('bar')]],
            [Symbol.for('js/obj'), 'foo', 'bar'],
          ],
        ],
      ],
      'let {foo: bar} = {\n' + "  foo: 'bar'\n" + '};',
    ]);
  });
});

describe('set!-fields', function (): any {
  it('((lambda () (let (x) (set!-fields (x) (js/obj "x" 1)) x)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('let'),
            [Symbol.for('x')],
            [
              Symbol.for('set!-fields'),
              [Symbol.for('x')],
              [Symbol.for('js/obj'), 'x', 1],
            ],
            Symbol.for('x'),
          ],
        ],
      ],
      1,
    ]);
  });
  return it('(compile \'(set!-fields (x) (js/obj "x" 1)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('set!-fields'),
            [Symbol.for('x')],
            [Symbol.for('js/obj'), 'x', 1],
          ],
        ],
      ],
      '({x} = {\n' + '  x: 1\n' + '});',
    ]);
  });
});

describe('destructuring-bind', function (): any {
  it("(destructuring-bind (x y) '(1 2) (list x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('destructuring-bind'),
        [Symbol.for('x'), Symbol.for('y')],
        [Symbol.for('quote'), [1, 2]],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  it("(destructuring-bind (x . y) '(1 2) (list x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('destructuring-bind'),
        [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')],
        [Symbol.for('quote'), [1, 2]],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [Symbol.for('quote'), [1, [2]]],
    ]);
  });
  it("(compile '(destructuring-bind (x y) '(1 2) (list x y)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('destructuring-bind'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('quote'), [1, 2]],
            [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
          ],
        ],
      ],
      'let [x, y] = [1, 2];\n' + '\n' + '[x, y];',
    ]);
  });
  return it("(compile '(destructuring-bind (x . y) '(1 2) (list x y)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('destructuring-bind'),
            [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')],
            [Symbol.for('quote'), [1, 2]],
            [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
          ],
        ],
      ],
      'let [x, ...y] = [1, 2];\n' + '\n' + '[x, y];',
    ]);
  });
});

describe('multiple-values-bind', function (): any {
  it('(multiple-values-bind (x y) (values 1 2) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('multiple-values-bind'),
        [Symbol.for('x'), Symbol.for('y')],
        [Symbol.for('values'), 1, 2],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  return it("(compile '(multiple-values-bind (x y) (values 1 2) (list x y)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('multiple-values-bind'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('values'), 1, 2],
            [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
          ],
        ],
      ],
      'let [x, y] = [1, 2];\n' + '\n' + '[x, y];',
    ]);
  });
});

describe('hash', function (): any {
  it('(hash)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('hash')],
      [Symbol.for('new'), Symbol.for('Map')],
    ]);
  });
  it('(hash \'(("foo" . "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('hash'),
        [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
      ],
      [
        Symbol.for('new'),
        Symbol.for('Map'),
        [Symbol.for('quote'), [['foo', 'bar']]],
      ],
    ]);
  });
  it("(compile '(hash))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash')]]],
      'new Map();',
    ]);
  });
  return it('(compile \'(hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('hash'),
            [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
          ],
        ],
      ],
      "new Map([['foo', 'bar']]);",
    ]);
  });
});

describe('make-hash', function (): any {
  it('(make-hash)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('make-hash')],
      [Symbol.for('new'), Symbol.for('Map')],
    ]);
  });
  it('(make-hash \'(("foo" . "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('make-hash'),
        [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
      ],
      [
        Symbol.for('new'),
        Symbol.for('Map'),
        [Symbol.for('quote'), [['foo', 'bar']]],
      ],
    ]);
  });
  it("(compile '(make-hash))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('make-hash')]]],
      'new Map();',
    ]);
  });
  return it('(compile \'(make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('make-hash'),
            [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
          ],
        ],
      ],
      "new Map([['foo', 'bar']]);",
    ]);
  });
});

describe('hash?', function (): any {
  it('(hash? (make-hash))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('hash?'), [Symbol.for('make-hash')]],
      true,
    ]);
  });
  it('(hash? 0)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('hash?'), 0],
      false,
    ]);
  });
  return it("(compile '(hash? x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('hash?'), Symbol.for('x')]],
      ],
      'x instanceof Map;',
    ]);
  });
});

describe('hash-clear', function (): any {
  return it('(hash-clear (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('hash-clear'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      [Symbol.for('new'), Symbol.for('Map')],
    ]);
  });
});

describe('hash-clear!', function (): any {
  it('(let ((ht (make-hash \'(("foo" . "bar"))))) (hash-clear! ht) ht)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('ht'),
            [
              Symbol.for('make-hash'),
              [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
            ],
          ],
        ],
        [Symbol.for('hash-clear!'), Symbol.for('ht')],
        Symbol.for('ht'),
      ],
      [Symbol.for('new'), Symbol.for('Map')],
    ]);
  });
  return it("(compile '(hash-clear! x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('hash-clear!'), Symbol.for('x')]],
      ],
      'x.clear();',
    ]);
  });
});

describe('hash-copy', function (): any {
  it('(hash-copy (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('hash-copy'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      [
        Symbol.for('new'),
        Symbol.for('Map'),
        [Symbol.for('quote'), [['foo', 'bar']]],
      ],
    ]);
  });
  return it("(compile '(hash-copy x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('hash-copy'), Symbol.for('x')]],
      ],
      'new Map(x);',
    ]);
  });
});

describe('hash-keys', function (): any {
  it('(hash-keys (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('hash-keys'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      [Symbol.for('quote'), ['foo']],
    ]);
  });
  return it("(compile '(hash-keys x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('hash-keys'), Symbol.for('x')]],
      ],
      '[...x.keys()];',
    ]);
  });
});

describe('hash-values', function (): any {
  it('(hash-values (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('hash-values'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      [Symbol.for('quote'), ['bar']],
    ]);
  });
  return it("(compile '(hash-values x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('hash-values'), Symbol.for('x')]],
      ],
      '[...x.values()];',
    ]);
  });
});

describe('hash->list', function (): any {
  return it('(hash->list (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('hash->list'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
    ]);
  });
});

describe('hash-set', function (): any {
  return it('(hash-set (make-hash) "foo" "bar")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('hash-set'), [Symbol.for('make-hash')], 'foo', 'bar'],
      [
        Symbol.for('new'),
        Symbol.for('Map'),
        [Symbol.for('quote'), [['foo', 'bar']]],
      ],
    ]);
  });
});

describe('hash-set!', function (): any {
  it('(let ((ht (make-hash))) (hash-set! ht "foo" "bar") ht)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('ht'), [Symbol.for('make-hash')]]],
        [Symbol.for('hash-set!'), Symbol.for('ht'), 'foo', 'bar'],
        Symbol.for('ht'),
      ],
      [
        Symbol.for('new'),
        Symbol.for('Map'),
        [Symbol.for('quote'), [['foo', 'bar']]],
      ],
    ]);
  });
  return it("(compile '(hash-set! ht key val))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('hash-set!'),
            Symbol.for('ht'),
            Symbol.for('key'),
            Symbol.for('val'),
          ],
        ],
      ],
      'ht.set(key, val);',
    ]);
  });
});

describe('hash-ref', function (): any {
  it('(hash-ref (make-hash \'(("foo" . "bar"))) "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('hash-ref'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
        'foo',
      ],
      'bar',
    ]);
  });
  it('(hash-ref (make-hash) "quux" #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('hash-ref'), [Symbol.for('make-hash')], 'quux', false],
      false,
    ]);
  });
  return it('(compile \'(hash-ref ht "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('hash-ref'), Symbol.for('ht'), 'foo'],
        ],
      ],
      "ht.get('foo');",
    ]);
  });
});

describe('hash-has-key?', function (): any {
  it('(hash-has-key? (make-hash \'(("foo" . "bar"))) "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('hash-has-key?'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
        'foo',
      ],
      true,
    ]);
  });
  it('(hash-has-key? (make-hash) "quux")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('hash-has-key?'), [Symbol.for('make-hash')], 'quux'],
      false,
    ]);
  });
  return it('(compile \'(hash-has-key? ht "quux"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('hash-has-key?'), Symbol.for('ht'), 'quux'],
        ],
      ],
      "ht.has('quux');",
    ]);
  });
});

describe('Map', function (): any {
  it('(new Map)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('new'), Symbol.for('Map')],
      [Symbol.for('new'), Symbol.for('Map')],
    ]);
  });
  return it("(~> (new Map '((1 2))) (send _ entries) (send Array from _))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('~>'),
        [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('quote'), [[1, 2]]]],
        [Symbol.for('send'), Symbol.for('_'), Symbol.for('entries')],
        [
          Symbol.for('send'),
          Symbol.for('Array'),
          Symbol.for('from'),
          Symbol.for('_'),
        ],
      ],
      [Symbol.for('quote'), [[1, 2]]],
    ]);
  });
});

describe('+', function (): any {
  it('(+)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+')],
      0,
    ]);
  });
  it('(+ 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+'), 1],
      1,
    ]);
  });
  it('(+ 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+'), 1, 2],
      3,
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
  it('(+ 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+'), 1, 2, 3],
      6,
    ]);
  });
  it('(+ 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+'), 1, 2, 4],
      7,
    ]);
  });
  it('(+ (+ 1 1) (+ 1 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('+'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 1, 1]],
      4,
    ]);
  });
  it('(let ((x 2)) (+ x x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('x'), 2]],
        [Symbol.for('+'), Symbol.for('x'), Symbol.for('x')],
      ],
      4,
    ]);
  });
  it("(apply + '(1 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('apply'), Symbol.for('+'), [Symbol.for('quote'), [1, 2]]],
      3,
    ]);
  });
  it("(compile '(+ 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('+'), 1]]],
      '1;',
    ]);
  });
  it("(compile '(+ 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('+'), 1, 1]]],
      '1 + 1;',
    ]);
  });
  return it("(compile '(+ 1 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('+'), 1, 1, 1]],
      ],
      '1 + 1 + 1;',
    ]);
  });
});

describe('js/+', function (): any {
  it('(js/+)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/+')],
      0,
    ]);
  });
  it('(js/+ 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/+'), 1],
      1,
    ]);
  });
  it('(js/+ 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/+'), 1, 2],
      3,
    ]);
  });
  it('(js/+ 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/+'), 2, 2],
      4,
    ]);
  });
  it('(js/+ 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/+'), 1, 2, 3],
      6,
    ]);
  });
  it('(js/+ 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/+'), 1, 2, 4],
      7,
    ]);
  });
  it('(js/+ (js/+ 1 1) (js/+ 1 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('js/+'),
        [Symbol.for('js/+'), 1, 1],
        [Symbol.for('js/+'), 1, 1],
      ],
      4,
    ]);
  });
  it('(let ((x 2)) (js/+ x x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('x'), 2]],
        [Symbol.for('js/+'), Symbol.for('x'), Symbol.for('x')],
      ],
      4,
    ]);
  });
  it('(js/+ 1 "")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/+'), 1, ''],
      '1',
    ]);
  });
  it("(compile '(js/+ 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/+'), 1, 1]],
      ],
      '1 + 1;',
    ]);
  });
  return it("(compile '(js/+ 1 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/+'), 1, 1, 1]],
      ],
      '1 + 1 + 1;',
    ]);
  });
});

describe('-', function (): any {
  it('(-)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('-')],
      0,
    ]);
  });
  it('(- 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('-'), 1],
      -1,
    ]);
  });
  it('(- 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('-'), 1, 2],
      -1,
    ]);
  });
  it('(- 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('-'), 1, 2, 3],
      -4,
    ]);
  });
  it('(- 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('-'), 1, 2, 4],
      -5,
    ]);
  });
  it("(compile '(- 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('-'), 1]]],
      '-1;',
    ]);
  });
  it("(compile '(- 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('-'), 1, 1]]],
      '1 - 1;',
    ]);
  });
  return it("(compile '(- 1 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('-'), 1, 1, 1]],
      ],
      '1 - 1 - 1;',
    ]);
  });
});

describe('js/-', function (): any {
  it('(js/-)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/-')],
      0,
    ]);
  });
  it('(js/- 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/-'), 1],
      -1,
    ]);
  });
  it('(js/- 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/-'), 1, 2],
      -1,
    ]);
  });
  it('(js/- 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/-'), 1, 2, 3],
      -4,
    ]);
  });
  it('(js/- 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/-'), 1, 2, 4],
      -5,
    ]);
  });
  it("(compile '(js/- 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/-'), 1]]],
      '-1;',
    ]);
  });
  it("(compile '(js/- 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/-'), 1, 1]],
      ],
      '1 - 1;',
    ]);
  });
  return it("(compile '(js/- 1 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/-'), 1, 1, 1]],
      ],
      '1 - 1 - 1;',
    ]);
  });
});

describe('*', function (): any {
  it('(*)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('*')],
      1,
    ]);
  });
  it('(* 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('*'), 1],
      1,
    ]);
  });
  it('(* 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('*'), 1, 2],
      2,
    ]);
  });
  it('(* 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('*'), 1, 2, 3],
      6,
    ]);
  });
  it('(* 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('*'), 1, 2, 4],
      8,
    ]);
  });
  it("(compile '(* 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('*'), 1, 1]]],
      '1 * 1;',
    ]);
  });
  return it("(compile '(* 1 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('*'), 1, 1, 1]],
      ],
      '1 * 1 * 1;',
    ]);
  });
});

describe('js/*', function (): any {
  it('(js/*)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/*')],
      1,
    ]);
  });
  it('(js/* 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/*'), 1],
      1,
    ]);
  });
  it('(js/* 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/*'), 1, 2],
      2,
    ]);
  });
  it('(js/* 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/*'), 1, 2, 3],
      6,
    ]);
  });
  it('(js/* 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/*'), 1, 2, 4],
      8,
    ]);
  });
  it("(compile '(js/* 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/*'), 1, 1]],
      ],
      '1 * 1;',
    ]);
  });
  return it("(compile '(js/* 1 1 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/*'), 1, 1, 1]],
      ],
      '1 * 1 * 1;',
    ]);
  });
});

describe('/', function (): any {
  it('(/)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('/')],
      undefined,
    ]);
  });
  it('(/ 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('/'), 1],
      1,
    ]);
  });
  it('(/ 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('/'), 1, 2],
      0.5,
    ]);
  });
  it('(/ 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('/'), 1, 2, 3],
      [Symbol.for('/'), 1, 2, 3],
    ]);
  });
  it('(/ 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('/'), 1, 2, 4],
      0.125,
    ]);
  });
  it("(compile '(/ 1 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('/'), 1, 2]]],
      '1 / 2;',
    ]);
  });
  return it("(compile '(/ 1 2 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('/'), 1, 2, 4]],
      ],
      '1 / 2 / 4;',
    ]);
  });
});

describe('js//', function (): any {
  it('(js//)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js//')],
      undefined,
    ]);
  });
  it('(js// 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js//'), 1],
      1,
    ]);
  });
  it('(js// 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js//'), 1, 2],
      0.5,
    ]);
  });
  it('(js// 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js//'), 1, 2, 3],
      [Symbol.for('js//'), 1, 2, 3],
    ]);
  });
  it('(/ 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('/'), 1, 2, 4],
      0.125,
    ]);
  });
  it("(compile '(js// 1 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js//'), 1, 2]],
      ],
      '1 / 2;',
    ]);
  });
  return it("(compile '(js// 1 2 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js//'), 1, 2, 4]],
      ],
      '1 / 2 / 4;',
    ]);
  });
});

describe('<', function (): any {
  it('(< 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<'), 1],
      true,
    ]);
  });
  it('(< 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<'), 1, 2],
      true,
    ]);
  });
  it('(< 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<'), 2, 1],
      false,
    ]);
  });
  it('(< 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<'), 1, 2, 3],
      true,
    ]);
  });
  it('(< 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<'), 2, 1, 3],
      false,
    ]);
  });
  it('(< 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<'), 1, 3, 2],
      false,
    ]);
  });
  it('(funcall < 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<'), 1, 2],
      true,
    ]);
  });
  it('(funcall < 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<'), 2, 1],
      false,
    ]);
  });
  it('(funcall < 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<'), 1, 2, 3],
      true,
    ]);
  });
  it('(funcall < 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<'), 2, 1, 3],
      false,
    ]);
  });
  it('(funcall < 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<'), 1, 3, 2],
      false,
    ]);
  });
  it("(compile '(< x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('<'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x < y;',
    ]);
  });
  return it("(compile '(< x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('<'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        ],
      ],
      '(x < y) && (y < z);',
    ]);
  });
});

describe('js/<', function (): any {
  it('(js/< 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<'), 1, 2],
      true,
    ]);
  });
  it('(js/< 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<'), 2, 1],
      false,
    ]);
  });
  it('(js/< 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<'), 1, 2, 3],
      true,
    ]);
  });
  it('(js/< 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<'), 2, 1, 3],
      false,
    ]);
  });
  it('(js/< 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<'), 1, 3, 2],
      false,
    ]);
  });
  it('(funcall js/< 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<'), 1, 2],
      true,
    ]);
  });
  it('(funcall js/< 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<'), 2, 1],
      false,
    ]);
  });
  it('(funcall js/< 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<'), 1, 2, 3],
      true,
    ]);
  });
  it('(funcall js/< 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<'), 2, 1, 3],
      false,
    ]);
  });
  it('(funcall js/< 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<'), 1, 3, 2],
      false,
    ]);
  });
  it("(compile '(js/< x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  return it("(compile '(js/< x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/<'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      '(x < y) && (y < z);',
    ]);
  });
});

describe('<=', function (): any {
  it('(<= 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<='), 1, 2],
      true,
    ]);
  });
  it('(<= 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<='), 2, 1],
      false,
    ]);
  });
  it('(<= 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<='), 1, 2, 3],
      true,
    ]);
  });
  it('(<= 1 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<='), 1, 1, 2],
      true,
    ]);
  });
  it('(<= 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<='), 2, 1, 3],
      false,
    ]);
  });
  it('(<= 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('<='), 1, 3, 2],
      false,
    ]);
  });
  it('(funcall <= 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<='), 1, 2],
      true,
    ]);
  });
  it('(funcall <= 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<='), 2, 1],
      false,
    ]);
  });
  it('(funcall <= 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<='), 1, 2, 3],
      true,
    ]);
  });
  it('(funcall <= 1 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<='), 1, 1, 2],
      true,
    ]);
  });
  it('(funcall <= 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<='), 2, 1, 3],
      false,
    ]);
  });
  it('(funcall <= 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('<='), 1, 3, 2],
      false,
    ]);
  });
  it("(compile '(<= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('<='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x <= y;',
    ]);
  });
  return it("(compile '(<= x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('<='), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        ],
      ],
      '(x <= y) && (y <= z);',
    ]);
  });
});

describe('js/<=', function (): any {
  it('(js/<= 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<='), 1, 2],
      true,
    ]);
  });
  it('(js/<= 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<='), 2, 1],
      false,
    ]);
  });
  it('(js/<= 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<='), 1, 2, 3],
      true,
    ]);
  });
  it('(js/<= 1 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<='), 1, 1, 2],
      true,
    ]);
  });
  it('(js/<= 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<='), 2, 1, 3],
      false,
    ]);
  });
  it('(js/<= 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/<='), 1, 3, 2],
      false,
    ]);
  });
  it('(funcall js/<= 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<='), 1, 2],
      true,
    ]);
  });
  it('(funcall js/<= 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<='), 2, 1],
      false,
    ]);
  });
  it('(funcall js/<= 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<='), 1, 2, 3],
      true,
    ]);
  });
  it('(funcall js/<= 1 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<='), 1, 1, 2],
      true,
    ]);
  });
  it('(funcall js/<= 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<='), 2, 1, 3],
      false,
    ]);
  });
  it('(funcall js/<= 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/<='), 1, 3, 2],
      false,
    ]);
  });
  it("(compile '(js/<= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/<='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x <= y;',
    ]);
  });
  return it("(compile '(js/<= x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/<='),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      '(x <= y) && (y <= z);',
    ]);
  });
});

describe('>', function (): any {
  it('(> 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>'), 1],
      true,
    ]);
  });
  it('(> 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>'), 2, 1],
      true,
    ]);
  });
  it('(> 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>'), 1, 2],
      false,
    ]);
  });
  it('(> 3 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>'), 3, 2, 1],
      true,
    ]);
  });
  it('(> 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>'), 1, 2, 3],
      false,
    ]);
  });
  it('(> 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>'), 2, 1, 3],
      false,
    ]);
  });
  it('(> 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>'), 1, 3, 2],
      false,
    ]);
  });
  it('(funcall > 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>'), 2, 1],
      true,
    ]);
  });
  it('(funcall > 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>'), 1, 2],
      false,
    ]);
  });
  it('(funcall > 3 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>'), 3, 2, 1],
      true,
    ]);
  });
  it('(funcall > 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>'), 1, 2, 3],
      false,
    ]);
  });
  it('(funcall > 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>'), 2, 1, 3],
      false,
    ]);
  });
  it('(funcall > 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>'), 1, 3, 2],
      false,
    ]);
  });
  it("(compile '(> x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('>'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x > y;',
    ]);
  });
  return it("(compile '(> x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('>'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        ],
      ],
      '(x > y) && (y > z);',
    ]);
  });
});

describe('js/>', function (): any {
  it('(js/> 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>'), 2, 1],
      true,
    ]);
  });
  it('(js/> 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>'), 1, 2],
      false,
    ]);
  });
  it('(js/> 3 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>'), 3, 2, 1],
      true,
    ]);
  });
  it('(js/> 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>'), 1, 2, 3],
      false,
    ]);
  });
  it('(js/> 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>'), 2, 1, 3],
      false,
    ]);
  });
  it('(js/> 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>'), 1, 3, 2],
      false,
    ]);
  });
  it('(funcall js/> 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>'), 2, 1],
      true,
    ]);
  });
  it('(funcall js/> 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>'), 1, 2],
      false,
    ]);
  });
  it('(funcall js/> 3 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>'), 3, 2, 1],
      true,
    ]);
  });
  it('(funcall js/> 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>'), 1, 2, 3],
      false,
    ]);
  });
  it('(funcall js/> 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>'), 2, 1, 3],
      false,
    ]);
  });
  it('(funcall js/> 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>'), 1, 3, 2],
      false,
    ]);
  });
  it("(compile '(js/> x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  return it("(compile '(js/> x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/>'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      '(x > y) && (y > z);',
    ]);
  });
});

describe('>=', function (): any {
  it('(>= 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>='), 1],
      true,
    ]);
  });
  it('(>= 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>='), 2, 1],
      true,
    ]);
  });
  it('(>= 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>='), 2, 2],
      true,
    ]);
  });
  it('(>= 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>='), 1, 2],
      false,
    ]);
  });
  it('(>= 3 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>='), 3, 2, 1],
      true,
    ]);
  });
  it('(>= 3 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>='), 3, 2, 2],
      true,
    ]);
  });
  it('(>= 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>='), 1, 2, 3],
      false,
    ]);
  });
  it('(>= 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>='), 2, 1, 3],
      false,
    ]);
  });
  it('(>= 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('>='), 1, 3, 2],
      false,
    ]);
  });
  it('(funcall >= 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>='), 2, 1],
      true,
    ]);
  });
  it('(funcall >= 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>='), 2, 2],
      true,
    ]);
  });
  it('(funcall >= 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>='), 1, 2],
      false,
    ]);
  });
  it('(funcall >= 3 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>='), 3, 2, 1],
      true,
    ]);
  });
  it('(funcall >= 3 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>='), 3, 2, 2],
      true,
    ]);
  });
  it('(funcall >= 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>='), 1, 2, 3],
      false,
    ]);
  });
  it('(funcall >= 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>='), 2, 1, 3],
      false,
    ]);
  });
  it('(funcall >= 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('>='), 1, 3, 2],
      false,
    ]);
  });
  it("(compile '(>= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('>='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x >= y;',
    ]);
  });
  return it("(compile '(>= x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('>='), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        ],
      ],
      '(x >= y) && (y >= z);',
    ]);
  });
});

describe('js/>=', function (): any {
  it('(js/>= 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>='), 2, 1],
      true,
    ]);
  });
  it('(js/>= 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>='), 2, 2],
      true,
    ]);
  });
  it('(js/>= 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>='), 1, 2],
      false,
    ]);
  });
  it('(js/>= 3 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>='), 3, 2, 1],
      true,
    ]);
  });
  it('(js/>= 3 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>='), 3, 2, 2],
      true,
    ]);
  });
  it('(js/>= 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>='), 1, 2, 3],
      false,
    ]);
  });
  it('(js/>= 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>='), 2, 1, 3],
      false,
    ]);
  });
  it('(js/>= 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('js/>='), 1, 3, 2],
      false,
    ]);
  });
  it('(funcall js/>= 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>='), 2, 1],
      true,
    ]);
  });
  it('(funcall js/>= 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>='), 2, 2],
      true,
    ]);
  });
  it('(funcall js/>= 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>='), 1, 2],
      false,
    ]);
  });
  it('(funcall js/>= 3 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>='), 3, 2, 1],
      true,
    ]);
  });
  it('(funcall js/>= 3 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>='), 3, 2, 2],
      true,
    ]);
  });
  it('(funcall js/>= 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>='), 1, 2, 3],
      false,
    ]);
  });
  it('(funcall js/>= 2 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>='), 2, 1, 3],
      false,
    ]);
  });
  it('(funcall js/>= 1 3 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('funcall'), Symbol.for('js/>='), 1, 3, 2],
      false,
    ]);
  });
  it("(compile '(js/>= x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('js/>='), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'x >= y;',
    ]);
  });
  it("(compile '(js/>= x y z))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('js/>='),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
        ],
      ],
      '(x >= y) && (y >= z);',
    ]);
  });
  return it("(compile '(js/% x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
});

describe('abs', function (): any {
  it('(abs 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('abs'), 1],
      1,
    ]);
  });
  it('(abs -1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('abs'), -1],
      1,
    ]);
  });
  it("(compile '(abs x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('abs'), Symbol.for('x')]],
      ],
      'Math.abs(x);',
    ]);
  });
  return it("(compile '(js/abs x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('js/abs'), Symbol.for('x')]],
      ],
      'Math.abs(x);',
    ]);
  });
});

describe('range', function (): any {
  it('(range 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('range'), 1, 2],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it('(range 10)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('range'), 10],
      [Symbol.for('quote'), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]],
    ]);
  });
  it('(range 10 20)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('range'), 10, 20],
      [Symbol.for('quote'), [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]],
    ]);
  });
  it('(range 20 40 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('range'), 20, 40, 2],
      [Symbol.for('quote'), [20, 22, 24, 26, 28, 30, 32, 34, 36, 38]],
    ]);
  });
  it('(range 20 10 -1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('range'), 20, 10, -1],
      [Symbol.for('quote'), [20, 19, 18, 17, 16, 15, 14, 13, 12, 11]],
    ]);
  });
  return it('(range 10 15 1.5)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('range'), 10, 15, 1.5],
      [Symbol.for('quote'), [10, 11.5, 13, 14.5]],
    ]);
  });
});

describe('member', function (): any {
  it("(member 2 '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('member'), 2, [Symbol.for('quote'), [1, 2, 3, 4]]],
      [Symbol.for('quote'), [2, 3, 4]],
    ]);
  });
  it("(member 9 '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('member'), 9, [Symbol.for('quote'), [1, 2, 3, 4]]],
      false,
    ]);
  });
  return it("(member 5 '(3 5 1 7 2 9) (lambda (x y) (< x y)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('member'),
        5,
        [Symbol.for('quote'), [3, 5, 1, 7, 2, 9]],
        [
          Symbol.for('lambda'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('<'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      [Symbol.for('quote'), [7, 2, 9]],
    ]);
  });
});

describe('member?', function (): any {
  it("(member? 2 '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('member?'), 2, [Symbol.for('quote'), [1, 2, 3, 4]]],
      true,
    ]);
  });
  return it("(member? 9 '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('member?'), 9, [Symbol.for('quote'), [1, 2, 3, 4]]],
      false,
    ]);
  });
});

describe('memq?', function (): any {
  it("(memq? 2 '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('memq?'), 2, [Symbol.for('quote'), [1, 2, 3, 4]]],
      true,
    ]);
  });
  it("(memq? 9 '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('memq?'), 9, [Symbol.for('quote'), [1, 2, 3, 4]]],
      false,
    ]);
  });
  return it("(compile '(memq? x lst))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('memq?'), Symbol.for('x'), Symbol.for('lst')],
        ],
      ],
      'lst.includes(x);',
    ]);
  });
});

describe('take', function (): any {
  it("(take '(1 2 3 4) 0)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3, 4]], 0],
      [Symbol.for('quote'), []],
    ]);
  });
  it("(take '(1 2 3 4) 1)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3, 4]], 1],
      [Symbol.for('quote'), [1]],
    ]);
  });
  return it("(take '(1 2 3 4) 2)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3, 4]], 2],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
});

describe('drop', function (): any {
  it("(drop '(1 2 3 4) 0)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('drop'), [Symbol.for('quote'), [1, 2, 3, 4]], 0],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  return it("(drop '(1 2 3 4) 1)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('drop'), [Symbol.for('quote'), [1, 2, 3, 4]], 1],
      [Symbol.for('quote'), [2, 3, 4]],
    ]);
  });
});

describe('drop-right', function (): any {
  it("(drop-right '(1 2 3 4) 0)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('drop-right'), [Symbol.for('quote'), [1, 2, 3, 4]], 0],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  return it("(drop-right '(1 2 3 4) 1)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('drop-right'), [Symbol.for('quote'), [1, 2, 3, 4]], 1],
      [Symbol.for('quote'), [1, 2, 3]],
    ]);
  });
});

describe('map', function (): any {
  it("(map list '(1 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('map'), Symbol.for('list'), [Symbol.for('quote'), [1, 2]]],
      [Symbol.for('quote'), [[1], [2]]],
    ]);
  });
  it("((lambda () (define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))) (map fact '(1 2 3 4 5 6))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            [Symbol.for('fact'), Symbol.for('n')],
            [
              Symbol.for('if'),
              [Symbol.for('<'), Symbol.for('n'), 2],
              1,
              [
                Symbol.for('*'),
                Symbol.for('n'),
                [Symbol.for('fact'), [Symbol.for('-'), Symbol.for('n'), 1]],
              ],
            ],
          ],
          [
            Symbol.for('map'),
            Symbol.for('fact'),
            [Symbol.for('quote'), [1, 2, 3, 4, 5, 6]],
          ],
        ],
      ],
      [Symbol.for('quote'), [1, 2, 6, 24, 120, 720]],
    ]);
  });
  it("(compile '(map f lst))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('map'), Symbol.for('f'), Symbol.for('lst')],
        ],
      ],
      'lst.map(function (x) {\n' + '  return f(x);\n' + '});',
    ]);
  });
  return it("(compile '(map (lambda (x) x) lst))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('map'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
            Symbol.for('lst'),
          ],
        ],
      ],
      'lst.map(function (x) {\n' + '  return x;\n' + '});',
    ]);
  });
});

describe('foldl', function (): any {
  it("(foldl cons '() '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('foldl'),
        Symbol.for('cons'),
        [Symbol.for('quote'), []],
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ],
      [Symbol.for('quote'), [4, 3, 2, 1]],
    ]);
  });
  it("(compile '(foldl (lambda (x acc) x) v lst))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('foldl'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('acc')],
              Symbol.for('x'),
            ],
            Symbol.for('v'),
            Symbol.for('lst'),
          ],
        ],
      ],
      'lst.reduce(function (acc, x) {\n' + '  return x;\n' + '}, v);',
    ]);
  });
  return it("(compile '(foldl f v lst))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('foldl'),
            Symbol.for('f'),
            Symbol.for('v'),
            Symbol.for('lst'),
          ],
        ],
      ],
      'lst.reduce(function (acc, x) {\n' + '  return f(x, acc);\n' + '}, v);',
    ]);
  });
});

describe('foldr', function (): any {
  it("(foldr cons '() '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('foldr'),
        Symbol.for('cons'),
        [Symbol.for('quote'), []],
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  it("(foldr (lambda (v l) (cons (add1 v) l)) '() '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('foldr'),
        [
          Symbol.for('lambda'),
          [Symbol.for('v'), Symbol.for('l')],
          [
            Symbol.for('cons'),
            [Symbol.for('add1'), Symbol.for('v')],
            Symbol.for('l'),
          ],
        ],
        [Symbol.for('quote'), []],
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ],
      [Symbol.for('quote'), [2, 3, 4, 5]],
    ]);
  });
  it("(compile '(foldr (lambda (x acc) x) v lst))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('foldr'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('acc')],
              Symbol.for('x'),
            ],
            Symbol.for('v'),
            Symbol.for('lst'),
          ],
        ],
      ],
      'lst.reduceRight(function (acc, x) {\n' + '  return x;\n' + '}, v);',
    ]);
  });
  return it("(compile '(foldr f v lst))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('foldr'),
            Symbol.for('f'),
            Symbol.for('v'),
            Symbol.for('lst'),
          ],
        ],
      ],
      'lst.reduceRight(function (acc, x) {\n' +
        '  return f(x, acc);\n' +
        '}, v);',
    ]);
  });
});

describe('filter', function (): any {
  it('(filter string? \'("foo" 1 2 3))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('filter'),
        Symbol.for('string?'),
        [Symbol.for('quote'), ['foo', 1, 2, 3]],
      ],
      [Symbol.for('quote'), ['foo']],
    ]);
  });
  return it("(compile '(filter f lst))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('filter'), Symbol.for('f'), Symbol.for('lst')],
        ],
      ],
      'lst.filter(f);',
    ]);
  });
});

describe('string?', function (): any {
  it('(string? "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string?'), 'foo'],
      true,
    ]);
  });
  it('(string? 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string?'), 1],
      false,
    ]);
  });
  it('(string? (js/obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string?'), [Symbol.for('js/obj')]],
      false,
    ]);
  });
  it('(string? (list "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string?'), [Symbol.for('list'), 'foo']],
      false,
    ]);
  });
  it('(string? (js/obj "foo" ""))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string?'), [Symbol.for('js/obj'), 'foo', '']],
      false,
    ]);
  });
  it('(string? (js/obj "foo" \'()))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('string?'),
        [Symbol.for('js/obj'), 'foo', [Symbol.for('quote'), []]],
      ],
      false,
    ]);
  });
  it('(string? (js/obj "foo" (js/obj)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('string?'),
        [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj')]],
      ],
      false,
    ]);
  });
  it('(string? (js/obj "foo" "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string?'), [Symbol.for('js/obj'), 'foo', 'foo']],
      false,
    ]);
  });
  it("(string? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
  return it("(compile '(string? x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('string?'), Symbol.for('x')]],
      ],
      "typeof x === 'string';",
    ]);
  });
});

describe('string-length', function (): any {
  it('(string-length "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-length'), 'foo'],
      3,
    ]);
  });
  return it("(compile '(string-length x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('string-length'), Symbol.for('x')]],
      ],
      'x.length;',
    ]);
  });
});

describe('string-append', function (): any {
  it('(string-append)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-append')],
      '',
    ]);
  });
  it('(string-append "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-append'), 'foo'],
      'foo',
    ]);
  });
  it('(string-append "foo" "bar")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-append'), 'foo', 'bar'],
      'foobar',
    ]);
  });
  it('(apply string-append \'("foo" "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('apply'),
        Symbol.for('string-append'),
        [Symbol.for('quote'), ['foo', 'bar']],
      ],
      'foobar',
    ]);
  });
  it('(compile \'(string-append "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('string-append'), 'foo']],
      ],
      "'foo';",
    ]);
  });
  it('(compile \'(string-append "foo" "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('string-append'), 'foo', 'bar']],
      ],
      "'foo' + 'bar';",
    ]);
  });
  return it('(compile \'(string-append "foo" "bar" "baz"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('string-append'), 'foo', 'bar', 'baz'],
        ],
      ],
      "'foo' + 'bar' + 'baz';",
    ]);
  });
});

describe('string-join', function (): any {
  it('(string-join \'("foo" "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-join'), [Symbol.for('quote'), ['foo', 'bar']]],
      'foo bar',
    ]);
  });
  it('(string-join \'("foo" "bar") ",")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-join'), [Symbol.for('quote'), ['foo', 'bar']], ','],
      'foo,bar',
    ]);
  });
  return it('(compile \'(string-join \'("foo" "bar") ","))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('string-join'),
            [Symbol.for('quote'), ['foo', 'bar']],
            ',',
          ],
        ],
      ],
      "['foo', 'bar'].join(',');",
    ]);
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
  it('(string-split "foo\n' + 'bar\n' + 'baz" "\n' + '")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-split'), 'foo\n' + 'bar\n' + 'baz', '\n'],
      [Symbol.for('quote'), ['foo', 'bar', 'baz']],
    ]);
  });
  return it('(compile \'(string-split "foo,bar,baz" ","))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('string-split'), 'foo,bar,baz', ',']],
      ],
      "'foo,bar,baz'.split(',');",
    ]);
  });
});

describe('string-trim', function (): any {
  it('(string-trim "  foo bar  baz  ")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-trim'), '  foo bar  baz  '],
      'foo bar  baz',
    ]);
  });
  it('(string-trim "  foo bar  baz \n' + '\n' + '	")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-trim'), '  foo bar  baz \n' + '\n' + '	'],
      'foo bar  baz',
    ]);
  });
  return it("(compile '(string-trim x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('string-trim'), Symbol.for('x')]],
      ],
      'x.trim();',
    ]);
  });
});

describe('string-upcase', function (): any {
  it('(string-upcase "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-upcase'), 'foo'],
      'FOO',
    ]);
  });
  return it("(compile '(string-upcase x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('string-upcase'), Symbol.for('x')]],
      ],
      'x.toUpperCase();',
    ]);
  });
});

describe('string-downcase', function (): any {
  it('(string-downcase "FOO")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('string-downcase'), 'FOO'],
      'foo',
    ]);
  });
  return it("(compile '(string-downcase x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('string-downcase'), Symbol.for('x')]],
      ],
      'x.toLowerCase();',
    ]);
  });
});

describe('substring', function (): any {
  it('(substring "Apple" 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('substring'), 'Apple', 1, 3],
      'pp',
    ]);
  });
  it('(substring "Apple" 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('substring'), 'Apple', 1],
      'pple',
    ]);
  });
  it("(compile '(substring str i))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('substring'), Symbol.for('str'), Symbol.for('i')],
        ],
      ],
      'str.substring(i);',
    ]);
  });
  return it("(compile '(substring str i j))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('substring'),
            Symbol.for('str'),
            Symbol.for('i'),
            Symbol.for('j'),
          ],
        ],
      ],
      'str.substring(i, j);',
    ]);
  });
});

describe('as~>', function (): any {
  it('(as~> 0 _ (+ _ 1) (+ _ 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('as~>'),
        0,
        Symbol.for('_'),
        [Symbol.for('+'), Symbol.for('_'), 1],
        [Symbol.for('+'), Symbol.for('_'), 1],
      ],
      2,
    ]);
  });
  it("(macroexpand '(as~> 0 _ (+ _ 1) (+ _ 1)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('macroexpand'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('as~>'),
            0,
            Symbol.for('_'),
            [Symbol.for('+'), Symbol.for('_'), 1],
            [Symbol.for('+'), Symbol.for('_'), 1],
          ],
        ],
      ],
      [Symbol.for('quote'), [Symbol.for('+'), [Symbol.for('+'), 0, 1], 1]],
    ]);
  });
  return it("(compile '(as~> 0 _ (+ _ 1) (+ _ 1)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('as~>'),
            0,
            Symbol.for('_'),
            [Symbol.for('+'), Symbol.for('_'), 1],
            [Symbol.for('+'), Symbol.for('_'), 1],
          ],
        ],
      ],
      '0 + 1 + 1;',
    ]);
  });
});

describe('ann', function (): any {
  it('(ann #u Any)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('ann'), undefined, Symbol.for('Any')],
      undefined,
    ]);
  });
  it("(compile '(ann #t Any))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]],
      ],
      'true;',
    ]);
  });
  return it("(compile '(ann #t Any) :to 'typescript)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]],
        Symbol.for(':to'),
        [Symbol.for('quote'), Symbol.for('typescript')],
      ],
      'true as any;',
    ]);
  });
});

describe('cons?', function (): any {
  return it("(cons? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cons?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
});

describe('list?', function (): any {
  it("(list? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list?'), [Symbol.for('quote'), []]],
      true,
    ]);
  });
  it("(list? '(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      false,
    ]);
  });
  it("(list? '(1 2 . 3))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list?'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]],
      false,
    ]);
  });
  it("(list? '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
      true,
    ]);
  });
  return it("(list? '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      true,
    ]);
  });
});

describe('vector?', function (): any {
  it("(vector? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('vector?'), [Symbol.for('quote'), []]],
      true,
    ]);
  });
  it("(vector? '(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      true,
    ]);
  });
  it("(vector? '(1 2 . 3))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('vector?'),
        [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
      ],
      true,
    ]);
  });
  it("(vector? '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
      true,
    ]);
  });
  return it("(vector? '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('vector?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      true,
    ]);
  });
});

describe('array-list?', function (): any {
  it("(array-list? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('array-list?'), [Symbol.for('quote'), []]],
      true,
    ]);
  });
  it("(array-list? '(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('array-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
      ],
      true,
    ]);
  });
  it("(array-list? '(1 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('array-list?'), [Symbol.for('quote'), [1, 2]]],
      true,
    ]);
  });
  return it("(array-list? '(1 2 3))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('array-list?'), [Symbol.for('quote'), [1, 2, 3]]],
      true,
    ]);
  });
});

describe('linked-list?', function (): any {
  it("(linked-list? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('linked-list?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
  it("(linked-list? '(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('linked-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
      ],
      false,
    ]);
  });
  it("(linked-list? '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('linked-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      true,
    ]);
  });
  it("(linked-list? '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('linked-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      true,
    ]);
  });
  return it("(linked-list? '(1 2 . (3 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('linked-list?'),
        [
          Symbol.for('quote'),
          [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]],
        ],
      ],
      true,
    ]);
  });
});

describe('linked-list-link?', function (): any {
  it("(linked-list-link? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('linked-list-link?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
  it("(linked-list-link? '(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('linked-list-link?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
      ],
      true,
    ]);
  });
  it("(linked-list-link? '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('linked-list-link?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      true,
    ]);
  });
  it("(linked-list-link? '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('linked-list-link?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      true,
    ]);
  });
  return it("(linked-list-link? '(1 2 . (3 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('linked-list-link?'),
        [
          Symbol.for('quote'),
          [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]],
        ],
      ],
      true,
    ]);
  });
});

describe('length', function (): any {
  it("(length '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('length'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
      1,
    ]);
  });
  it("(length '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('length'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
  return it("(length '(1 2 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('length'),
        [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]],
      ],
      2,
    ]);
  });
});

describe('last', function (): any {
  it("(last '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('last'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
      1,
    ]);
  });
  it("(last '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('last'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
  return it("(last '(1 2 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('last'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]],
      2,
    ]);
  });
});

describe('nth', function (): any {
  it("(nth 1 '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('nth'),
        1,
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
  return it("(nth 1 '(1 2 . (3 . ())))", function (): any {
    return testRepl([
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
    ]);
  });
});

describe('cdr', function (): any {
  it("(cdr '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('cdr'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      [Symbol.for('quote'), [2, Symbol.for('.'), []]],
    ]);
  });
  return it("(cdr '(1 2 . (3 . ())))", function (): any {
    return testRepl([
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
    ]);
  });
});

describe('set-car!', function (): any {
  it("((lambda () (define foo '()) (set-car! foo 'bar) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), []]],
          [
            Symbol.for('set-car!'),
            Symbol.for('foo'),
            [Symbol.for('quote'), Symbol.for('bar')],
          ],
          Symbol.for('foo'),
        ],
      ],
      [Symbol.for('quote'), []],
    ]);
  });
  return it("((lambda () (define foo '(foo)) (set-car! foo 'bar) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('foo')]],
          ],
          [
            Symbol.for('set-car!'),
            Symbol.for('foo'),
            [Symbol.for('quote'), Symbol.for('bar')],
          ],
          Symbol.for('foo'),
        ],
      ],
      [Symbol.for('quote'), [Symbol.for('bar')]],
    ]);
  });
});

describe('set-cdr!', function (): any {
  it("((lambda () (define foo '()) (set-cdr! foo '(bar)) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), []]],
          [
            Symbol.for('set-cdr!'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('bar')]],
          ],
          Symbol.for('foo'),
        ],
      ],
      [Symbol.for('quote'), []],
    ]);
  });
  it("((lambda () (define foo '(foo)) (set-cdr! foo '(bar)) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('foo')]],
          ],
          [
            Symbol.for('set-cdr!'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('bar')]],
          ],
          Symbol.for('foo'),
        ],
      ],
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
    ]);
  });
  it("((lambda () (define foo '(foo bar)) (set-cdr! foo '(baz)) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
          ],
          [
            Symbol.for('set-cdr!'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('baz')]],
          ],
          Symbol.for('foo'),
        ],
      ],
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('baz')]],
    ]);
  });
  it("((lambda () (define foo '(foo bar)) (set-cdr! foo '(baz . quux)) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
          ],
          [
            Symbol.for('set-cdr!'),
            Symbol.for('foo'),
            [
              Symbol.for('quote'),
              [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')],
            ],
          ],
          Symbol.for('foo'),
        ],
      ],
      [
        Symbol.for('quote'),
        [
          Symbol.for('foo'),
          Symbol.for('baz'),
          Symbol.for('.'),
          Symbol.for('quux'),
        ],
      ],
    ]);
  });
  it("((lambda () (define foo '(foo . bar)) (set-cdr! foo '(baz)) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [
              Symbol.for('quote'),
              [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
            ],
          ],
          [
            Symbol.for('set-cdr!'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('baz')]],
          ],
          Symbol.for('foo'),
        ],
      ],
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('baz')]],
    ]);
  });
  it("((lambda () (define foo '(foo . bar)) (set-cdr! foo '(baz . quux)) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [
              Symbol.for('quote'),
              [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
            ],
          ],
          [
            Symbol.for('set-cdr!'),
            Symbol.for('foo'),
            [
              Symbol.for('quote'),
              [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')],
            ],
          ],
          Symbol.for('foo'),
        ],
      ],
      [
        Symbol.for('quote'),
        [
          Symbol.for('foo'),
          Symbol.for('baz'),
          Symbol.for('.'),
          Symbol.for('quux'),
        ],
      ],
    ]);
  });
  it("((lambda () (define foo '(foo)) (set-cdr! foo 'bar) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('foo')]],
          ],
          [
            Symbol.for('set-cdr!'),
            Symbol.for('foo'),
            [Symbol.for('quote'), Symbol.for('bar')],
          ],
          Symbol.for('foo'),
        ],
      ],
      [
        Symbol.for('quote'),
        [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
      ],
    ]);
  });
  it("((lambda () (define foo '(foo bar . baz)) (set-cdr! foo '(quux)) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [
              Symbol.for('quote'),
              [
                Symbol.for('foo'),
                Symbol.for('bar'),
                Symbol.for('.'),
                Symbol.for('baz'),
              ],
            ],
          ],
          [
            Symbol.for('set-cdr!'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('quux')]],
          ],
          Symbol.for('foo'),
        ],
      ],
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('quux')]],
    ]);
  });
  return it("((lambda () (define foo '(foo bar . baz)) (set-cdr! foo 'quux) foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [
              Symbol.for('quote'),
              [
                Symbol.for('foo'),
                Symbol.for('bar'),
                Symbol.for('.'),
                Symbol.for('baz'),
              ],
            ],
          ],
          [
            Symbol.for('set-cdr!'),
            Symbol.for('foo'),
            [Symbol.for('quote'), Symbol.for('quux')],
          ],
          Symbol.for('foo'),
        ],
      ],
      [
        Symbol.for('quote'),
        [Symbol.for('foo'), Symbol.for('.'), Symbol.for('quux')],
      ],
    ]);
  });
});

describe('Dotted lists', function (): any {
  return it("(equal? '(1 2) '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('equal?'),
        [Symbol.for('quote'), [1, 2]],
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      true,
    ]);
  });
});

describe('dotted-list?', function (): any {
  it("(dotted-list? '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('dotted-list?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
  it("(dotted-list? '(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
      ],
      true,
    ]);
  });
  it("(dotted-list? '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      false,
    ]);
  });
  it("(dotted-list? '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      false,
    ]);
  });
  it("(dotted-list? '(1 . (2 . 3)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]],
      ],
      true,
    ]);
  });
  it("(dotted-list? '(foo . bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list?'),
        [
          Symbol.for('quote'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
        ],
      ],
      true,
    ]);
  });
  return it("(dotted-list? '(foo bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      false,
    ]);
  });
});

describe('dotted-list-length', function (): any {
  it("(dotted-list-length '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('dotted-list-length'), [Symbol.for('quote'), []]],
      0,
    ]);
  });
  it("(dotted-list-length '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-length'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      1,
    ]);
  });
  return it("(dotted-list-length '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-length'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
});

describe('dotted-list-head', function (): any {
  it("(dotted-list-head '(foo . bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-head'),
        [
          Symbol.for('quote'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
        ],
      ],
      [Symbol.for('quote'), [Symbol.for('foo')]],
    ]);
  });
  return it("(dotted-list-head '(foo bar . baz))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-head'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('foo'),
            Symbol.for('bar'),
            Symbol.for('.'),
            Symbol.for('baz'),
          ],
        ],
      ],
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
    ]);
  });
});

describe('dotted-list-tail', function (): any {
  it("(dotted-list-tail '(foo . bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-tail'),
        [
          Symbol.for('quote'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
        ],
      ],
      [Symbol.for('quote'), Symbol.for('bar')],
    ]);
  });
  return it("(dotted-list-tail '(foo bar . baz))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-tail'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('foo'),
            Symbol.for('bar'),
            Symbol.for('.'),
            Symbol.for('baz'),
          ],
        ],
      ],
      [Symbol.for('quote'), Symbol.for('baz')],
    ]);
  });
});

describe('dotted-list-last', function (): any {
  it("(dotted-list-last '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('dotted-list-last'), [Symbol.for('quote'), []]],
      undefined,
    ]);
  });
  it("(dotted-list-last '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-last'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      1,
    ]);
  });
  return it("(dotted-list-last '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-last'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
});

describe('dotted-list-last-cdr', function (): any {
  it("(dotted-list-last-cdr '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('dotted-list-last-cdr'), [Symbol.for('quote'), []]],
      [Symbol.for('quote'), []],
    ]);
  });
  it("(dotted-list-last-cdr '(1 . ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-last-cdr'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      [Symbol.for('quote'), []],
    ]);
  });
  return it("(dotted-list-last-cdr '(1 . (2 . ())))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list-last-cdr'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      [Symbol.for('quote'), []],
    ]);
  });
});

describe('dotted-list->proper-list', function (): any {
  it("(dotted-list->proper-list '(foo . bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list->proper-list'),
        [
          Symbol.for('quote'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
        ],
      ],
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
    ]);
  });
  return it("(dotted-list->proper-list '(foo bar . baz))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list->proper-list'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('foo'),
            Symbol.for('bar'),
            Symbol.for('.'),
            Symbol.for('baz'),
          ],
        ],
      ],
      [
        Symbol.for('quote'),
        [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
      ],
    ]);
  });
});

describe('proper-list?', function (): any {
  it("(proper-list? '(foo bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('proper-list?'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      true,
    ]);
  });
  return it("(proper-list? '(foo . bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('proper-list?'),
        [
          Symbol.for('quote'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
        ],
      ],
      false,
    ]);
  });
});

describe('circular-list?', function (): any {
  it("((lambda () (define foo '()) (circular-list? foo) (set-cdr! foo foo) (circular-list? foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), []]],
          [Symbol.for('circular-list?'), Symbol.for('foo')],
          [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')],
          [Symbol.for('circular-list?'), Symbol.for('foo')],
        ],
      ],
      false,
    ]);
  });
  it("(circular-list? '(foo))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('circular-list?'),
        [Symbol.for('quote'), [Symbol.for('foo')]],
      ],
      false,
    ]);
  });
  it("(circular-list? '(foo . bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('circular-list?'),
        [
          Symbol.for('quote'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
        ],
      ],
      false,
    ]);
  });
  it("((lambda () (define foo '(foo)) (set-cdr! foo foo) (circular-list? foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('foo')]],
          ],
          [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')],
          [Symbol.for('circular-list?'), Symbol.for('foo')],
        ],
      ],
      true,
    ]);
  });
  it("((lambda () (define foo '(foo . ())) (set-cdr! foo foo) (circular-list? foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), []]],
          ],
          [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')],
          [Symbol.for('circular-list?'), Symbol.for('foo')],
        ],
      ],
      true,
    ]);
  });
  return it("((lambda () (define foo '(foo bar)) (set-cdr! foo foo) (circular-list? foo)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
          ],
          [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')],
          [Symbol.for('circular-list?'), Symbol.for('foo')],
        ],
      ],
      true,
    ]);
  });
});

describe('proper-list->dotted-list', function (): any {
  it("(proper-list->dotted-list '(foo bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('proper-list->dotted-list'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      [
        Symbol.for('quote'),
        [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
      ],
    ]);
  });
  return it("(proper-list->dotted-list '(foo bar baz))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('proper-list->dotted-list'),
        [
          Symbol.for('quote'),
          [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
        ],
      ],
      [
        Symbol.for('quote'),
        [
          Symbol.for('foo'),
          Symbol.for('bar'),
          Symbol.for('.'),
          Symbol.for('baz'),
        ],
      ],
    ]);
  });
});

describe('list*', function (): any {
  it('(list*)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list*')],
      undefined,
    ]);
  });
  it('(list* 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list*'), 1],
      1,
    ]);
  });
  it('(list* 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list*'), 1, 2],
      [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
    ]);
  });
  it('(list* 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list*'), 1, 2, 3],
      [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
    ]);
  });
  it('(list* 1 2 3 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list*'), 1, 2, 3, 4],
      [Symbol.for('quote'), [1, 2, 3, Symbol.for('.'), 4]],
    ]);
  });
  it("(list* 1 '())", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list*'), 1, [Symbol.for('quote'), []]],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it("(list* 1 '(2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list*'), 1, [Symbol.for('quote'), [2]]],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  return it("(list* 1 '(2 . 3))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('list*'), 1, [Symbol.for('quote'), [2, Symbol.for('.'), 3]]],
      [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
    ]);
  });
});

describe('flatten', function (): any {
  it("(flatten '(1 2 3 4))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('flatten'), [Symbol.for('quote'), [1, 2, 3, 4]]],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  it("(flatten '(1 . 2))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('flatten'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  it("(flatten '((a) b (c (d) . e) ()))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('flatten'),
        [
          Symbol.for('quote'),
          [
            [Symbol.for('a')],
            Symbol.for('b'),
            [
              Symbol.for('c'),
              [Symbol.for('d')],
              Symbol.for('.'),
              Symbol.for('e'),
            ],
            [],
          ],
        ],
      ],
      [
        Symbol.for('quote'),
        [
          Symbol.for('a'),
          Symbol.for('b'),
          Symbol.for('c'),
          Symbol.for('d'),
          Symbol.for('e'),
        ],
      ],
    ]);
  });
  return it("(flatten '((((4)))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('flatten'), [Symbol.for('quote'), [[[[4]]]]]],
      [Symbol.for('quote'), [4]],
    ]);
  });
});

describe('Cons dot', function (): any {
  it('*cons-dot*', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      Symbol.for('*cons-dot*'),
      [Symbol.for('quote'), Symbol.for('.')],
    ]);
  });
  it('(cons-dot)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cons-dot')],
      [Symbol.for('quote'), Symbol.for('.')],
    ]);
  });
  return it('(cons-dot? *cons-dot*)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('cons-dot?'), Symbol.for('*cons-dot*')],
      true,
    ]);
  });
});

describe('require', function (): any {
  it('(compile \'(require "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('require'), 'foo']],
      ],
      "import * as foo from 'foo';",
    ]);
  });
  it('(compile \'(require "foo-bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('require'), 'foo-bar']],
      ],
      "import * as fooBar from 'foo-bar';",
    ]);
  });
  it('(compile \'(require foo "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('require'), Symbol.for('foo'), 'bar'],
        ],
      ],
      "import * as foo from 'bar';",
    ]);
  });
  it("(compile '(require (only-in foo bar)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('require'),
            [Symbol.for('only-in'), Symbol.for('foo'), Symbol.for('bar')],
          ],
        ],
      ],
      'import {\n' + '  bar\n' + "} from 'foo';",
    ]);
  });
  it("(compile '(require (only-in foo (bar baz))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('require'),
            [
              Symbol.for('only-in'),
              Symbol.for('foo'),
              [Symbol.for('bar'), Symbol.for('baz')],
            ],
          ],
        ],
      ],
      'import {\n' + '  bar as baz\n' + "} from 'foo';",
    ]);
  });
  it('(compile \'(require (only-in "foo" (bar baz))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
      ],
      'import {\n' + '  bar as baz\n' + "} from 'foo';",
    ]);
  });
  it("(compile '(require (only-in foo bar bar)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('require'),
            [
              Symbol.for('only-in'),
              Symbol.for('foo'),
              Symbol.for('bar'),
              Symbol.for('bar'),
            ],
          ],
        ],
      ],
      'import {\n' + '  bar\n' + "} from 'foo';",
    ]);
  });
  it("(compile '(require (only-in foo bar (baz bar))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('require'),
            [
              Symbol.for('only-in'),
              Symbol.for('foo'),
              Symbol.for('bar'),
              [Symbol.for('baz'), Symbol.for('bar')],
            ],
          ],
        ],
      ],
      'import {\n' + '  bar\n' + "} from 'foo';",
    ]);
  });
  it('(compile \'(require "foo") :fes-module-interop #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('require'), 'foo']],
        Symbol.for(':fes-module-interop'),
        true,
      ],
      "import foo from 'foo';",
    ]);
  });
  it('(compile \'(require "foo-bar") :fes-module-interop #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('require'), 'foo-bar']],
        Symbol.for(':fes-module-interop'),
        true,
      ],
      "import fooBar from 'foo-bar';",
    ]);
  });
  it('(compile \'(require foo "bar") :fes-module-interop #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('require'), Symbol.for('foo'), 'bar'],
        ],
        Symbol.for(':fes-module-interop'),
        true,
      ],
      "import foo from 'bar';",
    ]);
  });
  it('(compile \'(require "foo" "bar") :fes-module-interop #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('require'), 'foo', 'bar']],
        Symbol.for(':fes-module-interop'),
        true,
      ],
      "import foo from 'bar';",
    ]);
  });
  it('(compile \'(require "foo") :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('require'), 'foo']],
        Symbol.for(':fcommonjs'),
        true,
      ],
      "let foo = require('foo');",
    ]);
  });
  it('(compile \'(require foo "bar") :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it('(compile \'(require "foo" "bar") :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('require'), 'foo', 'bar']],
        Symbol.for(':fcommonjs'),
        true,
      ],
      "let foo = require('bar');",
    ]);
  });
  it('(compile \'(require (only-in "foo" bar)) :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  it('(compile \'(require (only-in "foo" (bar baz))) :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
  xit('(compile \'(require \'foo "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('require'),
            [Symbol.for('quote'), Symbol.for('foo')],
            'bar',
          ],
        ],
      ],
      "import foo from 'bar';",
    ]);
  });
  xit("(compile '(require foo :as bar))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('require'),
            Symbol.for('foo'),
            Symbol.for(':as'),
            Symbol.for('bar'),
          ],
        ],
      ],
      "import bar from 'foo';",
    ]);
  });
  xit("(compile '(require (foo :as bar)))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('require'),
            [Symbol.for('foo'), Symbol.for(':as'), Symbol.for('bar')],
          ],
        ],
      ],
      "import bar from 'foo';",
    ]);
  });
  return xit('(compile \'(require ("foo" :as "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('xit>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('require'), ['foo', Symbol.for(':as'), 'bar']],
        ],
      ],
      "import bar from 'foo';",
    ]);
  });
});

describe('provide', function (): any {
  it("(compile '(provide))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide')]]],
      '',
    ]);
  });
  it("(compile '(provide x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x')]],
      ],
      'export {\n' + '  x\n' + '};',
    ]);
  });
  it("(compile '(provide x y))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('provide'), Symbol.for('x'), Symbol.for('y')],
        ],
      ],
      'export {\n' + '  x,\n' + '  y\n' + '};',
    ]);
  });
  it("(compile '(provide (rename-out (x y))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('provide'),
            [Symbol.for('rename-out'), [Symbol.for('x'), Symbol.for('y')]],
          ],
        ],
      ],
      'export {\n' + '  x as y\n' + '};',
    ]);
  });
  it("(compile '(provide (rename-out (x y) (w z))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('provide'),
            [
              Symbol.for('rename-out'),
              [Symbol.for('x'), Symbol.for('y')],
              [Symbol.for('w'), Symbol.for('z')],
            ],
          ],
        ],
      ],
      'export {\n' + '  x as y,\n' + '  w as z\n' + '};',
    ]);
  });
  it("(compile '(provide x (rename-out (y z))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('provide'),
            Symbol.for('x'),
            [Symbol.for('rename-out'), [Symbol.for('y'), Symbol.for('z')]],
          ],
        ],
      ],
      'export {\n' + '  x,\n' + '  y as z\n' + '};',
    ]);
  });
  it("(compile '(provide x x))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('provide'), Symbol.for('x'), Symbol.for('x')],
        ],
      ],
      'export {\n' + '  x\n' + '};',
    ]);
  });
  it("(compile '(provide x (rename-out (y x))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('provide'),
            Symbol.for('x'),
            [Symbol.for('rename-out'), [Symbol.for('y'), Symbol.for('x')]],
          ],
        ],
      ],
      'export {\n' + '  x\n' + '};',
    ]);
  });
  it("(compile '(provide (rename-out (x js/undefined))))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('provide'),
            [
              Symbol.for('rename-out'),
              [Symbol.for('x'), Symbol.for('js/undefined')],
            ],
          ],
        ],
      ],
      'export {\n' + '  x as jsUndefined\n' + '};',
    ]);
  });
  it('(compile \'(provide (all-from-out "foo")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('provide'), [Symbol.for('all-from-out'), 'foo']],
        ],
      ],
      "export * from 'foo';",
    ]);
  });
  it('(compile \'(provide (all-from-out "foo") bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('provide'),
            [Symbol.for('all-from-out'), 'foo'],
            Symbol.for('bar'),
          ],
        ],
      ],
      "export * from 'foo';\n" + '\n' + 'export {\n' + '  bar\n' + '};',
    ]);
  });
  it("(compile '(provide x) :fcommonjs #t)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x')]],
        Symbol.for(':fcommonjs'),
        true,
      ],
      'module.exports = {\n' + '  x\n' + '};',
    ]);
  });
  it("(compile '(provide x y) :fcommonjs #t)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [Symbol.for('provide'), Symbol.for('x'), Symbol.for('y')],
        ],
        Symbol.for(':fcommonjs'),
        true,
      ],
      'module.exports = {\n' + '  x,\n' + '  y\n' + '};',
    ]);
  });
  it("(compile '(provide foo-bar) :fcommonjs #t)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('foo-bar')]],
        Symbol.for(':fcommonjs'),
        true,
      ],
      'module.exports = {\n' + '  fooBar\n' + '};',
    ]);
  });
  it("(compile '(provide (rename-out (x y))) :fcommonjs #t)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
      'module.exports = {\n' + '  x: y\n' + '};',
    ]);
  });
  return it('(compile \'(provide (all-from-out "foo-bar") baz) :fcommonjs #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [
          Symbol.for('quote'),
          [
            Symbol.for('provide'),
            [Symbol.for('all-from-out'), 'foo-bar'],
            Symbol.for('baz'),
          ],
        ],
        Symbol.for(':fcommonjs'),
        true,
      ],
      'module.exports = {\n' + '  ...fooBar,\n' + '  baz\n' + '};',
    ]);
  });
});

describe('compile', function (): any {
  it('(compile #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('compile'), true],
      'true;',
    ]);
  });
  it("(compile #t :to 'javascript)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        true,
        Symbol.for(':to'),
        [Symbol.for('quote'), Symbol.for('javascript')],
      ],
      'true;',
    ]);
  });
  it("(compile #t :from 'roselisp :to 'javascript)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        true,
        Symbol.for(':from'),
        [Symbol.for('quote'), Symbol.for('roselisp')],
        Symbol.for(':to'),
        [Symbol.for('quote'), Symbol.for('javascript')],
      ],
      'true;',
    ]);
  });
  it("(compile '(ann #t Any) :from 'roselisp :to 'typescript)", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]],
        Symbol.for(':from'),
        [Symbol.for('quote'), Symbol.for('roselisp')],
        Symbol.for(':to'),
        [Symbol.for('quote'), Symbol.for('typescript')],
      ],
      'true as any;',
    ]);
  });
  it('(compile "true" :to \'roselisp)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        'true',
        Symbol.for(':to'),
        [Symbol.for('quote'), Symbol.for('roselisp')],
      ],
      true,
    ]);
  });
  it('(compile "true" :from \'javascript :to \'roselisp)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        'true',
        Symbol.for(':from'),
        [Symbol.for('quote'), Symbol.for('javascript')],
        Symbol.for(':to'),
        [Symbol.for('quote'), Symbol.for('roselisp')],
      ],
      true,
    ]);
  });
  return it('(compile "true as any" :from \'typescript :to \'roselisp)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('compile'),
        'true as any',
        Symbol.for(':from'),
        [Symbol.for('quote'), Symbol.for('typescript')],
        Symbol.for(':to'),
        [Symbol.for('quote'), Symbol.for('roselisp')],
      ],
      [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]],
    ]);
  });
});

describe('decompile', function (): any {
  it('(decompile "true")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('decompile'), 'true'],
      true,
    ]);
  });
  it('(decompile "true" :from \'javascript)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('decompile'),
        'true',
        Symbol.for(':from'),
        [Symbol.for('quote'), Symbol.for('javascript')],
      ],
      true,
    ]);
  });
  it('(decompile "true" :from \'javascript :to \'roselisp)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('decompile'),
        'true',
        Symbol.for(':from'),
        [Symbol.for('quote'), Symbol.for('javascript')],
        Symbol.for(':to'),
        [Symbol.for('quote'), Symbol.for('roselisp')],
      ],
      true,
    ]);
  });
  it('(decompile "true as any" :from \'typescript)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('decompile'),
        'true as any',
        Symbol.for(':from'),
        [Symbol.for('quote'), Symbol.for('typescript')],
      ],
      [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]],
    ]);
  });
  return it('(decompile "true as any" :from \'typescript :to \'roselisp)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('decompile'),
        'true as any',
        Symbol.for(':from'),
        [Symbol.for('quote'), Symbol.for('typescript')],
        Symbol.for(':to'),
        [Symbol.for('quote'), Symbol.for('roselisp')],
      ],
      [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]],
    ]);
  });
});

describe('license', function (): any {
  return it('license', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      Symbol.for('license'),
      [Symbol.for('quote'), Symbol.for('MPL-2.0')],
    ]);
  });
});
