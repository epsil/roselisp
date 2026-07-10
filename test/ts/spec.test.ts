/**
 * # Test specification
 *
 * Tests expressed as a Roselisp REPL session.
 */

import { testRepl, testMacro } from './test-util';

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

describe('Numbers', function (): any {
  it('0', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, 0, 0]);
  });
  it('1', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, 1, 1]);
  });
  return it('2', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, 2, 2]);
  });
});

describe('Strings', function (): any {
  it('"foo"', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, 'foo', 'foo']);
  });
  it('"\\"foo\\""', function (): any {
    return testRepl([Symbol.for('roselisp'), undefined, '"foo"', '"foo"']);
  });
  return it('(eq? "	" "	")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('eq?'), '	', '	'],
      true,
    ]);
  });
});

describe('keywords', function (): any {
  it(':foo', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      Symbol.for(':foo'),
      [Symbol.for('quote'), Symbol.for(':foo')],
    ]);
  });
  return it('(quote :foo)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quote'), Symbol.for(':foo')],
      [Symbol.for('quote'), Symbol.for(':foo')],
    ]);
  });
});

describe('symbol?', function (): any {
  it('(symbol? (quote foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('symbol?'), [Symbol.for('quote'), Symbol.for('foo')]],
      true,
    ]);
  });
  it('(symbol? 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('symbol?'), 1],
      false,
    ]);
  });
  it('(symbol? "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('symbol?'), 'foo'],
      false,
    ]);
  });
  it('(symbol? (js-obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('symbol?'), [Symbol.for('js-obj')]],
      false,
    ]);
  });
  return it('(symbol? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('symbol?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
});

describe('symbol->string', function (): any {
  return it('(symbol->string (quote foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('symbol->string'), [Symbol.for('quote'), Symbol.for('foo')]],
      'foo',
    ]);
  });
});

describe('intern', function (): any {
  return it('(intern "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('intern'), 'foo'],
      [Symbol.for('quote'), Symbol.for('foo')],
    ]);
  });
});

describe('Cons cells', function (): any {
  it('(cons 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cons'), 1, 2],
      [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
    ]);
  });
  it('(cons 1 (cons 2 3))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cons'), 1, [Symbol.for('cons'), 2, 3]],
      [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
    ]);
  });
  it('(cons 1 (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cons'), 1, [Symbol.for('quote'), []]],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it('(cons 1 (quote (2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cons'), 1, [Symbol.for('quote'), [2]]],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  it('(car (quote (1 . 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('car'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      1,
    ]);
  });
  it('(cdr (quote (1 . 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      2,
    ]);
  });
  it('(car (cons 1 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('car'), [Symbol.for('cons'), 1, 2]],
      1,
    ]);
  });
  return it('(cdr (cons 1 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cdr'), [Symbol.for('cons'), 1, 2]],
      2,
    ]);
  });
});

describe('Lists', function (): any {
  it('(list 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list'), 1, 2],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  it('(aget (quote (1 2)) 0)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('aget'), [Symbol.for('quote'), [1, 2]], 0],
      1,
    ]);
  });
  it('(aget (quote ((1 2) (3 4))) 0 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(aref (quote (1 2)) 0)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('aref'), [Symbol.for('quote'), [1, 2]], 0],
      1,
    ]);
  });
  it('(aset (quote (1 2)) 0 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('aset'), [Symbol.for('quote'), [1, 2]], 0, 3],
      3,
    ]);
  });
  it('(let ((x (quote (1 2)))) (aset x 0 3) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2]]]],
        [Symbol.for('aset'), Symbol.for('x'), 0, 3],
        Symbol.for('x'),
      ],
      [Symbol.for('quote'), [3, 2]],
    ]);
  });
  return it('(let ((x (quote (1 2)))) (set! (aref x 0) 3) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2]]]],
        [Symbol.for('set!'), [Symbol.for('aref'), Symbol.for('x'), 0], 3],
        Symbol.for('x'),
      ],
      [Symbol.for('quote'), [3, 2]],
    ]);
  });
});

describe('quote', function (): any {
  it('(quote foo)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quote'), Symbol.for('foo')],
      [Symbol.for('quote'), Symbol.for('foo')],
    ]);
  });
  it('(quote (1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quote'), [1]],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it('(quote (1 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quote'), [1, 2]],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  return it('(quote ((1 2) (3 4)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('quasoquote', function (): any {
  it('(quasiquote foo)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quasiquote'), Symbol.for('foo')],
      [Symbol.for('quote'), Symbol.for('foo')],
    ]);
  });
  it('(quasiquote foo)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quasiquote'), Symbol.for('foo')],
      [Symbol.for('quote'), Symbol.for('foo')],
    ]);
  });
  it('(quasiquote ((unquote 1)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quasiquote'), [[Symbol.for('unquote'), 1]]],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it('(quasiquote (((unquote 1))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), 1]]]],
      [Symbol.for('quote'), [[1]]],
    ]);
  });
  return it('(quasiquote ((unquote-splicing (list 1 2 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('quasiquote'),
        [[Symbol.for('unquote-splicing'), [Symbol.for('list'), 1, 2, 3]]],
      ],
      [Symbol.for('quote'), [1, 2, 3]],
    ]);
  });
});

describe('Variables', function (): any {
  it('(let ((x 2)) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('let'), [[Symbol.for('x'), 2]], Symbol.for('x')],
      2,
    ]);
  });
  it('(let ((x 2) y) y)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
      [
        Symbol.for('let'),
        [Symbol.for('x')],
        [Symbol.for('set!'), Symbol.for('x'), 2],
        Symbol.for('x'),
      ],
      2,
    ]);
  });
  it('((lambda  (define x 2) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define x) (set! x 2) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
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
      undefined,
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
      undefined,
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
      undefined,
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
  it('((lambda  (define x 1) 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define (foo . args) args) (foo)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define (my-add x y) (+ x y)) (my-add 2 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(let ((my-add (lambda (x y) (+ x y)))) ((lambda  (define (my-add-2 x y) (my-add x y)) (my-add-2 2 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(let ((my-add (lambda (x y z) (+ x y z)))) ((lambda  (define (my-add-2 x y z) (my-add x y z)) (my-add-2 1 2 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('cefun', function (): any {
  it('((lambda  (defun my-add (x y) (+ x y)) (my-add 2 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(let ((my-add (lambda (x y) (+ x y)))) ((lambda  (defun my-add-2 (x y) (my-add x y)) (my-add-2 2 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(let ((my-add (lambda (x y z) (+ x y z)))) ((lambda  (defun my-add-2 (x y z) (my-add x y z)) (my-add-2 1 2 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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

describe('define-macro', function (): any {});

describe('defmacro', function (): any {});

describe('let', function (): any {
  it('(let ((x 0)) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('let'), [[Symbol.for('x'), 0]], Symbol.for('x')],
      0,
    ]);
  });
  it('(let ((x 1)) (let ((y 2)) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [[Symbol.for('x'), 1]],
        [Symbol.for('let'), [[Symbol.for('y'), 2]], Symbol.for('x')],
      ],
      1,
    ]);
  });
  it('(let ((x (quote ((1 2) (3 4))))) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
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
      undefined,
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
      undefined,
      [Symbol.for('let'), [Symbol.for('x')]],
      undefined,
    ]);
  });
  it('(let ((a 1)) (+ (let ((a 2)) a) a))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
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
      undefined,
      [Symbol.for('let*'), [[Symbol.for('x'), 1]], Symbol.for('x')],
      1,
    ]);
  });
});

describe('lambda', function (): any {
  it('((lambda (x) x) 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 1],
      1,
    ]);
  });
  it('((lambda (x) x) "Lisp")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 'Lisp'],
      'Lisp',
    ]);
  });
  it('((lambda x x) "Lisp")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [[Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')], 'Lisp'],
      [Symbol.for('quote'), ['Lisp']],
    ]);
  });
  it('((fn (x) x) 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [[Symbol.for('fn'), [Symbol.for('x')], Symbol.for('x')], 1],
      1,
    ]);
  });
  return it('((λ (x) x) 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [[Symbol.for('λ'), [Symbol.for('x')], Symbol.for('x')], 1],
      1,
    ]);
  });
});

describe('lexical scope', function (): any {
  it('((lambda  (define (K x) (lambda  x)) ((K 42))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define incrementer #u) (let ((x 1)) (set! incrementer (lambda  (set! x (+ x 1)) x))) (incrementer)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(let ((x 100) incrementer) (let ((x 1)) (set! incrementer (lambda  (set! x (+ x 1)) x))) (incrementer) x)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
      [Symbol.for('begin')],
      undefined,
    ]);
  });
});

describe('begin0', function (): any {
  return it('(begin0 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('begin0'), 1, 2],
      1,
    ]);
  });
});

describe('if', function (): any {
  it('(if #t 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('if'), true, 1, 2],
      1,
    ]);
  });
  it('(if #f 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('if'), false, 1, 2],
      2,
    ]);
  });
  it('(if (< 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('if'), [Symbol.for('<'), 1, 2], 1, 2],
      1,
    ]);
  });
  return it('(if (> 2 1) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('if'), [Symbol.for('>'), 2, 1], 1, 2],
      1,
    ]);
  });
});

describe('when', function (): any {
  it('(when (< 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('when'), [Symbol.for('<'), 1, 2], 1, 2],
      2,
    ]);
  });
  return it('(when (> 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('when'), [Symbol.for('>'), 1, 2], 1, 2],
      undefined,
    ]);
  });
});

describe('unless', function (): any {
  it('(unless (< 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('unless'), [Symbol.for('<'), 1, 2], 1, 2],
      undefined,
    ]);
  });
  return it('(unless (> 1 2) 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('unless'), [Symbol.for('>'), 1, 2], 1, 2],
      2,
    ]);
  });
});

describe('cond', function (): any {
  it('(cond (#f 1) (else 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cond'), [false, 1], [Symbol.for('else'), 2]],
      2,
    ]);
  });
  it('(cond (#t 1) (#f 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cond'), [true, 1], [false, 2]],
      1,
    ]);
  });
  it('(cond (#f 1) (#t 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cond'), [false, 1], [true, 2]],
      2,
    ]);
  });
  return it('(cond (#f 1) (#t 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cond'), [false, 1], [true, 2]],
      2,
    ]);
  });
});

describe('js/switch', function (): any {
  return it('((lambda  (define x "foo") (define y "bar") (js/switch x (case "foo" (set! y "baz") (break)) (default (set! y "quux"))) y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        [
          Symbol.for('lambda'),
          [],
          [Symbol.for('define'), Symbol.for('x'), 'foo'],
          [Symbol.for('define'), Symbol.for('y'), 'bar'],
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
      ],
      'baz',
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
  it('(eq? #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('eq?'), true, false],
      false,
    ]);
  });
  it('(eq (quote _) (quote _))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('eq'),
        [Symbol.for('quote'), Symbol.for('_')],
        [Symbol.for('quote'), Symbol.for('_')],
      ],
      true,
    ]);
  });
  return it('(eq _ (quote _))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('eq'),
        Symbol.for('_'),
        [Symbol.for('quote'), Symbol.for('_')],
      ],
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
  it('(equal? #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('equal?'), true, false],
      false,
    ]);
  });
  it('(equal? _ (quote _))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
      [Symbol.for('equal?'), 1, 1],
      true,
    ]);
  });
  return it('(equal? (quote ) (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('equal?'),
        [Symbol.for('quote'), []],
        [Symbol.for('quote'), []],
      ],
      true,
    ]);
  });
});

describe('and', function (): any {
  it('(and)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('and')],
      true,
    ]);
  });
  it('(and #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('and'), true],
      true,
    ]);
  });
  it('(and #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('and'), true, true],
      true,
    ]);
  });
  it('(and #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('and'), false, false],
      false,
    ]);
  });
  it('(and #f #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('and'), false, true],
      false,
    ]);
  });
  return it('(and #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('and'), true, false],
      false,
    ]);
  });
});

describe('or', function (): any {
  it('(or)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('or')],
      false,
    ]);
  });
  it('(or #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('or'), true],
      true,
    ]);
  });
  it('(or #t #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('or'), true, true],
      true,
    ]);
  });
  it('(or #f #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('or'), false, false],
      false,
    ]);
  });
  it('(or #f #t)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('or'), false, true],
      true,
    ]);
  });
  it('(or #t #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('or'), true, false],
      true,
    ]);
  });
  it('(or 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('or'), 1, 2],
      1,
    ]);
  });
  return it('(or #u 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('or'), undefined, 2],
      2,
    ]);
  });
});

describe('while', function (): any {
  return it('(let ((result (quote ))) (while (< (length result) 3) (set! result (cons 1 result))) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('for', function (): any {
  it('(let ((result (quote ))) (for ((x (quote (1 2 3)))) (set! result (cons x result))) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (1 2 3 4))) (define len (length foo)) (for ((i (range 0 len))) (pop-right! foo)) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('((lambda  (define foo (quote (1 2 3 4))) (for ((i (range 0 (length foo)))) (pop-right! foo)) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('break', function (): any {
  it('((lambda  (while #t (break)) 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(let ((result (list))) (for ((i (range 0 10))) (break) (push-right! result i)) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('continue', function (): any {
  it('(let ((result (list)) (i 0)) (while (< i 10) (set! i (+ i 1)) (when (< i 5) (continue)) (push-right! result i)) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(let ((result (list))) (for ((i (range 0 11))) (when (< i 5) (continue)) (push-right! result i)) result)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('return', function (): any {
  it('((lambda  (return 1) 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [[Symbol.for('lambda'), [], [Symbol.for('return'), 1], 2]],
      1,
    ]);
  });
  it('((js/function  (return 1) 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [[Symbol.for('js/function'), [], [Symbol.for('return'), 1], 2]],
      1,
    ]);
  });
  return it('((js/arrow  (return 1) 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [[Symbol.for('js/arrow'), [], [Symbol.for('return'), 1], 2]],
      1,
    ]);
  });
});

describe('get-field', function (): any {
  return it('(let ((obj (js-obj "foo" "bar"))) (get-field foo obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj'), 'foo', 'bar']]],
        [Symbol.for('get-field'), Symbol.for('foo'), Symbol.for('obj')],
      ],
      'bar',
    ]);
  });
});

describe('set-field!', function (): any {
  return it('(let ((obj (js-obj))) (set-field! foo obj "bar") (get-field foo obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj')]]],
        [Symbol.for('set-field!'), Symbol.for('foo'), Symbol.for('obj'), 'bar'],
        [Symbol.for('get-field'), Symbol.for('foo'), Symbol.for('obj')],
      ],
      'bar',
    ]);
  });
});

describe('field-bound?', function (): any {
  return it('(let ((obj (js-obj "foo" "bar"))) (field-bound? foo obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj'), 'foo', 'bar']]],
        [Symbol.for('field-bound?'), Symbol.for('foo'), Symbol.for('obj')],
      ],
      true,
    ]);
  });
});

describe('oget', function (): any {
  it('(let ((obj (js-obj "prop" "foo"))) (oget obj "prop"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj'), 'prop', 'foo']]],
        [Symbol.for('oget'), Symbol.for('obj'), 'prop'],
      ],
      'foo',
    ]);
  });
  return it('(oget _ "@@functional/placeholder")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('oget'), Symbol.for('_'), '@@functional/placeholder'],
      true,
    ]);
  });
});

describe('send', function (): any {
  it('(let ((obj (js-obj "add" (lambda (x y) (+ x y))))) (send obj add 1 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [
          [
            Symbol.for('obj'),
            [
              Symbol.for('js-obj'),
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
  it('(let ((obj (make-hash (quote (("foo" . "foo")))))) (send obj has "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(let ((obj (make-hash (quote (("foo" . "foo")))))) (send obj has "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('send/apply', function (): any {
  return it('(let ((obj (make-hash (quote (("foo" . "foo")))))) (send/apply obj has (quote ("foo"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('new', function (): any {
  it('(let (quux) (set! quux (new (class  (define/public (bar) "baz")))) (send quux bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(let (quux) (set! quux (new (class  (define/public val 1) (define (constructor x) (set-field! val this x)) (define/public (bar) (get-field val this))) 2)) (send quux bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('class', function (): any {
  it('((lambda  (define Foo (class object% (define/public (bar) "baz"))) (define quux (new Foo)) (send quux bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (defclass Foo  (define/public (bar) "baz")) (define quux (new Foo)) (send quux bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (defclass Foo  (define bar "baz")) (define quux (new Foo)) (get-field bar quux)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (defclass Foo  (define x) (define (constructor x) (set-field! x this x)) (define (bar) (get-field x this))) (define quux (new Foo "xyzzy")) (send quux bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('((lambda  (defclass Foo (Object) (define (bar) "baz")) (define quux (new Foo)) (send quux bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('define-class', function (): any {
  return it('((lambda  (define-class Foo  (define/public (bar) "bar")) (define foo (new Foo)) (send foo bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('defclass', function (): any {
  return it('((lambda  (defclass Foo  (define/public (bar) "bar")) (define foo (new Foo)) (send foo bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('instance-of?', function (): any {
  return it('(instance-of? (new Map) Map)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('instance-of?'),
        [Symbol.for('new'), Symbol.for('Map')],
        Symbol.for('Map'),
      ],
      true,
    ]);
  });
});

describe('is-a?', function (): any {
  return it('(is-a? (new Map) Map)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('is-a?'),
        [Symbol.for('new'), Symbol.for('Map')],
        Symbol.for('Map'),
      ],
      true,
    ]);
  });
});

describe('js-obj', function (): any {
  it('(js-obj)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('js-obj')],
      [Symbol.for('js-obj')],
    ]);
  });
  it('(js-obj "foo" "bar")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('js-obj'), 'foo', 'bar'],
      [Symbol.for('js-obj'), 'foo', 'bar'],
    ]);
  });
  return it('(js-obj "foo" 1 "bar" 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('js-obj'), 'foo', 1, 'bar', 2],
      [Symbol.for('js-obj'), 'foo', 1, 'bar', 2],
    ]);
  });
});

describe('js-keys', function (): any {
  it('(js-keys (js-obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('js-keys'), [Symbol.for('js-obj')]],
      [Symbol.for('quote'), []],
    ]);
  });
  it('(js-keys (js-obj "foo" "bar"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('js-keys'), [Symbol.for('js-obj'), 'foo', 'bar']],
      [Symbol.for('quote'), ['foo']],
    ]);
  });
  return it('(js-keys (js-obj "foo" "bar" "baz" "quux"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('js-keys'),
        [Symbol.for('js-obj'), 'foo', 'bar', 'baz', 'quux'],
      ],
      [Symbol.for('quote'), ['foo', 'baz']],
    ]);
  });
});

describe('js/in', function (): any {
  return it('(let ((obj (js-obj "foo" "bar"))) (js/in "foo" obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj'), 'foo', 'bar']]],
        [Symbol.for('js/in'), 'foo', Symbol.for('obj')],
      ],
      true,
    ]);
  });
});

describe('plist->alist', function (): any {
  it('(plist->alist (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('plist->alist'), [Symbol.for('quote'), []]],
      [Symbol.for('quote'), []],
    ]);
  });
  it('(plist->alist (quote (foo bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(plist->alist (quote (foo bar baz quux)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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

describe('module', function (): any {
  return it('(module foo bar (+ 1 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
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
  return it('(js/try (/ 1 3) (/ 1 2) (catch e (display "there was an error")) (finally (display "finally")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('clj/try', function (): any {
  it('(clj/try (/ 1 2) (catch Exception e "there was an error") (finally (display "finally")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(clj/try (/ 1 3) (/ 1 2) (catch Exception e "there was an error") (finally (display "finally")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('unwind-protect', function (): any {
  return it('(unwind-protect 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('unwind-protect'), 1, 2, 3],
      1,
    ]);
  });
});

describe('call/cc', function (): any {
  it('(+ 5 (call/cc (lambda (x) (* 10 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
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
      undefined,
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
  return it('(+ 5 (call/cc (lambda (x) (x 10) (error "error"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('define-values', function (): any {
  it('((lambda  (define-values (x y) (values 1 2))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define-values (x y) (values 1 2)) (list x y)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('((lambda  (define-values (x y) (values 1 2)) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
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
  return it('(let (x y) (set!-values (x y) (values 1 2)) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('let-values', function (): any {
  it('(let-values (((x y) (values 1 2))) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(let-values (((x . y) (values 1 2))) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('let*-values', function (): any {
  return it('(let*-values (((x y) (values 1 2)) ((w z) (values 3 4))) (list x y w z))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('define-fields', function (): any {
  it('((lambda  (define-fields (x) (js-obj "x" 1)) x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-fields'),
            [Symbol.for('x')],
            [Symbol.for('js-obj'), 'x', 1],
          ],
          Symbol.for('x'),
        ],
      ],
      1,
    ]);
  });
  it('((lambda  (define-fields (foo) (js-obj "foo" "bar")) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-fields'),
            [Symbol.for('foo')],
            [Symbol.for('js-obj'), 'foo', 'bar'],
          ],
          Symbol.for('foo'),
        ],
      ],
      'bar',
    ]);
  });
  return it('((lambda  (define-fields ((foo bar)) (js-obj "foo" "bar")) bar))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        [
          Symbol.for('lambda'),
          [],
          [
            Symbol.for('define-fields'),
            [[Symbol.for('foo'), Symbol.for('bar')]],
            [Symbol.for('js-obj'), 'foo', 'bar'],
          ],
          Symbol.for('bar'),
        ],
      ],
      'bar',
    ]);
  });
});

describe('set!-fields', function (): any {
  return it('((lambda  (let (x) (set!-fields (x) (js-obj "x" 1)) x)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
              [Symbol.for('js-obj'), 'x', 1],
            ],
            Symbol.for('x'),
          ],
        ],
      ],
      1,
    ]);
  });
});

describe('destructuring-bind', function (): any {
  it('(destructuring-bind (x y) (quote (1 2)) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('destructuring-bind'),
        [Symbol.for('x'), Symbol.for('y')],
        [Symbol.for('quote'), [1, 2]],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  return it('(destructuring-bind (x . y) (quote (1 2)) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('destructuring-bind'),
        [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')],
        [Symbol.for('quote'), [1, 2]],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [Symbol.for('quote'), [1, [2]]],
    ]);
  });
});

describe('multiple-values-bind', function (): any {
  return it('(multiple-values-bind (x y) (values 1 2) (list x y))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('multiple-values-bind'),
        [Symbol.for('x'), Symbol.for('y')],
        [Symbol.for('values'), 1, 2],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
});

describe('hash', function (): any {
  it('(hash)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('hash')],
      [Symbol.for('new'), Symbol.for('Map')],
    ]);
  });
  return it('(hash (quote (("foo" . "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('make-hash', function (): any {
  it('(make-hash)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('make-hash')],
      [Symbol.for('new'), Symbol.for('Map')],
    ]);
  });
  return it('(make-hash (quote (("foo" . "bar"))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('hash?', function (): any {
  it('(hash? (make-hash))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('hash?'), [Symbol.for('make-hash')]],
      true,
    ]);
  });
  return it('(hash? 0)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('hash?'), 0],
      false,
    ]);
  });
});

describe('hash-clear', function (): any {
  return it('(hash-clear (make-hash (quote (("foo" . "bar")))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(let ((ht (make-hash (quote (("foo" . "bar")))))) (hash-clear! ht) ht)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('hash-copy', function (): any {
  return it('(hash-copy (make-hash (quote (("foo" . "bar")))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('hash-keys', function (): any {
  return it('(hash-keys (make-hash (quote (("foo" . "bar")))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('hash-values', function (): any {
  return it('(hash-values (make-hash (quote (("foo" . "bar")))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('hash->list', function (): any {
  return it('(hash->list (make-hash (quote (("foo" . "bar")))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
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
  return it('(let ((ht (make-hash))) (hash-set! ht "foo" "bar") ht)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('hash-ref', function (): any {
  it('(hash-ref (make-hash (quote (("foo" . "bar")))) "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(hash-ref (make-hash) "quux" #f)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('hash-ref'), [Symbol.for('make-hash')], 'quux', false],
      false,
    ]);
  });
});

describe('hash-has-key?', function (): any {
  it('(hash-has-key? (make-hash (quote (("foo" . "bar")))) "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(hash-has-key? (make-hash) "quux")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('hash-has-key?'), [Symbol.for('make-hash')], 'quux'],
      false,
    ]);
  });
});

describe('Map', function (): any {
  it('(new Map)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('new'), Symbol.for('Map')],
      [Symbol.for('new'), Symbol.for('Map')],
    ]);
  });
  return it('(~> (new Map (quote ((1 2)))) (send _ entries) (send Array from _))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(+ 2 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('+'), 2, 2],
      4,
    ]);
  });
  it('(+ 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('+'), 1, 2, 3],
      6,
    ]);
  });
  it('(+ 1 2 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('+'), 1, 2, 4],
      7,
    ]);
  });
  it('(+ (+ 1 1) (+ 1 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('+'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 1, 1]],
      4,
    ]);
  });
  it('(let ((x 2)) (+ x x))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('let'),
        [[Symbol.for('x'), 2]],
        [Symbol.for('+'), Symbol.for('x'), Symbol.for('x')],
      ],
      4,
    ]);
  });
  return it('(apply + (quote (1 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('apply'), Symbol.for('+'), [Symbol.for('quote'), [1, 2]]],
      3,
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
  it('(- 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('-'), 1, 2, 3],
      -4,
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
  it('(* 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('*'), 1, 2, 3],
      6,
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
  it('(/ 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('/'), 1, 2, 3],
      [Symbol.for('/'), 1, 2, 3],
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

describe('<', function (): any {
  it('(< 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('<'), 1],
      true,
    ]);
  });
  it('(< 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('<'), 1, 2],
      true,
    ]);
  });
  it('(< 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('<'), 1, 2, 3],
      true,
    ]);
  });
  return it('(< 1 2 0)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('<'), 1, 2, 0],
      false,
    ]);
  });
});

describe('>', function (): any {
  it('(> 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('>'), 1],
      true,
    ]);
  });
  it('(> 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('>'), 2, 1],
      true,
    ]);
  });
  it('(> 3 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('>'), 3, 2, 1],
      true,
    ]);
  });
  return it('(> 0 2 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('>'), 0, 2, 1],
      false,
    ]);
  });
});

describe('range', function (): any {
  it('(range 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('range'), 1, 2],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it('(range 10)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('range'), 10],
      [Symbol.for('quote'), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]],
    ]);
  });
  it('(range 10 20)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('range'), 10, 20],
      [Symbol.for('quote'), [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]],
    ]);
  });
  it('(range 20 40 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('range'), 20, 40, 2],
      [Symbol.for('quote'), [20, 22, 24, 26, 28, 30, 32, 34, 36, 38]],
    ]);
  });
  it('(range 20 10 -1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('range'), 20, 10, -1],
      [Symbol.for('quote'), [20, 19, 18, 17, 16, 15, 14, 13, 12, 11]],
    ]);
  });
  return it('(range 10 15 1.5)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('range'), 10, 15, 1.5],
      [Symbol.for('quote'), [10, 11.5, 13, 14.5]],
    ]);
  });
});

describe('member', function (): any {
  it('(member 2 (list 1 2 3 4))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('member'), 2, [Symbol.for('list'), 1, 2, 3, 4]],
      [Symbol.for('quote'), [2, 3, 4]],
    ]);
  });
  it('(member 9 (list 1 2 3 4))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('member'), 9, [Symbol.for('list'), 1, 2, 3, 4]],
      false,
    ]);
  });
  return it('(member 5 (quote (3 5 1 7 2 9)) (lambda (x y) (< x y)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(member? 2 (list 1 2 3 4))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('member?'), 2, [Symbol.for('list'), 1, 2, 3, 4]],
      true,
    ]);
  });
  return it('(member? 9 (list 1 2 3 4))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('member?'), 9, [Symbol.for('list'), 1, 2, 3, 4]],
      false,
    ]);
  });
});

describe('take', function (): any {
  it('(take (quote (1 2 3 4)) 0)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3, 4]], 0],
      [Symbol.for('quote'), []],
    ]);
  });
  it('(take (quote (1 2 3 4)) 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3, 4]], 1],
      [Symbol.for('quote'), [1]],
    ]);
  });
  return it('(take (quote (1 2 3 4)) 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3, 4]], 2],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
});

describe('drop', function (): any {
  it('(drop (quote (1 2 3 4)) 0)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('drop'), [Symbol.for('quote'), [1, 2, 3, 4]], 0],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  return it('(drop (quote (1 2 3 4)) 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('drop'), [Symbol.for('quote'), [1, 2, 3, 4]], 1],
      [Symbol.for('quote'), [2, 3, 4]],
    ]);
  });
});

describe('drop-right', function (): any {
  it('(drop-right (quote (1 2 3 4)) 0)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('drop-right'), [Symbol.for('quote'), [1, 2, 3, 4]], 0],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  return it('(drop-right (quote (1 2 3 4)) 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('drop-right'), [Symbol.for('quote'), [1, 2, 3, 4]], 1],
      [Symbol.for('quote'), [1, 2, 3]],
    ]);
  });
});

describe('map', function (): any {
  it('(map list (quote (1 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('map'), Symbol.for('list'), [Symbol.for('quote'), [1, 2]]],
      [Symbol.for('quote'), [[1], [2]]],
    ]);
  });
  return it('((lambda  (define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))) (map fact (quote (1 2 3 4 5 6)))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('foldl', function (): any {
  return it('(foldl cons (quote ) (quote (1 2 3 4)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('foldl'),
        Symbol.for('cons'),
        [Symbol.for('quote'), []],
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ],
      [Symbol.for('quote'), [4, 3, 2, 1]],
    ]);
  });
});

describe('foldr', function (): any {
  it('(foldr cons (quote ) (quote (1 2 3 4)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('foldr'),
        Symbol.for('cons'),
        [Symbol.for('quote'), []],
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  return it('(foldr (lambda (v l) (cons (add1 v) l)) (quote ) (quote (1 2 3 4)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('filter', function (): any {
  return it('(filter string? (quote ("foo" 1 2 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('filter'),
        Symbol.for('string?'),
        [Symbol.for('quote'), ['foo', 1, 2, 3]],
      ],
      [Symbol.for('quote'), ['foo']],
    ]);
  });
});

describe('string?', function (): any {
  it('(string? "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string?'), 'foo'],
      true,
    ]);
  });
  it('(string? 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string?'), 1],
      false,
    ]);
  });
  it('(string? (js-obj))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string?'), [Symbol.for('js-obj')]],
      false,
    ]);
  });
  it('(string? (list "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string?'), [Symbol.for('list'), 'foo']],
      false,
    ]);
  });
  it('(string? (js-obj "foo" ""))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string?'), [Symbol.for('js-obj'), 'foo', '']],
      false,
    ]);
  });
  it('(string? (js-obj "foo" (quote )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('string?'),
        [Symbol.for('js-obj'), 'foo', [Symbol.for('quote'), []]],
      ],
      false,
    ]);
  });
  it('(string? (js-obj "foo" (js-obj)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('string?'),
        [Symbol.for('js-obj'), 'foo', [Symbol.for('js-obj')]],
      ],
      false,
    ]);
  });
  it('(string? (js-obj "foo" "foo"))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string?'), [Symbol.for('js-obj'), 'foo', 'foo']],
      false,
    ]);
  });
  return it('(string? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
});

describe('string-length', function (): any {
  return it('(string-length "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-length'), 'foo'],
      3,
    ]);
  });
});

describe('string-append', function (): any {
  it('(string-append)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-append')],
      '',
    ]);
  });
  it('(string-append "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-append'), 'foo'],
      'foo',
    ]);
  });
  it('(string-append "foo" "bar")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-append'), 'foo', 'bar'],
      'foobar',
    ]);
  });
  return it('(apply string-append (quote ("foo" "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('apply'),
        Symbol.for('string-append'),
        [Symbol.for('quote'), ['foo', 'bar']],
      ],
      'foobar',
    ]);
  });
});

describe('string-join', function (): any {
  it('(string-join (quote ("foo" "bar")))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-join'), [Symbol.for('quote'), ['foo', 'bar']]],
      'foo bar',
    ]);
  });
  return it('(string-join (quote ("foo" "bar")) ",")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-join'), [Symbol.for('quote'), ['foo', 'bar']], ','],
      'foo,bar',
    ]);
  });
});

describe('string-split', function (): any {
  it('(string-split "foo bar  baz")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-split'), 'foo bar  baz'],
      [Symbol.for('quote'), ['foo', 'bar', 'baz']],
    ]);
  });
  it('(string-split "foo,bar,baz" ",")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-split'), 'foo,bar,baz', ','],
      [Symbol.for('quote'), ['foo', 'bar', 'baz']],
    ]);
  });
  it('(string-split "foo, bar, baz" ", ")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-split'), 'foo, bar, baz', ', '],
      [Symbol.for('quote'), ['foo', 'bar', 'baz']],
    ]);
  });
  return it(
    '(string-split "foo\n' + 'bar\n' + 'baz" "\n' + '")',
    function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        undefined,
        [Symbol.for('string-split'), 'foo\n' + 'bar\n' + 'baz', '\n'],
        [Symbol.for('quote'), ['foo', 'bar', 'baz']],
      ]);
    }
  );
});

describe('string-trim', function (): any {
  it('(string-trim "  foo bar  baz  ")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-trim'), '  foo bar  baz  '],
      'foo bar  baz',
    ]);
  });
  return it('(string-trim "  foo bar  baz \n' + '\n' + '	")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-trim'), '  foo bar  baz \n' + '\n' + '	'],
      'foo bar  baz',
    ]);
  });
});

describe('string-upcase', function (): any {
  return it('(string-upcase "foo")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-upcase'), 'foo'],
      'FOO',
    ]);
  });
});

describe('string-downcase', function (): any {
  return it('(string-downcase "FOO")', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('string-downcase'), 'FOO'],
      'foo',
    ]);
  });
});

describe('substring', function (): any {
  it('(substring "Apple" 1 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('substring'), 'Apple', 1, 3],
      'pp',
    ]);
  });
  return it('(substring "Apple" 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('substring'), 'Apple', 1],
      'pple',
    ]);
  });
});

describe('as~>', function (): any {
  return it('(as~> 0 _ (+ _ 1) (+ _ 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
});

describe('ann', function (): any {
  return it('(ann #u Any)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('ann'), undefined, Symbol.for('Any')],
      undefined,
    ]);
  });
});

describe('cons?', function (): any {
  return it('(cons? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cons?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
});

describe('list?', function (): any {
  it('(list? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list?'), [Symbol.for('quote'), []]],
      true,
    ]);
  });
  it('(list? (quote (1 . 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      false,
    ]);
  });
  it('(list? (quote (1 2 . 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list?'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]],
      false,
    ]);
  });
  it('(list? (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
      true,
    ]);
  });
  return it('(list? (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      true,
    ]);
  });
});

describe('vector?', function (): any {
  it('(vector? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('vector?'), [Symbol.for('quote'), []]],
      true,
    ]);
  });
  it('(vector? (quote (1 . 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      true,
    ]);
  });
  it('(vector? (quote (1 2 . 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('vector?'),
        [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
      ],
      true,
    ]);
  });
  it('(vector? (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
      true,
    ]);
  });
  return it('(vector? (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('vector?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      true,
    ]);
  });
});

describe('array-list?', function (): any {
  it('(array-list? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('array-list?'), [Symbol.for('quote'), []]],
      true,
    ]);
  });
  it('(array-list? (quote (1 . 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('array-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
      ],
      true,
    ]);
  });
  it('(array-list? (quote (1 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('array-list?'), [Symbol.for('quote'), [1, 2]]],
      true,
    ]);
  });
  return it('(array-list? (quote (1 2 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('array-list?'), [Symbol.for('quote'), [1, 2, 3]]],
      true,
    ]);
  });
});

describe('linked-list?', function (): any {
  it('(linked-list? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('linked-list?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
  it('(linked-list? (quote (1 . 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('linked-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
      ],
      false,
    ]);
  });
  it('(linked-list? (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('linked-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      true,
    ]);
  });
  it('(linked-list? (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('linked-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      true,
    ]);
  });
  return it('(linked-list? (quote (1 2 . (3 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(linked-list-link? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('linked-list-link?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
  it('(linked-list-link? (quote (1 . 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('linked-list-link?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
      ],
      true,
    ]);
  });
  it('(linked-list-link? (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('linked-list-link?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      true,
    ]);
  });
  it('(linked-list-link? (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('linked-list-link?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      true,
    ]);
  });
  return it('(linked-list-link? (quote (1 2 . (3 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(length (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('length'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
      1,
    ]);
  });
  it('(length (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('length'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
  return it('(length (quote (1 2 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('length'),
        [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]],
      ],
      2,
    ]);
  });
});

describe('last', function (): any {
  it('(last (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('last'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]],
      1,
    ]);
  });
  it('(last (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('last'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
  return it('(last (quote (1 2 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('last'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]],
      2,
    ]);
  });
});

describe('nth', function (): any {
  it('(nth 1 (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('nth'),
        1,
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
  return it('(nth 1 (quote (1 2 . (3 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(cdr (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('cdr'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      [Symbol.for('quote'), [2, Symbol.for('.'), []]],
    ]);
  });
  return it('(cdr (quote (1 2 . (3 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote )) (set-car! foo (quote bar)) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('((lambda  (define foo (quote (foo))) (set-car! foo (quote bar)) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote )) (set-cdr! foo (quote (bar))) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (foo))) (set-cdr! foo (quote (bar))) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (foo bar))) (set-cdr! foo (quote (baz))) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (foo bar))) (set-cdr! foo (quote (baz . quux))) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (foo . bar))) (set-cdr! foo (quote (baz))) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (foo . bar))) (set-cdr! foo (quote (baz . quux))) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (foo))) (set-cdr! foo (quote bar)) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (foo bar . baz))) (set-cdr! foo (quote (quux))) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('((lambda  (define foo (quote (foo bar . baz))) (set-cdr! foo (quote quux)) foo))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(equal? (quote (1 2)) (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(dotted-list? (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('dotted-list?'), [Symbol.for('quote'), []]],
      false,
    ]);
  });
  it('(dotted-list? (quote (1 . 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
      ],
      true,
    ]);
  });
  it('(dotted-list? (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      false,
    ]);
  });
  it('(dotted-list? (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      false,
    ]);
  });
  it('(dotted-list? (quote (1 . (2 . 3))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]],
      ],
      true,
    ]);
  });
  it('(dotted-list? (quote (foo . bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(dotted-list? (quote (foo bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      false,
    ]);
  });
});

describe('dotted-list-length', function (): any {
  it('(dotted-list-length (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('dotted-list-length'), [Symbol.for('quote'), []]],
      0,
    ]);
  });
  it('(dotted-list-length (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list-length'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      1,
    ]);
  });
  return it('(dotted-list-length (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list-length'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
});

describe('dotted-list-head', function (): any {
  it('(dotted-list-head (quote (foo . bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(dotted-list-head (quote (foo bar . baz)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(dotted-list-tail (quote (foo . bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(dotted-list-tail (quote (foo bar . baz)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(dotted-list-last (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('dotted-list-last'), [Symbol.for('quote'), []]],
      undefined,
    ]);
  });
  it('(dotted-list-last (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list-last'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      1,
    ]);
  });
  return it('(dotted-list-last (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list-last'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      2,
    ]);
  });
});

describe('dotted-list-last-cdr', function (): any {
  it('(dotted-list-last-cdr (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('dotted-list-last-cdr'), [Symbol.for('quote'), []]],
      [Symbol.for('quote'), []],
    ]);
  });
  it('(dotted-list-last-cdr (quote (1 . )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list-last-cdr'),
        [Symbol.for('quote'), [1, Symbol.for('.'), []]],
      ],
      [Symbol.for('quote'), []],
    ]);
  });
  return it('(dotted-list-last-cdr (quote (1 . (2 . ))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('dotted-list-last-cdr'),
        [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]],
      ],
      [Symbol.for('quote'), []],
    ]);
  });
});

describe('dotted-list->proper-list', function (): any {
  it('(dotted-list->proper-list (quote (foo . bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(dotted-list->proper-list (quote (foo bar . baz)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(proper-list? (quote (foo bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('proper-list?'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      true,
    ]);
  });
  return it('(proper-list? (quote (foo . bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote )) (circular-list? foo) (set-cdr! foo foo) (circular-list? foo)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(circular-list? (quote (foo)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [
        Symbol.for('circular-list?'),
        [Symbol.for('quote'), [Symbol.for('foo')]],
      ],
      false,
    ]);
  });
  it('(circular-list? (quote (foo . bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (foo))) (set-cdr! foo foo) (circular-list? foo)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('((lambda  (define foo (quote (foo . ))) (set-cdr! foo foo) (circular-list? foo)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('((lambda  (define foo (quote (foo bar))) (set-cdr! foo foo) (circular-list? foo)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  it('(proper-list->dotted-list (quote (foo bar)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(proper-list->dotted-list (quote (foo bar baz)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
      undefined,
      [Symbol.for('list*')],
      undefined,
    ]);
  });
  it('(list* 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list*'), 1],
      1,
    ]);
  });
  it('(list* 1 2)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list*'), 1, 2],
      [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
    ]);
  });
  it('(list* 1 2 3)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list*'), 1, 2, 3],
      [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
    ]);
  });
  it('(list* 1 2 3 4)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list*'), 1, 2, 3, 4],
      [Symbol.for('quote'), [1, 2, 3, Symbol.for('.'), 4]],
    ]);
  });
  it('(list* 1 (quote ))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list*'), 1, [Symbol.for('quote'), []]],
      [Symbol.for('quote'), [1]],
    ]);
  });
  it('(list* 1 (quote (2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list*'), 1, [Symbol.for('quote'), [2]]],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  return it('(list* 1 (quote (2 . 3)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('list*'), 1, [Symbol.for('quote'), [2, Symbol.for('.'), 3]]],
      [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]],
    ]);
  });
});

describe('flatten', function (): any {
  it('(flatten (quote (1 2 3 4)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('flatten'), [Symbol.for('quote'), [1, 2, 3, 4]]],
      [Symbol.for('quote'), [1, 2, 3, 4]],
    ]);
  });
  it('(flatten (quote (1 . 2)))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('flatten'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      [Symbol.for('quote'), [1, 2]],
    ]);
  });
  it('(flatten (quote ((a) b (c (d) . e) )))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
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
  return it('(flatten (quote ((((4))))))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('flatten'), [Symbol.for('quote'), [[[[4]]]]]],
      [Symbol.for('quote'), [4]],
    ]);
  });
});

describe('Cons dot', function (): any {
  it('*cons-dot*', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      Symbol.for('*cons-dot*'),
      [Symbol.for('quote'), Symbol.for('.')],
    ]);
  });
  it('(cons-dot)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cons-dot')],
      [Symbol.for('quote'), Symbol.for('.')],
    ]);
  });
  return it('(cons-dot? *cons-dot*)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      undefined,
      [Symbol.for('cons-dot?'), Symbol.for('*cons-dot*')],
      true,
    ]);
  });
});

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
