/**
 * # Interpreter tests
 */

import * as chai from 'chai';

import {
  __,
  LispEnvironment,
  functionp,
  interpret,
  lisp,
  writeToString,
} from '../../src/ts/language';

import { s, sexp } from '../../src/ts/sexp';

import { assertEqual, assertThrows, testLisp, testRepl } from './test-util';

describe('boolean values', function (): any {
  it('true', function (): any {
    testLisp(true, true);
    testLisp(sexp`true`, true);
    testLisp(sexp`t`, true);
    return testLisp(sexp`#t`, true);
  });
  return it('false', function (): any {
    testLisp(false, false);
    testLisp(sexp`false`, false);
    return testLisp(sexp`#f`, false);
  });
});

describe('truep', function (): any {
  it('(truep true)', function (): any {
    return testLisp([Symbol.for('truep'), Symbol.for('true')], true);
  });
  it('(truep false)', function (): any {
    return testLisp([Symbol.for('truep'), Symbol.for('false')], false);
  });
  it('(truep undefined)', function (): any {
    return testLisp([s`truep`, undefined], false);
  });
  it('(truep true)', function (): any {
    return testLisp([s`truep`, s`true`], true);
  });
  it('(truep (quote ()))', function (): any {
    return testLisp([Symbol.for('truep'), [Symbol.for('quote'), []]], false);
  });
  return it("(truep '())", function (): any {
    return testLisp([Symbol.for('truep'), [Symbol.for('quote'), []]], false);
  });
});

describe('falsep', function (): any {
  it('(falsep true)', function (): any {
    return assertEqual(
      interpret([Symbol.for('falsep'), Symbol.for('true')]),
      false
    );
  });
  it('(falsep false)', function (): any {
    return assertEqual(
      interpret([Symbol.for('falsep'), Symbol.for('false')]),
      true
    );
  });
  return it('(falsep undefined)', function (): any {
    return assertEqual(
      interpret([Symbol.for('falsep'), Symbol.for('undefined')]),
      true
    );
  });
});

describe('nil', function (): any {
  xit('nil', function (): any {
    return assertEqual(interpret(sexp`nil`), []);
  });
  xit('(listp nil)', function (): any {
    return assertEqual(interpret(sexp`(listp nil)`), true);
  });
  return xit('(length nil)', function (): any {
    return assertEqual(interpret(sexp`(length nil)`), 0);
  });
});

describe('null', function (): any {
  it('null (JS)', function (): any {
    return testLisp(null, null);
  });
  it('null', function (): any {
    return testLisp(sexp`null`, []);
  });
  it('(listp null)', function (): any {
    return testLisp([Symbol.for('listp'), Symbol.for('null')], true);
  });
  return it('(length null)', function (): any {
    return testLisp([Symbol.for('length'), Symbol.for('null')], 0);
  });
});

describe('js-null', function (): any {
  it('js-null', function (): any {
    return testLisp(sexp`js-null`, null);
  });
  return it('js/null', function (): any {
    return testLisp(sexp`js/null`, null);
  });
});

describe('undefined', function (): any {
  it('undefined', function (): any {
    return testLisp(undefined, undefined);
  });
  xit('(undefined 0)', function (): any {
    return testLisp([Symbol.for('undefined'), 0], undefined);
  });
  return xit('((undefined 0) 0)', function (): any {
    return testLisp([[Symbol.for('undefined'), 0], 0], undefined);
  });
});

describe('numbers', function (): any {
  it('0', function (): any {
    return testLisp(sexp`0`, 0);
  });
  it('1', function (): any {
    return testLisp(sexp`1`, 1);
  });
  return it('2', function (): any {
    return testLisp(sexp`2`, 2);
  });
});

describe('strings', function (): any {
  describe('quotation marks', function (): any {
    return it('"foo"', function (): any {
      return testLisp('foo', 'foo');
    });
  });
  return describe('tabs', function (): any {
    return it('> (eq? "\\t" ...)', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('eq?'), '	', '	'],
        Symbol.for('#t'),
      ]);
    });
  });
});

describe('keywords', function (): any {
  return it(':foo', function (): any {
    return testLisp(Symbol.for(':foo'), Symbol.for(':foo'));
  });
});

describe('cons cells', function (): any {
  it('(cons 1 2)', function (): any {
    return testLisp([Symbol.for('cons'), 1, 2], [1, Symbol.for('.'), 2]);
  });
  it('(cons 1 (cons 2 3))', function (): any {
    return testLisp(
      [Symbol.for('cons'), 1, [Symbol.for('cons'), 2, 3]],
      [1, 2, Symbol.for('.'), 3]
    );
  });
  it("(cons 1 '())", function (): any {
    return testLisp(
      [Symbol.for('cons'), 1, [Symbol.for('quote'), []]],
      sexp`(1)`
    );
  });
  it("(cons 1 '(2))", function (): any {
    return testLisp(
      [Symbol.for('cons'), 1, [Symbol.for('quote'), [2]]],
      [1, 2]
    );
  });
  it("(car '(1 . 2))", function (): any {
    return testLisp(
      [Symbol.for('car'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      1
    );
  });
  it("(cdr '(1 . 2))", function (): any {
    return testLisp(
      [Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]],
      2
    );
  });
  xit('(car (cons 1 2))', function (): any {
    return testLisp([Symbol.for('car'), [Symbol.for('cons'), 1, 2]], 1);
  });
  return xit('(cdr (cons 1 2))', function (): any {
    return testLisp([Symbol.for('cdr'), [Symbol.for('cons'), 1, 2]], 2);
  });
});

describe('lists', function (): any {
  it('(list 1 2)', function (): any {
    return testLisp([Symbol.for('list'), 1, 2], [1, 2]);
  });
  it("(aget '(1 2) 0)", function (): any {
    return testLisp([Symbol.for('aget'), [Symbol.for('quote'), [1, 2]], 0], 1);
  });
  it("(aget '((1 2) (3 4)) 0 1)", function (): any {
    return testLisp(
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
      2
    );
  });
  it("(aref '(1 2) 0)", function (): any {
    return testLisp([Symbol.for('aref'), [Symbol.for('quote'), [1, 2]], 0], 1);
  });
  it("(aset '(1 2) 0 3)", function (): any {
    return testLisp(
      [Symbol.for('aset'), [Symbol.for('quote'), [1, 2]], 0, 3],
      3
    );
  });
  it("(let ((x '(1 2))) (aset x 0 3) x)", function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2]]]],
        [Symbol.for('aset'), Symbol.for('x'), 0, 3],
        Symbol.for('x'),
      ],
      [3, 2]
    );
  });
  return it("(let ((x '(1 2))) (set! (aref x 0) 3) x)", function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2]]]],
        [Symbol.for('set!'), [Symbol.for('aref'), Symbol.for('x'), 0], 3],
        Symbol.for('x'),
      ],
      [3, 2]
    );
  });
});

describe('quote', function (): any {
  it("'foo", function (): any {
    return testLisp(
      [Symbol.for('quote'), Symbol.for('foo')],
      Symbol.for('foo')
    );
  });
  it("'(1)", function (): any {
    return testLisp([Symbol.for('quote'), [1]], [1]);
  });
  it("'(1 2)", function (): any {
    return testLisp([Symbol.for('quote'), [1, 2]], [1, 2]);
  });
  it("'((1 2) (3 4))", function (): any {
    return testLisp(
      [
        Symbol.for('quote'),
        [
          [1, 2],
          [3, 4],
        ],
      ],
      [
        [1, 2],
        [3, 4],
      ]
    );
  });
  it('(quote foo)', function (): any {
    return testLisp(
      [Symbol.for('quote'), Symbol.for('foo')],
      Symbol.for('foo')
    );
  });
  return it('(quote (1))', function (): any {
    return testLisp([Symbol.for('quote'), [1]], [1]);
  });
});

describe('quasiquote', function (): any {
  it('`foo', function (): any {
    return testLisp(sexp`\`foo`, Symbol.for('foo'));
  });
  it('(quasiquote foo)', function (): any {
    return testLisp(
      [Symbol.for('quasiquote'), Symbol.for('foo')],
      Symbol.for('foo')
    );
  });
  it('`(,1)', function (): any {
    return testLisp(
      [Symbol.for('quasiquote'), [[Symbol.for('unquote'), 1]]],
      [1]
    );
  });
  it('`((,1))', function (): any {
    return testLisp(
      [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), 1]]]],
      [[1]]
    );
  });
  return it('`(,@(list 1 2 3))', function (): any {
    return testLisp(
      [
        Symbol.for('quasiquote'),
        [[Symbol.for('unquote-splicing'), [Symbol.for('list'), 1, 2, 3]]],
      ],
      [1, 2, 3]
    );
  });
});

describe('variables', function (): any {
  it('x', function (): any {
    return testLisp(Symbol.for('x'), 2, {
      env: new LispEnvironment([[Symbol.for('x'), 2, 'variable']]),
    });
  });
  it('y', function (): any {
    return testLisp(Symbol.for('y'), undefined, {
      compile: false,
      env: new LispEnvironment([[Symbol.for('x'), 2, 'variable']]),
    });
  });
  it('(begin (setq x 2) x)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [Symbol.for('setq'), Symbol.for('x'), 2],
        Symbol.for('x'),
      ],
      2,
      {
        compile: false,
      }
    );
  });
  it('(begin (define x 2) x)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [Symbol.for('define'), Symbol.for('x'), 2],
        Symbol.for('x'),
      ],
      2
    );
  });
  it('(begin (set! x 2) x)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [Symbol.for('set!'), Symbol.for('x'), 2],
        Symbol.for('x'),
      ],
      2,
      {
        compile: false,
      }
    );
  });
  it('(begin (set! x 2) (set! y 3) (+ x y))', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [Symbol.for('set!'), Symbol.for('x'), 2],
        [Symbol.for('set!'), Symbol.for('y'), 3],
        [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
      ],
      5,
      {
        compile: false,
        env: new LispEnvironment([
          [
            Symbol.for('+'),
            function (x: any, y: any): any {
              return x + y;
            },
            'function',
          ],
        ]),
      }
    );
  });
  return xit('(setq a 1 b 2 c 3)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('setq'),
          Symbol.for('a'),
          1,
          Symbol.for('b'),
          2,
          Symbol.for('c'),
          3,
        ],
        [Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')],
      ],
      [1, 2, 3],
      {
        compile: false,
      }
    );
  });
});

describe('function calls', function (): any {
  it('(identity "foo")', function (): any {
    return testLisp([Symbol.for('identity'), 'foo'], 'foo', {
      env: new LispEnvironment([
        [
          Symbol.for('identity'),
          function (x: any): any {
            return x;
          },
          'function',
        ],
      ]),
    });
  });
  xit('((add _ 2 3) 1)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [
            [[[Symbol.for('my-add')], Symbol.for('x')], Symbol.for('y')],
            Symbol.for('z'),
          ],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        ],
        [[Symbol.for('my-add'), Symbol.for('_'), 2, 3], 1],
      ],
      6
    );
  });
  xit('((my-add _ _ _) 1 2 3)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [
            [[[Symbol.for('my-add')], Symbol.for('x')], Symbol.for('y')],
            Symbol.for('z'),
          ],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        ],
        [
          [
            Symbol.for('my-add'),
            Symbol.for('_'),
            Symbol.for('_'),
            Symbol.for('_'),
          ],
          1,
          2,
          3,
        ],
      ],
      6
    );
  });
  xit('(((add _ _ 3) 1) 2)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [
            [[[Symbol.for('my-add')], Symbol.for('x')], Symbol.for('y')],
            Symbol.for('z'),
          ],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        ],
        [[[Symbol.for('my-add'), Symbol.for('_'), Symbol.for('_'), 3], 1], 2],
      ],
      6
    );
  });
  return xit('((add _ _ 3) 1 2)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [
            [[[Symbol.for('my-add')], Symbol.for('x')], Symbol.for('y')],
            Symbol.for('z'),
          ],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        ],
        [[Symbol.for('my-add'), Symbol.for('_'), Symbol.for('_'), 3], 1, 2],
      ],
      6
    );
  });
});

describe('define', function (): any {
  it('(begin (define (add x y) (+ x y)) (add 2 3))', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [Symbol.for('add'), Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
        ],
        [Symbol.for('add'), 2, 3],
      ],
      5,
      {
        env: new LispEnvironment([
          [
            Symbol.for('+'),
            function (x: any, y: any): any {
              return x + y;
            },
            'function',
          ],
        ]),
      }
    );
  });
  it('(begin (define (add x y) (+ x y)) (add 2 3))', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [Symbol.for('add'), Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
        ],
        [Symbol.for('add'), 2, 3],
      ],
      5,
      {
        env: new LispEnvironment([
          [
            Symbol.for('+'),
            function (x: any, y: any): any {
              return x + y;
            },
            'function',
          ],
        ]),
      }
    );
  });
  it('(define ((my-add x) y) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [[Symbol.for('my-add'), Symbol.for('x')], Symbol.for('y')],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
        ],
        [Symbol.for('my-add'), 2, 3],
      ],
      5
    );
  });
  xit('(define ((my-add x) y) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [[Symbol.for('my-add'), Symbol.for('x')], Symbol.for('y')],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
        ],
        [[Symbol.for('my-add'), 2], 3],
      ],
      5
    );
  });
  xit('(define (((add) x) y) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [[[Symbol.for('my-add')], Symbol.for('x')], Symbol.for('y')],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
        ],
        [[[Symbol.for('my-add')], 2], 3],
      ],
      5
    );
  });
  it('(define ((add x y) z) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [
            [Symbol.for('my-add'), Symbol.for('x'), Symbol.for('y')],
            Symbol.for('z'),
          ],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        ],
        [Symbol.for('my-add'), 1, 2, 3],
      ],
      6
    );
  });
  // (test-lisp
  //  '(begin
  //     (define ((my-add x y) z)
  //       (+ x y z))
  //     ((my-add 1 2) 3))
  //  6)
  // (test-lisp
  //  '(begin
  //     (define ((my-add x y) z)
  //       (+ x y z))
  //     ((my-add 1) 2 3))
  //  6)
  // (test-lisp
  //  '(begin
  //     (define ((my-add x y) z)
  //       (+ x y z))
  //     (((my-add 1) 2) 3))
  //  6)
  // (test-lisp
  //  '(begin
  //     (define ((my-add x y) z)
  //       (+ x y z))
  //     ((((my-add) 1) 2) 3))
  //  6)
  // (test-lisp
  //  '(begin
  //     (define ((((my-add) x) y) z)
  //       (+ x y z))
  //     ((((my-add) 1) 2) 3))
  //  6)
  return it('> (define x 1)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('define'), Symbol.for('x'), 1],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('x'),
      1,
    ]);
  });
});

describe('defun', function (): any {
  xit('(begin (defun add (x y) (+ x y)) (add 2 3))', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defun'),
          Symbol.for('add'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
        ],
        [Symbol.for('add'), 2, 3],
      ],
      5,
      {
        env: new LispEnvironment([
          [
            Symbol.for('+'),
            function (x: any, y: any): any {
              return x + y;
            },
            'function',
          ],
        ]),
      },
      {
        verbose: true,
      }
    );
  });
  return xit('(begin (defun add (x y) (+ x y)) (add 2 3))', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defun'),
          Symbol.for('add'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')],
        ],
        [Symbol.for('add'), 2, 3],
      ],
      5,
      {
        env: new LispEnvironment([
          [
            Symbol.for('+'),
            function (x: any, y: any): any {
              return x + y;
            },
            'function',
          ],
        ]),
      }
    );
  });
});

describe('defmacro', function (): any {
  xit('(defmacro my-macro (x) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defmacro'),
          Symbol.for('my-macro'),
          [Symbol.for('x')],
          [
            Symbol.for('quasiquote'),
            [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('x')]],
          ],
        ],
        [Symbol.for('my-macro'), 1],
      ],
      1,
      {
        compile: false,
      }
    );
  });
  xit('(defmacro my-macro (&environment env) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defmacro'),
          Symbol.for('my-macro'),
          [Symbol.for('&environment'), Symbol.for('env')],
          [
            Symbol.for('send'),
            Symbol.for('env'),
            Symbol.for('has'),
            [Symbol.for('quote'), Symbol.for('+')],
          ],
        ],
        [Symbol.for('my-macro')],
      ],
      true,
      {
        compile: false,
        env: new LispEnvironment([
          [
            Symbol.for('+'),
            function (x: any, y: any): any {
              return x + y;
            },
            'function',
          ],
        ]),
      }
    );
  });
  xit('(defmacro my-macro (&environment env-arg) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defmacro'),
          Symbol.for('my-macro'),
          [Symbol.for('&environment'), Symbol.for('env-arg')],
          [
            Symbol.for('send'),
            Symbol.for('env-arg'),
            Symbol.for('has'),
            [Symbol.for('quote'), Symbol.for('+')],
          ],
        ],
        [Symbol.for('my-macro')],
      ],
      true,
      {
        compile: false,
        env: new LispEnvironment([
          [
            Symbol.for('+'),
            function (x: any, y: any): any {
              return x + y;
            },
            'function',
          ],
        ]),
      }
    );
  });
  return xit('(defmacro (my-macro x) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defmacro'),
          [Symbol.for('my-macro'), Symbol.for('x')],
          [
            Symbol.for('quasiquote'),
            [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('x')]],
          ],
        ],
        [Symbol.for('my-macro'), 1],
      ],
      1,
      {
        compile: false,
      }
    );
  });
});

describe('define-macro', function (): any {
  xit('(define-macro (my-macro x) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define-macro'),
          [Symbol.for('my-macro'), Symbol.for('x')],
          [
            Symbol.for('quasiquote'),
            [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('x')]],
          ],
        ],
        [Symbol.for('my-macro'), 1],
      ],
      1,
      {
        compile: false,
      }
    );
  });
  return xit('(define-macro my-macro (x) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define-macro'),
          Symbol.for('my-macro'),
          [Symbol.for('x')],
          [
            Symbol.for('quasiquote'),
            [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('x')]],
          ],
        ],
        [Symbol.for('my-macro'), 1],
      ],
      1,
      {
        compile: false,
      }
    );
  });
});

describe('let', function (): any {
  it('(let ((x 0)) x)', function (): any {
    return testLisp(
      [Symbol.for('let'), [[Symbol.for('x'), 0]], Symbol.for('x')],
      0
    );
  });
  it('(let ((x 1)) x)', function (): any {
    return testLisp(
      [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
      1
    );
  });
  it('(let ((x 1)) (let ((y 2)) x))', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('x'), 1]],
        [Symbol.for('let'), [[Symbol.for('y'), 2]], Symbol.for('x')],
      ],
      1
    );
  });
  it("(let ((x '((1 2) (3 4)))) x)", function (): any {
    return testLisp(
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
        [1, 2],
        [3, 4],
      ]
    );
  });
  it('(let (x) (set! x 1) x)', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [Symbol.for('x')],
        [Symbol.for('set!'), Symbol.for('x'), 1],
        Symbol.for('x'),
      ],
      1
    );
  });
  it('(let (x) (set! x 1) (set! x 2) x)', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [Symbol.for('x')],
        [Symbol.for('set!'), Symbol.for('x'), 1],
        [Symbol.for('set!'), Symbol.for('x'), 2],
        Symbol.for('x'),
      ],
      2
    );
  });
  it('(let (x))', function (): any {
    return testLisp([Symbol.for('let'), [Symbol.for('x')]], undefined);
  });
  it('(let ((a 1)) (+ (let ((a 2)) a) a))', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('a'), 1]],
        [
          Symbol.for('+'),
          [Symbol.for('let'), [[Symbol.for('a'), 2]], Symbol.for('a')],
          Symbol.for('a'),
        ],
      ],
      3
    );
  });
  it('(let* ((x 1)) x)', function (): any {
    return testLisp(
      [Symbol.for('let*'), [[Symbol.for('x'), 1]], Symbol.for('x')],
      1
    );
  });
  return it('(let ((compose ...) ...) ...)', function (): any {
    return testLisp(
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
      36
    );
  });
});

describe('lambda', function (): any {
  it('((lambda (x) x) 1)', function (): any {
    return testLisp(
      [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 1],
      1
    );
  });
  it('((lambda (x) x) "Lisp")', function (): any {
    return testLisp(
      [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 'Lisp'],
      'Lisp'
    );
  });
  it('((lambda x x) "Lisp")', function (): any {
    return testLisp(
      [[Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')], 'Lisp'],
      ['Lisp']
    );
  });
  it('((fn (x) x) 1)', function (): any {
    return testLisp(
      [[Symbol.for('fn'), [Symbol.for('x')], Symbol.for('x')], 1],
      1
    );
  });
  it('((λ (x) x) 1)', function (): any {
    return testLisp(
      [[Symbol.for('λ'), [Symbol.for('x')], Symbol.for('x')], 1],
      1
    );
  });
  it('(fn (x) x))', function (): any {
    return assertEqual(
      functionp(
        interpret([Symbol.for('fn'), [Symbol.for('x')], Symbol.for('x')])
      ),
      true
    );
  });
  return it('(<fn> 1 1)', function (): any {
    return testLisp(
      [
        function (x: any, y: any): any {
          return x + y;
        },
        1,
        1,
      ],
      2,
      {
        wrapParens: true,
      }
    );
  });
});

describe('nlambda', function (): any {
  xit('(define f (nlambda ...))', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('setq'),
          Symbol.for('a'),
          1,
          Symbol.for('b'),
          2,
          Symbol.for('c'),
          3,
        ],
        [
          Symbol.for('define'),
          Symbol.for('f'),
          [
            Symbol.for('nlambda'),
            [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
            [
              Symbol.for('list'),
              Symbol.for('x'),
              Symbol.for('y'),
              Symbol.for('z'),
            ],
          ],
        ],
        [Symbol.for('f'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')],
      ],
      [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')],
      {
        compile: false,
      }
    );
  });
  return xit('((nlambda ...) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('setq'),
          Symbol.for('a'),
          1,
          Symbol.for('b'),
          2,
          Symbol.for('c'),
          3,
        ],
        [
          [
            Symbol.for('nlambda'),
            [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
            [
              Symbol.for('list'),
              Symbol.for('x'),
              Symbol.for('y'),
              Symbol.for('z'),
            ],
          ],
          Symbol.for('a'),
          Symbol.for('b'),
          Symbol.for('c'),
        ],
      ],
      [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')],
      {
        compile: false,
      }
    );
  });
});

describe('lexical scope', function (): any {
  it('lexical scope 1', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [Symbol.for('K'), Symbol.for('x')],
          [Symbol.for('lambda'), [], Symbol.for('x')],
        ],
        [[Symbol.for('K'), 42]],
      ],
      42
    );
  });
  it('lexical scope 2', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          Symbol.for('incrementer'),
          Symbol.for('undefined'),
        ],
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
      2
    );
  });
  return it('lexical scope 3', function (): any {
    return testLisp(
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
      100
    );
  });
});

describe('begin', function (): any {
  return it('(begin)', function (): any {
    return testLisp([Symbol.for('begin')], undefined);
  });
});

describe('begin0', function (): any {
  return it('(begin0 1 2)', function (): any {
    return testLisp([Symbol.for('begin0'), 1, 2], 1);
  });
});

describe('if', function (): any {
  it('(if true 1 2)', function (): any {
    return testLisp([Symbol.for('if'), Symbol.for('true'), 1, 2], 1);
  });
  it('(if (< 1 2) 1 2)', function (): any {
    return testLisp([Symbol.for('if'), [Symbol.for('<'), 1, 2], 1, 2], 1);
  });
  it('(if (> 2 1) 1 2)', function (): any {
    return testLisp([Symbol.for('if'), [Symbol.for('>'), 2, 1], 1, 2], 1);
  });
  it('(if false 1 2)', function (): any {
    return testLisp([Symbol.for('if'), Symbol.for('false'), 1, 2], 2);
  });
  return xit('(if "" 1 2)', function (): any {
    return testLisp([Symbol.for('if'), '', 1, 2], 1);
  });
});

describe('when', function (): any {
  it('(when (< 1 2) 1 2)', function (): any {
    return testLisp([Symbol.for('when'), [Symbol.for('<'), 1, 2], 1, 2], 2);
  });
  return it('(when (> 1 2) 1 2)', function (): any {
    return testLisp(
      [Symbol.for('when'), [Symbol.for('>'), 1, 2], 1, 2],
      undefined
    );
  });
});

describe('unless', function (): any {
  it('(unless (< 1 2) 1 2)', function (): any {
    return testLisp(
      [Symbol.for('unless'), [Symbol.for('<'), 1, 2], 1, 2],
      undefined
    );
  });
  return it('(unless (> 1 2) 1 2)', function (): any {
    return testLisp([Symbol.for('unless'), [Symbol.for('>'), 1, 2], 1, 2], 2);
  });
});

describe('cond', function (): any {
  it('(cond (#f 1) (else 2))', function (): any {
    return testLisp(
      [Symbol.for('cond'), [Symbol.for('#f'), 1], [Symbol.for('else'), 2]],
      2
    );
  });
  it('(cond (true 1) (false 2))', function (): any {
    return testLisp(
      [Symbol.for('cond'), [Symbol.for('true'), 1], [Symbol.for('false'), 2]],
      1
    );
  });
  it('(cond (false 1) (true 2))', function (): any {
    return testLisp(
      [Symbol.for('cond'), [Symbol.for('false'), 1], [Symbol.for('true'), 2]],
      2
    );
  });
  return it('(cond (false 1) (t 2))', function (): any {
    return testLisp(
      [Symbol.for('cond'), [Symbol.for('false'), 1], [Symbol.for('t'), 2]],
      2
    );
  });
});

describe('js/switch', function (): any {
  return it('(js/switch x (case "foo" ...) (default ...))', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
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
      'baz'
    );
  });
});

describe('eq?', function (): any {
  it("(eq (my-unit 'foo) 'foo)", function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [Symbol.for('my-unit'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        [Symbol.for('my-unit'), [Symbol.for('quote'), Symbol.for('foo')]],
      ],
      Symbol.for('foo')
    );
  });
  it("(eq (my-curried-unit '_) '_)", function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [[Symbol.for('my-curried-unit')], Symbol.for('x')],
          Symbol.for('x'),
        ],
        [Symbol.for('my-curried-unit'), [Symbol.for('quote'), Symbol.for('_')]],
      ],
      Symbol.for('_')
    );
  });
  it("(eq '_ '_)", function (): any {
    return testLisp(
      [
        Symbol.for('eq'),
        [Symbol.for('quote'), Symbol.for('_')],
        [Symbol.for('quote'), Symbol.for('_')],
      ],
      true
    );
  });
  xit("(assert (eq '_ '_))", function (): any {
    return testLisp(
      [
        Symbol.for('assert'),
        [
          Symbol.for('eq'),
          [Symbol.for('quote'), Symbol.for('_')],
          [Symbol.for('quote'), Symbol.for('_')],
        ],
      ],
      true
    );
  });
  return xit("(assert (not (eq _ '_)))", function (): any {
    return testLisp(
      [
        Symbol.for('assert'),
        [
          Symbol.for('not'),
          [
            Symbol.for('eq'),
            Symbol.for('_'),
            [Symbol.for('quote'), Symbol.for('_')],
          ],
        ],
      ],
      true
    );
  });
});

describe('equal?', function (): any {
  return xit("(assert (not (equal _ '_)))", function (): any {
    return testLisp(
      [
        Symbol.for('assert'),
        [
          Symbol.for('not'),
          [
            Symbol.for('equal'),
            Symbol.for('_'),
            [Symbol.for('quote'), Symbol.for('_')],
          ],
        ],
      ],
      true
    );
  });
});

describe('and', function (): any {
  it('(and)', function (): any {
    return testLisp([Symbol.for('and')], true);
  });
  it('(and false false)', function (): any {
    return testLisp(
      [Symbol.for('and'), Symbol.for('false'), Symbol.for('false')],
      false
    );
  });
  it('(and false true)', function (): any {
    return testLisp(
      [Symbol.for('and'), Symbol.for('false'), Symbol.for('true')],
      false
    );
  });
  return it('(and true true)', function (): any {
    return testLisp(
      [Symbol.for('and'), Symbol.for('false'), Symbol.for('true')],
      false
    );
  });
});

describe('or', function (): any {
  it('(or)', function (): any {
    return testLisp([Symbol.for('or')], false);
  });
  it('(or false false)', function (): any {
    return testLisp(
      [Symbol.for('or'), Symbol.for('false'), Symbol.for('false')],
      false
    );
  });
  it('(or false true)', function (): any {
    return testLisp(
      [Symbol.for('or'), Symbol.for('false'), Symbol.for('true')],
      true
    );
  });
  it('(or 1 2)', function (): any {
    return testLisp([Symbol.for('or'), 1, 2], 1);
  });
  return it('(or undefined 2)', function (): any {
    testLisp([Symbol.for('or'), Symbol.for('undefined'), 2], 2);
    return testLisp([Symbol.for('or'), undefined, 2], 2);
  });
});

describe('while', function (): any {
  return it('(let ... (while ...) ...)', function (): any {
    return testLisp(
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
      [1, 1, 1]
    );
  });
});

describe('for', function (): any {
  it('(let ... (for ...) ...)', function (): any {
    return testLisp(
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
      [3, 2, 1]
    );
  });
  xit('(let ... (for ((x ...) (y ...)) ...) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('result'), [Symbol.for('quote'), []]]],
        [
          Symbol.for('for'),
          [
            [Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]],
            [Symbol.for('y'), [Symbol.for('quote'), [4, 5, 6]]],
          ],
          [
            Symbol.for('set!'),
            Symbol.for('result'),
            [Symbol.for('cons'), Symbol.for('x'), Symbol.for('result')],
          ],
          [
            Symbol.for('set!'),
            Symbol.for('result'),
            [Symbol.for('cons'), Symbol.for('y'), Symbol.for('result')],
          ],
        ],
        Symbol.for('result'),
      ],
      [6, 3, 5, 2, 4, 1]
    );
  });
  it('(for ((i (range 0 len))) ...)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('len'),
        [Symbol.for('length'), Symbol.for('foo')],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('for'),
        [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('len')]]],
        [Symbol.for('pop-right!'), Symbol.for('foo')],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [Symbol.for('quote'), []],
    ]);
  });
  return it('(for ((i (range 0 (length foo)))) ...)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('for'),
        [
          [
            Symbol.for('i'),
            [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('foo')]],
          ],
        ],
        [Symbol.for('pop-right!'), Symbol.for('foo')],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [Symbol.for('quote'), []],
    ]);
  });
});

describe('break', function (): any {
  it('(begin (while #t (break) 1))', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [Symbol.for('while'), Symbol.for('#t'), [Symbol.for('break')]],
        1,
      ],
      1,
      {
        compile: true,
      }
    );
  });
  return it('(let ... (for ...) ...)', function (): any {
    return testLisp(
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
      [],
      {
        compile: true,
      }
    );
  });
});

describe('continue', function (): any {
  it('(begin (while #t (break) 1))', function (): any {
    return testLisp(
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
      [5, 6, 7, 8, 9, 10]
    );
  });
  return it('(let ... (for ...) ...)', function (): any {
    return testLisp(
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
      [5, 6, 7, 8, 9, 10]
    );
  });
});

describe('return', function (): any {
  it('((lambda () (return 1) 2))', function (): any {
    return testLisp(
      [[Symbol.for('lambda'), [], [Symbol.for('return'), 1], 2]],
      1
    );
  });
  it('((js/function () (return 1) 2))', function (): any {
    return testLisp(
      [[Symbol.for('js/function'), [], [Symbol.for('return'), 1], 2]],
      1
    );
  });
  return it('((js/arrow () (return 1) 2))', function (): any {
    return testLisp(
      [[Symbol.for('js/arrow'), [], [Symbol.for('return'), 1], 2]],
      1
    );
  });
});

describe('get-field', function (): any {
  return it('(get-field foo obj)', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj'), 'foo', 'bar']]],
        [Symbol.for('get-field'), Symbol.for('foo'), Symbol.for('obj')],
      ],
      'bar'
    );
  });
});

describe('set-field!', function (): any {
  return it('(set-field! foo obj "bar)', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj')]]],
        [Symbol.for('set-field!'), Symbol.for('foo'), Symbol.for('obj'), 'bar'],
        Symbol.for('obj'),
      ],
      {
        foo: 'bar',
      }
    );
  });
});

describe('field-bound?', function (): any {
  return it('(field-bound? foo obj)', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj'), 'foo', 'bar']]],
        [Symbol.for('field-bound?'), Symbol.for('foo'), Symbol.for('obj')],
      ],
      true
    );
  });
});

describe('oget', function (): any {
  it('(oget obj "prop")', function (): any {
    return testLisp([Symbol.for('oget'), Symbol.for('obj'), 'prop'], 'foo', {
      env: new LispEnvironment([
        [
          Symbol.for('obj'),
          {
            prop: 'foo',
          },
          'variable',
        ],
      ]),
    });
  });
  return it('(oget _ "@@functional/placeholder")', function (): any {
    return testLisp(
      [Symbol.for('oget'), Symbol.for('_'), '@@functional/placeholder'],
      true
    );
  });
});

describe('send', function (): any {
  xit("(send obj 'add 1 1)", function (): any {
    return testLisp(
      [
        Symbol.for('send'),
        Symbol.for('obj'),
        [Symbol.for('quote'), Symbol.for('add')],
        1,
        1,
      ],
      2,
      {
        compile: false,
        env: new LispEnvironment([
          [
            Symbol.for('obj'),
            {
              add: function (x: any, y: any): any {
                return x + y;
              },
            },
            'variable',
          ],
        ]),
      }
    );
  });
  it('(send obj add 1 1)', function (): any {
    return testLisp(
      [Symbol.for('send'), Symbol.for('obj'), Symbol.for('add'), 1, 1],
      2,
      {
        compile: false,
        env: new LispEnvironment([
          [
            Symbol.for('obj'),
            {
              add: function (x: any, y: any): any {
                return x + y;
              },
            },
            'variable',
          ],
        ]),
      }
    );
  });
  it('(send (make-hash \'(("foo" . "foo"))) has "foo")', function (): any {
    return testLisp(
      [
        Symbol.for('send'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]],
        ],
        Symbol.for('has'),
        'foo',
      ],
      true
    );
  });
  return it('(send (make-hash \'(("foo" . "foo"))) has \'("foo"))', function (): any {
    return testLisp(
      [
        Symbol.for('send'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]],
        ],
        Symbol.for('has'),
        [Symbol.for('quote'), ['foo']],
      ],
      false
    );
  });
});

describe('send/apply', function (): any {
  return it('(send/apply (make-hash \'(("foo" . "foo"))) has \'("foo"))', function (): any {
    return testLisp(
      [
        Symbol.for('send/apply'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]],
        ],
        Symbol.for('has'),
        [Symbol.for('quote'), ['foo']],
      ],
      true
    );
  });
});

describe('dot', function (): any {
  it('(. obj add1 1)', function (): any {
    return testLisp(
      [Symbol.for('.'), Symbol.for('obj'), Symbol.for('add1'), 1],
      2,
      {
        compile: false,
        env: new LispEnvironment([
          [
            Symbol.for('obj'),
            {
              add1: function (x: any): any {
                return x + 1;
              },
            },
            'variable',
          ],
        ]),
      }
    );
  });
  it('(.add1 obj 1)', function (): any {
    return testLisp([Symbol.for('.add1'), Symbol.for('obj'), 1], 2, {
      compile: false,
      env: new LispEnvironment([
        [
          Symbol.for('obj'),
          {
            add1: function (x: any): any {
              return x + 1;
            },
          },
          'variable',
        ],
      ]),
    });
  });
  it('(.add obj 1 1)', function (): any {
    return testLisp([Symbol.for('.add'), Symbol.for('obj'), 1, 1], 2, {
      compile: false,
      env: new LispEnvironment([
        [
          Symbol.for('obj'),
          {
            add: function (x: any, y: any): any {
              return x + y;
            },
          },
          'variable',
        ],
      ]),
    });
  });
  it('(.-prop obj)', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj')]]],
        [Symbol.for('set!'), [Symbol.for('.-prop'), Symbol.for('obj')], 'bar'],
        [Symbol.for('.-prop'), Symbol.for('obj')],
      ],
      'bar'
    );
  });
  return it('(.-prop obj) 2', function (): any {
    return testLisp([Symbol.for('.-prop'), Symbol.for('obj')], 'foo', {
      compile: false,
      env: new LispEnvironment([
        [
          Symbol.for('obj'),
          {
            prop: 'foo',
          },
          'variable',
        ],
      ]),
    });
  });
});

describe('new', function (): any {
  it('(new (class ...))', function (): any {
    return testLisp(
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
        [Symbol.for('.bar'), Symbol.for('quux')],
      ],
      'baz'
    );
  });
  it('(new (class ...)) with constructor', function (): any {
    return testLisp(
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
      {
        compile: false,
      }
    );
  });
  return it('(new (class ...)) extending Object', function (): any {
    return testLisp(
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
              [Symbol.for('Object')],
              [Symbol.for('define/public'), Symbol.for('val'), 1],
              [
                Symbol.for('define'),
                [Symbol.for('constructor'), Symbol.for('x')],
                [
                  Symbol.for('set!'),
                  [Symbol.for('.-val'), Symbol.for('this')],
                  Symbol.for('x'),
                ],
              ],
              [
                Symbol.for('define/public'),
                [Symbol.for('bar')],
                [Symbol.for('.-val'), Symbol.for('this')],
              ],
            ],
            2,
          ],
        ],
        [Symbol.for('.bar'), Symbol.for('quux')],
      ],
      2,
      {
        compile: false,
        env: new LispEnvironment([[Symbol.for('Object'), Object, 'function']]),
      }
    );
  });
});

describe('class', function (): any {
  it('(define Foo (class ...))', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
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
          Symbol.for('set!'),
          Symbol.for('quux'),
          [Symbol.for('new'), Symbol.for('Foo')],
        ],
        [Symbol.for('.bar'), Symbol.for('quux')],
      ],
      'baz',
      {
        compile: false,
      }
    );
  });
  it('(defclass Foo ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defclass'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define/public'), [Symbol.for('bar')], 'baz'],
        ],
        [
          Symbol.for('set!'),
          Symbol.for('quux'),
          [Symbol.for('new'), Symbol.for('Foo')],
        ],
        [Symbol.for('.bar'), Symbol.for('quux')],
      ],
      'baz',
      {
        compile: false,
      }
    );
  });
  it('(defclass Foo ...) with property', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defclass'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define'), Symbol.for('bar'), 'baz'],
        ],
        [
          Symbol.for('set!'),
          Symbol.for('quux'),
          [Symbol.for('new'), Symbol.for('Foo')],
        ],
        [Symbol.for('.-bar'), Symbol.for('quux')],
      ],
      'baz',
      {
        compile: false,
      }
    );
  });
  it('(defclass Foo ...) with constructor', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defclass'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define'), Symbol.for('x')],
          [
            Symbol.for('define'),
            [Symbol.for('constructor'), Symbol.for('x')],
            [
              Symbol.for('set!'),
              [Symbol.for('.-x'), Symbol.for('this')],
              Symbol.for('x'),
            ],
          ],
          [
            Symbol.for('define'),
            [Symbol.for('bar')],
            [Symbol.for('.-x'), Symbol.for('this')],
          ],
        ],
        [
          Symbol.for('set!'),
          Symbol.for('quux'),
          [Symbol.for('new'), Symbol.for('Foo'), 'xyzzy'],
        ],
        [Symbol.for('.bar'), Symbol.for('quux')],
      ],
      'xyzzy',
      {
        compile: false,
      }
    );
  });
  xit('(defclass Foo ...) with constructor and new', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defclass'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define'), Symbol.for('x')],
          [
            Symbol.for('define'),
            [Symbol.for('constructor'), Symbol.for('x')],
            [
              Symbol.for('set!'),
              [Symbol.for('.-x'), Symbol.for('this')],
              Symbol.for('x'),
            ],
          ],
          [
            Symbol.for('define'),
            [Symbol.for('bar')],
            [Symbol.for('.-x'), Symbol.for('this')],
          ],
        ],
        [
          Symbol.for('set!'),
          Symbol.for('quux'),
          [Symbol.for('new'), Symbol.for('Foo'), [Symbol.for('x'), 'xyzzy']],
        ],
        [Symbol.for('.bar'), Symbol.for('quux')],
      ],
      'xyzzy',
      {
        compile: false,
      }
    );
  });
  xit('(defclass Foo ...) with no arguments', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defclass'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define'), Symbol.for('x'), 'wobble'],
          [
            Symbol.for('define'),
            [Symbol.for('constructor'), Symbol.for('x')],
            [
              Symbol.for('set!'),
              [Symbol.for('.-x'), Symbol.for('this')],
              Symbol.for('x'),
            ],
          ],
          [
            Symbol.for('define'),
            [Symbol.for('bar')],
            [Symbol.for('.-x'), Symbol.for('this')],
          ],
        ],
        [
          Symbol.for('set!'),
          Symbol.for('quux'),
          [Symbol.for('new'), Symbol.for('Foo')],
        ],
        [Symbol.for('.bar'), Symbol.for('quux')],
      ],
      'wobble',
      {
        compile: false,
      }
    );
  });
  return it('(defclass Foo (Object) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('defclass'),
          Symbol.for('Foo'),
          [Symbol.for('Object')],
          [Symbol.for('define'), [Symbol.for('bar')], 'baz'],
        ],
        [
          Symbol.for('set!'),
          Symbol.for('quux'),
          [Symbol.for('new'), Symbol.for('Foo')],
        ],
        [Symbol.for('.bar'), Symbol.for('quux')],
      ],
      'baz',
      {
        compile: false,
        env: new LispEnvironment([[Symbol.for('Object'), Object, 'function']]),
      }
    );
  });
});

describe('js-obj', function (): any {
  it('(js-obj)', function (): any {
    return testLisp(
      [Symbol.for('js-obj')],
      {},
      {
        wrapParens: true,
      }
    );
  });
  it('(js-obj "foo" "bar")', function (): any {
    return testLisp(
      [Symbol.for('js-obj'), 'foo', 'bar'],
      {
        foo: 'bar',
      },
      {
        wrapParens: true,
      }
    );
  });
  return it('(js-obj "foo" 1 "bar" 2)', function (): any {
    return testLisp(
      [Symbol.for('js-obj'), 'foo', 1, 'bar', 2],
      {
        foo: 1,
        bar: 2,
      },
      {
        wrapParens: true,
      }
    );
  });
});

describe('js/in', function (): any {
  return it('(js/in "foo" (js-obj "foo" "bar"))', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('obj'), [Symbol.for('js-obj'), 'foo', 'bar']]],
        [Symbol.for('js/in'), 'foo', Symbol.for('obj')],
      ],
      true
    );
  });
});

describe('module', function (): any {
  return it('(module foo bar (+ 1 1))', function (): any {
    return testLisp(
      [
        Symbol.for('module'),
        Symbol.for('foo'),
        Symbol.for('bar'),
        [Symbol.for('+'), 1, 1],
      ],
      2
    );
  });
});

describe('error', function (): any {
  it('(error)', function (): any {
    return assertThrows(function (): any {
      return interpret([Symbol.for('error')], new LispEnvironment());
    });
  });
  return it('(error "foo")', function (): any {
    return assertThrows(function (): any {
      return interpret([Symbol.for('error'), 'foo'], new LispEnvironment());
    });
  });
});

describe('clj/try', function (): any {
  it('(/ 1 2)', function (): any {
    return testLisp(
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
      0.5
    );
  });
  it('(clj/try (/ 1 3) (/ 1 2) ...)', function (): any {
    return testLisp(
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
      0.5
    );
  });
  return xit('(throw (new Error "an error"))', function (): any {
    return testLisp(
      [
        Symbol.for('clj/try'),
        [
          Symbol.for('throw'),
          [Symbol.for('new'), Symbol.for('Error'), 'an error'],
        ],
        [
          Symbol.for('catch'),
          Symbol.for('Error'),
          Symbol.for('e'),
          'there was an error',
        ],
        [Symbol.for('finally'), [Symbol.for('display'), 'finally']],
      ],
      'there was an error'
    );
  });
});

describe('unwind-protect', function (): any {
  return it('(unwind-protect 1 2 3)', function (): any {
    return testLisp([Symbol.for('unwind-protect'), 1, 2, 3], 1);
  });
});

describe('define-values', function (): any {
  it('(begin (define-values (x y) ...) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define-values'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('values'), 1, 2],
        ],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [1, 2]
    );
  });
  it('(begin (define-values (x y) ...) x)', function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
        [
          Symbol.for('define-values'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('values'), 1, 2],
        ],
        Symbol.for('x'),
      ],
      1
    );
  });
  return it('> (define-values (x y) (values 1 2))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define-values'),
        [Symbol.for('x'), Symbol.for('y')],
        [Symbol.for('values'), 1, 2],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('x'),
      1,
    ]);
  });
});

describe('set!-values', function (): any {
  return it('> (set!-values (x y) (values 1 2))', function (): any {
    return testRepl(
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('set!-values'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('values'), 1, 2],
        ],
        [Symbol.for('quote'), [1, 2]],
        Symbol.for('>'),
        Symbol.for('x'),
        1,
      ],
      {
        compile: false,
      }
    );
  });
});

describe('let-values', function (): any {
  it('(let-values ...)', function (): any {
    return testLisp(
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
      [1, 2]
    );
  });
  return it('(let-values (((x . y) ...))...)', function (): any {
    return testLisp(
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
      [1, [2]]
    );
  });
});

describe('let*-values', function (): any {
  return it('(let*-values ...)', function (): any {
    return testLisp(
      [
        Symbol.for('let-values'),
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
      [1, 2, 3, 4]
    );
  });
});

describe('set!-values', function (): any {
  return it('(let ... (set!-values (x y) ...) ...)', function (): any {
    return testLisp(
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
      [1, 2]
    );
  });
});

describe('define-js-obj', function (): any {
  return it('> (define-js-obj (x) (js-obj "x" 1))', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define-js-obj'),
        [Symbol.for('x')],
        [Symbol.for('js-obj'), 'x', 1],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('x'),
      1,
    ]);
  });
});

describe('set!-js-obj', function (): any {
  return it('> (set!-js-obj (x) (js-obj "x" 1))', function (): any {
    return testRepl(
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('set!-js-obj'),
          [Symbol.for('x')],
          [Symbol.for('js-obj'), 'x', 1],
        ],
        [Symbol.for('js-obj'), 'x', 1],
        Symbol.for('>'),
        Symbol.for('x'),
        1,
      ],
      {
        compile: false,
      }
    );
  });
});

describe('destructuring-bind', function (): any {
  it('(destructuring-bind ...)', function (): any {
    return testLisp(
      [
        Symbol.for('destructuring-bind'),
        [Symbol.for('x'), Symbol.for('y')],
        [Symbol.for('quote'), [1, 2]],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [1, 2]
    );
  });
  return it('(destructuring-bind (x . y) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('destructuring-bind'),
        [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')],
        [Symbol.for('quote'), [1, 2]],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [1, [2]]
    );
  });
});

describe('multiple-values-bind', function (): any {
  return it('(multiple-values-bind ...)', function (): any {
    return testLisp(
      [
        Symbol.for('multiple-values-bind'),
        [Symbol.for('x'), Symbol.for('y')],
        [Symbol.for('values'), 1, 2],
        [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')],
      ],
      [1, 2]
    );
  });
});

describe('hash', function (): any {
  it('(hash)', function (): any {
    return testLisp([Symbol.for('hash')], new Map());
  });
  return it('(hash \'(("foo" . "bar")))', function (): any {
    return testLisp(
      [
        Symbol.for('hash'),
        [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
      ],
      new Map([['foo', 'bar']])
    );
  });
});

describe('make-hash', function (): any {
  it('(make-hash)', function (): any {
    return testLisp([Symbol.for('make-hash')], new Map());
  });
  return it('(make-hash \'(("foo" . "bar")))', function (): any {
    return testLisp(
      [
        Symbol.for('make-hash'),
        [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
      ],
      new Map([['foo', 'bar']])
    );
  });
});

describe('hash?', function (): any {
  it('(hash? (make-hash))', function (): any {
    return testLisp([Symbol.for('hash?'), [Symbol.for('make-hash')]], true);
  });
  return it('(hash? 0)', function (): any {
    return testLisp([Symbol.for('hash?'), 0], false);
  });
});

describe('hash-clear', function (): any {
  return xit('(hash-clear (make-hash \'(("foo" . "bar"))))', function (): any {
    return testLisp(
      [
        Symbol.for('hash-clear'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      new Map()
    );
  });
});

describe('hash-clear!', function (): any {
  return it('(let ... (hash-clear! ...) ...)', function (): any {
    return testLisp(
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
      new Map()
    );
  });
});

describe('hash-copy', function (): any {
  return it('(hash-copy (make-hash \'(("foo" . "bar"))))', function (): any {
    return testLisp(
      [
        Symbol.for('hash-copy'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      new Map([['foo', 'bar']])
    );
  });
});

describe('hash-keys', function (): any {
  return it('(hash-keys (make-hash \'(("foo" . "bar"))))', function (): any {
    return testLisp(
      [
        Symbol.for('hash-keys'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      ['foo']
    );
  });
});

describe('hash-values', function (): any {
  return it('(hash-values (make-hash \'(("foo" . "bar"))))', function (): any {
    return testLisp(
      [
        Symbol.for('hash-values'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      ['bar']
    );
  });
});

describe('hash->list', function (): any {
  return it('(hash->list (make-hash \'(("foo" . "bar"))))', function (): any {
    return testLisp(
      [
        Symbol.for('hash->list'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
      ],
      [['foo', Symbol.for('.'), 'bar']]
    );
  });
});

describe('hash-set', function (): any {
  return it('(hash-set (make-hash) "foo" "bar")', function (): any {
    return testLisp(
      [Symbol.for('hash-set'), [Symbol.for('make-hash')], 'foo', 'bar'],
      new Map([['foo', 'bar']])
    );
  });
});

describe('hash-set!', function (): any {
  return it('(let ... (hash-set! ...) ...)', function (): any {
    return testLisp(
      [
        Symbol.for('let'),
        [[Symbol.for('ht'), [Symbol.for('make-hash')]]],
        [Symbol.for('hash-set!'), Symbol.for('ht'), 'foo', 'bar'],
        Symbol.for('ht'),
      ],
      new Map([['foo', 'bar']])
    );
  });
});

describe('hash-ref', function (): any {
  it('(hash-ref (make-hash \'(("foo" . "bar"))) "foo")', function (): any {
    return testLisp(
      [
        Symbol.for('hash-ref'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
        'foo',
      ],
      'bar'
    );
  });
  return it('(hash-ref (make-hash) "quux" #f)', function (): any {
    return testLisp(
      [
        Symbol.for('hash-ref'),
        [Symbol.for('make-hash')],
        'quux',
        Symbol.for('#f'),
      ],
      false
    );
  });
});

describe('hash-has-key?', function (): any {
  it('(hash-has-key? (make-hash \'(("foo" . "bar"))) "foo")', function (): any {
    return testLisp(
      [
        Symbol.for('hash-has-key?'),
        [
          Symbol.for('make-hash'),
          [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]],
        ],
        'foo',
      ],
      true
    );
  });
  return it('(hash-has-key? (make-hash) "quux")', function (): any {
    return testLisp(
      [Symbol.for('hash-has-key?'), [Symbol.for('make-hash')], 'quux'],
      false
    );
  });
});

describe('Map', function (): any {
  it('(new Map)', function (): any {
    return testLisp([Symbol.for('new'), Symbol.for('Map')], new Map());
  });
  it('(new Map), Map bound in environment', function (): any {
    return assertEqual(
      interpret(
        [Symbol.for('new'), Symbol.for('Map')],
        new LispEnvironment([[Symbol.for('Map'), Map, 'function']])
      ) instanceof Map,
      true
    );
  });
  it('(new Map (list (list 1 2))), Map bound in environment', function (): any {
    return assertEqual(
      Array.from(
        interpret(
          [
            Symbol.for('new'),
            Symbol.for('Map'),
            [Symbol.for('quote'), [[1, 2]]],
          ],
          new LispEnvironment([[Symbol.for('Map'), Map, 'function']])
        ).entries()
      ),
      [[1, 2]]
    );
  });
  return xit('(new Map (list (list 1 2))), new, Map bound in environment', function (): any {
    return assertEqual(
      Array.from(
        interpret(
          [
            Symbol.for('new'),
            Symbol.for('Map'),
            [Symbol.for('quote'), [[1, 2]]],
          ],
          new LispEnvironment([[Symbol.for('Map'), Map, 'function']])
        ).entries()
      ),
      [[1, 2]]
    );
  });
});

describe('+', function (): any {
  it('(+)', function (): any {
    return testLisp([Symbol.for('+')], 0);
  });
  it('(+ 1)', function (): any {
    return testLisp([Symbol.for('+'), 1], 1);
  });
  it('(+ 1 2)', function (): any {
    return testLisp([Symbol.for('+'), 1, 2], 3);
  });
  it('(+ 1 2 3)', function (): any {
    return testLisp([Symbol.for('+'), 1, 2, 3], 6);
  });
  it('(+ 1 1), custom function', function (): any {
    return testLisp([Symbol.for('+'), 1, 1], 2, {
      env: new LispEnvironment([
        [
          Symbol.for('+'),
          function (x: any, y: any): any {
            return x + y;
          },
          'function',
        ],
      ]),
    });
  });
  it('(+ 1 1), custom function', function (): any {
    return testLisp([Symbol.for('+'), 1, 1], 2, {
      env: new LispEnvironment([
        [
          Symbol.for('+'),
          function (x: any, y: any): any {
            return x + y;
          },
          'function',
        ],
      ]),
    });
  });
  it('(+ x x)', function (): any {
    return testLisp([Symbol.for('+'), Symbol.for('x'), Symbol.for('x')], 4, {
      env: new LispEnvironment([
        [Symbol.for('x'), 2, 'variable'],
        [
          Symbol.for('+'),
          function (x: any, y: any): any {
            return x + y;
          },
          'function',
        ],
      ]),
    });
  });
  // (it "(+ x x), custom function and variable"
  //     (fn ()
  //       (test-lisp
  //        '(+ x x)
  //        2
  //        (js-obj "env"
  //                (new LispEnvironment
  //                     `((x
  //                        1
  //                        "variable")
  //                       (+
  //                        ,(lambda (x y)
  //                           (+ x y))
  //                        "function")))))))
  it('(+ x x), custom function and variable', function (): any {
    return testLisp([Symbol.for('+'), Symbol.for('x'), Symbol.for('x')], 2, {
      env: new LispEnvironment([
        [Symbol.for('x'), 1, 'variable'],
        [
          Symbol.for('+'),
          function (x: any, y: any): any {
            return x + y;
          },
          'function',
        ],
      ]),
    });
  });
  it('(+ x x), custom function and variable', function (): any {
    return testLisp([Symbol.for('+'), Symbol.for('x'), Symbol.for('x')], 2, {
      env: new LispEnvironment([
        [Symbol.for('x'), 1, 'variable'],
        [
          Symbol.for('+'),
          function (x: any, y: any): any {
            return x + y;
          },
          'function',
        ],
      ]),
    });
  });
  it('(+ (+ 1 1) (+ 1 1)), custom function', function (): any {
    return testLisp(
      [Symbol.for('+'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 1, 1]],
      4,
      {
        env: new LispEnvironment([
          [
            Symbol.for('+'),
            function (x: any, y: any): any {
              return x + y;
            },
            'function',
          ],
        ]),
      }
    );
  });
  return it("(apply + '(1 2))", function (): any {
    return testLisp(
      [Symbol.for('apply'), Symbol.for('+'), [Symbol.for('quote'), [1, 2]]],
      3
    );
  });
});

describe('-', function (): any {
  it('(-)', function (): any {
    return testLisp([Symbol.for('-')], 0);
  });
  it('(- 1)', function (): any {
    return testLisp([Symbol.for('-'), 1], -1);
  });
  it('(- 1 2)', function (): any {
    return testLisp([Symbol.for('-'), 1, 2], -1);
  });
  return it('(- 1 2 3)', function (): any {
    return testLisp([Symbol.for('-'), 1, 2, 3], -4);
  });
});

describe('*', function (): any {
  it('(*)', function (): any {
    return testLisp([Symbol.for('*')], 1);
  });
  it('(* 1)', function (): any {
    return testLisp([Symbol.for('*'), 1], 1);
  });
  it('(* 1 2)', function (): any {
    return testLisp([Symbol.for('*'), 1, 2], 2);
  });
  return it('(* 1 2 3)', function (): any {
    return testLisp([Symbol.for('*'), 1, 2, 3], 6);
  });
});

describe('/', function (): any {
  xit('(/)', function (): any {
    return testLisp([Symbol.for('/')], 1);
  });
  it('(/ 1)', function (): any {
    return testLisp([Symbol.for('/'), 1], 1);
  });
  it('(/ 2)', function (): any {
    return testLisp([Symbol.for('/'), 2], 0.5);
  });
  it('(/ 1 2)', function (): any {
    return testLisp([Symbol.for('/'), 1, 2], 0.5);
  });
  return it('(/ 1 2 3)', function (): any {
    return testLisp([Symbol.for('/'), 1, 2, 3], 1 / 2 / 3);
  });
});

describe('range', function (): any {
  it('(range 1 2)', function (): any {
    return testLisp([Symbol.for('range'), 1, 2], [1]);
  });
  it('(range 10)', function (): any {
    return testLisp([Symbol.for('range'), 10], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
  });
  it('(range 10 20)', function (): any {
    return testLisp(
      [Symbol.for('range'), 10, 20],
      [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
    );
  });
  it('(range 20 40 2)', function (): any {
    return testLisp(
      [Symbol.for('range'), 20, 40, 2],
      [20, 22, 24, 26, 28, 30, 32, 34, 36, 38]
    );
  });
  it('(range 20 10 -1)', function (): any {
    return testLisp(
      [Symbol.for('range'), 20, 10, -1],
      [20, 19, 18, 17, 16, 15, 14, 13, 12, 11]
    );
  });
  return it('(range 10 15 1.5)', function (): any {
    return testLisp([Symbol.for('range'), 10, 15, 1.5], [10, 11.5, 13, 14.5]);
  });
});

describe('member', function (): any {
  it('(member 2 (list 1 2 3 4))', function (): any {
    return testLisp(
      [Symbol.for('member'), 2, [Symbol.for('list'), 1, 2, 3, 4]],
      [2, 3, 4]
    );
  });
  it('(member 9 (list 1 2 3 4))', function (): any {
    return testLisp(
      [Symbol.for('member'), 9, [Symbol.for('list'), 1, 2, 3, 4]],
      false
    );
  });
  return it("(member 5 '(3 5 1 7 2 9) (lambda (x y) (< x y)))", function (): any {
    return testLisp(
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
      [7, 2, 9]
    );
  });
});

describe('member?', function (): any {
  it('(member? 2 (list 1 2 3 4))', function (): any {
    return testLisp(
      [Symbol.for('member?'), 2, [Symbol.for('list'), 1, 2, 3, 4]],
      true
    );
  });
  return it('(member? 9 (list 1 2 3 4))', function (): any {
    return testLisp(
      [Symbol.for('member?'), 9, [Symbol.for('list'), 1, 2, 3, 4]],
      false
    );
  });
});

describe('take', function (): any {
  it("(take '(1 2 3 4) 0)", function (): any {
    return testLisp(
      [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3, 4]], 0],
      []
    );
  });
  it("(take '(1 2 3 4) 1)", function (): any {
    return testLisp(
      [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3, 4]], 1],
      [1]
    );
  });
  return it("(take '(1 2 3 4) 2)", function (): any {
    return testLisp(
      [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3, 4]], 2],
      [1, 2]
    );
  });
});

describe('drop', function (): any {
  it("(drop '(1 2 3 4) 0)", function (): any {
    return testLisp(
      [Symbol.for('drop'), [Symbol.for('quote'), [1, 2, 3, 4]], 0],
      [1, 2, 3, 4]
    );
  });
  return it("(drop '(1 2 3 4) 1)", function (): any {
    return testLisp(
      [Symbol.for('drop'), [Symbol.for('quote'), [1, 2, 3, 4]], 1],
      [2, 3, 4]
    );
  });
});

describe('drop-right', function (): any {
  it("(drop-right '(1 2 3 4) 0)", function (): any {
    return testLisp(
      [Symbol.for('drop-right'), [Symbol.for('quote'), [1, 2, 3, 4]], 0],
      [1, 2, 3, 4]
    );
  });
  return it("(drop-right '(1 2 3 4) 1)", function (): any {
    return testLisp(
      [Symbol.for('drop-right'), [Symbol.for('quote'), [1, 2, 3, 4]], 1],
      [1, 2, 3]
    );
  });
});

describe('map', function (): any {
  it("(map list '(1 2))", function (): any {
    return testLisp(
      [Symbol.for('map'), Symbol.for('list'), [Symbol.for('quote'), [1, 2]]],
      [[1], [2]]
    );
  });
  return it("(map fact '(1 2 3 4 5 6))", function (): any {
    return testLisp(
      [
        Symbol.for('begin'),
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
      [1, 2, 6, 24, 120, 720]
    );
  });
});

describe('foldl', function (): any {
  return it("(foldr cons '() '(1 2 3 4))", function (): any {
    return testLisp(
      [
        Symbol.for('foldl'),
        Symbol.for('cons'),
        [Symbol.for('quote'), []],
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ],
      [4, 3, 2, 1]
    );
  });
});

describe('foldr', function (): any {
  it("(foldr cons '() '(1 2 3 4))", function (): any {
    return testLisp(
      [
        Symbol.for('foldr'),
        Symbol.for('cons'),
        [Symbol.for('quote'), []],
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ],
      [1, 2, 3, 4]
    );
  });
  return it('(foldr (lambda (v l) (cons (add1 v) l)) ...)', function (): any {
    return testLisp(
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
      [2, 3, 4, 5]
    );
  });
});

describe('filter', function (): any {
  return it('(filter string? \'("foo" 1 2 3))', function (): any {
    return testLisp(
      [
        Symbol.for('filter'),
        Symbol.for('string?'),
        [Symbol.for('quote'), ['foo', 1, 2, 3]],
      ],
      ['foo']
    );
  });
});

describe('string functions', function (): any {
  describe('string-length', function (): any {
    return it('> (string-length "foo")', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('string-length'), 'foo'],
        3,
      ]);
    });
  });
  describe('string-append', function (): any {
    it('(string-append)', function (): any {
      return testLisp([Symbol.for('string-append')], '');
    });
    it('(string-append "foo")', function (): any {
      return testLisp([Symbol.for('string-append'), 'foo'], 'foo');
    });
    it('(string-append "foo" "bar")', function (): any {
      return testLisp([Symbol.for('string-append'), 'foo', 'bar'], 'foobar');
    });
    return it('(apply string-append \'("foo" "bar"))', function (): any {
      return testLisp(
        [
          Symbol.for('apply'),
          Symbol.for('string-append'),
          [Symbol.for('quote'), ['foo', 'bar']],
        ],
        'foobar'
      );
    });
  });
  describe('string-trim', function (): any {
    it('> (string-trim "  foo bar  baz  ")', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('string-trim'), '  foo bar  baz  '],
        'foo bar  baz',
      ]);
    });
    it('> (string-trim "  foo bar  baz \\r\\n\\t")', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('string-trim'), '  foo bar  baz \n' + '\n' + '	'],
        'foo bar  baz',
      ]);
    });
    it('> (string-trim "_foo bar  baz_" "_")', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [Symbol.for('string-trim'), '_foo bar  baz_', '_'],
          'foo bar  baz',
        ],
        {
          compile: false,
        }
      );
    });
    it('> (string-trim "__foo bar  baz__" "_" :repeat? #t)', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('string-trim'),
            '__foo bar  baz__',
            '_',
            Symbol.for(':repeat?'),
            Symbol.for('#t'),
          ],
          'foo bar  baz',
        ],
        {
          compile: false,
        }
      );
    });
    return it('> (string-trim "  foo bar  baz \\r\\n\\t" " " :repeat? #t)', function (): any {
      return testRepl(
        [
          Symbol.for('roselisp'),
          Symbol.for('>'),
          [
            Symbol.for('string-trim'),
            '  foo bar  baz \n' + '\n' + '	',
            ' ',
            Symbol.for(':repeat?'),
            Symbol.for('#t'),
          ],
          'foo bar  baz \n' + '\n' + '	',
        ],
        {
          compile: false,
        }
      );
    });
  });
  return describe('substring', function (): any {
    it('(substring "Apple" 1 3)', function (): any {
      return testLisp([Symbol.for('substring'), 'Apple', 1, 3], 'pp');
    });
    return it('(substring "Apple" 1', function (): any {
      return testLisp([Symbol.for('substring'), 'Apple', 1], 'pple');
    });
  });
});

describe('apply', function (): any {
  it("(apply new make-hash '())", function (): any {
    return testLisp(
      [
        Symbol.for('apply'),
        Symbol.for('new'),
        Symbol.for('make-hash'),
        [Symbol.for('quote'), []],
      ],
      new Map(),
      {
        compile: false,
      }
    );
  });
  xit("(apply new make-hash '())", function (): any {
    return testLisp(
      [
        Symbol.for('apply'),
        Symbol.for('new'),
        Symbol.for('make-hash'),
        [Symbol.for('quote'), []],
      ],
      new Map(),
      {
        compile: false,
      }
    );
  });
  xit('(apply send (make-hash) \'has \'("foo"))', function (): any {
    return testLisp(
      [
        Symbol.for('apply'),
        Symbol.for('send'),
        [Symbol.for('make-hash')],
        [Symbol.for('quote'), Symbol.for('has')],
        [Symbol.for('quote'), ['foo']],
      ],
      false
    );
  });
  return xit('(apply send (make-hash) \'(has "foo"))', function (): any {
    return testLisp(
      [
        Symbol.for('apply'),
        Symbol.for('send'),
        [Symbol.for('make-hash')],
        [Symbol.for('quote'), [Symbol.for('has'), 'foo']],
      ],
      false,
      {
        compile: false,
      }
    );
  });
});

describe('as~>', function (): any {
  return it('(as~> 0 _ (+ _ 1) (+ _ 1))', function (): any {
    return testLisp(
      [
        Symbol.for('as~>'),
        0,
        Symbol.for('_'),
        [Symbol.for('+'), Symbol.for('_'), 1],
        [Symbol.for('+'), Symbol.for('_'), 1],
      ],
      2
    );
  });
});

describe('ann', function (): any {
  it('(ann undefined Any)', function (): any {
    return testLisp(
      [Symbol.for('ann'), Symbol.for('undefined'), Symbol.for('Any')],
      undefined
    );
  });
  return it('((ann undefined Any))', function (): any {
    return testLisp(
      [[Symbol.for('ann'), Symbol.for('undefined'), Symbol.for('Any')]],
      undefined,
      {
        compile: false,
      }
    );
  });
});

describe('interpret', function (): any {
  it('default environment', function (): any {
    assertEqual(interpret(sexp`t`), true);
    return assertEqual(interpret(sexp`t`, new LispEnvironment()), true);
  });
  return it('currying', function (): any {
    assertEqual(interpret(sexp`t`, __)(new LispEnvironment()), true);
    assertEqual(interpret(__, new LispEnvironment())(sexp`t`), true);
    return assertEqual(interpret(__, __)(sexp`t`)(new LispEnvironment()), true);
  });
});

describe('lisp', function (): any {
  it('(quote foo)', function (): any {
    return assertEqual(lisp('(quote foo)'), s`foo`);
  });
  it('(identity1 "foo")', function (): any {
    return assertEqual(
      lisp(
        '(identity1 "foo")',
        new LispEnvironment([
          [
            Symbol.for('identity1'),
            function (x: any): any {
              return x;
            },
            'variable',
          ],
        ])
      ),
      'foo'
    );
  });
  it('(list 1 2)', function (): any {
    return assertEqual(lisp('(list 1 2)'), [1, 2]);
  });
  return describe('+', function (): any {
    it('(+ 1 1)', function (): any {
      return assertEqual(
        lisp(
          '(+ 1 1)',
          new LispEnvironment([
            [
              Symbol.for('+'),
              function (x: any, y: any): any {
                return x + y;
              },
              'function',
            ],
          ])
        ),
        2
      );
    });
    return it('(+ foo foo)', function (): any {
      return assertEqual(
        lisp(
          '(+ foo foo)',
          new LispEnvironment([
            [Symbol.for('foo'), 2, 'variable'],
            [
              Symbol.for('+'),
              function (x: any, y: any): any {
                return x + y;
              },
              'function',
            ],
          ])
        ),
        4
      );
    });
  });
});

describe('current-environment', function (): any {
  return it("(send (current-environment) get 'x)", function (): any {
    return testLisp(
      [
        [
          Symbol.for('lambda'),
          [Symbol.for('x')],
          [
            Symbol.for('send'),
            [Symbol.for('current-environment')],
            Symbol.for('get'),
            [Symbol.for('quote'), Symbol.for('x')],
          ],
        ],
        1,
      ],
      1,
      {
        compile: false,
      }
    );
  });
});

describe('set-car!', function (): any {
  it('() -> ()', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), []]],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-car!'),
        Symbol.for('foo'),
        [Symbol.for('quote'), Symbol.for('bar')],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [Symbol.for('quote'), []],
    ]);
  });
  return it('(foo) -> (bar)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('foo')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-car!'),
        Symbol.for('foo'),
        [Symbol.for('quote'), Symbol.for('bar')],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [Symbol.for('quote'), [Symbol.for('bar')]],
    ]);
  });
});

describe('set-cdr!', function (): any {
  it('() -> ()', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), []]],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-cdr!'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('bar')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [Symbol.for('quote'), []],
    ]);
  });
  it('(foo) -> (foo bar)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('foo')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-cdr!'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('bar')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
    ]);
  });
  it('(foo bar) -> (foo baz)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-cdr!'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('baz')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('baz')]],
    ]);
  });
  it('(foo bar) -> (foo baz . quux)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-cdr!'),
        Symbol.for('foo'),
        [
          Symbol.for('quote'),
          [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')],
        ],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
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
  it('(foo . bar) -> (foo baz)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [
          Symbol.for('quote'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
        ],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-cdr!'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('baz')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('baz')]],
    ]);
  });
  it('(foo . bar) -> (foo baz . quux)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [
          Symbol.for('quote'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
        ],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-cdr!'),
        Symbol.for('foo'),
        [
          Symbol.for('quote'),
          [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')],
        ],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
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
  it('(foo) -> (foo . bar)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('foo')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-cdr!'),
        Symbol.for('foo'),
        [Symbol.for('quote'), Symbol.for('bar')],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [
        Symbol.for('quote'),
        [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')],
      ],
    ]);
  });
  it('(foo bar . baz) -> (foo quux)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-cdr!'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('quux')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('quux')]],
    ]);
  });
  return it('(foo bar . baz) -> (foo . quux)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
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
      Symbol.for('undefined'),
      Symbol.for('>'),
      [
        Symbol.for('set-cdr!'),
        Symbol.for('foo'),
        [Symbol.for('quote'), Symbol.for('quux')],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      Symbol.for('foo'),
      [
        Symbol.for('quote'),
        [Symbol.for('foo'), Symbol.for('.'), Symbol.for('quux')],
      ],
    ]);
  });
});

describe('dotted-list?', function (): any {
  it('(foo . bar)', function (): any {
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
      Symbol.for('#t'),
    ]);
  });
  return it('(foo bar)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('dotted-list?'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      Symbol.for('#f'),
    ]);
  });
});

describe('dotted-list-head', function (): any {
  it('(foo . bar)', function (): any {
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
  return it('(foo bar . baz)', function (): any {
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
  it('(foo . bar)', function (): any {
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
  return it('(foo bar . baz)', function (): any {
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

describe('dotted-list->proper-list', function (): any {
  it('(foo . bar)', function (): any {
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
  return it('(foo bar . baz)', function (): any {
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
  it('(foo bar)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('proper-list?'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      Symbol.for('#t'),
    ]);
  });
  return it('(foo . bar)', function (): any {
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
      Symbol.for('#f'),
    ]);
  });
});

describe('circular-list?', function (): any {
  it('()', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), []]],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [Symbol.for('circular-list?'), Symbol.for('foo')],
      Symbol.for('#f'),
      Symbol.for('>'),
      [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [Symbol.for('circular-list?'), Symbol.for('foo')],
      Symbol.for('#f'),
    ]);
  });
  it('(foo)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('circular-list?'),
        [Symbol.for('quote'), [Symbol.for('foo')]],
      ],
      Symbol.for('#f'),
    ]);
  });
  it('(foo . bar)', function (): any {
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
      Symbol.for('#f'),
    ]);
  });
  it('(foo . #0#)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('foo')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [Symbol.for('circular-list?'), Symbol.for('foo')],
      Symbol.for('#t'),
    ]);
  });
  it('(foo . #0#), from linked list', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), []]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [Symbol.for('circular-list?'), Symbol.for('foo')],
      Symbol.for('#t'),
    ]);
  });
  return it('(foo bar . #0#)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]],
      ],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')],
      Symbol.for('undefined'),
      Symbol.for('>'),
      [Symbol.for('circular-list?'), Symbol.for('foo')],
      Symbol.for('#t'),
    ]);
  });
});

describe('proper-list->dotted-list', function (): any {
  it('(foo bar) -> (foo . bar)', function (): any {
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
  return it('(foo bar baz) -> (foo bar . baz)', function (): any {
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
      Symbol.for('undefined'),
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

describe('eval', function (): any {
  // (it "1 + 1, eval true"
  //     (fn ()
  //       (assert-equal
  //        (interpret '(+ 1 1) undefined (js-obj "eval" #t))
  //        2)))
  // (it "1 + 1, eval false"
  //     (fn ()
  //       (assert-equal
  //        (interpret '(+ 1 1) undefined (js-obj "eval" #f))
  //        2)))

  // (send it only "js/eval, eval true"
  //       (fn ()
  //         (assert-equal
  //          (interpret 'js/eval undefined (js-obj "eval" #t))
  //          js/eval)))
  it('js/eval, eval true', function (): any {
    return assertEqual(
      interpret([Symbol.for('js/eval'), '1'], undefined, {
        eval: true,
      }),
      1
    );
  });
  // (send it only "js/eval, eval true"
  //       (fn ()
  //         (assert-equal
  //          (interpret '(js/eval "1") undefined (js-obj "eval" #f))
  //          undefined)))
  return it('js/eval, eval false', function (): any {
    return assertEqual(
      interpret(Symbol.for('js/eval'), undefined, {
        eval: false,
      }),
      undefined
    );
  });
});
