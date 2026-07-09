/**
 * # Interpreter tests
 */

import { __, LispEnvironment, interpret, lisp } from '../../src/ts/language';

import { s, sexp } from '../../src/ts/sexp';

import { assertEqual, assertThrows, testLisp, testRepl } from './test-util';

describe('boolean values', function (): any {
  it('true', function (): any {
    testLisp(sexp`true`, true);
    testLisp(sexp`t`, true);
    return testLisp(sexp`#t`, true);
  });
  return it('false', function (): any {
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
  return it('(truep true)', function (): any {
    return testLisp([s`truep`, s`true`], true);
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

describe('empty list', function (): any {
  it('cons?', function (): any {
    return testRepl(
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('cons?'), [Symbol.for('quote'), []]],
        false,
      ],
      {
        compile: false,
      }
    );
  });
  return it('list?', function (): any {
    return testRepl(
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('list?'), [Symbol.for('quote'), []]],
        true,
      ],
      {
        compile: false,
      }
    );
  });
});

describe('variables', function (): any {
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
  return it('(define ((add x y) z) ...)', function (): any {
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
describe('define-macro', function (): any {
  it('(define-macro (foo x) x)', function (): any {
    return testRepl(
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('define-macro'),
          [Symbol.for('foo'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        Symbol.for('_'),
        Symbol.for('>'),
        [Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), 1]]],
        [Symbol.for('quote'), [Symbol.for('foo'), 1]],
      ],
      {
        compile: false,
      }
    );
  });
  it('(define-macro (foo x) `(+ ,x ,x))', function (): any {
    return testRepl(
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('define-macro'),
          [Symbol.for('foo'), Symbol.for('x')],
          [
            Symbol.for('quasiquote'),
            [
              Symbol.for('+'),
              [Symbol.for('unquote'), Symbol.for('x')],
              [Symbol.for('unquote'), Symbol.for('x')],
            ],
          ],
        ],
        Symbol.for('_'),
        Symbol.for('>'),
        [Symbol.for('foo'), 1],
        2,
      ],
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

describe('defmacro', function (): any {
  it('(defmacro foo (x) x)', function (): any {
    return testRepl(
      [
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [
          Symbol.for('defmacro'),
          Symbol.for('foo'),
          [Symbol.for('x')],
          Symbol.for('x'),
        ],
        Symbol.for('_'),
        Symbol.for('>'),
        [Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), 1]]],
        [Symbol.for('quote'), [Symbol.for('foo'), 1]],
      ],
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

describe('lambda', function (): any {
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

describe('if', function (): any {
  return xit('(if "" 1 2)', function (): any {
    return testLisp([Symbol.for('if'), '', 1, 2], 1);
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
  return it("(eq (my-curried-unit '_) '_)", function (): any {
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
});

describe('for', function (): any {
  return xit('(let ... (for ((x ...) (y ...)) ...) ...)', function (): any {
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
  return xit('(defclass Foo ...) with no arguments', function (): any {
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
});

describe('js-obj', function (): any {
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

describe('Map', function (): any {
  it('(new Map), Map bound in environment', function (): any {
    return assertEqual(
      interpret(
        [Symbol.for('new'), Symbol.for('Map')],
        new LispEnvironment([[Symbol.for('Map'), Map, 'function']])
      ) instanceof Map,
      true
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
  return it('(+ (+ 1 1) (+ 1 1)), custom function', function (): any {
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
});

describe('string functions', function (): any {
  describe('string-split', function (): any {
    return xit('(string-split "  foo bar  baz \\r\\n\\t")', function (): any {
      return testRepl([
        Symbol.for('roselisp'),
        Symbol.for('>'),
        [Symbol.for('string-split'), '  foo bar  baz \n' + '\n' + '	'],
        [Symbol.for('quote'), ['foo', 'bar', 'baz']],
      ]);
    });
  });
  return describe('string-trim', function (): any {
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
            true,
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
            true,
          ],
          'foo bar  baz \n' + '\n' + '	',
        ],
        {
          compile: false,
        }
      );
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

describe('Y combinator', function (): any {
  return it('6!', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('define'),
        [Symbol.for('Y'), Symbol.for('f')],
        [
          [
            Symbol.for('lambda'),
            [Symbol.for('future')],
            [
              Symbol.for('f'),
              [
                Symbol.for('lambda'),
                [Symbol.for('arg')],
                [
                  [Symbol.for('future'), Symbol.for('future')],
                  Symbol.for('arg'),
                ],
              ],
            ],
          ],
          [
            Symbol.for('lambda'),
            [Symbol.for('future')],
            [
              Symbol.for('f'),
              [
                Symbol.for('lambda'),
                [Symbol.for('arg')],
                [
                  [Symbol.for('future'), Symbol.for('future')],
                  Symbol.for('arg'),
                ],
              ],
            ],
          ],
        ],
      ],
      undefined,
      Symbol.for('>'),
      [
        [
          Symbol.for('Y'),
          [
            Symbol.for('lambda'),
            [Symbol.for('f')],
            [
              Symbol.for('lambda'),
              [Symbol.for('x')],
              [
                Symbol.for('if'),
                [Symbol.for('zero?'), Symbol.for('x')],
                1,
                [
                  Symbol.for('*'),
                  Symbol.for('x'),
                  [Symbol.for('f'), [Symbol.for('-'), Symbol.for('x'), 1]],
                ],
              ],
            ],
          ],
        ],
        6,
      ],
      720,
    ]);
  });
});

describe('ann', function (): any {
  return xit('((ann #u Any))', function (): any {
    return testLisp(
      [[Symbol.for('ann'), undefined, Symbol.for('Any')]],
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

describe('eval', function (): any {
  // (it "1 + 1, eval true"
  //     (fn ()
  //       (assert-equal
  //        (interpret '(+ 1 1) #u (js-obj "eval" #t))
  //        2)))
  // (it "1 + 1, eval false"
  //     (fn ()
  //       (assert-equal
  //        (interpret '(+ 1 1) #u (js-obj "eval" #f))
  //        2)))
  // (send it only "js/eval, eval true"
  //       (fn ()
  //         (assert-equal
  //          (interpret 'js/eval #u (js-obj "eval" #t))
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
  //          (interpret '(js/eval "1") #u (js-obj "eval" #f))
  //          #u)))
  return it('js/eval, eval false', function (): any {
    return assertEqual(
      interpret(Symbol.for('js/eval'), undefined, {
        eval: false,
      }),
      undefined
    );
  });
});
