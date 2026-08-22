/**
 * # Interpreter tests
 */

import {
  __,
  LispEnvironment,
  interpret,
  lisp
} from '../../src/ts/language';

import {
  s,
  sexp
} from '../../src/ts/sexp';

import {
  assertEqual,
  assertThrows,
  testLisp,
  testRepl,
  testMacro
} from './test-util';

describe('boolean values', function (): any {
  it('true', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('true'), true]);
  });
  it('t', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('t'), true]);
  });
  it('#t', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), true, true]);
  });
  it('false', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('false'), false]);
  });
  return it('#f', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), false, false]);
  });
});

describe('truep', function (): any {
  it('(truep true)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('truep'), Symbol.for('true')], true]);
  });
  it('(truep false)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('truep'), Symbol.for('false')], false]);
  });
  it('(truep undefined)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('truep'), Symbol.for('undefined')], false]);
  });
  return it('(truep true)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('truep'), Symbol.for('true')], true]);
  });
});

describe('falsep', function (): any {
  it('(falsep true)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('falsep'), Symbol.for('true')], false]);
  });
  it('(falsep false)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('falsep'), Symbol.for('false')], true]);
  });
  return it('(falsep undefined)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('falsep'), Symbol.for('undefined')], true]);
  });
});

describe('define', function (): any {
  return it('(begin (define ((my-add x) y) (+ x y)) (my-add 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('begin'), [Symbol.for('define'), [[Symbol.for('my-add'), Symbol.for('x')], Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('my-add'), 2, 3]], 5]);
  });
});

describe('lambda', function (): any {
  return it('((lambda (x y) (+ x y)) 1 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], 1, 1], 2]);
  });
});

describe('eq?', function (): any {
  return it('(begin (define (my-unit x) x) (my-unit \'foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('my-unit'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('my-unit'), [Symbol.for('quote'), Symbol.for('foo')]]], [Symbol.for('quote'), Symbol.for('foo')]]);
  });
});

describe('send', function (): any {
  return it('(send (make-hash \'(("foo" . "foo"))) has \'("foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('send'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]]], Symbol.for('has'), [Symbol.for('quote'), ['foo']]], false]);
  });
});

describe('dot', function (): any {
  it('(let ((obj (js/obj "add1" (lambda (x) (+ x 1))))) (. obj add1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'add1', [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('+'), Symbol.for('x'), 1]]]]], [Symbol.for('.'), Symbol.for('obj'), Symbol.for('add1'), 1]], 2]);
  });
  it('(let ((obj (js/obj "add1" (lambda (x) (+ x 1))))) (.add1 obj 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'add1', [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('+'), Symbol.for('x'), 1]]]]], [Symbol.for('.add1'), Symbol.for('obj'), 1]], 2]);
  });
  it('(let ((obj (js/obj "add" (lambda (x y) (+ x y))))) (.add obj 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'add', [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]]], [Symbol.for('.add'), Symbol.for('obj'), 1, 1]], 2]);
  });
  it('(let ((obj (js/obj))) (set! (.-prop obj) "bar") (.-prop obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj')]]], [Symbol.for('set!'), [Symbol.for('.-prop'), Symbol.for('obj')], 'bar'], [Symbol.for('.-prop'), Symbol.for('obj')]], 'bar']);
  });
  return it('(let ((obj (js/obj "prop" "foo"))) (.-prop obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'prop', 'foo']]], [Symbol.for('.-prop'), Symbol.for('obj')]], 'foo']);
  });
});

describe('new', function (): any {
  return it('(let (quux) (set! quux (new (class (Object) (define/public val 1) (define (constructor x) (set! (.-val this) x)) (define/public (bar) (.-val this))) 2)) (.bar quux))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('quux')], [Symbol.for('set!'), Symbol.for('quux'), [Symbol.for('new'), [Symbol.for('class'), [Symbol.for('Object')], [Symbol.for('define/public'), Symbol.for('val'), 1], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('set!'), [Symbol.for('.-val'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-val'), Symbol.for('this')]]], 2]], [Symbol.for('.bar'), Symbol.for('quux')]], 2]);
  });
});

describe('class', function (): any {
  return it('(defclass Foo ...) with constructor and new', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('quux')], [Symbol.for('defclass'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]], [Symbol.for('set!'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo'), 'xyzzy']], [Symbol.for('.bar'), Symbol.for('quux')]], 'xyzzy']);
  });
});

describe('clj/try', function (): any {
  return it('(clj/try (throw (new Error "an error")) (catch Error e "there was an error") (finally (display "finally")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('clj/try'), [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), 'an error']], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e'), 'there was an error'], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]], 'there was an error']);
  });
});

describe('+', function (): any {
  it('(+ 1 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 1, 1], 2]);
  });
  it('(let ((x 1)) (+ x x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 1]], [Symbol.for('+'), Symbol.for('x'), Symbol.for('x')]], 2]);
  });
  return it('(+ (+ 1 1) (+ 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 1, 1]], 4]);
  });
});

describe('String functions', function (): any {
  it('(string-split "foo bar baz" " ")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-split'), 'foo bar baz', ' '], [Symbol.for('quote'), ['foo', 'bar', 'baz']]]);
  });
  it('(string-trim "_foo bar  baz_" "_")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-trim'), '_foo bar  baz_', '_'], 'foo bar  baz']);
  });
  it('(string-trim "__foo bar  baz__" "_" :repeat? #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-trim'), '__foo bar  baz__', '_', Symbol.for(':repeat?'), true], 'foo bar  baz']);
  });
  return it('(string-trim "  foo bar  baz \n' +
    '\n' +
    '	" " " :repeat? #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-trim'), '  foo bar  baz \n' +
      '\n' +
      '	', ' ', Symbol.for(':repeat?'), true], 'foo bar  baz \n' +
      '\n' +
      '	']);
  });
});

describe('apply', function (): any {
  return it('(apply new make-hash \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('apply'), Symbol.for('new'), Symbol.for('make-hash'), [Symbol.for('quote'), []]], [Symbol.for('new'), Symbol.for('Map')]]);
  });
});

describe('Y combinator', function (): any {
  return it('(let ((Y (lambda (f) ((lambda (future) (f (lambda (arg) ((future future) arg)))) (lambda (future) (f (lambda (arg) ((future future) arg)))))))) ((Y (lambda (f) (lambda (x) (if (zero? x) 1 (* x (f (- x 1))))))) 6))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('Y'), [Symbol.for('lambda'), [Symbol.for('f')], [[Symbol.for('lambda'), [Symbol.for('future')], [Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('arg')], [[Symbol.for('future'), Symbol.for('future')], Symbol.for('arg')]]]], [Symbol.for('lambda'), [Symbol.for('future')], [Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('arg')], [[Symbol.for('future'), Symbol.for('future')], Symbol.for('arg')]]]]]]]], [[Symbol.for('Y'), [Symbol.for('lambda'), [Symbol.for('f')], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('zero?'), Symbol.for('x')], 1, [Symbol.for('*'), Symbol.for('x'), [Symbol.for('f'), [Symbol.for('-'), Symbol.for('x'), 1]]]]]]], 6]], 720]);
  });
});

describe('current-environment', function (): any {
  return it('((lambda (x) (send (current-environment) get \'x)) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('send'), [Symbol.for('current-environment')], Symbol.for('get'), [Symbol.for('quote'), Symbol.for('x')]]], 1], 1]);
  });
});

describe('js/eval', function (): any {
  it('(js/eval "1")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/eval'), '1'], 1]);
  });
  return it('(interpret \'(js/eval "1") #u (js/obj :eval #t))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('interpret'), [Symbol.for('quote'), [Symbol.for('js/eval'), '1']], undefined, [Symbol.for('js/obj'), Symbol.for(':eval'), true]], 1]);
  });
});

describe('interpret', function (): any {
  return it('(interpret \'t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('interpret'), [Symbol.for('quote'), Symbol.for('t')]], true]);
  });
});

describe('interpret', function (): any {
  it('(interpret \'t)', function (): any {
    return assertEqual(interpret(Symbol.for('t')), true);
  });
  it('(interpret \'t (new LispEnvironment))', function (): any {
    return assertEqual(interpret(Symbol.for('t'), new LispEnvironment()), true);
  });
  xit('((interpret \'t __) (new LispEnvironment))', function (): any {
    return assertEqual(interpret(Symbol.for('t'), __)(new LispEnvironment()), true);
  });
  xit('((interpret __ (new LispEnvironment)) \'t)', function (): any {
    return assertEqual(interpret(__, new LispEnvironment())(Symbol.for('t')), true);
  });
  return xit('(((interpret __ __) \'t) (new LispEnvironment))', function (): any {
    return assertEqual(interpret(__, __)(Symbol.for('t'))(new LispEnvironment()), true);
  });
});

describe('lisp', function (): any {
  it('(lisp "(quote foo)")', function (): any {
    return assertEqual(lisp('(quote foo)'), Symbol.for('foo'));
  });
  it('(lisp "(identity1 \\"foo\\")" (new LispEnvironment `((identity1 ,(lambda (x) x) "variable"))))', function (): any {
    return assertEqual(lisp('(identity1 "foo")', new LispEnvironment([[Symbol.for('identity1'), function (x: any): any {
      return x;
    }, 'variable']])), 'foo');
  });
  it('(lisp "(list 1 2)")', function (): any {
    return assertEqual(lisp('(list 1 2)'), [1, 2]);
  });
  it('(lisp "(+ 1 1)" (new LispEnvironment `((+ ,(lambda (x y) (+ x y)) "function"))))', function (): any {
    return assertEqual(lisp('(+ 1 1)', new LispEnvironment([[Symbol.for('+'), function (x: any, y: any): any {
      return x + y;
    }, 'function']])), 2);
  });
  return it('(lisp "(+ foo foo)" (new LispEnvironment `((foo 2 "variable") (+ ,(lambda (x y) (+ x y)) "function"))))', function (): any {
    return assertEqual(lisp('(+ foo foo)', new LispEnvironment([[Symbol.for('foo'), 2, 'variable'], [Symbol.for('+'), function (x: any, y: any): any {
      return x + y;
    }, 'function']])), 4);
  });
});

describe('Map', function (): any {
  it('(~> (interpret \'(new Map) (new LispEnvironment `((Map ,Map "function")))) (instance-of? Map))', function (): any {
    return assertEqual(interpret([Symbol.for('new'), Symbol.for('Map')], new LispEnvironment([[Symbol.for('Map'), Map, 'function']])) instanceof Map, true);
  });
  return xit('(~> (interpret \'(new Map \'((1 2))) (new LispEnvironment `((Map ,Map "function")))) (send entries) (send Array from _))', function (): any {
    return assertEqual(Array.from(interpret([Symbol.for('new'), Symbol.for('Map'), [Symbol.for('quote'), [[1, 2]]]], new LispEnvironment([[Symbol.for('Map'), Map, 'function']])).entries()), [[1, 2]]);
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