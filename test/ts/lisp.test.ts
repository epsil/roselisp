/**
 * # Lisp tests
 *
 * Tests of some non-Scheme Lisp constructs. Intended to exercise the
 * language's capability to implement other Lisp dialects.
 */

import {
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('set', function (): any {
  return it('(compile \'(set \'x 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set'), [Symbol.for('quote'), Symbol.for('x')], 1]]], 'x = 1;']);
  });
});

describe('setq', function (): any {
  it('(compile \'(setq x 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('setq'), Symbol.for('x'), 1]]], 'x = 1;']);
  });
  return it('(compile \'(setq x 1 y 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('setq'), Symbol.for('x'), 1, Symbol.for('y'), 2]]], 'x = 1;\n' +
      '\n' +
      'y = 2;']);
  });
});

describe('destructuring-bind', function (): any {
  it('(destructuring-bind (x) \'(1) (list x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('destructuring-bind'), [Symbol.for('x')], [Symbol.for('quote'), [1]], [Symbol.for('list'), Symbol.for('x')]], [Symbol.for('quote'), [1]]]);
  });
  it('(destructuring-bind (x y) \'(1 2) (list x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('destructuring-bind'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quote'), [1, 2]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('quote'), [1, 2]]]);
  });
  it('(destructuring-bind (x y z) \'(1 2 3) (list x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('destructuring-bind'), [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')], [Symbol.for('quote'), [1, 2, 3]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(destructuring-bind ((x) y z) \'((1) 2 3) (list x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('destructuring-bind'), [[Symbol.for('x')], Symbol.for('y'), Symbol.for('z')], [Symbol.for('quote'), [[1], 2, 3]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(destructuring-bind (x . y) \'(1 2) (list x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('destructuring-bind'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('quote'), [1, 2]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('quote'), [1, [2]]]]);
  });
  it('(compile \'(destructuring-bind (x y) \'(1 2) (list x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('destructuring-bind'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quote'), [1, 2]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]]]], 'let [x, y] = [1, 2];\n' +
      '\n' +
      '[x, y];']);
  });
  return it('(compile \'(destructuring-bind (x . y) \'(1 2) (list x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('destructuring-bind'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('quote'), [1, 2]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]]]], 'let [x, ...y] = [1, 2];\n' +
      '\n' +
      '[x, y];']);
  });
});

describe('multiple-values-bind', function (): any {
  it('(multiple-values-bind (x y) (values 1 2) (list x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('multiple-values-bind'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('quote'), [1, 2]]]);
  });
  return it('(compile \'(multiple-values-bind (x y) (values 1 2) (list x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('multiple-values-bind'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]]]], 'let [x, y] = [1, 2];\n' +
      '\n' +
      '[x, y];']);
  });
});

describe('cl/listp', function (): any {
  it('(cl/listp #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cl/listp'), true], false]);
  });
  it('(cl/listp \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cl/listp'), [Symbol.for('quote'), []]], true]);
  });
  it('(cl/listp \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cl/listp'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]);
  });
  return it('(cl/listp \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cl/listp'), [Symbol.for('quote'), [1, 2, 3]]], true]);
  });
});

describe('el/listp', function (): any {
  it('(el/listp #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('el/listp'), true], false]);
  });
  it('(el/listp \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('el/listp'), [Symbol.for('quote'), []]], true]);
  });
  it('(el/listp \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('el/listp'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]);
  });
  return it('(el/listp \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('el/listp'), [Symbol.for('quote'), [1, 2, 3]]], true]);
  });
});

describe('el/if', function (): any {
  it('(el/if #t 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('el/if'), true, 1, 2], 1]);
  });
  it('(el/if #f 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('el/if'), false, 1, 2], 2]);
  });
  it('(el/if #f 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('el/if'), false, 1, 2, 3], 3]);
  });
  return it('(el/if #f 1 2 3 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('el/if'), false, 1, 2, 3, 4], 4]);
  });
});

describe('defun', function (): any {
  it('((lambda () (defun my-add (x y) (+ x y)) (my-add 2 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defun'), Symbol.for('my-add'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('my-add'), 2, 3]]], 5]);
  });
  it('(let ((my-add (lambda (x y) (+ x y)))) ((lambda () (defun my-add-2 (x y) (my-add x y)) (my-add-2 2 3))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('my-add'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]], [[Symbol.for('lambda'), [], [Symbol.for('defun'), Symbol.for('my-add-2'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('my-add'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('my-add-2'), 2, 3]]]], 5]);
  });
  return it('(let ((my-add (lambda (x y z) (+ x y z)))) ((lambda () (defun my-add-2 (x y z) (my-add x y z)) (my-add-2 1 2 3))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('my-add'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]]], [[Symbol.for('lambda'), [], [Symbol.for('defun'), Symbol.for('my-add-2'), [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')], [Symbol.for('my-add'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], [Symbol.for('my-add-2'), 1, 2, 3]]]], 6]);
  });
});

describe('defmacro', function (): any {
  it('((lambda () (defmacro my-macro (x) x) (my-macro 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defmacro'), Symbol.for('my-macro'), [Symbol.for('x')], Symbol.for('x')], [Symbol.for('my-macro'), 1]]], 1]);
  });
  it('(compile \'(module m scheme (defmacro foo () \'(begin)) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('defmacro'), Symbol.for('foo'), [], [Symbol.for('quote'), [Symbol.for('begin')]]], [Symbol.for('foo')]]]], 'function foo(exp, env) {\n' +
      '  return [Symbol.for(\'begin\')];\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'macro\';']);
  });
  it('(compile \'(module m scheme (defmacro foo (x) x) (define (bar x) (foo x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('defmacro'), Symbol.for('foo'), [Symbol.for('x')], Symbol.for('x')], [Symbol.for('define'), [Symbol.for('bar'), Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]]]]], 'function foo(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'macro\';\n' +
      '\n' +
      'function bar(x) {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(module m scheme (defmacro foo (x) `(begin ,x)) (define (bar x) (foo x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('defmacro'), Symbol.for('foo'), [Symbol.for('x')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('x')]]]], [Symbol.for('define'), [Symbol.for('bar'), Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]]]]], 'function foo(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return [Symbol.for(\'begin\'), x];\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'macro\';\n' +
      '\n' +
      'function bar(x) {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(module m scheme (defmacro foo (x . args) x) (define (bar x) (foo x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('defmacro'), Symbol.for('foo'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], Symbol.for('x')], [Symbol.for('define'), [Symbol.for('bar'), Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]]]]], 'function foo(exp, env) {\n' +
      '  let [x, ...args] = exp.slice(1);\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'macro\';\n' +
      '\n' +
      'function bar(x) {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(module m scheme (defmacro foo (x . args) x) (define bar (foo 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('defmacro'), Symbol.for('foo'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], Symbol.for('x')], [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('foo'), 1]]]]], 'function foo(exp, env) {\n' +
      '  let [x, ...args] = exp.slice(1);\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'macro\';\n' +
      '\n' +
      'let bar = 1;']);
  });
  it('(compile \'(begin (defmacro foo (x . args) x) (define bar (foo 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('defmacro'), Symbol.for('foo'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], Symbol.for('x')], [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('foo'), 1]]]]], 'function foo(exp, env) {\n' +
      '  let [x, ...args] = exp.slice(1);\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'macro\';\n' +
      '\n' +
      'let bar = 1;']);
  });
  it('(compile \'(begin (define (foo x) x) (defmacro bar (x) (foo x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'function foo(x) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return foo(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = 1;']);
  });
  it('(compile \'(begin (define (foo-bar x) x) (defmacro bar (x) (foo-bar x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('foo-bar'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo-bar'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'function fooBar(x) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return fooBar(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = 1;']);
  });
  it('(compile \'(begin (define (foo-bar x) x) (defmacro bar (x) (foo-bar \'x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('foo-bar'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo-bar'), [Symbol.for('quote'), Symbol.for('x')]]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'function fooBar(x) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return fooBar(Symbol.for(\'x\'));\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = x;']);
  });
  it('(compile \'(begin (define (foo-bar x) \'x) (defmacro bar (x) (foo-bar x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('foo-bar'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('x')]], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo-bar'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'function fooBar(x) {\n' +
      '  return Symbol.for(\'x\');\n' +
      '}\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return fooBar(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = x;']);
  });
  it('(compile \'(module m scheme (define (foo-bar x) (keyword? x)) (defmacro bar (x) (foo-bar x)) (define baz (bar 1))) :finline-functions #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo-bar'), Symbol.for('x')], [Symbol.for('keyword?'), Symbol.for('x')]], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo-bar'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]], Symbol.for(':finline-functions'), true], 'let [keywordp] = (() => {\n' +
      '  function keywordp_(obj) {\n' +
      '    return (typeof obj === \'symbol\') && (obj.description.match(new RegExp(\'^:\')) ? true : false);\n' +
      '  }\n' +
      '  return [keywordp_];\n' +
      '})();\n' +
      '\n' +
      'function fooBar(x) {\n' +
      '  return keywordp(x);\n' +
      '}\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return fooBar(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = false;']);
  });
  it('(compile \'(begin (define foo (lambda (x) x)) (defmacro bar (x) (foo x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'let foo = function (x) {\n' +
      '  return x;\n' +
      '};\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return foo(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = 1;']);
  });
  it('(compile \'(begin (define-values (foo) (list (lambda (x) x))) (defmacro bar (x) (foo x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-values'), [Symbol.for('foo')], [Symbol.for('list'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'let [foo] = [function (x) {\n' +
      '  return x;\n' +
      '}];\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return foo(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = 1;']);
  });
  it('(compile \'(begin (define-fields (foo) (js/obj "foo" (lambda (x) x))) (defmacro bar (x) (foo x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-fields'), [Symbol.for('foo')], [Symbol.for('js/obj'), 'foo', [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'let {foo} = {\n' +
      '  foo: function (x) {\n' +
      '    return x;\n' +
      '  }\n' +
      '};\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return foo(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = 1;']);
  });
  it('(compile \'(begin (define-fields ((foo foo1)) (js/obj "foo" (lambda (x) x))) (defmacro bar (x) (foo1 x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-fields'), [[Symbol.for('foo'), Symbol.for('foo1')]], [Symbol.for('js/obj'), 'foo', [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo1'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'let {foo: foo1} = {\n' +
      '  foo: function (x) {\n' +
      '    return x;\n' +
      '  }\n' +
      '};\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return foo1(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = 1;']);
  });
  it('(compile \'(begin (define/async (foo x) x) (defmacro bar (x) (foo x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define/async'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'async function foo(x) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return foo(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = 1;']);
  });
  it('(compile \'(begin (define foo (async (lambda (x) x))) (defmacro bar (x) (foo x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'async function foo(x) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return foo(x);\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = 1;']);
  });
  it('(compile \'(begin (define-fexpr (foo x) x) (defmacro bar (x) (foo x)) (define baz (bar 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-fexpr'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('defmacro'), Symbol.for('bar'), [Symbol.for('x')], [Symbol.for('foo'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]]]]], 'function foo(x) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'fexpr\';\n' +
      '\n' +
      'function bar(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return foo(Symbol.for(\'x\'));\n' +
      '}\n' +
      '\n' +
      'bar.ftype = \'macro\';\n' +
      '\n' +
      'let baz = x;']);
  });
  return it('(compile \'(begin (define-class Foo () (define/public (foo) "foo")) (define bar (new Foo)) (defmacro baz (x) (send bar foo)) (define quux (baz 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('foo')], 'foo']], [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('defmacro'), Symbol.for('baz'), [Symbol.for('x')], [Symbol.for('send'), Symbol.for('bar'), Symbol.for('foo')]], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('baz'), 1]]]]], 'class Foo {\n' +
      '  foo() {\n' +
      '    return \'foo\';\n' +
      '  }\n' +
      '}\n' +
      '\n' +
      'let bar = new Foo();\n' +
      '\n' +
      'function baz(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  return bar.foo();\n' +
      '}\n' +
      '\n' +
      'baz.ftype = \'macro\';\n' +
      '\n' +
      'let quux = \'foo\';']);
  });
});

describe('unwind-protect', function (): any {
  it('(unwind-protect 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('unwind-protect'), 1, 2, 3], 1]);
  });
  return it('(compile \'(unwind-protect (foo) (bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('unwind-protect'), [Symbol.for('foo')], [Symbol.for('bar')]]]], 'try {\n' +
      '  foo();\n' +
      '} finally {\n' +
      '  bar();\n' +
      '}']);
  });
});

describe('clj/try', function (): any {
  it('(clj/try (/ 1 2) (catch Exception e "there was an error") (finally (display "finally")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('clj/try'), [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('Exception'), Symbol.for('e'), 'there was an error'], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]], 0.5]);
  });
  it('(clj/try (/ 1 3) (/ 1 2) (catch Exception e "there was an error") (finally (display "finally")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('clj/try'), [Symbol.for('/'), 1, 3], [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('Exception'), Symbol.for('e'), 'there was an error'], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]], 0.5]);
  });
  it('(compile \'(clj/try))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('clj/try')]]], 'try {\n' +
      '}']);
  });
  it('(compile \'(clj/try (set! x (/ 2 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('clj/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '}']);
  });
  it('(compile \'(clj/try (set! x (/ 2 1)) (finally (display "cleanup"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('clj/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '} finally {\n' +
      '  console.log(\'cleanup\');\n' +
      '}']);
  });
  it('(compile \'(clj/try (/ 1 2) (catch Object e (display "there was an error")) (finally (display "finally"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('clj/try'), [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]]]], 'try {\n' +
      '  1 / 2;\n' +
      '} catch (e) {\n' +
      '  console.log(\'there was an error\');\n' +
      '} finally {\n' +
      '  console.log(\'finally\');\n' +
      '}']);
  });
  it('(compile \'(clj/try (/ 1 2) (catch Exception e (display "there was an error")) (finally (display "finally"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('clj/try'), [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('Exception'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]]]], 'try {\n' +
      '  1 / 2;\n' +
      '} catch (e) {\n' +
      '  if (e instanceof Exception) {\n' +
      '    console.log(\'there was an error\');\n' +
      '  } else {\n' +
      '    throw e;\n' +
      '  }\n' +
      '} finally {\n' +
      '  console.log(\'finally\');\n' +
      '}']);
  });
  it('(compile \'(clj/try (set! x (/ 2 1)) (catch Object e (display "there was an error")) (finally (display "cleanup"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('clj/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '} catch (e) {\n' +
      '  console.log(\'there was an error\');\n' +
      '} finally {\n' +
      '  console.log(\'cleanup\');\n' +
      '}']);
  });
  it('(compile \'(clj/try (set! x (/ 2 1)) (catch MyException e (display "there was an error") (return #f)) (finally (display "cleanup"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('clj/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('MyException'), Symbol.for('e'), [Symbol.for('display'), 'there was an error'], [Symbol.for('return'), false]], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '} catch (e) {\n' +
      '  if (e instanceof MyException) {\n' +
      '    console.log(\'there was an error\');\n' +
      '    return false;\n' +
      '  } else {\n' +
      '    throw e;\n' +
      '  }\n' +
      '} finally {\n' +
      '  console.log(\'cleanup\');\n' +
      '}']);
  });
  return it('(compile \'(clj/try (set! x (/ 2 1)) (catch Object e (display "there was an error") (return #f)) (finally (display "cleanup"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('clj/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e'), [Symbol.for('display'), 'there was an error'], [Symbol.for('return'), false]], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '} catch (e) {\n' +
      '  console.log(\'there was an error\');\n' +
      '  return false;\n' +
      '} finally {\n' +
      '  console.log(\'cleanup\');\n' +
      '}']);
  });
});

describe('cl/loop', function (): any {
  it('(compile \'(cl/loop for n in names collect (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cl/loop'), Symbol.for('for'), Symbol.for('n'), Symbol.for('in'), Symbol.for('names'), Symbol.for('collect'), [Symbol.for('foo')]]]], 'let result = [];\n' +
      '\n' +
      'for (let n of names) {\n' +
      '  result.push(foo());\n' +
      '}\n' +
      '\n' +
      'result;']);
  });
  return it('(compile \'(cl/loop for g in gensyms for n in names collect (list g n)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cl/loop'), Symbol.for('for'), Symbol.for('g'), Symbol.for('in'), Symbol.for('gensyms'), Symbol.for('for'), Symbol.for('n'), Symbol.for('in'), Symbol.for('names'), Symbol.for('collect'), [Symbol.for('list'), Symbol.for('g'), Symbol.for('n')]]]], 'let result = [];\n' +
      '\n' +
      'let _end = gensyms.length;\n' +
      '\n' +
      'let _end1 = names.length;\n' +
      '\n' +
      'for (let i = 0, j = 0; (i < _end) && (j < _end1); i++, j++) {\n' +
      '  let g = gensyms[i];\n' +
      '  let n = names[j];\n' +
      '  result.push([g, n]);\n' +
      '}\n' +
      '\n' +
      'result;']);
  });
});

describe('with-gensyms', function (): any {
  return it('(compile \'(with-gensyms (x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('with-gensyms'), [Symbol.for('x')], Symbol.for('x')]]], 'let x = Symbol(\'g\');\n' +
      '\n' +
      'x;']);
  });
});

describe('once-only', function (): any {
  return it('(compile \'(begin (define-macro (my-square x) (once-only (x) `(* ,x ,x))) (my-square (+ 1 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-macro'), [Symbol.for('my-square'), Symbol.for('x')], [Symbol.for('once-only'), [Symbol.for('x')], [Symbol.for('quasiquote'), [Symbol.for('*'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('x')]]]]], [Symbol.for('my-square'), [Symbol.for('+'), 1, 1]]]]], 'function mySquare(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  let g = Symbol(\'g\');\n' +
      '  return [Symbol.for(\'let\'), [[g, x]], (() => {\n' +
      '    let x = g;\n' +
      '    return [Symbol.for(\'*\'), x, x];\n' +
      '  })()];\n' +
      '}\n' +
      '\n' +
      'mySquare.ftype = \'macro\';\n' +
      '\n' +
      'let g = 1 + 1;\n' +
      '\n' +
      'g * g;']);
  });
});