/**
 * # Lisp tests
 *
 * Tests of some non-Scheme Lisp constructs. Intended to exercise
 * the language's capability to implement other Lisp dialects.
 */

import {
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('nil', function (): any {
  it('nil', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('nil'), [Symbol.for('quote'), []]]);
  });
  it('(list? nil)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list?'), Symbol.for('nil')], true]);
  });
  it('(length nil)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('length'), Symbol.for('nil')], 0]);
  });
  return it('(compile \'nil)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('nil')]], '[];']);
  });
});

describe('intern', function (): any {
  return it('(intern "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('intern'), 'foo'], [Symbol.for('quote'), Symbol.for('foo')]]);
  });
});

describe('gensym', function (): any {
  it('(symbol? (gensym "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('symbol?'), [Symbol.for('gensym'), 'foo']], true]);
  });
  it('(eq? (gensym "foo") \'foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('eq?'), [Symbol.for('gensym'), 'foo'], [Symbol.for('quote'), Symbol.for('foo')]], false]);
  });
  it('(eq? (gensym "foo") (gensym "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('eq?'), [Symbol.for('gensym'), 'foo'], [Symbol.for('gensym'), 'foo']], false]);
  });
  it('(compile \'(gensym "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('gensym'), 'foo']]], 'Symbol(\'foo\');']);
  });
  it('(compile `(begin ,(gensym "x")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']]]]], 'x;']);
  });
  it('(compile `(define ,(gensym "x") 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']], 1]]], 'let x = 1;']);
  });
  it('(compile `(define x ,(gensym "x")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']]]]], 'let x = x1;']);
  });
  it('(compile `(define x \',(gensym "x")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']]]]]], 'let x = Symbol.for(\'x1\');']);
  });
  it('(compile `(let ((x ,(gensym "x"))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('x'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']]]], [Symbol.for('foo')]]]], 'let x = x1;\n' +
      '\n' +
      'foo();']);
  });
  it('(compile `(let ((x \',(gensym "x"))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('x'), [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']]]]], [Symbol.for('foo')]]]], 'let x = Symbol.for(\'x1\');\n' +
      '\n' +
      'foo();']);
  });
  it('(compile `(let ((x \',(gensym "x"))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('x'), [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']]]]], [Symbol.for('foo')]]]], 'let x = Symbol.for(\'x1\');\n' +
      '\n' +
      'foo();']);
  });
  it('(compile `(let ((x 0)) (define ,(gensym "x") 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('x'), 0]], [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']], 1]]]], 'let x = 0;\n' +
      '\n' +
      'let x1 = 1;']);
  });
  it('(compile `(let ((x 1)) (define y \',(gensym "x"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('x'), 1]], [Symbol.for('define'), Symbol.for('y'), [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']]]]]]], 'let x = 1;\n' +
      '\n' +
      'let y = Symbol.for(\'x1\');']);
  });
  it('(compile `(let ((x 0)) (define ,(gensym "x") 1) (let ((x1 0)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('x'), 0]], [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']], 1], [Symbol.for('let'), [[Symbol.for('x1'), 0]]]]]], 'let x = 0;\n' +
      '\n' +
      'let x2 = 1;\n' +
      '\n' +
      'let x1 = 0;']);
  });
  it('(compile `(begin (define x 1) (define ,(gensym "x") 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']], 2]]]], 'let x = 1;\n' +
      '\n' +
      'let x1 = 2;']);
  });
  it('(compile `(begin (define x 1) (define x1 2) (define ,(gensym "x") 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), Symbol.for('x1'), 2], [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']], 3]]]], 'let x = 1;\n' +
      '\n' +
      'let x1 = 2;\n' +
      '\n' +
      'let x2 = 3;']);
  });
  it('(compile `(begin (define x 1) (define ,(gensym "x") 2) (define x1 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']], 2], [Symbol.for('define'), Symbol.for('x1'), 3]]]], 'let x = 1;\n' +
      '\n' +
      'let x2 = 2;\n' +
      '\n' +
      'let x1 = 3;']);
  });
  it('(compile `(begin (define x 1) (define ,(gensym "x") 2) (define-values (x1) (list 3))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']], 2], [Symbol.for('define-values'), [Symbol.for('x1')], [Symbol.for('list'), 3]]]]], 'let x = 1;\n' +
      '\n' +
      'let x2 = 2;\n' +
      '\n' +
      'let [x1] = [3];']);
  });
  it('(compile `(begin (define x 1) (define (,(gensym "x")) 2) (define-values (x1) (list 3))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), [[Symbol.for('unquote'), [Symbol.for('gensym'), 'x']]], 2], [Symbol.for('define-values'), [Symbol.for('x1')], [Symbol.for('list'), 3]]]]], 'let x = 1;\n' +
      '\n' +
      'function x2() {\n' +
      '  return 2;\n' +
      '}\n' +
      '\n' +
      'let [x1] = [3];']);
  });
  it('(compile `(begin (define x 1) (define ,(gensym "x") 2) (define ,(gensym "x") 3) (define x1 4) (define x2 5)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']], 2], [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'x']], 3], [Symbol.for('define'), Symbol.for('x1'), 4], [Symbol.for('define'), Symbol.for('x2'), 5]]]], 'let x = 1;\n' +
      '\n' +
      'let x3 = 2;\n' +
      '\n' +
      'let x4 = 3;\n' +
      '\n' +
      'let x1 = 4;\n' +
      '\n' +
      'let x2 = 5;']);
  });
  it('(compile (let ((gensym-x (gensym "x"))) `(let ((x 0)) (define ,gensym-x 1) (let ((x1 0)) (define ,gensym-x 1)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('let'), [[Symbol.for('gensym-x'), [Symbol.for('gensym'), 'x']]], [Symbol.for('quasiquote'), [Symbol.for('let'), [[Symbol.for('x'), 0]], [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('gensym-x')], 1], [Symbol.for('let'), [[Symbol.for('x1'), 0]], [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('gensym-x')], 1]]]]]], 'let x = 0;\n' +
      '\n' +
      'let x2 = 1;\n' +
      '\n' +
      'let x1 = 0;\n' +
      '\n' +
      'let x2 = 1;']);
  });
  return it('(compile `(begin (define foo ,(gensym "test")) (define bar ,(gensym "test"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'test']]], [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'test']]]]]], 'let foo = test;\n' +
      '\n' +
      'let bar = test1;']);
  });
});

describe('Keywords', function (): any {
  it(':foo', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for(':foo'), [Symbol.for('quote'), Symbol.for(':foo')]]);
  });
  it('\':foo', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), Symbol.for(':foo')], [Symbol.for('quote'), Symbol.for(':foo')]]);
  });
  it('(keyword? \'foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('keyword?'), [Symbol.for('quote'), Symbol.for('foo')]], false]);
  });
  it('(keyword? \':foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('keyword?'), [Symbol.for('quote'), Symbol.for(':foo')]], true]);
  });
  it('(keyword? \':foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('keyword?'), [Symbol.for('quote'), Symbol.for(':foo')]], true]);
  });
  it('(eq? \':foo \':foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('eq?'), [Symbol.for('quote'), Symbol.for(':foo')], [Symbol.for('quote'), Symbol.for(':foo')]], true]);
  });
  it('(compile \':foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for(':foo')]], 'Symbol.for(\':foo\');']);
  });
  return it('(compile \':foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for(':foo')]], 'Symbol.for(\':foo\');']);
  });
});

describe('nth', function (): any {
  it('(nth 0 \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nth'), 0, [Symbol.for('quote'), [1]]], 1]);
  });
  it('(nth 1 \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nth'), 1, [Symbol.for('quote'), [1, 2]]], 2]);
  });
  it('(nth 2 \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nth'), 2, [Symbol.for('quote'), [1, 2, 3]]], 3]);
  });
  it('(funcall nth 1 \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('nth'), 1, [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 2]);
  });
  it('(funcall nth 1 \'(1 2 . (3 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('nth'), 1, [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]]], 2]);
  });
  it('(compile \'(nth n x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('nth'), Symbol.for('n'), Symbol.for('x')]]], 'x[n];']);
  });
  it('(compile \'(module m scheme (nth n x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('nth'), Symbol.for('n'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[n];']);
  });
  return it('(compile \'(module m scheme (nth n x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('nth'), Symbol.for('n'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  nth\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'nth(n, x);']);
  });
});

describe('aref', function (): any {
  it('(compile \'(aref args 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('aref'), Symbol.for('args'), 0]]], 'args[0];']);
  });
  return it('(compile \'(aref args 0 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('aref'), Symbol.for('args'), 0, 1]]], 'args[0][1];']);
  });
});

describe('aget', function (): any {
  it('(compile \'(aget args 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('aget'), Symbol.for('args'), 0]]], 'args[0];']);
  });
  return it('(compile \'(aget args 0 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('aget'), Symbol.for('args'), 0, 1]]], 'args[0][1];']);
  });
});

describe('aset!', function (): any {
  return it('(compile \'(aset! args 0 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('aset!'), Symbol.for('args'), 0, 1]]], 'args[0] = 1;']);
  });
});

describe('nthcdr', function (): any {
  it('(nthcdr 0 \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nthcdr'), 0, [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(nthcdr 1 \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nthcdr'), 1, [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('quote'), [2, 3]]]);
  });
  it('(nthcdr 2 \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nthcdr'), 2, [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('quote'), [3]]]);
  });
  it('(nthcdr 3 \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nthcdr'), 3, [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('quote'), []]]);
  });
  it('(nthcdr 1 \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nthcdr'), 1, [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], 2]);
  });
  it('(compile \'(module m scheme (nthcdr n x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('nthcdr'), Symbol.for('n'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'import {\n' +
      '  nthcdr\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'nthcdr(n, x);']);
  });
  return it('(compile \'(module m scheme (nthcdr n x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('nthcdr'), Symbol.for('n'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  nthcdr\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'nthcdr(n, x);']);
  });
});

describe('funcall', function (): any {
  it('(compile \'(funcall f))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('funcall'), Symbol.for('f')]]], 'f();']);
  });
  it('(compile \'(funcall f x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('funcall'), Symbol.for('f'), Symbol.for('x')]]], 'f(x);']);
  });
  it('(compile \'(funcall f x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('funcall'), Symbol.for('f'), Symbol.for('x'), Symbol.for('y')]]], 'f(x, y);']);
  });
  return it('(compile \'(module m scheme (funcall length x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('funcall'), Symbol.for('length'), Symbol.for('x')]]]], 'import {\n' +
      '  length\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'length(x);']);
  });
});

describe('while', function (): any {
  it('(let ((result \'())) (while (< (length result) 3) (set! result (cons 1 result))) result)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('while'), [Symbol.for('<'), [Symbol.for('length'), Symbol.for('result')], 3], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('cons'), 1, Symbol.for('result')]]], Symbol.for('result')], [Symbol.for('quote'), [1, 1, 1]]]);
  });
  return it('(compile \'(while (> x 0) (set! x (- x 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('while'), [Symbol.for('>'), Symbol.for('x'), 0], [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('-'), Symbol.for('x'), 1]]]]], 'while (x > 0) {\n' +
      '  x--;\n' +
      '}']);
  });
});

describe('defclass', function (): any {
  it('((lambda () (defclass Foo () (define/public (bar) "bar")) (define foo (new Foo)) (send foo bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defclass'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar']], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('send'), Symbol.for('foo'), Symbol.for('bar')]]], 'bar']);
  });
  return it('(compile \'(defclass Foo () (define/public (bar) "bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('defclass'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar']]]], 'class Foo {\n' +
      '  bar() {\n' +
      '    return \'bar\';\n' +
      '  }\n' +
      '}']);
  });
});

describe('->', function (): any {
  it('(compile \'(-> x (.foo "bar") (.baz)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('x'), [Symbol.for('.foo'), 'bar'], [Symbol.for('.baz')]]]], 'x.foo(\'bar\').baz();']);
  });
  return it('(compile \'(-> regular-args (.map (lambda (arg) (compile-expression arg env inherited-options))) (.join ", ")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('regular-args'), [Symbol.for('.map'), [Symbol.for('lambda'), [Symbol.for('arg')], [Symbol.for('compile-expression'), Symbol.for('arg'), Symbol.for('env'), Symbol.for('inherited-options')]]], [Symbol.for('.join'), ', ']]]], 'regularArgs.map(function (arg) {\n' +
      '  return compileExpression(arg, env, inheritedOptions);\n' +
      '}).join(\', \');']);
  });
});

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

describe('defsubst', function (): any {
  return it('(compile \'(defsubst my-plus (x y) (+ x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('defsubst'), Symbol.for('my-plus'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]], 'function myPlus(x, y) {\n' +
      '  return x + y;\n' +
      '}\n' +
      '\n' +
      'myPlus.compilerMacro = (() => {\n' +
      '  let f = function (exp, env) {\n' +
      '    let [x, y] = exp.slice(1);\n' +
      '    return [Symbol.for(\'+\'), x, y];\n' +
      '  };\n' +
      '  f.ftype = \'macro\';\n' +
      '  return f;\n' +
      '})();']);
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

describe('macro', function (): any {
  it('(compile \'(macro (x y) `(+ ,x ,y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('macro'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]]], 'let f = function (exp, env) {\n' +
      '  let [x, y] = exp.slice(1);\n' +
      '  return [Symbol.for(\'+\'), x, y];\n' +
      '};\n' +
      '\n' +
      'f.ftype = \'macro\';\n' +
      '\n' +
      'f;']);
  });
  return it('(compile \'(define f (macro (x y) `(+ ,x ,y))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('f'), [Symbol.for('macro'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]]]], 'let f = (() => {\n' +
      '  let f1 = function (exp, env) {\n' +
      '    let [x, y] = exp.slice(1);\n' +
      '    return [Symbol.for(\'+\'), x, y];\n' +
      '  };\n' +
      '  f1.ftype = \'macro\';\n' +
      '  return f1;\n' +
      '})();']);
  });
});

describe('nlambda', function (): any {
  it('(compile \'(nlambda (x y) `(+ ,x ,y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('nlambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]]], 'let f = function (x, y) {\n' +
      '  return [Symbol.for(\'+\'), x, y];\n' +
      '};\n' +
      '\n' +
      'f.ftype = \'fexpr\';\n' +
      '\n' +
      'f;']);
  });
  return it('(compile \'(define f (nlambda (x y) `(+ ,x ,y))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('f'), [Symbol.for('nlambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]]]], 'let f = (() => {\n' +
      '  let f1 = function (x, y) {\n' +
      '    return [Symbol.for(\'+\'), x, y];\n' +
      '  };\n' +
      '  f1.ftype = \'fexpr\';\n' +
      '  return f1;\n' +
      '})();']);
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
  it('(compile \'(begin (define-macro (my-square x) (once-only (x) `(* ,x ,x))) (my-square (+ 1 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-macro'), [Symbol.for('my-square'), Symbol.for('x')], [Symbol.for('once-only'), [Symbol.for('x')], [Symbol.for('quasiquote'), [Symbol.for('*'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('x')]]]]], [Symbol.for('my-square'), [Symbol.for('+'), 1, 1]]]]], 'function mySquare(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  let x1 = Symbol(\'x\');\n' +
      '  return [Symbol.for(\'let\'), [[x1, x]], ((x) => {\n' +
      '    return [Symbol.for(\'*\'), x, x];\n' +
      '  })(x1)];\n' +
      '}\n' +
      '\n' +
      'mySquare.ftype = \'macro\';\n' +
      '\n' +
      'let x = 1 + 1;\n' +
      '\n' +
      'x * x;']);
  });
  return it('(compile \'(begin (define-macro (my-plus x y) (once-only (x y) `(+ ,x ,y))) (my-plus (+ 1 1) (+ 2 2))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-macro'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('once-only'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]], [Symbol.for('my-plus'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 2, 2]]]]], 'function myPlus(exp, env) {\n' +
      '  let [x, y] = exp.slice(1);\n' +
      '  let x1 = Symbol(\'x\');\n' +
      '  let y1 = Symbol(\'y\');\n' +
      '  return [Symbol.for(\'let\'), [[x1, x], [y1, y]], ((x, y) => {\n' +
      '    return [Symbol.for(\'+\'), x, y];\n' +
      '  })(x1, y1)];\n' +
      '}\n' +
      '\n' +
      'myPlus.ftype = \'macro\';\n' +
      '\n' +
      'let x = 1 + 1;\n' +
      '\n' +
      'let y = 2 + 2;\n' +
      '\n' +
      'x + y;']);
  });
});

describe('once-only*', function (): any {
  return it('(compile \'(module m scheme (define-macro (my-plus x y) (once-only* (x y) `(+ ,x ,y))) (my-plus (+ 1 1) 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define-macro'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('once-only*'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]], [Symbol.for('my-plus'), [Symbol.for('+'), 1, 1], 2]]]], 'function myPlus(exp, env) {\n' +
      '  let [x, y] = exp.slice(1);\n' +
      '  if (!(Array.isArray(x) && (x.length > 0))) {\n' +
      '    if (!(Array.isArray(y) && (y.length > 0))) {\n' +
      '      return [Symbol.for(\'+\'), x, y];\n' +
      '    } else {\n' +
      '      let y1 = Symbol(\'y\');\n' +
      '      return [Symbol.for(\'let\'), [[y1, y]], ((y) => {\n' +
      '        return [Symbol.for(\'+\'), x, y];\n' +
      '      })(y1)];\n' +
      '    }\n' +
      '  } else {\n' +
      '    if (!(Array.isArray(y) && (y.length > 0))) {\n' +
      '      let x1 = Symbol(\'x\');\n' +
      '      return [Symbol.for(\'let\'), [[x1, x]], ((x) => {\n' +
      '        return [Symbol.for(\'+\'), x, y];\n' +
      '      })(x1)];\n' +
      '    } else {\n' +
      '      let x2 = Symbol(\'x\');\n' +
      '      let y2 = Symbol(\'y\');\n' +
      '      return [Symbol.for(\'let\'), [[x2, x], [y2, y]], ((x, y) => {\n' +
      '        return [Symbol.for(\'+\'), x, y];\n' +
      '      })(x2, y2)];\n' +
      '    }\n' +
      '  }\n' +
      '}\n' +
      '\n' +
      'myPlus.ftype = \'macro\';\n' +
      '\n' +
      'let x = 1 + 1;\n' +
      '\n' +
      'x + 2;']);
  });
});