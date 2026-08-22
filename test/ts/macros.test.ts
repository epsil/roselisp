import {
  case_,
  caseEq_,
  threadAs_,
  threadFirst_,
  threadLast_
} from '../../src/ts/macros';

import {
  LispEnvironment,
  macroexpand,
  macroexpandStar,
  macroexpand1,
  macroexpandAll,
  makeLisp
} from '../../src/ts';

import {
  assertEqual,
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('macroexpand', function (): any {
  it('(macroexpand \'(foo bar) (new LispEnvironment `((foo ,(fn (exp env) \'(baz)) "macro"))))', function (): any {
    return assertEqual(macroexpand([Symbol.for('foo'), Symbol.for('bar')], new LispEnvironment([[Symbol.for('foo'), function (exp: any, env: any): any {
      return [Symbol.for('baz')];
    }, 'macro']])), [Symbol.for('baz')]);
  });
  return it('(macroexpand \'(+ 1 1) (new LispEnvironment))', function (): any {
    return assertEqual(macroexpand([Symbol.for('+'), 1, 1], new LispEnvironment()), [Symbol.for('+'), 1, 1]);
  });
});

describe('macroexpand*', function (): any {
  it('(macroexpand* \'(foo bar) (new LispEnvironment `((foo ,(fn (exp env) \'(baz)) "macro"))))', function (): any {
    return assertEqual(macroexpandStar([Symbol.for('foo'), Symbol.for('bar')], new LispEnvironment([[Symbol.for('foo'), function (exp: any, env: any): any {
      return [Symbol.for('baz')];
    }, 'macro']])), [[Symbol.for('baz')], true]);
  });
  return it('(macroexpand* \'(+ 1 1) (new LispEnvironment))', function (): any {
    return assertEqual(macroexpandStar([Symbol.for('+'), 1, 1], new LispEnvironment()), [[Symbol.for('+'), 1, 1], false]);
  });
});

describe('macroexpand-1', function (): any {
  it('(macroexpand-1 \'(~> "a b c d" .toUpperCase (.replace "A" "X") (.split " ") first) (make-lisp))', function (): any {
    return assertEqual(macroexpand1([Symbol.for('~>'), 'a b c d', Symbol.for('.toUpperCase'), [Symbol.for('.replace'), 'A', 'X'], [Symbol.for('.split'), ' '], Symbol.for('first')], makeLisp()), [Symbol.for('as~>'), 'a b c d', Symbol.for('_'), [Symbol.for('.toUpperCase'), Symbol.for('_')], [Symbol.for('.replace'), Symbol.for('_'), 'A', 'X'], [Symbol.for('.split'), Symbol.for('_'), ' '], [Symbol.for('first'), Symbol.for('_')]]);
  });
  it('(macroexpand-1 \'(~>> foo) (make-lisp))', function (): any {
    return assertEqual(macroexpand1([Symbol.for('~>>'), Symbol.for('foo')], makeLisp()), [Symbol.for('as~>'), Symbol.for('foo'), Symbol.for('_')]);
  });
  it('(macroexpand-1 \'(~>> foo (bar)) (make-lisp))', function (): any {
    return assertEqual(macroexpand1([Symbol.for('~>>'), Symbol.for('foo'), [Symbol.for('bar')]], makeLisp()), [Symbol.for('as~>'), Symbol.for('foo'), Symbol.for('_'), [Symbol.for('bar'), Symbol.for('_')]]);
  });
  return it('(macroexpand-1 \'(~>> (range) (map (fn (x) (* x x))) (filter even?) (take 10) (reduce +)) (make-lisp))', function (): any {
    return assertEqual(macroexpand1([Symbol.for('~>>'), [Symbol.for('range')], [Symbol.for('map'), [Symbol.for('fn'), [Symbol.for('x')], [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')]]], [Symbol.for('filter'), Symbol.for('even?')], [Symbol.for('take'), 10], [Symbol.for('reduce'), Symbol.for('+')]], makeLisp()), [Symbol.for('as~>'), [Symbol.for('range')], Symbol.for('_'), [Symbol.for('map'), [Symbol.for('fn'), [Symbol.for('x')], [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')]], Symbol.for('_')], [Symbol.for('filter'), Symbol.for('even?'), Symbol.for('_')], [Symbol.for('take'), 10, Symbol.for('_')], [Symbol.for('reduce'), Symbol.for('+'), Symbol.for('_')]]);
  });
});

describe('macroexpand-all', function (): any {
  it('(macroexpand-all \'(~> "a b c d" .toUpperCase (.replace "A" "X") (.split " ") first) (make-lisp))', function (): any {
    return assertEqual(macroexpandAll([Symbol.for('~>'), 'a b c d', Symbol.for('.toUpperCase'), [Symbol.for('.replace'), 'A', 'X'], [Symbol.for('.split'), ' '], Symbol.for('first')], makeLisp()), [Symbol.for('first'), [Symbol.for('.split'), [Symbol.for('.replace'), [Symbol.for('.toUpperCase'), 'a b c d'], 'A', 'X'], ' ']]);
  });
  xit('(macroexpand-all \'(begin (~> "a b c d" .toUpperCase (.replace "A" "X") (.split " ") first)) (make-lisp))', function (): any {
    return assertEqual(macroexpandAll([Symbol.for('begin'), [Symbol.for('~>'), 'a b c d', Symbol.for('.toUpperCase'), [Symbol.for('.replace'), 'A', 'X'], [Symbol.for('.split'), ' '], Symbol.for('first')]], makeLisp()), [Symbol.for('begin'), [Symbol.for('first'), [Symbol.for('.split'), [Symbol.for('.replace'), [Symbol.for('.toUpperCase'), 'a b c d'], 'A', 'X'], ' ']]]);
  });
  return xit('(macroexpand-all \'(begin (~> "a b c d" .toUpperCase (.replace "A" (~> "x" (.toUpperCase))) (.split " ") first)) (make-lisp))', function (): any {
    return assertEqual(macroexpandAll([Symbol.for('begin'), [Symbol.for('~>'), 'a b c d', Symbol.for('.toUpperCase'), [Symbol.for('.replace'), 'A', [Symbol.for('~>'), 'x', [Symbol.for('.toUpperCase')]]], [Symbol.for('.split'), ' '], Symbol.for('first')]], makeLisp()), [Symbol.for('begin'), [Symbol.for('first'), [Symbol.for('.split'), [Symbol.for('.replace'), [Symbol.for('.toUpperCase'), 'a b c d'], 'A', [Symbol.for('.toUpperCase'), 'x']], ' ']]]);
  });
});

describe('as->', function (): any {
  it('(thread-as_ \'(as~> x _))', function (): any {
    return assertEqual(threadAs_([Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_')]), Symbol.for('x'));
  });
  it('(thread-as_ \'(as~> x _ (foo)))', function (): any {
    return assertEqual(threadAs_([Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('foo')]]), [Symbol.for('begin'), Symbol.for('x'), [Symbol.for('foo')]]);
  });
  it('(thread-as_ \'(as~> x _ (foo) (bar)))', function (): any {
    return assertEqual(threadAs_([Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('foo')], [Symbol.for('bar')]]), [Symbol.for('begin'), Symbol.for('x'), [Symbol.for('foo')], [Symbol.for('bar')]]);
  });
  it('(thread-as_ \'(as~> x _ (+ _ 1)))', function (): any {
    return assertEqual(threadAs_([Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), 1]]), [Symbol.for('+'), Symbol.for('x'), 1]);
  });
  it('(thread-as_ \'(as~> x _ (+ _ _)))', function (): any {
    return assertEqual(threadAs_([Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), Symbol.for('_')]]), [Symbol.for('let'), [[Symbol.for('_'), Symbol.for('x')]], [Symbol.for('set!'), Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), Symbol.for('_')]], Symbol.for('_')]);
  });
  it('(thread-as_ \'(as~> x _ (+ _ 1) (+ _ 1)))', function (): any {
    return assertEqual(threadAs_([Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), 1], [Symbol.for('+'), Symbol.for('_'), 1]]), [Symbol.for('+'), [Symbol.for('+'), Symbol.for('x'), 1], 1]);
  });
  return it('(thread-as_ \'(as~> x _ (+ _ 1) (+ _ _)))', function (): any {
    return assertEqual(threadAs_([Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), 1], [Symbol.for('+'), Symbol.for('_'), Symbol.for('_')]]), [Symbol.for('let'), [[Symbol.for('_'), [Symbol.for('+'), Symbol.for('x'), 1]]], [Symbol.for('set!'), Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), Symbol.for('_')]], Symbol.for('_')]);
  });
});

describe('~>', function (): any {
  it('(thread-first_ \'(~> x foo))', function (): any {
    return assertEqual(threadFirst_([Symbol.for('~>'), Symbol.for('x'), Symbol.for('foo')]), [Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('foo'), Symbol.for('_')]]);
  });
  it('(thread-first_ \'(~> x (foo)))', function (): any {
    return assertEqual(threadFirst_([Symbol.for('~>'), Symbol.for('x'), [Symbol.for('foo')]]), [Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('foo'), Symbol.for('_')]]);
  });
  it('(thread-first_ \'(~> x (foo _)))', function (): any {
    return assertEqual(threadFirst_([Symbol.for('~>'), Symbol.for('x'), [Symbol.for('foo'), Symbol.for('_')]]), [Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('foo'), Symbol.for('_')]]);
  });
  return it('(thread-first_ \'(~> x :hole-marker * (foo *)))', function (): any {
    return assertEqual(threadFirst_([Symbol.for('~>'), Symbol.for('x'), Symbol.for(':hole-marker'), Symbol.for('*'), [Symbol.for('foo'), Symbol.for('*')]]), [Symbol.for('as~>'), Symbol.for('x'), Symbol.for('*'), [Symbol.for('foo'), Symbol.for('*')]]);
  });
});

describe('~>>', function (): any {
  it('(thread-last_ \'(~>> x foo))', function (): any {
    return assertEqual(threadLast_([Symbol.for('~>>'), Symbol.for('x'), Symbol.for('foo')]), [Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('foo'), Symbol.for('_')]]);
  });
  it('(thread-last_ \'(~>> x (foo)))', function (): any {
    return assertEqual(threadLast_([Symbol.for('~>>'), Symbol.for('x'), [Symbol.for('foo')]]), [Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('foo'), Symbol.for('_')]]);
  });
  it('(thread-last_ \'(~>> x (foo _)))', function (): any {
    return assertEqual(threadLast_([Symbol.for('~>>'), Symbol.for('x'), [Symbol.for('foo'), Symbol.for('_')]]), [Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_'), [Symbol.for('foo'), Symbol.for('_')]]);
  });
  return it('(thread-last_ \'(~>> x :hole-marker * (foo *)))', function (): any {
    return assertEqual(threadLast_([Symbol.for('~>>'), Symbol.for('x'), Symbol.for(':hole-marker'), Symbol.for('*'), [Symbol.for('foo'), Symbol.for('*')]]), [Symbol.for('as~>'), Symbol.for('x'), Symbol.for('*'), [Symbol.for('foo'), Symbol.for('*')]]);
  });
});

describe('case/eq', function (): any {
  it('(case-eq_ \'(case/eq x (("foo") foo) (else bar)))', function (): any {
    return assertEqual(caseEq_([Symbol.for('case/eq'), Symbol.for('x'), [['foo'], Symbol.for('foo')], [Symbol.for('else'), Symbol.for('bar')]]), [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), [Symbol.for('quote'), 'foo'], Symbol.for('foo'), [Symbol.for('break')]], [Symbol.for('default'), Symbol.for('bar')]]);
  });
  it('(case-eq_ \'(case/eq x (("foo" "bar") foo) (else baz)))', function (): any {
    return assertEqual(caseEq_([Symbol.for('case/eq'), Symbol.for('x'), [['foo', 'bar'], Symbol.for('foo')], [Symbol.for('else'), Symbol.for('baz')]]), [Symbol.for('cond'), [[Symbol.for('member?'), Symbol.for('x'), [Symbol.for('quote'), ['foo', 'bar']]], Symbol.for('foo')], [Symbol.for('else'), Symbol.for('baz')]]);
  });
  return it('(let* ((actual (case-eq_ \'(case/eq (get-field prop x) (("foo" "bar") foo) (else baz)))) (result-var (first (first (second actual)))) (expected `(let ((,result-var (get-field prop x))) (cond ((member? ,result-var \'("foo" "bar")) foo) (else baz))))) (assert-equal actual expected))', function (): any {
    return assertEqual(((): any => {
      const actual: any = caseEq_([Symbol.for('case/eq'), [Symbol.for('get-field'), Symbol.for('prop'), Symbol.for('x')], [['foo', 'bar'], Symbol.for('foo')], [Symbol.for('else'), Symbol.for('baz')]]);
      const resultVar: any = actual[1][0][0];
      const expected: any = [Symbol.for('let'), [[resultVar, [Symbol.for('get-field'), Symbol.for('prop'), Symbol.for('x')]]], [Symbol.for('cond'), [[Symbol.for('member?'), resultVar, [Symbol.for('quote'), ['foo', 'bar']]], Symbol.for('foo')], [Symbol.for('else'), Symbol.for('baz')]]];
      return assertEqual(actual, expected);
    })(), undefined);
  });
});

describe('case', function (): any {
  it('(case_ \'(case x (("foo") foo) (else bar)))', function (): any {
    return assertEqual(case_([Symbol.for('case'), Symbol.for('x'), [['foo'], Symbol.for('foo')], [Symbol.for('else'), Symbol.for('bar')]]), [Symbol.for('case/eq'), Symbol.for('x'), [['foo'], Symbol.for('foo')], [Symbol.for('else'), Symbol.for('bar')]]);
  });
  return it('(case_ \'(case x ((("foo")) foo) (else bar)))', function (): any {
    return assertEqual(case_([Symbol.for('case'), Symbol.for('x'), [[['foo']], Symbol.for('foo')], [Symbol.for('else'), Symbol.for('bar')]]), [Symbol.for('cond'), [[Symbol.for('member?'), Symbol.for('x'), [Symbol.for('quote'), [['foo']]], Symbol.for('equal?')], Symbol.for('foo')], [Symbol.for('else'), Symbol.for('bar')]]);
  });
});

describe('case', function (): any {
  return it('(case \'foo ((foo) 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('case'), [Symbol.for('quote'), Symbol.for('foo')], [[Symbol.for('foo')], 1]], 1]);
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