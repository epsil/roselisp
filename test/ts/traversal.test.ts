import {
  lispEnvironment,
  mapSyntax
} from '../../src/ts/language';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('map-syntax', (): any => {
  it('()', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [], lispEnvironment);
    return expressions;
  })([]), [[]]));
  it('(f x)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('f'), Symbol.for('x')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('f'), Symbol.for('x'), [Symbol.for('f'), Symbol.for('x')]]));
  it('(f (g x) y)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')], Symbol.for('y')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('f'), Symbol.for('g'), Symbol.for('x'), [Symbol.for('g'), Symbol.for('x')], Symbol.for('y'), [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')], Symbol.for('y')]]));
  it('(begin x y)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('begin'), Symbol.for('x'), Symbol.for('y')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('x'), Symbol.for('y'), [Symbol.for('begin'), Symbol.for('x'), Symbol.for('y')]]));
  it('(begin0 x y)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('begin0'), Symbol.for('x'), Symbol.for('y')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('x'), Symbol.for('y'), [Symbol.for('begin0'), Symbol.for('x'), Symbol.for('y')]]));
  it('(let ((x 1)) x)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('x'), 1, Symbol.for('x'), [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')]]));
  it('(let-values (((x) (foo))) x)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('let-values'), [[[Symbol.for('x')], [Symbol.for('foo')]]], Symbol.for('x')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('x'), Symbol.for('foo'), [Symbol.for('foo')], Symbol.for('x'), [Symbol.for('let-values'), [[[Symbol.for('x')], [Symbol.for('foo')]]], Symbol.for('x')]]));
  it('(cond ((foo bar) (baz quux)))', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('cond'), [[Symbol.for('foo'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('quux')]]], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('foo'), Symbol.for('bar'), [Symbol.for('foo'), Symbol.for('bar')], Symbol.for('baz'), Symbol.for('quux'), [Symbol.for('baz'), Symbol.for('quux')], [Symbol.for('cond'), [[Symbol.for('foo'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('quux')]]]]));
  it('(cond ((foo bar) (baz quux)))', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('cond'), [[Symbol.for('foo'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('quux')]]], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('foo'), Symbol.for('bar'), [Symbol.for('foo'), Symbol.for('bar')], Symbol.for('baz'), Symbol.for('quux'), [Symbol.for('baz'), Symbol.for('quux')], [Symbol.for('cond'), [[Symbol.for('foo'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('quux')]]]]));
  it('(lambda (x) x)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('x'), Symbol.for('x'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]));
  it('(lambda (x (y 1)) x)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('lambda'), [Symbol.for('x'), [Symbol.for('y'), 1]], Symbol.for('x')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('x'), Symbol.for('y'), 1, Symbol.for('x'), [Symbol.for('lambda'), [Symbol.for('x'), [Symbol.for('y'), 1]], Symbol.for('x')]]));
  it('(define (I x) x)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('I'), Symbol.for('x'), Symbol.for('x'), [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')]]));
  it('(define I (lambda (x) x))', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('define'), Symbol.for('I'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('I'), Symbol.for('x'), Symbol.for('x'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], [Symbol.for('define'), Symbol.for('I'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]));
  it('(quasiquote x)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('quasiquote'), Symbol.for('x')], lispEnvironment);
    return expressions;
  })([]), [[Symbol.for('quasiquote'), Symbol.for('x')]]));
  it('(quasiquote (x))', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('quasiquote'), [Symbol.for('x')]], lispEnvironment);
    return expressions;
  })([]), [[Symbol.for('quasiquote'), [Symbol.for('x')]]]));
  it('(quasiquote (unquote x))', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('quasiquote'), [Symbol.for('unquote'), Symbol.for('x')]], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('x'), [Symbol.for('quasiquote'), [Symbol.for('unquote'), Symbol.for('x')]]]));
  it('(quasiquote (x (unquote y) (unquote-splicing z)))', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('quasiquote'), [Symbol.for('x'), [Symbol.for('unquote'), Symbol.for('y')], [Symbol.for('unquote-splicing'), Symbol.for('z')]]], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('y'), Symbol.for('z'), [Symbol.for('quasiquote'), [Symbol.for('x'), [Symbol.for('unquote'), Symbol.for('y')], [Symbol.for('unquote-splicing'), Symbol.for('z')]]]]));
  return it('(defmacro f (x) x)', (): any => assertEqual(((expressions: any): any => {
    mapSyntax((x: any): any => {
      expressions.push(x);
      return x;
    }, [Symbol.for('defmacro'), Symbol.for('f'), [Symbol.for('x')], Symbol.for('x')], lispEnvironment);
    return expressions;
  })([]), [Symbol.for('f'), Symbol.for('x'), Symbol.for('x'), [Symbol.for('defmacro'), Symbol.for('f'), [Symbol.for('x')], Symbol.for('x')]]));
});