/**
 * # JavaScript
 *
 * Tests of JavaScript constructs.
 */

import {
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('js/is-NaN', (): any => {
  it('(js/is-NaN NaN)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/is-NaN'), Symbol.for('NaN')], true]));
  it('(js/is-NaN 0)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/is-NaN'), 0], false]));
  return it('(compile \'(js/is-NaN x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/is-NaN'), Symbol.for('x')]]], 'isNaN(x);']));
});

describe('js/[]', (): any => it('(compile \'(js/[] x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/[]'), Symbol.for('x'), Symbol.for('y')]]], 'x[y];'])));

describe('js/var', (): any => {
  it('(compile \'(js/var x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/var'), Symbol.for('x')]]], 'var x;']));
  it('(compile \'(js/var x 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/var'), Symbol.for('x'), 0]]], 'var x = 0;']));
  return it('(compile \'(js/var x 0 y 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/var'), Symbol.for('x'), 0, Symbol.for('y'), 0]]], 'var x = 0, y = 0;']));
});

describe('js/let', (): any => {
  it('(compile \'(js/let x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/let'), Symbol.for('x')]]], 'let x;']));
  it('(compile \'(js/let x 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/let'), Symbol.for('x'), 0]]], 'let x = 0;']));
  return it('(compile \'(js/let x 0 y 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/let'), Symbol.for('x'), 0, Symbol.for('y'), 0]]], 'let x = 0, y = 0;']));
});

describe('js/const', (): any => {
  it('(compile \'(js/const x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/const'), Symbol.for('x')]]], 'const x;']));
  it('(compile \'(js/const x 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/const'), Symbol.for('x'), 0]]], 'const x = 0;']));
  return it('(compile \'(js/const x 0 y 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/const'), Symbol.for('x'), 0, Symbol.for('y'), 0]]], 'const x = 0, y = 0;']));
});

describe('js/function', (): any => {
  it('(compile \'(js/function () 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [], 0]]], `function () {
  return 0;
};`]));
  it('(compile \'(js/function () : Number 0) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [], Symbol.for(':'), Symbol.for('Number'), 0]], Symbol.for(':to'), 'typescript'], `function (): number {
  return 0;
};`]));
  it('(compile \'(js/function () :name foo 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [], Symbol.for(':name'), Symbol.for('foo'), 0]]], `function foo() {
  return 0;
}`]));
  return it('(compile \'(js/function () : Number :name foo 0) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [], Symbol.for(':'), Symbol.for('Number'), Symbol.for(':name'), Symbol.for('foo'), 0]], Symbol.for(':to'), 'typescript'], `function foo(): number {
  return 0;
}`]));
});

describe('js/arrow', (): any => {
  it('((js/arrow () 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('js/arrow'), [], 1]], 1]));
  it('((js/arrow (x) x) 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('js/arrow'), [Symbol.for('x')], Symbol.for('x')], 1], 1]));
  it('((js/arrow (x y) x) 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('x')], 1, 2], 1]));
  it('(compile \'(js/arrow () 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [], 0]]], '() => 0;']));
  it('(compile \'(js/arrow () : Number 0) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [], Symbol.for(':'), Symbol.for('Number'), 0]], Symbol.for(':to'), 'typescript'], '(): number => 0;']));
  it('(compile \'(js/arrow (x) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [Symbol.for('x')], Symbol.for('x')]]], 'x => x;']));
  it('(compile \'(js/arrow (x) x) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [Symbol.for('x')], Symbol.for('x')]], Symbol.for(':to'), 'typescript'], '(x: any): any => x;']));
  it('(compile \'(js/arrow (x y) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('x')]]], '(x, y) => x;']));
  it('(compile \'(js/arrow (x y) x) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('x')]], Symbol.for(':to'), 'typescript'], '(x: any, y: any): any => x;']));
  it('(compile \'(js/arrow () :name foo 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [], Symbol.for(':name'), Symbol.for('foo'), 0]]], 'let foo = () => 0;']));
  it('(compile \'(js/arrow () : Number :name foo 0) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [], Symbol.for(':'), Symbol.for('Number'), Symbol.for(':name'), Symbol.for('foo'), 0]], Symbol.for(':to'), 'typescript'], 'let foo: any = (): number => 0;']));
  return it('(compile \'(js/arrow (x) (js/obj :foo x)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [Symbol.for('x')], [Symbol.for('js/obj'), Symbol.for(':foo'), Symbol.for('x')]]]], `x => ({
  foo: x
});`]));
});

describe('js/arrow?', (): any => {
  it('(js/arrow? (js/arrow (x) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/arrow?'), [Symbol.for('js/arrow'), [Symbol.for('x')], Symbol.for('x')]], true]));
  it('(js/arrow? (js/function (x) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/arrow?'), [Symbol.for('js/function'), [Symbol.for('x')], Symbol.for('x')]], false]));
  return it('(js/arrow? (lambda (x) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/arrow?'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]], true]));
});

describe('js/=>', (): any => it('(compile \'(js/=> () 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/=>'), [], 0]]], '() => 0;'])));

describe('js/iife', (): any => {
  it('(compile \'(js/iife (js/arrow (x y) (+ x y)) (list 1 2)) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('list'), 1, 2]]], Symbol.for(':as'), 'expression'], '((x, y) => x + y)(1, 2)']));
  it('(compile \'(js/iife (js/arrow (x . y) (+ x (first y))) (list* a b)) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), [Symbol.for('first'), Symbol.for('y')]]], [Symbol.for('list*'), Symbol.for('a'), Symbol.for('b')]]], Symbol.for(':as'), 'expression'], '((x, ...y) => x + y[0])(a, ...b)']));
  it('(compile \'(js/iife (js/arrow (x y) (+ x y)) (list 1 2)) :as "statement")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('list'), 1, 2]]], Symbol.for(':as'), 'statement'], `let x = 1;

let y = 2;

x + y;`]));
  it('(compile \'(js/iife (js/arrow (x . y) (+ x (first y))) (list a b c)) :as "statement")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), [Symbol.for('first'), Symbol.for('y')]]], [Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')]]], Symbol.for(':as'), 'statement'], `let y = [b, c];

a + y[0];`]));
  return it('(compile \'(js/iife (js/arrow (x y) (+ x y)) (list 1 2)) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('list'), 1, 2]]], Symbol.for(':as'), 'return'], `let x = 1;

let y = 2;

return x + y;`]));
});

describe('js/()', (): any => {
  it('(compile \'(js/() x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/()'), Symbol.for('x')]]], 'x();']));
  it('(compile \'(js/() x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/()'), Symbol.for('x'), Symbol.for('y')]]], 'x(y);']));
  return it('(compile \'(js/() x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/()'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x(y, z);']));
});

describe('js/=', (): any => {
  it('(compile \'(js/= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), Symbol.for('x'), Symbol.for('y')]]], 'x = y;']));
  it('(compile \'(js/= (aget x i) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('aget'), Symbol.for('x'), Symbol.for('i')], Symbol.for('y')]]], 'x[i] = y;']));
  it('(compile \'(js/= (list-ref x i) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list-ref'), Symbol.for('x'), Symbol.for('i')], Symbol.for('y')]]], 'x[i] = y;']));
  it('(compile \'(js/= \'(x y) z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('quote'), [Symbol.for('x'), Symbol.for('y')]], Symbol.for('z')]]], '[x, y] = z;']));
  it('(compile \'(js/= \'((x) y) z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('quote'), [[Symbol.for('x')], Symbol.for('y')]], Symbol.for('z')]]], '[[x], y] = z;']));
  it('(compile \'(js/= \'(x . y) z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('quote'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')]], Symbol.for('z')]]], '[x, ...y] = z;']));
  it('(compile \'(module m scheme (js/= \'(length) x)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('js/='), [Symbol.for('quote'), [Symbol.for('length')]], Symbol.for('x')]]]], '[length] = x;']));
  it('(compile \'(js/= (list x y) z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '[x, y] = z;']));
  it('(compile \'(js/= (list #f y) z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list'), false, Symbol.for('y')], Symbol.for('z')]]], '[, y] = z;']));
  it('(compile \'(js/= (list* x) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list*'), Symbol.for('x')], Symbol.for('y')]]], 'x = y;']));
  it('(compile \'(js/= (list* x y) z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list*'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '[x, ...y] = z;']));
  it('(compile \'(js/= (list* #f x y) z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list*'), false, Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '[, x, ...y] = z;']));
  it('(compile \'(js/= (values x y) z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('values'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '[x, y] = z;']));
  it('(compile \'(js/= (js/obj x x) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('js/obj'), Symbol.for('x'), Symbol.for('x')], Symbol.for('y')]]], '({x} = y);']));
  it('(compile \'(js/= (js/obj x y) z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('js/obj'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '({x: y} = z);']));
  it('(compile \'(js/= (set! x) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('set!'), Symbol.for('x')], Symbol.for('y')]]], 'x = y;']));
  it('(compile \'(js/= (aset! x i) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('aset!'), Symbol.for('x'), Symbol.for('i')], Symbol.for('y')]]], 'x[i] = y;']));
  it('(compile \'(js/= (list-set! x i) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list-set!'), Symbol.for('x'), Symbol.for('i')], Symbol.for('y')]]], 'x[i] = y;']));
  it('(compile \'(js/= (oset! x "y") z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('oset!'), Symbol.for('x'), 'y'], Symbol.for('z')]]], 'x[\'y\'] = z;']));
  it('(compile \'(js/= (set!-values (x)) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('set!-values'), [Symbol.for('x')]], Symbol.for('y')]]], '[x] = y;']));
  it('(compile \'(js/= (set!-fields (x)) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('set!-fields'), [Symbol.for('x')]], Symbol.for('y')]]], '({x} = y);']));
  it('(compile \'(js/= (define x) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define'), Symbol.for('x')], Symbol.for('y')]]], 'let x = y;']));
  it('(compile \'(js/= (define-values (x)) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define-values'), [Symbol.for('x')]], Symbol.for('y')]]], 'let [x] = y;']));
  it('(compile \'(js/= (define-values (_ x)) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('x')]], Symbol.for('y')]]], 'let [, x] = y;']));
  it('(compile \'(js/= (define-values (_ __ x) :hole-marker __) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('__'), Symbol.for('x')], Symbol.for(':hole-marker'), Symbol.for('__')], Symbol.for('y')]]], 'let [_, , x] = y;']));
  return it('(compile \'(js/= (define-fields (x)) y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define-fields'), [Symbol.for('x')]], Symbol.for('y')]]], 'let {x} = y;']));
});

describe('js/,', (): any => {
  it('(compile \'(js/, x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/,'), Symbol.for('x')]]], 'x;']));
  it('(compile \'(js/, x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/,'), Symbol.for('x'), Symbol.for('y')]]], 'x, y;']));
  return it('(compile \'(js/, x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/,'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x, y, z;']));
});

describe('js/;', (): any => {
  it('(compile \'(js/; x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/;'), Symbol.for('x')]]], 'x;']));
  it('(compile \'(js/; x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/;'), Symbol.for('x'), Symbol.for('y')]]], `x;

y;`]));
  return it('(compile \'(js/; x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/;'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], `x;

y;

z;`]));
});

describe('js/block', (): any => {
  it('(compile \'(js/block x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/block'), Symbol.for('x')]]], `{
  x;
}`]));
  it('(compile \'(js/block x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/block'), Symbol.for('x'), Symbol.for('y')]]], `{
  x;
  y;
}`]));
  return it('(compile \'(js/block x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/block'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], `{
  x;
  y;
  z;
}`]));
});

describe('js/{}', (): any => {
  it('(compile \'(js/{} x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/{}'), Symbol.for('x')]]], `{
  x;
}`]));
  it('(compile \'(js/{} x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/{}'), Symbol.for('x'), Symbol.for('y')]]], `{
  x;
  y;
}`]));
  return it('(compile \'(js/{} x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/{}'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], `{
  x;
  y;
  z;
}`]));
});

describe('js/?', (): any => {
  it('(compile \'(js/? x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y')]]], 'x ? y : undefined;']));
  it('(compile \'(js/? x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x ? y : z;']));
  it('(compile \'(js/? x y (js/? z w)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), [Symbol.for('js/?'), Symbol.for('z'), Symbol.for('w')]]]], 'x ? y : (z ? w : undefined);']));
  it('(compile \'(js/? x y z) :as "statement")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'statement'], 'x ? y : z;']));
  it('(compile \'(js/? x y z) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'return'], 'return x ? y : z;']));
  return it('(compile \'(js/? x y z) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'expression'], 'x ? y : z']));
});

describe('js/if', (): any => {
  it('(compile \'(js/if x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y')]]], `if (x) {
  y;
}`]));
  it('(compile \'(js/if x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], `if (x) {
  y;
} else {
  z;
}`]));
  it('(compile \'(js/if x y (js/if z w)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), [Symbol.for('js/if'), Symbol.for('z'), Symbol.for('w')]]]], `if (x) {
  y;
} else if (z) {
  w;
}`]));
  it('(compile \'(js/if x y z) :as "statement")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'statement'], `if (x) {
  y;
} else {
  z;
}`]));
  it('(compile \'(js/if x y z) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'return'], `if (x) {
  return y;
} else {
  return z;
}`]));
  return it('(compile \'(js/if x y z) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('it>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'expression'], `(() => {
  if (x) {
    return y;
  } else {
    return z;
  }
})()`]));
});

describe('js/switch', (): any => {
  it('(let* ((x "foo") (y "bar")) (js/switch x (case "foo" (set! y "baz") (break)) (default (set! y "quux"))) y)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let*'), [[Symbol.for('x'), 'foo'], [Symbol.for('y'), 'bar']], [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('set!'), Symbol.for('y'), 'baz'], [Symbol.for('break')]], [Symbol.for('default'), [Symbol.for('set!'), Symbol.for('y'), 'quux']]], Symbol.for('y')], 'baz']));
  it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('display'), 'foo'], [Symbol.for('break')]], [Symbol.for('default'), [Symbol.for('display'), 'bar']]]]], `switch (x) {
  case 'foo': {
    console.log('foo');
    break;
  }
  default: {
    console.log('bar');
  }
}`]));
  it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('display'), 'foo'], [Symbol.for('break')]], [Symbol.for('default'), [Symbol.for('display'), 'bar']]]], Symbol.for(':as'), 'return'], `switch (x) {
  case 'foo': {
    return console.log('foo');
    break;
  }
  default: {
    return console.log('bar');
  }
}`]));
  it('(compile \'(js/switch x (case "foo" (display "foo")) (default (display "bar"))) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('display'), 'foo']], [Symbol.for('default'), [Symbol.for('display'), 'bar']]]], Symbol.for(':as'), 'return'], `switch (x) {
  case 'foo': {
    console.log('foo');
  }
  default: {
    return console.log('bar');
  }
}`]));
  return it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('display'), 'foo'], [Symbol.for('break')]], [Symbol.for('default'), [Symbol.for('display'), 'bar']]]], Symbol.for(':as'), 'expression'], `(() => {
  switch (x) {
    case 'foo': {
      return console.log('foo');
      break;
    }
    default: {
      return console.log('bar');
    }
  }
})()`]));
});

describe('js/!', (): any => {
  it('(js/! #f)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/!'), false], true]));
  it('(js/! #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/!'), true], false]));
  return it('(compile \'(js/! x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/!'), Symbol.for('x')]]], '!x;']));
});

describe('js/&&', (): any => {
  it('(js/&&)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/&&')], true]));
  it('(js/&& #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/&&'), true], true]));
  it('(js/&& #t #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/&&'), true, true], true]));
  it('(js/&& #t #f)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/&&'), true, false], false]));
  it('(funcall js/&&)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/&&')], true]));
  it('(funcall js/&& #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/&&'), true], true]));
  it('(funcall js/&& #t #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/&&'), true, true], true]));
  it('(funcall js/&& #t #f)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/&&'), true, false], false]));
  it('(compile \'(js/&& x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/&&'), Symbol.for('x'), Symbol.for('y')]]], 'x && y;']));
  return it('(compile \'(js/&& x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/&&'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x && y && z;']));
});

describe('js/||', (): any => {
  it('(js/||)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/||')], false]));
  it('(js/|| #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/||'), true], true]));
  it('(js/|| #t #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/||'), true, true], true]));
  it('(js/|| #t #f)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/||'), true, false], true]));
  it('(funcall js/||)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/||')], false]));
  it('(funcall js/|| #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/||'), true], true]));
  it('(funcall js/|| #t #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/||'), true, true], true]));
  it('(funcall js/|| #t #f)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/||'), true, false], true]));
  it('(compile \'(js/|| x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/||'), Symbol.for('x'), Symbol.for('y')]]], 'x || y;']));
  return it('(compile \'(js/|| x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/||'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x || y || z;']));
});

describe('js/op', (): any => {
  it('(js/op ! #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/op'), Symbol.for('!'), true], false]));
  it('(js/op && #t #f)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/op'), Symbol.for('&&'), true, false], false]));
  it('(js/op || #t #f)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/op'), Symbol.for('||'), true, false], true]));
  it('(compile \'(js/op ! x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('!'), Symbol.for('x')]]], '!x;']));
  it('(compile \'(js/op ~ x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('~'), Symbol.for('x')]]], '~x;']));
  it('(compile \'(js/op & x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('&'), Symbol.for('x'), Symbol.for('y')]]], 'x & y;']));
  it('(compile \'(js/op && x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('&&'), Symbol.for('x'), Symbol.for('y')]]], 'x && y;']));
  return it('(compile \'(js/op || x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('||'), Symbol.for('x'), Symbol.for('y')]]], 'x || y;']));
});

describe('js/while', (): any => {
  it('(compile \'(js/while (< (length result) 3) (display result)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/while'), [Symbol.for('<'), [Symbol.for('length'), Symbol.for('result')], 3], [Symbol.for('display'), Symbol.for('result')]]]], `while (result.length < 3) {
  console.log(result);
}`]));
  return it('(compile \'(js/while (begin (set! x (- x 1)) (> x 0)) (display x)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/while'), [Symbol.for('begin'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('-'), Symbol.for('x'), 1]], [Symbol.for('>'), Symbol.for('x'), 0]], [Symbol.for('display'), Symbol.for('x')]]]], `while (x--, x > 0) {
  console.log(x);
}`]));
});

describe('js/do-while', (): any => {
  it('(compile \'(js/do-while ((display result)) (< (length result) 3)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/do-while'), [[Symbol.for('display'), Symbol.for('result')]], [Symbol.for('<'), [Symbol.for('length'), Symbol.for('result')], 3]]]], `do {
  console.log(result);
} while (result.length < 3);`]));
  return it('(compile \'(js/do-while ((foo) (display result)) (< (length result) 3)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/do-while'), [[Symbol.for('foo')], [Symbol.for('display'), Symbol.for('result')]], [Symbol.for('<'), [Symbol.for('length'), Symbol.for('result')], 3]]]], `do {
  foo();
  console.log(result);
} while (result.length < 3);`]));
});

describe('js/for', (): any => {
  it('(let ((result 0)) (js/for ((i 0) (< i 10) (+ i 1)) (set! result (+ result 2))) result)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), 0]], [Symbol.for('js/for'), [[Symbol.for('i'), 0], [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('+'), Symbol.for('result'), 2]]], Symbol.for('result')], 20]));
  it('(compile \'(js/for ((i 0) (< i 10) (+ i 1)) (foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('i'), 0], [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('foo')]]]], `for (let i = 0; i < 10; i++) {
  foo();
}`]));
  it('(compile \'(js/for ((set! i 0) (< i 10) (+ i 1)) (foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('set!'), Symbol.for('i'), 0], [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('foo')]]]], `for (i = 0; i < 10; i++) {
  foo();
}`]));
  it('(compile \'(js/for ((define i 0) (< i 10) (+ i 1)) (foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('foo')]]]], `for (let i = 0; i < 10; i++) {
  foo();
}`]));
  it('(compile \'(js/for ((begin (set! i 0) (set! j 0)) (and (< i 10) (< j 10)) (begin (set! i (+ i 1)) (set! j (+ j 1)))) (foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('begin'), [Symbol.for('set!'), Symbol.for('i'), 0], [Symbol.for('set!'), Symbol.for('j'), 0]], [Symbol.for('and'), [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('<'), Symbol.for('j'), 10]], [Symbol.for('begin'), [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('set!'), Symbol.for('j'), [Symbol.for('+'), Symbol.for('j'), 1]]]], [Symbol.for('foo')]]]], `for (i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {
  foo();
}`]));
  it('(compile \'(js/for ((js/define i 0 j 0) (and (< i 10) (< j 10)) (begin (set! i (+ i 1)) (set! j (+ j 1)))) (foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('js/define'), Symbol.for('i'), 0, Symbol.for('j'), 0], [Symbol.for('and'), [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('<'), Symbol.for('j'), 10]], [Symbol.for('begin'), [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('set!'), Symbol.for('j'), [Symbol.for('+'), Symbol.for('j'), 1]]]], [Symbol.for('foo')]]]], `for (let i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {
  foo();
}`]));
  it('(let (result) (js/for (() () ()) (set! result 1) (break)) result)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('result')], [Symbol.for('js/for'), [[], [], []], [Symbol.for('set!'), Symbol.for('result'), 1], [Symbol.for('break')]], Symbol.for('result')], 1]));
  it('(let (result) (js/for (#u #u #u) (set! result 1) (break)) result)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('result')], [Symbol.for('js/for'), [undefined, undefined, undefined], [Symbol.for('set!'), Symbol.for('result'), 1], [Symbol.for('break')]], Symbol.for('result')], 1]));
  it('(compile \'(js/for (() () ()) (break)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[], [], []], [Symbol.for('break')]]]], `for (;;) {
  break;
}`]));
  it('(compile \'(js/for (#f #f #f) (break)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [false, false, false], [Symbol.for('break')]]]], `for (;;) {
  break;
}`]));
  return it('(compile \'(js/for (#u #u #u) (break)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [undefined, undefined, undefined], [Symbol.for('break')]]]], `for (;;) {
  break;
}`]));
});

describe('js/for-in', (): any => it('(compile \'(js/for-in ((i obj)) (foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for-in'), [[Symbol.for('i'), Symbol.for('obj')]], [Symbol.for('foo')]]]], `for (let i in obj) {
  foo();
}`])));

describe('js/for-of', (): any => it('(compile \'(js/for-of ((i lst)) (foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for-of'), [[Symbol.for('i'), Symbol.for('lst')]], [Symbol.for('foo')]]]], `for (let i of lst) {
  foo();
}`])));

describe('js/.', (): any => {
  it('(let ((obj (js/obj "foo" "bar"))) (js/. obj foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('foo')]], 'bar']));
  it('(let ((obj (js/obj "foo" "bar"))) (js/. obj "foo"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/.'), Symbol.for('obj'), 'foo']], 'bar']));
  it('(let ((obj (js/obj "foo" (js/obj "bar" "baz")))) (js/. obj foo bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'bar', 'baz']]]], [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('foo'), Symbol.for('bar')]], 'baz']));
  it('(let ((obj (js/obj "foo" (js/obj "bar" "baz")))) (js/. (js/. obj foo) bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'bar', 'baz']]]], [Symbol.for('js/.'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('foo')], Symbol.for('bar')]], 'baz']));
  it('(compile \'(js/. obj prop))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop')]]], 'obj.prop;']));
  it('(compile \'(js/. obj "prop"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), 'prop']]], 'obj[\'prop\'];']));
  it('(compile \'(js/. obj :foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for(':foo')]]], 'obj.foo;']));
  it('(compile \'(js/. obj :foo-bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for(':foo-bar')]]], 'obj.fooBar;']));
  it('(compile \'(js/. obj \'foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo')]]]], 'obj.foo;']));
  it('(compile \'(js/. obj \'foo-bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')]]]], 'obj.fooBar;']));
  it('(compile \'(js/. obj prop1 prop2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop1'), Symbol.for('prop2')]]], 'obj.prop1.prop2;']));
  return it('(compile \'(js/. (js/. obj prop1) prop2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop1')], Symbol.for('prop2')]]], 'obj.prop1.prop2;']));
});

describe('js/?.', (): any => {
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('foo')]], 'bar']));
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj "foo"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), Symbol.for('obj'), 'foo']], 'bar']));
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj quux))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux')]], undefined]));
  it('(let ((obj (js/obj "foo" "bar"))) ((js/?. obj quux)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [[Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux')]]], undefined]));
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj quux wobble))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux'), Symbol.for('wobble')]], undefined]));
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. (js/?. obj quux) wobble))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux')], Symbol.for('wobble')]], undefined]));
  it('(compile \'(js/?. obj prop))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('prop')]]], 'obj?.prop;']));
  it('(compile \'(js/?. obj prop1 prop2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('prop1'), Symbol.for('prop2')]]], 'obj?.prop1?.prop2;']));
  it('(compile \'(js/?. (js/?. obj prop1) prop2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?.'), [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('prop1')], Symbol.for('prop2')]]], 'obj?.prop1?.prop2;']));
  it('(compile \'(js/?. (js/?. (js/?. obj) prop1) prop2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?.'), [Symbol.for('js/?.'), [Symbol.for('js/?.'), Symbol.for('obj')], Symbol.for('prop1')], Symbol.for('prop2')]]], 'obj?.prop1?.prop2;']));
  it('(compile \'(define x (js/?. foo bar)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('js/?.'), Symbol.for('foo'), Symbol.for('bar')]]]], 'let x = foo?.bar;']));
  it('(compile \'(define x ((js/?. foo bar) baz)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), [[Symbol.for('js/?.'), Symbol.for('foo'), Symbol.for('bar')], Symbol.for('baz')]]]], 'let x = foo?.bar(baz);']));
  return it('(compile \'(define x (js/?. foo (bar))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('js/?.'), Symbol.for('foo'), [Symbol.for('bar')]]]]], 'let x = foo?.(bar);']));
});

describe('js/obj', (): any => {
  it('(js/obj)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/obj')], [Symbol.for('js/obj')]]));
  it('(js/obj "foo" "bar")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/obj'), 'foo', 'bar'], [Symbol.for('js/obj'), 'foo', 'bar']]));
  it('(js/obj "foo" 1 "bar" 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2], [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]]));
  it('(compile \'(js/obj))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj')]]], '({});']));
  it('(compile \'(js/obj "foo" foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', Symbol.for('foo')]]], `({
  foo
});`]));
  it('(compile \'(js/obj "foo" "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 'bar']]], `({
  foo: 'bar'
});`]));
  it('(compile \'(js/obj foo foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for('foo'), Symbol.for('foo')]]], `({
  [foo]: foo
});`]));
  it('(compile \'(js/obj foo "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for('foo'), 'bar']]], `({
  [foo]: 'bar'
});`]));
  it('(compile \'(js/obj \'foo "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), [Symbol.for('quote'), Symbol.for('foo')], 'bar']]], `({
  foo: 'bar'
});`]));
  it('(compile \'(js/obj :foo "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for(':foo'), 'bar']]], `({
  foo: 'bar'
});`]));
  it('(compile \'(js/obj "foo-bar" "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo-bar', 'baz']]], `({
  'foo-bar': 'baz'
});`]));
  it('(compile \'(js/obj foo-bar "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for('foo-bar'), 'baz']]], `({
  [fooBar]: 'baz'
});`]));
  it('(compile \'(js/obj \'foo-bar "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), [Symbol.for('quote'), Symbol.for('foo-bar')], 'baz']]], `({
  fooBar: 'baz'
});`]));
  it('(compile \'(js/obj :foo-bar "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for(':foo-bar'), 'baz']]], `({
  fooBar: 'baz'
});`]));
  it('(compile \'(js/obj "foo bar" "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo bar', 'baz']]], `({
  'foo bar': 'baz'
});`]));
  it('(compile \'(js/obj "foo bar" baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo bar', Symbol.for('baz')]]], `({
  'foo bar': baz
});`]));
  it('(compile \'(js/obj "foo bar" \'baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo bar', [Symbol.for('quote'), Symbol.for('baz')]]]], `({
  'foo bar': Symbol.for('baz')
});`]));
  it('(compile \'(js/obj "foo bar" :baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo bar', Symbol.for(':baz')]]], `({
  'foo bar': Symbol.for(':baz')
});`]));
  it('(compile \'(js/obj "foo" 1 "bar" 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]]], `({
  foo: 1,
  bar: 2
});`]));
  it('(compile \'(js/obj "foo" foo "bar" bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', Symbol.for('foo'), 'bar', Symbol.for('bar')]]], `({
  foo,
  bar
});`]));
  it('(compile \'(js/obj "foo" (js/obj "bar" "baz")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'bar', 'baz']]]], `({
  foo: {
    bar: 'baz'
  }
});`]));
  it('(compile \'(js/obj "foo" (js/obj "foo" "foo") "bar" (js/obj "bar" "bar")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'foo', 'foo'], 'bar', [Symbol.for('js/obj'), 'bar', 'bar']]]], `({
  foo: {
    foo: 'foo'
  },
  bar: {
    bar: 'bar'
  }
});`]));
  it('(compile \'(js/obj "foo" (js/obj) "bar" (js/obj "bar" "bar") "baz" (js/obj "baz" "baz")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj')], 'bar', [Symbol.for('js/obj'), 'bar', 'bar'], 'baz', [Symbol.for('js/obj'), 'baz', 'baz']]]], `({
  foo: {},
  bar: {
    bar: 'bar'
  },
  baz: {
    baz: 'baz'
  }
});`]));
  it('(compile \'(js/obj) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj')]], Symbol.for(':as'), 'expression'], '{}']));
  it('(compile \'(js/obj "foo" "bar") :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 'bar']], Symbol.for(':as'), 'expression'], `{
  foo: 'bar'
}`]));
  it('(compile \'(js/obj "foo" 1 "bar" 2) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]], Symbol.for(':as'), 'expression'], `{
  foo: 1,
  bar: 2
}`]));
  it('(compile \'(js/obj) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj')]], Symbol.for(':as'), 'return'], 'return {};']));
  it('(compile \'(js/obj "foo" "bar") :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 'bar']], Symbol.for(':as'), 'return'], `return {
  foo: 'bar'
};`]));
  return it('(compile \'(js/obj "foo" 1 "bar" 2) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]], Symbol.for(':as'), 'return'], `return {
  foo: 1,
  bar: 2
};`]));
});

describe('js/obj?', (): any => it('(compile \'(js/obj? x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj?'), Symbol.for('x')]]], '(x !== null) && (typeof x === \'object\');'])));

describe('js/obj-append', (): any => it('(compile \'(js/obj-append obj (js/obj "foo" "bar")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj-append'), Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]]], `({
  ...obj,
  foo: 'bar'
});`])));

describe('js/keys', (): any => {
  it('(js/keys (js/obj))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/keys'), [Symbol.for('js/obj')]], [Symbol.for('quote'), []]]));
  it('(js/keys (js/obj "foo" "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/keys'), [Symbol.for('js/obj'), 'foo', 'bar']], [Symbol.for('quote'), ['foo']]]));
  it('(js/keys (js/obj "foo" "bar" "baz" "quux"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/keys'), [Symbol.for('js/obj'), 'foo', 'bar', 'baz', 'quux']], [Symbol.for('quote'), ['foo', 'baz']]]));
  return it('(compile \'(js/keys x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/keys'), Symbol.for('x')]]], 'Object.keys(x);']));
});

describe('js/in', (): any => {
  it('(let ((obj (js/obj "foo" "bar"))) (js/in "foo" obj))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/in'), 'foo', Symbol.for('obj')]], true]));
  return it('(compile \'(js/in "foo" obj))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/in'), 'foo', Symbol.for('obj')]]], '\'foo\' in obj;']));
});

describe('js/delete', (): any => it('(compile \'(js/delete x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/delete'), Symbol.for('x')]]], 'delete x;'])));

describe('js/try', (): any => {
  it('(js/try (/ 1 2) (catch e (display "there was an error")) (finally (display "finally")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/try'), [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]], 0.5]));
  it('(js/try (/ 1 3) (/ 1 2) (catch e (display "there was an error")) (finally (display "finally")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/try'), [Symbol.for('/'), 1, 3], [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]], 0.5]));
  it('(compile \'(js/try))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try')]]], `try {
}`]));
  it('(compile \'(js/try (set! x (/ 2 1))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]]]]], `try {
  x = 2 / 1;
}`]));
  it('(compile \'(js/try (set! x (/ 2 1)) (finally (display "cleanup"))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], `try {
  x = 2 / 1;
} finally {
  console.log('cleanup');
}`]));
  it('(compile \'(js/try (/ 1 2) (catch e (display "there was an error")) (finally (display "finally"))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]]]], `try {
  1 / 2;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('finally');
}`]));
  it('(compile \'(js/try (set! x (/ 2 1)) (catch _ (display "there was an error")) (finally (display "cleanup"))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('_'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], `try {
  x = 2 / 1;
} catch {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}`]));
  it('(compile \'(js/try (set! x (/ 2 1)) (catch e (display "there was an error")) (finally (display "cleanup"))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], `try {
  x = 2 / 1;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}`]));
  return it('(compile \'(js/try (set! x (/ 2 1)) (catch e (display "there was an error")) (finally (display "cleanup"))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], `try {
  x = 2 / 1;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}`]));
});

describe('js/+', (): any => {
  it('(js/+)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+')], 0]));
  it('(js/+ 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1], 1]));
  it('(js/+ 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1, 2], 3]));
  it('(js/+ 2 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 2, 2], 4]));
  it('(js/+ 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1, 2, 3], 6]));
  it('(js/+ 1 2 4)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1, 2, 4], 7]));
  it('(js/+ (js/+ 1 1) (js/+ 1 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), [Symbol.for('js/+'), 1, 1], [Symbol.for('js/+'), 1, 1]], 4]));
  it('(let ((x 2)) (js/+ x x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 2]], [Symbol.for('js/+'), Symbol.for('x'), Symbol.for('x')]], 4]));
  it('(js/+ 1 "")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1, ''], '1']));
  it('(compile \'(js/+ 1 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/+'), 1, 1]]], '1 + 1;']));
  return it('(compile \'(js/+ 1 1 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/+'), 1, 1, 1]]], '1 + 1 + 1;']));
});

describe('js/-', (): any => {
  it('(js/-)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-')], 0]));
  it('(js/- 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-'), 1], -1]));
  it('(js/- 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-'), 1, 2], -1]));
  it('(js/- 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-'), 1, 2, 3], -4]));
  it('(js/- 1 2 4)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-'), 1, 2, 4], -5]));
  it('(compile \'(js/- 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/-'), 1]]], '-1;']));
  it('(compile \'(js/- 1 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/-'), 1, 1]]], '1 - 1;']));
  return it('(compile \'(js/- 1 1 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/-'), 1, 1, 1]]], '1 - 1 - 1;']));
});

describe('js/*', (): any => {
  it('(js/*)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*')], 1]));
  it('(js/* 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*'), 1], 1]));
  it('(js/* 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*'), 1, 2], 2]));
  it('(js/* 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*'), 1, 2, 3], 6]));
  it('(js/* 1 2 4)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*'), 1, 2, 4], 8]));
  it('(compile \'(js/* 1 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/*'), 1, 1]]], '1 * 1;']));
  return it('(compile \'(js/* 1 1 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/*'), 1, 1, 1]]], '1 * 1 * 1;']));
});

describe('js//', (): any => {
  it('(js//)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js//')], undefined]));
  it('(js// 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js//'), 1], 1]));
  it('(js// 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js//'), 1, 2], 0.5]));
  it('(js// 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js//'), 1, 2, 3], [Symbol.for('js//'), 1, 2, 3]]));
  it('(/ 1 2 4)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('/'), 1, 2, 4], 0.125]));
  it('(compile \'(js// 1 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js//'), 1, 2]]], '1 / 2;']));
  return it('(compile \'(js// 1 2 4))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js//'), 1, 2, 4]]], '1 / 2 / 4;']));
});

describe('js/<', (): any => {
  it('(js/< 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 1, 2], true]));
  it('(js/< 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 2, 1], false]));
  it('(js/< 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 1, 2, 3], true]));
  it('(js/< 2 1 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 2, 1, 3], false]));
  it('(js/< 1 3 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 1, 3, 2], false]));
  it('(funcall js/< 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 1, 2], true]));
  it('(funcall js/< 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 2, 1], false]));
  it('(funcall js/< 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 1, 2, 3], true]));
  it('(funcall js/< 2 1 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 2, 1, 3], false]));
  it('(funcall js/< 1 3 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 1, 3, 2], false]));
  it('(compile \'(js/< x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<'), Symbol.for('x'), Symbol.for('y')]]], 'x < y;']));
  return it('(compile \'(js/< x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x < y) && (y < z);']));
});

describe('js/<=', (): any => {
  it('(js/<= 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 1, 2], true]));
  it('(js/<= 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 2, 1], false]));
  it('(js/<= 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 1, 2, 3], true]));
  it('(js/<= 1 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 1, 1, 2], true]));
  it('(js/<= 2 1 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 2, 1, 3], false]));
  it('(js/<= 1 3 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 1, 3, 2], false]));
  it('(funcall js/<= 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 1, 2], true]));
  it('(funcall js/<= 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 2, 1], false]));
  it('(funcall js/<= 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 1, 2, 3], true]));
  it('(funcall js/<= 1 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 1, 1, 2], true]));
  it('(funcall js/<= 2 1 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 2, 1, 3], false]));
  it('(funcall js/<= 1 3 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 1, 3, 2], false]));
  it('(compile \'(js/<= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<='), Symbol.for('x'), Symbol.for('y')]]], 'x <= y;']));
  return it('(compile \'(js/<= x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<='), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x <= y) && (y <= z);']));
});

describe('js/>', (): any => {
  it('(js/> 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 2, 1], true]));
  it('(js/> 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 1, 2], false]));
  it('(js/> 3 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 3, 2, 1], true]));
  it('(js/> 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 1, 2, 3], false]));
  it('(js/> 2 1 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 2, 1, 3], false]));
  it('(js/> 1 3 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 1, 3, 2], false]));
  it('(funcall js/> 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 2, 1], true]));
  it('(funcall js/> 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 1, 2], false]));
  it('(funcall js/> 3 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 3, 2, 1], true]));
  it('(funcall js/> 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 1, 2, 3], false]));
  it('(funcall js/> 2 1 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 2, 1, 3], false]));
  it('(funcall js/> 1 3 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 1, 3, 2], false]));
  it('(compile \'(js/> x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>'), Symbol.for('x'), Symbol.for('y')]]], 'x > y;']));
  return it('(compile \'(js/> x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x > y) && (y > z);']));
});

describe('js/>=', (): any => {
  it('(js/>= 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 2, 1], true]));
  it('(js/>= 2 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 2, 2], true]));
  it('(js/>= 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 1, 2], false]));
  it('(js/>= 3 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 3, 2, 1], true]));
  it('(js/>= 3 2 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 3, 2, 2], true]));
  it('(js/>= 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 1, 2, 3], false]));
  it('(js/>= 2 1 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 2, 1, 3], false]));
  it('(js/>= 1 3 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 1, 3, 2], false]));
  it('(funcall js/>= 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 2, 1], true]));
  it('(funcall js/>= 2 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 2, 2], true]));
  it('(funcall js/>= 1 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 1, 2], false]));
  it('(funcall js/>= 3 2 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 3, 2, 1], true]));
  it('(funcall js/>= 3 2 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 3, 2, 2], true]));
  it('(funcall js/>= 1 2 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 1, 2, 3], false]));
  it('(funcall js/>= 2 1 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 2, 1, 3], false]));
  it('(funcall js/>= 1 3 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 1, 3, 2], false]));
  it('(compile \'(js/>= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>='), Symbol.for('x'), Symbol.for('y')]]], 'x >= y;']));
  return it('(compile \'(js/>= x y z))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>='), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x >= y) && (y >= z);']));
});

describe('js/%', (): any => it('(compile \'(js/% x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/%'), Symbol.for('x'), Symbol.for('y')]]], 'x % y;'])));

describe('js/abs', (): any => it('(compile \'(js/abs x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/abs'), Symbol.for('x')]]], 'Math.abs(x);'])));

describe('js/tag', (): any => it('(compile \'(js/tag foo "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/tag'), Symbol.for('foo'), 'bar']]], 'foo`bar`;'])));

describe('js/rename', (): any => it('(compile \'(js/rename ((x y)) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/rename'), [[Symbol.for('x'), Symbol.for('y')]], Symbol.for('x')]]], 'y;'])));

describe('js/statement-or-expression', (): any => {
  it('(compile \'(js/statement-or-expression :statement 1 :expression 2) :as "statement")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1, Symbol.for(':expression'), 2]], Symbol.for(':as'), 'statement'], '1;']));
  it('(compile \'(js/statement-or-expression :statement 1 :expression 2) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1, Symbol.for(':expression'), 2]], Symbol.for(':as'), 'expression'], '2']));
  it('(compile \'(js/statement-or-expression :statement 1 :expression 2 :return 3) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1, Symbol.for(':expression'), 2, Symbol.for(':return'), 3]], Symbol.for(':as'), 'return'], 'return 3;']));
  it('(compile \'(js/statement-or-expression :statement 1 :expression 2) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1, Symbol.for(':expression'), 2]], Symbol.for(':as'), 'return'], 'return 2;']));
  return it('(compile \'(js/statement-or-expression :statement 1) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1]], Symbol.for(':as'), 'return'], 'return 1;']));
});

describe('js/raw', (): any => {
  it('(compile \'(js/raw "1"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/raw'), '1']]], '1']));
  return it('(compile \'(js/raw "function I(x) { return x; }"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/raw'), 'function I(x) { return x; }']]], 'function I(x) { return x; }']));
});