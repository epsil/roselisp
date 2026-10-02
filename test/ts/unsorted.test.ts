/**
 * # Unsorted tests
 *
 * This file functions as an "inbox" for incoming tests,
 * as well as an "outbox" for legacy tests pending deletion.
 */

import {
  assertEqual,
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('To do', (): any => {
});

describe('Avoiding IIFEs', (): any => xit('(compile \'(define x (begin y z)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('begin'), Symbol.for('y'), Symbol.for('z')]]]], `let y;

let x = z;`])));

describe('declare', (): any => xit('(compile \'(begin (define (my-plus x y) (+ x y 0)) (declare my-plus (compiler-macro (macro (x y) `(+ ,x ,y)))) (define x (my-plus 1 2))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), 0]], [Symbol.for('declare'), Symbol.for('my-plus'), [Symbol.for('compiler-macro'), [Symbol.for('macro'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('my-plus'), 1, 2]]]]], `function myPlus(x, y) {
  return x + y + 0;
}

myPlus.compilerMacro = (() => {
  let f = function (exp, env) {
    let [x, y] = exp.slice(1);
    return [Symbol.for('+'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();

let x = 1 + 2;`])));

describe('define-subst', (): any => xit('(compile \'(begin (define-subst (my-plus x y) (+ x y)) (define x (my-plus 1 2))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-subst'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('my-plus'), 1, 2]]]]], `function myPlus(x, y) {
  return x + y;
}

myPlus.compilerMacro = (() => {
  let f = function (exp, env) {
    let [x, y] = exp.slice(1);
    return [Symbol.for('+'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();

let x = 1 + 2;`])));

describe('Assignment operators', (): any => {
  xit('(compile \'(js/+= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/+='), Symbol.for('x'), Symbol.for('y')]]], 'x += y;']));
  xit('(compile \'(js/-= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/-='), Symbol.for('x'), Symbol.for('y')]]], 'x -= y;']));
  xit('(compile \'(js/*= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/*='), Symbol.for('x'), Symbol.for('y')]]], 'x *= y;']));
  xit('(compile \'(js//= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js//='), Symbol.for('x'), Symbol.for('y')]]], 'x /= y;']));
  xit('(compile \'(js/^= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/^='), Symbol.for('x'), Symbol.for('y')]]], 'x ^= y;']));
  xit('(compile \'(js/&= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/&='), Symbol.for('x'), Symbol.for('y')]]], 'x &= y;']));
  xit('(compile \'(js/|= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/|='), Symbol.for('x'), Symbol.for('y')]]], 'x |= y;']));
  xit('(compile \'(js/<<= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<<='), Symbol.for('x'), Symbol.for('y')]]], 'x <<= y;']));
  xit('(compile \'(js/>>= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>>='), Symbol.for('x'), Symbol.for('y')]]], 'x >>= y;']));
  return xit('(compile \'(js/>>>= x y))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>>>='), Symbol.for('x'), Symbol.for('y')]]], 'x >>>= y;']));
});

describe('js/iife', (): any => xit('(compile \'(js/iife (js/arrow (x . y) (+ x (first y))) (list* a b)) :as \'statement)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), [Symbol.for('first'), Symbol.for('y')]]], [Symbol.for('list*'), Symbol.for('a'), Symbol.for('b')]]], Symbol.for(':as'), [Symbol.for('quote'), Symbol.for('statement')]], `let x = a;

let y = b;

x + y[0];`])));

describe('parse', (): any => xit('(parse "foo;" :as \'javascript)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('parse'), 'foo;', Symbol.for(':as'), [Symbol.for('quote'), Symbol.for('javascript')]], [Symbol.for('js/obj'), 'type', 'Program', 'body', [Symbol.for('list'), [Symbol.for('js/obj'), 'type', 'ExpressionStatement', 'expression', [Symbol.for('js/obj'), 'type', 'Identifier', 'name', 'foo']]]]])));

describe('interpret', (): any => xit('(interpret \'(length \'(1 . ())) :fdottedlists #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('interpret'), [Symbol.for('quote'), [Symbol.for('length'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]]], Symbol.for(':fdottedlists'), true], 1])));

describe('gensym', (): any => xit('(compile `(module m scheme (define ,(gensym "length") length)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'length']], Symbol.for('length')]]]], `import {
  length
} from 'roselisp';

let length1 = length;`])));

describe('define-syntax', (): any => xit('(compile \'(module m scheme (define x 1) (define-syntax (foo x) (syntax (begin (define x 2) x))) (foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define-syntax'), [Symbol.for('foo'), Symbol.for('x')], [Symbol.for('syntax'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 2], Symbol.for('x')]]], [Symbol.for('foo')]]]], `import {
  datumToSyntax
} from 'roselisp';

let x = 1;

function foo(x) {
  return datumToSyntax(false, [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 2], Symbol.for('x')]);
}

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

let x1 = 2;

x1;`])));

describe('Dotted lists', (): any => {
  xit('\'(1 . 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]], [Symbol.for('quote'), [1, Symbol.for('.'), 2]]]));
  xit('\'(1 . ())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('quote'), [1, Symbol.for('.'), []]], [Symbol.for('quote'), [1]]]));
  xit('\'(1 . (2 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]], [Symbol.for('quote'), [1, 2]]]));
  xit('\'(1 . (2 . 3))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]], [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]]));
  xit('(dotted-list? \'(1 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], false]));
  xit('(dotted-list? \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], false]));
  xit('(dotted-pair? \'(1 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], false]));
  xit('(dotted-pair? \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], false]));
  xit('(compile \'(module m scheme \'(1 . ())) :fdottedlists #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]]], Symbol.for(':fdottedlists'), true], `import {
  normalizeList
} from 'roselisp';

normalizeList([1, Symbol.for('.'), []);`]));
  return xit('(compile \'(module m scheme (define (normalize-list x) \'(1 . x))) :fdottedlists #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('normalize-list'), Symbol.for('x')], [Symbol.for('quote'), [1, Symbol.for('.'), Symbol.for('x')]]]]], Symbol.for(':fdottedlists'), true], `import {
  normalizeList1
} from 'roselisp';

function normalizeList(x) {
  normalizeList1([1, Symbol.for('.'), x);
}`]));
});