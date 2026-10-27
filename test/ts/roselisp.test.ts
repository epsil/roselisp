/**
 * # Roselisp tests
 *
 * Tests of procedures and constructs that are specific to Roselisp.
 */

import {
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('#u', (): any => {
  it('#u', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), undefined, undefined]));
  it('\'#u', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), undefined], undefined]));
  it('undefined', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('undefined'), undefined]));
  it('(compile #u)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), undefined], 'undefined;']));
  return it('(compile \'undefined)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('undefined')]], 'undefined;']));
});

describe('#n', (): any => {
  it('#n', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), null, null]));
  it('\'#n', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), null], null]));
  it('js-null', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('js-null'), null]));
  it('js/null', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('js/null'), null]));
  return it('(compile #n)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), null], 'null;']));
});

describe('NaN', (): any => {
  it('NaN', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('NaN'), Symbol.for('NaN')]));
  it('(nan? NaN)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nan?'), Symbol.for('NaN')], true]));
  it('(nan? 0)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nan?'), 0], false]));
  it('(compile \'NaN)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('NaN')]], 'NaN;']));
  return it('(compile \'(nan? x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('nan?'), Symbol.for('x')]]], 'isNaN(x);']));
});

describe('true?', (): any => {
  it('(true? #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), true], true]));
  it('(true? #f)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), false], false]));
  it('(true? #u)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), undefined], false]));
  it('(true? #n)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), null], false]));
  return it('(true? \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), [Symbol.for('quote'), []]], true]));
});

describe('false?', (): any => {
  it('(false? #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), true], false]));
  it('(false? #f)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), false], true]));
  it('(false? #u)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), undefined], true]));
  it('(false? #n)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), null], true]));
  return it('(false? \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), [Symbol.for('quote'), []]], false]));
});

describe('atom?', (): any => {
  it('(atom? #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('atom?'), true], true]));
  it('(atom? \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('atom?'), [Symbol.for('quote'), []]], true]));
  it('(atom? \'(1 . 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('atom?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], false]));
  return it('(atom? \'(1 2 3))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('atom?'), [Symbol.for('quote'), [1, 2, 3]]], false]));
});

describe('keyword->string', (): any => it('(keyword->string :foo)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('keyword->string'), Symbol.for(':foo')], 'foo'])));

describe('string->keyword', (): any => it('(string->keyword "foo")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string->keyword'), 'foo'], [Symbol.for('quote'), Symbol.for(':foo')]])));

describe('keyword->symbol', (): any => it('(keyword->symbol :foo)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('keyword->symbol'), Symbol.for(':foo')], [Symbol.for('quote'), Symbol.for('foo')]])));

describe('symbol->keyword', (): any => it('(symbol->keyword \'foo)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('symbol->keyword'), [Symbol.for('quote'), Symbol.for('foo')]], [Symbol.for('quote'), Symbol.for(':foo')]])));

describe('pair-or-list?', (): any => {
  it('(pair-or-list? #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair-or-list?'), true], false]));
  it('(pair-or-list? \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair-or-list?'), [Symbol.for('quote'), []]], true]));
  it('(pair-or-list? \'(1 . 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair-or-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]));
  it('(pair-or-list? \'(1 2 3))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair-or-list?'), [Symbol.for('quote'), [1, 2, 3]]], true]));
  return it('(compile \'(pair-or-list? x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('pair-or-list?'), Symbol.for('x')]]], 'Array.isArray(x);']));
});

describe('vector?', (): any => {
  it('(vector? \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), []]], true]));
  it('(vector? \'(1 . 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]));
  it('(vector? \'(1 2 . 3))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]], true]));
  it('(vector? \'(1 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], true]));
  return it('(vector? \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], true]));
});

describe('array-sort', (): any => {
  it('(array-sort \'(1 30 4 21 100000))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-sort'), [Symbol.for('quote'), [1, 30, 4, 21, 100000]]], [Symbol.for('quote'), [1, 4, 21, 30, 100000]]]));
  it('(array-sort \'("1" "30" "4" "21" "100000"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-sort'), [Symbol.for('quote'), ['1', '30', '4', '21', '100000']]], [Symbol.for('quote'), ['1', '100000', '21', '30', '4']]]));
  it('(array-sort \'(4 3 2 1) (lambda (x y) (cond ((< x y) -1) ((> x y) 1) (else 0))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-sort'), [Symbol.for('quote'), [4, 3, 2, 1]], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('x'), Symbol.for('y')], -1], [[Symbol.for('>'), Symbol.for('x'), Symbol.for('y')], 1], [Symbol.for('else'), 0]]]], [Symbol.for('quote'), [1, 2, 3, 4]]]));
  it('(compile \'(array-sort arr))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort'), Symbol.for('arr')]]], '[...arr].sort((x, y) => (x < y) ? -1 : ((x > y) ? 1 : 0));']));
  it('(compile \'(array-sort arr) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort'), Symbol.for('arr')]], Symbol.for(':to'), 'typescript'], '[...arr].sort((x: any, y: any): number => (x < y) ? -1 : ((x > y) ? 1 : 0));']));
  return it('(compile \'(array-sort arr (lambda (x y) (cond ((< x y) -1) ((> x y) 1) (else 0)))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort'), Symbol.for('arr'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('x'), Symbol.for('y')], -1], [[Symbol.for('>'), Symbol.for('x'), Symbol.for('y')], 1], [Symbol.for('else'), 0]]]]]], `[...arr].sort((x, y) => {
  if (x < y) {
    return -1;
  } else if (x > y) {
    return 1;
  } else {
    return 0;
  }
});`]));
});

describe('array-sort!', (): any => {
  it('(array-sort! \'(1 30 4 21 100000))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-sort!'), [Symbol.for('quote'), [1, 30, 4, 21, 100000]]], [Symbol.for('quote'), [1, 4, 21, 30, 100000]]]));
  it('(array-sort! \'("1" "30" "4" "21" "100000"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-sort!'), [Symbol.for('quote'), ['1', '30', '4', '21', '100000']]], [Symbol.for('quote'), ['1', '100000', '21', '30', '4']]]));
  it('(array-sort! \'(4 3 2 1) (lambda (x y) (cond ((< x y) -1) ((> x y) 1) (else 0))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-sort!'), [Symbol.for('quote'), [4, 3, 2, 1]], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('x'), Symbol.for('y')], -1], [[Symbol.for('>'), Symbol.for('x'), Symbol.for('y')], 1], [Symbol.for('else'), 0]]]], [Symbol.for('quote'), [1, 2, 3, 4]]]));
  it('(compile \'(array-sort! arr))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort!'), Symbol.for('arr')]]], 'arr.sort((x, y) => (x < y) ? -1 : ((x > y) ? 1 : 0));']));
  it('(compile \'(array-sort! arr) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort!'), Symbol.for('arr')]], Symbol.for(':to'), 'typescript'], 'arr.sort((x: any, y: any): number => (x < y) ? -1 : ((x > y) ? 1 : 0));']));
  return it('(compile \'(array-sort! arr (lambda (x y) (cond ((< x y) -1) ((> x y) 1) (else 0)))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort!'), Symbol.for('arr'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('x'), Symbol.for('y')], -1], [[Symbol.for('>'), Symbol.for('x'), Symbol.for('y')], 1], [Symbol.for('else'), 0]]]]]], `arr.sort((x, y) => {
  if (x < y) {
    return -1;
  } else if (x > y) {
    return 1;
  } else {
    return 0;
  }
});`]));
});

describe('array-copy', (): any => {
  it('(array-copy \'(1 2 3))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-copy'), [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('quote'), [1, 2, 3]]]));
  it('(let* ((arr \'(1 2 3)) (arr1 (array-copy arr))) (equal? arr arr1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let*'), [[Symbol.for('arr'), [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('arr1'), [Symbol.for('array-copy'), Symbol.for('arr')]]], [Symbol.for('equal?'), Symbol.for('arr'), Symbol.for('arr1')]], true]));
  it('(let* ((arr \'(1 2 3)) (arr1 (array-copy arr))) (eq? arr arr1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let*'), [[Symbol.for('arr'), [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('arr1'), [Symbol.for('array-copy'), Symbol.for('arr')]]], [Symbol.for('eq?'), Symbol.for('arr'), Symbol.for('arr1')]], false]));
  return it('(compile \'(array-copy arr))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-copy'), Symbol.for('arr')]]], '[...arr];']));
});

describe('Dotted lists', (): any => it('(equal? \'(1 2) \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('equal?'), [Symbol.for('quote'), [1, 2]], [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], true])));

describe('dotted-list?', (): any => {
  it('(dotted-list? \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), []]], false]));
  it('(dotted-list? \'(1 . 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]));
  it('(dotted-list? \'(1 . (2 . 3)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]]], true]));
  it('(dotted-list? \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], true]));
  it('(dotted-list? \'(foo bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], false]));
  return it('(compile \'(dotted-list? x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('dotted-list?'), Symbol.for('x')]]], 'Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for(\'.\'));']));
});

describe('dotted-pair?', (): any => {
  it('(dotted-pair? \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), []]], false]));
  it('(dotted-pair? \'(1 . 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]));
  it('(dotted-pair? \'(1 . (2 . 3)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]]], true]));
  it('(dotted-pair? \'(1 2 . 3))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]], false]));
  it('(dotted-pair? \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], true]));
  it('(dotted-pair? \'(foo bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], false]));
  return it('(compile \'(dotted-pair? x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('dotted-pair?'), Symbol.for('x')]]], 'Array.isArray(x) && (x.length === 3) && (x[1] === Symbol.for(\'.\'));']));
});

describe('dotted-proper-list?', (): any => {
  it('(dotted-proper-list? \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), []]], false]));
  it('(dotted-proper-list? \'(1 . 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], false]));
  it('(dotted-proper-list? \'(1 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], true]));
  it('(dotted-proper-list? \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], true]));
  it('(dotted-proper-list? \'(1 . (2 . 3)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]]], false]));
  it('(dotted-proper-list? \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], false]));
  return it('(dotted-proper-list? \'(foo bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], false]));
});

describe('dotted-improper-list?', (): any => {
  it('(dotted-improper-list? \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), []]], false]));
  it('(dotted-improper-list? \'(1 . 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]));
  it('(dotted-improper-list? \'(1 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], false]));
  it('(dotted-improper-list? \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], false]));
  it('(dotted-improper-list? \'(1 . (2 . 3)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]]], true]));
  it('(dotted-improper-list? \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], true]));
  return it('(dotted-improper-list? \'(foo bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], false]));
});

describe('dotted-list-head', (): any => {
  it('(dotted-list-head \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-head'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], [Symbol.for('quote'), [Symbol.for('foo')]]]));
  it('(dotted-list-head \'(foo bar . baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-head'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]]));
  return it('(compile \'(dotted-list-head x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('dotted-list-head'), Symbol.for('x')]]], 'x.slice(0, -2);']));
});

describe('dotted-list-tail', (): any => {
  it('(dotted-list-tail \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-tail'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], [Symbol.for('quote'), Symbol.for('bar')]]));
  it('(dotted-list-tail \'(foo bar . baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-tail'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]], [Symbol.for('quote'), Symbol.for('baz')]]));
  return it('(compile \'(dotted-list-tail x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('dotted-list-tail'), Symbol.for('x')]]], 'x[x.length - 1];']));
});

describe('dotted-list-parse', (): any => {
  it('(dotted-list-parse \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-parse'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], [Symbol.for('values'), [Symbol.for('quote'), [Symbol.for('foo')]], [Symbol.for('quote'), Symbol.for('bar')]]]));
  return it('(dotted-list-parse \'(foo bar . baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-parse'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]], [Symbol.for('values'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]], [Symbol.for('quote'), Symbol.for('baz')]]]));
});

describe('dotted-list-length', (): any => {
  it('(dotted-list-length \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-length'), [Symbol.for('quote'), []]], 0]));
  it('(dotted-list-length \'(1 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-length'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], 1]));
  return it('(dotted-list-length \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-length'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 2]));
});

describe('dotted-list-ref', (): any => {
  it('(dotted-list-ref \'(1 . ()) 0)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ref'), [Symbol.for('quote'), [1, Symbol.for('.'), []]], 0], 1]));
  it('(dotted-list-ref \'(1 . (2 . ())) 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ref'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]], 1], 2]));
  it('(dotted-list-ref \'(1 2 . ()) 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ref'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]], 1], 2]));
  return it('(dotted-list-ref \'(1 2 . (3 . 4)) 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ref'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), 4]]], 2], 3]));
});

describe('dotted-list-set', (): any => {
  it('(dotted-list-set \'(1 . ()) 0 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-set'), [Symbol.for('quote'), [1, Symbol.for('.'), []]], 0, 2], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]));
  it('(dotted-list-set \'(1 . (2 . ())) 1 3)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-set'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]], 1, 3], [Symbol.for('quote'), [1, Symbol.for('.'), [3, Symbol.for('.'), []]]]]));
  it('(dotted-list-set \'((1 . 2) . (3 . ())) 0 0 4)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-set'), [Symbol.for('quote'), [[1, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]], 0, 0, 4], [Symbol.for('quote'), [[4, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]]]));
  return it('(dotted-list-set \'(1 2 . (3 . ())) 1 4)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-set'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]], 1, 4], [Symbol.for('quote'), [1, 4, Symbol.for('.'), [3, Symbol.for('.'), []]]]]));
});

describe('dotted-list-set!', (): any => {
  it('(let ((lst \'(1 . ()))) (dotted-list-set! lst 0 2) lst)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]]], [Symbol.for('dotted-list-set!'), Symbol.for('lst'), 0, 2], Symbol.for('lst')], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]));
  it('(let ((lst \'(1 . (2 . ())))) (dotted-list-set! lst 1 3) lst)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]]], [Symbol.for('dotted-list-set!'), Symbol.for('lst'), 1, 3], Symbol.for('lst')], [Symbol.for('quote'), [1, Symbol.for('.'), [3, Symbol.for('.'), []]]]]));
  it('(let ((lst \'((1 . 2) . (3 . ())))) (dotted-list-set! lst 0 0 4) lst)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [[1, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]]]], [Symbol.for('dotted-list-set!'), Symbol.for('lst'), 0, 0, 4], Symbol.for('lst')], [Symbol.for('quote'), [[4, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]]]));
  return it('(let ((lst \'(1 2 . (3 . ())))) (dotted-list-set! lst 1 4) lst)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]]]], [Symbol.for('dotted-list-set!'), Symbol.for('lst'), 1, 4], Symbol.for('lst')], [Symbol.for('quote'), [1, 4, Symbol.for('.'), [3, Symbol.for('.'), []]]]]));
});

describe('dotted-list-first', (): any => {
  it('(dotted-list-first \'(1 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-first'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], 1]));
  it('(dotted-list-first \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-first'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 1]));
  return it('(dotted-list-first \'(1 2 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-first'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]], 1]));
});

describe('dotted-list-second', (): any => {
  it('(dotted-list-second \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-second'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 2]));
  return it('(dotted-list-second \'(1 2 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-second'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]], 2]));
});

describe('dotted-list-third', (): any => {
  it('(dotted-list-third \'(1 . (2 . (3 . ()))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-third'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), []]]]]], 3]));
  return it('(dotted-list-third \'(1 2 3 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-third'), [Symbol.for('quote'), [1, 2, 3, Symbol.for('.'), []]]], 3]));
});

describe('dotted-list-fourth', (): any => {
  it('(dotted-list-fourth \'(1 . (2 . (3 . (4 . ())))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-fourth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), []]]]]]], 4]));
  return it('(dotted-list-fourth \'(1 2 3 4 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-fourth'), [Symbol.for('quote'), [1, 2, 3, 4, Symbol.for('.'), []]]], 4]));
});

describe('dotted-list-fifth', (): any => {
  it('(dotted-list-fifth \'(1 . (2 . (3 . (4 . (5 . ()))))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-fifth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), []]]]]]]], 5]));
  return it('(dotted-list-fifth \'(1 2 3 4 5 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-fifth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, Symbol.for('.'), []]]], 5]));
});

describe('dotted-list-sixth', (): any => {
  it('(dotted-list-sixth \'(1 . (2 . (3 . (4 . (5 . (6 . ())))))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-sixth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), []]]]]]]]], 6]));
  return it('(dotted-list-sixth \'(1 2 3 4 5 6 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-sixth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, Symbol.for('.'), []]]], 6]));
});

describe('dotted-list-seventh', (): any => {
  it('(dotted-list-seventh \'(1 . (2 . (3 . (4 . (5 . (6 . (7 . ()))))))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-seventh'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), [7, Symbol.for('.'), []]]]]]]]]], 7]));
  return it('(dotted-list-seventh \'(1 2 3 4 5 6 7 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-seventh'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, 7, Symbol.for('.'), []]]], 7]));
});

describe('dotted-list-eighth', (): any => {
  it('(dotted-list-eighth \'(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . ())))))))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-eighth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), [7, Symbol.for('.'), [8, Symbol.for('.'), []]]]]]]]]]], 8]));
  return it('(dotted-list-eighth \'(1 2 3 4 5 6 7 8 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-eighth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, 7, 8, Symbol.for('.'), []]]], 8]));
});

describe('dotted-list-ninth', (): any => {
  it('(dotted-list-ninth \'(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . ()))))))))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ninth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), [7, Symbol.for('.'), [8, Symbol.for('.'), [9, Symbol.for('.'), []]]]]]]]]]]], 9]));
  return it('(dotted-list-ninth \'(1 2 3 4 5 6 7 8 9 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ninth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, 7, 8, 9, Symbol.for('.'), []]]], 9]));
});

describe('dotted-list-tenth', (): any => {
  it('(dotted-list-tenth \'(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . (10 . ())))))))))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-tenth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), [7, Symbol.for('.'), [8, Symbol.for('.'), [9, Symbol.for('.'), [10, Symbol.for('.'), []]]]]]]]]]]]], 10]));
  return it('(dotted-list-tenth \'(1 2 3 4 5 6 7 8 9 10 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-tenth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, Symbol.for('.'), []]]], 10]));
});

describe('dotted-list-last', (): any => {
  it('(dotted-list-last \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last'), [Symbol.for('quote'), []]], undefined]));
  it('(dotted-list-last \'(1 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], 1]));
  return it('(dotted-list-last \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 2]));
});

describe('dotted-list-last-cdr', (): any => {
  it('(dotted-list-last-cdr \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last-cdr'), [Symbol.for('quote'), []]], [Symbol.for('quote'), []]]));
  it('(dotted-list-last-cdr \'(1 . ()))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last-cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], [Symbol.for('quote'), []]]));
  return it('(dotted-list-last-cdr \'(1 . (2 . ())))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last-cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], [Symbol.for('quote'), []]]));
});

describe('dotted-list->proper-list', (): any => {
  it('(dotted-list->proper-list \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list->proper-list'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]]));
  return it('(dotted-list->proper-list \'(foo bar . baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list->proper-list'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]]));
});

describe('proper-list?', (): any => {
  it('(proper-list? \'(foo bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('proper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], true]));
  return it('(proper-list? \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('proper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], false]));
});

describe('circular-list?', (): any => {
  it('((lambda () (define foo \'()) (circular-list? foo) (set-cdr! foo foo) (circular-list? foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), []]], [Symbol.for('circular-list?'), Symbol.for('foo')], [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')], [Symbol.for('circular-list?'), Symbol.for('foo')]]], false]));
  it('(circular-list? \'(foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('circular-list?'), [Symbol.for('quote'), [Symbol.for('foo')]]], false]));
  it('(circular-list? \'(foo . bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('circular-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], false]));
  it('((lambda () (define foo \'(foo)) (set-cdr! foo foo) (circular-list? foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo')]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')], [Symbol.for('circular-list?'), Symbol.for('foo')]]], true]));
  it('((lambda () (define foo \'(foo . ())) (set-cdr! foo foo) (circular-list? foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), []]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')], [Symbol.for('circular-list?'), Symbol.for('foo')]]], true]));
  return it('((lambda () (define foo \'(foo bar)) (set-cdr! foo foo) (circular-list? foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')], [Symbol.for('circular-list?'), Symbol.for('foo')]]], true]));
});

describe('object', (): any => {
  it('(object)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('object')], [Symbol.for('object')]]));
  it('(object "foo" "bar")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('object'), 'foo', 'bar'], [Symbol.for('object'), 'foo', 'bar']]));
  it('(object "foo" 1 "bar" 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('object'), 'foo', 1, 'bar', 2], [Symbol.for('object'), 'foo', 1, 'bar', 2]]));
  it('(object \'foo 1 \'bar 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('object'), [Symbol.for('quote'), Symbol.for('foo')], 1, [Symbol.for('quote'), Symbol.for('bar')], 2], [Symbol.for('object'), [Symbol.for('quote'), Symbol.for('foo')], 1, [Symbol.for('quote'), Symbol.for('bar')], 2]]));
  it('(object :foo 1 :bar 2)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('object'), Symbol.for(':foo'), 1, Symbol.for(':bar'), 2], [Symbol.for('object'), Symbol.for(':foo'), 1, Symbol.for(':bar'), 2]]));
  it('(let ((x "foo")) (object x "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 'foo']], [Symbol.for('object'), Symbol.for('x'), 'bar']], [Symbol.for('object'), Symbol.for(':foo'), 'bar']]));
  it('(funcall object "foo" "bar")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('object'), 'foo', 'bar'], [Symbol.for('object'), Symbol.for(':foo'), 'bar']]));
  it('(funcall object \'foo "bar")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('object'), [Symbol.for('quote'), Symbol.for('foo')], 'bar'], [Symbol.for('object'), Symbol.for(':foo'), 'bar']]));
  it('(funcall object :foo "bar")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('object'), Symbol.for(':foo'), 'bar'], [Symbol.for('object'), Symbol.for(':foo'), 'bar']]));
  it('(compile \'(object))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object')]]], '({});']));
  it('(compile \'(object "foo" foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', Symbol.for('foo')]]], `({
  foo
});`]));
  it('(compile \'(object "foo" "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', 'bar']]], `({
  foo: 'bar'
});`]));
  it('(compile \'(object foo foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), Symbol.for('foo'), Symbol.for('foo')]]], `({
  [foo]: foo
});`]));
  it('(compile \'(object foo "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), Symbol.for('foo'), 'bar']]], `({
  [foo]: 'bar'
});`]));
  it('(compile \'(object \'foo "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), [Symbol.for('quote'), Symbol.for('foo')], 'bar']]], `({
  foo: 'bar'
});`]));
  it('(compile \'(object :foo "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), Symbol.for(':foo'), 'bar']]], `({
  foo: 'bar'
});`]));
  it('(compile \'(object "foo-bar" "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo-bar', 'baz']]], `({
  'foo-bar': 'baz'
});`]));
  it('(compile \'(object foo-bar "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), Symbol.for('foo-bar'), 'baz']]], `({
  [fooBar]: 'baz'
});`]));
  it('(compile \'(object \'foo-bar "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), [Symbol.for('quote'), Symbol.for('foo-bar')], 'baz']]], `({
  fooBar: 'baz'
});`]));
  it('(compile \'(object :foo-bar "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), Symbol.for(':foo-bar'), 'baz']]], `({
  fooBar: 'baz'
});`]));
  it('(compile \'(object "foo bar" "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo bar', 'baz']]], `({
  'foo bar': 'baz'
});`]));
  it('(compile \'(object "foo bar" baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo bar', Symbol.for('baz')]]], `({
  'foo bar': baz
});`]));
  it('(compile \'(object "foo bar" \'baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo bar', [Symbol.for('quote'), Symbol.for('baz')]]]], `({
  'foo bar': Symbol.for('baz')
});`]));
  it('(compile \'(object "foo bar" :baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo bar', Symbol.for(':baz')]]], `({
  'foo bar': Symbol.for(':baz')
});`]));
  it('(compile \'(object "foo" 1 "bar" 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', 1, 'bar', 2]]], `({
  foo: 1,
  bar: 2
});`]));
  it('(compile \'(object "foo" foo "bar" bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', Symbol.for('foo'), 'bar', Symbol.for('bar')]]], `({
  foo,
  bar
});`]));
  it('(compile \'(object "foo" (object "bar" "baz")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', [Symbol.for('object'), 'bar', 'baz']]]], `({
  foo: {
    bar: 'baz'
  }
});`]));
  it('(compile \'(object "foo" (object "foo" "foo") "bar" (object "bar" "bar")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', [Symbol.for('object'), 'foo', 'foo'], 'bar', [Symbol.for('object'), 'bar', 'bar']]]], `({
  foo: {
    foo: 'foo'
  },
  bar: {
    bar: 'bar'
  }
});`]));
  it('(compile \'(object "foo" (object) "bar" (object "bar" "bar") "baz" (object "baz" "baz")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', [Symbol.for('object')], 'bar', [Symbol.for('object'), 'bar', 'bar'], 'baz', [Symbol.for('object'), 'baz', 'baz']]]], `({
  foo: {},
  bar: {
    bar: 'bar'
  },
  baz: {
    baz: 'baz'
  }
});`]));
  it('(compile \'(object) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object')]], Symbol.for(':as'), 'expression'], '{}']));
  it('(compile \'(object "foo" "bar") :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', 'bar']], Symbol.for(':as'), 'expression'], `{
  foo: 'bar'
}`]));
  it('(compile \'(object "foo" 1 "bar" 2) :as "expression")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', 1, 'bar', 2]], Symbol.for(':as'), 'expression'], `{
  foo: 1,
  bar: 2
}`]));
  it('(compile \'(object) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object')]], Symbol.for(':as'), 'return'], 'return {};']));
  it('(compile \'(object "foo" "bar") :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', 'bar']], Symbol.for(':as'), 'return'], `return {
  foo: 'bar'
};`]));
  return it('(compile \'(object "foo" 1 "bar" 2) :as "return")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object'), 'foo', 1, 'bar', 2]], Symbol.for(':as'), 'return'], `return {
  foo: 1,
  bar: 2
};`]));
});

describe('object?', (): any => it('(compile \'(object? x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object?'), Symbol.for('x')]]], '(x !== null) && (typeof x === \'object\');'])));

describe('object-append', (): any => it('(compile \'(object-append obj (object "foo" "bar")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object-append'), Symbol.for('obj'), [Symbol.for('object'), 'foo', 'bar']]]], `({
  ...obj,
  foo: 'bar'
});`])));

describe('object-keys', (): any => {
  it('(object-keys (object))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('object-keys'), [Symbol.for('object')]], [Symbol.for('quote'), []]]));
  it('(object-keys (object "foo" "bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('object-keys'), [Symbol.for('object'), 'foo', 'bar']], [Symbol.for('quote'), ['foo']]]));
  it('(object-keys (object "foo" "bar" "baz" "quux"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('object-keys'), [Symbol.for('object'), 'foo', 'bar', 'baz', 'quux']], [Symbol.for('quote'), ['foo', 'baz']]]));
  return it('(compile \'(object-keys x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('object-keys'), Symbol.for('x')]]], 'Object.keys(x);']));
});

describe('proper-list->dotted-list', (): any => {
  it('(proper-list->dotted-list \'(foo bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('proper-list->dotted-list'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]]));
  return it('(proper-list->dotted-list \'(foo bar baz))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('proper-list->dotted-list'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]]));
});

describe('define-macro', (): any => {
  it('((lambda () (define-macro (my-macro x) x) (my-macro 1)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-macro'), [Symbol.for('my-macro'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('my-macro'), 1]]], 1]));
  it('(compile \'(define-macro (my-macro env &rest body) `(begin ,env ,@body)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-macro'), [Symbol.for('my-macro'), Symbol.for('env'), Symbol.for('&rest'), Symbol.for('body')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('env')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]], `function myMacro(exp, env1) {
  let [env, ...body] = exp.slice(1);
  return [Symbol.for('begin'), env, ...body];
}

myMacro.ftype = 'macro';`]));
  it('(compile \'(define-macro (foo (x #u)) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-macro'), [Symbol.for('foo'), [Symbol.for('x'), undefined]], Symbol.for('x')]]], `function foo(exp, env) {
  let [x] = exp.slice(1);
  if (x === undefined) {
    x = undefined;
  }
  return x;
}

foo.ftype = 'macro';`]));
  it('(compile \'(define-macro (foo &optional x) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-macro'), [Symbol.for('foo'), Symbol.for('&optional'), Symbol.for('x')], Symbol.for('x')]]], `function foo(exp, env) {
  let [x] = exp.slice(1);
  if (x === undefined) {
    x = undefined;
  }
  return x;
}

foo.ftype = 'macro';`]));
  return it('(compile \'(define-macro (my-macro exp &rest body) `(begin ,exp ,@body)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-macro'), [Symbol.for('my-macro'), Symbol.for('exp'), Symbol.for('&rest'), Symbol.for('body')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]], `function myMacro(exp1, env) {
  let [exp, ...body] = exp1.slice(1);
  return [Symbol.for('begin'), exp, ...body];
}

myMacro.ftype = 'macro';`]));
});

describe('define-fexpr', (): any => it('(compile \'(begin (define-fexpr (foo x) x) (define x 1) (define bar (foo x))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-fexpr'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('foo'), Symbol.for('x')]]]]], `function foo(x) {
  return x;
}

foo.ftype = 'fexpr';

let x = 1;

let bar = foo(Symbol.for('x'));`])));

describe('define-inline', (): any => {
  it('(compile \'(define-inline (my-plus x y) (+ x y)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-inline'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]], `function myPlus(x, y) {
  return x + y;
}

myPlus.compilerMacro = (() => {
  let f = (exp, env) => {
    let [x, y] = exp.slice(1);
    return [Symbol.for('+'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();`]));
  return it('(compile \'(define-inline (my-plus . args) (foldl (lambda (x y) (+ x y)) args)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-inline'), [Symbol.for('my-plus'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], Symbol.for('args')]]]], `function myPlus(...args) {
  return undefined.reduce((y, x) => x + y, args);
}

myPlus.compilerMacro = (() => {
  let f = (exp, env) => {
    let args = exp.slice(1);
    return [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('list'), ...args]];
  };
  f.ftype = 'macro';
  return f;
})();`]));
});

describe('define-subst', (): any => it('(compile \'(define-subst (my-plus x y) (+ x y)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-subst'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]], `function myPlus(x, y) {
  return x + y;
}

myPlus.compilerMacro = (() => {
  let f = (exp, env) => {
    let [x, y] = exp.slice(1);
    return [Symbol.for('+'), x, y];
  };
  f.ftype = 'macro';
  return f;
})();`])));

describe('syntax-macro', (): any => it('(compile \'(syntax-macro (x y) `(+ ,x ,y)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('syntax-macro'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]]], `let f = (x, y) => [Symbol.for('+'), x, y];

f.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

f;`])));

describe('let-fields', (): any => it('(compile \'(let-fields (((prop) obj)) prop))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let-fields'), [[[Symbol.for('prop')], Symbol.for('obj')]], Symbol.for('prop')]]], `let {prop} = obj;

prop;`])));

describe('arity', (): any => {
  it('(arity (lambda () 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('arity'), [Symbol.for('lambda'), [], 1]], 0]));
  it('(arity (lambda (x) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('arity'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]], 1]));
  it('(arity (lambda (x y) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('arity'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('x')]], 2]));
  return it('(compile \'(arity f))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('arity'), Symbol.for('f')]]], 'f.length;']));
});

describe('break', (): any => {
  it('((lambda () (while #t (break)) 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('while'), true, [Symbol.for('break')]], 1]], 1]));
  it('(let ((result (list))) (for ((i (range 0 10))) (break) (push-right! result i)) result)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('list')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]], [Symbol.for('break')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')]], Symbol.for('result')], [Symbol.for('quote'), []]]));
  return it('(compile \'(while #t (break)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('while'), true, [Symbol.for('break')]]]], `while (true) {
  break;
}`]));
});

describe('continue', (): any => {
  it('(let ((result (list)) (i 0)) (while (< i 10) (set! i (+ i 1)) (when (< i 5) (continue)) (push-right! result i)) result)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('list')]], [Symbol.for('i'), 0]], [Symbol.for('while'), [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('when'), [Symbol.for('<'), Symbol.for('i'), 5], [Symbol.for('continue')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')]], Symbol.for('result')], [Symbol.for('quote'), [5, 6, 7, 8, 9, 10]]]));
  it('(let ((result (list))) (for ((i (range 0 11))) (when (< i 5) (continue)) (push-right! result i)) result)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('list')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 11]]], [Symbol.for('when'), [Symbol.for('<'), Symbol.for('i'), 5], [Symbol.for('continue')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')]], Symbol.for('result')], [Symbol.for('quote'), [5, 6, 7, 8, 9, 10]]]));
  return it('(compile \'(while #f (continue)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('while'), false, [Symbol.for('continue')]]]], `while (false) {
  continue;
}`]));
});

describe('return', (): any => {
  it('((lambda () (return 1) 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('return'), 1], 2]], 1]));
  it('((js/function () (return 1) 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('js/function'), [], [Symbol.for('return'), 1], 2]], 1]));
  it('((js/arrow () (return 1) 2))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('js/arrow'), [], [Symbol.for('return'), 1], 2]], 1]));
  it('(compile \'(return))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('return')]]], 'return;']));
  it('(compile \'(return 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('return'), 0]]], 'return 0;']));
  return it('(compile \'(while #t (return 0)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('while'), true, [Symbol.for('return'), 0]]]], `while (true) {
  return 0;
}`]));
});

describe('yield', (): any => {
  it('(compile \'(yield))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('yield')]]], 'yield;']));
  return it('(compile \'(yield 0))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('yield'), 0]]], 'yield 0;']));
});

describe('throw', (): any => it('(compile \'(throw (new Error "An error")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), 'An error']]]], 'throw new Error(\'An error\');'])));

describe('await', (): any => it('(compile \'(await (foo)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('await'), [Symbol.for('foo')]]]], 'await foo();'])));

describe('async', (): any => {
  it('(compile \'(async (lambda (x) x)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], 'async x => x;']));
  it('(compile \'(define foo (async (lambda (x) x))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]]], `async function foo(x) {
  return x;
}`]));
  it('(compile \'(define foo (async (lambda (x) x))) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], `async function foo(x: any): Promise<any> {
  return x;
}`]));
  return it('(compile \'(define/async (foo x) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define/async'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')]]], `async function foo(x) {
  return x;
}`]));
});

describe('oget', (): any => {
  it('(let ((obj (object "prop" "foo"))) (oget obj "prop"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('object'), 'prop', 'foo']]], [Symbol.for('oget'), Symbol.for('obj'), 'prop']], 'foo']));
  it('(oget _ "@@functional/placeholder")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('oget'), Symbol.for('_'), '@@functional/placeholder'], true]));
  it('(compile \'(oget obj foo-bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), Symbol.for('foo-bar')]]], 'obj[fooBar];']));
  it('(compile \'(oget obj \'foo-bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')]]]], 'obj[\'fooBar\'];']));
  it('(compile \'(oget obj :foo-bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), Symbol.for(':foo-bar')]]], 'obj[\'fooBar\'];']));
  it('(compile \'(oget obj "foo-bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), 'foo-bar']]], 'obj[\'foo-bar\'];']));
  return it('(compile \'(oget obj (foo-bar)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), [Symbol.for('foo-bar')]]]], 'obj[fooBar()];']));
});

describe('oset!', (): any => {
  it('(let ((obj (object))) (oset! obj \'foo-bar "baz") (oget obj \'foo-bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('object')]]], [Symbol.for('oset!'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')], 'baz'], [Symbol.for('oget'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')]]], 'baz']));
  it('(let ((obj (object))) (oset! obj :foo-bar "baz") (oget obj :foo-bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('object')]]], [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for(':foo-bar'), 'baz'], [Symbol.for('oget'), Symbol.for('obj'), Symbol.for(':foo-bar')]], 'baz']));
  it('(let ((obj (object))) (oset! obj "foo-bar" "baz") (oget obj "foo-bar"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('object')]]], [Symbol.for('oset!'), Symbol.for('obj'), 'foo-bar', 'baz'], [Symbol.for('oget'), Symbol.for('obj'), 'foo-bar']], 'baz']));
  it('(compile \'(oset! obj foo-bar "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for('foo-bar'), 'baz']]], 'obj[fooBar] = \'baz\';']));
  it('(compile \'(oset! obj \'foo-bar "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')], 'baz']]], 'obj[\'fooBar\'] = \'baz\';']));
  it('(compile \'(oset! obj :foo-bar "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for(':foo-bar'), 'baz']]], 'obj[\'fooBar\'] = \'baz\';']));
  it('(compile \'(oset! obj "foo-bar" "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), 'foo-bar', 'baz']]], 'obj[\'foo-bar\'] = \'baz\';']));
  return it('(compile \'(oset! obj (foo-bar) "baz"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), [Symbol.for('foo-bar')], 'baz']]], 'obj[fooBar()] = \'baz\';']));
});

describe('Dot', (): any => {
  it('\'.', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), Symbol.for('.')], [Symbol.for('quote'), Symbol.for('.')]]));
  it('(array-ref \'(1 . 2) 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-ref'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]], 1], [Symbol.for('quote'), Symbol.for('.')]]));
  it('(compile \'.)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('.')]], 'Symbol.for(\'.\');']));
  it('(compile \'(. map get "foo"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('.'), Symbol.for('map'), Symbol.for('get'), 'foo']]], 'map.get(\'foo\');']));
  it('(compile \'(.get map "foo"))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('.get'), Symbol.for('map'), 'foo']]], 'map.get(\'foo\');']));
  return it('(compile \'(.-length arr))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('.-length'), Symbol.for('arr')]]], 'arr.length;']));
});

describe('new', (): any => {
  it('(let (quux) (set! quux (new (class () (define/public (bar) "baz")))) (send quux bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('quux')], [Symbol.for('set!'), Symbol.for('quux'), [Symbol.for('new'), [Symbol.for('class'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'baz']]]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]], 'baz']));
  it('(let (quux) (set! quux (new (class () (define/public val 1) (define (constructor x) (set-field! val this x)) (define/public (bar) (get-field val this))) 2)) (send quux bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('quux')], [Symbol.for('set!'), Symbol.for('quux'), [Symbol.for('new'), [Symbol.for('class'), [], [Symbol.for('define/public'), Symbol.for('val'), 1], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('set-field!'), Symbol.for('val'), Symbol.for('this'), Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('get-field'), Symbol.for('val'), Symbol.for('this')]]], 2]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]], 2]));
  it('(compile \'(new Foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('new'), Symbol.for('Foo')]]], 'new Foo();']));
  return it('(compile \'(new Foo x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('new'), Symbol.for('Foo'), Symbol.for('x')]]], 'new Foo(x);']));
});

describe('new/apply', (): any => it('(compile \'(new/apply Foo args))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('new/apply'), Symbol.for('Foo'), Symbol.for('args')]]], 'new Foo(...args);'])));

describe('class', (): any => {
  it('((lambda () (define Foo (class object% (define/public (bar) "baz"))) (define quux (new Foo)) (send quux bar)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('Foo'), [Symbol.for('class'), Symbol.for('object%'), [Symbol.for('define/public'), [Symbol.for('bar')], 'baz']]], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]]], 'baz']));
  it('((lambda () (defclass Foo () (define/public (bar) "baz")) (define quux (new Foo)) (send quux bar)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defclass'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'baz']], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]]], 'baz']));
  it('((lambda () (defclass Foo () (define bar "baz")) (define quux (new Foo)) (get-field bar quux)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defclass'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('bar'), 'baz']], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('get-field'), Symbol.for('bar'), Symbol.for('quux')]]], 'baz']));
  it('((lambda () (defclass Foo () (define x) (define (constructor x) (set-field! x this x)) (define (bar) (get-field x this))) (define quux (new Foo "xyzzy")) (send quux bar)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defclass'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('set-field!'), Symbol.for('x'), Symbol.for('this'), Symbol.for('x')]], [Symbol.for('define'), [Symbol.for('bar')], [Symbol.for('get-field'), Symbol.for('x'), Symbol.for('this')]]], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo'), 'xyzzy']], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]]], 'xyzzy']));
  it('((lambda () (defclass Foo (Object) (define (bar) "baz")) (define quux (new Foo)) (send quux bar)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defclass'), Symbol.for('Foo'), [Symbol.for('Object')], [Symbol.for('define'), [Symbol.for('bar')], 'baz']], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]]], 'baz']));
  return it('(compile \'(class () (define/public (bar) "bar")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('class'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar']]]], `class {
  bar() {
    return 'bar';
  }
}`]));
});

describe('define-class', (): any => {
  it('((lambda () (define-class Foo () (define/public (bar) "bar")) (define foo (new Foo)) (send foo bar)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar']], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('send'), Symbol.for('foo'), Symbol.for('bar')]]], 'bar']));
  it('(compile \'(define-class Foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo')]]], `class Foo {
}`]));
  it('(compile \'(define-class Foo () (define/public (bar) "bar")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar']]]], `class Foo {
  bar() {
    return 'bar';
  }
}`]));
  it('(compile \'(define-class Foo () (define/public (bar) "bar") (define/public (baz) "baz")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'], [Symbol.for('define/public'), [Symbol.for('baz')], 'baz']]]], `class Foo {
  bar() {
    return 'bar';
  }

  baz() {
    return 'baz';
  }
}`]));
  it('(compile \'(define-class Foo () (define/public bar) (define/public baz "baz") (define/public (quux) "quux")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('bar')], [Symbol.for('define/public'), Symbol.for('baz'), 'baz'], [Symbol.for('define/public'), [Symbol.for('quux')], 'quux']]]], `class Foo {
  bar;

  baz = 'baz';

  quux() {
    return 'quux';
  }
}`]));
  it('(compile \'(define-class Foo () (define x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]]], `class Foo {
  x;

  constructor(x) {
    super();
    this.x = x;
  }

  bar() {
    return this.x;
  }
}`]));
  it('(compile \'(define-class Foo () (define x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], `class Foo {
  private x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  bar(): any {
    return this.x;
  }
}`]));
  it('(compile \'(define-class Foo () (define/public x) (define/public (constructor . args) (super) (set! (.-stack this) args)) (define/public (bar) (.-x this))) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-stack'), Symbol.for('this')], Symbol.for('args')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], `class Foo {
  x: any;

  constructor(...args: any[]) {
    super();
    this.stack = args;
  }

  bar(): any {
    return this.x;
  }
}`]));
  it('(compile \'(define-class Foo (Object) (define/public x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Object')], [Symbol.for('define/public'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]]], `class Foo extends Object {
  x;

  constructor(x) {
    super();
    this.x = x;
  }

  bar() {
    return this.x;
  }
}`]));
  it('(compile \'(define-class Foo (Object) (define/public x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Object')], [Symbol.for('define/public'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]]], `class Foo extends Object {
  x;

  constructor(x) {
    super();
    this.x = x;
  }

  bar() {
    return this.x;
  }
}`]));
  it('(compile \'(define-class Foo (Object) (define/private x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/private (bar) (.-x this))) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Object')], [Symbol.for('define/private'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/private'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], `class Foo extends Object {
  private x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  private bar(): any {
    return this.x;
  }
}`]));
  it('(compile \'(define-class Foo () (public x) (define x) (public constructor) (define (constructor x) (super) (set! (.-x this) x)) (public bar) (define (bar) (.-x this))) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('public'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('public'), Symbol.for('constructor')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('public'), Symbol.for('bar')], [Symbol.for('define'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], `class Foo {
  x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  bar(): any {
    return this.x;
  }
}`]));
  it('(compile \'(define-class Foo () (private x) (define x) (define (constructor x) (super) (set! (.-x this) x)) (private bar) (define (bar) (.-x this))) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('private'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('private'), Symbol.for('bar')], [Symbol.for('define'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], `class Foo {
  private x: any;

  constructor(x: any) {
    super();
    this.x = x;
  }

  private bar(): any {
    return this.x;
  }
}`]));
  it('(compile \'(define-class Foo () (define/public arr) (define/public (constructor arr) (set-field! arr this arr)) (define/public (nth i) (aget (get-field arr this) i))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('arr')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('arr')], [Symbol.for('set-field!'), Symbol.for('arr'), Symbol.for('this'), Symbol.for('arr')]], [Symbol.for('define/public'), [Symbol.for('nth'), Symbol.for('i')], [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('arr'), Symbol.for('this')], Symbol.for('i')]]]]], `class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  nth(i) {
    return this.arr[i];
  }
}`]));
  it('(compile \'(define-class Foo () (define/public arr) (define (constructor arr) (set-field! arr this arr)) (define/generator (generator) (for ((x (get-field arr this))) (yield x)))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('arr')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('arr')], [Symbol.for('set-field!'), Symbol.for('arr'), Symbol.for('this'), Symbol.for('arr')]], [Symbol.for('define/generator'), [Symbol.for('generator')], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('get-field'), Symbol.for('arr'), Symbol.for('this')]]], [Symbol.for('yield'), Symbol.for('x')]]]]]], `class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *generator() {
    for (let x of this.arr) {
      yield x;
    }
  }
}`]));
  it('(compile \'(define-class Foo () (define/public arr) (define (constructor arr) (set-field! arr this arr)) (define/generator ((get-field iterator Symbol)) (for ((x (get-field arr this))) (yield x)))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('arr')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('arr')], [Symbol.for('set-field!'), Symbol.for('arr'), Symbol.for('this'), Symbol.for('arr')]], [Symbol.for('define/generator'), [[Symbol.for('get-field'), Symbol.for('iterator'), Symbol.for('Symbol')]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('get-field'), Symbol.for('arr'), Symbol.for('this')]]], [Symbol.for('yield'), Symbol.for('x')]]]]]], `class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *[Symbol.iterator]() {
    for (let x of this.arr) {
      yield x;
    }
  }
}`]));
  return it('(compile \'(define-class Foo () (define foo 0) (define (bar) (define (baz) (get-field foo this)) (baz))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('foo'), 0], [Symbol.for('define'), [Symbol.for('bar')], [Symbol.for('define'), [Symbol.for('baz')], [Symbol.for('get-field'), Symbol.for('foo'), Symbol.for('this')]], [Symbol.for('baz')]]]]], `class Foo {
  foo = 0;

  bar() {
    let baz = () => this.foo;
    return baz();
  }
}`]));
});

describe('this', (): any => {
  it('(compile \'(js/function (this) #u))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [Symbol.for('this')], undefined]]], `function () {
  return undefined;
};`]));
  it('(compile \'(js/function (this) #u) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [Symbol.for('this')], undefined]], Symbol.for(':to'), 'typescript'], `function (this: any): any {
  return undefined;
};`]));
  it('(compile \'(js/function (this arg) arg))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [Symbol.for('this'), Symbol.for('arg')], Symbol.for('arg')]]], `function (arg) {
  return arg;
};`]));
  it('(compile \'(js/function (this arg) arg) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [Symbol.for('this'), Symbol.for('arg')], Symbol.for('arg')]], Symbol.for(':to'), 'typescript'], `function (this: any, arg: any): any {
  return arg;
};`]));
  it('(compile \'(js/function (this . args) args))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')]]], `function (...args) {
  return args;
};`]));
  return it('(compile \'(js/function (this . args) args) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')]], Symbol.for(':to'), 'typescript'], `function (this: any, ...args: any[]): any {
  return args;
};`]));
});

describe('instance-of?', (): any => {
  it('(instance-of? (new Map) Map)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('instance-of?'), [Symbol.for('new'), Symbol.for('Map')], Symbol.for('Map')], true]));
  return it('(compile \'(instance-of? x Foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('instance-of?'), Symbol.for('x'), Symbol.for('Foo')]]], 'x instanceof Foo;']));
});

describe('plist->alist', (): any => {
  it('(plist->alist \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->alist'), [Symbol.for('quote'), []]], [Symbol.for('quote'), []]]));
  it('(plist->alist \'(foo bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->alist'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]]]));
  return it('(plist->alist \'(foo bar baz quux))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->alist'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz'), Symbol.for('quux')]]], [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')]]]]));
});

describe('plist->object', (): any => {
  it('(plist->object \'())', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->object'), [Symbol.for('quote'), []]], [Symbol.for('object')]]));
  it('(plist->object \'(foo bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->object'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], [Symbol.for('object'), 'foo', [Symbol.for('quote'), Symbol.for('bar')]]]));
  return it('(plist->object \'(foo bar baz quux))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->object'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz'), Symbol.for('quux')]]], [Symbol.for('object'), 'foo', [Symbol.for('quote'), Symbol.for('bar')], 'baz', [Symbol.for('quote'), Symbol.for('quux')]]]));
});

describe('define-fields', (): any => {
  it('((lambda () (define-fields (x) (object "x" 1)) x))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-fields'), [Symbol.for('x')], [Symbol.for('object'), 'x', 1]], Symbol.for('x')]], 1]));
  it('((lambda () (define-fields (foo) (object "foo" "bar")) foo))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-fields'), [Symbol.for('foo')], [Symbol.for('object'), 'foo', 'bar']], Symbol.for('foo')]], 'bar']));
  it('((lambda () (define-fields ((foo bar)) (object "foo" "bar")) bar))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-fields'), [[Symbol.for('foo'), Symbol.for('bar')]], [Symbol.for('object'), 'foo', 'bar']], Symbol.for('bar')]], 'bar']));
  it('(compile \'(define-fields (prop) obj))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [Symbol.for('prop')], Symbol.for('obj')]]], 'let {prop} = obj;']));
  it('(compile \'(define-fields (prop) obj))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [Symbol.for('prop')], Symbol.for('obj')]]], 'let {prop} = obj;']));
  it('(compile \'(define-fields ((x y) z) obj))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')], Symbol.for('obj')]]], 'let {x: y, z} = obj;']));
  it('(compile \'(define-fields ((x y) z) obj))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')], Symbol.for('obj')]]], 'let {x: y, z} = obj;']));
  it('(compile \'(define-fields (foo) (object "foo" "bar")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [Symbol.for('foo')], [Symbol.for('object'), 'foo', 'bar']]]], `let {foo} = {
  foo: 'bar'
};`]));
  it('(compile \'(define-fields ((foo bar)) (object "foo" "bar")))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [[Symbol.for('foo'), Symbol.for('bar')]], [Symbol.for('object'), 'foo', 'bar']]]], `let {foo: bar} = {
  foo: 'bar'
};`]));
  it('(compile \'(module m scheme (define (foo) (define obj (object)) (define-fields (x rest) obj) (append rest \'(5)))) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo')], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('object')]], [Symbol.for('define-fields'), [Symbol.for('x'), Symbol.for('rest')], Symbol.for('obj')], [Symbol.for('append'), Symbol.for('rest'), [Symbol.for('quote'), [5]]]]]], Symbol.for(':to'), 'typescript'], `function foo(): any {
  let obj: any = {};
  let {x, rest} = obj;
  return [...rest, 5];
}`]));
  return it('(compile \'(module m scheme (define (foo) (define obj (object)) (define-fields ((rest r) x) obj) (list r x))) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo')], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('object')]], [Symbol.for('define-fields'), [[Symbol.for('rest'), Symbol.for('r')], Symbol.for('x')], Symbol.for('obj')], [Symbol.for('list'), Symbol.for('r'), Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], `function foo(): any {
  let obj: any = {};
  let {rest: r, x} = obj;
  return [r, x];
}`]));
});

describe('set!-fields', (): any => {
  it('((lambda () (let (x) (set!-fields (x) (object "x" 1)) x)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('let'), [Symbol.for('x')], [Symbol.for('set!-fields'), [Symbol.for('x')], [Symbol.for('object'), 'x', 1]], Symbol.for('x')]]], 1]));
  it('(compile \'(set!-fields (prop) obj))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!-fields'), [Symbol.for('prop')], Symbol.for('obj')]]], '({prop} = obj);']));
  return it('(compile \'(set!-fields (x) (object "x" 1)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!-fields'), [Symbol.for('x')], [Symbol.for('object'), 'x', 1]]]], `({x} = {
  x: 1
});`]));
});

describe('Map', (): any => {
  it('(new Map)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('new'), Symbol.for('Map')], [Symbol.for('new'), Symbol.for('Map')]]));
  return it('(~> (new Map \'((1 2))) (send _ entries) (send Array from _))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('~>'), [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('quote'), [[1, 2]]]], [Symbol.for('send'), Symbol.for('_'), Symbol.for('entries')], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('from'), Symbol.for('_')]], [Symbol.for('quote'), [[1, 2]]]]));
});

describe('member?', (): any => {
  it('(member? 2 \'(1 2 3 4))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('member?'), 2, [Symbol.for('quote'), [1, 2, 3, 4]]], true]));
  it('(member? 9 \'(1 2 3 4))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('member?'), 9, [Symbol.for('quote'), [1, 2, 3, 4]]], false]));
  it('(compile \'(member? 2 (list 1 2 3 4) f))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('member?'), 2, [Symbol.for('list'), 1, 2, 3, 4], Symbol.for('f')]]], '[1, 2, 3, 4].findIndex(x => f(2, x)) >= 0;']));
  it('(compile \'(member? (+ 1 1) (list 1 2 3 4) f))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('member?'), [Symbol.for('+'), 1, 1], [Symbol.for('list'), 1, 2, 3, 4], Symbol.for('f')]]], `let v = 1 + 1;

[1, 2, 3, 4].findIndex(x => f(v, x)) >= 0;`]));
  return it('(compile \'(member? (+ 1 1) (list 1 2 3 4) (memoize f)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('member?'), [Symbol.for('+'), 1, 1], [Symbol.for('list'), 1, 2, 3, 4], [Symbol.for('memoize'), Symbol.for('f')]]]], `let v = 1 + 1;

let isEqual = memoize(f);

[1, 2, 3, 4].findIndex(x => isEqual(v, x)) >= 0;`]));
});

describe('memq?', (): any => {
  it('(memq? 2 \'(1 2 3 4))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('memq?'), 2, [Symbol.for('quote'), [1, 2, 3, 4]]], true]));
  it('(memq? 9 \'(1 2 3 4))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('memq?'), 9, [Symbol.for('quote'), [1, 2, 3, 4]]], false]));
  it('(compile \'(memq? x lst))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('memq?'), Symbol.for('x'), Symbol.for('lst')]]], 'lst.includes(x);']));
  it('(compile \'(memq? 2 (list 1 2 3 4)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('memq?'), 2, [Symbol.for('list'), 1, 2, 3, 4]]]], '[1, 2, 3, 4].includes(2);']));
  return it('(compile \'(memq? (+ 1 1) (list 1 2 3 4)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('memq?'), [Symbol.for('+'), 1, 1], [Symbol.for('list'), 1, 2, 3, 4]]]], '[1, 2, 3, 4].includes(1 + 1);']));
});

describe('as~>', (): any => {
  it('(as~> 0 _ (+ _ 1) (+ _ 1))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('as~>'), 0, Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), 1], [Symbol.for('+'), Symbol.for('_'), 1]], 2]));
  it('(macroexpand \'(as~> 0 _ (+ _ 1) (+ _ 1)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('macroexpand'), [Symbol.for('quote'), [Symbol.for('as~>'), 0, Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), 1], [Symbol.for('+'), Symbol.for('_'), 1]]]], [Symbol.for('quote'), [Symbol.for('+'), [Symbol.for('+'), 0, 1], 1]]]));
  return it('(compile \'(as~> 0 _ (+ _ 1) (+ _ 1)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('as~>'), 0, Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), 1], [Symbol.for('+'), Symbol.for('_'), 1]]]], '0 + 1 + 1;']));
});

describe('alist->tuples', (): any => {
  it('(alist->tuples \'((foo . bar)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('alist->tuples'), [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]]], [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('bar')]]]]));
  it('(alist->tuples \'((foo . bar) (baz . quux)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('alist->tuples'), [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')]]]], [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('quux')]]]]));
  it('(compile \'(alist->tuples alst))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('alist->tuples'), Symbol.for('alst')]]], 'alst.map(x => [x[0], ((x.length === 3) && (x[1] === Symbol.for(\'.\'))) ? x[2] : x.slice(1)]);']));
  it('(compile \'(alist->tuples \'((1 . 2))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('alist->tuples'), [Symbol.for('quote'), [[1, Symbol.for('.'), 2]]]]]], '[[1, 2]];']));
  it('(compile \'(alist->tuples \'((1 . 2) (3 . 4))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('alist->tuples'), [Symbol.for('quote'), [[1, Symbol.for('.'), 2], [3, Symbol.for('.'), 4]]]]]], '[[1, 2], [3, 4]];']));
  it('(compile \'(alist->tuples `((1 . 2) ,@(tuples->alist \'((3 4))))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('alist->tuples'), [Symbol.for('quasiquote'), [[1, Symbol.for('.'), 2], [Symbol.for('unquote-splicing'), [Symbol.for('tuples->alist'), [Symbol.for('quote'), [[3, 4]]]]]]]]]], '[[1, 2], [3, 4]];']));
  it('(compile \'(alist->tuples `((1 . 2) ,@(tuples->alist lst))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('alist->tuples'), [Symbol.for('quasiquote'), [[1, Symbol.for('.'), 2], [Symbol.for('unquote-splicing'), [Symbol.for('tuples->alist'), Symbol.for('lst')]]]]]]], '[[1, 2], ...lst];']));
  return it('(compile \'(alist->tuples (tuples->alist x)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('alist->tuples'), [Symbol.for('tuples->alist'), Symbol.for('x')]]]], 'x;']));
});

describe('tuples->alist', (): any => {
  it('(tuples->alist \'((foo bar)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('tuples->alist'), [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('bar')]]]], [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]]]));
  it('(tuples->alist \'((foo bar) (baz quux)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('tuples->alist'), [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('quux')]]]], [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')]]]]));
  it('(compile \'(tuples->alist alst))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('tuples->alist'), Symbol.for('alst')]]], 'alst.map(x => [x[0], ...(x1 => Array.isArray(x1) ? x1 : [Symbol.for(\'.\'), x1])(x[1])]);']));
  it('(compile \'(tuples->alist \'((1 2))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('tuples->alist'), [Symbol.for('quote'), [[1, 2]]]]]], '[[1, Symbol.for(\'.\'), 2]];']));
  it('(compile \'(tuples->alist \'((1 2) (3 4))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('tuples->alist'), [Symbol.for('quote'), [[1, 2], [3, 4]]]]]], '[[1, Symbol.for(\'.\'), 2], [3, Symbol.for(\'.\'), 4]];']));
  it('(compile \'(tuples->alist `((1 2) ,@(alist->tuples \'((3 . 4))))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('tuples->alist'), [Symbol.for('quasiquote'), [[1, 2], [Symbol.for('unquote-splicing'), [Symbol.for('alist->tuples'), [Symbol.for('quote'), [[3, Symbol.for('.'), 4]]]]]]]]]], '[[1, Symbol.for(\'.\'), 2], [3, Symbol.for(\'.\'), 4]];']));
  it('(compile \'(tuples->alist `((1 2) ,@(alist->tuples lst))))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('tuples->alist'), [Symbol.for('quasiquote'), [[1, 2], [Symbol.for('unquote-splicing'), [Symbol.for('alist->tuples'), Symbol.for('lst')]]]]]]], '[[1, Symbol.for(\'.\'), 2], ...lst];']));
  return it('(compile \'(tuples->alist (alist->tuples x)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('tuples->alist'), [Symbol.for('alist->tuples'), Symbol.for('x')]]]], 'x;']));
});

describe('define-type', (): any => {
  it('(compile \'(define-type NN (-> Number Number)) :to "javascript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-type'), Symbol.for('NN'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]]], Symbol.for(':to'), 'javascript'], '']));
  it('(compile \'(define-type NN (-> Number Number)) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-type'), Symbol.for('NN'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]]], Symbol.for(':to'), 'typescript'], 'type NN = (a: number) => number;']));
  it('(compile \'(begin (define-type NN (-> Number Number)) (: f NN) (define f (lambda (x) x))) :to "javascript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-type'), Symbol.for('NN'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]], [Symbol.for(':'), Symbol.for('f'), Symbol.for('NN')], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], Symbol.for(':to'), 'javascript'], 'let f = x => x;']));
  return it('(compile \'(begin (define-type NN (-> Number Number)) (: f NN) (define f (lambda (x) x))) :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-type'), Symbol.for('NN'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]], [Symbol.for(':'), Symbol.for('f'), Symbol.for('NN')], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], `type NN = (a: number) => number;

let f: NN = (x: any): any => x;`]));
});

describe('parse', (): any => {
  it('(parse "foo")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('parse'), 'foo'], [Symbol.for('quote'), Symbol.for('foo')]]));
  return it('(parse "(foo)")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('parse'), '(foo)'], [Symbol.for('quote'), [Symbol.for('foo')]]]));
});

describe('interpret', (): any => {
  it('(interpret 1)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('interpret'), 1], 1]));
  it('(interpret \'\'foo)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('interpret'), [Symbol.for('quote'), [Symbol.for('quote'), Symbol.for('foo')]]], [Symbol.for('quote'), Symbol.for('foo')]]));
  it('(interpret \'(second \'(1 . (2 . ()))) :fdottedlists #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('interpret'), [Symbol.for('quote'), [Symbol.for('second'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]]], Symbol.for(':fdottedlists'), true], 2]));
  it('(compile \'(module m scheme (interpret 1)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('interpret'), 1]]]], `import {
  interpret
} from 'roselisp';

interpret(1);`]));
  return it('(compile \'(module m scheme (eval 1)))', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('eval'), 1]]]], `import {
  interpret
} from 'roselisp';

interpret(1);`]));
});

describe('compile', (): any => {
  it('(compile #t)', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true], 'true;']));
  it('(compile #t :to "javascript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true, Symbol.for(':to'), 'javascript'], 'true;']));
  it('(compile #t :from \'roselisp :to "javascript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true, Symbol.for(':from'), [Symbol.for('quote'), Symbol.for('roselisp')], Symbol.for(':to'), 'javascript'], 'true;']));
  it('(compile \'(ann #t Any) :from "roselisp" :to "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]], Symbol.for(':from'), 'roselisp', Symbol.for(':to'), 'typescript'], 'true as any;']));
  it('(compile "true" :to "roselisp")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'true', Symbol.for(':to'), 'roselisp'], true]));
  it('(compile "true" :from "javascript" :to "roselisp")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'true', Symbol.for(':from'), 'javascript', Symbol.for(':to'), 'roselisp'], true]));
  return it('(compile "true as any" :from "typescript" :to "roselisp")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'true as any', Symbol.for(':from'), 'typescript', Symbol.for(':to'), 'roselisp'], [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]]]));
});

describe('decompile', (): any => {
  it('(decompile "true")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true'], true]));
  it('(decompile "true" :from "javascript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true', Symbol.for(':from'), 'javascript'], true]));
  it('(decompile "true" :from "javascript" :to "roselisp")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true', Symbol.for(':from'), 'javascript', Symbol.for(':to'), 'roselisp'], true]));
  it('(decompile "true as any" :from "typescript")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true as any', Symbol.for(':from'), 'typescript'], [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]]]));
  return it('(decompile "true as any" :from "typescript" :to "roselisp")', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true as any', Symbol.for(':from'), 'typescript', Symbol.for(':to'), 'roselisp'], [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]]]));
});

describe('license', (): any => it('license', (): any => testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('license'), [Symbol.for('quote'), Symbol.for('MPL-2.0')]])));