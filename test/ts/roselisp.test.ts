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

describe('#u', function (): any {
  it('#u', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), undefined, undefined]);
  });
  it('\'#u', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), undefined], undefined]);
  });
  it('undefined', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('undefined'), undefined]);
  });
  it('(compile #u)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), undefined], 'undefined;']);
  });
  return it('(compile \'undefined)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('undefined')]], 'undefined;']);
  });
});

describe('#n', function (): any {
  it('#n', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), null, null]);
  });
  it('\'#n', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), null], null]);
  });
  it('js-null', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('js-null'), null]);
  });
  it('js/null', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('js/null'), null]);
  });
  return it('(compile #n)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), null], 'null;']);
  });
});

describe('NaN', function (): any {
  it('NaN', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('NaN'), Symbol.for('NaN')]);
  });
  it('(nan? NaN)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nan?'), Symbol.for('NaN')], true]);
  });
  it('(nan? 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('nan?'), 0], false]);
  });
  it('(compile \'NaN)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('NaN')]], 'NaN;']);
  });
  return it('(compile \'(nan? x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('nan?'), Symbol.for('x')]]], 'isNaN(x);']);
  });
});

describe('true?', function (): any {
  it('(true? #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), true], true]);
  });
  it('(true? #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), false], false]);
  });
  it('(true? #u)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), undefined], false]);
  });
  it('(true? #n)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), null], false]);
  });
  return it('(true? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('true?'), [Symbol.for('quote'), []]], true]);
  });
});

describe('false?', function (): any {
  it('(false? #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), true], false]);
  });
  it('(false? #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), false], true]);
  });
  it('(false? #u)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), undefined], true]);
  });
  it('(false? #n)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), null], true]);
  });
  return it('(false? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('false?'), [Symbol.for('quote'), []]], false]);
  });
});

describe('atom?', function (): any {
  it('(atom? #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('atom?'), true], true]);
  });
  it('(atom? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('atom?'), [Symbol.for('quote'), []]], true]);
  });
  it('(atom? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('atom?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], false]);
  });
  return it('(atom? \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('atom?'), [Symbol.for('quote'), [1, 2, 3]]], false]);
  });
});

describe('keyword->string', function (): any {
  return it('(keyword->string :foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('keyword->string'), Symbol.for(':foo')], 'foo']);
  });
});

describe('string->keyword', function (): any {
  return it('(string->keyword "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string->keyword'), 'foo'], [Symbol.for('quote'), Symbol.for(':foo')]]);
  });
});

describe('keyword->symbol', function (): any {
  return it('(keyword->symbol :foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('keyword->symbol'), Symbol.for(':foo')], [Symbol.for('quote'), Symbol.for('foo')]]);
  });
});

describe('symbol->keyword', function (): any {
  return it('(symbol->keyword \'foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('symbol->keyword'), [Symbol.for('quote'), Symbol.for('foo')]], [Symbol.for('quote'), Symbol.for(':foo')]]);
  });
});

describe('pair-or-list?', function (): any {
  it('(pair-or-list? #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair-or-list?'), true], false]);
  });
  it('(pair-or-list? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair-or-list?'), [Symbol.for('quote'), []]], true]);
  });
  it('(pair-or-list? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair-or-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]);
  });
  it('(pair-or-list? \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair-or-list?'), [Symbol.for('quote'), [1, 2, 3]]], true]);
  });
  return it('(compile \'(pair-or-list? x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('pair-or-list?'), Symbol.for('x')]]], 'Array.isArray(x);']);
  });
});

describe('vector?', function (): any {
  it('(vector? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), []]], true]);
  });
  it('(vector? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]);
  });
  it('(vector? \'(1 2 . 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]], true]);
  });
  it('(vector? \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], true]);
  });
  return it('(vector? \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('vector?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], true]);
  });
});

describe('array-sort', function (): any {
  it('(array-sort \'(4 3 2 1) (lambda (x y) (cond ((< x y) -1) ((> x y) 1) (else 0))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-sort'), [Symbol.for('quote'), [4, 3, 2, 1]], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('x'), Symbol.for('y')], -1], [[Symbol.for('>'), Symbol.for('x'), Symbol.for('y')], 1], [Symbol.for('else'), 0]]]], [Symbol.for('quote'), [1, 2, 3, 4]]]);
  });
  it('(compile \'(array-sort arr))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort'), Symbol.for('arr')]]], '[...arr].sort();']);
  });
  return it('(compile \'(array-sort arr (lambda (x y) (cond ((< x y) -1) ((> x y) 1) (else 0)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort'), Symbol.for('arr'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('x'), Symbol.for('y')], -1], [[Symbol.for('>'), Symbol.for('x'), Symbol.for('y')], 1], [Symbol.for('else'), 0]]]]]], '[...arr].sort(function (x, y) {\n' +
      '  if (x < y) {\n' +
      '    return -1;\n' +
      '  } else if (x > y) {\n' +
      '    return 1;\n' +
      '  } else {\n' +
      '    return 0;\n' +
      '  }\n' +
      '});']);
  });
});

describe('array-sort!', function (): any {
  it('(array-sort! \'(4 3 2 1) (lambda (x y) (cond ((< x y) -1) ((> x y) 1) (else 0))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-sort!'), [Symbol.for('quote'), [4, 3, 2, 1]], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('x'), Symbol.for('y')], -1], [[Symbol.for('>'), Symbol.for('x'), Symbol.for('y')], 1], [Symbol.for('else'), 0]]]], [Symbol.for('quote'), [1, 2, 3, 4]]]);
  });
  it('(compile \'(array-sort! arr))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort!'), Symbol.for('arr')]]], 'arr.sort();']);
  });
  return it('(compile \'(array-sort! arr (lambda (x y) (cond ((< x y) -1) ((> x y) 1) (else 0)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-sort!'), Symbol.for('arr'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('x'), Symbol.for('y')], -1], [[Symbol.for('>'), Symbol.for('x'), Symbol.for('y')], 1], [Symbol.for('else'), 0]]]]]], 'arr.sort(function (x, y) {\n' +
      '  if (x < y) {\n' +
      '    return -1;\n' +
      '  } else if (x > y) {\n' +
      '    return 1;\n' +
      '  } else {\n' +
      '    return 0;\n' +
      '  }\n' +
      '});']);
  });
});

describe('array-copy', function (): any {
  it('(array-copy \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-copy'), [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(let* ((arr \'(1 2 3)) (arr1 (array-copy arr))) (equal? arr arr1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let*'), [[Symbol.for('arr'), [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('arr1'), [Symbol.for('array-copy'), Symbol.for('arr')]]], [Symbol.for('equal?'), Symbol.for('arr'), Symbol.for('arr1')]], true]);
  });
  it('(let* ((arr \'(1 2 3)) (arr1 (array-copy arr))) (eq? arr arr1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let*'), [[Symbol.for('arr'), [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('arr1'), [Symbol.for('array-copy'), Symbol.for('arr')]]], [Symbol.for('eq?'), Symbol.for('arr'), Symbol.for('arr1')]], false]);
  });
  return it('(compile \'(array-copy arr))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('array-copy'), Symbol.for('arr')]]], '[...arr];']);
  });
});

describe('Dotted lists', function (): any {
  return it('(equal? \'(1 2) \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('equal?'), [Symbol.for('quote'), [1, 2]], [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], true]);
  });
});

describe('dotted-list?', function (): any {
  it('(dotted-list? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), []]], false]);
  });
  it('(dotted-list? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]);
  });
  it('(dotted-list? \'(1 . (2 . 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]]], true]);
  });
  it('(dotted-list? \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], true]);
  });
  it('(dotted-list? \'(foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], false]);
  });
  return it('(compile \'(dotted-list? x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('dotted-list?'), Symbol.for('x')]]], 'Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for(\'.\'));']);
  });
});

describe('dotted-pair?', function (): any {
  it('(dotted-pair? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), []]], false]);
  });
  it('(dotted-pair? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]);
  });
  it('(dotted-pair? \'(1 . (2 . 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]]], true]);
  });
  it('(dotted-pair? \'(1 2 . 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]], false]);
  });
  it('(dotted-pair? \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], true]);
  });
  it('(dotted-pair? \'(foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], false]);
  });
  return it('(compile \'(dotted-pair? x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('dotted-pair?'), Symbol.for('x')]]], 'Array.isArray(x) && (x.length === 3) && (x[1] === Symbol.for(\'.\'));']);
  });
});

describe('dotted-proper-list?', function (): any {
  it('(dotted-proper-list? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), []]], false]);
  });
  it('(dotted-proper-list? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], false]);
  });
  it('(dotted-proper-list? \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], true]);
  });
  it('(dotted-proper-list? \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], true]);
  });
  it('(dotted-proper-list? \'(1 . (2 . 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]]], false]);
  });
  it('(dotted-proper-list? \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], false]);
  });
  return it('(dotted-proper-list? \'(foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-proper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], false]);
  });
});

describe('dotted-improper-list?', function (): any {
  it('(dotted-improper-list? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), []]], false]);
  });
  it('(dotted-improper-list? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]);
  });
  it('(dotted-improper-list? \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], false]);
  });
  it('(dotted-improper-list? \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], false]);
  });
  it('(dotted-improper-list? \'(1 . (2 . 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]]], true]);
  });
  it('(dotted-improper-list? \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], true]);
  });
  return it('(dotted-improper-list? \'(foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-improper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], false]);
  });
});

describe('dotted-list-head', function (): any {
  it('(dotted-list-head \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-head'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], [Symbol.for('quote'), [Symbol.for('foo')]]]);
  });
  it('(dotted-list-head \'(foo bar . baz))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-head'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]]);
  });
  return it('(compile \'(dotted-list-head x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('dotted-list-head'), Symbol.for('x')]]], 'x.slice(0, -2);']);
  });
});

describe('dotted-list-tail', function (): any {
  it('(dotted-list-tail \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-tail'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], [Symbol.for('quote'), Symbol.for('bar')]]);
  });
  it('(dotted-list-tail \'(foo bar . baz))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-tail'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]], [Symbol.for('quote'), Symbol.for('baz')]]);
  });
  return it('(compile \'(dotted-list-tail x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('dotted-list-tail'), Symbol.for('x')]]], 'x[x.length - 1];']);
  });
});

describe('dotted-list-parse', function (): any {
  it('(dotted-list-parse \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-parse'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], [Symbol.for('values'), [Symbol.for('quote'), [Symbol.for('foo')]], [Symbol.for('quote'), Symbol.for('bar')]]]);
  });
  return it('(dotted-list-parse \'(foo bar . baz))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-parse'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]], [Symbol.for('values'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]], [Symbol.for('quote'), Symbol.for('baz')]]]);
  });
});

describe('dotted-list-length', function (): any {
  it('(dotted-list-length \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-length'), [Symbol.for('quote'), []]], 0]);
  });
  it('(dotted-list-length \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-length'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], 1]);
  });
  return it('(dotted-list-length \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-length'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 2]);
  });
});

describe('dotted-list-ref', function (): any {
  it('(dotted-list-ref \'(1 . ()) 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ref'), [Symbol.for('quote'), [1, Symbol.for('.'), []]], 0], 1]);
  });
  it('(dotted-list-ref \'(1 . (2 . ())) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ref'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]], 1], 2]);
  });
  it('(dotted-list-ref \'(1 2 . ()) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ref'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]], 1], 2]);
  });
  return it('(dotted-list-ref \'(1 2 . (3 . 4)) 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ref'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), 4]]], 2], 3]);
  });
});

describe('dotted-list-set', function (): any {
  it('(dotted-list-set \'(1 . ()) 0 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-set'), [Symbol.for('quote'), [1, Symbol.for('.'), []]], 0, 2], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(dotted-list-set \'(1 . (2 . ())) 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-set'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]], 1, 3], [Symbol.for('quote'), [1, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  it('(dotted-list-set \'((1 . 2) . (3 . ())) 0 0 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-set'), [Symbol.for('quote'), [[1, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]], 0, 0, 4], [Symbol.for('quote'), [[4, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  return it('(dotted-list-set \'(1 2 . (3 . ())) 1 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-set'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]], 1, 4], [Symbol.for('quote'), [1, 4, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
});

describe('dotted-list-set!', function (): any {
  it('(let ((lst \'(1 . ()))) (dotted-list-set! lst 0 2) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]]], [Symbol.for('dotted-list-set!'), Symbol.for('lst'), 0, 2], Symbol.for('lst')], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(let ((lst \'(1 . (2 . ())))) (dotted-list-set! lst 1 3) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]]], [Symbol.for('dotted-list-set!'), Symbol.for('lst'), 1, 3], Symbol.for('lst')], [Symbol.for('quote'), [1, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  it('(let ((lst \'((1 . 2) . (3 . ())))) (dotted-list-set! lst 0 0 4) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [[1, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]]]], [Symbol.for('dotted-list-set!'), Symbol.for('lst'), 0, 0, 4], Symbol.for('lst')], [Symbol.for('quote'), [[4, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  return it('(let ((lst \'(1 2 . (3 . ())))) (dotted-list-set! lst 1 4) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]]]], [Symbol.for('dotted-list-set!'), Symbol.for('lst'), 1, 4], Symbol.for('lst')], [Symbol.for('quote'), [1, 4, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
});

describe('dotted-list-first', function (): any {
  it('(dotted-list-first \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-first'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], 1]);
  });
  it('(dotted-list-first \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-first'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 1]);
  });
  return it('(dotted-list-first \'(1 2 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-first'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]], 1]);
  });
});

describe('dotted-list-second', function (): any {
  it('(dotted-list-second \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-second'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 2]);
  });
  return it('(dotted-list-second \'(1 2 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-second'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]], 2]);
  });
});

describe('dotted-list-third', function (): any {
  it('(dotted-list-third \'(1 . (2 . (3 . ()))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-third'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), []]]]]], 3]);
  });
  return it('(dotted-list-third \'(1 2 3 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-third'), [Symbol.for('quote'), [1, 2, 3, Symbol.for('.'), []]]], 3]);
  });
});

describe('dotted-list-fourth', function (): any {
  it('(dotted-list-fourth \'(1 . (2 . (3 . (4 . ())))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-fourth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), []]]]]]], 4]);
  });
  return it('(dotted-list-fourth \'(1 2 3 4 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-fourth'), [Symbol.for('quote'), [1, 2, 3, 4, Symbol.for('.'), []]]], 4]);
  });
});

describe('dotted-list-fifth', function (): any {
  it('(dotted-list-fifth \'(1 . (2 . (3 . (4 . (5 . ()))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-fifth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), []]]]]]]], 5]);
  });
  return it('(dotted-list-fifth \'(1 2 3 4 5 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-fifth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, Symbol.for('.'), []]]], 5]);
  });
});

describe('dotted-list-sixth', function (): any {
  it('(dotted-list-sixth \'(1 . (2 . (3 . (4 . (5 . (6 . ())))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-sixth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), []]]]]]]]], 6]);
  });
  return it('(dotted-list-sixth \'(1 2 3 4 5 6 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-sixth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, Symbol.for('.'), []]]], 6]);
  });
});

describe('dotted-list-seventh', function (): any {
  it('(dotted-list-seventh \'(1 . (2 . (3 . (4 . (5 . (6 . (7 . ()))))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-seventh'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), [7, Symbol.for('.'), []]]]]]]]]], 7]);
  });
  return it('(dotted-list-seventh \'(1 2 3 4 5 6 7 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-seventh'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, 7, Symbol.for('.'), []]]], 7]);
  });
});

describe('dotted-list-eighth', function (): any {
  it('(dotted-list-eighth \'(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . ())))))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-eighth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), [7, Symbol.for('.'), [8, Symbol.for('.'), []]]]]]]]]]], 8]);
  });
  return it('(dotted-list-eighth \'(1 2 3 4 5 6 7 8 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-eighth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, 7, 8, Symbol.for('.'), []]]], 8]);
  });
});

describe('dotted-list-ninth', function (): any {
  it('(dotted-list-ninth \'(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . ()))))))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ninth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), [7, Symbol.for('.'), [8, Symbol.for('.'), [9, Symbol.for('.'), []]]]]]]]]]]], 9]);
  });
  return it('(dotted-list-ninth \'(1 2 3 4 5 6 7 8 9 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-ninth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, 7, 8, 9, Symbol.for('.'), []]]], 9]);
  });
});

describe('dotted-list-tenth', function (): any {
  it('(dotted-list-tenth \'(1 . (2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . (10 . ())))))))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-tenth'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), [3, Symbol.for('.'), [4, Symbol.for('.'), [5, Symbol.for('.'), [6, Symbol.for('.'), [7, Symbol.for('.'), [8, Symbol.for('.'), [9, Symbol.for('.'), [10, Symbol.for('.'), []]]]]]]]]]]]], 10]);
  });
  return it('(dotted-list-tenth \'(1 2 3 4 5 6 7 8 9 10 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-tenth'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, Symbol.for('.'), []]]], 10]);
  });
});

describe('dotted-list-last', function (): any {
  it('(dotted-list-last \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last'), [Symbol.for('quote'), []]], undefined]);
  });
  it('(dotted-list-last \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], 1]);
  });
  return it('(dotted-list-last \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 2]);
  });
});

describe('dotted-list-last-cdr', function (): any {
  it('(dotted-list-last-cdr \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last-cdr'), [Symbol.for('quote'), []]], [Symbol.for('quote'), []]]);
  });
  it('(dotted-list-last-cdr \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last-cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], [Symbol.for('quote'), []]]);
  });
  return it('(dotted-list-last-cdr \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list-last-cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], [Symbol.for('quote'), []]]);
  });
});

describe('dotted-list->proper-list', function (): any {
  it('(dotted-list->proper-list \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list->proper-list'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]]);
  });
  return it('(dotted-list->proper-list \'(foo bar . baz))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list->proper-list'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]]);
  });
});

describe('proper-list?', function (): any {
  it('(proper-list? \'(foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('proper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], true]);
  });
  return it('(proper-list? \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('proper-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], false]);
  });
});

describe('circular-list?', function (): any {
  it('((lambda () (define foo \'()) (circular-list? foo) (set-cdr! foo foo) (circular-list? foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), []]], [Symbol.for('circular-list?'), Symbol.for('foo')], [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')], [Symbol.for('circular-list?'), Symbol.for('foo')]]], false]);
  });
  it('(circular-list? \'(foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('circular-list?'), [Symbol.for('quote'), [Symbol.for('foo')]]], false]);
  });
  it('(circular-list? \'(foo . bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('circular-list?'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]], false]);
  });
  it('((lambda () (define foo \'(foo)) (set-cdr! foo foo) (circular-list? foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo')]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')], [Symbol.for('circular-list?'), Symbol.for('foo')]]], true]);
  });
  it('((lambda () (define foo \'(foo . ())) (set-cdr! foo foo) (circular-list? foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), []]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')], [Symbol.for('circular-list?'), Symbol.for('foo')]]], true]);
  });
  return it('((lambda () (define foo \'(foo bar)) (set-cdr! foo foo) (circular-list? foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), Symbol.for('foo')], [Symbol.for('circular-list?'), Symbol.for('foo')]]], true]);
  });
});

describe('proper-list->dotted-list', function (): any {
  it('(proper-list->dotted-list \'(foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('proper-list->dotted-list'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]]);
  });
  return it('(proper-list->dotted-list \'(foo bar baz))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('proper-list->dotted-list'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]]);
  });
});

describe('define-macro', function (): any {
  it('((lambda () (define-macro (my-macro x) x) (my-macro 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-macro'), [Symbol.for('my-macro'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('my-macro'), 1]]], 1]);
  });
  it('(compile \'(define-macro (my-macro env &rest body) `(begin ,env ,@body)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-macro'), [Symbol.for('my-macro'), Symbol.for('env'), Symbol.for('&rest'), Symbol.for('body')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('env')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]], 'function myMacro(exp, env1) {\n' +
      '  let [env, ...body] = exp.slice(1);\n' +
      '  return [Symbol.for(\'begin\'), env, ...body];\n' +
      '}\n' +
      '\n' +
      'myMacro.ftype = \'macro\';']);
  });
  it('(compile \'(define-macro (foo (x #u)) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-macro'), [Symbol.for('foo'), [Symbol.for('x'), undefined]], Symbol.for('x')]]], 'function foo(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  if (x === undefined) {\n' +
      '    x = undefined;\n' +
      '  }\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'macro\';']);
  });
  it('(compile \'(define-macro (foo &optional x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-macro'), [Symbol.for('foo'), Symbol.for('&optional'), Symbol.for('x')], Symbol.for('x')]]], 'function foo(exp, env) {\n' +
      '  let [x] = exp.slice(1);\n' +
      '  if (x === undefined) {\n' +
      '    x = undefined;\n' +
      '  }\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'macro\';']);
  });
  return it('(compile \'(define-macro (my-macro exp &rest body) `(begin ,exp ,@body)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-macro'), [Symbol.for('my-macro'), Symbol.for('exp'), Symbol.for('&rest'), Symbol.for('body')], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]], 'function myMacro(exp1, env) {\n' +
      '  let [exp, ...body] = exp1.slice(1);\n' +
      '  return [Symbol.for(\'begin\'), exp, ...body];\n' +
      '}\n' +
      '\n' +
      'myMacro.ftype = \'macro\';']);
  });
});

describe('define-fexpr', function (): any {
  return it('(compile \'(begin (define-fexpr (foo x) x) (define x 1) (define bar (foo x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-fexpr'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('foo'), Symbol.for('x')]]]]], 'function foo(x) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'fexpr\';\n' +
      '\n' +
      'let x = 1;\n' +
      '\n' +
      'let bar = foo(Symbol.for(\'x\'));']);
  });
});

describe('define-inline', function (): any {
  return it('(compile \'(define-inline (my-plus x y) (+ x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-inline'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]], 'function myPlus(x, y) {\n' +
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

describe('define-subst', function (): any {
  return it('(compile \'(define-subst (my-plus x y) (+ x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-subst'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]], 'function myPlus(x, y) {\n' +
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

describe('syntax-macro', function (): any {
  return it('(compile \'(syntax-macro (x y) `(+ ,x ,y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('syntax-macro'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]]], 'let f = function (x, y) {\n' +
      '  return [Symbol.for(\'+\'), x, y];\n' +
      '};\n' +
      '\n' +
      'f.ftype = [Symbol.for(\'macro->\'), Symbol.for(\'Syntax\'), Symbol.for(\'Syntax\')];\n' +
      '\n' +
      'f;']);
  });
});

describe('declare', function (): any {
  it('(compile \'(define (foo x) (declare (ftype "macro")) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], Symbol.for('x')]]], 'function foo(x) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'foo.ftype = \'macro\';']);
  });
  it('(compile \'(lambda (x) (declare (ftype "macro")) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], Symbol.for('x')]]], 'let f = function (x) {\n' +
      '  return x;\n' +
      '};\n' +
      '\n' +
      'f.ftype = \'macro\';\n' +
      '\n' +
      'f;']);
  });
  return it('(compile \'(begin (define (my-plus x y) (+ x y 0)) (declare my-plus (compiler-macro (macro (x y) `(+ ,x ,y))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), 0]], [Symbol.for('declare'), Symbol.for('my-plus'), [Symbol.for('compiler-macro'), [Symbol.for('macro'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]]]]]], 'function myPlus(x, y) {\n' +
      '  return x + y + 0;\n' +
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

describe('let-fields', function (): any {
  return it('(compile \'(let-fields (((prop) obj)) prop))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let-fields'), [[[Symbol.for('prop')], Symbol.for('obj')]], Symbol.for('prop')]]], 'let {prop} = obj;\n' +
      '\n' +
      'prop;']);
  });
});

describe('arity', function (): any {
  it('(arity (lambda () 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('arity'), [Symbol.for('lambda'), [], 1]], 0]);
  });
  it('(arity (lambda (x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('arity'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]], 1]);
  });
  it('(arity (lambda (x y) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('arity'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('x')]], 2]);
  });
  return it('(compile \'(arity f))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('arity'), Symbol.for('f')]]], 'f.length;']);
  });
});

describe('break', function (): any {
  it('((lambda () (while #t (break)) 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('while'), true, [Symbol.for('break')]], 1]], 1]);
  });
  it('(let ((result (list))) (for ((i (range 0 10))) (break) (push-right! result i)) result)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('list')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]], [Symbol.for('break')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')]], Symbol.for('result')], [Symbol.for('quote'), []]]);
  });
  return it('(compile \'(while #t (break)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('while'), true, [Symbol.for('break')]]]], 'while (true) {\n' +
      '  break;\n' +
      '}']);
  });
});

describe('continue', function (): any {
  it('(let ((result (list)) (i 0)) (while (< i 10) (set! i (+ i 1)) (when (< i 5) (continue)) (push-right! result i)) result)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('list')]], [Symbol.for('i'), 0]], [Symbol.for('while'), [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('when'), [Symbol.for('<'), Symbol.for('i'), 5], [Symbol.for('continue')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')]], Symbol.for('result')], [Symbol.for('quote'), [5, 6, 7, 8, 9, 10]]]);
  });
  it('(let ((result (list))) (for ((i (range 0 11))) (when (< i 5) (continue)) (push-right! result i)) result)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('list')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 11]]], [Symbol.for('when'), [Symbol.for('<'), Symbol.for('i'), 5], [Symbol.for('continue')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')]], Symbol.for('result')], [Symbol.for('quote'), [5, 6, 7, 8, 9, 10]]]);
  });
  return it('(compile \'(while #f (continue)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('while'), false, [Symbol.for('continue')]]]], 'while (false) {\n' +
      '  continue;\n' +
      '}']);
  });
});

describe('return', function (): any {
  it('((lambda () (return 1) 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('return'), 1], 2]], 1]);
  });
  it('((js/function () (return 1) 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('js/function'), [], [Symbol.for('return'), 1], 2]], 1]);
  });
  it('((js/arrow () (return 1) 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('js/arrow'), [], [Symbol.for('return'), 1], 2]], 1]);
  });
  it('(compile \'(return))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('return')]]], 'return;']);
  });
  it('(compile \'(return 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('return'), 0]]], 'return 0;']);
  });
  return it('(compile \'(while #t (return 0)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('while'), true, [Symbol.for('return'), 0]]]], 'while (true) {\n' +
      '  return 0;\n' +
      '}']);
  });
});

describe('yield', function (): any {
  it('(compile \'(yield))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('yield')]]], 'yield;']);
  });
  return it('(compile \'(yield 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('yield'), 0]]], 'yield 0;']);
  });
});

describe('throw', function (): any {
  return it('(compile \'(throw (new Error "An error")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), 'An error']]]], 'throw new Error(\'An error\');']);
  });
});

describe('await', function (): any {
  return it('(compile \'(await (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('await'), [Symbol.for('foo')]]]], 'await foo();']);
  });
});

describe('async', function (): any {
  it('(compile \'(async (lambda (x) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], 'async function (x) {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(define foo (async (lambda (x) x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]]], 'async function foo(x) {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define foo (async (lambda (x) x))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'async function foo(x: any): Promise<any> {\n' +
      '  return x;\n' +
      '}']);
  });
  return it('(compile \'(define/async (foo x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define/async'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')]]], 'async function foo(x) {\n' +
      '  return x;\n' +
      '}']);
  });
});

describe('oget', function (): any {
  it('(let ((obj (js/obj "prop" "foo"))) (oget obj "prop"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'prop', 'foo']]], [Symbol.for('oget'), Symbol.for('obj'), 'prop']], 'foo']);
  });
  it('(oget _ "@@functional/placeholder")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('oget'), Symbol.for('_'), '@@functional/placeholder'], true]);
  });
  it('(compile \'(oget obj foo-bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), Symbol.for('foo-bar')]]], 'obj[fooBar];']);
  });
  it('(compile \'(oget obj \'foo-bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')]]]], 'obj[\'fooBar\'];']);
  });
  it('(compile \'(oget obj :foo-bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), Symbol.for(':foo-bar')]]], 'obj[\'fooBar\'];']);
  });
  it('(compile \'(oget obj "foo-bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), 'foo-bar']]], 'obj[\'foo-bar\'];']);
  });
  return it('(compile \'(oget obj (foo-bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oget'), Symbol.for('obj'), [Symbol.for('foo-bar')]]]], 'obj[fooBar()];']);
  });
});

describe('oset!', function (): any {
  it('(let ((obj (js/obj))) (oset! obj \'foo-bar "baz") (oget obj \'foo-bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj')]]], [Symbol.for('oset!'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')], 'baz'], [Symbol.for('oget'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')]]], 'baz']);
  });
  it('(let ((obj (js/obj))) (oset! obj :foo-bar "baz") (oget obj :foo-bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj')]]], [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for(':foo-bar'), 'baz'], [Symbol.for('oget'), Symbol.for('obj'), Symbol.for(':foo-bar')]], 'baz']);
  });
  it('(let ((obj (js/obj))) (oset! obj "foo-bar" "baz") (oget obj "foo-bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj')]]], [Symbol.for('oset!'), Symbol.for('obj'), 'foo-bar', 'baz'], [Symbol.for('oget'), Symbol.for('obj'), 'foo-bar']], 'baz']);
  });
  it('(compile \'(oset! obj foo-bar "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for('foo-bar'), 'baz']]], 'obj[fooBar] = \'baz\';']);
  });
  it('(compile \'(oset! obj \'foo-bar "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')], 'baz']]], 'obj[\'fooBar\'] = \'baz\';']);
  });
  it('(compile \'(oset! obj :foo-bar "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for(':foo-bar'), 'baz']]], 'obj[\'fooBar\'] = \'baz\';']);
  });
  it('(compile \'(oset! obj "foo-bar" "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), 'foo-bar', 'baz']]], 'obj[\'foo-bar\'] = \'baz\';']);
  });
  return it('(compile \'(oset! obj (foo-bar) "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('oset!'), Symbol.for('obj'), [Symbol.for('foo-bar')], 'baz']]], 'obj[fooBar()] = \'baz\';']);
  });
});

describe('Dot', function (): any {
  it('(compile \'(. map get "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('.'), Symbol.for('map'), Symbol.for('get'), 'foo']]], 'map.get(\'foo\');']);
  });
  it('(compile \'(.get map "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('.get'), Symbol.for('map'), 'foo']]], 'map.get(\'foo\');']);
  });
  return it('(compile \'(.-length arr))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('.-length'), Symbol.for('arr')]]], 'arr.length;']);
  });
});

describe('new', function (): any {
  it('(let (quux) (set! quux (new (class () (define/public (bar) "baz")))) (send quux bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('quux')], [Symbol.for('set!'), Symbol.for('quux'), [Symbol.for('new'), [Symbol.for('class'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'baz']]]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]], 'baz']);
  });
  it('(let (quux) (set! quux (new (class () (define/public val 1) (define (constructor x) (set-field! val this x)) (define/public (bar) (get-field val this))) 2)) (send quux bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('quux')], [Symbol.for('set!'), Symbol.for('quux'), [Symbol.for('new'), [Symbol.for('class'), [], [Symbol.for('define/public'), Symbol.for('val'), 1], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('set-field!'), Symbol.for('val'), Symbol.for('this'), Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('get-field'), Symbol.for('val'), Symbol.for('this')]]], 2]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]], 2]);
  });
  it('(compile \'(new Foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('new'), Symbol.for('Foo')]]], 'new Foo();']);
  });
  return it('(compile \'(new Foo x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('new'), Symbol.for('Foo'), Symbol.for('x')]]], 'new Foo(x);']);
  });
});

describe('new/apply', function (): any {
  return it('(compile \'(new/apply Foo args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('new/apply'), Symbol.for('Foo'), Symbol.for('args')]]], 'new Foo(...args);']);
  });
});

describe('class', function (): any {
  it('((lambda () (define Foo (class object% (define/public (bar) "baz"))) (define quux (new Foo)) (send quux bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('Foo'), [Symbol.for('class'), Symbol.for('object%'), [Symbol.for('define/public'), [Symbol.for('bar')], 'baz']]], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]]], 'baz']);
  });
  it('((lambda () (defclass Foo () (define/public (bar) "baz")) (define quux (new Foo)) (send quux bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defclass'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'baz']], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]]], 'baz']);
  });
  it('((lambda () (defclass Foo () (define bar "baz")) (define quux (new Foo)) (get-field bar quux)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defclass'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('bar'), 'baz']], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('get-field'), Symbol.for('bar'), Symbol.for('quux')]]], 'baz']);
  });
  it('((lambda () (defclass Foo () (define x) (define (constructor x) (set-field! x this x)) (define (bar) (get-field x this))) (define quux (new Foo "xyzzy")) (send quux bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defclass'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('set-field!'), Symbol.for('x'), Symbol.for('this'), Symbol.for('x')]], [Symbol.for('define'), [Symbol.for('bar')], [Symbol.for('get-field'), Symbol.for('x'), Symbol.for('this')]]], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo'), 'xyzzy']], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]]], 'xyzzy']);
  });
  it('((lambda () (defclass Foo (Object) (define (bar) "baz")) (define quux (new Foo)) (send quux bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('defclass'), Symbol.for('Foo'), [Symbol.for('Object')], [Symbol.for('define'), [Symbol.for('bar')], 'baz']], [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('send'), Symbol.for('quux'), Symbol.for('bar')]]], 'baz']);
  });
  return it('(compile \'(class () (define/public (bar) "bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('class'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar']]]], 'class {\n' +
      '  bar() {\n' +
      '    return \'bar\';\n' +
      '  }\n' +
      '}']);
  });
});

describe('define-class', function (): any {
  it('((lambda () (define-class Foo () (define/public (bar) "bar")) (define foo (new Foo)) (send foo bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar']], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('new'), Symbol.for('Foo')]], [Symbol.for('send'), Symbol.for('foo'), Symbol.for('bar')]]], 'bar']);
  });
  it('(compile \'(define-class Foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo')]]], 'class Foo {\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (define/public (bar) "bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar']]]], 'class Foo {\n' +
      '  bar() {\n' +
      '    return \'bar\';\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (define/public (bar) "bar") (define/public (baz) "baz")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'], [Symbol.for('define/public'), [Symbol.for('baz')], 'baz']]]], 'class Foo {\n' +
      '  bar() {\n' +
      '    return \'bar\';\n' +
      '  }\n' +
      '\n' +
      '  baz() {\n' +
      '    return \'baz\';\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (define/public bar) (define/public baz "baz") (define/public (quux) "quux")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('bar')], [Symbol.for('define/public'), Symbol.for('baz'), 'baz'], [Symbol.for('define/public'), [Symbol.for('quux')], 'quux']]]], 'class Foo {\n' +
      '  bar;\n' +
      '\n' +
      '  baz = \'baz\';\n' +
      '\n' +
      '  quux() {\n' +
      '    return \'quux\';\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (define x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]]], 'class Foo {\n' +
      '  x;\n' +
      '\n' +
      '  constructor(x) {\n' +
      '    super();\n' +
      '    this.x = x;\n' +
      '  }\n' +
      '\n' +
      '  bar() {\n' +
      '    return this.x;\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (define x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], 'class Foo {\n' +
      '  private x: any;\n' +
      '\n' +
      '  constructor(x: any) {\n' +
      '    super();\n' +
      '    this.x = x;\n' +
      '  }\n' +
      '\n' +
      '  bar(): any {\n' +
      '    return this.x;\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (define/public x) (define/public (constructor . args) (super) (set! (.-stack this) args)) (define/public (bar) (.-x this))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-stack'), Symbol.for('this')], Symbol.for('args')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], 'class Foo {\n' +
      '  x: any;\n' +
      '\n' +
      '  constructor(...args: any[]) {\n' +
      '    super();\n' +
      '    this.stack = args;\n' +
      '  }\n' +
      '\n' +
      '  bar(): any {\n' +
      '    return this.x;\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo (Object) (define/public x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Object')], [Symbol.for('define/public'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]]], 'class Foo extends Object {\n' +
      '  x;\n' +
      '\n' +
      '  constructor(x) {\n' +
      '    super();\n' +
      '    this.x = x;\n' +
      '  }\n' +
      '\n' +
      '  bar() {\n' +
      '    return this.x;\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo (Object) (define/public x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Object')], [Symbol.for('define/public'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/public'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]]], 'class Foo extends Object {\n' +
      '  x;\n' +
      '\n' +
      '  constructor(x) {\n' +
      '    super();\n' +
      '    this.x = x;\n' +
      '  }\n' +
      '\n' +
      '  bar() {\n' +
      '    return this.x;\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo (Object) (define/private x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/private (bar) (.-x this))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Object')], [Symbol.for('define/private'), Symbol.for('x')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('define/private'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], 'class Foo extends Object {\n' +
      '  private x: any;\n' +
      '\n' +
      '  constructor(x: any) {\n' +
      '    super();\n' +
      '    this.x = x;\n' +
      '  }\n' +
      '\n' +
      '  private bar(): any {\n' +
      '    return this.x;\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (public x) (define x) (public constructor) (define (constructor x) (super) (set! (.-x this) x)) (public bar) (define (bar) (.-x this))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('public'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('public'), Symbol.for('constructor')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('public'), Symbol.for('bar')], [Symbol.for('define'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], 'class Foo {\n' +
      '  x: any;\n' +
      '\n' +
      '  constructor(x: any) {\n' +
      '    super();\n' +
      '    this.x = x;\n' +
      '  }\n' +
      '\n' +
      '  bar(): any {\n' +
      '    return this.x;\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (private x) (define x) (define (constructor x) (super) (set! (.-x this) x)) (private bar) (define (bar) (.-x this))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('private'), Symbol.for('x')], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super')], [Symbol.for('set!'), [Symbol.for('.-x'), Symbol.for('this')], Symbol.for('x')]], [Symbol.for('private'), Symbol.for('bar')], [Symbol.for('define'), [Symbol.for('bar')], [Symbol.for('.-x'), Symbol.for('this')]]]], Symbol.for(':to'), 'typescript'], 'class Foo {\n' +
      '  private x: any;\n' +
      '\n' +
      '  constructor(x: any) {\n' +
      '    super();\n' +
      '    this.x = x;\n' +
      '  }\n' +
      '\n' +
      '  private bar(): any {\n' +
      '    return this.x;\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (define/public arr) (define/public (constructor arr) (set-field! arr this arr)) (define/public (nth i) (aget (get-field arr this) i))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('arr')], [Symbol.for('define/public'), [Symbol.for('constructor'), Symbol.for('arr')], [Symbol.for('set-field!'), Symbol.for('arr'), Symbol.for('this'), Symbol.for('arr')]], [Symbol.for('define/public'), [Symbol.for('nth'), Symbol.for('i')], [Symbol.for('aget'), [Symbol.for('get-field'), Symbol.for('arr'), Symbol.for('this')], Symbol.for('i')]]]]], 'class Foo {\n' +
      '  arr;\n' +
      '\n' +
      '  constructor(arr) {\n' +
      '    this.arr = arr;\n' +
      '  }\n' +
      '\n' +
      '  nth(i) {\n' +
      '    return this.arr[i];\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define-class Foo () (define/public arr) (define (constructor arr) (set-field! arr this arr)) (define/generator (generator) (for ((x (get-field arr this))) (yield x)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('arr')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('arr')], [Symbol.for('set-field!'), Symbol.for('arr'), Symbol.for('this'), Symbol.for('arr')]], [Symbol.for('define/generator'), [Symbol.for('generator')], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('get-field'), Symbol.for('arr'), Symbol.for('this')]]], [Symbol.for('yield'), Symbol.for('x')]]]]]], 'class Foo {\n' +
      '  arr;\n' +
      '\n' +
      '  constructor(arr) {\n' +
      '    this.arr = arr;\n' +
      '  }\n' +
      '\n' +
      '  *generator() {\n' +
      '    for (let x of this.arr) {\n' +
      '      yield x;\n' +
      '    }\n' +
      '  }\n' +
      '}']);
  });
  return it('(compile \'(define-class Foo () (define/public arr) (define (constructor arr) (set-field! arr this arr)) (define/generator ((get-field iterator Symbol)) (for ((x (get-field arr this))) (yield x)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('arr')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('arr')], [Symbol.for('set-field!'), Symbol.for('arr'), Symbol.for('this'), Symbol.for('arr')]], [Symbol.for('define/generator'), [[Symbol.for('get-field'), Symbol.for('iterator'), Symbol.for('Symbol')]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('get-field'), Symbol.for('arr'), Symbol.for('this')]]], [Symbol.for('yield'), Symbol.for('x')]]]]]], 'class Foo {\n' +
      '  arr;\n' +
      '\n' +
      '  constructor(arr) {\n' +
      '    this.arr = arr;\n' +
      '  }\n' +
      '\n' +
      '  *[Symbol.iterator]() {\n' +
      '    for (let x of this.arr) {\n' +
      '      yield x;\n' +
      '    }\n' +
      '  }\n' +
      '}']);
  });
});

describe('this', function (): any {
  it('(compile \'(lambda (this) #u))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('this')], undefined]]], 'function () {\n' +
      '  return undefined;\n' +
      '};']);
  });
  it('(compile \'(lambda (this) #u) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('this')], undefined]], Symbol.for(':to'), 'typescript'], 'function (this: any): any {\n' +
      '  return undefined;\n' +
      '};']);
  });
  it('(compile \'(lambda (this arg) arg))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('this'), Symbol.for('arg')], Symbol.for('arg')]]], 'function (arg) {\n' +
      '  return arg;\n' +
      '};']);
  });
  it('(compile \'(lambda (this arg) arg) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('this'), Symbol.for('arg')], Symbol.for('arg')]], Symbol.for(':to'), 'typescript'], 'function (this: any, arg: any): any {\n' +
      '  return arg;\n' +
      '};']);
  });
  it('(compile \'(lambda (this . args) args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')]]], 'function (...args) {\n' +
      '  return args;\n' +
      '};']);
  });
  return it('(compile \'(lambda (this . args) args) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')]], Symbol.for(':to'), 'typescript'], 'function (this: any, ...args: any[]): any {\n' +
      '  return args;\n' +
      '};']);
  });
});

describe('instance-of?', function (): any {
  it('(instance-of? (new Map) Map)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('instance-of?'), [Symbol.for('new'), Symbol.for('Map')], Symbol.for('Map')], true]);
  });
  return it('(compile \'(instance-of? x Foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('instance-of?'), Symbol.for('x'), Symbol.for('Foo')]]], 'x instanceof Foo;']);
  });
});

describe('plist->alist', function (): any {
  it('(plist->alist \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->alist'), [Symbol.for('quote'), []]], [Symbol.for('quote'), []]]);
  });
  it('(plist->alist \'(foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->alist'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]]]);
  });
  return it('(plist->alist \'(foo bar baz quux))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->alist'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz'), Symbol.for('quux')]]], [Symbol.for('quote'), [[Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')], [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')]]]]);
  });
});

describe('plist->object', function (): any {
  it('(plist->object \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->object'), [Symbol.for('quote'), []]], [Symbol.for('js/obj')]]);
  });
  it('(plist->object \'(foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->object'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]], [Symbol.for('js/obj'), 'foo', [Symbol.for('quote'), Symbol.for('bar')]]]);
  });
  return it('(plist->object \'(foo bar baz quux))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('plist->object'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz'), Symbol.for('quux')]]], [Symbol.for('js/obj'), 'foo', [Symbol.for('quote'), Symbol.for('bar')], 'baz', [Symbol.for('quote'), Symbol.for('quux')]]]);
  });
});

describe('define-fields', function (): any {
  it('((lambda () (define-fields (x) (js/obj "x" 1)) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-fields'), [Symbol.for('x')], [Symbol.for('js/obj'), 'x', 1]], Symbol.for('x')]], 1]);
  });
  it('((lambda () (define-fields (foo) (js/obj "foo" "bar")) foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-fields'), [Symbol.for('foo')], [Symbol.for('js/obj'), 'foo', 'bar']], Symbol.for('foo')]], 'bar']);
  });
  it('((lambda () (define-fields ((foo bar)) (js/obj "foo" "bar")) bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-fields'), [[Symbol.for('foo'), Symbol.for('bar')]], [Symbol.for('js/obj'), 'foo', 'bar']], Symbol.for('bar')]], 'bar']);
  });
  it('(compile \'(define-fields (prop) obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [Symbol.for('prop')], Symbol.for('obj')]]], 'let {prop} = obj;']);
  });
  it('(compile \'(define-fields (prop) obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [Symbol.for('prop')], Symbol.for('obj')]]], 'let {prop} = obj;']);
  });
  it('(compile \'(define-fields ((x y) z) obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')], Symbol.for('obj')]]], 'let {x: y, z} = obj;']);
  });
  it('(compile \'(define-fields ((x y) z) obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')], Symbol.for('obj')]]], 'let {x: y, z} = obj;']);
  });
  it('(compile \'(define-fields (foo) (js/obj "foo" "bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [Symbol.for('foo')], [Symbol.for('js/obj'), 'foo', 'bar']]]], 'let {foo} = {\n' +
      '  foo: \'bar\'\n' +
      '};']);
  });
  it('(compile \'(define-fields ((foo bar)) (js/obj "foo" "bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-fields'), [[Symbol.for('foo'), Symbol.for('bar')]], [Symbol.for('js/obj'), 'foo', 'bar']]]], 'let {foo: bar} = {\n' +
      '  foo: \'bar\'\n' +
      '};']);
  });
  it('(compile \'(module m scheme (define (foo) (define obj (js/obj)) (define-fields (x rest) obj) (append rest \'(5)))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo')], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('js/obj')]], [Symbol.for('define-fields'), [Symbol.for('x'), Symbol.for('rest')], Symbol.for('obj')], [Symbol.for('append'), Symbol.for('rest'), [Symbol.for('quote'), [5]]]]]], Symbol.for(':to'), 'typescript'], 'function foo(): any {\n' +
      '  let obj: any = {};\n' +
      '  let {x, rest} = obj;\n' +
      '  return [...rest, 5];\n' +
      '}']);
  });
  return it('(compile \'(module m scheme (define (foo) (define obj (js/obj)) (define-fields ((rest r) x) obj) (list r x))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo')], [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('js/obj')]], [Symbol.for('define-fields'), [[Symbol.for('rest'), Symbol.for('r')], Symbol.for('x')], Symbol.for('obj')], [Symbol.for('list'), Symbol.for('r'), Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'function foo(): any {\n' +
      '  let obj: any = {};\n' +
      '  let {rest: r, x} = obj;\n' +
      '  return [r, x];\n' +
      '}']);
  });
});

describe('set!-fields', function (): any {
  it('((lambda () (let (x) (set!-fields (x) (js/obj "x" 1)) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('let'), [Symbol.for('x')], [Symbol.for('set!-fields'), [Symbol.for('x')], [Symbol.for('js/obj'), 'x', 1]], Symbol.for('x')]]], 1]);
  });
  it('(compile \'(set!-fields (prop) obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!-fields'), [Symbol.for('prop')], Symbol.for('obj')]]], '({prop} = obj);']);
  });
  return it('(compile \'(set!-fields (x) (js/obj "x" 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!-fields'), [Symbol.for('x')], [Symbol.for('js/obj'), 'x', 1]]]], '({x} = {\n' +
      '  x: 1\n' +
      '});']);
  });
});

describe('Map', function (): any {
  it('(new Map)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('new'), Symbol.for('Map')], [Symbol.for('new'), Symbol.for('Map')]]);
  });
  return it('(~> (new Map \'((1 2))) (send _ entries) (send Array from _))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('~>'), [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('quote'), [[1, 2]]]], [Symbol.for('send'), Symbol.for('_'), Symbol.for('entries')], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('from'), Symbol.for('_')]], [Symbol.for('quote'), [[1, 2]]]]);
  });
});

describe('member?', function (): any {
  it('(member? 2 \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('member?'), 2, [Symbol.for('quote'), [1, 2, 3, 4]]], true]);
  });
  it('(member? 9 \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('member?'), 9, [Symbol.for('quote'), [1, 2, 3, 4]]], false]);
  });
  it('(compile \'(member? 2 (list 1 2 3 4) f))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('member?'), 2, [Symbol.for('list'), 1, 2, 3, 4], Symbol.for('f')]]], '[1, 2, 3, 4].findIndex(function (x) {\n' +
      '  return f(2, x);\n' +
      '}) >= 0;']);
  });
  it('(compile \'(member? (+ 1 1) (list 1 2 3 4) f))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('member?'), [Symbol.for('+'), 1, 1], [Symbol.for('list'), 1, 2, 3, 4], Symbol.for('f')]]], 'let v = 1 + 1;\n' +
      '\n' +
      '[1, 2, 3, 4].findIndex(function (x) {\n' +
      '  return f(v, x);\n' +
      '}) >= 0;']);
  });
  return it('(compile \'(member? (+ 1 1) (list 1 2 3 4) (memoize f)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('member?'), [Symbol.for('+'), 1, 1], [Symbol.for('list'), 1, 2, 3, 4], [Symbol.for('memoize'), Symbol.for('f')]]]], 'let v = 1 + 1;\n' +
      '\n' +
      'let isEqual = memoize(f);\n' +
      '\n' +
      '[1, 2, 3, 4].findIndex(function (x) {\n' +
      '  return isEqual(v, x);\n' +
      '}) >= 0;']);
  });
});

describe('memq?', function (): any {
  it('(memq? 2 \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('memq?'), 2, [Symbol.for('quote'), [1, 2, 3, 4]]], true]);
  });
  it('(memq? 9 \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('memq?'), 9, [Symbol.for('quote'), [1, 2, 3, 4]]], false]);
  });
  it('(compile \'(memq? x lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('memq?'), Symbol.for('x'), Symbol.for('lst')]]], 'lst.includes(x);']);
  });
  it('(compile \'(memq? 2 (list 1 2 3 4)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('memq?'), 2, [Symbol.for('list'), 1, 2, 3, 4]]]], '[1, 2, 3, 4].includes(2);']);
  });
  return it('(compile \'(memq? (+ 1 1) (list 1 2 3 4)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('memq?'), [Symbol.for('+'), 1, 1], [Symbol.for('list'), 1, 2, 3, 4]]]], '[1, 2, 3, 4].includes(1 + 1);']);
  });
});

describe('as~>', function (): any {
  it('(as~> 0 _ (+ _ 1) (+ _ 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('as~>'), 0, Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), 1], [Symbol.for('+'), Symbol.for('_'), 1]], 2]);
  });
  it('(macroexpand \'(as~> 0 _ (+ _ 1) (+ _ 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('macroexpand'), [Symbol.for('quote'), [Symbol.for('as~>'), 0, Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), 1], [Symbol.for('+'), Symbol.for('_'), 1]]]], [Symbol.for('quote'), [Symbol.for('+'), [Symbol.for('+'), 0, 1], 1]]]);
  });
  return it('(compile \'(as~> 0 _ (+ _ 1) (+ _ 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('as~>'), 0, Symbol.for('_'), [Symbol.for('+'), Symbol.for('_'), 1], [Symbol.for('+'), Symbol.for('_'), 1]]]], '0 + 1 + 1;']);
  });
});

describe('ann', function (): any {
  it('(ann #u Any)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('ann'), undefined, Symbol.for('Any')], undefined]);
  });
  it('(compile \'(ann #t Any))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]]], 'true;']);
  });
  it('(compile \'(ann #t Any) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]], Symbol.for(':to'), 'typescript'], 'true as any;']);
  });
  it('(compile \'(ann 1 Number) :to "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), 1, Symbol.for('Number')]], Symbol.for(':to'), 'javascript'], '1;']);
  });
  it('(compile \'(ann 1 Number) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), 1, Symbol.for('Number')]], Symbol.for(':to'), 'typescript'], '1 as number;']);
  });
  it('(compile \'(ann (list) Any) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), [Symbol.for('list')], Symbol.for('Any')]], Symbol.for(':to'), 'typescript'], '[] as any;']);
  });
  it('(compile \'(ann \'() Any) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), [Symbol.for('quote'), []], Symbol.for('Any')]], Symbol.for(':to'), 'typescript'], '[] as any;']);
  });
  it('(compile \'(ann x (List Any)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), Symbol.for('x'), [Symbol.for('List'), Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], 'x as [any];']);
  });
  it('(compile \'(ann x (List Number Any)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), Symbol.for('x'), [Symbol.for('List'), Symbol.for('Number'), Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], 'x as [number, any];']);
  });
  it('(compile \'(ann x NN) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), Symbol.for('x'), Symbol.for('NN')]], Symbol.for(':to'), 'typescript'], 'x as NN;']);
  });
  it('(compile \'(ann x (NN Any)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), Symbol.for('x'), [Symbol.for('NN'), Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], 'x as NN<any>;']);
  });
  it('(compile \'(ann x (NN Any Any)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), Symbol.for('x'), [Symbol.for('NN'), Symbol.for('Any'), Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], 'x as NN<any,any>;']);
  });
  it('(compile \'((ann (lambda (x) x) Any) 1) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [[Symbol.for('ann'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], Symbol.for('Any')], 1]], Symbol.for(':to'), 'typescript'], '(function (x: any): any {\n' +
      '  return x;\n' +
      '} as any)(1);']);
  });
  return it('(compile \'(lambda (x) (ann (send x foo) Any)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('ann'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('foo')], Symbol.for('Any')]]], Symbol.for(':to'), 'typescript'], 'function (x: any): any {\n' +
      '  return x.foo() as any;\n' +
      '};']);
  });
});

describe(':', function (): any {
  it('(compile \'(begin (: x Any) (define x 1)) :to "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('Any')], [Symbol.for('define'), Symbol.for('x'), 1]]], Symbol.for(':to'), 'javascript'], 'let x = 1;']);
  });
  it('(compile \'(begin (: x Any) (define x 1)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('Any')], [Symbol.for('define'), Symbol.for('x'), 1]]], Symbol.for(':to'), 'typescript'], 'let x: any = 1;']);
  });
  it('(compile \'(begin (: x String) (define x "1")) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('String')], [Symbol.for('define'), Symbol.for('x'), '1']]], Symbol.for(':to'), 'typescript'], 'let x: string = \'1\';']);
  });
  it('(compile \'(begin (: x Number) (define x 1)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('Number')], [Symbol.for('define'), Symbol.for('x'), 1]]], Symbol.for(':to'), 'typescript'], 'let x: number = 1;']);
  });
  it('(compile \'(begin (: x Integer) (define x 1)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('Integer')], [Symbol.for('define'), Symbol.for('x'), 1]]], Symbol.for(':to'), 'typescript'], 'let x: number = 1;']);
  });
  it('(compile \'(begin (: x Natural) (define x 1)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('Natural')], [Symbol.for('define'), Symbol.for('x'), 1]]], Symbol.for(':to'), 'typescript'], 'let x: number = 1;']);
  });
  it('(compile \'(begin (: x Real) (define x 1)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('Real')], [Symbol.for('define'), Symbol.for('x'), 1]]], Symbol.for(':to'), 'typescript'], 'let x: number = 1;']);
  });
  it('(compile \'(begin (: x Symbol) (define x \'x)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('Symbol')], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'let x: Symbol = Symbol.for(\'x\');']);
  });
  it('(compile \'(begin (: x Boolean) (define x #t)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('Boolean')], [Symbol.for('define'), Symbol.for('x'), true]]], Symbol.for(':to'), 'typescript'], 'let x: boolean = true;']);
  });
  it('(compile \'(begin (: x True) (define x #t)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('True')], [Symbol.for('define'), Symbol.for('x'), true]]], Symbol.for(':to'), 'typescript'], 'let x: true = true;']);
  });
  it('(compile \'(begin (: x False) (define x #f)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('False')], [Symbol.for('define'), Symbol.for('x'), false]]], Symbol.for(':to'), 'typescript'], 'let x: false = false;']);
  });
  it('(compile \'(begin (: x (U Number String)) (define x 1)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), [Symbol.for('U'), Symbol.for('Number'), Symbol.for('String')]], [Symbol.for('define'), Symbol.for('x'), 1]]], Symbol.for(':to'), 'typescript'], 'let x: number | string = 1;']);
  });
  it('(compile \'(begin (: x (U Number String Boolean)) (define x 1)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), [Symbol.for('U'), Symbol.for('Number'), Symbol.for('String'), Symbol.for('Boolean')]], [Symbol.for('define'), Symbol.for('x'), 1]]], Symbol.for(':to'), 'typescript'], 'let x: number | string | boolean = 1;']);
  });
  it('(compile \'(begin (: x (U Number (U String Boolean))) (define x 1)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), [Symbol.for('U'), Symbol.for('Number'), [Symbol.for('U'), Symbol.for('String'), Symbol.for('Boolean')]]], [Symbol.for('define'), Symbol.for('x'), 1]]], Symbol.for(':to'), 'typescript'], 'let x: number | (string | boolean) = 1;']);
  });
  it('(compile \'(begin (: x (Listof Number)) (define x (list 1))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), [Symbol.for('Listof'), Symbol.for('Number')]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('list'), 1]]]], Symbol.for(':to'), 'typescript'], 'let x: number[] = [1];']);
  });
  it('(compile \'(begin (: x (Pairof Number)) (define x \'(1 . 2))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), [Symbol.for('Pairof'), Symbol.for('Number')]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]]]], Symbol.for(':to'), 'typescript'], 'let x: (number | Symbol)[] = [1, Symbol.for(\'.\'), 2];']);
  });
  it('(compile \'(begin (: hello-world (-> Void)) (define (hello-world) (display "Hello world!"))) :to "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('hello-world'), [Symbol.for('->'), Symbol.for('Void')]], [Symbol.for('define'), [Symbol.for('hello-world')], [Symbol.for('display'), 'Hello world!']]]], Symbol.for(':to'), 'javascript'], 'function helloWorld() {\n' +
      '  console.log(\'Hello world!\');\n' +
      '}']);
  });
  it('(compile \'(begin (: hello-world (-> Void)) (define (hello-world) (display "Hello world!"))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('hello-world'), [Symbol.for('->'), Symbol.for('Void')]], [Symbol.for('define'), [Symbol.for('hello-world')], [Symbol.for('display'), 'Hello world!']]]], Symbol.for(':to'), 'typescript'], 'function helloWorld(): void {\n' +
      '  console.log(\'Hello world!\');\n' +
      '}']);
  });
  it('(compile \'(begin (: f (-> Number Number)) (define (f x) x)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('f'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('x')], Symbol.for('x')]]], Symbol.for(':to'), 'typescript'], 'function f(x: number): number {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(begin (: f (-> Number Number)) (define f (lambda (x) x))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('f'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'let f: (a: number) => number = function (x: any): any {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(begin (: f (-> Number Number)) (define f (foo (lambda (x) x)))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('f'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('foo'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]]], Symbol.for(':to'), 'typescript'], 'let f: (a: number) => number = foo(function (x: any): any {\n' +
      '  return x;\n' +
      '});']);
  });
  it('(compile \'(begin (: f (-> Number Number Number)) (define (f x (y 1)) x)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('f'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number'), Symbol.for('Number')]], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('x'), [Symbol.for('y'), 1]], Symbol.for('x')]]], Symbol.for(':to'), 'typescript'], 'function f(x: number, y: number = 1): number {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(begin (: f (->* (Number) (Number) Number)) (define f (lambda (x (y 1)) x))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('f'), [Symbol.for('->*'), [Symbol.for('Number')], [Symbol.for('Number')], Symbol.for('Number')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('x'), [Symbol.for('y'), 1]], Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'let f: (a: number, b?: number) => number = function (x: any, y: any = 1): any {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(begin (: f (-> Any * Any)) (define f (lambda x x))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('f'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'let f: (...a: any) => any = function (...x: any[]): any {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(begin (: f (-> :rest Any Any)) (define f (lambda x x))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('f'), [Symbol.for('->'), Symbol.for(':rest'), Symbol.for('Any'), Symbol.for('Any')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'let f: (...a: any) => any = function (...x: any[]): any {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(begin (: f (->* :rest Any Any)) (define f (lambda x x))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('f'), [Symbol.for('->*'), Symbol.for(':rest'), Symbol.for('Any'), Symbol.for('Any')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'let f: (...a: any) => any = function (...x: any[]): any {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(begin (: f (->* :rest (Listof Any) Any)) (define f (lambda x x))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('f'), [Symbol.for('->*'), Symbol.for(':rest'), [Symbol.for('Listof'), Symbol.for('Any')], Symbol.for('Any')]], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'let f: (...a: any[]) => any = function (...x: any[]): any {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(begin (: x Foo) (define x (new Foo))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for(':'), Symbol.for('x'), Symbol.for('Foo')], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('new'), Symbol.for('Foo')]]]], Symbol.for(':to'), 'typescript'], 'let x: Foo = new Foo();']);
  });
  it('(compile \'(define f (lambda ((x : Number)) x)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), [[Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')]], Symbol.for('x')]]], Symbol.for(':to'), 'typescript'], 'let f: any = function (x: number): any {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(define f (js/arrow ((x : Number)) x)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('f'), [Symbol.for('js/arrow'), [[Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')]], Symbol.for('x')]]], Symbol.for(':to'), 'typescript'], 'let f: any = (x: number): any => {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(define (f (x : Number)) x) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('f'), [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')]], Symbol.for('x')]], Symbol.for(':to'), 'typescript'], 'function f(x: number): any {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define (f (x : Number) . args) x) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('f'), [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')], Symbol.for('.'), Symbol.for('args')], Symbol.for('x')]], Symbol.for(':to'), 'typescript'], 'function f(x: number, ...args: any[]): any {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define (id (x : Number)) : Number x) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('id'), [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')]], Symbol.for(':'), Symbol.for('Number'), Symbol.for('x')]], Symbol.for(':to'), 'typescript'], 'function id(x: number): number {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define (f (x : Number 1)) : Number x) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('f'), [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number'), 1]], Symbol.for(':'), Symbol.for('Number'), Symbol.for('x')]], Symbol.for(':to'), 'typescript'], 'function f(x: number = 1): number {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define (f (options : Any (js/obj))) : Any x) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('f'), [Symbol.for('options'), Symbol.for(':'), Symbol.for('Any'), [Symbol.for('js/obj')]]], Symbol.for(':'), Symbol.for('Any'), Symbol.for('x')]], Symbol.for(':to'), 'typescript'], 'function f(options: any = {}): any {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define Foo (class object% (define/public x) (define (constructor (x : Number)) (set-field! x this x)))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('Foo'), [Symbol.for('class'), Symbol.for('object%'), [Symbol.for('define/public'), Symbol.for('x')], [Symbol.for('define'), [Symbol.for('constructor'), [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')]], [Symbol.for('set-field!'), Symbol.for('x'), Symbol.for('this'), Symbol.for('x')]]]]], Symbol.for(':to'), 'typescript'], 'class Foo {\n' +
      '  x: any;\n' +
      '\n' +
      '  constructor(x: number) {\n' +
      '    this.x = x;\n' +
      '  }\n' +
      '}']);
  });
  return it('(compile \'(define Foo (class object% (define/public x) (define (constructor (x : Number) . args) (set-field! x this x)))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('Foo'), [Symbol.for('class'), Symbol.for('object%'), [Symbol.for('define/public'), Symbol.for('x')], [Symbol.for('define'), [Symbol.for('constructor'), [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')], Symbol.for('.'), Symbol.for('args')], [Symbol.for('set-field!'), Symbol.for('x'), Symbol.for('this'), Symbol.for('x')]]]]], Symbol.for(':to'), 'typescript'], 'class Foo {\n' +
      '  x: any;\n' +
      '\n' +
      '  constructor(x: number, ...args: any[]) {\n' +
      '    this.x = x;\n' +
      '  }\n' +
      '}']);
  });
});

describe('define-type', function (): any {
  it('(compile \'(define-type NN (-> Number Number)) :to "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-type'), Symbol.for('NN'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]]], Symbol.for(':to'), 'javascript'], '']);
  });
  it('(compile \'(define-type NN (-> Number Number)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-type'), Symbol.for('NN'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]]], Symbol.for(':to'), 'typescript'], 'type NN = (a: number) => number;']);
  });
  it('(compile \'(begin (define-type NN (-> Number Number)) (: f NN) (define f (lambda (x) x))) :to "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-type'), Symbol.for('NN'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]], [Symbol.for(':'), Symbol.for('f'), Symbol.for('NN')], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], Symbol.for(':to'), 'javascript'], 'let f = function (x) {\n' +
      '  return x;\n' +
      '};']);
  });
  return it('(compile \'(begin (define-type NN (-> Number Number)) (: f NN) (define f (lambda (x) x))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-type'), Symbol.for('NN'), [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')]], [Symbol.for(':'), Symbol.for('f'), Symbol.for('NN')], [Symbol.for('define'), Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], Symbol.for(':to'), 'typescript'], 'type NN = (a: number) => number;\n' +
      '\n' +
      'let f: NN = function (x: any): any {\n' +
      '  return x;\n' +
      '};']);
  });
});

describe('require', function (): any {
  it('(compile \'(require "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), 'foo']]], 'import * as foo from \'foo\';']);
  });
  it('(compile \'(require "foo-bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), 'foo-bar']]], 'import * as fooBar from \'foo-bar\';']);
  });
  it('(compile \'(require foo "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), Symbol.for('foo'), 'bar']]], 'import * as foo from \'bar\';']);
  });
  it('(compile \'(require (only-in foo bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), [Symbol.for('only-in'), Symbol.for('foo'), Symbol.for('bar')]]]], 'import {\n' +
      '  bar\n' +
      '} from \'foo\';']);
  });
  it('(compile \'(require (only-in foo (bar baz))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), [Symbol.for('only-in'), Symbol.for('foo'), [Symbol.for('bar'), Symbol.for('baz')]]]]], 'import {\n' +
      '  bar as baz\n' +
      '} from \'foo\';']);
  });
  it('(compile \'(require (only-in "foo" (bar baz))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), [Symbol.for('only-in'), 'foo', [Symbol.for('bar'), Symbol.for('baz')]]]]], 'import {\n' +
      '  bar as baz\n' +
      '} from \'foo\';']);
  });
  it('(compile \'(require (only-in foo bar bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), [Symbol.for('only-in'), Symbol.for('foo'), Symbol.for('bar'), Symbol.for('bar')]]]], 'import {\n' +
      '  bar\n' +
      '} from \'foo\';']);
  });
  it('(compile \'(require (only-in foo bar (baz bar))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), [Symbol.for('only-in'), Symbol.for('foo'), Symbol.for('bar'), [Symbol.for('baz'), Symbol.for('bar')]]]]], 'import {\n' +
      '  bar\n' +
      '} from \'foo\';']);
  });
  it('(compile \'(require "foo") :fes-module-interop #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), 'foo']], Symbol.for(':fes-module-interop'), true], 'import foo from \'foo\';']);
  });
  it('(compile \'(require "foo-bar") :fes-module-interop #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), 'foo-bar']], Symbol.for(':fes-module-interop'), true], 'import fooBar from \'foo-bar\';']);
  });
  it('(compile \'(require foo "bar") :fes-module-interop #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), Symbol.for('foo'), 'bar']], Symbol.for(':fes-module-interop'), true], 'import foo from \'bar\';']);
  });
  it('(compile \'(require "foo" "bar") :fes-module-interop #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), 'foo', 'bar']], Symbol.for(':fes-module-interop'), true], 'import foo from \'bar\';']);
  });
  it('(compile \'(require "foo") :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), 'foo']], Symbol.for(':fcommonjs'), true], 'let foo = require(\'foo\');']);
  });
  it('(compile \'(require foo "bar") :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), Symbol.for('foo'), 'bar']], Symbol.for(':fcommonjs'), true], 'let foo = require(\'bar\');']);
  });
  it('(compile \'(require "foo" "bar") :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), 'foo', 'bar']], Symbol.for(':fcommonjs'), true], 'let foo = require(\'bar\');']);
  });
  it('(compile \'(require (only-in "foo" bar)) :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), [Symbol.for('only-in'), 'foo', Symbol.for('bar')]]], Symbol.for(':fcommonjs'), true], 'let {bar} = require(\'foo\');']);
  });
  return it('(compile \'(require (only-in "foo" (bar baz))) :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('require'), [Symbol.for('only-in'), 'foo', [Symbol.for('bar'), Symbol.for('baz')]]]], Symbol.for(':fcommonjs'), true], 'let {bar: baz} = require(\'foo\');']);
  });
});

describe('provide', function (): any {
  it('(compile \'(provide))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide')]]], '']);
  });
  it('(compile \'(provide x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x')]]], 'export {\n' +
      '  x\n' +
      '};']);
  });
  it('(compile \'(provide x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x'), Symbol.for('y')]]], 'export {\n' +
      '  x,\n' +
      '  y\n' +
      '};']);
  });
  it('(compile \'(provide (rename-out (x y))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), [Symbol.for('rename-out'), [Symbol.for('x'), Symbol.for('y')]]]]], 'export {\n' +
      '  x as y\n' +
      '};']);
  });
  it('(compile \'(provide (rename-out (x y) (w z))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), [Symbol.for('rename-out'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('w'), Symbol.for('z')]]]]], 'export {\n' +
      '  x as y,\n' +
      '  w as z\n' +
      '};']);
  });
  it('(compile \'(provide x (rename-out (y z))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x'), [Symbol.for('rename-out'), [Symbol.for('y'), Symbol.for('z')]]]]], 'export {\n' +
      '  x,\n' +
      '  y as z\n' +
      '};']);
  });
  it('(compile \'(provide x x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x'), Symbol.for('x')]]], 'export {\n' +
      '  x\n' +
      '};']);
  });
  it('(compile \'(provide x (rename-out (y x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x'), [Symbol.for('rename-out'), [Symbol.for('y'), Symbol.for('x')]]]]], 'export {\n' +
      '  x\n' +
      '};']);
  });
  it('(compile \'(provide (rename-out (x js/undefined))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), [Symbol.for('rename-out'), [Symbol.for('x'), Symbol.for('js/undefined')]]]]], 'export {\n' +
      '  x as jsUndefined\n' +
      '};']);
  });
  it('(compile \'(provide (all-from-out "foo")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), [Symbol.for('all-from-out'), 'foo']]]], 'export * from \'foo\';']);
  });
  it('(compile \'(provide (all-from-out "foo") bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), [Symbol.for('all-from-out'), 'foo'], Symbol.for('bar')]]], 'export * from \'foo\';\n' +
      '\n' +
      'export {\n' +
      '  bar\n' +
      '};']);
  });
  it('(compile \'(provide x) :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x')]], Symbol.for(':fcommonjs'), true], 'module.exports = {\n' +
      '  x\n' +
      '};']);
  });
  it('(compile \'(provide x y) :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('x'), Symbol.for('y')]], Symbol.for(':fcommonjs'), true], 'module.exports = {\n' +
      '  x,\n' +
      '  y\n' +
      '};']);
  });
  it('(compile \'(provide foo-bar) :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), Symbol.for('foo-bar')]], Symbol.for(':fcommonjs'), true], 'module.exports = {\n' +
      '  fooBar\n' +
      '};']);
  });
  it('(compile \'(provide (rename-out (x y))) :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), [Symbol.for('rename-out'), [Symbol.for('x'), Symbol.for('y')]]]], Symbol.for(':fcommonjs'), true], 'module.exports = {\n' +
      '  x: y\n' +
      '};']);
  });
  return it('(compile \'(provide (all-from-out "foo-bar") baz) :fcommonjs #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('provide'), [Symbol.for('all-from-out'), 'foo-bar'], Symbol.for('baz')]], Symbol.for(':fcommonjs'), true], 'module.exports = {\n' +
      '  ...fooBar,\n' +
      '  baz\n' +
      '};']);
  });
});

describe('compile', function (): any {
  it('(compile #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true], 'true;']);
  });
  it('(compile #t :to "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true, Symbol.for(':to'), 'javascript'], 'true;']);
  });
  it('(compile #t :from \'roselisp :to "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true, Symbol.for(':from'), [Symbol.for('quote'), Symbol.for('roselisp')], Symbol.for(':to'), 'javascript'], 'true;']);
  });
  it('(compile \'(ann #t Any) :from "roselisp" :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]], Symbol.for(':from'), 'roselisp', Symbol.for(':to'), 'typescript'], 'true as any;']);
  });
  it('(compile "true" :to "roselisp")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'true', Symbol.for(':to'), 'roselisp'], true]);
  });
  it('(compile "true" :from "javascript" :to "roselisp")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'true', Symbol.for(':from'), 'javascript', Symbol.for(':to'), 'roselisp'], true]);
  });
  return it('(compile "true as any" :from "typescript" :to "roselisp")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'true as any', Symbol.for(':from'), 'typescript', Symbol.for(':to'), 'roselisp'], [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]]]);
  });
});

describe('decompile', function (): any {
  it('(decompile "true")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true'], true]);
  });
  it('(decompile "true" :from "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true', Symbol.for(':from'), 'javascript'], true]);
  });
  it('(decompile "true" :from "javascript" :to "roselisp")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true', Symbol.for(':from'), 'javascript', Symbol.for(':to'), 'roselisp'], true]);
  });
  it('(decompile "true as any" :from "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true as any', Symbol.for(':from'), 'typescript'], [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]]]);
  });
  return it('(decompile "true as any" :from "typescript" :to "roselisp")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('decompile'), 'true as any', Symbol.for(':from'), 'typescript', Symbol.for(':to'), 'roselisp'], [Symbol.for('quote'), [Symbol.for('ann'), true, Symbol.for('Any')]]]);
  });
});

describe('license', function (): any {
  return it('license', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('license'), [Symbol.for('quote'), Symbol.for('MPL-2.0')]]);
  });
});