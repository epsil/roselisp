/**
 * # Scheme procedures
 *
 * Scheme procedures and constructs from Scheme and various
 * Scheme implementations.
 */

import {
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('#t', function (): any {
  it('#t', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), true, true]);
  });
  it('\'#t', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), true], true]);
  });
  it('(compile #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true], 'true;']);
  });
  it('(compile #t :as "statement")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true, Symbol.for(':as'), 'statement'], 'true;']);
  });
  it('(compile #t :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true, Symbol.for(':as'), 'expression'], 'true']);
  });
  return it('(compile #t :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), true, Symbol.for(':as'), 'return'], 'return true;']);
  });
});

describe('#f', function (): any {
  it('#f', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), false, false]);
  });
  it('\'#f', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), false], false]);
  });
  it('(compile #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), false], 'false;']);
  });
  it('(compile #f :as "statement")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), false, Symbol.for(':as'), 'statement'], 'false;']);
  });
  it('(compile #f :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), false, Symbol.for(':as'), 'expression'], 'false']);
  });
  return it('(compile #f :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), false, Symbol.for(':as'), 'return'], 'return false;']);
  });
});

describe('null', function (): any {
  it('null', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), Symbol.for('null'), [Symbol.for('quote'), []]]);
  });
  it('(listp null)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('listp'), Symbol.for('null')], true]);
  });
  it('(length null)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('length'), Symbol.for('null')], 0]);
  });
  return it('(compile \'null)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('null')]], '[];']);
  });
});

describe('Numbers', function (): any {
  it('0', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), 0, 0]);
  });
  it('1', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), 1, 1]);
  });
  it('2', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), 2, 2]);
  });
  it('(compile 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 0], '0;']);
  });
  it('(compile 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 1], '1;']);
  });
  return it('(compile 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 2], '2;']);
  });
});

describe('Strings', function (): any {
  it('""', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), '', '']);
  });
  it('"foo"', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), 'foo', 'foo']);
  });
  it('"\\"foo\\""', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), '"foo"', '"foo"']);
  });
  it('(eq? "	" "	")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('eq?'), '	', '	'], true]);
  });
  it('(compile "")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), ''], '\'\';']);
  });
  it('(compile "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'foo'], '\'foo\';']);
  });
  it('(compile "don\'t")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'don\'t'], '\'don\\\'t\';']);
  });
  it('(compile "newline\n' +
    'test")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'newline\n' +
      'test'], '\'newline\\n\' +\n' +
      '  \'test\';']);
  });
  it('(compile "newline\n' +
    'test")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'newline\n' +
      'test'], '\'newline\\n\' +\n' +
      '  \'test\';']);
  });
  it('(compile "newline\n' +
    'test\n' +
    'three")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), 'newline\n' +
      'test\n' +
      'three'], '\'newline\\n\' +\n' +
      '  \'test\\n\' +\n' +
      '  \'three\';']);
  });
  return it('(compile "\\\\s")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), '\\s'], '\'\\\\s\';']);
  });
});

describe('Symbols', function (): any {
  it('\'foo', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), Symbol.for('foo')], [Symbol.for('quote'), Symbol.for('foo')]]);
  });
  it('(compile \'foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo')]], 'foo;']);
  });
  it('(compile \'foo-bar)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo-bar')]], 'fooBar;']);
  });
  it('(compile \'\'foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), Symbol.for('foo')]]], 'Symbol.for(\'foo\');']);
  });
  it('(compile \'\'foo-bar)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), Symbol.for('foo-bar')]]], 'Symbol.for(\'foo-bar\');']);
  });
  it('(compile \'js/undefined)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('js/undefined')]], 'undefined;']);
  });
  it('(compile \'js/null)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('js/null')]], 'null;']);
  });
  it('(compile \'foo-bar :case "none")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo-bar')], Symbol.for(':case'), 'none'], 'foo-bar;']);
  });
  it('(compile \'foo-bar)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo-bar')]], 'fooBar;']);
  });
  it('(compile \'foo/bar)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo/bar')]], 'fooBar;']);
  });
  it('(compile \'foo!)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo!')]], 'foox;']);
  });
  it('(compile \'foo-bar!)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo-bar!')]], 'fooBarX;']);
  });
  it('(compile \'foo?)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo?')]], 'foop;']);
  });
  it('(compile \'foo-bar?)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('foo-bar?')]], 'fooBarP;']);
  });
  it('(compile \'*foo-bar*)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('*foo-bar*')]], 'starFooBarStar;']);
  });
  it('(compile \'\'*foo-bar*)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), Symbol.for('*foo-bar*')]]], 'Symbol.for(\'*foo-bar*\');']);
  });
  it('(compile \'A)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('A')]], 'A;']);
  });
  return it('(compile \'(module m scheme (define lst (map symbol? \'(a b c)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('lst'), [Symbol.for('map'), Symbol.for('symbol?'), [Symbol.for('quote'), [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')]]]]]]], 'let lst = [Symbol.for(\'a\'), Symbol.for(\'b\'), Symbol.for(\'c\')].map(function (x) {\n' +
      '  return typeof x === \'symbol\';\n' +
      '});']);
  });
});

describe('symbol?', function (): any {
  it('(symbol? \'foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('symbol?'), [Symbol.for('quote'), Symbol.for('foo')]], true]);
  });
  it('(symbol? 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('symbol?'), 1], false]);
  });
  it('(symbol? "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('symbol?'), 'foo'], false]);
  });
  it('(symbol? (js/obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('symbol?'), [Symbol.for('js/obj')]], false]);
  });
  return it('(symbol? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('symbol?'), [Symbol.for('quote'), []]], false]);
  });
});

describe('symbol->string', function (): any {
  return it('(symbol->string \'foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('symbol->string'), [Symbol.for('quote'), Symbol.for('foo')]], 'foo']);
  });
});

describe('Cons cells', function (): any {
  it('(cons 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons'), 1, 2], [Symbol.for('quote'), [1, Symbol.for('.'), 2]]]);
  });
  it('(cons 1 (cons 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons'), 1, [Symbol.for('cons'), 2, 3]], [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]]);
  });
  it('(cons 1 \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons'), 1, [Symbol.for('quote'), []]], [Symbol.for('quote'), [1]]]);
  });
  it('(cons 1 \'(2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons'), 1, [Symbol.for('quote'), [2]]], [Symbol.for('quote'), [1, 2]]]);
  });
  it('(car \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('car'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], 1]);
  });
  it('(cdr \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], 2]);
  });
  it('(car (cons 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('car'), [Symbol.for('cons'), 1, 2]], 1]);
  });
  it('(cdr (cons 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cdr'), [Symbol.for('cons'), 1, 2]], 2]);
  });
  it('(compile \'\'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]]], '[1, Symbol.for(\'.\'), []];']);
  });
  it('(compile \'\'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]]], '[1, Symbol.for(\'.\'), 2];']);
  });
  return it('(compile \'\'(1 2 . 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]]], '[1, 2, Symbol.for(\'.\'), 3];']);
  });
});

describe('Lists', function (): any {
  it('(list 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list'), 1, 2], [Symbol.for('quote'), [1, 2]]]);
  });
  it('(aget \'(1 2) 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('aget'), [Symbol.for('quote'), [1, 2]], 0], 1]);
  });
  it('(aget \'((1 2) (3 4)) 0 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('aget'), [Symbol.for('quote'), [[1, 2], [3, 4]]], 0, 1], 2]);
  });
  it('(aref \'(1 2) 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('aref'), [Symbol.for('quote'), [1, 2]], 0], 1]);
  });
  it('(aset! \'(1 2) 0 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('aset!'), [Symbol.for('quote'), [1, 2]], 0, 3], 3]);
  });
  it('(let ((lst \'(1 2))) (aset! lst 0 3) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2]]]], [Symbol.for('aset!'), Symbol.for('lst'), 0, 3], Symbol.for('lst')], [Symbol.for('quote'), [3, 2]]]);
  });
  it('(let ((lst \'(1 2)) (i 0)) (aget lst i))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2]]], [Symbol.for('i'), 0]], [Symbol.for('aget'), Symbol.for('lst'), Symbol.for('i')]], 1]);
  });
  it('(let ((lst \'(1 2)) (i 0)) (aget lst (+ i 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2]]], [Symbol.for('i'), 0]], [Symbol.for('aget'), Symbol.for('lst'), [Symbol.for('+'), Symbol.for('i'), 1]]], 2]);
  });
  it('(let ((lst \'(1 2))) (set! (aref lst 0) 3) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2]]]], [Symbol.for('set!'), [Symbol.for('aref'), Symbol.for('lst'), 0], 3], Symbol.for('lst')], [Symbol.for('quote'), [3, 2]]]);
  });
  it('(compile \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), []]], '[];']);
  });
  it('(compile \'\'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), []]]], '[];']);
  });
  it('(compile \'\'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [1]]]], '[1];']);
  });
  it('(compile \'\'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [1, 2]]]], '[1, 2];']);
  });
  it('(compile \'(aget x 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('aget'), Symbol.for('x'), 0]]], 'x[0];']);
  });
  it('(compile \'(let ((length 0)) (aget x length)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [[Symbol.for('length'), 0]], [Symbol.for('aget'), Symbol.for('x'), Symbol.for('length')]]]], 'let length = 0;\n' +
      '\n' +
      'x[length];']);
  });
  it('(compile \'(aget x \'length))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('aget'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('length')]]]], 'x[\'length\'];']);
  });
  it('(compile \'(aget x :length))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('aget'), Symbol.for('x'), Symbol.for(':length')]]], 'x[\'length\'];']);
  });
  return it('(compile \'(aget (js/?. x) 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('aget'), [Symbol.for('js/?.'), Symbol.for('x')], 0]]], 'x?.[0];']);
  });
});

describe('pair?', function (): any {
  it('(pair? 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), 0], false]);
  });
  it('(pair? \'x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), Symbol.for('x')]], false]);
  });
  it('(pair? "x")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), 'x'], false]);
  });
  it('(pair? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), []]], false]);
  });
  it('(pair? \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), [1]]], true]);
  });
  it('(pair? \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), [1, 2]]], true]);
  });
  it('(pair? \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), [1, 2, 3]]], true]);
  });
  it('(pair? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]);
  });
  it('(pair? \'(1 2 . 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]], true]);
  });
  it('(pair? \'(()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), [[]]]], true]);
  });
  it('(pair? \'(.))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), [Symbol.for('.')]]], true]);
  });
  it('(pair? \'(. 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), [Symbol.for('.'), 1]]], true]);
  });
  return it('(pair? \'(. 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('pair?'), [Symbol.for('quote'), [Symbol.for('.'), 1, 2]]], true]);
  });
});

describe('cons', function (): any {
  it('(cons 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons'), 1, 2], [Symbol.for('quote'), [1, Symbol.for('.'), 2]]]);
  });
  it('(cons 1 (cons 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons'), 1, [Symbol.for('cons'), 2, 3]], [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]]);
  });
  it('(cons 1 \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons'), 1, [Symbol.for('quote'), []]], [Symbol.for('quote'), [1]]]);
  });
  it('(cons 1 \'(2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons'), 1, [Symbol.for('quote'), [2]]], [Symbol.for('quote'), [1, 2]]]);
  });
  it('(compile \'(cons 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), 1, 2]]], '[1, Symbol.for(\'.\'), 2];']);
  });
  it('(compile \'(cons "1" "2"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), '1', '2']]], '[\'1\', Symbol.for(\'.\'), \'2\'];']);
  });
  it('(compile \'(cons x (list y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), Symbol.for('x'), [Symbol.for('list'), Symbol.for('y')]]]], '[x, y];']);
  });
  it('(compile \'(cons x \'(y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), Symbol.for('x'), [Symbol.for('quote'), [Symbol.for('y')]]]]], '[x, Symbol.for(\'y\')];']);
  });
  it('(compile \'(cons x `(y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), Symbol.for('x'), [Symbol.for('quasiquote'), [Symbol.for('y')]]]]], '[x, Symbol.for(\'y\')];']);
  });
  it('(compile \'(cons x `(,y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), Symbol.for('x'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('y')]]]]]], '[x, y];']);
  });
  it('(compile \'(cons x `(,@y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), Symbol.for('x'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('y')]]]]]], '[x, ...y];']);
  });
  it('(compile \'(cons x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), Symbol.for('x'), Symbol.for('y')]]], '[x, ...(Array.isArray(y) ? y : [Symbol.for(\'.\'), y])];']);
  });
  it('(compile \'(cons (x) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), [Symbol.for('x')], Symbol.for('y')]]], '[x(), ...(Array.isArray(y) ? y : [Symbol.for(\'.\'), y])];']);
  });
  it('(compile \'(cons x (y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), Symbol.for('x'), [Symbol.for('y')]]]], '[x, ...((x) => {\n' +
      '  return Array.isArray(x) ? x : [Symbol.for(\'.\'), x];\n' +
      '})(y())];']);
  });
  return it('(compile \'(cons (x) (y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cons'), [Symbol.for('x')], [Symbol.for('y')]]]], '[x(), ...((x) => {\n' +
      '  return Array.isArray(x) ? x : [Symbol.for(\'.\'), x];\n' +
      '})(y())];']);
  });
});

describe('cons?', function (): any {
  it('(cons? 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), 0], false]);
  });
  it('(cons? \'x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), Symbol.for('x')]], false]);
  });
  it('(cons? "x")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), 'x'], false]);
  });
  it('(cons? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), []]], false]);
  });
  it('(cons? \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), [1]]], true]);
  });
  it('(cons? \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), [1, 2]]], true]);
  });
  it('(cons? \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), [1, 2, 3]]], true]);
  });
  it('(cons? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], true]);
  });
  it('(cons? \'(1 2 . 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]], true]);
  });
  it('(cons? \'(()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), [[]]]], true]);
  });
  it('(cons? \'(.))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), [Symbol.for('.')]]], true]);
  });
  it('(cons? \'(. 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), [Symbol.for('.'), 1]]], true]);
  });
  return it('(cons? \'(. 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cons?'), [Symbol.for('quote'), [Symbol.for('.'), 1, 2]]], true]);
  });
});

describe('list?', function (): any {
  it('(list? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list?'), [Symbol.for('quote'), []]], true]);
  });
  it('(list? \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list?'), [Symbol.for('quote'), [1]]], true]);
  });
  it('(list? \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list?'), [Symbol.for('quote'), [1, 2]]], true]);
  });
  it('(list? \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list?'), [Symbol.for('quote'), [1, 2, 3]]], true]);
  });
  it('(list? \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list?'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], false]);
  });
  it('(list? \'(1 2 . 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list?'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]], false]);
  });
  it('(compile \'(list? x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list?'), Symbol.for('x')]]], 'Array.isArray(x) && !((x.length >= 3) && (x.at(-2) === Symbol.for(\'.\')) && !Array.isArray(x.at(-1)));']);
  });
  it('(compile \'(module m scheme (list? x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('list?'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'Array.isArray(x) && !((x.length >= 3) && (x.at(-2) === Symbol.for(\'.\')) && !Array.isArray(x.at(-1)));']);
  });
  return it('(compile \'(module m scheme (list? x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('list?'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  listp\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'listp(x);']);
  });
});

describe('list-ref', function (): any {
  it('(compile \'(list-ref lst i))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list-ref'), Symbol.for('lst'), Symbol.for('i')]]], 'lst[i];']);
  });
  it('(compile \'(list-ref lst i j))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list-ref'), Symbol.for('lst'), Symbol.for('i'), Symbol.for('j')]]], 'lst[i][j];']);
  });
  it('(compile \'(module m scheme (list-ref lst i)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('list-ref'), Symbol.for('lst'), Symbol.for('i')]]], Symbol.for(':fdottedlists'), false], 'lst[i];']);
  });
  return it('(compile \'(module m scheme (list-ref lst i)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('list-ref'), Symbol.for('lst'), Symbol.for('i')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  listRef\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'listRef(lst, i);']);
  });
});

describe('list-set', function (): any {
  it('(list-set \'(1 2 3) 0 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list-set'), [Symbol.for('quote'), [1, 2, 3]], 0, 4], [Symbol.for('quote'), [4, 2, 3]]]);
  });
  it('(list-set \'((1) 2 3) 0 0 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list-set'), [Symbol.for('quote'), [[1], 2, 3]], 0, 0, 4], [Symbol.for('quote'), [[4], 2, 3]]]);
  });
  it('(funcall list-set \'(1 . ()) 0 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('list-set'), [Symbol.for('quote'), [1, Symbol.for('.'), []]], 0, 2], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(funcall list-set \'(1 . (2 . ())) 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('list-set'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]], 1, 3], [Symbol.for('quote'), [1, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  it('(funcall list-set \'((1 . 2) . (3 . ())) 0 0 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('list-set'), [Symbol.for('quote'), [[1, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]], 0, 0, 4], [Symbol.for('quote'), [[4, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  return it('(funcall list-set \'(1 2 . (3 . ())) 1 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('list-set'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]], 1, 4], [Symbol.for('quote'), [1, 4, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
});

describe('list-set!', function (): any {
  it('(let ((lst \'(1 2 3))) (list-set! lst 0 4) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('list-set!'), Symbol.for('lst'), 0, 4], Symbol.for('lst')], [Symbol.for('quote'), [4, 2, 3]]]);
  });
  it('(let ((lst \'((1) 2 3))) (list-set! lst 0 0 4) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [[1], 2, 3]]]], [Symbol.for('list-set!'), Symbol.for('lst'), 0, 0, 4], Symbol.for('lst')], [Symbol.for('quote'), [[4], 2, 3]]]);
  });
  it('(let ((lst \'(1 . ()))) (funcall list-set! lst 0 2) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]]], [Symbol.for('funcall'), Symbol.for('list-set!'), Symbol.for('lst'), 0, 2], Symbol.for('lst')], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(let ((lst \'(1 . (2 . ())))) (funcall list-set! lst 1 3) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]]], [Symbol.for('funcall'), Symbol.for('list-set!'), Symbol.for('lst'), 1, 3], Symbol.for('lst')], [Symbol.for('quote'), [1, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  it('(let ((lst \'((1 . 2) . (3 . ())))) (funcall list-set! lst 0 0 4) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [[1, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]]]], [Symbol.for('funcall'), Symbol.for('list-set!'), Symbol.for('lst'), 0, 0, 4], Symbol.for('lst')], [Symbol.for('quote'), [[4, Symbol.for('.'), 2], Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  it('(let ((lst \'(1 2 . (3 . ())))) (funcall list-set! lst 1 4) lst)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]]]], [Symbol.for('funcall'), Symbol.for('list-set!'), Symbol.for('lst'), 1, 4], Symbol.for('lst')], [Symbol.for('quote'), [1, 4, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  it('(compile \'(list-set! lst i x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list-set!'), Symbol.for('lst'), Symbol.for('i'), Symbol.for('x')]]], 'lst[i] = x;']);
  });
  it('(compile \'(list-set! lst i j x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list-set!'), Symbol.for('lst'), Symbol.for('i'), Symbol.for('j'), Symbol.for('x')]]], 'lst[i][j] = x;']);
  });
  it('(compile \'(module m scheme (list-set! lst i x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('list-set!'), Symbol.for('lst'), Symbol.for('i'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'lst[i] = x;']);
  });
  return it('(compile \'(module m scheme (list-set! lst i x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('list-set!'), Symbol.for('lst'), Symbol.for('i'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  listSetX\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'listSetX(lst, i, x);']);
  });
});

describe('length', function (): any {
  it('(length \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('length'), [Symbol.for('quote'), []]], 0]);
  });
  it('(length \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('length'), [Symbol.for('quote'), [1]]], 1]);
  });
  it('(length \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('length'), [Symbol.for('quote'), [1, 2]]], 2]);
  });
  it('(length \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('length'), [Symbol.for('quote'), [1, 2, 3]]], 3]);
  });
  it('(funcall length \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('length'), [Symbol.for('quote'), []]], 0]);
  });
  it('(funcall length \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('length'), [Symbol.for('quote'), [1]]], 1]);
  });
  it('(funcall length \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('length'), [Symbol.for('quote'), [1, 2]]], 2]);
  });
  it('(funcall length \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('length'), [Symbol.for('quote'), [1, 2, 3]]], 3]);
  });
  it('(funcall length \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('length'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], 1]);
  });
  it('(funcall length \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('length'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 2]);
  });
  it('(funcall length \'(1 2 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('length'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]], 2]);
  });
  it('(compile \'(length x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('length'), Symbol.for('x')]]], 'x.length;']);
  });
  it('(compile \'(module m scheme (length x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('length'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x.length;']);
  });
  return it('(compile \'(module m scheme (length x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('length'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  length\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'length(x);']);
  });
});

describe('first', function (): any {
  it('(compile \'(first x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('first'), Symbol.for('x')]]], 'x[0];']);
  });
  it('(compile \'(module m scheme (first x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('first'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[0];']);
  });
  return it('(compile \'(module m scheme (first x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('first'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  first\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'first(x);']);
  });
});

describe('second', function (): any {
  it('(compile \'(second x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('second'), Symbol.for('x')]]], 'x[1];']);
  });
  it('(compile \'(module m scheme (second x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('second'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[1];']);
  });
  return it('(compile \'(module m scheme (second x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('second'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  second\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'second(x);']);
  });
});

describe('third', function (): any {
  it('(compile \'(third x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('third'), Symbol.for('x')]]], 'x[2];']);
  });
  it('(compile \'(module m scheme (third x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('third'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[2];']);
  });
  return it('(compile \'(module m scheme (third x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('third'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  third\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'third(x);']);
  });
});

describe('fourth', function (): any {
  it('(compile \'(fourth x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('fourth'), Symbol.for('x')]]], 'x[3];']);
  });
  it('(compile \'(module m scheme (fourth x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('fourth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[3];']);
  });
  return it('(compile \'(module m scheme (fourth x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('fourth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  fourth\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'fourth(x);']);
  });
});

describe('fifth', function (): any {
  it('(compile \'(fifth x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('fifth'), Symbol.for('x')]]], 'x[4];']);
  });
  it('(compile \'(module m scheme (fifth x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('fifth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[4];']);
  });
  return it('(compile \'(module m scheme (fifth x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('fifth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  fifth\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'fifth(x);']);
  });
});

describe('sixth', function (): any {
  it('(compile \'(sixth x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('sixth'), Symbol.for('x')]]], 'x[5];']);
  });
  it('(compile \'(module m scheme (sixth x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('sixth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[5];']);
  });
  return it('(compile \'(module m scheme (sixth x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('sixth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  sixth\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'sixth(x);']);
  });
});

describe('seventh', function (): any {
  it('(compile \'(seventh x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('seventh'), Symbol.for('x')]]], 'x[6];']);
  });
  it('(compile \'(module m scheme (seventh x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('seventh'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[6];']);
  });
  return it('(compile \'(module m scheme (seventh x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('seventh'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  seventh\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'seventh(x);']);
  });
});

describe('eighth', function (): any {
  it('(compile \'(eighth x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('eighth'), Symbol.for('x')]]], 'x[7];']);
  });
  it('(compile \'(module m scheme (eighth x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('eighth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[7];']);
  });
  return it('(compile \'(module m scheme (eighth x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('eighth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  eighth\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'eighth(x);']);
  });
});

describe('ninth', function (): any {
  it('(compile \'(ninth x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('ninth'), Symbol.for('x')]]], 'x[8];']);
  });
  it('(compile \'(module m scheme (ninth x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('ninth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[8];']);
  });
  return it('(compile \'(module m scheme (ninth x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('ninth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  ninth\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'ninth(x);']);
  });
});

describe('tenth', function (): any {
  it('(compile \'(tenth x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('tenth'), Symbol.for('x')]]], 'x[9];']);
  });
  it('(compile \'(module m scheme (tenth x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('tenth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x[9];']);
  });
  return it('(compile \'(module m scheme (tenth x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('tenth'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  tenth\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'tenth(x);']);
  });
});

describe('last', function (): any {
  it('(last \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('last'), [Symbol.for('quote'), [1]]], 1]);
  });
  it('(last \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('last'), [Symbol.for('quote'), [1, 2]]], 2]);
  });
  it('(last \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('last'), [Symbol.for('quote'), [1, 2, 3]]], 3]);
  });
  it('(funcall last \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('last'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], 1]);
  });
  it('(funcall last \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('last'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], 2]);
  });
  it('(funcall last \'(1 2 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('last'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]], 2]);
  });
  it('(compile \'(last lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('last'), Symbol.for('lst')]]], 'lst.at(-1);']);
  });
  it('(compile \'(module m scheme (last lst)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('last'), Symbol.for('lst')]]], Symbol.for(':fdottedlists'), false], 'lst.at(-1);']);
  });
  return it('(compile \'(module m scheme (last lst)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('last'), Symbol.for('lst')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  last\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'last(lst);']);
  });
});

describe('list-tail', function (): any {
  it('(list-tail \'(1 2 3) 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list-tail'), [Symbol.for('quote'), [1, 2, 3]], 0], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(list-tail \'(1 2 3) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list-tail'), [Symbol.for('quote'), [1, 2, 3]], 1], [Symbol.for('quote'), [2, 3]]]);
  });
  it('(list-tail \'(1 2 3) 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list-tail'), [Symbol.for('quote'), [1, 2, 3]], 2], [Symbol.for('quote'), [3]]]);
  });
  it('(list-tail \'(1 2 3) 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list-tail'), [Symbol.for('quote'), [1, 2, 3]], 3], [Symbol.for('quote'), []]]);
  });
  it('(list-tail \'(1 . 2) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list-tail'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]], 1], 2]);
  });
  it('(compile \'(module m scheme (list-tail x n)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('list-tail'), Symbol.for('x'), Symbol.for('n')]]], Symbol.for(':fdottedlists'), false], 'import {\n' +
      '  listTail\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'listTail(x, n);']);
  });
  return it('(compile \'(module m scheme (list-tail x n)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('list-tail'), Symbol.for('x'), Symbol.for('n')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  listTail\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'listTail(x, n);']);
  });
});

describe('cdr', function (): any {
  it('(cdr \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cdr'), [Symbol.for('quote'), [1]]], [Symbol.for('quote'), []]]);
  });
  it('(cdr \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cdr'), [Symbol.for('quote'), [1, 2]]], [Symbol.for('quote'), [2]]]);
  });
  it('(cdr \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], 2]);
  });
  it('(cdr \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], [Symbol.for('quote'), []]]);
  });
  it('(cdr \'(1 2 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cdr'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(cdr \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(cdr \'(1 2 . (3 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cdr'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]]], [Symbol.for('quote'), [2, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  it('(funcall cdr \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('cdr'), [Symbol.for('quote'), [1]]], [Symbol.for('quote'), []]]);
  });
  it('(funcall cdr \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('cdr'), [Symbol.for('quote'), [1, 2]]], [Symbol.for('quote'), [2]]]);
  });
  it('(funcall cdr \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], 2]);
  });
  it('(funcall cdr \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], [Symbol.for('quote'), []]]);
  });
  it('(funcall cdr \'(1 2 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('cdr'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(funcall cdr \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('cdr'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(funcall cdr \'(1 2 . (3 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('cdr'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]]], [Symbol.for('quote'), [2, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  it('(compile \'(cdr x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cdr'), Symbol.for('x')]]], '((x.length === 3) && (x[1] === Symbol.for(\'.\'))) ? x[2] : x.slice(1);']);
  });
  it('(compile \'(module m scheme (cdr x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('cdr'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], '((x.length === 3) && (x[1] === Symbol.for(\'.\'))) ? x[2] : x.slice(1);']);
  });
  return it('(compile \'(module m scheme (cdr x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('cdr'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  cdr\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'cdr(x);']);
  });
});

describe('rest', function (): any {
  it('(rest \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('rest'), [Symbol.for('quote'), [1]]], [Symbol.for('quote'), []]]);
  });
  it('(rest \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('rest'), [Symbol.for('quote'), [1, 2]]], [Symbol.for('quote'), [2]]]);
  });
  it('(rest \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('rest'), [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('quote'), [2, 3]]]);
  });
  it('(funcall rest \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('rest'), [Symbol.for('quote'), [1]]], [Symbol.for('quote'), []]]);
  });
  it('(funcall rest \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('rest'), [Symbol.for('quote'), [1, 2]]], [Symbol.for('quote'), [2]]]);
  });
  it('(funcall rest \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('rest'), [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('quote'), [2, 3]]]);
  });
  it('(funcall rest \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('rest'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], 2]);
  });
  it('(funcall rest \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('rest'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], [Symbol.for('quote'), []]]);
  });
  it('(funcall rest \'(1 2 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('rest'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), []]]], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(funcall rest \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('rest'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], [Symbol.for('quote'), [2, Symbol.for('.'), []]]]);
  });
  it('(funcall rest \'(1 2 . (3 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('rest'), [Symbol.for('quote'), [1, 2, Symbol.for('.'), [3, Symbol.for('.'), []]]]], [Symbol.for('quote'), [2, Symbol.for('.'), [3, Symbol.for('.'), []]]]]);
  });
  it('(compile \'(rest x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('rest'), Symbol.for('x')]]], 'x.slice(1);']);
  });
  it('(compile \'(module m scheme (rest x)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('rest'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), false], 'x.slice(1);']);
  });
  return it('(compile \'(module m scheme (rest x)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('rest'), Symbol.for('x')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  rest\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'rest(x);']);
  });
});

describe('set-car!', function (): any {
  it('((lambda () (define foo \'()) (set-car! foo \'bar) foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), []]], [Symbol.for('set-car!'), Symbol.for('foo'), [Symbol.for('quote'), Symbol.for('bar')]], Symbol.for('foo')]], [Symbol.for('quote'), []]]);
  });
  return it('((lambda () (define foo \'(foo)) (set-car! foo \'bar) foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo')]]], [Symbol.for('set-car!'), Symbol.for('foo'), [Symbol.for('quote'), Symbol.for('bar')]], Symbol.for('foo')]], [Symbol.for('quote'), [Symbol.for('bar')]]]);
  });
});

describe('set-cdr!', function (): any {
  it('(let ((foo \'())) (set-cdr! foo \'(bar)) foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('foo'), [Symbol.for('quote'), []]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('bar')]]], Symbol.for('foo')], [Symbol.for('quote'), []]]);
  });
  it('(let ((foo \'(foo))) (set-cdr! foo \'(bar)) foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo')]]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('bar')]]], Symbol.for('foo')], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]]);
  });
  it('(let ((foo \'(foo bar))) (set-cdr! foo \'(baz)) foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('baz')]]], Symbol.for('foo')], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('baz')]]]);
  });
  it('(let ((foo \'(foo bar))) (set-cdr! foo \'(baz . quux)) foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar')]]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')]]], Symbol.for('foo')], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')]]]);
  });
  it('(let ((foo \'(foo . bar))) (set-cdr! foo \'(baz)) foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('baz')]]], Symbol.for('foo')], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('baz')]]]);
  });
  it('(let ((foo \'(foo . bar))) (set-cdr! foo \'(baz . quux)) foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')]]], Symbol.for('foo')], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('baz'), Symbol.for('.'), Symbol.for('quux')]]]);
  });
  it('(let ((foo \'(foo))) (set-cdr! foo \'bar) foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo')]]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), [Symbol.for('quote'), Symbol.for('bar')]], Symbol.for('foo')], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('bar')]]]);
  });
  it('(let ((foo \'(foo bar . baz))) (set-cdr! foo \'(quux)) foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('quux')]]], Symbol.for('foo')], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('quux')]]]);
  });
  return it('(let ((foo \'(foo bar . baz))) (set-cdr! foo \'quux) foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('foo'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('.'), Symbol.for('baz')]]]], [Symbol.for('set-cdr!'), Symbol.for('foo'), [Symbol.for('quote'), Symbol.for('quux')]], Symbol.for('foo')], [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('quux')]]]);
  });
});

describe('list*', function (): any {
  it('(list*)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list*')], undefined]);
  });
  it('(list* 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list*'), 1], 1]);
  });
  it('(list* 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list*'), 1, 2], [Symbol.for('quote'), [1, Symbol.for('.'), 2]]]);
  });
  it('(list* 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list*'), 1, 2, 3], [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]]);
  });
  it('(list* 1 2 3 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list*'), 1, 2, 3, 4], [Symbol.for('quote'), [1, 2, 3, Symbol.for('.'), 4]]]);
  });
  it('(list* 1 \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list*'), 1, [Symbol.for('quote'), []]], [Symbol.for('quote'), [1]]]);
  });
  it('(list* 1 \'(2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list*'), 1, [Symbol.for('quote'), [2]]], [Symbol.for('quote'), [1, 2]]]);
  });
  return it('(list* 1 \'(2 . 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('list*'), 1, [Symbol.for('quote'), [2, Symbol.for('.'), 3]]], [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]]);
  });
});

describe('flatten', function (): any {
  it('(flatten \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('flatten'), [Symbol.for('quote'), [1, 2, 3, 4]]], [Symbol.for('quote'), [1, 2, 3, 4]]]);
  });
  it('(flatten \'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('flatten'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]], [Symbol.for('quote'), [1, 2]]]);
  });
  it('(flatten \'((a) b (c (d) . e) ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('flatten'), [Symbol.for('quote'), [[Symbol.for('a')], Symbol.for('b'), [Symbol.for('c'), [Symbol.for('d')], Symbol.for('.'), Symbol.for('e')], []]]], [Symbol.for('quote'), [Symbol.for('a'), Symbol.for('b'), Symbol.for('c'), Symbol.for('d'), Symbol.for('e')]]]);
  });
  return it('(flatten \'((((4)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('flatten'), [Symbol.for('quote'), [[[[4]]]]]], [Symbol.for('quote'), [4]]]);
  });
});

describe('list', function (): any {
  it('(compile \'(list))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list')]]], '[];']);
  });
  it('(compile \'(list 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list'), 1]]], '[1];']);
  });
  it('(compile \'(list 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list'), 1, 2]]], '[1, 2];']);
  });
  return it('(compile \'(list (list 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('list'), [Symbol.for('list'), 1]]]], '[[1]];']);
  });
});

describe('append', function (): any {
  it('(compile \'(append))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('append')]]], '[];']);
  });
  it('(compile \'(append foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('append'), Symbol.for('foo')]]], '[...foo];']);
  });
  it('(compile \'(append foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('append'), Symbol.for('foo'), Symbol.for('bar')]]], '[...foo, ...bar];']);
  });
  it('(compile \'(append (list)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('append'), [Symbol.for('list')]]]], '[];']);
  });
  it('(compile \'(append (list x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('append'), [Symbol.for('list'), Symbol.for('x')]]]], '[x];']);
  });
  return it('(compile \'(append \'("foo") \'("bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('append'), [Symbol.for('quote'), ['foo']], [Symbol.for('quote'), ['bar']]]]], '[\'foo\', \'bar\'];']);
  });
});

describe('quote', function (): any {
  it('\'foo', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), Symbol.for('foo')], [Symbol.for('quote'), Symbol.for('foo')]]);
  });
  it('\'(1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), [1]], [Symbol.for('quote'), [1]]]);
  });
  it('\'(1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), [1, 2]], [Symbol.for('quote'), [1, 2]]]);
  });
  it('\'((1 2) (3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), [[1, 2], [3, 4]]], [Symbol.for('quote'), [[1, 2], [3, 4]]]]);
  });
  it('(compile \'\'foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), Symbol.for('foo')]]], 'Symbol.for(\'foo\');']);
  });
  it('(compile \'\'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), []]]], '[];']);
  });
  it('(compile \'\'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [1]]]], '[1];']);
  });
  it('(compile \'\'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [1, 2]]]], '[1, 2];']);
  });
  it('(compile \'\'((1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [[1]]]]], '[[1]];']);
  });
  it('(compile \'\'((1 2) (3 4)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [[1, 2], [3, 4]]]]], '[[1, 2], [3, 4]];']);
  });
  it('(compile \'\'(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]]]], '[1, Symbol.for(\'.\'), 2];']);
  });
  it('(compile \'\'(#t #f))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [true, false]]]], '[true, false];']);
  });
  return it('(compile \'\'(x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quote'), [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]]], '[Symbol.for(\'x\'), Symbol.for(\'y\'), Symbol.for(\'z\')];']);
  });
});

describe('quasiquote', function (): any {
  it('`foo', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quasiquote'), Symbol.for('foo')], [Symbol.for('quote'), Symbol.for('foo')]]);
  });
  it('`foo', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quasiquote'), Symbol.for('foo')], [Symbol.for('quote'), Symbol.for('foo')]]);
  });
  it('`(,1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), 1]]], [Symbol.for('quote'), [1]]]);
  });
  it('`((,1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), 1]]]], [Symbol.for('quote'), [[1]]]]);
  });
  it('`(,@(list 1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('list'), 1, 2, 3]]]], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('`(`(,,1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quasiquote'), [[Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('unquote'), 1]]]]]], [Symbol.for('quote'), [[Symbol.for('quasiquote'), [[Symbol.for('unquote'), 1]]]]]]);
  });
  it('(let ((x 1)) `(`(,,x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 1]], [Symbol.for('quasiquote'), [[Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('unquote'), Symbol.for('x')]]]]]]], [Symbol.for('quote'), [[Symbol.for('quasiquote'), [[Symbol.for('unquote'), 1]]]]]]);
  });
  it('`(1 `,(+ 1 ,(+ 2 3)) 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quasiquote'), [1, [Symbol.for('quasiquote'), [Symbol.for('unquote'), [Symbol.for('+'), 1, [Symbol.for('unquote'), [Symbol.for('+'), 2, 3]]]]], 4]], [Symbol.for('quote'), [1, [Symbol.for('quasiquote'), [Symbol.for('unquote'), [Symbol.for('+'), 1, 5]]], 4]]]);
  });
  it('`(1 ```,,@,,@(list (+ 1 2)) 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quasiquote'), [1, [Symbol.for('quasiquote'), [Symbol.for('quasiquote'), [Symbol.for('quasiquote'), [Symbol.for('unquote'), [Symbol.for('unquote-splicing'), [Symbol.for('unquote'), [Symbol.for('unquote-splicing'), [Symbol.for('list'), [Symbol.for('+'), 1, 2]]]]]]]]], 4]], [Symbol.for('quote'), [1, [Symbol.for('quasiquote'), [Symbol.for('quasiquote'), [Symbol.for('quasiquote'), [Symbol.for('unquote'), [Symbol.for('unquote-splicing'), [Symbol.for('unquote'), 3]]]]]], 4]]]);
  });
  it('``(,,@(list 1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quasiquote'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('unquote-splicing'), [Symbol.for('list'), 1, 2, 3]]]]]], [Symbol.for('quote'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), 1], [Symbol.for('unquote'), 2], [Symbol.for('unquote'), 3]]]]]);
  });
  it('(let ((lst \'(foo bar baz))) ``(,,@lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('lst'), [Symbol.for('quote'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]]], [Symbol.for('quasiquote'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('unquote-splicing'), Symbol.for('lst')]]]]]], [Symbol.for('quote'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]]]]);
  });
  it('(compile \'`foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), Symbol.for('foo')]]], 'Symbol.for(\'foo\');']);
  });
  it('(compile \'`())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), []]]], '[];']);
  });
  it('(compile \'`(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [1]]]], '[1];']);
  });
  it('(compile \'`((1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [[1]]]]], '[[1]];']);
  });
  it('(compile \'`(1 ,2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [1, [Symbol.for('unquote'), 2]]]]], '[1, 2];']);
  });
  it('(compile \'`(1 ,2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [1, [Symbol.for('unquote'), 2, 3]]]]], '[1, 2, 3];']);
  });
  it('(compile \'`(1 . 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [1, Symbol.for('.'), 2]]]], '[1, Symbol.for(\'.\'), 2];']);
  });
  it('(compile \'`((1 . 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [[1, Symbol.for('.'), 2]]]]], '[[1, Symbol.for(\'.\'), 2]];']);
  });
  it('(compile \'`(x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]]], '[Symbol.for(\'x\'), Symbol.for(\'y\'), Symbol.for(\'z\')];']);
  });
  it('(compile \'`(,1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), 1]]]]], '[1];']);
  });
  it('(compile \'`((,1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), 1]]]]]], '[[1]];']);
  });
  it('(compile \'`(1 ,@(list 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [1, [Symbol.for('unquote-splicing'), [Symbol.for('list'), 2]]]]]], '[1, 2];']);
  });
  it('(compile \'`(,@(list 1 2 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('list'), 1, 2, 3]]]]]], '[1, 2, 3];']);
  });
  it('(compile \'`((1 . ,2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [[1, Symbol.for('.'), [Symbol.for('unquote'), 2]]]]]], '[[1, Symbol.for(\'.\'), 2]];']);
  });
  it('(compile \'`((1 . ,2) (3 . ,4)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [[1, Symbol.for('.'), [Symbol.for('unquote'), 2]], [3, Symbol.for('.'), [Symbol.for('unquote'), 4]]]]]], '[[1, Symbol.for(\'.\'), 2], [3, Symbol.for(\'.\'), 4]];']);
  });
  it('(compile \'`(1 ,@(list 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [1, [Symbol.for('unquote-splicing'), [Symbol.for('list'), 2], [Symbol.for('list'), 3]]]]]], '[1, 2, 3];']);
  });
  it('(compile \'`(,@x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('x')]]]]], '[...x];']);
  });
  it('(compile \'`(,@x ,@y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('x')], [Symbol.for('unquote-splicing'), Symbol.for('y')]]]]], '[...x, ...y];']);
  });
  it('(compile \'``(1 ,,@(list 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [Symbol.for('quasiquote'), [1, [Symbol.for('unquote'), [Symbol.for('unquote-splicing'), [Symbol.for('list'), 2]]]]]]]], '[Symbol.for(\'quasiquote\'), [1, [Symbol.for(\'unquote\'), 2]]];']);
  });
  it('(compile \'``(1 ,,@(list 2 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [Symbol.for('quasiquote'), [1, [Symbol.for('unquote'), [Symbol.for('unquote-splicing'), [Symbol.for('list'), 2, 3]]]]]]]], '[Symbol.for(\'quasiquote\'), [1, [Symbol.for(\'unquote\'), 2], [Symbol.for(\'unquote\'), 3]]];']);
  });
  it('(compile \'`(x y ,z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [Symbol.for('x'), Symbol.for('y'), [Symbol.for('unquote'), Symbol.for('z')]]]]], '[Symbol.for(\'x\'), Symbol.for(\'y\'), z];']);
  });
  it('(compile \'`(x y ,@z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [Symbol.for('x'), Symbol.for('y'), [Symbol.for('unquote-splicing'), Symbol.for('z')]]]]], '[Symbol.for(\'x\'), Symbol.for(\'y\'), ...z];']);
  });
  it('(compile \'`(x y `z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), Symbol.for('z')]]]]], '[Symbol.for(\'x\'), Symbol.for(\'y\'), [Symbol.for(\'quasiquote\'), Symbol.for(\'z\')]];']);
  });
  it('(compile \'`(x y `(z)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [Symbol.for('z')]]]]]], '[Symbol.for(\'x\'), Symbol.for(\'y\'), [Symbol.for(\'quasiquote\'), [Symbol.for(\'z\')]]];']);
  });
  it('(compile \'`(x y `(,z)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('z')]]]]]]], '[Symbol.for(\'x\'), Symbol.for(\'y\'), [Symbol.for(\'quasiquote\'), [[Symbol.for(\'unquote\'), Symbol.for(\'z\')]]]];']);
  });
  it('(compile \'`(x y `(,@z)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('quasiquote'), [Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('z')]]]]]]], '[Symbol.for(\'x\'), Symbol.for(\'y\'), [Symbol.for(\'quasiquote\'), [[Symbol.for(\'unquote-splicing\'), Symbol.for(\'z\')]]]];']);
  });
  it('(compile \'(define test-map-1 `(("foo" . ,test-fn) ("bar" . ,test-fn))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('test-map-1'), [Symbol.for('quasiquote'), [['foo', Symbol.for('.'), [Symbol.for('unquote'), Symbol.for('test-fn')]], ['bar', Symbol.for('.'), [Symbol.for('unquote'), Symbol.for('test-fn')]]]]]]], 'let testMap1 = [[\'foo\', Symbol.for(\'.\'), testFn], [\'bar\', Symbol.for(\'.\'), testFn]];']);
  });
  return it('(compile \'(set! let-exp `(let ((,arg-list \',args)) ,@body)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!'), Symbol.for('let-exp'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('arg-list')], [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('args')]]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]]], 'letExp = [Symbol.for(\'let\'), [[argList, [Symbol.for(\'quote\'), args]]], ...body];']);
  });
});

describe('Variables', function (): any {
  it('(let ((x 2)) x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 2]], Symbol.for('x')], 2]);
  });
  it('(let ((x 2) y) y)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 2], Symbol.for('y')], Symbol.for('y')], undefined]);
  });
  it('(let (x) (set! x 2) x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('x')], [Symbol.for('set!'), Symbol.for('x'), 2], Symbol.for('x')], 2]);
  });
  it('((lambda () (define x 2) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('x'), 2], Symbol.for('x')]], 2]);
  });
  it('((lambda () (define x) (set! x 2) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('x')], [Symbol.for('set!'), Symbol.for('x'), 2], Symbol.for('x')]], 2]);
  });
  return it('(let (x y) (set! x 2) (set! y 3) (+ x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('set!'), Symbol.for('x'), 2], [Symbol.for('set!'), Symbol.for('y'), 3], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], 5]);
  });
});

describe('Function calls', function (): any {
  it('(let ((identity (lambda (x) x))) (identity "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('identity'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]], [Symbol.for('identity'), 'foo']], 'foo']);
  });
  it('(let ((my-add (lambda (x y) (+ x y)))) (my-add 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('my-add'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]], [Symbol.for('my-add'), 1, 2]], 3]);
  });
  return it('(let ((my-add (lambda (x y z) (+ x y z)))) (my-add 1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('my-add'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]]], [Symbol.for('my-add'), 1, 2, 3]], 6]);
  });
});

describe('define', function (): any {
  it('((lambda () (define x 1) 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('x'), 1], 1]], 1]);
  });
  it('((lambda () (define (foo . args) args) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')], [Symbol.for('foo')]]], [Symbol.for('quote'), []]]);
  });
  it('((lambda () (define (my-add x y) (+ x y)) (my-add 2 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), [Symbol.for('my-add'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('my-add'), 2, 3]]], 5]);
  });
  it('(let ((my-add (lambda (x y) (+ x y)))) ((lambda () (define (my-add-2 x y) (my-add x y)) (my-add-2 2 3))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('my-add'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]], [[Symbol.for('lambda'), [], [Symbol.for('define'), [Symbol.for('my-add-2'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('my-add'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('my-add-2'), 2, 3]]]], 5]);
  });
  it('(let ((my-add (lambda (x y z) (+ x y z)))) ((lambda () (define (my-add-2 x y z) (my-add x y z)) (my-add-2 1 2 3))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('my-add'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]]], [[Symbol.for('lambda'), [], [Symbol.for('define'), [Symbol.for('my-add-2'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')], [Symbol.for('my-add'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], [Symbol.for('my-add-2'), 1, 2, 3]]]], 6]);
  });
  it('(compile \'(define x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x')]]], 'let x;']);
  });
  it('(compile \'(define x 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), 1]]], 'let x = 1;']);
  });
  it('(compile \'(define (foo x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')]]], 'function foo(x) {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define foo (lambda (x) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], 'let foo = function (x) {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(define x) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x')]], Symbol.for(':to'), 'typescript'], 'let x: any;']);
  });
  it('(compile \'(define x 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), 1]]], 'let x = 1;']);
  });
  it('(compile \'(define x 1) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), 1]], Symbol.for(':to'), 'typescript'], 'let x: any = 1;']);
  });
  it('(compile \'(define I (lambda (x) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('I'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]], 'let I = function (x) {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(define I (memoize (lambda (x) x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('I'), [Symbol.for('memoize'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]]], 'let I = memoize(function (x) {\n' +
      '  return x;\n' +
      '});']);
  });
  it('(compile \'(define (identity-function x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('identity-function'), Symbol.for('x')], Symbol.for('x')]]], 'function identityFunction(x) {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define (I x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')]]], 'function I(x) {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define (K x y) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('K'), Symbol.for('x'), Symbol.for('y')], Symbol.for('x')]]], 'function K(x, y) {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define (S f g x) (f x (g x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('S'), Symbol.for('f'), Symbol.for('g'), Symbol.for('x')], [Symbol.for('f'), Symbol.for('x'), [Symbol.for('g'), Symbol.for('x')]]]]], 'function S(f, g, x) {\n' +
      '  return f(x, g(x));\n' +
      '}']);
  });
  it('(compile \'(define (S f g x) ((f x) (g x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('S'), Symbol.for('f'), Symbol.for('g'), Symbol.for('x')], [[Symbol.for('f'), Symbol.for('x')], [Symbol.for('g'), Symbol.for('x')]]]]], 'function S(f, g, x) {\n' +
      '  return f(x)(g(x));\n' +
      '}']);
  });
  it('(compile \'(define (C f x y) (f y x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('C'), Symbol.for('f'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')]]]], 'function C(f, x, y) {\n' +
      '  return f(y, x);\n' +
      '}']);
  });
  it('(compile \'(define (U f) (f f)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('U'), Symbol.for('f')], [Symbol.for('f'), Symbol.for('f')]]]], 'function U(f) {\n' +
      '  return f(f);\n' +
      '}']);
  });
  it('(compile \'(define (A f . args) (apply f args)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('A'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]]]], 'function A(f, ...args) {\n' +
      '  return f(...args);\n' +
      '}']);
  });
  it('(compile \'(define (A f . args) (apply f args)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('A'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]]], Symbol.for(':to'), 'typescript'], 'function A(f: any, ...args: any[]): any {\n' +
      '  return f(...args);\n' +
      '}']);
  });
  it('(compile \'(define (Q . args) (cond ((= (.-length args) 0) #u) ((= (.-length args) 1) (aref args 0)) (else (let ((fs (.slice args 0 -1)) (x (aref args (- (.-length args) 1)))) (.reduce fs (lambda (acc f) (f acc)) x))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('Q'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('.-length'), Symbol.for('args')], 0], undefined], [[Symbol.for('='), [Symbol.for('.-length'), Symbol.for('args')], 1], [Symbol.for('aref'), Symbol.for('args'), 0]], [Symbol.for('else'), [Symbol.for('let'), [[Symbol.for('fs'), [Symbol.for('.slice'), Symbol.for('args'), 0, -1]], [Symbol.for('x'), [Symbol.for('aref'), Symbol.for('args'), [Symbol.for('-'), [Symbol.for('.-length'), Symbol.for('args')], 1]]]], [Symbol.for('.reduce'), Symbol.for('fs'), [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('f')], [Symbol.for('f'), Symbol.for('acc')]], Symbol.for('x')]]]]]]], 'function Q(...args) {\n' +
      '  if (args.length === 0) {\n' +
      '    return undefined;\n' +
      '  } else if (args.length === 1) {\n' +
      '    return args[0];\n' +
      '  } else {\n' +
      '    let fs = args.slice(0, -1);\n' +
      '    let x = args[args.length - 1];\n' +
      '    return fs.reduce(function (acc, f) {\n' +
      '      return f(acc);\n' +
      '    }, x);\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define (T . args) (cond ((= (.-length args) 0) #u) ((= (.-length args) 1) (aref args 0)) (else (let-values (((x . fs) args)) (.reduce fs (lambda (acc f) (f acc)) x))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('T'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('.-length'), Symbol.for('args')], 0], undefined], [[Symbol.for('='), [Symbol.for('.-length'), Symbol.for('args')], 1], [Symbol.for('aref'), Symbol.for('args'), 0]], [Symbol.for('else'), [Symbol.for('let-values'), [[[Symbol.for('x'), Symbol.for('.'), Symbol.for('fs')], Symbol.for('args')]], [Symbol.for('.reduce'), Symbol.for('fs'), [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('f')], [Symbol.for('f'), Symbol.for('acc')]], Symbol.for('x')]]]]]]], 'function T(...args) {\n' +
      '  if (args.length === 0) {\n' +
      '    return undefined;\n' +
      '  } else if (args.length === 1) {\n' +
      '    return args[0];\n' +
      '  } else {\n' +
      '    let [x, ...fs] = args;\n' +
      '    return fs.reduce(function (acc, f) {\n' +
      '      return f(acc);\n' +
      '    }, x);\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define (Y f) ((lambda (future) (f (lambda (arg) ((future future) arg)))) (lambda (future) (f (lambda (arg) ((future future) arg)))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('Y'), Symbol.for('f')], [[Symbol.for('lambda'), [Symbol.for('future')], [Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('arg')], [[Symbol.for('future'), Symbol.for('future')], Symbol.for('arg')]]]], [Symbol.for('lambda'), [Symbol.for('future')], [Symbol.for('f'), [Symbol.for('lambda'), [Symbol.for('arg')], [[Symbol.for('future'), Symbol.for('future')], Symbol.for('arg')]]]]]]]], 'function Y(f) {\n' +
      '  return (function (future) {\n' +
      '    return f(function (arg) {\n' +
      '      return future(future)(arg);\n' +
      '    });\n' +
      '  })(function (future) {\n' +
      '    return f(function (arg) {\n' +
      '      return future(future)(arg);\n' +
      '    });\n' +
      '  });\n' +
      '}']);
  });
  it('(compile \'(define (compose f g) (lambda (x) (f (g x)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('compose'), Symbol.for('f'), Symbol.for('g')], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')]]]]]], 'function compose(f, g) {\n' +
      '  return function (x) {\n' +
      '    return f(g(x));\n' +
      '  };\n' +
      '}']);
  });
  it('(compile \'(define (foo) (set! x (+ x 1)) (set! y (+ y 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('foo')], [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('+'), Symbol.for('x'), 1]], [Symbol.for('set!'), Symbol.for('y'), [Symbol.for('+'), Symbol.for('y'), 1]]]]], 'function foo() {\n' +
      '  x++;\n' +
      '  return ++y;\n' +
      '}']);
  });
  it('(compile \'(define (map-get map path) (let-values (((value) (map-get-2 map path))) value)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('map-get'), Symbol.for('map'), Symbol.for('path')], [Symbol.for('let-values'), [[[Symbol.for('value')], [Symbol.for('map-get-2'), Symbol.for('map'), Symbol.for('path')]]], Symbol.for('value')]]]], 'function mapGet(map, path) {\n' +
      '  let [value] = mapGet2(map, path);\n' +
      '  return value;\n' +
      '}']);
  });
  it('(compile \'(define (add-matrix m1 m2) (let ((l1 (length m1)) (l2 (length m2))) (let ((matrix (make-matrix l1 l2))) (for ((i (range 0 l1))) (for ((j (range 0 l2))) (set! (aget matrix i j) (+ (aget m1 i j) (aget m2 i j))))) matrix))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('add-matrix'), Symbol.for('m1'), Symbol.for('m2')], [Symbol.for('let'), [[Symbol.for('l1'), [Symbol.for('length'), Symbol.for('m1')]], [Symbol.for('l2'), [Symbol.for('length'), Symbol.for('m2')]]], [Symbol.for('let'), [[Symbol.for('matrix'), [Symbol.for('make-matrix'), Symbol.for('l1'), Symbol.for('l2')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('l1')]]], [Symbol.for('for'), [[Symbol.for('j'), [Symbol.for('range'), 0, Symbol.for('l2')]]], [Symbol.for('set!'), [Symbol.for('aget'), Symbol.for('matrix'), Symbol.for('i'), Symbol.for('j')], [Symbol.for('+'), [Symbol.for('aget'), Symbol.for('m1'), Symbol.for('i'), Symbol.for('j')], [Symbol.for('aget'), Symbol.for('m2'), Symbol.for('i'), Symbol.for('j')]]]]], Symbol.for('matrix')]]]]], 'function addMatrix(m1, m2) {\n' +
      '  let l1 = m1.length;\n' +
      '  let l2 = m2.length;\n' +
      '  let matrix = makeMatrix(l1, l2);\n' +
      '  for (let i = 0; i < l1; i++) {\n' +
      '    for (let j = 0; j < l2; j++) {\n' +
      '      matrix[i][j] = m1[i][j] + m2[i][j];\n' +
      '    }\n' +
      '  }\n' +
      '  return matrix;\n' +
      '}']);
  });
  it('(compile \'(define _ (js/obj "dash" #t)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('_'), [Symbol.for('js/obj'), 'dash', true]]]], 'let _ = {\n' +
      '  dash: true\n' +
      '};']);
  });
  it('(compile \'(define __ (js/obj "dash" #t)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('__'), [Symbol.for('js/obj'), 'dash', true]]]], 'let __ = {\n' +
      '  dash: true\n' +
      '};']);
  });
  it('(compile \'(module m scheme (define I (curry-n 1 (lambda (x) x))) (define K (curry-n 2 (lambda (x y) x)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('I'), [Symbol.for('curry-n'), 1, [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]], [Symbol.for('define'), Symbol.for('K'), [Symbol.for('curry-n'), 2, [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('x')]]]]]], 'import {\n' +
      '  curryN\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'let I = curryN(1, function (x) {\n' +
      '  return x;\n' +
      '});\n' +
      '\n' +
      'let K = curryN(2, function (x, y) {\n' +
      '  return x;\n' +
      '});']);
  });
  it('(compile \'(define Foo (class object%)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('Foo'), [Symbol.for('class'), Symbol.for('object%')]]]], 'class Foo {\n' +
      '}']);
  });
  return it('(compile \'(define Foo (class Bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('Foo'), [Symbol.for('class'), Symbol.for('Bar')]]]], 'class Foo extends Bar {\n' +
      '}']);
  });
});

describe('define-syntax', function (): any {
  it('(compile \'(module m scheme (define-syntax foo (lambda (x) (syntax test))) (foo 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define-syntax'), Symbol.for('foo'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('syntax'), Symbol.for('test')]]], [Symbol.for('foo'), 1]]]], 'import {\n' +
      '  datumToSyntax\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'let foo = function (x) {\n' +
      '  return datumToSyntax(false, Symbol.for(\'test\'));\n' +
      '};\n' +
      '\n' +
      'foo.ftype = [Symbol.for(\'macro->\'), Symbol.for(\'Syntax\'), Symbol.for(\'Syntax\')];\n' +
      '\n' +
      'test;']);
  });
  it('(compile \'(module m scheme (define-syntax (foo x) (syntax test)) (foo 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define-syntax'), [Symbol.for('foo'), Symbol.for('x')], [Symbol.for('syntax'), Symbol.for('test')]], [Symbol.for('foo'), 1]]]], 'import {\n' +
      '  datumToSyntax\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'function foo(x) {\n' +
      '  return datumToSyntax(false, Symbol.for(\'test\'));\n' +
      '}\n' +
      '\n' +
      'foo.ftype = [Symbol.for(\'macro->\'), Symbol.for(\'Syntax\'), Symbol.for(\'Syntax\')];\n' +
      '\n' +
      'test;']);
  });
  return it('(compile \'(module m scheme (define-syntax (foo x) (second (syntax-e x))) (foo 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define-syntax'), [Symbol.for('foo'), Symbol.for('x')], [Symbol.for('second'), [Symbol.for('syntax-e'), Symbol.for('x')]]], [Symbol.for('foo'), 1]]]], 'import {\n' +
      '  syntaxE\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'function foo(x) {\n' +
      '  return syntaxE(x)[1];\n' +
      '}\n' +
      '\n' +
      'foo.ftype = [Symbol.for(\'macro->\'), Symbol.for(\'Syntax\'), Symbol.for(\'Syntax\')];\n' +
      '\n' +
      '1;']);
  });
});

describe('syntax->list', function (): any {
  it('(syntax->list (syntax ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('syntax->list'), [Symbol.for('syntax'), []]], [Symbol.for('quote'), []]]);
  });
  return it('(syntax->list (syntax (1 . 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('syntax->list'), [Symbol.for('syntax'), [1, Symbol.for('.'), 2]]], false]);
  });
});

describe('syntax-e', function (): any {
  it('(syntax-e (syntax ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('syntax-e'), [Symbol.for('syntax'), []]], [Symbol.for('quote'), []]]);
  });
  it('(dotted-list? (syntax-e (syntax (1 . 2))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('syntax-e'), [Symbol.for('syntax'), [1, Symbol.for('.'), 2]]]], true]);
  });
  return it('(dotted-list? (cdr (syntax-e (syntax (1 . (2 . 3))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('dotted-list?'), [Symbol.for('cdr'), [Symbol.for('syntax-e'), [Symbol.for('syntax'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]]]]], true]);
  });
});

describe('let', function (): any {
  it('(let ((x 0)) x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 0]], Symbol.for('x')], 0]);
  });
  it('(let ((x 1)) (let ((y 2)) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 1]], [Symbol.for('let'), [[Symbol.for('y'), 2]], Symbol.for('x')]], 1]);
  });
  it('(let ((x \'((1 2) (3 4)))) x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), [Symbol.for('quote'), [[1, 2], [3, 4]]]]], Symbol.for('x')], [Symbol.for('quote'), [[1, 2], [3, 4]]]]);
  });
  it('(let (x) (set! x 1) x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('x')], [Symbol.for('set!'), Symbol.for('x'), 1], Symbol.for('x')], 1]);
  });
  it('(let (x) (set! x 1) (set! x 2) x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('x')], [Symbol.for('set!'), Symbol.for('x'), 1], [Symbol.for('set!'), Symbol.for('x'), 2], Symbol.for('x')], 2]);
  });
  it('(let (x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('x')]], undefined]);
  });
  it('(let ((a 1)) (+ (let ((a 2)) a) a))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('a'), 1]], [Symbol.for('+'), [Symbol.for('let'), [[Symbol.for('a'), 2]], Symbol.for('a')], Symbol.for('a')]], 3]);
  });
  it('(let ((compose (lambda (f g) (lambda (x) (f (g x))))) (square (lambda (x) (* x x))) (add1 (lambda (x) (+ x 1)))) ((compose square add1) (add1 4)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('compose'), [Symbol.for('lambda'), [Symbol.for('f'), Symbol.for('g')], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')]]]]], [Symbol.for('square'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')]]], [Symbol.for('add1'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('+'), Symbol.for('x'), 1]]]], [[Symbol.for('compose'), Symbol.for('square'), Symbol.for('add1')], [Symbol.for('add1'), 4]]], 36]);
  });
  it('(compile \'(let (x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [Symbol.for('x')]]]], 'let x;']);
  });
  it('(compile \'(let (x) x) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [Symbol.for('x')], Symbol.for('x')]], Symbol.for(':as'), 'return'], 'let x;\n' +
      '\n' +
      'return x;']);
  });
  it('(compile \'(let (x) x) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [Symbol.for('x')], Symbol.for('x')]], Symbol.for(':as'), 'expression'], '(() => {\n' +
      '  let x;\n' +
      '  return x;\n' +
      '})()']);
  });
  it('(compile \'(let (x) x) :as "return" :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [Symbol.for('x')], Symbol.for('x')]], Symbol.for(':as'), 'return', Symbol.for(':to'), 'typescript'], 'let x: any;\n' +
      '\n' +
      'return x;']);
  });
  it('(compile \'(let ((x 1)) x) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')]], Symbol.for(':as'), 'return'], 'let x = 1;\n' +
      '\n' +
      'return x;']);
  });
  it('(compile \'(let ((x 1)) x) :as "return" :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')]], Symbol.for(':as'), 'return', Symbol.for(':to'), 'typescript'], 'let x: any = 1;\n' +
      '\n' +
      'return x;']);
  });
  it('(compile \'(let ((compose (lambda (f g) (lambda (x) (f (g x))))) (square (lambda (x) (* x x))) (add1 (lambda (x) (+ x 1)))) (display ((compose square add1) (add1 4)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [[Symbol.for('compose'), [Symbol.for('lambda'), [Symbol.for('f'), Symbol.for('g')], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')]]]]], [Symbol.for('square'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')]]], [Symbol.for('add1'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('+'), Symbol.for('x'), 1]]]], [Symbol.for('display'), [[Symbol.for('compose'), Symbol.for('square'), Symbol.for('add1')], [Symbol.for('add1'), 4]]]]]], 'let compose = function (f, g) {\n' +
      '  return function (x) {\n' +
      '    return f(g(x));\n' +
      '  };\n' +
      '};\n' +
      '\n' +
      'let square = function (x) {\n' +
      '  return x * x;\n' +
      '};\n' +
      '\n' +
      'let add1 = function (x) {\n' +
      '  return x + 1;\n' +
      '};\n' +
      '\n' +
      'console.log(compose(square, add1)(add1(4)));']);
  });
  it('(compile \'(let ((and (lambda (x y) (if x (if y #t #f) #f)))) (and x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [[Symbol.for('and'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('if'), Symbol.for('x'), [Symbol.for('if'), Symbol.for('y'), true, false], false]]]], [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')]]]], 'let and = function (x, y) {\n' +
      '  if (x) {\n' +
      '    if (y) {\n' +
      '      return true;\n' +
      '    } else {\n' +
      '      return false;\n' +
      '    }\n' +
      '  } else {\n' +
      '    return false;\n' +
      '  }\n' +
      '};\n' +
      '\n' +
      'and(x, y);']);
  });
  it('(compile \'(begin x (let ((x 1)) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), Symbol.for('x'), [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')]]]], 'x;\n' +
      '\n' +
      'let x = 1;\n' +
      '\n' +
      'x;']);
  });
  it('(compile \'(begin (let ((x 1)) (display x)) (let ((x 1)) (display x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('let'), [[Symbol.for('x'), 1]], [Symbol.for('display'), Symbol.for('x')]], [Symbol.for('let'), [[Symbol.for('x'), 1]], [Symbol.for('display'), Symbol.for('x')]]]]], 'let x = 1;\n' +
      '\n' +
      'console.log(x);\n' +
      '\n' +
      '{\n' +
      '  let x = 1;\n' +
      '  console.log(x);\n' +
      '}']);
  });
  it('(compile \'(cond (foo bar) (else x (let ((x 1)) x))) :as "return" :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [Symbol.for('foo'), Symbol.for('bar')], [Symbol.for('else'), Symbol.for('x'), [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')]]]], Symbol.for(':as'), 'return', Symbol.for(':to'), 'typescript'], 'if (foo) {\n' +
      '  return bar;\n' +
      '} else {\n' +
      '  x;\n' +
      '  let x: any = 1;\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(define make-compilation-evaluator (memoize (lambda (env (options (js/obj))) (let ((language (oget options "language"))) (set! language (or language default-language)) (let ((compilation-env (or (.get compilation-map language) javascript-env))) (new CompilationEvaluator env compilation-env options)))))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('make-compilation-evaluator'), [Symbol.for('memoize'), [Symbol.for('lambda'), [Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('let'), [[Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']]], [Symbol.for('set!'), Symbol.for('language'), [Symbol.for('or'), Symbol.for('language'), Symbol.for('default-language')]], [Symbol.for('let'), [[Symbol.for('compilation-env'), [Symbol.for('or'), [Symbol.for('.get'), Symbol.for('compilation-map'), Symbol.for('language')], Symbol.for('javascript-env')]]], [Symbol.for('new'), Symbol.for('CompilationEvaluator'), Symbol.for('env'), Symbol.for('compilation-env'), Symbol.for('options')]]]]]]], Symbol.for(':to'), 'typescript'], 'let makeCompilationEvaluator: any = memoize(function (env: any, options: any = {}): any {\n' +
      '  let language: any = options[\'language\'];\n' +
      '  language = language || defaultLanguage;\n' +
      '  let compilationEnv: any = compilationMap.get(language) || javascriptEnv;\n' +
      '  return new CompilationEvaluator(env, compilationEnv, options);\n' +
      '});']);
  });
  return it('(compile \'(cond (foo (let ((x #t)) x)) (else #f)) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [Symbol.for('foo'), [Symbol.for('let'), [[Symbol.for('x'), true]], Symbol.for('x')]], [Symbol.for('else'), false]]], Symbol.for(':as'), 'return'], 'if (foo) {\n' +
      '  let x = true;\n' +
      '  return x;\n' +
      '} else {\n' +
      '  return false;\n' +
      '}']);
  });
});

describe('let*', function (): any {
  return it('(let* ((x 1)) x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let*'), [[Symbol.for('x'), 1]], Symbol.for('x')], 1]);
  });
});

describe('let-values', function (): any {
  it('(let-values (((x y) (values 1 2))) (list x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let-values'), [[[Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('quote'), [1, 2]]]);
  });
  it('(let-values (((x . y) (values 1 2))) (list x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let-values'), [[[Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('values'), 1, 2]]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('quote'), [1, [2]]]]);
  });
  it('(compile \'(let-values (((x y) (values 1 2))) (define z (+ x y))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let-values'), [[[Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]]], [Symbol.for('define'), Symbol.for('z'), [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]]], 'let [x, y] = [1, 2];\n' +
      '\n' +
      'let z = x + y;']);
  });
  it('(compile \'(let-values ((value (foo bar baz))) value) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let-values'), [[Symbol.for('value'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], Symbol.for('value')]], Symbol.for(':as'), 'return'], 'let value = foo(bar, baz);\n' +
      '\n' +
      'return value;']);
  });
  it('(compile \'(let-values (((value) (foo bar baz))) value) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let-values'), [[[Symbol.for('value')], [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], Symbol.for('value')]], Symbol.for(':as'), 'return'], 'let [value] = foo(bar, baz);\n' +
      '\n' +
      'return value;']);
  });
  it('(compile \'(let-values (((value) (foo bar baz))) value) :as "return" :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let-values'), [[[Symbol.for('value')], [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], Symbol.for('value')]], Symbol.for(':as'), 'return', Symbol.for(':to'), 'typescript'], 'let [value]: any[] = foo(bar, baz);\n' +
      '\n' +
      'return value;']);
  });
  it('(compile \'(let-values (((x . fs) args)) (.reduce fs (lambda (acc f) (f acc)) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let-values'), [[[Symbol.for('x'), Symbol.for('.'), Symbol.for('fs')], Symbol.for('args')]], [Symbol.for('.reduce'), Symbol.for('fs'), [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('f')], [Symbol.for('f'), Symbol.for('acc')]], Symbol.for('x')]]]], 'let [x, ...fs] = args;\n' +
      '\n' +
      'fs.reduce(function (acc, f) {\n' +
      '  return f(acc);\n' +
      '}, x);']);
  });
  it('(compile \'(let-values (((x . fs) args)) (.reduce fs (lambda (acc f) (f acc)) x)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let-values'), [[[Symbol.for('x'), Symbol.for('.'), Symbol.for('fs')], Symbol.for('args')]], [Symbol.for('.reduce'), Symbol.for('fs'), [Symbol.for('lambda'), [Symbol.for('acc'), Symbol.for('f')], [Symbol.for('f'), Symbol.for('acc')]], Symbol.for('x')]]], Symbol.for(':to'), 'typescript'], 'let [x, ...fs]: any[] = args;\n' +
      '\n' +
      'fs.reduce(function (acc: any, f: any): any {\n' +
      '  return f(acc);\n' +
      '}, x);']);
  });
  it('(compile \'(let-values (((value1) (foo bar)) ((value2) (bar baz))) (list value1 value2)) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let-values'), [[[Symbol.for('value1')], [Symbol.for('foo'), Symbol.for('bar')]], [[Symbol.for('value2')], [Symbol.for('bar'), Symbol.for('baz')]]], [Symbol.for('list'), Symbol.for('value1'), Symbol.for('value2')]]], Symbol.for(':as'), 'return'], 'let [value1] = foo(bar);\n' +
      '\n' +
      'let [value2] = bar(baz);\n' +
      '\n' +
      'return [value1, value2];']);
  });
  return it('(compile \'(begin value (let-values ((value (foo bar baz))) value)) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), Symbol.for('value'), [Symbol.for('let-values'), [[Symbol.for('value'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], Symbol.for('value')]]], Symbol.for(':as'), 'return'], 'value;\n' +
      '\n' +
      'let value = foo(bar, baz);\n' +
      '\n' +
      'return value;']);
  });
});

describe('let*-values', function (): any {
  it('(let*-values (((x y) (values 1 2)) ((w z) (values 3 4))) (list x y w z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let*-values'), [[[Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]], [[Symbol.for('w'), Symbol.for('z')], [Symbol.for('values'), 3, 4]]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y'), Symbol.for('w'), Symbol.for('z')]], [Symbol.for('quote'), [1, 2, 3, 4]]]);
  });
  return it('(compile \'(let*-values (((x y) (values 1 2)) ((w z) (values 3 4))) (define z (+ x y w z))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let*-values'), [[[Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]], [[Symbol.for('w'), Symbol.for('z')], [Symbol.for('values'), 3, 4]]], [Symbol.for('define'), Symbol.for('z'), [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), Symbol.for('w'), Symbol.for('z')]]]]], 'let [x, y] = [1, 2];\n' +
      '\n' +
      'let [w, z] = [3, 4];\n' +
      '\n' +
      'let z = x + y + w + z;']);
  });
});

describe('lambda', function (): any {
  it('((lambda (x) x) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 1], 1]);
  });
  it('((lambda (x) x) "Lisp")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 'Lisp'], 'Lisp']);
  });
  it('((lambda ((x "Lisp")) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [[Symbol.for('x'), 'Lisp']], Symbol.for('x')]], 'Lisp']);
  });
  it('((lambda ((x "Lisp")) x) "Scheme")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [[Symbol.for('x'), 'Lisp']], Symbol.for('x')], 'Scheme'], 'Scheme']);
  });
  it('((lambda x x) "Lisp")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')], 'Lisp'], [Symbol.for('quote'), ['Lisp']]]);
  });
  it('((fn (x) x) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('fn'), [Symbol.for('x')], Symbol.for('x')], 1], 1]);
  });
  it('((λ (x) x) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('λ'), [Symbol.for('x')], Symbol.for('x')], 1], 1]);
  });
  it('(compile \'(lambda (x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]], 'function (x) {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(lambda (x) x) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]], Symbol.for(':to'), 'typescript'], 'function (x: any): any {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(lambda args args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), Symbol.for('args'), Symbol.for('args')]]], 'function (...args) {\n' +
      '  return args;\n' +
      '};']);
  });
  it('(compile \'(lambda (x . args) args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')]]], 'function (x, ...args) {\n' +
      '  return args;\n' +
      '};']);
  });
  it('(compile \'(lambda (x y . args) args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')]]], 'function (x, y, ...args) {\n' +
      '  return args;\n' +
      '};']);
  });
  it('(compile \'(lambda (x) (let ((x 1)) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')]]]], 'function (x) {\n' +
      '  {\n' +
      '    let x = 1;\n' +
      '    return x;\n' +
      '  }\n' +
      '};']);
  });
  it('(compile \'(lambda (x) (let ((y 1)) y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('let'), [[Symbol.for('y'), 1]], Symbol.for('y')]]]], 'function (x) {\n' +
      '  let y = 1;\n' +
      '  return y;\n' +
      '};']);
  });
  it('(compile \'(lambda (given (surname "Smith")) (string-append "Hello, " given " " surname)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('given'), [Symbol.for('surname'), 'Smith']], [Symbol.for('string-append'), 'Hello, ', Symbol.for('given'), ' ', Symbol.for('surname')]]]], 'function (given, surname = \'Smith\') {\n' +
      '  return \'Hello, \' + given + \' \' + surname;\n' +
      '};']);
  });
  it('(compile \'(lambda (given (surname "Smith")) (string-append "Hello, " given " " surname)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('given'), [Symbol.for('surname'), 'Smith']], [Symbol.for('string-append'), 'Hello, ', Symbol.for('given'), ' ', Symbol.for('surname')]]], Symbol.for(':to'), 'typescript'], 'function (given: any, surname: any = \'Smith\'): any {\n' +
      '  return \'Hello, \' + given + \' \' + surname;\n' +
      '};']);
  });
  return it('(compile \'(lambda (arg (options (js/obj))) arg) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('lambda'), [Symbol.for('arg'), [Symbol.for('options'), [Symbol.for('js/obj')]]], Symbol.for('arg')]], Symbol.for(':to'), 'typescript'], 'function (arg: any, options: any = {}): any {\n' +
      '  return arg;\n' +
      '};']);
  });
});

describe('thunk', function (): any {
  it('(procedure? (thunk 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('procedure?'), [Symbol.for('thunk'), 1]], true]);
  });
  it('(thunk? (thunk 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('thunk?'), [Symbol.for('thunk'), 1]], true]);
  });
  it('(arity (thunk 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('arity'), [Symbol.for('thunk'), 1]], 0]);
  });
  it('((thunk 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('thunk'), 1]], 1]);
  });
  it('((thunk 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('thunk'), 1, 2]], 2]);
  });
  return it('((thunk 1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('thunk'), 1, 2, 3]], 3]);
  });
});

describe('delay', function (): any {
  it('(force (delay 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('force'), [Symbol.for('delay'), 1]], 1]);
  });
  it('(force (delay 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('force'), [Symbol.for('delay'), 1, 2]], 2]);
  });
  return it('(force (delay 1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('force'), [Symbol.for('delay'), 1, 2, 3]], 3]);
  });
});

describe('lazy', function (): any {
  it('(force (lazy 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('force'), [Symbol.for('lazy'), 1]], 1]);
  });
  it('(force (lazy 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('force'), [Symbol.for('lazy'), 1, 2]], 2]);
  });
  it('(force (lazy 1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('force'), [Symbol.for('lazy'), 1, 2, 3]], 3]);
  });
  it('(force (lazy (delay 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('force'), [Symbol.for('lazy'), [Symbol.for('delay'), 1]]], 1]);
  });
  it('(force (lazy (lazy 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('force'), [Symbol.for('lazy'), [Symbol.for('lazy'), 1]]], 1]);
  });
  return it('(force (lazy (lazy (lazy 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('force'), [Symbol.for('lazy'), [Symbol.for('lazy'), [Symbol.for('lazy'), 1]]]], 1]);
  });
});

describe('promise?', function (): any {
  it('(promise? (delay 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('promise?'), [Symbol.for('delay'), 1]], true]);
  });
  return it('(promise? (lazy 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('promise?'), [Symbol.for('lazy'), 1]], true]);
  });
});

describe('promise-forced?', function (): any {
  it('(promise-forced? (delay 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('promise-forced?'), [Symbol.for('delay'), 1]], false]);
  });
  return it('(let ((p (delay 1))) (force p) (promise-forced? p))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('p'), [Symbol.for('delay'), 1]]], [Symbol.for('force'), Symbol.for('p')], [Symbol.for('promise-forced?'), Symbol.for('p')]], true]);
  });
});

describe('apply', function (): any {
  it('(compile \'(apply f args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]]], 'f(...args);']);
  });
  it('(compile \'(apply f x args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('apply'), Symbol.for('f'), Symbol.for('x'), Symbol.for('args')]]], 'f(x, ...args);']);
  });
  it('(compile \'(apply new Foo args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('apply'), Symbol.for('new'), Symbol.for('Foo'), Symbol.for('args')]]], 'new Foo(...args);']);
  });
  it('(compile \'(apply new Foo x y args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('apply'), Symbol.for('new'), Symbol.for('Foo'), Symbol.for('x'), Symbol.for('y'), Symbol.for('args')]]], 'new Foo(x, y, ...args);']);
  });
  it('(compile \'(apply (get-field method obj) args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('apply'), [Symbol.for('get-field'), Symbol.for('method'), Symbol.for('obj')], Symbol.for('args')]]], 'obj.method(...args);']);
  });
  return it('(compile \'(apply (.-method obj) args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('apply'), [Symbol.for('.-method'), Symbol.for('obj')], Symbol.for('args')]]], 'obj.method(...args);']);
  });
});

describe('lexical scope', function (): any {
  it('((lambda () (define (K x) (lambda () x)) ((K 42))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), [Symbol.for('K'), Symbol.for('x')], [Symbol.for('lambda'), [], Symbol.for('x')]], [[Symbol.for('K'), 42]]]], 42]);
  });
  it('((lambda () (define incrementer #u) (let ((x 1)) (set! incrementer (lambda () (set! x (+ x 1)) x))) (incrementer)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('incrementer'), undefined], [Symbol.for('let'), [[Symbol.for('x'), 1]], [Symbol.for('set!'), Symbol.for('incrementer'), [Symbol.for('lambda'), [], [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('+'), Symbol.for('x'), 1]], Symbol.for('x')]]], [Symbol.for('incrementer')]]], 2]);
  });
  return it('(let ((x 100) incrementer) (let ((x 1)) (set! incrementer (lambda () (set! x (+ x 1)) x))) (incrementer) x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 100], Symbol.for('incrementer')], [Symbol.for('let'), [[Symbol.for('x'), 1]], [Symbol.for('set!'), Symbol.for('incrementer'), [Symbol.for('lambda'), [], [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('+'), Symbol.for('x'), 1]], Symbol.for('x')]]], [Symbol.for('incrementer')], Symbol.for('x')], 100]);
  });
});

describe('begin', function (): any {
  it('(begin)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('begin')], undefined]);
  });
  it('(compile \'(begin x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x;\n' +
      '\n' +
      'y;\n' +
      '\n' +
      'z;']);
  });
  it('(compile \'(begin x (begin y z)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), Symbol.for('x'), [Symbol.for('begin'), Symbol.for('y'), Symbol.for('z')]]]], 'x;\n' +
      '\n' +
      'y;\n' +
      '\n' +
      'z;']);
  });
  it('(compile `(begin x y) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('begin'), Symbol.for('x'), Symbol.for('y')]], Symbol.for(':as'), 'expression'], 'x, y']);
  });
  it('(compile \'(begin x y z) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'expression'], 'x, y, z']);
  });
  return it('(compile \'(begin (define (and x y) (or x y)) (define (or x y) x) (and x (or y z))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('or'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('define'), [Symbol.for('or'), Symbol.for('x'), Symbol.for('y')], Symbol.for('x')], [Symbol.for('and'), Symbol.for('x'), [Symbol.for('or'), Symbol.for('y'), Symbol.for('z')]]]]], 'function and(x, y) {\n' +
      '  return or(x, y);\n' +
      '}\n' +
      '\n' +
      'function or(x, y) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'and(x, or(y, z));']);
  });
});

describe('begin0', function (): any {
  return it('(begin0 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('begin0'), 1, 2], 1]);
  });
});

describe('if', function (): any {
  it('(if #t 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('if'), true, 1, 2], 1]);
  });
  it('(if #f 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('if'), false, 1, 2], 2]);
  });
  it('(if (< 1 2) 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('if'), [Symbol.for('<'), 1, 2], 1, 2], 1]);
  });
  it('(if (> 2 1) 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('if'), [Symbol.for('>'), 2, 1], 1, 2], 1]);
  });
  it('(compile \'(if x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'if (x) {\n' +
      '  y;\n' +
      '} else {\n' +
      '  z;\n' +
      '}']);
  });
  it('(compile \'(if #t (foo) (bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), true, [Symbol.for('foo')], [Symbol.for('bar')]]]], 'if (true) {\n' +
      '  foo();\n' +
      '} else {\n' +
      '  bar();\n' +
      '}']);
  });
  it('(compile \'(if #t (foo) (bar)) :as "statement")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), true, [Symbol.for('foo')], [Symbol.for('bar')]]], Symbol.for(':as'), 'statement'], 'if (true) {\n' +
      '  foo();\n' +
      '} else {\n' +
      '  bar();\n' +
      '}']);
  });
  it('(compile \'(if #t (foo) (bar)) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), true, [Symbol.for('foo')], [Symbol.for('bar')]]], Symbol.for(':as'), 'return'], 'if (true) {\n' +
      '  return foo();\n' +
      '} else {\n' +
      '  return bar();\n' +
      '}']);
  });
  it('(compile \'(if #t (foo) (bar)) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), true, [Symbol.for('foo')], [Symbol.for('bar')]]], Symbol.for(':as'), 'expression'], 'true ? foo() : bar()']);
  });
  it('(compile \'(if #t (foo) (bar) (baz)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), true, [Symbol.for('foo')], [Symbol.for('bar')], [Symbol.for('baz')]]]], 'if (true) {\n' +
      '  foo();\n' +
      '} else {\n' +
      '  bar();\n' +
      '}']);
  });
  it('(compile \'(if x y) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), Symbol.for('x'), Symbol.for('y')]], Symbol.for(':as'), 'expression'], 'x ? y : undefined']);
  });
  it('(compile \'(if x y z) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'expression'], 'x ? y : z']);
  });
  it('(compile \'(if x y z) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'return'], 'if (x) {\n' +
      '  return y;\n' +
      '} else {\n' +
      '  return z;\n' +
      '}']);
  });
  it('(compile \'(if "foo" "bar" "baz") :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), 'foo', 'bar', 'baz']], Symbol.for(':as'), 'expression'], '\'foo\' ? \'bar\' : \'baz\'']);
  });
  it('(compile \'(if x (begin y z) w) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), Symbol.for('x'), [Symbol.for('begin'), Symbol.for('y'), Symbol.for('z')], Symbol.for('w')]], Symbol.for(':as'), 'return'], 'if (x) {\n' +
      '  y;\n' +
      '  return z;\n' +
      '} else {\n' +
      '  return w;\n' +
      '}']);
  });
  it('(compile \'(if (set! x y) z w))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), [Symbol.for('set!'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z'), Symbol.for('w')]]], 'if ((x = y)) {\n' +
      '  z;\n' +
      '} else {\n' +
      '  w;\n' +
      '}']);
  });
  it('(compile \'(if (set! x y) z w) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), [Symbol.for('set!'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z'), Symbol.for('w')]], Symbol.for(':as'), 'return'], 'if ((x = y)) {\n' +
      '  return z;\n' +
      '} else {\n' +
      '  return w;\n' +
      '}']);
  });
  it('(compile \'(if (set!-values (x) y) z w))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), [Symbol.for('set!-values'), [Symbol.for('x')], Symbol.for('y')], Symbol.for('z'), Symbol.for('w')]]], 'if (([x] = y)) {\n' +
      '  z;\n' +
      '} else {\n' +
      '  w;\n' +
      '}']);
  });
  return it('(compile \'(if (set!-fields (x) y) z w))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('if'), [Symbol.for('set!-fields'), [Symbol.for('x')], Symbol.for('y')], Symbol.for('z'), Symbol.for('w')]]], 'if (({x} = y)) {\n' +
      '  z;\n' +
      '} else {\n' +
      '  w;\n' +
      '}']);
  });
});

describe('when', function (): any {
  it('(when (< 1 2) 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('when'), [Symbol.for('<'), 1, 2], 1, 2], 2]);
  });
  it('(when (> 1 2) 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('when'), [Symbol.for('>'), 1, 2], 1, 2], undefined]);
  });
  it('(compile \'(when x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('when'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'if (x) {\n' +
      '  y;\n' +
      '  z;\n' +
      '}']);
  });
  it('(compile \'(when (< 1 2) (foo) (bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('when'), [Symbol.for('<'), 1, 2], [Symbol.for('foo')], [Symbol.for('bar')]]]], 'if (1 < 2) {\n' +
      '  foo();\n' +
      '  bar();\n' +
      '}']);
  });
  return it('(compile \'(when (> (length args) 0) (set! args (.concat (.slice args 0 (- (length args) 1)) (aref args (- (length args) 1))))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('length'), Symbol.for('args')], 0], [Symbol.for('set!'), Symbol.for('args'), [Symbol.for('.concat'), [Symbol.for('.slice'), Symbol.for('args'), 0, [Symbol.for('-'), [Symbol.for('length'), Symbol.for('args')], 1]], [Symbol.for('aref'), Symbol.for('args'), [Symbol.for('-'), [Symbol.for('length'), Symbol.for('args')], 1]]]]]]], 'if (args.length > 0) {\n' +
      '  args = args.slice(0, args.length - 1).concat(args[args.length - 1]);\n' +
      '}']);
  });
});

describe('unless', function (): any {
  it('(unless (< 1 2) 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('unless'), [Symbol.for('<'), 1, 2], 1, 2], undefined]);
  });
  it('(unless (> 1 2) 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('unless'), [Symbol.for('>'), 1, 2], 1, 2], 2]);
  });
  it('(compile \'(unless x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('unless'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'if (!x) {\n' +
      '  y;\n' +
      '  z;\n' +
      '}']);
  });
  return it('(compile \'(unless (> 1 2) (foo) (bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('unless'), [Symbol.for('>'), 1, 2], [Symbol.for('foo')], [Symbol.for('bar')]]]], 'if (!(1 > 2)) {\n' +
      '  foo();\n' +
      '  bar();\n' +
      '}']);
  });
});

describe('cond', function (): any {
  it('(cond (#f 1) (else 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cond'), [false, 1], [Symbol.for('else'), 2]], 2]);
  });
  it('(cond (#t 1) (#f 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cond'), [true, 1], [false, 2]], 1]);
  });
  it('(cond (#f 1) (#t 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cond'), [false, 1], [true, 2]], 2]);
  });
  it('(cond (#f 1) (#t 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cond'), [false, 1], [true, 2]], 2]);
  });
  it('(cond (1 => add1) (else 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('cond'), [1, Symbol.for('=>'), Symbol.for('add1')], [Symbol.for('else'), 3]], 2]);
  });
  it('(macroexpand-1 \'(cond (#f (foo)) (else (bar))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('macroexpand-1'), [Symbol.for('quote'), [Symbol.for('cond'), [false, [Symbol.for('foo')]], [Symbol.for('else'), [Symbol.for('bar')]]]]], [Symbol.for('quote'), [Symbol.for('if'), false, [Symbol.for('foo')], [Symbol.for('bar')]]]]);
  });
  it('(macroexpand-1 \'(cond (#f (foo) (bar)) (else (baz))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('macroexpand-1'), [Symbol.for('quote'), [Symbol.for('cond'), [false, [Symbol.for('foo')], [Symbol.for('bar')]], [Symbol.for('else'), [Symbol.for('baz')]]]]], [Symbol.for('quote'), [Symbol.for('if'), false, [Symbol.for('begin'), [Symbol.for('foo')], [Symbol.for('bar')]], [Symbol.for('baz')]]]]);
  });
  it('(macroexpand-1 \'(cond (#f (foo) (bar)) (else (baz) (quux))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('macroexpand-1'), [Symbol.for('quote'), [Symbol.for('cond'), [false, [Symbol.for('foo')], [Symbol.for('bar')]], [Symbol.for('else'), [Symbol.for('baz')], [Symbol.for('quux')]]]]], [Symbol.for('quote'), [Symbol.for('if'), false, [Symbol.for('begin'), [Symbol.for('foo')], [Symbol.for('bar')]], [Symbol.for('begin'), [Symbol.for('baz')], [Symbol.for('quux')]]]]]);
  });
  it('(macroexpand-1 \'(cond (x (foo)) (y (bar)) (else (baz))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('macroexpand-1'), [Symbol.for('quote'), [Symbol.for('cond'), [Symbol.for('x'), [Symbol.for('foo')]], [Symbol.for('y'), [Symbol.for('bar')]], [Symbol.for('else'), [Symbol.for('baz')]]]]], [Symbol.for('quote'), [Symbol.for('if'), Symbol.for('x'), [Symbol.for('foo')], [Symbol.for('if'), Symbol.for('y'), [Symbol.for('bar')], [Symbol.for('baz')]]]]]);
  });
  it('(compile \'(cond (#f (foo)) (else (bar))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [false, [Symbol.for('foo')]], [Symbol.for('else'), [Symbol.for('bar')]]]]], 'if (false) {\n' +
      '  foo();\n' +
      '} else {\n' +
      '  bar();\n' +
      '}']);
  });
  it('(compile \'(cond (x (foo)) (y (bar)) (else (baz))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [Symbol.for('x'), [Symbol.for('foo')]], [Symbol.for('y'), [Symbol.for('bar')]], [Symbol.for('else'), [Symbol.for('baz')]]]]], 'if (x) {\n' +
      '  foo();\n' +
      '} else if (y) {\n' +
      '  bar();\n' +
      '} else {\n' +
      '  baz();\n' +
      '}']);
  });
  it('(compile \'(cond (#f (foo)) (else (bar))) :as "statement")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [false, [Symbol.for('foo')]], [Symbol.for('else'), [Symbol.for('bar')]]]], Symbol.for(':as'), 'statement'], 'if (false) {\n' +
      '  foo();\n' +
      '} else {\n' +
      '  bar();\n' +
      '}']);
  });
  it('(compile \'(cond (x y)) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('y')]]], Symbol.for(':as'), 'return'], 'if (x) {\n' +
      '  return y;\n' +
      '}']);
  });
  it('(compile \'(cond (#f (foo)) (else (bar))) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [false, [Symbol.for('foo')]], [Symbol.for('else'), [Symbol.for('bar')]]]], Symbol.for(':as'), 'return'], 'if (false) {\n' +
      '  return foo();\n' +
      '} else {\n' +
      '  return bar();\n' +
      '}']);
  });
  it('(compile \'(cond (x y) (else z)) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('else'), Symbol.for('z')]]], Symbol.for(':as'), 'return'], 'if (x) {\n' +
      '  return y;\n' +
      '} else {\n' +
      '  return z;\n' +
      '}']);
  });
  it('(compile \'(cond ((set! x y) z) (else w)) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [[Symbol.for('set!'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')], [Symbol.for('else'), Symbol.for('w')]]], Symbol.for(':as'), 'return'], 'if ((x = y)) {\n' +
      '  return z;\n' +
      '} else {\n' +
      '  return w;\n' +
      '}']);
  });
  it('(compile \'(cond (x y)) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('y')]]], Symbol.for(':as'), 'expression'], 'x ? y : undefined']);
  });
  it('(compile \'(cond (x y) (else z)) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('else'), Symbol.for('z')]]], Symbol.for(':as'), 'expression'], 'x ? y : z']);
  });
  it('(compile \'(cond (#f (foo)) (else (bar))) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [false, [Symbol.for('foo')]], [Symbol.for('else'), [Symbol.for('bar')]]]], Symbol.for(':as'), 'expression'], 'false ? foo() : bar()']);
  });
  it('(compile \'(cond (x y) (else w z)) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('else'), Symbol.for('w'), Symbol.for('z')]]], Symbol.for(':as'), 'expression'], 'x ? y : (w, z)']);
  });
  it('(compile \'(cond (#t => y) (else #f)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('cond'), [true, Symbol.for('=>'), Symbol.for('y')], [Symbol.for('else'), false]]]], 'let _condVar;\n' +
      '\n' +
      'if ((_condVar = true)) {\n' +
      '  y(_condVar);\n' +
      '} else {\n' +
      '  false;\n' +
      '}']);
  });
  return it('(compile \'(begin (cond (#t => y) (else #f)) (cond (#t => y) (else #f))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('cond'), [true, Symbol.for('=>'), Symbol.for('y')], [Symbol.for('else'), false]], [Symbol.for('cond'), [true, Symbol.for('=>'), Symbol.for('y')], [Symbol.for('else'), false]]]]], 'let _condVar;\n' +
      '\n' +
      'if ((_condVar = true)) {\n' +
      '  y(_condVar);\n' +
      '} else {\n' +
      '  false;\n' +
      '}\n' +
      '\n' +
      'let _condVar1;\n' +
      '\n' +
      'if ((_condVar1 = true)) {\n' +
      '  y(_condVar1);\n' +
      '} else {\n' +
      '  false;\n' +
      '}']);
  });
});

describe('=', function (): any {
  it('(compile \'(= 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('='), 1, 1]]], '1 === 1;']);
  });
  return it('(compile \'(= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('='), Symbol.for('x'), Symbol.for('y')]]], 'x === y;']);
  });
});

describe('eq?', function (): any {
  it('(eq? #t #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('eq?'), true, true], true]);
  });
  it('(eq? #f #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('eq?'), false, false], true]);
  });
  it('(eq? #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('eq?'), true, false], false]);
  });
  it('(eq \'_ \'_)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('eq'), [Symbol.for('quote'), Symbol.for('_')], [Symbol.for('quote'), Symbol.for('_')]], true]);
  });
  it('(eq _ \'_)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('eq'), Symbol.for('_'), [Symbol.for('quote'), Symbol.for('_')]], false]);
  });
  return it('(compile \'(eq? #t #t))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('eq?'), true, true]]], 'true === true;']);
  });
});

describe('equal?', function (): any {
  it('(equal? #t #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('equal?'), true, true], true]);
  });
  it('(equal? #f #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('equal?'), false, false], true]);
  });
  it('(equal? #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('equal?'), true, false], false]);
  });
  it('(equal? _ \'_)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('equal?'), Symbol.for('_'), [Symbol.for('quote'), Symbol.for('_')]], false]);
  });
  it('(equal? 1 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('equal?'), 1, 1], true]);
  });
  return it('(equal? \'() \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('equal?'), [Symbol.for('quote'), []], [Symbol.for('quote'), []]], true]);
  });
});

describe('not', function (): any {
  it('(not #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('not'), false], true]);
  });
  it('(not #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('not'), true], false]);
  });
  it('(compile \'(not x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('not'), Symbol.for('x')]]], '!x;']);
  });
  it('(compile \'(not (f x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('not'), [Symbol.for('f'), Symbol.for('x')]]]], '!f(x);']);
  });
  it('(compile \'(not (= 1 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('not'), [Symbol.for('='), 1, 2]]]], '1 !== 2;']);
  });
  it('(compile \'(not (> 1 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('not'), [Symbol.for('>'), 1, 2]]]], '!(1 > 2);']);
  });
  return it('(compile \'(not (and x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('not'), [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')]]]], '!(x && y);']);
  });
});

describe('and', function (): any {
  it('(and)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('and')], true]);
  });
  it('(and #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('and'), true], true]);
  });
  it('(and #t #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('and'), true, true], true]);
  });
  it('(and #f #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('and'), false, false], false]);
  });
  it('(and #f #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('and'), false, true], false]);
  });
  it('(and #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('and'), true, false], false]);
  });
  it('(compile \'(and))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('and')]]], 'true;']);
  });
  it('(compile \'(and x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('and'), Symbol.for('x')]]], 'x;']);
  });
  it('(compile \'(and #t #t))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('and'), true, true]]], 'true && true;']);
  });
  it('(compile \'(and x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')]]], 'x && y;']);
  });
  it('(compile \'(and x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('and'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x && y && z;']);
  });
  it('(compile \'(and x y (or w z)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('and'), Symbol.for('x'), Symbol.for('y'), [Symbol.for('or'), Symbol.for('w'), Symbol.for('z')]]]], 'x && y && (w || z);']);
  });
  it('(compile \'(and x y (w z)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('and'), Symbol.for('x'), Symbol.for('y'), [Symbol.for('w'), Symbol.for('z')]]]], 'x && y && w(z);']);
  });
  return it('(compile \'(and (not (f x)) (not (g y))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('f'), Symbol.for('x')]], [Symbol.for('not'), [Symbol.for('g'), Symbol.for('y')]]]]], '!f(x) && !g(y);']);
  });
});

describe('or', function (): any {
  it('(or)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('or')], false]);
  });
  it('(or #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('or'), true], true]);
  });
  it('(or #t #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('or'), true, true], true]);
  });
  it('(or #f #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('or'), false, false], false]);
  });
  it('(or #f #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('or'), false, true], true]);
  });
  it('(or #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('or'), true, false], true]);
  });
  it('(or 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('or'), 1, 2], 1]);
  });
  it('(or #u 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('or'), undefined, 2], 2]);
  });
  it('(compile \'(or))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('or')]]], 'false;']);
  });
  it('(compile \'(or x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('or'), Symbol.for('x')]]], 'x;']);
  });
  it('(compile \'(or #t #t))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('or'), true, true]]], 'true || true;']);
  });
  it('(compile \'(or x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('or'), Symbol.for('x'), Symbol.for('y')]]], 'x || y;']);
  });
  return it('(compile \'(or x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('or'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x || y || z;']);
  });
});

describe('bitwise-and', function (): any {
  it('(compile \'(bitwise-and x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bitwise-and'), Symbol.for('x'), Symbol.for('y')]]], 'x & y;']);
  });
  it('(compile \'(bit-and x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bit-and'), Symbol.for('x'), Symbol.for('y')]]], 'x & y;']);
  });
  it('(compile \'(js/& x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/&'), Symbol.for('x'), Symbol.for('y')]]], 'x & y;']);
  });
  return it('(compile \'(js/& x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/&'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x & y & z;']);
  });
});

describe('bitwise-or', function (): any {
  it('(compile \'(bitwise-or x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bitwise-or'), Symbol.for('x'), Symbol.for('y')]]], 'x | y;']);
  });
  it('(compile \'(bit-or x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bit-or'), Symbol.for('x'), Symbol.for('y')]]], 'x | y;']);
  });
  it('(compile \'(js/| x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/|'), Symbol.for('x'), Symbol.for('y')]]], 'x | y;']);
  });
  return it('(compile \'(js/| x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/|'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x | y | z;']);
  });
});

describe('bitwise-xor', function (): any {
  it('(compile \'(bitwise-xor x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bitwise-xor'), Symbol.for('x'), Symbol.for('y')]]], 'x ^ y;']);
  });
  it('(compile \'(bit-xor x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bit-xor'), Symbol.for('x'), Symbol.for('y')]]], 'x ^ y;']);
  });
  return it('(compile \'(js/^ x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/^'), Symbol.for('x'), Symbol.for('y')]]], 'x ^ y;']);
  });
});

describe('bitwise-not', function (): any {
  it('(compile \'(bitwise-negation x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bitwise-negation'), Symbol.for('x')]]], '~x;']);
  });
  it('(compile \'(bitwise-not x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bitwise-not'), Symbol.for('x')]]], '~x;']);
  });
  it('(compile \'(bit-not x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bit-not'), Symbol.for('x')]]], '~x;']);
  });
  return it('(compile \'(js/~ x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/~'), Symbol.for('x')]]], '~x;']);
  });
});

describe('bitwise-shift-left', function (): any {
  it('(compile \'(bitwise-shift-left x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bitwise-shift-left'), Symbol.for('x'), Symbol.for('y')]]], 'x << y;']);
  });
  it('(compile \'(bit-shift-left x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bit-shift-left'), Symbol.for('x'), Symbol.for('y')]]], 'x << y;']);
  });
  return it('(compile \'(js/<< x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<<'), Symbol.for('x'), Symbol.for('y')]]], 'x << y;']);
  });
});

describe('bitwise-shift-right', function (): any {
  it('(compile \'(bitwise-shift-right x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bitwise-shift-right'), Symbol.for('x'), Symbol.for('y')]]], 'x >> y;']);
  });
  it('(compile \'(bit-shift-right x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('bit-shift-right'), Symbol.for('x'), Symbol.for('y')]]], 'x >> y;']);
  });
  return it('(compile \'(js/>> x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>>'), Symbol.for('x'), Symbol.for('y')]]], 'x >> y;']);
  });
});

describe('unsigned-bitwise-shift-right', function (): any {
  it('(compile \'(unsigned-bitwise-shift-right x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('unsigned-bitwise-shift-right'), Symbol.for('x'), Symbol.for('y')]]], 'x >>> y;']);
  });
  it('(compile \'(unsigned-bit-shift-right x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('unsigned-bit-shift-right'), Symbol.for('x'), Symbol.for('y')]]], 'x >>> y;']);
  });
  return it('(compile \'(js/>>> x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>>>'), Symbol.for('x'), Symbol.for('y')]]], 'x >>> y;']);
  });
});

describe('for', function (): any {
  it('(let ((result \'())) (for ((x \'(1 2 3))) (set! result (cons x result))) result)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('cons'), Symbol.for('x'), Symbol.for('result')]]], Symbol.for('result')], [Symbol.for('quote'), [3, 2, 1]]]);
  });
  it('((lambda () (define foo \'(1 2 3 4)) (define len (length foo)) (for ((i (range 0 len))) (pop-right! foo)) foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), [1, 2, 3, 4]]], [Symbol.for('define'), Symbol.for('len'), [Symbol.for('length'), Symbol.for('foo')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('len')]]], [Symbol.for('pop-right!'), Symbol.for('foo')]], Symbol.for('foo')]], [Symbol.for('quote'), []]]);
  });
  it('((lambda () (define foo \'(1 2 3 4)) (for ((i (range 0 (length foo)))) (pop-right! foo)) foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quote'), [1, 2, 3, 4]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('foo')]]]], [Symbol.for('pop-right!'), Symbol.for('foo')]], Symbol.for('foo')]], [Symbol.for('quote'), []]]);
  });
  it('(compile \'(for ((i (range 0 10))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]], [Symbol.for('foo')]]]], 'for (let i = 0; i < 10; i++) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 0 10 2))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 10, 2]]], [Symbol.for('foo')]]]], 'for (let i = 0; i < 10; i = i + 2) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(for ((x lst)) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('lst')]], [Symbol.for('foo')]]]], 'for (let x of lst) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(for ((x \'(1 2 3))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('foo')]]]], 'for (let x of [1, 2, 3]) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(for ((x \'(1 2 3))) (break)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('break')]]]], 'for (let x of [1, 2, 3]) {\n' +
      '  break;\n' +
      '}']);
  });
  it('(compile \'(for ((x \'(1 2 3))) (continue)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('continue')]]]], 'for (let x of [1, 2, 3]) {\n' +
      '  continue;\n' +
      '}']);
  });
  it('(compile \'(for ((x \'(1 2 3))) (let ((x 1)) (display x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('let'), [[Symbol.for('x'), 1]], [Symbol.for('display'), Symbol.for('x')]]]]], 'for (let x of [1, 2, 3]) {\n' +
      '  {\n' +
      '    let x = 1;\n' +
      '    console.log(x);\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(for ((x \'(1 2 3))) (let ((y 1)) (display x y))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('let'), [[Symbol.for('y'), 1]], [Symbol.for('display'), Symbol.for('x'), Symbol.for('y')]]]]], 'for (let x of [1, 2, 3]) {\n' +
      '  let y = 1;\n' +
      '  console.log(x, y);\n' +
      '}']);
  });
  it('(compile \'(for ((x \'(1 2 3))) (let ((y 1)) (display y)) (display x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('let'), [[Symbol.for('y'), 1]], [Symbol.for('display'), Symbol.for('y')]], [Symbol.for('display'), Symbol.for('x')]]]], 'for (let x of [1, 2, 3]) {\n' +
      '  let y = 1;\n' +
      '  console.log(y);\n' +
      '  console.log(x);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 0 len))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('len')]]], [Symbol.for('foo')]]]], 'for (let i = 0; i < len; i++) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 0 (js/length foo)))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('js/length'), Symbol.for('foo')]]]], [Symbol.for('foo')]]]], 'let _end = foo.length;\n' +
      '\n' +
      'for (let i = 0; i < _end; i++) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(for ((x \'(1 2 3))) (display x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('display'), Symbol.for('x')]]]], 'for (let x of [1, 2, 3]) {\n' +
      '  console.log(x);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 0 10))) (display x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]], [Symbol.for('display'), Symbol.for('x')]]]], 'for (let i = 0; i < 10; i++) {\n' +
      '  console.log(x);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 0 10))) (display x)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]], [Symbol.for('display'), Symbol.for('x')]]], Symbol.for(':to'), 'typescript'], 'for (let i: any = 0; i < 10; i++) {\n' +
      '  console.log(x);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 1 10 2))) (display x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, 10, 2]]], [Symbol.for('display'), Symbol.for('x')]]]], 'for (let i = 1; i < 10; i = i + 2) {\n' +
      '  console.log(x);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 10 1 -1))) (display x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 10, 1, -1]]], [Symbol.for('display'), Symbol.for('x')]]]], 'for (let i = 10; i > 1; i--) {\n' +
      '  console.log(x);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 10 1 -2))) (display x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 10, 1, -2]]], [Symbol.for('display'), Symbol.for('x')]]]], 'for (let i = 10; i > 1; i = i - 2) {\n' +
      '  console.log(x);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 0 (+ 1 1)))) (display i)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('+'), 1, 1]]]], [Symbol.for('display'), Symbol.for('i')]]]], 'let _end = 1 + 1;\n' +
      '\n' +
      'for (let i = 0; i < _end; i++) {\n' +
      '  console.log(i);\n' +
      '}']);
  });
  it('(compile \'(begin (for ((i (range 0 (+ 1 1)))) (display i)) (for ((j (range 0 (+ 2 2)))) (display j))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('+'), 1, 1]]]], [Symbol.for('display'), Symbol.for('i')]], [Symbol.for('for'), [[Symbol.for('j'), [Symbol.for('range'), 0, [Symbol.for('+'), 2, 2]]]], [Symbol.for('display'), Symbol.for('j')]]]]], 'let _end = 1 + 1;\n' +
      '\n' +
      'for (let i = 0; i < _end; i++) {\n' +
      '  console.log(i);\n' +
      '}\n' +
      '\n' +
      'let _end1 = 2 + 2;\n' +
      '\n' +
      'for (let j = 0; j < _end1; j++) {\n' +
      '  console.log(j);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range (+ 1 1) (+ 2 2)))) (display i)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 2, 2]]]], [Symbol.for('display'), Symbol.for('i')]]]], 'let _start = 1 + 1;\n' +
      '\n' +
      'let _end = 2 + 2;\n' +
      '\n' +
      'for (let i = _start; i < _end; i++) {\n' +
      '  console.log(i);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range (+ 1 1) (+ 2 2)))) (display i)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 2, 2]]]], [Symbol.for('display'), Symbol.for('i')]]], Symbol.for(':to'), 'typescript'], 'let _start: any = 1 + 1;\n' +
      '\n' +
      'let _end: any = 2 + 2;\n' +
      '\n' +
      'for (let i: any = _start; i < _end; i++) {\n' +
      '  console.log(i);\n' +
      '}']);
  });
  it('(compile \'(let ((_start 0) (_end 0)) (for ((i (range (+ 1 1) (+ 2 2)))) (display i))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [[Symbol.for('_start'), 0], [Symbol.for('_end'), 0]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 2, 2]]]], [Symbol.for('display'), Symbol.for('i')]]]], Symbol.for(':to'), 'typescript'], 'let _start: any = 0;\n' +
      '\n' +
      'let _end: any = 0;\n' +
      '\n' +
      'let _start1: any = 1 + 1;\n' +
      '\n' +
      'let _end1: any = 2 + 2;\n' +
      '\n' +
      'for (let i: any = _start1; i < _end1; i++) {\n' +
      '  console.log(i);\n' +
      '}']);
  });
  it('(compile \'(for ((i (range (+ 1 1) (+ 2 2)))) (for ((j (range (+ 3 3) (+ 4 4)))) (display j))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 2, 2]]]], [Symbol.for('for'), [[Symbol.for('j'), [Symbol.for('range'), [Symbol.for('+'), 3, 3], [Symbol.for('+'), 4, 4]]]], [Symbol.for('display'), Symbol.for('j')]]]], Symbol.for(':to'), 'typescript'], 'let _start: any = 1 + 1;\n' +
      '\n' +
      'let _end: any = 2 + 2;\n' +
      '\n' +
      'for (let i: any = _start; i < _end; i++) {\n' +
      '  let _start1: any = 3 + 3;\n' +
      '  let _end1: any = 4 + 4;\n' +
      '  for (let j: any = _start1; j < _end1; j++) {\n' +
      '    console.log(j);\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(define (foo) (for ((x \'(1 2 3))) (display x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), [Symbol.for('foo')], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]], [Symbol.for('display'), Symbol.for('x')]]]]], 'function foo() {\n' +
      '  for (let x of [1, 2, 3]) {\n' +
      '    console.log(x);\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(for ((i (range 0 10)) (j (range 0 10))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 10]], [Symbol.for('j'), [Symbol.for('range'), 0, 10]]], [Symbol.for('foo')]]]], 'for (let i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(for ((x foo) (y bar)) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('foo')], [Symbol.for('y'), Symbol.for('bar')]], [Symbol.for('foo')]]]], 'let _end = foo.length;\n' +
      '\n' +
      'let _end1 = bar.length;\n' +
      '\n' +
      'for (let i = 0, j = 0; (i < _end) && (j < _end1); i++, j++) {\n' +
      '  let x = foo[i];\n' +
      '  let y = bar[j];\n' +
      '  foo();\n' +
      '}']);
  });
  return it('(compile \'(for ((x (foo)) (y (bar))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('foo')]], [Symbol.for('y'), [Symbol.for('bar')]]], [Symbol.for('foo')]]]], 'let _val = foo();\n' +
      '\n' +
      'let _end = _val.length;\n' +
      '\n' +
      'let _val1 = bar();\n' +
      '\n' +
      'let _end1 = _val1.length;\n' +
      '\n' +
      'for (let i = 0, j = 0; (i < _end) && (j < _end1); i++, j++) {\n' +
      '  let x = _val[i];\n' +
      '  let y = _val1[j];\n' +
      '  foo();\n' +
      '}']);
  });
});

describe('for-each', function (): any {
  return it('(compile \'(for-each (lambda (x) x) lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('for-each'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], Symbol.for('lst')]]], 'lst.forEach(function (x) {\n' +
      '  return x;\n' +
      '});']);
  });
});

describe('do', function (): any {
  return it('(compile \'(do () ((not (< (length result) 3))) (display result)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('do'), [], [[Symbol.for('not'), [Symbol.for('<'), [Symbol.for('length'), Symbol.for('result')], 3]]], [Symbol.for('display'), Symbol.for('result')]]]], 'while (result.length < 3) {\n' +
      '  console.log(result);\n' +
      '}']);
  });
});

describe('get-field', function (): any {
  it('(let ((obj (js/obj "foo" "bar"))) (get-field foo obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('get-field'), Symbol.for('foo'), Symbol.for('obj')]], 'bar']);
  });
  it('(compile \'(get-field foo obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('get-field'), Symbol.for('foo'), Symbol.for('obj')]]], 'obj.foo;']);
  });
  it('(compile \'(get-field foo-bar obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('get-field'), Symbol.for('foo-bar'), Symbol.for('obj')]]], 'obj.fooBar;']);
  });
  it('(compile \'(get-field "foo" obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('get-field'), 'foo', Symbol.for('obj')]]], 'obj[\'foo\'];']);
  });
  it('(compile \'(get-field "foo-bar" obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('get-field'), 'foo-bar', Symbol.for('obj')]]], 'obj[\'foo-bar\'];']);
  });
  it('(compile \'(get-field (foo-bar) obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('get-field'), [Symbol.for('foo-bar')], Symbol.for('obj')]]], 'obj[fooBar()];']);
  });
  return it('(compile \'(get-field length arr))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('arr')]]], 'arr.length;']);
  });
});

describe('set-field!', function (): any {
  it('(let ((obj (js/obj))) (set-field! foo-bar obj "baz") (get-field foo-bar obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj')]]], [Symbol.for('set-field!'), Symbol.for('foo-bar'), Symbol.for('obj'), 'baz'], [Symbol.for('get-field'), Symbol.for('foo-bar'), Symbol.for('obj')]], 'baz']);
  });
  it('(compile \'(set-field! foo-bar obj "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set-field!'), Symbol.for('foo-bar'), Symbol.for('obj'), 'baz']]], 'obj.fooBar = \'baz\';']);
  });
  it('(compile \'(set-field! \'foo-bar obj "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set-field!'), [Symbol.for('quote'), Symbol.for('foo-bar')], Symbol.for('obj'), 'baz']]], 'obj.fooBar = \'baz\';']);
  });
  it('(compile \'(set-field! :foo-bar obj "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set-field!'), Symbol.for(':foo-bar'), Symbol.for('obj'), 'baz']]], 'obj.fooBar = \'baz\';']);
  });
  it('(compile \'(set-field! "foo-bar" obj "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set-field!'), 'foo-bar', Symbol.for('obj'), 'baz']]], 'obj[\'foo-bar\'] = \'baz\';']);
  });
  it('(compile \'(set-field! (foo-bar) obj "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set-field!'), [Symbol.for('foo-bar')], Symbol.for('obj'), 'baz']]], 'obj[fooBar()] = \'baz\';']);
  });
  return it('(compile \'(set-field! def-method generic-function (lambda (arglist function-definition) (let ((entry (list arglist function-definition))) (push! (get-field methods generic-function) entry) generic-function))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set-field!'), Symbol.for('def-method'), Symbol.for('generic-function'), [Symbol.for('lambda'), [Symbol.for('arglist'), Symbol.for('function-definition')], [Symbol.for('let'), [[Symbol.for('entry'), [Symbol.for('list'), Symbol.for('arglist'), Symbol.for('function-definition')]]], [Symbol.for('push!'), [Symbol.for('get-field'), Symbol.for('methods'), Symbol.for('generic-function')], Symbol.for('entry')], Symbol.for('generic-function')]]]]], 'genericFunction.defMethod = function (arglist, functionDefinition) {\n' +
      '  let entry = [arglist, functionDefinition];\n' +
      '  genericFunction.methods.unshift(entry);\n' +
      '  return genericFunction;\n' +
      '};']);
  });
});

describe('field-bound?', function (): any {
  it('(let ((obj (js/obj "foo" "bar"))) (field-bound? foo obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('field-bound?'), Symbol.for('foo'), Symbol.for('obj')]], true]);
  });
  it('(compile \'(begin (define foo (js/obj)) (define bar (field-bound? baz foo))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('js/obj')]], [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('field-bound?'), Symbol.for('baz'), Symbol.for('foo')]]]]], 'let foo = {};\n' +
      '\n' +
      'let bar = foo && (\'baz\' in foo);']);
  });
  return it('(compile \'(begin (define foo (js/obj)) (define bar (field-bound? baz-baz foo))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('js/obj')]], [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('field-bound?'), Symbol.for('baz-baz'), Symbol.for('foo')]]]]], 'let foo = {};\n' +
      '\n' +
      'let bar = foo && (\'bazBaz\' in foo);']);
  });
});

describe('send', function (): any {
  it('(let ((obj (js/obj "add" (lambda (x y) (+ x y))))) (send obj add 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'add', [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]]]]], [Symbol.for('send'), Symbol.for('obj'), Symbol.for('add'), 1, 1]], 2]);
  });
  it('(let ((obj (make-hash \'(("foo" . "foo"))))) (send obj has "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]]]]], [Symbol.for('send'), Symbol.for('obj'), Symbol.for('has'), 'foo']], true]);
  });
  it('(let ((obj (make-hash \'(("foo" . "foo"))))) (send obj has "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]]]]], [Symbol.for('send'), Symbol.for('obj'), Symbol.for('has'), 'bar']], false]);
  });
  it('(compile \'(send obj m arg))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('send'), Symbol.for('obj'), Symbol.for('m'), Symbol.for('arg')]]], 'obj.m(arg);']);
  });
  return it('(compile \'(send map get "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('send'), Symbol.for('map'), Symbol.for('get'), 'foo']]], 'map.get(\'foo\');']);
  });
});

describe('send/apply', function (): any {
  it('(let ((obj (make-hash \'(("foo" . "foo"))))) (send/apply obj has \'("foo")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'foo']]]]]], [Symbol.for('send/apply'), Symbol.for('obj'), Symbol.for('has'), [Symbol.for('quote'), ['foo']]]], true]);
  });
  it('(compile \'(send/apply obj m args))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('send/apply'), Symbol.for('obj'), Symbol.for('m'), Symbol.for('args')]]], 'obj.m(...args);']);
  });
  it('(compile \'(send/apply map get foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('send/apply'), Symbol.for('map'), Symbol.for('get'), Symbol.for('foo')]]], 'map.get(...foo);']);
  });
  return it('(compile \'(send/apply map get \'("foo")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('send/apply'), Symbol.for('map'), Symbol.for('get'), [Symbol.for('quote'), ['foo']]]]], 'map.get(\'foo\');']);
  });
});

describe('is-a?', function (): any {
  it('(is-a? (new Map) Map)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('is-a?'), [Symbol.for('new'), Symbol.for('Map')], Symbol.for('Map')], true]);
  });
  return it('(compile \'(is-a? x Foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Foo')]]], 'x instanceof Foo;']);
  });
});

describe('module', function (): any {
  it('(module foo bar (+ 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('module'), Symbol.for('foo'), Symbol.for('bar'), [Symbol.for('+'), 1, 1]], 2]);
  });
  it('(compile \'(module m scheme (define (I x) x) (define (K x y) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('define'), [Symbol.for('K'), Symbol.for('x'), Symbol.for('y')], Symbol.for('x')]]]], 'function I(x) {\n' +
      '  return x;\n' +
      '}\n' +
      '\n' +
      'function K(x, y) {\n' +
      '  return x;\n' +
      '}']);
  });
  it('(compile \'(module m scheme (define I (lambda (x) x)) (define K (lambda (x y) x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('I'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]], [Symbol.for('define'), Symbol.for('K'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('x')]]]]], 'let I = function (x) {\n' +
      '  return x;\n' +
      '};\n' +
      '\n' +
      'let K = function (x, y) {\n' +
      '  return x;\n' +
      '};']);
  });
  it('(compile \'(module m scheme (define (foo length) length)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('length')], Symbol.for('length')]]], Symbol.for(':to'), 'typescript'], 'function foo(length: any): any {\n' +
      '  return length;\n' +
      '}']);
  });
  it('(compile \'(module m scheme (define (foo (length : Number)) : Number length)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo'), [Symbol.for('length'), Symbol.for(':'), Symbol.for('Number')]], Symbol.for(':'), Symbol.for('Number'), Symbol.for('length')]]], Symbol.for(':to'), 'typescript'], 'function foo(length: number): number {\n' +
      '  return length;\n' +
      '}']);
  });
  it('(compile \'(module m scheme (define truish #t) (define falsy (not truish))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('truish'), true], [Symbol.for('define'), Symbol.for('falsy'), [Symbol.for('not'), Symbol.for('truish')]]]]], 'let truish = true;\n' +
      '\n' +
      'let falsy = !truish;']);
  });
  it('(compile \'(module m scheme (require (only-in "foo" and or)) (and x (or y z))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('require'), [Symbol.for('only-in'), 'foo', Symbol.for('and'), Symbol.for('or')]], [Symbol.for('and'), Symbol.for('x'), [Symbol.for('or'), Symbol.for('y'), Symbol.for('z')]]]]], 'import {\n' +
      '  and,\n' +
      '  or\n' +
      '} from \'foo\';\n' +
      '\n' +
      'and(x, or(y, z));']);
  });
  it('(compile \'(module m lisp (define x 1) (define y 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), Symbol.for('y'), 2]]]], 'let x = 1;\n' +
      '\n' +
      'let y = 2;']);
  });
  it('(compile \'(module m lisp (define (js_ str) (js/eval str))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('js_'), Symbol.for('str')], [Symbol.for('js/eval'), Symbol.for('str')]]]]], 'function js_(str) {\n' +
      '  return eval(str);\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-fn foldl f v l) (foldl f v l))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-fn'), Symbol.for('foldl'), Symbol.for('f'), Symbol.for('v'), Symbol.for('l')], [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('v'), Symbol.for('l')]]]]], 'function myFn(foldl, f, v, l) {\n' +
      '  return foldl(f, v, l);\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-foldl-obj obj f v l) (.foldl obj f v l))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-foldl-obj'), Symbol.for('obj'), Symbol.for('f'), Symbol.for('v'), Symbol.for('l')], [Symbol.for('.foldl'), Symbol.for('obj'), Symbol.for('f'), Symbol.for('v'), Symbol.for('l')]]]]], 'function myFoldlObj(obj, f, v, l) {\n' +
      '  return obj.foldl(f, v, l);\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define-class Foo () (define/public (foldl f v l) l))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('v'), Symbol.for('l')], Symbol.for('l')]]]]], 'class Foo {\n' +
      '  foldl(f, v, l) {\n' +
      '    return l;\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-pop lst x) (pop! lst x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-pop'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('pop!'), Symbol.for('lst'), Symbol.for('x')]]]]], 'function myPop(lst, x) {\n' +
      '  return lst.shift();\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-pop-2 lst x) (pop! (append lst) x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-pop-2'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('pop!'), [Symbol.for('append'), Symbol.for('lst')], Symbol.for('x')]]]]], 'function myPop2(lst, x) {\n' +
      '  return [...lst].shift();\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-pop-right lst x) (pop-right! lst x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-pop-right'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('pop-right!'), Symbol.for('lst'), Symbol.for('x')]]]]], 'function myPopRight(lst, x) {\n' +
      '  return lst.pop();\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-pop-right-2 lst x) (pop-right! (append lst) x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-pop-right-2'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('pop-right!'), [Symbol.for('append'), Symbol.for('lst')], Symbol.for('x')]]]]], 'function myPopRight2(lst, x) {\n' +
      '  return [...lst].pop();\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-push lst x) (push! lst x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-push'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('push!'), Symbol.for('lst'), Symbol.for('x')]]]]], 'function myPush(lst, x) {\n' +
      '  lst.unshift(x);\n' +
      '  return lst;\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-push-2 lst x) (push! (append lst) x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-push-2'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('push!'), [Symbol.for('append'), Symbol.for('lst')], Symbol.for('x')]]]]], 'function myPush2(lst, x) {\n' +
      '  let arr = [...lst];\n' +
      '  arr.unshift(x);\n' +
      '  return arr;\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-push-3 lst x) (push! lst x) lst)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-push-3'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('push!'), Symbol.for('lst'), Symbol.for('x')], Symbol.for('lst')]]]], 'function myPush3(lst, x) {\n' +
      '  lst.unshift(x);\n' +
      '  return lst;\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-push-right lst x) (push-right! lst x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-push-right'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('push-right!'), Symbol.for('lst'), Symbol.for('x')]]]]], 'function myPushRight(lst, x) {\n' +
      '  lst.push(x);\n' +
      '  return lst;\n' +
      '}']);
  });
  it('(compile \'(module m lisp (define (my-push-right-2 lst x) (push-right! (append lst) x))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-push-right-2'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('push-right!'), [Symbol.for('append'), Symbol.for('lst')], Symbol.for('x')]]]]], 'function myPushRight2(lst, x) {\n' +
      '  let arr = [...lst];\n' +
      '  arr.push(x);\n' +
      '  return arr;\n' +
      '}']);
  });
  return it('(compile \'(module m lisp (define (my-push-right-3 lst x) (push-right! lst x) lst)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('lisp'), [Symbol.for('define'), [Symbol.for('my-push-right-3'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('push-right!'), Symbol.for('lst'), Symbol.for('x')], Symbol.for('lst')]]]], 'function myPushRight3(lst, x) {\n' +
      '  lst.push(x);\n' +
      '  return lst;\n' +
      '}']);
  });
});

describe('call/cc', function (): any {
  it('(+ 5 (call/cc (lambda (x) (* 10 3))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 5, [Symbol.for('call/cc'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('*'), 10, 3]]]], 35]);
  });
  it('(+ 5 (call/cc (lambda (x) (* 10 (x 3)))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 5, [Symbol.for('call/cc'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('*'), 10, [Symbol.for('x'), 3]]]]], 8]);
  });
  it('(+ 5 (call/cc (lambda (x) (x 10) 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 5, [Symbol.for('call/cc'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('x'), 10], 3]]], 15]);
  });
  it('(+ 5 (call/cc (lambda (x) (x 10) (error "error"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 5, [Symbol.for('call/cc'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('x'), 10], [Symbol.for('error'), 'error']]]], 15]);
  });
  return it('(try ... (+ 5 (call/cc (lambda (x) (error ...)))) ...)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), 0]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('+'), 5, [Symbol.for('call/cc'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('error'), 'error']]]]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e')]], Symbol.for('result')], 0]);
  });
});

describe('define-values', function (): any {
  it('((lambda () (define-values (x y) (values 1 2))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]]]], undefined]);
  });
  it('((lambda () (define-values (x y) (values 1 2)) (list x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]]], [Symbol.for('quote'), [1, 2]]]);
  });
  it('((lambda () (define-values (x y) (values 1 2)) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]], Symbol.for('x')]], 1]);
  });
  it('(compile \'(define-values value (foo bar baz)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-values'), Symbol.for('value'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]]], 'let value = foo(bar, baz);']);
  });
  it('(compile \'(define-values (value) (foo bar baz)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-values'), [Symbol.for('value')], [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]]], 'let [value] = foo(bar, baz);']);
  });
  it('(compile \'(define-values (value) (foo bar baz)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-values'), [Symbol.for('value')], [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], Symbol.for(':to'), 'typescript'], 'let [value]: any[] = foo(bar, baz);']);
  });
  it('(compile \'(define-values (#f #f value) (foo bar baz)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-values'), [false, false, Symbol.for('value')], [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], Symbol.for(':to'), 'typescript'], 'let [, , value]: any[] = foo(bar, baz);']);
  });
  it('(compile \'(define-values (_ _ value) (foo bar baz)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('_'), Symbol.for('value')], [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], Symbol.for(':to'), 'typescript'], 'let [, , value]: any[] = foo(bar, baz);']);
  });
  it('(compile \'(define-values (_ __ value) :hole-marker __ (foo bar baz)) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('__'), Symbol.for('value')], Symbol.for(':hole-marker'), Symbol.for('__'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]], Symbol.for(':to'), 'typescript'], 'let [_, , value]: any[] = foo(bar, baz);']);
  });
  return it('(compile \'(module m scheme (define (foo) (define xs \'(1 2 3 4)) (define-values (x . rest) xs) (append rest \'(5)))) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo')], [Symbol.for('define'), Symbol.for('xs'), [Symbol.for('quote'), [1, 2, 3, 4]]], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('rest')], Symbol.for('xs')], [Symbol.for('append'), Symbol.for('rest'), [Symbol.for('quote'), [5]]]]]], Symbol.for(':to'), 'typescript'], 'function foo(): any {\n' +
      '  let xs: any = [1, 2, 3, 4];\n' +
      '  let [x, ...rest]: any[] = xs;\n' +
      '  return [...rest, 5];\n' +
      '}']);
  });
});

describe('set!', function (): any {
  it('(compile \'(set! x 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!'), Symbol.for('x'), 1]]], 'x = 1;']);
  });
  it('(compile \'(set! (aref args 0) 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!'), [Symbol.for('aref'), Symbol.for('args'), 0], 1]]], 'args[0] = 1;']);
  });
  it('(compile \'(set! x (add1 x)) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('add1'), Symbol.for('x')]]], Symbol.for(':as'), 'expression'], '++x']);
  });
  it('(compile \'(set! x (sub1 x)) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('sub1'), Symbol.for('x')]]], Symbol.for(':as'), 'expression'], '--x']);
  });
  it('(compile \'(set! x (+ x 1)) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('+'), Symbol.for('x'), 1]]], Symbol.for(':as'), 'expression'], '++x']);
  });
  it('(compile \'(set! x (+ x 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('+'), Symbol.for('x'), 1]]]], 'x++;']);
  });
  return it('(compile \'(set! x (+ x 1)) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('+'), Symbol.for('x'), 1]]], Symbol.for(':as'), 'return'], 'return ++x;']);
  });
});

describe('set!-values', function (): any {
  it('(let (x y) (set!-values (x y) (values 1 2)) x)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('set!-values'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]], Symbol.for('x')], 1]);
  });
  it('(let (x y) (set!-values (x y) (values 1 2)) (list x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('set!-values'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('quote'), [1, 2]]]);
  });
  it('(compile \'(set!-values (x y) (values 1 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!-values'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('values'), 1, 2]]]], '[x, y] = [1, 2];']);
  });
  it('(compile \'(set!-values (value) (foo bar baz)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!-values'), [Symbol.for('value')], [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]]], '[value] = foo(bar, baz);']);
  });
  it('(compile \'(set!-values (_ value) (foo bar baz)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!-values'), [Symbol.for('_'), Symbol.for('value')], [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]]], '[, value] = foo(bar, baz);']);
  });
  return it('(compile \'(set!-values (_ __ value) :hole-marker __ (foo bar baz)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('set!-values'), [Symbol.for('_'), Symbol.for('__'), Symbol.for('value')], Symbol.for(':hole-marker'), Symbol.for('__'), [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')]]]], '[_, , value] = foo(bar, baz);']);
  });
});

describe('hash', function (): any {
  it('(hash)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash')], [Symbol.for('new'), Symbol.for('Map')]]);
  });
  it('(hash \'(("foo" . "bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]], [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('quote'), [['foo', 'bar']]]]]);
  });
  it('(compile \'(hash))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash')]]], 'new Map();']);
  });
  return it('(compile \'(hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]]]], 'new Map([[\'foo\', \'bar\']]);']);
  });
});

describe('make-hash', function (): any {
  it('(make-hash)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('make-hash')], [Symbol.for('new'), Symbol.for('Map')]]);
  });
  it('(make-hash \'(("foo" . "bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]], [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('quote'), [['foo', 'bar']]]]]);
  });
  it('(apply new make-hash \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('apply'), Symbol.for('new'), Symbol.for('make-hash'), [Symbol.for('quote'), []]], [Symbol.for('new'), Symbol.for('Map')]]);
  });
  it('(compile \'(make-hash))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('make-hash')]]], 'new Map();']);
  });
  it('(compile \'(make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]]]], 'new Map([[\'foo\', \'bar\']]);']);
  });
  it('(compile \'(make-hash \'(("foo" . "bar") ("baz" . "quux"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar'], ['baz', Symbol.for('.'), 'quux']]]]]], 'new Map([[\'foo\', \'bar\'], [\'baz\', \'quux\']]);']);
  });
  it('(compile \'(make-hash \'(("foo" "bar") ("baz" "quux"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', 'bar'], ['baz', 'quux']]]]]], 'new Map([[\'foo\', [\'bar\']], [\'baz\', [\'quux\']]]);']);
  });
  it('(compile \'(make-hash `(("foo" . "bar") ("baz" . "quux"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('make-hash'), [Symbol.for('quasiquote'), [['foo', Symbol.for('.'), 'bar'], ['baz', Symbol.for('.'), 'quux']]]]]], 'new Map([[\'foo\', \'bar\'], [\'baz\', \'quux\']]);']);
  });
  it('(compile \'(make-hash `(("foo" . "bar") ("baz" . "quux") ,@(hash->list xyzzy))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('make-hash'), [Symbol.for('quasiquote'), [['foo', Symbol.for('.'), 'bar'], ['baz', Symbol.for('.'), 'quux'], [Symbol.for('unquote-splicing'), [Symbol.for('hash->list'), Symbol.for('xyzzy')]]]]]]], 'new Map([[\'foo\', \'bar\'], [\'baz\', \'quux\'], ...xyzzy.entries()]);']);
  });
  return it('(compile \'(make-hash `(("foo" "bar") ("baz" "quux"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('make-hash'), [Symbol.for('quasiquote'), [['foo', 'bar'], ['baz', 'quux']]]]]], 'new Map([[\'foo\', [\'bar\']], [\'baz\', [\'quux\']]]);']);
  });
});

describe('hash?', function (): any {
  it('(hash? (make-hash))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash?'), [Symbol.for('make-hash')]], true]);
  });
  it('(hash? 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash?'), 0], false]);
  });
  return it('(compile \'(hash? x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash?'), Symbol.for('x')]]], 'x instanceof Map;']);
  });
});

describe('hash-clear', function (): any {
  return it('(hash-clear (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash-clear'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]]], [Symbol.for('new'), Symbol.for('Map')]]);
  });
});

describe('hash-clear!', function (): any {
  it('(let ((ht (make-hash \'(("foo" . "bar"))))) (hash-clear! ht) ht)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('ht'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]]]], [Symbol.for('hash-clear!'), Symbol.for('ht')], Symbol.for('ht')], [Symbol.for('new'), Symbol.for('Map')]]);
  });
  return it('(compile \'(hash-clear! x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash-clear!'), Symbol.for('x')]]], 'x.clear();']);
  });
});

describe('hash-copy', function (): any {
  it('(hash-copy (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash-copy'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]]], [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('quote'), [['foo', 'bar']]]]]);
  });
  return it('(compile \'(hash-copy x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash-copy'), Symbol.for('x')]]], 'new Map(x);']);
  });
});

describe('hash-keys', function (): any {
  it('(hash-keys (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash-keys'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]]], [Symbol.for('quote'), ['foo']]]);
  });
  return it('(compile \'(hash-keys x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash-keys'), Symbol.for('x')]]], '[...x.keys()];']);
  });
});

describe('hash-values', function (): any {
  it('(hash-values (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash-values'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]]], [Symbol.for('quote'), ['bar']]]);
  });
  return it('(compile \'(hash-values x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash-values'), Symbol.for('x')]]], '[...x.values()];']);
  });
});

describe('hash->list', function (): any {
  return it('(hash->list (make-hash \'(("foo" . "bar"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash->list'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]]], [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]]);
  });
});

describe('hash-set', function (): any {
  return it('(hash-set (make-hash) "foo" "bar")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash-set'), [Symbol.for('make-hash')], 'foo', 'bar'], [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('quote'), [['foo', 'bar']]]]]);
  });
});

describe('hash-set!', function (): any {
  it('(let ((ht (make-hash))) (hash-set! ht "foo" "bar") ht)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('ht'), [Symbol.for('make-hash')]]], [Symbol.for('hash-set!'), Symbol.for('ht'), 'foo', 'bar'], Symbol.for('ht')], [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('quote'), [['foo', 'bar']]]]]);
  });
  return it('(compile \'(hash-set! ht key val))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash-set!'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('val')]]], 'ht.set(key, val);']);
  });
});

describe('hash-ref', function (): any {
  it('(hash-ref (make-hash \'(("foo" . "bar"))) "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash-ref'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]], 'foo'], 'bar']);
  });
  it('(hash-ref (make-hash) "quux" #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash-ref'), [Symbol.for('make-hash')], 'quux', false], false]);
  });
  return it('(compile \'(hash-ref ht "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash-ref'), Symbol.for('ht'), 'foo']]], 'ht.get(\'foo\');']);
  });
});

describe('hash-has-key?', function (): any {
  it('(hash-has-key? (make-hash \'(("foo" . "bar"))) "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash-has-key?'), [Symbol.for('make-hash'), [Symbol.for('quote'), [['foo', Symbol.for('.'), 'bar']]]], 'foo'], true]);
  });
  it('(hash-has-key? (make-hash) "quux")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('hash-has-key?'), [Symbol.for('make-hash')], 'quux'], false]);
  });
  return it('(compile \'(hash-has-key? ht "quux"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('hash-has-key?'), Symbol.for('ht'), 'quux']]], 'ht.has(\'quux\');']);
  });
});

describe('+', function (): any {
  it('(+)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+')], 0]);
  });
  it('(+ 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 1], 1]);
  });
  it('(+ 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 1, 2], 3]);
  });
  it('(+ 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 2, 2], 4]);
  });
  it('(+ 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 1, 2, 3], 6]);
  });
  it('(+ 1 2 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), 1, 2, 4], 7]);
  });
  it('(+ (+ 1 1) (+ 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('+'), [Symbol.for('+'), 1, 1], [Symbol.for('+'), 1, 1]], 4]);
  });
  it('(let ((x 2)) (+ x x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 2]], [Symbol.for('+'), Symbol.for('x'), Symbol.for('x')]], 4]);
  });
  it('(apply + \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('apply'), Symbol.for('+'), [Symbol.for('quote'), [1, 2]]], 3]);
  });
  it('(compile \'(+ 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('+'), 1]]], '1;']);
  });
  it('(compile \'(+ 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('+'), 1, 1]]], '1 + 1;']);
  });
  it('(compile \'(+ 1 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('+'), 1, 1, 1]]], '1 + 1 + 1;']);
  });
  it('(compile \'(+ x 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('+'), Symbol.for('x'), 1]]], 'x + 1;']);
  });
  return it('(compile \'(+ x 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('+'), Symbol.for('x'), 1, 2]]], 'x + 1 + 2;']);
  });
});

describe('-', function (): any {
  it('(-)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('-')], 0]);
  });
  it('(- 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('-'), 1], -1]);
  });
  it('(- 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('-'), 1, 2], -1]);
  });
  it('(- 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('-'), 1, 2, 3], -4]);
  });
  it('(- 1 2 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('-'), 1, 2, 4], -5]);
  });
  it('(compile \'(- 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('-'), 1]]], '-1;']);
  });
  it('(compile \'(- 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('-'), 1, 1]]], '1 - 1;']);
  });
  it('(compile \'(- 1 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('-'), 1, 1, 1]]], '1 - 1 - 1;']);
  });
  it('(compile \'(- x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('-'), Symbol.for('x')]]], '-x;']);
  });
  it('(compile \'(- x 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('-'), Symbol.for('x'), 1]]], 'x - 1;']);
  });
  return it('(compile \'(- x 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('-'), Symbol.for('x'), 1, 2]]], 'x - 1 - 2;']);
  });
});

describe('*', function (): any {
  it('(*)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('*')], 1]);
  });
  it('(* 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('*'), 1], 1]);
  });
  it('(* 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('*'), 1, 2], 2]);
  });
  it('(* 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('*'), 1, 2, 3], 6]);
  });
  it('(* 1 2 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('*'), 1, 2, 4], 8]);
  });
  it('(compile \'(* 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('*'), 1, 1]]], '1 * 1;']);
  });
  return it('(compile \'(* 1 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('*'), 1, 1, 1]]], '1 * 1 * 1;']);
  });
});

describe('/', function (): any {
  it('(/)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('/')], undefined]);
  });
  it('(/ 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('/'), 1], 1]);
  });
  it('(/ 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('/'), 1, 2], 0.5]);
  });
  it('(/ 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('/'), 1, 2, 3], [Symbol.for('/'), 1, 2, 3]]);
  });
  it('(/ 1 2 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('/'), 1, 2, 4], 0.125]);
  });
  it('(compile \'(/ 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('/'), 1, 2]]], '1 / 2;']);
  });
  return it('(compile \'(/ 1 2 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('/'), 1, 2, 4]]], '1 / 2 / 4;']);
  });
});

describe('<', function (): any {
  it('(< 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<'), 1], true]);
  });
  it('(< 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<'), 1, 2], true]);
  });
  it('(< 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<'), 2, 1], false]);
  });
  it('(< 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<'), 1, 2, 3], true]);
  });
  it('(< 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<'), 2, 1, 3], false]);
  });
  it('(< 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<'), 1, 3, 2], false]);
  });
  it('(funcall < 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<'), 1, 2], true]);
  });
  it('(funcall < 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<'), 2, 1], false]);
  });
  it('(funcall < 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<'), 1, 2, 3], true]);
  });
  it('(funcall < 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<'), 2, 1, 3], false]);
  });
  it('(funcall < 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<'), 1, 3, 2], false]);
  });
  it('(compile \'(< 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('<'), 1]]], 'true;']);
  });
  it('(compile \'(< 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('<'), 1, 2]]], '1 < 2;']);
  });
  it('(compile \'(< x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('<'), Symbol.for('x'), Symbol.for('y')]]], 'x < y;']);
  });
  it('(compile \'(< 1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('<'), 1, 2, 3]]], '(1 < 2) && (2 < 3);']);
  });
  return it('(compile \'(< x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('<'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x < y) && (y < z);']);
  });
});

describe('<=', function (): any {
  it('(<= 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<='), 1, 2], true]);
  });
  it('(<= 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<='), 2, 1], false]);
  });
  it('(<= 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<='), 1, 2, 3], true]);
  });
  it('(<= 1 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<='), 1, 1, 2], true]);
  });
  it('(<= 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<='), 2, 1, 3], false]);
  });
  it('(<= 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('<='), 1, 3, 2], false]);
  });
  it('(funcall <= 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<='), 1, 2], true]);
  });
  it('(funcall <= 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<='), 2, 1], false]);
  });
  it('(funcall <= 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<='), 1, 2, 3], true]);
  });
  it('(funcall <= 1 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<='), 1, 1, 2], true]);
  });
  it('(funcall <= 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<='), 2, 1, 3], false]);
  });
  it('(funcall <= 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('<='), 1, 3, 2], false]);
  });
  it('(compile \'(<= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('<='), Symbol.for('x'), Symbol.for('y')]]], 'x <= y;']);
  });
  return it('(compile \'(<= x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('<='), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x <= y) && (y <= z);']);
  });
});

describe('>', function (): any {
  it('(> 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>'), 1], true]);
  });
  it('(> 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>'), 2, 1], true]);
  });
  it('(> 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>'), 1, 2], false]);
  });
  it('(> 3 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>'), 3, 2, 1], true]);
  });
  it('(> 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>'), 1, 2, 3], false]);
  });
  it('(> 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>'), 2, 1, 3], false]);
  });
  it('(> 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>'), 1, 3, 2], false]);
  });
  it('(funcall > 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>'), 2, 1], true]);
  });
  it('(funcall > 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>'), 1, 2], false]);
  });
  it('(funcall > 3 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>'), 3, 2, 1], true]);
  });
  it('(funcall > 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>'), 1, 2, 3], false]);
  });
  it('(funcall > 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>'), 2, 1, 3], false]);
  });
  it('(funcall > 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>'), 1, 3, 2], false]);
  });
  it('(compile \'(> 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('>'), 1]]], 'true;']);
  });
  it('(compile \'(> 2 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('>'), 2, 1]]], '2 > 1;']);
  });
  it('(compile \'(> x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('>'), Symbol.for('x'), Symbol.for('y')]]], 'x > y;']);
  });
  it('(compile \'(> 3 2 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('>'), 3, 2, 1]]], '(3 > 2) && (2 > 1);']);
  });
  return it('(compile \'(> x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('>'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x > y) && (y > z);']);
  });
});

describe('>=', function (): any {
  it('(>= 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>='), 1], true]);
  });
  it('(>= 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>='), 2, 1], true]);
  });
  it('(>= 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>='), 2, 2], true]);
  });
  it('(>= 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>='), 1, 2], false]);
  });
  it('(>= 3 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>='), 3, 2, 1], true]);
  });
  it('(>= 3 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>='), 3, 2, 2], true]);
  });
  it('(>= 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>='), 1, 2, 3], false]);
  });
  it('(>= 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>='), 2, 1, 3], false]);
  });
  it('(>= 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('>='), 1, 3, 2], false]);
  });
  it('(funcall >= 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>='), 2, 1], true]);
  });
  it('(funcall >= 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>='), 2, 2], true]);
  });
  it('(funcall >= 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>='), 1, 2], false]);
  });
  it('(funcall >= 3 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>='), 3, 2, 1], true]);
  });
  it('(funcall >= 3 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>='), 3, 2, 2], true]);
  });
  it('(funcall >= 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>='), 1, 2, 3], false]);
  });
  it('(funcall >= 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>='), 2, 1, 3], false]);
  });
  it('(funcall >= 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('>='), 1, 3, 2], false]);
  });
  it('(compile \'(>= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('>='), Symbol.for('x'), Symbol.for('y')]]], 'x >= y;']);
  });
  return it('(compile \'(>= x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('>='), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x >= y) && (y >= z);']);
  });
});

describe('mod', function (): any {
  return it('(compile \'(mod x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('mod'), Symbol.for('x'), Symbol.for('y')]]], 'x % y;']);
  });
});

describe('abs', function (): any {
  it('(abs 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('abs'), 1], 1]);
  });
  it('(abs -1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('abs'), -1], 1]);
  });
  return it('(compile \'(abs x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('abs'), Symbol.for('x')]]], 'Math.abs(x);']);
  });
});

describe('range', function (): any {
  it('(range 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('range'), 1, 2], [Symbol.for('quote'), [1]]]);
  });
  it('(range 10)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('range'), 10], [Symbol.for('quote'), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]]);
  });
  it('(range 10 20)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('range'), 10, 20], [Symbol.for('quote'), [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]]]);
  });
  it('(range 20 40 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('range'), 20, 40, 2], [Symbol.for('quote'), [20, 22, 24, 26, 28, 30, 32, 34, 36, 38]]]);
  });
  it('(range 20 10 -1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('range'), 20, 10, -1], [Symbol.for('quote'), [20, 19, 18, 17, 16, 15, 14, 13, 12, 11]]]);
  });
  return it('(range 10 15 1.5)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('range'), 10, 15, 1.5], [Symbol.for('quote'), [10, 11.5, 13, 14.5]]]);
  });
});

describe('member', function (): any {
  it('(member 2 \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('member'), 2, [Symbol.for('quote'), [1, 2, 3, 4]]], [Symbol.for('quote'), [2, 3, 4]]]);
  });
  it('(member 9 \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('member'), 9, [Symbol.for('quote'), [1, 2, 3, 4]]], false]);
  });
  return it('(member 5 \'(3 5 1 7 2 9) (lambda (x y) (< x y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('member'), 5, [Symbol.for('quote'), [3, 5, 1, 7, 2, 9]], [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('<'), Symbol.for('x'), Symbol.for('y')]]], [Symbol.for('quote'), [7, 2, 9]]]);
  });
});

describe('take', function (): any {
  it('(take \'(1 2 3) 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3]], 0], [Symbol.for('quote'), []]]);
  });
  it('(take \'(1 2 3) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3]], 1], [Symbol.for('quote'), [1]]]);
  });
  it('(take \'(1 2 3) 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3]], 2], [Symbol.for('quote'), [1, 2]]]);
  });
  it('(take \'(1 2 3) 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3]], 3], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(funcall take \'(1 2 3) 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3]], 0], [Symbol.for('quote'), []]]);
  });
  it('(funcall take \'(1 2 3) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3]], 1], [Symbol.for('quote'), [1]]]);
  });
  it('(funcall take \'(1 2 3) 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3]], 2], [Symbol.for('quote'), [1, 2]]]);
  });
  it('(funcall take \'(1 2 3) 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('take'), [Symbol.for('quote'), [1, 2, 3]], 3], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(compile \'(take lst 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('take'), Symbol.for('lst'), 0]]], '[];']);
  });
  it('(compile \'(take lst 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('take'), Symbol.for('lst'), 1]]], 'lst.slice(0, -(lst.length - 1) || undefined);']);
  });
  it('(compile \'(define x (take lst 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('take'), Symbol.for('lst'), 1]]]], 'let x = lst.slice(0, -(lst.length - 1) || undefined);']);
  });
  it('(compile \'(let ((n 1)) (take lst n)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('let'), [[Symbol.for('n'), 1]], [Symbol.for('take'), Symbol.for('lst'), Symbol.for('n')]]]], 'let n = 1;\n' +
      '\n' +
      'lst.slice(0, -(lst.length - n) || undefined);']);
  });
  it('(compile \'(module m scheme (let ((n 1)) (take lst n))) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('let'), [[Symbol.for('n'), 1]], [Symbol.for('take'), Symbol.for('lst'), Symbol.for('n')]]]], Symbol.for(':fdottedlists'), false], 'let n = 1;\n' +
      '\n' +
      'lst.slice(0, -(lst.length - n) || undefined);']);
  });
  return it('(compile \'(module m scheme (let ((n 1)) (take lst n))) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('let'), [[Symbol.for('n'), 1]], [Symbol.for('take'), Symbol.for('lst'), Symbol.for('n')]]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  take\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'let n = 1;\n' +
      '\n' +
      'take(lst, n);']);
  });
});

describe('drop', function (): any {
  it('(drop \'(1 2 3 4) 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('drop'), [Symbol.for('quote'), [1, 2, 3, 4]], 0], [Symbol.for('quote'), [1, 2, 3, 4]]]);
  });
  it('(drop \'(1 2 3 4) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('drop'), [Symbol.for('quote'), [1, 2, 3, 4]], 1], [Symbol.for('quote'), [2, 3, 4]]]);
  });
  it('(compile \'(drop lst 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('drop'), Symbol.for('lst'), 0]]], 'lst;']);
  });
  it('(compile \'(drop lst 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('drop'), Symbol.for('lst'), 1]]], 'lst.slice(1);']);
  });
  it('(compile \'(drop lst n))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('drop'), Symbol.for('lst'), Symbol.for('n')]]], 'lst.slice(n);']);
  });
  it('(compile \'(module m scheme (drop lst n)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('drop'), Symbol.for('lst'), Symbol.for('n')]]], Symbol.for(':fdottedlists'), false], 'lst.slice(n);']);
  });
  return it('(compile \'(module m scheme (drop lst n)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('drop'), Symbol.for('lst'), Symbol.for('n')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  drop\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'drop(lst, n);']);
  });
});

describe('drop-right', function (): any {
  it('(drop-right \'(1 2 3 4) 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('drop-right'), [Symbol.for('quote'), [1, 2, 3, 4]], 0], [Symbol.for('quote'), [1, 2, 3, 4]]]);
  });
  it('(drop-right \'(1 2 3 4) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('drop-right'), [Symbol.for('quote'), [1, 2, 3, 4]], 1], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(compile \'(drop-right lst 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('drop-right'), Symbol.for('lst'), 0]]], 'lst;']);
  });
  it('(compile \'(drop-right lst 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('drop-right'), Symbol.for('lst'), 1]]], 'lst.slice(0, -1);']);
  });
  it('(compile \'(drop-right lst n))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('drop-right'), Symbol.for('lst'), Symbol.for('n')]]], 'lst.slice(0, -n || undefined);']);
  });
  it('(compile \'(module m scheme (drop-right lst n)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('drop-right'), Symbol.for('lst'), Symbol.for('n')]]], Symbol.for(':fdottedlists'), false], 'lst.slice(0, -n || undefined);']);
  });
  return it('(compile \'(module m scheme (drop-right lst n)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('drop-right'), Symbol.for('lst'), Symbol.for('n')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  dropRight\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'dropRight(lst, n);']);
  });
});

describe('reverse', function (): any {
  it('(reverse \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('reverse'), [Symbol.for('quote'), []]], [Symbol.for('quote'), []]]);
  });
  it('(reverse \'(1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('reverse'), [Symbol.for('quote'), [1]]], [Symbol.for('quote'), [1]]]);
  });
  it('(reverse \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('reverse'), [Symbol.for('quote'), [1, 2]]], [Symbol.for('quote'), [2, 1]]]);
  });
  it('(reverse \'(1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('reverse'), [Symbol.for('quote'), [1, 2, 3]]], [Symbol.for('quote'), [3, 2, 1]]]);
  });
  it('(compile \'(reverse lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('reverse'), Symbol.for('lst')]]], '[...lst].reverse();']);
  });
  it('(compile \'(module m scheme (reverse lst)) :fdottedlists #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('reverse'), Symbol.for('lst')]]], Symbol.for(':fdottedlists'), false], '[...lst].reverse();']);
  });
  return it('(compile \'(module m scheme (reverse lst)) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('reverse'), Symbol.for('lst')]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  reverse\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'reverse(lst);']);
  });
});

describe('map', function (): any {
  it('(map list \'(1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('map'), Symbol.for('list'), [Symbol.for('quote'), [1, 2]]], [Symbol.for('quote'), [[1], [2]]]]);
  });
  it('((lambda () (define (fact n) (if (< n 2) 1 (* n (fact (- n 1))))) (map fact \'(1 2 3 4 5 6))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [[Symbol.for('lambda'), [], [Symbol.for('define'), [Symbol.for('fact'), Symbol.for('n')], [Symbol.for('if'), [Symbol.for('<'), Symbol.for('n'), 2], 1, [Symbol.for('*'), Symbol.for('n'), [Symbol.for('fact'), [Symbol.for('-'), Symbol.for('n'), 1]]]]], [Symbol.for('map'), Symbol.for('fact'), [Symbol.for('quote'), [1, 2, 3, 4, 5, 6]]]]], [Symbol.for('quote'), [1, 2, 6, 24, 120, 720]]]);
  });
  it('(compile \'(map f lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('map'), Symbol.for('f'), Symbol.for('lst')]]], 'lst.map(function (x) {\n' +
      '  return f(x);\n' +
      '});']);
  });
  it('(compile \'(map (lambda (x) x) lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], Symbol.for('lst')]]], 'lst.map(function (x) {\n' +
      '  return x;\n' +
      '});']);
  });
  it('(compile \'(map f x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('map'), Symbol.for('f'), Symbol.for('x')]]], 'x.map(function (x) {\n' +
      '  return f(x);\n' +
      '});']);
  });
  it('(compile \'(map (lambda (x) x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], Symbol.for('x')]]], 'x.map(function (x) {\n' +
      '  return x;\n' +
      '});']);
  });
  return it('(compile \'(map (g y) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('map'), [Symbol.for('g'), Symbol.for('y')], Symbol.for('x')]]], 'x.map((function (f) {\n' +
      '  return function (x) {\n' +
      '    return f(x);\n' +
      '  };\n' +
      '})(g(y)));']);
  });
});

describe('foldl', function (): any {
  it('(foldl cons \'() \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('foldl'), Symbol.for('cons'), [Symbol.for('quote'), []], [Symbol.for('quote'), [1, 2, 3, 4]]], [Symbol.for('quote'), [4, 3, 2, 1]]]);
  });
  it('(compile \'(foldl (lambda (x acc) x) v lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], Symbol.for('x')], Symbol.for('v'), Symbol.for('lst')]]], 'lst.reduce(function (acc, x) {\n' +
      '  return x;\n' +
      '}, v);']);
  });
  it('(compile \'(foldl f v lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')]]], 'lst.reduce(function (acc, x) {\n' +
      '  return f(x, acc);\n' +
      '}, v);']);
  });
  it('(compile \'(foldl f v l))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('v'), Symbol.for('l')]]], 'l.reduce(function (acc, x) {\n' +
      '  return f(x, acc);\n' +
      '}, v);']);
  });
  it('(compile \'(foldl (lambda (x acc) (f x acc)) v l))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('f'), Symbol.for('x'), Symbol.for('acc')]], Symbol.for('v'), Symbol.for('l')]]], 'l.reduce(function (acc, x) {\n' +
      '  return f(x, acc);\n' +
      '}, v);']);
  });
  return it('(compile \'(foldl + 0 \'(1 2 3 4)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('foldl'), Symbol.for('+'), 0, [Symbol.for('quote'), [1, 2, 3, 4]]]]], '[1, 2, 3, 4].reduce(function (acc, x) {\n' +
      '  return x + acc;\n' +
      '}, 0);']);
  });
});

describe('foldr', function (): any {
  it('(foldr cons \'() \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('foldr'), Symbol.for('cons'), [Symbol.for('quote'), []], [Symbol.for('quote'), [1, 2, 3, 4]]], [Symbol.for('quote'), [1, 2, 3, 4]]]);
  });
  it('(foldr (lambda (v l) (cons (add1 v) l)) \'() \'(1 2 3 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('foldr'), [Symbol.for('lambda'), [Symbol.for('v'), Symbol.for('l')], [Symbol.for('cons'), [Symbol.for('add1'), Symbol.for('v')], Symbol.for('l')]], [Symbol.for('quote'), []], [Symbol.for('quote'), [1, 2, 3, 4]]], [Symbol.for('quote'), [2, 3, 4, 5]]]);
  });
  it('(compile \'(foldr (lambda (x acc) x) v lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('foldr'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], Symbol.for('x')], Symbol.for('v'), Symbol.for('lst')]]], 'lst.reduceRight(function (acc, x) {\n' +
      '  return x;\n' +
      '}, v);']);
  });
  it('(compile \'(foldr f v lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('foldr'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')]]], 'lst.reduceRight(function (acc, x) {\n' +
      '  return f(x, acc);\n' +
      '}, v);']);
  });
  return it('(compile \'(foldr (f g) v lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('foldr'), [Symbol.for('f'), Symbol.for('g')], Symbol.for('v'), Symbol.for('lst')]]], 'lst.reduceRight((function (f) {\n' +
      '  return function (x, y) {\n' +
      '    return f(y, x);\n' +
      '  };\n' +
      '})(f(g)), v);']);
  });
});

describe('filter', function (): any {
  it('(filter string? \'("foo" 1 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('filter'), Symbol.for('string?'), [Symbol.for('quote'), ['foo', 1, 2, 3]]], [Symbol.for('quote'), ['foo']]]);
  });
  return it('(compile \'(filter f lst))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('filter'), Symbol.for('f'), Symbol.for('lst')]]], 'lst.filter(f);']);
  });
});

describe('string?', function (): any {
  it('(string? "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string?'), 'foo'], true]);
  });
  it('(string? 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string?'), 1], false]);
  });
  it('(string? (js/obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string?'), [Symbol.for('js/obj')]], false]);
  });
  it('(string? (list "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string?'), [Symbol.for('list'), 'foo']], false]);
  });
  it('(string? (js/obj "foo" ""))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string?'), [Symbol.for('js/obj'), 'foo', '']], false]);
  });
  it('(string? (js/obj "foo" \'()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string?'), [Symbol.for('js/obj'), 'foo', [Symbol.for('quote'), []]]], false]);
  });
  it('(string? (js/obj "foo" (js/obj)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string?'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj')]]], false]);
  });
  it('(string? (js/obj "foo" "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string?'), [Symbol.for('js/obj'), 'foo', 'foo']], false]);
  });
  it('(string? \'())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string?'), [Symbol.for('quote'), []]], false]);
  });
  return it('(compile \'(string? x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string?'), Symbol.for('x')]]], 'typeof x === \'string\';']);
  });
});

describe('string-length', function (): any {
  it('(string-length "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-length'), 'foo'], 3]);
  });
  return it('(compile \'(string-length x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string-length'), Symbol.for('x')]]], 'x.length;']);
  });
});

describe('string-append', function (): any {
  it('(string-append)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-append')], '']);
  });
  it('(string-append "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-append'), 'foo'], 'foo']);
  });
  it('(string-append "foo" "bar")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-append'), 'foo', 'bar'], 'foobar']);
  });
  it('(apply string-append \'("foo" "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('apply'), Symbol.for('string-append'), [Symbol.for('quote'), ['foo', 'bar']]], 'foobar']);
  });
  it('(compile \'(string-append "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string-append'), 'foo']]], '\'foo\';']);
  });
  it('(compile \'(string-append "foo" "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string-append'), 'foo', 'bar']]], '\'foo\' + \'bar\';']);
  });
  return it('(compile \'(string-append "foo" "bar" "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string-append'), 'foo', 'bar', 'baz']]], '\'foo\' + \'bar\' + \'baz\';']);
  });
});

describe('string-join', function (): any {
  it('(string-join \'("foo" "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-join'), [Symbol.for('quote'), ['foo', 'bar']]], 'foo bar']);
  });
  it('(string-join \'("foo" "bar") ",")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-join'), [Symbol.for('quote'), ['foo', 'bar']], ','], 'foo,bar']);
  });
  return it('(compile \'(string-join \'("foo" "bar") ","))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string-join'), [Symbol.for('quote'), ['foo', 'bar']], ',']]], '[\'foo\', \'bar\'].join(\',\');']);
  });
});

describe('string-split', function (): any {
  it('(string-split "foo bar  baz")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-split'), 'foo bar  baz'], [Symbol.for('quote'), ['foo', 'bar', 'baz']]]);
  });
  it('(string-split "foo,bar,baz" ",")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-split'), 'foo,bar,baz', ','], [Symbol.for('quote'), ['foo', 'bar', 'baz']]]);
  });
  it('(string-split "foo, bar, baz" ", ")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-split'), 'foo, bar, baz', ', '], [Symbol.for('quote'), ['foo', 'bar', 'baz']]]);
  });
  it('(string-split "foo\n' +
    'bar\n' +
    'baz" "\n' +
    '")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-split'), 'foo\n' +
      'bar\n' +
      'baz', '\n'], [Symbol.for('quote'), ['foo', 'bar', 'baz']]]);
  });
  return it('(compile \'(string-split "foo,bar,baz" ","))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string-split'), 'foo,bar,baz', ',']]], '\'foo,bar,baz\'.split(\',\');']);
  });
});

describe('string-trim', function (): any {
  it('(string-trim "  foo bar  baz  ")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-trim'), '  foo bar  baz  '], 'foo bar  baz']);
  });
  it('(string-trim "  foo bar  baz \n' +
    '\n' +
    '	")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-trim'), '  foo bar  baz \n' +
      '\n' +
      '	'], 'foo bar  baz']);
  });
  return it('(compile \'(string-trim x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string-trim'), Symbol.for('x')]]], 'x.trim();']);
  });
});

describe('string-upcase', function (): any {
  it('(string-upcase "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-upcase'), 'foo'], 'FOO']);
  });
  return it('(compile \'(string-upcase x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string-upcase'), Symbol.for('x')]]], 'x.toUpperCase();']);
  });
});

describe('string-downcase', function (): any {
  it('(string-downcase "FOO")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('string-downcase'), 'FOO'], 'foo']);
  });
  return it('(compile \'(string-downcase x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('string-downcase'), Symbol.for('x')]]], 'x.toLowerCase();']);
  });
});

describe('substring', function (): any {
  it('(substring "Apple" 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('substring'), 'Apple', 1, 3], 'pp']);
  });
  it('(substring "Apple" 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('substring'), 'Apple', 1], 'pple']);
  });
  it('(compile \'(substring str i))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('substring'), Symbol.for('str'), Symbol.for('i')]]], 'str.substring(i);']);
  });
  return it('(compile \'(substring str i j))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('substring'), Symbol.for('str'), Symbol.for('i'), Symbol.for('j')]]], 'str.substring(i, j);']);
  });
});

describe('match', function (): any {
  it('(match 1 (x x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), 1, [Symbol.for('x'), Symbol.for('x')]], 1]);
  });
  it('(match 1 ((var x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), 1, [[Symbol.for('var'), Symbol.for('x')], Symbol.for('x')]], 1]);
  });
  it('(match "foo" ("foo" 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), 'foo', ['foo', 1]], 1]);
  });
  it('(match "foo" ((not "bar") 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), 'foo', [[Symbol.for('not'), 'bar'], 1]], 1]);
  });
  it('(match \'a (\'a 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), [Symbol.for('quote'), Symbol.for('a')], [[Symbol.for('quote'), Symbol.for('a')], 1]], 1]);
  });
  it('(match \'(1 2 3) ((list a b c) (list a b c)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')], [Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')]]], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(match \'(1 2 3) ((list a b c) a))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')], Symbol.for('a')]], 1]);
  });
  it('(match \'(1 2 3) ((list _ _ a) a))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list'), Symbol.for('_'), Symbol.for('_'), Symbol.for('a')], Symbol.for('a')]], 3]);
  });
  it('(match \'(1 2 3) ((list x y ...) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list'), Symbol.for('x'), Symbol.for('y'), Symbol.for('...')], Symbol.for('y')]], [Symbol.for('quote'), [2, 3]]]);
  });
  it('(compile \'(match "foo" ("foo" 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 'foo', ['foo', 1]]]], 'if (\'foo\' === \'foo\') {\n' +
      '  1;\n' +
      '}']);
  });
  it('(compile \'(match "foo" ("foo" 1) (_ 2)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 'foo', ['foo', 1], [Symbol.for('_'), 2]]]], 'if (\'foo\' === \'foo\') {\n' +
      '  1;\n' +
      '} else {\n' +
      '  2;\n' +
      '}']);
  });
  it('(match \'((1) 2 3) ((list (list a) b c) (list a b c)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('match'), [Symbol.for('quote'), [[1], 2, 3]], [[Symbol.for('list'), [Symbol.for('list'), Symbol.for('a')], Symbol.for('b'), Symbol.for('c')], [Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')]]], [Symbol.for('quote'), [1, 2, 3]]]);
  });
  it('(compile \'(match "foo" ((not "bar") 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 'foo', [[Symbol.for('not'), 'bar'], 1]]]], 'if (\'foo\' !== \'bar\') {\n' +
      '  1;\n' +
      '}']);
  });
  it('(compile \'(match 1 (x x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 1, [Symbol.for('x'), Symbol.for('x')]]]], 'let x = 1;\n' +
      '\n' +
      'x;']);
  });
  it('(compile \'(match 1 ((var x) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 1, [[Symbol.for('var'), Symbol.for('x')], Symbol.for('x')]]]], 'let x = 1;\n' +
      '\n' +
      'x;']);
  });
  it('(compile \'(match \'a (\'a 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), Symbol.for('a')], [[Symbol.for('quote'), Symbol.for('a')], 1]]]], 'let matchVal = Symbol.for(\'a\');\n' +
      '\n' +
      'if (matchVal === Symbol.for(\'a\')) {\n' +
      '  1;\n' +
      '}']);
  });
  it('(compile \'(match \'(1 2 3) ((list a b c) a)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')], Symbol.for('a')]]]], 'let matchVal = [1, 2, 3];\n' +
      '\n' +
      'if (Array.isArray(matchVal) && (matchVal.length === 3)) {\n' +
      '  let [a, b, c] = matchVal;\n' +
      '  a;\n' +
      '}']);
  });
  it('(compile \'(match \'(1 2 3) ((list _ _ a) a)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list'), Symbol.for('_'), Symbol.for('_'), Symbol.for('a')], Symbol.for('a')]]]], 'let matchVal = [1, 2, 3];\n' +
      '\n' +
      'if (Array.isArray(matchVal) && (matchVal.length === 3)) {\n' +
      '  let [, , a] = matchVal;\n' +
      '  a;\n' +
      '}']);
  });
  it('(compile \'(match \'(1 2 3) ((list x ...) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list'), Symbol.for('x'), Symbol.for('...')], Symbol.for('x')]]]], 'let matchVal = [1, 2, 3];\n' +
      '\n' +
      'if (Array.isArray(matchVal) && (matchVal.length >= 0)) {\n' +
      '  let x = matchVal;\n' +
      '  x;\n' +
      '}']);
  });
  it('(compile \'(match \'(1 2 3) ((list x y ...) y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list'), Symbol.for('x'), Symbol.for('y'), Symbol.for('...')], Symbol.for('y')]]]], 'let matchVal = [1, 2, 3];\n' +
      '\n' +
      'if (Array.isArray(matchVal) && (matchVal.length >= 1)) {\n' +
      '  let [x, ...y] = matchVal;\n' +
      '  y;\n' +
      '}']);
  });
  it('(compile \'(match \'(1 2 3) ((list* x) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list*'), Symbol.for('x')], Symbol.for('x')]]]], 'let matchVal = [1, 2, 3];\n' +
      '\n' +
      'if (Array.isArray(matchVal) && (matchVal.length >= 0)) {\n' +
      '  let x = matchVal;\n' +
      '  x;\n' +
      '}']);
  });
  it('(compile \'(match \'(1 2 3) ((list* x y) y)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list*'), Symbol.for('x'), Symbol.for('y')], Symbol.for('y')]]]], 'let matchVal = [1, 2, 3];\n' +
      '\n' +
      'if (Array.isArray(matchVal) && (matchVal.length >= 1)) {\n' +
      '  let [x, ...y] = matchVal;\n' +
      '  y;\n' +
      '}']);
  });
  it('(compile \'(match \'(1 2 3) ((cons x y) (list x y))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('cons'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]]]]], 'let matchVal = [1, 2, 3];\n' +
      '\n' +
      'if (Array.isArray(matchVal) && (matchVal.length >= 1)) {\n' +
      '  let [x, ...y] = matchVal;\n' +
      '  [x, y];\n' +
      '}']);
  });
  it('(compile \'(match \'(1 2 3) ((list a b c) (list a b c))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), [1, 2, 3]], [[Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')], [Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')]]]]], 'let matchVal = [1, 2, 3];\n' +
      '\n' +
      'if (Array.isArray(matchVal) && (matchVal.length === 3)) {\n' +
      '  let [a, b, c] = matchVal;\n' +
      '  [a, b, c];\n' +
      '}']);
  });
  it('(compile \'(match \'((1) 2 3) ((list (list a) b c) (list a b c))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), [Symbol.for('quote'), [[1], 2, 3]], [[Symbol.for('list'), [Symbol.for('list'), Symbol.for('a')], Symbol.for('b'), Symbol.for('c')], [Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')]]]]], 'let matchVal = [[1], 2, 3];\n' +
      '\n' +
      'if (Array.isArray(matchVal) && (matchVal.length === 3) && Array.isArray(matchVal[0]) && (matchVal[0].length === 1)) {\n' +
      '  let [[a], b, c] = matchVal;\n' +
      '  [a, b, c];\n' +
      '}']);
  });
  it('(compile \'(match exp ((list (list \'foo x) y ...) (list x y)) (_ exp)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), Symbol.for('exp'), [[Symbol.for('list'), [Symbol.for('list'), [Symbol.for('quote'), Symbol.for('foo')], Symbol.for('x')], Symbol.for('y'), Symbol.for('...')], [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('_'), Symbol.for('exp')]]]], 'if (Array.isArray(exp) && (exp.length >= 1) && Array.isArray(exp[0]) && (exp[0].length === 2) && (exp[0][0] === Symbol.for(\'foo\'))) {\n' +
      '  let [[, x], ...y] = exp;\n' +
      '  [x, y];\n' +
      '} else {\n' +
      '  exp;\n' +
      '}']);
  });
  it('(compile \'(match exp ((and _ ()) #t)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), Symbol.for('exp'), [[Symbol.for('and'), Symbol.for('_'), []], true]]]], 'if (Array.isArray(exp) && (exp.length === 0)) {\n' +
      '  true;\n' +
      '}']);
  });
  it('(compile \'(match exp ((or _ ()) #t)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), Symbol.for('exp'), [[Symbol.for('or'), Symbol.for('_'), []], true]]]], 'true;']);
  });
  it('(compile \'(match "foo" ((regexp "foo") #t)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 'foo', [[Symbol.for('regexp'), 'foo'], true]]]], 'if (\'foo\'.match(new RegExp(\'foo\'))) {\n' +
      '  true;\n' +
      '}']);
  });
  it('(compile \'(match "foo" ((? string?) #t)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 'foo', [[Symbol.for('?'), Symbol.for('string?')], true]]]], 'if (typeof \'foo\' === \'string\') {\n' +
      '  true;\n' +
      '}']);
  });
  it('(compile \'(match "foo" ((? string? "foo") #t)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 'foo', [[Symbol.for('?'), Symbol.for('string?'), 'foo'], true]]]], 'if ((typeof \'foo\' === \'string\') && (\'foo\' === \'foo\')) {\n' +
      '  true;\n' +
      '}']);
  });
  it('(compile \'(match "foo" ((app string-length 3) #t)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 'foo', [[Symbol.for('app'), Symbol.for('string-length'), 3], true]]]], 'if (\'foo\'.length === 3) {\n' +
      '  true;\n' +
      '}']);
  });
  return it('(compile \'(match "foo" ((app string-length (? number?) 3) #t)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('match'), 'foo', [[Symbol.for('app'), Symbol.for('string-length'), [Symbol.for('?'), Symbol.for('number?')], 3], true]]]], 'if ((() => {\n' +
      '  let patternMatchVal = \'foo\'.length;\n' +
      '  return Number.isFinite(patternMatchVal) && (patternMatchVal === 3);\n' +
      '})()) {\n' +
      '  true;\n' +
      '}']);
  });
});

describe('assert', function (): any {
  it('(compile \'(assert #t))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('assert'), true]]], 'console.assert(true);']);
  });
  return it('(compile \'(assert #t "test"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('assert'), true, 'test']]], 'console.assert(true, \'test\');']);
  });
});

describe('display', function (): any {
  it('(compile \'(display #t))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('display'), true]]], 'console.log(true);']);
  });
  return it('(compile \'(display #t "test"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('display'), true, 'test']]], 'console.log(true, \'test\');']);
  });
});