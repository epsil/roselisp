import {
  LeadingCommentToken,
  NumberToken,
  StringToken,
  SymbolToken,
  TrailingCommentToken,
  parse,
  read,
  readSyntax,
  tokenize
} from '../../src/ts/parser';

import {
  syntaxToDatum
} from '../../src/ts/rose';

import {
  s,
  sexp
} from '../../src/ts/sexp';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('tokenize', (): any => {
  it('(tokenize "")', (): any => assertEqual(tokenize(''), []));
  it('(tokenize "1")', (): any => assertEqual(tokenize('1'), [new NumberToken(1)]));
  it('(tokenize "foo")', (): any => assertEqual(tokenize('foo'), [new SymbolToken('foo')]));
  it('(tokenize "\\\\foo")', (): any => assertEqual(tokenize('\\foo'), [new SymbolToken('foo')]));
  it('(tokenize "f\\\\oo")', (): any => assertEqual(tokenize('f\\oo'), [new SymbolToken('foo')]));
  it('(tokenize "|foo|")', (): any => assertEqual(tokenize('|foo|'), [new SymbolToken('foo')]));
  it('(tokenize "\\"foo\\"")', (): any => assertEqual(tokenize('"foo"'), [new StringToken('foo')]));
  it('(tokenize "\\"foo\\\\\\"bar\\"")', (): any => assertEqual(tokenize('"foo\\"bar"'), [new StringToken('foo"bar')]));
  it('(tokenize "\'foo")', (): any => assertEqual(tokenize('\'foo'), [new SymbolToken('\''), new SymbolToken('foo')]));
  it('(tokenize "()")', (): any => assertEqual(tokenize('()'), [new SymbolToken('('), new SymbolToken(')')]));
  it('(tokenize "\'(foo)")', (): any => assertEqual(tokenize('\'(foo)'), [new SymbolToken('\''), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')')]));
  it('(tokenize "(foo \\"bar\\")")', (): any => assertEqual(tokenize('(foo "bar")'), [new SymbolToken('('), new SymbolToken('foo'), new StringToken('bar'), new SymbolToken(')')]));
  it('(tokenize "(foo\n' +
    '\\"bar\\")")', (): any => assertEqual(tokenize('(foo\n' +
    '"bar")'), [new SymbolToken('('), new SymbolToken('foo'), new StringToken('bar'), new SymbolToken(')')]));
  it('(tokenize "(foo) ; bar" (js/obj :comments #f))', (): any => assertEqual(tokenize('(foo) ; bar', {
    comments: false
  }), [new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')')]));
  xit('(tokenize "\'(foo) ; bar" (js/obj :comments #f))', (): any => assertEqual(tokenize('\'(foo) ; bar', {
    comments: false
  }), [new SymbolToken('\''), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')')]));
  xit('(tokenize "(foo ; baz\n' +
    'bar)")', (): any => assertEqual(tokenize('(foo ; baz\n' +
    'bar)'), [new SymbolToken('('), new SymbolToken('foo'), new SymbolToken('bar'), new SymbolToken(')')]));
  xit('(tokenize "(foo ; baz\n' +
    'bar)" (js/obj :comments #t))', (): any => assertEqual(tokenize('(foo ; baz\n' +
    'bar)', {
    comments: true
  }), [new SymbolToken('('), new SymbolToken('foo'), new SymbolToken('bar'), new SymbolToken(')'), new TrailingCommentToken('; baz')]));
  it('(tokenize ";; baz\n' +
    '(foo bar)" (js/obj :comments #t))', (): any => assertEqual(tokenize(';; baz\n' +
    '(foo bar)', {
    comments: true
  }), [new LeadingCommentToken(';; baz\n'), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken('bar'), new SymbolToken(')')]));
  it('(tokenize "  ;; baz\n' +
    '  (foo bar)" (js/obj :comments #t))', (): any => assertEqual(tokenize('  ;; baz\n' +
    '  (foo bar)', {
    comments: true
  }), [new LeadingCommentToken(';; baz\n'), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken('bar'), new SymbolToken(')')]));
  it('(tokenize ";; baz\n' +
    ';; quux\n' +
    '(foo bar)" (js/obj :comments #t))', (): any => assertEqual(tokenize(';; baz\n' +
    ';; quux\n' +
    '(foo bar)', {
    comments: true
  }), [new LeadingCommentToken(';; baz\n' +
    ';; quux\n'), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken('bar'), new SymbolToken(')')]));
  it('(tokenize ";; baz\n' +
    ';;\n' +
    ';; quux\n' +
    '(foo bar)" (js/obj :comments #t))', (): any => assertEqual(tokenize(';; baz\n' +
    ';;\n' +
    ';; quux\n' +
    '(foo bar)', {
    comments: true
  }), [new LeadingCommentToken(';; baz\n' +
    ';;\n' +
    ';; quux\n'), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken('bar'), new SymbolToken(')')]));
  it('(tokenize ";; baz\n' +
    '\n' +
    ';; quux\n' +
    '(foo bar)" (js/obj :comments #t))', (): any => assertEqual(tokenize(';; baz\n' +
    '\n' +
    ';; quux\n' +
    '(foo bar)', {
    comments: true
  }), [new LeadingCommentToken(';; baz\n' +
    '\n' +
    ';; quux\n'), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken('bar'), new SymbolToken(')')]));
  it('(tokenize ";; foo\n' +
    '`(foo)" (js/obj :comments #t))', (): any => assertEqual(tokenize(';; foo\n' +
    '`(foo)', {
    comments: true
  }), [new LeadingCommentToken(';; foo\n'), new SymbolToken('`'), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')')]));
  it('(tokenize "(foo \'(bar))")', (): any => assertEqual(tokenize('(foo \'(bar))'), [new SymbolToken('('), new SymbolToken('foo'), new SymbolToken('\''), new SymbolToken('('), new SymbolToken('bar'), new SymbolToken(')'), new SymbolToken(')')]));
  it('(tokenize "((lambda (x) x) \\"Lisp\\")")', (): any => assertEqual(tokenize('((lambda (x) x) "Lisp")'), [new SymbolToken('('), new SymbolToken('('), new SymbolToken('lambda'), new SymbolToken('('), new SymbolToken('x'), new SymbolToken(')'), new SymbolToken('x'), new SymbolToken(')'), new StringToken('Lisp'), new SymbolToken(')')]));
  return it('(tokenize "(define (foo)\n' +
    '  ;; this\n' +
    '  this)" (js/obj :comments #t))', (): any => assertEqual(tokenize('(define (foo)\n' +
    '  ;; this\n' +
    '  this)', {
    comments: true
  }), [new SymbolToken('('), new SymbolToken('define'), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')'), new LeadingCommentToken(';; this\n'), new SymbolToken('this'), new SymbolToken(')')]));
});

describe('parse', (): any => {
  it('(parse (list (new SymbolToken "exp")))', (): any => assertEqual(parse([new SymbolToken('exp')]), Symbol.for('exp')));
  it('(parse (list (new SymbolToken "(") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('('), new SymbolToken(')')]), []));
  it('(parse (list (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken ")") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('('), new SymbolToken('('), new SymbolToken(')'), new SymbolToken(')')]), [[]]));
  it('(parse (list (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')')]), [Symbol.for('foo')]));
  it('(parse (list (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken "lambda") (new SymbolToken "(") (new SymbolToken "x") (new SymbolToken ")") (new SymbolToken "x") (new SymbolToken ")") (new StringToken "Lisp") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('('), new SymbolToken('('), new SymbolToken('lambda'), new SymbolToken('('), new SymbolToken('x'), new SymbolToken(')'), new SymbolToken('x'), new SymbolToken(')'), new StringToken('Lisp'), new SymbolToken(')')]), [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 'Lisp']));
  it('(parse (list (new SymbolToken "\'") (new SymbolToken "foo")))', (): any => assertEqual(parse([new SymbolToken('\''), new SymbolToken('foo')]), [Symbol.for('quote'), Symbol.for('foo')]));
  it('(parse (list (new SymbolToken "\'") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('\''), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')')]), [Symbol.for('quote'), [Symbol.for('foo')]]));
  it('(parse (list (new SymbolToken "\'") (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('\''), new SymbolToken('('), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')'), new SymbolToken(')')]), [Symbol.for('quote'), [[Symbol.for('foo')]]]));
  it('(parse (list (new SymbolToken "\'") (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken "(") (new SymbolToken "bar") (new SymbolToken ")") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('\''), new SymbolToken('('), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')'), new SymbolToken('('), new SymbolToken('bar'), new SymbolToken(')'), new SymbolToken(')')]), [Symbol.for('quote'), [[Symbol.for('foo')], [Symbol.for('bar')]]]));
  it('(parse (list (new SymbolToken "(") (new SymbolToken "quote") (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken "(") (new SymbolToken "bar") (new SymbolToken ")") (new SymbolToken ")") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('('), new SymbolToken('quote'), new SymbolToken('('), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')'), new SymbolToken('('), new SymbolToken('bar'), new SymbolToken(')'), new SymbolToken(')'), new SymbolToken(')')]), [Symbol.for('quote'), [[Symbol.for('foo')], [Symbol.for('bar')]]]));
  it('(parse (list (new SymbolToken "(") (new SymbolToken "truep") (new SymbolToken "\'") (new SymbolToken "foo") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('('), new SymbolToken('truep'), new SymbolToken('\''), new SymbolToken('foo'), new SymbolToken(')')]), [Symbol.for('truep'), [Symbol.for('quote'), Symbol.for('foo')]]));
  it('(parse (list (new SymbolToken "(") (new SymbolToken "truep") (new SymbolToken "\'") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('('), new SymbolToken('truep'), new SymbolToken('\''), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')'), new SymbolToken(')')]), [Symbol.for('truep'), [Symbol.for('quote'), [Symbol.for('foo')]]]));
  it('(parse (list (new SymbolToken "(") (new SymbolToken "truep") (new SymbolToken "`") (new SymbolToken "foo") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('('), new SymbolToken('truep'), new SymbolToken('`'), new SymbolToken('foo'), new SymbolToken(')')]), [Symbol.for('truep'), [Symbol.for('quasiquote'), Symbol.for('foo')]]));
  it('(parse (list (new SymbolToken "(") (new SymbolToken "truep") (new SymbolToken "`") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken ")")))', (): any => assertEqual(parse([new SymbolToken('('), new SymbolToken('truep'), new SymbolToken('`'), new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')'), new SymbolToken(')')]), [Symbol.for('truep'), [Symbol.for('quasiquote'), [Symbol.for('foo')]]]));
  it('(parse (list (new SymbolToken "`") (new SymbolToken "foo")))', (): any => assertEqual(parse([new SymbolToken('`'), new SymbolToken('foo')]), [Symbol.for('quasiquote'), Symbol.for('foo')]));
  it('(parse (list (new SymbolToken ",") (new SymbolToken "foo")))', (): any => assertEqual(parse([new SymbolToken(','), new SymbolToken('foo')]), [Symbol.for('unquote'), Symbol.for('foo')]));
  return it('(parse (list (new SymbolToken ",@") (new SymbolToken "foo")))', (): any => assertEqual(parse([new SymbolToken(',@'), new SymbolToken('foo')]), [Symbol.for('unquote-splicing'), Symbol.for('foo')]));
});

describe('read', (): any => {
  it('(read "(foo) ;comment")', (): any => assertEqual(read('(foo) ;comment'), [Symbol.for('foo')]));
  it('(read "(foo) ;; this is a comment")', (): any => assertEqual(read('(foo) ;; this is a comment'), [Symbol.for('foo')]));
  it('(read "(define (foo)\n' +
    '  ;; this is a comment\n' +
    '  (bar) ; this is also a comment\n' +
    '  (baz))")', (): any => assertEqual(read('(define (foo)\n' +
    '  ;; this is a comment\n' +
    '  (bar) ; this is also a comment\n' +
    '  (baz))'), [Symbol.for('define'), [Symbol.for('foo')], [Symbol.for('bar')], [Symbol.for('baz')]]));
  it('(read "\\"string ;-D\\"")', (): any => assertEqual(read('"string ;-D"'), 'string ;-D'));
  it('(read "\\"string\\\\\\"test\\"")', (): any => assertEqual(read('"string\\"test"'), 'string"test'));
  it('(read "\\"string\\\\ntest\\"")', (): any => assertEqual(read('"string\\ntest"'), 'string\n' +
    'test'));
  it('(read "\\"string\\\\\\\\ntest\\"")', (): any => assertEqual(read('"string\\\\ntest"'), 'string\\ntest'));
  it('(read "\\"string\n' +
    'test\\"")', (): any => assertEqual(read('"string\n' +
    'test"'), 'string\n' +
    'test'));
  it('(read "\'foo")', (): any => assertEqual(read('\'foo'), [Symbol.for('quote'), Symbol.for('foo')]));
  it('(read "\'|foo|")', (): any => assertEqual(read('\'|foo|'), [Symbol.for('quote'), Symbol.for('foo')]));
  it('(read "\'()")', (): any => assertEqual(read('\'()'), [Symbol.for('quote'), []]));
  it('(read "`()")', (): any => assertEqual(read('`()'), [Symbol.for('quasiquote'), []]));
  it('(read "`(,exp)")', (): any => assertEqual(read('`(,exp)'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('exp')]]]));
  it('(read "`((quote ,exp))")', (): any => assertEqual(read('`((quote ,exp))'), [Symbol.for('quasiquote'), [[Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]]));
  it('(read "`(\',exp)")', (): any => assertEqual(read('`(\',exp)'), [Symbol.for('quasiquote'), [[Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]]));
  it('(read "`(\'\',exp)")', (): any => assertEqual(read('`(\'\',exp)'), [Symbol.for('quasiquote'), [[Symbol.for('quote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]]]));
  it('(read "`(\'\'\',exp)")', (): any => assertEqual(read('`(\'\'\',exp)'), [Symbol.for('quasiquote'), [[Symbol.for('quote'), [Symbol.for('quote'), [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]]]]]));
  return it('(read "(define foo `(,bar))")', (): any => assertEqual(read('(define foo `(,bar))'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('bar')]]]]));
});

describe('read-syntax', (): any => {
  it('(syntax->datum (read-syntax ";; comment\n' +
    '(foo)"))', (): any => assertEqual(syntaxToDatum(readSyntax(';; comment\n' +
    '(foo)')), [Symbol.for('foo')]));
  it(';; comment\n' +
    '(foo), comments', (): any => {
    const actual: any = readSyntax(';; comment\n' +
      '(foo)', {
      comments: true
    });
    assertEqual(actual.getValue(), [Symbol.for('foo')]);
    return assertEqual(actual.getProperty('comments'), [new LeadingCommentToken(';; comment\n')]);
  });
  it(';; comment\n' +
    '`(foo), comments', (): any => {
    const actual: any = readSyntax(';; comment\n' +
      '`(foo)', {
      comments: true
    });
    assertEqual(actual.getValue(), [Symbol.for('quasiquote'), [Symbol.for('foo')]]);
    return assertEqual(actual.getProperty('comments'), [new LeadingCommentToken(';; comment\n')]);
  });
  return it('(syntax->datum (read-syntax "(foo) ;comment"))', (): any => assertEqual(syntaxToDatum(readSyntax('(foo) ;comment')), [Symbol.for('foo')]));
});

describe('sexp', (): any => {
  it('(js/tag sexp "")', (): any => assertEqual(sexp``, []));
  it('(js/tag sexp "()")', (): any => assertEqual(sexp`()`, []));
  it('(sexp "()")', (): any => assertEqual(sexp('()'), []));
  it('(js/tag sexp "\'()")', (): any => assertEqual(sexp`'()`, [Symbol.for('quote'), []]));
  it('(sexp "\'()")', (): any => assertEqual(sexp('\'()'), [Symbol.for('quote'), []]));
  it('(js/tag sexp "(truep \'())")', (): any => assertEqual(sexp`(truep '())`, [Symbol.for('truep'), [Symbol.for('quote'), []]]));
  it('(sexp "(truep \'())")', (): any => assertEqual(sexp('(truep \'())'), [Symbol.for('truep'), [Symbol.for('quote'), []]]));
  it('(js/tag sexp "1")', (): any => assertEqual(sexp`1`, 1));
  it('(sexp "1")', (): any => assertEqual(sexp('1'), 1));
  it('(js/tag sexp "\\"foo\\"")', (): any => assertEqual(sexp`"foo"`, 'foo'));
  it('(sexp "\\"foo\\"")', (): any => assertEqual(sexp('"foo"'), 'foo'));
  it('(js/tag sexp "\\"foo;-D\\"")', (): any => assertEqual(sexp`"foo;-D"`, 'foo;-D'));
  it('(sexp "\\"foo;-D\\"")', (): any => assertEqual(sexp('"foo;-D"'), 'foo;-D'));
  it('(js/tag sexp "a")', (): any => assertEqual(sexp`a`, Symbol.for('a')));
  it('(sexp "a")', (): any => assertEqual(sexp('a'), Symbol.for('a')));
  it('(js/tag sexp "(or 1 2)")', (): any => assertEqual(sexp`(or 1 2)`, [Symbol.for('or'), 1, 2]));
  it('(sexp "(or 1 2)")', (): any => assertEqual(sexp('(or 1 2)'), [Symbol.for('or'), 1, 2]));
  xit('(js/tag sexp "(or true false)")', (): any => assertEqual(sexp`(or true false)`, [Symbol.for('or'), true, false]));
  xit('(sexp "(or true false)")', (): any => assertEqual(sexp('(or true false)'), [Symbol.for('or'), true, false]));
  it('(js/tag sexp "foo")', (): any => assertEqual(sexp`foo`, Symbol.for('foo')));
  it('(sexp "foo")', (): any => assertEqual(sexp('foo'), Symbol.for('foo')));
  it('(js/tag sexp "(foo)")', (): any => assertEqual(sexp`(foo)`, [Symbol.for('foo')]));
  it('(sexp "(foo)")', (): any => assertEqual(sexp('(foo)'), [Symbol.for('foo')]));
  it('(js/tag sexp "\n' +
    '      (foo)\n' +
    '  ")', (): any => assertEqual(sexp`
      (foo)
  `, [Symbol.for('foo')]));
  it('(sexp "\n' +
    '      (foo)\n' +
    '  ")', (): any => assertEqual(sexp('\n' +
    '      (foo)\n' +
    '  '), [Symbol.for('foo')]));
  it('(js/tag sexp "\n' +
    '      (foo\n' +
    '        (bar))\n' +
    '  ")', (): any => assertEqual(sexp`
      (foo
        (bar))
  `, [Symbol.for('foo'), [Symbol.for('bar')]]));
  it('(sexp "\n' +
    '      (foo\n' +
    '        (bar))\n' +
    '  ")', (): any => assertEqual(sexp('\n' +
    '      (foo\n' +
    '        (bar))\n' +
    '  '), [Symbol.for('foo'), [Symbol.for('bar')]]));
  it('(js/tag sexp "(foo \\"bar\\")")', (): any => assertEqual(sexp`(foo "bar")`, [Symbol.for('foo'), 'bar']));
  it('(sexp "(foo \\"bar\\")")', (): any => assertEqual(sexp('(foo "bar")'), [Symbol.for('foo'), 'bar']));
  it('(js/tag sexp "(+ 1 1)")', (): any => assertEqual(sexp`(+ 1 1)`, [Symbol.for('+'), 1, 1]));
  it('(sexp "(+ 1 1)")', (): any => assertEqual(sexp('(+ 1 1)'), [Symbol.for('+'), 1, 1]));
  it('(js/tag sexp "\'foo")', (): any => assertEqual(sexp`'foo`, [Symbol.for('quote'), Symbol.for('foo')]));
  it('(sexp "\'foo")', (): any => assertEqual(sexp('\'foo'), [Symbol.for('quote'), Symbol.for('foo')]));
  it('(js/tag sexp "`foo")', (): any => assertEqual(sexp`\`foo`, [Symbol.for('quasiquote'), Symbol.for('foo')]));
  it('(sexp "`foo")', (): any => assertEqual(sexp('`foo'), [Symbol.for('quasiquote'), Symbol.for('foo')]));
  it('(dotted-list? (js/tag sexp "(1 . 2)"))', (): any => assertEqual(((x: any): any => Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')))(sexp`(1 . 2)`), true));
  it('(dotted-list? (js/tag sexp "(1 \'. 2)"))', (): any => assertEqual(((x: any): any => Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')))(sexp`(1 '. 2)`), false));
  it('(js/tag sexp "(+ 2 2)")', (): any => assertEqual(sexp`(+ 2 2)`, [Symbol.for('+'), 2, 2]));
  it('(sexp "(+ 2 2)")', (): any => assertEqual(sexp('(+ 2 2)'), [Symbol.for('+'), 2, 2]));
  xit('(js/tag sexp "(+ ")', (): any => assertEqual(sexp`(+ `, [s`+`, 2, 2]));
  return xit('(sexp "(+ ")', (): any => assertEqual(sexp('(+ '), [s`+`, 2, 2]));
});