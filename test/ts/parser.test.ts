import * as chai from 'chai';

import {
  LeadingCommentToken,
  NumberToken,
  StringToken,
  SymbolToken,
  TrailingCommentToken,
  parseRose,
  read,
  readRose,
  tokenize,
} from '../../src/ts/parser';

import { s, sexp } from '../../src/ts/sexp';

import { assertEqual } from './test-util';

const [lastCdr]: any[] = ((): any => {
  function lastCdr_(lst: any): any {
    if (!Array.isArray(lst)) {
      return undefined;
    } else if (
      Array.isArray(lst) &&
      lst.length >= 3 &&
      lst[lst.length - 2] === Symbol.for('.')
    ) {
      let result: any = lst;
      while (
        Array.isArray(result) &&
        result.length >= 3 &&
        result[result.length - 2] === Symbol.for('.')
      ) {
        result = result[result.length - 1];
      }
      return result;
    } else {
      return [];
    }
  }
  return [lastCdr_];
})();

describe('tokenize', function (): any {
  it("''", function (): any {
    return assertEqual(tokenize(''), []);
  });
  it('1', function (): any {
    return assertEqual(tokenize('1'), [new NumberToken(1)]);
  });
  it('foo', function (): any {
    return assertEqual(tokenize('foo'), [new SymbolToken('foo')]);
  });
  it('"foo"', function (): any {
    return assertEqual(tokenize('"foo"'), [new StringToken('foo')]);
  });
  it('"foo\\"bar"', function (): any {
    return assertEqual(tokenize('"foo\\"bar"'), [new StringToken('foo"bar')]);
  });
  it("'foo", function (): any {
    return assertEqual(tokenize("'foo"), [
      new SymbolToken("'"),
      new SymbolToken('foo'),
    ]);
  });
  it('()', function (): any {
    return assertEqual(tokenize('()'), [
      new SymbolToken('('),
      new SymbolToken(')'),
    ]);
  });
  it("'(foo)", function (): any {
    return assertEqual(tokenize("'(foo)"), [
      new SymbolToken("'"),
      new SymbolToken('('),
      new SymbolToken('foo'),
      new SymbolToken(')'),
    ]);
  });
  it('(foo "bar")', function (): any {
    return assertEqual(tokenize('(foo "bar")'), [
      new SymbolToken('('),
      new SymbolToken('foo'),
      new StringToken('bar'),
      new SymbolToken(')'),
    ]);
  });
  it('(foo\n' + '"bar")', function (): any {
    return assertEqual(tokenize('(foo\n' + '"bar")'), [
      new SymbolToken('('),
      new SymbolToken('foo'),
      new StringToken('bar'),
      new SymbolToken(')'),
    ]);
  });
  it('(foo) ; bar', function (): any {
    return assertEqual(
      tokenize('(foo) ; bar', {
        comments: false,
      }),
      [new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')')]
    );
  });
  it("'(foo) ; bar", function (): any {
    return assertEqual(
      tokenize("'(foo) ; bar", {
        comments: false,
      }),
      [
        new SymbolToken("'"),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
      ]
    );
  });
  xit('(foo ; baz\n' + 'bar)', function (): any {
    return assertEqual(tokenize('(foo ; baz\n' + 'bar)'), [
      new SymbolToken('('),
      new SymbolToken('foo'),
      new SymbolToken('bar'),
      new SymbolToken(')'),
    ]);
  });
  xit('(foo ; baz\n' + 'bar)', function (): any {
    return assertEqual(
      tokenize('(foo ; baz\n' + 'bar)', {
        comments: true,
      }),
      [
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken('bar'),
        new SymbolToken(')'),
        new TrailingCommentToken('; baz'),
      ]
    );
  });
  it(';; baz\n' + '(foo bar)', function (): any {
    return assertEqual(
      tokenize(';; baz\n' + '(foo bar)', {
        comments: true,
      }),
      [
        new LeadingCommentToken(';; baz\n'),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken('bar'),
        new SymbolToken(')'),
      ]
    );
  });
  it('  ;; baz\n' + '  (foo bar)', function (): any {
    return assertEqual(
      tokenize('  ;; baz\n' + '  (foo bar)', {
        comments: true,
      }),
      [
        new LeadingCommentToken(';; baz\n'),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken('bar'),
        new SymbolToken(')'),
      ]
    );
  });
  it(';; baz\n' + ';; quux\n' + '(foo bar)', function (): any {
    return assertEqual(
      tokenize(';; baz\n' + ';; quux\n' + '(foo bar)', {
        comments: true,
      }),
      [
        new LeadingCommentToken(';; baz\n' + ';; quux\n'),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken('bar'),
        new SymbolToken(')'),
      ]
    );
  });
  it(';; baz\n' + ';;\n' + ';; quux\n' + '(foo bar)', function (): any {
    return assertEqual(
      tokenize(';; baz\n' + ';;\n' + ';; quux\n' + '(foo bar)', {
        comments: true,
      }),
      [
        new LeadingCommentToken(';; baz\n' + ';;\n' + ';; quux\n'),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken('bar'),
        new SymbolToken(')'),
      ]
    );
  });
  it(';; baz\n' + '\n' + ';; quux\n' + '(foo bar)', function (): any {
    return assertEqual(
      tokenize(';; baz\n' + '\n' + ';; quux\n' + '(foo bar)', {
        comments: true,
      }),
      [
        new LeadingCommentToken(';; baz\n' + '\n' + ';; quux\n'),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken('bar'),
        new SymbolToken(')'),
      ]
    );
  });
  it(';; foo\n' + '`(foo)', function (): any {
    return assertEqual(
      tokenize(';; foo\n' + '`(foo)', {
        comments: true,
      }),
      [
        new LeadingCommentToken(';; foo\n'),
        new SymbolToken('`'),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
      ]
    );
  });
  it("(foo '(bar))", function (): any {
    return assertEqual(tokenize("(foo '(bar))"), [
      new SymbolToken('('),
      new SymbolToken('foo'),
      new SymbolToken("'"),
      new SymbolToken('('),
      new SymbolToken('bar'),
      new SymbolToken(')'),
      new SymbolToken(')'),
    ]);
  });
  it('((lambda (x) x) "Lisp")', function (): any {
    return assertEqual(tokenize('((lambda (x) x) "Lisp")'), [
      new SymbolToken('('),
      new SymbolToken('('),
      new SymbolToken('lambda'),
      new SymbolToken('('),
      new SymbolToken('x'),
      new SymbolToken(')'),
      new SymbolToken('x'),
      new SymbolToken(')'),
      new StringToken('Lisp'),
      new SymbolToken(')'),
    ]);
  });
  return it('(define (foo)\n' + '  ;; this\n' + '  this)', function (): any {
    return assertEqual(
      tokenize('(define (foo)\n' + '  ;; this\n' + '  this)', {
        comments: true,
      }),
      [
        new SymbolToken('('),
        new SymbolToken('define'),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
        new LeadingCommentToken(';; this\n'),
        new SymbolToken('this'),
        new SymbolToken(')'),
      ]
    );
  });
});

describe('parse-rose', function (): any {
  it('[s`exp`]', function (): any {
    return assertEqual(
      parseRose([new SymbolToken('exp')]).getValue(),
      Symbol.for('exp')
    );
  });
  it('[s`(`, s`)`]', function (): any {
    return assertEqual(
      parseRose([new SymbolToken('('), new SymbolToken(')')]).getValue(),
      []
    );
  });
  it('[s`(`, s`(`, s`)`, s`)`]', function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken('('),
        new SymbolToken('('),
        new SymbolToken(')'),
        new SymbolToken(')'),
      ]).getValue(),
      [[]]
    );
  });
  it('[s`(`, s`foo`, s`)`]', function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
      ]).getValue(),
      [Symbol.for('foo')]
    );
  });
  it("[s`(`, s`(`, s`lambda`, s`(`, s`x`, s`)`, s`x`, s`)`, 'Lisp', s`)`]", function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken('('),
        new SymbolToken('('),
        new SymbolToken('lambda'),
        new SymbolToken('('),
        new SymbolToken('x'),
        new SymbolToken(')'),
        new SymbolToken('x'),
        new SymbolToken(')'),
        new StringToken('Lisp'),
        new SymbolToken(')'),
      ]).getValue(),
      [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 'Lisp']
    );
  });
  it("[s`'`, s`foo`]", function (): any {
    return assertEqual(
      parseRose([new SymbolToken("'"), new SymbolToken('foo')]).getValue(),
      [Symbol.for('quote'), Symbol.for('foo')]
    );
  });
  it("[s`'`, s`(`, s`foo`, s`)`]", function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken("'"),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
      ]).getValue(),
      [Symbol.for('quote'), [Symbol.for('foo')]]
    );
  });
  it("[s`'`, s`(`, s`(`, s`foo`, s`)`, s`)`]", function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken("'"),
        new SymbolToken('('),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
        new SymbolToken(')'),
      ]).getValue(),
      [Symbol.for('quote'), [[Symbol.for('foo')]]]
    );
  });
  it("[s`'`, s`(`, s`(`, s`foo`, s`)`, s`(`, s`bar`, s`)`, s`)`]", function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken("'"),
        new SymbolToken('('),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
        new SymbolToken('('),
        new SymbolToken('bar'),
        new SymbolToken(')'),
        new SymbolToken(')'),
      ]).getValue(),
      [Symbol.for('quote'), [[Symbol.for('foo')], [Symbol.for('bar')]]]
    );
  });
  it('[s`(`, s`quote`, s`(`, s`(`, s`foo`, s`)`, s`(`, s`bar`, s`)`, s`)`, s`)`]', function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken('('),
        new SymbolToken('quote'),
        new SymbolToken('('),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
        new SymbolToken('('),
        new SymbolToken('bar'),
        new SymbolToken(')'),
        new SymbolToken(')'),
        new SymbolToken(')'),
      ]).getValue(),
      [Symbol.for('quote'), [[Symbol.for('foo')], [Symbol.for('bar')]]]
    );
  });
  it("[s`(`, s`truep`, s`'`, s`foo`, s`)`]", function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken('('),
        new SymbolToken('truep'),
        new SymbolToken("'"),
        new SymbolToken('foo'),
        new SymbolToken(')'),
      ]).getValue(),
      [Symbol.for('truep'), [Symbol.for('quote'), Symbol.for('foo')]]
    );
  });
  it("[s`(`, s`truep`, s`'`, s`foo`, s`)`]", function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken('('),
        new SymbolToken('truep'),
        new SymbolToken("'"),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
        new SymbolToken(')'),
      ]).getValue(),
      [Symbol.for('truep'), [Symbol.for('quote'), [Symbol.for('foo')]]]
    );
  });
  it('[s`(`, s`truep`, s`\\``, s`foo`, s`)`]', function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken('('),
        new SymbolToken('truep'),
        new SymbolToken('`'),
        new SymbolToken('foo'),
        new SymbolToken(')'),
      ]).getValue(),
      [Symbol.for('truep'), [Symbol.for('quasiquote'), Symbol.for('foo')]]
    );
  });
  it('[s`(`, s`truep`, s`\\``, s`foo`, s`)`]', function (): any {
    return assertEqual(
      parseRose([
        new SymbolToken('('),
        new SymbolToken('truep'),
        new SymbolToken('`'),
        new SymbolToken('('),
        new SymbolToken('foo'),
        new SymbolToken(')'),
        new SymbolToken(')'),
      ]).getValue(),
      [Symbol.for('truep'), [Symbol.for('quasiquote'), [Symbol.for('foo')]]]
    );
  });
  it('[s`\\``, s`foo`]', function (): any {
    return assertEqual(
      parseRose([new SymbolToken('`'), new SymbolToken('foo')]).getValue(),
      [Symbol.for('quasiquote'), Symbol.for('foo')]
    );
  });
  it('[s`,`, s`foo`]', function (): any {
    return assertEqual(
      parseRose([new SymbolToken(','), new SymbolToken('foo')]).getValue(),
      [Symbol.for('unquote'), Symbol.for('foo')]
    );
  });
  return it('[s`,@`, s`foo`]', function (): any {
    return assertEqual(
      parseRose([new SymbolToken(',@'), new SymbolToken('foo')]).getValue(),
      [Symbol.for('unquote-splicing'), Symbol.for('foo')]
    );
  });
});

describe('read', function (): any {
  it('(foo) ;comment', function (): any {
    return assertEqual(read('(foo) ;comment'), [Symbol.for('foo')]);
  });
  it('(foo) ;; this is a comment', function (): any {
    return assertEqual(read('(foo) ;; this is a comment'), [Symbol.for('foo')]);
  });
  it('(define (foo) (bar) (baz))', function (): any {
    return assertEqual(
      read(
        '(define (foo)\n' +
          '  ;; this is a comment\n' +
          '  (bar) ; this is also a comment\n' +
          '  (baz))'
      ),
      [
        Symbol.for('define'),
        [Symbol.for('foo')],
        [Symbol.for('bar')],
        [Symbol.for('baz')],
      ]
    );
  });
  it('string ;-D', function (): any {
    return assertEqual(read('"string ;-D"'), 'string ;-D');
  });
  it('"string\\"test"', function (): any {
    return assertEqual(read('"string\\"test"'), 'string"test');
  });
  it('"string\\ntest"', function (): any {
    return assertEqual(read('"string\\ntest"'), 'string\n' + 'test');
  });
  it('"string\\\\ntest"', function (): any {
    return assertEqual(read('"string\\\\ntest"'), 'string\\ntest');
  });
  return it('"string\\ntest"', function (): any {
    return assertEqual(read('"string\n' + 'test"'), 'string\n' + 'test');
  });
});

describe('read-rose', function (): any {
  it(';; comment\n' + '(foo)', function (): any {
    return assertEqual(readRose(';; comment\n' + '(foo)').getValue(), [
      Symbol.for('foo'),
    ]);
  });
  it(';; comment\n' + '(foo), comments', function (): any {
    const actual: any = readRose(';; comment\n' + '(foo)', {
      comments: true,
    });
    assertEqual(actual.getValue(), [Symbol.for('foo')]);
    return assertEqual(actual.getProperty('comments'), [
      new LeadingCommentToken(';; comment\n'),
    ]);
  });
  it(';; comment\n' + '`(foo), comments', function (): any {
    const actual: any = readRose(';; comment\n' + '`(foo)', {
      comments: true,
    });
    assertEqual(actual.getValue(), [
      Symbol.for('quasiquote'),
      [Symbol.for('foo')],
    ]);
    return assertEqual(actual.getProperty('comments'), [
      new LeadingCommentToken(';; comment\n'),
    ]);
  });
  return it('(foo) ;comment', function (): any {
    return assertEqual(readRose('(foo) ;comment').getValue(), [
      Symbol.for('foo'),
    ]);
  });
});

describe('sexp', function (): any {
  it("sexp('')", function (): any {
    assertEqual(sexp``, []);
    return assertEqual(sexp(''), []);
  });
  it("sexp('()')", function (): any {
    assertEqual(sexp`()`, []);
    return assertEqual(sexp('()'), []);
  });
  it("sexp('()')", function (): any {
    assertEqual(sexp`'()`, [Symbol.for('quote'), []]);
    return assertEqual(sexp("'()"), [Symbol.for('quote'), []]);
  });
  it("sexp(`(truep '())`)", function (): any {
    assertEqual(sexp`(truep '())`, [
      Symbol.for('truep'),
      [Symbol.for('quote'), []],
    ]);
    return assertEqual(sexp("(truep '())"), [
      Symbol.for('truep'),
      [Symbol.for('quote'), []],
    ]);
  });
  it("sexp('1')", function (): any {
    assertEqual(sexp`1`, 1);
    return assertEqual(sexp('1'), 1);
  });
  it('sexp(\'"foo"\')', function (): any {
    assertEqual(sexp`"foo"`, 'foo');
    return assertEqual(sexp('"foo"'), 'foo');
  });
  it('sexp(\'"foo;-D"\')', function (): any {
    assertEqual(sexp`"foo;-D"`, 'foo;-D');
    return assertEqual(sexp('"foo;-D"'), 'foo;-D');
  });
  it("sexp('a')", function (): any {
    assertEqual(sexp`a`, Symbol.for('a'));
    return assertEqual(sexp('a'), Symbol.for('a'));
  });
  it("sexp('(or 1 2)')", function (): any {
    assertEqual(sexp`(or 1 2)`, [Symbol.for('or'), 1, 2]);
    return assertEqual(sexp('(or 1 2)'), [Symbol.for('or'), 1, 2]);
  });
  xit("sexp('(or true false)')", function (): any {
    assertEqual(sexp`(or true false)`, [Symbol.for('or'), true, false]);
    return assertEqual(sexp('(or true false)'), [
      Symbol.for('or'),
      true,
      false,
    ]);
  });
  it('sexp`foo`', function (): any {
    assertEqual(sexp`foo`, Symbol.for('foo'));
    return assertEqual(sexp('foo'), Symbol.for('foo'));
  });
  it("sexp('(foo)')", function (): any {
    assertEqual(sexp`(foo)`, [Symbol.for('foo')]);
    return assertEqual(sexp('(foo)'), [Symbol.for('foo')]);
  });
  it("sexp('\\n(foo)\\n')", function (): any {
    assertEqual(
      sexp`
      (foo)
  `,
      [Symbol.for('foo')]
    );
    return assertEqual(sexp('\n' + '      (foo)\n' + '  '), [
      Symbol.for('foo'),
    ]);
  });
  it("sexp('\\n(foo\\n(bar))\\n')", function (): any {
    assertEqual(
      sexp`
      (foo
        (bar))
  `,
      [Symbol.for('foo'), [Symbol.for('bar')]]
    );
    return assertEqual(
      sexp('\n' + '      (foo\n' + '        (bar))\n' + '  '),
      [Symbol.for('foo'), [Symbol.for('bar')]]
    );
  });
  it('sexp(\'(foo "bar")\')', function (): any {
    assertEqual(sexp`(foo "bar")`, [Symbol.for('foo'), 'bar']);
    return assertEqual(sexp('(foo "bar")'), [Symbol.for('foo'), 'bar']);
  });
  it("sexp('(+ 1 1')", function (): any {
    assertEqual(sexp`(+ 1 1)`, [Symbol.for('+'), 1, 1]);
    return assertEqual(sexp('(+ 1 1)'), [Symbol.for('+'), 1, 1]);
  });
  it("sexp(`'foo`)", function (): any {
    assertEqual(sexp`'foo`, [Symbol.for('quote'), Symbol.for('foo')]);
    return assertEqual(sexp("'foo"), [Symbol.for('quote'), Symbol.for('foo')]);
  });
  it("sexp('`foo')", function (): any {
    assertEqual(sexp`\`foo`, [Symbol.for('quasiquote'), Symbol.for('foo')]);
    return assertEqual(sexp('`foo'), [
      Symbol.for('quasiquote'),
      Symbol.for('foo'),
    ]);
  });
  it('sexp`(1 . 2)`', function (): any {
    return assertEqual(
      ((): any => {
        const x: any = sexp`(1 . 2)`;
        return (
          Array.isArray(x) &&
          x.length >= 3 &&
          x[x.length - 2] === Symbol.for('.') &&
          !((): any => {
            const x1: any = lastCdr(x);
            return Array.isArray(x1) && x1.length === 0;
          })()
        );
      })(),
      true
    );
  });
  it("sexp`(1 '. 2)`", function (): any {
    return assertEqual(
      ((): any => {
        const x: any = sexp`(1 '. 2)`;
        return (
          Array.isArray(x) &&
          x.length >= 3 &&
          x[x.length - 2] === Symbol.for('.') &&
          !((): any => {
            const x1: any = lastCdr(x);
            return Array.isArray(x1) && x1.length === 0;
          })()
        );
      })(),
      false
    );
  });
  it('sexp`(+ 2 2)`', function (): any {
    assertEqual(sexp`(+ 2 2)`, [Symbol.for('+'), 2, 2]);
    return assertEqual(sexp('(+ 2 2)'), [Symbol.for('+'), 2, 2]);
  });
  return xit('sexp`(+ ${2} ${2})`', function (): any {});
});
