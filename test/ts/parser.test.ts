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

import { roseToSexp } from '../../src/ts/rose';

import { s, sexp } from '../../src/ts/sexp';

import { assertEqual, testMacro } from './test-util';

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
  it('(tokenize "")', function (): any {
    return assertEqual(tokenize(''), []);
  });
  it('(tokenize "1")', function (): any {
    return assertEqual(tokenize('1'), [new NumberToken(1)]);
  });
  it('(tokenize "foo")', function (): any {
    return assertEqual(tokenize('foo'), [new SymbolToken('foo')]);
  });
  it('(tokenize "\\\\foo")', function (): any {
    return assertEqual(tokenize('\\foo'), [new SymbolToken('foo')]);
  });
  it('(tokenize "f\\\\oo")', function (): any {
    return assertEqual(tokenize('f\\oo'), [new SymbolToken('foo')]);
  });
  xit('(tokenize "|foo|")', function (): any {
    return assertEqual(tokenize('|foo|'), [new SymbolToken('foo')]);
  });
  it('(tokenize "\\"foo\\"")', function (): any {
    return assertEqual(tokenize('"foo"'), [new StringToken('foo')]);
  });
  it('(tokenize "\\"foo\\\\\\"bar\\"")', function (): any {
    return assertEqual(tokenize('"foo\\"bar"'), [new StringToken('foo"bar')]);
  });
  it('(tokenize "\'foo")', function (): any {
    return assertEqual(tokenize("'foo"), [
      new SymbolToken("'"),
      new SymbolToken('foo'),
    ]);
  });
  it('(tokenize "()")', function (): any {
    return assertEqual(tokenize('()'), [
      new SymbolToken('('),
      new SymbolToken(')'),
    ]);
  });
  it('(tokenize "\'(foo)")', function (): any {
    return assertEqual(tokenize("'(foo)"), [
      new SymbolToken("'"),
      new SymbolToken('('),
      new SymbolToken('foo'),
      new SymbolToken(')'),
    ]);
  });
  it('(tokenize "(foo \\"bar\\")")', function (): any {
    return assertEqual(tokenize('(foo "bar")'), [
      new SymbolToken('('),
      new SymbolToken('foo'),
      new StringToken('bar'),
      new SymbolToken(')'),
    ]);
  });
  it('(tokenize "(foo\n' + '\\"bar\\")")', function (): any {
    return assertEqual(tokenize('(foo\n' + '"bar")'), [
      new SymbolToken('('),
      new SymbolToken('foo'),
      new StringToken('bar'),
      new SymbolToken(')'),
    ]);
  });
  it('(tokenize "(foo) ; bar" (js/obj "comments" #f))', function (): any {
    return assertEqual(
      tokenize('(foo) ; bar', {
        comments: false,
      }),
      [new SymbolToken('('), new SymbolToken('foo'), new SymbolToken(')')]
    );
  });
  xit('(tokenize "\'(foo) ; bar" (js/obj "comments" #f))', function (): any {
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
  xit('(tokenize "(foo ; baz\n' + 'bar)")', function (): any {
    return assertEqual(tokenize('(foo ; baz\n' + 'bar)'), [
      new SymbolToken('('),
      new SymbolToken('foo'),
      new SymbolToken('bar'),
      new SymbolToken(')'),
    ]);
  });
  xit(
    '(tokenize "(foo ; baz\n' + 'bar)" (js/obj "comments" #t))',
    function (): any {
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
    }
  );
  it(
    '(tokenize ";; baz\n' + '(foo bar)" (js/obj "comments" #t))',
    function (): any {
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
    }
  );
  it(
    '(tokenize "  ;; baz\n' + '  (foo bar)" (js/obj "comments" #t))',
    function (): any {
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
    }
  );
  it(
    '(tokenize ";; baz\n' + ';; quux\n' + '(foo bar)" (js/obj "comments" #t))',
    function (): any {
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
    }
  );
  it(
    '(tokenize ";; baz\n' +
      ';;\n' +
      ';; quux\n' +
      '(foo bar)" (js/obj "comments" #t))',
    function (): any {
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
    }
  );
  it(
    '(tokenize ";; baz\n' +
      '\n' +
      ';; quux\n' +
      '(foo bar)" (js/obj "comments" #t))',
    function (): any {
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
    }
  );
  it(
    '(tokenize ";; foo\n' + '`(foo)" (js/obj "comments" #t))',
    function (): any {
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
    }
  );
  it('(tokenize "(foo \'(bar))")', function (): any {
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
  it('(tokenize "((lambda (x) x) \\"Lisp\\")")', function (): any {
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
  return it(
    '(tokenize "(define (foo)\n' +
      '  ;; this\n' +
      '  this)" (js/obj "comments" #t))',
    function (): any {
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
    }
  );
});

describe('parse-rose', function (): any {
  it('(rose->sexp (parse-rose (list (new SymbolToken "exp"))))', function (): any {
    return assertEqual(
      roseToSexp(parseRose([new SymbolToken('exp')])),
      Symbol.for('exp')
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "(") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(parseRose([new SymbolToken('('), new SymbolToken(')')])),
      []
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken ")") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
        parseRose([
          new SymbolToken('('),
          new SymbolToken('('),
          new SymbolToken(')'),
          new SymbolToken(')'),
        ])
      ),
      [[]]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
        parseRose([
          new SymbolToken('('),
          new SymbolToken('foo'),
          new SymbolToken(')'),
        ])
      ),
      [Symbol.for('foo')]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken "lambda") (new SymbolToken "(") (new SymbolToken "x") (new SymbolToken ")") (new SymbolToken "x") (new SymbolToken ")") (new StringToken "Lisp") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
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
        ])
      ),
      [[Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')], 'Lisp']
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "\'") (new SymbolToken "foo"))))', function (): any {
    return assertEqual(
      roseToSexp(parseRose([new SymbolToken("'"), new SymbolToken('foo')])),
      [Symbol.for('quote'), Symbol.for('foo')]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "\'") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
        parseRose([
          new SymbolToken("'"),
          new SymbolToken('('),
          new SymbolToken('foo'),
          new SymbolToken(')'),
        ])
      ),
      [Symbol.for('quote'), [Symbol.for('foo')]]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "\'") (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
        parseRose([
          new SymbolToken("'"),
          new SymbolToken('('),
          new SymbolToken('('),
          new SymbolToken('foo'),
          new SymbolToken(')'),
          new SymbolToken(')'),
        ])
      ),
      [Symbol.for('quote'), [[Symbol.for('foo')]]]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "\'") (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken "(") (new SymbolToken "bar") (new SymbolToken ")") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
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
        ])
      ),
      [Symbol.for('quote'), [[Symbol.for('foo')], [Symbol.for('bar')]]]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "(") (new SymbolToken "quote") (new SymbolToken "(") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken "(") (new SymbolToken "bar") (new SymbolToken ")") (new SymbolToken ")") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
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
        ])
      ),
      [Symbol.for('quote'), [[Symbol.for('foo')], [Symbol.for('bar')]]]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "(") (new SymbolToken "truep") (new SymbolToken "\'") (new SymbolToken "foo") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
        parseRose([
          new SymbolToken('('),
          new SymbolToken('truep'),
          new SymbolToken("'"),
          new SymbolToken('foo'),
          new SymbolToken(')'),
        ])
      ),
      [Symbol.for('truep'), [Symbol.for('quote'), Symbol.for('foo')]]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "(") (new SymbolToken "truep") (new SymbolToken "\'") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
        parseRose([
          new SymbolToken('('),
          new SymbolToken('truep'),
          new SymbolToken("'"),
          new SymbolToken('('),
          new SymbolToken('foo'),
          new SymbolToken(')'),
          new SymbolToken(')'),
        ])
      ),
      [Symbol.for('truep'), [Symbol.for('quote'), [Symbol.for('foo')]]]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "(") (new SymbolToken "truep") (new SymbolToken "`") (new SymbolToken "foo") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
        parseRose([
          new SymbolToken('('),
          new SymbolToken('truep'),
          new SymbolToken('`'),
          new SymbolToken('foo'),
          new SymbolToken(')'),
        ])
      ),
      [Symbol.for('truep'), [Symbol.for('quasiquote'), Symbol.for('foo')]]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "(") (new SymbolToken "truep") (new SymbolToken "`") (new SymbolToken "(") (new SymbolToken "foo") (new SymbolToken ")") (new SymbolToken ")"))))', function (): any {
    return assertEqual(
      roseToSexp(
        parseRose([
          new SymbolToken('('),
          new SymbolToken('truep'),
          new SymbolToken('`'),
          new SymbolToken('('),
          new SymbolToken('foo'),
          new SymbolToken(')'),
          new SymbolToken(')'),
        ])
      ),
      [Symbol.for('truep'), [Symbol.for('quasiquote'), [Symbol.for('foo')]]]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken "`") (new SymbolToken "foo"))))', function (): any {
    return assertEqual(
      roseToSexp(parseRose([new SymbolToken('`'), new SymbolToken('foo')])),
      [Symbol.for('quasiquote'), Symbol.for('foo')]
    );
  });
  it('(rose->sexp (parse-rose (list (new SymbolToken ",") (new SymbolToken "foo"))))', function (): any {
    return assertEqual(
      roseToSexp(parseRose([new SymbolToken(','), new SymbolToken('foo')])),
      [Symbol.for('unquote'), Symbol.for('foo')]
    );
  });
  return it('(rose->sexp (parse-rose (list (new SymbolToken ",@") (new SymbolToken "foo"))))', function (): any {
    return assertEqual(
      roseToSexp(parseRose([new SymbolToken(',@'), new SymbolToken('foo')])),
      [Symbol.for('unquote-splicing'), Symbol.for('foo')]
    );
  });
});

describe('read', function (): any {
  it('(read "(foo) ;comment")', function (): any {
    return assertEqual(read('(foo) ;comment'), [Symbol.for('foo')]);
  });
  it('(read "(foo) ;; this is a comment")', function (): any {
    return assertEqual(read('(foo) ;; this is a comment'), [Symbol.for('foo')]);
  });
  it(
    '(read "(define (foo)\n' +
      '  ;; this is a comment\n' +
      '  (bar) ; this is also a comment\n' +
      '  (baz))")',
    function (): any {
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
    }
  );
  it('(read "\\"string ;-D\\"")', function (): any {
    return assertEqual(read('"string ;-D"'), 'string ;-D');
  });
  it('(read "\\"string\\\\\\"test\\"")', function (): any {
    return assertEqual(read('"string\\"test"'), 'string"test');
  });
  it('(read "\\"string\\\\ntest\\"")', function (): any {
    return assertEqual(read('"string\\ntest"'), 'string\n' + 'test');
  });
  it('(read "\\"string\\\\\\\\ntest\\"")', function (): any {
    return assertEqual(read('"string\\\\ntest"'), 'string\\ntest');
  });
  it('(read "\\"string\n' + 'test\\"")', function (): any {
    return assertEqual(read('"string\n' + 'test"'), 'string\n' + 'test');
  });
  it('(read "\'()")', function (): any {
    return assertEqual(read("'()"), [Symbol.for('quote'), []]);
  });
  it('(read "`()")', function (): any {
    return assertEqual(read('`()'), [Symbol.for('quasiquote'), []]);
  });
  it('(read "`(,exp)")', function (): any {
    return assertEqual(read('`(,exp)'), [
      Symbol.for('quasiquote'),
      [[Symbol.for('unquote'), Symbol.for('exp')]],
    ]);
  });
  it('(read "`((quote ,exp))")', function (): any {
    return assertEqual(read('`((quote ,exp))'), [
      Symbol.for('quasiquote'),
      [[Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]],
    ]);
  });
  it('(read "`(\',exp)")', function (): any {
    return assertEqual(read("`(',exp)"), [
      Symbol.for('quasiquote'),
      [[Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]]],
    ]);
  });
  it('(read "`(\'\',exp)")', function (): any {
    return assertEqual(read("`('',exp)"), [
      Symbol.for('quasiquote'),
      [
        [
          Symbol.for('quote'),
          [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]],
        ],
      ],
    ]);
  });
  it("(read \"`(''',exp)\")", function (): any {
    return assertEqual(read("`(''',exp)"), [
      Symbol.for('quasiquote'),
      [
        [
          Symbol.for('quote'),
          [
            Symbol.for('quote'),
            [Symbol.for('quote'), [Symbol.for('unquote'), Symbol.for('exp')]],
          ],
        ],
      ],
    ]);
  });
  return it('(read "(define foo `(,bar))")', function (): any {
    return assertEqual(read('(define foo `(,bar))'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('bar')]]],
    ]);
  });
});

describe('read-rose', function (): any {
  it('(rose->sexp (read-rose ";; comment\n' + '(foo)"))', function (): any {
    return assertEqual(roseToSexp(readRose(';; comment\n' + '(foo)')), [
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
  return it('(rose->sexp (read-rose "(foo) ;comment"))', function (): any {
    return assertEqual(roseToSexp(readRose('(foo) ;comment')), [
      Symbol.for('foo'),
    ]);
  });
});

describe('sexp', function (): any {
  it('(js/tag sexp "")', function (): any {
    return assertEqual(sexp``, []);
  });
  it('(js/tag sexp "()")', function (): any {
    return assertEqual(sexp`()`, []);
  });
  it('(sexp "()")', function (): any {
    return assertEqual(sexp('()'), []);
  });
  it('(js/tag sexp "\'()")', function (): any {
    return assertEqual(sexp`'()`, [Symbol.for('quote'), []]);
  });
  it('(sexp "\'()")', function (): any {
    return assertEqual(sexp("'()"), [Symbol.for('quote'), []]);
  });
  it('(js/tag sexp "(truep \'())")', function (): any {
    return assertEqual(sexp`(truep '())`, [
      Symbol.for('truep'),
      [Symbol.for('quote'), []],
    ]);
  });
  it('(sexp "(truep \'())")', function (): any {
    return assertEqual(sexp("(truep '())"), [
      Symbol.for('truep'),
      [Symbol.for('quote'), []],
    ]);
  });
  it('(js/tag sexp "1")', function (): any {
    return assertEqual(sexp`1`, 1);
  });
  it('(sexp "1")', function (): any {
    return assertEqual(sexp('1'), 1);
  });
  it('(js/tag sexp "\\"foo\\"")', function (): any {
    return assertEqual(sexp`"foo"`, 'foo');
  });
  it('(sexp "\\"foo\\"")', function (): any {
    return assertEqual(sexp('"foo"'), 'foo');
  });
  it('(js/tag sexp "\\"foo;-D\\"")', function (): any {
    return assertEqual(sexp`"foo;-D"`, 'foo;-D');
  });
  it('(sexp "\\"foo;-D\\"")', function (): any {
    return assertEqual(sexp('"foo;-D"'), 'foo;-D');
  });
  it('(js/tag sexp "a")', function (): any {
    return assertEqual(sexp`a`, Symbol.for('a'));
  });
  it('(sexp "a")', function (): any {
    return assertEqual(sexp('a'), Symbol.for('a'));
  });
  it('(js/tag sexp "(or 1 2)")', function (): any {
    return assertEqual(sexp`(or 1 2)`, [Symbol.for('or'), 1, 2]);
  });
  it('(sexp "(or 1 2)")', function (): any {
    return assertEqual(sexp('(or 1 2)'), [Symbol.for('or'), 1, 2]);
  });
  xit('(js/tag sexp "(or true false)")', function (): any {
    return assertEqual(sexp`(or true false)`, [Symbol.for('or'), true, false]);
  });
  xit('(sexp "(or true false)")', function (): any {
    return assertEqual(sexp('(or true false)'), [
      Symbol.for('or'),
      true,
      false,
    ]);
  });
  it('(js/tag sexp "foo")', function (): any {
    return assertEqual(sexp`foo`, Symbol.for('foo'));
  });
  it('(sexp "foo")', function (): any {
    return assertEqual(sexp('foo'), Symbol.for('foo'));
  });
  it('(js/tag sexp "(foo)")', function (): any {
    return assertEqual(sexp`(foo)`, [Symbol.for('foo')]);
  });
  it('(sexp "(foo)")', function (): any {
    return assertEqual(sexp('(foo)'), [Symbol.for('foo')]);
  });
  it('(js/tag sexp "\n' + '      (foo)\n' + '  ")', function (): any {
    return assertEqual(
      sexp`
      (foo)
  `,
      [Symbol.for('foo')]
    );
  });
  it('(sexp "\n' + '      (foo)\n' + '  ")', function (): any {
    return assertEqual(sexp('\n' + '      (foo)\n' + '  '), [
      Symbol.for('foo'),
    ]);
  });
  it(
    '(js/tag sexp "\n' + '      (foo\n' + '        (bar))\n' + '  ")',
    function (): any {
      return assertEqual(
        sexp`
      (foo
        (bar))
  `,
        [Symbol.for('foo'), [Symbol.for('bar')]]
      );
    }
  );
  it(
    '(sexp "\n' + '      (foo\n' + '        (bar))\n' + '  ")',
    function (): any {
      return assertEqual(
        sexp('\n' + '      (foo\n' + '        (bar))\n' + '  '),
        [Symbol.for('foo'), [Symbol.for('bar')]]
      );
    }
  );
  it('(js/tag sexp "(foo \\"bar\\")")', function (): any {
    return assertEqual(sexp`(foo "bar")`, [Symbol.for('foo'), 'bar']);
  });
  it('(sexp "(foo \\"bar\\")")', function (): any {
    return assertEqual(sexp('(foo "bar")'), [Symbol.for('foo'), 'bar']);
  });
  it('(js/tag sexp "(+ 1 1)")', function (): any {
    return assertEqual(sexp`(+ 1 1)`, [Symbol.for('+'), 1, 1]);
  });
  it('(sexp "(+ 1 1)")', function (): any {
    return assertEqual(sexp('(+ 1 1)'), [Symbol.for('+'), 1, 1]);
  });
  it('(js/tag sexp "\'foo")', function (): any {
    return assertEqual(sexp`'foo`, [Symbol.for('quote'), Symbol.for('foo')]);
  });
  it('(sexp "\'foo")', function (): any {
    return assertEqual(sexp("'foo"), [Symbol.for('quote'), Symbol.for('foo')]);
  });
  it('(js/tag sexp "`foo")', function (): any {
    return assertEqual(sexp`\`foo`, [
      Symbol.for('quasiquote'),
      Symbol.for('foo'),
    ]);
  });
  it('(sexp "`foo")', function (): any {
    return assertEqual(sexp('`foo'), [
      Symbol.for('quasiquote'),
      Symbol.for('foo'),
    ]);
  });
  it('(dotted-list? (js/tag sexp "(1 . 2)"))', function (): any {
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
  it('(dotted-list? (js/tag sexp "(1 \'. 2)"))', function (): any {
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
  it('(js/tag sexp "(+ 2 2)")', function (): any {
    return assertEqual(sexp`(+ 2 2)`, [Symbol.for('+'), 2, 2]);
  });
  it('(sexp "(+ 2 2)")', function (): any {
    return assertEqual(sexp('(+ 2 2)'), [Symbol.for('+'), 2, 2]);
  });
  xit('(js/tag sexp "(+ ")', function (): any {
    return assertEqual(sexp`(+ `, [s`+`, 2, 2]);
  });
  return xit('(sexp "(+ ")', function (): any {
    return assertEqual(sexp('(+ '), [s`+`, 2, 2]);
  });
});
