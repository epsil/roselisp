import { I } from '../../src/ts/combinators';

import {
  LispEnvironment,
  compilationEnvironment,
  compile,
  compileModules,
  compileWithEnvironment,
  definitionToMacro,
  splitComments,
} from '../../src/ts/language';

import { defineMacroToLambdaForm } from '../../src/ts/macros';

import { readRose } from '../../src/ts/parser';

import { sexp } from '../../src/ts/sexp';

import { assertEqual, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('Symbols', function (): any {
  it('(compile #t)', function (): any {
    return assertEqual(compile(true), 'true;');
  });
  it('(compile #f)', function (): any {
    return assertEqual(compile(false), 'false;');
  });
  it("(compile 'undefined)", function (): any {
    return assertEqual(compile(Symbol.for('undefined')), 'undefined;');
  });
  it("(compile 'js/undefined)", function (): any {
    return assertEqual(compile(Symbol.for('js/undefined')), 'undefined;');
  });
  it("(compile 'js-undefined)", function (): any {
    return assertEqual(compile(Symbol.for('js-undefined')), 'undefined;');
  });
  it("(compile 'js/null)", function (): any {
    return assertEqual(compile(Symbol.for('js/null')), 'null;');
  });
  it("(compile 'js-null)", function (): any {
    return assertEqual(compile(Symbol.for('js-null')), 'null;');
  });
  xit("(compile 'nil)", function (): any {
    return assertEqual(compile(Symbol.for('nil')), 'null;');
  });
  xit("(compile 'null)", function (): any {
    return assertEqual(compile(Symbol.for('null')), '[];');
  });
  it('(compile \'foo-bar :case "none")', function (): any {
    return assertEqual(
      compile(Symbol.for('foo-bar'), Symbol.for(':case'), 'none'),
      'foo-bar;'
    );
  });
  it("(compile 'foo-bar)", function (): any {
    return assertEqual(compile(Symbol.for('foo-bar')), 'fooBar;');
  });
  it("(compile 'foo/bar)", function (): any {
    return assertEqual(compile(Symbol.for('foo/bar')), 'fooBar;');
  });
  it("(compile 'foo!)", function (): any {
    return assertEqual(compile(Symbol.for('foo!')), 'foox;');
  });
  it("(compile 'foo-bar!)", function (): any {
    return assertEqual(compile(Symbol.for('foo-bar!')), 'fooBarX;');
  });
  it("(compile 'foo?)", function (): any {
    return assertEqual(compile(Symbol.for('foo?')), 'foop;');
  });
  it("(compile 'foo-bar?)", function (): any {
    return assertEqual(compile(Symbol.for('foo-bar?')), 'fooBarP;');
  });
  it("(compile '*foo-bar*)", function (): any {
    return assertEqual(compile(Symbol.for('*foo-bar*')), 'starFooBarStar;');
  });
  it("(compile ''*foo-bar*)", function (): any {
    return assertEqual(
      compile([Symbol.for('quote'), Symbol.for('*foo-bar*')]),
      "Symbol.for('*foo-bar*');"
    );
  });
  it("(compile 'A)", function (): any {
    return assertEqual(compile(Symbol.for('A')), 'A;');
  });
  return it("(compile '(module m scheme (define lst (map symbol? '(a b c)))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('define'),
          Symbol.for('lst'),
          [
            Symbol.for('map'),
            Symbol.for('symbol?'),
            [
              Symbol.for('quote'),
              [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')],
            ],
          ],
        ],
      ]),
      "let lst = [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')].map(function (x) {\n" +
        "  return typeof x === 'symbol';\n" +
        '});'
    );
  });
});

describe('gensym', function (): any {
  it('(compile \'(gensym "x"))', function (): any {
    return assertEqual(compile([Symbol.for('gensym'), 'x']), "Symbol('x');");
  });
  it('(compile `(define ,(gensym "x") 1))', function (): any {
    return assertEqual(
      compile([Symbol.for('define'), Symbol('x'), 1]),
      'let x = 1;'
    );
  });
  it('(compile `(let ((x 0)) (define ,(gensym "x") 1)))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('let'),
        [[Symbol.for('x'), 0]],
        [Symbol.for('define'), Symbol('x'), 1],
      ]),
      'let x = 0;\n' + '\n' + 'let x1 = 1;'
    );
  });
  it('(compile `(let ((x 0)) (define ,(gensym "x") 1) (let ((x1 0)))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('let'),
        [[Symbol.for('x'), 0]],
        [Symbol.for('define'), Symbol('x'), 1],
        [Symbol.for('let'), [[Symbol.for('x1'), 0]]],
      ]),
      'let x = 0;\n' +
        '\n' +
        'let x1 = 1;\n' +
        '\n' +
        '{\n' +
        '  let x1 = 0;\n' +
        '}'
    );
  });
  xit('(compile (let ((gensym-x (gensym "x"))) `(let ((x 0)) (define ,gensym-x 1) (let ((x1 0)) (define ,gensym-x 1)))))', function (): any {
    return assertEqual(
      compile(
        ((): any => {
          const gensymX: any = Symbol('x');
          return [
            Symbol.for('let'),
            [[Symbol.for('x'), 0]],
            [Symbol.for('define'), gensymX, 1],
            [
              Symbol.for('let'),
              [[Symbol.for('x1'), 0]],
              [Symbol.for('define'), gensymX, 1],
            ],
          ];
        })()
      ),
      'let x = 0;\n' +
        '\n' +
        'let x2 = 1;\n' +
        '\n' +
        '{\n' +
        '  let x1 = 0;\n' +
        '  let x2 = 1;\n' +
        '}'
    );
  });
  return it('(compile `(begin (define foo ,(gensym "test")) (define bar ,(gensym "test"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [Symbol.for('define'), Symbol.for('foo'), Symbol('test')],
        [Symbol.for('define'), Symbol.for('bar'), Symbol('test')],
      ]),
      'let foo = test;\n' + '\n' + 'let bar = test1;'
    );
  });
});

describe('Strings', function (): any {
  it('(compile "")', function (): any {
    return assertEqual(compile(''), "'';");
  });
  it('(compile "foo")', function (): any {
    return assertEqual(compile('foo'), "'foo';");
  });
  it('(compile "don\'t")', function (): any {
    return assertEqual(compile("don't"), "'don\\'t';");
  });
  it('(compile "newline\n' + 'test")', function (): any {
    return assertEqual(
      compile('newline\n' + 'test'),
      "'newline\\n' +\n" + "  'test';"
    );
  });
  it('(compile "newline\n' + 'test")', function (): any {
    return assertEqual(
      compile('newline\n' + 'test'),
      "'newline\\n' +\n" + "  'test';"
    );
  });
  it('(compile "newline\n' + 'test\n' + 'three")', function (): any {
    return assertEqual(
      compile('newline\n' + 'test\n' + 'three'),
      "'newline\\n' +\n" + "  'test\\n' +\n" + "  'three';"
    );
  });
  xit('(compile "\\\\t")', function (): any {
    return assertEqual(compile('\\t'), '	;');
  });
  it('(compile "\\\\s")', function (): any {
    return assertEqual(compile('\\s'), "'\\\\s';");
  });
  return xit('(compile (js/tag sexp "\\"\\\\\\\\s\\""))', function (): any {
    return assertEqual(compile(sexp`"\\\\s"`), "'\\\\s';");
  });
});

describe('string-append', function (): any {
  it('(compile \'(string-append "a"))', function (): any {
    return assertEqual(compile([Symbol.for('string-append'), 'a']), "'a';");
  });
  return it('(compile \'(string-append "a" "b"))', function (): any {
    return assertEqual(
      compile([Symbol.for('string-append'), 'a', 'b']),
      "'a' + 'b';"
    );
  });
});

describe('js/tag', function (): any {
  return it('(compile \'(js/tag foo "bar"))', function (): any {
    return assertEqual(
      compile([Symbol.for('js/tag'), Symbol.for('foo'), 'bar']),
      'foo`bar`;'
    );
  });
});

describe('()', function (): any {
  return it("(compile '())", function (): any {
    return assertEqual(compile([]), '[];');
  });
});

describe('list', function (): any {
  it("(compile '(list))", function (): any {
    return assertEqual(compile([Symbol.for('list')]), '[];');
  });
  it("(compile '(list 1))", function (): any {
    return assertEqual(compile([Symbol.for('list'), 1]), '[1];');
  });
  return it("(compile '(list (list 1)))", function (): any {
    return assertEqual(
      compile([Symbol.for('list'), [Symbol.for('list'), 1]]),
      '[[1]];'
    );
  });
});

describe('append', function (): any {
  it("(compile '(append))", function (): any {
    return assertEqual(compile([Symbol.for('append')]), '[];');
  });
  it("(compile '(append foo))", function (): any {
    return assertEqual(
      compile([Symbol.for('append'), Symbol.for('foo')]),
      '[...foo];'
    );
  });
  it("(compile '(append foo bar))", function (): any {
    return assertEqual(
      compile([Symbol.for('append'), Symbol.for('foo'), Symbol.for('bar')]),
      '[...foo, ...bar];'
    );
  });
  it("(compile '(append (list)))", function (): any {
    return assertEqual(
      compile([Symbol.for('append'), [Symbol.for('list')]]),
      '[];'
    );
  });
  it("(compile '(append (list x)))", function (): any {
    return assertEqual(
      compile([Symbol.for('append'), [Symbol.for('list'), Symbol.for('x')]]),
      '[x];'
    );
  });
  return it('(compile \'(append \'("foo") \'("bar")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('append'),
        [Symbol.for('quote'), ['foo']],
        [Symbol.for('quote'), ['bar']],
      ]),
      "['foo', 'bar'];"
    );
  });
});

describe('quote', function (): any {
  it("(compile ''x)", function (): any {
    return assertEqual(
      compile([Symbol.for('quote'), Symbol.for('x')]),
      "Symbol.for('x');"
    );
  });
  it("(compile ''())", function (): any {
    return assertEqual(compile([Symbol.for('quote'), []]), '[];');
  });
  it("(compile ''(1))", function (): any {
    return assertEqual(compile([Symbol.for('quote'), [1]]), '[1];');
  });
  it("(compile ''(1 . 2))", function (): any {
    return assertEqual(
      compile([Symbol.for('quote'), [1, Symbol.for('.'), 2]]),
      "[1, Symbol.for('.'), 2];"
    );
  });
  it("(compile ''((1)))", function (): any {
    return assertEqual(compile([Symbol.for('quote'), [[1]]]), '[[1]];');
  });
  it("(compile ''(x y z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quote'),
        [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
      ]),
      "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')];"
    );
  });
  return it("(compile ''(#t #f))", function (): any {
    return assertEqual(
      compile([Symbol.for('quote'), [true, false]]),
      '[true, false];'
    );
  });
});

describe('quasiquote', function (): any {
  it("(compile '`x)", function (): any {
    return assertEqual(
      compile([Symbol.for('quasiquote'), Symbol.for('x')]),
      "Symbol.for('x');"
    );
  });
  it("(compile '`())", function (): any {
    return assertEqual(compile([Symbol.for('quasiquote'), []]), '[];');
  });
  it("(compile '`(1))", function (): any {
    return assertEqual(compile([Symbol.for('quasiquote'), [1]]), '[1];');
  });
  it("(compile '`(1 . 2))", function (): any {
    return assertEqual(
      compile([Symbol.for('quasiquote'), [1, Symbol.for('.'), 2]]),
      "[1, Symbol.for('.'), 2];"
    );
  });
  it("(compile '`((1 . 2)))", function (): any {
    return assertEqual(
      compile([Symbol.for('quasiquote'), [[1, Symbol.for('.'), 2]]]),
      "[[1, Symbol.for('.'), 2]];"
    );
  });
  it("(compile '`((1 . ,2)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [[1, Symbol.for('.'), [Symbol.for('unquote'), 2]]],
      ]),
      "[[1, Symbol.for('.'), 2]];"
    );
  });
  it("(compile '`((1 . ,2) (3 . ,4)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [
          [1, Symbol.for('.'), [Symbol.for('unquote'), 2]],
          [3, Symbol.for('.'), [Symbol.for('unquote'), 4]],
        ],
      ]),
      "[[1, Symbol.for('.'), 2], [3, Symbol.for('.'), 4]];"
    );
  });
  it('(compile \'(define test-map-1 `(("foo" . ,test-fn) ("bar" . ,test-fn))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('test-map-1'),
        [
          Symbol.for('quasiquote'),
          [
            [
              'foo',
              Symbol.for('.'),
              [Symbol.for('unquote'), Symbol.for('test-fn')],
            ],
            [
              'bar',
              Symbol.for('.'),
              [Symbol.for('unquote'), Symbol.for('test-fn')],
            ],
          ],
        ],
      ]),
      "let testMap1 = [['foo', Symbol.for('.'), testFn], ['bar', Symbol.for('.'), testFn]];"
    );
  });
  it("(compile '`((1)))", function (): any {
    return assertEqual(compile([Symbol.for('quasiquote'), [[1]]]), '[[1]];');
  });
  it("(compile '`(x y z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
      ]),
      "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')];"
    );
  });
  it("(compile '`(x y ,z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [
          Symbol.for('x'),
          Symbol.for('y'),
          [Symbol.for('unquote'), Symbol.for('z')],
        ],
      ]),
      "[Symbol.for('x'), Symbol.for('y'), z];"
    );
  });
  it("(compile '`(x y ,@z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [
          Symbol.for('x'),
          Symbol.for('y'),
          [Symbol.for('unquote-splicing'), Symbol.for('z')],
        ],
      ]),
      "[Symbol.for('x'), Symbol.for('y'), ...z];"
    );
  });
  it("(compile '`(x y `z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [
          Symbol.for('x'),
          Symbol.for('y'),
          [Symbol.for('quasiquote'), Symbol.for('z')],
        ],
      ]),
      "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), Symbol.for('z')]];"
    );
  });
  it("(compile '`(x y `(z)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [
          Symbol.for('x'),
          Symbol.for('y'),
          [Symbol.for('quasiquote'), [Symbol.for('z')]],
        ],
      ]),
      "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [Symbol.for('z')]]];"
    );
  });
  it("(compile '`(x y `(,z)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [
          Symbol.for('x'),
          Symbol.for('y'),
          [
            Symbol.for('quasiquote'),
            [[Symbol.for('unquote'), Symbol.for('z')]],
          ],
        ],
      ]),
      "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('z')]]]];"
    );
  });
  it("(compile '`(x y `(,@z)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [
          Symbol.for('x'),
          Symbol.for('y'),
          [
            Symbol.for('quasiquote'),
            [[Symbol.for('unquote-splicing'), Symbol.for('z')]],
          ],
        ],
      ]),
      "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('z')]]]];"
    );
  });
  it("(compile '`(,@x))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [[Symbol.for('unquote-splicing'), Symbol.for('x')]],
      ]),
      '[...x];'
    );
  });
  it("(compile '`(,@x ,@y))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('quasiquote'),
        [
          [Symbol.for('unquote-splicing'), Symbol.for('x')],
          [Symbol.for('unquote-splicing'), Symbol.for('y')],
        ],
      ]),
      '[...x, ...y];'
    );
  });
  return it("(compile '(set! let-exp `(let ((,arg-list ',args)) ,@body)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('set!'),
        Symbol.for('let-exp'),
        [
          Symbol.for('quasiquote'),
          [
            Symbol.for('let'),
            [
              [
                [Symbol.for('unquote'), Symbol.for('arg-list')],
                [
                  Symbol.for('quote'),
                  [Symbol.for('unquote'), Symbol.for('args')],
                ],
              ],
            ],
            [Symbol.for('unquote-splicing'), Symbol.for('body')],
          ],
        ],
      ]),
      "letExp = [Symbol.for('let'), [[argList, [Symbol.for('quote'), args]]], ...body];"
    );
  });
});

describe('begin', function (): any {
  it("(compile '(begin x y z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        Symbol.for('x'),
        Symbol.for('y'),
        Symbol.for('z'),
      ]),
      'x;\n' + '\n' + 'y;\n' + '\n' + 'z;'
    );
  });
  it("(compile '(begin x (begin y z)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        Symbol.for('x'),
        [Symbol.for('begin'), Symbol.for('y'), Symbol.for('z')],
      ]),
      'x;\n' + '\n' + 'y;\n' + '\n' + 'z;'
    );
  });
  it("(compile '(begin x y z) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          Symbol.for('x'),
          Symbol.for('y'),
          Symbol.for('z'),
        ],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      '(() => {\n' + '  x;\n' + '  y;\n' + '  return z;\n' + '})()'
    );
  });
  return it("(compile '(begin (define (and x y) (or x y)) (define (or x y) x) (and x (or y z))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('or'), Symbol.for('x'), Symbol.for('y')],
        ],
        [
          Symbol.for('define'),
          [Symbol.for('or'), Symbol.for('x'), Symbol.for('y')],
          Symbol.for('x'),
        ],
        [
          Symbol.for('and'),
          Symbol.for('x'),
          [Symbol.for('or'), Symbol.for('y'), Symbol.for('z')],
        ],
      ]),
      'function and(x, y) {\n' +
        '  return or(x, y);\n' +
        '}\n' +
        '\n' +
        'function or(x, y) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        'and(x, or(y, z));'
    );
  });
});

describe('+', function (): any {
  it("(compile '(+ x 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('+'), Symbol.for('x'), 1]),
      'x + 1;'
    );
  });
  return it("(compile '(+ x 1 2))", function (): any {
    return assertEqual(
      compile([Symbol.for('+'), Symbol.for('x'), 1, 2]),
      'x + 1 + 2;'
    );
  });
});

describe('-', function (): any {
  it("(compile '(- x))", function (): any {
    return assertEqual(compile([Symbol.for('-'), Symbol.for('x')]), '-x;');
  });
  xit("(compile '(- (- x)))", function (): any {
    return assertEqual(
      compile([Symbol.for('-'), [Symbol.for('-'), Symbol.for('x')]]),
      'x;'
    );
  });
  it("(compile '(- x 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('-'), Symbol.for('x'), 1]),
      'x - 1;'
    );
  });
  return it("(compile '(- x 1 2))", function (): any {
    return assertEqual(
      compile([Symbol.for('-'), Symbol.for('x'), 1, 2]),
      'x - 1 - 2;'
    );
  });
});

describe('mod', function (): any {
  return it("(compile '(mod x y))", function (): any {
    return assertEqual(
      compile([Symbol.for('mod'), Symbol.for('x'), Symbol.for('y')]),
      'x % y;'
    );
  });
});

describe('=', function (): any {
  it("(compile '(= 1 1))", function (): any {
    return assertEqual(compile([Symbol.for('='), 1, 1]), '1 === 1;');
  });
  return it("(compile '(= x y))", function (): any {
    return assertEqual(
      compile([Symbol.for('='), Symbol.for('x'), Symbol.for('y')]),
      'x === y;'
    );
  });
});

describe('<', function (): any {
  it("(compile '(< 1))", function (): any {
    return assertEqual(compile([Symbol.for('<'), 1]), 'true;');
  });
  it("(compile '(< 1 2))", function (): any {
    return assertEqual(compile([Symbol.for('<'), 1, 2]), '1 < 2;');
  });
  return it("(compile '(< 1 2 3))", function (): any {
    return assertEqual(
      compile([Symbol.for('<'), 1, 2, 3]),
      '(1 < 2) && (2 < 3);'
    );
  });
});

describe('>', function (): any {
  it("(compile '(> 1))", function (): any {
    return assertEqual(compile([Symbol.for('>'), 1]), 'true;');
  });
  it("(compile '(> 2 1))", function (): any {
    return assertEqual(compile([Symbol.for('>'), 2, 1]), '2 > 1;');
  });
  return it("(compile '(> 3 2 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('>'), 3, 2, 1]),
      '(3 > 2) && (2 > 1);'
    );
  });
});

describe('not', function (): any {
  it("(compile '(not (and x y)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('not'),
        [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')],
      ]),
      '!(x && y);'
    );
  });
  it("(compile '(not (= 1 2)))", function (): any {
    return assertEqual(
      compile([Symbol.for('not'), [Symbol.for('='), 1, 2]]),
      '1 !== 2;'
    );
  });
  it("(compile '(not (> 1 2)))", function (): any {
    return assertEqual(
      compile([Symbol.for('not'), [Symbol.for('>'), 1, 2]]),
      '!(1 > 2);'
    );
  });
  it("(compile '(not (f x)))", function (): any {
    return assertEqual(
      compile([Symbol.for('not'), [Symbol.for('f'), Symbol.for('x')]]),
      '!f(x);'
    );
  });
  return xit("(compile '(and (not (f x)) (not (g y))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('and'),
        [Symbol.for('not'), [Symbol.for('f'), Symbol.for('x')]],
        [Symbol.for('not'), [Symbol.for('g'), Symbol.for('y')]],
      ]),
      '!f(x) && !g(y);'
    );
  });
});

describe('and', function (): any {
  it("(compile '(and))", function (): any {
    return assertEqual(compile([Symbol.for('and')]), 'true;');
  });
  it("(compile '(and x))", function (): any {
    return assertEqual(compile([Symbol.for('and'), Symbol.for('x')]), 'x;');
  });
  it("(compile '(and x y))", function (): any {
    return assertEqual(
      compile([Symbol.for('and'), Symbol.for('x'), Symbol.for('y')]),
      'x && y;'
    );
  });
  xit("(compile '(and x y z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('and'),
        Symbol.for('x'),
        Symbol.for('y'),
        Symbol.for('z'),
      ]),
      'x && y && z;'
    );
  });
  xit("(compile '(and x y (w z)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('and'),
        Symbol.for('x'),
        Symbol.for('y'),
        [Symbol.for('w'), Symbol.for('z')],
      ]),
      'x && y && w(z);'
    );
  });
  return xit("(compile '(and x y (or w z)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('and'),
        Symbol.for('x'),
        Symbol.for('y'),
        [Symbol.for('or'), Symbol.for('w'), Symbol.for('z')],
      ]),
      'x && y && (w || z);'
    );
  });
});

describe('or', function (): any {
  it("(compile '(or))", function (): any {
    return assertEqual(compile([Symbol.for('or')]), 'false;');
  });
  it("(compile '(or x))", function (): any {
    return assertEqual(compile([Symbol.for('or'), Symbol.for('x')]), 'x;');
  });
  it("(compile '(or x y))", function (): any {
    return assertEqual(
      compile([Symbol.for('or'), Symbol.for('x'), Symbol.for('y')]),
      'x || y;'
    );
  });
  return xit("(compile '(or x y z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('or'),
        Symbol.for('x'),
        Symbol.for('y'),
        Symbol.for('z'),
      ]),
      'x || y || z;'
    );
  });
});

describe('if', function (): any {
  it("(compile '(if x y z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('if'),
        Symbol.for('x'),
        Symbol.for('y'),
        Symbol.for('z'),
      ]),
      'if (x) {\n' + '  y;\n' + '} else {\n' + '  z;\n' + '}'
    );
  });
  it("(compile '(if x y) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('if'), Symbol.for('x'), Symbol.for('y')],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      'x ? y : undefined'
    );
  });
  it("(compile '(if x y z) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      'x ? y : z'
    );
  });
  it("(compile '(if x y z) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'if (x) {\n' + '  return y;\n' + '} else {\n' + '  return z;\n' + '}'
    );
  });
  it('(compile \'(if "foo" "bar" "baz") :as \'expression)', function (): any {
    return assertEqual(
      compile(
        [Symbol.for('if'), 'foo', 'bar', 'baz'],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      "'foo' ? 'bar' : 'baz'"
    );
  });
  it("(compile '(if x (begin y z) w) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('if'),
          Symbol.for('x'),
          [Symbol.for('begin'), Symbol.for('y'), Symbol.for('z')],
          Symbol.for('w'),
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'if (x) {\n' +
        '  y;\n' +
        '  return z;\n' +
        '} else {\n' +
        '  return w;\n' +
        '}'
    );
  });
  it("(compile '(if (set! x y) z w))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('if'),
        [Symbol.for('set!'), Symbol.for('x'), Symbol.for('y')],
        Symbol.for('z'),
        Symbol.for('w'),
      ]),
      'if ((x = y)) {\n' + '  z;\n' + '} else {\n' + '  w;\n' + '}'
    );
  });
  it("(compile '(if (set! x y) z w) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('if'),
          [Symbol.for('set!'), Symbol.for('x'), Symbol.for('y')],
          Symbol.for('z'),
          Symbol.for('w'),
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'if ((x = y)) {\n' +
        '  return z;\n' +
        '} else {\n' +
        '  return w;\n' +
        '}'
    );
  });
  it("(compile '(if (set!-values (x) y) z w))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('if'),
        [Symbol.for('set!-values'), [Symbol.for('x')], Symbol.for('y')],
        Symbol.for('z'),
        Symbol.for('w'),
      ]),
      'if (([x] = y)) {\n' + '  z;\n' + '} else {\n' + '  w;\n' + '}'
    );
  });
  return it("(compile '(if (set!-fields (x) y) z w))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('if'),
        [Symbol.for('set!-fields'), [Symbol.for('x')], Symbol.for('y')],
        Symbol.for('z'),
        Symbol.for('w'),
      ]),
      'if (({x} = y)) {\n' + '  z;\n' + '} else {\n' + '  w;\n' + '}'
    );
  });
});

describe('when', function (): any {
  it("(compile '(when x y z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('when'),
        Symbol.for('x'),
        Symbol.for('y'),
        Symbol.for('z'),
      ]),
      'if (x) {\n' + '  y;\n' + '  z;\n' + '}'
    );
  });
  return it("(compile '(when (> (array-list-length args) 0) (set! args (.concat (.slice args 0 (- (array-list-length args) 1)) (aref args (- (array-list-length args) 1))))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('when'),
        [
          Symbol.for('>'),
          [Symbol.for('array-list-length'), Symbol.for('args')],
          0,
        ],
        [
          Symbol.for('set!'),
          Symbol.for('args'),
          [
            Symbol.for('.concat'),
            [
              Symbol.for('.slice'),
              Symbol.for('args'),
              0,
              [
                Symbol.for('-'),
                [Symbol.for('array-list-length'), Symbol.for('args')],
                1,
              ],
            ],
            [
              Symbol.for('aref'),
              Symbol.for('args'),
              [
                Symbol.for('-'),
                [Symbol.for('array-list-length'), Symbol.for('args')],
                1,
              ],
            ],
          ],
        ],
      ]),
      'if (args.length > 0) {\n' +
        '  args = args.slice(0, args.length - 1).concat(args[args.length - 1]);\n' +
        '}'
    );
  });
});

describe('unless', function (): any {
  return it("(compile '(unless x y z))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('unless'),
        Symbol.for('x'),
        Symbol.for('y'),
        Symbol.for('z'),
      ]),
      'if (!x) {\n' + '  y;\n' + '  z;\n' + '}'
    );
  });
});

describe('cond', function (): any {
  it("(compile '(cond (x y)) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('y')]],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'if (x) {\n' + '  return y;\n' + '}'
    );
  });
  it("(compile '(cond (x y)) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('y')]],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      'x ? y : undefined'
    );
  });
  it("(compile '(cond (x y) (else z)) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('cond'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('else'), Symbol.for('z')],
        ],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      'x ? y : z'
    );
  });
  it("(compile '(cond (x y) (else w z)) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('cond'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('else'), Symbol.for('w'), Symbol.for('z')],
        ],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      'x ? y : (() => {\n' + '  w;\n' + '  return z;\n' + '})()'
    );
  });
  it("(compile '(cond (x y) (else z)) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('cond'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('else'), Symbol.for('z')],
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'if (x) {\n' + '  return y;\n' + '} else {\n' + '  return z;\n' + '}'
    );
  });
  return xit("(compile '(cond ((set! x y) z) (else w)) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('cond'),
          [
            [Symbol.for('set!'), Symbol.for('x'), Symbol.for('y')],
            Symbol.for('z'),
          ],
          [Symbol.for('else'), Symbol.for('w')],
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'if ((x = y)) {\n' +
        '  return z;\n' +
        '} else {\n' +
        '  return w;\n' +
        '}'
    );
  });
});

describe('let', function (): any {
  it("(compile '(let (x)))", function (): any {
    return assertEqual(
      compile([Symbol.for('let'), [Symbol.for('x')]]),
      'let x;'
    );
  });
  it("(compile '(let (x) x) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('let'), [Symbol.for('x')], Symbol.for('x')],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'let x;\n' + '\n' + 'return x;'
    );
  });
  it("(compile '(let (x) x) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('let'), [Symbol.for('x')], Symbol.for('x')],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      '(() => {\n' + '  let x;\n' + '  return x;\n' + '})()'
    );
  });
  it("(compile '(let (x) x) :as 'return :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('let'), [Symbol.for('x')], Symbol.for('x')],
        Symbol.for(':as'),
        Symbol.for('return'),
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: any;\n' + '\n' + 'return x;'
    );
  });
  it("(compile '(let ((x 1)) x) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'let x = 1;\n' + '\n' + 'return x;'
    );
  });
  it("(compile '(let ((x 1)) x) :as 'return :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
        Symbol.for(':as'),
        Symbol.for('return'),
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: any = 1;\n' + '\n' + 'return x;'
    );
  });
  xit("(compile '(let ((a 1)) (+ (let ((a 2)) a) a)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('let'),
        [[Symbol.for('a'), 1]],
        [
          Symbol.for('+'),
          [Symbol.for('let'), [[Symbol.for('a'), 2]], Symbol.for('a')],
          Symbol.for('a'),
        ],
      ]),
      'let a = 1;\n' +
        '\n' +
        '(() => {\n' +
        '  let a = 2;\n' +
        '  return a;\n' +
        '})() + a;'
    );
  });
  it("(compile '(let ((compose (lambda (f g) (lambda (x) (f (g x))))) (square (lambda (x) (* x x))) (add1 (lambda (x) (+ x 1)))) (display ((compose square add1) (add1 4)))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('let'),
        [
          [
            Symbol.for('compose'),
            [
              Symbol.for('lambda'),
              [Symbol.for('f'), Symbol.for('g')],
              [
                Symbol.for('lambda'),
                [Symbol.for('x')],
                [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')]],
              ],
            ],
          ],
          [
            Symbol.for('square'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x')],
              [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')],
            ],
          ],
          [
            Symbol.for('add1'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x')],
              [Symbol.for('+'), Symbol.for('x'), 1],
            ],
          ],
        ],
        [
          Symbol.for('display'),
          [
            [Symbol.for('compose'), Symbol.for('square'), Symbol.for('add1')],
            [Symbol.for('add1'), 4],
          ],
        ],
      ]),
      'let compose = function (f, g) {\n' +
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
        'console.log(compose(square, add1)(add1(4)));'
    );
  });
  it("(compile '(let ((and (lambda (x y) (if x (if y #t #f) #f)))) (and x y)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('let'),
        [
          [
            Symbol.for('and'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('y')],
              [
                Symbol.for('if'),
                Symbol.for('x'),
                [Symbol.for('if'), Symbol.for('y'), true, false],
                false,
              ],
            ],
          ],
        ],
        [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')],
      ]),
      'let and = function (x, y) {\n' +
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
        'and(x, y);'
    );
  });
  xit("(compile '(begin x (let ((x 1)) x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        Symbol.for('x'),
        [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
      ]),
      'x;\n' + '\n' + 'let x: any = 1;\n' + '\n' + 'return x;'
    );
  });
  xit("(compile '(begin x (let ((x 1)) x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        Symbol.for('x'),
        [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
      ]),
      'x;\n' + '\n' + '{\n' + '  let x: any = 1;\n' + '  x;\n' + '}'
    );
  });
  it("(compile '(begin (let ((x 1)) (display x)) (let ((x 1)) (display x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('let'),
          [[Symbol.for('x'), 1]],
          [Symbol.for('display'), Symbol.for('x')],
        ],
        [
          Symbol.for('let'),
          [[Symbol.for('x'), 1]],
          [Symbol.for('display'), Symbol.for('x')],
        ],
      ]),
      'let x = 1;\n' +
        '\n' +
        'console.log(x);\n' +
        '\n' +
        '{\n' +
        '  let x = 1;\n' +
        '  console.log(x);\n' +
        '}'
    );
  });
  it("(compile '(cond (foo bar) (else x (let ((x 1)) x))) :as 'return :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('cond'),
          [Symbol.for('foo'), Symbol.for('bar')],
          [
            Symbol.for('else'),
            Symbol.for('x'),
            [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
          ],
        ],
        Symbol.for(':as'),
        Symbol.for('return'),
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'if (foo) {\n' +
        '  return bar;\n' +
        '} else {\n' +
        '  x;\n' +
        '  let x: any = 1;\n' +
        '  return x;\n' +
        '}'
    );
  });
  it('(compile \'(define make-compilation-evaluator (memoize (lambda (env (options (js/obj))) (let ((language (oget options "language"))) (set! language (or language default-language)) (let ((compilation-env (or (.get compilation-map language) javascript-env))) (new CompilationEvaluator env compilation-env options)))))) :to \'typescript)', function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          Symbol.for('make-compilation-evaluator'),
          [
            Symbol.for('memoize'),
            [
              Symbol.for('lambda'),
              [
                Symbol.for('env'),
                [Symbol.for('options'), [Symbol.for('js/obj')]],
              ],
              [
                Symbol.for('let'),
                [
                  [
                    Symbol.for('language'),
                    [Symbol.for('oget'), Symbol.for('options'), 'language'],
                  ],
                ],
                [
                  Symbol.for('set!'),
                  Symbol.for('language'),
                  [
                    Symbol.for('or'),
                    Symbol.for('language'),
                    Symbol.for('default-language'),
                  ],
                ],
                [
                  Symbol.for('let'),
                  [
                    [
                      Symbol.for('compilation-env'),
                      [
                        Symbol.for('or'),
                        [
                          Symbol.for('.get'),
                          Symbol.for('compilation-map'),
                          Symbol.for('language'),
                        ],
                        Symbol.for('javascript-env'),
                      ],
                    ],
                  ],
                  [
                    Symbol.for('new'),
                    Symbol.for('CompilationEvaluator'),
                    Symbol.for('env'),
                    Symbol.for('compilation-env'),
                    Symbol.for('options'),
                  ],
                ],
              ],
            ],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let makeCompilationEvaluator: any = memoize(function (env: any, options: any = {}): any {\n' +
        "  let language: any = options['language'];\n" +
        '  language = language || defaultLanguage;\n' +
        '  let compilationEnv: any = compilationMap.get(language) || javascriptEnv;\n' +
        '  return new CompilationEvaluator(env, compilationEnv, options);\n' +
        '});'
    );
  });
  return it("(compile '(cond (foo (let ((x #t)) x)) (else #f)) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('cond'),
          [
            Symbol.for('foo'),
            [Symbol.for('let'), [[Symbol.for('x'), true]], Symbol.for('x')],
          ],
          [Symbol.for('else'), false],
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'if (foo) {\n' +
        '  let x = true;\n' +
        '  return x;\n' +
        '} else {\n' +
        '  return false;\n' +
        '}'
    );
  });
});

describe('let-values', function (): any {
  it("(compile '(let-values ((value (foo bar baz))) value) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('let-values'),
          [
            [
              Symbol.for('value'),
              [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
            ],
          ],
          Symbol.for('value'),
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'let value = foo(bar, baz);\n' + '\n' + 'return value;'
    );
  });
  it("(compile '(let-values (((value) (foo bar baz))) value) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('let-values'),
          [
            [
              [Symbol.for('value')],
              [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
            ],
          ],
          Symbol.for('value'),
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'let [value] = foo(bar, baz);\n' + '\n' + 'return value;'
    );
  });
  it("(compile '(let-values (((value) (foo bar baz))) value) :as 'return :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('let-values'),
          [
            [
              [Symbol.for('value')],
              [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
            ],
          ],
          Symbol.for('value'),
        ],
        Symbol.for(':as'),
        Symbol.for('return'),
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let [value]: any[] = foo(bar, baz);\n' + '\n' + 'return value;'
    );
  });
  it("(compile '(let-values (((x . fs) args)) (.reduce fs (lambda (acc f) (f acc)) x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('let-values'),
        [
          [
            [Symbol.for('x'), Symbol.for('.'), Symbol.for('fs')],
            Symbol.for('args'),
          ],
        ],
        [
          Symbol.for('.reduce'),
          Symbol.for('fs'),
          [
            Symbol.for('lambda'),
            [Symbol.for('acc'), Symbol.for('f')],
            [Symbol.for('f'), Symbol.for('acc')],
          ],
          Symbol.for('x'),
        ],
      ]),
      'let [x, ...fs] = args;\n' +
        '\n' +
        'fs.reduce(function (acc, f) {\n' +
        '  return f(acc);\n' +
        '}, x);'
    );
  });
  it("(compile '(let-values (((x . fs) args)) (.reduce fs (lambda (acc f) (f acc)) x)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('let-values'),
          [
            [
              [Symbol.for('x'), Symbol.for('.'), Symbol.for('fs')],
              Symbol.for('args'),
            ],
          ],
          [
            Symbol.for('.reduce'),
            Symbol.for('fs'),
            [
              Symbol.for('lambda'),
              [Symbol.for('acc'), Symbol.for('f')],
              [Symbol.for('f'), Symbol.for('acc')],
            ],
            Symbol.for('x'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let [x, ...fs]: any[] = args;\n' +
        '\n' +
        'fs.reduce(function (acc: any, f: any): any {\n' +
        '  return f(acc);\n' +
        '}, x);'
    );
  });
  it("(compile '(let-values (((value1) (foo bar)) ((value2) (bar baz))) (list value1 value2)) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('let-values'),
          [
            [[Symbol.for('value1')], [Symbol.for('foo'), Symbol.for('bar')]],
            [[Symbol.for('value2')], [Symbol.for('bar'), Symbol.for('baz')]],
          ],
          [Symbol.for('list'), Symbol.for('value1'), Symbol.for('value2')],
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'let [value1] = foo(bar);\n' +
        '\n' +
        'let [value2] = bar(baz);\n' +
        '\n' +
        'return [value1, value2];'
    );
  });
  return it("(compile '(begin value (let-values ((value (foo bar baz))) value)) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          Symbol.for('value'),
          [
            Symbol.for('let-values'),
            [
              [
                Symbol.for('value'),
                [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
              ],
            ],
            Symbol.for('value'),
          ],
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'value;\n' +
        '\n' +
        'let value = foo(bar, baz);\n' +
        '\n' +
        'return value;'
    );
  });
});

describe('let-fields', function (): any {
  return it("(compile '(let-fields (((prop) obj)) prop))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('let-fields'),
        [[[Symbol.for('prop')], Symbol.for('obj')]],
        Symbol.for('prop'),
      ]),
      'let {prop} = obj;\n' + '\n' + 'prop;'
    );
  });
});

describe('lambda', function (): any {
  it("(compile '(lambda (x) x))", function (): any {
    return assertEqual(
      compile([Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]),
      'function (x) {\n' + '  return x;\n' + '};'
    );
  });
  it("(compile '(lambda (x) x) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function (x: any): any {\n' + '  return x;\n' + '};'
    );
  });
  it("(compile '(lambda args args))", function (): any {
    return assertEqual(
      compile([Symbol.for('lambda'), Symbol.for('args'), Symbol.for('args')]),
      'function (...args) {\n' + '  return args;\n' + '};'
    );
  });
  it("(compile '(lambda (x . args) args))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('lambda'),
        [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')],
        Symbol.for('args'),
      ]),
      'function (x, ...args) {\n' + '  return args;\n' + '};'
    );
  });
  it("(compile '(lambda (x y . args) args))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('lambda'),
        [Symbol.for('x'), Symbol.for('y'), Symbol.for('.'), Symbol.for('args')],
        Symbol.for('args'),
      ]),
      'function (x, y, ...args) {\n' + '  return args;\n' + '};'
    );
  });
  it("(compile '(lambda (x) (let ((x 1)) x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('lambda'),
        [Symbol.for('x')],
        [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
      ]),
      'function (x) {\n' +
        '  {\n' +
        '    let x = 1;\n' +
        '    return x;\n' +
        '  }\n' +
        '};'
    );
  });
  it("(compile '(lambda (x) (let ((y 1)) y)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('lambda'),
        [Symbol.for('x')],
        [Symbol.for('let'), [[Symbol.for('y'), 1]], Symbol.for('y')],
      ]),
      'function (x) {\n' + '  let y = 1;\n' + '  return y;\n' + '};'
    );
  });
  it('(compile \'(lambda (given (surname "Smith")) (string-append "Hello, " given " " surname)))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('lambda'),
        [Symbol.for('given'), [Symbol.for('surname'), 'Smith']],
        [
          Symbol.for('string-append'),
          'Hello, ',
          Symbol.for('given'),
          ' ',
          Symbol.for('surname'),
        ],
      ]),
      "function (given, surname = 'Smith') {\n" +
        "  return 'Hello, ' + given + ' ' + surname;\n" +
        '};'
    );
  });
  it('(compile \'(lambda (given (surname "Smith")) (string-append "Hello, " given " " surname)) :to \'typescript)', function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('lambda'),
          [Symbol.for('given'), [Symbol.for('surname'), 'Smith']],
          [
            Symbol.for('string-append'),
            'Hello, ',
            Symbol.for('given'),
            ' ',
            Symbol.for('surname'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      "function (given: any, surname: any = 'Smith'): any {\n" +
        "  return 'Hello, ' + given + ' ' + surname;\n" +
        '};'
    );
  });
  it("(compile '(lambda (arg (options (js/obj))) arg) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('lambda'),
          [Symbol.for('arg'), [Symbol.for('options'), [Symbol.for('js/obj')]]],
          Symbol.for('arg'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function (arg: any, options: any = {}): any {\n' +
        '  return arg;\n' +
        '};'
    );
  });
  xit("(compile '(lambda (this arg) arg) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('lambda'),
          [Symbol.for('this'), Symbol.for('arg')],
          Symbol.for('arg'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function (arg: any): any {\n' + '  return arg;\n' + '};'
    );
  });
  xit("(compile '(lambda (this . args) args) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('lambda'),
          [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')],
          Symbol.for('args'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function (...args: any[]): any {\n' + '  return args;\n' + '};'
    );
  });
  xit("(compile '(lambda (this arg) arg) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('lambda'),
          [Symbol.for('this'), Symbol.for('arg')],
          Symbol.for('arg'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function (this: any, arg: any): any {\n' + '  return arg;\n' + '};'
    );
  });
  return xit("(compile '(lambda (this . args) args) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('lambda'),
          [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')],
          Symbol.for('args'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function (this: any, ...args: any[]): any {\n' +
        '  return args;\n' +
        '};'
    );
  });
});

describe('funcall', function (): any {
  it("(compile '(funcall f x))", function (): any {
    return assertEqual(
      compile([Symbol.for('funcall'), Symbol.for('f'), Symbol.for('x')]),
      'f(x);'
    );
  });
  return it("(compile '(funcall f x y))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('funcall'),
        Symbol.for('f'),
        Symbol.for('x'),
        Symbol.for('y'),
      ]),
      'f(x, y);'
    );
  });
});

describe('apply', function (): any {
  it("(compile '(apply f args))", function (): any {
    return assertEqual(
      compile([Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]),
      'f(...args);'
    );
  });
  it("(compile '(apply f x args))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('apply'),
        Symbol.for('f'),
        Symbol.for('x'),
        Symbol.for('args'),
      ]),
      'f(x, ...args);'
    );
  });
  it("(compile '(apply new Foo args))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('apply'),
        Symbol.for('new'),
        Symbol.for('Foo'),
        Symbol.for('args'),
      ]),
      'new Foo(...args);'
    );
  });
  it("(compile '(apply new Foo x y args))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('apply'),
        Symbol.for('new'),
        Symbol.for('Foo'),
        Symbol.for('x'),
        Symbol.for('y'),
        Symbol.for('args'),
      ]),
      'new Foo(x, y, ...args);'
    );
  });
  xit("(compile '(apply send obj method args))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('apply'),
        Symbol.for('send'),
        Symbol.for('obj'),
        Symbol.for('method'),
        Symbol.for('args'),
      ]),
      'obj.method(...args);'
    );
  });
  it("(compile '(apply (get-field method obj) args))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('apply'),
        [Symbol.for('get-field'), Symbol.for('method'), Symbol.for('obj')],
        Symbol.for('args'),
      ]),
      'obj.method(...args);'
    );
  });
  return it("(compile '(apply (.-method obj) args))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('apply'),
        [Symbol.for('.-method'), Symbol.for('obj')],
        Symbol.for('args'),
      ]),
      'obj.method(...args);'
    );
  });
});

describe('define', function (): any {
  it("(compile '(define x))", function (): any {
    return assertEqual(
      compile([Symbol.for('define'), Symbol.for('x')]),
      'let x;'
    );
  });
  it("(compile '(define x) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('define'), Symbol.for('x')],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: any;'
    );
  });
  it("(compile '(define x 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('define'), Symbol.for('x'), 1]),
      'let x = 1;'
    );
  });
  it("(compile '(define x 1) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('define'), Symbol.for('x'), 1],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: any = 1;'
    );
  });
  xit("(compile '(define I (lambda (x) x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('I'),
        [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
      ]),
      'function I(x) {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define I (memoize (lambda (x) x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('I'),
        [
          Symbol.for('memoize'),
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        ],
      ]),
      'let I = memoize(function (x) {\n' + '  return x;\n' + '});'
    );
  });
  it("(compile '(define (identity-function x) x))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('identity-function'), Symbol.for('x')],
        Symbol.for('x'),
      ]),
      'function identityFunction(x) {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define (I x) x))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('I'), Symbol.for('x')],
        Symbol.for('x'),
      ]),
      'function I(x) {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define (K x y) x))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('K'), Symbol.for('x'), Symbol.for('y')],
        Symbol.for('x'),
      ]),
      'function K(x, y) {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define (S f g x) (f x (g x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('S'), Symbol.for('f'), Symbol.for('g'), Symbol.for('x')],
        [Symbol.for('f'), Symbol.for('x'), [Symbol.for('g'), Symbol.for('x')]],
      ]),
      'function S(f, g, x) {\n' + '  return f(x, g(x));\n' + '}'
    );
  });
  it("(compile '(define (S f g x) ((f x) (g x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('S'), Symbol.for('f'), Symbol.for('g'), Symbol.for('x')],
        [
          [Symbol.for('f'), Symbol.for('x')],
          [Symbol.for('g'), Symbol.for('x')],
        ],
      ]),
      'function S(f, g, x) {\n' + '  return f(x)(g(x));\n' + '}'
    );
  });
  it("(compile '(define (C f x y) (f y x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('C'), Symbol.for('f'), Symbol.for('x'), Symbol.for('y')],
        [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')],
      ]),
      'function C(f, x, y) {\n' + '  return f(y, x);\n' + '}'
    );
  });
  it("(compile '(define (U f) (f f)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('U'), Symbol.for('f')],
        [Symbol.for('f'), Symbol.for('f')],
      ]),
      'function U(f) {\n' + '  return f(f);\n' + '}'
    );
  });
  it("(compile '(define (A f . args) (apply f args)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('A'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')],
        [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')],
      ]),
      'function A(f, ...args) {\n' + '  return f(...args);\n' + '}'
    );
  });
  it("(compile '(define (A f . args) (apply f args)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          [
            Symbol.for('A'),
            Symbol.for('f'),
            Symbol.for('.'),
            Symbol.for('args'),
          ],
          [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function A(f: any, ...args: any[]): any {\n' +
        '  return f(...args);\n' +
        '}'
    );
  });
  it("(compile '(define (Q . args) (cond ((= (.-length args) 0) #u) ((= (.-length args) 1) (aref args 0)) (else (let ((fs (.slice args 0 -1)) (x (aref args (- (.-length args) 1)))) (.reduce fs (lambda (acc f) (f acc)) x))))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('Q'), Symbol.for('.'), Symbol.for('args')],
        [
          Symbol.for('cond'),
          [
            [Symbol.for('='), [Symbol.for('.-length'), Symbol.for('args')], 0],
            undefined,
          ],
          [
            [Symbol.for('='), [Symbol.for('.-length'), Symbol.for('args')], 1],
            [Symbol.for('aref'), Symbol.for('args'), 0],
          ],
          [
            Symbol.for('else'),
            [
              Symbol.for('let'),
              [
                [
                  Symbol.for('fs'),
                  [Symbol.for('.slice'), Symbol.for('args'), 0, -1],
                ],
                [
                  Symbol.for('x'),
                  [
                    Symbol.for('aref'),
                    Symbol.for('args'),
                    [
                      Symbol.for('-'),
                      [Symbol.for('.-length'), Symbol.for('args')],
                      1,
                    ],
                  ],
                ],
              ],
              [
                Symbol.for('.reduce'),
                Symbol.for('fs'),
                [
                  Symbol.for('lambda'),
                  [Symbol.for('acc'), Symbol.for('f')],
                  [Symbol.for('f'), Symbol.for('acc')],
                ],
                Symbol.for('x'),
              ],
            ],
          ],
        ],
      ]),
      'function Q(...args) {\n' +
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
        '}'
    );
  });
  it("(compile '(define (T . args) (cond ((= (.-length args) 0) #u) ((= (.-length args) 1) (aref args 0)) (else (let-values (((x . fs) args)) (.reduce fs (lambda (acc f) (f acc)) x))))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('T'), Symbol.for('.'), Symbol.for('args')],
        [
          Symbol.for('cond'),
          [
            [Symbol.for('='), [Symbol.for('.-length'), Symbol.for('args')], 0],
            undefined,
          ],
          [
            [Symbol.for('='), [Symbol.for('.-length'), Symbol.for('args')], 1],
            [Symbol.for('aref'), Symbol.for('args'), 0],
          ],
          [
            Symbol.for('else'),
            [
              Symbol.for('let-values'),
              [
                [
                  [Symbol.for('x'), Symbol.for('.'), Symbol.for('fs')],
                  Symbol.for('args'),
                ],
              ],
              [
                Symbol.for('.reduce'),
                Symbol.for('fs'),
                [
                  Symbol.for('lambda'),
                  [Symbol.for('acc'), Symbol.for('f')],
                  [Symbol.for('f'), Symbol.for('acc')],
                ],
                Symbol.for('x'),
              ],
            ],
          ],
        ],
      ]),
      'function T(...args) {\n' +
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
        '}'
    );
  });
  it("(compile '(define (Y f) ((lambda (future) (f (lambda (arg) ((future future) arg)))) (lambda (future) (f (lambda (arg) ((future future) arg)))))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('Y'), Symbol.for('f')],
        [
          [
            Symbol.for('lambda'),
            [Symbol.for('future')],
            [
              Symbol.for('f'),
              [
                Symbol.for('lambda'),
                [Symbol.for('arg')],
                [
                  [Symbol.for('future'), Symbol.for('future')],
                  Symbol.for('arg'),
                ],
              ],
            ],
          ],
          [
            Symbol.for('lambda'),
            [Symbol.for('future')],
            [
              Symbol.for('f'),
              [
                Symbol.for('lambda'),
                [Symbol.for('arg')],
                [
                  [Symbol.for('future'), Symbol.for('future')],
                  Symbol.for('arg'),
                ],
              ],
            ],
          ],
        ],
      ]),
      'function Y(f) {\n' +
        '  return (function (future) {\n' +
        '    return f(function (arg) {\n' +
        '      return future(future)(arg);\n' +
        '    });\n' +
        '  })(function (future) {\n' +
        '    return f(function (arg) {\n' +
        '      return future(future)(arg);\n' +
        '    });\n' +
        '  });\n' +
        '}'
    );
  });
  it("(compile '(define (compose f g) (lambda (x) (f (g x)))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('compose'), Symbol.for('f'), Symbol.for('g')],
        [
          Symbol.for('lambda'),
          [Symbol.for('x')],
          [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')]],
        ],
      ]),
      'function compose(f, g) {\n' +
        '  return function (x) {\n' +
        '    return f(g(x));\n' +
        '  };\n' +
        '}'
    );
  });
  it("(compile '(define (foo) (set! x (+ x 1)) (set! y (+ y 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('foo')],
        [
          Symbol.for('set!'),
          Symbol.for('x'),
          [Symbol.for('+'), Symbol.for('x'), 1],
        ],
        [
          Symbol.for('set!'),
          Symbol.for('y'),
          [Symbol.for('+'), Symbol.for('y'), 1],
        ],
      ]),
      'function foo() {\n' + '  x++;\n' + '  return ++y;\n' + '}'
    );
  });
  it("(compile '(define (mapGet map path) (let-values (((value) (mapGet2 map path))) value)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('mapGet'), Symbol.for('map'), Symbol.for('path')],
        [
          Symbol.for('let-values'),
          [
            [
              [Symbol.for('value')],
              [Symbol.for('mapGet2'), Symbol.for('map'), Symbol.for('path')],
            ],
          ],
          Symbol.for('value'),
        ],
      ]),
      'function mapGet(map, path) {\n' +
        '  let [value] = mapGet2(map, path);\n' +
        '  return value;\n' +
        '}'
    );
  });
  it('(compile \'(define _ (js/obj "dash" #t)))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('_'),
        [Symbol.for('js/obj'), 'dash', true],
      ]),
      'let _ = {\n' + '  dash: true\n' + '};'
    );
  });
  it('(compile \'(define __ (js/obj "dash" #t)))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('__'),
        [Symbol.for('js/obj'), 'dash', true],
      ]),
      'let __ = {\n' + '  dash: true\n' + '};'
    );
  });
  xit('(compile \'(lambda (env (options (js/obj))) (let ((language (oget options "language"))) (set! language (or language default-language)) (let ((compilation-env (or (.get compilation-map language) javascript-env))) (new CompilationEvaluator env compilation-env options)))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('lambda'),
        [Symbol.for('env'), [Symbol.for('options'), [Symbol.for('js/obj')]]],
        [
          Symbol.for('let'),
          [
            [
              Symbol.for('language'),
              [Symbol.for('oget'), Symbol.for('options'), 'language'],
            ],
          ],
          [
            Symbol.for('set!'),
            Symbol.for('language'),
            [
              Symbol.for('or'),
              Symbol.for('language'),
              Symbol.for('default-language'),
            ],
          ],
          [
            Symbol.for('let'),
            [
              [
                Symbol.for('compilation-env'),
                [
                  Symbol.for('or'),
                  [
                    Symbol.for('.get'),
                    Symbol.for('compilation-map'),
                    Symbol.for('language'),
                  ],
                  Symbol.for('javascript-env'),
                ],
              ],
            ],
            [
              Symbol.for('new'),
              Symbol.for('CompilationEvaluator'),
              Symbol.for('env'),
              Symbol.for('compilation-env'),
              Symbol.for('options'),
            ],
          ],
        ],
      ]),
      'function (env: any, options: any = {}): any {\n' +
        "  let language: any = options['language'];\n" +
        '  language = language || (default-language);\n' +
        '  {\n' +
        '    {\n' +
        '      let compilation-env: any = (compilation-map.get(language)) || (javascript-env);\n' +
        '      return new CompilationEvaluator(env, compilation-env, options);\n' +
        '    }\n' +
        '  }\n' +
        '}'
    );
  });
  return it("(compile '(define (add-matrix m1 m2) (let ((l1 (array-list-length m1)) (l2 (array-list-length m2))) (let ((matrix (make-matrix l1 l2))) (for ((i (range 0 l1))) (for ((j (range 0 l2))) (set! (aget (aget matrix j) i) (+ (aget (aget m1 j) i) (aget (aget m2 j) i))))) matrix))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('add-matrix'), Symbol.for('m1'), Symbol.for('m2')],
        [
          Symbol.for('let'),
          [
            [
              Symbol.for('l1'),
              [Symbol.for('array-list-length'), Symbol.for('m1')],
            ],
            [
              Symbol.for('l2'),
              [Symbol.for('array-list-length'), Symbol.for('m2')],
            ],
          ],
          [
            Symbol.for('let'),
            [
              [
                Symbol.for('matrix'),
                [Symbol.for('make-matrix'), Symbol.for('l1'), Symbol.for('l2')],
              ],
            ],
            [
              Symbol.for('for'),
              [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('l1')]]],
              [
                Symbol.for('for'),
                [[Symbol.for('j'), [Symbol.for('range'), 0, Symbol.for('l2')]]],
                [
                  Symbol.for('set!'),
                  [
                    Symbol.for('aget'),
                    [Symbol.for('aget'), Symbol.for('matrix'), Symbol.for('j')],
                    Symbol.for('i'),
                  ],
                  [
                    Symbol.for('+'),
                    [
                      Symbol.for('aget'),
                      [Symbol.for('aget'), Symbol.for('m1'), Symbol.for('j')],
                      Symbol.for('i'),
                    ],
                    [
                      Symbol.for('aget'),
                      [Symbol.for('aget'), Symbol.for('m2'), Symbol.for('j')],
                      Symbol.for('i'),
                    ],
                  ],
                ],
              ],
            ],
            Symbol.for('matrix'),
          ],
        ],
      ]),
      'function addMatrix(m1, m2) {\n' +
        '  let l1 = m1.length;\n' +
        '  let l2 = m2.length;\n' +
        '  let matrix = makeMatrix(l1, l2);\n' +
        '  for (let i = 0; i < l1; i++) {\n' +
        '    for (let j = 0; j < l2; j++) {\n' +
        '      matrix[j][i] = m1[j][i] + m2[j][i];\n' +
        '    }\n' +
        '  }\n' +
        '  return matrix;\n' +
        '}'
    );
  });
});

describe('define-values', function (): any {
  it("(compile '(define-values value (foo bar baz)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-values'),
        Symbol.for('value'),
        [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
      ]),
      'let value = foo(bar, baz);'
    );
  });
  it("(compile '(define-values (value) (foo bar baz)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-values'),
        [Symbol.for('value')],
        [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
      ]),
      'let [value] = foo(bar, baz);'
    );
  });
  it("(compile '(define-values (value) (foo bar baz)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-values'),
          [Symbol.for('value')],
          [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let [value]: any[] = foo(bar, baz);'
    );
  });
  it("(compile '(define-values (_ _ value) (foo bar baz)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-values'),
          [Symbol.for('_'), Symbol.for('_'), Symbol.for('value')],
          [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let [, , value]: any[] = foo(bar, baz);'
    );
  });
  it("(compile '(define-values (_ __ value) :hole-marker __ (foo bar baz)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-values'),
          [Symbol.for('_'), Symbol.for('__'), Symbol.for('value')],
          Symbol.for(':hole-marker'),
          Symbol.for('__'),
          [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let [_, , value]: any[] = foo(bar, baz);'
    );
  });
  return it("(compile '(module m scheme (define (foo) (define xs '(1 2 3 4)) (define-values (x . rest) xs) (append rest '(5)))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            [Symbol.for('foo')],
            [
              Symbol.for('define'),
              Symbol.for('xs'),
              [Symbol.for('quote'), [1, 2, 3, 4]],
            ],
            [
              Symbol.for('define-values'),
              [Symbol.for('x'), Symbol.for('.'), Symbol.for('rest')],
              Symbol.for('xs'),
            ],
            [
              Symbol.for('append'),
              Symbol.for('rest'),
              [Symbol.for('quote'), [5]],
            ],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function foo(): any {\n' +
        '  let xs: any = [1, 2, 3, 4];\n' +
        '  let [x, ...rest]: any[] = xs;\n' +
        '  return [...rest, 5];\n' +
        '}'
    );
  });
});

describe('define-fields', function (): any {
  it("(compile '(define-fields (prop) obj))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-fields'),
        [Symbol.for('prop')],
        Symbol.for('obj'),
      ]),
      'let {prop} = obj;'
    );
  });
  it("(compile '(define-fields (prop) obj))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-fields'),
        [Symbol.for('prop')],
        Symbol.for('obj'),
      ]),
      'let {prop} = obj;'
    );
  });
  it("(compile '(define-fields ((x y) z) obj))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-fields'),
        [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')],
        Symbol.for('obj'),
      ]),
      'let {x: y, z} = obj;'
    );
  });
  it("(compile '(define-fields ((x y) z) obj))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-fields'),
        [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')],
        Symbol.for('obj'),
      ]),
      'let {x: y, z} = obj;'
    );
  });
  it("(compile '(module m scheme (define (foo) (define obj (js/obj)) (define-fields (x rest) obj) (append rest '(5)))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            [Symbol.for('foo')],
            [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('js/obj')]],
            [
              Symbol.for('define-fields'),
              [Symbol.for('x'), Symbol.for('rest')],
              Symbol.for('obj'),
            ],
            [
              Symbol.for('append'),
              Symbol.for('rest'),
              [Symbol.for('quote'), [5]],
            ],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function foo(): any {\n' +
        '  let obj: any = {};\n' +
        '  let {x, rest} = obj;\n' +
        '  return [...rest, 5];\n' +
        '}'
    );
  });
  return it("(compile '(module m scheme (define (foo) (define obj (js/obj)) (define-fields ((rest r) x) obj) (list r x))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            [Symbol.for('foo')],
            [Symbol.for('define'), Symbol.for('obj'), [Symbol.for('js/obj')]],
            [
              Symbol.for('define-fields'),
              [[Symbol.for('rest'), Symbol.for('r')], Symbol.for('x')],
              Symbol.for('obj'),
            ],
            [Symbol.for('list'), Symbol.for('r'), Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function foo(): any {\n' +
        '  let obj: any = {};\n' +
        '  let {rest: r, x} = obj;\n' +
        '  return [r, x];\n' +
        '}'
    );
  });
});

describe('set!-fields', function (): any {
  return it("(compile '(set!-fields (prop) obj))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('set!-fields'),
        [Symbol.for('prop')],
        Symbol.for('obj'),
      ]),
      '({prop} = obj);'
    );
  });
});

describe('set!', function (): any {
  it("(compile '(set! x 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('set!'), Symbol.for('x'), 1]),
      'x = 1;'
    );
  });
  it("(compile '(set! x (add1 x)) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('set!'),
          Symbol.for('x'),
          [Symbol.for('add1'), Symbol.for('x')],
        ],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      '++x'
    );
  });
  it("(compile '(set! x (sub1 x)) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('set!'),
          Symbol.for('x'),
          [Symbol.for('sub1'), Symbol.for('x')],
        ],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      '--x'
    );
  });
  it("(compile '(set! x (+ x 1)) :as 'expression)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('set!'),
          Symbol.for('x'),
          [Symbol.for('+'), Symbol.for('x'), 1],
        ],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      '++x'
    );
  });
  it("(compile '(set! x (+ x 1)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('set!'),
        Symbol.for('x'),
        [Symbol.for('+'), Symbol.for('x'), 1],
      ]),
      'x++;'
    );
  });
  return it("(compile '(set! x (+ x 1)) :as 'return)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('set!'),
          Symbol.for('x'),
          [Symbol.for('+'), Symbol.for('x'), 1],
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'return ++x;'
    );
  });
});

describe('setq', function (): any {
  return it("(compile '(setq x 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('setq'), Symbol.for('x'), 1]),
      'x = 1;'
    );
  });
});

describe('set!-values', function (): any {
  it("(compile '(set!-values (value) (foo bar baz)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('set!-values'),
        [Symbol.for('value')],
        [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
      ]),
      '[value] = foo(bar, baz);'
    );
  });
  it("(compile '(set!-values (_ value) (foo bar baz)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('set!-values'),
        [Symbol.for('_'), Symbol.for('value')],
        [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
      ]),
      '[, value] = foo(bar, baz);'
    );
  });
  return it("(compile '(set!-values (_ __ value) :hole-marker __ (foo bar baz)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('set!-values'),
        [Symbol.for('_'), Symbol.for('__'), Symbol.for('value')],
        Symbol.for(':hole-marker'),
        Symbol.for('__'),
        [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
      ]),
      '[_, , value] = foo(bar, baz);'
    );
  });
});

describe('aget', function (): any {
  it("(compile '(aget args 0))", function (): any {
    return assertEqual(
      compile([Symbol.for('aget'), Symbol.for('args'), 0]),
      'args[0];'
    );
  });
  return it("(compile '(aget args 0 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('aget'), Symbol.for('args'), 0, 1]),
      'args[0][1];'
    );
  });
});

describe('aref', function (): any {
  it("(compile '(aref args 0))", function (): any {
    return assertEqual(
      compile([Symbol.for('aref'), Symbol.for('args'), 0]),
      'args[0];'
    );
  });
  return it("(compile '(aref args 0 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('aref'), Symbol.for('args'), 0, 1]),
      'args[0][1];'
    );
  });
});

describe('aset', function (): any {
  return it("(compile '(aset args 0 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('aset'), Symbol.for('args'), 0, 1]),
      'args[0] = 1;'
    );
  });
});

describe('set!...aref!', function (): any {
  return it("(compile '(set! (aref args 0) 1))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('set!'),
        [Symbol.for('aref'), Symbol.for('args'), 0],
        1,
      ]),
      'args[0] = 1;'
    );
  });
});

describe('first', function (): any {
  return it("(compile '(first x))", function (): any {
    return assertEqual(
      compile([Symbol.for('first'), Symbol.for('x')]),
      'x[0];'
    );
  });
});

describe('last', function (): any {
  return xit("(compile '(last x))", function (): any {
    return assertEqual(
      compile([Symbol.for('last'), Symbol.for('x')]),
      'x[x.length - 1];'
    );
  });
});

describe('nth', function (): any {
  xit("(compile '(nth 1 x))", function (): any {
    return assertEqual(
      compile([Symbol.for('nth'), 1, Symbol.for('x')]),
      'x[1];'
    );
  });
  return xit("(compile '(nth 2 (nth 1 x)))", function (): any {
    return assertEqual(
      compile([Symbol.for('nth'), 2, [Symbol.for('nth'), 1, Symbol.for('x')]]),
      'x[1][2];'
    );
  });
});

describe('nthcdr', function (): any {
  return xit("(compile '(nthcdr 1 x))", function (): any {
    return assertEqual(
      compile([Symbol.for('nthcdr'), 1, Symbol.for('x')]),
      'x.slice(1);'
    );
  });
});

describe('drop', function (): any {
  return it("(compile '(drop x 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('drop'), Symbol.for('x'), 1]),
      'x.slice(1);'
    );
  });
});

describe('drop-right', function (): any {
  return it("(compile '(drop-right x 1))", function (): any {
    return assertEqual(
      compile([Symbol.for('drop-right'), Symbol.for('x'), 1]),
      'x.slice(0, -1);'
    );
  });
});

describe('length', function (): any {
  return it("(compile '(array-list-length x))", function (): any {
    return assertEqual(
      compile([Symbol.for('array-list-length'), Symbol.for('x')]),
      'x.length;'
    );
  });
});

describe('get-field', function (): any {
  it("(compile '(get-field length arr))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('get-field'),
        Symbol.for('length'),
        Symbol.for('arr'),
      ]),
      'arr.length;'
    );
  });
  return it("(compile '(get-field (- len 1) arr))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('get-field'),
        [Symbol.for('-'), Symbol.for('len'), 1],
        Symbol.for('arr'),
      ]),
      'arr[len - 1];'
    );
  });
});

describe('set-field!', function (): any {
  it("(compile '(set-field! prop obj val))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('set-field!'),
        Symbol.for('prop'),
        Symbol.for('obj'),
        Symbol.for('val'),
      ]),
      'obj.prop = val;'
    );
  });
  return it("(compile '(set-field! def-method generic-function (lambda (arglist function-definition) (let ((entry (list arglist function-definition))) (push! (get-field methods generic-function) entry) generic-function))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('set-field!'),
        Symbol.for('def-method'),
        Symbol.for('generic-function'),
        [
          Symbol.for('lambda'),
          [Symbol.for('arglist'), Symbol.for('function-definition')],
          [
            Symbol.for('let'),
            [
              [
                Symbol.for('entry'),
                [
                  Symbol.for('list'),
                  Symbol.for('arglist'),
                  Symbol.for('function-definition'),
                ],
              ],
            ],
            [
              Symbol.for('push!'),
              [
                Symbol.for('get-field'),
                Symbol.for('methods'),
                Symbol.for('generic-function'),
              ],
              Symbol.for('entry'),
            ],
            Symbol.for('generic-function'),
          ],
        ],
      ]),
      'genericFunction.defMethod = function (arglist, functionDefinition) {\n' +
        '  let entry = [arglist, functionDefinition];\n' +
        '  genericFunction.methods.unshift(entry);\n' +
        '  return genericFunction;\n' +
        '};'
    );
  });
});

describe('send', function (): any {
  return it('(compile \'(send map get "foo"))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('send'),
        Symbol.for('map'),
        Symbol.for('get'),
        'foo',
      ]),
      "map.get('foo');"
    );
  });
});

describe('send/apply', function (): any {
  it("(compile '(send/apply map get foo))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('send/apply'),
        Symbol.for('map'),
        Symbol.for('get'),
        Symbol.for('foo'),
      ]),
      'map.get(...foo);'
    );
  });
  return it('(compile \'(send/apply map get \'("foo")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('send/apply'),
        Symbol.for('map'),
        Symbol.for('get'),
        [Symbol.for('quote'), ['foo']],
      ]),
      "map.get('foo');"
    );
  });
});

describe('.', function (): any {
  it('(compile \'(. map get "foo"))', function (): any {
    return assertEqual(
      compile([Symbol.for('.'), Symbol.for('map'), Symbol.for('get'), 'foo']),
      "map.get('foo');"
    );
  });
  it('(compile \'(.get map "foo"))', function (): any {
    return assertEqual(
      compile([Symbol.for('.get'), Symbol.for('map'), 'foo']),
      "map.get('foo');"
    );
  });
  return it("(compile '(.-length arr))", function (): any {
    return assertEqual(
      compile([Symbol.for('.-length'), Symbol.for('arr')]),
      'arr.length;'
    );
  });
});

describe('memq?', function (): any {
  it("(compile '(memq? 2 (list 1 2 3 4)))", function (): any {
    return assertEqual(
      compile([Symbol.for('memq?'), 2, [Symbol.for('list'), 1, 2, 3, 4]]),
      '[1, 2, 3, 4].includes(2);'
    );
  });
  return it("(compile '(memq? (+ 1 1) (list 1 2 3 4)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('memq?'),
        [Symbol.for('+'), 1, 1],
        [Symbol.for('list'), 1, 2, 3, 4],
      ]),
      '[1, 2, 3, 4].includes(1 + 1);'
    );
  });
});

describe('member?', function (): any {
  it("(compile '(member? 2 (list 1 2 3 4) f))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('member?'),
        2,
        [Symbol.for('list'), 1, 2, 3, 4],
        Symbol.for('f'),
      ]),
      '[1, 2, 3, 4].findIndex(function (x) {\n' +
        '  return f(2, x);\n' +
        '}) >= 0;'
    );
  });
  return it("(compile '(member? (+ 1 1) (list 1 2 3 4) f))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('member?'),
        [Symbol.for('+'), 1, 1],
        [Symbol.for('list'), 1, 2, 3, 4],
        Symbol.for('f'),
      ]),
      '[1, 2, 3, 4].findIndex(function (x) {\n' +
        '  return f(1 + 1, x);\n' +
        '}) >= 0;'
    );
  });
});

describe('map', function (): any {
  it("(compile '(map f x))", function (): any {
    return assertEqual(
      compile([Symbol.for('map'), Symbol.for('f'), Symbol.for('x')]),
      'x.map(function (x) {\n' + '  return f(x);\n' + '});'
    );
  });
  it("(compile '(map (lambda (x) x) x))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('map'),
        [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        Symbol.for('x'),
      ]),
      'x.map(function (x) {\n' + '  return x;\n' + '});'
    );
  });
  return it("(compile '(map (g y) x))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('map'),
        [Symbol.for('g'), Symbol.for('y')],
        Symbol.for('x'),
      ]),
      'x.map((function (f) {\n' +
        '  return function (x) {\n' +
        '    return f(x);\n' +
        '  };\n' +
        '})(g(y)));'
    );
  });
});

describe('foldl', function (): any {
  it("(compile '(foldl f v l))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('foldl'),
        Symbol.for('f'),
        Symbol.for('v'),
        Symbol.for('l'),
      ]),
      'l.reduce(function (acc, x) {\n' + '  return f(x, acc);\n' + '}, v);'
    );
  });
  it("(compile '(foldl (lambda (x acc) (f x acc)) v l))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('foldl'),
        [
          Symbol.for('lambda'),
          [Symbol.for('x'), Symbol.for('acc')],
          [Symbol.for('f'), Symbol.for('x'), Symbol.for('acc')],
        ],
        Symbol.for('v'),
        Symbol.for('l'),
      ]),
      'l.reduce(function (acc, x) {\n' + '  return f(x, acc);\n' + '}, v);'
    );
  });
  return it("(compile '(foldl + 0 '(1 2 3 4)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('foldl'),
        Symbol.for('+'),
        0,
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ]),
      '[1, 2, 3, 4].reduce(function (acc, x) {\n' +
        '  return x + acc;\n' +
        '}, 0);'
    );
  });
});

describe('foldr', function (): any {
  xit("(compile '(foldr f v x))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('foldr'),
        Symbol.for('f'),
        Symbol.for('v'),
        Symbol.for('x'),
      ]),
      'x.reduceRight((function (f) {\n' +
        '  return function (x, y) {\n' +
        '    return f(y, x);\n' +
        '  };\n' +
        '})(f), v);'
    );
  });
  xit("(compile '(foldr (f g) v x))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('foldr'),
        [Symbol.for('f'), Symbol.for('g')],
        Symbol.for('v'),
        Symbol.for('x'),
      ]),
      'x.reduceRight((function (f) {\n' +
        '  return function (x, y) {\n' +
        '    return f(y, x);\n' +
        '  };\n' +
        '})(f(g)), v);'
    );
  });
  return xit("(compile '(foldr cons '() '(1 2 3 4)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('foldr'),
        Symbol.for('cons'),
        [Symbol.for('quote'), []],
        [Symbol.for('quote'), [1, 2, 3, 4]],
      ]),
      '[1, 2, 3, 4].reduceRight((function (f) {\n' +
        '  return function (x, y) {\n' +
        '    return f(y, x);\n' +
        '  };\n' +
        '})(cons), []);'
    );
  });
});

describe('for', function (): any {
  it("(compile '(for ((x '(1 2 3))) (display x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
        [Symbol.for('display'), Symbol.for('x')],
      ]),
      'for (let x of [1, 2, 3]) {\n' + '  console.log(x);\n' + '}'
    );
  });
  it("(compile '(for ((x '(1 2 3))) (break)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
        [Symbol.for('break')],
      ]),
      'for (let x of [1, 2, 3]) {\n' + '  break;\n' + '}'
    );
  });
  it("(compile '(for ((x '(1 2 3))) (continue)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
        [Symbol.for('continue')],
      ]),
      'for (let x of [1, 2, 3]) {\n' + '  continue;\n' + '}'
    );
  });
  it("(compile '(for ((x '(1 2 3))) (let ((x 1)) (display x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
        [
          Symbol.for('let'),
          [[Symbol.for('x'), 1]],
          [Symbol.for('display'), Symbol.for('x')],
        ],
      ]),
      'for (let x of [1, 2, 3]) {\n' +
        '  {\n' +
        '    let x = 1;\n' +
        '    console.log(x);\n' +
        '  }\n' +
        '}'
    );
  });
  it("(compile '(for ((x '(1 2 3))) (let ((y 1)) (display x y))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
        [
          Symbol.for('let'),
          [[Symbol.for('y'), 1]],
          [Symbol.for('display'), Symbol.for('x'), Symbol.for('y')],
        ],
      ]),
      'for (let x of [1, 2, 3]) {\n' +
        '  let y = 1;\n' +
        '  console.log(x, y);\n' +
        '}'
    );
  });
  it("(compile '(for ((x '(1 2 3))) (let ((y 1)) (display y)) (display x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
        [
          Symbol.for('let'),
          [[Symbol.for('y'), 1]],
          [Symbol.for('display'), Symbol.for('y')],
        ],
        [Symbol.for('display'), Symbol.for('x')],
      ]),
      'for (let x of [1, 2, 3]) {\n' +
        '  let y = 1;\n' +
        '  console.log(y);\n' +
        '  console.log(x);\n' +
        '}'
    );
  });
  it("(compile '(for ((i (range 0 10))) (display x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]],
        [Symbol.for('display'), Symbol.for('x')],
      ]),
      'for (let i = 0; i < 10; i++) {\n' + '  console.log(x);\n' + '}'
    );
  });
  it("(compile '(for ((i (range 0 10))) (display x)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('for'),
          [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]],
          [Symbol.for('display'), Symbol.for('x')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'for (let i: any = 0; i < 10; i++) {\n' + '  console.log(x);\n' + '}'
    );
  });
  it("(compile '(for ((i (range 1 10 2))) (display x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('i'), [Symbol.for('range'), 1, 10, 2]]],
        [Symbol.for('display'), Symbol.for('x')],
      ]),
      'for (let i = 1; i < 10; i = i + 2) {\n' + '  console.log(x);\n' + '}'
    );
  });
  it("(compile '(for ((i (range 10 1 -1))) (display x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('i'), [Symbol.for('range'), 10, 1, -1]]],
        [Symbol.for('display'), Symbol.for('x')],
      ]),
      'for (let i = 10; i > 1; i--) {\n' + '  console.log(x);\n' + '}'
    );
  });
  it("(compile '(for ((i (range 10 1 -2))) (display x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('i'), [Symbol.for('range'), 10, 1, -2]]],
        [Symbol.for('display'), Symbol.for('x')],
      ]),
      'for (let i = 10; i > 1; i = i - 2) {\n' + '  console.log(x);\n' + '}'
    );
  });
  it("(compile '(for ((i (range 0 (+ 1 1)))) (display i)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('+'), 1, 1]]]],
        [Symbol.for('display'), Symbol.for('i')],
      ]),
      'let _end = 1 + 1;\n' +
        '\n' +
        'for (let i = 0; i < _end; i++) {\n' +
        '  console.log(i);\n' +
        '}'
    );
  });
  it("(compile '(begin (for ((i (range 0 (+ 1 1)))) (display i)) (for ((j (range 0 (+ 2 2)))) (display j))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('for'),
          [
            [
              Symbol.for('i'),
              [Symbol.for('range'), 0, [Symbol.for('+'), 1, 1]],
            ],
          ],
          [Symbol.for('display'), Symbol.for('i')],
        ],
        [
          Symbol.for('for'),
          [
            [
              Symbol.for('j'),
              [Symbol.for('range'), 0, [Symbol.for('+'), 2, 2]],
            ],
          ],
          [Symbol.for('display'), Symbol.for('j')],
        ],
      ]),
      'let _end = 1 + 1;\n' +
        '\n' +
        'for (let i = 0; i < _end; i++) {\n' +
        '  console.log(i);\n' +
        '}\n' +
        '\n' +
        'let _end1 = 2 + 2;\n' +
        '\n' +
        'for (let j = 0; j < _end1; j++) {\n' +
        '  console.log(j);\n' +
        '}'
    );
  });
  it("(compile '(for ((i (range (+ 1 1) (+ 2 2)))) (display i)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('for'),
        [
          [
            Symbol.for('i'),
            [
              Symbol.for('range'),
              [Symbol.for('+'), 1, 1],
              [Symbol.for('+'), 2, 2],
            ],
          ],
        ],
        [Symbol.for('display'), Symbol.for('i')],
      ]),
      'let _start = 1 + 1;\n' +
        '\n' +
        'let _end = 2 + 2;\n' +
        '\n' +
        'for (let i = _start; i < _end; i++) {\n' +
        '  console.log(i);\n' +
        '}'
    );
  });
  it("(compile '(for ((i (range (+ 1 1) (+ 2 2)))) (display i)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('for'),
          [
            [
              Symbol.for('i'),
              [
                Symbol.for('range'),
                [Symbol.for('+'), 1, 1],
                [Symbol.for('+'), 2, 2],
              ],
            ],
          ],
          [Symbol.for('display'), Symbol.for('i')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let _start: any = 1 + 1;\n' +
        '\n' +
        'let _end: any = 2 + 2;\n' +
        '\n' +
        'for (let i: any = _start; i < _end; i++) {\n' +
        '  console.log(i);\n' +
        '}'
    );
  });
  it("(compile '(let ((_start 0) (_end 0)) (for ((i (range (+ 1 1) (+ 2 2)))) (display i))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('let'),
          [
            [Symbol.for('_start'), 0],
            [Symbol.for('_end'), 0],
          ],
          [
            Symbol.for('for'),
            [
              [
                Symbol.for('i'),
                [
                  Symbol.for('range'),
                  [Symbol.for('+'), 1, 1],
                  [Symbol.for('+'), 2, 2],
                ],
              ],
            ],
            [Symbol.for('display'), Symbol.for('i')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let _start: any = 0;\n' +
        '\n' +
        'let _end: any = 0;\n' +
        '\n' +
        'let _start1: any = 1 + 1;\n' +
        '\n' +
        'let _end1: any = 2 + 2;\n' +
        '\n' +
        'for (let i: any = _start1; i < _end1; i++) {\n' +
        '  console.log(i);\n' +
        '}'
    );
  });
  it("(compile '(for ((i (range (+ 1 1) (+ 2 2)))) (for ((j (range (+ 3 3) (+ 4 4)))) (display j))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('for'),
          [
            [
              Symbol.for('i'),
              [
                Symbol.for('range'),
                [Symbol.for('+'), 1, 1],
                [Symbol.for('+'), 2, 2],
              ],
            ],
          ],
          [
            Symbol.for('for'),
            [
              [
                Symbol.for('j'),
                [
                  Symbol.for('range'),
                  [Symbol.for('+'), 3, 3],
                  [Symbol.for('+'), 4, 4],
                ],
              ],
            ],
            [Symbol.for('display'), Symbol.for('j')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let _start: any = 1 + 1;\n' +
        '\n' +
        'let _end: any = 2 + 2;\n' +
        '\n' +
        'for (let i: any = _start; i < _end; i++) {\n' +
        '  let _start1: any = 3 + 3;\n' +
        '  let _end1: any = 4 + 4;\n' +
        '  for (let j: any = _start1; j < _end1; j++) {\n' +
        '    console.log(j);\n' +
        '  }\n' +
        '}'
    );
  });
  return it("(compile '(define (foo) (for ((x '(1 2 3))) (display x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        [Symbol.for('foo')],
        [
          Symbol.for('for'),
          [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
          [Symbol.for('display'), Symbol.for('x')],
        ],
      ]),
      'function foo() {\n' +
        '  for (let x of [1, 2, 3]) {\n' +
        '    console.log(x);\n' +
        '  }\n' +
        '}'
    );
  });
});

describe('do', function (): any {
  it("(compile '(do () ((not (< (array-list-length result) 3))) (display result)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('do'),
        [],
        [
          [
            Symbol.for('not'),
            [
              Symbol.for('<'),
              [Symbol.for('array-list-length'), Symbol.for('result')],
              3,
            ],
          ],
        ],
        [Symbol.for('display'), Symbol.for('result')],
      ]),
      'while (result.length < 3) {\n' + '  console.log(result);\n' + '}'
    );
  });
  return xit("(compile '(do ((*do-result* (display result))) ((not (< (array-list-length result) 3)))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('do'),
        [
          [
            Symbol.for('*do-result*'),
            [Symbol.for('display'), Symbol.for('result')],
          ],
        ],
        [
          [
            Symbol.for('not'),
            [
              Symbol.for('<'),
              [Symbol.for('array-list-length'), Symbol.for('result')],
              3,
            ],
          ],
        ],
      ]),
      'do {\n' + '  console.log(result);\n' + '} while (result.length < 3);'
    );
  });
});

describe('js/while', function (): any {
  it("(compile '(js/while (< (array-list-length result) 3) (display result)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/while'),
        [
          Symbol.for('<'),
          [Symbol.for('array-list-length'), Symbol.for('result')],
          3,
        ],
        [Symbol.for('display'), Symbol.for('result')],
      ]),
      'while (result.length < 3) {\n' + '  console.log(result);\n' + '}'
    );
  });
  return it("(compile '(js/while (begin (set! x (- x 1)) (> x 0)) (display x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/while'),
        [
          Symbol.for('begin'),
          [
            Symbol.for('set!'),
            Symbol.for('x'),
            [Symbol.for('-'), Symbol.for('x'), 1],
          ],
          [Symbol.for('>'), Symbol.for('x'), 0],
        ],
        [Symbol.for('display'), Symbol.for('x')],
      ]),
      'while ((() => {\n' +
        '  x--;\n' +
        '  return x > 0;\n' +
        '})()) {\n' +
        '  console.log(x);\n' +
        '}'
    );
  });
});

describe('js/do-while', function (): any {
  it("(compile '(js/do-while ((display result)) (< (array-list-length result) 3)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/do-while'),
        [[Symbol.for('display'), Symbol.for('result')]],
        [
          Symbol.for('<'),
          [Symbol.for('array-list-length'), Symbol.for('result')],
          3,
        ],
      ]),
      'do {\n' + '  console.log(result);\n' + '} while (result.length < 3);'
    );
  });
  it("(compile '(js/do-while ((foo) (display result)) (< (array-list-length result) 3)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/do-while'),
        [[Symbol.for('foo')], [Symbol.for('display'), Symbol.for('result')]],
        [
          Symbol.for('<'),
          [Symbol.for('array-list-length'), Symbol.for('result')],
          3,
        ],
      ]),
      'do {\n' +
        '  foo();\n' +
        '  console.log(result);\n' +
        '} while (result.length < 3);'
    );
  });
  return it("(compile '(js/while (< (array-list-length result) 3) (display result)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/while'),
        [
          Symbol.for('<'),
          [Symbol.for('array-list-length'), Symbol.for('result')],
          3,
        ],
        [Symbol.for('display'), Symbol.for('result')],
      ]),
      'while (result.length < 3) {\n' + '  console.log(result);\n' + '}'
    );
  });
});

describe('js/obj', function (): any {
  it("(compile '(js/obj))", function (): any {
    return assertEqual(compile([Symbol.for('js/obj')]), '({});');
  });
  it('(compile \'(js/obj foo "bar"))', function (): any {
    return assertEqual(
      compile([Symbol.for('js/obj'), Symbol.for('foo'), 'bar']),
      '({\n' + "  [foo]: 'bar'\n" + '});'
    );
  });
  it('(compile \'(js/obj "foo" "bar"))', function (): any {
    return assertEqual(
      compile([Symbol.for('js/obj'), 'foo', 'bar']),
      '({\n' + "  foo: 'bar'\n" + '});'
    );
  });
  it('(compile \'(js/obj "foo bar" "foo bar"))', function (): any {
    return assertEqual(
      compile([Symbol.for('js/obj'), 'foo bar', 'foo bar']),
      '({\n' + "  'foo bar': 'foo bar'\n" + '});'
    );
  });
  it('(compile \'(js/obj "foo" (js/obj "bar" "baz")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/obj'),
        'foo',
        [Symbol.for('js/obj'), 'bar', 'baz'],
      ]),
      '({\n' + '  foo: {\n' + "    bar: 'baz'\n" + '  }\n' + '});'
    );
  });
  it('(compile \'(js/obj "foo" (js/obj "foo" "foo") "bar" (js/obj "bar" "bar")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/obj'),
        'foo',
        [Symbol.for('js/obj'), 'foo', 'foo'],
        'bar',
        [Symbol.for('js/obj'), 'bar', 'bar'],
      ]),
      '({\n' +
        '  foo: {\n' +
        "    foo: 'foo'\n" +
        '  },\n' +
        '  bar: {\n' +
        "    bar: 'bar'\n" +
        '  }\n' +
        '});'
    );
  });
  return it('(compile \'(js/obj "foo" (js/obj) "bar" (js/obj "bar" "bar") "baz" (js/obj "baz" "baz")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/obj'),
        'foo',
        [Symbol.for('js/obj')],
        'bar',
        [Symbol.for('js/obj'), 'bar', 'bar'],
        'baz',
        [Symbol.for('js/obj'), 'baz', 'baz'],
      ]),
      '({\n' +
        '  foo: {},\n' +
        '  bar: {\n' +
        "    bar: 'bar'\n" +
        '  },\n' +
        '  baz: {\n' +
        "    baz: 'baz'\n" +
        '  }\n' +
        '});'
    );
  });
});

describe('js/obj?', function (): any {
  return it("(compile '(js/obj? x))", function (): any {
    return assertEqual(
      compile([Symbol.for('js/obj?'), Symbol.for('x')]),
      "(x !== null) && (typeof x === 'object');"
    );
  });
});

describe('js/obj-append', function (): any {
  return it('(compile \'(js/obj-append obj (js/obj "foo" "bar")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/obj-append'),
        Symbol.for('obj'),
        [Symbol.for('js/obj'), 'foo', 'bar'],
      ]),
      '({\n' + '  ...obj,\n' + "  foo: 'bar'\n" + '});'
    );
  });
});

describe('js/keys', function (): any {
  return it("(compile '(js/keys x))", function (): any {
    return assertEqual(
      compile([Symbol.for('js/keys'), Symbol.for('x')]),
      'Object.keys(x);'
    );
  });
});

describe('js/delete', function (): any {
  return it("(compile '(js/delete x))", function (): any {
    return assertEqual(
      compile([Symbol.for('js/delete'), Symbol.for('x')]),
      'delete x;'
    );
  });
});

describe('class', function (): any {
  return it('(compile \'(class () (define/public (bar) "bar")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('class'),
        [],
        [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
      ]),
      'class {\n' + '  bar() {\n' + "    return 'bar';\n" + '  }\n' + '}'
    );
  });
});

describe('define-class', function (): any {
  it("(compile '(define-class Foo))", function (): any {
    return assertEqual(
      compile([Symbol.for('define-class'), Symbol.for('Foo')]),
      'class Foo {\n' + '}'
    );
  });
  it('(compile \'(define-class Foo () (define/public (bar) "bar")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-class'),
        Symbol.for('Foo'),
        [],
        [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
      ]),
      'class Foo {\n' + '  bar() {\n' + "    return 'bar';\n" + '  }\n' + '}'
    );
  });
  it('(compile \'(define-class Foo () (define/public (bar) "bar") (define/public (baz) "baz")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-class'),
        Symbol.for('Foo'),
        [],
        [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
        [Symbol.for('define/public'), [Symbol.for('baz')], 'baz'],
      ]),
      'class Foo {\n' +
        '  bar() {\n' +
        "    return 'bar';\n" +
        '  }\n' +
        '\n' +
        '  baz() {\n' +
        "    return 'baz';\n" +
        '  }\n' +
        '}'
    );
  });
  it('(compile \'(define-class Foo () (define/public bar) (define/public baz "baz") (define/public (quux) "quux")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-class'),
        Symbol.for('Foo'),
        [],
        [Symbol.for('define/public'), Symbol.for('bar')],
        [Symbol.for('define/public'), Symbol.for('baz'), 'baz'],
        [Symbol.for('define/public'), [Symbol.for('quux')], 'quux'],
      ]),
      'class Foo {\n' +
        '  bar;\n' +
        '\n' +
        "  baz = 'baz';\n" +
        '\n' +
        '  quux() {\n' +
        "    return 'quux';\n" +
        '  }\n' +
        '}'
    );
  });
  it("(compile '(define-class Foo () (define x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-class'),
        Symbol.for('Foo'),
        [],
        [Symbol.for('define'), Symbol.for('x')],
        [
          Symbol.for('define/public'),
          [Symbol.for('constructor'), Symbol.for('x')],
          [Symbol.for('super')],
          [
            Symbol.for('set!'),
            [Symbol.for('.-x'), Symbol.for('this')],
            Symbol.for('x'),
          ],
        ],
        [
          Symbol.for('define/public'),
          [Symbol.for('bar')],
          [Symbol.for('.-x'), Symbol.for('this')],
        ],
      ]),
      'class Foo {\n' +
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
        '}'
    );
  });
  it("(compile '(define-class Foo () (define x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define'), Symbol.for('x')],
          [
            Symbol.for('define/public'),
            [Symbol.for('constructor'), Symbol.for('x')],
            [Symbol.for('super')],
            [
              Symbol.for('set!'),
              [Symbol.for('.-x'), Symbol.for('this')],
              Symbol.for('x'),
            ],
          ],
          [
            Symbol.for('define/public'),
            [Symbol.for('bar')],
            [Symbol.for('.-x'), Symbol.for('this')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'class Foo {\n' +
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
        '}'
    );
  });
  it("(compile '(define-class Foo () (define/public x) (define/public (constructor . args) (super) (set! (.-stack this) args)) (define/public (bar) (.-x this))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define/public'), Symbol.for('x')],
          [
            Symbol.for('define/public'),
            [Symbol.for('constructor'), Symbol.for('.'), Symbol.for('args')],
            [Symbol.for('super')],
            [
              Symbol.for('set!'),
              [Symbol.for('.-stack'), Symbol.for('this')],
              Symbol.for('args'),
            ],
          ],
          [
            Symbol.for('define/public'),
            [Symbol.for('bar')],
            [Symbol.for('.-x'), Symbol.for('this')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'class Foo {\n' +
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
        '}'
    );
  });
  it("(compile '(define-class Foo (Object) (define/public x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-class'),
        Symbol.for('Foo'),
        [Symbol.for('Object')],
        [Symbol.for('define/public'), Symbol.for('x')],
        [
          Symbol.for('define/public'),
          [Symbol.for('constructor'), Symbol.for('x')],
          [Symbol.for('super')],
          [
            Symbol.for('set!'),
            [Symbol.for('.-x'), Symbol.for('this')],
            Symbol.for('x'),
          ],
        ],
        [
          Symbol.for('define/public'),
          [Symbol.for('bar')],
          [Symbol.for('.-x'), Symbol.for('this')],
        ],
      ]),
      'class Foo extends Object {\n' +
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
        '}'
    );
  });
  it("(compile '(define-class Foo (Object) (define/public x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/public (bar) (.-x this))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-class'),
        Symbol.for('Foo'),
        [Symbol.for('Object')],
        [Symbol.for('define/public'), Symbol.for('x')],
        [
          Symbol.for('define/public'),
          [Symbol.for('constructor'), Symbol.for('x')],
          [Symbol.for('super')],
          [
            Symbol.for('set!'),
            [Symbol.for('.-x'), Symbol.for('this')],
            Symbol.for('x'),
          ],
        ],
        [
          Symbol.for('define/public'),
          [Symbol.for('bar')],
          [Symbol.for('.-x'), Symbol.for('this')],
        ],
      ]),
      'class Foo extends Object {\n' +
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
        '}'
    );
  });
  it("(compile '(define-class Foo (Object) (define/private x) (define/public (constructor x) (super) (set! (.-x this) x)) (define/private (bar) (.-x this))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [Symbol.for('Object')],
          [Symbol.for('define/private'), Symbol.for('x')],
          [
            Symbol.for('define/public'),
            [Symbol.for('constructor'), Symbol.for('x')],
            [Symbol.for('super')],
            [
              Symbol.for('set!'),
              [Symbol.for('.-x'), Symbol.for('this')],
              Symbol.for('x'),
            ],
          ],
          [
            Symbol.for('define/private'),
            [Symbol.for('bar')],
            [Symbol.for('.-x'), Symbol.for('this')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'class Foo extends Object {\n' +
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
        '}'
    );
  });
  it("(compile '(define-class Foo () (public x) (define x) (public constructor) (define (constructor x) (super) (set! (.-x this) x)) (public bar) (define (bar) (.-x this))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('public'), Symbol.for('x')],
          [Symbol.for('define'), Symbol.for('x')],
          [Symbol.for('public'), Symbol.for('constructor')],
          [
            Symbol.for('define'),
            [Symbol.for('constructor'), Symbol.for('x')],
            [Symbol.for('super')],
            [
              Symbol.for('set!'),
              [Symbol.for('.-x'), Symbol.for('this')],
              Symbol.for('x'),
            ],
          ],
          [Symbol.for('public'), Symbol.for('bar')],
          [
            Symbol.for('define'),
            [Symbol.for('bar')],
            [Symbol.for('.-x'), Symbol.for('this')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'class Foo {\n' +
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
        '}'
    );
  });
  it("(compile '(define-class Foo () (private x) (define x) (define (constructor x) (super) (set! (.-x this) x)) (private bar) (define (bar) (.-x this))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('private'), Symbol.for('x')],
          [Symbol.for('define'), Symbol.for('x')],
          [
            Symbol.for('define'),
            [Symbol.for('constructor'), Symbol.for('x')],
            [Symbol.for('super')],
            [
              Symbol.for('set!'),
              [Symbol.for('.-x'), Symbol.for('this')],
              Symbol.for('x'),
            ],
          ],
          [Symbol.for('private'), Symbol.for('bar')],
          [
            Symbol.for('define'),
            [Symbol.for('bar')],
            [Symbol.for('.-x'), Symbol.for('this')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'class Foo {\n' +
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
        '}'
    );
  });
  it("(compile '(define-class Foo () (define/public arr) (define/public (constructor arr) (set-field! arr this arr)) (define/public (nth i) (aget (get-field arr this) i))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-class'),
        Symbol.for('Foo'),
        [],
        [Symbol.for('define/public'), Symbol.for('arr')],
        [
          Symbol.for('define/public'),
          [Symbol.for('constructor'), Symbol.for('arr')],
          [
            Symbol.for('set-field!'),
            Symbol.for('arr'),
            Symbol.for('this'),
            Symbol.for('arr'),
          ],
        ],
        [
          Symbol.for('define/public'),
          [Symbol.for('nth'), Symbol.for('i')],
          [
            Symbol.for('aget'),
            [Symbol.for('get-field'), Symbol.for('arr'), Symbol.for('this')],
            Symbol.for('i'),
          ],
        ],
      ]),
      'class Foo {\n' +
        '  arr;\n' +
        '\n' +
        '  constructor(arr) {\n' +
        '    this.arr = arr;\n' +
        '  }\n' +
        '\n' +
        '  nth(i) {\n' +
        '    return this.arr[i];\n' +
        '  }\n' +
        '}'
    );
  });
  it("(compile '(define-class Foo () (define/public arr) (define (constructor arr) (set-field! arr this arr)) (define/generator (generator) (for ((x (get-field arr this))) (yield x)))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-class'),
        Symbol.for('Foo'),
        [],
        [Symbol.for('define/public'), Symbol.for('arr')],
        [
          Symbol.for('define'),
          [Symbol.for('constructor'), Symbol.for('arr')],
          [
            Symbol.for('set-field!'),
            Symbol.for('arr'),
            Symbol.for('this'),
            Symbol.for('arr'),
          ],
        ],
        [
          Symbol.for('define/generator'),
          [Symbol.for('generator')],
          [
            Symbol.for('for'),
            [
              [
                Symbol.for('x'),
                [
                  Symbol.for('get-field'),
                  Symbol.for('arr'),
                  Symbol.for('this'),
                ],
              ],
            ],
            [Symbol.for('yield'), Symbol.for('x')],
          ],
        ],
      ]),
      'class Foo {\n' +
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
        '}'
    );
  });
  return it("(compile '(define-class Foo () (define/public arr) (define (constructor arr) (set-field! arr this arr)) (define/generator ((get-field iterator Symbol)) (for ((x (get-field arr this))) (yield x)))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define-class'),
        Symbol.for('Foo'),
        [],
        [Symbol.for('define/public'), Symbol.for('arr')],
        [
          Symbol.for('define'),
          [Symbol.for('constructor'), Symbol.for('arr')],
          [
            Symbol.for('set-field!'),
            Symbol.for('arr'),
            Symbol.for('this'),
            Symbol.for('arr'),
          ],
        ],
        [
          Symbol.for('define/generator'),
          [
            [
              Symbol.for('get-field'),
              Symbol.for('iterator'),
              Symbol.for('Symbol'),
            ],
          ],
          [
            Symbol.for('for'),
            [
              [
                Symbol.for('x'),
                [
                  Symbol.for('get-field'),
                  Symbol.for('arr'),
                  Symbol.for('this'),
                ],
              ],
            ],
            [Symbol.for('yield'), Symbol.for('x')],
          ],
        ],
      ]),
      'class Foo {\n' +
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
        '}'
    );
  });
});

describe('define...class', function (): any {
  it("(compile '(define Foo (class object%)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('Foo'),
        [Symbol.for('class'), Symbol.for('object%')],
      ]),
      'class Foo {\n' + '}'
    );
  });
  return it("(compile '(define Foo (class Bar)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('Foo'),
        [Symbol.for('class'), Symbol.for('Bar')],
      ]),
      'class Foo extends Bar {\n' + '}'
    );
  });
});

describe('js', function (): any {
  it('(compile \'(js "1"))', function (): any {
    return assertEqual(compile([Symbol.for('js'), '1']), '1');
  });
  return it('(compile \'(js "function I(x) { return x; }"))', function (): any {
    return assertEqual(
      compile([Symbol.for('js'), 'function I(x) { return x; }']),
      'function I(x) { return x; }'
    );
  });
});

describe('make-hash', function (): any {
  it("(compile '(make-hash))", function (): any {
    return assertEqual(compile([Symbol.for('make-hash')]), 'new Map();');
  });
  it('(compile \'(make-hash \'(("foo" . "bar") ("baz" . "quux"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('make-hash'),
        [
          Symbol.for('quote'),
          [
            ['foo', Symbol.for('.'), 'bar'],
            ['baz', Symbol.for('.'), 'quux'],
          ],
        ],
      ]),
      "new Map([['foo', 'bar'], ['baz', 'quux']]);"
    );
  });
  it('(compile \'(make-hash \'(("foo" "bar") ("baz" "quux"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('make-hash'),
        [
          Symbol.for('quote'),
          [
            ['foo', 'bar'],
            ['baz', 'quux'],
          ],
        ],
      ]),
      "new Map([['foo', ['bar']], ['baz', ['quux']]]);"
    );
  });
  it('(compile \'(make-hash `(("foo" . "bar") ("baz" . "quux"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('make-hash'),
        [
          Symbol.for('quasiquote'),
          [
            ['foo', Symbol.for('.'), 'bar'],
            ['baz', Symbol.for('.'), 'quux'],
          ],
        ],
      ]),
      "new Map([['foo', 'bar'], ['baz', 'quux']]);"
    );
  });
  it('(compile \'(make-hash `(("foo" . "bar") ("baz" . "quux") ,@(hash->list xyzzy))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('make-hash'),
        [
          Symbol.for('quasiquote'),
          [
            ['foo', Symbol.for('.'), 'bar'],
            ['baz', Symbol.for('.'), 'quux'],
            [
              Symbol.for('unquote-splicing'),
              [Symbol.for('hash->list'), Symbol.for('xyzzy')],
            ],
          ],
        ],
      ]),
      "new Map([['foo', 'bar'], ['baz', 'quux'], ...xyzzy.entries()]);"
    );
  });
  xit('(compile \'(make-hash (append `(("foo" . "bar") ("baz" . "quux")) (hash->list xyzzy))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('make-hash'),
        [
          Symbol.for('append'),
          [
            Symbol.for('quasiquote'),
            [
              ['foo', Symbol.for('.'), 'bar'],
              ['baz', Symbol.for('.'), 'quux'],
            ],
          ],
          [Symbol.for('hash->list'), Symbol.for('xyzzy')],
        ],
      ]),
      "new Map([...[['foo', 'bar'], ['baz', 'quux']], ...xyzzy.entries()]);"
    );
  });
  return it('(compile \'(make-hash `(("foo" "bar") ("baz" "quux"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('make-hash'),
        [
          Symbol.for('quasiquote'),
          [
            ['foo', 'bar'],
            ['baz', 'quux'],
          ],
        ],
      ]),
      "new Map([['foo', ['bar']], ['baz', ['quux']]]);"
    );
  });
});

describe('->', function (): any {
  it('(compile \'(-> x (.foo "bar") (.baz)))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('->'),
        Symbol.for('x'),
        [Symbol.for('.foo'), 'bar'],
        [Symbol.for('.baz')],
      ]),
      "x.foo('bar').baz();"
    );
  });
  return it('(compile \'(-> regular-args (.map (lambda (arg) (compile-expression arg env inherited-options))) (.join ", ")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('->'),
        Symbol.for('regular-args'),
        [
          Symbol.for('.map'),
          [
            Symbol.for('lambda'),
            [Symbol.for('arg')],
            [
              Symbol.for('compile-expression'),
              Symbol.for('arg'),
              Symbol.for('env'),
              Symbol.for('inherited-options'),
            ],
          ],
        ],
        [Symbol.for('.join'), ', '],
      ]),
      'regularArgs.map(function (arg) {\n' +
        '  return compileExpression(arg, env, inheritedOptions);\n' +
        "}).join(', ');"
    );
  });
});

describe('js/switch', function (): any {
  it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/switch'),
        Symbol.for('x'),
        [
          Symbol.for('case'),
          'foo',
          [Symbol.for('display'), 'foo'],
          [Symbol.for('break')],
        ],
        [Symbol.for('default'), [Symbol.for('display'), 'bar']],
      ]),
      'switch (x) {\n' +
        "  case 'foo': {\n" +
        "    console.log('foo');\n" +
        '    break;\n' +
        '  }\n' +
        '  default: {\n' +
        "    console.log('bar');\n" +
        '  }\n' +
        '}'
    );
  });
  it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))) :as \'return)', function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('js/switch'),
          Symbol.for('x'),
          [
            Symbol.for('case'),
            'foo',
            [Symbol.for('display'), 'foo'],
            [Symbol.for('break')],
          ],
          [Symbol.for('default'), [Symbol.for('display'), 'bar']],
        ],
        Symbol.for(':as'),
        Symbol.for('return')
      ),
      'switch (x) {\n' +
        "  case 'foo': {\n" +
        "    return console.log('foo');\n" +
        '    break;\n' +
        '  }\n' +
        '  default: {\n' +
        "    return console.log('bar');\n" +
        '  }\n' +
        '}'
    );
  });
  return it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))) :as \'expression)', function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('js/switch'),
          Symbol.for('x'),
          [
            Symbol.for('case'),
            'foo',
            [Symbol.for('display'), 'foo'],
            [Symbol.for('break')],
          ],
          [Symbol.for('default'), [Symbol.for('display'), 'bar']],
        ],
        Symbol.for(':as'),
        Symbol.for('expression')
      ),
      '(() => {\n' +
        '  switch (x) {\n' +
        "    case 'foo': {\n" +
        "      return console.log('foo');\n" +
        '      break;\n' +
        '    }\n' +
        '    default: {\n' +
        "      return console.log('bar');\n" +
        '    }\n' +
        '  }\n' +
        '})()'
    );
  });
});

describe('js/try', function (): any {
  it("(compile '(js/try))", function (): any {
    return assertEqual(compile([Symbol.for('js/try')]), 'try {\n' + '}');
  });
  it("(compile '(js/try (set! x (/ 2 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
      ]),
      'try {\n' + '  x = 2 / 1;\n' + '}'
    );
  });
  it('(compile \'(js/try (set! x (/ 2 1)) (finally (display "cleanup"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
        [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
      ]),
      'try {\n' +
        '  x = 2 / 1;\n' +
        '} finally {\n' +
        "  console.log('cleanup');\n" +
        '}'
    );
  });
  it('(compile \'(js/try (set! x (/ 2 1)) (catch _ (display "there was an error")) (finally (display "cleanup"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
        [
          Symbol.for('catch'),
          Symbol.for('_'),
          [Symbol.for('display'), 'there was an error'],
        ],
        [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
      ]),
      'try {\n' +
        '  x = 2 / 1;\n' +
        '} catch {\n' +
        "  console.log('there was an error');\n" +
        '} finally {\n' +
        "  console.log('cleanup');\n" +
        '}'
    );
  });
  it('(compile \'(js/try (set! x (/ 2 1)) (catch e (display "there was an error")) (finally (display "cleanup"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
        [
          Symbol.for('catch'),
          Symbol.for('e'),
          [Symbol.for('display'), 'there was an error'],
        ],
        [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
      ]),
      'try {\n' +
        '  x = 2 / 1;\n' +
        '} catch (e) {\n' +
        "  console.log('there was an error');\n" +
        '} finally {\n' +
        "  console.log('cleanup');\n" +
        '}'
    );
  });
  return it('(compile \'(js/try (set! x (/ 2 1)) (catch e (display "there was an error")) (finally (display "cleanup"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('js/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
        [
          Symbol.for('catch'),
          Symbol.for('e'),
          [Symbol.for('display'), 'there was an error'],
        ],
        [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
      ]),
      'try {\n' +
        '  x = 2 / 1;\n' +
        '} catch (e) {\n' +
        "  console.log('there was an error');\n" +
        '} finally {\n' +
        "  console.log('cleanup');\n" +
        '}'
    );
  });
});

describe('clj/try', function (): any {
  it("(compile '(clj/try))", function (): any {
    return assertEqual(compile([Symbol.for('clj/try')]), 'try {\n' + '}');
  });
  it("(compile '(clj/try (set! x (/ 2 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('clj/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
      ]),
      'try {\n' + '  x = 2 / 1;\n' + '}'
    );
  });
  it('(compile \'(clj/try (set! x (/ 2 1)) (finally (display "cleanup"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('clj/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
        [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
      ]),
      'try {\n' +
        '  x = 2 / 1;\n' +
        '} finally {\n' +
        "  console.log('cleanup');\n" +
        '}'
    );
  });
  it('(compile \'(clj/try (set! x (/ 2 1)) (catch Object e (display "there was an error")) (finally (display "cleanup"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('clj/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
        [
          Symbol.for('catch'),
          Symbol.for('Object'),
          Symbol.for('e'),
          [Symbol.for('display'), 'there was an error'],
        ],
        [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
      ]),
      'try {\n' +
        '  x = 2 / 1;\n' +
        '} catch (e) {\n' +
        "  console.log('there was an error');\n" +
        '} finally {\n' +
        "  console.log('cleanup');\n" +
        '}'
    );
  });
  it('(compile \'(clj/try (set! x (/ 2 1)) (catch MyException e (display "there was an error") (return #f)) (finally (display "cleanup"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('clj/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
        [
          Symbol.for('catch'),
          Symbol.for('MyException'),
          Symbol.for('e'),
          [Symbol.for('display'), 'there was an error'],
          [Symbol.for('return'), false],
        ],
        [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
      ]),
      'try {\n' +
        '  x = 2 / 1;\n' +
        '} catch (e) {\n' +
        '  if (e instanceof MyException) {\n' +
        "    console.log('there was an error');\n" +
        '    return false;\n' +
        '  } else {\n' +
        '    throw e;\n' +
        '  }\n' +
        '} finally {\n' +
        "  console.log('cleanup');\n" +
        '}'
    );
  });
  return it('(compile \'(clj/try (set! x (/ 2 1)) (catch Object e (display "there was an error") (return #f)) (finally (display "cleanup"))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('clj/try'),
        [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
        [
          Symbol.for('catch'),
          Symbol.for('Object'),
          Symbol.for('e'),
          [Symbol.for('display'), 'there was an error'],
          [Symbol.for('return'), false],
        ],
        [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
      ]),
      'try {\n' +
        '  x = 2 / 1;\n' +
        '} catch (e) {\n' +
        "  console.log('there was an error');\n" +
        '  return false;\n' +
        '} finally {\n' +
        "  console.log('cleanup');\n" +
        '}'
    );
  });
});

describe('throw', function (): any {
  return it('(compile \'(throw (new Error "An error")))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('throw'),
        [Symbol.for('new'), Symbol.for('Error'), 'An error'],
      ]),
      "throw new Error('An error');"
    );
  });
});

describe('return', function (): any {
  it("(compile '(return))", function (): any {
    return assertEqual(compile([Symbol.for('return')]), 'return;');
  });
  return it("(compile '(return 0))", function (): any {
    return assertEqual(compile([Symbol.for('return'), 0]), 'return 0;');
  });
});

describe('yield', function (): any {
  it("(compile '(yield))", function (): any {
    return assertEqual(compile([Symbol.for('yield')]), 'yield;');
  });
  return it("(compile '(yield 0))", function (): any {
    return assertEqual(compile([Symbol.for('yield'), 0]), 'yield 0;');
  });
});

describe('await', function (): any {
  return it("(compile '(await (foo)))", function (): any {
    return assertEqual(
      compile([Symbol.for('await'), [Symbol.for('foo')]]),
      'await foo();'
    );
  });
});

describe('async', function (): any {
  it("(compile '(async (lambda (x) x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('async'),
        [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
      ]),
      'async function (x) {\n' + '  return x;\n' + '};'
    );
  });
  it("(compile '(define foo (async (lambda (x) x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('foo'),
        [
          Symbol.for('async'),
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        ],
      ]),
      'async function foo(x) {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define foo (async (lambda (x) x))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          Symbol.for('foo'),
          [
            Symbol.for('async'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'async function foo(x: any): Promise<any> {\n' + '  return x;\n' + '}'
    );
  });
  return it("(compile '(define/async (foo x) x))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define/async'),
        [Symbol.for('foo'), Symbol.for('x')],
        Symbol.for('x'),
      ]),
      'async function foo(x) {\n' + '  return x;\n' + '}'
    );
  });
});

describe('require', function (): any {
  it('(compile \'(require "foo"))', function (): any {
    return assertEqual(
      compile([Symbol.for('require'), 'foo']),
      "import * as foo from 'foo';"
    );
  });
  it('(compile \'(require "foo") :fes-module-interop #t)', function (): any {
    return assertEqual(
      compile(
        [Symbol.for('require'), 'foo'],
        Symbol.for(':fes-module-interop'),
        true
      ),
      "import foo from 'foo';"
    );
  });
  it('(compile \'(require foo "bar"))', function (): any {
    return assertEqual(
      compile([Symbol.for('require'), Symbol.for('foo'), 'bar']),
      "import * as foo from 'bar';"
    );
  });
  it('(compile \'(require foo "bar") :fes-module-interop #t)', function (): any {
    return assertEqual(
      compile(
        [Symbol.for('require'), Symbol.for('foo'), 'bar'],
        Symbol.for(':fes-module-interop'),
        true
      ),
      "import foo from 'bar';"
    );
  });
  it('(compile \'(require "foo" "bar") :fes-module-interop #t)', function (): any {
    return assertEqual(
      compile(
        [Symbol.for('require'), 'foo', 'bar'],
        Symbol.for(':fes-module-interop'),
        true
      ),
      "import foo from 'bar';"
    );
  });
  it("(compile '(require (only-in foo bar)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('require'),
        [Symbol.for('only-in'), Symbol.for('foo'), Symbol.for('bar')],
      ]),
      'import {\n' + '  bar\n' + "} from 'foo';"
    );
  });
  it("(compile '(require (only-in foo (bar baz))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('require'),
        [
          Symbol.for('only-in'),
          Symbol.for('foo'),
          [Symbol.for('bar'), Symbol.for('baz')],
        ],
      ]),
      'import {\n' + '  bar as baz\n' + "} from 'foo';"
    );
  });
  it('(compile \'(require (only-in "foo" (bar baz))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('require'),
        [Symbol.for('only-in'), 'foo', [Symbol.for('bar'), Symbol.for('baz')]],
      ]),
      'import {\n' + '  bar as baz\n' + "} from 'foo';"
    );
  });
  it("(compile '(require (only-in foo bar bar)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('require'),
        [
          Symbol.for('only-in'),
          Symbol.for('foo'),
          Symbol.for('bar'),
          Symbol.for('bar'),
        ],
      ]),
      'import {\n' + '  bar\n' + "} from 'foo';"
    );
  });
  it("(compile '(require (only-in foo bar (baz bar))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('require'),
        [
          Symbol.for('only-in'),
          Symbol.for('foo'),
          Symbol.for('bar'),
          [Symbol.for('baz'), Symbol.for('bar')],
        ],
      ]),
      'import {\n' + '  bar\n' + "} from 'foo';"
    );
  });
  xit('(compile \'(require \'foo "bar"))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('require'),
        [Symbol.for('quote'), Symbol.for('foo')],
        'bar',
      ]),
      "import foo from 'bar';"
    );
  });
  xit("(compile '(require foo :as bar))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('require'),
        Symbol.for('foo'),
        Symbol.for(':as'),
        Symbol.for('bar'),
      ]),
      "import bar from 'foo';"
    );
  });
  xit("(compile '(require (foo :as bar)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('require'),
        [Symbol.for('foo'), Symbol.for(':as'), Symbol.for('bar')],
      ]),
      "import bar from 'foo';"
    );
  });
  return xit('(compile \'(require ("foo" :as "bar")))', function (): any {
    return assertEqual(
      compile([Symbol.for('require'), ['foo', Symbol.for(':as'), 'bar']]),
      "import bar from 'foo';"
    );
  });
});

describe('provide', function (): any {
  it("(compile '(provide))", function (): any {
    return assertEqual(compile([Symbol.for('provide')]), '');
  });
  it("(compile '(provide x))", function (): any {
    return assertEqual(
      compile([Symbol.for('provide'), Symbol.for('x')]),
      'export {\n' + '  x\n' + '};'
    );
  });
  it("(compile '(provide x y))", function (): any {
    return assertEqual(
      compile([Symbol.for('provide'), Symbol.for('x'), Symbol.for('y')]),
      'export {\n' + '  x,\n' + '  y\n' + '};'
    );
  });
  it("(compile '(provide (rename-out (x y))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('provide'),
        [Symbol.for('rename-out'), [Symbol.for('x'), Symbol.for('y')]],
      ]),
      'export {\n' + '  x as y\n' + '};'
    );
  });
  it("(compile '(provide (rename-out (x y) (w z))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('provide'),
        [
          Symbol.for('rename-out'),
          [Symbol.for('x'), Symbol.for('y')],
          [Symbol.for('w'), Symbol.for('z')],
        ],
      ]),
      'export {\n' + '  x as y,\n' + '  w as z\n' + '};'
    );
  });
  it("(compile '(provide x (rename-out (y z))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('provide'),
        Symbol.for('x'),
        [Symbol.for('rename-out'), [Symbol.for('y'), Symbol.for('z')]],
      ]),
      'export {\n' + '  x,\n' + '  y as z\n' + '};'
    );
  });
  it("(compile '(provide x x))", function (): any {
    return assertEqual(
      compile([Symbol.for('provide'), Symbol.for('x'), Symbol.for('x')]),
      'export {\n' + '  x\n' + '};'
    );
  });
  it("(compile '(provide x (rename-out (y x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('provide'),
        Symbol.for('x'),
        [Symbol.for('rename-out'), [Symbol.for('y'), Symbol.for('x')]],
      ]),
      'export {\n' + '  x\n' + '};'
    );
  });
  it("(compile '(provide (rename-out (x js/undefined))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('provide'),
        [
          Symbol.for('rename-out'),
          [Symbol.for('x'), Symbol.for('js/undefined')],
        ],
      ]),
      'export {\n' + '  x as jsUndefined\n' + '};'
    );
  });
  it('(compile \'(provide (all-from-out "foo")))', function (): any {
    return assertEqual(
      compile([Symbol.for('provide'), [Symbol.for('all-from-out'), 'foo']]),
      "export * from 'foo';"
    );
  });
  return it('(compile \'(provide (all-from-out "foo") bar))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('provide'),
        [Symbol.for('all-from-out'), 'foo'],
        Symbol.for('bar'),
      ]),
      "export * from 'foo';\n" + '\n' + 'export {\n' + '  bar\n' + '};'
    );
  });
});

describe('module', function (): any {
  it("(compile '(module m scheme (define (I x) x) (define (K x y) x)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        [
          Symbol.for('define'),
          [Symbol.for('K'), Symbol.for('x'), Symbol.for('y')],
          Symbol.for('x'),
        ],
      ]),
      'function I(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        'function K(x, y) {\n' +
        '  return x;\n' +
        '}'
    );
  });
  it("(compile '(module m scheme (define I (lambda (x) x)) (define K (lambda (x y) x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        ],
        [
          Symbol.for('define'),
          Symbol.for('K'),
          [
            Symbol.for('lambda'),
            [Symbol.for('x'), Symbol.for('y')],
            Symbol.for('x'),
          ],
        ],
      ]),
      'let I = function (x) {\n' +
        '  return x;\n' +
        '};\n' +
        '\n' +
        'let K = function (x, y) {\n' +
        '  return x;\n' +
        '};'
    );
  });
  it("(compile '(module m scheme (define (foo length) length)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            [Symbol.for('foo'), Symbol.for('length')],
            Symbol.for('length'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function foo(length: any): any {\n' + '  return length;\n' + '}'
    );
  });
  it("(compile '(module m scheme (define (foo (length : Number)) : Number length)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            [
              Symbol.for('foo'),
              [Symbol.for('length'), Symbol.for(':'), Symbol.for('Number')],
            ],
            Symbol.for(':'),
            Symbol.for('Number'),
            Symbol.for('length'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function foo(length: number): number {\n' + '  return length;\n' + '}'
    );
  });
  xit("(compile '(module m scheme (define I (curry-n 1 (lambda (x) x))) (define K (curry-n 2 (lambda (x y) x)))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [
            Symbol.for('curry-n'),
            1,
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        [
          Symbol.for('define'),
          Symbol.for('K'),
          [
            Symbol.for('curry-n'),
            2,
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('y')],
              Symbol.for('x'),
            ],
          ],
        ],
      ]),
      'let I = curryN(1, function (x) {\n' +
        '  return x;\n' +
        '});\n' +
        '\n' +
        'let K = curryN(2, function (x, y) {\n' +
        '  return x;\n' +
        '});'
    );
  });
  it("(compile '(module m scheme (define truish #t) (define falsy (not truish))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [Symbol.for('define'), Symbol.for('truish'), true],
        [
          Symbol.for('define'),
          Symbol.for('falsy'),
          [Symbol.for('not'), Symbol.for('truish')],
        ],
      ]),
      'let truish = true;\n' + '\n' + 'let falsy = !truish;'
    );
  });
  it('(compile \'(module m scheme (require (only-in "foo" and or)) (and x (or y z))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('require'),
          [Symbol.for('only-in'), 'foo', Symbol.for('and'), Symbol.for('or')],
        ],
        [
          Symbol.for('and'),
          Symbol.for('x'),
          [Symbol.for('or'), Symbol.for('y'), Symbol.for('z')],
        ],
      ]),
      'import {\n' +
        '  and,\n' +
        '  or\n' +
        "} from 'foo';\n" +
        '\n' +
        'and(x, or(y, z));'
    );
  });
  it("(compile '(module m lisp (define x 1) (define y 2)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [Symbol.for('define'), Symbol.for('x'), 1],
        [Symbol.for('define'), Symbol.for('y'), 2],
      ]),
      'let x = 1;\n' + '\n' + 'let y = 2;'
    );
  });
  xit("(compile '(module m lisp (define (I x) x) (define x 1) (define *lisp-map* #t)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        [Symbol.for('define'), Symbol.for('x'), 1],
        [Symbol.for('define'), Symbol.for('*lisp-map*'), true],
      ]),
      'function I(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "I.fsource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];\n" +
        '\n' +
        'let x = 1;'
    );
  });
  xit('(compile \'(module m lisp (require (only-in "./combinators" I)) (define x 1) (define *lisp-map* #t)))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('require'),
          [Symbol.for('only-in'), './combinators', Symbol.for('I')],
        ],
        [Symbol.for('define'), Symbol.for('x'), 1],
        [Symbol.for('define'), Symbol.for('*lisp-map*'), true],
      ]),
      'import {\n' + '  I\n' + "} from './combinators';\n" + '\n' + 'let x = 1;'
    );
  });
  xit('(compile \'(module m lisp (require (only-in "./combinators" I)) (define x 1) (define *lisp-map* #t)))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('require'),
          [Symbol.for('only-in'), './combinators', Symbol.for('I')],
        ],
        [Symbol.for('define'), Symbol.for('x'), 1],
        [Symbol.for('define'), Symbol.for('*lisp-map*'), true],
      ]),
      'import {\n' +
        '  I\n' +
        "} from './combinators';\n" +
        '\n' +
        'let x: any = 1;'
    );
  });
  it("(compile '(module m lisp (define (js_ str) (js/eval str))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('js_'), Symbol.for('str')],
          [Symbol.for('js/eval'), Symbol.for('str')],
        ],
      ]),
      'function js_(str) {\n' + '  return eval(str);\n' + '}'
    );
  });
  it("(compile '(module m lisp (define (my-fn foldl f v l) (foldl f v l))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [
            Symbol.for('my-fn'),
            Symbol.for('foldl'),
            Symbol.for('f'),
            Symbol.for('v'),
            Symbol.for('l'),
          ],
          [
            Symbol.for('foldl'),
            Symbol.for('f'),
            Symbol.for('v'),
            Symbol.for('l'),
          ],
        ],
      ]),
      'function myFn(foldl, f, v, l) {\n' + '  return foldl(f, v, l);\n' + '}'
    );
  });
  it("(compile '(module m lisp (define (my-foldl-obj obj f v l) (.foldl obj f v l))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [
            Symbol.for('my-foldl-obj'),
            Symbol.for('obj'),
            Symbol.for('f'),
            Symbol.for('v'),
            Symbol.for('l'),
          ],
          [
            Symbol.for('.foldl'),
            Symbol.for('obj'),
            Symbol.for('f'),
            Symbol.for('v'),
            Symbol.for('l'),
          ],
        ],
      ]),
      'function myFoldlObj(obj, f, v, l) {\n' +
        '  return obj.foldl(f, v, l);\n' +
        '}'
    );
  });
  it("(compile '(module m lisp (define-class Foo () (define/public (foldl f v l) l))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [
            Symbol.for('define/public'),
            [
              Symbol.for('foldl'),
              Symbol.for('f'),
              Symbol.for('v'),
              Symbol.for('l'),
            ],
            Symbol.for('l'),
          ],
        ],
      ]),
      'class Foo {\n' +
        '  foldl(f, v, l) {\n' +
        '    return l;\n' +
        '  }\n' +
        '}'
    );
  });
  it("(compile '(module m lisp (define (my-pop lst x) (pop! lst x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-pop'), Symbol.for('lst'), Symbol.for('x')],
          [Symbol.for('pop!'), Symbol.for('lst'), Symbol.for('x')],
        ],
      ]),
      'function myPop(lst, x) {\n' + '  return lst.shift();\n' + '}'
    );
  });
  it("(compile '(module m lisp (define (my-pop-2 lst x) (pop! (append lst) x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-pop-2'), Symbol.for('lst'), Symbol.for('x')],
          [
            Symbol.for('pop!'),
            [Symbol.for('append'), Symbol.for('lst')],
            Symbol.for('x'),
          ],
        ],
      ]),
      'function myPop2(lst, x) {\n' + '  return [...lst].shift();\n' + '}'
    );
  });
  it("(compile '(module m lisp (define (my-pop-right lst x) (pop-right! lst x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-pop-right'), Symbol.for('lst'), Symbol.for('x')],
          [Symbol.for('pop-right!'), Symbol.for('lst'), Symbol.for('x')],
        ],
      ]),
      'function myPopRight(lst, x) {\n' + '  return lst.pop();\n' + '}'
    );
  });
  it("(compile '(module m lisp (define (my-pop-right-2 lst x) (pop-right! (append lst) x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-pop-right-2'), Symbol.for('lst'), Symbol.for('x')],
          [
            Symbol.for('pop-right!'),
            [Symbol.for('append'), Symbol.for('lst')],
            Symbol.for('x'),
          ],
        ],
      ]),
      'function myPopRight2(lst, x) {\n' + '  return [...lst].pop();\n' + '}'
    );
  });
  it("(compile '(module m lisp (define (my-push lst x) (push! lst x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-push'), Symbol.for('lst'), Symbol.for('x')],
          [Symbol.for('push!'), Symbol.for('lst'), Symbol.for('x')],
        ],
      ]),
      'function myPush(lst, x) {\n' +
        '  lst.unshift(x);\n' +
        '  return lst;\n' +
        '}'
    );
  });
  it("(compile '(module m lisp (define (my-push-2 lst x) (push! (append lst) x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-push-2'), Symbol.for('lst'), Symbol.for('x')],
          [
            Symbol.for('push!'),
            [Symbol.for('append'), Symbol.for('lst')],
            Symbol.for('x'),
          ],
        ],
      ]),
      'function myPush2(lst, x) {\n' +
        '  return (function (lst, x) {\n' +
        '    lst.unshift(x);\n' +
        '    return lst;\n' +
        '  })([...lst], x);\n' +
        '}'
    );
  });
  it("(compile '(module m lisp (define (my-push-3 lst x) (push! lst x) lst)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-push-3'), Symbol.for('lst'), Symbol.for('x')],
          [Symbol.for('push!'), Symbol.for('lst'), Symbol.for('x')],
          Symbol.for('lst'),
        ],
      ]),
      'function myPush3(lst, x) {\n' +
        '  lst.unshift(x);\n' +
        '  return lst;\n' +
        '}'
    );
  });
  it("(compile '(module m lisp (define (my-push-right lst x) (push-right! lst x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-push-right'), Symbol.for('lst'), Symbol.for('x')],
          [Symbol.for('push-right!'), Symbol.for('lst'), Symbol.for('x')],
        ],
      ]),
      'function myPushRight(lst, x) {\n' +
        '  lst.push(x);\n' +
        '  return lst;\n' +
        '}'
    );
  });
  it("(compile '(module m lisp (define (my-push-right-2 lst x) (push-right! (append lst) x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-push-right-2'), Symbol.for('lst'), Symbol.for('x')],
          [
            Symbol.for('push-right!'),
            [Symbol.for('append'), Symbol.for('lst')],
            Symbol.for('x'),
          ],
        ],
      ]),
      'function myPushRight2(lst, x) {\n' +
        '  return (function (lst, x) {\n' +
        '    lst.push(x);\n' +
        '    return lst;\n' +
        '  })([...lst], x);\n' +
        '}'
    );
  });
  return it("(compile '(module m lisp (define (my-push-right-3 lst x) (push-right! lst x) lst)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('lisp'),
        [
          Symbol.for('define'),
          [Symbol.for('my-push-right-3'), Symbol.for('lst'), Symbol.for('x')],
          [Symbol.for('push-right!'), Symbol.for('lst'), Symbol.for('x')],
          Symbol.for('lst'),
        ],
      ]),
      'function myPushRight3(lst, x) {\n' +
        '  lst.push(x);\n' +
        '  return lst;\n' +
        '}'
    );
  });
});

describe('ann', function (): any {
  it("(compile '(ann 1 Number) :to 'javascript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('ann'), 1, Symbol.for('Number')],
        Symbol.for(':to'),
        Symbol.for('javascript')
      ),
      '1;'
    );
  });
  it("(compile '(ann 1 Number) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('ann'), 1, Symbol.for('Number')],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      '1 as number;'
    );
  });
  it("(compile '(ann (list) Any) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('ann'), [Symbol.for('list')], Symbol.for('Any')],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      '[] as any;'
    );
  });
  it("(compile '(ann '() Any) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('ann'), [Symbol.for('quote'), []], Symbol.for('Any')],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      '[] as any;'
    );
  });
  it("(compile '(ann x (List Any)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('ann'),
          Symbol.for('x'),
          [Symbol.for('List'), Symbol.for('Any')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'x as [any];'
    );
  });
  it("(compile '(ann x (List Number Any)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('ann'),
          Symbol.for('x'),
          [Symbol.for('List'), Symbol.for('Number'), Symbol.for('Any')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'x as [number, any];'
    );
  });
  it("(compile '(ann x NN) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [Symbol.for('ann'), Symbol.for('x'), Symbol.for('NN')],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'x as NN;'
    );
  });
  it("(compile '(ann x (NN Any)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('ann'),
          Symbol.for('x'),
          [Symbol.for('NN'), Symbol.for('Any')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'x as NN<any>;'
    );
  });
  it("(compile '(ann x (NN Any Any)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('ann'),
          Symbol.for('x'),
          [Symbol.for('NN'), Symbol.for('Any'), Symbol.for('Any')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'x as NN<any,any>;'
    );
  });
  it("(compile '((ann (lambda (x) x) Any) 1) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          [
            Symbol.for('ann'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
            Symbol.for('Any'),
          ],
          1,
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      '(function (x: any): any {\n' + '  return x;\n' + '} as any)(1);'
    );
  });
  return it("(compile '(lambda (x) (ann (send x foo) Any)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('lambda'),
          [Symbol.for('x')],
          [
            Symbol.for('ann'),
            [Symbol.for('send'), Symbol.for('x'), Symbol.for('foo')],
            Symbol.for('Any'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function (x: any): any {\n' + '  return x.foo() as any;\n' + '};'
    );
  });
});

describe(':', function (): any {
  it("(compile '(begin (: x Any) (define x 1)) :to 'javascript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('Any')],
          [Symbol.for('define'), Symbol.for('x'), 1],
        ],
        Symbol.for(':to'),
        Symbol.for('javascript')
      ),
      'let x = 1;'
    );
  });
  it("(compile '(begin (: x Any) (define x 1)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('Any')],
          [Symbol.for('define'), Symbol.for('x'), 1],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: any = 1;'
    );
  });
  it('(compile \'(begin (: x String) (define x "1")) :to \'typescript)', function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('String')],
          [Symbol.for('define'), Symbol.for('x'), '1'],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      "let x: string = '1';"
    );
  });
  it("(compile '(begin (: x Number) (define x 1)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('Number')],
          [Symbol.for('define'), Symbol.for('x'), 1],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: number = 1;'
    );
  });
  it("(compile '(begin (: x Integer) (define x 1)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('Integer')],
          [Symbol.for('define'), Symbol.for('x'), 1],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: number = 1;'
    );
  });
  it("(compile '(begin (: x Natural) (define x 1)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('Natural')],
          [Symbol.for('define'), Symbol.for('x'), 1],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: number = 1;'
    );
  });
  it("(compile '(begin (: x Real) (define x 1)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('Real')],
          [Symbol.for('define'), Symbol.for('x'), 1],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: number = 1;'
    );
  });
  it("(compile '(begin (: x Symbol) (define x 'x)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('Symbol')],
          [
            Symbol.for('define'),
            Symbol.for('x'),
            [Symbol.for('quote'), Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      "let x: Symbol = Symbol.for('x');"
    );
  });
  it("(compile '(begin (: x Boolean) (define x #t)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('Boolean')],
          [Symbol.for('define'), Symbol.for('x'), true],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: boolean = true;'
    );
  });
  it("(compile '(begin (: x True) (define x #t)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('True')],
          [Symbol.for('define'), Symbol.for('x'), true],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: true = true;'
    );
  });
  it("(compile '(begin (: x False) (define x #f)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('False')],
          [Symbol.for('define'), Symbol.for('x'), false],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: false = false;'
    );
  });
  it("(compile '(begin (: x (U Number String)) (define x 1)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('x'),
            [Symbol.for('U'), Symbol.for('Number'), Symbol.for('String')],
          ],
          [Symbol.for('define'), Symbol.for('x'), 1],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: number | string = 1;'
    );
  });
  it("(compile '(begin (: x (U Number String Boolean)) (define x 1)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('x'),
            [
              Symbol.for('U'),
              Symbol.for('Number'),
              Symbol.for('String'),
              Symbol.for('Boolean'),
            ],
          ],
          [Symbol.for('define'), Symbol.for('x'), 1],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: number | string | boolean = 1;'
    );
  });
  it("(compile '(begin (: x (U Number (U String Boolean))) (define x 1)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('x'),
            [
              Symbol.for('U'),
              Symbol.for('Number'),
              [Symbol.for('U'), Symbol.for('String'), Symbol.for('Boolean')],
            ],
          ],
          [Symbol.for('define'), Symbol.for('x'), 1],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: number | (string | boolean) = 1;'
    );
  });
  it("(compile '(begin (: x (Listof Number)) (define x (list 1))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('x'),
            [Symbol.for('Listof'), Symbol.for('Number')],
          ],
          [Symbol.for('define'), Symbol.for('x'), [Symbol.for('list'), 1]],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: number[] = [1];'
    );
  });
  it("(compile '(begin (: x (Pairof Number)) (define x '(1 . 2))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('x'),
            [Symbol.for('Pairof'), Symbol.for('Number')],
          ],
          [
            Symbol.for('define'),
            Symbol.for('x'),
            [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      "let x: (number | Symbol)[] = [1, Symbol.for('.'), 2];"
    );
  });
  it('(compile \'(begin (: hello-world (-> Void)) (define (hello-world) (display "Hello world!"))) :to \'javascript)', function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('hello-world'),
            [Symbol.for('->'), Symbol.for('Void')],
          ],
          [
            Symbol.for('define'),
            [Symbol.for('hello-world')],
            [Symbol.for('display'), 'Hello world!'],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('javascript')
      ),
      'function helloWorld() {\n' + "  console.log('Hello world!');\n" + '}'
    );
  });
  it('(compile \'(begin (: hello-world (-> Void)) (define (hello-world) (display "Hello world!"))) :to \'typescript)', function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('hello-world'),
            [Symbol.for('->'), Symbol.for('Void')],
          ],
          [
            Symbol.for('define'),
            [Symbol.for('hello-world')],
            [Symbol.for('display'), 'Hello world!'],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function helloWorld(): void {\n' +
        "  console.log('Hello world!');\n" +
        '}'
    );
  });
  it("(compile '(begin (: f (-> Number Number)) (define (f x) x)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('f'),
            [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
          ],
          [
            Symbol.for('define'),
            [Symbol.for('f'), Symbol.for('x')],
            Symbol.for('x'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function f(x: number): number {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(begin (: f (-> Number Number)) (define f (lambda (x) x))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('f'),
            [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
          ],
          [
            Symbol.for('define'),
            Symbol.for('f'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let f: (a: number) => number = function (x: any): any {\n' +
        '  return x;\n' +
        '};'
    );
  });
  it("(compile '(begin (: f (-> Number Number)) (define f (foo (lambda (x) x)))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('f'),
            [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
          ],
          [
            Symbol.for('define'),
            Symbol.for('f'),
            [
              Symbol.for('foo'),
              [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
            ],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let f: (a: number) => number = foo(function (x: any): any {\n' +
        '  return x;\n' +
        '});'
    );
  });
  it("(compile '(begin (: f (-> Number Number Number)) (define (f x (y 1)) x)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('f'),
            [
              Symbol.for('->'),
              Symbol.for('Number'),
              Symbol.for('Number'),
              Symbol.for('Number'),
            ],
          ],
          [
            Symbol.for('define'),
            [Symbol.for('f'), Symbol.for('x'), [Symbol.for('y'), 1]],
            Symbol.for('x'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function f(x: number, y: number = 1): number {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(begin (: f (->* (Number) (Number) Number)) (define f (lambda (x (y 1)) x))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('f'),
            [
              Symbol.for('->*'),
              [Symbol.for('Number')],
              [Symbol.for('Number')],
              Symbol.for('Number'),
            ],
          ],
          [
            Symbol.for('define'),
            Symbol.for('f'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), [Symbol.for('y'), 1]],
              Symbol.for('x'),
            ],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let f: (a: number, b?: number) => number = function (x: any, y: any = 1): any {\n' +
        '  return x;\n' +
        '};'
    );
  });
  it("(compile '(begin (: f (-> Any * Any)) (define f (lambda x x))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('f'),
            [
              Symbol.for('->'),
              Symbol.for('Any'),
              Symbol.for('*'),
              Symbol.for('Any'),
            ],
          ],
          [
            Symbol.for('define'),
            Symbol.for('f'),
            [Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let f: (...a: any) => any = function (...x: any[]): any {\n' +
        '  return x;\n' +
        '};'
    );
  });
  it("(compile '(begin (: f (-> :rest Any Any)) (define f (lambda x x))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('f'),
            [
              Symbol.for('->'),
              Symbol.for(':rest'),
              Symbol.for('Any'),
              Symbol.for('Any'),
            ],
          ],
          [
            Symbol.for('define'),
            Symbol.for('f'),
            [Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let f: (...a: any) => any = function (...x: any[]): any {\n' +
        '  return x;\n' +
        '};'
    );
  });
  it("(compile '(begin (: f (->* :rest Any Any)) (define f (lambda x x))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('f'),
            [
              Symbol.for('->*'),
              Symbol.for(':rest'),
              Symbol.for('Any'),
              Symbol.for('Any'),
            ],
          ],
          [
            Symbol.for('define'),
            Symbol.for('f'),
            [Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let f: (...a: any) => any = function (...x: any[]): any {\n' +
        '  return x;\n' +
        '};'
    );
  });
  it("(compile '(begin (: f (->* :rest (Listof Any) Any)) (define f (lambda x x))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for(':'),
            Symbol.for('f'),
            [
              Symbol.for('->*'),
              Symbol.for(':rest'),
              [Symbol.for('Listof'), Symbol.for('Any')],
              Symbol.for('Any'),
            ],
          ],
          [
            Symbol.for('define'),
            Symbol.for('f'),
            [Symbol.for('lambda'), Symbol.for('x'), Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let f: (...a: any[]) => any = function (...x: any[]): any {\n' +
        '  return x;\n' +
        '};'
    );
  });
  return it("(compile '(begin (: x Foo) (define x (new Foo))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [Symbol.for(':'), Symbol.for('x'), Symbol.for('Foo')],
          [
            Symbol.for('define'),
            Symbol.for('x'),
            [Symbol.for('new'), Symbol.for('Foo')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let x: Foo = new Foo();'
    );
  });
});

describe('define-type', function (): any {
  it("(compile '(define-type NN (-> Number Number)) :to 'javascript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-type'),
          Symbol.for('NN'),
          [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
        ],
        Symbol.for(':to'),
        Symbol.for('javascript')
      ),
      ''
    );
  });
  it("(compile '(define-type NN (-> Number Number)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-type'),
          Symbol.for('NN'),
          [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'type NN = (a: number) => number;'
    );
  });
  it("(compile '(begin (define-type NN (-> Number Number)) (: f NN) (define f (lambda (x) x))) :to 'javascript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for('define-type'),
            Symbol.for('NN'),
            [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
          ],
          [Symbol.for(':'), Symbol.for('f'), Symbol.for('NN')],
          [
            Symbol.for('define'),
            Symbol.for('f'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('javascript')
      ),
      'let f = function (x) {\n' + '  return x;\n' + '};'
    );
  });
  it("(compile '(begin (define-type NN (-> Number Number)) (: f NN) (define f (lambda (x) x))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for('define-type'),
            Symbol.for('NN'),
            [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
          ],
          [Symbol.for(':'), Symbol.for('f'), Symbol.for('NN')],
          [
            Symbol.for('define'),
            Symbol.for('f'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'type NN = (a: number) => number;\n' +
        '\n' +
        'let f: NN = function (x: any): any {\n' +
        '  return x;\n' +
        '};'
    );
  });
  it("(compile '(define f (lambda ((x : Number)) x)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          Symbol.for('f'),
          [
            Symbol.for('lambda'),
            [[Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')]],
            Symbol.for('x'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let f: any = function (x: number): any {\n' + '  return x;\n' + '};'
    );
  });
  it("(compile '(define f (js/arrow ((x : Number)) x)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          Symbol.for('f'),
          [
            Symbol.for('js/arrow'),
            [[Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')]],
            Symbol.for('x'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'let f: any = (x: number): any => {\n' + '  return x;\n' + '};'
    );
  });
  it("(compile '(define (f (x : Number)) x) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          [
            Symbol.for('f'),
            [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')],
          ],
          Symbol.for('x'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function f(x: number): any {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define (f (x : Number) . args) x) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          [
            Symbol.for('f'),
            [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')],
            Symbol.for('.'),
            Symbol.for('args'),
          ],
          Symbol.for('x'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function f(x: number, ...args: any[]): any {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define (id (x : Number)) : Number x) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          [
            Symbol.for('id'),
            [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')],
          ],
          Symbol.for(':'),
          Symbol.for('Number'),
          Symbol.for('x'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function id(x: number): number {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define (f (x : Number 1)) : Number x) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          [
            Symbol.for('f'),
            [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number'), 1],
          ],
          Symbol.for(':'),
          Symbol.for('Number'),
          Symbol.for('x'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function f(x: number = 1): number {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define (f (options : Any (js/obj))) : Any x) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          [
            Symbol.for('f'),
            [
              Symbol.for('options'),
              Symbol.for(':'),
              Symbol.for('Any'),
              [Symbol.for('js/obj')],
            ],
          ],
          Symbol.for(':'),
          Symbol.for('Any'),
          Symbol.for('x'),
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'function f(options: any = {}): any {\n' + '  return x;\n' + '}'
    );
  });
  it("(compile '(define Foo (class object% (define/public x) (define (constructor (x : Number)) (set-field! x this x)))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          Symbol.for('Foo'),
          [
            Symbol.for('class'),
            Symbol.for('object%'),
            [Symbol.for('define/public'), Symbol.for('x')],
            [
              Symbol.for('define'),
              [
                Symbol.for('constructor'),
                [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')],
              ],
              [
                Symbol.for('set-field!'),
                Symbol.for('x'),
                Symbol.for('this'),
                Symbol.for('x'),
              ],
            ],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'class Foo {\n' +
        '  x: any;\n' +
        '\n' +
        '  constructor(x: number) {\n' +
        '    this.x = x;\n' +
        '  }\n' +
        '}'
    );
  });
  it("(compile '(define Foo (class object% (define/public x) (define (constructor (x : Number) . args) (set-field! x this x)))) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define'),
          Symbol.for('Foo'),
          [
            Symbol.for('class'),
            Symbol.for('object%'),
            [Symbol.for('define/public'), Symbol.for('x')],
            [
              Symbol.for('define'),
              [
                Symbol.for('constructor'),
                [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')],
                Symbol.for('.'),
                Symbol.for('args'),
              ],
              [
                Symbol.for('set-field!'),
                Symbol.for('x'),
                Symbol.for('this'),
                Symbol.for('x'),
              ],
            ],
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'class Foo {\n' +
        '  x: any;\n' +
        '\n' +
        '  constructor(x: number, ...args: any[]) {\n' +
        '    this.x = x;\n' +
        '  }\n' +
        '}'
    );
  });
  return xit("(compile '(begin (define-type NN (-> Number Number)) (: f NN) (define (f x) x)) :to 'typescript)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          [
            Symbol.for('define-type'),
            Symbol.for('NN'),
            [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
          ],
          [Symbol.for(':'), Symbol.for('f'), Symbol.for('NN')],
          [
            Symbol.for('define'),
            [Symbol.for('f'), Symbol.for('x')],
            Symbol.for('x'),
          ],
        ],
        Symbol.for(':to'),
        Symbol.for('typescript')
      ),
      'type NN = (a: number) => number;\n' +
        '\n' +
        'function f(x: number): number {\n' +
        '  return x;\n' +
        '};'
    );
  });
});

describe('field-bound?', function (): any {
  it("(compile '(begin (define foo (js/obj)) (define bar (field-bound? baz foo))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('js/obj')]],
        [
          Symbol.for('define'),
          Symbol.for('bar'),
          [Symbol.for('field-bound?'), Symbol.for('baz'), Symbol.for('foo')],
        ],
      ]),
      'let foo = {};\n' + '\n' + "let bar = foo && ('baz' in foo);"
    );
  });
  return it("(compile '(begin (define foo (js/obj)) (define bar (field-bound? baz-baz foo))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('js/obj')]],
        [
          Symbol.for('define'),
          Symbol.for('bar'),
          [
            Symbol.for('field-bound?'),
            Symbol.for('baz-baz'),
            Symbol.for('foo'),
          ],
        ],
      ]),
      'let foo = {};\n' + '\n' + "let bar = foo && ('bazBaz' in foo);"
    );
  });
});

describe('js/?.', function (): any {
  it("(compile '(define x (js/?. foo bar)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('x'),
        [Symbol.for('js/?.'), Symbol.for('foo'), Symbol.for('bar')],
      ]),
      'let x = foo?.bar;'
    );
  });
  it("(compile '(define x ((js/?. foo bar) baz)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('x'),
        [
          [Symbol.for('js/?.'), Symbol.for('foo'), Symbol.for('bar')],
          Symbol.for('baz'),
        ],
      ]),
      'let x = foo?.bar(baz);'
    );
  });
  return it("(compile '(define x (js/?. foo (bar))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('define'),
        Symbol.for('x'),
        [Symbol.for('js/?.'), Symbol.for('foo'), [Symbol.for('bar')]],
      ]),
      'let x = foo?.(bar);'
    );
  });
});

describe('assert', function (): any {
  it("(compile '(assert #t))", function (): any {
    return assertEqual(
      compile([Symbol.for('assert'), true]),
      'console.assert(true);'
    );
  });
  return it('(compile \'(assert #t "test"))', function (): any {
    return assertEqual(
      compile([Symbol.for('assert'), true, 'test']),
      "console.assert(true, 'test');"
    );
  });
});

describe('display', function (): any {
  it("(compile '(display #t))", function (): any {
    return assertEqual(
      compile([Symbol.for('display'), true]),
      'console.log(true);'
    );
  });
  return it('(compile \'(display #t "test"))', function (): any {
    return assertEqual(
      compile([Symbol.for('display'), true, 'test']),
      "console.log(true, 'test');"
    );
  });
});

describe('Macros', function (): any {
  xit("(compile '(module m scheme (defmacro foo () '(begin)) (foo)))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('defmacro'),
          Symbol.for('foo'),
          [],
          [Symbol.for('quote'), [Symbol.for('begin')]],
        ],
        [Symbol.for('foo')],
      ]),
      'function foo(exp, env) {\n' +
        "  return [Symbol.for('begin')];\n" +
        '}\n' +
        '\n' +
        "foo.ftype = 'macro';"
    );
  });
  it("(compile '(module m scheme (defmacro foo (x) x) (define (bar x) (foo x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('defmacro'),
          Symbol.for('foo'),
          [Symbol.for('x')],
          Symbol.for('x'),
        ],
        [
          Symbol.for('define'),
          [Symbol.for('bar'), Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
      ]),
      'function foo(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "foo.ftype = 'macro';\n" +
        '\n' +
        'function bar(x) {\n' +
        '  return x;\n' +
        '}'
    );
  });
  it("(compile '(module m scheme (defmacro foo (x) `(begin ,x)) (define (bar x) (foo x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('defmacro'),
          Symbol.for('foo'),
          [Symbol.for('x')],
          [
            Symbol.for('quasiquote'),
            [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('x')]],
          ],
        ],
        [
          Symbol.for('define'),
          [Symbol.for('bar'), Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
      ]),
      'function foo(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        "  return [Symbol.for('begin'), x];\n" +
        '}\n' +
        '\n' +
        "foo.ftype = 'macro';\n" +
        '\n' +
        'function bar(x) {\n' +
        '  return x;\n' +
        '}'
    );
  });
  it("(compile '(module m scheme (defmacro foo (x . args) x) (define (bar x) (foo x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('defmacro'),
          Symbol.for('foo'),
          [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')],
          Symbol.for('x'),
        ],
        [
          Symbol.for('define'),
          [Symbol.for('bar'), Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
      ]),
      'function foo(exp, env) {\n' +
        '  let [x, ...args] = exp.slice(1);\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "foo.ftype = 'macro';\n" +
        '\n' +
        'function bar(x) {\n' +
        '  return x;\n' +
        '}'
    );
  });
  it("(compile '(module m scheme (defmacro foo (x . args) x) (define bar (foo 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('defmacro'),
          Symbol.for('foo'),
          [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')],
          Symbol.for('x'),
        ],
        [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('foo'), 1]],
      ]),
      'function foo(exp, env) {\n' +
        '  let [x, ...args] = exp.slice(1);\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "foo.ftype = 'macro';\n" +
        '\n' +
        'let bar = 1;'
    );
  });
  it("(compile '(begin (defmacro foo (x . args) x) (define bar (foo 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('defmacro'),
          Symbol.for('foo'),
          [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')],
          Symbol.for('x'),
        ],
        [Symbol.for('define'), Symbol.for('bar'), [Symbol.for('foo'), 1]],
      ]),
      'function foo(exp, env) {\n' +
        '  let [x, ...args] = exp.slice(1);\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "foo.ftype = 'macro';\n" +
        '\n' +
        'let bar = 1;'
    );
  });
  it("(compile '(begin (define (foo x) x) (defmacro bar (x) (foo x)) (define baz (bar 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [Symbol.for('foo'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'function foo(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return foo(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = 1;'
    );
  });
  it("(compile '(begin (define (foo-bar x) x) (defmacro bar (x) (foo-bar x)) (define baz (bar 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [Symbol.for('foo-bar'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo-bar'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'function fooBar(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return fooBar(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = 1;'
    );
  });
  it("(compile '(begin (define (foo-bar x) x) (defmacro bar (x) (foo-bar 'x)) (define baz (bar 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [Symbol.for('foo-bar'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo-bar'), [Symbol.for('quote'), Symbol.for('x')]],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'function fooBar(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        "  return fooBar(Symbol.for('x'));\n" +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = x;'
    );
  });
  it("(compile '(begin (define (foo-bar x) 'x) (defmacro bar (x) (foo-bar x)) (define baz (bar 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          [Symbol.for('foo-bar'), Symbol.for('x')],
          [Symbol.for('quote'), Symbol.for('x')],
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo-bar'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'function fooBar(x) {\n' +
        "  return Symbol.for('x');\n" +
        '}\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return fooBar(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = x;'
    );
  });
  it("(compile '(module m scheme (define (foo-bar x) (keyword? x)) (defmacro bar (x) (foo-bar x)) (define baz (bar 1))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            [Symbol.for('foo-bar'), Symbol.for('x')],
            [Symbol.for('keyword?'), Symbol.for('x')],
          ],
          [
            Symbol.for('defmacro'),
            Symbol.for('bar'),
            [Symbol.for('x')],
            [Symbol.for('foo-bar'), Symbol.for('x')],
          ],
          [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [keywordp] = (() => {\n' +
        '  function keywordp_(obj) {\n' +
        "    return (typeof obj === 'symbol') && (obj.description.match(new RegExp('^:')) ? true : false);\n" +
        '  }\n' +
        '  return [keywordp_];\n' +
        '})();\n' +
        '\n' +
        'function fooBar(x) {\n' +
        '  return keywordp(x);\n' +
        '}\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return fooBar(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = false;'
    );
  });
  it("(compile '(begin (define foo (lambda (x) x)) (defmacro bar (x) (foo x)) (define baz (bar 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          Symbol.for('foo'),
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'let foo = function (x) {\n' +
        '  return x;\n' +
        '};\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return foo(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = 1;'
    );
  });
  it("(compile '(begin (define-values (foo) (list (lambda (x) x))) (defmacro bar (x) (foo x)) (define baz (bar 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define-values'),
          [Symbol.for('foo')],
          [
            Symbol.for('list'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'let [foo] = [function (x) {\n' +
        '  return x;\n' +
        '}];\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return foo(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = 1;'
    );
  });
  it('(compile \'(begin (define-fields (foo) (js/obj "foo" (lambda (x) x))) (defmacro bar (x) (foo x)) (define baz (bar 1))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define-fields'),
          [Symbol.for('foo')],
          [
            Symbol.for('js/obj'),
            'foo',
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'let {foo} = {\n' +
        '  foo: function (x) {\n' +
        '    return x;\n' +
        '  }\n' +
        '};\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return foo(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = 1;'
    );
  });
  it('(compile \'(begin (define-fields ((foo foo1)) (js/obj "foo" (lambda (x) x))) (defmacro bar (x) (foo1 x)) (define baz (bar 1))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define-fields'),
          [[Symbol.for('foo'), Symbol.for('foo1')]],
          [
            Symbol.for('js/obj'),
            'foo',
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo1'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'let {foo: foo1} = {\n' +
        '  foo: function (x) {\n' +
        '    return x;\n' +
        '  }\n' +
        '};\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return foo1(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = 1;'
    );
  });
  it("(compile '(begin (define/async (foo x) x) (defmacro bar (x) (foo x)) (define baz (bar 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define/async'),
          [Symbol.for('foo'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'async function foo(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return foo(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = 1;'
    );
  });
  it("(compile '(begin (define foo (async (lambda (x) x))) (defmacro bar (x) (foo x)) (define baz (bar 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define'),
          Symbol.for('foo'),
          [
            Symbol.for('async'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'async function foo(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return foo(x);\n' +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = 1;'
    );
  });
  it("(compile '(begin (define-fexpr (foo x) x) (defmacro bar (x) (foo x)) (define baz (bar 1))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define-fexpr'),
          [Symbol.for('foo'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('bar'),
          [Symbol.for('x')],
          [Symbol.for('foo'), Symbol.for('x')],
        ],
        [Symbol.for('define'), Symbol.for('baz'), [Symbol.for('bar'), 1]],
      ]),
      'function foo(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "foo.ftype = 'fexpr';\n" +
        '\n' +
        'function bar(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        "  return foo(Symbol.for('x'));\n" +
        '}\n' +
        '\n' +
        "bar.ftype = 'macro';\n" +
        '\n' +
        'let baz = x;'
    );
  });
  return it('(compile \'(begin (define-class Foo () (define/public (foo) "foo")) (define bar (new Foo)) (defmacro baz (x) (send bar foo)) (define quux (baz 1))))', function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define/public'), [Symbol.for('foo')], 'foo'],
        ],
        [
          Symbol.for('define'),
          Symbol.for('bar'),
          [Symbol.for('new'), Symbol.for('Foo')],
        ],
        [
          Symbol.for('defmacro'),
          Symbol.for('baz'),
          [Symbol.for('x')],
          [Symbol.for('send'), Symbol.for('bar'), Symbol.for('foo')],
        ],
        [Symbol.for('define'), Symbol.for('quux'), [Symbol.for('baz'), 1]],
      ]),
      'class Foo {\n' +
        '  foo() {\n' +
        "    return 'foo';\n" +
        '  }\n' +
        '}\n' +
        '\n' +
        'let bar = new Foo();\n' +
        '\n' +
        'function baz(exp, env) {\n' +
        '  let [x] = exp.slice(1);\n' +
        '  return bar.foo();\n' +
        '}\n' +
        '\n' +
        "baz.ftype = 'macro';\n" +
        '\n' +
        "let quux = 'foo';"
    );
  });
});

describe('Fexprs', function (): any {
  return it("(compile '(begin (define-fexpr (foo x) x) (define x 1) (define bar (foo x))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('begin'),
        [
          Symbol.for('define-fexpr'),
          [Symbol.for('foo'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        [Symbol.for('define'), Symbol.for('x'), 1],
        [
          Symbol.for('define'),
          Symbol.for('bar'),
          [Symbol.for('foo'), Symbol.for('x')],
        ],
      ]),
      'function foo(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "foo.ftype = 'fexpr';\n" +
        '\n' +
        'let x = 1;\n' +
        '\n' +
        "let bar = foo(Symbol.for('x'));"
    );
  });
});

describe('Global environment', function (): any {
  it("(compile '(module m scheme (define lst `(,symbol? ,boolean?))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            Symbol.for('lst'),
            [
              Symbol.for('quasiquote'),
              [
                [Symbol.for('unquote'), Symbol.for('symbol?')],
                [Symbol.for('unquote'), Symbol.for('boolean?')],
              ],
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [symbolp, booleanp] = (() => {\n' +
        '  function symbolp_(obj) {\n' +
        "    return typeof obj === 'symbol';\n" +
        '  }\n' +
        '  function booleanp_(obj) {\n' +
        "    return typeof obj === 'boolean';\n" +
        '  }\n' +
        '  return [symbolp_, booleanp_];\n' +
        '})();\n' +
        '\n' +
        'let lst = [symbolp, booleanp];'
    );
  });
  xit('(compile \'(define-values (_ regexp) (rl/sandbox ((js/arrow () (define __ (js/obj "@@functional/placeholder" #t)) (define (js/regexp_ input (flags #u)) (if (eq? (type-of input) "string") (new RegExp input flags) input)) (values __ js/regexp_))))) :finline-functions #t)', function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('define-values'),
          [Symbol.for('_'), Symbol.for('regexp')],
          [
            Symbol.for('rl/sandbox'),
            [
              [
                Symbol.for('js/arrow'),
                [],
                [
                  Symbol.for('define'),
                  Symbol.for('__'),
                  [Symbol.for('js/obj'), '@@functional/placeholder', true],
                ],
                [
                  Symbol.for('define'),
                  [
                    Symbol.for('js/regexp_'),
                    Symbol.for('input'),
                    [Symbol.for('flags'), undefined],
                  ],
                  [
                    Symbol.for('if'),
                    [
                      Symbol.for('eq?'),
                      [Symbol.for('type-of'), Symbol.for('input')],
                      'string',
                    ],
                    [
                      Symbol.for('new'),
                      Symbol.for('RegExp'),
                      Symbol.for('input'),
                      Symbol.for('flags'),
                    ],
                    Symbol.for('input'),
                  ],
                ],
                [
                  Symbol.for('values'),
                  Symbol.for('__'),
                  Symbol.for('js/regexp_'),
                ],
              ],
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [, regexp] = (() => {\n' +
        '  let __ = {\n' +
        "    '@@functional/placeholder': true\n" +
        '  };\n' +
        '  function jsRegexp_(input, flags = undefined) {\n' +
        "    if (typeof input === 'string') {\n" +
        '      return new RegExp(input, flags);\n' +
        '    } else {\n' +
        '      return input;\n' +
        '    }\n' +
        '  }\n' +
        '  return [__, jsRegexp_];\n' +
        '})();'
    );
  });
  it("(compile '(module m scheme (define one-plus-one (apply + '(1 1)))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            Symbol.for('one-plus-one'),
            [
              Symbol.for('apply'),
              Symbol.for('+'),
              [Symbol.for('quote'), [1, 1]],
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [_add] = (() => {\n' +
        '  function add_(...args) {\n' +
        '    let result = 0;\n' +
        '    for (let arg of args) {\n' +
        '      result = result + arg;\n' +
        '    }\n' +
        '    return result;\n' +
        '  }\n' +
        '  return [add_];\n' +
        '})();\n' +
        '\n' +
        'let onePlusOne = _add(1, 1);'
    );
  });
  it("(compile '(module m scheme (define one-minus-one (apply - '(1 1)))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            Symbol.for('one-minus-one'),
            [
              Symbol.for('apply'),
              Symbol.for('-'),
              [Symbol.for('quote'), [1, 1]],
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [_sub] = (() => {\n' +
        '  function sub_(...args) {\n' +
        '    let len = args.length;\n' +
        '    if (len === 0) {\n' +
        '      return 0;\n' +
        '    } else if (len === 1) {\n' +
        '      return -args[0];\n' +
        '    } else {\n' +
        '      let result = args[0];\n' +
        '      for (let i = 1; i < len; i++) {\n' +
        '        result = result - args[i];\n' +
        '      }\n' +
        '      return result;\n' +
        '    }\n' +
        '  }\n' +
        '  return [sub_];\n' +
        '})();\n' +
        '\n' +
        'let oneMinusOne = _sub(1, 1);'
    );
  });
  it("(compile '(module m scheme (define one-minus-one (apply - '(1 1)))))", function (): any {
    return assertEqual(
      compile([
        Symbol.for('module'),
        Symbol.for('m'),
        Symbol.for('scheme'),
        [
          Symbol.for('define'),
          Symbol.for('one-minus-one'),
          [Symbol.for('apply'), Symbol.for('-'), [Symbol.for('quote'), [1, 1]]],
        ],
      ]),
      'import {\n' +
        '  _sub\n' +
        "} from 'roselisp';\n" +
        '\n' +
        'let oneMinusOne = _sub(1, 1);'
    );
  });
  it("(compile '(module m scheme (define one-times-one (apply * '(1 1)))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            Symbol.for('one-times-one'),
            [
              Symbol.for('apply'),
              Symbol.for('*'),
              [Symbol.for('quote'), [1, 1]],
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [_mul] = (() => {\n' +
        '  function mul_(...args) {\n' +
        '    let result = 1;\n' +
        '    for (let arg of args) {\n' +
        '      result = result * arg;\n' +
        '    }\n' +
        '    return result;\n' +
        '  }\n' +
        '  return [mul_];\n' +
        '})();\n' +
        '\n' +
        'let oneTimesOne = _mul(1, 1);'
    );
  });
  it("(compile '(module m scheme (define one-divided-by-one (apply / '(1 1)))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            Symbol.for('one-divided-by-one'),
            [
              Symbol.for('apply'),
              Symbol.for('/'),
              [Symbol.for('quote'), [1, 1]],
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [_div] = (() => {\n' +
        '  function div_(...args) {\n' +
        '    if (args.length === 1) {\n' +
        '      return 1 / args[0];\n' +
        '    } else {\n' +
        '      let result = args[0];\n' +
        '      let _end = args.length;\n' +
        '      for (let i = 1; i < _end; i++) {\n' +
        '        result = result / args[i];\n' +
        '      }\n' +
        '      return result;\n' +
        '    }\n' +
        '  }\n' +
        '  return [div_];\n' +
        '})();\n' +
        '\n' +
        'let oneDividedByOne = _div(1, 1);'
    );
  });
  it('(compile \'(module m scheme (define foo-bar (apply string-append \'("foo" "bar")))) :finline-functions #t)', function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            Symbol.for('foo-bar'),
            [
              Symbol.for('apply'),
              Symbol.for('string-append'),
              [Symbol.for('quote'), ['foo', 'bar']],
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [stringAppend] = (() => {\n' +
        '  function stringAppend_(...args) {\n' +
        '    return args.reduce(function (acc, x) {\n' +
        '      return acc + x;\n' +
        "    }, '');\n" +
        '  }\n' +
        '  return [stringAppend_];\n' +
        '})();\n' +
        '\n' +
        "let fooBar = stringAppend('foo', 'bar');"
    );
  });
  xit("(compile '(module m lisp (define (my-foldl f v l) (foldl f v l)) (define bar (my-foldl + 0 '(1 2 3 4)))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('lisp'),
          [
            Symbol.for('define'),
            [
              Symbol.for('my-foldl'),
              Symbol.for('f'),
              Symbol.for('v'),
              Symbol.for('l'),
            ],
            [
              Symbol.for('foldl'),
              Symbol.for('f'),
              Symbol.for('v'),
              Symbol.for('l'),
            ],
          ],
          [
            Symbol.for('define'),
            Symbol.for('bar'),
            [
              Symbol.for('my-foldl'),
              Symbol.for('+'),
              0,
              [Symbol.for('quote'), [1, 2, 3, 4]],
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [add] = (function () {\n' +
        '  function add(...args) {\n' +
        '    return args.reduce(function (y, x) {\n' +
        '      return y + x;\n' +
        '    }, 0);\n' +
        '  }\n' +
        '  return [add];\n' +
        '})();\n' +
        '\n' +
        'function myFoldl(f, v, l) {\n' +
        '  return l.reduce(function (acc, x) {\n' +
        '    return f(x, acc);\n' +
        '  }, v);\n' +
        '}\n' +
        '\n' +
        'let bar = myFoldl(add, 0, [1, 2, 3, 4]);'
    );
  });
  xit("(compile '(module m lisp (define (my-foldl f v l) (foldl f v l))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('lisp'),
          [
            Symbol.for('define'),
            [
              Symbol.for('my-foldl'),
              Symbol.for('f'),
              Symbol.for('v'),
              Symbol.for('l'),
            ],
            [
              Symbol.for('foldl'),
              Symbol.for('f'),
              Symbol.for('v'),
              Symbol.for('l'),
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [foldl] = (function () {\n' +
        '  function foldl(f, v, lst) {\n' +
        '    return lst.reduce(function (acc, x) {\n' +
        '      return f(x, acc);\n' +
        '    }, v);\n' +
        '  }\n' +
        '  return [foldl];\n' +
        '})();\n' +
        '\n' +
        'function myFoldl(f, v, l) {\n' +
        '  return foldl(f, v, l);\n' +
        '}'
    );
  });
  it("(compile '(module m lisp (define (my-map f x) (map f x)) (define bar (my-map first '((1) (2) (3))))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('lisp'),
          [
            Symbol.for('define'),
            [Symbol.for('my-map'), Symbol.for('f'), Symbol.for('x')],
            [Symbol.for('map'), Symbol.for('f'), Symbol.for('x')],
          ],
          [
            Symbol.for('define'),
            Symbol.for('bar'),
            [
              Symbol.for('my-map'),
              Symbol.for('first'),
              [Symbol.for('quote'), [[1], [2], [3]]],
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [first] = (() => {\n' +
        '  function first_(lst) {\n' +
        '    return lst[0];\n' +
        '  }\n' +
        '  return [first_];\n' +
        '})();\n' +
        '\n' +
        'function myMap(f, x) {\n' +
        '  return x.map(function (x) {\n' +
        '    return f(x);\n' +
        '  });\n' +
        '}\n' +
        '\n' +
        'let bar = myMap(first, [[1], [2], [3]]);'
    );
  });
  xit("(compile '(module m lisp (define (foo f x y) (f x y)) (define (my-push-4 lst x) (foo push! lst x))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('lisp'),
          [
            Symbol.for('define'),
            [
              Symbol.for('foo'),
              Symbol.for('f'),
              Symbol.for('x'),
              Symbol.for('y'),
            ],
            [Symbol.for('f'), Symbol.for('x'), Symbol.for('y')],
          ],
          [
            Symbol.for('define'),
            [Symbol.for('my-push-4'), Symbol.for('lst'), Symbol.for('x')],
            [
              Symbol.for('foo'),
              Symbol.for('push!'),
              Symbol.for('lst'),
              Symbol.for('x'),
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [pushX] = (function () {\n' +
        '  function pushX(lst, x) {\n' +
        '    lst.unshift(x);\n' +
        '    return lst;\n' +
        '  }\n' +
        '  return [pushX];\n' +
        '})();\n' +
        '\n' +
        'function foo(f, x, y) {\n' +
        '  return f(x, y);\n' +
        '}\n' +
        '\n' +
        'function myPush4(lst, x) {\n' +
        '  return foo(pushX, lst, x);\n' +
        '}'
    );
  });
  xit("(compile '(module m lisp (define (get-push-function) push!) (define (my-push-4 lst x) ((get-push-function) lst x))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('lisp'),
          [
            Symbol.for('define'),
            [Symbol.for('get-push-function')],
            Symbol.for('push!'),
          ],
          [
            Symbol.for('define'),
            [Symbol.for('my-push-4'), Symbol.for('lst'), Symbol.for('x')],
            [
              [Symbol.for('get-push-function')],
              Symbol.for('lst'),
              Symbol.for('x'),
            ],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [pushX] = (function () {\n' +
        '  function pushX(lst, x) {\n' +
        '    lst.unshift(x);\n' +
        '    return lst;\n' +
        '  }\n' +
        '  return [pushX];\n' +
        '})();\n' +
        '\n' +
        'function getPushFunction() {\n' +
        '  return pushX;\n' +
        '}\n' +
        '\n' +
        'function myPush4(lst, x) {\n' +
        '  return getPushFunction()(lst, x);\n' +
        '}'
    );
  });
  it("(compile '(module m lisp (define (my-cdr x) (cdr x))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('lisp'),
          [
            Symbol.for('define'),
            [Symbol.for('my-cdr'), Symbol.for('x')],
            [Symbol.for('cdr'), Symbol.for('x')],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [cdr] = (() => {\n' +
        '  function cdr_(lst) {\n' +
        "    if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {\n" +
        '      return lst[2];\n' +
        '    } else {\n' +
        '      return lst.slice(1);\n' +
        '    }\n' +
        '  }\n' +
        '  return [cdr_];\n' +
        '})();\n' +
        '\n' +
        'function myCdr(x) {\n' +
        '  return cdr(x);\n' +
        '}'
    );
  });
  return it("(compile '(module m lisp (define (my-intersection x y) (intersection x y))) :finline-functions #t)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('lisp'),
          [
            Symbol.for('define'),
            [Symbol.for('my-intersection'), Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('intersection'), Symbol.for('x'), Symbol.for('y')],
          ],
        ],
        Symbol.for(':finline-functions'),
        true
      ),
      'let [intersection] = (() => {\n' +
        '  function intersection_(...args) {\n' +
        '    function intersection2(arr1, arr2) {\n' +
        '      let result = [];\n' +
        '      for (let element of arr1) {\n' +
        '        if (arr2.includes(element) && !result.includes(element)) {\n' +
        '          result.push(element);\n' +
        '        }\n' +
        '      }\n' +
        '      return result;\n' +
        '    }\n' +
        '    if (args.length === 0) {\n' +
        '      return [];\n' +
        '    } else if (args.length === 1) {\n' +
        '      return args[0];\n' +
        '    } else {\n' +
        '      return args.slice(1).reduce(function (acc, x) {\n' +
        '        return intersection2(acc, x);\n' +
        '      }, args[0]);\n' +
        '    }\n' +
        '  }\n' +
        '  return [intersection_];\n' +
        '})();\n' +
        '\n' +
        'function myIntersection(x, y) {\n' +
        '  return intersection(x, y);\n' +
        '}'
    );
  });
});

describe('compile-modules', function (): any {
  it('(module ... (define ...) ...)', function (): any {
    return assertEqual(
      compileModules(
        [
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('scheme'),
            [
              Symbol.for('define'),
              [Symbol.for('I'), Symbol.for('x')],
              Symbol.for('x'),
            ],
          ],
        ],
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      ['function I(x) {\n' + '  return x;\n' + '}']
    );
  });
  it('import macro from another module', function (): any {
    return assertEqual(
      compileModules(
        [
          [
            Symbol.for('module'),
            Symbol.for('a'),
            Symbol.for('scheme'),
            [
              Symbol.for('defmacro'),
              Symbol.for('foo'),
              [Symbol.for('x')],
              Symbol.for('x'),
            ],
            [Symbol.for('provide'), Symbol.for('foo')],
          ],
          [
            Symbol.for('module'),
            Symbol.for('b'),
            Symbol.for('scheme'),
            [
              Symbol.for('require'),
              [Symbol.for('only-in'), './a', Symbol.for('foo')],
            ],
            [Symbol.for('declare-macro'), Symbol.for('foo')],
            [
              Symbol.for('define'),
              [Symbol.for('bar'), Symbol.for('x')],
              [Symbol.for('foo'), Symbol.for('x')],
            ],
          ],
        ],
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      [
        'function foo(exp, env) {\n' +
          '  const [x] = exp.slice(1);\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          "foo.ftype = 'macro';\n" +
          '\n' +
          'export {\n' +
          '  foo\n' +
          '};',
        'import {\n' +
          '  foo\n' +
          "} from './a';\n" +
          '\n' +
          "foo.ftype = 'macro';\n" +
          '\n' +
          'function bar(x) {\n' +
          '  return x;\n' +
          '}',
      ]
    );
  });
  it('import macro from a module defined later', function (): any {
    return assertEqual(
      compileModules(
        [
          [
            Symbol.for('module'),
            Symbol.for('a'),
            Symbol.for('scheme'),
            [
              Symbol.for('require'),
              [Symbol.for('only-in'), './b', Symbol.for('bar')],
            ],
            [Symbol.for('declare-macro'), Symbol.for('bar')],
            [
              Symbol.for('define'),
              [Symbol.for('foo'), Symbol.for('x')],
              [Symbol.for('bar'), Symbol.for('x')],
            ],
          ],
          [
            Symbol.for('module'),
            Symbol.for('b'),
            Symbol.for('scheme'),
            [
              Symbol.for('defmacro'),
              Symbol.for('bar'),
              [Symbol.for('x')],
              Symbol.for('x'),
            ],
            [Symbol.for('provide'), Symbol.for('bar')],
          ],
        ],
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      [
        'import {\n' +
          '  bar\n' +
          "} from './b';\n" +
          '\n' +
          "bar.ftype = 'macro';\n" +
          '\n' +
          'function foo(x) {\n' +
          '  return x;\n' +
          '}',
        'function bar(exp, env) {\n' +
          '  const [x] = exp.slice(1);\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          "bar.ftype = 'macro';\n" +
          '\n' +
          'export {\n' +
          '  bar\n' +
          '};',
      ]
    );
  });
  it('import function for use in a macro', function (): any {
    return assertEqual(
      compileModules(
        [
          [
            Symbol.for('module'),
            Symbol.for('a'),
            Symbol.for('scheme'),
            [
              Symbol.for('require'),
              [Symbol.for('only-in'), './b', Symbol.for('baz')],
            ],
            [
              Symbol.for('defmacro'),
              Symbol.for('bar'),
              [Symbol.for('x')],
              [Symbol.for('baz'), Symbol.for('x')],
            ],
            [
              Symbol.for('define'),
              [Symbol.for('foo'), Symbol.for('x')],
              [Symbol.for('bar'), Symbol.for('x')],
            ],
          ],
          [
            Symbol.for('module'),
            Symbol.for('b'),
            Symbol.for('scheme'),
            [
              Symbol.for('define'),
              [Symbol.for('baz'), Symbol.for('x')],
              Symbol.for('x'),
            ],
            [Symbol.for('provide'), Symbol.for('baz')],
          ],
        ],
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      [
        'import {\n' +
          '  baz\n' +
          "} from './b';\n" +
          '\n' +
          'function bar(exp, env) {\n' +
          '  const [x] = exp.slice(1);\n' +
          '  return baz(x);\n' +
          '}\n' +
          '\n' +
          "bar.ftype = 'macro';\n" +
          '\n' +
          'function foo(x) {\n' +
          '  return x;\n' +
          '}',
        'function baz(x) {\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          'export {\n' +
          '  baz\n' +
          '};',
      ]
    );
  });
  return it('import renamed macro from another module', function (): any {
    return assertEqual(
      compileModules(
        [
          [
            Symbol.for('module'),
            Symbol.for('a'),
            Symbol.for('scheme'),
            [
              Symbol.for('defmacro'),
              Symbol.for('foo'),
              [Symbol.for('x')],
              Symbol.for('x'),
            ],
            [Symbol.for('provide'), Symbol.for('foo')],
          ],
          [
            Symbol.for('module'),
            Symbol.for('b'),
            Symbol.for('scheme'),
            [
              Symbol.for('require'),
              [
                Symbol.for('only-in'),
                './a',
                [Symbol.for('foo'), Symbol.for('foo1')],
              ],
            ],
            [Symbol.for('declare-macro'), Symbol.for('foo1')],
            [
              Symbol.for('define'),
              [Symbol.for('bar'), Symbol.for('x')],
              [Symbol.for('foo1'), Symbol.for('x')],
            ],
          ],
        ],
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      [
        'function foo(exp, env) {\n' +
          '  const [x] = exp.slice(1);\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          "foo.ftype = 'macro';\n" +
          '\n' +
          'export {\n' +
          '  foo\n' +
          '};',
        'import {\n' +
          '  foo as foo1\n' +
          "} from './a';\n" +
          '\n' +
          "foo1.ftype = 'macro';\n" +
          '\n' +
          'function bar(x) {\n' +
          '  return x;\n' +
          '}',
      ]
    );
  });
});

describe('--fsemicolon false', function (): any {
  return it("(compile '(begin x y z) :fsemicolon #f)", function (): any {
    return assertEqual(
      compile(
        [
          Symbol.for('begin'),
          Symbol.for('x'),
          Symbol.for('y'),
          Symbol.for('z'),
        ],
        Symbol.for(':fsemicolon'),
        false
      ),
      'x\n' + '\n' + 'y\n' + '\n' + 'z'
    );
  });
});

describe('compile-with-environment', function (): any {
  it('compiledEnvironment', function (): any {
    return assertEqual(
      ((): any => {
        const options: any = {};
        compileWithEnvironment(Symbol.for('foo'), undefined, options);
        const compiledEnv: any = options['compiledEnvironment'];
        return compiledEnv instanceof LispEnvironment;
      })(),
      true
    );
  });
  xit('has', function (): any {
    return assertEqual(
      ((): any => {
        const options: any = {};
        compileWithEnvironment(
          [Symbol.for('define'), Symbol.for('foo'), 1],
          undefined,
          options
        );
        const continuationEnv: any = options['continuationEnv'];
        return continuationEnv.has(Symbol.for('foo'));
      })(),
      true
    );
  });
  xit('EnvironmentStack', function (): any {
    return assertEqual(
      ((): any => {
        const options: any = {};
        compileWithEnvironment(Symbol.for('foo'), undefined, options);
        const continuationEnv: any = options['continuationEnv'];
        return continuationEnv instanceof EnvironmentStack;
      })(),
      true
    );
  });
  it(';; comment\n' + '(foo)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(';; comment\n' + '(foo)'),
        compilationEnvironment,
        {
          expressionType: 'statement',
          language: 'javascript',
          optimize: true,
        }
      ),
      '// comment\n' + 'foo();'
    );
  });
  it(';; multi-line\n' + ';; comment\n' + '(foo)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(';; multi-line\n' + ';; comment\n' + '(foo)'),
        compilationEnvironment,
        {
          expressionType: 'statement',
          language: 'javascript',
          optimize: true,
        }
      ),
      '// multi-line\n' + '// comment\n' + 'foo();'
    );
  });
  xit(';; multi-line\n' + ';;\n' + ';; comment\n' + '(foo)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(';; multi-line\n' + ';;\n' + ';; comment\n' + '(foo)'),
        compilationEnvironment,
        {
          expressionType: 'statement',
          language: 'javascript',
          optimize: true,
        }
      ),
      '// multi-line\n' + '//\n' + '// comment\n' + 'foo();'
    );
  });
  it(';; multiple\n' + '\n' + ';; comments\n' + '(foo)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(';; multiple\n' + '\n' + ';; comments\n' + '(foo)'),
        compilationEnvironment,
        {
          expressionType: 'statement',
          language: 'javascript',
          optimize: true,
        }
      ),
      '// multiple\n' + '\n' + '// comments\n' + 'foo();'
    );
  });
  it('(+\n' + ' ;; foo\n' + ' foo\n' + ' ;; bar\n' + ' bar)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(+\n' +
            '            ;; foo\n' +
            '            foo\n' +
            '            ;; bar\n' +
            '            bar)'
        ),
        compilationEnvironment,
        {
          expressionType: 'statement',
          language: 'javascript',
          optimize: true,
        }
      ),
      '(\n' + ' // foo\n' + ' foo +\n' + ' // bar\n' + ' bar\n' + ');'
    );
  });
  it(
    '(list foo\n' +
      '      ;; bar\n' +
      '      bar\n' +
      '      ;; baz\n' +
      '      baz)',
    function (): any {
      return assertEqual(
        compileWithEnvironment(
          readRose(
            '(list foo\n' +
              '      ;; bar\n' +
              '      bar\n' +
              '      ;; baz\n' +
              '      baz)'
          ),
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'javascript',
            optimize: true,
          }
        ),
        '[\n' +
          ' foo,\n' +
          ' // bar\n' +
          ' bar,\n' +
          ' // baz\n' +
          ' baz\n' +
          '];'
      );
    }
  );
  it('(+\n' + ' ;; foo\n' + ' foo\n' + ' ;; bar\n' + ' bar)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(+\n' +
            '            ;; foo\n' +
            '            foo\n' +
            '            ;; bar\n' +
            '            bar)'
        ),
        compilationEnvironment,
        {
          expressionType: 'statement',
          language: 'javascript',
          optimize: true,
        }
      ),
      '(\n' + ' // foo\n' + ' foo +\n' + ' // bar\n' + ' bar\n' + ');'
    );
  });
  it(';; comment\n' + '(foo)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(';; comment\n' + '(foo)'),
        compilationEnvironment,
        {
          expressionType: 'statement',
          language: 'javascript',
          optimize: true,
        }
      ),
      '// comment\n' + 'foo();'
    );
  });
  it('I & K', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; I combinator.\n' +
            '  (define (I x)\n' +
            '   ;; Just return x.\n' +
            '   x)\n' +
            '  ;;; K combinator.\n' +
            '  (define (K x y)\n' +
            '    x))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * I combinator.\n' +
        ' */\n' +
        'function I(x) {\n' +
        '  // Just return x.\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        '/**\n' +
        ' * K combinator.\n' +
        ' */\n' +
        'function K(x, y) {\n' +
        '  return x;\n' +
        '}'
    );
  });
  it('A, JS', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; A combinator.\n' +
            '  (define (A f . args)\n' +
            '    ;; Apply f to args.\n' +
            '    (apply f args)))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * A combinator.\n' +
        ' */\n' +
        'function A(f, ...args) {\n' +
        '  // Apply f to args.\n' +
        '  return f(...args);\n' +
        '}'
    );
  });
  it('A, TS', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; A combinator.\n' +
            '  (define (A f . args)\n' +
            '    ;; Apply f to args.\n' +
            '    (apply f args)))'
        ),
        compilationEnvironment,
        {
          language: 'typescript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * A combinator.\n' +
        ' */\n' +
        'function A(f: any, ...args: any[]): any {\n' +
        '  // Apply f to args.\n' +
        '  return f(...args);\n' +
        '}'
    );
  });
  it('B2, TS', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; B2 combinator.\n' +
            '  (define (B2 . args)\n' +
            '    (let ((fs (drop-right args 1))\n' +
            '          (x (array-list-last args)))\n' +
            '      (foldr A x fs))))'
        ),
        compilationEnvironment,
        {
          language: 'typescript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * B2 combinator.\n' +
        ' */\n' +
        'function B2(...args: any[]): any {\n' +
        '  const fs: any = args.slice(0, -1);\n' +
        '  const x: any = args[args.length - 1];\n' +
        '  return fs.reduceRight(function (acc: any, x: any): any {\n' +
        '    return A(x, acc);\n' +
        '  }, x);\n' +
        '}'
    );
  });
  it('(define ... (let ...))', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Foo.\n' +
            '  (define (foo x)\n' +
            '    ;; Bind y.\n' +
            '    (let ((y 1))\n' +
            '      ;; Return y.\n' +
            '      y)))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Foo.\n' +
        ' */\n' +
        'function foo(x) {\n' +
        '  // Bind y.\n' +
        '  const y = 1;\n' +
        '  // Return y.\n' +
        '  return y;\n' +
        '}'
    );
  });
  it('(define ... (if ...))', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Whether x is a truish value.\n' +
            '  (define (truish x)\n' +
            '    (if x\n' +
            '        ;; If x is truish, return true.\n' +
            '        #t\n' +
            '      ;; If x is falsey, return false.\n' +
            '      #f)))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Whether x is a truish value.\n' +
        ' */\n' +
        'function truish(x) {\n' +
        '  if (x) {\n' +
        '    // If x is truish, return true.\n' +
        '    return true;\n' +
        '  } else {\n' +
        '    // If x is falsey, return false.\n' +
        '    return false;\n' +
        '  }\n' +
        '}'
    );
  });
  it('(define ... (cond ...))', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Whether x is a truish value.\n' +
            '  (define (truish x)\n' +
            '    (cond\n' +
            '      ;; If x is truish, return true.\n' +
            '      (x\n' +
            '       #t)\n' +
            '      ;; If x is falsey, return false.\n' +
            '      (else\n' +
            '       #f))))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Whether x is a truish value.\n' +
        ' */\n' +
        'function truish(x) {\n' +
        '  if (x) {\n' +
        '    // If x is truish, return true.\n' +
        '    return true;\n' +
        '  } else {\n' +
        '    // If x is falsey, return false.\n' +
        '    return false;\n' +
        '  }\n' +
        '}'
    );
  });
  it('(define ... (let ...))', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Wrap a value in a list.\n' +
            '  (define (wrap-in-list x)\n' +
            '    ;; Return x wrapped in a list.\n' +
            '    `(,x)))'
        ),
        compilationEnvironment,
        {
          case: 'camelcase',
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Wrap a value in a list.\n' +
        ' */\n' +
        'function wrapInList(x) {\n' +
        '  // Return x wrapped in a list.\n' +
        '  return [x];\n' +
        '}'
    );
  });
  it('while...if', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; test function.\n' +
            '  (define (test)\n' +
            '    ;; while loop.\n' +
            '    (while foo\n' +
            '      (cond\n' +
            '       ;; bar case.\n' +
            '       (bar\n' +
            '        ;; inner cond.\n' +
            '        (cond\n' +
            '         (baz\n' +
            '          "baz")))\n' +
            '       ;; else case.\n' +
            '       (else\n' +
            '        "baz")))))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * test function.\n' +
        ' */\n' +
        'function test() {\n' +
        '  // while loop.\n' +
        '  while (foo) {\n' +
        '    if (bar) {\n' +
        '      // bar case.\n' +
        '      // inner cond.\n' +
        '      if (baz) {\n' +
        "        return 'baz';\n" +
        '      }\n' +
        '    } else {\n' +
        '      // else case.\n' +
        "      return 'baz';\n" +
        '    }\n' +
        '  }\n' +
        '}'
    );
  });
  it('(define-class Foo ...)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Foo class.\n' +
            '  (define-class Foo ()\n' +
            '    ;;; bar property.\n' +
            '    (define/public bar 0)\n' +
            '\n' +
            '    ;;; Foo constructor.\n' +
            '    (define/public (constructor n)\n' +
            '      ;; Set bar to n.\n' +
            '      (set! (.-this bar) n))))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Foo class.\n' +
        ' */\n' +
        'class Foo {\n' +
        '  /**\n' +
        '   * bar property.\n' +
        '   */\n' +
        '  bar = 0;\n' +
        '\n' +
        '  /**\n' +
        '   * Foo constructor.\n' +
        '   */\n' +
        '  constructor(n) {\n' +
        '    // Set bar to n.\n' +
        '    bar.this = n;\n' +
        '  }\n' +
        '}'
    );
  });
  it('(define-class Foo ...)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Foo class.\n' +
            '  (define-class Foo ()\n' +
            '    ;;; foo method.\n' +
            '    (define/public (foo)\n' +
            '      ;; this\n' +
            '      this)))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Foo class.\n' +
        ' */\n' +
        'class Foo {\n' +
        '  /**\n' +
        '   * foo method.\n' +
        '   */\n' +
        '  foo() {\n' +
        '    // this\n' +
        '    return this;\n' +
        '  }\n' +
        '}'
    );
  });
  it('(define Foo (class ...))', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Foo class.\n' +
            '  (define Foo\n' +
            '    (class object%\n' +
            '      ;;; bar method.\n' +
            '      (define/public (bar)\n' +
            '        ;; this\n' +
            '        this))))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Foo class.\n' +
        ' */\n' +
        'class Foo {\n' +
        '  /**\n' +
        '   * bar method.\n' +
        '   */\n' +
        '  bar() {\n' +
        '    // this\n' +
        '    return this;\n' +
        '  }\n' +
        '}'
    );
  });
  it('(define Foo (class ...))', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Foo class.\n' +
            '  (define Foo\n' +
            '    (class object%\n' +
            '      ;;; foo method.\n' +
            '      (define/public (foo)\n' +
            '        0)\n' +
            '\n' +
            '      ;;; bar generator method.\n' +
            '      (define/generator ((get-field iterator Symbol))\n' +
            '        (for ((x (list 1 2 3 4)))\n' +
            '          (yield x))))))'
        ),
        compilationEnvironment,
        {
          language: 'typescript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Foo class.\n' +
        ' */\n' +
        'class Foo {\n' +
        '  /**\n' +
        '   * foo method.\n' +
        '   */\n' +
        '  foo(): any {\n' +
        '    return 0;\n' +
        '  }\n' +
        '\n' +
        '  /**\n' +
        '   * bar generator method.\n' +
        '   */\n' +
        '  *[Symbol.iterator](): any {\n' +
        '    for (let x of [1, 2, 3, 4]) {\n' +
        '      yield x;\n' +
        '    }\n' +
        '  }\n' +
        '}'
    );
  });
  xit('(define (hello-world) ...)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Hello, world.\n' +
            '  (: hello-world (-> Void))\n' +
            '  (define (hello-world)\n' +
            '    (display "hello, world")))'
        ),
        compilationEnvironment,
        {
          case: 'camelcase',
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Hello, world.\n' +
        ' */\n' +
        'function helloWorld() {\n' +
        "  console.log('hello, world');\n" +
        '}'
    );
  });
  it(';;; Foo, blank line, (define (hello-world) ...)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(';;; Foo\n' + '\n' + '(require "foo")'),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' + ' * Foo\n' + ' */\n' + '\n' + "import * as foo from 'foo';"
    );
  });
  it(';; Foo, blank line, ;;; Bar, (define (hello-world) ...)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(';; Foo\n' + '\n' + ';;; Bar\n' + '(require "foo")'),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '// Foo\n' +
        '\n' +
        '/**\n' +
        ' * Bar\n' +
        ' */\n' +
        "import * as foo from 'foo';"
    );
  });
  it('(define (hello-world) ...)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(';; Foo\n' + ';;; Bar\n' + '\n' + '(require "foo")'),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '// Foo\n' +
        '/**\n' +
        ' * Bar\n' +
        ' */\n' +
        '\n' +
        "import * as foo from 'foo';"
    );
  });
  it('(define foo\n' + '  ;; bar\n' + '  bar)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose('(define foo\n' + '  ;; bar\n' + '  bar)'),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      'const foo =\n' + '  // bar\n' + '  bar;'
    );
  });
  it('(set! foo\n' + '  ;; bar\n' + '  bar)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose('(set! foo\n' + '  ;; bar\n' + '  bar)'),
        compilationEnvironment,
        {
          language: 'javascript',
          expressionType: 'statement',
          optimize: true,
        }
      ),
      'foo =\n' + '  // bar\n' + '  bar;'
    );
  });
  xit('x, camelCase', function (): any {
    return assertEqual(
      compileWithEnvironment(
        Symbol.for('x'),
        new LispEnvironment([['x', 1, 'variable']]),
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '1'
    );
  });
  it("(module m scheme ... (apply + '(1 1)) ...), comment", function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Module header.\n' +
            '\n' +
            '  (define one-plus-one\n' +
            "    (apply + '(1 1))))"
        ),
        compilationEnvironment,
        {
          case: 'camelcase',
          finlineFunctions: true,
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Module header.\n' +
        ' */\n' +
        '\n' +
        'const [_add] = (() => {\n' +
        '  function add_(...args) {\n' +
        '    let result = 0;\n' +
        '    for (let arg of args) {\n' +
        '      result = result + arg;\n' +
        '    }\n' +
        '    return result;\n' +
        '  }\n' +
        '  return [add_];\n' +
        '})();\n' +
        '\n' +
        'const onePlusOne = _add(1, 1);'
    );
  });
  it("(module m scheme ... (apply + '(1 1)) ...), comments", function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  ;;; Module header.\n' +
            '\n' +
            '  ;;; Custom addition function.\n' +
            '  (define one-plus-one\n' +
            "    (apply + '(1 1))))"
        ),
        compilationEnvironment,
        {
          case: 'camelcase',
          finlineFunctions: true,
          language: 'javascript',
          optimize: true,
        }
      ),
      '/**\n' +
        ' * Module header.\n' +
        ' */\n' +
        '\n' +
        'const [_add] = (() => {\n' +
        '  function add_(...args) {\n' +
        '    let result = 0;\n' +
        '    for (let arg of args) {\n' +
        '      result = result + arg;\n' +
        '    }\n' +
        '    return result;\n' +
        '  }\n' +
        '  return [add_];\n' +
        '})();\n' +
        '\n' +
        '/**\n' +
        ' * Custom addition function.\n' +
        ' */\n' +
        'const onePlusOne = _add(1, 1);'
    );
  });
  xit('(I x), JS function', function (): any {
    return assertEqual(
      compileWithEnvironment(
        [Symbol.for('I'), Symbol.for('x')],
        new LispEnvironment([
          [
            'I',
            function (x: any): any {
              return x;
            },
            'function',
          ],
        ]),
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      '(function {\n' +
        '   let I = function(x) {\n' +
        '     return x;\n' +
        '   }\n' +
        '   return I;\n' +
        '})()(x)'
    );
  });
  it('(truep x)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        [Symbol.for('truep'), Symbol.for('x')],
        compilationEnvironment,
        {
          case: 'camelcase',
          language: 'javascript',
          optimize: true,
        }
      ),
      'x ? true : false'
    );
  });
  it('(falsep x)', function (): any {
    return assertEqual(
      compileWithEnvironment(
        [Symbol.for('falsep'), Symbol.for('x')],
        compilationEnvironment,
        {
          case: 'camelcase',
          language: 'javascript',
          optimize: true,
        }
      ),
      'x ? false : true'
    );
  });
  it('read-rose', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose('(module m scheme\n' + '  (define foo\n' + '    `(foo)))'),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      "const foo = [Symbol.for('foo')];"
    );
  });
  it('read-rose, quasiquote', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  (define foo 1)\n' +
            '  (define bar\n' +
            '    `(,foo)))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      'const foo = 1;\n' + '\n' + 'const bar = [foo];'
    );
  });
  it('read-rose, quasiquoted list of pairs', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m scheme\n' +
            '  (define foo 1)\n' +
            '  (define bar 2)\n' +
            '  (define quux\n' +
            '    `(("foo" . ,foo)\n' +
            '       ("bar" . ,bar))))'
        ),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      'const foo = 1;\n' +
        '\n' +
        'const bar = 2;\n' +
        '\n' +
        "const quux = [['foo', Symbol.for('.'), foo], ['bar', Symbol.for('.'), bar]];"
    );
  });
  xit("(module m lisp ... (define *lisp-map* '()))", function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(module m lisp\n' +
            '  ;; inline-lisp-sources: true\n' +
            '\n' +
            '  (define (I x) x))'
        ),
        compilationEnvironment,
        {
          case: 'camelcase',
          language: 'javascript',
          optimize: true,
        }
      ),
      '// inline-lisp-sources: true\n' +
        '\n' +
        'function I(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "I.fsource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];"
    );
  });
  it('(: f (-> Number Number)), lambda, comments, TS', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readRose(
          '(begin\n' +
            '  ;; NN type alias.\n' +
            '  (define-type NN (-> Number Number))\n' +
            '  (: f NN)\n' +
            '  (define f\n' +
            '    (lambda (x)\n' +
            '      x)))'
        ),
        compilationEnvironment,
        {
          language: 'typescript',
          expressionType: 'statement',
          optimize: true,
        }
      ),
      '// NN type alias.\n' +
        'type NN = (a: number) => number;\n' +
        '\n' +
        'const f: NN = function (x: any): any {\n' +
        '  return x;\n' +
        '};'
    );
  });
  it('(compile-with-environment \'(module m scheme (define (foo x) x)) compilation-environment (js/obj "language" "javascript" "inlineLispSources" #t "optimize" #t))', function (): any {
    return assertEqual(
      compileWithEnvironment(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            [Symbol.for('foo'), Symbol.for('x')],
            Symbol.for('x'),
          ],
        ],
        compilationEnvironment,
        {
          language: 'javascript',
          inlineLispSources: true,
          optimize: true,
        }
      ),
      'function foo(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "foo.fsource = [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];"
    );
  });
  xit('(compile-with-environment \'(module m scheme (define foo (lambda (x) x))) compilation-environment (js/obj "language" "javascript" "inlineLispSources" #t "optimize" #t))', function (): any {
    return assertEqual(
      compileWithEnvironment(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ],
        compilationEnvironment,
        {
          language: 'javascript',
          inlineLispSources: true,
          optimize: true,
        }
      ),
      'const foo = function (x) {\n' +
        '  return x;\n' +
        '};\n' +
        '\n' +
        "foo.fsource = [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')];"
    );
  });
  return it('(compile-with-environment \'(module m scheme (define foo (async (lambda (x) x)))) compilation-environment (js/obj "language" "javascript" "inlineLispSources" #t "optimize" #t))', function (): any {
    return assertEqual(
      compileWithEnvironment(
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            Symbol.for('foo'),
            [
              Symbol.for('async'),
              [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
            ],
          ],
        ],
        compilationEnvironment,
        {
          language: 'javascript',
          inlineLispSources: true,
          optimize: true,
        }
      ),
      'async function foo(x) {\n' +
        '  return x;\n' +
        '}\n' +
        '\n' +
        "foo.fsource = [Symbol.for('define/async'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];"
    );
  });
});

describe('definition->macro', function (): any {
  it("(definition->macro '(define (inc x) (+ x 1)) '(1))", function (): any {
    return assertEqual(
      definitionToMacro(
        [
          Symbol.for('define'),
          [Symbol.for('inc'), Symbol.for('x')],
          [Symbol.for('+'), Symbol.for('x'), 1],
        ],
        [1]
      ),
      [Symbol.for('+'), 1, 1]
    );
  });
  it("(definition->macro '(define (logical-or x) (or x x)) '(#t))", function (): any {
    return assertEqual(
      definitionToMacro(
        [
          Symbol.for('define'),
          [Symbol.for('logical-or'), Symbol.for('x')],
          [Symbol.for('or'), Symbol.for('x'), Symbol.for('x')],
        ],
        [true]
      ),
      [Symbol.for('or'), true, true]
    );
  });
  it('(definition->macro \'(define (repeat x) (string-append x x)) \'("1"))', function (): any {
    return assertEqual(
      definitionToMacro(
        [
          Symbol.for('define'),
          [Symbol.for('repeat'), Symbol.for('x')],
          [Symbol.for('string-append'), Symbol.for('x'), Symbol.for('x')],
        ],
        ['1']
      ),
      [Symbol.for('string-append'), '1', '1']
    );
  });
  it("(definition->macro '(define (square x) (* x x)) '(1))", function (): any {
    return assertEqual(
      definitionToMacro(
        [
          Symbol.for('define'),
          [Symbol.for('square'), Symbol.for('x')],
          [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')],
        ],
        [1]
      ),
      [Symbol.for('*'), 1, 1]
    );
  });
  it("(definition->macro '(define (square x) (* x x)) '(x))", function (): any {
    return assertEqual(
      definitionToMacro(
        [
          Symbol.for('define'),
          [Symbol.for('square'), Symbol.for('x')],
          [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')],
        ],
        [Symbol.for('x')]
      ),
      [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')]
    );
  });
  return xit("(definition->macro '(define (square x) (* x x)) '((+ 1 1)))", function (): any {
    return assertEqual(
      definitionToMacro(
        [
          Symbol.for('define'),
          [Symbol.for('square'), Symbol.for('x')],
          [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')],
        ],
        [[Symbol.for('+'), 1, 1]]
      ),
      [
        [
          Symbol.for('lambda'),
          [Symbol.for('x')],
          [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')],
        ],
        [Symbol.for('+'), 1, 1],
      ]
    );
  });
});

describe('define-macro->lambda-form', function (): any {
  it("(define-macro->lambda-form '(define-macro (foo x) x))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm([
        Symbol.for('define-macro'),
        [Symbol.for('foo'), Symbol.for('x')],
        Symbol.for('x'),
      ]),
      [
        Symbol.for('lambda'),
        [Symbol.for('exp'), Symbol.for('env')],
        [
          Symbol.for('define-values'),
          [Symbol.for('x')],
          [Symbol.for('rest'), Symbol.for('exp')],
        ],
        Symbol.for('x'),
      ]
    );
  });
  it("(define-macro->lambda-form '(define-macro (foo &whole expression x) x))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm([
        Symbol.for('define-macro'),
        [
          Symbol.for('foo'),
          Symbol.for('&whole'),
          Symbol.for('expression'),
          Symbol.for('x'),
        ],
        Symbol.for('x'),
      ]),
      [
        Symbol.for('lambda'),
        [Symbol.for('expression'), Symbol.for('env')],
        [
          Symbol.for('define-values'),
          [Symbol.for('x')],
          [Symbol.for('rest'), Symbol.for('expression')],
        ],
        Symbol.for('x'),
      ]
    );
  });
  it("(define-macro->lambda-form '(define-macro (foo &whole exp &environment env) exp))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm([
        Symbol.for('define-macro'),
        [
          Symbol.for('foo'),
          Symbol.for('&whole'),
          Symbol.for('exp'),
          Symbol.for('&environment'),
          Symbol.for('env'),
        ],
        Symbol.for('exp'),
      ]),
      [
        Symbol.for('lambda'),
        [Symbol.for('exp'), Symbol.for('env')],
        Symbol.for('exp'),
      ]
    );
  });
  it("(define-macro->lambda-form '(define-macro (foo &whole exp &environment env x) x))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm([
        Symbol.for('define-macro'),
        [
          Symbol.for('foo'),
          Symbol.for('&whole'),
          Symbol.for('exp'),
          Symbol.for('&environment'),
          Symbol.for('env'),
          Symbol.for('x'),
        ],
        Symbol.for('x'),
      ]),
      [
        Symbol.for('lambda'),
        [Symbol.for('exp'), Symbol.for('env')],
        [
          Symbol.for('define-values'),
          [Symbol.for('x')],
          [Symbol.for('rest'), Symbol.for('exp')],
        ],
        Symbol.for('x'),
      ]
    );
  });
  it("(define-macro->lambda-form '(define-macro (foo &rest x) x))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm([
        Symbol.for('define-macro'),
        [Symbol.for('foo'), Symbol.for('&rest'), Symbol.for('x')],
        Symbol.for('x'),
      ]),
      [
        Symbol.for('lambda'),
        [Symbol.for('exp'), Symbol.for('env')],
        [
          Symbol.for('define-values'),
          Symbol.for('x'),
          [Symbol.for('rest'), Symbol.for('exp')],
        ],
        Symbol.for('x'),
      ]
    );
  });
  return it("(define-macro->lambda-form '(define-macro (foo x &rest y) x))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm([
        Symbol.for('define-macro'),
        [
          Symbol.for('foo'),
          Symbol.for('x'),
          Symbol.for('&rest'),
          Symbol.for('y'),
        ],
        Symbol.for('x'),
      ]),
      [
        Symbol.for('lambda'),
        [Symbol.for('exp'), Symbol.for('env')],
        [
          Symbol.for('define-values'),
          [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')],
          [Symbol.for('rest'), Symbol.for('exp')],
        ],
        Symbol.for('x'),
      ]
    );
  });
});

describe('split-comments', function (): any {
  xit('(split-comments ";;; Foo")', function (): any {
    return assertEqual(splitComments(';;; Foo'), [';;; Foo']);
  });
  it('(split-comments ";;; Foo\n' + '")', function (): any {
    return assertEqual(splitComments(';;; Foo\n'), [';;; Foo\n']);
  });
  xit('(split-comments ";; Foo\n' + ';;; Bar")', function (): any {
    return assertEqual(splitComments(';; Foo\n' + ';;; Bar'), [
      ';; Foo\n',
      ';;; Bar',
    ]);
  });
  return xit(
    '(split-comments ";; Foo\n' + ';;; Bar\n' + '")',
    function (): any {
      return assertEqual(splitComments(';; Foo\n' + ';;; Bar\n'), [
        ';; Foo\n',
        ';;; Bar\n',
      ]);
    }
  );
});
