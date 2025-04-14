import * as chai from 'chai';

import { I } from '../../src/ts/combinators';

import {
  LispEnvironment,
  compilationEnvironment,
  compile,
  compileModules,
  definitionToMacro,
  defineMacroToLambdaForm,
  splitComments,
} from '../../src/ts/language';

import { readRose } from '../../src/ts/parser';

import { sexp } from '../../src/ts/sexp';

import { assertEqual } from './test-util';

describe('compile-modules', function (): any {
  describe('single module', function (): any {
    return it('(module ... (define ...) ...)', function (): any {
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
            language: 'JavaScript',
          }
        ),
        ['function I(x) {\n' + '  return x;\n' + '}']
      );
    });
  });
  return describe('multiple modules', function (): any {
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
              [
                Symbol.for('define'),
                [Symbol.for('bar'), Symbol.for('x')],
                [Symbol.for('foo'), Symbol.for('x')],
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        [
          'function foo(exp, env) {\n' +
            '  const [x] = exp.slice(1);\n' +
            '  return x;\n' +
            '}\n' +
            '\n' +
            'foo.lispMacro = true;\n' +
            '\n' +
            'export {\n' +
            '  foo\n' +
            '};',
          'import {\n' +
            '  foo\n' +
            "} from './a';\n" +
            '\n' +
            'function bar(x) {\n' +
            '  return x;\n' +
            '}',
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
              [
                Symbol.for('define'),
                [Symbol.for('bar'), Symbol.for('x')],
                [Symbol.for('foo1'), Symbol.for('x')],
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        [
          'function foo(exp, env) {\n' +
            '  const [x] = exp.slice(1);\n' +
            '  return x;\n' +
            '}\n' +
            '\n' +
            'foo.lispMacro = true;\n' +
            '\n' +
            'export {\n' +
            '  foo\n' +
            '};',
          'import {\n' +
            '  foo as foo1\n' +
            "} from './a';\n" +
            '\n' +
            'function bar(x) {\n' +
            '  return x;\n' +
            '}',
        ]
      );
    });
  });
});

describe('compile', function (): any {
  describe('macros', function (): any {
    // FIXME: Failing test
    xit('(module ... (defmacro foo ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function foo(exp, env) {\n' +
          "  return [Symbol.for('begin')];\n" +
          '}\n' +
          '\n' +
          'foo.lispMacro = true;'
      );
    });
    it('(module ... (defmacro foo ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function foo(exp, env) {\n' +
          '  const [x] = exp.slice(1);\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          'foo.lispMacro = true;\n' +
          '\n' +
          'function bar(x) {\n' +
          '  return x;\n' +
          '}'
      );
    });
    it('(module ... (defmacro foo (x . args) ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function foo(exp, env) {\n' +
          '  const [x, ...args] = exp.slice(1);\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          'foo.lispMacro = true;\n' +
          '\n' +
          'function bar(x) {\n' +
          '  return x;\n' +
          '}'
      );
    });
    it('(module ... (defmacro foo (x . args) ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
              Symbol.for('bar'),
              [Symbol.for('foo'), 1, 2, 3],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function foo(exp, env) {\n' +
          '  const [x, ...args] = exp.slice(1);\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          'foo.lispMacro = true;\n' +
          '\n' +
          'const bar = 1;'
      );
    });
    return xit('(begin (defmacro foo (x . args) ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [
              Symbol.for('defmacro'),
              Symbol.for('foo'),
              [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')],
              Symbol.for('x'),
            ],
            [
              Symbol.for('define'),
              Symbol.for('bar'),
              [Symbol.for('foo'), 1, 2, 3],
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'function foo(exp, env) {\n' +
          '  const [x, ...args] = exp.slice(1);\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          'foo.lispMacro = true;\n' +
          '\n' +
          'const bar = 1;'
      );
    });
  });
  describe('comments', function (): any {
    it(';; comment\n' + '(foo)', function (): any {
      return assertEqual(
        compile(readRose(';; comment\n' + '(foo)'), compilationEnvironment, {
          expressionType: 'statement',
          language: 'JavaScript',
        }),
        '// comment\n' + 'foo();'
      );
    });
    it(';; multi-line\n' + ';; comment\n' + '(foo)', function (): any {
      return assertEqual(
        compile(
          readRose(';; multi-line\n' + ';; comment\n' + '(foo)'),
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        '// multi-line\n' + '// comment\n' + 'foo();'
      );
    });
    xit(
      ';; multi-line\n' + ';;\n' + ';; comment\n' + '(foo)',
      function (): any {
        return assertEqual(
          compile(
            readRose(';; multi-line\n' + ';;\n' + ';; comment\n' + '(foo)'),
            compilationEnvironment,
            {
              expressionType: 'statement',
              language: 'JavaScript',
            }
          ),
          '// multi-line\n' + '//\n' + '// comment\n' + 'foo();'
        );
      }
    );
    it(';; multiple\n' + '\n' + ';; comments\n' + '(foo)', function (): any {
      return assertEqual(
        compile(
          readRose(';; multiple\n' + '\n' + ';; comments\n' + '(foo)'),
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        '// multiple\n' + '\n' + '// comments\n' + 'foo();'
      );
    });
    it(
      '(+\n' + ' ;; foo\n' + ' foo\n' + ' ;; bar\n' + ' bar)',
      function (): any {
        return assertEqual(
          compile(
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
              language: 'JavaScript',
            }
          ),
          '(\n' + ' // foo\n' + ' foo +\n' + ' // bar\n' + ' bar\n' + ');'
        );
      }
    );
    it(
      '(list foo\n' +
        '      ;; bar\n' +
        '      bar\n' +
        '      ;; baz\n' +
        '      baz)',
      function (): any {
        return assertEqual(
          compile(
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
              language: 'JavaScript',
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
    it(
      '(+\n' + ' ;; foo\n' + ' foo\n' + ' ;; bar\n' + ' bar)',
      function (): any {
        return assertEqual(
          compile(
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
              language: 'JavaScript',
            }
          ),
          '(\n' + ' // foo\n' + ' foo +\n' + ' // bar\n' + ' bar\n' + ');'
        );
      }
    );
    it(';; comment\n' + '(foo)', function (): any {
      return assertEqual(
        compile(readRose(';; comment\n' + '(foo)'), compilationEnvironment, {
          expressionType: 'statement',
          language: 'JavaScript',
        }),
        '// comment\n' + 'foo();'
      );
    });
    it('I & K', function (): any {
      return assertEqual(
        compile(
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
            language: 'JavaScript',
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
        compile(
          readRose(
            '(module m scheme\n' +
              '  ;;; A combinator.\n' +
              '  (define (A f . args)\n' +
              '    ;; Apply f to args.\n' +
              '    (apply f args)))'
          ),
          compilationEnvironment,
          {
            language: 'JavaScript',
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
        compile(
          readRose(
            '(module m scheme\n' +
              '  ;;; A combinator.\n' +
              '  (define (A f . args)\n' +
              '    ;; Apply f to args.\n' +
              '    (apply f args)))'
          ),
          compilationEnvironment,
          {
            language: 'TypeScript',
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
        compile(
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
            language: 'TypeScript',
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
        compile(
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
            language: 'JavaScript',
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
        compile(
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
            language: 'JavaScript',
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
        compile(
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
            language: 'JavaScript',
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
        compile(
          readRose(
            '(module m scheme\n' +
              '  ;;; Wrap a value in a list.\n' +
              '  (define (wrap-in-list x)\n' +
              '    ;; Return x wrapped in a list.\n' +
              '    `(,x)))'
          ),
          compilationEnvironment,
          {
            language: 'JavaScript',
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
        compile(
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
            language: 'JavaScript',
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
        compile(
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
            language: 'JavaScript',
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
        compile(
          readRose(
            '(module m scheme\n' +
              '  ;;; Foo class.\n' +
              '  (define-class Foo ()\n' +
              '    ;;; bar property.\n' +
              '    (define/public (foo)\n' +
              '      ;; this\n' +
              '      this)))'
          ),
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '/**\n' +
          ' * Foo class.\n' +
          ' */\n' +
          'class Foo {\n' +
          '  /**\n' +
          '   * bar property.\n' +
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
        compile(
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
            language: 'JavaScript',
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
    xit('(define (hello-world) ...)', function (): any {
      return assertEqual(
        compile(
          readRose(
            '(module m scheme\n' +
              '             ;; Hello, world.\n' +
              '             (: hello-world (-> Void))\n' +
              '             (define (hello-world)\n' +
              '               (display "hello, world")))'
          ),
          compilationEnvironment,
          {
            language: 'JavaScript',
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
        compile(
          readRose(';;; Foo\n' + '\n' + '(require "foo")'),
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '/**\n' + ' * Foo\n' + ' */\n' + '\n' + "import * as foo from 'foo';"
      );
    });
    it(';; Foo, blank line, ;;; Bar, (define (hello-world) ...)', function (): any {
      return assertEqual(
        compile(
          readRose(';; Foo\n' + '\n' + ';;; Bar\n' + '(require "foo")'),
          compilationEnvironment,
          {
            language: 'JavaScript',
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
        compile(
          readRose(';; Foo\n' + ';;; Bar\n' + '\n' + '(require "foo")'),
          compilationEnvironment,
          {
            language: 'JavaScript',
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
        compile(
          readRose('(define foo\n' + '  ;; bar\n' + '  bar)'),
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'const foo =\n' + '  // bar\n' + '  bar;'
      );
    });
    return it('(set! foo\n' + '  ;; bar\n' + '  bar)', function (): any {
      return assertEqual(
        compile(
          readRose('(set! foo\n' + '  ;; bar\n' + '  bar)'),
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'foo =\n' + '  // bar\n' + '  bar;'
      );
    });
  });
  describe('symbols', function (): any {
    it('#t', function (): any {
      return assertEqual(
        compile(true, compilationEnvironment, {
          language: 'JavaScript',
        }),
        'true'
      );
    });
    it('#f', function (): any {
      return assertEqual(
        compile(false, compilationEnvironment, {
          language: 'JavaScript',
        }),
        'false'
      );
    });
    /**
     * fails
     */
    it('undefined', function (): any {
      return assertEqual(
        compile(sexp`undefined`, compilationEnvironment, {
          language: 'JavaScript',
        }),
        'undefined'
      );
    });
    it('js/undefined', function (): any {
      return assertEqual(
        compile(sexp`js/undefined`, compilationEnvironment, {
          language: 'JavaScript',
        }),
        'undefined'
      );
    });
    it('js-undefined', function (): any {
      return assertEqual(
        compile(sexp`js-undefined`, compilationEnvironment, {
          language: 'JavaScript',
        }),
        'undefined'
      );
    });
    it('js/null', function (): any {
      return assertEqual(
        compile(sexp`js/null`, compilationEnvironment, {
          language: 'JavaScript',
        }),
        'null'
      );
    });
    it('js-null', function (): any {
      return assertEqual(
        compile(sexp`js-null`, compilationEnvironment, {
          language: 'JavaScript',
        }),
        'null'
      );
    });
    xit('nil', function (): any {
      return assertEqual(
        compile(sexp`nil`, compilationEnvironment, {
          language: 'JavaScript',
        }),
        'null'
      );
    });
    xit('null', function (): any {
      return assertEqual(
        compile(sexp`null`, compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[]'
      );
    });
    it('foo-bar', function (): any {
      return assertEqual(
        compile(Symbol.for('foo-bar'), compilationEnvironment, {
          language: 'JavaScript',
        }),
        'fooBar'
      );
    });
    it('foo-bar, camelCase', function (): any {
      return assertEqual(
        compile(Symbol.for('foo-bar'), compilationEnvironment, {
          camelCase: true,
          language: 'JavaScript',
        }),
        'fooBar'
      );
    });
    it('foo/bar, camelCase', function (): any {
      return assertEqual(
        compile(Symbol.for('foo/bar'), compilationEnvironment, {
          camelCase: true,
          language: 'JavaScript',
        }),
        'fooBar'
      );
    });
    it('foo-bar!, camelCase', function (): any {
      return assertEqual(
        compile(Symbol.for('foo-bar!'), compilationEnvironment, {
          camelCase: true,
          language: 'JavaScript',
        }),
        'fooBarX'
      );
    });
    it('foo-bar?, camelCase', function (): any {
      return assertEqual(
        compile(Symbol.for('foo-bar?'), compilationEnvironment, {
          camelCase: true,
          language: 'JavaScript',
        }),
        'fooBarP'
      );
    });
    it('*foo-bar*, camelCase', function (): any {
      return assertEqual(
        compile(Symbol.for('*foo-bar*'), compilationEnvironment, {
          camelCase: true,
          language: 'JavaScript',
        }),
        'starFooBarStar'
      );
    });
    it("'*foo-bar*, camelCase", function (): any {
      return assertEqual(
        compile(
          [Symbol.for('quote'), Symbol.for('*foo-bar*')],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        "Symbol.for('*foo-bar*')"
      );
    });
    it('A, camelCase', function (): any {
      return assertEqual(
        compile(Symbol.for('A'), compilationEnvironment, {
          camelCase: true,
          language: 'JavaScript',
        }),
        'A'
      );
    });
    xit('x, camelCase', function (): any {
      return assertEqual(
        compile(Symbol.for('x'), new LispEnvironment([['x', 1, 'variable']]), {
          language: 'JavaScript',
        }),
        '1'
      );
    });
    return it("(map symbol? '(a b c))", function (): any {
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
                Symbol.for('map'),
                Symbol.for('symbol?'),
                [
                  Symbol.for('quote'),
                  [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')],
                ],
              ],
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        "const lst = [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')].map(function (x) {\n" +
          "  return typeof x === 'symbol';\n" +
          '});'
      );
    });
  });
  describe('gensym', function (): any {
    it('(gensym "x")', function (): any {
      return assertEqual(
        compile([Symbol.for('gensym'), 'x'], compilationEnvironment, {
          language: 'JavaScript',
        }),
        "Symbol('x')"
      );
    });
    it('`(define ,(gensym "x") 1)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('define'), Symbol('x'), 1],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'const x = 1;'
      );
    });
    it('`(let ((x 0)) (define ,(gensym "x") 1))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('let'),
            [[Symbol.for('x'), 0]],
            [Symbol.for('define'), Symbol('x'), 1],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const x = 0;\n' + '\n' + 'const x1 = 1;'
      );
    });
    it('`(let ((x 0)) (define ,(gensym "x") 1) (let ((x1 0))))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('let'),
            [[Symbol.for('x'), 0]],
            [Symbol.for('define'), Symbol('x'), 1],
            [Symbol.for('let'), [[Symbol.for('x1'), 0]]],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const x = 0;\n' +
          '\n' +
          'const x1 = 1;\n' +
          '\n' +
          '{\n' +
          '  const x1 = 0;\n' +
          '}'
      );
    });
    return xit('`(let ((x 0)) ... (let ((x1 0)) ...))', function (): any {
      const gensymX: any = Symbol('x');
      return assertEqual(
        compile(
          [
            Symbol.for('let'),
            [[Symbol.for('x'), 0]],
            [Symbol.for('define'), gensymX, 1],
            [
              Symbol.for('let'),
              [[Symbol.for('x1'), 0]],
              [Symbol.for('define'), gensymX, 1],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const x = 0;\n' +
          '\n' +
          'const x2 = 1;\n' +
          '\n' +
          '{\n' +
          '  const x1 = 0;\n' +
          '  const x2 = 1;\n' +
          '}'
      );
    });
  });
  describe('global environment', function (): any {
    it('(define lst `(,symbol? ,boolean?))', function (): any {
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
          compilationEnvironment,
          {
            camelCase: true,
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
        'const [symbolp, booleanp] = (() => {\n' +
          '  function symbolp_(obj) {\n' +
          "    return typeof obj === 'symbol';\n" +
          '  }\n' +
          '  function booleanp_(obj) {\n' +
          "    return typeof obj === 'boolean';\n" +
          '  }\n' +
          '  return [symbolp_, booleanp_];\n' +
          '})();\n' +
          '\n' +
          'const lst = [symbolp, booleanp];'
      );
    });
    xit('(define __ (js-obj "dash" #t)), JS', function (): any {
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
                    [
                      Symbol.for('js-obj'),
                      '@@functional/placeholder',
                      Symbol.for('#t'),
                    ],
                  ],
                  [
                    Symbol.for('define'),
                    [
                      Symbol.for('js-regexp_'),
                      Symbol.for('input'),
                      [Symbol.for('flags'), Symbol.for('undefined')],
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
                    Symbol.for('js-regexp_'),
                  ],
                ],
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
            inlineFunctions: true,
            expressionType: 'statement',
          }
        ),
        'const [, regexp] = (() => {\n' +
          '  const __ = {\n' +
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
    it("(module m scheme ... (apply + '(1 1)) ...)", function (): any {
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
          compilationEnvironment,
          {
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
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
          'const onePlusOne = _add(...[1, 1]);'
      );
    });
    it("(module m scheme ... (apply - '(1 1)) ...)", function (): any {
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
          compilationEnvironment,
          {
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
        'const [_sub] = (() => {\n' +
          '  function sub_(...args) {\n' +
          '    const len = args.length;\n' +
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
          'const oneMinusOne = _sub(...[1, 1]);'
      );
    });
    it("(module m scheme ... (apply - '(1 1)) ...), inlineFunctions false", function (): any {
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
          compilationEnvironment,
          {
            inlineFunctions: false,
            language: 'JavaScript',
          }
        ),
        'import {\n' +
          '  _sub\n' +
          "} from 'roselisp';\n" +
          '\n' +
          'const oneMinusOne = _sub(...[1, 1]);'
      );
    });
    it("(module m scheme ... (apply * '(1 1)) ...)", function (): any {
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
          compilationEnvironment,
          {
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
        'const [_mul] = (() => {\n' +
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
          'const oneTimesOne = _mul(...[1, 1]);'
      );
    });
    it("(module m scheme ... (apply / '(1 1)) ...)", function (): any {
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
          compilationEnvironment,
          {
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
        'const [_div] = (() => {\n' +
          '  function div_(...args) {\n' +
          '    if (args.length === 1) {\n' +
          '      return 1 / args[0];\n' +
          '    } else {\n' +
          '      let result = args[0];\n' +
          '      const _end = args.length;\n' +
          '      for (let i = 1; i < _end; i++) {\n' +
          '        result = result / args[i];\n' +
          '      }\n' +
          '      return result;\n' +
          '    }\n' +
          '  }\n' +
          '  return [div_];\n' +
          '})();\n' +
          '\n' +
          'const oneDividedByOne = _div(...[1, 1]);'
      );
    });
    it('(module m scheme ... (apply string-append \'("foo" "bar")) ...)', function (): any {
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
          compilationEnvironment,
          {
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
        'const [stringAppend] = (() => {\n' +
          '  function stringAppend_(...args) {\n' +
          '    return args.reduce(function (acc, x) {\n' +
          '      return acc + x;\n' +
          "    }, '');\n" +
          '  }\n' +
          '  return [stringAppend_];\n' +
          '})();\n' +
          '\n' +
          "const fooBar = stringAppend(...['foo', 'bar']);"
      );
    });
    xit("(module m lisp ... (my-foldl + 0 '(1 2 3 4)) ...)", function (): any {
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
          compilationEnvironment,
          {
            camelCase: true,
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
        'const [add] = (function () {\n' +
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
          'const bar = myFoldl(add, 0, [1, 2, 3, 4]);'
      );
    });
    xit('(module m lisp (define (my-foldl ...) ...))', function (): any {
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
          compilationEnvironment,
          {
            camelCase: true,
            inlineFunctions: true,
            language: 'JavaScript',
          }
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
    it("(module m lisp ... (my-map first '((1) (2) (3))) ...)", function (): any {
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
          compilationEnvironment,
          {
            camelCase: true,
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
        'const [first] = (() => {\n' +
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
          'const bar = myMap(first, [[1], [2], [3]]);'
      );
    });
    xit('(module m lisp (define (my-push-4 ...) ...))', function (): any {
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
          compilationEnvironment,
          {
            camelCase: true,
            inlineFunctions: true,
            language: 'JavaScript',
          }
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
    xit('(module m lisp (define (my-push-5 ...) ...))', function (): any {
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
          compilationEnvironment,
          {
            camelCase: true,
            inlineFunctions: true,
            language: 'JavaScript',
          }
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
    it('(module m lisp (define (my-cdr ...) ...))', function (): any {
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
          compilationEnvironment,
          {
            camelCase: true,
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
        'const [cdr] = (() => {\n' +
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
    it('(module m lisp (define (my-intersection ...) ...))', function (): any {
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
          compilationEnvironment,
          {
            camelCase: true,
            inlineFunctions: true,
            language: 'JavaScript',
          }
        ),
        'const [intersection] = (() => {\n' +
          '  function intersection_(...args) {\n' +
          '    function intersection2(arr1, arr2) {\n' +
          '      const result = [];\n' +
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
    it("(module m scheme ... (apply + '(1 1)) ...), comment", function (): any {
      return assertEqual(
        compile(
          readRose(
            '(module m scheme\n' +
              '  ;;; Module header.\n' +
              '\n' +
              '  (define one-plus-one\n' +
              "    (apply + '(1 1))))"
          ),
          compilationEnvironment,
          {
            inlineFunctions: true,
            language: 'JavaScript',
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
          'const onePlusOne = _add(...[1, 1]);'
      );
    });
    return it("(module m scheme ... (apply + '(1 1)) ...), comments", function (): any {
      return assertEqual(
        compile(
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
            inlineFunctions: true,
            language: 'JavaScript',
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
          'const onePlusOne = _add(...[1, 1]);'
      );
    });
  });
  describe('strings', function (): any {
    it('""', function (): any {
      return assertEqual(
        compile('', compilationEnvironment, {
          language: 'JavaScript',
        }),
        "''"
      );
    });
    it('"foo"', function (): any {
      return assertEqual(
        compile('foo', compilationEnvironment, {
          language: 'JavaScript',
        }),
        "'foo'"
      );
    });
    it('"don\'t"', function (): any {
      return assertEqual(
        compile("don't", compilationEnvironment, {
          language: 'JavaScript',
        }),
        "'don\\'t'"
      );
    });
    it('"newline\\ntest" 1', function (): any {
      return assertEqual(
        compile('newline\n' + 'test', compilationEnvironment, {
          language: 'JavaScript',
        }),
        "'newline\\n' +\n" + "  'test'"
      );
    });
    it('"newline\\ntest" 2', function (): any {
      return assertEqual(
        compile('newline\n' + 'test', compilationEnvironment, {
          language: 'JavaScript',
        }),
        "'newline\\n' +\n" + "  'test'"
      );
    });
    it('"newline\\ntest" 3', function (): any {
      return assertEqual(
        compile('newline\n' + 'test\n' + 'three', compilationEnvironment, {
          language: 'JavaScript',
        }),
        "'newline\\n' +\n" + "  'test\\n' +\n" + "  'three'"
      );
    });
    xit('tab', function (): any {
      return assertEqual(
        compile('\\t', compilationEnvironment, {
          language: 'JavaScript',
        }),
        '	'
      );
    });
    it('"\\s" 1', function (): any {
      return assertEqual(
        compile('\\s', compilationEnvironment, {
          language: 'JavaScript',
        }),
        "'\\\\s'"
      );
    });
    return it('"\\s" 2', function (): any {
      return assertEqual(
        compile(sexp`"\\\\s"`, compilationEnvironment, {
          language: 'JavaScript',
        }),
        "'\\\\s'"
      );
    });
  });
  describe('function calls', function (): any {
    xit('(I x), JS function', function (): any {
      return assertEqual(
        compile(
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
            language: 'JavaScript',
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
        compile(
          [Symbol.for('truep'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '(() => {\n' +
          '  function truep(x) {\n' +
          '    return (x !== undefined) && x && !(Array.isArray(x) && (x.length === 0)) && !((Object.keys(x).length === 0) && (x.constructor === Object)) && true;\n' +
          '  }\n' +
          '  return truep;\n' +
          '})()(x)'
      );
    });
    return it('(falsep x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('falsep'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '(() => {\n' +
          '  function falsep(x) {\n' +
          '    return !truep(x);\n' +
          '  }\n' +
          '  function truep(x) {\n' +
          '    return (x !== undefined) && x && !(Array.isArray(x) && (x.length === 0)) && !((Object.keys(x).length === 0) && (x.constructor === Object)) && true;\n' +
          '  }\n' +
          '  return falsep;\n' +
          '})()(x)'
      );
    });
  });
  describe('()', function (): any {
    return it('()', function (): any {
      return assertEqual(
        compile([], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[]'
      );
    });
  });
  describe('list', function (): any {
    it('(list)', function (): any {
      return assertEqual(
        compile([Symbol.for('list')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[]'
      );
    });
    it('(list 1)', function (): any {
      return assertEqual(
        compile([Symbol.for('list'), 1], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[1]'
      );
    });
    return it('(list (list 1))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('list'), [Symbol.for('list'), 1]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[[1]]'
      );
    });
  });
  describe('member?', function (): any {
    it('(member? 2 (list 1 2 3 4) f)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('member?'),
            2,
            [Symbol.for('list'), 1, 2, 3, 4],
            Symbol.for('f'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[1, 2, 3, 4].findIndex(function (x) {\n' +
          '  return f(2, x);\n' +
          '}) >= 0'
      );
    });
    return it('(member? (+ 1 1) (list 1 2 3 4) f)', // `v` should be stored in a local variable. // TODO: Better compilation of this case:
    function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('member?'),
            [Symbol.for('+'), 1, 1],
            [Symbol.for('list'), 1, 2, 3, 4],
            Symbol.for('f'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[1, 2, 3, 4].findIndex(function (x) {\n' +
          '  return f(1 + 1, x);\n' +
          '}) >= 0'
      );
    });
  });
  describe('memq?', function (): any {
    it('(memq? 2 (list 1 2 3 4))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('memq?'), 2, [Symbol.for('list'), 1, 2, 3, 4]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[1, 2, 3, 4].includes(2)'
      );
    });
    return it('(memq? (+ 1 1) (list 1 2 3 4))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('memq?'),
            [Symbol.for('+'), 1, 1],
            [Symbol.for('list'), 1, 2, 3, 4],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[1, 2, 3, 4].includes(1 + 1)'
      );
    });
  });
  describe('append', function (): any {
    it('(append)', function (): any {
      return assertEqual(
        compile([Symbol.for('append')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[]'
      );
    });
    it('(append foo)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('append'), Symbol.for('foo')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[...foo]'
      );
    });
    it('(append foo bar)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('append'), Symbol.for('foo'), Symbol.for('bar')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[...foo, ...bar]'
      );
    });
    it('(append (list))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('append'), [Symbol.for('list')]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[]'
      );
    });
    it('(append (list x))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('append'), [Symbol.for('list'), Symbol.for('x')]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[x]'
      );
    });
    return it('(append \'("foo") \'("bar"))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('append'),
            [Symbol.for('quote'), ['foo']],
            [Symbol.for('quote'), ['bar']],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "['foo', 'bar']"
      );
    });
  });
  describe('quote', function (): any {
    it("'x", function (): any {
      return assertEqual(
        compile(
          [Symbol.for('quote'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "Symbol.for('x')"
      );
    });
    it("'()", function (): any {
      return assertEqual(
        compile([Symbol.for('quote'), []], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[]'
      );
    });
    it("'(1)", function (): any {
      return assertEqual(
        compile([Symbol.for('quote'), [1]], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[1]'
      );
    });
    it("'(1 . 2)", function (): any {
      return assertEqual(
        compile(
          [Symbol.for('quote'), [1, Symbol.for('.'), 2]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[1, Symbol.for('.'), 2]"
      );
    });
    it("'((1))", function (): any {
      return assertEqual(
        compile([Symbol.for('quote'), [[1]]], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[[1]]'
      );
    });
    return it("'(x y z)", function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quote'),
            [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]"
      );
    });
  });
  describe('quasiquote', function (): any {
    it('`x', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('quasiquote'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "Symbol.for('x')"
      );
    });
    it('`()', function (): any {
      return assertEqual(
        compile([Symbol.for('quasiquote'), []], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[]'
      );
    });
    it('`(1)', function (): any {
      return assertEqual(
        compile([Symbol.for('quasiquote'), [1]], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[1]'
      );
    });
    it('`(1 . 2)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('quasiquote'), [1, Symbol.for('.'), 2]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[1, Symbol.for('.'), 2]"
      );
    });
    it('`((1 . 2))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('quasiquote'), [[1, Symbol.for('.'), 2]]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[[1, Symbol.for('.'), 2]]"
      );
    });
    it('`((1 . ,2))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [[1, Symbol.for('.'), [Symbol.for('unquote'), 2]]],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[[1, Symbol.for('.'), 2]]"
      );
    });
    it('`((1 . ,2) (3 . ,4))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [
              [1, Symbol.for('.'), [Symbol.for('unquote'), 2]],
              [3, Symbol.for('.'), [Symbol.for('unquote'), 4]],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[[1, Symbol.for('.'), 2], [3, Symbol.for('.'), 4]]"
      );
    });
    it('(define test-map-1 ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "const testMap1 = [['foo', Symbol.for('.'), testFn], ['bar', Symbol.for('.'), testFn]];"
      );
    });
    it('`((1))', function (): any {
      return assertEqual(
        compile([Symbol.for('quasiquote'), [[1]]], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '[[1]]'
      );
    });
    it('`(x y z)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]"
      );
    });
    it('`(x y ,z)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [
              Symbol.for('x'),
              Symbol.for('y'),
              [Symbol.for('unquote'), Symbol.for('z')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[Symbol.for('x'), Symbol.for('y'), z]"
      );
    });
    it('`(x y ,@z)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [
              Symbol.for('x'),
              Symbol.for('y'),
              [Symbol.for('unquote-splicing'), Symbol.for('z')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[Symbol.for('x'), Symbol.for('y'), ...z]"
      );
    });
    it('`(x y `z)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [
              Symbol.for('x'),
              Symbol.for('y'),
              [Symbol.for('quasiquote'), Symbol.for('z')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), Symbol.for('z')]]"
      );
    });
    it('`(x y `(z))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [
              Symbol.for('x'),
              Symbol.for('y'),
              [Symbol.for('quasiquote'), [Symbol.for('z')]],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [Symbol.for('z')]]]"
      );
    });
    it('`(x y `(,z))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [
              Symbol.for('x'),
              Symbol.for('y'),
              [
                Symbol.for('quasiquote'),
                [[Symbol.for('unquote'), Symbol.for('z')]],
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('z')]]]]"
      );
    });
    it('`(x y `(,@z))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [
              Symbol.for('x'),
              Symbol.for('y'),
              [
                Symbol.for('quasiquote'),
                [[Symbol.for('unquote-splicing'), Symbol.for('z')]],
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('z')]]]]"
      );
    });
    it('`(,@x)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [[Symbol.for('unquote-splicing'), Symbol.for('x')]],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[...x]'
      );
    });
    it('`(,@x ,@y)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('quasiquote'),
            [
              [Symbol.for('unquote-splicing'), Symbol.for('x')],
              [Symbol.for('unquote-splicing'), Symbol.for('y')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[...x, ...y]'
      );
    });
    return it('(set! let-exp `(let ((,arg-list (quote ,args))) ,@body))', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        "letExp = [Symbol.for('let'), [[argList, [Symbol.for('quote'), args]]], ...body];"
      );
    });
  });
  describe('apply', function (): any {
    it('(apply f args)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'f(...args)'
      );
    });
    it('(apply f x args)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('apply'),
            Symbol.for('f'),
            Symbol.for('x'),
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'f(x, ...args)'
      );
    });
    it('(apply new Foo args)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('apply'),
            Symbol.for('new'),
            Symbol.for('Foo'),
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'new Foo(...args)'
      );
    });
    it('(apply new Foo x y args)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('apply'),
            Symbol.for('new'),
            Symbol.for('Foo'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'new Foo(x, y, ...args)'
      );
    });
    xit('(apply send obj method args)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('apply'),
            Symbol.for('send'),
            Symbol.for('obj'),
            Symbol.for('method'),
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'obj.method(...args)'
      );
    });
    it('(apply (get-field method obj) args)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('apply'),
            [Symbol.for('get-field'), Symbol.for('method'), Symbol.for('obj')],
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'obj.method(...args)'
      );
    });
    return it('(apply (.-method obj) args)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('apply'),
            [Symbol.for('.-method'), Symbol.for('obj')],
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'obj.method(...args)'
      );
    });
  });
  describe('define', function (): any {
    it('(define x), JS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('define'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'let x;'
      );
    });
    it('(define x), TS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('define'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'let x: any;'
      );
    });
    it('(define x 1), JS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('define'), Symbol.for('x'), 1],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'const x = 1;'
      );
    });
    it('(define x 1), TS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('define'), Symbol.for('x'), 1],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'const x: any = 1;'
      );
    });
    xit('(define I (lambda (x) x))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            Symbol.for('I'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function I(x) {\n' + '  return x;\n' + '}'
      );
    });
    it('(define I (memoize (lambda (x) x)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            Symbol.for('I'),
            [
              Symbol.for('memoize'),
              [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const I = memoize(function (x) {\n' + '  return x;\n' + '});'
      );
    });
    it('(define (identity-function x) x), camelCase', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [Symbol.for('identity-function'), Symbol.for('x')],
            Symbol.for('x'),
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function identityFunction(x) {\n' + '  return x;\n' + '}'
      );
    });
    it('(define (I x) x)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [Symbol.for('I'), Symbol.for('x')],
            Symbol.for('x'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function I(x) {\n' + '  return x;\n' + '}'
      );
    });
    it('(define (K x y) x)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [Symbol.for('K'), Symbol.for('x'), Symbol.for('y')],
            Symbol.for('x'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function K(x, y) {\n' + '  return x;\n' + '}'
      );
    });
    it('(define (S f g x) (f x (g x)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [
              Symbol.for('S'),
              Symbol.for('f'),
              Symbol.for('g'),
              Symbol.for('x'),
            ],
            [
              Symbol.for('f'),
              Symbol.for('x'),
              [Symbol.for('g'), Symbol.for('x')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function S(f, g, x) {\n' + '  return f(x, g(x));\n' + '}'
      );
    });
    it('(define (S f g x) ((f x) (g x)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [
              Symbol.for('S'),
              Symbol.for('f'),
              Symbol.for('g'),
              Symbol.for('x'),
            ],
            [
              [Symbol.for('f'), Symbol.for('x')],
              [Symbol.for('g'), Symbol.for('x')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function S(f, g, x) {\n' + '  return f(x)(g(x));\n' + '}'
      );
    });
    it('(define (C f x y) (f y x))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [
              Symbol.for('C'),
              Symbol.for('f'),
              Symbol.for('x'),
              Symbol.for('y'),
            ],
            [Symbol.for('f'), Symbol.for('y'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function C(f, x, y) {\n' + '  return f(y, x);\n' + '}'
      );
    });
    it('(define (U f) (f f))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [Symbol.for('U'), Symbol.for('f')],
            [Symbol.for('f'), Symbol.for('f')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function U(f) {\n' + '  return f(f);\n' + '}'
      );
    });
    it('(define (A f . args) (apply f args)), JS', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function A(f, ...args) {\n' + '  return f(...args);\n' + '}'
      );
    });
    it('(define (A f . args) (apply f args)), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'function A(f: any, ...args: any[]): any {\n' +
          '  return f(...args);\n' +
          '}'
      );
    });
    it('(define (Q . args) ...)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [Symbol.for('Q'), Symbol.for('.'), Symbol.for('args')],
            [
              Symbol.for('cond'),
              [
                [
                  Symbol.for('='),
                  [Symbol.for('.-length'), Symbol.for('args')],
                  0,
                ],
                Symbol.for('undefined'),
              ],
              [
                [
                  Symbol.for('='),
                  [Symbol.for('.-length'), Symbol.for('args')],
                  1,
                ],
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function Q(...args) {\n' +
          '  if (args.length === 0) {\n' +
          '    return undefined;\n' +
          '  } else if (args.length === 1) {\n' +
          '    return args[0];\n' +
          '  } else {\n' +
          '    const fs = args.slice(0, -1);\n' +
          '    const x = args[args.length - 1];\n' +
          '    return fs.reduce(function (acc, f) {\n' +
          '      return f(acc);\n' +
          '    }, x);\n' +
          '  }\n' +
          '}'
      );
    });
    it('(define (T . args) ...)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [Symbol.for('T'), Symbol.for('.'), Symbol.for('args')],
            [
              Symbol.for('cond'),
              [
                [
                  Symbol.for('='),
                  [Symbol.for('.-length'), Symbol.for('args')],
                  0,
                ],
                Symbol.for('undefined'),
              ],
              [
                [
                  Symbol.for('='),
                  [Symbol.for('.-length'), Symbol.for('args')],
                  1,
                ],
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function T(...args) {\n' +
          '  if (args.length === 0) {\n' +
          '    return undefined;\n' +
          '  } else if (args.length === 1) {\n' +
          '    return args[0];\n' +
          '  } else {\n' +
          '    const [x, ...fs] = args;\n' +
          '    return fs.reduce(function (acc, f) {\n' +
          '      return f(acc);\n' +
          '    }, x);\n' +
          '  }\n' +
          '}'
      );
    });
    it('(define (Y f) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(define (compose f g) (lambda (x) (f (g x))))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [Symbol.for('compose'), Symbol.for('f'), Symbol.for('g')],
            [
              Symbol.for('lambda'),
              [Symbol.for('x')],
              [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')]],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function compose(f, g) {\n' +
          '  return function (x) {\n' +
          '    return f(g(x));\n' +
          '  };\n' +
          '}'
      );
    });
    it('(define (foo) (set! x (+ x 1)) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function foo() {\n' + '  x++;\n' + '  return ++y;\n' + '}'
      );
    });
    it('(define (mapGet map path) ...), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [Symbol.for('mapGet'), Symbol.for('map'), Symbol.for('path')],
            [
              Symbol.for('let-values'),
              [
                [
                  [Symbol.for('value')],
                  [
                    Symbol.for('mapGet2'),
                    Symbol.for('map'),
                    Symbol.for('path'),
                  ],
                ],
              ],
              Symbol.for('value'),
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function mapGet(map, path) {\n' +
          '  const [value] = mapGet2(map, path);\n' +
          '  return value;\n' +
          '}'
      );
    });
    it('(define _ (js-obj "dash" #t)), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            Symbol.for('_'),
            [Symbol.for('js-obj'), 'dash', Symbol.for('#t')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'const _ = {\n' + '  dash: true\n' + '};'
      );
    });
    it('(define __ (js-obj "dash" #t)), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            Symbol.for('__'),
            [Symbol.for('js-obj'), 'dash', Symbol.for('#t')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'const __ = {\n' + '  dash: true\n' + '};'
      );
    });
    xit('(lambda (env (options (js-obj))) ...), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [
              Symbol.for('env'),
              [Symbol.for('options'), [Symbol.for('js-obj')]],
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
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
    return it('(define (add-matrix ...) ...), JS', function (): any {
      return assertEqual(
        compile(
          [
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
                    [
                      Symbol.for('make-matrix'),
                      Symbol.for('l1'),
                      Symbol.for('l2'),
                    ],
                  ],
                ],
                [
                  Symbol.for('for'),
                  [
                    [
                      Symbol.for('i'),
                      [Symbol.for('range'), 0, Symbol.for('l1')],
                    ],
                  ],
                  [
                    Symbol.for('for'),
                    [
                      [
                        Symbol.for('j'),
                        [Symbol.for('range'), 0, Symbol.for('l2')],
                      ],
                    ],
                    [
                      Symbol.for('set!'),
                      [
                        Symbol.for('aget'),
                        [
                          Symbol.for('aget'),
                          Symbol.for('matrix'),
                          Symbol.for('j'),
                        ],
                        Symbol.for('i'),
                      ],
                      [
                        Symbol.for('+'),
                        [
                          Symbol.for('aget'),
                          [
                            Symbol.for('aget'),
                            Symbol.for('m1'),
                            Symbol.for('j'),
                          ],
                          Symbol.for('i'),
                        ],
                        [
                          Symbol.for('aget'),
                          [
                            Symbol.for('aget'),
                            Symbol.for('m2'),
                            Symbol.for('j'),
                          ],
                          Symbol.for('i'),
                        ],
                      ],
                    ],
                  ],
                ],
                Symbol.for('matrix'),
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function addMatrix(m1, m2) {\n' +
          '  const l1 = m1.length;\n' +
          '  const l2 = m2.length;\n' +
          '  const matrix = makeMatrix(l1, l2);\n' +
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
  describe('funcall', function (): any {
    it('(funcall f x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('funcall'), Symbol.for('f'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'f(x)'
      );
    });
    return it('(funcall f x y)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('funcall'),
            Symbol.for('f'),
            Symbol.for('x'),
            Symbol.for('y'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'f(x, y)'
      );
    });
  });
  describe('lambda', function (): any {
    it('(lambda (x) x), JS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function (x) {\n' + '  return x;\n' + '}'
      );
    });
    it('(lambda (x) x), TS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'function (x: any): any {\n' + '  return x;\n' + '}'
      );
    });
    it('(lambda args args)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('lambda'), Symbol.for('args'), Symbol.for('args')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function (...args) {\n' + '  return args;\n' + '}'
      );
    });
    it('(lambda (x . args) args)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [Symbol.for('x'), Symbol.for('.'), Symbol.for('args')],
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function (x, ...args) {\n' + '  return args;\n' + '}'
      );
    });
    it('(lambda (x y . args) args)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [
              Symbol.for('x'),
              Symbol.for('y'),
              Symbol.for('.'),
              Symbol.for('args'),
            ],
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function (x, y, ...args) {\n' + '  return args;\n' + '}'
      );
    });
    it('(lambda (x) (let ((x 1)) x))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [Symbol.for('x')],
            [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function (x) {\n' +
          '  {\n' +
          '    const x = 1;\n' +
          '    return x;\n' +
          '  }\n' +
          '}'
      );
    });
    it('(lambda (x) (let ((y 1)) y))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [Symbol.for('x')],
            [Symbol.for('let'), [[Symbol.for('y'), 1]], Symbol.for('y')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function (x) {\n' + '  const y = 1;\n' + '  return y;\n' + '}'
      );
    });
    it('(lambda (given (surname "Smith")) ...), JS', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "function (given, surname = 'Smith') {\n" +
          "  return 'Hello, ' + given + ' ' + surname;\n" +
          '}'
      );
    });
    it('(lambda (given (surname "Smith")) ...), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        "function (given: any, surname: any = 'Smith'): any {\n" +
          "  return 'Hello, ' + given + ' ' + surname;\n" +
          '}'
      );
    });
    it('(lambda (arg (options (js-obj))) ...), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [
              Symbol.for('arg'),
              [Symbol.for('options'), [Symbol.for('js-obj')]],
            ],
            Symbol.for('arg'),
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'function (arg: any, options: any = {}): any {\n' +
          '  return arg;\n' +
          '}'
      );
    });
    xit('(lambda (this arg) args), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [Symbol.for('this'), Symbol.for('arg')],
            Symbol.for('arg'),
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'function (arg: any): any {\n' + '  return arg;\n' + '}'
      );
    });
    xit('(lambda (this . args) args), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')],
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function (...args: any[]): any {\n' + '  return args;\n' + '}'
      );
    });
    xit('(lambda (this arg) args), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [Symbol.for('this'), Symbol.for('arg')],
            Symbol.for('arg'),
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'function (this: any, arg: any): any {\n' + '  return arg;\n' + '}'
      );
    });
    return xit('(lambda (this . args) args), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('lambda'),
            [Symbol.for('this'), Symbol.for('.'), Symbol.for('args')],
            Symbol.for('args'),
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'function (this: any, ...args: any[]): any {\n' +
          '  return args;\n' +
          '}'
      );
    });
  });
  describe('let', function (): any {
    it('(let (x)), statement, JS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('let'), [Symbol.for('x')]],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'let x;'
      );
    });
    it('(let (x) x), statement, JS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('let'), [Symbol.for('x')], Symbol.for('x')],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'let x;\n' + '\n' + 'return x;'
      );
    });
    it('(let (x) x), expression, JS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('let'), [Symbol.for('x')], Symbol.for('x')],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        '(() => {\n' + '  let x;\n' + '  return x;\n' + '})()'
      );
    });
    it('(let (x) x), statement, TS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('let'), [Symbol.for('x')], Symbol.for('x')],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'TypeScript',
          }
        ),
        'let x: any;\n' + '\n' + 'return x;'
      );
    });
    it('(let ((x 1)) x), JS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'const x = 1;\n' + '\n' + 'return x;'
      );
    });
    it('(let ((x 1)) x), TS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'TypeScript',
          }
        ),
        'const x: any = 1;\n' + '\n' + 'return x;'
      );
    });
    xit('(let ((a 1)) (+ (let ((a 2)) a) a))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('let'),
            [[Symbol.for('a'), 1]],
            [
              Symbol.for('+'),
              [Symbol.for('let'), [[Symbol.for('a'), 2]], Symbol.for('a')],
              Symbol.for('a'),
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const a = 1;\n' +
          '\n' +
          '(() => {\n' +
          '  const a = 2;\n' +
          '  return a;\n' +
          '})() + a;'
      );
    });
    it('(let ((compose ...) ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
                [
                  Symbol.for('compose'),
                  Symbol.for('square'),
                  Symbol.for('add1'),
                ],
                [Symbol.for('add1'), 4],
              ],
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const compose = function (f, g) {\n' +
          '  return function (x) {\n' +
          '    return f(g(x));\n' +
          '  };\n' +
          '};\n' +
          '\n' +
          'const square = function (x) {\n' +
          '  return x * x;\n' +
          '};\n' +
          '\n' +
          'const add1 = function (x) {\n' +
          '  return x + 1;\n' +
          '};\n' +
          '\n' +
          'console.log(compose(square, add1)(add1(4)));'
      );
    });
    it('(let ((and ...)) (and x y))', function (): any {
      return assertEqual(
        compile(
          [
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
                    [
                      Symbol.for('if'),
                      Symbol.for('y'),
                      Symbol.for('#t'),
                      Symbol.for('#f'),
                    ],
                    Symbol.for('#f'),
                  ],
                ],
              ],
            ],
            [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const and = function (x, y) {\n' +
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
    // FIXME: This test is incorrect.
    // See the one below.
    xit('(begin x (let ((x 1)) x)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            Symbol.for('x'),
            [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'TypeScript',
          }
        ),
        'x;\n' + '\n' + 'const x: any = 1;\n' + '\n' + 'return x;'
      );
    });
    // FIXME: Make this test pass.
    xit('(begin x (let ((x 1)) x)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            Symbol.for('x'),
            [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'TypeScript',
          }
        ),
        'x;\n' + '\n' + '{\n' + '  const x: any = 1;\n' + '  x;\n' + '}'
      );
    });
    it('(cond ... (else x (let ...))), TS', function (): any {
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
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'TypeScript',
          }
        ),
        'if (foo) {\n' +
          '  return bar;\n' +
          '} else {\n' +
          '  x;\n' +
          '  const x: any = 1;\n' +
          '  return x;\n' +
          '}'
      );
    });
    it('(define make-compilation-evaluator ...), TS', function (): any {
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
                  [Symbol.for('options'), [Symbol.for('js-obj')]],
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'const makeCompilationEvaluator: any = memoize(function (env: any, options: any = {}): any {\n' +
          "  let language: any = options['language'];\n" +
          '  language = language || defaultLanguage;\n' +
          '  const compilationEnv: any = compilationMap.get(language) || javascriptEnv;\n' +
          '  return new CompilationEvaluator(env, compilationEnv, options);\n' +
          '});'
      );
    });
    return it('(define make-compilation-evaluator ...), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('cond'),
            [
              Symbol.for('foo'),
              [
                Symbol.for('let'),
                [[Symbol.for('x'), Symbol.for('#t')]],
                Symbol.for('x'),
              ],
            ],
            [Symbol.for('else'), Symbol.for('#f')],
          ],
          compilationEnvironment,
          {
            expressionType: 'return',
          }
        ),
        'if (foo) {\n' +
          '  const x = true;\n' +
          '  return x;\n' +
          '} else {\n' +
          '  return false;\n' +
          '}'
      );
    });
  });
  describe('let-values', function (): any {
    it('(let-values ((value (foo bar baz))) value), JS', function (): any {
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
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'const value = foo(bar, baz);\n' + '\n' + 'return value;'
      );
    });
    it('(let-values (((value) (foo bar baz))) value), JS', function (): any {
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
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'const [value] = foo(bar, baz);\n' + '\n' + 'return value;'
      );
    });
    it('(let-values (((value) (foo bar baz))) value), TS', function (): any {
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
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'TypeScript',
          }
        ),
        'const [value]: any[] = foo(bar, baz);\n' + '\n' + 'return value;'
      );
    });
    it('(let-values (((x . fs) args)) ...), JS', function (): any {
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
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const [x, ...fs] = args;\n' +
          '\n' +
          'fs.reduce(function (acc, f) {\n' +
          '  return f(acc);\n' +
          '}, x);'
      );
    });
    it('(let-values (((x . fs) args)) ...), TS', function (): any {
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
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'TypeScript',
          }
        ),
        'const [x, ...fs]: any[] = args;\n' +
          '\n' +
          'fs.reduce(function (acc: any, f: any): any {\n' +
          '  return f(acc);\n' +
          '}, x);'
      );
    });
    it('(let-values (((value1) ...) ((value2) ...)) ...)', function (): any {
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
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'const [value1] = foo(bar);\n' +
          '\n' +
          'const [value2] = bar(baz);\n' +
          '\n' +
          'return [value1, value2];'
      );
    });
    return it('(begin ... (let-values ...)), JS', function (): any {
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
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'value;\n' +
          '\n' +
          'const value = foo(bar, baz);\n' +
          '\n' +
          'return value;'
      );
    });
  });
  describe('define-values', function (): any {
    it('(define-values value (foo bar baz)), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-values'),
            Symbol.for('value'),
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const value = foo(bar, baz);'
      );
    });
    it('(define-values (value) (foo bar baz)), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-values'),
            [Symbol.for('value')],
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const [value] = foo(bar, baz);'
      );
    });
    it('(define-values (value) (foo bar baz)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-values'),
            [Symbol.for('value')],
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'TypeScript',
          }
        ),
        'const [value]: any[] = foo(bar, baz);'
      );
    });
    it('(define-values (_ _ value) (foo bar baz)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-values'),
            [Symbol.for('_'), Symbol.for('_'), Symbol.for('value')],
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'TypeScript',
          }
        ),
        'const [, , value]: any[] = foo(bar, baz);'
      );
    });
    return it('(define-values (_ __ value) :hole-marker __ (foo bar baz)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-values'),
            [Symbol.for('_'), Symbol.for('__'), Symbol.for('value')],
            Symbol.for(':hole-marker'),
            Symbol.for('__'),
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'TypeScript',
          }
        ),
        'const [_, , value]: any[] = foo(bar, baz);'
      );
    });
  });
  describe('set!-values', function (): any {
    it('(set!-values (value) (foo bar baz)), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('set!-values'),
            [Symbol.for('value')],
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        '[value] = foo(bar, baz);'
      );
    });
    it('(set!-values (_ value) (foo bar baz)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('set!-values'),
            [Symbol.for('_'), Symbol.for('value')],
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        '[, value] = foo(bar, baz);'
      );
    });
    return it('(set!-values (_ __ value) :hole-marker __ (foo bar baz)), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('set!-values'),
            [Symbol.for('_'), Symbol.for('__'), Symbol.for('value')],
            Symbol.for(':hole-marker'),
            Symbol.for('__'),
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        '[_, , value] = foo(bar, baz);'
      );
    });
  });
  describe('let-js-obj', function (): any {
    return it('(let-js-obj (((prop) obj)) prop), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('let-js-obj'),
            [[[Symbol.for('prop')], Symbol.for('obj')]],
            Symbol.for('prop'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const {prop} = obj;\n' + '\n' + 'prop;'
      );
    });
  });
  describe('define-js-obj', function (): any {
    it('(define-js-obj (prop) obj), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-js-obj'),
            [Symbol.for('prop')],
            Symbol.for('obj'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const {prop} = obj;'
      );
    });
    it('(define-js-obj (prop) obj), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-js-obj'),
            [Symbol.for('prop')],
            Symbol.for('obj'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'TypeScript',
          }
        ),
        'const {prop} = obj;'
      );
    });
    it('(define-js-obj ((x y) z) obj), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-js-obj'),
            [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')],
            Symbol.for('obj'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const {x: y, z} = obj;'
      );
    });
    return it('(define-js-obj ((x y) z) obj), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-js-obj'),
            [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')],
            Symbol.for('obj'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'TypeScript',
          }
        ),
        'const {x: y, z} = obj;'
      );
    });
  });
  describe('set!-js-obj', function (): any {
    return it('(set!-js-obj (prop) obj), JS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('set!-js-obj'), [Symbol.for('prop')], Symbol.for('obj')],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        '({prop} = obj);'
      );
    });
  });
  describe('set!', function (): any {
    it('(set! x 1)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('set!'), Symbol.for('x'), 1],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'x = 1;'
      );
    });
    it('(set! x (add1 x)), expression', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('set!'),
            Symbol.for('x'),
            [Symbol.for('add1'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        '++x'
      );
    });
    it('(set! x (sub1 x)), expression', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('set!'),
            Symbol.for('x'),
            [Symbol.for('sub1'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        '--x'
      );
    });
    it('(set! x (+ x 1)), expression', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('set!'),
            Symbol.for('x'),
            [Symbol.for('+'), Symbol.for('x'), 1],
          ],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        '++x'
      );
    });
    it('(set! x (+ x 1)), statement', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('set!'),
            Symbol.for('x'),
            [Symbol.for('+'), Symbol.for('x'), 1],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'x++;'
      );
    });
    return it('(set! x (+ x 1)), return statement', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('set!'),
            Symbol.for('x'),
            [Symbol.for('+'), Symbol.for('x'), 1],
          ],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'return ++x;'
      );
    });
  });
  describe('setq', function (): any {
    return it('(setq x 1)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('setq'), Symbol.for('x'), 1],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'x = 1;'
      );
    });
  });
  describe('+', function (): any {
    it('(+ x 1)', function (): any {
      return assertEqual(
        compile([Symbol.for('+'), Symbol.for('x'), 1], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'x + 1'
      );
    });
    return it('(+ x 1 2)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('+'), Symbol.for('x'), 1, 2],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x + 1 + 2'
      );
    });
  });
  describe('-', function (): any {
    it('(- x)', function (): any {
      return assertEqual(
        compile([Symbol.for('-'), Symbol.for('x')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '-x'
      );
    });
    xit('(- (- x))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('-'), [Symbol.for('-'), Symbol.for('x')]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x'
      );
    });
    it('(- x 1)', function (): any {
      return assertEqual(
        compile([Symbol.for('-'), Symbol.for('x'), 1], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'x - 1'
      );
    });
    return it('(- x 1 2)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('-'), Symbol.for('x'), 1, 2],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x - 1 - 2'
      );
    });
  });
  describe('mod', function (): any {
    return it('(mod x y)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('mod'), Symbol.for('x'), Symbol.for('y')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x % y'
      );
    });
  });
  describe('begin', function (): any {
    it('(begin x y z), statement', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'x;\n' + '\n' + 'y;\n' + '\n' + 'z;'
      );
    });
    it('(begin x (begin y z)), statement', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            Symbol.for('x'),
            [Symbol.for('begin'), Symbol.for('y'), Symbol.for('z')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'x;\n' + '\n' + 'y;\n' + '\n' + 'z;'
      );
    });
    it('(begin x y z), expression', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        '(() => {\n' + '  x;\n' + '  y;\n' + '  return z;\n' + '})()'
      );
    });
    it('redefine core functions', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
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
    return it('import core functions', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('scheme'),
            [
              Symbol.for('require'),
              [
                Symbol.for('only-in'),
                'foo',
                Symbol.for('and'),
                Symbol.for('or'),
              ],
            ],
            [
              Symbol.for('and'),
              Symbol.for('x'),
              [Symbol.for('or'), Symbol.for('y'), Symbol.for('z')],
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'import {\n' +
          '  and,\n' +
          '  or\n' +
          "} from 'foo';\n" +
          '\n' +
          'and(x, or(y, z));'
      );
    });
  });
  describe('provide', function (): any {
    it('(provide)', function (): any {
      return assertEqual(
        compile([Symbol.for('provide')], compilationEnvironment, {
          expressionType: 'statement',
          language: 'JavaScript',
        }),
        ''
      );
    });
    it('(provide x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('provide'), Symbol.for('x')],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'export {\n' + '  x\n' + '};'
      );
    });
    it('(provide x y)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('provide'), Symbol.for('x'), Symbol.for('y')],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'export {\n' + '  x,\n' + '  y\n' + '};'
      );
    });
    it('(provide (rename-out (x y)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('provide'),
            [Symbol.for('rename-out'), [Symbol.for('x'), Symbol.for('y')]],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'export {\n' + '  x as y\n' + '};'
      );
    });
    it('(provide (rename-out (x y) (w z)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('provide'),
            [
              Symbol.for('rename-out'),
              [Symbol.for('x'), Symbol.for('y')],
              [Symbol.for('w'), Symbol.for('z')],
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'export {\n' + '  x as y,\n' + '  w as z\n' + '};'
      );
    });
    it('(provide x (rename-out (y z)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('provide'),
            Symbol.for('x'),
            [Symbol.for('rename-out'), [Symbol.for('y'), Symbol.for('z')]],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'export {\n' + '  x,\n' + '  y as z\n' + '};'
      );
    });
    it('(provide x x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('provide'), Symbol.for('x'), Symbol.for('x')],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'export {\n' + '  x\n' + '};'
      );
    });
    it('(provide x (rename-out (y x)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('provide'),
            Symbol.for('x'),
            [Symbol.for('rename-out'), [Symbol.for('y'), Symbol.for('x')]],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'export {\n' + '  x\n' + '};'
      );
    });
    it('(provide (rename-out (y x)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('provide'),
            [
              Symbol.for('rename-out'),
              [Symbol.for('x'), Symbol.for('js/undefined')],
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'export {\n' + '  x as jsUndefined\n' + '};'
      );
    });
    it('(provide (all-from-out "foo"))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('provide'), [Symbol.for('all-from-out'), 'foo']],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        "export * from 'foo';"
      );
    });
    return it('(provide (all-from-out "foo") bar)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('provide'),
            [Symbol.for('all-from-out'), 'foo'],
            Symbol.for('bar'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        "export * from 'foo';\n" + '\n' + 'export {\n' + '  bar\n' + '};'
      );
    });
  });
  describe('require', function (): any {
    it('(require "foo")', function (): any {
      return assertEqual(
        compile([Symbol.for('require'), 'foo'], compilationEnvironment, {
          expressionType: 'statement',
          language: 'JavaScript',
          esModuleInterop: false,
        }),
        "import * as foo from 'foo';"
      );
    });
    it('(require "foo"), esModuleInterop', function (): any {
      return assertEqual(
        compile([Symbol.for('require'), 'foo'], compilationEnvironment, {
          expressionType: 'statement',
          language: 'JavaScript',
          esModuleInterop: true,
        }),
        "import foo from 'foo';"
      );
    });
    it('(require foo "bar")', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('require'), Symbol.for('foo'), 'bar'],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
            esModuleInterop: false,
          }
        ),
        "import * as foo from 'bar';"
      );
    });
    it('(require foo "bar"), esModuleInterop', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('require'), Symbol.for('foo'), 'bar'],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
            esModuleInterop: true,
          }
        ),
        "import foo from 'bar';"
      );
    });
    it('(require "foo" "bar"), esModuleInterop', function (): any {
      return assertEqual(
        compile([Symbol.for('require'), 'foo', 'bar'], compilationEnvironment, {
          expressionType: 'statement',
          language: 'JavaScript',
          esModuleInterop: true,
        }),
        "import foo from 'bar';"
      );
    });
    it('(require (only-in foo bar))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('require'),
            [Symbol.for('only-in'), Symbol.for('foo'), Symbol.for('bar')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'import {\n' + '  bar\n' + "} from 'foo';"
      );
    });
    it('(require (only-in foo (bar baz)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('require'),
            [
              Symbol.for('only-in'),
              Symbol.for('foo'),
              [Symbol.for('bar'), Symbol.for('baz')],
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'import {\n' + '  bar as baz\n' + "} from 'foo';"
      );
    });
    it('(require (only-in "foo" (bar baz)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('require'),
            [
              Symbol.for('only-in'),
              'foo',
              [Symbol.for('bar'), Symbol.for('baz')],
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'import {\n' + '  bar as baz\n' + "} from 'foo';"
      );
    });
    it('(require (only-in foo bar bar))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('require'),
            [
              Symbol.for('only-in'),
              Symbol.for('foo'),
              Symbol.for('bar'),
              Symbol.for('bar'),
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'import {\n' + '  bar\n' + "} from 'foo';"
      );
    });
    it('(require (only-in foo bar (baz bar)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('require'),
            [
              Symbol.for('only-in'),
              Symbol.for('foo'),
              Symbol.for('bar'),
              [Symbol.for('baz'), Symbol.for('bar')],
            ],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'import {\n' + '  bar\n' + "} from 'foo';"
      );
    });
    xit('(require \'foo "bar")', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('require'),
            [Symbol.for('quote'), Symbol.for('foo')],
            'bar',
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        "import foo from 'bar';"
      );
    });
    xit('(require foo :as bar)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('require'),
            Symbol.for('foo'),
            Symbol.for(':as'),
            Symbol.for('bar'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        "import bar from 'foo';"
      );
    });
    xit('(require (foo :as bar))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('require'),
            [Symbol.for('foo'), Symbol.for(':as'), Symbol.for('bar')],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        "import bar from 'foo';"
      );
    });
    return xit('(require ("foo" :as "bar"))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('require'), ['foo', Symbol.for(':as'), 'bar']],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        "import bar from 'foo';"
      );
    });
  });
  describe('module', function (): any {
    it('(module m scheme ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function I(x) {\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          'function K(x, y) {\n' +
          '  return x;\n' +
          '}'
      );
    });
    it('(module m scheme (define I ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'const I = function (x) {\n' +
          '  return x;\n' +
          '};\n' +
          '\n' +
          'const K = function (x, y) {\n' +
          '  return x;\n' +
          '};'
      );
    });
    it('(module m scheme (define (foo length) length))', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'function foo(length: any): any {\n' + '  return length;\n' + '}'
      );
    });
    it('(module m scheme (define (foo (length : Number)) : Number length))', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'function foo(length: number): number {\n' + '  return length;\n' + '}'
      );
    });
    xit('(module m scheme (define I (curry-n ...)) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'const I = curryN(1, function (x) {\n' +
          '  return x;\n' +
          '});\n' +
          '\n' +
          'const K = curryN(2, function (x, y) {\n' +
          '  return x;\n' +
          '});'
      );
    });
    xit('(module m scheme (define I (curry-n ...)) ...)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('scheme'),
            [Symbol.for('define'), Symbol.for('truish'), Symbol.for('#t')],
            [
              Symbol.for('define'),
              Symbol.for('falsy'),
              [Symbol.for('not'), Symbol.for('truish')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'const truish = true;\n' + '\n' + 'const falsy = !truish;'
      );
    });
    it('read-rose', function (): any {
      return assertEqual(
        compile(
          readRose('(module m scheme\n' + '  (define foo\n' + '    `(foo)))'),
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "const foo = [Symbol.for('foo')];"
      );
    });
    it('read-rose, quasiquote', function (): any {
      return assertEqual(
        compile(
          readRose(
            '(module m scheme\n' +
              '  (define foo 1)\n' +
              '  (define bar\n' +
              '    `(,foo)))'
          ),
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'const foo = 1;\n' + '\n' + 'const bar = [foo];'
      );
    });
    return it('read-rose, quasiquoted list of pairs', function (): any {
      return assertEqual(
        compile(
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
            language: 'JavaScript',
          }
        ),
        'const foo = 1;\n' +
          '\n' +
          'const bar = 2;\n' +
          '\n' +
          "const quux = [['foo', Symbol.for('.'), foo], ['bar', Symbol.for('.'), bar]];"
      );
    });
  });
  describe('cond', function (): any {
    it('(cond (x y)), statement', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('y')]],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'if (x) {\n' + '  return y;\n' + '}'
      );
    });
    it('(cond (x y)), expression', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('y')]],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        'x ? y : undefined'
      );
    });
    it('(cond (x y) (else z)), expression', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('cond'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('else'), Symbol.for('z')],
          ],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        'x ? y : z'
      );
    });
    it('(cond (x y) (else w z)), expression', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('cond'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('else'), Symbol.for('w'), Symbol.for('z')],
          ],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        'x ? y : (() => {\n' + '  w;\n' + '  return z;\n' + '})()'
      );
    });
    it('(cond (x y) (else z)), statement', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('cond'),
            [Symbol.for('x'), Symbol.for('y')],
            [Symbol.for('else'), Symbol.for('z')],
          ],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'if (x) {\n' + '  return y;\n' + '} else {\n' + '  return z;\n' + '}'
      );
    });
    return xit('(cond ((set! x y) z) (else w)), statement', function (): any {
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
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'if ((x = y)) {\n' +
          '  return z;\n' +
          '} else {\n' +
          '  return w;\n' +
          '}'
      );
    });
  });
  describe('if', function (): any {
    it('(if x y z), statement', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'if (x) {\n' + '  y;\n' + '} else {\n' + '  z;\n' + '}'
      );
    });
    it('(if x y), expression', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('if'), Symbol.for('x'), Symbol.for('y')],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        'x ? y : undefined'
      );
    });
    it('(if x y z), expression', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        'x ? y : z'
      );
    });
    it('(if x y z), statement', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'if (x) {\n' + '  return y;\n' + '} else {\n' + '  return z;\n' + '}'
      );
    });
    it('(if "foo" "bar" "baz"), expression', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('if'), 'foo', 'bar', 'baz'],
          compilationEnvironment,
          {
            expressionType: 'expression',
            language: 'JavaScript',
          }
        ),
        "'foo' ? 'bar' : 'baz'"
      );
    });
    it('(if x (begin y z) w), statement', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('if'),
            Symbol.for('x'),
            [Symbol.for('begin'), Symbol.for('y'), Symbol.for('z')],
            Symbol.for('w'),
          ],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'if (x) {\n' +
          '  y;\n' +
          '  return z;\n' +
          '} else {\n' +
          '  return w;\n' +
          '}'
      );
    });
    return xit('(if (set! x y) z w), statement', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('if'),
            [Symbol.for('set!'), Symbol.for('x'), Symbol.for('y')],
            Symbol.for('z'),
            Symbol.for('w'),
          ],
          compilationEnvironment,
          {
            expressionType: 'return',
            language: 'JavaScript',
          }
        ),
        'if ((x = y)) {\n' +
          '  return z;\n' +
          '} else {\n' +
          '  return w;\n' +
          '}'
      );
    });
  });
  describe('when', function (): any {
    it('(when x y z), statement', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('when'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'if (x) {\n' + '  y;\n' + '  z;\n' + '}'
      );
    });
    return it('(when (> (array-list-length args) 0) ...), statement', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'if (args.length > 0) {\n' +
          '  args = args.slice(0, args.length - 1).concat(args[args.length - 1]);\n' +
          '}'
      );
    });
  });
  describe('unless', function (): any {
    return it('(unless x y z), statement', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('unless'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'if (!x) {\n' + '  y;\n' + '  z;\n' + '}'
      );
    });
  });
  describe('aget', function (): any {
    it('(aget args 0)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('aget'), Symbol.for('args'), 0],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'args[0]'
      );
    });
    return it('(aget args 0 1)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('aget'), Symbol.for('args'), 0, 1],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'args[0][1]'
      );
    });
  });
  describe('aref', function (): any {
    it('(aref args 0)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('aref'), Symbol.for('args'), 0],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'args[0]'
      );
    });
    return it('(aref args 0 1)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('aref'), Symbol.for('args'), 0, 1],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'args[0][1]'
      );
    });
  });
  describe('aset', function (): any {
    return it('(aset args 0 1)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('aset'), Symbol.for('args'), 0, 1],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'args[0] = 1;'
      );
    });
  });
  describe('set!...aref', function (): any {
    return it('(set! (aref args 0) 1)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('set!'), [Symbol.for('aref'), Symbol.for('args'), 0], 1],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'args[0] = 1;'
      );
    });
  });
  describe('=', function (): any {
    it('(= 1 1)', function (): any {
      return assertEqual(
        compile([Symbol.for('='), 1, 1], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '1 === 1'
      );
    });
    return it('(= x y)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('='), Symbol.for('x'), Symbol.for('y')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x === y'
      );
    });
  });
  describe('<', function (): any {
    it('(< 1)', function (): any {
      return assertEqual(
        compile([Symbol.for('<'), 1], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'true'
      );
    });
    it('(< 1 2)', function (): any {
      return assertEqual(
        compile([Symbol.for('<'), 1, 2], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '1 < 2'
      );
    });
    return it('(< 1 2 3)', function (): any {
      return assertEqual(
        compile([Symbol.for('<'), 1, 2, 3], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '(1 < 2) && (2 < 3)'
      );
    });
  });
  describe('>', function (): any {
    it('(> 1)', function (): any {
      return assertEqual(
        compile([Symbol.for('>'), 1], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'true'
      );
    });
    it('(> 2 1)', function (): any {
      return assertEqual(
        compile([Symbol.for('>'), 2, 1], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '2 > 1'
      );
    });
    return it('(> 3 2 1)', function (): any {
      return assertEqual(
        compile([Symbol.for('>'), 3, 2, 1], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '(3 > 2) && (2 > 1)'
      );
    });
  });
  describe('not', function (): any {
    it('(not (and x y))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('not'),
            [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '!(x && y)'
      );
    });
    it('(not (= 1 2))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('not'), [Symbol.for('='), 1, 2]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '1 !== 2'
      );
    });
    it('(not (> 1 2))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('not'), [Symbol.for('>'), 1, 2]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '!(1 > 2)'
      );
    });
    it('(not (f x))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('not'), [Symbol.for('f'), Symbol.for('x')]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '!f(x)'
      );
    });
    return xit('(and (not (f x)) (not (g y)))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('and'),
            [Symbol.for('not'), [Symbol.for('f'), Symbol.for('x')]],
            [Symbol.for('not'), [Symbol.for('g'), Symbol.for('y')]],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '!f(x) && !g(y)'
      );
    });
  });
  describe('and', function (): any {
    it('(and)', function (): any {
      return assertEqual(
        compile([Symbol.for('and')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'true'
      );
    });
    it('(and x)', function (): any {
      return assertEqual(
        compile([Symbol.for('and'), Symbol.for('x')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'x'
      );
    });
    it('(and x y)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x && y'
      );
    });
    xit('(and x y z)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('and'),
            Symbol.for('x'),
            Symbol.for('y'),
            Symbol.for('z'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x && y && z'
      );
    });
    xit('(and x y (w z))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('and'),
            Symbol.for('x'),
            Symbol.for('y'),
            [Symbol.for('w'), Symbol.for('z')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x && y && w(z)'
      );
    });
    return xit('(and x y (or w z))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('and'),
            Symbol.for('x'),
            Symbol.for('y'),
            [Symbol.for('or'), Symbol.for('w'), Symbol.for('z')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x && y && (w || z)'
      );
    });
  });
  describe('or', function (): any {
    it('(or)', function (): any {
      return assertEqual(
        compile([Symbol.for('or')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'false'
      );
    });
    it('(or x)', function (): any {
      return assertEqual(
        compile([Symbol.for('or'), Symbol.for('x')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'x'
      );
    });
    it('(or x y)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('or'), Symbol.for('x'), Symbol.for('y')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x || y'
      );
    });
    return xit('(or x y z)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('or'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x || y || z'
      );
    });
  });
  describe('.', function (): any {
    it('(. map get "foo")', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('.'), Symbol.for('map'), Symbol.for('get'), 'foo'],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "map.get('foo')"
      );
    });
    it('(.get map "foo")', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('.get'), Symbol.for('map'), 'foo'],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "map.get('foo')"
      );
    });
    return it('(.-length arr)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('.-length'), Symbol.for('arr')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'arr.length'
      );
    });
  });
  describe('send', function (): any {
    return it('(send map get "foo")', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('send'), Symbol.for('map'), Symbol.for('get'), 'foo'],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "map.get('foo')"
      );
    });
  });
  describe('get-field', function (): any {
    it('(get-field length arr)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('arr')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'arr.length'
      );
    });
    return it('(get-field (- len 1) arr)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('get-field'),
            [Symbol.for('-'), Symbol.for('len'), 1],
            Symbol.for('arr'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'arr[len - 1]'
      );
    });
  });
  describe('set-field!', function (): any {
    it('(set-field! prop obj val)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('set-field!'),
            Symbol.for('prop'),
            Symbol.for('obj'),
            Symbol.for('val'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'obj.prop = val'
      );
    });
    return it('(set-field! def-method ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'genericFunction.defMethod = function (arglist, functionDefinition) {\n' +
          '  const entry = [arglist, functionDefinition];\n' +
          '  genericFunction.methods.unshift(entry);\n' +
          '  return genericFunction;\n' +
          '}'
      );
    });
  });
  describe('length', function (): any {
    return it('(array-list-length x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('array-list-length'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x.length'
      );
    });
  });
  describe('foldl', function (): any {
    it('(foldl f v l)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('foldl'),
            Symbol.for('f'),
            Symbol.for('v'),
            Symbol.for('l'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'l.reduce(function (acc, x) {\n' + '  return f(x, acc);\n' + '}, v)'
      );
    });
    it('(foldl (lambda ...) v l)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('foldl'),
            [
              Symbol.for('lambda'),
              [Symbol.for('x'), Symbol.for('acc')],
              [Symbol.for('f'), Symbol.for('x'), Symbol.for('acc')],
            ],
            Symbol.for('v'),
            Symbol.for('l'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'l.reduce(function (acc, x) {\n' + '  return f(x, acc);\n' + '}, v)'
      );
    });
    return it("(foldl + 0 '(1 2 3 4))", function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('foldl'),
            Symbol.for('+'),
            0,
            [Symbol.for('quote'), [1, 2, 3, 4]],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[1, 2, 3, 4].reduce(function (acc, x) {\n' +
          '  return x + acc;\n' +
          '}, 0)'
      );
    });
  });
  describe('foldr', function (): any {
    xit('(foldr f v x)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('foldr'),
            Symbol.for('f'),
            Symbol.for('v'),
            Symbol.for('x'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x.reduceRight((function (f) {\n' +
          '  return function (x, y) {\n' +
          '    return f(y, x);\n' +
          '  };\n' +
          '})(f), v)'
      );
    });
    xit('(foldr (f g) v x)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('foldr'),
            [Symbol.for('f'), Symbol.for('g')],
            Symbol.for('v'),
            Symbol.for('x'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x.reduceRight((function (f) {\n' +
          '  return function (x, y) {\n' +
          '    return f(y, x);\n' +
          '  };\n' +
          '})(f(g)), v)'
      );
    });
    return xit("(foldr cons '() '(1 2 3 4))", function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('foldr'),
            Symbol.for('cons'),
            [Symbol.for('quote'), []],
            [Symbol.for('quote'), [1, 2, 3, 4]],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '[1, 2, 3, 4].reduceRight((function (f) {\n' +
          '  return function (x, y) {\n' +
          '    return f(y, x);\n' +
          '  };\n' +
          '})(cons), [])'
      );
    });
  });
  describe('nth', function (): any {
    xit('(nth 1 x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('nth'), 1, Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x[1]'
      );
    });
    return xit('(nth 2 (nth 1 x))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('nth'), 2, [Symbol.for('nth'), 1, Symbol.for('x')]],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x[1][2]'
      );
    });
  });
  describe('nthcdr', function (): any {
    return xit('(nthcdr 1 x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('nthcdr'), 1, Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x.slice(1)'
      );
    });
  });
  describe('drop', function (): any {
    return it('(drop x 1)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('drop'), Symbol.for('x'), 1],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x.slice(1)'
      );
    });
  });
  describe('drop-right', function (): any {
    return it('(drop-right x 1)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('drop-right'), Symbol.for('x'), 1],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x.slice(0, -1)'
      );
    });
  });
  describe('for', function (): any {
    it("(for ((x '(1 2 3))) (display x))", function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
            [Symbol.for('display'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let x of [1, 2, 3]) {\n' + '  console.log(x);\n' + '}'
      );
    });
    it("(for ((x '(1 2 3))) (break))", function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
            [Symbol.for('break')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let x of [1, 2, 3]) {\n' + '  break;\n' + '}'
      );
    });
    it("(for ((x '(1 2 3))) (continue))", function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
            [Symbol.for('continue')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let x of [1, 2, 3]) {\n' + '  continue;\n' + '}'
      );
    });
    it('(for ((x ...)) (let ((x ...)) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
            [
              Symbol.for('let'),
              [[Symbol.for('x'), 1]],
              [Symbol.for('display'), Symbol.for('x')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let x of [1, 2, 3]) {\n' +
          '  {\n' +
          '    const x = 1;\n' +
          '    console.log(x);\n' +
          '  }\n' +
          '}'
      );
    });
    it('(for ((x ...)) (let ((y ...)) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
            [
              Symbol.for('let'),
              [[Symbol.for('y'), 1]],
              [Symbol.for('display'), Symbol.for('x'), Symbol.for('y')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let x of [1, 2, 3]) {\n' +
          '  const y = 1;\n' +
          '  console.log(x, y);\n' +
          '}'
      );
    });
    it('(for ((x ...)) (let ((y ...)) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
            [
              Symbol.for('let'),
              [[Symbol.for('y'), 1]],
              [Symbol.for('display'), Symbol.for('y')],
            ],
            [Symbol.for('display'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let x of [1, 2, 3]) {\n' +
          '  const y = 1;\n' +
          '  console.log(y);\n' +
          '  console.log(x);\n' +
          '}'
      );
    });
    it('(for ((i (range 0 10))) (display x)), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]],
            [Symbol.for('display'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let i = 0; i < 10; i++) {\n' + '  console.log(x);\n' + '}'
      );
    });
    it('(for ((i (range 0 10))) (display x)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]],
            [Symbol.for('display'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'for (let i: any = 0; i < 10; i++) {\n' + '  console.log(x);\n' + '}'
      );
    });
    it('(for ((i (range 1 10 2))) (display x))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('i'), [Symbol.for('range'), 1, 10, 2]]],
            [Symbol.for('display'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let i = 1; i < 10; i = i + 2) {\n' + '  console.log(x);\n' + '}'
      );
    });
    it('(for ((i (range 10 1 -1))) (display x))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('i'), [Symbol.for('range'), 10, 1, -1]]],
            [Symbol.for('display'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let i = 10; i > 1; i--) {\n' + '  console.log(x);\n' + '}'
      );
    });
    it('(for ((i (range 10 1 -2))) (display x))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('for'),
            [[Symbol.for('i'), [Symbol.for('range'), 10, 1, -2]]],
            [Symbol.for('display'), Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'for (let i = 10; i > 1; i = i - 2) {\n' + '  console.log(x);\n' + '}'
      );
    });
    it('(for ((i (range 0 (+ 1 1)))) (display i))', function (): any {
      return assertEqual(
        compile(
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
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const _end = 1 + 1;\n' +
          '\n' +
          'for (let i = 0; i < _end; i++) {\n' +
          '  console.log(i);\n' +
          '}'
      );
    });
    it('(for ((i (range (+ 1 1) (+ 2 2)))) (display i))', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const _start = 1 + 1;\n' +
          '\n' +
          'const _end = 2 + 2;\n' +
          '\n' +
          'for (let i = _start; i < _end; i++) {\n' +
          '  console.log(i);\n' +
          '}'
      );
    });
    it('(for ((i (range (+ 1 1) (+ 2 2)))) (display i))', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const _start: any = 1 + 1;\n' +
          '\n' +
          'const _end: any = 2 + 2;\n' +
          '\n' +
          'for (let i: any = _start; i < _end; i++) {\n' +
          '  console.log(i);\n' +
          '}'
      );
    });
    it('(let ((_start 0) (_end 0)) (for ((i (range (+ 1 1) (+ 2 2)))) (display i)))', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const _start: any = 0;\n' +
          '\n' +
          'const _end: any = 0;\n' +
          '\n' +
          'const _start1: any = 1 + 1;\n' +
          '\n' +
          'const _end1: any = 2 + 2;\n' +
          '\n' +
          'for (let i: any = _start1; i < _end1; i++) {\n' +
          '  console.log(i);\n' +
          '}'
      );
    });
    it('(for ((i (range (+ 1 1) (+ 2 2)))) (for ((j (range (+ 3 3) (+ 4 4)))) (display j)))', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const _start: any = 1 + 1;\n' +
          '\n' +
          'const _end: any = 2 + 2;\n' +
          '\n' +
          'for (let i: any = _start; i < _end; i++) {\n' +
          '  const _start1: any = 3 + 3;\n' +
          '  const _end1: any = 4 + 4;\n' +
          '  for (let j: any = _start1; j < _end1; j++) {\n' +
          '    console.log(j);\n' +
          '  }\n' +
          '}'
      );
    });
    return it('(define (foo) (for ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            [Symbol.for('foo')],
            [
              Symbol.for('for'),
              [[Symbol.for('x'), [Symbol.for('quote'), [1, 2, 3]]]],
              [Symbol.for('display'), Symbol.for('x')],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function foo() {\n' +
          '  for (let x of [1, 2, 3]) {\n' +
          '    console.log(x);\n' +
          '  }\n' +
          '}'
      );
    });
  });
  describe('do', function (): any {
    it('(do () ((not (< (array-list-length result) 3))) (display result))', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'while (result.length < 3) {\n' + '  console.log(result);\n' + '}'
      );
    });
    return xit('(do ((*do-result* (display result))) ((not (< (array-list-length result) 3))))', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'do {\n' + '  console.log(result);\n' + '} while (result.length < 3);'
      );
    });
  });
  describe('js/while', function (): any {
    it('(js/while (< (array-list-length result) 3) (display result))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js/while'),
            [
              Symbol.for('<'),
              [Symbol.for('array-list-length'), Symbol.for('result')],
              3,
            ],
            [Symbol.for('display'), Symbol.for('result')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'while (result.length < 3) {\n' + '  console.log(result);\n' + '}'
      );
    });
    return it('(js/while (begin ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(js/do-while (display result) (< (array-list-length result) 3))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js/do-while'),
            [Symbol.for('display'), Symbol.for('result')],
            [
              Symbol.for('<'),
              [Symbol.for('array-list-length'), Symbol.for('result')],
              3,
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'do {\n' + '  console.log(result);\n' + '} while (result.length < 3);'
      );
    });
    return it('(js/do-while (begin (foo) (display result)) (< (array-list-length result) 3))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js/do-while'),
            [
              Symbol.for('begin'),
              [Symbol.for('foo')],
              [Symbol.for('display'), Symbol.for('result')],
            ],
            [
              Symbol.for('<'),
              [Symbol.for('array-list-length'), Symbol.for('result')],
              3,
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'do {\n' +
          '  foo();\n' +
          '  console.log(result);\n' +
          '} while (result.length < 3);'
      );
    });
  });
  describe('first', function (): any {
    return it('(first x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('first'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x[0]'
      );
    });
  });
  describe('last', function (): any {
    return xit('(last x)', function (): any {
      return assertEqual(
        compile([Symbol.for('last'), Symbol.for('x')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'x[x.length - 1]'
      );
    });
  });
  describe('class', function (): any {
    return it('(class () (define (bar) "bar"))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('class'),
            [],
            [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'class {\n' + '  bar() {\n' + "    return 'bar';\n" + '  }\n' + '}'
      );
    });
  });
  describe('define-class', function (): any {
    it('(define-class Foo)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('define-class'), Symbol.for('Foo')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'class Foo {\n' + '}'
      );
    });
    it('(define-class Foo () (define (bar) "bar"))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-class'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'class Foo {\n' + '  bar() {\n' + "    return 'bar';\n" + '  }\n' + '}'
      );
    });
    it('(define-class Foo () (define (bar) "bar") (define (baz) "baz"))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-class'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define/public'), [Symbol.for('bar')], 'bar'],
            [Symbol.for('define/public'), [Symbol.for('baz')], 'baz'],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(define-class Foo () (define bar) (define baz "baz") (define (quux) "quux"))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-class'),
            Symbol.for('Foo'),
            [],
            [Symbol.for('define/public'), Symbol.for('bar')],
            [Symbol.for('define/public'), Symbol.for('baz'), 'baz'],
            [Symbol.for('define/public'), [Symbol.for('quux')], 'quux'],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(define-class Foo () (define x) (define (constructor x) ...) ...), JS', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(define-class Foo () (define x) (define (constructor x) ...) ...), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
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
    it('(define-class Foo () (define x) (define (constructor . args) ...) ...), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
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
    it('(define-class Foo (Object) (define x) (define (constructor x) ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(define-class Foo (Object) (define/public ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(define-class Foo (Object) (define/private ...) ...)', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
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
    it('(define-class Foo () (public ...) (define ...) ...)', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
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
    it('(define-class Foo () (private ...) (define ...) ...)', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
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
    it('(define-class Foo () ... (define (aget ...) ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
                [
                  Symbol.for('get-field'),
                  Symbol.for('arr'),
                  Symbol.for('this'),
                ],
                Symbol.for('i'),
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(define-class Foo () ... (define/generator (generator) ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    return it('(define-class Foo () ... (define/generator ((get-field iterator Symbol)) ...) ...)', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(define Foo (class object%))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            Symbol.for('Foo'),
            [Symbol.for('class'), Symbol.for('object%')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'class Foo {\n' + '}'
      );
    });
    return it('(define Foo (class Bar))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            Symbol.for('Foo'),
            [Symbol.for('class'), Symbol.for('Bar')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'class Foo extends Bar {\n' + '}'
      );
    });
  });
  describe('js', function (): any {
    it('(js "1")', function (): any {
      return assertEqual(
        compile([Symbol.for('js'), '1'], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '1'
      );
    });
    return it('(js "function I(x) { return x; }")', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('js'), 'function I(x) { return x; }'],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'function I(x) { return x; }'
      );
    });
  });
  describe('make-hash', function (): any {
    it('(make-hash)', function (): any {
      return assertEqual(
        compile([Symbol.for('make-hash')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'new Map()'
      );
    });
    it('(make-hash (quote ...)), list of pairs', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('make-hash'),
            [
              Symbol.for('quote'),
              [
                ['foo', Symbol.for('.'), 'bar'],
                ['baz', Symbol.for('.'), 'quux'],
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "new Map([['foo', 'bar'], ['baz', 'quux']])"
      );
    });
    it('(make-hash (quote ...)), list of lists', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('make-hash'),
            [
              Symbol.for('quote'),
              [
                ['foo', 'bar'],
                ['baz', 'quux'],
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "new Map([['foo', ['bar']], ['baz', ['quux']]])"
      );
    });
    it('(make-hash (quasiquote ...)), list of pairs', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('make-hash'),
            [
              Symbol.for('quasiquote'),
              [
                ['foo', Symbol.for('.'), 'bar'],
                ['baz', Symbol.for('.'), 'quux'],
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "new Map([['foo', 'bar'], ['baz', 'quux']])"
      );
    });
    it('(make-hash (quasiquote ...)), list of pairs, hash>list', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "new Map([['foo', 'bar'], ['baz', 'quux'], ...xyzzy.entries()])"
      );
    });
    xit('(make-hash (quasiquote ...)), list of pairs', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "new Map([...[['foo', 'bar'], ['baz', 'quux']], ...xyzzy.entries()])"
      );
    });
    return it('(make-hash (quasiquote ...)), list of lists', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('make-hash'),
            [
              Symbol.for('quasiquote'),
              [
                ['foo', 'bar'],
                ['baz', 'quux'],
              ],
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "new Map([['foo', ['bar']], ['baz', ['quux']]])"
      );
    });
  });
  describe('JavaScript objects', function (): any {
    xit('{}', function (): any {
      return assertEqual(
        compile({}, compilationEnvironment, {
          language: 'JavaScript',
        }),
        '{}'
      );
    });
    return xit("{ foo: 'bar' }", function (): any {
      return assertEqual(
        compile(
          {
            foo: 'bar',
          },
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "{ foo: 'bar' }"
      );
    });
  });
  describe('js-obj', function (): any {
    it('(js-obj)', function (): any {
      return assertEqual(
        compile([Symbol.for('js-obj')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '{}'
      );
    });
    it('(js-obj foo "bar")', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('js-obj'), Symbol.for('foo'), 'bar'],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '{\n' + "  [foo]: 'bar'\n" + '}'
      );
    });
    it('(js-obj "foo" "bar")', function (): any {
      return assertEqual(
        compile([Symbol.for('js-obj'), 'foo', 'bar'], compilationEnvironment, {
          language: 'JavaScript',
        }),
        '{\n' + "  foo: 'bar'\n" + '}'
      );
    });
    it('(js-obj "foo bar" "foo bar")', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('js-obj'), 'foo bar', 'foo bar'],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '{\n' + "  'foo bar': 'foo bar'\n" + '}'
      );
    });
    it('(js-obj "foo" (js-obj "bar" "baz"))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('js-obj'), 'foo', [Symbol.for('js-obj'), 'bar', 'baz']],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '{\n' + '  foo: {\n' + "    bar: 'baz'\n" + '  }\n' + '}'
      );
    });
    it('(js-obj "foo" (js-obj "foo" "foo") "bar" (js-obj "bar" "bar"))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js-obj'),
            'foo',
            [Symbol.for('js-obj'), 'foo', 'foo'],
            'bar',
            [Symbol.for('js-obj'), 'bar', 'bar'],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '{\n' +
          '  foo: {\n' +
          "    foo: 'foo'\n" +
          '  },\n' +
          '  bar: {\n' +
          "    bar: 'bar'\n" +
          '  }\n' +
          '}'
      );
    });
    return it('(js-obj "foo" (js-obj) "bar" (js-obj "bar" "bar") "baz" (js-obj "baz" "baz"))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js-obj'),
            'foo',
            [Symbol.for('js-obj')],
            'bar',
            [Symbol.for('js-obj'), 'bar', 'bar'],
            'baz',
            [Symbol.for('js-obj'), 'baz', 'baz'],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '{\n' +
          '  foo: {},\n' +
          '  bar: {\n' +
          "    bar: 'bar'\n" +
          '  },\n' +
          '  baz: {\n' +
          "    baz: 'baz'\n" +
          '  }\n' +
          '}'
      );
    });
  });
  describe('js-obj?', function (): any {
    return it('(js-obj? x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('js-obj?'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "(x !== null) && (typeof x === 'object')"
      );
    });
  });
  describe('js-obj-append', function (): any {
    return it('(js-obj-append obj (js-obj "foo" "bar"))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js-obj-append'),
            Symbol.for('obj'),
            [Symbol.for('js-obj'), 'foo', 'bar'],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '{\n' + '  ...obj,\n' + "  foo: 'bar'\n" + '}'
      );
    });
  });
  describe('js-keys', function (): any {
    return it('(js-keys x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('js-keys'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'Object.keys(x)'
      );
    });
  });
  describe('js/tag', function (): any {
    return it('(js/tag foo "bar")', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('js/tag'), Symbol.for('foo'), 'bar'],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'foo`bar`'
      );
    });
  });
  describe('->', function (): any {
    it('(-> x (.foo "bar") (.baz))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('->'),
            Symbol.for('x'),
            [Symbol.for('.foo'), 'bar'],
            [Symbol.for('.baz')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "x.foo('bar').baz()"
      );
    });
    return it('(-> regular-args (.map ...) (.join ...))', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'regularArgs.map(function (arg) {\n' +
          '  return compileExpression(arg, env, inheritedOptions);\n' +
          "}).join(', ')"
      );
    });
  });
  describe('js/try', function (): any {
    it('(js/try)', function (): any {
      return assertEqual(
        compile([Symbol.for('js/try')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'try {\n' + '}'
      );
    });
    it('(js/try ...)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js/try'),
            [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'try {\n' + '  x = 2 / 1;\n' + '}'
      );
    });
    it('(js/try ... (finally ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js/try'),
            [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
            [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'try {\n' +
          '  x = 2 / 1;\n' +
          '} finally {\n' +
          "  console.log('cleanup');\n" +
          '}'
      );
    });
    it('(js/try ... (catch _ ...) (finally ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js/try'),
            [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
            [
              Symbol.for('catch'),
              Symbol.for('_'),
              [Symbol.for('display'), 'there was an error'],
            ],
            [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'try {\n' +
          '  x = 2 / 1;\n' +
          '} catch {\n' +
          "  console.log('there was an error');\n" +
          '} finally {\n' +
          "  console.log('cleanup');\n" +
          '}'
      );
    });
    return it('(js/try ... (catch e ...) (finally ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('js/try'),
            [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
            [
              Symbol.for('catch'),
              Symbol.for('e'),
              [Symbol.for('display'), 'there was an error'],
            ],
            [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
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
    it('(clj/try)', function (): any {
      return assertEqual(
        compile([Symbol.for('clj/try')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'try {\n' + '}'
      );
    });
    it('(clj/try ...)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('clj/try'),
            [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'try {\n' + '  x = 2 / 1;\n' + '}'
      );
    });
    it('(clj/try ... (finally ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('clj/try'),
            [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
            [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'try {\n' +
          '  x = 2 / 1;\n' +
          '} finally {\n' +
          "  console.log('cleanup');\n" +
          '}'
      );
    });
    it('(clj/try ... (catch ...) (finally ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('clj/try'),
            [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
            [
              Symbol.for('catch'),
              Symbol.for('Object'),
              Symbol.for('e'),
              [Symbol.for('display'), 'there was an error'],
            ],
            [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'try {\n' +
          '  x = 2 / 1;\n' +
          '} catch (e) {\n' +
          "  console.log('there was an error');\n" +
          '} finally {\n' +
          "  console.log('cleanup');\n" +
          '}'
      );
    });
    return it('(clj/try ... (catch ...) (finally ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('clj/try'),
            [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
            [
              Symbol.for('catch'),
              Symbol.for('MyException'),
              Symbol.for('e'),
              [Symbol.for('display'), 'there was an error'],
            ],
            [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'try {\n' +
          '  x = 2 / 1;\n' +
          '} catch (e) {\n' +
          '  if (e instanceof MyException) {\n' +
          "    console.log('there was an error');\n" +
          '  } else {\n' +
          '    throw e;\n' +
          '  }\n' +
          '} finally {\n' +
          "  console.log('cleanup');\n" +
          '}'
      );
    });
  });
  describe('map', function (): any {
    it('(map f x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('map'), Symbol.for('f'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x.map(function (x) {\n' + '  return f(x);\n' + '})'
      );
    });
    it('(map (lambda (x) x) x)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('map'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
            Symbol.for('x'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x.map(function (x) {\n' + '  return x;\n' + '})'
      );
    });
    return it('(map (g y) x)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('map'),
            [Symbol.for('g'), Symbol.for('y')],
            Symbol.for('x'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'x.map((function (f) {\n' +
          '  return function (x) {\n' +
          '    return f(x);\n' +
          '  };\n' +
          '})(g(y)))'
      );
    });
  });
  describe('throw', function (): any {
    return it('(throw (new Error "An error"))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('throw'),
            [Symbol.for('new'), Symbol.for('Error'), 'An error'],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "throw new Error('An error');"
      );
    });
  });
  describe('js/delete', function (): any {
    return it('(js/delete x)', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('js/delete'), Symbol.for('x')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'delete x'
      );
    });
  });
  describe('return', function (): any {
    it('(return)', function (): any {
      return assertEqual(
        compile([Symbol.for('return')], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'return;'
      );
    });
    return it('(return 0)', function (): any {
      return assertEqual(
        compile([Symbol.for('return'), 0], compilationEnvironment, {
          language: 'JavaScript',
        }),
        'return 0;'
      );
    });
  });
  describe('yield', function (): any {
    it('(yield)', function (): any {
      return assertEqual(
        compile([Symbol.for('yield')], compilationEnvironment, {
          expressionType: 'statement',
          language: 'JavaScript',
        }),
        'yield;'
      );
    });
    return it('(yield 0)', function (): any {
      return assertEqual(
        compile([Symbol.for('yield'), 0], compilationEnvironment, {
          expressionType: 'statement',
          language: 'JavaScript',
        }),
        'yield 0;'
      );
    });
  });
  describe('await', function (): any {
    return it('(await (foo))', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('await'), [Symbol.for('foo')]],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'await foo();'
      );
    });
  });
  describe('async', function (): any {
    it('(async (lambda (x) x))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('async'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'async function (x) {\n' + '  return x;\n' + '}'
      );
    });
    it('(define foo (async (lambda (x) x))), JS', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'async function foo(x) {\n' + '  return x;\n' + '}'
      );
    });
    it('(define foo (async (lambda (x) x))), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'async function foo(x: any): Promise<any> {\n' + '  return x;\n' + '}'
      );
    });
    return it('(define/async (foo x) x)', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define/async'),
            [Symbol.for('foo'), Symbol.for('x')],
            Symbol.for('x'),
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        'async function foo(x) {\n' + '  return x;\n' + '}'
      );
    });
  });
  describe('module', function (): any {
    it('(module m lisp (define x 1) (define y 2))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [Symbol.for('define'), Symbol.for('x'), 1],
            [Symbol.for('define'), Symbol.for('y'), 2],
          ],
          compilationEnvironment,
          {
            expressionType: 'statement',
            language: 'JavaScript',
          }
        ),
        'const x = 1;\n' + '\n' + 'const y = 2;'
      );
    });
    xit("(module m lisp ... (define *lisp-map* '()))", function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [Symbol.for('I'), Symbol.for('x')],
              Symbol.for('x'),
            ],
            [Symbol.for('define'), Symbol.for('x'), 1],
            [Symbol.for('define'), Symbol.for('*lisp-map*'), Symbol.for('#t')],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function I(x) {\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          "I.lispSource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];\n" +
          '\n' +
          'const x = 1;'
      );
    });
    xit("(module m lisp ... (define *lisp-map* '()))", function (): any {
      return assertEqual(
        compile(
          readRose(
            '(module m lisp\n' +
              '  ;; inline-lisp-sources: true\n' +
              '\n' +
              '  (define (I x) x))'
          ),
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        '// inline-lisp-sources: true\n' +
          '\n' +
          'function I(x) {\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          "I.lispSource = [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')];"
      );
    });
    xit("(module m lisp (require ...) ... (define *lisp-map* '())), JS", function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('require'),
              [Symbol.for('only-in'), './combinators', Symbol.for('I')],
            ],
            [Symbol.for('define'), Symbol.for('x'), 1],
            [Symbol.for('define'), Symbol.for('*lisp-map*'), Symbol.for('#t')],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'import {\n' +
          '  I\n' +
          "} from './combinators';\n" +
          '\n' +
          'const x = 1;'
      );
    });
    xit("(module m lisp (require ...) ... (define *lisp-map* '())), TS", function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('require'),
              [Symbol.for('only-in'), './combinators', Symbol.for('I')],
            ],
            [Symbol.for('define'), Symbol.for('x'), 1],
            [Symbol.for('define'), Symbol.for('*lisp-map*'), Symbol.for('#t')],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'TypeScript',
          }
        ),
        'import {\n' +
          '  I\n' +
          "} from './combinators';\n" +
          '\n' +
          'const x: any = 1;'
      );
    });
    it('(module m lisp (define (js_ str) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [Symbol.for('js_'), Symbol.for('str')],
              [Symbol.for('js/eval'), Symbol.for('str')],
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function js_(str) {\n' + '  return eval(str);\n' + '}'
      );
    });
    it('(module m lisp (define (my-foldl ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myFn(foldl, f, v, l) {\n' + '  return foldl(f, v, l);\n' + '}'
      );
    });
    it('(module m lisp (define (my-foldl-obj ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myFoldlObj(obj, f, v, l) {\n' +
          '  return obj.foldl(f, v, l);\n' +
          '}'
      );
    });
    it('(module ... (define-class ... (define ...)))', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'class Foo {\n' +
          '  foldl(f, v, l) {\n' +
          '    return l;\n' +
          '  }\n' +
          '}'
      );
    });
    it('(module m lisp (define (my-pop ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [Symbol.for('my-pop'), Symbol.for('lst'), Symbol.for('x')],
              [Symbol.for('pop!'), Symbol.for('lst'), Symbol.for('x')],
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPop(lst, x) {\n' + '  return lst.shift();\n' + '}'
      );
    });
    it('(module m lisp (define (my-pop-2 ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPop2(lst, x) {\n' + '  return [...lst].shift();\n' + '}'
      );
    });
    it('(module m lisp (define (my-pop-right ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [Symbol.for('my-pop-right'), Symbol.for('lst'), Symbol.for('x')],
              [Symbol.for('pop-right!'), Symbol.for('lst'), Symbol.for('x')],
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPopRight(lst, x) {\n' + '  return lst.pop();\n' + '}'
      );
    });
    it('(module m lisp (define (my-pop-right-2 ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [
                Symbol.for('my-pop-right-2'),
                Symbol.for('lst'),
                Symbol.for('x'),
              ],
              [
                Symbol.for('pop-right!'),
                [Symbol.for('append'), Symbol.for('lst')],
                Symbol.for('x'),
              ],
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPopRight2(lst, x) {\n' + '  return [...lst].pop();\n' + '}'
      );
    });
    it('(module m lisp (define (my-push ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [Symbol.for('my-push'), Symbol.for('lst'), Symbol.for('x')],
              [Symbol.for('push!'), Symbol.for('lst'), Symbol.for('x')],
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPush(lst, x) {\n' +
          '  lst.unshift(x);\n' +
          '  return lst;\n' +
          '}'
      );
    });
    it('(module m lisp (define (my-push-2 ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
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
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPush2(lst, x) {\n' +
          '  return (function (lst, x) {\n' +
          '    lst.unshift(x);\n' +
          '    return lst;\n' +
          '  })([...lst], x);\n' +
          '}'
      );
    });
    it('(module m lisp (define (my-push-3 ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [Symbol.for('my-push-3'), Symbol.for('lst'), Symbol.for('x')],
              [Symbol.for('push!'), Symbol.for('lst'), Symbol.for('x')],
              Symbol.for('lst'),
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPush3(lst, x) {\n' +
          '  lst.unshift(x);\n' +
          '  return lst;\n' +
          '}'
      );
    });
    it('(module m lisp (define (my-push-right ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [Symbol.for('my-push-right'), Symbol.for('lst'), Symbol.for('x')],
              [Symbol.for('push-right!'), Symbol.for('lst'), Symbol.for('x')],
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPushRight(lst, x) {\n' +
          '  lst.push(x);\n' +
          '  return lst;\n' +
          '}'
      );
    });
    it('(module m lisp (define (my-push-right-2 ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [
                Symbol.for('my-push-right-2'),
                Symbol.for('lst'),
                Symbol.for('x'),
              ],
              [
                Symbol.for('push-right!'),
                [Symbol.for('append'), Symbol.for('lst')],
                Symbol.for('x'),
              ],
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPushRight2(lst, x) {\n' +
          '  return (function (lst, x) {\n' +
          '    lst.push(x);\n' +
          '    return lst;\n' +
          '  })([...lst], x);\n' +
          '}'
      );
    });
    return it('(module m lisp (define (my-push-right-3 ...) ...))', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('module'),
            Symbol.for('m'),
            Symbol.for('lisp'),
            [
              Symbol.for('define'),
              [
                Symbol.for('my-push-right-3'),
                Symbol.for('lst'),
                Symbol.for('x'),
              ],
              [Symbol.for('push-right!'), Symbol.for('lst'), Symbol.for('x')],
              Symbol.for('lst'),
            ],
          ],
          compilationEnvironment,
          {
            camelCase: true,
            language: 'JavaScript',
          }
        ),
        'function myPushRight3(lst, x) {\n' +
          '  lst.push(x);\n' +
          '  return lst;\n' +
          '}'
      );
    });
  });
  describe('string-append', function (): any {
    it('(string-append "a")', function (): any {
      return assertEqual(
        compile([Symbol.for('string-append'), 'a'], compilationEnvironment, {
          language: 'JavaScript',
        }),
        "'a'"
      );
    });
    return it('(string-append "a" "b")', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('string-append'), 'a', 'b'],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        "'a' + 'b'"
      );
    });
  });
  describe('ann', function (): any {
    it('(ann 1 Number), JS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('ann'), 1, Symbol.for('Number')],
          compilationEnvironment,
          {
            language: 'JavaScript',
          }
        ),
        '1'
      );
    });
    it('(ann 1 Number), TS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('ann'), 1, Symbol.for('Number')],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        '1 as number'
      );
    });
    it('(ann (list) Any), TS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('ann'), [Symbol.for('list')], Symbol.for('Any')],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        '[] as any'
      );
    });
    it("(ann '() Any), TS", function (): any {
      return assertEqual(
        compile(
          [Symbol.for('ann'), [Symbol.for('quote'), []], Symbol.for('Any')],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        '[] as any'
      );
    });
    it('(ann x (List Any)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('ann'),
            Symbol.for('x'),
            [Symbol.for('List'), Symbol.for('Any')],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'x as [any]'
      );
    });
    it('(ann x (List Number Any)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('ann'),
            Symbol.for('x'),
            [Symbol.for('List'), Symbol.for('Number'), Symbol.for('Any')],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'x as [number, any]'
      );
    });
    it('(ann x NN), TS', function (): any {
      return assertEqual(
        compile(
          [Symbol.for('ann'), Symbol.for('x'), Symbol.for('NN')],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'x as NN'
      );
    });
    it('(ann x (NN Any)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('ann'),
            Symbol.for('x'),
            [Symbol.for('NN'), Symbol.for('Any')],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'x as NN<any>'
      );
    });
    it('(ann x (NN Any Any)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('ann'),
            Symbol.for('x'),
            [Symbol.for('NN'), Symbol.for('Any'), Symbol.for('Any')],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'x as NN<any,any>'
      );
    });
    it('((ann (lambda (x) x) Any) 1), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        '(function (x: any): any {\n' + '  return x;\n' + '} as any)(1)'
      );
    });
    return it('(lambda (x) (ann (send x foo) Any)), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
          }
        ),
        'function (x: any): any {\n' + '  return x.foo() as any;\n' + '}'
      );
    });
  });
  describe(':', function (): any {
    it('(: x Any), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('Any')],
            [Symbol.for('define'), Symbol.for('x'), 1],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const x = 1;'
      );
    });
    it('(: x Any), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('Any')],
            [Symbol.for('define'), Symbol.for('x'), 1],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: any = 1;'
      );
    });
    it('(: x String), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('String')],
            [Symbol.for('define'), Symbol.for('x'), '1'],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        "const x: string = '1';"
      );
    });
    it('(: x Number), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('Number')],
            [Symbol.for('define'), Symbol.for('x'), 1],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: number = 1;'
      );
    });
    it('(: x Integer), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('Integer')],
            [Symbol.for('define'), Symbol.for('x'), 1],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: number = 1;'
      );
    });
    it('(: x Natural), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('Natural')],
            [Symbol.for('define'), Symbol.for('x'), 1],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: number = 1;'
      );
    });
    it('(: x Real), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('Real')],
            [Symbol.for('define'), Symbol.for('x'), 1],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: number = 1;'
      );
    });
    it('(: x Symbol), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        "const x: Symbol = Symbol.for('x');"
      );
    });
    it('(: x Boolean), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('Boolean')],
            [Symbol.for('define'), Symbol.for('x'), Symbol.for('#t')],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: boolean = true;'
      );
    });
    it('(: x True), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('True')],
            [Symbol.for('define'), Symbol.for('x'), Symbol.for('#t')],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: true = true;'
      );
    });
    it('(: x False), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('begin'),
            [Symbol.for(':'), Symbol.for('x'), Symbol.for('False')],
            [Symbol.for('define'), Symbol.for('x'), Symbol.for('#f')],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: false = false;'
      );
    });
    it('(: x (U Number String)), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: number | string = 1;'
      );
    });
    it('(: x (U Number String Boolean)), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: number | string | boolean = 1;'
      );
    });
    it('(: x (U Number (U String Boolean))), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: number | (string | boolean) = 1;'
      );
    });
    it('(: x (Listof Number)), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: number[] = [1];'
      );
    });
    it('(: x (Pairof Number)), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        "const x: (number | Symbol)[] = [1, Symbol.for('.'), 2];"
      );
    });
    it('(: hello-world (-> Void)), JS', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'function helloWorld() {\n' + "  console.log('Hello world!');\n" + '}'
      );
    });
    it('(: hello-world (-> Void)), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'function helloWorld(): void {\n' +
          "  console.log('Hello world!');\n" +
          '}'
      );
    });
    it('(: f (-> Number Number)), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'function f(x: number): number {\n' + '  return x;\n' + '}'
      );
    });
    it('(: f (-> Number Number)), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const f: (a: number) => number = function (x: any): any {\n' +
          '  return x;\n' +
          '};'
      );
    });
    it("(: f (-> Number Number)), foo'd lambda, TS", function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const f: (a: number) => number = foo(function (x: any): any {\n' +
          '  return x;\n' +
          '});'
      );
    });
    it('(: f (-> Number Number Number)), function with optional argument, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'function f(x: number, y: number = 1): number {\n' +
          '  return x;\n' +
          '}'
      );
    });
    it('(: f (->* (Number) (Number) Number)), lambda with optional argument, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const f: (a: number, b?: number) => number = function (x: any, y: any = 1): any {\n' +
          '  return x;\n' +
          '};'
      );
    });
    return it('(: x Foo), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const x: Foo = new Foo();'
      );
    });
  });
  return describe('define-type', function (): any {
    it('(define-type NN (-> Number Number)), JS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-type'),
            Symbol.for('NN'),
            [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        ''
      );
    });
    it('(define-type NN (-> Number Number)), TS', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define-type'),
            Symbol.for('NN'),
            [Symbol.for('->'), Symbol.for('Number'), Symbol.for('Number')],
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'type NN = (a: number) => number;'
      );
    });
    it('(: f (-> Number Number)), lambda, JS', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const f = function (x) {\n' + '  return x;\n' + '};'
      );
    });
    it('(: f (-> Number Number)), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'type NN = (a: number) => number;\n' +
          '\n' +
          'const f: NN = function (x: any): any {\n' +
          '  return x;\n' +
          '};'
      );
    });
    it('(lambda ((x : Number)) x), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const f: any = function (x: number): any {\n' + '  return x;\n' + '};'
      );
    });
    it('(js/arrow ((x : Number)) x), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'const f: any = (x: number): any => {\n' + '  return x;\n' + '};'
      );
    });
    it('(define (f (x : Number)) x), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'function f(x: number): any {\n' + '  return x;\n' + '}'
      );
    });
    it('(define (f (x : Number) . args) x), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'function f(x: number, ...args: any[]): any {\n' + '  return x;\n' + '}'
      );
    });
    it('(define (id (x : Number)) : Number x), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'function id(x: number): number {\n' + '  return x;\n' + '}'
      );
    });
    it('(define (f (x : Number 1)) : Number x), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'function f(x: number = 1): number {\n' + '  return x;\n' + '}'
      );
    });
    it('(define (f (options : Any (js-obj))) : Any x), lambda, TS', function (): any {
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
                [Symbol.for('js-obj')],
              ],
            ],
            Symbol.for(':'),
            Symbol.for('Any'),
            Symbol.for('x'),
          ],
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'function f(options: any = {}): any {\n' + '  return x;\n' + '}'
      );
    });
    it('(define Foo (class object% ... (define (constructor (x : Number)) ...))), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
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
    it('(define Foo (class object% ... (define (constructor (x : Number) . args) ...))), lambda, TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
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
    xit('(: f NN), TS', function (): any {
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
          compilationEnvironment,
          {
            language: 'TypeScript',
            expressionType: 'statement',
          }
        ),
        'type NN = (a: number) => number;\n' +
          '\n' +
          'function f(x: number): number {\n' +
          '  return x;\n' +
          '};'
      );
    });
    return it('(: f (-> Number Number)), lambda, comments, TS', function (): any {
      return assertEqual(
        compile(
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
            language: 'TypeScript',
            expressionType: 'statement',
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
  });
});

describe('definition-to-macro', function (): any {
  it('(define (inc x) (+ x 1)), 1', function (): any {
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
  it('(define (logical-or x) (or x x)), #t', function (): any {
    return assertEqual(
      definitionToMacro(
        [
          Symbol.for('define'),
          [Symbol.for('logical-or'), Symbol.for('x')],
          [Symbol.for('or'), Symbol.for('x'), Symbol.for('x')],
        ],
        [Symbol.for('#t')]
      ),
      [Symbol.for('or'), Symbol.for('#t'), Symbol.for('#t')]
    );
  });
  it('(define (repeat x) (string-append x x)), "1"', function (): any {
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
  it('(define (square x) (* x x)), 1', function (): any {
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
  it('(define (square x) (* x x)), x', function (): any {
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
  return xit('(define (square x) (* x x)), (+ 1 1)', function (): any {
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

/**
 * Unsorted tests, to be sorted later.
 */
describe('unsorted', function (): any {
  describe('?.', function (): any {
    it('const x = foo?.bar;', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            Symbol.for('x'),
            [Symbol.for('js/?.'), Symbol.for('foo'), Symbol.for('bar')],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const x = foo?.bar;'
      );
    });
    it('const x = foo?.bar(baz);', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            Symbol.for('x'),
            [
              [Symbol.for('js/?.'), Symbol.for('foo'), Symbol.for('bar')],
              Symbol.for('baz'),
            ],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const x = foo?.bar(baz);'
      );
    });
    return it('const x = foo?.bar(baz);', function (): any {
      return assertEqual(
        compile(
          [
            Symbol.for('define'),
            Symbol.for('x'),
            [Symbol.for('js/?.'), Symbol.for('foo'), [Symbol.for('bar')]],
          ],
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
        'const x = foo?.(bar);'
      );
    });
  });
  describe('define-macro-to-lambda-form', function (): any {
    it('(define-macro (foo x) x)', function (): any {
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
    it('(define-macro (foo &whole expression x) x)', function (): any {
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
    it('(define-macro (foo &whole exp &environment env) exp)', function (): any {
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
    it('(define-macro (foo &whole exp &environment env x) x)', function (): any {
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
    it('(define-macro (foo &rest ...) ...)', function (): any {
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
    return it('(define-macro (foo &rest ...) ...)', function (): any {
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
  // (describe "defmacro-to-lambda-form"
  //   (fn ()
  //     (it "(defmacro foo (x) x)"
  //         (fn ()
  //           (assert-equal
  //            (defmacro-to-lambda-form
  //              '(defmacro foo (x)
  //                 x))
  //            '(lambda (exp env)
  //               (define-values (x)
  //                 (rest exp))
  //               x))))
  //     (it "(defmacro foo (&whole expression x) x)"
  //         (fn ()
  //           (assert-equal
  //            (defmacro-to-lambda-form
  //              '(defmacro foo (&whole expression x)
  //                 x))
  //            '(lambda (expression env)
  //               (define-values (x)
  //                 (rest expression))
  //               x))))
  //     (it "(defmacro foo (&whole exp &environment env) exp)"
  //         (fn ()
  //           (assert-equal
  //            (defmacro-to-lambda-form
  //              '(defmacro foo (&whole exp &environment env)
  //                 exp))
  //            '(lambda (exp env)
  //               exp))))
  //     (it "(defmacro foo (&whole exp &environment env x) x)"
  //         (fn ()
  //           (assert-equal
  //            (defmacro-to-lambda-form
  //              '(defmacro foo (&whole exp &environment env x)
  //                 x))
  //            '(lambda (exp env)
  //               (define-values (x)
  //                 (rest exp))
  //               x))))
  //     (it "(defmacro foo (&rest ...) ...)"
  //         (fn ()
  //           (assert-equal
  //            (defmacro-to-lambda-form
  //              '(defmacro foo (&rest x)
  //                 x))
  //            '(lambda (exp env)
  //               (define-values x
  //                 (rest exp))
  //               x))))
  //     (it "(defmacro foo (&rest ...) ...)"
  //         (fn ()
  //           (assert-equal
  //            (defmacro-to-lambda-form
  //              '(defmacro foo (x &rest y)
  //                 x))
  //            '(lambda (exp env)
  //               (define-values (x . y)
  //                 (rest exp))
  //               x))))))
  return describe('js/switch', function (): any {
    it('(js/switch x (case "foo" ...) (default ...))', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'statement',
          }
        ),
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
    it('(js/switch x (case "foo" ...) (default ...)), return', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'return',
          }
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
    return it('(js/switch x (case "foo" ...) (default ...)), return', function (): any {
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
          compilationEnvironment,
          {
            language: 'JavaScript',
            expressionType: 'expression',
          }
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
});

describe('compilation options', function (): any {
  return describe('inlineLispSources', function (): any {
    it('(define (foo x) x)', function (): any {
      return assertEqual(
        compile(
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
            language: 'JavaScript',
            inlineLispSources: true,
          }
        ),
        'function foo(x) {\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          "foo.lispSource = [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];"
      );
    });
    xit('(define foo (lambda (foo x) x))', function (): any {
      return assertEqual(
        compile(
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
            language: 'JavaScript',
            inlineLispSources: true,
          }
        ),
        'const foo = function (x) {\n' +
          '  return x;\n' +
          '};\n' +
          '\n' +
          "foo.lispSource = [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')];"
      );
    });
    return it('(define foo (lambda (foo x) x))', function (): any {
      return assertEqual(
        compile(
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
            language: 'JavaScript',
            inlineLispSources: true,
          }
        ),
        'async function foo(x) {\n' +
          '  return x;\n' +
          '}\n' +
          '\n' +
          "foo.lispSource = [Symbol.for('define/async'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')];"
      );
    });
  });
});

describe('assert', function (): any {
  it('(assert #t)', function (): any {
    return assertEqual(
      compile(
        [Symbol.for('assert'), Symbol.for('#t')],
        compilationEnvironment,
        {
          language: 'JavaScript',
        }
      ),
      'console.assert(true)'
    );
  });
  return it('(assert #t "test")', function (): any {
    return assertEqual(
      compile(
        [Symbol.for('assert'), Symbol.for('#t'), 'test'],
        compilationEnvironment,
        {
          language: 'JavaScript',
        }
      ),
      "console.assert(true, 'test')"
    );
  });
});

describe('display', function (): any {
  it('(display #t)', function (): any {
    return assertEqual(
      compile(
        [Symbol.for('display'), Symbol.for('#t')],
        compilationEnvironment,
        {
          language: 'JavaScript',
        }
      ),
      'console.log(true)'
    );
  });
  return it('(display #t "test")', function (): any {
    return assertEqual(
      compile(
        [Symbol.for('display'), Symbol.for('#t'), 'test'],
        compilationEnvironment,
        {
          language: 'JavaScript',
        }
      ),
      "console.log(true, 'test')"
    );
  });
});

describe('split-comments', function (): any {
  xit('1', function (): any {
    return assertEqual(splitComments(';;; Foo'), [';;; Foo']);
  });
  it('2', function (): any {
    return assertEqual(splitComments(';;; Foo\n'), [';;; Foo\n']);
  });
  xit('3', function (): any {
    return assertEqual(splitComments(';; Foo\n' + ';;; Bar'), [
      ';; Foo\n',
      ';;; Bar',
    ]);
  });
  return it('4', function (): any {
    return assertEqual(splitComments(';; Foo\n' + ';;; Bar\n'), [
      ';; Foo\n',
      ';;; Bar\n',
    ]);
  });
});
