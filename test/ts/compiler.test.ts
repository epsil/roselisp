/**
 * # Compiler tests
 *
 * Various compiler tests.
 */

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

import { readSyntax } from '../../src/ts/parser';

import { assertEqual, testMacro } from './test-util';

testMacro.ftype = 'macro';

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
        readSyntax(';; comment\n' + '(foo)'),
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
        readSyntax(';; multi-line\n' + ';; comment\n' + '(foo)'),
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
        readSyntax(';; multi-line\n' + ';;\n' + ';; comment\n' + '(foo)'),
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
        readSyntax(';; multiple\n' + '\n' + ';; comments\n' + '(foo)'),
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
        readSyntax(
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
          readSyntax(
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
        readSyntax(
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
        readSyntax(';; comment\n' + '(foo)'),
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
        readSyntax(';;; Foo\n' + '\n' + '(require "foo")'),
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
        readSyntax(';; Foo\n' + '\n' + ';;; Bar\n' + '(require "foo")'),
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
        readSyntax(';; Foo\n' + ';;; Bar\n' + '\n' + '(require "foo")'),
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
        readSyntax('(define foo\n' + '  ;; bar\n' + '  bar)'),
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
        readSyntax('(set! foo\n' + '  ;; bar\n' + '  bar)'),
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
        readSyntax(
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
        readSyntax(
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
  it('read-syntax', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readSyntax('(module m scheme\n' + '  (define foo\n' + '    `(foo)))'),
        compilationEnvironment,
        {
          language: 'javascript',
          optimize: true,
        }
      ),
      "const foo = [Symbol.for('foo')];"
    );
  });
  it('read-syntax, quasiquote', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readSyntax(
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
  it('read-syntax, quasiquoted list of pairs', function (): any {
    return assertEqual(
      compileWithEnvironment(
        readSyntax(
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
        readSyntax(
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
        readSyntax(
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
  it('(compile-with-environment \'(module m scheme (define (foo x) x)) compilation-environment (js/obj :language "javascript" :inline-lisp-sources #t :optimize #t))', function (): any {
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
  xit('(compile-with-environment \'(module m scheme (define foo (lambda (x) x))) compilation-environment (js/obj :language "javascript" :inline-lisp-sources #t :optimize #t))', function (): any {
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
  return it('(compile-with-environment \'(module m scheme (define foo (async (lambda (x) x)))) compilation-environment (js/obj :language "javascript" :inline-lisp-sources #t :optimize #t))', function (): any {
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
  it("(define-macro->lambda-form '(define-macro (foo x) x) (js/obj :exp 'exp :env 'env))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm(
        [
          Symbol.for('define-macro'),
          [Symbol.for('foo'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        {
          exp: Symbol.for('exp'),
          env: Symbol.for('env'),
        }
      ),
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
  it("(define-macro->lambda-form '(define-macro (foo &whole expression x) x) (js/obj :env 'env))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm(
        [
          Symbol.for('define-macro'),
          [
            Symbol.for('foo'),
            Symbol.for('&whole'),
            Symbol.for('expression'),
            Symbol.for('x'),
          ],
          Symbol.for('x'),
        ],
        {
          env: Symbol.for('env'),
        }
      ),
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
  it("(define-macro->lambda-form '(define-macro (foo &rest x) x) (js/obj :exp 'exp :env 'env))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm(
        [
          Symbol.for('define-macro'),
          [Symbol.for('foo'), Symbol.for('&rest'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        {
          exp: Symbol.for('exp'),
          env: Symbol.for('env'),
        }
      ),
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
  return it("(define-macro->lambda-form '(define-macro (foo x &rest y) x) (js/obj :exp 'exp :env 'env))", function (): any {
    return assertEqual(
      defineMacroToLambdaForm(
        [
          Symbol.for('define-macro'),
          [
            Symbol.for('foo'),
            Symbol.for('x'),
            Symbol.for('&rest'),
            Symbol.for('y'),
          ],
          Symbol.for('x'),
        ],
        {
          exp: Symbol.for('exp'),
          env: Symbol.for('env'),
        }
      ),
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
  it('(split-comments ";;; Foo\n' + '")', function (): any {
    return assertEqual(splitComments(';;; Foo\n'), [';;; Foo\n']);
  });
  xit('(split-comments ";;; Foo")', function (): any {
    return assertEqual(splitComments(';;; Foo'), [';;; Foo']);
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
