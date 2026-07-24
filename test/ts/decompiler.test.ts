import { I } from '../../src/ts/combinators';

import { decompile } from '../../src/ts/language';

import { writeToString } from '../../src/ts/printer';

import { sexp } from '../../src/ts/sexp';

import { assertEqual, testMacro } from './test-util';

describe('decompile', function (): any {
  it('(decompile "true;")', function (): any {
    return assertEqual(decompile('true;'), true);
  });
  it('(decompile "false")', function (): any {
    return assertEqual(decompile('false'), false);
  });
  it('(decompile "const foo = undefined;")', function (): any {
    return assertEqual(decompile('const foo = undefined;'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      Symbol.for('undefined'),
    ]);
  });
  it('(decompile "const foo = null;")', function (): any {
    return assertEqual(decompile('const foo = null;'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      Symbol.for('js/null'),
    ]);
  });
  it('(decompile "const foo = this;")', function (): any {
    return assertEqual(decompile('const foo = this;'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      Symbol.for('this'),
    ]);
  });
  it('(decompile "0")', function (): any {
    return assertEqual(decompile('0'), 0);
  });
  it('(decompile "1")', function (): any {
    return assertEqual(decompile('1'), 1);
  });
  it('(decompile "-1")', function (): any {
    return assertEqual(decompile('-1'), -1);
  });
  it('(decompile "\'foo\'")', function (): any {
    return assertEqual(decompile("'foo'"), 'foo');
  });
  it('(decompile "const foo = `bar`;")', function (): any {
    return assertEqual(decompile('const foo = `bar`;'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      'bar',
    ]);
  });
  it('(decompile "const foo = bar`baz`;")', function (): any {
    return assertEqual(decompile('const foo = bar`baz`;'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      [Symbol.for('js/tag'), Symbol.for('bar'), 'baz'],
    ]);
  });
  it('(decompile "const foo = `bar\n' + '\\\\`baz`;")', function (): any {
    return assertEqual(decompile('const foo = `bar\n' + '\\`baz`;'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      'bar\n' + '`baz',
    ]);
  });
  it('(decompile "/foo/")', function (): any {
    return assertEqual(decompile('/foo/'), [Symbol.for('js/regexp'), 'foo']);
  });
  it('(decompile "/foo/g")', function (): any {
    return assertEqual(decompile('/foo/g'), [
      Symbol.for('js/regexp'),
      'foo',
      'g',
    ]);
  });
  it('(decompile "const exp = /.*/;")', function (): any {
    return assertEqual(decompile('const exp = /.*/;'), [
      Symbol.for('define'),
      Symbol.for('exp'),
      [Symbol.for('js/regexp'), '.*'],
    ]);
  });
  it('(decompile "const exp = /.*/;")', function (): any {
    return assertEqual(decompile('const exp = /.*/;'), [
      Symbol.for('define'),
      Symbol.for('exp'),
      [Symbol.for('js/regexp'), '.*'],
    ]);
  });
  it('(decompile "[]")', function (): any {
    return assertEqual(decompile('[]'), [Symbol.for('list')]);
  });
  it('(decompile "const foo = [];")', function (): any {
    return assertEqual(decompile('const foo = [];'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      [Symbol.for('list')],
    ]);
  });
  it('(decompile "[1, 2, 3]")', function (): any {
    return assertEqual(decompile('[1, 2, 3]'), [Symbol.for('list'), 1, 2, 3]);
  });
  it('(decompile "[...x]")', function (): any {
    return assertEqual(decompile('[...x]'), [
      Symbol.for('append'),
      Symbol.for('x'),
    ]);
  });
  it('(decompile "[x, ...y]")', function (): any {
    return assertEqual(decompile('[x, ...y]'), [
      Symbol.for('append'),
      [Symbol.for('list'), Symbol.for('x')],
      Symbol.for('y'),
    ]);
  });
  it('(decompile "x[0]")', function (): any {
    return assertEqual(decompile('x[0]'), [
      Symbol.for('aget'),
      Symbol.for('x'),
      0,
    ]);
  });
  it('(decompile "x[0](a, b)")', function (): any {
    return assertEqual(decompile('x[0](a, b)'), [
      [Symbol.for('aget'), Symbol.for('x'), 0],
      Symbol.for('a'),
      Symbol.for('b'),
    ]);
  });
  it('(decompile "x[0][1]")', function (): any {
    return assertEqual(decompile('x[0][1]'), [
      Symbol.for('aget'),
      Symbol.for('x'),
      0,
      1,
    ]);
  });
  it('(decompile "x[len]")', function (): any {
    return assertEqual(decompile('x[len]'), [
      Symbol.for('oget'),
      Symbol.for('x'),
      Symbol.for('len'),
    ]);
  });
  it('(decompile "x[len - 1]")', function (): any {
    return assertEqual(decompile('x[len - 1]'), [
      Symbol.for('oget'),
      Symbol.for('x'),
      [Symbol.for('-'), Symbol.for('len'), 1],
    ]);
  });
  it('(decompile "x[0] = 1")', function (): any {
    return assertEqual(decompile('x[0] = 1'), [
      Symbol.for('aset!'),
      Symbol.for('x'),
      0,
      1,
    ]);
  });
  it('(decompile "x[\'foo\'] = 1")', function (): any {
    return assertEqual(decompile("x['foo'] = 1"), [
      Symbol.for('oset!'),
      Symbol.for('x'),
      'foo',
      1,
    ]);
  });
  it('(decompile "!foo")', function (): any {
    return assertEqual(decompile('!foo'), [
      Symbol.for('not'),
      Symbol.for('foo'),
    ]);
  });
  it('(decompile "1 + 2")', function (): any {
    return assertEqual(decompile('1 + 2'), [Symbol.for('+'), 1, 2]);
  });
  it('(decompile "1 + 2 + 3")', function (): any {
    return assertEqual(decompile('1 + 2 + 3'), [Symbol.for('+'), 1, 2, 3]);
  });
  it('(decompile "1 + \'\'")', function (): any {
    return assertEqual(decompile("1 + ''"), [
      Symbol.for('string-append'),
      1,
      '',
    ]);
  });
  it('(decompile "1 + 2 + \'\'")', function (): any {
    return assertEqual(decompile("1 + 2 + ''"), [
      Symbol.for('string-append'),
      1,
      2,
      '',
    ]);
  });
  it('(decompile "-1")', function (): any {
    return assertEqual(decompile('-1'), -1);
  });
  it('(decompile "-(1)")', function (): any {
    return assertEqual(decompile('-(1)'), -1);
  });
  it('(decompile "-x")', function (): any {
    return assertEqual(decompile('-x'), [Symbol.for('-'), Symbol.for('x')]);
  });
  it('(decompile "1 - 2")', function (): any {
    return assertEqual(decompile('1 - 2'), [Symbol.for('-'), 1, 2]);
  });
  it('(decompile "1 - 2 - 3")', function (): any {
    return assertEqual(decompile('1 - 2 - 3'), [Symbol.for('-'), 1, 2, 3]);
  });
  it('(decompile "1 - (2 - 3)")', function (): any {
    return assertEqual(decompile('1 - (2 - 3)'), [
      Symbol.for('-'),
      1,
      [Symbol.for('-'), 2, 3],
    ]);
  });
  it('(decompile "1 * 2")', function (): any {
    return assertEqual(decompile('1 * 2'), [Symbol.for('*'), 1, 2]);
  });
  it('(decompile "1 * 2 * 3")', function (): any {
    return assertEqual(decompile('1 * 2 * 3'), [Symbol.for('*'), 1, 2, 3]);
  });
  it('(decompile "1 / 2")', function (): any {
    return assertEqual(decompile('1 / 2'), [Symbol.for('/'), 1, 2]);
  });
  xit('(decompile "1 / 2 / 3")', function (): any {
    return assertEqual(decompile('1 / 2 / 3'), [Symbol.for('/'), 1, 2, 3]);
  });
  it('(decompile "x && y")', function (): any {
    return assertEqual(decompile('x && y'), [
      Symbol.for('and'),
      Symbol.for('x'),
      Symbol.for('y'),
    ]);
  });
  it('(decompile "x && y && z")', function (): any {
    return assertEqual(decompile('x && y && z'), [
      Symbol.for('and'),
      Symbol.for('x'),
      Symbol.for('y'),
      Symbol.for('z'),
    ]);
  });
  it("(decompile \"typeof x === 'number' && typeof y === 'number'\")", function (): any {
    return assertEqual(
      decompile("typeof x === 'number' && typeof y === 'number'"),
      [
        Symbol.for('and'),
        [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'number'],
        [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('y')], 'number'],
      ]
    );
  });
  it('(decompile "x || y")', function (): any {
    return assertEqual(decompile('x || y'), [
      Symbol.for('or'),
      Symbol.for('x'),
      Symbol.for('y'),
    ]);
  });
  it('(decompile "x || y || z")', function (): any {
    return assertEqual(decompile('x || y || z'), [
      Symbol.for('or'),
      Symbol.for('x'),
      Symbol.for('y'),
      Symbol.for('z'),
    ]);
  });
  it('(decompile "x === y")', function (): any {
    return assertEqual(decompile('x === y'), [
      Symbol.for('eq?'),
      Symbol.for('x'),
      Symbol.for('y'),
    ]);
  });
  it('(decompile "x !== y")', function (): any {
    return assertEqual(decompile('x !== y'), [
      Symbol.for('not'),
      [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('y')],
    ]);
  });
  it('(decompile "x in y")', function (): any {
    return assertEqual(decompile('x in y'), [
      Symbol.for('js/in'),
      Symbol.for('x'),
      Symbol.for('y'),
    ]);
  });
  it('(decompile "x instanceof y")', function (): any {
    return assertEqual(decompile('x instanceof y'), [
      Symbol.for('is-a?'),
      Symbol.for('x'),
      Symbol.for('y'),
    ]);
  });
  it('(decompile "typeof x")', function (): any {
    return assertEqual(decompile('typeof x'), [
      Symbol.for('type-of'),
      Symbol.for('x'),
    ]);
  });
  it('(decompile "foo(bar);")', function (): any {
    return assertEqual(decompile('foo(bar);'), [
      Symbol.for('foo'),
      Symbol.for('bar'),
    ]);
  });
  it('(decompile "foo(bar);")', function (): any {
    return assertEqual(decompile('foo(bar);'), [
      Symbol.for('foo'),
      Symbol.for('bar'),
    ]);
  });
  it('(decompile "foo(bar);" :module #t)', function (): any {
    return assertEqual(decompile('foo(bar);', Symbol.for(':module'), true), [
      Symbol.for('module'),
      Symbol.for('m'),
      Symbol.for('scheme'),
      [Symbol.for('foo'), Symbol.for('bar')],
    ]);
  });
  it('(decompile "foo(bar);" :module #t)', function (): any {
    return assertEqual(decompile('foo(bar);', Symbol.for(':module'), true), [
      Symbol.for('module'),
      Symbol.for('m'),
      Symbol.for('scheme'),
      [Symbol.for('foo'), Symbol.for('bar')],
    ]);
  });
  it('(decompile "foo(\'bar\');")', function (): any {
    return assertEqual(decompile("foo('bar');"), [Symbol.for('foo'), 'bar']);
  });
  it('(decompile "foo(\'bar\');")', function (): any {
    return assertEqual(decompile("foo('bar');"), [Symbol.for('foo'), 'bar']);
  });
  it("(decompile \"foo('bar', 'baz');\")", function (): any {
    return assertEqual(decompile("foo('bar', 'baz');"), [
      Symbol.for('foo'),
      'bar',
      'baz',
    ]);
  });
  it("(decompile \"foo('bar', 'baz');\")", function (): any {
    return assertEqual(decompile("foo('bar', 'baz');"), [
      Symbol.for('foo'),
      'bar',
      'baz',
    ]);
  });
  it('(decompile "foo(1, 2, 3);")', function (): any {
    return assertEqual(decompile('foo(1, 2, 3);'), [
      Symbol.for('foo'),
      1,
      2,
      3,
    ]);
  });
  it('(decompile "foo(...args);")', function (): any {
    return assertEqual(decompile('foo(...args);'), [
      Symbol.for('apply'),
      Symbol.for('foo'),
      Symbol.for('args'),
    ]);
  });
  it('(decompile "foo(x, ...args);")', function (): any {
    return assertEqual(decompile('foo(x, ...args);'), [
      Symbol.for('apply'),
      Symbol.for('foo'),
      Symbol.for('x'),
      Symbol.for('args'),
    ]);
  });
  it('(decompile "foo(...args, x);")', function (): any {
    return assertEqual(decompile('foo(...args, x);'), [
      Symbol.for('apply'),
      Symbol.for('foo'),
      [
        Symbol.for('append'),
        Symbol.for('args'),
        [Symbol.for('list'), Symbol.for('x')],
      ],
    ]);
  });
  it('(decompile "foo(x, ...args, y);")', function (): any {
    return assertEqual(decompile('foo(x, ...args, y);'), [
      Symbol.for('apply'),
      Symbol.for('foo'),
      [
        Symbol.for('append'),
        [Symbol.for('list'), Symbol.for('x')],
        Symbol.for('args'),
        [Symbol.for('list'), Symbol.for('y')],
      ],
    ]);
  });
  xit('(decompile "// comment\n' + 'foo(bar);")', function (): any {
    return assertEqual(decompile('// comment\n' + 'foo(bar);'), [
      Symbol.for('foo'),
      Symbol.for('bar'),
    ]);
  });
  it('(decompile "x = 1;")', function (): any {
    return assertEqual(decompile('x = 1;'), [
      Symbol.for('set!'),
      Symbol.for('x'),
      1,
    ]);
  });
  it('(decompile "x += 1;")', function (): any {
    return assertEqual(decompile('x += 1;'), [
      Symbol.for('set!'),
      Symbol.for('x'),
      [Symbol.for('+'), Symbol.for('x'), 1],
    ]);
  });
  it('(decompile "let x = 1;")', function (): any {
    return assertEqual(decompile('let x = 1;'), [
      Symbol.for('define'),
      Symbol.for('x'),
      1,
    ]);
  });
  it('(decompile "let x = undefined;")', function (): any {
    return assertEqual(decompile('let x = undefined;'), [
      Symbol.for('define'),
      Symbol.for('x'),
      Symbol.for('undefined'),
    ]);
  });
  it('(decompile "let x;")', function (): any {
    return assertEqual(decompile('let x;'), [
      Symbol.for('define'),
      Symbol.for('x'),
    ]);
  });
  it('(decompile "let x = 1, y = 2;")', function (): any {
    return assertEqual(decompile('let x = 1, y = 2;'), [
      Symbol.for('begin'),
      [Symbol.for('define'), Symbol.for('x'), 1],
      [Symbol.for('define'), Symbol.for('y'), 2],
    ]);
  });
  it('(decompile "let x = 1, y = 2;\n' + 'let z = 3;")', function (): any {
    return assertEqual(decompile('let x = 1, y = 2;\n' + 'let z = 3;'), [
      Symbol.for('begin'),
      [Symbol.for('define'), Symbol.for('x'), 1],
      [Symbol.for('define'), Symbol.for('y'), 2],
      [Symbol.for('define'), Symbol.for('z'), 3],
    ]);
  });
  it('(decompile "const x = 1")', function (): any {
    return assertEqual(decompile('const x = 1'), [
      Symbol.for('define'),
      Symbol.for('x'),
      1,
    ]);
  });
  it('(decompile "let [x] = arr;")', function (): any {
    return assertEqual(decompile('let [x] = arr;'), [
      Symbol.for('define-values'),
      [Symbol.for('x')],
      Symbol.for('arr'),
    ]);
  });
  it('(decompile "let [x, y] = arr;")', function (): any {
    return assertEqual(decompile('let [x, y] = arr;'), [
      Symbol.for('define-values'),
      [Symbol.for('x'), Symbol.for('y')],
      Symbol.for('arr'),
    ]);
  });
  it('(decompile "[x, y] = arr;")', function (): any {
    return assertEqual(decompile('[x, y] = arr;'), [
      Symbol.for('set!-values'),
      [Symbol.for('x'), Symbol.for('y')],
      Symbol.for('arr'),
    ]);
  });
  it('(decompile "let [x, ...y] = arr;")', function (): any {
    return assertEqual(decompile('let [x, ...y] = arr;'), [
      Symbol.for('define-values'),
      [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')],
      Symbol.for('arr'),
    ]);
  });
  it('(decompile "let [, y] = arr;")', function (): any {
    return assertEqual(decompile('let [, y] = arr;'), [
      Symbol.for('define-values'),
      [Symbol.for('_'), Symbol.for('y')],
      Symbol.for('arr'),
    ]);
  });
  it('(decompile "let {x, y} = obj;")', function (): any {
    return assertEqual(decompile('let {x, y} = obj;'), [
      Symbol.for('define-fields'),
      [Symbol.for('x'), Symbol.for('y')],
      Symbol.for('obj'),
    ]);
  });
  it('(decompile "({x, y} = obj);")', function (): any {
    return assertEqual(decompile('({x, y} = obj);'), [
      Symbol.for('set!-fields'),
      [Symbol.for('x'), Symbol.for('y')],
      Symbol.for('obj'),
    ]);
  });
  it('(decompile "let {x: y, z} = obj;")', function (): any {
    return assertEqual(decompile('let {x: y, z} = obj;'), [
      Symbol.for('define-fields'),
      [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')],
      Symbol.for('obj'),
    ]);
  });
  it('(decompile "x.y;")', function (): any {
    return assertEqual(decompile('x.y;'), [
      Symbol.for('get-field'),
      Symbol.for('y'),
      Symbol.for('x'),
    ]);
  });
  it('(decompile "x?.y;")', function (): any {
    return assertEqual(decompile('x?.y;'), [
      Symbol.for('and'),
      [Symbol.for('field-bound?'), Symbol.for('y'), Symbol.for('x')],
      [Symbol.for('get-field'), Symbol.for('y'), Symbol.for('x')],
    ]);
  });
  it('(decompile "foo()?.y;")', function (): any {
    return assertEqual(decompile('foo()?.y;'), [
      Symbol.for('~>'),
      [Symbol.for('foo')],
      [
        Symbol.for('and'),
        [Symbol.for('field-bound?'), Symbol.for('y'), Symbol.for('_')],
        [Symbol.for('get-field'), Symbol.for('y'), Symbol.for('_')],
      ],
    ]);
  });
  it('(decompile "x.y = z;")', function (): any {
    return assertEqual(decompile('x.y = z;'), [
      Symbol.for('set-field!'),
      Symbol.for('y'),
      Symbol.for('x'),
      Symbol.for('z'),
    ]);
  });
  it('(decompile "x.y();")', function (): any {
    return assertEqual(decompile('x.y();'), [
      Symbol.for('send'),
      Symbol.for('x'),
      Symbol.for('y'),
    ]);
  });
  it('(decompile "x?.y();")', function (): any {
    return assertEqual(decompile('x?.y();'), [
      Symbol.for('and'),
      [Symbol.for('field-bound?'), Symbol.for('y'), Symbol.for('x')],
      [Symbol.for('send'), Symbol.for('x'), Symbol.for('y')],
    ]);
  });
  it('(decompile "foo()?.y();")', function (): any {
    return assertEqual(decompile('foo()?.y();'), [
      Symbol.for('~>'),
      [Symbol.for('foo')],
      [
        Symbol.for('and'),
        [Symbol.for('field-bound?'), Symbol.for('y'), Symbol.for('_')],
        [Symbol.for('send'), Symbol.for('_'), Symbol.for('y')],
      ],
    ]);
  });
  it('(decompile "x.y(z);")', function (): any {
    return assertEqual(decompile('x.y(z);'), [
      Symbol.for('send'),
      Symbol.for('x'),
      Symbol.for('y'),
      Symbol.for('z'),
    ]);
  });
  it('(decompile "x.y(...z);")', function (): any {
    return assertEqual(decompile('x.y(...z);'), [
      Symbol.for('send/apply'),
      Symbol.for('x'),
      Symbol.for('y'),
      Symbol.for('z'),
    ]);
  });
  it('(decompile "function I(x) {\n' + '  return x;}")', function (): any {
    return assertEqual(decompile('function I(x) {\n' + '  return x;}'), [
      Symbol.for('define'),
      [Symbol.for('I'), Symbol.for('x')],
      Symbol.for('x'),
    ]);
  });
  it(
    '(decompile "function I(x) {\n' + '  foo();\n' + '  return x;\n' + '}")',
    function (): any {
      return assertEqual(
        decompile('function I(x) {\n' + '  foo();\n' + '  return x;\n' + '}'),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          [Symbol.for('foo')],
          Symbol.for('x'),
        ]
      );
    }
  );
  it(
    '(decompile "function I(x: any, y?: any) {\n' + '  return x;\n' + '}")',
    function (): any {
      return assertEqual(
        decompile('function I(x: any, y?: any) {\n' + '  return x;\n' + '}'),
        [
          Symbol.for('define'),
          [
            Symbol.for('I'),
            [Symbol.for('x'), Symbol.for(':'), Symbol.for('Any')],
            [Symbol.for('y'), Symbol.for('undefined')],
          ],
          Symbol.for('x'),
        ]
      );
    }
  );
  it(
    '(decompile "function I(x: any, y: any = true) {\n' +
      '  return x;\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'function I(x: any, y: any = true) {\n' + '  return x;\n' + '}'
        ),
        [
          Symbol.for('define'),
          [
            Symbol.for('I'),
            [Symbol.for('x'), Symbol.for(':'), Symbol.for('Any')],
            [Symbol.for('y'), Symbol.for(':'), Symbol.for('Any'), true],
          ],
          Symbol.for('x'),
        ]
      );
    }
  );
  it(
    '(decompile "function I(x: number, y: number = 1) {\n' +
      '  return x;\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'function I(x: number, y: number = 1) {\n' + '  return x;\n' + '}'
        ),
        [
          Symbol.for('define'),
          [
            Symbol.for('I'),
            [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')],
            [Symbol.for('y'), Symbol.for(':'), Symbol.for('Number'), 1],
          ],
          Symbol.for('x'),
        ]
      );
    }
  );
  it(
    '(decompile "function foo(x = 1) {\n' + '  return x;\n' + '}")',
    function (): any {
      return assertEqual(
        decompile('function foo(x = 1) {\n' + '  return x;\n' + '}'),
        [
          Symbol.for('define'),
          [Symbol.for('foo'), [Symbol.for('x'), 1]],
          Symbol.for('x'),
        ]
      );
    }
  );
  it(
    '(decompile "function foo(...args) {\n' + '  return args;\n' + '}")',
    function (): any {
      return assertEqual(
        decompile('function foo(...args) {\n' + '  return args;\n' + '}'),
        [
          Symbol.for('define'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('args')],
          Symbol.for('args'),
        ]
      );
    }
  );
  it(
    '(decompile "function I(x) {\n' +
      '  if (x) {\n' +
      '    return x;\n' +
      '  } else {\n' +
      '    return false;\n' +
      '  }\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'function I(x) {\n' +
            '  if (x) {\n' +
            '    return x;\n' +
            '  } else {\n' +
            '    return false;\n' +
            '  }\n' +
            '}'
        ),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          [Symbol.for('if'), Symbol.for('x'), Symbol.for('x'), false],
        ]
      );
    }
  );
  it(
    '(decompile "function I(x) {\n' +
      '  if (x) {\n' +
      '    return x;\n' +
      '  } else if (false) {\n' +
      '    return false;\n' +
      '  } else {\n' +
      '    return false;\n' +
      '  }\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'function I(x) {\n' +
            '  if (x) {\n' +
            '    return x;\n' +
            '  } else if (false) {\n' +
            '    return false;\n' +
            '  } else {\n' +
            '    return false;\n' +
            '  }\n' +
            '}'
        ),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          [
            Symbol.for('cond'),
            [Symbol.for('x'), Symbol.for('x')],
            [false, false],
            [Symbol.for('else'), false],
          ],
        ]
      );
    }
  );
  it(
    '(decompile "let I = function (x) {\n' + '  return x;\n' + '};")',
    function (): any {
      return assertEqual(
        decompile('let I = function (x) {\n' + '  return x;\n' + '};'),
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        ]
      );
    }
  );
  it(
    '(decompile "let foo = function (...args) {\n' +
      '  return args;\n' +
      '};")',
    function (): any {
      return assertEqual(
        decompile(
          'let foo = function (...args) {\n' + '  return args;\n' + '};'
        ),
        [
          Symbol.for('define'),
          Symbol.for('foo'),
          [Symbol.for('lambda'), Symbol.for('args'), Symbol.for('args')],
        ]
      );
    }
  );
  it(
    '(decompile "let I = (x) => {\n' + '  return x;\n' + '};")',
    function (): any {
      return assertEqual(
        decompile('let I = (x) => {\n' + '  return x;\n' + '};'),
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [Symbol.for('js/arrow'), [Symbol.for('x')], Symbol.for('x')],
        ]
      );
    }
  );
  it(
    '(decompile "let I = (x: any) => {\n' + '  return x;\n' + '};")',
    function (): any {
      return assertEqual(
        decompile('let I = (x: any) => {\n' + '  return x;\n' + '};'),
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [
            Symbol.for('js/arrow'),
            [[Symbol.for('x'), Symbol.for(':'), Symbol.for('Any')]],
            Symbol.for('x'),
          ],
        ]
      );
    }
  );
  it('(decompile "if (true) {\n' + "  foo('bar');\n" + '}")', function (): any {
    return assertEqual(decompile('if (true) {\n' + "  foo('bar');\n" + '}'), [
      Symbol.for('when'),
      true,
      [Symbol.for('foo'), 'bar'],
    ]);
  });
  it('(decompile "if (!foo) {\n' + "  bar('baz');\n" + '}")', function (): any {
    return assertEqual(decompile('if (!foo) {\n' + "  bar('baz');\n" + '}'), [
      Symbol.for('unless'),
      Symbol.for('foo'),
      [Symbol.for('bar'), 'baz'],
    ]);
  });
  it(
    '(decompile "if (true) {\n' +
      "  foo('bar');\n" +
      '} else {\n' +
      "  bar('baz');\n" +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'if (true) {\n' +
            "  foo('bar');\n" +
            '} else {\n' +
            "  bar('baz');\n" +
            '}'
        ),
        [
          Symbol.for('if'),
          true,
          [Symbol.for('foo'), 'bar'],
          [Symbol.for('bar'), 'baz'],
        ]
      );
    }
  );
  it(
    '(decompile "if (x) {\n' +
      '  if (y) {\n' +
      "    foo('bar');\n" +
      '  }\n' +
      '} else {\n' +
      "  bar('baz');\n" +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            '  if (y) {\n' +
            "    foo('bar');\n" +
            '  }\n' +
            '} else {\n' +
            "  bar('baz');\n" +
            '}'
        ),
        [
          Symbol.for('if'),
          Symbol.for('x'),
          [Symbol.for('when'), Symbol.for('y'), [Symbol.for('foo'), 'bar']],
          [Symbol.for('bar'), 'baz'],
        ]
      );
    }
  );
  it(
    '(decompile "if (x) {\n' +
      '  if (y) {\n' +
      "    foo('bar');\n" +
      '  }\n' +
      '} else {\n' +
      '  if (z) {\n' +
      "    bar('baz');\n" +
      '  }\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            '  if (y) {\n' +
            "    foo('bar');\n" +
            '  }\n' +
            '} else {\n' +
            '  if (z) {\n' +
            "    bar('baz');\n" +
            '  }\n' +
            '}'
        ),
        [
          Symbol.for('cond'),
          [
            Symbol.for('x'),
            [Symbol.for('when'), Symbol.for('y'), [Symbol.for('foo'), 'bar']],
          ],
          [Symbol.for('z'), [Symbol.for('bar'), 'baz']],
        ]
      );
    }
  );
  it(
    '(decompile "if (x) {\n' +
      '  foo();\n' +
      '  bar();\n' +
      '} else if (y) {\n' +
      '  baz();\n' +
      '  quux();\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            '  foo();\n' +
            '  bar();\n' +
            '} else if (y) {\n' +
            '  baz();\n' +
            '  quux();\n' +
            '}'
        ),
        [
          Symbol.for('cond'),
          [Symbol.for('x'), [Symbol.for('foo')], [Symbol.for('bar')]],
          [Symbol.for('y'), [Symbol.for('baz')], [Symbol.for('quux')]],
        ]
      );
    }
  );
  it(
    '(decompile "if (x) {\n' +
      '  if (y) {\n' +
      "    foo('bar');\n" +
      '  }\n' +
      '} else {\n' +
      '  if (z) {\n' +
      "    bar('baz');\n" +
      '  } else {\n' +
      "    baz('quux');\n" +
      '  }\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            '  if (y) {\n' +
            "    foo('bar');\n" +
            '  }\n' +
            '} else {\n' +
            '  if (z) {\n' +
            "    bar('baz');\n" +
            '  } else {\n' +
            "    baz('quux');\n" +
            '  }\n' +
            '}'
        ),
        [
          Symbol.for('cond'),
          [
            Symbol.for('x'),
            [Symbol.for('when'), Symbol.for('y'), [Symbol.for('foo'), 'bar']],
          ],
          [Symbol.for('z'), [Symbol.for('bar'), 'baz']],
          [Symbol.for('else'), [Symbol.for('baz'), 'quux']],
        ]
      );
    }
  );
  it(
    '(decompile "if (x) {\n' +
      '  if (y) {\n' +
      "    foo('bar');\n" +
      '  }\n' +
      '} else {\n' +
      '  if (!z) {\n' +
      "    bar('baz');\n" +
      '  } else {\n' +
      "    baz('quux');\n" +
      '  }\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            '  if (y) {\n' +
            "    foo('bar');\n" +
            '  }\n' +
            '} else {\n' +
            '  if (!z) {\n' +
            "    bar('baz');\n" +
            '  } else {\n' +
            "    baz('quux');\n" +
            '  }\n' +
            '}'
        ),
        [
          Symbol.for('cond'),
          [
            Symbol.for('x'),
            [Symbol.for('when'), Symbol.for('y'), [Symbol.for('foo'), 'bar']],
          ],
          [
            [Symbol.for('not'), Symbol.for('z')],
            [Symbol.for('bar'), 'baz'],
          ],
          [Symbol.for('else'), [Symbol.for('baz'), 'quux']],
        ]
      );
    }
  );
  it(
    '(decompile "if (x) {\n' +
      "  foo('bar');\n" +
      '} else if (y) {\n' +
      "  bar('baz');\n" +
      '} else {\n' +
      "  baz('quux');\n" +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            "  foo('bar');\n" +
            '} else if (y) {\n' +
            "  bar('baz');\n" +
            '} else {\n' +
            "  baz('quux');\n" +
            '}'
        ),
        [
          Symbol.for('cond'),
          [Symbol.for('x'), [Symbol.for('foo'), 'bar']],
          [Symbol.for('y'), [Symbol.for('bar'), 'baz']],
          [Symbol.for('else'), [Symbol.for('baz'), 'quux']],
        ]
      );
    }
  );
  it(
    '(decompile "if (x) {\n' +
      '  foo();\n' +
      '} else {\n' +
      '  bar();\n' +
      '  baz();\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            '  foo();\n' +
            '} else {\n' +
            '  bar();\n' +
            '  baz();\n' +
            '}'
        ),
        [
          Symbol.for('cond'),
          [Symbol.for('x'), [Symbol.for('foo')]],
          [Symbol.for('else'), [Symbol.for('bar')], [Symbol.for('baz')]],
        ]
      );
    }
  );
  it('(decompile "let x = true ? foo : bar;")', function (): any {
    return assertEqual(decompile('let x = true ? foo : bar;'), [
      Symbol.for('define'),
      Symbol.for('x'),
      [Symbol.for('if'), true, Symbol.for('foo'), Symbol.for('bar')],
    ]);
  });
  it('(decompile "let x = 1 ? foo : 2 ? bar : baz")', function (): any {
    return assertEqual(decompile('let x = 1 ? foo : 2 ? bar : baz'), [
      Symbol.for('define'),
      Symbol.for('x'),
      [
        Symbol.for('cond'),
        [1, Symbol.for('foo')],
        [2, Symbol.for('bar')],
        [Symbol.for('else'), Symbol.for('baz')],
      ],
    ]);
  });
  it('(decompile "while (foo) {\n' + '  bar();}")', function (): any {
    return assertEqual(decompile('while (foo) {\n' + '  bar();}'), [
      Symbol.for('do'),
      [],
      [[Symbol.for('not'), Symbol.for('foo')]],
      [Symbol.for('bar')],
    ]);
  });
  it(
    '(decompile "do {\n' + '  bar();\n' + '} while (foo);")',
    function (): any {
      return assertEqual(
        decompile('do {\n' + '  bar();\n' + '} while (foo);'),
        [Symbol.for('js/do-while'), [Symbol.for('bar')], Symbol.for('foo')]
      );
    }
  );
  it(
    '(decompile "for (let i = 0; i < 10; i++) {\n' +
      '  foo();\n' +
      '  break;\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'for (let i = 0; i < 10; i++) {\n' + '  foo();\n' + '  break;\n' + '}'
        ),
        [
          Symbol.for('for'),
          [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]],
          [Symbol.for('foo')],
          [Symbol.for('break')],
        ]
      );
    }
  );
  it(
    '(decompile "for (i = 0; i < arr.length; i++) {\n' + '  foo();\n' + '}")',
    function (): any {
      return assertEqual(
        decompile('for (i = 0; i < arr.length; i++) {\n' + '  foo();\n' + '}'),
        [
          Symbol.for('for'),
          [
            [
              Symbol.for('i'),
              [
                Symbol.for('range'),
                0,
                [
                  Symbol.for('get-field'),
                  Symbol.for('length'),
                  Symbol.for('arr'),
                ],
              ],
            ],
          ],
          [Symbol.for('foo')],
        ]
      );
    }
  );
  it(
    '(decompile "for (let i = 10; i > 0; i--) {\n' + '  foo();\n' + '}")',
    function (): any {
      return assertEqual(
        decompile('for (let i = 10; i > 0; i--) {\n' + '  foo();\n' + '}'),
        [
          Symbol.for('for'),
          [[Symbol.for('i'), [Symbol.for('range'), 10, 0, -1]]],
          [Symbol.for('foo')],
        ]
      );
    }
  );
  it(
    '(decompile "for (let i = 0, j = 0; i < 10; i++, j++) {\n' +
      '  foo();\n' +
      '  break bar;\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'for (let i = 0, j = 0; i < 10; i++, j++) {\n' +
            '  foo();\n' +
            '  break bar;\n' +
            '}'
        ),
        [
          Symbol.for('do'),
          [
            [Symbol.for('i'), 0, [Symbol.for('+'), Symbol.for('i'), 1]],
            [Symbol.for('j'), 0, [Symbol.for('+'), Symbol.for('j'), 1]],
          ],
          [[Symbol.for('not'), [Symbol.for('<'), Symbol.for('i'), 10]]],
          [Symbol.for('foo')],
          [Symbol.for('break'), Symbol.for('bar')],
        ]
      );
    }
  );
  it(
    '(decompile "for (let x of foo) {\n' +
      '  bar();\n' +
      '  continue;\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'for (let x of foo) {\n' + '  bar();\n' + '  continue;\n' + '}'
        ),
        [
          Symbol.for('for'),
          [[Symbol.for('x'), Symbol.for('foo')]],
          [Symbol.for('bar')],
          [Symbol.for('continue')],
        ]
      );
    }
  );
  it(
    '(decompile "for (const [name, value] of entries) {\n' +
      '  result.insert(value, [name]);\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'for (const [name, value] of entries) {\n' +
            '  result.insert(value, [name]);\n' +
            '}'
        ),
        [
          Symbol.for('for'),
          [[Symbol.for('x'), Symbol.for('entries')]],
          [
            Symbol.for('define-values'),
            [Symbol.for('name'), Symbol.for('value')],
            Symbol.for('x'),
          ],
          [
            Symbol.for('send'),
            Symbol.for('result'),
            Symbol.for('insert'),
            Symbol.for('value'),
            [Symbol.for('list'), Symbol.for('name')],
          ],
        ]
      );
    }
  );
  it(
    '(decompile "for (const [name, value] of x) {\n' +
      '  result.insert(value, [name]);\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'for (const [name, value] of x) {\n' +
            '  result.insert(value, [name]);\n' +
            '}'
        ),
        [
          Symbol.for('for'),
          [[Symbol.for('x1'), Symbol.for('x')]],
          [
            Symbol.for('define-values'),
            [Symbol.for('name'), Symbol.for('value')],
            Symbol.for('x1'),
          ],
          [
            Symbol.for('send'),
            Symbol.for('result'),
            Symbol.for('insert'),
            Symbol.for('value'),
            [Symbol.for('list'), Symbol.for('name')],
          ],
        ]
      );
    }
  );
  it(
    '(decompile "for (let x in foo) {\n' + '  bar();\n' + '}")',
    function (): any {
      return assertEqual(
        decompile('for (let x in foo) {\n' + '  bar();\n' + '}'),
        [
          Symbol.for('for'),
          [[Symbol.for('x'), [Symbol.for('js-keys'), Symbol.for('foo')]]],
          [Symbol.for('bar')],
        ]
      );
    }
  );
  it('(decompile "new Foo();")', function (): any {
    return assertEqual(decompile('new Foo();'), [
      Symbol.for('new'),
      Symbol.for('Foo'),
    ]);
  });
  it('(decompile "new Foo(\'bar\');")', function (): any {
    return assertEqual(decompile("new Foo('bar');"), [
      Symbol.for('new'),
      Symbol.for('Foo'),
      'bar',
    ]);
  });
  it('(decompile "new Foo(...args);")', function (): any {
    return assertEqual(decompile('new Foo(...args);'), [
      Symbol.for('apply'),
      Symbol.for('new'),
      Symbol.for('Foo'),
      Symbol.for('args'),
    ]);
  });
  it('(decompile "new Foo(x, ...args);")', function (): any {
    return assertEqual(decompile('new Foo(x, ...args);'), [
      Symbol.for('apply'),
      Symbol.for('new'),
      Symbol.for('Foo'),
      Symbol.for('x'),
      Symbol.for('args'),
    ]);
  });
  it('(decompile "delete x")', function (): any {
    return assertEqual(decompile('delete x'), [
      Symbol.for('js/delete'),
      Symbol.for('x'),
    ]);
  });
  it('(decompile "throw new Error(\'An error\')")', function (): any {
    return assertEqual(decompile("throw new Error('An error')"), [
      Symbol.for('throw'),
      [Symbol.for('new'), Symbol.for('Error'), 'An error'],
    ]);
  });
  xit('(decompile "try {\n' + '}")', function (): any {
    return assertEqual(decompile('try {\n' + '}'), [Symbol.for('try')]);
  });
  it(
    '(decompile "try {\n' +
      '  x = 2 / 1;\n' +
      '} finally {\n' +
      '  foo();\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'try {\n' + '  x = 2 / 1;\n' + '} finally {\n' + '  foo();\n' + '}'
        ),
        [
          Symbol.for('try'),
          [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
          [Symbol.for('finally'), [Symbol.for('foo')]],
        ]
      );
    }
  );
  it(
    '(decompile "try {\n' +
      '  x = 2 / 1;\n' +
      '} catch {\n' +
      '  foo();\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'try {\n' + '  x = 2 / 1;\n' + '} catch {\n' + '  foo();\n' + '}'
        ),
        [
          Symbol.for('try'),
          [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
          [
            Symbol.for('catch'),
            Symbol.for('Object'),
            Symbol.for('_'),
            [Symbol.for('foo')],
          ],
        ]
      );
    }
  );
  it(
    '(decompile "try {\n' +
      '  x = 2 / 1;\n' +
      '} catch (e) {\n' +
      '  foo();\n' +
      '} finally {\n' +
      '  bar();\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'try {\n' +
            '  x = 2 / 1;\n' +
            '} catch (e) {\n' +
            '  foo();\n' +
            '} finally {\n' +
            '  bar();\n' +
            '}'
        ),
        [
          Symbol.for('try'),
          [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
          [
            Symbol.for('catch'),
            Symbol.for('Object'),
            Symbol.for('e'),
            [Symbol.for('foo')],
          ],
          [Symbol.for('finally'), [Symbol.for('bar')]],
        ]
      );
    }
  );
  it(
    '(decompile "const I = async function (x) {\n' + '  return x;\n' + '};")',
    function (): any {
      return assertEqual(
        decompile('const I = async function (x) {\n' + '  return x;\n' + '};'),
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [
            Symbol.for('async'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ]
      );
    }
  );
  it(
    '(decompile "async function I(x) {\n' + '  return x;\n' + '}" :module #t)',
    function (): any {
      return assertEqual(
        decompile(
          'async function I(x) {\n' + '  return x;\n' + '}',
          Symbol.for(':module'),
          true
        ),
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [
            Symbol.for('define'),
            Symbol.for('I'),
            [
              Symbol.for('async'),
              [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
            ],
          ],
        ]
      );
    }
  );
  it('(decompile "import \'foo\';")', function (): any {
    return assertEqual(decompile("import 'foo';"), [
      Symbol.for('require'),
      'foo',
    ]);
  });
  it('(decompile "import * as foo from \'bar\';")', function (): any {
    return assertEqual(decompile("import * as foo from 'bar';"), [
      Symbol.for('require'),
      Symbol.for('foo'),
      'bar',
    ]);
  });
  it('(decompile "import { foo } from \'bar\';")', function (): any {
    return assertEqual(decompile("import { foo } from 'bar';"), [
      Symbol.for('require'),
      [Symbol.for('only-in'), 'bar', Symbol.for('foo')],
    ]);
  });
  it('(decompile "import { foo as bar } from \'baz\';")', function (): any {
    return assertEqual(decompile("import { foo as bar } from 'baz';"), [
      Symbol.for('require'),
      [Symbol.for('only-in'), 'baz', [Symbol.for('foo'), Symbol.for('bar')]],
    ]);
  });
  it('(decompile "export {};")', function (): any {
    return assertEqual(decompile('export {};'), [Symbol.for('provide')]);
  });
  it('(decompile "export {\n' + '  foo\n' + '};")', function (): any {
    return assertEqual(decompile('export {\n' + '  foo\n' + '};'), [
      Symbol.for('provide'),
      Symbol.for('foo'),
    ]);
  });
  it('(decompile "export {\n' + '  foo as bar\n' + '};")', function (): any {
    return assertEqual(decompile('export {\n' + '  foo as bar\n' + '};'), [
      Symbol.for('provide'),
      [Symbol.for('rename-out'), [Symbol.for('foo'), Symbol.for('bar')]],
    ]);
  });
  it('(decompile "export * from \'foo\';")', function (): any {
    return assertEqual(decompile("export * from 'foo';"), [
      Symbol.for('provide'),
      [Symbol.for('all-from-out'), 'foo'],
    ]);
  });
  it('(decompile "const foo = {};")', function (): any {
    return assertEqual(decompile('const foo = {};'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      [Symbol.for('js-obj')],
    ]);
  });
  it('(decompile "const foo = { bar: true };")', function (): any {
    return assertEqual(decompile('const foo = { bar: true };'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      [Symbol.for('js-obj'), 'bar', true],
    ]);
  });
  it('(decompile "const foo = { ...{ bar: true } };")', function (): any {
    return assertEqual(decompile('const foo = { ...{ bar: true } };'), [
      Symbol.for('define'),
      Symbol.for('foo'),
      [Symbol.for('js-obj-append'), [Symbol.for('js-obj'), 'bar', true]],
    ]);
  });
  it('(decompile "class Foo {\n' + '}")', function (): any {
    return assertEqual(decompile('class Foo {\n' + '}'), [
      Symbol.for('define-class'),
      Symbol.for('Foo'),
      [],
    ]);
  });
  it('(decompile "class Foo extends Bar {\n' + '}")', function (): any {
    return assertEqual(decompile('class Foo extends Bar {\n' + '}'), [
      Symbol.for('define-class'),
      Symbol.for('Foo'),
      [Symbol.for('Bar')],
    ]);
  });
  it('(decompile "class Foo {\n' + '  bar = 1;\n' + '}")', function (): any {
    return assertEqual(decompile('class Foo {\n' + '  bar = 1;\n' + '}'), [
      Symbol.for('define-class'),
      Symbol.for('Foo'),
      [],
      [Symbol.for('define/public'), Symbol.for('bar'), 1],
    ]);
  });
  it(
    '(decompile "class Foo {\n' +
      '  bar() {\n' +
      '    return 1;\n' +
      '  }\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'class Foo {\n' + '  bar() {\n' + '    return 1;\n' + '  }\n' + '}'
        ),
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define/public'), [Symbol.for('bar')], 1],
        ]
      );
    }
  );
  it(
    '(decompile "class Foo {\n' +
      '  bar;\n' +
      '\n' +
      '  constructor() {\n' +
      '    this.bar = 1;\n' +
      '  }\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'class Foo {\n' +
            '  bar;\n' +
            '\n' +
            '  constructor() {\n' +
            '    this.bar = 1;\n' +
            '  }\n' +
            '}'
        ),
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define/public'), Symbol.for('bar')],
          [
            Symbol.for('define'),
            [Symbol.for('constructor')],
            [
              Symbol.for('set-field!'),
              Symbol.for('bar'),
              Symbol.for('this'),
              1,
            ],
          ],
        ]
      );
    }
  );
  it(
    '(decompile "class Foo extends Bar {\n' +
      '  constructor() {\n' +
      '    super();\n' +
      '  }\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'class Foo extends Bar {\n' +
            '  constructor() {\n' +
            '    super();\n' +
            '  }\n' +
            '}'
        ),
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [Symbol.for('Bar')],
          [
            Symbol.for('define'),
            [Symbol.for('constructor')],
            [Symbol.for('super')],
          ],
        ]
      );
    }
  );
  it(
    '(decompile "class Foo extends Bar {\n' +
      '  constructor(x) {\n' +
      '    super(x);\n' +
      '  }\n' +
      '}")',
    function (): any {
      return assertEqual(
        decompile(
          'class Foo extends Bar {\n' +
            '  constructor(x) {\n' +
            '    super(x);\n' +
            '  }\n' +
            '}'
        ),
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [Symbol.for('Bar')],
          [
            Symbol.for('define'),
            [Symbol.for('constructor'), Symbol.for('x')],
            [Symbol.for('super'), Symbol.for('x')],
          ],
        ]
      );
    }
  );
  it(
    '(decompile "class Foo {\n' +
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
      '}")',
    function (): any {
      return assertEqual(
        decompile(
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
        ),
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
        ]
      );
    }
  );
  it(
    '(decompile "class Foo {\n' +
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
      '}")',
    function (): any {
      return assertEqual(
        decompile(
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
        ),
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
        ]
      );
    }
  );
  it('(decompile "foo as any" :from \'typescript)', function (): any {
    return assertEqual(
      decompile('foo as any', Symbol.for(':from'), Symbol.for('typescript')),
      [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('Any')]
    );
  });
  it('(decompile "foo as number" :from \'typescript)', function (): any {
    return assertEqual(
      decompile('foo as number', Symbol.for(':from'), Symbol.for('typescript')),
      [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('Number')]
    );
  });
  it('(decompile "foo as boolean" :from \'typescript)', function (): any {
    return assertEqual(
      decompile(
        'foo as boolean',
        Symbol.for(':from'),
        Symbol.for('typescript')
      ),
      [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('Boolean')]
    );
  });
  it('(decompile "foo as true" :from \'typescript)', function (): any {
    return assertEqual(
      decompile('foo as true', Symbol.for(':from'), Symbol.for('typescript')),
      [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('True')]
    );
  });
  it('(decompile "foo as MyClass" :from \'typescript)', function (): any {
    return assertEqual(
      decompile(
        'foo as MyClass',
        Symbol.for(':from'),
        Symbol.for('typescript')
      ),
      [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('MyClass')]
    );
  });
  it('(decompile "foo as MyClass<Any>" :from \'typescript\')', function (): any {
    return assertEqual(
      decompile(
        'foo as MyClass<Any>',
        Symbol.for(':from'),
        Symbol.for("typescript'")
      ),
      [
        Symbol.for('ann'),
        Symbol.for('foo'),
        [Symbol.for('MyClass'), Symbol.for('Any')],
      ]
    );
  });
  it('(decompile "foo as any[]" :from \'typescript)', function (): any {
    return assertEqual(
      decompile('foo as any[]', Symbol.for(':from'), Symbol.for('typescript')),
      [
        Symbol.for('ann'),
        Symbol.for('foo'),
        [Symbol.for('Listof'), Symbol.for('Any')],
      ]
    );
  });
  it('(decompile "foo as [any]" :from \'typescript)', function (): any {
    return assertEqual(
      decompile('foo as [any]', Symbol.for(':from'), Symbol.for('typescript')),
      [
        Symbol.for('ann'),
        Symbol.for('foo'),
        [Symbol.for('List'), Symbol.for('Any')],
      ]
    );
  });
  it('(decompile "foo as [number, any]" :from \'typescript)', function (): any {
    return assertEqual(
      decompile(
        'foo as [number, any]',
        Symbol.for(':from'),
        Symbol.for('typescript')
      ),
      [
        Symbol.for('ann'),
        Symbol.for('foo'),
        [Symbol.for('List'), Symbol.for('Number'), Symbol.for('Any')],
      ]
    );
  });
  it('(decompile "foo as number | string" :from \'typescript)', function (): any {
    return assertEqual(
      decompile(
        'foo as number | string',
        Symbol.for(':from'),
        Symbol.for('typescript')
      ),
      [
        Symbol.for('ann'),
        Symbol.for('foo'),
        [Symbol.for('U'), Symbol.for('Number'), Symbol.for('String')],
      ]
    );
  });
  it('(decompile "foo as string | undefined" :from \'typescript)', function (): any {
    return assertEqual(
      decompile(
        'foo as string | undefined',
        Symbol.for(':from'),
        Symbol.for('typescript')
      ),
      [
        Symbol.for('ann'),
        Symbol.for('foo'),
        [Symbol.for('U'), Symbol.for('String'), Symbol.for('Undefined')],
      ]
    );
  });
  it('(decompile "foo as (a: any) => void" :from \'typescript)', function (): any {
    return assertEqual(
      decompile(
        'foo as (a: any) => void',
        Symbol.for(':from'),
        Symbol.for('typescript')
      ),
      [
        Symbol.for('ann'),
        Symbol.for('foo'),
        [Symbol.for('->'), Symbol.for('Any'), Symbol.for('Void')],
      ]
    );
  });
  return it('(decompile "type NN = number;" :from \'typescript)', function (): any {
    return assertEqual(
      decompile(
        'type NN = number;',
        Symbol.for(':from'),
        Symbol.for('typescript')
      ),
      [Symbol.for('define-type'), Symbol.for('NN'), Symbol.for('Number')]
    );
  });
});
