import * as chai from 'chai';

import { I } from '../../src/ts/combinators';

import { decompile } from '../../src/ts/decompiler';

import { writeToString } from '../../src/ts/printer';

import { sexp } from '../../src/ts/sexp';

import { assertEqual } from './test-util';

describe('decompile', function (): any {
  describe('boolean values', function (): any {
    it('true', function (): any {
      const actual: any = decompile('true;', {
        language: 'JavaScript',
        sexp: true,
      });
      return assertEqual(actual, sexp`#t`);
    });
    return it('false', function (): any {
      return assertEqual(
        decompile('false', {
          language: 'JavaScript',
          sexp: true,
        }),
        Symbol.for('#f')
      );
    });
  });
  describe('undefined', function (): any {
    return it('undefined', function (): any {
      return assertEqual(
        decompile('const foo = undefined;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('foo'), Symbol.for('undefined')]
      );
    });
  });
  describe('null', function (): any {
    return it('null', function (): any {
      return assertEqual(
        decompile('const foo = null;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('foo'), Symbol.for('js/null')]
      );
    });
  });
  describe('this', function (): any {
    return it('this', function (): any {
      return assertEqual(
        decompile('const foo = this;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('foo'), Symbol.for('this')]
      );
    });
  });
  describe('numbers', function (): any {
    it('0', function (): any {
      return assertEqual(
        decompile('0', {
          language: 'JavaScript',
        }),
        '0'
      );
    });
    it('1', function (): any {
      return assertEqual(
        decompile('1', {
          language: 'JavaScript',
        }),
        '1'
      );
    });
    return it('-1', function (): any {
      return assertEqual(
        decompile('-1', {
          language: 'JavaScript',
          sexp: true,
        }),
        -1
      );
    });
  });
  describe('strings', function (): any {
    it("'foo'", function (): any {
      return assertEqual(
        decompile("'foo'", {
          language: 'JavaScript',
        }),
        '"foo"'
      );
    });
    it("'foo'", function (): any {
      return assertEqual(
        decompile('const foo = `bar`;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('foo'), 'bar']
      );
    });
    it('bar`baz`', function (): any {
      return assertEqual(
        decompile('const foo = bar`baz`;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          Symbol.for('foo'),
          [Symbol.for('js/tag'), Symbol.for('bar'), 'baz'],
        ]
      );
    });
    return it("'foo'", function (): any {
      return assertEqual(
        decompile('const foo = `bar\n' + '\\`baz`;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('foo'), 'bar\n' + '`baz']
      );
    });
  });
  describe('regexps', function (): any {
    it('/foo/', function (): any {
      return assertEqual(
        decompile('/foo/', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('js/regexp'), 'foo']
      );
    });
    it('/foo/g', function (): any {
      return assertEqual(
        decompile('/foo/g', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('js/regexp'), 'foo', 'g']
      );
    });
    it('const exp = /.*/;, JS', function (): any {
      return assertEqual(
        decompile('const exp = /.*/;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          Symbol.for('exp'),
          [Symbol.for('js/regexp'), '.*'],
        ]
      );
    });
    return it('const exp = /.*/;, TS', function (): any {
      return assertEqual(
        decompile('const exp = /.*/;', {
          language: 'TypeScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          Symbol.for('exp'),
          [Symbol.for('js/regexp'), '.*'],
        ]
      );
    });
  });
  describe('arrays', function (): any {
    it('[]', function (): any {
      return assertEqual(
        decompile('[]', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('list')]
      );
    });
    it('const foo = [];', function (): any {
      return assertEqual(
        decompile('const foo = [];', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('list')]]
      );
    });
    it('[1, 2, 3]', function (): any {
      return assertEqual(
        decompile('[1, 2, 3]', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('list'), 1, 2, 3]
      );
    });
    it('[...x]', function (): any {
      return assertEqual(
        decompile('[...x]', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('append'), Symbol.for('x')]
      );
    });
    it('[x, ...y]', function (): any {
      return assertEqual(
        decompile('[x, ...y]', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('append'),
          [Symbol.for('list'), Symbol.for('x')],
          Symbol.for('y'),
        ]
      );
    });
    it('x[0]', function (): any {
      return assertEqual(
        decompile('x[0]', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('aget'), Symbol.for('x'), 0]
      );
    });
    it('x[0](a, b)', function (): any {
      return assertEqual(
        decompile('x[0](a, b)', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          [Symbol.for('aget'), Symbol.for('x'), 0],
          Symbol.for('a'),
          Symbol.for('b'),
        ]
      );
    });
    it('x[0][1]', function (): any {
      return assertEqual(
        decompile('x[0][1]', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('aget'), Symbol.for('x'), 0, 1]
      );
    });
    it('x[len]', function (): any {
      return assertEqual(
        decompile('x[len]', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('oget'), Symbol.for('x'), Symbol.for('len')]
      );
    });
    it('x[len - 1]', function (): any {
      return assertEqual(
        decompile('x[len - 1]', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('oget'),
          Symbol.for('x'),
          [Symbol.for('-'), Symbol.for('len'), 1],
        ]
      );
    });
    it('x[0] = 1', function (): any {
      return assertEqual(
        decompile('x[0] = 1', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('aset!'), Symbol.for('x'), 0, 1]
      );
    });
    return it("x['foo'] = 1", function (): any {
      return assertEqual(
        decompile("x['foo'] = 1", {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('oset!'), Symbol.for('x'), 'foo', 1]
      );
    });
  });
  describe('!', function (): any {
    return it('!foo', function (): any {
      return assertEqual(
        decompile('!foo', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('not'), Symbol.for('foo')]
      );
    });
  });
  describe('+', function (): any {
    it('1 + 2', function (): any {
      return assertEqual(
        decompile('1 + 2', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('+'), 1, 2]
      );
    });
    it('1 + 2 + 3', function (): any {
      return assertEqual(
        decompile('1 + 2 + 3', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('+'), 1, 2, 3]
      );
    });
    it("1 + ''", function (): any {
      return assertEqual(
        decompile("1 + ''", {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('string-append'), 1, '']
      );
    });
    return it("1 + 2 + ''", function (): any {
      return assertEqual(
        decompile("1 + 2 + ''", {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('string-append'), 1, 2, '']
      );
    });
  });
  describe('-', function (): any {
    it('-1', function (): any {
      return assertEqual(
        decompile('-1', {
          language: 'JavaScript',
          sexp: true,
        }),
        -1
      );
    });
    it('-(1)', function (): any {
      return assertEqual(
        decompile('-(1)', {
          language: 'JavaScript',
          sexp: true,
        }),
        -1
      );
    });
    it('-x', function (): any {
      return assertEqual(
        decompile('-x', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('-'), Symbol.for('x')]
      );
    });
    it('1 - 2', function (): any {
      return assertEqual(
        decompile('1 - 2', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('-'), 1, 2]
      );
    });
    it('1 - 2 - 3', function (): any {
      return assertEqual(
        decompile('1 - 2 - 3', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('-'), 1, 2, 3]
      );
    });
    return it('1 - (2 - 3)', function (): any {
      return assertEqual(
        decompile('1 - (2 - 3)', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('-'), 1, [Symbol.for('-'), 2, 3]]
      );
    });
  });
  describe('*', function (): any {
    it('1 * 2', function (): any {
      return assertEqual(
        decompile('1 * 2', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('*'), 1, 2]
      );
    });
    return it('1 * 2 * 3', function (): any {
      return assertEqual(
        decompile('1 * 2 * 3', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('*'), 1, 2, 3]
      );
    });
  });
  describe('/', function (): any {
    it('1 / 2', function (): any {
      return assertEqual(
        decompile('1 / 2', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('/'), 1, 2]
      );
    });
    return xit('1 / 2 / 3', function (): any {
      return assertEqual(
        decompile('1 / 2 / 3', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('/'), 1, 2, 3]
      );
    });
  });
  describe('&&', function (): any {
    it('x && y', function (): any {
      return assertEqual(
        decompile('x && y', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')]
      );
    });
    it('x && y && z', function (): any {
      return assertEqual(
        decompile('x && y && z', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('and'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]
      );
    });
    return it("typeof x === 'number' && typeof y === 'number'", function (): any {
      return assertEqual(
        decompile("typeof x === 'number' && typeof y === 'number'", {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('and'),
          [
            Symbol.for('eq?'),
            [Symbol.for('type-of'), Symbol.for('x')],
            'number',
          ],
          [
            Symbol.for('eq?'),
            [Symbol.for('type-of'), Symbol.for('y')],
            'number',
          ],
        ]
      );
    });
  });
  describe('||', function (): any {
    it('x || y', function (): any {
      return assertEqual(
        decompile('x || y', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('or'), Symbol.for('x'), Symbol.for('y')]
      );
    });
    return it('x || y || z', function (): any {
      return assertEqual(
        decompile('x || y || z', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('or'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]
      );
    });
  });
  describe('===', function (): any {
    return it('x === y', function (): any {
      return assertEqual(
        decompile('x === y', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('y')]
      );
    });
  });
  describe('!==', function (): any {
    return it('x !== y', function (): any {
      return assertEqual(
        decompile('x !== y', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('not'),
          [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('y')],
        ]
      );
    });
  });
  describe('in', function (): any {
    return it('x in y', function (): any {
      return assertEqual(
        decompile('x in y', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('js/in'), Symbol.for('x'), Symbol.for('y')]
      );
    });
  });
  describe('instanceof', function (): any {
    return it('x instanceof y', function (): any {
      return assertEqual(
        decompile('x instanceof y', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('y')]
      );
    });
  });
  describe('typeof', function (): any {
    return it('typeof x', function (): any {
      return assertEqual(
        decompile('typeof x', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('type-of'), Symbol.for('x')]
      );
    });
  });
  describe('function call', function (): any {
    it('foo(bar);, JS', function (): any {
      return assertEqual(
        decompile('foo(bar);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('foo'), Symbol.for('bar')]
      );
    });
    it('foo(bar);, TS', function (): any {
      return assertEqual(
        decompile('foo(bar);', {
          language: 'TypeScript',
          sexp: true,
        }),
        [Symbol.for('foo'), Symbol.for('bar')]
      );
    });
    it('foo(bar);, JS, module', function (): any {
      return assertEqual(
        decompile('foo(bar);', {
          language: 'JavaScript',
          module: true,
          sexp: true,
        }),
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [Symbol.for('foo'), Symbol.for('bar')],
        ]
      );
    });
    it('foo(bar);, TS, module', function (): any {
      return assertEqual(
        decompile('foo(bar);', {
          language: 'TypeScript',
          module: true,
          sexp: true,
        }),
        [
          Symbol.for('module'),
          Symbol.for('m'),
          Symbol.for('scheme'),
          [Symbol.for('foo'), Symbol.for('bar')],
        ]
      );
    });
    it("foo('bar');, JS", function (): any {
      return assertEqual(
        decompile("foo('bar');", {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('foo'), 'bar']
      );
    });
    it("foo('bar');, TS", function (): any {
      return assertEqual(
        decompile("foo('bar');", {
          language: 'TypeScript',
          sexp: true,
        }),
        [Symbol.for('foo'), 'bar']
      );
    });
    it("foo('bar', 'baz');, JS", function (): any {
      return assertEqual(
        decompile("foo('bar', 'baz');", {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('foo'), 'bar', 'baz']
      );
    });
    it("foo('bar', 'baz');, TS", function (): any {
      return assertEqual(
        decompile("foo('bar', 'baz');", {
          language: 'TypeScript',
          sexp: true,
        }),
        [Symbol.for('foo'), 'bar', 'baz']
      );
    });
    it('foo(1, 2, 3);, JS', function (): any {
      return assertEqual(
        decompile('foo(1, 2, 3);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('foo'), 1, 2, 3]
      );
    });
    it('foo(...args);', function (): any {
      return assertEqual(
        decompile('foo(...args);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('apply'), Symbol.for('foo'), Symbol.for('args')]
      );
    });
    it('foo(x, ...args);', function (): any {
      return assertEqual(
        decompile('foo(x, ...args);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('apply'),
          Symbol.for('foo'),
          Symbol.for('x'),
          Symbol.for('args'),
        ]
      );
    });
    it('foo(...args, x);', function (): any {
      return assertEqual(
        decompile('foo(...args, x);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('apply'),
          Symbol.for('foo'),
          [
            Symbol.for('append'),
            Symbol.for('args'),
            [Symbol.for('list'), Symbol.for('x')],
          ],
        ]
      );
    });
    it('foo(x, ...args, y);', function (): any {
      return assertEqual(
        decompile('foo(x, ...args, y);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('apply'),
          Symbol.for('foo'),
          [
            Symbol.for('append'),
            [Symbol.for('list'), Symbol.for('x')],
            Symbol.for('args'),
            [Symbol.for('list'), Symbol.for('y')],
          ],
        ]
      );
    });
    return xit('// comment\n' + 'foo(bar);, TS', function (): any {
      return assertEqual(
        decompile('// comment\n' + 'foo(bar);', {
          language: 'TypeScript',
          sexp: true,
        }),
        [Symbol.for('foo'), Symbol.for('bar')]
      );
    });
  });
  describe('assignment', function (): any {
    it('x = 1;', function (): any {
      return assertEqual(
        decompile('x = 1;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('set!'), Symbol.for('x'), 1]
      );
    });
    return it('x += 1;', function (): any {
      return assertEqual(
        decompile('x += 1;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('set!'),
          Symbol.for('x'),
          [Symbol.for('+'), Symbol.for('x'), 1],
        ]
      );
    });
  });
  describe('variables', function (): any {
    it('let x = 1;', function (): any {
      return assertEqual(
        decompile('let x = 1;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('x'), 1]
      );
    });
    it('let x = undefined;', function (): any {
      return assertEqual(
        decompile('let x = undefined;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('x'), Symbol.for('undefined')]
      );
    });
    it('let x;', function (): any {
      return assertEqual(
        decompile('let x;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('x')]
      );
    });
    it('let x = 1, y = 2;', function (): any {
      return assertEqual(
        decompile('let x = 1, y = 2;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('begin'),
          [Symbol.for('define'), Symbol.for('x'), 1],
          [Symbol.for('define'), Symbol.for('y'), 2],
        ]
      );
    });
    it('let x = 1, y = 2; let z = 3;', function (): any {
      return assertEqual(
        decompile('let x = 1, y = 2;\n' + 'let z = 3;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('begin'),
          [Symbol.for('define'), Symbol.for('x'), 1],
          [Symbol.for('define'), Symbol.for('y'), 2],
          [Symbol.for('define'), Symbol.for('z'), 3],
        ]
      );
    });
    it('const x = 1;', function (): any {
      return assertEqual(
        decompile('const x = 1', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define'), Symbol.for('x'), 1]
      );
    });
    it('let [x] = arr;', function (): any {
      return assertEqual(
        decompile('let [x] = arr;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define-values'), [Symbol.for('x')], Symbol.for('arr')]
      );
    });
    it('let [x, y] = arr;', function (): any {
      return assertEqual(
        decompile('let [x, y] = arr;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define-values'),
          [Symbol.for('x'), Symbol.for('y')],
          Symbol.for('arr'),
        ]
      );
    });
    it('[x, y] = arr;', function (): any {
      return assertEqual(
        decompile('[x, y] = arr;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('set!-values'),
          [Symbol.for('x'), Symbol.for('y')],
          Symbol.for('arr'),
        ]
      );
    });
    it('let [x, ...y] = arr;', function (): any {
      return assertEqual(
        decompile('let [x, ...y] = arr;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define-values'),
          [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')],
          Symbol.for('arr'),
        ]
      );
    });
    it('let [, y] = arr;', function (): any {
      return assertEqual(
        decompile('let [, y] = arr;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define-values'),
          [Symbol.for('_'), Symbol.for('y')],
          Symbol.for('arr'),
        ]
      );
    });
    it('let {x, y} = obj;', function (): any {
      return assertEqual(
        decompile('let {x, y} = obj;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define-js-obj'),
          [Symbol.for('x'), Symbol.for('y')],
          Symbol.for('obj'),
        ]
      );
    });
    it('({x, y} = obj);', function (): any {
      return assertEqual(
        decompile('({x, y} = obj);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('set!-js-obj'),
          [Symbol.for('x'), Symbol.for('y')],
          Symbol.for('obj'),
        ]
      );
    });
    return it('let {x: y, z} = obj;', function (): any {
      return assertEqual(
        decompile('let {x: y, z} = obj;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define-js-obj'),
          [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')],
          Symbol.for('obj'),
        ]
      );
    });
  });
  describe('fields', function (): any {
    it('x.y;', function (): any {
      return assertEqual(
        decompile('x.y;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('get-field'), Symbol.for('y'), Symbol.for('x')]
      );
    });
    it('x?.y;', function (): any {
      return assertEqual(
        decompile('x?.y;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('and'),
          [Symbol.for('field-bound?'), Symbol.for('y'), Symbol.for('x')],
          [Symbol.for('get-field'), Symbol.for('y'), Symbol.for('x')],
        ]
      );
    });
    it('foo()?.y;', function (): any {
      return assertEqual(
        decompile('foo()?.y;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('~>'),
          [Symbol.for('foo')],
          [
            Symbol.for('and'),
            [Symbol.for('field-bound?'), Symbol.for('y'), Symbol.for('_')],
            [Symbol.for('get-field'), Symbol.for('y'), Symbol.for('_')],
          ],
        ]
      );
    });
    return it('x.y = z;', function (): any {
      return assertEqual(
        decompile('x.y = z;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('set-field!'),
          Symbol.for('y'),
          Symbol.for('x'),
          Symbol.for('z'),
        ]
      );
    });
  });
  describe('methods', function (): any {
    it('x.y();', function (): any {
      return assertEqual(
        decompile('x.y();', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('send'), Symbol.for('x'), Symbol.for('y')]
      );
    });
    it('x?.y();', function (): any {
      return assertEqual(
        decompile('x?.y();', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('and'),
          [Symbol.for('field-bound?'), Symbol.for('y'), Symbol.for('x')],
          [Symbol.for('send'), Symbol.for('x'), Symbol.for('y')],
        ]
      );
    });
    it('foo()?.y();', function (): any {
      return assertEqual(
        decompile('foo()?.y();', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('~>'),
          [Symbol.for('foo')],
          [
            Symbol.for('and'),
            [Symbol.for('field-bound?'), Symbol.for('y'), Symbol.for('_')],
            [Symbol.for('send'), Symbol.for('_'), Symbol.for('y')],
          ],
        ]
      );
    });
    it('x.y(z);', function (): any {
      return assertEqual(
        decompile('x.y(z);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('send'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]
      );
    });
    return it('x.y(...z);', function (): any {
      return assertEqual(
        decompile('x.y(...z);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('send/apply'),
          Symbol.for('x'),
          Symbol.for('y'),
          Symbol.for('z'),
        ]
      );
    });
  });
  describe('function definitions', function (): any {
    it('function I(x) { return x; }', function (): any {
      return assertEqual(
        decompile('function I(x) {\n' + '  return x;\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          Symbol.for('x'),
        ]
      );
    });
    it('function I(x) { foo(); return x; }', function (): any {
      return assertEqual(
        decompile('function I(x) {\n' + '  foo();\n' + '  return x;\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          [Symbol.for('foo')],
          Symbol.for('x'),
        ]
      );
    });
    it('function I(x: any, y?: any) { return x; }', function (): any {
      return assertEqual(
        decompile('function I(x: any, y?: any) {\n' + '  return x;\n' + '}', {
          language: 'TypeScript',
          sexp: true,
        }),
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
    });
    it('function I(x: any, y: any = true) { return x; }', function (): any {
      return assertEqual(
        decompile(
          'function I(x: any, y: any = true) {\n' + '  return x;\n' + '}',
          {
            language: 'TypeScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('define'),
          [
            Symbol.for('I'),
            [Symbol.for('x'), Symbol.for(':'), Symbol.for('Any')],
            [
              Symbol.for('y'),
              Symbol.for(':'),
              Symbol.for('Any'),
              Symbol.for('#t'),
            ],
          ],
          Symbol.for('x'),
        ]
      );
    });
    it('function I(x: number, y: number = 1) { return x; }', function (): any {
      return assertEqual(
        decompile(
          'function I(x: number, y: number = 1) {\n' + '  return x;\n' + '}',
          {
            language: 'TypeScript',
            sexp: true,
          }
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
    });
    it('function foo(x = 1) { x; }', function (): any {
      return assertEqual(
        decompile('function foo(x = 1) {\n' + '  return x;\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          [Symbol.for('foo'), [Symbol.for('x'), 1]],
          Symbol.for('x'),
        ]
      );
    });
    it('function foo(...args) { return args; }', function (): any {
      return assertEqual(
        decompile('function foo(...args) {\n' + '  return args;\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          [Symbol.for('foo'), Symbol.for('.'), Symbol.for('args')],
          Symbol.for('args'),
        ]
      );
    });
    it('function I(x) { if (x) { return x; } else { return false; } }', function (): any {
      return assertEqual(
        decompile(
          'function I(x) {\n' +
            '  if (x) {\n' +
            '    return x;\n' +
            '  } else {\n' +
            '    return false;\n' +
            '  }\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          [
            Symbol.for('if'),
            Symbol.for('x'),
            Symbol.for('x'),
            Symbol.for('#f'),
          ],
        ]
      );
    });
    return it('function I(x) { if (x) { return x; } else if (false) { return false; } else { return false; } }', function (): any {
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
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          [
            Symbol.for('cond'),
            [Symbol.for('x'), Symbol.for('x')],
            [Symbol.for('#f'), Symbol.for('#f')],
            [Symbol.for('else'), Symbol.for('#f')],
          ],
        ]
      );
    });
  });
  describe('function expressions', function (): any {
    it('let I = function (x) { return x; };', function (): any {
      return assertEqual(
        decompile('let I = function (x) {\n' + '  return x;\n' + '};', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        ]
      );
    });
    return it('function foo(...args) { return args; }', function (): any {
      return assertEqual(
        decompile(
          'let foo = function (...args) {\n' + '  return args;\n' + '};',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('define'),
          Symbol.for('foo'),
          [Symbol.for('lambda'), Symbol.for('args'), Symbol.for('args')],
        ]
      );
    });
  });
  describe('arrow functions', function (): any {
    it('let I = function (x) { return x; };', function (): any {
      return assertEqual(
        decompile('let I = (x) => {\n' + '  return x;\n' + '};', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [Symbol.for('js/arrow'), [Symbol.for('x')], Symbol.for('x')],
        ]
      );
    });
    return it('let I = function (x: any) { return x; };', function (): any {
      return assertEqual(
        decompile('let I = (x: any) => {\n' + '  return x;\n' + '};', {
          language: 'JavaScript',
          sexp: true,
        }),
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
    });
  });
  describe('if', function (): any {
    it("if (true) { foo('bar'); }", function (): any {
      return assertEqual(
        decompile('if (true) {\n' + "  foo('bar');\n" + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('when'), Symbol.for('#t'), [Symbol.for('foo'), 'bar']]
      );
    });
    it("if (!foo) { bar('baz'); }", function (): any {
      return assertEqual(
        decompile('if (!foo) {\n' + "  bar('baz');\n" + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('unless'), Symbol.for('foo'), [Symbol.for('bar'), 'baz']]
      );
    });
    it("if (true) { foo('bar'); } else { bar('baz'); }", function (): any {
      return assertEqual(
        decompile(
          'if (true) {\n' +
            "  foo('bar');\n" +
            '} else {\n' +
            "  bar('baz');\n" +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('if'),
          Symbol.for('#t'),
          [Symbol.for('foo'), 'bar'],
          [Symbol.for('bar'), 'baz'],
        ]
      );
    });
    it("if (x) { if (y) { foo('bar'); } } else { bar('baz'); }", function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            '  if (y) {\n' +
            "    foo('bar');\n" +
            '  }\n' +
            '} else {\n' +
            "  bar('baz');\n" +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('if'),
          Symbol.for('x'),
          [Symbol.for('when'), Symbol.for('y'), [Symbol.for('foo'), 'bar']],
          [Symbol.for('bar'), 'baz'],
        ]
      );
    });
    it("if (x) { if (y) { foo('bar'); } } else { if (z) { bar('baz'); } }", function (): any {
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
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
    it("if (x) { if (y) { foo('bar'); } } else { if (z) { bar('baz'); } }", function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            '  foo();\n' +
            '  bar();\n' +
            '} else if (y) {\n' +
            '  baz();\n' +
            '  quux();\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('cond'),
          [Symbol.for('x'), [Symbol.for('foo')], [Symbol.for('bar')]],
          [Symbol.for('y'), [Symbol.for('baz')], [Symbol.for('quux')]],
        ]
      );
    });
    it("if (x) { if (y) { foo('bar'); } } else { if (z) { bar('baz'); } else { baz('quux'); } }", function (): any {
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
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
    it("if (x) { if (y) { foo('bar'); } } else { if (z) { bar('baz'); } else { baz('quux'); } }", function (): any {
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
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
    it("if (x) { foo('bar'); } else if (y) { bar('baz'); } else { baz('quux'); }", function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            "  foo('bar');\n" +
            '} else if (y) {\n' +
            "  bar('baz');\n" +
            '} else {\n' +
            "  baz('quux');\n" +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('cond'),
          [Symbol.for('x'), [Symbol.for('foo'), 'bar']],
          [Symbol.for('y'), [Symbol.for('bar'), 'baz']],
          [Symbol.for('else'), [Symbol.for('baz'), 'quux']],
        ]
      );
    });
    return it('if (x) { foo(); } else { bar(); baz(); }', function (): any {
      return assertEqual(
        decompile(
          'if (x) {\n' +
            '  foo();\n' +
            '} else {\n' +
            '  bar();\n' +
            '  baz();\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('cond'),
          [Symbol.for('x'), [Symbol.for('foo')]],
          [Symbol.for('else'), [Symbol.for('bar')], [Symbol.for('baz')]],
        ]
      );
    });
  });
  describe('?', function (): any {
    it('let x = true ? foo : bar;', function (): any {
      return assertEqual(
        decompile('let x = true ? foo : bar;', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          Symbol.for('x'),
          [
            Symbol.for('if'),
            Symbol.for('#t'),
            Symbol.for('foo'),
            Symbol.for('bar'),
          ],
        ]
      );
    });
    return it('let x = 1 ? foo : 2 ? bar : baz', function (): any {
      return assertEqual(
        decompile('let x = 1 ? foo : 2 ? bar : baz', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          Symbol.for('x'),
          [
            Symbol.for('cond'),
            [1, Symbol.for('foo')],
            [2, Symbol.for('bar')],
            [Symbol.for('else'), Symbol.for('baz')],
          ],
        ]
      );
    });
  });
  describe('while', function (): any {
    return it('while (foo) { bar(); }', function (): any {
      return assertEqual(
        decompile('while (foo) {\n' + '  bar();\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('do'),
          [],
          [[Symbol.for('not'), Symbol.for('foo')]],
          [Symbol.for('bar')],
        ]
      );
    });
  });
  describe('do...while', function (): any {
    return it('do { bar(); } while (foo)', function (): any {
      return assertEqual(
        decompile('do {\n' + '  bar();\n' + '} while (foo);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('js/do-while'), [Symbol.for('bar')], Symbol.for('foo')]
      );
    });
  });
  describe('for', function (): any {
    it('for (let i = 0; i < 10; i++) { foo(); }', function (): any {
      return assertEqual(
        decompile(
          'for (let i = 0; i < 10; i++) {\n' +
            '  foo();\n' +
            '  break;\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('for'),
          [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]],
          [Symbol.for('foo')],
          [Symbol.for('break')],
        ]
      );
    });
    it('for (i = 0; i < arr.length; i++) { foo(); }', function (): any {
      return assertEqual(
        decompile('for (i = 0; i < arr.length; i++) {\n' + '  foo();\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
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
    });
    it('for (let i = 10; i > 0; i--) { foo(); }', function (): any {
      return assertEqual(
        decompile('for (let i = 10; i > 0; i--) {\n' + '  foo();\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('for'),
          [[Symbol.for('i'), [Symbol.for('range'), 10, 0, -1]]],
          [Symbol.for('foo')],
        ]
      );
    });
    return it('for (let i = 0, j = 0; i < 10; i++, j++) { foo(); }', function (): any {
      return assertEqual(
        decompile(
          'for (let i = 0, j = 0; i < 10; i++, j++) {\n' +
            '  foo();\n' +
            '  break bar;\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
  });
  describe('for...of', function (): any {
    it('for (let x of foo) { bar(); }', function (): any {
      return assertEqual(
        decompile(
          'for (let x of foo) {\n' + '  bar();\n' + '  continue;\n' + '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('for'),
          [[Symbol.for('x'), Symbol.for('foo')]],
          [Symbol.for('bar')],
          [Symbol.for('continue')],
        ]
      );
    });
    it('for (const [name, value] of entries) { ... }', function (): any {
      return assertEqual(
        decompile(
          'for (const [name, value] of entries) {\n' +
            '  result.insert(value, [name]);\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
    return it('for (const [name, value] of x) { ... }', function (): any {
      return assertEqual(
        decompile(
          'for (const [name, value] of x) {\n' +
            '  result.insert(value, [name]);\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
  });
  describe('for...in', function (): any {
    return it('for (let x in foo) { bar(); }', function (): any {
      return assertEqual(
        decompile('for (let x in foo) {\n' + '  bar();\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('for'),
          [[Symbol.for('x'), [Symbol.for('js-keys'), Symbol.for('foo')]]],
          [Symbol.for('bar')],
        ]
      );
    });
  });
  describe('new', function (): any {
    it('new Foo()', function (): any {
      return assertEqual(
        decompile('new Foo();', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('new'), Symbol.for('Foo')]
      );
    });
    it("new Foo('bar')", function (): any {
      return assertEqual(
        decompile("new Foo('bar');", {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('new'), Symbol.for('Foo'), 'bar']
      );
    });
    it('new Foo(...args);', function (): any {
      return assertEqual(
        decompile('new Foo(...args);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('apply'),
          Symbol.for('new'),
          Symbol.for('Foo'),
          Symbol.for('args'),
        ]
      );
    });
    return it('new Foo(x, ...args);', function (): any {
      return assertEqual(
        decompile('new Foo(x, ...args);', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('apply'),
          Symbol.for('new'),
          Symbol.for('Foo'),
          Symbol.for('x'),
          Symbol.for('args'),
        ]
      );
    });
  });
  describe('delete', function (): any {
    return it('delete x', function (): any {
      return assertEqual(
        decompile('delete x', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('js/delete'), Symbol.for('x')]
      );
    });
  });
  describe('throw', function (): any {
    return it("throw new Error('An error')", function (): any {
      return assertEqual(
        decompile("throw new Error('An error')", {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('throw'),
          [Symbol.for('new'), Symbol.for('Error'), 'An error'],
        ]
      );
    });
  });
  describe('try', function (): any {
    xit('try {}', function (): any {
      return assertEqual(
        decompile('try {\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('try')]
      );
    });
    it('try { x = 2 / 1; } finally { foo(); }', function (): any {
      return assertEqual(
        decompile(
          'try {\n' + '  x = 2 / 1;\n' + '} finally {\n' + '  foo();\n' + '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('try'),
          [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]],
          [Symbol.for('finally'), [Symbol.for('foo')]],
        ]
      );
    });
    it('try { x = 2 / 1; } catch { foo(); }', function (): any {
      return assertEqual(
        decompile(
          'try {\n' + '  x = 2 / 1;\n' + '} catch {\n' + '  foo();\n' + '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
    return it('try { x = 2 / 1; } catch (e) { foo(); } finally { bar(); }', function (): any {
      return assertEqual(
        decompile(
          'try {\n' +
            '  x = 2 / 1;\n' +
            '} catch (e) {\n' +
            '  foo();\n' +
            '} finally {\n' +
            '  bar();\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
  });
  describe('async', function (): any {
    it('const I = async function (x) { return x; };', function (): any {
      return assertEqual(
        decompile('const I = async function (x) {\n' + '  return x;\n' + '};', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [
            Symbol.for('async'),
            [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
          ],
        ]
      );
    });
    return it('async function I(x) { return x; }', function (): any {
      return assertEqual(
        decompile('async function I(x) {\n' + '  return x;\n' + '}', {
          language: 'JavaScript',
          module: true,
          sexp: true,
        }),
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
    });
  });
  describe('import', function (): any {
    it("import 'foo';", function (): any {
      return assertEqual(
        decompile("import 'foo';", {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('require'), 'foo']
      );
    });
    it("import * as foo from 'bar';", function (): any {
      return assertEqual(
        decompile("import * as foo from 'bar';", {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('require'), Symbol.for('foo'), 'bar']
      );
    });
    it("import { foo } from 'bar';", function (): any {
      return assertEqual(
        decompile("import { foo } from 'bar';", {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('require'),
          [Symbol.for('only-in'), 'bar', Symbol.for('foo')],
        ]
      );
    });
    return it("import { foo as bar } from 'baz';", function (): any {
      return assertEqual(
        decompile("import { foo as bar } from 'baz';", {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('require'),
          [
            Symbol.for('only-in'),
            'baz',
            [Symbol.for('foo'), Symbol.for('bar')],
          ],
        ]
      );
    });
  });
  describe('export', function (): any {
    it('export {};', function (): any {
      return assertEqual(
        decompile('export {};', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('provide')]
      );
    });
    it('export { foo };', function (): any {
      return assertEqual(
        decompile('export {\n' + '  foo\n' + '};', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('provide'), Symbol.for('foo')]
      );
    });
    it('export { foo as bar };', function (): any {
      return assertEqual(
        decompile('export {\n' + '  foo as bar\n' + '};', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('provide'),
          [Symbol.for('rename-out'), [Symbol.for('foo'), Symbol.for('bar')]],
        ]
      );
    });
    return it("export * from 'foo';", function (): any {
      return assertEqual(
        decompile("export * from 'foo';", {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('provide'), [Symbol.for('all-from-out'), 'foo']]
      );
    });
  });
  describe('objects', function (): any {
    it('{}', function (): any {
      const actual: any = decompile('const foo = {};', {
        language: 'JavaScript',
        sexp: true,
      });
      return assertEqual(actual, [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('js-obj')],
      ]);
    });
    it("{ bar: 'baz' }", function (): any {
      const actual: any = decompile('const foo = { bar: true };', {
        language: 'JavaScript',
        sexp: true,
      });
      return assertEqual(actual, [
        Symbol.for('define'),
        Symbol.for('foo'),
        [Symbol.for('js-obj'), 'bar', Symbol.for('#t')],
      ]);
    });
    return it('{ ...{ bar: true } }', function (): any {
      const actual: any = decompile('const foo = { ...{ bar: true } };', {
        language: 'JavaScript',
        sexp: true,
      });
      return assertEqual(actual, [
        Symbol.for('define'),
        Symbol.for('foo'),
        [
          Symbol.for('js-obj-append'),
          [Symbol.for('js-obj'), 'bar', Symbol.for('#t')],
        ],
      ]);
    });
  });
  describe('classes', function (): any {
    it('class Foo {}', function (): any {
      return assertEqual(
        decompile('class Foo {\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define-class'), Symbol.for('Foo'), []]
      );
    });
    it('class Foo extends Bar {}', function (): any {
      return assertEqual(
        decompile('class Foo extends Bar {\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Bar')]]
      );
    });
    it('class Foo { bar = 1; }', function (): any {
      return assertEqual(
        decompile('class Foo {\n' + '  bar = 1;\n' + '}', {
          language: 'JavaScript',
          sexp: true,
        }),
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define/public'), Symbol.for('bar'), 1],
        ]
      );
    });
    it('class Foo { bar() { return 1; } }', function (): any {
      return assertEqual(
        decompile(
          'class Foo {\n' + '  bar() {\n' + '    return 1;\n' + '  }\n' + '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
        ),
        [
          Symbol.for('define-class'),
          Symbol.for('Foo'),
          [],
          [Symbol.for('define/public'), [Symbol.for('bar')], 1],
        ]
      );
    });
    it('class Foo { bar; constructor() { this.bar = 1; } }', function (): any {
      return assertEqual(
        decompile(
          'class Foo {\n' +
            '  bar;\n' +
            '\n' +
            '  constructor() {\n' +
            '    this.bar = 1;\n' +
            '  }\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
    it('class Foo extends Bar { constructor() { super(); } }', function (): any {
      return assertEqual(
        decompile(
          'class Foo extends Bar {\n' +
            '  constructor() {\n' +
            '    super();\n' +
            '  }\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
    it('class Foo extends Bar { constructor(x) { super(x); } }', function (): any {
      return assertEqual(
        decompile(
          'class Foo extends Bar {\n' +
            '  constructor(x) {\n' +
            '    super(x);\n' +
            '  }\n' +
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
    it('class Foo { *generator() { ... } }', function (): any {
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
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
    return it('class Foo { *[Symbol.iterator]() { ... } }', function (): any {
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
            '}',
          {
            language: 'JavaScript',
            sexp: true,
          }
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
    });
  });
  return describe('TypeScript', function (): any {
    describe('as', function (): any {
      it('foo as any', function (): any {
        return assertEqual(
          decompile('foo as any', {
            language: 'TypeScript',
            sexp: true,
          }),
          [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('Any')]
        );
      });
      it('foo as number', function (): any {
        return assertEqual(
          decompile('foo as number', {
            language: 'TypeScript',
            sexp: true,
          }),
          [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('Number')]
        );
      });
      it('foo as boolean', function (): any {
        return assertEqual(
          decompile('foo as boolean', {
            language: 'TypeScript',
            sexp: true,
          }),
          [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('Boolean')]
        );
      });
      it('foo as false', function (): any {
        return assertEqual(
          decompile('foo as true', {
            language: 'TypeScript',
            sexp: true,
          }),
          [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('True')]
        );
      });
      it('foo as MyClass', function (): any {
        return assertEqual(
          decompile('foo as MyClass', {
            language: 'TypeScript',
            sexp: true,
          }),
          [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('MyClass')]
        );
      });
      it('foo as MyClass<Any>', function (): any {
        return assertEqual(
          decompile('foo as MyClass<Any>', {
            language: 'TypeScript',
            sexp: true,
          }),
          [
            Symbol.for('ann'),
            Symbol.for('foo'),
            [Symbol.for('MyClass'), Symbol.for('Any')],
          ]
        );
      });
      it('foo as any[]', function (): any {
        return assertEqual(
          decompile('foo as any[]', {
            language: 'TypeScript',
            sexp: true,
          }),
          [
            Symbol.for('ann'),
            Symbol.for('foo'),
            [Symbol.for('Listof'), Symbol.for('Any')],
          ]
        );
      });
      it('foo as [any]', function (): any {
        return assertEqual(
          decompile('foo as [any]', {
            language: 'TypeScript',
            sexp: true,
          }),
          [
            Symbol.for('ann'),
            Symbol.for('foo'),
            [Symbol.for('List'), Symbol.for('Any')],
          ]
        );
      });
      it('foo as [number, any]', function (): any {
        return assertEqual(
          decompile('foo as [number, any]', {
            language: 'TypeScript',
            sexp: true,
          }),
          [
            Symbol.for('ann'),
            Symbol.for('foo'),
            [Symbol.for('List'), Symbol.for('Number'), Symbol.for('Any')],
          ]
        );
      });
      it('foo as number | string', function (): any {
        return assertEqual(
          decompile('foo as number | string', {
            language: 'TypeScript',
            sexp: true,
          }),
          [
            Symbol.for('ann'),
            Symbol.for('foo'),
            [Symbol.for('U'), Symbol.for('Number'), Symbol.for('String')],
          ]
        );
      });
      it('foo as string | undefined', function (): any {
        return assertEqual(
          decompile('foo as string | undefined', {
            language: 'TypeScript',
            sexp: true,
          }),
          [
            Symbol.for('ann'),
            Symbol.for('foo'),
            [Symbol.for('U'), Symbol.for('String'), Symbol.for('Undefined')],
          ]
        );
      });
      return it('foo as (a: any) => void', function (): any {
        return assertEqual(
          decompile('foo as (a: any) => void', {
            language: 'TypeScript',
            sexp: true,
          }),
          [
            Symbol.for('ann'),
            Symbol.for('foo'),
            [Symbol.for('->'), Symbol.for('Any'), Symbol.for('Void')],
          ]
        );
      });
    });
    return describe('type', function (): any {
      return it('type NN = number', function (): any {
        return assertEqual(
          decompile('type NN = number;', {
            language: 'TypeScript',
            sexp: true,
          }),
          [Symbol.for('define-type'), Symbol.for('NN'), Symbol.for('Number')]
        );
      });
    });
  });
});
