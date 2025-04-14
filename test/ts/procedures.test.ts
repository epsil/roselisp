import * as chai from 'chai';

import { cons } from '../../src/ts/cons';

import { compose, gt, lt, pipe } from '../../src/ts/procedures';

import { dottedListP, flatten } from '../../src/ts/list';

import { s, sexp } from '../../src/ts/sexp';

import { stringp } from '../../src/ts/string';

import { assertEqual } from './test-util';

describe('stringp', function (): any {
  it("'foo'", function (): any {
    return assertEqual(stringp('foo'), true);
  });
  it("new String('foo')", function (): any {
    return assertEqual(stringp(new String('foo')), true);
  });
  it('s`foo`', function (): any {
    return assertEqual(stringp(s`foo`), false);
  });
  it('1', function (): any {
    return assertEqual(stringp(1), false);
  });
  it('{}', function (): any {
    return assertEqual(stringp({}), false);
  });
  it("['foo']", function (): any {
    return assertEqual(stringp(['foo']), false);
  });
  it("{ 'foo': '' }", function (): any {
    return assertEqual(
      stringp({
        foo: '',
      }),
      false
    );
  });
  it("{ 'foo': []", function (): any {
    return assertEqual(
      stringp({
        foo: [],
      }),
      false
    );
  });
  it("{ 'foo': {}", function (): any {
    return assertEqual(
      stringp({
        foo: {},
      }),
      false
    );
  });
  it("{ 'foo': 'foo' }", function (): any {
    return assertEqual(
      stringp({
        foo: 'foo',
      }),
      false
    );
  });
  return it('[]', function (): any {
    return assertEqual(stringp([]), false);
  });
});

describe('flatten', function (): any {
  it('(1 2 3 4)', function (): any {
    return assertEqual(flatten([1, 2, 3, 4]), [1, 2, 3, 4]);
  });
  it('(1 . 2)', function (): any {
    return assertEqual(flatten([1, Symbol.for('.'), 2]), [1, 2]);
  });
  return it('((a) b (c (d) . e) ())', function (): any {
    return assertEqual(
      flatten([
        [Symbol.for('a')],
        Symbol.for('b'),
        [Symbol.for('c'), [Symbol.for('d')], Symbol.for('.'), Symbol.for('e')],
        [],
      ]),
      [
        Symbol.for('a'),
        Symbol.for('b'),
        Symbol.for('c'),
        Symbol.for('d'),
        Symbol.for('e'),
      ]
    );
  });
});

describe('<', function (): any {
  it('(< 1)', function (): any {
    return assertEqual(lt(1), true);
  });
  it('(< 1 2)', function (): any {
    return assertEqual(lt(1, 2), true);
  });
  it('(< 1 2 3)', function (): any {
    return assertEqual(lt(1, 2, 3), true);
  });
  return it('(< 1 2 0)', function (): any {
    return assertEqual(lt(1, 2, 0), false);
  });
});

describe('>', function (): any {
  it('(> 1)', function (): any {
    return assertEqual(gt(1), true);
  });
  it('(> 2 1)', function (): any {
    return assertEqual(gt(2, 1), true);
  });
  it('(> 3 2 1)', function (): any {
    return assertEqual(gt(3, 2, 1), true);
  });
  return it('(> 0 2 1)', function (): any {
    return assertEqual(gt(0, 2, 1), false);
  });
});

describe('compose', function (): any {
  it('f . g', function (): any {
    function f(x: any): any {
      return x + 1;
    }
    function g(x: any): any {
      return x + 2;
    }
    return assertEqual(compose(f, g)(1), 4);
  });
  return it('f . g . h', function (): any {
    function f(x: any): any {
      return x + 1;
    }
    function g(x: any): any {
      return x + 2;
    }
    function h(x: any): any {
      return x + 3;
    }
    return assertEqual(compose(f, g, h)(1), 7);
  });
});

describe('pipe', function (): any {
  it('f | g', function (): any {
    function f(x: any): any {
      return x + 1;
    }
    function g(x: any): any {
      return x + 2;
    }
    return assertEqual(pipe(f, g)(1), 4);
  });
  return it('f | g | h', function (): any {
    function f(x: any): any {
      return x + 1;
    }
    function g(x: any): any {
      return x + 2;
    }
    function h(x: any): any {
      return x + 3;
    }
    return assertEqual(pipe(f, g, h)(1), 7);
  });
});
