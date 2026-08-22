import {
  compose,
  pipe
} from '../../src/ts/procedures';

import {
  s
} from '../../src/ts/sexp';

import {
  stringp
} from '../../src/ts/string';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('string?', function (): any {
  it('(string? "foo")', function (): any {
    return assertEqual(stringp('foo'), true);
  });
  it('(string? (new String "foo"))', function (): any {
    return assertEqual(stringp(new String('foo')), true);
  });
  return it('(string? \'foo)', function (): any {
    return assertEqual(stringp(Symbol.for('foo')), false);
  });
});

describe('compose', function (): any {
  it('g . f', function (): any {
    return assertEqual(((): any => {
      const f: any = function (x: any): any {
        return x + 1;
      };
      const g: any = function (x: any): any {
        return x + 2;
      };
      return compose(g, f)(1);
    })(), 4);
  });
  return it('h . g . f', function (): any {
    return assertEqual(((): any => {
      const f: any = function (x: any): any {
        return x + 1;
      };
      const g: any = function (x: any): any {
        return x + 2;
      };
      const h: any = function (x: any): any {
        return x + 3;
      };
      return compose(h, g, f)(1);
    })(), 7);
  });
});

describe('pipe', function (): any {
  it('f ; g', function (): any {
    return assertEqual(((): any => {
      const f: any = function (x: any): any {
        return x + 1;
      };
      const g: any = function (x: any): any {
        return x + 2;
      };
      return pipe(f, g)(1);
    })(), 4);
  });
  return it('f ; g ; h', function (): any {
    return assertEqual(((): any => {
      const f: any = function (x: any): any {
        return x + 1;
      };
      const g: any = function (x: any): any {
        return x + 2;
      };
      const h: any = function (x: any): any {
        return x + 3;
      };
      return pipe(f, g, h)(1);
    })(), 7);
  });
});