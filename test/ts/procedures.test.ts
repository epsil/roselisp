import { compose, pipe } from '../../src/ts/procedures';

import { s } from '../../src/ts/sexp';

import { stringp } from '../../src/ts/string';

import { assertEqual } from './test-util';

describe('stringp', function (): any {
  it("new String('foo')", function (): any {
    return assertEqual(stringp(new String('foo')), true);
  });
  return it('s`foo`', function (): any {
    return assertEqual(stringp(s`foo`), false);
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
