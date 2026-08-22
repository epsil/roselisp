import {
  __,
  curry,
  dashify
} from '../../src/ts/curry';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('curry', function (): any {
  function a(x: any): any {
    return [x];
  }
  const aC: any = curry(a);
  it('(a-c 1)', function (): any {
    return assertEqual(aC(1), [1]);
  });
  it('((a-c) 1)', function (): any {
    return assertEqual(aC()(1), [1]);
  });
  function ab(x: any, y: any): any {
    return [x, y];
  }
  const abC: any = curry(ab);
  it('(ab-c 1 2)', function (): any {
    return assertEqual(abC(1, 2), [1, 2]);
  });
  it('((ab-c 1) 2)', function (): any {
    return assertEqual(abC(1)(2), [1, 2]);
  });
  it('(((ab-c) 1) 2)', function (): any {
    return assertEqual(abC()(1)(2), [1, 2]);
  });
  function abc(x: any, y: any, z: any): any {
    return [x, y, z];
  }
  const abcC: any = curry(abc);
  it('(abc-c 1 2 3)', function (): any {
    return assertEqual(abcC(1, 2, 3), [1, 2, 3]);
  });
  it('((abc-c 1 2) 3)', function (): any {
    return assertEqual(abcC(1, 2)(3), [1, 2, 3]);
  });
  it('(((abc-c 1) 2) 3)', function (): any {
    return assertEqual(abcC(1)(2)(3), [1, 2, 3]);
  });
  it('((((abc-c) 1) 2) 3)', function (): any {
    return assertEqual(abcC()(1)(2)(3), [1, 2, 3]);
  });
  const abcC1: any = curry(abc, 1);
  it('(abc-c1 1)', function (): any {
    return assertEqual(abcC1(1), [1, undefined, undefined]);
  });
  it('(abc-c1 1 2)', function (): any {
    return assertEqual(abcC1(1, 2), [1, 2, undefined]);
  });
  it('(abc-c 1 2 3)', function (): any {
    return assertEqual(abcC(1, 2, 3), [1, 2, 3]);
  });
  it('((abc-c __ __ __) 1 2 3)', function (): any {
    return assertEqual(abcC(__, __, __)(1, 2, 3), [1, 2, 3]);
  });
  return it('(not (eq? __ \'_))', function (): any {
    return assertEqual(__ !== Symbol.for('_'), true);
  });
});

describe('dashify', function (): any {
  function I(x: any): any {
    return x;
  }
  it('((dashify I) I)', function (): any {
    return assertEqual(dashify(I)(I), I);
  });
  it('(((dashify I) __) I)', function (): any {
    return assertEqual(dashify(I)(__)(I), I);
  });
  function add(x: any, y: any): any {
    return x + y;
  }
  it('((dashify add) 1 2)', function (): any {
    return assertEqual(dashify(add)(1, 2), 3);
  });
  it('(((dashify add) __ 2) 1)', function (): any {
    return assertEqual(dashify(add)(__, 2)(1), 3);
  });
  function sub(x: any, y: any): any {
    return x - y;
  }
  it('((dashify sub) 1 2)', function (): any {
    return assertEqual(dashify(sub)(1, 2), -1);
  });
  it('(((dashify sub) __ 2) 1)', function (): any {
    return assertEqual(dashify(sub)(__, 2)(1), -1);
  });
  it('((((dashify sub) __ 2) __) 1)', function (): any {
    return assertEqual(dashify(sub)(__, 2)(__)(1), -1);
  });
  it('((((dashify sub) __ 2) __ __) 1)', function (): any {
    return assertEqual(dashify(sub)(__, 2)(__, __)(1), -1);
  });
  it('(((((dashify sub) __ 2) __) __) 1)', function (): any {
    return assertEqual(dashify(sub)(__, 2)(__)(__)(1), -1);
  });
  return it('(((dashify sub) 1 __) 2)', function (): any {
    return assertEqual(dashify(sub)(1, __)(2), -1);
  });
});