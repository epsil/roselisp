import { __, curry } from '../../src/ts/curry';

import { assertEqual, testMacro } from './test-util';

describe('curry', function (): any {
  it('(a 1)', function (): any {
    return assertEqual(
      ((): any => {
        const a: any = function (x: any): any {
          return [x];
        };
        const aC: any = curry(a);
        return aC(1);
      })(),
      [1]
    );
  });
  it('((a) 1)', function (): any {
    return assertEqual(
      ((): any => {
        const a: any = function (x: any): any {
          return [x];
        };
        const aC: any = curry(a);
        return aC()(1);
      })(),
      [1]
    );
  });
  it('(ab 1 2)', function (): any {
    return assertEqual(
      ((): any => {
        const ab: any = function (x: any, y: any): any {
          return [x, y];
        };
        const abC: any = curry(ab);
        return abC(1, 2);
      })(),
      [1, 2]
    );
  });
  it('((ab 1) 2)', function (): any {
    return assertEqual(
      ((): any => {
        const ab: any = function (x: any, y: any): any {
          return [x, y];
        };
        const abC: any = curry(ab);
        return abC(1)(2);
      })(),
      [1, 2]
    );
  });
  it('(((ab) 1) 2)', function (): any {
    return assertEqual(
      ((): any => {
        const ab: any = function (x: any, y: any): any {
          return [x, y];
        };
        const abC: any = curry(ab);
        return abC()(1)(2);
      })(),
      [1, 2]
    );
  });
  it('(abc 1 2 3)', function (): any {
    return assertEqual(
      ((): any => {
        const abc: any = function (x: any, y: any, z: any): any {
          return [x, y, z];
        };
        const abcC: any = curry(abc);
        return abcC(1, 2, 3);
      })(),
      [1, 2, 3]
    );
  });
  it('((abc 1 2) 3)', function (): any {
    return assertEqual(
      ((): any => {
        const abc: any = function (x: any, y: any, z: any): any {
          return [x, y, z];
        };
        const abcC: any = curry(abc);
        return abcC(1, 2)(3);
      })(),
      [1, 2, 3]
    );
  });
  it('(((abc 1) 2) 3)', function (): any {
    return assertEqual(
      ((): any => {
        const abc: any = function (x: any, y: any, z: any): any {
          return [x, y, z];
        };
        const abcC: any = curry(abc);
        return abcC(1)(2)(3);
      })(),
      [1, 2, 3]
    );
  });
  it('((((abc) 1) 2) 3)', function (): any {
    return assertEqual(
      ((): any => {
        const abc: any = function (x: any, y: any, z: any): any {
          return [x, y, z];
        };
        const abcC: any = curry(abc);
        return abcC()(1)(2)(3);
      })(),
      [1, 2, 3]
    );
  });
  it('(abc 1)', function (): any {
    return assertEqual(
      ((): any => {
        const abc: any = function (x: any, y: any, z: any): any {
          return [x, y, z];
        };
        const abcC: any = curry(abc, 1);
        return abcC(1);
      })(),
      [1, undefined, undefined]
    );
  });
  it('(abc 1 2)', function (): any {
    return assertEqual(
      ((): any => {
        const abc: any = function (x: any, y: any, z: any): any {
          return [x, y, z];
        };
        const abcC: any = curry(abc, 1);
        return abcC(1, 2);
      })(),
      [1, 2, undefined]
    );
  });
  it('(abc 1 2 3)', function (): any {
    return assertEqual(
      ((): any => {
        const abc: any = function (x: any, y: any, z: any): any {
          return [x, y, z];
        };
        const abcC: any = curry(abc, 1);
        return abcC(1, 2, 3);
      })(),
      [1, 2, 3]
    );
  });
  it('((abc _ _ _) 1 2 3)', function (): any {
    return assertEqual(
      ((): any => {
        const abc: any = function (x: any, y: any, z: any): any {
          return [x, y, z];
        };
        const abcC: any = curry(abc);
        return abcC(__, __, __)(1, 2, 3);
      })(),
      [1, 2, 3]
    );
  });
  return it("_ !== '_", function (): any {
    return assertEqual(__ === Symbol.for('_'), false);
  });
});
