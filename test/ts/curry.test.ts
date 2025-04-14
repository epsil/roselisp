import * as chai from 'chai';

import { __, curry } from '../../src/ts/curry';

import { assertEqual, assertNotEqual } from './test-util';

describe('curry', function (): any {
  it('a', function (): any {
    function a(b: any): any {
      return [b];
    }
    const curriedA: any = curry(a);
    assertEqual(curriedA(1), [1]);
    return assertEqual(curriedA()(1), [1]);
  });
  it('ab', function (): any {
    function ab(a: any, b: any): any {
      return [a, b];
    }
    const curriedAB: any = curry(ab);
    assertEqual(curriedAB(1, 2), [1, 2]);
    assertEqual(curriedAB(1)(2), [1, 2]);
    return assertEqual(curriedAB()(1)(2), [1, 2]);
  });
  it('abc', function (): any {
    function abc(a: any, b: any, c: any): any {
      return [a, b, c];
    }
    const curriedABC: any = curry(abc);
    assertEqual(curriedABC(1, 2, 3), [1, 2, 3]);
    assertEqual(curriedABC(1, 2)(3), [1, 2, 3]);
    assertEqual(curriedABC(1)(2)(3), [1, 2, 3]);
    return assertEqual(curriedABC()(1)(2)(3), [1, 2, 3]);
  });
  it('arity', function (): any {
    function abc(a: any, b: any, c: any): any {
      return [a, b, c];
    }
    const curriedABC1: any = curry(abc, 1);
    assertEqual(curriedABC1(1), [1, undefined, undefined]);
    assertEqual(curriedABC1(1, 2), [1, 2, undefined]);
    return assertEqual(curriedABC1(1, 2, 3), [1, 2, 3]);
  });
  it('wildcards', function (): any {
    function abc(a: any, b: any, c: any): any {
      return [a, b, c];
    }
    const curriedABC1: any = curry(abc);
    return assertEqual(curriedABC1(__, __, __)(1, 2, 3), [1, 2, 3]);
  });
  return it("__ !== '_", function (): any {
    return assertNotEqual(__, Symbol.for('_'));
  });
});
