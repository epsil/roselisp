import * as chai from 'chai';

import { I, K } from '../../src/ts/combinators';

import { eof, memoize } from '../../src/ts/memo';

import { assertEqual } from './test-util';

describe('memoize', function (): any {
  it('cache property', function (): any {
    const memoizedF: any = memoize(I);
    return assertEqual(memoizedF.cache instanceof Map, true);
  });
  it('cache I()', function (): any {
    const memoizedF: any = memoize(I);
    assertEqual(memoizedF() === undefined, true);
    return assertEqual(memoizedF.cache, new Map([[eof, undefined]]));
  });
  it('cache I(1)', function (): any {
    const memoizedF: any = memoize(I);
    assertEqual(memoizedF(1), 1);
    assertEqual(memoizedF.cache, new Map([[1, new Map([[eof, 1]])]]));
    // Change cached value and verify that
    // the cached value is returned.
    memoizedF.cache = new Map([[1, new Map([[eof, 500]])]]);
    return assertEqual(memoizedF(1), 500);
  });
  return it('cache K(1, 2)', function (): any {
    const memoizedF: any = memoize(K);
    assertEqual(memoizedF(1, 2), 1);
    return assertEqual(
      memoizedF.cache,
      new Map([[1, new Map([[2, new Map([[eof, 1]])]])]])
    );
  });
});
