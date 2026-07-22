import { I, K } from '../../src/ts/combinators';

import { eof, memoize } from '../../src/ts/memo';

import { assertEqual, testMacro } from './test-util';

describe('memoize', function (): any {
  it('cache', function (): any {
    return assertEqual(
      ((): any => {
        const IM: any = memoize(I);
        return IM.cache instanceof Map;
      })(),
      true
    );
  });
  it('(I)', function (): any {
    return assertEqual(
      ((): any => {
        const IM: any = memoize(I);
        return IM() === undefined;
      })(),
      true
    );
  });
  it('(I), (I)', function (): any {
    return assertEqual(
      ((): any => {
        const IM: any = memoize(I);
        IM();
        return IM() === undefined;
      })(),
      true
    );
  });
  it('(I), cache', function (): any {
    return assertEqual(
      ((): any => {
        const IM: any = memoize(I);
        IM();
        return IM.cache;
      })(),
      new Map([[eof, undefined]])
    );
  });
  it('(I 1)', function (): any {
    return assertEqual(
      ((): any => {
        const IM: any = memoize(I);
        return IM(1);
      })(),
      1
    );
  });
  it('(I 1), (I 1)', function (): any {
    return assertEqual(
      ((): any => {
        const IM: any = memoize(I);
        IM(1);
        return IM(1);
      })(),
      1
    );
  });
  it('(I 1), cache', function (): any {
    return assertEqual(
      ((): any => {
        const IM: any = memoize(I);
        IM(1);
        return IM.cache;
      })(),
      new Map([[1, new Map([[eof, 1]])]])
    );
  });
  it('(I 1), change cache', function (): any {
    return assertEqual(
      ((): any => {
        const IM: any = memoize(I);
        IM(1);
        IM.cache = new Map([[1, new Map([[eof, 500]])]]);
        return IM(1);
      })(),
      500
    );
  });
  it('(K 1 2)', function (): any {
    return assertEqual(
      ((): any => {
        const KM: any = memoize(K);
        return KM(1, 2);
      })(),
      1
    );
  });
  it('(K 1 2), (K 1 2)', function (): any {
    return assertEqual(
      ((): any => {
        const KM: any = memoize(K);
        KM(1, 2);
        return KM(1, 2);
      })(),
      1
    );
  });
  return it('(K 1 2), cache', function (): any {
    return assertEqual(
      ((): any => {
        const KM: any = memoize(K);
        KM(1, 2);
        return KM.cache;
      })(),
      new Map([[1, new Map([[2, new Map([[eof, 1]])]])]])
    );
  });
});
