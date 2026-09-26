import {
  I,
  K
} from '../../src/ts/combinators';

import {
  eof,
  memoize
} from '../../src/ts/memo';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('memoize', (): any => {
  it('cache', (): any => assertEqual(((IM: any): any => IM.cache instanceof Map)(memoize(I)), true));
  it('(I)', (): any => assertEqual(((IM: any): any => IM() === undefined)(memoize(I)), true));
  it('(I), (I)', (): any => assertEqual(((IM: any): any => {
    IM();
    return IM() === undefined;
  })(memoize(I)), true));
  it('(I), cache', (): any => assertEqual(((IM: any): any => {
    IM();
    return IM.cache;
  })(memoize(I)), new Map([[eof, undefined]])));
  it('(I 1)', (): any => assertEqual(((IM: any): any => IM(1))(memoize(I)), 1));
  it('(I 1), (I 1)', (): any => assertEqual(((IM: any): any => {
    IM(1);
    return IM(1);
  })(memoize(I)), 1));
  it('(I 1), cache', (): any => assertEqual(((IM: any): any => {
    IM(1);
    return IM.cache;
  })(memoize(I)), new Map([[1, new Map([[eof, 1]])]])));
  it('(I 1), change cache', (): any => assertEqual(((IM: any): any => {
    IM(1);
    IM.cache = new Map([[1, new Map([[eof, 500]])]]);
    return IM(1);
  })(memoize(I)), 500));
  it('(K 1 2)', (): any => assertEqual(((KM: any): any => KM(1, 2))(memoize(K)), 1));
  it('(K 1 2), (K 1 2)', (): any => assertEqual(((KM: any): any => {
    KM(1, 2);
    return KM(1, 2);
  })(memoize(K)), 1));
  return it('(K 1 2), cache', (): any => assertEqual(((KM: any): any => {
    KM(1, 2);
    return KM.cache;
  })(memoize(K)), new Map([[1, new Map([[2, new Map([[eof, 1]])]])]])));
});