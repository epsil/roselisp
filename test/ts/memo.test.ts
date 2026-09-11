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

describe('memoize', function (): any {
  it('cache', function (): any {
    return assertEqual(((IM: any): any => {
      return IM.cache instanceof Map;
    })(memoize(I)), true);
  });
  it('(I)', function (): any {
    return assertEqual(((IM: any): any => {
      return IM() === undefined;
    })(memoize(I)), true);
  });
  it('(I), (I)', function (): any {
    return assertEqual(((IM: any): any => {
      IM();
      return IM() === undefined;
    })(memoize(I)), true);
  });
  it('(I), cache', function (): any {
    return assertEqual(((IM: any): any => {
      IM();
      return IM.cache;
    })(memoize(I)), new Map([[eof, undefined]]));
  });
  it('(I 1)', function (): any {
    return assertEqual(((IM: any): any => {
      return IM(1);
    })(memoize(I)), 1);
  });
  it('(I 1), (I 1)', function (): any {
    return assertEqual(((IM: any): any => {
      IM(1);
      return IM(1);
    })(memoize(I)), 1);
  });
  it('(I 1), cache', function (): any {
    return assertEqual(((IM: any): any => {
      IM(1);
      return IM.cache;
    })(memoize(I)), new Map([[1, new Map([[eof, 1]])]]));
  });
  it('(I 1), change cache', function (): any {
    return assertEqual(((IM: any): any => {
      IM(1);
      IM.cache = new Map([[1, new Map([[eof, 500]])]]);
      return IM(1);
    })(memoize(I)), 500);
  });
  it('(K 1 2)', function (): any {
    return assertEqual(((KM: any): any => {
      return KM(1, 2);
    })(memoize(K)), 1);
  });
  it('(K 1 2), (K 1 2)', function (): any {
    return assertEqual(((KM: any): any => {
      KM(1, 2);
      return KM(1, 2);
    })(memoize(K)), 1);
  });
  return it('(K 1 2), cache', function (): any {
    return assertEqual(((KM: any): any => {
      KM(1, 2);
      return KM.cache;
    })(memoize(K)), new Map([[1, new Map([[2, new Map([[eof, 1]])]])]]));
  });
});