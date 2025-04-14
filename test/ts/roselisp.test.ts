import * as chai from 'chai';

import { add, div, license, mul, sub } from '../../src/ts/index';

import { assertEqual } from './test-util';

describe('license', function (): any {
  return it('MPL-2.0', function (): any {
    return assertEqual(license, Symbol.for('MPL-2.0'));
  });
});

describe('add', function (): any {
  it('(add)', function (): any {
    return assertEqual(add(), 0);
  });
  it('(add 1)', function (): any {
    return assertEqual(add(1), 1);
  });
  it('(add 1 2)', function (): any {
    return assertEqual(add(1, 2), 3);
  });
  return it('(add 1 2 4)', function (): any {
    return assertEqual(add(1, 2, 4), 7);
  });
});

describe('sub', function (): any {
  it('(sub)', function (): any {
    return assertEqual(sub(), 0);
  });
  it('(sub 1)', function (): any {
    return assertEqual(sub(1), -1);
  });
  it('(sub 1 2)', function (): any {
    return assertEqual(sub(1, 2), -1);
  });
  return it('(sub 1 2 4)', function (): any {
    return assertEqual(sub(1, 2, 4), -5);
  });
});

describe('mul', function (): any {
  it('(mul)', function (): any {
    return assertEqual(mul(), 1);
  });
  it('(mul 1)', function (): any {
    return assertEqual(mul(1), 1);
  });
  it('(mul 1 2)', function (): any {
    return assertEqual(mul(1, 2), 2);
  });
  return it('(mul 1 2 4)', function (): any {
    return assertEqual(mul(1, 2, 4), 8);
  });
});

describe('div', function (): any {
  xit('(div)', function (): any {
    return assertEqual(div(), undefined);
  });
  it('(div 1)', function (): any {
    return assertEqual(div(1), 1);
  });
  it('(div 1 2)', function (): any {
    return assertEqual(div(1, 2), 0.5);
  });
  return it('(div 1 2 4)', function (): any {
    return assertEqual(div(1, 2, 4), 0.125);
  });
});
