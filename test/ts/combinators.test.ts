import {
  __,
  curried,
  variadic
} from '../../src/ts/combinators';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('curried', (): any => {
  const {C, Y} = curried;
  function subtraction(x: any, y: any): any {
    return x - y;
  }
  it('(subtraction 1 2)', (): any => assertEqual(subtraction(1, 2), -1));
  it('(C subtraction 1 2)', (): any => assertEqual(C(subtraction, 1, 2), 1));
  it('((C subtraction) 1 2)', (): any => assertEqual(C(subtraction)(1, 2), 1));
  it('(((C subtraction) 1) 2)', (): any => assertEqual(C(subtraction)(1)(2), 1));
  function factorial(x: any): any {
    if (x === 0) {
      return 1;
    } else {
      return x * factorial(x - 1);
    }
  }
  const factorialY: any = Y((factorial: any): any => (x: any): any => {
    if (x === 0) {
      return 1;
    } else {
      return x * factorial(x - 1);
    }
  });
  it('(factorial 6)', (): any => assertEqual(factorial(6), 1 * 2 * 3 * 4 * 5 * 6));
  return it('(factorialY 6)', (): any => assertEqual(factorialY(6), 1 * 2 * 3 * 4 * 5 * 6));
});

describe('variadic', (): any => {
  const {A, B, I, Q, T} = variadic;
  it('(A I 1)', (): any => assertEqual(A(I, 1), 1));
  it('(A (fn (x) (+ x 4)) 1)', (): any => assertEqual(A((x: any): any => x + 4, 1), 5));
  it('(A (fn (x y) (+ x y)) 1 1)', (): any => assertEqual(A((x: any, y: any): any => x + y, 1, 1), 2));
  it('((A __ 1 1) (fn (x y) (+ x y)))', (): any => assertEqual(A(__, 1, 1)((x: any, y: any): any => x + y), 2));
  it('((A (fn (x y) (+ x y)) __ 1) 1)', (): any => assertEqual(A((x: any, y: any): any => x + y, __, 1)(1), 2));
  it('((A (fn (x y) (+ x y)) 1 __) 1)', (): any => assertEqual(A((x: any, y: any): any => x + y, 1, __)(1), 2));
  it('(eq? (B) #u)', (): any => assertEqual(B() === undefined, true));
  it('(B 1)', (): any => assertEqual(B(1), 1));
  it('(B I 1)', (): any => assertEqual(B(I, 1), 1));
  it('(B I I 1)', (): any => assertEqual(B(I, I, 1), 1));
  it('(B I I I 1)', (): any => assertEqual(B(I, I, I, 1), 1));
  it('(B (fn (x) (- x)) (fn (x) (+ x 4)) 5)', (): any => assertEqual(B((x: any): any => -x, (x: any): any => x + 4, 5), -9));
  it('((B (fn (x) (- x)) (fn (x) (+ x 4)) __) 5)', (): any => assertEqual(B((x: any): any => -x, (x: any): any => x + 4, __)(5), -9));
  it('(eq? (I) #u)', (): any => assertEqual(I() === undefined, true));
  it('(I I)', (): any => assertEqual(I(I), I));
  it('(I 1)', (): any => assertEqual(I(1), 1));
  it('(I I I 1)', (): any => assertEqual(I(I, I, 1), I));
  it('(I I I I 1)', (): any => assertEqual(I(I, I, I, 1), I));
  it('(I I I I I 1)', (): any => assertEqual(I(I, I, I, I, 1), I));
  it('((I __) 1)', (): any => assertEqual(I(__)(1), 1));
  it('((I __) I)', (): any => assertEqual(I(__)(I), I));
  it('(eq? (Q) #u)', (): any => assertEqual(Q() === undefined, true));
  it('(Q 1)', (): any => assertEqual(Q(1), 1));
  it('(Q I 1)', (): any => assertEqual(Q(I, 1), 1));
  it('(Q I I 1)', (): any => assertEqual(Q(I, I, 1), 1));
  it('(Q I I I 1)', (): any => assertEqual(Q(I, I, I, 1), 1));
  it('(Q (fn (x) (+ x 4)) (fn (x) (- x)) 5)', (): any => assertEqual(Q((x: any): any => x + 4, (x: any): any => -x, 5), -9));
  it('((Q (fn (x) (+ x 4)) (fn (x) (- x)) __) 5)', (): any => assertEqual(Q((x: any): any => x + 4, (x: any): any => -x, __)(5), -9));
  it('(eq? (T) #u)', (): any => assertEqual(T() === undefined, true));
  it('(T 1)', (): any => assertEqual(T(1), 1));
  it('(T 1 I)', (): any => assertEqual(T(1, I), 1));
  it('(T 1 I I)', (): any => assertEqual(T(1, I, I), 1));
  it('(T 1 I I I)', (): any => assertEqual(T(1, I, I, I), 1));
  it('(T 5 (fn (x) (+ x 4)) (fn (x) (- x)))', (): any => assertEqual(T(5, (x: any): any => x + 4, (x: any): any => -x), -9));
  return it('((T __ (fn (x) (+ x 4)) (fn (x) (- x))) 5)', (): any => assertEqual(T(__, (x: any): any => x + 4, (x: any): any => -x)(5), -9));
});