import { __, curried, variadic } from '../../src/ts/combinators';

import { assertEqual, testMacro } from './test-util';

describe('curried', function (): any {
  const { C, Y } = curried;
  function subtraction(x: any, y: any): any {
    return x - y;
  }
  it('(subtraction 1 2)', function (): any {
    return assertEqual(subtraction(1, 2), -1);
  });
  it('(C subtraction 1 2)', function (): any {
    return assertEqual(C(subtraction, 1, 2), 1);
  });
  it('((C subtraction) 1 2)', function (): any {
    return assertEqual(C(subtraction)(1, 2), 1);
  });
  it('(((C subtraction) 1) 2)', function (): any {
    return assertEqual(C(subtraction)(1)(2), 1);
  });
  function factorial(x: any): any {
    if (x === 0) {
      return 1;
    } else {
      return x * factorial(x - 1);
    }
  }
  const factorialY: any = Y(function (factorial: any): any {
    return function (x: any): any {
      if (x === 0) {
        return 1;
      } else {
        return x * factorial(x - 1);
      }
    };
  });
  it('(factorial 6)', function (): any {
    return assertEqual(factorial(6), 1 * 2 * 3 * 4 * 5 * 6);
  });
  return it('(factorialY 6)', function (): any {
    return assertEqual(factorialY(6), 1 * 2 * 3 * 4 * 5 * 6);
  });
});

describe('variadic', function (): any {
  const { A, B, I, Q, T } = variadic;
  it('(A I 1)', function (): any {
    return assertEqual(A(I, 1), 1);
  });
  it('(A (fn (x) (+ x 4)) 1)', function (): any {
    return assertEqual(
      A(function (x: any): any {
        return x + 4;
      }, 1),
      5
    );
  });
  it('(A (fn (x y) (+ x y)) 1 1)', function (): any {
    return assertEqual(
      A(
        function (x: any, y: any): any {
          return x + y;
        },
        1,
        1
      ),
      2
    );
  });
  it('((A __ 1 1) (fn (x y) (+ x y)))', function (): any {
    return assertEqual(
      A(
        __,
        1,
        1
      )(function (x: any, y: any): any {
        return x + y;
      }),
      2
    );
  });
  it('((A (fn (x y) (+ x y)) __ 1) 1)', function (): any {
    return assertEqual(
      A(
        function (x: any, y: any): any {
          return x + y;
        },
        __,
        1
      )(1),
      2
    );
  });
  it('((A (fn (x y) (+ x y)) 1 __) 1)', function (): any {
    return assertEqual(
      A(
        function (x: any, y: any): any {
          return x + y;
        },
        1,
        __
      )(1),
      2
    );
  });
  it('(eq? (B) #u)', function (): any {
    return assertEqual(B() === undefined, true);
  });
  it('(B 1)', function (): any {
    return assertEqual(B(1), 1);
  });
  it('(B I 1)', function (): any {
    return assertEqual(B(I, 1), 1);
  });
  it('(B I I 1)', function (): any {
    return assertEqual(B(I, I, 1), 1);
  });
  it('(B I I I 1)', function (): any {
    return assertEqual(B(I, I, I, 1), 1);
  });
  it('(B (fn (x) (- x)) (fn (x) (+ x 4)) 5)', function (): any {
    return assertEqual(
      B(
        function (x: any): any {
          return -x;
        },
        function (x: any): any {
          return x + 4;
        },
        5
      ),
      -9
    );
  });
  it('((B (fn (x) (- x)) (fn (x) (+ x 4)) __) 5)', function (): any {
    return assertEqual(
      B(
        function (x: any): any {
          return -x;
        },
        function (x: any): any {
          return x + 4;
        },
        __
      )(5),
      -9
    );
  });
  it('(eq? (I) #u)', function (): any {
    return assertEqual(I() === undefined, true);
  });
  it('(I I)', function (): any {
    return assertEqual(I(I), I);
  });
  it('(I 1)', function (): any {
    return assertEqual(I(1), 1);
  });
  it('(I I I 1)', function (): any {
    return assertEqual(I(I, I, 1), I);
  });
  it('(I I I I 1)', function (): any {
    return assertEqual(I(I, I, I, 1), I);
  });
  it('(I I I I I 1)', function (): any {
    return assertEqual(I(I, I, I, I, 1), I);
  });
  it('((I __) 1)', function (): any {
    return assertEqual(I(__)(1), 1);
  });
  it('((I __) I)', function (): any {
    return assertEqual(I(__)(I), I);
  });
  it('(eq? (Q) #u)', function (): any {
    return assertEqual(Q() === undefined, true);
  });
  it('(Q 1)', function (): any {
    return assertEqual(Q(1), 1);
  });
  it('(Q I 1)', function (): any {
    return assertEqual(Q(I, 1), 1);
  });
  it('(Q I I 1)', function (): any {
    return assertEqual(Q(I, I, 1), 1);
  });
  it('(Q I I I 1)', function (): any {
    return assertEqual(Q(I, I, I, 1), 1);
  });
  it('(Q (fn (x) (+ x 4)) (fn (x) (- x)) 5)', function (): any {
    return assertEqual(
      Q(
        function (x: any): any {
          return x + 4;
        },
        function (x: any): any {
          return -x;
        },
        5
      ),
      -9
    );
  });
  it('((Q (fn (x) (+ x 4)) (fn (x) (- x)) __) 5)', function (): any {
    return assertEqual(
      Q(
        function (x: any): any {
          return x + 4;
        },
        function (x: any): any {
          return -x;
        },
        __
      )(5),
      -9
    );
  });
  it('(eq? (T) #u)', function (): any {
    return assertEqual(T() === undefined, true);
  });
  it('(T 1)', function (): any {
    return assertEqual(T(1), 1);
  });
  it('(T 1 I)', function (): any {
    return assertEqual(T(1, I), 1);
  });
  it('(T 1 I I)', function (): any {
    return assertEqual(T(1, I, I), 1);
  });
  it('(T 1 I I I)', function (): any {
    return assertEqual(T(1, I, I, I), 1);
  });
  it('(T 5 (fn (x) (+ x 4)) (fn (x) (- x)))', function (): any {
    return assertEqual(
      T(
        5,
        function (x: any): any {
          return x + 4;
        },
        function (x: any): any {
          return -x;
        }
      ),
      -9
    );
  });
  return it('((T __ (fn (x) (+ x 4)) (fn (x) (- x))) 5)', function (): any {
    return assertEqual(
      T(
        __,
        function (x: any): any {
          return x + 4;
        },
        function (x: any): any {
          return -x;
        }
      )(5),
      -9
    );
  });
});
