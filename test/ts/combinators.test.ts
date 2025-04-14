import * as chai from 'chai';

import { __, curried, variadic } from '../../src/ts/combinators';

import { assertEqual } from './test-util';

describe('curried', function (): any {
  const { C, Y } = curried;
  describe('C', function (): any {
    return it('C (-) 1 2', function (): any {
      function subtraction(x: any, y: any): any {
        return x - y;
      }
      assertEqual(subtraction(1, 2), -1);
      assertEqual(C(subtraction, 1, 2), 1);
      assertEqual(C(subtraction)(1, 2), 1);
      return assertEqual(C(subtraction)(1)(2), 1);
    });
  });
  return describe('Y', function (): any {
    return it('6!', function (): any {
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
      assertEqual(factorial(6), 1 * 2 * 3 * 4 * 5 * 6);
      return assertEqual(factorialY(6), 1 * 2 * 3 * 4 * 5 * 6);
    });
  });
});

describe('variadic', function (): any {
  const { A, B, I, Q, T } = variadic;
  describe('A', function (): any {
    it('A I 1', function (): any {
      return assertEqual(A(I, 1), 1);
    });
    it('A (λx. x + 4) 1', function (): any {
      return assertEqual(
        A(function (x: any): any {
          return x + 4;
        }, 1),
        5
      );
    });
    it('A (+) 1 1', function (): any {
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
    it('((A _ 1 1) +)', function (): any {
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
    it('((A + _ 1) 1)', function (): any {
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
    return it('((A + 1 _) 1)', function (): any {
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
  });
  describe('B', function (): any {
    it('B', function (): any {
      return assertEqual(B() === undefined, true);
    });
    it('B 1', function (): any {
      return assertEqual(B(1), 1);
    });
    it('B I 1', function (): any {
      return assertEqual(B(I, 1), 1);
    });
    it('B I I 1', function (): any {
      return assertEqual(B(I, I, 1), 1);
    });
    it('B I I I 1', function (): any {
      return assertEqual(B(I, I, I, 1), 1);
    });
    it('B (λx. -x) (λx. x + 4) 5', function (): any {
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
    return it('((B (λx. -x) (λx. x + 4) _) 5)', function (): any {
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
  });
  describe('I', function (): any {
    it('I', function (): any {
      return assertEqual(I() === undefined, true);
    });
    it('I I', function (): any {
      return assertEqual(I(I), I);
    });
    it('I 1', function (): any {
      return assertEqual(I(1), 1);
    });
    it('I I 1', function (): any {
      return assertEqual(I(I, I, 1), I);
    });
    it('I I I 1', function (): any {
      return assertEqual(I(I, I, I, 1), I);
    });
    it('I I I I 1', function (): any {
      return assertEqual(I(I, I, I, I, 1), I);
    });
    it('(I _) 1', function (): any {
      return assertEqual(I(__)(1), 1);
    });
    return it('(I _) I', function (): any {
      return assertEqual(I(__)(I), I);
    });
  });
  describe('Q', function (): any {
    it('Q', function (): any {
      return assertEqual(Q() === undefined, true);
    });
    it('Q 1', function (): any {
      return assertEqual(Q(1), 1);
    });
    it('Q I 1', function (): any {
      return assertEqual(Q(I, 1), 1);
    });
    it('Q I I 1', function (): any {
      return assertEqual(Q(I, I, 1), 1);
    });
    it('Q I I I 1', function (): any {
      return assertEqual(Q(I, I, I, 1), 1);
    });
    it('Q (λx. x + 4) (λx. -x) 5', function (): any {
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
    return it('((Q (λx. x + 4) (λx. -x) _) 5)', function (): any {
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
  });
  return describe('T', function (): any {
    it('T', function (): any {
      return assertEqual(T() === undefined, true);
    });
    it('T 1', function (): any {
      return assertEqual(T(1), 1);
    });
    it('T 1 I', function (): any {
      return assertEqual(T(1, I), 1);
    });
    it('T 1 I I', function (): any {
      return assertEqual(T(1, I, I), 1);
    });
    it('T 1 I I I', function (): any {
      return assertEqual(T(1, I, I, I), 1);
    });
    it('T 5 (λx. x + 4) (λx. -x)', function (): any {
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
    return it('((T _ (λx. x + 4) (λx. -x)) 5)', function (): any {
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
});
