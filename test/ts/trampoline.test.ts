import * as chai from 'chai';

import { tcall, trampoline } from '../../src/ts/trampoline';

import { assertEqual } from './test-util';

describe('trampoline', function (): any {
  it('1', function (): any {
    return assertEqual(
      trampoline(function (x: any): any {
        return x;
      }, 1),
      1
    );
  });
  describe('fibonacci', function (): any {
    function add(x: any, y: any): any {
      return x + y;
    }
    function fibonacci(n: any): any {
      if (n < 2) {
        return n;
      } else {
        return tcall(add, tcall(fibonacci, n - 1), tcall(fibonacci, n - 2));
      }
    }
    it('0', function (): any {
      return assertEqual(trampoline(fibonacci, 0), 0);
    });
    it('1', function (): any {
      return assertEqual(trampoline(fibonacci, 1), 1);
    });
    it('2', function (): any {
      return assertEqual(trampoline(fibonacci, 2), 1);
    });
    it('3', function (): any {
      return assertEqual(trampoline(fibonacci, 3), 2);
    });
    it('4', function (): any {
      return assertEqual(trampoline(fibonacci, 4), 3);
    });
    it('5', function (): any {
      return assertEqual(trampoline(fibonacci, 5), 5);
    });
    it('6', function (): any {
      return assertEqual(trampoline(fibonacci, 6), 8);
    });
    it('7', function (): any {
      return assertEqual(trampoline(fibonacci, 7), 13);
    });
    it('8', function (): any {
      return assertEqual(trampoline(fibonacci, 8), 21);
    });
    it('9', function (): any {
      return assertEqual(trampoline(fibonacci, 9), 34);
    });
    return it('10', function (): any {
      return assertEqual(trampoline(fibonacci, 10), 55);
    });
  });
  return describe('sequence', function (): any {
    function sub(x: any, y: any): any {
      return x - y;
    }
    function sequence(n: any): any {
      if (n < 2) {
        return n;
      } else {
        return tcall(sub, tcall(sequence, n - 1), tcall(sequence, n - 2));
      }
    }
    it('0', function (): any {
      return assertEqual(trampoline(sequence, 0), 0);
    });
    it('1', function (): any {
      return assertEqual(trampoline(sequence, 1), 1);
    });
    it('2', function (): any {
      return assertEqual(trampoline(sequence, 2), 1);
    });
    it('3', function (): any {
      return assertEqual(trampoline(sequence, 3), 0);
    });
    it('4', function (): any {
      return assertEqual(trampoline(sequence, 4), -1);
    });
    it('5', function (): any {
      return assertEqual(trampoline(sequence, 5), -1);
    });
    it('6', function (): any {
      return assertEqual(trampoline(sequence, 6), 0);
    });
    it('7', function (): any {
      return assertEqual(trampoline(sequence, 7), 1);
    });
    it('8', function (): any {
      return assertEqual(trampoline(sequence, 8), 1);
    });
    it('9', function (): any {
      return assertEqual(trampoline(sequence, 9), 0);
    });
    return it('10', function (): any {
      return assertEqual(trampoline(sequence, 10), -1);
    });
  });
});
