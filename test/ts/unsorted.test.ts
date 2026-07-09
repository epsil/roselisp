/**
 * # Unsorted tests
 *
 * Tests that have not been sorted yet.
 */

import { assertEqual, testRepl } from './test-util';

const [callCc]: any[] = ((): any => {
  function callWithCurrentContinuation_(
    proc: any,
    promptTag: any = undefined
  ): any {
    class CallCCWrapper {
      value: any;

      constructor(value: any) {
        this.value = value;
      }
    }
    try {
      return proc((value: any): any => {
        throw new CallCCWrapper(value);
      });
    } catch (e) {
      if (e instanceof CallCCWrapper) {
        return e.value;
      } else {
        throw e;
      }
    }
  }
  return [callWithCurrentContinuation_];
})();

/**
 * Test inbox
 */
describe('Unsorted tests', function (): any {
  return describe('call/cc', function (): any {
    return it('(try ... (+ 5 (call/cc (lambda (x) (error "error")))) ...)', function (): any {
      let result: any = 0;
      try {
        result =
          5 +
          callCc(function (x: any): any {
            throw new Error('error');
          });
      } catch (e) {}
      return assertEqual(result, 0);
    });
  });
});
