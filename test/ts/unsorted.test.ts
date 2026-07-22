/**
 * # Unsorted tests
 *
 * Tests that have not been sorted yet.
 */

import { assertEqual, testRepl, testMacro } from './test-util';

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
describe('call/cc', function (): any {
  return it('(let ((result 0)) (try (set! result (+ 5 (call/cc (lambda (x) (error "error"))))) (catch Object e)) result)', function (): any {
    return assertEqual(
      ((): any => {
        let result: any = 0;
        try {
          result =
            5 +
            callCc(function (x: any): any {
              throw new Error('error');
            });
        } catch (e) {}
        return result;
      })(),
      0
    );
  });
});
