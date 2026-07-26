/**
 * # Unsorted tests
 *
 * Tests that have not been sorted yet.
 */

import { assertEqual, testRepl, testMacro } from './test-util';

testMacro.ftype = 'macro';

/**
 * Test inbox
 */
describe('call/cc', function (): any {
  return it('(try ... (+ 5 (call/cc (lambda (x) (error ...)))) ...)', function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('let'),
        [[Symbol.for('result'), 0]],
        [
          Symbol.for('try'),
          [
            Symbol.for('set!'),
            Symbol.for('result'),
            [
              Symbol.for('+'),
              5,
              [
                Symbol.for('call/cc'),
                [
                  Symbol.for('lambda'),
                  [Symbol.for('x')],
                  [Symbol.for('error'), 'error'],
                ],
              ],
            ],
          ],
          [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e')],
        ],
        Symbol.for('result'),
      ],
      0,
    ]);
  });
});
