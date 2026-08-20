/**
 * # REPL
 *
 * Various tests of the interactive interface.
 */

import { rep } from '../../src/ts/repl';

import { assertEqual, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('REPL', function (): any {
  it('(rep "#t")', function (): any {
    return assertEqual(rep('#t'), '#t');
  });
  it('(rep "\\"string\\"")', function (): any {
    return assertEqual(rep('"string"'), '"string"');
  });
  it('(rep "(+ 1 1)")', function (): any {
    return assertEqual(rep('(+ 1 1)'), '2');
  });
  return it('(rep "(+ 1 1) (+ 1 1)")', function (): any {
    return assertEqual(rep('(+ 1 1) (+ 1 1)'), '2\n' + '2');
  });
});
