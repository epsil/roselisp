/**
 * # Property lists
 *
 * Tests of property list functions.
 */

import {
  plistSetX_
} from '../../src/ts/plist';

import {
  assertEqual,
  testRepl,
  testMacro
} from './test-util';

const [plistSetX]: any[] = ((): any => {
  function plistSetX_(plst: any, prop: any, val: any): any {
    let found: any = false;
    const _end: any = plst.length;
    for (let i: any = 0; i < _end; i = i + 2) {
      if ((plst as any)[i] === prop) {
        plst[i + 1] = val;
        found = true;
        break;
      }
    }
    if (!found) {
      plst.push(prop);
      plst.push(val);
    }
    return undefined;
  }
  return [plistSetX_];
})();

testMacro.ftype = 'macro';

describe('Property lists', function (): any {
  it('(let ((plst \'())) (plist-set! plst :foo \'bar) plst)', function (): any {
    return assertEqual(((): any => {
      const plst: any = [];
      plistSetX(plst, Symbol.for(':foo'), Symbol.for('bar'));
      return plst;
    })(), [Symbol.for(':foo'), Symbol.for('bar')]);
  });
  return it('(let ((plst \'(:foo bar))) (plist-set! plst :foo \'baz) plst)', function (): any {
    return assertEqual(((): any => {
      const plst: any = [Symbol.for(':foo'), Symbol.for('bar')];
      plistSetX(plst, Symbol.for(':foo'), Symbol.for('baz'));
      return plst;
    })(), [Symbol.for(':foo'), Symbol.for('baz')]);
  });
});