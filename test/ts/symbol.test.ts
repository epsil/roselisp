import {
  s
} from '../../src/ts/sexp';

import {
  symbolp_
} from '../../src/ts/symbol';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('s', (): any => {
  it('(s "foo")', (): any => assertEqual(s('foo'), Symbol.for('foo')));
  it('(js/tag s "foo")', (): any => assertEqual(s`foo`, Symbol.for('foo')));
  it('(js/tag s "foo${1}")', (): any => assertEqual(s`foo${1}`, Symbol.for('foo1')));
  return it('(js/tag s "${\'foo\'}")', (): any => assertEqual(s`${'foo'}`, Symbol.for('foo')));
});

describe('symbolp', (): any => it('(symbolp_ \'foo)', (): any => assertEqual(symbolp_(Symbol.for('foo')), true)));