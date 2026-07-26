import { s } from '../../src/ts/sexp';

import { symbolp_ } from '../../src/ts/symbol';

import { assertEqual, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('s', function (): any {
  it('(s "foo")', function (): any {
    return assertEqual(s('foo'), Symbol.for('foo'));
  });
  it('(js/tag s "foo")', function (): any {
    return assertEqual(s`foo`, Symbol.for('foo'));
  });
  it('(js/tag s "foo${1}")', function (): any {
    return assertEqual(s`foo${1}`, Symbol.for('foo1'));
  });
  return it('(js/tag s "${\'foo\'}")', function (): any {
    return assertEqual(s`${'foo'}`, Symbol.for('foo'));
  });
});

describe('symbolp', function (): any {
  return it("(symbolp_ 'foo)", function (): any {
    return assertEqual(symbolp_(Symbol.for('foo')), true);
  });
});
