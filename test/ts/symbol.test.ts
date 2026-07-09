import { s } from '../../src/ts/sexp';

import { symbolp_ } from '../../src/ts/symbol';

import { assertEqual } from './test-util';

describe('s', function (): any {
  it("s('foo')", function (): any {
    assertEqual(s`foo`, Symbol.for('foo'));
    return assertEqual(s('foo'), Symbol.for('foo'));
  });
  it("s`${'foo'}`", function (): any {
    return assertEqual(s`${'foo'}`, Symbol.for('foo'));
  });
  return it('s`foo${2}`', function (): any {
    return assertEqual(s`foo${2}`, Symbol.for('foo2'));
  });
});

describe('symbolp', function (): any {
  return it('s`foo`', function (): any {
    return assertEqual(symbolp_(s`foo`), true);
  });
});
