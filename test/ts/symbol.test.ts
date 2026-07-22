import { s } from '../../src/ts/sexp';

import { symbolp_ } from '../../src/ts/symbol';

import { assertEqual, testMacro } from './test-util';

describe('s', function (): any {
  it('(js/tag s "foo")', function (): any {
    return assertEqual(s`foo`, Symbol.for('foo'));
  });
  it('(s "foo")', function (): any {
    return assertEqual(s('foo'), Symbol.for('foo'));
  });
  it('(js/tag s "${\'foo\'}")', function (): any {
    return assertEqual(s`${'foo'}`, Symbol.for('foo'));
  });
  return it('(js/tag s "foo${2}")', function (): any {
    return assertEqual(s`foo${2}`, Symbol.for('foo2'));
  });
});

describe('symbolp', function (): any {
  return it('(symbolp_ (js/tag s "foo"))', function (): any {
    return assertEqual(symbolp_(s`foo`), true);
  });
});
