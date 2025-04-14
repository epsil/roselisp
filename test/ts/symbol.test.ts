import * as chai from 'chai';

import { s } from '../../src/ts/sexp';

import { intern_, symbolp_, symbolToString_ } from '../../src/ts/symbol';

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

describe('intern', function (): any {
  return it('foo', function (): any {
    return assertEqual(intern_('foo'), s`foo`);
  });
});

describe('symbolp', function (): any {
  it('s`foo`', function (): any {
    return assertEqual(symbolp_(s`foo`), true);
  });
  it('1', function (): any {
    return assertEqual(symbolp_(1), false);
  });
  it("'foo'", function (): any {
    return assertEqual(symbolp_('foo'), false);
  });
  it("'{}'", function (): any {
    return assertEqual(symbolp_({}), false);
  });
  return it("'[]'", function (): any {
    return assertEqual(symbolp_([]), false);
  });
});

describe('symbol-to-string', function (): any {
  return it('foo', function (): any {
    return assertEqual(symbolToString_(s`foo`), 'foo');
  });
});
