/**
 * # REPL
 *
 * Various tests of the interactive interface.
 */

import {
  historyToString,
  rep,
  stringToHistory
} from '../../src/ts/repl';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('REPL', (): any => {
  it('(rep "#t")', (): any => assertEqual(rep('#t'), '#t'));
  it('(rep "\\"string\\"")', (): any => assertEqual(rep('"string"'), '"string"'));
  it('(rep "(+ 1 1)")', (): any => assertEqual(rep('(+ 1 1)'), '2'));
  return it('(rep "(+ 1 1) (+ 1 1)")', (): any => assertEqual(rep('(+ 1 1) (+ 1 1)'), `2
2`));
});

describe('string->history', (): any => {
  it('(string->history "")', (): any => assertEqual(stringToHistory(''), []));
  it('(string->history "foo")', (): any => assertEqual(stringToHistory('foo'), ['foo']));
  it(`(string->history "foo
bar")`, (): any => assertEqual(stringToHistory(`foo
bar`), ['bar', 'foo']));
  it(`(string->history "foo

bar")`, (): any => assertEqual(stringToHistory(`foo

bar`), ['bar', 'foo']));
  it(`(string->history "foo
bar
baz")`, (): any => assertEqual(stringToHistory(`foo
bar
baz`), ['baz', 'bar', 'foo']));
  it(`(string->history "\\"foo
bar\\"")`, (): any => assertEqual(stringToHistory(`"foo
bar"`), [`"foo
bar"`]));
  it(`(string->history "(foo))
(bar)")`, (): any => assertEqual(stringToHistory(`(foo))
(bar)`), ['(bar)', '(foo))']));
  return it(`(string->history ")(
(foo)")`, (): any => assertEqual(stringToHistory(`)(
(foo)`), ['(foo)', ')(']));
});

describe('history->string', (): any => {
  it('(history->string \'())', (): any => assertEqual(historyToString([]), ''));
  it('(history->string \'("foo"))', (): any => assertEqual(historyToString(['foo']), 'foo'));
  it('(history->string \'("foo" "bar"))', (): any => assertEqual(historyToString(['foo', 'bar']), `bar
foo`));
  it('(history->string \'("foo" "bar" "baz"))', (): any => assertEqual(historyToString(['foo', 'bar', 'baz']), `baz
bar
foo`));
  return xit(`(history->string '("foo
 bar"))`, (): any => assertEqual(historyToString([`foo
 bar`]), `"foo
bar"`));
});