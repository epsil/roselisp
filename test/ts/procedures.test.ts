import {
  compose,
  pipe
} from '../../src/ts/procedures';

import {
  s
} from '../../src/ts/sexp';

import {
  stringp
} from '../../src/ts/string';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('string?', (): any => {
  it('(string? "foo")', (): any => assertEqual(stringp('foo'), true));
  it('(string? (new String "foo"))', (): any => assertEqual(stringp(new String('foo')), true));
  return it('(string? \'foo)', (): any => assertEqual(stringp(Symbol.for('foo')), false));
});

describe('compose', (): any => {
  it('g . f', (): any => assertEqual(((f: any, g: any): any => compose(g, f)(1))((x: any): any => x + 1, (x: any): any => x + 2), 4));
  return it('h . g . f', (): any => assertEqual(((f: any, g: any, h: any): any => compose(h, g, f)(1))((x: any): any => x + 1, (x: any): any => x + 2, (x: any): any => x + 3), 7));
});

describe('pipe', (): any => {
  it('f ; g', (): any => assertEqual(((f: any, g: any): any => pipe(f, g)(1))((x: any): any => x + 1, (x: any): any => x + 2), 4));
  return it('f ; g ; h', (): any => assertEqual(((f: any, g: any, h: any): any => pipe(f, g, h)(1))((x: any): any => x + 1, (x: any): any => x + 2, (x: any): any => x + 3), 7));
});