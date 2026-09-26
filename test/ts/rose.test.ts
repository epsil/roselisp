import {
  Rose,
  Forest,
  wrapSexpInRose
} from '../../src/ts/rose';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('Rose', (): any => it('insert', (): any => {
  const foo: any = new Rose('foo');
  const bar: any = new Rose('bar');
  foo.insert(bar);
  return assertEqual(foo.getForest(), new Forest(bar).setParent(foo));
}));

describe('wrap-sexp-in-rose', (): any => {
  it('(wrap-sexp-in-rose 1)', (): any => assertEqual(wrapSexpInRose(1), new Rose(1)));
  it('(wrap-sexp-in-rose "1")', (): any => assertEqual(wrapSexpInRose('1'), new Rose('1')));
  it('(wrap-sexp-in-rose \'foo)', (): any => assertEqual(wrapSexpInRose(Symbol.for('foo')), new Rose(Symbol.for('foo'))));
  it('(wrap-sexp-in-rose \'(foo bar))', (): any => assertEqual(wrapSexpInRose([Symbol.for('foo'), Symbol.for('bar')]), new Rose([Symbol.for('foo'), Symbol.for('bar')], new Forest(new Rose(Symbol.for('foo')), new Rose(Symbol.for('bar'))))));
  it('(wrap-sexp-in-rose \'(+ 1 1))', (): any => assertEqual(wrapSexpInRose([Symbol.for('+'), 1, 1]), new Rose([Symbol.for('+'), 1, 1], new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(1)))));
  it('(wrap-sexp-in-rose \'(+ 1 2))', (): any => assertEqual(wrapSexpInRose([Symbol.for('+'), 1, 2]), new Rose([Symbol.for('+'), 1, 2], new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(2)))));
  it('(wrap-sexp-in-rose \'(+ (+ 1)))', (): any => assertEqual(wrapSexpInRose([Symbol.for('+'), [Symbol.for('+'), 1]]), new Rose([Symbol.for('+'), [Symbol.for('+'), 1]], new Forest(new Rose(Symbol.for('+')), new Rose([Symbol.for('+'), 1], new Forest(new Rose(Symbol.for('+')), new Rose(1)))))));
  it('(wrap-sexp-in-rose \'(+ (+ 1 1)))', (): any => assertEqual(wrapSexpInRose([Symbol.for('+'), [Symbol.for('+'), 1, 1]]), new Rose([Symbol.for('+'), [Symbol.for('+'), 1, 1]], new Forest(new Rose(Symbol.for('+')), new Rose([Symbol.for('+'), 1, 1], new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(1)))))));
  return it('(wrap-sexp-in-rose \'(+ (+ 1 1) 2))', (): any => assertEqual(wrapSexpInRose([Symbol.for('+'), [Symbol.for('+'), 1, 1], 2]), new Rose([Symbol.for('+'), [Symbol.for('+'), 1, 1], 2], new Forest(new Rose(Symbol.for('+')), new Rose([Symbol.for('+'), 1, 1], new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(1))), new Rose(2)))));
});