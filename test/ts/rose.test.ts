import * as chai from 'chai';

import { sexp } from '../../src/ts/language';

import { Rose, Forest, wrapSexpInRose } from '../../src/ts/rose';

import { assertEqual } from './test-util';

describe('Rose', function (): any {
  return describe('insert', function (): any {
    return it('foo/bar', function (): any {
      const foo: any = new Rose('foo');
      const bar: any = new Rose('bar');
      foo.insert(bar);
      return assertEqual(foo.getForest(), new Forest(bar).setParent(foo));
    });
  });
});

describe('wrap-sexp-in-rose', function (): any {
  it('1', function (): any {
    return assertEqual(wrapSexpInRose(1), new Rose(1));
  });
  it('"1"', function (): any {
    return assertEqual(wrapSexpInRose('1'), new Rose('1'));
  });
  it('foo', function (): any {
    return assertEqual(
      wrapSexpInRose(Symbol.for('foo')),
      new Rose(Symbol.for('foo'))
    );
  });
  it('(foo bar)', function (): any {
    return assertEqual(
      wrapSexpInRose([Symbol.for('foo'), Symbol.for('bar')]),
      new Rose(
        [Symbol.for('foo'), Symbol.for('bar')],
        new Forest(new Rose(Symbol.for('foo')), new Rose(Symbol.for('bar')))
      )
    );
  });
  it('(+ 1 1)', function (): any {
    return assertEqual(
      wrapSexpInRose([Symbol.for('+'), 1, 1]),
      new Rose(
        [Symbol.for('+'), 1, 1],
        new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(1))
      )
    );
  });
  it('(+ 1 2)', function (): any {
    return assertEqual(
      wrapSexpInRose([Symbol.for('+'), 1, 2]),
      new Rose(
        [Symbol.for('+'), 1, 2],
        new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(2))
      )
    );
  });
  it('(+ (+ 1))', function (): any {
    return assertEqual(
      wrapSexpInRose([Symbol.for('+'), [Symbol.for('+'), 1]]),
      new Rose(
        [Symbol.for('+'), [Symbol.for('+'), 1]],
        new Forest(
          new Rose(Symbol.for('+')),
          new Rose(
            [Symbol.for('+'), 1],
            new Forest(new Rose(Symbol.for('+')), new Rose(1))
          )
        )
      )
    );
  });
  it('(+ (+ 1 1))', function (): any {
    return assertEqual(
      wrapSexpInRose([Symbol.for('+'), [Symbol.for('+'), 1, 1]]),
      new Rose(
        [Symbol.for('+'), [Symbol.for('+'), 1, 1]],
        new Forest(
          new Rose(Symbol.for('+')),
          new Rose(
            [Symbol.for('+'), 1, 1],
            new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(1))
          )
        )
      )
    );
  });
  return it('(+ (+ 1 1) 2)', function (): any {
    return assertEqual(
      wrapSexpInRose([Symbol.for('+'), [Symbol.for('+'), 1, 1], 2]),
      new Rose(
        [Symbol.for('+'), [Symbol.for('+'), 1, 1], 2],
        new Forest(
          new Rose(Symbol.for('+')),
          new Rose(
            [Symbol.for('+'), 1, 1],
            new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(1))
          ),
          new Rose(2)
        )
      )
    );
  });
});
