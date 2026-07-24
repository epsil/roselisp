import { Rose, Forest, wrapSexpInRose } from '../../src/ts/rose';

import { assertEqual, testMacro } from './test-util';

describe('Rose', function (): any {
  return it('insert', function (): any {
    const foo: any = new Rose('foo');
    const bar: any = new Rose('bar');
    foo.insert(bar);
    return assertEqual(foo.getForest(), new Forest(bar).setParent(foo));
  });
});

describe('wrap-sexp-in-rose', function (): any {
  it('(wrap-sexp-in-rose 1)', function (): any {
    return assertEqual(wrapSexpInRose(1), new Rose(1));
  });
  it('(wrap-sexp-in-rose "1")', function (): any {
    return assertEqual(wrapSexpInRose('1'), new Rose('1'));
  });
  it("(wrap-sexp-in-rose 'foo)", function (): any {
    return assertEqual(
      wrapSexpInRose(Symbol.for('foo')),
      new Rose(Symbol.for('foo'))
    );
  });
  it("(wrap-sexp-in-rose '(foo bar))", function (): any {
    return assertEqual(
      wrapSexpInRose([Symbol.for('foo'), Symbol.for('bar')]),
      new Rose(
        [Symbol.for('foo'), Symbol.for('bar')],
        new Forest(new Rose(Symbol.for('foo')), new Rose(Symbol.for('bar')))
      )
    );
  });
  it("(wrap-sexp-in-rose '(+ 1 1))", function (): any {
    return assertEqual(
      wrapSexpInRose([Symbol.for('+'), 1, 1]),
      new Rose(
        [Symbol.for('+'), 1, 1],
        new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(1))
      )
    );
  });
  it("(wrap-sexp-in-rose '(+ 1 2))", function (): any {
    return assertEqual(
      wrapSexpInRose([Symbol.for('+'), 1, 2]),
      new Rose(
        [Symbol.for('+'), 1, 2],
        new Forest(new Rose(Symbol.for('+')), new Rose(1), new Rose(2))
      )
    );
  });
  it("(wrap-sexp-in-rose '(+ (+ 1)))", function (): any {
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
  it("(wrap-sexp-in-rose '(+ (+ 1 1)))", function (): any {
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
  return it("(wrap-sexp-in-rose '(+ (+ 1 1) 2))", function (): any {
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
