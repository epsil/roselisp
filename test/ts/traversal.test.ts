import * as chai from 'chai';

import { lispEnvironment, mapRose } from '../../src/ts/language';

import { assertEqual } from './test-util';

describe('map-rose', function (): any {
  describe('empty list', function (): any {
    return it('()', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [],
        lispEnvironment
      );
      return assertEqual(expressions, [[]]);
    });
  });
  describe('function application', function (): any {
    it('(f x)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [Symbol.for('f'), Symbol.for('x')],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('f'),
        Symbol.for('x'),
        [Symbol.for('f'), Symbol.for('x')],
      ]);
    });
    return it('(f (g x) y)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')], Symbol.for('y')],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('f'),
        Symbol.for('g'),
        Symbol.for('x'),
        [Symbol.for('g'), Symbol.for('x')],
        Symbol.for('y'),
        [Symbol.for('f'), [Symbol.for('g'), Symbol.for('x')], Symbol.for('y')],
      ]);
    });
  });
  describe('begin', function (): any {
    return it('(begin x y)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [Symbol.for('begin'), Symbol.for('x'), Symbol.for('y')],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('x'),
        Symbol.for('y'),
        [Symbol.for('begin'), Symbol.for('x'), Symbol.for('y')],
      ]);
    });
  });
  describe('begin0', function (): any {
    return it('(begin0 x y)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [Symbol.for('begin0'), Symbol.for('x'), Symbol.for('y')],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('x'),
        Symbol.for('y'),
        [Symbol.for('begin0'), Symbol.for('x'), Symbol.for('y')],
      ]);
    });
  });
  describe('let', function (): any {
    return it('(let ((x 1)) x)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('x'),
        1,
        Symbol.for('x'),
        [Symbol.for('let'), [[Symbol.for('x'), 1]], Symbol.for('x')],
      ]);
    });
  });
  describe('let-values', function (): any {
    return it('(let-values (((x) (foo))) x)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [
          Symbol.for('let-values'),
          [[[Symbol.for('x')], [Symbol.for('foo')]]],
          Symbol.for('x'),
        ],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('x'),
        Symbol.for('foo'),
        [Symbol.for('foo')],
        Symbol.for('x'),
        [
          Symbol.for('let-values'),
          [[[Symbol.for('x')], [Symbol.for('foo')]]],
          Symbol.for('x'),
        ],
      ]);
    });
  });
  describe('cond', function (): any {
    return it('(cond ((foo bar) (baz quux)))', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [
          Symbol.for('cond'),
          [
            [Symbol.for('foo'), Symbol.for('bar')],
            [Symbol.for('baz'), Symbol.for('quux')],
          ],
        ],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('foo'),
        Symbol.for('bar'),
        [Symbol.for('foo'), Symbol.for('bar')],
        Symbol.for('baz'),
        Symbol.for('quux'),
        [Symbol.for('baz'), Symbol.for('quux')],
        [
          Symbol.for('cond'),
          [
            [Symbol.for('foo'), Symbol.for('bar')],
            [Symbol.for('baz'), Symbol.for('quux')],
          ],
        ],
      ]);
    });
  });
  describe('cond', function (): any {
    return it('(cond ((foo bar) (baz quux)))', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [
          Symbol.for('cond'),
          [
            [Symbol.for('foo'), Symbol.for('bar')],
            [Symbol.for('baz'), Symbol.for('quux')],
          ],
        ],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('foo'),
        Symbol.for('bar'),
        [Symbol.for('foo'), Symbol.for('bar')],
        Symbol.for('baz'),
        Symbol.for('quux'),
        [Symbol.for('baz'), Symbol.for('quux')],
        [
          Symbol.for('cond'),
          [
            [Symbol.for('foo'), Symbol.for('bar')],
            [Symbol.for('baz'), Symbol.for('quux')],
          ],
        ],
      ]);
    });
  });
  describe('lambda', function (): any {
    it('(lambda (x) x)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('x'),
        Symbol.for('x'),
        [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
      ]);
    });
    return it('(lambda (x (y 1)) x)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [
          Symbol.for('lambda'),
          [Symbol.for('x'), [Symbol.for('y'), 1]],
          Symbol.for('x'),
        ],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('x'),
        Symbol.for('y'),
        1,
        Symbol.for('x'),
        [
          Symbol.for('lambda'),
          [Symbol.for('x'), [Symbol.for('y'), 1]],
          Symbol.for('x'),
        ],
      ]);
    });
  });
  describe('define', function (): any {
    it('(define (I x) x)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          Symbol.for('x'),
        ],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('I'),
        Symbol.for('x'),
        Symbol.for('x'),
        [
          Symbol.for('define'),
          [Symbol.for('I'), Symbol.for('x')],
          Symbol.for('x'),
        ],
      ]);
    });
    return it('(define I (lambda (x) x))', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        ],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('I'),
        Symbol.for('x'),
        Symbol.for('x'),
        [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        [
          Symbol.for('define'),
          Symbol.for('I'),
          [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')],
        ],
      ]);
    });
  });
  describe('quasiquote', function (): any {
    it('(quasiquote x)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [Symbol.for('quasiquote'), Symbol.for('x')],
        lispEnvironment
      );
      return assertEqual(expressions, [
        [Symbol.for('quasiquote'), Symbol.for('x')],
      ]);
    });
    it('(quasiquote (x))', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [Symbol.for('quasiquote'), [Symbol.for('x')]],
        lispEnvironment
      );
      return assertEqual(expressions, [
        [Symbol.for('quasiquote'), [Symbol.for('x')]],
      ]);
    });
    it('(quasiquote (unquote x))', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [Symbol.for('quasiquote'), [Symbol.for('unquote'), Symbol.for('x')]],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('x'),
        [Symbol.for('quasiquote'), [Symbol.for('unquote'), Symbol.for('x')]],
      ]);
    });
    return it('(quasiquote (x (unquote y) (unquote-splicing z)))', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [
          Symbol.for('quasiquote'),
          [
            Symbol.for('x'),
            [Symbol.for('unquote'), Symbol.for('y')],
            [Symbol.for('unquote-splicing'), Symbol.for('z')],
          ],
        ],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('y'),
        Symbol.for('z'),
        [
          Symbol.for('quasiquote'),
          [
            Symbol.for('x'),
            [Symbol.for('unquote'), Symbol.for('y')],
            [Symbol.for('unquote-splicing'), Symbol.for('z')],
          ],
        ],
      ]);
    });
  });
  return describe('defmacro', function (): any {
    return it('(defmacro f (x) x)', function (): any {
      const expressions: any = [];
      mapRose(
        function (x: any): any {
          expressions.push(x);
          return x;
        },
        [
          Symbol.for('defmacro'),
          Symbol.for('f'),
          [Symbol.for('x')],
          Symbol.for('x'),
        ],
        lispEnvironment
      );
      return assertEqual(expressions, [
        Symbol.for('f'),
        Symbol.for('x'),
        Symbol.for('x'),
        [
          Symbol.for('defmacro'),
          Symbol.for('f'),
          [Symbol.for('x')],
          Symbol.for('x'),
        ],
      ]);
    });
  });
});
