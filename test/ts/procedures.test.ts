import { compose, pipe } from '../../src/ts/procedures';

import { s } from '../../src/ts/sexp';

import { stringp } from '../../src/ts/string';

import { assertEqual, testMacro } from './test-util';

describe('stringp', function (): any {
  it('(stringp (new String "foo"))', function (): any {
    return assertEqual(stringp(new String('foo')), true);
  });
  return it('(stringp (js/tag s "foo"))', function (): any {
    return assertEqual(stringp(s`foo`), false);
  });
});

describe('compose', function (): any {
  it('(let ((f (lambda (x) (+ x 1))) (g (lambda (x) (+ x 2)))) ((compose g f) 1))', function (): any {
    return assertEqual(
      ((): any => {
        const f: any = function (x: any): any {
          return x + 1;
        };
        const g: any = function (x: any): any {
          return x + 2;
        };
        return compose(g, f)(1);
      })(),
      4
    );
  });
  return it('(let ((f (lambda (x) (+ x 1))) (g (lambda (x) (+ x 2))) (h (lambda (x) (+ x 3)))) ((compose h g f) 1))', function (): any {
    return assertEqual(
      ((): any => {
        const f: any = function (x: any): any {
          return x + 1;
        };
        const g: any = function (x: any): any {
          return x + 2;
        };
        const h: any = function (x: any): any {
          return x + 3;
        };
        return compose(h, g, f)(1);
      })(),
      7
    );
  });
});

describe('pipe', function (): any {
  it('(let ((f (lambda (x) (+ x 1))) (g (lambda (x) (+ x 2)))) ((pipe f g) 1))', function (): any {
    return assertEqual(
      ((): any => {
        const f: any = function (x: any): any {
          return x + 1;
        };
        const g: any = function (x: any): any {
          return x + 2;
        };
        return pipe(f, g)(1);
      })(),
      4
    );
  });
  return it('(let ((f (lambda (x) (+ x 1))) (g (lambda (x) (+ x 2))) (h (lambda (x) (+ x 3)))) ((pipe f g h) 1))', function (): any {
    return assertEqual(
      ((): any => {
        const f: any = function (x: any): any {
          return x + 1;
        };
        const g: any = function (x: any): any {
          return x + 2;
        };
        const h: any = function (x: any): any {
          return x + 3;
        };
        return pipe(f, g, h)(1);
      })(),
      7
    );
  });
});
