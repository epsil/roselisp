import * as chai from 'chai';

import { equalp_ } from '../../src/ts/equal';

import { assertEqual } from './test-util';

describe('equal?', function (): any {
  it('(equal? "" "")', function (): any {
    return assertEqual(equalp_('', ''), true);
  });
  it('(equal? "foo" "foo")', function (): any {
    return assertEqual(equalp_('foo', 'foo'), true);
  });
  it('(equal? "foo" "bar")', function (): any {
    return assertEqual(equalp_('foo', 'bar'), false);
  });
  it("(equal? '() '())", function (): any {
    return assertEqual(equalp_([], []), true);
  });
  it("(equal? '(1 2 3) '(1 2 3))", function (): any {
    return assertEqual(equalp_([1, 2, 3], [1, 2, 3]), true);
  });
  it('(equal? (make-hash) (make-hash))', function (): any {
    return assertEqual(equalp_(new Map(), new Map()), true);
  });
  it('(equal? (make-hash \'(("foo" . "bar"))) (make-hash \'(("foo" . "bar"))))', function (): any {
    return assertEqual(
      equalp_(
        new Map([['foo', 'bar']] as any),
        new Map([['foo', 'bar']] as any)
      ),
      true
    );
  });
  it('(equal? (js-obj) (js-obj))', function (): any {
    return assertEqual(equalp_({}, {}), true);
  });
  it('(equal? (js-obj "foo" "bar") (js-obj "foo" "bar"))', function (): any {
    return assertEqual(
      equalp_(
        {
          foo: 'bar',
        },
        {
          foo: 'bar',
        }
      ),
      true
    );
  });
  return it('(equal? (js-obj "foo" (js-obj "bar" "baz")) (js-obj "foo" (js-obj "bar" "baz")))', function (): any {
    return assertEqual(
      equalp_(
        {
          foo: {
            bar: 'baz',
          },
        },
        {
          foo: {
            bar: 'baz',
          },
        }
      ),
      true
    );
  });
});
