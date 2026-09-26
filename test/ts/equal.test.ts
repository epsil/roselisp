import {
  equalp_
} from '../../src/ts/equal';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('equal?', (): any => {
  it('(equal?_ "" "")', (): any => assertEqual(equalp_('', ''), true));
  it('(equal?_ "foo" "foo")', (): any => assertEqual(equalp_('foo', 'foo'), true));
  it('(equal?_ "foo" "bar")', (): any => assertEqual(equalp_('foo', 'bar'), false));
  it('(equal?_ \'() \'())', (): any => assertEqual(equalp_([], []), true));
  it('(equal?_ \'(1 2 3) \'(1 2 3))', (): any => assertEqual(equalp_([1, 2, 3], [1, 2, 3]), true));
  it('(equal?_ (make-hash) (make-hash))', (): any => assertEqual(equalp_(new Map(), new Map()), true));
  it('(equal?_ (make-hash \'(("foo" . "bar"))) (make-hash \'(("foo" . "bar"))))', (): any => assertEqual(equalp_(new Map([['foo', 'bar']] as any), new Map([['foo', 'bar']] as any)), true));
  it('(equal?_ (js/obj) (js/obj))', (): any => assertEqual(equalp_({}, {}), true));
  it('(equal?_ (js/obj "foo" "bar") (js/obj "foo" "bar"))', (): any => assertEqual(equalp_({
    foo: 'bar'
  }, {
    foo: 'bar'
  }), true));
  return it('(equal?_ (js/obj "foo" (js/obj "bar" "baz")) (js/obj "foo" (js/obj "bar" "baz")))', (): any => assertEqual(equalp_({
    foo: {
      bar: 'baz'
    }
  }, {
    foo: {
      bar: 'baz'
    }
  }), true));
});