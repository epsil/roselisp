// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Thunks
 *
 * Thunk implementation.
 *
 * ## Description
 *
 * Defines a `Thunk` class for thunks, which can be forced by calling
 * the `.force()` method. Also provides functions for creating and
 * forcing thunks.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/**
 * Thunk class.
 *
 * This class is a wrapper around a function `f` that is called with
 * zero arguments. The function `f` is passed to the constructor, and
 * is called only once, when the `.force` method is invoked for the
 * first time; subsequent invocations return a cached value.
 */
class Thunk {
  /**
   * Thunk function.
   */
  private f: any;

  /**
   * Whether the thunk has been forced yet.
   */
  private forced: any = false;

  /**
   * Cached value.
   */
  private value: any = undefined;

  /**
   * Create a new thunk.
   * `f` should be a function of zero arguments.
   */
  constructor(f: any) {
    this.f = f;
  }

  /**
   * Get the value of the thunk.
   * Alias for `.force()`
   */
  getValue(): any {
    return this.force();
  }

  /**
   * Force the thunk.
   */
  force(): any {
    if (this.forced) {
      return this.value;
    } else {
      this.forced = true;
      const f: any = this.f;
      const value: any = f();
      this.value = value;
      return value;
    }
  }
}

/**
 * Make a thunk.
 *
 * `f` should be a function of zero arguments.
 */
function thunk(f: any): any {
  return new Thunk(f);
}

thunk.lispSource = [Symbol.for('define'), [Symbol.for('thunk'), Symbol.for('f')], [Symbol.for('new'), Symbol.for('Thunk'), Symbol.for('f')]];

/**
 * Whether something is a thunk.
 */
function thunkp(x: any): any {
  return x instanceof Thunk;
}

thunkp.lispSource = [Symbol.for('define'), [Symbol.for('thunk?'), Symbol.for('x')], [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('Thunk')]];

/**
 * Whether something appears to be a thunk.
 */
function thunkishp(x: any): any {
  return (x !== null) && (typeof x === 'object') && (x.force instanceof Function);
}

thunkishp.lispSource = [Symbol.for('define'), [Symbol.for('thunkish?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('object?'), Symbol.for('x')], [Symbol.for('procedure?'), [Symbol.for('get-field'), Symbol.for('force'), Symbol.for('x')]]]];

/**
 * Whether something is a thunk,
 * or appears to be a thunk.
 */
function thunkablep(x: any): any {
  return thunkp(x) || thunkishp(x);
}

thunkablep.lispSource = [Symbol.for('define'), [Symbol.for('thunkable?'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('thunk?'), Symbol.for('x')], [Symbol.for('thunkish?'), Symbol.for('x')]]];

/**
 * Force a thunk.
 */
function force(x: any): any {
  return x.force();
}

force.lispSource = [Symbol.for('define'), [Symbol.for('force'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('x'), Symbol.for('force')]];

/**
 * Map for storing thunks in.
 *
 * Like [`Map`][js:Map], but stores thunked values transparently.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
class ThunkedMap extends Map {
  get(x: any): any {
    let val: any = super.get(x);
    if (thunkp(val)) {
      val = force(val);
      super.set(x, val);
      return val;
    } else {
      return val;
    }
  }
}

export {
  thunk as delay,
  Thunk,
  ThunkedMap,
  force,
  thunk,
  thunkp,
  thunkablep,
  thunkishp
};