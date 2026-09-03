// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Thunks and promises
 *
 * Implementation of thunks and promises.
 *
 * ## Description
 *
 * A thunk is a function of zero arguments. A promise is like a thunk,
 * but is only evaluated once. (A promise, in this context, is not to
 * be confused with a JavaScript `Promise`, which is a different
 * construct.)
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/**
 * Make a thunk.
 */
function thunk_(exp: any, env: any): any {
  const body: any = exp.slice(1);
  return [Symbol.for('lambda'), [], ...body];
}

thunk_.fsource = [Symbol.for('define'), [Symbol.for('thunk_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

thunk_.ftype = 'macro';

/**
 * Whether something is a thunk.
 */
function thunkp_(x: any): any {
  return (x instanceof Function) && (x.length === 0);
}

thunkp_.fsource = [Symbol.for('define'), [Symbol.for('thunk?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('procedure?'), Symbol.for('x')], [Symbol.for('zero?'), [Symbol.for('arity'), Symbol.for('x')]]]];

/**
 * Make a promise.
 */
function delay_(exp: any, env: any): any {
  const body: any = exp.slice(1);
  const sym: any = Symbol('promise-f');
  return [Symbol.for('begin'), [Symbol.for('define'), sym, [Symbol.for('thunk'), [Symbol.for('cond'), [[Symbol.for('get-field'), Symbol.for('forced'), sym], [Symbol.for('get-field'), Symbol.for('value'), sym]], [Symbol.for('else'), [Symbol.for('set-field!'), Symbol.for('forced'), sym, undefined], [Symbol.for('set-field!'), Symbol.for('value'), sym, [Symbol.for('begin'), ...body]], [Symbol.for('set-field!'), Symbol.for('forced'), sym, true], [Symbol.for('get-field'), Symbol.for('value'), sym]]]]], [Symbol.for('set-field!'), Symbol.for('value'), sym, [Symbol.for('ann'), undefined, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('forced'), sym, [Symbol.for('ann'), false, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('ftype'), sym, 'thunk'], sym];
}

delay_.fsource = [Symbol.for('define'), [Symbol.for('delay_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('let'), [[Symbol.for('sym'), [Symbol.for('gensym'), 'promise-f']]], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('thunk'), [Symbol.for('cond'), [[Symbol.for('get-field'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('sym')]], [Symbol.for('get-field'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('sym')]]], [Symbol.for('else'), [Symbol.for('set-field!'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('sym')], undefined], [Symbol.for('set-field!'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]], [Symbol.for('set-field!'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('sym')], true], [Symbol.for('get-field'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('sym')]]]]]], [Symbol.for('set-field!'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('ann'), undefined, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('ann'), false, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('ftype'), [Symbol.for('unquote'), Symbol.for('sym')], 'thunk'], [Symbol.for('unquote'), Symbol.for('sym')]]]]];

delay_.ftype = 'macro';

/**
 * Make a composable promise.
 */
function lazy_(exp: any, env: any): any {
  const body: any = exp.slice(1);
  return [Symbol.for('delay'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('begin'), ...body]], [Symbol.for('when'), [Symbol.for('promise?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('force'), Symbol.for('result')]]], Symbol.for('result')];
}

lazy_.fsource = [Symbol.for('define'), [Symbol.for('lazy_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('delay'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]], [Symbol.for('when'), [Symbol.for('promise?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('force'), Symbol.for('result')]]], Symbol.for('result')]]];

lazy_.ftype = 'macro';

/**
 * Whether something is a promise.
 */
function promisep_(x: any): any {
  return (typeof x === 'function') && ((x as any).ftype === 'thunk');
}

promisep_.fsource = [Symbol.for('define'), [Symbol.for('promise?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('js/function-type?'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('ftype'), [Symbol.for('ann'), Symbol.for('x'), Symbol.for('Any')]], 'thunk']]];

/**
 * Force a promise.
 */
function force_(x: any): any {
  return (x as any)();
}

force_.fsource = [Symbol.for('define'), [Symbol.for('force_'), Symbol.for('x')], [[Symbol.for('ann'), Symbol.for('x'), Symbol.for('Any')]]];

/**
 * Whether a promise has been forced.
 */
function promiseForcedP_(x: any): any {
  if (x.forced) {
    return true;
  } else {
    return false;
  }
}

promiseForcedP_.fsource = [Symbol.for('define'), [Symbol.for('promise-forced?_'), Symbol.for('x')], [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('forced'), Symbol.for('x')], true, false]];

/**
 * Whether a promise is running.
 */
function promiseRunningP_(x: any): any {
  return x.forced === undefined;
}

promiseRunningP_.fsource = [Symbol.for('define'), [Symbol.for('promise-running?_'), Symbol.for('x')], [Symbol.for('undefined?'), [Symbol.for('get-field'), Symbol.for('forced'), Symbol.for('x')]]];

/**
 * Map for storing promises in.
 *
 * Like [`Map`][js:Map], but stores promised values transparently.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
class PromiseMap extends Map {
  get(x: any): any {
    let val: any = super.get(x);
    if ((typeof val === 'function') && ((val as any).ftype === 'thunk')) {
      val = (val as any)();
      super.set(x, val);
      return val;
    } else {
      return val;
    }
  }
}

/**
 * Promise wrapper, for use within the language implementation
 * in a way that does not interfere with user-defined promises.
 */
class InternalPromise {
  private promise: any;

  constructor(promise: any) {
    this.promise = promise;
  }

  force(): any {
    return (this.promise as any)();
  }
}

export {
  delay_ as delay,
  force_ as force,
  lazy_ as lazy,
  promiseForcedP_ as promiseForcedP,
  promiseRunningP_ as promiseRunningP,
  promisep_ as promisep,
  thunkp_ as thunk,
  InternalPromise,
  PromiseMap,
  delay_,
  force_,
  lazy_,
  promiseForcedP_,
  promiseRunningP_,
  promisep_,
  thunkp_,
  thunk_
};