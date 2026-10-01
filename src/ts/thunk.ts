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

thunk_.ftype = 'macro';

thunk_.fsource = [Symbol.for('define'), [Symbol.for('thunk_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

/**
 * Whether something is a thunk.
 */
function thunkp_(x: any): any {
  return (x instanceof Function) && (x.length === 0);
}

thunkp_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    if (!(Array.isArray(x) && (x.length > 0))) {
      return [Symbol.for('and'), [Symbol.for('procedure?'), x], [Symbol.for('zero?'), [Symbol.for('arity'), x]]];
    } else {
      const x1: any = Symbol('x');
      return [Symbol.for('let'), [[x1, x]], ((x: any): any => [Symbol.for('and'), [Symbol.for('procedure?'), x], [Symbol.for('zero?'), [Symbol.for('arity'), x]]])(x1)];
    }
  };
  f.ftype = 'macro';
  return f;
})();

thunkp_.fsource = [Symbol.for('define'), [Symbol.for('thunk?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('and'), [Symbol.for('procedure?'), Symbol.for('x')], [Symbol.for('zero?'), [Symbol.for('arity'), Symbol.for('x')]]]];

/**
 * Make a promise.
 */
function delay_(exp: any, env: any): any {
  const body: any = exp.slice(1);
  const promiseF: any = Symbol('promise-f');
  return [Symbol.for('let*'), [[promiseF, [Symbol.for('thunk'), [Symbol.for('cond'), [[Symbol.for('get-field'), Symbol.for('forced'), promiseF], [Symbol.for('get-field'), Symbol.for('value'), promiseF]], [Symbol.for('else'), [Symbol.for('set-field!'), Symbol.for('forced'), promiseF, undefined], [Symbol.for('set-field!'), Symbol.for('value'), promiseF, [Symbol.for('begin'), ...body]], [Symbol.for('set-field!'), Symbol.for('forced'), promiseF, true], [Symbol.for('get-field'), Symbol.for('value'), promiseF]]]]]], [Symbol.for('set-field!'), Symbol.for('value'), promiseF, [Symbol.for('ann'), undefined, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('forced'), promiseF, [Symbol.for('ann'), false, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('ftype'), promiseF, 'thunk'], promiseF];
}

delay_.ftype = 'macro';

delay_.fsource = [Symbol.for('define'), [Symbol.for('delay_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('with-gensyms'), [Symbol.for('promise-f')], [Symbol.for('quasiquote'), [Symbol.for('let*'), [[[Symbol.for('unquote'), Symbol.for('promise-f')], [Symbol.for('thunk'), [Symbol.for('cond'), [[Symbol.for('get-field'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('promise-f')]], [Symbol.for('get-field'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('promise-f')]]], [Symbol.for('else'), [Symbol.for('set-field!'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('promise-f')], undefined], [Symbol.for('set-field!'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('promise-f')], [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]], [Symbol.for('set-field!'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('promise-f')], true], [Symbol.for('get-field'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('promise-f')]]]]]]], [Symbol.for('set-field!'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('promise-f')], [Symbol.for('ann'), undefined, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('promise-f')], [Symbol.for('ann'), false, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('ftype'), [Symbol.for('unquote'), Symbol.for('promise-f')], 'thunk'], [Symbol.for('unquote'), Symbol.for('promise-f')]]]]];

/**
 * Make a composable promise.
 */
function lazy_(exp: any, env: any): any {
  const body: any = exp.slice(1);
  return [Symbol.for('delay'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('begin'), ...body]], [Symbol.for('when'), [Symbol.for('promise?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('force'), Symbol.for('result')]]], Symbol.for('result')];
}

lazy_.ftype = 'macro';

lazy_.fsource = [Symbol.for('define'), [Symbol.for('lazy_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('delay'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]], [Symbol.for('when'), [Symbol.for('promise?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('force'), Symbol.for('result')]]], Symbol.for('result')]]];

/**
 * Whether something is a promise.
 */
function promisep_(x: any): any {
  return (typeof x === 'function') && ((x as any).ftype === 'thunk');
}

promisep_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    if (!(Array.isArray(x) && (x.length > 0))) {
      return [Symbol.for('and'), [Symbol.for('js/function-type?'), x], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('ftype'), [Symbol.for('ann'), x, Symbol.for('Any')]], 'thunk']];
    } else {
      const x1: any = Symbol('x');
      return [Symbol.for('let'), [[x1, x]], ((x: any): any => [Symbol.for('and'), [Symbol.for('js/function-type?'), x], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('ftype'), [Symbol.for('ann'), x, Symbol.for('Any')]], 'thunk']])(x1)];
    }
  };
  f.ftype = 'macro';
  return f;
})();

promisep_.fsource = [Symbol.for('define'), [Symbol.for('promise?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('and'), [Symbol.for('js/function-type?'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('ftype'), [Symbol.for('ann'), Symbol.for('x'), Symbol.for('Any')]], 'thunk']]];

/**
 * Force a promise.
 */
function force_(x: any): any {
  return (x as any)();
}

force_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [[Symbol.for('ann'), x, Symbol.for('Any')]];
  };
  f.ftype = 'macro';
  return f;
})();

force_.fsource = [Symbol.for('define'), [Symbol.for('force_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [[Symbol.for('ann'), Symbol.for('x'), Symbol.for('Any')]]];

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

promiseForcedP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('forced'), x], true, false];
  };
  f.ftype = 'macro';
  return f;
})();

promiseForcedP_.fsource = [Symbol.for('define'), [Symbol.for('promise-forced?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('forced'), Symbol.for('x')], true, false]];

/**
 * Whether a promise is running.
 */
function promiseRunningP_(x: any): any {
  return x.forced === undefined;
}

promiseRunningP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('undefined?'), [Symbol.for('get-field'), Symbol.for('forced'), x]];
  };
  f.ftype = 'macro';
  return f;
})();

promiseRunningP_.fsource = [Symbol.for('define'), [Symbol.for('promise-running?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('undefined?'), [Symbol.for('get-field'), Symbol.for('forced'), Symbol.for('x')]]];

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
    if (val instanceof InternalPromise) {
      val = val.force();
      super.set(x, val);
      return val;
    } else if ((typeof val === 'function') && ((val as any).ftype === 'thunk')) {
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