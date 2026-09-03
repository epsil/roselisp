"use strict";
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.thunk_ = exports.thunkp_ = exports.promisep_ = exports.promiseRunningP_ = exports.promiseForcedP_ = exports.lazy_ = exports.force_ = exports.delay_ = exports.PromiseMap = exports.InternalPromise = exports.thunk = exports.promisep = exports.promiseRunningP = exports.promiseForcedP = exports.lazy = exports.force = exports.delay = void 0;
/**
 * Make a thunk.
 */
function thunk_(exp, env) {
    const body = exp.slice(1);
    return [Symbol.for('lambda'), [], ...body];
}
exports.thunk_ = thunk_;
thunk_.fsource = [Symbol.for('define'), [Symbol.for('thunk_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('lambda'), [], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];
thunk_.ftype = 'macro';
/**
 * Whether something is a thunk.
 */
function thunkp_(x) {
    return (x instanceof Function) && (x.length === 0);
}
exports.thunk = thunkp_;
exports.thunkp_ = thunkp_;
thunkp_.fsource = [Symbol.for('define'), [Symbol.for('thunk?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('procedure?'), Symbol.for('x')], [Symbol.for('zero?'), [Symbol.for('arity'), Symbol.for('x')]]]];
/**
 * Make a promise.
 */
function delay_(exp, env) {
    const body = exp.slice(1);
    const sym = Symbol('promise-f');
    return [Symbol.for('begin'), [Symbol.for('define'), sym, [Symbol.for('thunk'), [Symbol.for('cond'), [[Symbol.for('get-field'), Symbol.for('forced'), sym], [Symbol.for('get-field'), Symbol.for('value'), sym]], [Symbol.for('else'), [Symbol.for('set-field!'), Symbol.for('forced'), sym, undefined], [Symbol.for('set-field!'), Symbol.for('value'), sym, [Symbol.for('begin'), ...body]], [Symbol.for('set-field!'), Symbol.for('forced'), sym, true], [Symbol.for('get-field'), Symbol.for('value'), sym]]]]], [Symbol.for('set-field!'), Symbol.for('value'), sym, [Symbol.for('ann'), undefined, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('forced'), sym, [Symbol.for('ann'), false, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('ftype'), sym, 'thunk'], sym];
}
exports.delay = delay_;
exports.delay_ = delay_;
delay_.fsource = [Symbol.for('define'), [Symbol.for('delay_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('let'), [[Symbol.for('sym'), [Symbol.for('gensym'), 'promise-f']]], [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('thunk'), [Symbol.for('cond'), [[Symbol.for('get-field'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('sym')]], [Symbol.for('get-field'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('sym')]]], [Symbol.for('else'), [Symbol.for('set-field!'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('sym')], undefined], [Symbol.for('set-field!'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]], [Symbol.for('set-field!'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('sym')], true], [Symbol.for('get-field'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('sym')]]]]]], [Symbol.for('set-field!'), Symbol.for('value'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('ann'), undefined, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('forced'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('ann'), false, Symbol.for('Any')]], [Symbol.for('set-field!'), Symbol.for('ftype'), [Symbol.for('unquote'), Symbol.for('sym')], 'thunk'], [Symbol.for('unquote'), Symbol.for('sym')]]]]];
delay_.ftype = 'macro';
/**
 * Make a composable promise.
 */
function lazy_(exp, env) {
    const body = exp.slice(1);
    return [Symbol.for('delay'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('begin'), ...body]], [Symbol.for('when'), [Symbol.for('promise?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('force'), Symbol.for('result')]]], Symbol.for('result')];
}
exports.lazy = lazy_;
exports.lazy_ = lazy_;
lazy_.fsource = [Symbol.for('define'), [Symbol.for('lazy_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('delay'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]], [Symbol.for('when'), [Symbol.for('promise?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('force'), Symbol.for('result')]]], Symbol.for('result')]]];
lazy_.ftype = 'macro';
/**
 * Whether something is a promise.
 */
function promisep_(x) {
    return (typeof x === 'function') && (x.ftype === 'thunk');
}
exports.promisep = promisep_;
exports.promisep_ = promisep_;
promisep_.fsource = [Symbol.for('define'), [Symbol.for('promise?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('js/function-type?'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('ftype'), [Symbol.for('ann'), Symbol.for('x'), Symbol.for('Any')]], 'thunk']]];
/**
 * Force a promise.
 */
function force_(x) {
    return x();
}
exports.force = force_;
exports.force_ = force_;
force_.fsource = [Symbol.for('define'), [Symbol.for('force_'), Symbol.for('x')], [[Symbol.for('ann'), Symbol.for('x'), Symbol.for('Any')]]];
/**
 * Whether a promise has been forced.
 */
function promiseForcedP_(x) {
    if (x.forced) {
        return true;
    }
    else {
        return false;
    }
}
exports.promiseForcedP = promiseForcedP_;
exports.promiseForcedP_ = promiseForcedP_;
promiseForcedP_.fsource = [Symbol.for('define'), [Symbol.for('promise-forced?_'), Symbol.for('x')], [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('forced'), Symbol.for('x')], true, false]];
/**
 * Whether a promise is running.
 */
function promiseRunningP_(x) {
    return x.forced === undefined;
}
exports.promiseRunningP = promiseRunningP_;
exports.promiseRunningP_ = promiseRunningP_;
promiseRunningP_.fsource = [Symbol.for('define'), [Symbol.for('promise-running?_'), Symbol.for('x')], [Symbol.for('undefined?'), [Symbol.for('get-field'), Symbol.for('forced'), Symbol.for('x')]]];
/**
 * Map for storing promises in.
 *
 * Like [`Map`][js:Map], but stores promised values transparently.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
class PromiseMap extends Map {
    get(x) {
        let val = super.get(x);
        if ((typeof val === 'function') && (val.ftype === 'thunk')) {
            val = val();
            super.set(x, val);
            return val;
        }
        else {
            return val;
        }
    }
}
exports.PromiseMap = PromiseMap;
/**
 * Promise wrapper, for use within the language implementation
 * in a way that does not interfere with user-defined promises.
 */
class InternalPromise {
    constructor(promise) {
        this.promise = promise;
    }
    force() {
        return this.promise();
    }
}
exports.InternalPromise = InternalPromise;
