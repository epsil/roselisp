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
declare function thunk_(exp: any, env: any): any;
declare namespace thunk_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
    var ftype: string;
}
/**
 * Whether something is a thunk.
 */
declare function thunkp_(x: any): any;
declare namespace thunkp_ {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Make a promise.
 */
declare function delay_(exp: any, env: any): any;
declare namespace delay_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | symbol[] | undefined)[] | (boolean | symbol | symbol[])[])[])[])[])[] | (symbol | (symbol | undefined)[])[] | (symbol | (boolean | symbol)[])[] | (string | symbol | symbol[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Make a composable promise.
 */
declare function lazy_(exp: any, env: any): any;
declare namespace lazy_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
    var ftype: string;
}
/**
 * Whether something is a promise.
 */
declare function promisep_(x: any): any;
declare namespace promisep_ {
    var fsource: (symbol | (symbol | (string | symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Force a promise.
 */
declare function force_(x: any): any;
declare namespace force_ {
    var fsource: (symbol | symbol[] | symbol[][])[];
}
/**
 * Whether a promise has been forced.
 */
declare function promiseForcedP_(x: any): any;
declare namespace promiseForcedP_ {
    var fsource: (symbol | (boolean | symbol | symbol[])[])[];
}
/**
 * Whether a promise is running.
 */
declare function promiseRunningP_(x: any): any;
declare namespace promiseRunningP_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Map for storing promises in.
 *
 * Like [`Map`][js:Map], but stores promised values transparently.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
declare class PromiseMap extends Map {
    get(x: any): any;
}
/**
 * Promise wrapper, for use within the language implementation
 * in a way that does not interfere with user-defined promises.
 */
declare class InternalPromise {
    private promise;
    constructor(promise: any);
    force(): any;
}
export { delay_ as delay, force_ as force, lazy_ as lazy, promiseForcedP_ as promiseForcedP, promiseRunningP_ as promiseRunningP, promisep_ as promisep, thunkp_ as thunk, InternalPromise, PromiseMap, delay_, force_, lazy_, promiseForcedP_, promiseRunningP_, promisep_, thunkp_, thunk_ };
