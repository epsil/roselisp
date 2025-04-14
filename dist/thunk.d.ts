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
declare class Thunk {
    /**
     * Thunk function.
     */
    private f;
    /**
     * Whether the thunk has been forced yet.
     */
    private forced;
    /**
     * Cached value.
     */
    private value;
    /**
     * Create a new thunk.
     * `f` should be a function of zero arguments.
     */
    constructor(f: any);
    /**
     * Get the value of the thunk.
     * Alias for `.force()`
     */
    getValue(): any;
    /**
     * Force the thunk.
     */
    force(): any;
}
/**
 * Make a thunk.
 *
 * `f` should be a function of zero arguments.
 */
declare function thunk(f: any): any;
declare namespace thunk {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether something is a thunk.
 */
declare function thunkp(x: any): any;
declare namespace thunkp {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Whether something appears to be a thunk.
 */
declare function thunkishp(x: any): any;
declare namespace thunkishp {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Whether something is a thunk,
 * or appears to be a thunk.
 */
declare function thunkablep(x: any): any;
declare namespace thunkablep {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Force a thunk.
 */
declare function force(x: any): any;
declare namespace force {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Map for storing thunks in.
 *
 * Like [`Map`][js:Map], but stores thunked values transparently.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
declare class ThunkedMap extends Map {
    get(x: any): any;
}
export { thunk as delay, Thunk, ThunkedMap, force, thunk, thunkp, thunkablep, thunkishp };
