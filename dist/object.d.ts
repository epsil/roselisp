/**
 * # Objects
 *
 * JavaScript objects.
 *
 * ## Description
 *
 * Functions for working with JavaScript objects.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Make a JavaScript object.
 *
 * Similar to [`js-obj` in ClojureScript][cljs:js-obj].
 *
 * [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
 */
declare function object_(...args: any[]): any;
declare namespace object_ {
    var fsource: (symbol | (symbol | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | (number | symbol)[])[])[][])[])[])[])[];
}
/**
 * Whether something is a JavaScript object.
 */
declare function objectp_(x: any): any;
declare namespace objectp_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Whether something types as a JavaScript object.
 *
 * Note that this includes JavaScript's `null` value.
 */
declare function objectTypeP_(x: any): any;
declare namespace objectTypeP_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[] | (string | symbol | symbol[])[])[];
}
/**
 * Combine multiple JavaScript objects into a new JavaScript object.
 *
 * Like `append`, but for JavaScript objects.
 */
declare function objectAppend_(...args: any[]): any;
declare namespace objectAppend_ {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Copy a JavaScript object.
 */
declare function objectCopy_(obj: any): any;
declare namespace objectCopy_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Spread a JavaScript object into another.
 */
declare function objectSpread_(x: any): any;
declare namespace objectSpread_ {
    var fsource: (symbol | symbol[])[];
}
/**
 * Return the keys for a JavaScript object.
 *
 * Similar to [`js-keys` in ClojureScript][cljs:js-keys].
 * [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
 */
declare function objectKeys_(obj: any): any;
declare namespace objectKeys_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Look up the property `key` in `obj`.
 *
 * Similar to [`object-get` in Racket][rkt:object-get] and
 * [`oget` in ClojureScript][cljs:oget].
 *
 * [rkt:object-get]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-get%29%29
 * [cljs:oget]: https://github.com/binaryage/cljs-oops#object-operations
 */
declare function objectRef_(obj: any, key: any): any;
declare namespace objectRef_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Set the property `key` in `obj` to `val`.
 *
 * Similar to [`object-set!` in Racket][rkt:object-set] and
 * [`oset!` in ClojureScript][cljs:oset].
 *
 * [rkt:object-set]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-set%21%29%29
 * [cljs:oset]: https://github.com/binaryage/cljs-oops#object-operations
 */
declare function objectSetX_(obj: any, key: any, val: any): any;
declare namespace objectSetX_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
/**
 * Return the keys for an object.
 *
 * Similar to [`field-names` in Racket][rkt:field-names].
 *
 * [rkt:field-names]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._field-names%29%29
 */
declare function fieldNames_(obj: any): any;
declare namespace fieldNames_ {
    var compilerMacro: any;
    var fsource: (symbol | (symbol | (boolean | symbol)[])[])[];
}
export { objectRef_ as objectGet_, objectRef_ as oget_, objectSetX_ as objectSet_, objectSetX_ as osetx_, objectSetX_ as oset_, fieldNames_, objectAppend_, objectCopy_, objectKeys_, objectRef_, objectSetX_, objectSpread_, objectTypeP_, objectp_, object_ };
