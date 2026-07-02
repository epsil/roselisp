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
declare function jsObj_(...args: any[]): any;
declare namespace jsObj_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (number | symbol)[])[])[])[] | (symbol | (number | symbol | symbol[])[])[][])[])[];
}
/**
 * Whether something is a JavaScript object.
 */
declare function jsObjP_(x: any): any;
declare namespace jsObjP_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Whether something types as a JavaScript object.
 *
 * Note that this includes JavaScript's `null` value.
 */
declare function jsObjectTypeP_(x: any): any;
declare namespace jsObjectTypeP_ {
    var lispSource: (symbol | (string | symbol | symbol[])[])[];
}
/**
 * Combine multiple JavaScript objects into a new JavaScript object.
 *
 * Like `append`, but for JavaScript objects.
 */
declare function jsObjAppend_(...args: any[]): any;
declare namespace jsObjAppend_ {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Return the keys for a JavaScript object.
 *
 * Similar to [`js-keys` in ClojureScript][cljs:js-keys].
 * [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
 */
declare function jsKeys_(obj: any): any;
declare namespace jsKeys_ {
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | symbol[])[];
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
    var lispSource: (symbol | symbol[])[];
}
export { jsObjP_, jsObjectTypeP_, objectRef_ as objectGet_, objectRef_ as oget_, objectSetX_ as objectSet_, objectSetX_ as osetX_, objectSetX_ as oset_, fieldNames_, jsKeys_, jsObjAppend_, jsObj_, objectRef_, objectSetX_ };
