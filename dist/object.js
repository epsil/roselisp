"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.objectSetX_ = exports.objectRef_ = exports.jsObj_ = exports.jsObjAppend_ = exports.jsKeys_ = exports.fieldNames_ = exports.oset_ = exports.osetX_ = exports.objectSet_ = exports.oget_ = exports.objectGet_ = exports.jsObjectTypeP_ = exports.jsObjP_ = void 0;
/**
 * Make a JavaScript object.
 *
 * Similar to [`js-obj` in ClojureScript][cljs:js-obj].
 *
 * [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
 */
function jsObj_(...args) {
    const entries = [];
    const _end = args.length;
    for (let i = 0; i < _end; i = i + 2) {
        entries.push([args[i], args[i + 1]]);
    }
    return Object.fromEntries(entries);
}
exports.jsObj_ = jsObj_;
jsObj_.lispSource = [Symbol.for('define'), [Symbol.for('js-obj_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('entries'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args')], 2]]], [Symbol.for('push-right!'), Symbol.for('entries'), [Symbol.for('list'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')], [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]]]], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('fromEntries'), Symbol.for('entries')]];
/**
 * Whether something is a JavaScript object.
 */
function jsObjP_(x) {
    // This function avoids regarding JavaScript's `null` value as an
    // object (even if JavaScript does), because it has no properties;
    // and unlike the empty object, attempting to access a property on
    // it causes an error to be thrown. This is more trouble than it is
    // worth, so only non-`null` object values are considered to be
    // proper objects here.
    return (x !== null) && (typeof x === 'object');
}
exports.jsObjP_ = jsObjP_;
jsObjP_.lispSource = [Symbol.for('define'), [Symbol.for('js-obj?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('js/null')]], [Symbol.for('js/object-type?'), Symbol.for('x')]]];
/**
 * Whether something types as a JavaScript object.
 *
 * Note that this includes JavaScript's `null` value.
 */
function jsObjectTypeP_(x) {
    return typeof x === 'object';
}
exports.jsObjectTypeP_ = jsObjectTypeP_;
jsObjectTypeP_.lispSource = [Symbol.for('define'), [Symbol.for('js-object-type?_'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'object']];
/**
 * Combine multiple JavaScript objects into a new JavaScript object.
 *
 * Like `append`, but for JavaScript objects.
 */
function jsObjAppend_(...args) {
    return Object.assign({}, ...args);
}
exports.jsObjAppend_ = jsObjAppend_;
jsObjAppend_.lispSource = [Symbol.for('define'), [Symbol.for('js-obj-append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('Object'), Symbol.for('assign'), [Symbol.for('js-obj')], Symbol.for('args')]];
/**
 * Return the keys for a JavaScript object.
 *
 * Similar to [`js-keys` in ClojureScript][cljs:js-keys].
 * [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
 */
function jsKeys_(obj) {
    return Object.keys(obj);
}
exports.jsKeys_ = jsKeys_;
jsKeys_.lispSource = [Symbol.for('define'), [Symbol.for('js-keys_'), Symbol.for('obj')], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('keys'), Symbol.for('obj')]];
/**
 * Look up the property `key` in `obj`.
 *
 * Similar to [`object-get` in Racket][rkt:object-get] and
 * [`oget` in ClojureScript][cljs:oget].
 *
 * [rkt:object-get]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-get%29%29
 * [cljs:oget]: https://github.com/binaryage/cljs-oops#object-operations
 */
function objectRef_(obj, key) {
    return obj[key];
}
exports.objectGet_ = objectRef_;
exports.oget_ = objectRef_;
exports.objectRef_ = objectRef_;
objectRef_.lispSource = [Symbol.for('define'), [Symbol.for('object-ref_'), Symbol.for('obj'), Symbol.for('key')], [Symbol.for('oget'), Symbol.for('obj'), Symbol.for('key')]];
/**
 * Set the property `key` in `obj` to `val`.
 *
 * Similar to [`object-set!` in Racket][rkt:object-set] and
 * [`oset!` in ClojureScript][cljs:oset].
 *
 * [rkt:object-set]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-set%21%29%29
 * [cljs:oset]: https://github.com/binaryage/cljs-oops#object-operations
 */
function objectSetX_(obj, key, val) {
    return obj[key] = val;
}
exports.objectSet_ = objectSetX_;
exports.osetX_ = objectSetX_;
exports.oset_ = objectSetX_;
exports.objectSetX_ = objectSetX_;
objectSetX_.lispSource = [Symbol.for('define'), [Symbol.for('object-set!_'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')], [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')]];
/**
 * Return the keys for an object.
 *
 * Similar to [`field-names` in Racket][rkt:field-names].
 *
 * [rkt:field-names]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._field-names%29%29
 */
function fieldNames_(obj) {
    return Object.keys(obj);
}
exports.fieldNames_ = fieldNames_;
fieldNames_.lispSource = [Symbol.for('define'), [Symbol.for('field-names_'), Symbol.for('obj')], [Symbol.for('js-keys'), Symbol.for('obj')]];
