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
exports.objectSetX_ = exports.objectRef_ = exports.jsObjectTypeP_ = exports.jsObj_ = exports.jsObjP_ = exports.jsObjAppend_ = exports.jsKeys_ = exports.fieldNames_ = exports.oset_ = exports.osetx_ = exports.objectSet_ = exports.oget_ = exports.objectGet_ = void 0;
const javascript_1 = require("./javascript");
Object.defineProperty(exports, "jsObj_", { enumerable: true, get: function () { return javascript_1.jsObj_; } });
Object.defineProperty(exports, "jsObjP_", { enumerable: true, get: function () { return javascript_1.jsObjP_; } });
Object.defineProperty(exports, "jsObjectTypeP_", { enumerable: true, get: function () { return javascript_1.jsObjectTypeP_; } });
Object.defineProperty(exports, "jsObjAppend_", { enumerable: true, get: function () { return javascript_1.jsObjAppend_; } });
Object.defineProperty(exports, "jsKeys_", { enumerable: true, get: function () { return javascript_1.jsKeys_; } });
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
objectRef_.fsource = [Symbol.for('define'), [Symbol.for('object-ref_'), Symbol.for('obj'), Symbol.for('key')], [Symbol.for('js/get'), Symbol.for('obj'), Symbol.for('key')]];
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
exports.osetx_ = objectSetX_;
exports.oset_ = objectSetX_;
exports.objectSetX_ = objectSetX_;
objectSetX_.fsource = [Symbol.for('define'), [Symbol.for('object-set!_'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')], [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')]];
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
fieldNames_.fsource = [Symbol.for('define'), [Symbol.for('field-names_'), Symbol.for('obj')], [Symbol.for('js/keys'), Symbol.for('obj')]];
