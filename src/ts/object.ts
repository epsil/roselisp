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

import {
  jsObj_,
  jsObjP_,
  jsObjectTypeP_,
  jsObjAppend_,
  jsKeys_
} from './javascript';

/**
 * Look up the property `key` in `obj`.
 *
 * Similar to [`object-get` in Racket][rkt:object-get] and
 * [`oget` in ClojureScript][cljs:oget].
 *
 * [rkt:object-get]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-get%29%29
 * [cljs:oget]: https://github.com/binaryage/cljs-oops#object-operations
 */
function objectRef_(obj: any, key: any): any {
  return (obj as any)[key];
}

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
function objectSetX_(obj: any, key: any, val: any): any {
  return (obj as any)[key] = val;
}

objectSetX_.fsource = [Symbol.for('define'), [Symbol.for('object-set!_'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')], [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')]];

/**
 * Return the keys for an object.
 *
 * Similar to [`field-names` in Racket][rkt:field-names].
 *
 * [rkt:field-names]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._field-names%29%29
 */
function fieldNames_(obj: any): any {
  return Object.keys(obj);
}

fieldNames_.fsource = [Symbol.for('define'), [Symbol.for('field-names_'), Symbol.for('obj')], [Symbol.for('js/keys'), Symbol.for('obj')]];

export {
  objectRef_ as objectGet_,
  objectRef_ as oget_,
  objectSetX_ as objectSet_,
  objectSetX_ as osetx_,
  objectSetX_ as oset_,
  fieldNames_,
  jsKeys_,
  jsObjAppend_,
  jsObjP_,
  jsObj_,
  jsObjectTypeP_,
  objectRef_,
  objectSetX_
};