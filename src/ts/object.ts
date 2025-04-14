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

/**
 * Make a JavaScript object.
 *
 * Similar to [`js-obj` in ClojureScript][cljs:js-obj].
 *
 * [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
 */
function jsObj_(...args: any[]): any {
  const entries: any = [];
  const _end: any = args.length;
  for (let i: any = 0; i < _end; i = i + 2) {
    entries.push([(args as any)[i], args[i + 1]]);
  }
  return Object.fromEntries(entries);
}

jsObj_.lispSource = [Symbol.for('define'), [Symbol.for('js-obj_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('entries'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('args')], 2]]], [Symbol.for('push-right!'), Symbol.for('entries'), [Symbol.for('list'), [Symbol.for('aget'), Symbol.for('args'), Symbol.for('i')], [Symbol.for('aget'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]]]], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('fromEntries'), Symbol.for('entries')]];

/**
 * Whether something is a JavaScript object.
 */
function jsObjP_(x: any): any {
  // This function avoids regarding JavaScript's `null` value as an
  // object (even if JavaScript does), because it has no properties;
  // and unlike the empty object, attempting to access a property on
  // it causes an error to be thrown. This is more trouble than it is
  // worth, so only non-`null` object values are considered to be
  // proper objects here.
  return (x !== null) && (typeof x === 'object');
}

jsObjP_.lispSource = [Symbol.for('define'), [Symbol.for('js-obj?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('js/null')]], [Symbol.for('js/object-type?'), Symbol.for('x')]]];

/**
 * Whether something types as a JavaScript object.
 *
 * Note that this includes JavaScript's `null` value.
 */
function jsObjectTypeP_(x: any): any {
  return typeof x === 'object';
}

jsObjectTypeP_.lispSource = [Symbol.for('define'), [Symbol.for('js-object-type?_'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'object']];

/**
 * Combine multiple JavaScript objects into a new JavaScript object.
 *
 * Like `append`, but for JavaScript objects.
 */
function jsObjAppend_(...args: any[]): any {
  return Object.assign({}, ...args);
}

jsObjAppend_.lispSource = [Symbol.for('define'), [Symbol.for('js-obj-append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('Object'), Symbol.for('assign'), [Symbol.for('js-obj')], Symbol.for('args')]];

/**
 * Return the keys for a JavaScript object.
 *
 * Similar to [`js-keys` in ClojureScript][cljs:js-keys].
 * [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
 */
function jsKeys_(obj: any): any {
  return Object.keys(obj);
}

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
function objectRef_(obj: any, key: any): any {
  return (obj as any)[key];
}

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
function objectSetX_(obj: any, key: any, val: any): any {
  return (obj as any)[key] = val;
}

objectSetX_.lispSource = [Symbol.for('define'), [Symbol.for('object-set!_'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')], [Symbol.for('oset!'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')]];

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

fieldNames_.lispSource = [Symbol.for('define'), [Symbol.for('field-names_'), Symbol.for('obj')], [Symbol.for('js-keys'), Symbol.for('obj')]];

export {
  jsObjP_,
  jsObjectTypeP_,
  objectRef_ as objectGet_,
  objectRef_ as oget_,
  objectSetX_ as objectSet_,
  objectSetX_ as osetX_,
  objectSetX_ as oset_,
  fieldNames_,
  jsKeys_,
  jsObjAppend_,
  jsObj_,
  objectRef_,
  objectSetX_
};