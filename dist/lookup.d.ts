/**
 * # JavaScript lookup
 *
 * Look up JavaScript values.
 *
 * ## Description
 *
 * This file defines a function that looks up JavaScript values.
 * It is used by `JavaScriptEnvironment` to provide bindings for
 * JavaScript classes such as `Map`.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Look up a JavaScript value.
 *
 * Note that this function calls [JavaScript's `eval`][js:eval].
 * For the sake of safety, it only accepts identifier strings, but
 * it will only work in environments that permit access to `eval`.
 *
 * Returns a tuple `(value found)`, where `found` is `#t` if there is
 * a JavaScript value under that name, and `#f` otherwise. If there
 * is not, then `value` is `undefined`.
 *
 * [js:eval]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
 */
declare function lookupJsValue(name: any): any;
declare namespace lookupJsValue {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
export { lookupJsValue };
