"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.lookupJsValue = void 0;
/**
 * Regular expression for matching JavaScript identifiers.
 */
const jsIdentifierRegexp = 
// Very simple expression, does not
// handle Unicode identifiers.
new RegExp('^\\w+$');
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
function lookupJsValue(name) {
    const str = (typeof name === 'symbol') ? name.description : name;
    let value = undefined;
    let found = false;
    // Only evaluate the string if it is an identifier string.
    if ((typeof str === 'string') && str.match(jsIdentifierRegexp)) {
        try {
            value = eval(str);
            found = true;
        }
        catch (e) {
            if (e instanceof Error) {
            }
            else {
                throw e;
            }
        }
    }
    return [value, found];
}
exports.lookupJsValue = lookupJsValue;
lookupJsValue.lispSource = [Symbol.for('define'), [Symbol.for('lookup-js-value'), Symbol.for('name')], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('if'), [Symbol.for('symbol?'), Symbol.for('name')], [Symbol.for('symbol->string'), Symbol.for('name')], Symbol.for('name')]], [Symbol.for('define'), Symbol.for('value'), Symbol.for('undefined')], [Symbol.for('define'), Symbol.for('found'), Symbol.for('#f')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('string?'), Symbol.for('str')], [Symbol.for('regexp-match'), Symbol.for('js-identifier-regexp'), Symbol.for('str')]], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('value'), [Symbol.for('js/eval'), Symbol.for('str')]], [Symbol.for('set!'), Symbol.for('found'), Symbol.for('#t')], [Symbol.for('catch'), Symbol.for('Error'), Symbol.for('e')]]], [Symbol.for('values'), Symbol.for('value'), Symbol.for('found')]];
