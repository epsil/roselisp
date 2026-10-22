"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # TypeScript
 *
 * TypeScript constructs.
 *
 * ## Description
 *
 * Various TypeScript constructs. Note that these make a tangible
 * difference only if the output language is TypeScript.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.tsAs_ = void 0;
/**
 * Expand a `(ts/as ...)` expression.
 */
function tsAs_(exp1, env) {
    const [exp, typ] = exp1.slice(1);
    return exp;
}
exports.tsAs_ = tsAs_;
tsAs_.ftype = 'macro';
tsAs_.fsource = [Symbol.for('define'), [Symbol.for('ts/as_'), Symbol.for('exp1'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), [Symbol.for('exp'), Symbol.for('typ')], [Symbol.for('rest'), Symbol.for('exp1')]], Symbol.for('exp')];
