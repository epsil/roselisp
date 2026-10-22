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

import {
  currentCompilationOptions
} from './env';

/**
 * Expand a `(ts/as ...)` expression.
 */
function tsAs_(exp1: any, env: any): any {
  const [exp, typ]: any[] = exp1.slice(1);
  return exp;
}

tsAs_.ftype = 'macro';

tsAs_.fsource = [Symbol.for('define'), [Symbol.for('ts/as_'), Symbol.for('exp1'), Symbol.for('env')], [Symbol.for('declare'), [Symbol.for('ftype'), 'macro']], [Symbol.for('define-values'), [Symbol.for('exp'), Symbol.for('typ')], [Symbol.for('rest'), Symbol.for('exp1')]], Symbol.for('exp')];

export {
  tsAs_
};