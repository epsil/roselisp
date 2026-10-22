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
/**
 * Expand a `(ts/as ...)` expression.
 */
declare function tsAs_(exp1: any, env: any): any;
declare namespace tsAs_ {
    var ftype: string;
    var fsource: (symbol | (symbol | (string | symbol)[])[])[];
}
export { tsAs_ };
