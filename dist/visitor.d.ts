/**
 * # Visitor pattern
 *
 * A simple implementation of the Visitor pattern.
 *
 * ## Description
 *
 * This file contains a simple implementation of the [Visitor
 * pattern][w:Visitor pattern]. It is based on predicate functions,
 * which are called to distinguish between different types of nodes.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [w:Visitor pattern]: https://en.wikipedia.org/wiki/Visitor_pattern
 */
/**
 * Visit `visitor` upon `node`, passing additional `args` if provided.
 */
declare function visit(visitor: any, node: any, ...args: any[]): any;
declare namespace visit {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Make a visitor function from a list of visitor clauses.
 *
 * Each clause is a list `(pred-f visit-f)`, where `pred-f` is a predicate
 * function and `visit-f` is a visitor function. The clauses are tried
 * in order, similar to a `cond` form.
 */
declare function makeVisitor(visitors?: any): any;
declare namespace makeVisitor {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
export { makeVisitor, visit };
