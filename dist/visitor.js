"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.visit = exports.makeVisitor = void 0;
/**
 * Visit `visitor` upon `node`, passing additional `args` if provided.
 */
function visit(visitor, node, ...args) {
    if (visitor instanceof Function) {
        return visitor(node, ...args);
    }
    else {
        return visitor.visit(node, ...args);
    }
}
exports.visit = visit;
visit.lispSource = [Symbol.for('define'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('procedure?'), Symbol.for('visitor')], [Symbol.for('apply'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('send/apply'), Symbol.for('visitor'), Symbol.for('visit'), Symbol.for('node'), Symbol.for('args')]]]];
/**
 * Make a visitor function from a list of visitor clauses.
 *
 * Each clause is a list `(pred-f visit-f)`, where `pred-f` is a predicate
 * function and `visit-f` is a visitor function. The clauses are tried
 * in order, similar to a `cond` form.
 */
function makeVisitor(visitors = []) {
    return function (node, ...args) {
        let result = node;
        for (let entry of visitors) {
            const [predicate, visitor] = entry;
            if (predicate(node)) {
                result = visitor(node, ...args);
                break;
            }
        }
        return result;
    };
}
exports.makeVisitor = makeVisitor;
makeVisitor.lispSource = [Symbol.for('define'), [Symbol.for('make-visitor'), [Symbol.for('visitors'), [Symbol.for('quote'), []]]], [Symbol.for('lambda'), [Symbol.for('node'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('node')], [Symbol.for('for'), [[Symbol.for('entry'), Symbol.for('visitors')]], [Symbol.for('define-values'), [Symbol.for('predicate'), Symbol.for('visitor')], Symbol.for('entry')], [Symbol.for('when'), [Symbol.for('predicate'), Symbol.for('node')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('apply'), Symbol.for('visitor'), Symbol.for('node'), Symbol.for('args')]], [Symbol.for('break')]]], Symbol.for('result')]];
