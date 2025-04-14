/**
 * # Exceptions
 *
 * Exceptions thrown during evaluation.
 *
 * ## Description
 *
 * These exceptions are thrown by the evaluator. For example, a
 * `BreakException` is thrown while evaluating a `BreakStatement`
 * ESTree node, which is what a `(break)` form compiles to.
 * Similarly, `ContinueException` is thrown when evaluating
 * `ContinueStatement` and `(continue)`.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * `BreakException`.
 *
 * Used for evaluating `BreakStatement` and `(break)`.
 */
declare class BreakException extends Error {
    constructor();
}
/**
 * `ContinueException`.
 *
 * Used for evaluating `ContinueStatement` and `(continue)`.
 */
declare class ContinueException extends Error {
    constructor();
}
/**
 * `YieldException`.
 *
 * Used for evaluating `YieldExpression` and `(yield ...)`.
 */
declare class YieldException extends Error {
    value: any;
    constructor(value: any);
}
/**
 * `ReturnException`.
 *
 * Used for evaluating `ReturnStatement` and `(return ...)`.
 */
declare class ReturnException extends Error {
    value: any;
    constructor(value: any);
}
export { BreakException, ContinueException, YieldException, ReturnException };
