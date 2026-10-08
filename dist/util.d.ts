/**
 * # Utilities.
 *
 * Various utilities.
 *
 * ## Description
 *
 * A "miscellaneous" category for various utility functions.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Get the value stored under `path` in the map `map`.
 */
declare function mapGet(map: any, path: any): any;
/**
 * Get the value stored under `path` in the map `map`.
 * Returns a tuple `(value found)`, where `found`
 * is `#f` if there is no value stored under that path.
 */
declare function mapGetTuple(map: any, path: any): any;
/**
 * Whether there is a value stored under `path` in the map `map`.
 */
declare function mapHasP(map: any, path: any): any;
/**
 * Store a value `value` under `path` in the map `map`.
 */
declare function mapSetX(map: any, path: any, value: any): any;
/**
 * Make a symbol with a name different from the ones in `lst`.
 * The prefix to use may be specified with `prefix`.
 */
declare function makeUniqueSymbol(lst?: any, prefix?: any): any;
/**
 * Whether `casing-style` is a casing style that is
 * appropriate for JavaScript identifiers. Camel case
 * and snake case can be used in JavaScript, but
 * kebab case cannot.
 */
declare function validJsCasingStyleP(casingStyle: any): any;
/**
 * Whether we are compiling to JavaScript code,
 * rather than an ESTree intended for evaluation.
 */
declare function compilingToJsP(options: any): any;
/**
 * Transform a string to a valid JavaScript identifier
 * string, provided an appropriate casing style
 * (camel case or snake case) is specified in `options`.
 * The input is assumed to be kebab case.
 */
declare function makeIdentifierString(str: any, options?: any): any;
/**
 * Convert an identifier string from kebab case
 * to camel case.
 *
 *    > (camel-case "foo-bar")
 *    "fooBar"
 *
 * See also `kebab-case->snake-case`.
 */
declare function kebabCaseToCamelCase(str: any): any;
/**
 * Convert an identifier string from kebab case
 * to snake case.
 *
 *    > (camel-case "foo-bar")
 *    "foo_bar"
 *
 * See also `kebab-case->camel-case`.
 */
declare function kebabCaseToSnakeCase(str: any): any;
/**
 * Whether `exp` is a list whose first element is `tag`.
 * If `len` is specified, also checks whether the list is
 * of that length.
 *
 * Similar to [`tagged-list?` in
 * *Structure and Interpreation of Computer
 * Programs*][sicp:tagged-list-p].
 *
 * [sicp:tagged-list-p]: https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book-Z-H-26.html#%_idx_4290
 */
declare function taggedListP(exp: any, tag: any, len?: any): any;
/**
 * Unwrap a `(quote ...)` expression.
 */
declare function textOfQuotation(exp: any): any;
/**
 * Whether `exp` is a form referencing `f` in `env`.
 */
declare function formp(exp: any, f: any, env: any): any;
/**
 * Whether `exp` is a `(: ...)` expression.
 */
declare function colonFormP(exp: any): any;
/**
 * Whether `exp` is a `(quote ...)` expression.
 */
declare function quotep(exp: any): any;
/**
 * Whether `exp` is a `(quasiquote ...)` expression.
 */
declare function quasiquotep(exp: any): any;
/**
 * Whether `exp` is an `(unquote ...)` expression.
 */
declare function unquotep(exp: any): any;
/**
 * Whether `exp` is an `(unquote-splicing ...)` expression.
 */
declare function unquoteSplicingP(obj: any): any;
/**
 * Convert a `lambda` expression to a `let` expression.
 */
declare function lambdaToLet(lambdaExp: any, args: any): any;
/**
 * Map a function `f` over a tree `x`.
 *
 * `f` is called with two arguments: the current element
 * and a stack of parent elements.
 */
declare function mapTree(f: any, x: any): any;
/**
 * Count the number of occurrences in a tree
 * of elements matching the predicate `f`.
 */
declare function countTree(f: any, x: any): any;
/**
 * Wrap a list of expressions in a `(begin ...)` expression.
 */
declare function beginWrap(expressions: any): any;
/**
 * Wrap a list of expressions in a `(begin ...)` expression,
 * but do it smartly: in the case of a single expression,
 * no wrapping is necessary.
 */
declare function beginWrapSmart(expressions: any): any;
/**
 * Define a generic function.
 *
 * Similar to [`defgeneric`][cl:defgeneric] in Common Lisp.
 *
 * [cl:defgeneric]: http://clhs.lisp.se/Body/m_defgen.htm
 */
declare function defineGeneric(f: any): any;
/**
 * Define a method for a generic function.
 *
 * Similar to [`defmethod`][cl:defmethod] in Common Lisp.
 *
 * [cl:defmethod]: http://clhs.lisp.se/Body/m_defmet.htm
 */
declare function defineMethod(genericFunction: any, arglist: any, functionDefinition: any): any;
/**
 * Convert a list expression like `(list ...)`
 * or `(list* ...)` to a list expression pattern.
 */
declare function listExpressionToPattern(exp: any): any;
/**
 * Convert a number to a letter.
 *
 * Counting starts at zero. `0` corresponds to
 * `a`, `1` to `b`, etc., unless a different
 * starting letter is specified with `start`.
 */
declare function numberToLetter(n: any, start?: any): any;
/**
 * Given a binary function expression, produce a new
 * function expression that flips the argument order.
 */
declare function flipFunctionExpression(exp: any, x?: any, y?: any): any;
/**
 * Parse a parameter list into regular parameters
 * and rest parameter, if any.
 */
declare function parseParamsList(params: any): any;
/**
 * Convert a `(define ...)` expression to
 * a `(define-macro ...)` expression.
 */
declare function defineToDefineMacro(x: any, onceOnly?: any): any;
/**
 * Separate a property list from a list of body forms.
 * The property list is assumed to use keywords for
 * its keys.
 */
declare function parsePlistAndBody(plstAndBody: any): any;
/**
 * Make a function of the specified arity.
 */
declare function makeArityFunction(fun: any, n?: any, arrow?: any): any;
/**
 * Unwrap a `(quote ...)` expression.
 * Returns other values as-is.
 */
declare function unwrapQuoteExpression(exp: any): any;
/**
 * Rewrite `[...[x]]` to `[x]`, recursively.
 */
declare function insertIntoArrayExpX(x: any, arr: any): any;
/**
 * Copy elements from the list `source` to the list `dest`
 * so that the two become equal. Modifies `dest` in place.
 * Only the beginning of `dest` is modified; the rest is
 * assumed to be equal to that of `source`. Returns the
 * modified `dest` list.
 */
declare function copyIntoArrayX(source: any, dest: any): any;
/**
 * Remove indentation from a multi-line string.
 */
declare function removeIndentation(str: any): any;
/**
 * Call the thunk `f` with the current working directory
 * temporarily set to `dir`. Restores the previous
 * working directory afterwards.
 */
declare function withCwd(dir: any, f: any): any;
/**
 * Macro for fetching a package property from `package.json`
 * during compile time.
 */
declare function getPackageProperty(exp: any, env: any): any;
declare namespace getPackageProperty {
    var ftype: string;
}
/**
 * Locate the `node_modules/` directory nearest to `dir`.
 * Returns the absolute path as a string, or `#f` if
 * not found.
 *
 * This function searches for `node_modules/` in `dir`, then in the
 * parent directory of `dir`, then in the parent of the parent, and
 * so on, in a manner similar to
 * [Node's `require` algorithm][nodejs:require-algorithm].
 *
 * [nodejs:require-algorithm]: https://nodejs.org/api/modules.html#all-together
 */
declare function getNodeModulesPath(dir?: any): any;
export { mapHasP as mapHas, mapSetX as mapSet, beginWrap, beginWrapSmart, colonFormP, compilingToJsP, copyIntoArrayX, countTree, defineToDefineMacro, defineGeneric, defineMethod, flipFunctionExpression, formp, getNodeModulesPath, getPackageProperty, insertIntoArrayExpX, kebabCaseToCamelCase, kebabCaseToSnakeCase, lambdaToLet, listExpressionToPattern, makeArityFunction, makeIdentifierString, makeUniqueSymbol, mapGet, mapGetTuple, mapHasP, mapSetX, mapTree, numberToLetter, parseParamsList, parsePlistAndBody, quasiquotep, quotep, removeIndentation, taggedListP, textOfQuotation, unquoteSplicingP, unquotep, unwrapQuoteExpression, validJsCasingStyleP, withCwd };
