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
 * Map a function over a tree.
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
export { mapHasP as mapHas, mapSetX as mapSet, beginWrap, beginWrapSmart, colonFormP, countTree, defineGeneric, defineMethod, formp, kebabCaseToCamelCase, kebabCaseToSnakeCase, lambdaToLet, makeUniqueSymbol, mapGet, mapGetTuple, mapHasP, mapSetX, mapTree, quasiquotep, quotep, taggedListP, textOfQuotation, unquoteSplicingP, unquotep };
