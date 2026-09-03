"use strict";
// SPDX-License-Identifier: MPL-2.0
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.validJsCasingStyleP = exports.unquotep = exports.unquoteSplicingP = exports.textOfQuotation = exports.taggedListP = exports.quotep = exports.quasiquotep = exports.numberToLetter = exports.mapTree = exports.mapSetX = exports.mapHasP = exports.mapGetTuple = exports.mapGet = exports.makeUniqueSymbol = exports.makeIdentifierString = exports.listExpressionToPattern = exports.lambdaToLet = exports.kebabCaseToSnakeCase = exports.kebabCaseToCamelCase = exports.formp = exports.defineMethod = exports.defineGeneric = exports.countTree = exports.colonFormP = exports.beginWrapSmart = exports.beginWrap = exports.mapSet = exports.mapHas = void 0;
const constants_1 = require("./constants");
const rose_1 = require("./rose");
/**
 * Get the value stored under `path` in the map `map`.
 */
function mapGet(map, path) {
    let [value] = mapGetTuple(map, path);
    return value;
}
exports.mapGet = mapGet;
/**
 * Get the value stored under `path` in the map `map`.
 * Returns a tuple `(value found)`, where `found`
 * is `#f` if there is no value stored under that path.
 */
function mapGetTuple(map, path) {
    let value = map;
    let found = true;
    for (let key of path) {
        if (mapp(value) && value.has(key)) {
            value = value.get(key);
        }
        else {
            value = undefined;
            found = false;
            break;
        }
    }
    return [value, found];
}
exports.mapGetTuple = mapGetTuple;
/**
 * Whether there is a value stored under `path` in the map `map`.
 */
function mapHasP(map, path) {
    let [, found] = mapGetTuple(map, path);
    return found;
}
exports.mapHas = mapHasP;
exports.mapHasP = mapHasP;
/**
 * Store a value `value` under `path` in the map `map`.
 */
function mapSetX(map, path, value) {
    const mapConstructor = map.constructor;
    const mapPath = path.slice(0, -1);
    const mapKey = path.at(-1);
    let currentMap = map;
    for (let key of mapPath) {
        let currentValue = currentMap.get(key);
        if (!(currentValue instanceof mapConstructor)) {
            currentValue = new mapConstructor();
            currentMap.set(key, currentValue);
        }
        currentMap = currentValue;
    }
    currentMap.set(mapKey, value);
    return map;
}
exports.mapSet = mapSetX;
exports.mapSetX = mapSetX;
/**
 * Whether an object is a map.
 *
 * An object is regarded as a map if it is an instance of
 * [`Map`][js:Map], or if it defines `.has()`, `.get()` and `.set()`.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
function mapp(x) {
    return mapInstanceP(x) || mapLikeP(x);
}
/**
 * Whether an object is a `Map` instance.
 */
function mapInstanceP(x) {
    return x instanceof Map;
}
/**
 * Whether an object implements a [`Map`][js:Map]-like
 * interface of `.has()`, `.get()` and `.set()`.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
function mapLikeP(x) {
    return methodp('has', x) && methodp('get', x) && methodp('set', x);
}
/**
 * Whether a method is defined on an object.
 */
function methodp(method, obj) {
    return (obj !== null) && (typeof obj === 'object') && (obj[method] instanceof Function);
}
/**
 * Make a symbol with a name different from the ones in `lst`.
 * The prefix to use may be specified with `prefix`.
 */
function makeUniqueSymbol(lst = [], prefix = Symbol.for('x')) {
    let result = prefix;
    const name = result.description;
    let i = 1;
    while (lst.includes(result)) {
        result = Symbol.for(name + i + '');
        i++;
    }
    return result;
}
exports.makeUniqueSymbol = makeUniqueSymbol;
/**
 * Whether `casing-style` is a casing style that is
 * appropriate for JavaScript identifiers. Camel case
 * and snake case can be used in JavaScript, but
 * kebab case cannot.
 */
function validJsCasingStyleP(casingStyle) {
    return ['camelcase', 'snakecase'].includes(casingStyle);
}
exports.validJsCasingStyleP = validJsCasingStyleP;
/**
 * Transform a string to a valid JavaScript identifier
 * string, provided an appropriate casing style
 * (camel case or snake case) is specified in `options`.
 * The input is assumed to be kebab case.
 */
function makeIdentifierString(str, options = {}) {
    let result = str;
    const caseOption = options['case'] || 'none';
    if (validJsCasingStyleP(caseOption)) {
        result = makeIdentifierStringHelper(result);
    }
    if (caseOption === 'camelcase') {
        return kebabCaseToCamelCase(result);
    }
    else if (caseOption === 'snakecase') {
        return kebabCaseToSnakeCase(result);
    }
    else {
        return result;
    }
}
exports.makeIdentifierString = makeIdentifierString;
/**
 * Helper function for `make-identifier-string`.
 */
function makeIdentifierStringHelper(str) {
    let result = str.replace(new RegExp('^\\+$', 'g'), '_add').replace(new RegExp('^-$', 'g'), '_sub').replace(new RegExp('^\\*$', 'g'), '_mul').replace(new RegExp('^/$', 'g'), '_div').replace(new RegExp('%', 'g'), '').replace(new RegExp('/', 'g'), '-').replace(new RegExp(':', 'g'), '-').replace(new RegExp('->', 'g'), '-to-').replace(new RegExp('\\+', 'g'), '_').replace(new RegExp('\\*$', 'g'), '-star').replace(new RegExp('\\*', 'g'), 'star-');
    const containsMultipleSegments = result.match(new RegExp('-', 'g'));
    if (containsMultipleSegments) {
        return result.replace(new RegExp('\\?', 'g'), '-p').replace(new RegExp('!', 'g'), '-x');
    }
    else {
        return result.replace(new RegExp('\\?', 'g'), 'p').replace(new RegExp('!', 'g'), 'x');
    }
}
/**
 * Convert an identifier string from kebab case
 * to camel case.
 *
 *    > (camel-case "foo-bar")
 *    "fooBar"
 *
 * See also `kebab-case->snake-case`.
 */
function kebabCaseToCamelCase(str) {
    const segments = str.split('-').filter(function (x) {
        return x !== '';
    });
    if (segments.length === 0) {
        return '';
    }
    else if (segments.length === 1) {
        return segments[0];
    }
    else {
        const [firstSegment, ...restSegments] = segments;
        return firstSegment + restSegments.map(function (x) {
            return x.charAt(0).toUpperCase() + x.substring(1);
        }).join('');
    }
}
exports.kebabCaseToCamelCase = kebabCaseToCamelCase;
/**
 * Convert an identifier string from kebab case
 * to snake case.
 *
 *    > (camel-case "foo-bar")
 *    "foo_bar"
 *
 * See also `kebab-case->camel-case`.
 */
function kebabCaseToSnakeCase(str) {
    return str.replace(new RegExp('-', 'g'), '_');
}
exports.kebabCaseToSnakeCase = kebabCaseToSnakeCase;
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
function taggedListP(exp, tag, len = undefined) {
    if ((0, rose_1.syntaxp)(exp)) {
        return taggedListP((0, rose_1.syntaxToDatum)(exp), tag, len);
    }
    else if (Array.isArray(tag)) {
        for (let x of tag) {
            if (taggedListP(exp, x, len)) {
                return true;
            }
        }
        return false;
    }
    else if (Number.isFinite(len)) {
        return taggedListP(exp, tag) && (exp.length === len);
    }
    else {
        return Array.isArray(exp) && (typeof exp[0] === 'symbol') && (exp[0] === tag);
    }
}
exports.taggedListP = taggedListP;
/**
 * Unwrap a `(quote ...)` expression.
 */
function textOfQuotation(exp) {
    return exp[1];
}
exports.textOfQuotation = textOfQuotation;
/**
 * Whether `exp` is a form referencing `f` in `env`.
 */
function formp(exp, f, env) {
    if ((0, rose_1.syntaxp)(exp)) {
        return formp((0, rose_1.syntaxToDatum)(exp), f, env);
    }
    else {
        if (Array.isArray(exp) && (exp.length > 0)) {
            const op = exp[0];
            if (typeof op !== 'symbol') {
                return false;
            }
            else if (env.hasPromiseP(op)) {
                // If the operator is bound to a thunk,
                // it is a user-defined binding.
                return false;
            }
            else {
                const val = env.get(op);
                return f === val;
            }
        }
        else {
            return false;
        }
    }
}
exports.formp = formp;
/**
 * Whether `exp` is a `(: ...)` expression.
 */
function colonFormP(exp) {
    if ((0, rose_1.syntaxp)(exp)) {
        return colonFormP((0, rose_1.syntaxToDatum)(exp));
    }
    else {
        return Array.isArray(exp) && (exp.length >= 3) && (exp[1] === Symbol.for(':'));
    }
}
exports.colonFormP = colonFormP;
/**
 * Whether `exp` is a `(quote ...)` expression.
 */
function quotep(exp) {
    return taggedListP(exp, constants_1.quoteSym_);
}
exports.quotep = quotep;
/**
 * Whether `exp` is a `(quasiquote ...)` expression.
 */
function quasiquotep(exp) {
    return taggedListP(exp, constants_1.quasiquoteSym_);
}
exports.quasiquotep = quasiquotep;
/**
 * Whether `exp` is an `(unquote ...)` expression.
 */
function unquotep(exp) {
    return taggedListP(exp, constants_1.unquoteSym_);
}
exports.unquotep = unquotep;
/**
 * Whether `exp` is an `(unquote-splicing ...)` expression.
 */
function unquoteSplicingP(obj) {
    return taggedListP(obj, constants_1.unquoteSplicingSym_);
}
exports.unquoteSplicingP = unquoteSplicingP;
/**
 * Convert a `lambda` expression to a `let` expression.
 */
function lambdaToLet(lambdaExp, args) {
    const params = lambdaExp[1];
    const body = lambdaExp.slice(2);
    const bindings = [];
    if (typeof params === 'symbol') {
        bindings.push([params, [Symbol.for('quote'), args]]);
    }
    else {
        const _end = params.length;
        for (let i = 0; i < _end; i++) {
            const param = params[i];
            const name = Array.isArray(param) ? param[0] : param;
            let value = (i >= args.length) ? (Array.isArray(param) ? param[1] : undefined) : [Symbol.for('quote'), args[i]];
            const binding = [name, value];
            bindings.push(binding);
        }
    }
    return [Symbol.for('let*'), bindings, ...body];
}
exports.lambdaToLet = lambdaToLet;
/**
 * Map a function over a tree.
 */
function mapTree(f, x) {
    if (Array.isArray(x)) {
        return x.map(function (x1) {
            return mapTree(f, x1);
        });
    }
    else {
        return f(x);
    }
}
exports.mapTree = mapTree;
/**
 * Count the number of occurrences in a tree
 * of elements matching the predicate `f`.
 */
function countTree(f, x) {
    let n = 0;
    mapTree(function (x) {
        if (f(x)) {
            n++;
        }
        return x;
    }, x);
    return n;
}
exports.countTree = countTree;
/**
 * Wrap a list of expressions in a `(begin ...)` expression.
 */
function beginWrap(expressions) {
    if (!(Array.isArray(expressions) && !((expressions.length >= 3) && (expressions.at(-2) === Symbol.for('.')) && !Array.isArray(expressions.at(-1))))) {
        return expressions;
    }
    else {
        return [Symbol.for('begin'), ...expressions];
    }
}
exports.beginWrap = beginWrap;
/**
 * Wrap a list of expressions in a `(begin ...)` expression,
 * but do it smartly: in the case of a single expression,
 * no wrapping is necessary.
 */
function beginWrapSmart(expressions) {
    if (!(Array.isArray(expressions) && !((expressions.length >= 3) && (expressions.at(-2) === Symbol.for('.')) && !Array.isArray(expressions.at(-1))))) {
        return expressions;
    }
    else if (expressions.length === 1) {
        return expressions[0];
    }
    else {
        return beginWrap(expressions);
    }
}
exports.beginWrapSmart = beginWrapSmart;
/**
 * Define a generic function.
 *
 * Similar to [`defgeneric`][cl:defgeneric] in Common Lisp.
 *
 * [cl:defgeneric]: http://clhs.lisp.se/Body/m_defgen.htm
 */
function defineGeneric(f) {
    const methods = [];
    function genericFunction(...args) {
        const methods = genericFunction.methods;
        for (let entry of methods) {
            const [params, functionDefinition] = entry;
            if (argsMatchesParamsP(args, params)) {
                return functionDefinition(...args);
            }
        }
        if (f) {
            return f(...args);
        }
        else {
            return undefined;
        }
    }
    genericFunction.methods = methods;
    genericFunction.defmethod = function (argList, functionDefinition) {
        const entry = [argList, functionDefinition];
        genericFunction.methods.unshift(entry);
        return genericFunction;
    };
    return genericFunction;
}
exports.defineGeneric = defineGeneric;
/**
 * Define a method for a generic function.
 *
 * Similar to [`defmethod`][cl:defmethod] in Common Lisp.
 *
 * [cl:defmethod]: http://clhs.lisp.se/Body/m_defmet.htm
 */
function defineMethod(genericFunction, arglist, functionDefinition) {
    return genericFunction.defmethod(arglist, functionDefinition);
}
exports.defineMethod = defineMethod;
/**
 * Helper function for `defGeneric`.
 */
function argsMatchesParamsP(args, params) {
    if (args.length !== params.length) {
        return false;
    }
    const _end = params.length;
    for (let i = 0; i < _end; i++) {
        const param = params[i];
        const arg = args[i];
        if (Array.isArray(param)) {
            if (param[0] === 'eql') {
                let value = param[1];
                if (arg !== value) {
                    return false;
                }
            }
            else if (param[0] === 'pred') {
                const pred = param[1];
                if (!pred(arg)) {
                    return false;
                }
            }
        }
        else if (typeof param === 'string') {
            if (param === 'any') {
                continue;
            }
            else if (param === 'array') {
                if (Array.isArray(arg)) {
                    continue;
                }
                else {
                    return false;
                }
            }
            else if (typeof arg !== param) {
                return false;
            }
        }
        else {
            if (!(arg instanceof param)) {
                return false;
            }
        }
    }
    return true;
}
/**
 * Convert a list expression like `(list ...)`
 * or `(list* ...)` to a list expression pattern.
 */
function listExpressionToPattern(exp) {
    if (Array.isArray(exp)) {
        if (taggedListP(exp, [Symbol.for('list'), Symbol.for('values')])) {
            if (exp.at(-1) === Symbol.for('...')) {
                const head = exp.slice(1).slice(0, -2);
                const tail = exp[exp.length - 2];
                return listExpressionToPattern([Symbol.for('list*'), ...head, tail]);
            }
            else {
                return exp.slice(1).map(function (x) {
                    return listExpressionToPattern(x);
                });
            }
        }
        else if (taggedListP(exp, Symbol.for('list*'))) {
            const head = exp.slice(1).slice(0, -1);
            const tail = exp.at(-1);
            if (head.length === 0) {
                return listExpressionToPattern(tail);
            }
            else {
                return [...head.map(function (x) {
                        return listExpressionToPattern(x);
                    }), Symbol.for('.'), listExpressionToPattern(tail)];
            }
        }
        else {
            return false;
        }
    }
    else {
        return exp;
    }
}
exports.listExpressionToPattern = listExpressionToPattern;
/**
 * Convert a number to a letter.
 *
 * Counting starts at zero. `0` corresponds to
 * `a`, `1` to `b`, etc., unless a different
 * starting letter is specified with `start`.
 */
function numberToLetter(n, start = 'a') {
    return String.fromCharCode(start.charCodeAt(0) + n);
}
exports.numberToLetter = numberToLetter;
