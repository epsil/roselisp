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

import {
  quoteSym_,
  quasiquoteSym_,
  unquoteSym_,
  unquoteSplicingSym_
} from './constants';

import {
  Rose
} from './rose';

const [lastCdr]: any[] = ((): any => {
  function lastCdr_(lst: any): any {
    if (!Array.isArray(lst)) {
      return undefined;
    } else if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
      let result: any = lst;
      while (Array.isArray(result) && (result.length >= 3) && (result[result.length - 2] === Symbol.for('.'))) {
        result = result[result.length - 1];
      }
      return result;
    } else {
      return [];
    }
  }
  return [lastCdr_];
})();

/**
 * Get the value stored under `path` in the map `map`.
 */
function mapGet(map: any, path: any): any {
  let [value]: any[] = mapGetTuple(map, path);
  return value;
}

/**
 * Get the value stored under `path` in the map `map`.
 * Returns a tuple `(value found)`, where `found`
 * is `#f` if there is no value stored under that path.
 */
function mapGetTuple(map: any, path: any): any {
  let value: any = map;
  let found: any = true;
  for (let key of path) {
    if (mapp(value) && value.has(key)) {
      value = value.get(key);
    } else {
      value = undefined;
      found = false;
      break;
    }
  }
  return [value, found];
}

/**
 * Whether there is a value stored under `path` in the map `map`.
 */
function mapHasP(map: any, path: any): any {
  let [, found]: any[] = mapGetTuple(map, path);
  return found;
}

/**
 * Store a value `value` under `path` in the map `map`.
 */
function mapSetX(map: any, path: any, value: any): any {
  const mapConstructor: any = map.constructor;
  const mapPath: any = path.slice(0, -1);
  const mapKey: any = path[path.length - 1];
  let currentMap: any = map;
  for (let key of mapPath) {
    let currentValue: any = currentMap.get(key);
    if (!(currentValue instanceof mapConstructor)) {
      currentValue = new mapConstructor();
      currentMap.set(key, currentValue);
    }
    currentMap = currentValue;
  }
  currentMap.set(mapKey, value);
  return map;
}

/**
 * Whether an object is a map.
 *
 * An object is regarded as a map if it is an instance of
 * [`Map`][js:Map], or if it defines `.has()`, `.get()` and `.set()`.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
function mapp(x: any): any {
  return mapInstanceP(x) || mapLikeP(x);
}

/**
 * Whether an object is a `Map` instance.
 */
function mapInstanceP(x: any): any {
  return x instanceof Map;
}

/**
 * Whether an object implements a [`Map`][js:Map]-like
 * interface of `.has()`, `.get()` and `.set()`.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
function mapLikeP(x: any): any {
  return methodp('has', x) && methodp('get', x) && methodp('set', x);
}

/**
 * Whether a method is defined on an object.
 */
function methodp(method: any, obj: any): any {
  return (obj !== null) && (typeof obj === 'object') && ((obj as any)[method] instanceof Function);
}

/**
 * Make a symbol with a name different from the ones in `lst`.
 * The prefix to use may be specified with `prefix`.
 */
function makeUniqueSymbol(lst: any = [], prefix: any = Symbol.for('x')): any {
  let result: any = prefix;
  const name: any = result.description as string;
  let i: any = 1;
  while (lst.includes(result)) {
    result = Symbol.for(name + i + '');
    i++;
  }
  return result;
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
function kebabCaseToCamelCase(str: any): any {
  const segments: any = str.split('-').filter(function (x: any): any {
    return x !== '';
  });
  if (segments.length === 0) {
    return '';
  } else if (segments.length === 1) {
    return segments[0];
  } else {
    const [firstSegment, ...restSegments]: any[] = segments;
    return firstSegment + restSegments.map(function (x: any): any {
      return x.charAt(0).toUpperCase() + x.substring(1);
    }).join('');
  }
}

/**
 * Convert an identifier string from kebab case
 * to snake case.
 *
 *    > (camel-case "foo-bar")
 *    "foo_bar"
 *
 * See also `kebab-case->camel-case`.
 */
function kebabCaseToSnakeCase(str: any): any {
  return str.replace(new RegExp('-', 'g'), '_');
}

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
function taggedListP(exp: any, tag: any, len: any = undefined): any {
  if (exp instanceof Rose) {
    return taggedListP(exp.getValue(), tag, len);
  } else if (Number.isFinite(len)) {
    return taggedListP(exp, tag) && (exp.length === len);
  } else {
    return Array.isArray(exp) && (typeof exp[0] === 'symbol') && (exp[0] === tag);
  }
}

/**
 * Unwrap a `(quote ...)` expression.
 */
function textOfQuotation(exp: any): any {
  if (Array.isArray(exp) && (exp.length >= 3) && (exp[exp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(exp);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    let i: any = 1;
    let result: any = exp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = exp[exp.length - 1];
      } else {
        result = exp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  } else {
    return exp[1];
  }
}

/**
 * Whether `exp` is a form referencing `f` in `env`.
 */
function formp(exp: any, f: any, env: any): any {
  if (exp instanceof Rose) {
    return formp(exp.getValue(), f, env);
  } else {
    return Array.isArray(exp) && (exp.length > 0) && (typeof exp[0] === 'symbol') && (env.get(exp[0]) === f);
  }
}

/**
 * Whether `exp` is a `(: ...)` expression.
 */
function colonFormP(exp: any): any {
  if (exp instanceof Rose) {
    return colonFormP(exp.getValue());
  } else {
    return Array.isArray(exp) && (exp.length >= 3) && (exp[1] === Symbol.for(':'));
  }
}

/**
 * Whether `exp` is a `(quote ...)` expression.
 */
function quotep(exp: any): any {
  return taggedListP(exp, quoteSym_);
}

/**
 * Whether `exp` is a `(quasiquote ...)` expression.
 */
function quasiquotep(exp: any): any {
  return taggedListP(exp, quasiquoteSym_);
}

/**
 * Whether `exp` is an `(unquote ...)` expression.
 */
function unquotep(exp: any): any {
  return taggedListP(exp, unquoteSym_);
}

/**
 * Whether `exp` is an `(unquote-splicing ...)` expression.
 */
function unquoteSplicingP(obj: any): any {
  return taggedListP(obj, unquoteSplicingSym_);
}

/**
 * Convert a `lambda` expression to a `let` expression.
 */
function lambdaToLet(lambdaExp: any, args: any): any {
  const params: any = (Array.isArray(lambdaExp) && (lambdaExp.length >= 3) && (lambdaExp[lambdaExp.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(lambdaExp);
    return Array.isArray(x) && (x.length === 0);
  })()) ? ((): any => {
    let i: any = 1;
    let result: any = lambdaExp;
    while (i > 0) {
      if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
        result = lambdaExp[lambdaExp.length - 1];
      } else {
        result = lambdaExp.slice(1);
      }
      i--;
    }
    if (Array.isArray(result)) {
      result = result[0];
    }
    return result;
  })() : lambdaExp[1];
  const body: any = lambdaExp.slice(2);
  const bindings: any = [];
  if (typeof params === 'symbol') {
    bindings.push([params, [Symbol.for('quote'), args]]);
  } else {
    const _end: any = params.length;
    for (let i: any = 0; i < _end; i++) {
      const param: any = (params as any)[i];
      const name: any = Array.isArray(param) ? param[0] : param;
      let value: any = (i >= args.length) ? (Array.isArray(param) ? ((Array.isArray(param) && (param.length >= 3) && (param[param.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(param);
        return Array.isArray(x) && (x.length === 0);
      })()) ? ((): any => {
        let i: any = 1;
        let result: any = param;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = param[param.length - 1];
          } else {
            result = param.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      })() : param[1]) : undefined) : [Symbol.for('quote'), (args as any)[i]];
      const binding: any = [name, value];
      bindings.push(binding);
    }
  }
  return [Symbol.for('let*'), bindings, ...body];
}

/**
 * Map a function over a tree.
 */
function mapTree(f: any, x: any): any {
  if (Array.isArray(x)) {
    return x.map(function (x1: any): any {
      return mapTree(f, x1);
    });
  } else {
    return f(x);
  }
}

/**
 * Count the number of occurrences in a tree
 * of elements matching the predicate `f`.
 */
function countTree(f: any, x: any): any {
  let n: any = 0;
  mapTree(function (x: any): any {
    if (f(x)) {
      n++;
    }
    return x;
  }, x);
  return n;
}

/**
 * Wrap a list of expressions in a `(begin ...)` expression.
 */
function beginWrap(expressions: any): any {
  if (!((): any => {
    const x: any = lastCdr(expressions);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    return expressions;
  } else {
    return [Symbol.for('begin'), ...expressions];
  }
}

/**
 * Wrap a list of expressions in a `(begin ...)` expression,
 * but do it smartly: in the case of a single expression,
 * no wrapping is necessary.
 */
function beginWrapSmart(expressions: any): any {
  if (!((): any => {
    const x: any = lastCdr(expressions);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    return expressions;
  } else if (expressions.length === 1) {
    return expressions[0];
  } else {
    return beginWrap(expressions);
  }
}

/**
 * Define a generic function.
 *
 * Similar to [`defgeneric`][cl:defgeneric] in Common Lisp.
 *
 * [cl:defgeneric]: http://clhs.lisp.se/Body/m_defgen.htm
 */
function defineGeneric(f: any): any {
  const methods: any = [];
  function genericFunction(...args: any[]): any {
    const methods: any = genericFunction.methods;
    for (let entry of methods) {
      const [params, functionDefinition]: any[] = entry;
      if (argsMatchesParams(args, params)) {
        return functionDefinition(...args);
      }
    }
    if (f) {
      return f(...args);
    } else {
      return undefined;
    }
  }
  genericFunction.methods = methods;
  genericFunction.defmethod = function (argList: any, functionDefinition: any): any {
    const entry: any = [argList, functionDefinition];
    genericFunction.methods.unshift(entry);
    return genericFunction;
  };
  return genericFunction;
}

/**
 * Define a method for a generic function.
 *
 * Similar to [`defmethod`][cl:defmethod] in Common Lisp.
 *
 * [cl:defmethod]: http://clhs.lisp.se/Body/m_defmet.htm
 */
function defineMethod(genericFunction: any, arglist: any, functionDefinition: any): any {
  return genericFunction.defmethod(arglist, functionDefinition);
}

/**
 * Helper function for `defGeneric`.
 */
function argsMatchesParams(args: any, params: any): any {
  if (args.length !== params.length) {
    return false;
  }
  const _end: any = params.length;
  for (let i: any = 0; i < _end; i++) {
    const param: any = (params as any)[i];
    const arg: any = (args as any)[i];
    if (Array.isArray(param)) {
      if (param[0] === 'eql') {
        let value: any = param[1];
        if (arg !== value) {
          return false;
        }
      } else if (param[0] === 'pred') {
        const pred: any = param[1];
        if (!pred(arg)) {
          return false;
        }
      }
    } else if (typeof param === 'string') {
      if (param === 'any') {
        continue;
      } else if (param === 'array') {
        if (Array.isArray(arg)) {
          continue;
        } else {
          return false;
        }
      } else if (typeof arg !== param) {
        return false;
      }
    } else {
      if (!(arg instanceof param)) {
        return false;
      }
    }
  }
  return true;
}

export {
  mapHasP as mapHas,
  mapSetX as mapSet,
  beginWrap,
  beginWrapSmart,
  colonFormP,
  countTree,
  defineGeneric,
  defineMethod,
  formp,
  kebabCaseToCamelCase,
  kebabCaseToSnakeCase,
  lambdaToLet,
  makeUniqueSymbol,
  mapGet,
  mapGetTuple,
  mapHasP,
  mapSetX,
  mapTree,
  quasiquotep,
  quotep,
  taggedListP,
  textOfQuotation,
  unquoteSplicingP,
  unquotep
};