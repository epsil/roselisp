// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Objects
 *
 * JavaScript objects.
 *
 * ## Description
 *
 * Functions for working with JavaScript objects.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

const [keywordp]: any[] = ((): any => {
  function keywordp_(obj: any): any {
    return (typeof obj === 'symbol') && ((obj.description as string).match(/^:/) ? true : false);
  }
  return [keywordp_];
})();

/**
 * Make a JavaScript object.
 *
 * Similar to [`js-obj` in ClojureScript][cljs:js-obj].
 *
 * [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
 */
function object_(...args: any[]): any {
  const entries: any = [];
  const _end: any = args.length;
  for (let i: any = 0; i < _end; i = i + 2) {
    let prop: any = (args as any)[i];
    const val: any = args[i + 1];
    if (keywordp(prop)) {
      prop = (prop.description as string).replace(/^:/, '');
    }
    if (typeof prop === 'symbol') {
      prop = prop.description as string;
    }
    entries.push([prop, val]);
  }
  return Object.fromEntries(entries);
}

object_.fsource = [Symbol.for('define'), [Symbol.for('object_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('entries'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('args')], 2]]], [Symbol.for('let'), [[Symbol.for('prop'), [Symbol.for('list-ref'), Symbol.for('args'), Symbol.for('i')]], [Symbol.for('val'), [Symbol.for('list-ref'), Symbol.for('args'), [Symbol.for('+'), Symbol.for('i'), 1]]]], [Symbol.for('when'), [Symbol.for('keyword?'), Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('prop'), [Symbol.for('keyword->string'), Symbol.for('prop')]]], [Symbol.for('when'), [Symbol.for('symbol?'), Symbol.for('prop')], [Symbol.for('set!'), Symbol.for('prop'), [Symbol.for('symbol->string'), Symbol.for('prop')]]], [Symbol.for('push-right!'), Symbol.for('entries'), [Symbol.for('list'), Symbol.for('prop'), Symbol.for('val')]]]], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('fromEntries'), Symbol.for('entries')]]];

/**
 * Whether something is a JavaScript object.
 */
function objectp_(x: any): any {
  // This function avoids regarding JavaScript's `null` value as an
  // object (even if JavaScript does), because it has no properties;
  // and unlike the empty object, attempting to access a property on
  // it causes an error to be thrown. This is more trouble than it is
  // worth, so only non-`null` object values are considered to be
  // proper objects here.
  return (x !== null) && (typeof x === 'object');
}

objectp_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    if (!(Array.isArray(x) && (x.length > 0))) {
      return [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('js/null?'), x]], [Symbol.for('object-type?'), x]];
    } else {
      const x1: any = Symbol('x');
      return [Symbol.for('let'), [[x1, x]], ((x: any): any => [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('js/null?'), x]], [Symbol.for('object-type?'), x]])(x1)];
    }
  };
  f.ftype = 'macro';
  return f;
})();

objectp_.fsource = [Symbol.for('define'), [Symbol.for('object?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('js/null?'), Symbol.for('x')]], [Symbol.for('object-type?'), Symbol.for('x')]]];

/**
 * Whether something types as a JavaScript object.
 *
 * Note that this includes JavaScript's `null` value.
 */
function objectTypeP_(x: any): any {
  return typeof x === 'object';
}

objectTypeP_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [x]: any[] = exp.slice(1);
    return [Symbol.for('eq?'), [Symbol.for('type-of'), x], 'object'];
  };
  f.ftype = 'macro';
  return f;
})();

objectTypeP_.fsource = [Symbol.for('define'), [Symbol.for('object-type?_'), Symbol.for('x')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'object']];

/**
 * Combine multiple JavaScript objects into a new JavaScript object.
 *
 * Like `append`, but for JavaScript objects.
 */
function objectAppend_(...args: any[]): any {
  return Object.assign({}, ...args);
}

objectAppend_.fsource = [Symbol.for('define'), [Symbol.for('object-append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('Object'), Symbol.for('assign'), [Symbol.for('object')], Symbol.for('args')]];

/**
 * Copy a JavaScript object.
 */
function objectCopy_(obj: any): any {
  return {
    ...obj
  };
}

objectCopy_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [obj]: any[] = exp.slice(1);
    return [Symbol.for('object-append'), obj];
  };
  f.ftype = 'macro';
  return f;
})();

objectCopy_.fsource = [Symbol.for('define'), [Symbol.for('object-copy_'), Symbol.for('obj')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('object-append'), Symbol.for('obj')]];

/**
 * Spread a JavaScript object into another.
 */
function objectSpread_(x: any): any {
  return x;
}

objectSpread_.fsource = [Symbol.for('define'), [Symbol.for('object-spread_'), Symbol.for('x')], Symbol.for('x')];

/**
 * Return the keys for a JavaScript object.
 *
 * Similar to [`js-keys` in ClojureScript][cljs:js-keys].
 * [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
 */
function objectKeys_(obj: any): any {
  return Object.keys(obj);
}

objectKeys_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [obj]: any[] = exp.slice(1);
    return [Symbol.for('send'), Symbol.for('Object'), Symbol.for('keys'), obj];
  };
  f.ftype = 'macro';
  return f;
})();

objectKeys_.fsource = [Symbol.for('define'), [Symbol.for('object-keys_'), Symbol.for('obj')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('send'), Symbol.for('Object'), Symbol.for('keys'), Symbol.for('obj')]];

/**
 * Look up the property `key` in `obj`.
 *
 * Similar to [`object-get` in Racket][rkt:object-get] and
 * [`oget` in ClojureScript][cljs:oget].
 *
 * [rkt:object-get]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-get%29%29
 * [cljs:oget]: https://github.com/binaryage/cljs-oops#object-operations
 */
function objectRef_(obj: any, key: any): any {
  return (obj as any)[key];
}

objectRef_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [obj, key]: any[] = exp.slice(1);
    return [Symbol.for('js/ref'), obj, key];
  };
  f.ftype = 'macro';
  return f;
})();

objectRef_.fsource = [Symbol.for('define'), [Symbol.for('object-ref_'), Symbol.for('obj'), Symbol.for('key')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/ref'), Symbol.for('obj'), Symbol.for('key')]];

/**
 * Set the property `key` in `obj` to `val`.
 *
 * Similar to [`object-set!` in Racket][rkt:object-set] and
 * [`oset!` in ClojureScript][cljs:oset].
 *
 * [rkt:object-set]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-set%21%29%29
 * [cljs:oset]: https://github.com/binaryage/cljs-oops#object-operations
 */
function objectSetX_(obj: any, key: any, val: any): any {
  return (obj as any)[key] = val;
}

objectSetX_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [obj, key, val]: any[] = exp.slice(1);
    return [Symbol.for('js/='), [Symbol.for('object-ref'), obj, key], val];
  };
  f.ftype = 'macro';
  return f;
})();

objectSetX_.fsource = [Symbol.for('define'), [Symbol.for('object-set!_'), Symbol.for('obj'), Symbol.for('key'), Symbol.for('val')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('js/='), [Symbol.for('object-ref'), Symbol.for('obj'), Symbol.for('key')], Symbol.for('val')]];

/**
 * Return the keys for an object.
 *
 * Similar to [`field-names` in Racket][rkt:field-names].
 *
 * [rkt:field-names]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._field-names%29%29
 */
function fieldNames_(obj: any): any {
  return Object.keys(obj);
}

fieldNames_.compilerMacro = ((): any => {
  const f: any = (exp: any, env: any): any => {
    const [obj]: any[] = exp.slice(1);
    return [Symbol.for('object-keys'), obj];
  };
  f.ftype = 'macro';
  return f;
})();

fieldNames_.fsource = [Symbol.for('define'), [Symbol.for('field-names_'), Symbol.for('obj')], [Symbol.for('declare'), [Symbol.for('inline'), true]], [Symbol.for('object-keys'), Symbol.for('obj')]];

export {
  objectRef_ as objectGet_,
  objectRef_ as oget_,
  objectSetX_ as objectSet_,
  objectSetX_ as osetx_,
  objectSetX_ as oset_,
  fieldNames_,
  objectAppend_,
  objectCopy_,
  objectKeys_,
  objectRef_,
  objectSetX_,
  objectSpread_,
  objectTypeP_,
  objectp_,
  object_
};