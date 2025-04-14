// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Equality
 *
 * Equality algorithms.
 *
 * ## Description
 *
 * This file provides various functions for checking if two values
 * are equal.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

const [cdr]: any[] = ((): any => {
  function cdr_(lst: any): any {
    if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
      return lst[2];
    } else {
      return lst.slice(1);
    }
  }
  return [cdr_];
})();

/**
 * Strict equality.
 *
 * Similar to [`eq?` in Racket][rkt:eqp] and
 * [`eq` in Common Lisp][cl:eq].
 *
 * [rkt:eqp]: https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._eq~3f%29%29
 * [cl:eq]: http://clhs.lisp.se/Body/f_eq.htm#eq
 */
function eqp_(x: any, y: any): any {
  return x === y;
}

eqp_.lispSource = [Symbol.for('define'), [Symbol.for('eq?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/==='), Symbol.for('x'), Symbol.for('y')]];

/**
 * Loose equality.
 *
 * Similar to [`eqv?` in Racket][rkt:eqvp] and
 * [`eql` in Common Lisp][cl:eql].
 *
 * [rkt:eqvp]: https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._eqv~3f%29%29
 * [cl:eql]: http://clhs.lisp.se/Body/f_eql.htm#eql
 */
function eqvp_(x: any, y: any): any {
  return Object.is(x, y);
}

eqvp_.lispSource = [Symbol.for('define'), [Symbol.for('eqv?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('js/same-value?'), Symbol.for('x'), Symbol.for('y')]];

/**
 * Structural equality.
 *
 * Similar to [`equal?` in Racket][rkt:equalp] and
 * [`equal` in Common Lisp][cl:equal].
 *
 * [rkt:equalp]: https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._equal~3f%29%29
 * [cl:equal]: http://clhs.lisp.se/Body/f_equal.htm#equal
 */
function equalp_(x: any, y: any): any {
  if (x === y) {
    // Compare equivalent values.
    return true;
  } else if (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && Array.isArray(y)) {
    // Compare linked lists.
    const cdrX: any = (Array.isArray(x) && (x.length === 3) && (x[1] === Symbol.for('.'))) ? x[2] : x.slice(1);
    if (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && (x.length === 3) && !Array.isArray(cdrX) && !(Array.isArray(cdrX) && (cdrX.length >= 3) && (cdrX[cdrX.length - 2] === Symbol.for('.')))) {
      return false;
    } else if (equalp_(x[0], y[0])) {
      return equalp_(cdrX, cdr(y));
    } else {
      return false;
    }
  } else if (Array.isArray(x) && Array.isArray(y) && (y.length >= 3) && (y[y.length - 2] === Symbol.for('.'))) {
    return equalp_(y, x);
  } else if (Array.isArray(x) && Array.isArray(y)) {
    // Compare arrays.
    if (x.length !== y.length) {
      return false;
    }
    const _end: any = x.length;
    for (let i: any = 0; i < _end; i++) {
      if (!equalp_((x as any)[i], (y as any)[i])) {
        return false;
      }
    }
    return true;
  } else if ((x instanceof Map) && (y instanceof Map)) {
    // Compare hash maps.
    if (x.size !== y.size) {
      return false;
    }
    for (let entry of x.entries()) {
      const [key1, value1]: any[] = entry;
      const value2: any = y.get(key1);
      if (!equalp_(value1, value2)) {
        return false;
      }
    }
    return true;
  } else if ((x !== null) && (typeof x === 'object') && (y !== null) && (typeof y === 'object')) {
    // Compare objects.
    if (Object.keys(x).length !== Object.keys(y).length) {
      return false;
    }
    for (let key of Object.keys(x)) {
      if (!equalp_((x as any)[key], (y as any)[key])) {
        return false;
      }
    }
    return true;
  } else {
    return false;
  }
}

equalp_.lispSource = [Symbol.for('define'), [Symbol.for('equal?_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('x'), Symbol.for('y')], Symbol.for('#t')], [[Symbol.for('and'), [Symbol.for('linked-list-link?'), Symbol.for('x')], [Symbol.for('array-list?'), Symbol.for('y')]], [Symbol.for('define'), Symbol.for('cdr-x'), [Symbol.for('linked-list-cdr'), Symbol.for('x')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('linked-list-link?'), Symbol.for('x')], [Symbol.for('='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('not'), [Symbol.for('array-list?'), Symbol.for('cdr-x')]], [Symbol.for('not'), [Symbol.for('linked-list-link?'), Symbol.for('cdr-x')]]], Symbol.for('#f')], [[Symbol.for('equal?_'), [Symbol.for('car'), Symbol.for('x')], [Symbol.for('car'), Symbol.for('y')]], [Symbol.for('equal?_'), Symbol.for('cdr-x'), [Symbol.for('cdr'), Symbol.for('y')]]], [Symbol.for('else'), Symbol.for('#f')]]], [[Symbol.for('and'), [Symbol.for('array-list?'), Symbol.for('x')], [Symbol.for('linked-list-link?'), Symbol.for('y')]], [Symbol.for('equal?_'), Symbol.for('y'), Symbol.for('x')]], [[Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('array?'), Symbol.for('y')]], [Symbol.for('unless'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('x')], [Symbol.for('array-list-length'), Symbol.for('y')]], [Symbol.for('return'), Symbol.for('#f')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('x')]]]], [Symbol.for('unless'), [Symbol.for('equal?_'), [Symbol.for('aget'), Symbol.for('x'), Symbol.for('i')], [Symbol.for('aget'), Symbol.for('y'), Symbol.for('i')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')], [[Symbol.for('and'), [Symbol.for('hash?'), Symbol.for('x')], [Symbol.for('hash?'), Symbol.for('y')]], [Symbol.for('unless'), [Symbol.for('='), [Symbol.for('hash-size'), Symbol.for('x')], [Symbol.for('hash-size'), Symbol.for('y')]], [Symbol.for('return'), Symbol.for('#f')]], [Symbol.for('for'), [[Symbol.for('entry'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('entries')]]], [Symbol.for('define-values'), [Symbol.for('key1'), Symbol.for('value1')], Symbol.for('entry')], [Symbol.for('define'), Symbol.for('value2'), [Symbol.for('send'), Symbol.for('y'), Symbol.for('get'), Symbol.for('key1')]], [Symbol.for('unless'), [Symbol.for('equal?_'), Symbol.for('value1'), Symbol.for('value2')], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')], [[Symbol.for('and'), [Symbol.for('object?'), Symbol.for('x')], [Symbol.for('object?'), Symbol.for('y')]], [Symbol.for('unless'), [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('js-keys'), Symbol.for('x')]], [Symbol.for('array-list-length'), [Symbol.for('js-keys'), Symbol.for('y')]]], [Symbol.for('return'), Symbol.for('#f')]], [Symbol.for('for'), [[Symbol.for('key'), [Symbol.for('js-keys'), Symbol.for('x')]]], [Symbol.for('unless'), [Symbol.for('equal?_'), [Symbol.for('oget'), Symbol.for('x'), Symbol.for('key')], [Symbol.for('oget'), Symbol.for('y'), Symbol.for('key')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')], [Symbol.for('else'), Symbol.for('#f')]]];

export {
  eqp_ as eq,
  eqp_ as eqp,
  eqp_ as eq_,
  equalp_ as equalp,
  equalp_ as equal_,
  eqvp_ as eql,
  eqvp_ as eqlp,
  eqvp_ as eqlp_,
  eqvp_ as eql_,
  eqvp_ as eqv,
  eqvp_ as eqvp,
  eqvp_ as eqv_,
  eqp_,
  equalp_,
  eqvp_
};