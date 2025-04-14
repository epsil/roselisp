// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Procedures
 *
 * Various procedures.
 *
 * ## Description
 *
 * This file defines various procedures that are part of the language
 * environment.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

const [equalp, keywordp]: any[] = ((): any => {
  function equalp_(x: any, y: any): any {
    if (x === y) {
      return true;
    } else if (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && Array.isArray(y)) {
      const cdrX: any = (Array.isArray(x) && (x.length === 3) && (x[1] === Symbol.for('.'))) ? x[2] : x.slice(1);
      if (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && (x.length === 3) && !Array.isArray(cdrX) && !(Array.isArray(cdrX) && (cdrX.length >= 3) && (cdrX[cdrX.length - 2] === Symbol.for('.')))) {
        return false;
      } else if (equalp_(x[0], y[0])) {
        return equalp_(cdrX, ((): any => {
          function cdr_(lst: any): any {
            if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
              return lst[2];
            } else {
              return lst.slice(1);
            }
          }
          return cdr_;
        })()(y));
      } else {
        return false;
      }
    } else if (Array.isArray(x) && Array.isArray(y) && (y.length >= 3) && (y[y.length - 2] === Symbol.for('.'))) {
      return equalp_(y, x);
    } else if (Array.isArray(x) && Array.isArray(y)) {
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
  function keywordp_(obj: any): any {
    return (typeof obj === 'symbol') && (obj.description as string).match(new RegExp('^:'));
  }
  function cdr_(lst: any): any {
    if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
      return lst[2];
    } else {
      return lst.slice(1);
    }
  }
  return [equalp_, keywordp_];
})();

/**
 * Call `f` with `args`, using the last arg as a list of args.
 * Returns the value `f` returns.
 *
 * Similar to [`apply` in Racket][rkt:apply] and
 * [`apply` in Common Lisp][cl:apply].
 *
 * [rkt:apply]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._apply%29%29
 * [cl:apply]: http://clhs.lisp.se/Body/f_apply.htm#apply
 */
function apply_(f: any, ...args: any[]): any {
  if (args.length > 0) {
    args = [...args.slice(0, -1), ...args[args.length - 1]];
  }
  return f.apply(null, args);
}

apply_.lispSource = [Symbol.for('define'), [Symbol.for('apply_'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('args')], 0], [Symbol.for('set!'), Symbol.for('args'), [Symbol.for('append'), [Symbol.for('drop-right'), Symbol.for('args'), 1], [Symbol.for('array-list-last'), Symbol.for('args')]]]], [Symbol.for('send'), Symbol.for('f'), Symbol.for('apply'), Symbol.for('js/null'), Symbol.for('args')]];

/**
 * Call `f` with `args`.
 * Returns the value `f` returns.
 *
 * Similar to [`funcall` in Common Lisp][cl:funcall].
 *
 * [cl:funcall]: http://clhs.lisp.se/Body/f_funcal.htm#funcall
 */
function funcall_(f: any, ...args: any[]): any {
  return f.call(null, ...args);
}

funcall_.lispSource = [Symbol.for('define'), [Symbol.for('funcall_'), Symbol.for('f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('f'), Symbol.for('call'), Symbol.for('js/null'), Symbol.for('args')]];

/**
 * Whether `obj` is a procedure (i.e., a function).
 *
 * Similar to [`procedure?` in Racket][rkt:procedurep] and
 * [`functionp` in Common Lisp][cl:functionp].
 *
 * [rkt:procedurep]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28quote._~23~25kernel%29._procedure~3f%29%29
 * [cl:functionp]: http://clhs.lisp.se/Body/f_fnp.htm#functionp
 */
function procedurep_(obj: any): any {
  return obj instanceof Function;
}

procedurep_.lispSource = [Symbol.for('define'), [Symbol.for('procedure?_'), Symbol.for('obj')], [Symbol.for('js/function?'), Symbol.for('obj')]];

/**
 * Whether `obj` is a fexpr, that is, a procedure that
 * does not evaluate its arguments.
 */
function fexprp_(obj: any): any {
  return (obj instanceof Function) && obj.fexpr;
}

fexprp_.lispSource = [Symbol.for('define'), [Symbol.for('fexpr?_'), Symbol.for('obj')], [Symbol.for('and'), [Symbol.for('procedure?'), Symbol.for('obj')], [Symbol.for('get-field'), Symbol.for('fexpr'), Symbol.for('obj')]]];

/**
 * Logical negation.
 *
 * Similar to [`not` in Racket][rkt:not] and
 * [`not` in Common Lisp][cl:not].
 *
 * [rkt:not]: https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._not%29%29
 * [cl:not]: http://clhs.lisp.se/Body/f_not.htm
 */
function not_(x: any): any {
  return !x;
}

not_.lispSource = [Symbol.for('define'), [Symbol.for('not_'), Symbol.for('x')], [Symbol.for('not'), Symbol.for('x')]];

/**
 * Map a procedure over a list.
 *
 * Similar to [`map` in Racket][rkt:map] and
 * [`mapcar` in Common Lisp][cl:mapcar].
 *
 * [rkt:map]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29
 * [cl:mapcar]: http://clhs.lisp.se/Body/f_mapc_.htm#mapcar
 */
function map_(f: any, seq: any): any {
  return seq.map(function (x: any): any {
    return f(x);
  });
}

map_.lispSource = [Symbol.for('define'), [Symbol.for('map_'), Symbol.for('f'), Symbol.for('seq')], [Symbol.for('map'), Symbol.for('f'), Symbol.for('seq')]];

/**
 * Less than operator.
 *
 * Similar to [`<` in Racket][rkt:lt] and [`<` in Common Lisp][cl:lt].
 *
 * [rkt:lt]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3c%29%29
 * [cl:lt]: http://clhs.lisp.se/Body/f_eq_sle.htm#LT
 */
function lt_(...args: any[]): any {
  if (args.length < 2) {
    return true;
  } else {
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      // !(x < y) === (x >= y)
      if (args[i - 1] >= (args as any)[i]) {
        return false;
      }
    }
    return true;
  }
}

lt_.lispSource = [Symbol.for('define'), [Symbol.for('lt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], Symbol.for('#t')], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('>='), [Symbol.for('array-list-nth'), [Symbol.for('-'), Symbol.for('i'), 1], Symbol.for('args')], [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')]]];

/**
 * Less than or equal operator.
 *
 * Similar to [`<=` in Racket][rkt:lte] and [`<=` in Common Lisp][cl:lte].
 *
 * [rkt:lte]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3c~3d%29%29
 * [cl:lte]: http://clhs.lisp.se/Body/f_eq_sle.htm#LTEQ
 */
function lte_(...args: any[]): any {
  if (args.length < 2) {
    return true;
  } else {
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      // !(x <= y) === (x > y)
      if (args[i - 1] > (args as any)[i]) {
        return false;
      }
    }
    return true;
  }
}

lte_.lispSource = [Symbol.for('define'), [Symbol.for('lte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], Symbol.for('#t')], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-nth'), [Symbol.for('-'), Symbol.for('i'), 1], Symbol.for('args')], [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')]]];

/**
 * Greater than operator.
 *
 * Similar to [`>` in Racket][rkt:gt] and [`>` in Common Lisp][cl:gt].
 *
 * [rkt:gt]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3e%29%29
 * [cl:gt]: http://clhs.lisp.se/Body/f_eq_sle.htm#GT
 */
function gt_(...args: any[]): any {
  if (args.length < 2) {
    return true;
  } else {
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      // !(x > y) === (x <= y)
      if (args[i - 1] <= (args as any)[i]) {
        return false;
      }
    }
    return true;
  }
}

gt_.lispSource = [Symbol.for('define'), [Symbol.for('gt_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], Symbol.for('#t')], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('<='), [Symbol.for('array-list-nth'), [Symbol.for('-'), Symbol.for('i'), 1], Symbol.for('args')], [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')]]];

/**
 * Greater than or equal operator.
 *
 * Similar to [`>=` in Racket][rkt:gte] and [`>=` in Common Lisp][cl:gte].
 *
 * [rkt:gte]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3e~3d%29%29
 * [cl:gte]: http://clhs.lisp.se/Body/f_eq_sle.htm#GTEQ
 */
function gte_(...args: any[]): any {
  if (args.length < 2) {
    return true;
  } else {
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      // !(x >= y) === (x < y)
      if (args[i - 1] < (args as any)[i]) {
        return false;
      }
    }
    return true;
  }
}

gte_.lispSource = [Symbol.for('define'), [Symbol.for('gte_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('<'), [Symbol.for('array-list-length'), Symbol.for('args')], 2], Symbol.for('#t')], [Symbol.for('else'), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('when'), [Symbol.for('<'), [Symbol.for('array-list-nth'), [Symbol.for('-'), Symbol.for('i'), 1], Symbol.for('args')], [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]], [Symbol.for('return'), Symbol.for('#f')]]], Symbol.for('#t')]]];

/**
 * Modulo operation.
 *
 * Similar to [`modulo` in Racket][rkt:modulo] and
 * [`mod` in Common Lisp][cl:mod].
 *
 * [rkt:modulo]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._modulo%29%29
 * [cl:mod]: http://clhs.lisp.se/Body/f_mod_r.htm
 */
function modulo_(x: any, y: any): any {
  return x % y;
}

modulo_.lispSource = [Symbol.for('define'), [Symbol.for('modulo_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('modulo'), Symbol.for('x'), Symbol.for('y')]];

/**
 * Addition.
 *
 * Similar to [`+` in Racket][rkt:add] and [`+` in Common Lisp][cl:add].
 *
 * [rkt:add]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2B%29%29
 * [cl:add]: http://clhs.lisp.se/Body/f_pl.htm
 */
function add_(...args: any[]): any {
  let result: any = 0;
  for (let arg of args) {
    result = result + arg;
  }
  return result;
}

add_.lispSource = [Symbol.for('define'), [Symbol.for('add_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), 0]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('args')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('+'), Symbol.for('result'), Symbol.for('arg')]]], Symbol.for('result')]];

/**
 * Return `(+ x 1)`.
 *
 * Similar to [`add1` in Racket][rkt:add1].
 *
 * [rkt:add1]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._add1%29%29
 */
function add1_(x: any): any {
  return x + 1;
}

add1_.lispSource = [Symbol.for('define'), [Symbol.for('add1_'), Symbol.for('x')], [Symbol.for('+'), Symbol.for('x'), 1]];

/**
 * Subtraction.
 *
 * Similar to [`-` in Racket][rkt:sub] and [`-` in Common Lisp][cl:sub].
 *
 * [rkt:sub]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._-%29%29
 * [cl:sub]: http://clhs.lisp.se/Body/f__.htm
 */
function sub_(...args: any[]): any {
  const len: any = args.length;
  if (len === 0) {
    return 0;
  } else if (len === 1) {
    return -args[0];
  } else {
    let result: any = args[0];
    for (let i: any = 1; i < len; i++) {
      result = result - (args as any)[i];
    }
    return result;
  }
}

sub_.lispSource = [Symbol.for('define'), [Symbol.for('sub_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('len'), [Symbol.for('array-list-length'), Symbol.for('args')]]], [Symbol.for('cond'), [[Symbol.for('zero?'), Symbol.for('len')], 0], [[Symbol.for('one?'), Symbol.for('len')], [Symbol.for('-'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('else'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, Symbol.for('len')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('-'), Symbol.for('result'), [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]]]], Symbol.for('result')]]]]];

/**
 * Return `(- x 1)`.
 *
 * Similar to [`sub1` in Racket][rkt:sub1].
 *
 * [rkt:sub1]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._sub1%29%29
 */
function sub1_(x: any): any {
  return x - 1;
}

sub1_.lispSource = [Symbol.for('define'), [Symbol.for('sub1_'), Symbol.for('x')], [Symbol.for('-'), Symbol.for('x'), 1]];

/**
 * Multiplication.
 *
 * Similar to [`*` in Racket][rkt:mul] and [`*` in Common Lisp][cl:mul].
 *
 * [rkt:mul]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2A%29%29
 * [cl:mul]: http://clhs.lisp.se/Body/f_st.htm
 */
function mul_(...args: any[]): any {
  let result: any = 1;
  for (let arg of args) {
    result = result * arg;
  }
  return result;
}

mul_.lispSource = [Symbol.for('define'), [Symbol.for('mul_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('result'), 1]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('args')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('*'), Symbol.for('result'), Symbol.for('arg')]]], Symbol.for('result')]];

/**
 * Division.
 *
 * Similar to [`/` in Racket][rkt:div] and [`/` in Common Lisp][cl:div].
 *
 * [rkt:div]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2F%29%29
 * [cl:div]: http://clhs.lisp.se/Body/f_sl.htm
 */
function div_(...args: any[]): any {
  if (args.length === 1) {
    return 1 / args[0];
  } else {
    let result: any = args[0];
    const _end: any = args.length;
    for (let i: any = 1; i < _end; i++) {
      result = result / (args as any)[i];
    }
    return result;
  }
}

div_.lispSource = [Symbol.for('define'), [Symbol.for('div_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 1], [Symbol.for('/'), 1, [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('else'), [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 1, [Symbol.for('array-list-length'), Symbol.for('args')]]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('/'), Symbol.for('result'), [Symbol.for('array-list-nth'), Symbol.for('i'), Symbol.for('args')]]]], Symbol.for('result')]]]];

/**
 * Whether a value is the number zero.
 *
 * Similar to [`zerop` in Racket][rkt:zerop] and
 * [`zerop` in Common Lisp][cl:zerop].
 *
 * [rkt:zerop]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._zero~3f%29%29
 * [cl:zerop]: http://clhs.lisp.se/Body/f_zerop.htm#zerop
 */
function zerop_(n: any): any {
  return n === 0;
}

zerop_.lispSource = [Symbol.for('define'), [Symbol.for('zero?_'), Symbol.for('n')], [Symbol.for('='), Symbol.for('n'), 0]];

/**
 * Whether a value is the number one.
 */
function onep_(n: any): any {
  return n === 1;
}

onep_.lispSource = [Symbol.for('define'), [Symbol.for('one?_'), Symbol.for('n')], [Symbol.for('='), Symbol.for('n'), 1]];

/**
 * Whether a number is odd.
 *
 * Similar to [`odd?` in Racket][rkt:oddp] and
 * [`oddp` in Common Lisp][rkt:oddp].
 *
 * [rkt:oddp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._odd~3f%29%29
 * [cl:oddp]: http://clhs.lisp.se/Body/f_evenpc.htm#oddp
 */
function oddp_(n: any): any {
  return (n % 2) !== 0;
}

oddp_.lispSource = [Symbol.for('define'), [Symbol.for('odd?_'), Symbol.for('n')], [Symbol.for('not'), [Symbol.for('even?'), Symbol.for('n')]]];

/**
 * Whether a number is even.
 *
 * Similar to [`even?` in Racket][rkt:evenp] and
 * [`evenp` in Common Lisp][rkt:evenp].
 *
 * [rkt:oddp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._even~3f%29%29
 * [cl:oddp]: http://clhs.lisp.se/Body/f_evenpc.htm#evenp
 */
function evenp_(n: any): any {
  return (n % 2) === 0;
}

evenp_.lispSource = [Symbol.for('define'), [Symbol.for('even?_'), Symbol.for('n')], [Symbol.for('zero?'), [Symbol.for('modulo'), Symbol.for('n'), 2]]];

/**
 * Whether a value is truthy.
 */
function truep(x: any): any {
  // TODO: Remove.
  return (x !== undefined) && x && !(Array.isArray(x) && (x.length === 0)) && !((Object.keys(x).length === 0) && (x.constructor === Object)) && true;
}

truep.lispSource = [Symbol.for('define'), [Symbol.for('true?'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('undefined?'), Symbol.for('x')]], Symbol.for('x'), [Symbol.for('not'), [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('x')], 0]]], [Symbol.for('not'), [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), [Symbol.for('js-keys'), Symbol.for('x')]], 0], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('constructor'), Symbol.for('x')], Symbol.for('Object')]]], Symbol.for('#t')]];

/**
 * Whether a value is falsy.
 */
function falsep(x: any): any {
  // TODO: Remove.
  return !truep(x);
}

falsep.lispSource = [Symbol.for('define'), [Symbol.for('false?'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('true?'), Symbol.for('x')]]];

/**
 * The identity function.
 *
 * Similar to [`identity` in Racket][rkt:identity] and
 * [`identity` in Common Lisp][cl:identity].
 *
 * [rkt:identity]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._identity%29%29
 * [cl:identity]: http://clhs.lisp.se/Body/f_identi.htm#identity
 */
function identity_(x: any): any {
  return x;
}

identity_.lispSource = [Symbol.for('define'), [Symbol.for('identity_'), Symbol.for('x')], Symbol.for('x')];

/**
 * Returns a procedure that accepts any arguments and returns `x`.
 *
 * Similar to [`const` in Racket][rkt:const] and
 * [`constantly` in Common Lisp][cl:constantly].
 *
 * [rkt:const]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._const%29%29
 * [cl:constantly]: http://clhs.lisp.se/Body/f_cons_1.htm#constantly
 */
function const_(x: any = undefined): any {
  return function (...args: any[]): any {
    return x;
  };
}

const_.lispSource = [Symbol.for('define'), [Symbol.for('const_'), [Symbol.for('x'), Symbol.for('undefined')]], [Symbol.for('lambda'), Symbol.for('args'), Symbol.for('x')]];

/**
 * Return a tuple of multiple values.
 *
 * Similar to [`values` in Racket][rkt:values] and
 * [`values` in Common Lisp][cl:values].
 *
 * [rkt:values]: https://docs.racket-lang.org/reference/values.html#%28def._%28%28quote._~23~25kernel%29._values%29%29
 * [cl:values]: http://clhs.lisp.se/Body/f_values.htm
 */
function values_(...args: any[]): any {
  // Implementation-wise, `values` does the same as `list`;
  // there is no separate data type for value tuples.
  return args;
}

values_.lispSource = [Symbol.for('define'), [Symbol.for('values_'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')];

/**
 * Whether something is a keyword, i.e., a symbol
 * whose first character is `:`.
 *
 * Similar to [`keyword?` in Racket][rkt:keywordp] and
 * [`keywordp` in Common Lisp][cl:keywordp].
 *
 * [rkt:keywordp]: https://docs.racket-lang.org/reference/keywords.html#%28def._%28%28quote._~23~25kernel%29._keyword~3f%29%29
 * [cl:keywordp]: http://clhs.lisp.se/Body/f_kwdp.htm#keywordp
 */
function keywordp_(obj: any): any {
  return (typeof obj === 'symbol') && (obj.description as string).match(new RegExp('^:'));
}

keywordp_.lispSource = [Symbol.for('define'), [Symbol.for('keyword?_'), Symbol.for('obj')], [Symbol.for('and'), [Symbol.for('symbol?'), Symbol.for('obj')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^:'], [Symbol.for('symbol->string'), Symbol.for('obj')]]]];

/**
 * Whether something is a number.
 *
 * Similar to [`number?` in Racket][rkt:numberp] and
 * [`numberp` in Common Lisp][cl:numberp].
 *
 * [rkt:numberp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._number~3f%29%29
 * [cl:numberp]: http://clhs.lisp.se/Body/f_nump.htm#numberp
 */
function numberp_(obj: any): any {
  return Number.isFinite(obj);
}

numberp_.lispSource = [Symbol.for('define'), [Symbol.for('number?_'), Symbol.for('obj')], [Symbol.for('send'), Symbol.for('Number'), Symbol.for('isFinite'), Symbol.for('obj')]];

/**
 * Whether something is a boolean value.
 *
 * Similar to [`boolean?` in Racket][rkt:booleanp] and
 * [`booleanp` in Emacs Lisp][el:booleanp].
 *
 * [rkt:booleanp]: https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._boolean~3f%29%29
 * [el:booleanp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/nil-and-t.html#index-booleanp
 */
function booleanp_(obj: any): any {
  return typeof obj === 'boolean';
}

booleanp_.lispSource = [Symbol.for('define'), [Symbol.for('boolean?_'), Symbol.for('obj')], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('obj')], 'boolean']];

/**
 * Whether something is the value `undefined`.
 */
function undefinedp_(obj: any): any {
  return obj === undefined;
}

undefinedp_.lispSource = [Symbol.for('define'), [Symbol.for('undefined?_'), Symbol.for('obj')], [Symbol.for('eq?'), Symbol.for('obj'), Symbol.for('undefined')]];

/**
 * Fold up a list left to right.
 *
 * Similar to [`foldl` in Racket][rkt:foldl] and
 * [`reduce` in Common Lisp][cl:reduce].
 *
 * [rkt:foldl]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldl%29%29
 * [cl:reduce]: http://clhs.lisp.se/Body/f_reduce.htm#reduce
 */
function foldl_(f: any, v: any, lst: any): any {
  return lst.reduce(function (acc: any, x: any): any {
    return f(x, acc);
  }, v);
}

foldl_.lispSource = [Symbol.for('define'), [Symbol.for('foldl_'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')]];

/**
 * Fold up a list right to left.
 *
 * Similar to [`foldr` in Racket][rkt:foldr] and
 * [`reduce` in Common Lisp][cl:reduce].
 *
 * [rkt:foldr]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldr%29%29
 * [cl:reduce]: http://clhs.lisp.se/Body/f_reduce.htm#reduce
 */
function foldr_(f: any, v: any, lst: any): any {
  return lst.reduceRight(function (acc: any, x: any): any {
    return f(x, acc);
  }, v);
}

foldr_.lispSource = [Symbol.for('define'), [Symbol.for('foldr_'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('foldr'), Symbol.for('f'), Symbol.for('v'), Symbol.for('lst')]];

/**
 * Whether a list contains a value.
 * Returns a sublist if found, otherwise `#f`.
 * By default, comparison is done with `equal?`.
 *
 * Similar to [`member` in Racket][rkt:member] and
 * [`member` in Common Lisp][cl:member].
 *
 * [rkt:member]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._member%29%29
 * [cl:member]: http://clhs.lisp.se/Body/f_mem_m.htm
 */
function member_(v: any, lst: any, isEqual: any = undefined): any {
  const idx: any = lst.findIndex(isEqual ? (function (x: any): any {
    return isEqual(v, x);
  }) : (function (x: any): any {
    return equalp(v, x);
  }));
  if (idx >= 0) {
    if (idx === 0) {
      return lst;
    } else {
      return lst.slice(idx);
    }
  } else {
    return false;
  }
}

member_.lispSource = [Symbol.for('define'), [Symbol.for('member_'), Symbol.for('v'), Symbol.for('lst'), [Symbol.for('is-equal'), Symbol.for('undefined')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), [Symbol.for('if'), Symbol.for('is-equal'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('v'), Symbol.for('x')]], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('v'), Symbol.for('x')]]], Symbol.for('lst')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('drop'), Symbol.for('lst'), Symbol.for('idx')], Symbol.for('#f')]]];

/**
 * Whether a list contains a value.
 * Like `member`, but always returns a boolean value.
 */
function memberp_(v: any, lst: any, isEqual: any = undefined): any {
  return lst.findIndex(isEqual ? (function (x: any): any {
    return isEqual(v, x);
  }) : (function (x: any): any {
    return equalp(v, x);
  })) >= 0;
}

memberp_.lispSource = [Symbol.for('define'), [Symbol.for('member?_'), Symbol.for('v'), Symbol.for('lst'), [Symbol.for('is-equal'), Symbol.for('undefined')]], [Symbol.for('memf?'), [Symbol.for('if'), Symbol.for('is-equal'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('v'), Symbol.for('x')]], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('v'), Symbol.for('x')]]], Symbol.for('lst')]];

/**
 * Whether a list contains a value.
 * Like `member`, but comparison is done with `eq?`.
 *
 * Similar to [`meq` in Racket][rkt:memq] and
 * [`memq` in Emacs Lisp][el:memq].
 *
 * [rkt:memq]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._memq%29%29
 * [el:memq]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Sets-And-Lists.html#index-memq
 */
function memq_(v: any, lst: any): any {
  const idx: any = lst.findIndex(function (x: any): any {
    return v === x;
  });
  if (idx >= 0) {
    if (idx === 0) {
      return lst;
    } else {
      return lst.slice(idx);
    }
  } else {
    return false;
  }
}

memq_.lispSource = [Symbol.for('define'), [Symbol.for('memq_'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('v'), Symbol.for('x')]], Symbol.for('lst')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('drop'), Symbol.for('lst'), Symbol.for('idx')], Symbol.for('#f')]]];

/**
 * Whether a list contains a value,
 * using `eq?` for comparisons. Like `memq`,
 * but always returns a boolean value.
 */
function memqp_(v: any, lst: any): any {
  // This construct maps neatly onto
  // [`Array.prototype.includes()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes).
  return lst.includes(v);
}

memqp_.lispSource = [Symbol.for('define'), [Symbol.for('memq?_'), Symbol.for('v'), Symbol.for('lst')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('includes'), Symbol.for('v')]];

/**
 * Whether a list contains a value matching a predicate.
 * Applies the predicate `proc` to elements in the list
 * and returns a sublist if found, otherwise `#f`.
 *
 * Similar to [`memf` in Racket][rkt:memf] and
 * [`member-if` in Common Lisp][cl:member-if].
 *
 * [rkt:memf]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._memf%29%29
 * [cl:member-if]: http://clhs.lisp.se/Body/f_mem_m.htm#member-if
 */
function memf_(proc: any, lst: any, notFound: any = false): any {
  const idx: any = lst.findIndex(proc);
  if (idx >= 0) {
    if (idx === 0) {
      return lst;
    } else {
      return lst.slice(idx);
    }
  } else {
    return notFound;
  }
}

memf_.lispSource = [Symbol.for('define'), [Symbol.for('memf_'), Symbol.for('proc'), Symbol.for('lst'), [Symbol.for('not-found'), Symbol.for('#f')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('lst')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('drop'), Symbol.for('lst'), Symbol.for('idx')], Symbol.for('not-found')]]];

/**
 * Whether a list contains a value matching a predicate.
 * Like `memf`, but always returns a boolean value.
 */
function memfp_(proc: any, lst: any): any {
  return lst.findIndex(proc) >= 0;
}

memfp_.lispSource = [Symbol.for('define'), [Symbol.for('memf?_'), Symbol.for('proc'), Symbol.for('lst')], [Symbol.for('>='), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('lst')], 0]];

/**
 * Find a list element matching a predicate.
 *
 * Similar to [`findf` in Racket][rkt:findf] and
 * [`find-if` in Common Lisp][cl:find-if].
 *
 * [rkt:findf]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._findf%29%29
 * [cl:find-if]: http://clhs.lisp.se/Body/f_find_.htm#find-if
 */
function findf_(proc: any, lst: any, notFound: any = false): any {
  const idx: any = lst.findIndex(proc);
  if (idx >= 0) {
    return (lst as any)[idx];
  } else {
    return notFound;
  }
}

findf_.lispSource = [Symbol.for('define'), [Symbol.for('findf_'), Symbol.for('proc'), Symbol.for('lst'), [Symbol.for('not-found'), Symbol.for('#f')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('lst')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], [Symbol.for('array-list-nth'), Symbol.for('idx'), Symbol.for('lst')], Symbol.for('not-found')]]];

/**
 * Find the index of a list element matching a predicate.
 *
 * Similar to [`index-where` in Racket][rkt:index-where] and
 * [`position-if` in Common Lisp][cl:position-if].
 *
 * [rkt:index-where]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-where%29%29
 * [cl:position-if]: http://clhs.lisp.se/Body/f_pos_p.htm#position-if
 */
function findfIndex_(proc: any, seq: any, notFound: any = false): any {
  const idx: any = seq.findIndex(proc);
  if (idx >= 0) {
    return idx;
  } else {
    return notFound;
  }
}

findfIndex_.lispSource = [Symbol.for('define'), [Symbol.for('findf-index_'), Symbol.for('proc'), Symbol.for('seq'), [Symbol.for('not-found'), Symbol.for('#f')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('seq')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], Symbol.for('idx'), Symbol.for('not-found')]]];

/**
 * Find the index of a list element matching a predicate.
 *
 * Similar to [`index-where` in Racket][rkt:index-where] and
 * [`position-if` in Common Lisp][cl:position-if].
 *
 * [rkt:index-where]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-where%29%29
 * [cl:position-if]: http://clhs.lisp.se/Body/f_pos_p.htm#position-if
 */
function indexWhere_(seq: any, proc: any, notFound: any = false): any {
  const idx: any = seq.findIndex(proc);
  if (idx >= 0) {
    return idx;
  } else {
    return notFound;
  }
}

indexWhere_.lispSource = [Symbol.for('define'), [Symbol.for('index-where_'), Symbol.for('seq'), Symbol.for('proc'), [Symbol.for('not-found'), Symbol.for('#f')]], [Symbol.for('let'), [[Symbol.for('idx'), [Symbol.for('js/find-index'), Symbol.for('proc'), Symbol.for('seq')]]], [Symbol.for('if'), [Symbol.for('>='), Symbol.for('idx'), 0], Symbol.for('idx'), Symbol.for('not-found')]]];

/**
 * Find the index of a list element.
 *
 * Similar to [`index-of` in Racket][rkt:index-of] and
 * [`position` in Common Lisp][cl:position].
 *
 * [rkt:index-of]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-of%29%29
 * [cl:position]: http://clhs.lisp.se/Body/f_pos_p.htm#position
 */
function indexOf_(seq: any, v: any, isEqual: any = undefined): any {
  const idx: any = seq.findIndex(isEqual ? (function (x: any): any {
    return isEqual(x, v);
  }) : (function (x: any): any {
    return equalp(x, v);
  }));
  if (idx >= 0) {
    return idx;
  } else {
    return false;
  }
}

indexOf_.lispSource = [Symbol.for('define'), [Symbol.for('index-of_'), Symbol.for('seq'), Symbol.for('v'), [Symbol.for('is-equal'), Symbol.for('undefined')]], [Symbol.for('index-where'), Symbol.for('seq'), [Symbol.for('if'), Symbol.for('is-equal'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('is-equal'), Symbol.for('x'), Symbol.for('v')]], [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('equal?'), Symbol.for('x'), Symbol.for('v')]]]]];

/**
 * Return the intersection of multiple lists.
 *
 * Similar to [`intersection` in Common Lisp][cl:intersection].
 *
 * [cl:intersection]: http://clhs.lisp.se/Body/f_isec_.htm#intersection
 */
function intersection_(...args: any[]): any {
  function intersection2(arr1: any, arr2: any): any {
    let result: any = [];
    for (let element of arr1) {
      if (arr2.includes(element) && !result.includes(element)) {
        result.push(element);
      }
    }
    return result;
  }
  intersection2.lispSource = [Symbol.for('define'), [Symbol.for('intersection2'), Symbol.for('arr1'), Symbol.for('arr2')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr1')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('arr2')], [Symbol.for('not'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], Symbol.for('result')]];
  if (args.length === 0) {
    return [];
  } else if (args.length === 1) {
    return args[0];
  } else {
    return args.slice(1).reduce(function (acc: any, x: any): any {
      return intersection2(acc, x);
    }, args[0]);
  }
}

intersection_.lispSource = [Symbol.for('define'), [Symbol.for('intersection_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), [Symbol.for('intersection2'), Symbol.for('arr1'), Symbol.for('arr2')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr1')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('arr2')], [Symbol.for('not'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], Symbol.for('result')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 0], [Symbol.for('quote'), []]], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('intersection2'), Symbol.for('acc'), Symbol.for('x')]], [Symbol.for('first'), Symbol.for('args')], [Symbol.for('rest'), Symbol.for('args')]]]]];

/**
 * Return the union of multiple lists.
 *
 * Similar to [`union` in Common Lisp][cl:union].
 *
 * [cl:union]: http://clhs.lisp.se/Body/f_unionc.htm#union
 */
function union_(...args: any[]): any {
  function union2(arr1: any, arr2: any): any {
    let result: any = [];
    for (let element of arr1) {
      if (!result.includes(element)) {
        result.push(element);
      }
    }
    for (let element of arr2) {
      if (!result.includes(element)) {
        result.push(element);
      }
    }
    return result;
  }
  union2.lispSource = [Symbol.for('define'), [Symbol.for('union2'), Symbol.for('arr1'), Symbol.for('arr2')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr1')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr2')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], Symbol.for('result')]];
  return args.reduce(function (acc: any, x: any): any {
    return union2(acc, x);
  }, []);
}

union_.lispSource = [Symbol.for('define'), [Symbol.for('union_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), [Symbol.for('union2'), Symbol.for('arr1'), Symbol.for('arr2')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr1')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], [Symbol.for('for'), [[Symbol.for('element'), Symbol.for('arr2')]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('element'), Symbol.for('result')], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('element')]]], Symbol.for('result')]], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('union2'), Symbol.for('acc'), Symbol.for('x')]], [Symbol.for('quote'), []], Symbol.for('args')]];

/**
 * Produce a list of numbers from `start`, inclusive,
 * to `end`, exclusive.
 *
 * Similar to [`range` in Racket][rkt:range].
 *
 * [rkt:range]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._range%29%29
 */
function range_(start: any, end: any = undefined, step: any = undefined): any {
  const startN: any = (end === undefined) ? 0 : start;
  const endN: any = (end === undefined) ? start : end;
  const stepN: any = step || 1;
  let result: any = [];
  for (let i: any = startN; (stepN < 0) ? (i > endN) : (i < endN); i = i + stepN) {
    result.push(i);
  }
  return result;
}

range_.lispSource = [Symbol.for('define'), [Symbol.for('range_'), Symbol.for('start'), [Symbol.for('end'), Symbol.for('undefined')], [Symbol.for('step'), Symbol.for('undefined')]], [Symbol.for('let*'), [[Symbol.for('start-n'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('end'), Symbol.for('undefined')], 0, Symbol.for('start')]], [Symbol.for('end-n'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('end'), Symbol.for('undefined')], Symbol.for('start'), Symbol.for('end')]], [Symbol.for('step-n'), [Symbol.for('or'), Symbol.for('step'), 1]], [Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), Symbol.for('start-n'), Symbol.for('end-n'), Symbol.for('step-n')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('i')]], Symbol.for('result')]];

/**
 * Right-to-left function composition.
 *
 * Similar to [`compose` in Racket][rkt:compose].
 *
 * [rkt:compose]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._compose%29%29
 */
function compose_(...args: any[]): any {
  const functions: any = args.slice(0, -1);
  const lastFunction: any = args[args.length - 1];
  return function (...args: any[]): any {
    const val: any = lastFunction(...args);
    return functions.reduceRight(function (x: any, f: any): any {
      return f(x);
    }, val);
  };
}

compose_.lispSource = [Symbol.for('define'), [Symbol.for('compose_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('functions'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('last-function'), [Symbol.for('array-list-last'), Symbol.for('args')]]], [Symbol.for('lambda'), Symbol.for('args'), [Symbol.for('let'), [[Symbol.for('val'), [Symbol.for('apply'), Symbol.for('last-function'), Symbol.for('args')]]], [Symbol.for('foldr'), [Symbol.for('lambda'), [Symbol.for('f'), Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]], Symbol.for('val'), Symbol.for('functions')]]]]];

/**
 * Left-to-right function composition.
 *
 * Like `compose`, but in the other direction.
 */
function pipe_(...args: any[]): any {
  const functions: any = args.slice(1);
  const firstFunction: any = args[0];
  return function (...args: any[]): any {
    const val: any = firstFunction(...args);
    return functions.reduce(function (x: any, f: any): any {
      return f(x);
    }, val);
  };
}

pipe_.lispSource = [Symbol.for('define'), [Symbol.for('pipe_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('let'), [[Symbol.for('functions'), [Symbol.for('rest'), Symbol.for('args')]], [Symbol.for('first-function'), [Symbol.for('first'), Symbol.for('args')]]], [Symbol.for('lambda'), Symbol.for('args'), [Symbol.for('let'), [[Symbol.for('val'), [Symbol.for('apply'), Symbol.for('first-function'), Symbol.for('args')]]], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('f'), Symbol.for('x')], [Symbol.for('f'), Symbol.for('x')]], Symbol.for('val'), Symbol.for('functions')]]]]];

/**
 * Filter a list by a predicate.
 *
 * Similar to [`filter` in Racket][rkt:filter] and
 * [`remove-if-not` in Common Lisp][cl:remove-if-not].
 *
 * [rkt:filter]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29
 * [cl:remove-if-not]: http://clhs.lisp.se/Body/f_rm_rm.htm#remove-if-not
 */
function filter_(pred: any, lst: any): any {
  return lst.filter(pred);
}

filter_.lispSource = [Symbol.for('define'), [Symbol.for('filter_'), Symbol.for('pred'), Symbol.for('lst')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('filter'), Symbol.for('pred')]];

/**
 * Whether a value is self-evaluating.
 */
function selfEvaluatingP_(x: any): any {
  return (typeof x === 'boolean') || Number.isFinite(x) || (typeof x === 'string') || keywordp(x) || (x === null) || (x === undefined);
}

selfEvaluatingP_.lispSource = [Symbol.for('define'), [Symbol.for('self-evaluating?_'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('boolean?'), Symbol.for('x')], [Symbol.for('number?'), Symbol.for('x')], [Symbol.for('string?'), Symbol.for('x')], [Symbol.for('keyword?'), Symbol.for('x')], [Symbol.for('js/null?'), Symbol.for('x')], [Symbol.for('undefined?'), Symbol.for('x')]]];

/**
 * Write an error message to the console
 * if the assertion is false.
 *
 * Similar to [`assert` in Clojure][clj:assert]
 * and [`assert` in Racket][rkt:assert].
 *
 * [clj:assert]: https://clojuredocs.org/clojure.core/assert
 * [rkt:assert]: https://docs.racket-lang.org/ts-reference/Utilities.html#(def._((lib._typed-racket/base-env/extra-procs..rkt)._assert))
 */
function assert_(x: any, ...args: any[]): any {
  return console.assert(x, ...args);
}

assert_.lispSource = [Symbol.for('define'), [Symbol.for('assert_'), Symbol.for('x'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('console'), Symbol.for('assert'), Symbol.for('x'), Symbol.for('args')]];

/**
 * Output a message to the console.
 *
 * Similar to [`display` in Racket][rkt:display] and
 * [`print` in Common Lisp][cl:print].
 *
 * [rkt:display]: https://docs.racket-lang.org/reference/Writing.html#%28def._%28%28quote._~23~25kernel%29._display%29%29
 * [cl:print]: http://clhs.lisp.se/Body/f_wr_pr.htm#print
 */
function display_(...args: any[]): any {
  return console.log(...args);
}

display_.lispSource = [Symbol.for('define'), [Symbol.for('display_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('console'), Symbol.for('log'), Symbol.for('args')]];

/**
 * Throw an error.
 *
 * Similar to [`error` in Racket][rkt:error] and
 * [`error` in Common Lisp][cl:error].
 *
 * [rkt:error]: https://docs.racket-lang.org/reference/exns.html#%28def._%28%28quote._~23~25kernel%29._error%29%29
 * [cl:error]: http://clhs.lisp.se/Body/f_error.htm
 */
function error_(arg: any = undefined): any {
  throw new Error(arg);
}

error_.lispSource = [Symbol.for('define'), [Symbol.for('error_'), [Symbol.for('arg'), Symbol.for('undefined')]], [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), Symbol.for('arg')]]];

/**
 * Get the type of a value.
 *
 * Similar to [`type-of` in Common Lisp][cl:type-of].
 *
 * [cl:type-of]: http://clhs.lisp.se/Body/f_tp_of.htm#type-of
 */
function typeOf_(x: any): any {
  return typeof x;
}

typeOf_.lispSource = [Symbol.for('define'), [Symbol.for('type-of_'), Symbol.for('x')], [Symbol.for('js/typeof'), Symbol.for('x')]];

/**
 * Whether `obj` is an instance of `cls`.
 *
 * Similar to [`is-a?` in Racket][rkt:is-a-p].
 *
 * [rkt:is-a-p]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._is-a~3f%29%29
 */
function isAP_(obj: any, cls: any): any {
  return obj instanceof cls;
}

isAP_.lispSource = [Symbol.for('define'), [Symbol.for('is-a?_'), Symbol.for('obj'), Symbol.for('cls')], [Symbol.for('js/instanceof?'), Symbol.for('obj'), Symbol.for('cls')]];

export {
  add1_ as add1,
  add_ as _add,
  add_ as add,
  add_ as plus,
  apply_ as apply,
  compose_ as compose,
  display_ as display,
  div_ as _div,
  div_ as div,
  error_ as error,
  falsep as falsep_,
  fexprp_ as fexprp,
  findfIndex_ as findfIndex,
  findf_ as findf,
  foldl_ as foldl,
  foldr_ as foldr,
  funcall_ as funcall,
  gt_ as gt,
  gte_ as gte,
  intersection_ as intersection,
  isAP_ as instanceOf,
  isAP_ as instanceOfP,
  isAP_ as instanceOfP_,
  isAP_ as instanceOf_,
  isAP_ as instanceofp,
  isAP_ as isAP,
  keywordp_ as keywordp,
  lt_ as lt,
  lte_ as lte,
  map_ as map,
  map_ as mapcar,
  memberp_ as memberP,
  memberp_ as memberP_,
  memberp_ as memberp,
  memberp_,
  member_ as member,
  member_ as memq,
  memf_ as memf,
  mul_ as _mul,
  mul_ as mul,
  not_ as not,
  numberp_ as numberp,
  pipe_ as pipe,
  procedurep_ as functionp,
  procedurep_ as procedurep,
  range_ as range,
  sub1_ as sub1,
  sub_ as _sub,
  sub_ as minus,
  sub_ as sub,
  sub_ as subtract,
  truep as truep_,
  typeOf_ as typeOf,
  union_ as union,
  values_ as values,
  zerop_ as zerop,
  add1_,
  add_,
  apply_,
  assert_,
  booleanp_,
  compose_,
  const_,
  display_,
  div_,
  error_,
  evenp_,
  falsep,
  fexprp_,
  filter_,
  findfIndex_,
  findf_,
  foldl_,
  foldr_,
  funcall_,
  gt_,
  gte_,
  identity_,
  indexOf_,
  indexWhere_,
  intersection_,
  isAP_,
  keywordp_,
  lt_,
  lte_,
  map_,
  member_,
  memfp_,
  memf_,
  memqp_,
  memq_,
  modulo_,
  mul_,
  not_,
  numberp_,
  oddp_,
  onep_,
  pipe_,
  procedurep_,
  range_,
  selfEvaluatingP_,
  sub1_,
  sub_,
  truep,
  typeOf_,
  undefinedp_,
  union_,
  values_,
  zerop_
};