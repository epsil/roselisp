// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Macros
 *
 * Macro definitions.
 *
 * ## Description
 *
 * This file provides macro implementations of some special forms.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

import {
  currentEnvironment,
  emptyEnvironment
} from './env';

import {
  eval_
} from './eval';

import {
  flatten
} from './list';

import {
  Rose,
  makeRose
} from './rose';

import {
  countTree,
  mapTree,
  taggedListP
} from './util';

const [cons, take, lastCdr]: any[] = ((): any => {
  function cons_(x: any, y: any): any {
    if (Array.isArray(y)) {
      return [x, ...y];
    } else {
      return [x, Symbol.for('.'), y];
    }
  }
  function take_(lst: any, n: any): any {
    const n1: any = lst.length - n;
    if (n1 === 0) {
      return lst;
    } else {
      return lst.slice(0, -n1);
    }
  }
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
  return [cons_, take_, lastCdr_];
})();

/**
 * Expand a `(define/private ...)` expression.
 */
function definePrivate_(exp: any, env: any): any {
  const body: any = exp.slice(1);
  return [Symbol.for('define'), ...body];
}

definePrivate_.lispSource = [Symbol.for('define'), [Symbol.for('define-private_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

definePrivate_.lispMacro = true;

/**
 * Expand a `(define/public ...)` expression.
 */
function definePublic_(exp: any, env: any): any {
  const body: any = exp.slice(1);
  return [Symbol.for('define'), ...body];
}

definePublic_.lispSource = [Symbol.for('define'), [Symbol.for('define-public_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

definePublic_.lispMacro = true;

/**
 * Expand a `(defclass ...)` expression.
 */
function defclass_(exp: any, env: any): any {
  const body: any = exp.slice(1);
  return [Symbol.for('define-class'), ...body];
}

defclass_.lispSource = [Symbol.for('define'), [Symbol.for('defclass_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define-class'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

defclass_.lispMacro = true;

/**
 * Expand a `(defmacro ...)` expression.
 */
function defmacro_(exp: any, env: any): any {
  const [name, args, ...body]: any[] = exp.slice(1);
  return [Symbol.for('define-macro'), cons(name, args), ...body];
}

defmacro_.lispSource = [Symbol.for('define'), [Symbol.for('defmacro_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name'), Symbol.for('args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define-macro'), [Symbol.for('unquote'), [Symbol.for('cons'), Symbol.for('name'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

defmacro_.lispMacro = true;

/**
 * Expand a `(defun ...)` expression.
 */
function defun_(exp: any, env: any): any {
  const [name, args, ...body]: any[] = exp.slice(1);
  return [Symbol.for('define'), [name, ...args], ...body];
}

defun_.lispSource = [Symbol.for('define'), [Symbol.for('defun_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('name'), Symbol.for('args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('define'), [[Symbol.for('unquote'), Symbol.for('name')], [Symbol.for('unquote-splicing'), Symbol.for('args')]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

defun_.lispMacro = true;

/**
 * Expand a `(begin0 ...)` or `(prog1 ...)` expression.
 */
function begin0_(exp: any, env: any): any {
  const [x, ...xs]: any[] = exp.slice(1);
  if (xs.length === 0) {
    return x;
  } else {
    let result: any = Symbol('begin0-result');
    return [Symbol.for('let'), [[result, x]], ...xs, result];
  }
}

begin0_.lispSource = [Symbol.for('define'), [Symbol.for('begin0_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('xs')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('xs')], 0], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('gensym'), 'begin0-result']], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('result')], [Symbol.for('unquote'), Symbol.for('x')]]], [Symbol.for('unquote-splicing'), Symbol.for('xs')], [Symbol.for('unquote'), Symbol.for('result')]]]]]];

begin0_.lispMacro = true;

/**
 * Expand a `(multiple-values-bind ...)` expression.
 */
function multipleValueBind_(exp: any, env: any): any {
  const [bindings, expression, ...body]: any[] = exp.slice(1);
  return [Symbol.for('let-values'), [[bindings, expression]], ...body];
}

multipleValueBind_.lispSource = [Symbol.for('define'), [Symbol.for('multiple-value-bind_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('bindings'), Symbol.for('expression'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('let-values'), [[[Symbol.for('unquote'), Symbol.for('bindings')], [Symbol.for('unquote'), Symbol.for('expression')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

multipleValueBind_.lispMacro = true;

/**
 * Expand a `(rkt/new ...)' expression.
 */
function rktNew_(exp: any, env: any): any {
  const [constructor, ...args]: any[] = exp.slice(1);
  // We are not able to do much here other than to rewrite the
  // expression to a `(make-object ...)` expression. JavaScript lacks
  // support for creating a new object on the basis of by-name
  // initialization arguments; it only supports by-position
  // initialization arguments, which are passed to the constructor.
  return [Symbol.for('make-object'), constructor, ...args.map(function (x: any): any {
    return x[1];
  })];
}

rktNew_.lispSource = [Symbol.for('define'), [Symbol.for('rkt-new_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('constructor'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('make-object'), [Symbol.for('unquote'), Symbol.for('constructor')], [Symbol.for('unquote-splicing'), [Symbol.for('map'), Symbol.for('array-list-second'), Symbol.for('args')]]]]];

rktNew_.lispMacro = true;

/**
 * Expand an `(if ...)` expression.
 */
function if_(exp: any, env: any): any {
  const [condition, thenClause, ...elseClauses]: any[] = exp.slice(1);
  return [Symbol.for('cond'), [condition, thenClause], ...((elseClauses.length > 0) ? [[Symbol.for('else'), ...elseClauses]] : [])];
}

if_.lispSource = [Symbol.for('define'), [Symbol.for('if_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('condition'), Symbol.for('then-clause'), Symbol.for('.'), Symbol.for('else-clauses')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('cond'), [[Symbol.for('unquote'), Symbol.for('condition')], [Symbol.for('unquote'), Symbol.for('then-clause')]], [Symbol.for('unquote-splicing'), [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('else-clauses')], 0], [Symbol.for('quasiquote'), [[Symbol.for('else'), [Symbol.for('unquote-splicing'), Symbol.for('else-clauses')]]]]], [Symbol.for('else'), [Symbol.for('quote'), []]]]]]]];

if_.lispMacro = true;

/**
 * Expand a `(when ...)` expression.
 */
function when_(exp: any, env: any): any {
  const [condition, ...body]: any[] = exp.slice(1);
  return [Symbol.for('if'), condition, [Symbol.for('begin'), ...body]];
}

when_.lispSource = [Symbol.for('define'), [Symbol.for('when_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('condition'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('if'), [Symbol.for('unquote'), Symbol.for('condition')], [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]];

when_.lispMacro = true;

/**
 * Expand an `(unless ...)` expression.
 */
function unless_(exp: any, env: any): any {
  const [condition, ...body]: any[] = exp.slice(1);
  return [Symbol.for('if'), [Symbol.for('not'), condition], [Symbol.for('begin'), ...body]];
}

unless_.lispSource = [Symbol.for('define'), [Symbol.for('unless_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('condition'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('if'), [Symbol.for('not'), [Symbol.for('unquote'), Symbol.for('condition')]], [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]]]];

unless_.lispMacro = true;

/**
 * Expand an `(as~> ...)` expression.
 *
 * Similar to the [`as->` macro][clj:thread-as] in Clojure.
 *
 * [clj:thread-as]: https://clojuredocs.org/clojure.core/as-%3E
 */
function threadAs_(exp: any, env: any): any {
  let [val, sym, ...forms]: any[] = exp.slice(1);
  // This macro goes to some lengths to avoid introducing a `let`
  // variable unless it is absolutely necessary. In many cases, the
  // forms can simply be chained together, using `sym` as the
  // insertion point. A variable is needed only if `sym` occurs more
  // than once in the same form.

  // Whether a `let` variable has been defined.
  let isLet: any = false;
  // Reducer function.
  function f(form: any, exp: any): any {
    if (isLet) {
      // If we are in the process of creating a `let` expression,
      // simply append a `set!` expression to it.
      return [...exp, [Symbol.for('set!'), sym, form]];
    } else {
      // Otherwise, count the occurrences of `sym` in the form
      // in order to determine what to do.
      const n: any = countTree(function (el: any): any {
        return el === sym;
      }, form);
      if (n === 0) {
        // If `sym` occurs zero times in the form, create a
        // `begin` expression to chain things togethr.
        if (taggedListP(exp, Symbol.for('begin'))) {
          // If chaining two `begin` expressions together,
          // simply append the latter to the former.
          return [...exp, form];
        } else {
          return [Symbol.for('begin'), exp, form];
        }
      } else if (n === 1) {
        // If `sym` occurs exactly once in the form, chain it together
        // with the preceding expression, using `sym` as the insertion
        // point.
        return mapTree(function (x: any): any {
          if (x === sym) {
            return exp;
          } else {
            return x;
          }
        }, form);
      } else {
        // If `sym` occurs more than once in the form, create a
        // `let` expression with `sym` as a variable.
        isLet = true;
        return [Symbol.for('let'), [[sym, exp]], [Symbol.for('set!'), sym, form]];
      }
    }
  }
  f.lispSource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('form'), Symbol.for('exp')], [Symbol.for('cond'), [Symbol.for('is-let'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('exp')], [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('form')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('n'), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('el')], [Symbol.for('eq?'), Symbol.for('el'), Symbol.for('sym')]], Symbol.for('form')]], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('begin')]], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('form')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('form')]]]]]], [[Symbol.for('='), Symbol.for('n'), 1], [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('sym')], Symbol.for('exp'), Symbol.for('x')]], Symbol.for('form')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('is-let'), Symbol.for('#t')], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('exp')]]], [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('form')]]]]]]]]];
  // Fold up `forms` left-to-right.
  let result: any = forms.reduce(function (acc: any, x: any): any {
    return f(x, acc);
  }, val);
  // If a `let` expression was indeed created, add `sym` as
  // the final expression.
  if (isLet) {
    result = [...result, sym];
  }
  return result;
}

threadAs_.lispSource = [Symbol.for('define'), [Symbol.for('thread-as_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('val'), Symbol.for('sym'), Symbol.for('.'), Symbol.for('forms')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('is-let'), Symbol.for('#f')], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('form'), Symbol.for('exp')], [Symbol.for('cond'), [Symbol.for('is-let'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('exp')], [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('form')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('n'), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('el')], [Symbol.for('eq?'), Symbol.for('el'), Symbol.for('sym')]], Symbol.for('form')]], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('exp'), [Symbol.for('quote'), Symbol.for('begin')]], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('form')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('unquote'), Symbol.for('exp')], [Symbol.for('unquote'), Symbol.for('form')]]]]]], [[Symbol.for('='), Symbol.for('n'), 1], [Symbol.for('map-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('sym')], Symbol.for('exp'), Symbol.for('x')]], Symbol.for('form')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('is-let'), Symbol.for('#t')], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('exp')]]], [Symbol.for('set!'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('form')]]]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('val'), Symbol.for('forms')]], [Symbol.for('when'), Symbol.for('is-let'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('result')], [Symbol.for('unquote'), Symbol.for('sym')]]]]], Symbol.for('result')];

threadAs_.lispMacro = true;

/**
 * Evaluate a `(~> ...)` expression. Based on the
 * [`->` macro][clj:thread-first] in Clojure (also known as
 * the "`thread-first` macro").
 *
 * [clj:thread-first]: https://clojuredocs.org/clojure.core/-%3E
 */
function threadFirst_(exp: any, env: any): any {
  let [x, ...forms]: any[] = exp.slice(1);
  let holeMarker: any = Symbol.for('_');
  if ((forms.length > 1) && (forms[0] === Symbol.for(':hole-marker'))) {
    holeMarker = forms[1];
    forms = forms.slice(2);
  }
  function f(val: any, acc: any): any {
    if (typeof val === 'symbol') {
      return [...acc, [val, Symbol.for('_')]];
    } else if (countTree(function (x: any): any {
      return x === holeMarker;
    }, val) === 0) {
      return [...acc, [val[0], holeMarker, ...val.slice(1)]];
    } else {
      return [...acc, val];
    }
  }
  f.lispSource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('val'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('val')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), Symbol.for('val')], Symbol.for('_')]]]], [[Symbol.for('='), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')]], Symbol.for('val')], 0], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), [Symbol.for('array-list-first'), Symbol.for('val')]], [Symbol.for('unquote'), Symbol.for('hole-marker')], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('val')]]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [Symbol.for('unquote'), Symbol.for('val')]]]]]];
  const asExp: any = [Symbol.for('as~>'), x, holeMarker];
  return forms.reduce(function (acc: any, x: any): any {
    return f(x, acc);
  }, asExp);
}

threadFirst_.lispSource = [Symbol.for('define'), [Symbol.for('thread-first_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('forms')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('hole-marker'), [Symbol.for('quote'), Symbol.for('_')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('forms')], 1], [Symbol.for('eq?'), [Symbol.for('array-list-first'), Symbol.for('forms')], [Symbol.for('quote'), Symbol.for(':hole-marker')]]], [Symbol.for('set!'), Symbol.for('hole-marker'), [Symbol.for('array-list-second'), Symbol.for('forms')]], [Symbol.for('set!'), Symbol.for('forms'), [Symbol.for('drop'), Symbol.for('forms'), 2]]], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('val'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('val')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), Symbol.for('val')], Symbol.for('_')]]]], [[Symbol.for('='), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')]], Symbol.for('val')], 0], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), [Symbol.for('array-list-first'), Symbol.for('val')]], [Symbol.for('unquote'), Symbol.for('hole-marker')], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('val')]]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [Symbol.for('unquote'), Symbol.for('val')]]]]]], [Symbol.for('define'), Symbol.for('as-exp'), [Symbol.for('quasiquote'), [Symbol.for('as~>'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('hole-marker')]]]], [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('as-exp'), Symbol.for('forms')]];

threadFirst_.lispMacro = true;

/**
 * Evaluate a `(~>> ...)` expression. Based on the
 * [`->>` macro][clj:thread-last] in Clojure (also known as
 * the "`thread-last` macro").
 *
 * [clj:thread-last]: https://clojuredocs.org/clojure.core/-%3E%3E
 */
function threadLast_(exp: any, env: any): any {
  let [x, ...forms]: any[] = exp.slice(1);
  let holeMarker: any = Symbol.for('_');
  if ((forms.length > 1) && (forms[0] === Symbol.for(':hole-marker'))) {
    holeMarker = forms[1];
    forms = forms.slice(2);
  }
  function f(val: any, acc: any): any {
    if (typeof val === 'symbol') {
      return [...acc, [val, Symbol.for('_')]];
    } else if (countTree(function (x: any): any {
      return x === holeMarker;
    }, val) === 0) {
      return [...acc, [...val, holeMarker]];
    } else {
      return [...acc, val];
    }
  }
  f.lispSource = [Symbol.for('define'), [Symbol.for('f'), Symbol.for('val'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('val')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), Symbol.for('val')], Symbol.for('_')]]]], [[Symbol.for('='), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')]], Symbol.for('val')], 0], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote-splicing'), Symbol.for('val')], [Symbol.for('unquote'), Symbol.for('hole-marker')]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [Symbol.for('unquote'), Symbol.for('val')]]]]]];
  const asExp: any = [Symbol.for('as~>'), x, holeMarker];
  return forms.reduce(function (acc: any, x: any): any {
    return f(x, acc);
  }, asExp);
}

threadLast_.lispSource = [Symbol.for('define'), [Symbol.for('thread-last_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('forms')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('hole-marker'), [Symbol.for('quote'), Symbol.for('_')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('forms')], 1], [Symbol.for('eq?'), [Symbol.for('array-list-first'), Symbol.for('forms')], [Symbol.for('quote'), Symbol.for(':hole-marker')]]], [Symbol.for('set!'), Symbol.for('hole-marker'), [Symbol.for('array-list-second'), Symbol.for('forms')]], [Symbol.for('set!'), Symbol.for('forms'), [Symbol.for('drop'), Symbol.for('forms'), 2]]], [Symbol.for('define'), [Symbol.for('f'), Symbol.for('val'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('val')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote'), Symbol.for('val')], Symbol.for('_')]]]], [[Symbol.for('='), [Symbol.for('count-tree'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('hole-marker')]], Symbol.for('val')], 0], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [[Symbol.for('unquote-splicing'), Symbol.for('val')], [Symbol.for('unquote'), Symbol.for('hole-marker')]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('acc')], [Symbol.for('unquote'), Symbol.for('val')]]]]]], [Symbol.for('define'), Symbol.for('as-exp'), [Symbol.for('quasiquote'), [Symbol.for('as~>'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('hole-marker')]]]], [Symbol.for('foldl'), Symbol.for('f'), Symbol.for('as-exp'), Symbol.for('forms')]];

threadLast_.lispMacro = true;

/**
 * Expand an `(unwind-protect ...)` expression.
 */
function unwindProtect_(exp: any, env: any): any {
  const [bodyForm, ...unwindForms]: any[] = exp.slice(1);
  return [Symbol.for('try'), bodyForm, [Symbol.for('finally'), ...unwindForms]];
}

unwindProtect_.lispSource = [Symbol.for('define'), [Symbol.for('unwind-protect_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('body-form'), Symbol.for('.'), Symbol.for('unwind-forms')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('try'), [Symbol.for('unquote'), Symbol.for('body-form')], [Symbol.for('finally'), [Symbol.for('unquote-splicing'), Symbol.for('unwind-forms')]]]]];

unwindProtect_.lispMacro = true;

/**
 * Expand a `(do ...)` expression.
 */
function do_(exp: any, env: any): any {
  const [bindings, tests, ...body]: any[] = exp.slice(1);
  if (bindings.length === 0) {
    // For expressions with no bindings, we wrap
    // the expansion in `begin`.
    let result: any = [Symbol.for('begin'), [Symbol.for('js/while'), [Symbol.for('not'), tests[0]], ...body], ...tests.slice(1)];
    // If there is no finishing expression,
    // the code can be simplified further.
    if (result.length === 2) {
      result = result[1];
    }
    return result;
  } else {
    // For expressions with bindings, we wrap
    // the expansion in `let`.
    const letBindings: any = [];
    const setters: any = [];
    for (let binding of bindings) {
      letBindings.push(take(binding, 2));
      if (binding.length === 3) {
        setters.push([Symbol.for('set!'), binding[0], binding[2]]);
      }
    }
    let result: any = [Symbol.for('let'), letBindings, [Symbol.for('js/while'), [Symbol.for('not'), tests[0]], ...body, ...setters], ...tests.slice(1)];
    return result;
  }
}

do_.lispSource = [Symbol.for('define'), [Symbol.for('do_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('bindings'), Symbol.for('tests'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('bindings')], 0], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('begin'), [Symbol.for('js/while'), [Symbol.for('not'), [Symbol.for('unquote'), [Symbol.for('array-list-first'), Symbol.for('tests')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('tests'), 1]]]]], [Symbol.for('when'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('result')], 2], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('array-list-second'), Symbol.for('result')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('let-bindings'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('setters'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('binding'), Symbol.for('bindings')]], [Symbol.for('push-right!'), Symbol.for('let-bindings'), [Symbol.for('take'), Symbol.for('binding'), 2]], [Symbol.for('when'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('binding')], 3], [Symbol.for('push-right!'), Symbol.for('setters'), [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), [Symbol.for('array-list-first'), Symbol.for('binding')]], [Symbol.for('unquote'), [Symbol.for('array-list-third'), Symbol.for('binding')]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('let'), [Symbol.for('unquote'), Symbol.for('let-bindings')], [Symbol.for('js/while'), [Symbol.for('not'), [Symbol.for('unquote'), [Symbol.for('array-list-first'), Symbol.for('tests')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')], [Symbol.for('unquote-splicing'), Symbol.for('setters')]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('tests'), 1]]]]], Symbol.for('result')]]];

do_.lispMacro = true;

/**
 * Expand a `(while ...)` expression.
 */
function while_(exp: any, env: any): any {
  const [test, ...body]: any[] = exp.slice(1);
  return [Symbol.for('do'), [], [[Symbol.for('not'), test]], ...body];
}

while_.lispSource = [Symbol.for('define'), [Symbol.for('while_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('test'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('do'), [], [[Symbol.for('not'), [Symbol.for('unquote'), Symbol.for('test')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

while_.lispMacro = true;

/**
 * Expand a `(js/for ...)` expression.
 */
function jsFor_(exp: any, env: any): any {
  const [args, ...body]: any[] = exp.slice(1);
  const inits: any = [];
  const tests: any = [];
  for (let arg of args) {
    let init: any = arg[0];
    const test: any = arg[1];
    let update: any = arg[2];
    if (taggedListP(init, Symbol.for('define'))) {
      init = init.slice(1);
    }
    if (taggedListP(update, Symbol.for('set!'))) {
      update = init[2];
    }
    inits.push([...init, update]);
    tests.push(test);
  }
  const testExp: any = (tests.length === 1) ? tests[0] : [Symbol.for('and'), ...tests];
  return [Symbol.for('do'), inits, [[Symbol.for('not'), testExp]], ...body];
}

jsFor_.lispSource = [Symbol.for('define'), [Symbol.for('js/for_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('inits'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('tests'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('arg'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('array-list-first'), Symbol.for('arg')]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('array-list-second'), Symbol.for('arg')]], [Symbol.for('define'), Symbol.for('update'), [Symbol.for('array-list-third'), Symbol.for('arg')]], [Symbol.for('when'), [Symbol.for('tagged-list?'), Symbol.for('init'), [Symbol.for('quote'), Symbol.for('define')]], [Symbol.for('set!'), Symbol.for('init'), [Symbol.for('drop'), Symbol.for('init'), 1]]], [Symbol.for('when'), [Symbol.for('tagged-list?'), Symbol.for('update'), [Symbol.for('quote'), Symbol.for('set!')]], [Symbol.for('set!'), Symbol.for('update'), [Symbol.for('array-list-third'), Symbol.for('init')]]], [Symbol.for('push-right!'), Symbol.for('inits'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('init')], [Symbol.for('unquote'), Symbol.for('update')]]]], [Symbol.for('push-right!'), Symbol.for('tests'), Symbol.for('test')]], [Symbol.for('define'), Symbol.for('test-exp'), [Symbol.for('if'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('tests')], 1], [Symbol.for('array-list-first'), Symbol.for('tests')], [Symbol.for('quasiquote'), [Symbol.for('and'), [Symbol.for('unquote-splicing'), Symbol.for('tests')]]]]], [Symbol.for('quasiquote'), [Symbol.for('do'), [Symbol.for('unquote'), Symbol.for('inits')], [[Symbol.for('not'), [Symbol.for('unquote'), Symbol.for('test-exp')]]], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

jsFor_.lispMacro = true;

/**
 * Expand a `(js/for-in ...)` expression.
 */
function jsForIn_(exp: any, env: any): any {
  const [args, ...body]: any[] = exp.slice(1);
  const bindings: any = args.map(function (x: any): any {
    let left: any = x[0];
    const right: any = x[1];
    return [left, [Symbol.for('js-keys'), right]];
  });
  return [Symbol.for('js/for-of'), bindings, ...body];
}

jsForIn_.lispSource = [Symbol.for('define'), [Symbol.for('js/for-in_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('array-list-first'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('array-list-second'), Symbol.for('x')]], [Symbol.for('list'), Symbol.for('left'), [Symbol.for('quasiquote'), [Symbol.for('js-keys'), [Symbol.for('unquote'), Symbol.for('right')]]]]], Symbol.for('args')]], [Symbol.for('quasiquote'), [Symbol.for('js/for-of'), [Symbol.for('unquote'), Symbol.for('bindings')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

jsForIn_.lispMacro = true;

/**
 * Expand a `(js/for-of ...)` expression.
 */
function jsForOf_(exp: any, env: any): any {
  const [args, ...body]: any[] = exp.slice(1);
  const bindings: any = args.map(function (x: any): any {
    let left: any = x[0];
    const right: any = x[1];
    if (taggedListP(left, Symbol.for('define'))) {
      left = left[1];
    }
    return [left, right];
  });
  return [Symbol.for('for'), bindings, ...body];
}

jsForOf_.lispSource = [Symbol.for('define'), [Symbol.for('js/for-of_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('args'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('bindings'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('array-list-first'), Symbol.for('x')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('array-list-second'), Symbol.for('x')]], [Symbol.for('when'), [Symbol.for('tagged-list?'), Symbol.for('left'), [Symbol.for('quote'), Symbol.for('define')]], [Symbol.for('set!'), Symbol.for('left'), [Symbol.for('array-list-second'), Symbol.for('left')]]], [Symbol.for('list'), Symbol.for('left'), Symbol.for('right')]], Symbol.for('args')]], [Symbol.for('quasiquote'), [Symbol.for('for'), [Symbol.for('unquote'), Symbol.for('bindings')], [Symbol.for('unquote-splicing'), Symbol.for('body')]]]];

jsForOf_.lispMacro = true;

/**
 * Expand a `(field-bound? ...)` expression.
 */
function fieldBoundP_(exp: any, env: any): any {
  const [id, obj]: any[] = exp.slice(1);
  if (typeof obj === 'symbol') {
    return [Symbol.for('and'), obj, [Symbol.for('js/in'), id.description as string, obj]];
  } else {
    const objSym: any = Symbol('obj');
    return [Symbol.for('let'), [[objSym, obj]], [Symbol.for('and'), objSym, [Symbol.for('js/in'), id.description as string, objSym]]];
  }
}

fieldBoundP_.lispSource = [Symbol.for('define'), [Symbol.for('field-bound?_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('id'), Symbol.for('obj')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('cond'), [[Symbol.for('symbol?'), Symbol.for('obj')], [Symbol.for('quasiquote'), [Symbol.for('and'), [Symbol.for('unquote'), Symbol.for('obj')], [Symbol.for('js/in'), [Symbol.for('unquote'), [Symbol.for('symbol->string'), Symbol.for('id')]], [Symbol.for('unquote'), Symbol.for('obj')]]]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('obj-sym'), [Symbol.for('gensym'), 'obj']], [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('obj-sym')], [Symbol.for('unquote'), Symbol.for('obj')]]], [Symbol.for('and'), [Symbol.for('unquote'), Symbol.for('obj-sym')], [Symbol.for('js/in'), [Symbol.for('unquote'), [Symbol.for('symbol->string'), Symbol.for('id')]], [Symbol.for('unquote'), Symbol.for('obj-sym')]]]]]]]];

fieldBoundP_.lispMacro = true;

/**
 * Expand a `(case ...)` expression.
 */
function case_(exp: any, env: any): any {
  const [val, ...clauses]: any[] = exp.slice(1);
  let hasComplexClauses: any = false;
  function isSimpleValue(x: any): any {
    return (typeof x === 'boolean') || Number.isFinite(x) || (typeof x === 'symbol') || (typeof x === 'string');
  }
  isSimpleValue.lispSource = [Symbol.for('define'), [Symbol.for('is-simple-value'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('boolean?'), Symbol.for('x')], [Symbol.for('number?'), Symbol.for('x')], [Symbol.for('symbol?'), Symbol.for('x')], [Symbol.for('string?'), Symbol.for('x')]]];
  function isComplexValue(x: any): any {
    return !isSimpleValue(x);
  }
  isComplexValue.lispSource = [Symbol.for('define'), [Symbol.for('is-complex-value'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('is-simple-value'), Symbol.for('x')]]];
  for (let x of clauses) {
    if ((x[0] !== Symbol.for('else')) && (x[0].findIndex(isComplexValue) >= 0)) {
      hasComplexClauses = true;
      break;
    }
  }
  if (hasComplexClauses) {
    // Complex case: there is one or more regular clauses that contain
    // patterns that must be matched against with `equal?`, not `eq? `.
    const isComplexVal: any = typeof val !== 'symbol';
    const valueVar: any = isComplexVal ? Symbol('_value') : val;
    const condClauses: any = clauses.map(function (x: any): any {
      if (x[0] === Symbol.for('else')) {
        return x;
      } else {
        return [[Symbol.for('member?'), valueVar, [Symbol.for('quote'), x[0]], Symbol.for('equal?')], ...x.slice(1)];
      }
    });
    let result: any = [Symbol.for('cond'), ...condClauses];
    // If `val` is a complex expression, we get a
    // `(let ... (cond ...))` form.
    if (isComplexVal) {
      result = [Symbol.for('let'), [[valueVar, val]], result];
    }
    return result;
  } else {
    // Simple case: all patterns can be matched against with `eq?`,
    // so translate the entire form to a `case/eq` form.
    return [Symbol.for('case/eq'), val, ...clauses];
  }
}

case_.lispSource = [Symbol.for('define'), [Symbol.for('case_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('val'), Symbol.for('.'), Symbol.for('clauses')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('has-complex-clauses'), Symbol.for('#f')], [Symbol.for('define'), [Symbol.for('is-simple-value'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('boolean?'), Symbol.for('x')], [Symbol.for('number?'), Symbol.for('x')], [Symbol.for('symbol?'), Symbol.for('x')], [Symbol.for('string?'), Symbol.for('x')]]], [Symbol.for('define'), [Symbol.for('is-complex-value'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('is-simple-value'), Symbol.for('x')]]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('clauses')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('array-list-first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]]], [Symbol.for('memf?'), Symbol.for('is-complex-value'), [Symbol.for('array-list-first'), Symbol.for('x')]]], [Symbol.for('set!'), Symbol.for('has-complex-clauses'), Symbol.for('#t')], [Symbol.for('break')]]], [Symbol.for('cond'), [Symbol.for('has-complex-clauses'), [Symbol.for('define'), Symbol.for('is-complex-val'), [Symbol.for('not'), [Symbol.for('symbol?'), Symbol.for('val')]]], [Symbol.for('define'), Symbol.for('value-var'), [Symbol.for('if'), Symbol.for('is-complex-val'), [Symbol.for('gensym'), '_value'], Symbol.for('val')]], [Symbol.for('define'), Symbol.for('cond-clauses'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('array-list-first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('member?'), [Symbol.for('unquote'), Symbol.for('value-var')], [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('array-list-first'), Symbol.for('x')]]], Symbol.for('equal?')], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('x')]]]]]]], Symbol.for('clauses')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('cond'), [Symbol.for('unquote-splicing'), Symbol.for('cond-clauses')]]]], [Symbol.for('when'), Symbol.for('is-complex-val'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('value-var')], [Symbol.for('unquote'), Symbol.for('val')]]], [Symbol.for('unquote'), Symbol.for('result')]]]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('case/eq'), [Symbol.for('unquote'), Symbol.for('val')], [Symbol.for('unquote-splicing'), Symbol.for('clauses')]]]]]];

case_.lispMacro = true;

/**
 * Expand a `(case/eq ...)` expression.
 */
function caseEq_(exp: any, env: any): any {
  const [val, ...clauses]: any[] = exp.slice(1);
  let hasComplexClauses: any = false;
  for (let x of clauses) {
    if ((x[0] !== Symbol.for('else')) && (x[0].length > 1)) {
      hasComplexClauses = true;
      break;
    }
  }
  if (hasComplexClauses) {
    // Complex case: there is one or more regular clauses that contain
    // multiple patterns. This is translatable to a `(cond ...)` form
    // that performs pattern matching.
    const isComplexVal: any = typeof val !== 'symbol';
    const valueVar: any = isComplexVal ? Symbol('_value') : val;
    const condClauses: any = clauses.map(function (x: any): any {
      if (x[0] === Symbol.for('else')) {
        return x;
      } else {
        return [[Symbol.for('member?'), valueVar, [Symbol.for('quote'), x[0]]], ...x.slice(1)];
      }
    });
    let result: any = [Symbol.for('cond'), ...condClauses];
    // If `val` is a complex expression, we get a
    // `(let ... (cond ...))` form.
    if (isComplexVal) {
      result = [Symbol.for('let'), [[valueVar, val]], result];
    }
    return result;
  } else {
    // Simple case: each regular clause contains exactly one pattern.
    // This is translatable to a `(js/swith ...)` form.
    const switchClauses: any = clauses.map(function (x: any): any {
      if (x[0] === Symbol.for('else')) {
        return [Symbol.for('default'), ...x.slice(1)];
      } else {
        return [Symbol.for('case'), [Symbol.for('quote'), x[0][0]], ...x.slice(1), [Symbol.for('break')]];
      }
    });
    return [Symbol.for('js/switch'), val, ...switchClauses];
  }
}

caseEq_.lispSource = [Symbol.for('define'), [Symbol.for('case-eq_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('val'), Symbol.for('.'), Symbol.for('clauses')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('has-complex-clauses'), Symbol.for('#f')], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('clauses')]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('not'), [Symbol.for('eq?'), [Symbol.for('array-list-first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]]], [Symbol.for('>'), [Symbol.for('array-list-length'), [Symbol.for('array-list-first'), Symbol.for('x')]], 1]], [Symbol.for('set!'), Symbol.for('has-complex-clauses'), Symbol.for('#t')], [Symbol.for('break')]]], [Symbol.for('cond'), [Symbol.for('has-complex-clauses'), [Symbol.for('define'), Symbol.for('is-complex-val'), [Symbol.for('not'), [Symbol.for('symbol?'), Symbol.for('val')]]], [Symbol.for('define'), Symbol.for('value-var'), [Symbol.for('if'), Symbol.for('is-complex-val'), [Symbol.for('gensym'), '_value'], Symbol.for('val')]], [Symbol.for('define'), Symbol.for('cond-clauses'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('array-list-first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]], Symbol.for('x')], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('member?'), [Symbol.for('unquote'), Symbol.for('value-var')], [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('array-list-first'), Symbol.for('x')]]]], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('x')]]]]]]], Symbol.for('clauses')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('cond'), [Symbol.for('unquote-splicing'), Symbol.for('cond-clauses')]]]], [Symbol.for('when'), Symbol.for('is-complex-val'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('quasiquote'), [Symbol.for('let'), [[[Symbol.for('unquote'), Symbol.for('value-var')], [Symbol.for('unquote'), Symbol.for('val')]]], [Symbol.for('unquote'), Symbol.for('result')]]]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('switch-clauses'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cond'), [[Symbol.for('eq?'), [Symbol.for('array-list-first'), Symbol.for('x')], [Symbol.for('quote'), Symbol.for('else')]], [Symbol.for('quasiquote'), [Symbol.for('default'), [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('x')]]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [Symbol.for('case'), [Symbol.for('quote'), [Symbol.for('unquote'), [Symbol.for('array-list-first'), [Symbol.for('array-list-first'), Symbol.for('x')]]]], [Symbol.for('unquote-splicing'), [Symbol.for('rest'), Symbol.for('x')]], [Symbol.for('break')]]]]]], Symbol.for('clauses')]], [Symbol.for('quasiquote'), [Symbol.for('js/switch'), [Symbol.for('unquote'), Symbol.for('val')], [Symbol.for('unquote-splicing'), Symbol.for('switch-clauses')]]]]]];

caseEq_.lispMacro = true;

/**
 * Expand a `(let-env ...)` expression.
 */
function letEnv_(exp: any, env: any): any {
  const [x, ...body]: any[] = exp.slice(1);
  return [Symbol.for('scm/eval'), [Symbol.for('quote'), [Symbol.for('begin'), ...body]], [Symbol.for('extend-environment'), x, [Symbol.for('current-environment')]]];
}

letEnv_.lispSource = [Symbol.for('define'), [Symbol.for('let-env_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('body')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('scm/eval'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('unquote-splicing'), Symbol.for('body')]]], [Symbol.for('extend-environment'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('current-environment')]]]]];

letEnv_.lispMacro = true;

/**
 * Expand a `(set ...)` expression.
 */
function set_(exp: any, env: any): any {
  const [sym, val]: any[] = exp.slice(1);
  return [Symbol.for('set!'), sym[1], val];
}

set_.lispSource = [Symbol.for('define'), [Symbol.for('set_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), [Symbol.for('sym'), Symbol.for('val')], [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('set!'), [Symbol.for('unquote'), [Symbol.for('array-list-second'), Symbol.for('sym')]], [Symbol.for('unquote'), Symbol.for('val')]]]];

set_.lispMacro = true;

/**
 * Expand a `(new/apply ...)` expression.
 */
function newApply_(exp: any, env: any): any {
  const args: any = exp.slice(1);
  return [Symbol.for('apply'), Symbol.for('new'), ...args];
}

newApply_.lispSource = [Symbol.for('define'), [Symbol.for('new-apply_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('args'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('quasiquote'), [Symbol.for('apply'), Symbol.for('new'), [Symbol.for('unquote-splicing'), Symbol.for('args')]]]];

newApply_.lispMacro = true;

/**
 * Expand a `(clj/try ...)` expression.
 *
 * Similar to the [`try` special form][clj:try] in Clojure.
 *
 * [clj:try]: https://clojuredocs.org/clojure.core/try
 */
function cljTry_(exp: any, env: any): any {
  const body: any = exp.slice(1);
  const bodyExps: any = [];
  let catchClauses: any = [];
  const cljCatchClauses: any = [];
  const finalizerClauses: any = [];
  for (let x of body) {
    if (taggedListP(x, Symbol.for('catch'))) {
      cljCatchClauses.push(x);
    } else if (taggedListP(x, Symbol.for('finally'))) {
      finalizerClauses.push(x);
    } else {
      bodyExps.push(x);
    }
  }
  if (cljCatchClauses.length > 0) {
    const exception: any = ((): any => {
      const lst: any = cljCatchClauses[0];
      if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(lst);
        return Array.isArray(x) && (x.length === 0);
      })()) {
        let i: any = 1;
        let result: any = lst;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = lst[lst.length - 1];
          } else {
            result = lst.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      } else {
        return lst[1];
      }
    })();
    const sym: any = ((): any => {
      const lst: any = cljCatchClauses[0];
      if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
        const x: any = lastCdr(lst);
        return Array.isArray(x) && (x.length === 0);
      })()) {
        let i: any = 2;
        let result: any = lst;
        while (i > 0) {
          if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
            result = lst[lst.length - 1];
          } else {
            result = lst.slice(1);
          }
          i--;
        }
        if (Array.isArray(result)) {
          result = result[0];
        }
        return result;
      } else {
        return lst[2];
      }
    })();
    if ((cljCatchClauses.length === 1) && [Symbol.for('_'), Symbol.for('js/Object'), Symbol.for('Object'), Symbol.for('object%')].includes(exception)) {
      const cljCatchClause: any = cljCatchClauses[0];
      const catchClause: any = [Symbol.for('catch'), sym, ...cljCatchClause.slice(3)];
      catchClauses = [catchClause];
    } else {
      const condExp: any = [Symbol.for('cond'), ...cljCatchClauses.map(function (x: any): any {
        return [[Symbol.for('is-a?'), sym, (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && ((): any => {
          const x1: any = lastCdr(x);
          return Array.isArray(x1) && (x1.length === 0);
        })()) ? ((): any => {
          let i: any = 1;
          let result: any = x;
          while (i > 0) {
            if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
              result = x[x.length - 1];
            } else {
              result = x.slice(1);
            }
            i--;
          }
          if (Array.isArray(result)) {
            result = result[0];
          }
          return result;
        })() : x[1]], ...x.slice(3)];
      }), [Symbol.for('else'), [Symbol.for('throw'), sym]]];
      const catchClause: any = [Symbol.for('catch'), sym, condExp];
      catchClauses = [catchClause];
    }
  }
  return [Symbol.for('js/try'), ...bodyExps, ...catchClauses, ...finalizerClauses];
}

cljTry_.lispSource = [Symbol.for('define'), [Symbol.for('clj-try_'), Symbol.for('exp'), Symbol.for('env')], [Symbol.for('define-values'), Symbol.for('body'), [Symbol.for('rest'), Symbol.for('exp')]], [Symbol.for('define'), Symbol.for('body-exps'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('catch-clauses'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('clj-catch-clauses'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('finalizer-clauses'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('body')]], [Symbol.for('cond'), [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('catch')]], [Symbol.for('push-right!'), Symbol.for('clj-catch-clauses'), Symbol.for('x')]], [[Symbol.for('tagged-list?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('finally')]], [Symbol.for('push-right!'), Symbol.for('finalizer-clauses'), Symbol.for('x')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('body-exps'), Symbol.for('x')]]]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('clj-catch-clauses')], 0], [Symbol.for('define'), Symbol.for('exception'), [Symbol.for('second'), [Symbol.for('first'), Symbol.for('clj-catch-clauses')]]], [Symbol.for('define'), Symbol.for('sym'), [Symbol.for('third'), [Symbol.for('first'), Symbol.for('clj-catch-clauses')]]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('clj-catch-clauses')], 1], [Symbol.for('memq?'), Symbol.for('exception'), [Symbol.for('quote'), [Symbol.for('_'), Symbol.for('js/Object'), Symbol.for('Object'), Symbol.for('object%')]]]], [Symbol.for('define'), Symbol.for('clj-catch-clause'), [Symbol.for('first'), Symbol.for('clj-catch-clauses')]], [Symbol.for('define'), Symbol.for('catch-clause'), [Symbol.for('quasiquote'), [Symbol.for('catch'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('clj-catch-clause'), 3]]]]], [Symbol.for('set!'), Symbol.for('catch-clauses'), [Symbol.for('list'), Symbol.for('catch-clause')]]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('cond-exp'), [Symbol.for('quasiquote'), [Symbol.for('cond'), [Symbol.for('unquote-splicing'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('quasiquote'), [[Symbol.for('is-a?'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), [Symbol.for('second'), Symbol.for('x')]]], [Symbol.for('unquote-splicing'), [Symbol.for('drop'), Symbol.for('x'), 3]]]]], Symbol.for('clj-catch-clauses')]], [Symbol.for('else'), [Symbol.for('throw'), [Symbol.for('unquote'), Symbol.for('sym')]]]]]], [Symbol.for('define'), Symbol.for('catch-clause'), [Symbol.for('quasiquote'), [Symbol.for('catch'), [Symbol.for('unquote'), Symbol.for('sym')], [Symbol.for('unquote'), Symbol.for('cond-exp')]]]], [Symbol.for('set!'), Symbol.for('catch-clauses'), [Symbol.for('list'), Symbol.for('catch-clause')]]]]], [Symbol.for('quasiquote'), [Symbol.for('js/try'), [Symbol.for('unquote-splicing'), Symbol.for('body-exps')], [Symbol.for('unquote-splicing'), Symbol.for('catch-clauses')], [Symbol.for('unquote-splicing'), Symbol.for('finalizer-clauses')]]]];

cljTry_.lispMacro = true;

export {
  fieldBoundP_ as fieldBoundp_,
  begin0_,
  caseEq_,
  case_,
  cljTry_,
  defclass_,
  definePrivate_,
  definePublic_,
  defmacro_,
  defun_,
  do_,
  fieldBoundP_,
  if_,
  jsForIn_,
  jsForOf_,
  jsFor_,
  letEnv_,
  multipleValueBind_,
  newApply_,
  rktNew_,
  set_,
  threadAs_,
  threadFirst_,
  threadLast_,
  unless_,
  unwindProtect_,
  when_,
  while_
};