// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # List functions
 *
 * Functions for working with lists.
 *
 * ## Description
 *
 * Lists are implemented in terms of JavaScript arrays. There is,
 * however, no direct JavaScript equivalent to the Lisp concept of a
 * *cons cell*, such as `'(1 . 2)`. Therefore, Roselisp defines a
 * list type called a linked list, whose constituent links correspond
 * to the cons cell concept.
 *
 * ### Terminology
 *
 * The *empty list* is the list `'()`, represented by the JavaScript
 * array `[]`. The function `null?` returns `#t` only when passed the
 * empty list.
 *
 *     > (null? '())
 *     #t
 *
 * A list with one or more elements is a *nonempty list*. There are
 * two forms of nonempty lists: *array lists* and *linked lists*. An
 * array list is a list that is straightforwardly implemented in
 * terms of a JavaScript array, such as `'(1)`, `'(1 2)` and
 * `'(1 2 3)`. These correspond to the JavaScript arrays `[1]`,
 * `[1, 2]` and `[1, 2, 3]`, respectively. The function `array-list?`
 * returns `#t` when passed an array list.
 *
 *     > (array-list? '(1))
 *     #t
 *     > (array-list? '(1 2))
 *     #t
 *     > (array-list? '(1 2 3))
 *     #t
 *
 * The other type of nonempty list is the *linked list*, which is
 * composed out of *linked list links*. A linked list link is an
 * expression such as `'(1 . ())`, which is implemented in terms of
 * the JavaScript array `[1, Symbol.for('.'), []]`. Here, the
 * penultimate array element is the special symbol `.` (i.e., `'.`),
 * which is also referred to as the *cons dot*. The function
 * `linked-list-link?` returns `#t` when passed a linked list link.
 *
 *     > (linked-list-link? '(1 . ()))
 *     #t
 *     > (linked-list-link? '(1 2 . ()))
 *     #t
 *
 * By chaining such links together, a sequence of values is obtained.
 * If the final chain is the empty list, then the resulting structure
 * is referred to as a *linked list*. For example, the linked list
 * `'(1 . (2 . ()))` may be considered to represent the same sequence
 * as the array list `(1 2)`, even though the underlying JavaScript
 * arrays are different. The function `linked-list?` returns `#t`
 * when passed a linked list.
 *
 *     > (linked-list? '(1 . ()))
 *     #t
 *     > (linked-list? '(1 . (2 . ())))
 *     #t
 *     > (linked-list? '(1 2 . (3 . ())))
 *     #t
 *
 * However, if the final chain is *not* the empty list, then the
 * sequence is called a *dotted list*. The function `dotted-list?`
 * returns `#t` when passed a dotted list.
 *
 *     > (dotted-list? '(1 . 2))
 *     #t
 *     > (dotted-list? '(1 2 . 3))
 *     #t
 *     > (dotted-list? '(1 . (2 . 3)))
 *     #t
 *     > (dotted-list? '(1 2 . (3 . 4)))
 *     #t
 *
 * Dotted lists are sometimes referred to as "improper lists", to
 * distinguish them from "proper lists", which are `()`-terminated.
 * Thus, array lists and linked lists are proper lists, while dotted
 * lists are improper. The function `list?` returns `#t` when passed
 * a proper list, but not when passed an improper list.
 *
 *     > (list? '())
 *     #t
 *     > (list? '(1))
 *     #t
 *     > (list? '(1 . ()))
 *     #t
 *     > (list? '(1 . (2 . ())))
 *     #t
 *     > (list? '(1 2 . (3 . ())))
 *     #t
 *
 * While array lists are the fastest and are preferable for most
 * tasks, linked lists are more versatile. For example, a linked list
 * may be turned into a *circular list*, i.e., a list that loops back
 * on itself. The function `circular-list?` returns `#t` when passed
 * a circular list.
 *
 *     > (define circ-lst
 *         '(1 . ()))
 *     undefined
 *     > (set-cdr! circ-lst circ-lst)
 *     undefined
 *     > (circular-list? circ-lst)
 *     #t))))
 *
 * It should be appreciated that Lisp's "list" concept is not a type,
 * but a structural property. Whether something is a list or not can
 * be determined only by inspecting the structure itself. One can
 * avoid this check by working exclusively with array lists and using
 * `array-list?` to determine whether something is an array list or
 * not.
 *
 * ### Subtleties
 *
 * Note that the function `array-list?` also returns `#t` when passed
 * a linked list:
 *
 *     > (array-list? '(1 . ()))
 *     #t
 *
 * Roselisp allows for the possibility that you simply wants to treat
 * the list `'(1 . ())` as an array list of three elements, with the
 * cons dot as the second element. If you wish to distinguish linked
 * lists from array lists, you should use `linked-list?`, not
 * `array-list?`. The standard list functions---`cdr`, `nth`,
 * `first`, `second`, `third`, etc.---treat the input as a linked
 * list if `linked-list?` returns `#t`, and as an array list
 * otherwise.
 *
 * The term "dotted list" refers exclusively to improper lists.
 * Therefore, while the linked list `'(1 . ())` does indeed contain
 * a dot, it is not regarded as a dotted list. The improper list
 * `'(1 . 2)`, on the other hand, is regarded as dotted.
 *
 * ### Cons dot
 *
 * TODO: Merge this section into the other text.
 *
 * There is no direct analogue to the cons cell concept in
 * JavaScript. Therefore, Roselisp represents cons cells in terms of
 * *dotted lists*, which are lists where the penultimate element is
 * the *cons dot*, the symbol `.` (i.e., `|.|`, or `Symbol.for('.')`
 * in JavaScript).
 *
 * Thus, in Roselisp, the expression `'(1 . 2)` is the same as
 * `(list 1 '. 2)`, or, in JavaScript, `[1, Symbol.for('.'), 2]`.
 * A cons cell can be considered from two perspectives: either as a
 * pair of two elements, or as a list where the second element is the
 * cons dot.
 *
 * Cons cells are implemented as a *dotted list*, i.e., as a
 * three-element list with a special cons dot symbol as the second
 * element. Thus, the cons cell `(a . b)` is represented as the list
 * `(a <cons-dot> b)`, where `<cons-dot>` is a special value defined
 * in this file. It is just the symbol `.` (also written `|.|`).
 *
 * It is possible to have more than one element before the dot, as in
 * `(a b c . d)`. Thus, dotted lists can be understood as a
 * generalization of cons cells, with a cons cell being a dotted list
 * with two elements.
 *
 * It is also possible to represent circular lists in terms of dotted
 * lists. In Roselisp, a circular list is a dotted list containing
 * itself.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

import {
  CallExpression,
  Identifier,
  Literal,
  MemberExpression
} from './estree';

const [lastCdr, range, linkedListLength, linkedListLast]: any[] = ((): any => {
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
  function linkedListLength_(lst: any): any {
    let len: any = 0;
    let current: any = lst;
    while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
      len = len + (lst.length - 2);
      current = current[current.length - 1];
    }
    return len;
  }
  function linkedListLast_(lst: any): any {
    let current: any = lst;
    let result: any = undefined;
    while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.')) && !((): any => {
      const x: any = current[current.length - 1];
      return Array.isArray(x) && (x.length === 0);
    })()) {
      current = current[current.length - 1];
    }
    if (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
      result = current[current.length - 3];
    }
    return result;
  }
  return [lastCdr_, range_, linkedListLength_, linkedListLast_];
})();

/**
 * Cons dot value.
 */
const consDot_: any = Symbol.for('.');

/**
 * Compiled cons dot value.
 */
const consDotCompiled_: any =
  // `Symbol.for('.')`
  new CallExpression(new MemberExpression(new Identifier('Symbol'), new Identifier('for')), [new Literal('.')]);

/**
 * `cons-dot` function.
 */
function consDotF_(): any {
  return Symbol.for('.');
}

consDotF_.lispSource = [Symbol.for('define'), [Symbol.for('cons-dot-f_')], Symbol.for('*cons-dot*')];

/**
 * Whether a value is the cons dot.
 */
function consDotP_(obj: any): any {
  return obj === Symbol.for('.');
}

consDotP_.lispSource = [Symbol.for('define'), [Symbol.for('cons-dot?_'), Symbol.for('obj')], [Symbol.for('eq?'), Symbol.for('obj'), Symbol.for('*cons-dot*')]];

/**
 * Create a cons cell whose CAR is `x` and CDR is `y`.
 *
 * Similar to [`cons` in Racket][rkt:cons] and
 * [`cons` in Common Lisp][cl:cons].
 *
 * [rkt:cons]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cons%29%29
 * [cl:cons]: http://clhs.lisp.se/Body/f_cons.htm
 */
function cons_(x: any, y: any): any {
  // Create an array list whenever possible;
  // otherwise create a dotted list.
  if (Array.isArray(y)) {
    return [x, ...y];
  } else {
    return [x, Symbol.for('.'), y];
  }
}

cons_.lispSource = [Symbol.for('define'), [Symbol.for('cons_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('y')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote-splicing'), Symbol.for('y')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('*cons-dot*')], [Symbol.for('unquote'), Symbol.for('y')]]]]]];

/**
 * Whether something is a cons cell.
 *
 * Similar to [`cons?` in Racket][rkt:consp] and
 * [`consp` in Common Lisp][cl:consp].
 *
 * [rkt:consp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._cons~3f%29%29
 * [cl:consp]: http://clhs.lisp.se/Body/f_consp.htm
 */
function consp_(obj: any): any {
  // All lists except the empty list
  // qualify as cons cells.
  return Array.isArray(obj) && !(Array.isArray(obj) && (obj.length === 0));
}

consp_.lispSource = [Symbol.for('define'), [Symbol.for('cons?_'), Symbol.for('obj')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('obj')], [Symbol.for('not'), [Symbol.for('null?'), Symbol.for('obj')]]]];

/**
 * Make a list.
 *
 * Similar to [`list` in Racket][rkt:list] and
 * [`list` in Common Lisp][cl:list].
 *
 * [rkt:list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list%29%29
 * [cl:list]: http://clhs.lisp.se/Body/f_list_.htm
 */
function list_(...args: any[]): any {
  return args;
}

list_.lispSource = [Symbol.for('define'), [Symbol.for('list_'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')];

/**
 * Whether something is a list.
 *
 * This function checks whether the list is a proper list, i.e.,
 * whether the list is terminated by the empty list. In that regard,
 * this function is more similar to [`list?` in Racket][rkt:listp]
 * than to [`listp` in Common Lisp][cl:listp].
 *
 * [rkt:listp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list~3f%29%29
 * [cl:listp]: http://clhs.lisp.se/Body/f_listp.htm#listp
 */
function listp_(x: any): any {
  const x1: any = lastCdr(x);
  return Array.isArray(x1) && (x1.length === 0);
}

listp_.lispSource = [Symbol.for('define'), [Symbol.for('list?_'), Symbol.for('x')], [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]];

/**
 * Make a dotted list. Like `list`, but the final argument
 * is used as the tail, instead of as the final element.
 *
 *     > (list* 1 '())
 *     '(1)
 *     > (list* 1 2)
 *     '(1 . 2)
 *     > (list* 1 2 3)
 *     '(1 2 . 3)
 *
 * Also known as `cons*`. Similar to
 * [`list*` in Racket][rkt:list-star].
 *
 * [rkt:list-star]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list%2A%29%29
 */
function listStar_(...args: any[]): any {
  if (args.length === 0) {
    return undefined;
  } else if (args.length === 1) {
    return args[0];
  } else {
    const tailLst: any = args[args.length - 1];
    const headLst: any = args.slice(0, -1);
    if (Array.isArray(tailLst)) {
      // Make a proper list if possible.
      return [...headLst, ...tailLst];
    } else {
      // If not, make a dotted list.
      return [...headLst, Symbol.for('.'), tailLst];
    }
  }
}

listStar_.lispSource = [Symbol.for('define'), [Symbol.for('list-star_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 0], Symbol.for('undefined')], [[Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], 1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('tail-lst'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('head-lst'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('tail-lst')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('head-lst')], [Symbol.for('unquote-splicing'), Symbol.for('tail-lst')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('head-lst')], [Symbol.for('unquote'), Symbol.for('*cons-dot*')], [Symbol.for('unquote'), Symbol.for('tail-lst')]]]]]]]];

/**
 * Make a list of `n` elements. The function `proc` is applied
 * to the integers from `0` to `n - 1`.
 *
 * Similar to [`build-list` in Racket][rkt:build-list].
 *
 * [rkt:build-list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._build-list%29%29
 */
function buildList_(n: any, proc: any): any {
  return range(0, n).map(function (x: any): any {
    return proc(x);
  });
}

buildList_.lispSource = [Symbol.for('define'), [Symbol.for('build-list_'), Symbol.for('n'), Symbol.for('proc')], [Symbol.for('map'), Symbol.for('proc'), [Symbol.for('range'), 0, Symbol.for('n')]]];

/**
 * Make a list of length `k`, where every element is the value `v`.
 *
 * Similar to [`make-list` in Racket][rkt:make-list].
 *
 * [rkt:make-list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._make-list%29%29
 */
function makeList_(k: any, v: any): any {
  let result: any = [];
  for (let i: any = 0; i < k; i++) {
    result.push(v);
  }
  return result;
}

makeList_.lispSource = [Symbol.for('define'), [Symbol.for('make-list_'), Symbol.for('k'), Symbol.for('v')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('k')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('v')]], Symbol.for('result')]];

/**
 * List concatenation.
 *
 * Similar to [`append` in Racket][rkt:append] and
 * [`append` in Common Lisp][cl:append].
 *
 * [rkt:append]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._append%29%29
 * [cl:append]: http://clhs.lisp.se/Body/f_append.htm#append
 */
function append_(...args: any[]): any {
  return args.reduce(function (acc: any, x: any): any {
    return [...acc, ...x];
  }, []);
}

append_.lispSource = [Symbol.for('define'), [Symbol.for('append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('append'), Symbol.for('acc'), Symbol.for('x')]], [Symbol.for('quote'), []], Symbol.for('args')]];

/**
 * Flatten an arbitrarily nested list.
 *
 * Similar to [`flatten` in Racket][rkt:flatten].
 *
 * [rkt:flatten]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._flatten%29%29
 */
function flatten_(lst: any): any {
  return lst.reduce(function (acc: any, x: any): any {
    if (Array.isArray(x)) {
      return [...acc, ...flatten_(x)];
    } else if (x === Symbol.for('.')) {
      return acc;
    } else {
      acc.push(x);
      return acc;
    }
  }, []);
}

flatten_.lispSource = [Symbol.for('define'), [Symbol.for('flatten_'), Symbol.for('lst')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('x')], [Symbol.for('append'), Symbol.for('acc'), [Symbol.for('flatten_'), Symbol.for('x')]]], [[Symbol.for('cons-dot?'), Symbol.for('x')], Symbol.for('acc')], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('acc'), Symbol.for('x')]]]], [Symbol.for('quote'), []], Symbol.for('lst')]];

/**
 * Return the first element of a list.
 *
 * Similar to [`first` in Racket][rkt:first].
 *
 * [rkt:first]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._first%29%29
 */
function first_(lst: any): any {
  return lst[0];
}

first_.lispSource = [Symbol.for('define'), [Symbol.for('first_'), Symbol.for('lst')], [Symbol.for('array-first'), Symbol.for('lst')]];

/**
 * Return the second element of a list.
 *
 * Similar to [`second` in Racket][rkt:second].
 *
 * [rkt:second]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._second%29%29
 */
function second_(lst: any): any {
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
}

second_.lispSource = [Symbol.for('define'), [Symbol.for('second_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list?'), Symbol.for('lst')], [Symbol.for('linked-list-second'), Symbol.for('lst')], [Symbol.for('array-list-second'), Symbol.for('lst')]]];

/**
 * Return the third element of a list.
 *
 * Similar to [`third` in Racket][rkt:third].
 *
 * [rkt:third]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._third%29%29
 */
function third_(lst: any): any {
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
}

third_.lispSource = [Symbol.for('define'), [Symbol.for('third_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list?'), Symbol.for('lst')], [Symbol.for('linked-list-third'), Symbol.for('lst')], [Symbol.for('array-list-third'), Symbol.for('lst')]]];

/**
 * Return the fourth element of a list.
 *
 * Similar to [`fourth` in Racket][rkt:fourth].
 *
 * [rkt:fourth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fourth%29%29
 */
function fourth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(lst);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    let i: any = 3;
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
    return lst[3];
  }
}

fourth_.lispSource = [Symbol.for('define'), [Symbol.for('fourth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list?'), Symbol.for('lst')], [Symbol.for('linked-list-fourth'), Symbol.for('lst')], [Symbol.for('array-list-fourth'), Symbol.for('lst')]]];

/**
 * Return the fifth element of a list.
 *
 * Similar to [`fifth` in Racket][rkt:fifth].
 *
 * [rkt:fifth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fifth%29%29
 */
function fifth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(lst);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    let i: any = 4;
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
    return lst[4];
  }
}

fifth_.lispSource = [Symbol.for('define'), [Symbol.for('fifth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list?'), Symbol.for('lst')], [Symbol.for('linked-list-fifth'), Symbol.for('lst')], [Symbol.for('array-list-fifth'), Symbol.for('lst')]]];

/**
 * Return the sixth element of a list.
 *
 * Similar to [`sixth` in Racket][rkt:sixth].
 *
 * [rkt:sixth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._sixth%29%29
 */
function sixth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(lst);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    let i: any = 5;
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
    return lst[5];
  }
}

sixth_.lispSource = [Symbol.for('define'), [Symbol.for('sixth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list?'), Symbol.for('lst')], [Symbol.for('linked-list-sixth'), Symbol.for('lst')], [Symbol.for('array-list-sixth'), Symbol.for('lst')]]];

/**
 * Return the seventh element of a list.
 *
 * Similar to [`seventh` in Racket][rkt:seventh].
 *
 * [rkt:seventh]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._seventh%29%29
 */
function seventh_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(lst);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    let i: any = 6;
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
    return lst[6];
  }
}

seventh_.lispSource = [Symbol.for('define'), [Symbol.for('seventh_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list?'), Symbol.for('lst')], [Symbol.for('linked-list-seventh'), Symbol.for('lst')], [Symbol.for('array-list-seventh'), Symbol.for('lst')]]];

/**
 * Return the eighth element of a list.
 *
 * Similar to [`eighth` in Racket][rkt:eighth].
 *
 * [rkt:eighth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._eighth%29%29
 */
function eighth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(lst);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    let i: any = 7;
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
    return lst[7];
  }
}

eighth_.lispSource = [Symbol.for('define'), [Symbol.for('eighth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list?'), Symbol.for('lst')], [Symbol.for('linked-list-eighth'), Symbol.for('lst')], [Symbol.for('array-list-eighth'), Symbol.for('lst')]]];

/**
 * Return the ninth element of a list.
 *
 * Similar to [`ninth` in Racket][rkt:ninth].
 *
 * [rkt:ninth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._ninth%29%29
 */
function ninth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(lst);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    let i: any = 8;
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
    return lst[8];
  }
}

ninth_.lispSource = [Symbol.for('define'), [Symbol.for('ninth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list?'), Symbol.for('lst')], [Symbol.for('linked-list-ninth'), Symbol.for('lst')], [Symbol.for('array-list-ninth'), Symbol.for('lst')]]];

/**
 * Return the tenth element of a list.
 *
 * Similar to [`tenth` in Racket][rkt:tenth].
 *
 * [rkt:tenth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._tenth%29%29
 */
function tenth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.')) && ((): any => {
    const x: any = lastCdr(lst);
    return Array.isArray(x) && (x.length === 0);
  })()) {
    let i: any = 9;
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
    return lst[9];
  }
}

tenth_.lispSource = [Symbol.for('define'), [Symbol.for('tenth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list?'), Symbol.for('lst')], [Symbol.for('linked-list-tenth'), Symbol.for('lst')], [Symbol.for('array-list-tenth'), Symbol.for('lst')]]];

/**
 * Return the tail of a list.
 *
 * Interprets `lst` as a linked list of pairs, and returns the second
 * element of the first pair.
 *
 * Similar to [`cdr` in Racket][rkt:cdr] and
 * [`cdr` in Common Lisp][cl:cdr].
 *
 * [rkt:cdr]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cdr%29%29
 * [cl:cdr]: http://clhs.lisp.se/Body/f_car_c.htm#cdr
 */
function cdr_(lst: any): any {
  if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
    return lst[2];
  } else {
    return lst.slice(1);
  }
}

cdr_.lispSource = [Symbol.for('define'), [Symbol.for('cdr_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-pair?'), Symbol.for('lst')], [Symbol.for('linked-pair-cdr'), Symbol.for('lst')], [Symbol.for('array-list-cdr'), Symbol.for('lst')]]];

/**
 * Return the tail of a list.
 *
 * Similar to [`rest` in Racket][rkt:rest].
 *
 * [rkt:rest]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._rest%29%29
 */
function rest_(lst: any): any {
  // TODO: Handle linked lists.
  // TODO: Alias of `cdr`.
  return lst.slice(1);
}

rest_.lispSource = [Symbol.for('define'), [Symbol.for('rest_'), Symbol.for('lst')], [Symbol.for('array-list-rest'), Symbol.for('lst')]];

/**
 * Return the `n`-th element of a list.
 *
 * Similar to [`nth` in Racket][rkt:nth] and
 * [`nth` in Common Lisp][cl:nth].
 *
 * [rkt:nth]: https://docs.racket-lang.org/collections/collections-api.html#%28def._%28%28lib._data%2Fcollection..rkt%29._nth%29%29
 * [cl:nth]: http://clhs.lisp.se/Body/f_nth.htm#nth
 */
function nth_(n: any, lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
    let i: any = n;
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
    return (lst as any)[n];
  }
}

nth_.lispSource = [Symbol.for('define'), [Symbol.for('nth_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list-link?'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('array-list-nth'), Symbol.for('n'), Symbol.for('lst')]]];

/**
 * Return the `n`-th CDR element of a list.
 *
 * Similar to [`nthcdr` in Common Lisp][cl:nthcdr].
 *
 * [cl:nth]: http://clhs.lisp.se/Body/f_nthcdr.htm#nthcdr
 */
function nthcdr_(n: any, lst: any): any {
  if ((lst.length === (n + 2)) && ((lst as any)[n] === Symbol.for('.'))) {
    return lst[lst.length - 1];
  } else {
    if (n === 0) {
      return lst;
    } else {
      return lst.slice(n);
    }
  }
}

nthcdr_.lispSource = [Symbol.for('define'), [Symbol.for('nthcdr_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-length'), Symbol.for('lst')], [Symbol.for('+'), Symbol.for('n'), 2]], [Symbol.for('cons-dot?'), [Symbol.for('aget'), Symbol.for('lst'), Symbol.for('n')]]], [Symbol.for('linked-list-nthcdr'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('array-list-nthcdr'), Symbol.for('n'), Symbol.for('lst')]]];

/**
 * Take the `n` first elements from `lst`.
 *
 * Similar to [`take` in Racket][rkt:take].
 *
 * [rkt:take]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._take%29%29
 */
function take_(lst: any, n: any): any {
  const n1: any = lst.length - n;
  if (n1 === 0) {
    return lst;
  } else {
    return lst.slice(0, -n1);
  }
}

take_.lispSource = [Symbol.for('define'), [Symbol.for('take_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-list-take'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Return the list obtained by dropping
 * the first `n` elements from `lst`.
 *
 * Similar to [`drop` in Racket][rkt:drop].
 *
 * [rkt:drop]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop%29%29
 */
function drop_(lst: any, n: any): any {
  if (n === 0) {
    return lst;
  } else {
    return lst.slice(n);
  }
}

drop_.lispSource = [Symbol.for('define'), [Symbol.for('drop_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-list-drop'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Return the list obtained by dropping
 * the last `n` elements from `lst`.
 *
 * Similar to [`drop-right` in Racket][rkt:drop-right].
 *
 * [rkt:drop-right]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop-right%29%29
 */
function dropRight_(lst: any, n: any): any {
  if (n === 0) {
    return lst;
  } else {
    return lst.slice(0, -n);
  }
}

dropRight_.lispSource = [Symbol.for('define'), [Symbol.for('drop-right_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-list-drop-right'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Reverse the order of a list.
 * Returns a new list.
 *
 * Similar to [`reverse` in Racket][rkt:reverse].
 *
 * [rkt:reverse]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._reverse%29%29
 */
function reverse_(lst: any): any {
  return lst.reverse();
}

reverse_.lispSource = [Symbol.for('define'), [Symbol.for('reverse_'), Symbol.for('lst')], [Symbol.for('array-list-reverse'), Symbol.for('lst')]];

/**
 * Return a list where the last `n` conses have been omitted.
 *
 * Similar to [`butlast` in Common Lisp][cl:butlast].
 *
 * [cl:butlast]: http://clhs.lisp.se/Body/f_butlas.htm#butlast
 */
function butlast_(x: any, n: any = 1): any {
  let result: any = [...x];
  let i: any = n;
  while ((i > 0) && (result.length > 0)) {
    result.pop();
    i--;
  }
  return result;
}

butlast_.lispSource = [Symbol.for('define'), [Symbol.for('butlast_'), Symbol.for('x'), [Symbol.for('n'), 1]], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('x')]]]], [Symbol.for('i'), Symbol.for('n')]], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('>'), [Symbol.for('array-length'), Symbol.for('result')], 0]], [Symbol.for('pop-right!'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('result')]];

/**
 * Return a list where the last `n` conses have been omitted.
 * Changes the original list.
 *
 * Similar to [`nbutlast` in Common Lisp][cl:nbutlast].
 *
 * [cl:nbutlast]: http://clhs.lisp.se/Body/f_butlas.htm#nbutlast
 */
function nbutlast_(x: any, n: any = 1): any {
  let i: any = n;
  while ((i > 0) && (x.length > 0)) {
    x.pop();
    i--;
  }
  return x;
}

nbutlast_.lispSource = [Symbol.for('define'), [Symbol.for('nbutlast_'), Symbol.for('x'), [Symbol.for('n'), 1]], [Symbol.for('let'), [[Symbol.for('i'), Symbol.for('n')]], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('>'), [Symbol.for('array-length'), Symbol.for('x')], 0]], [Symbol.for('pop-right!'), Symbol.for('x')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('x')]];

/**
 * Pop an element off the beginning of a list.
 *
 * Similar to [`pop` in Common Lisp][cl:pop].
 *
 * [cl:pop]: http://clhs.lisp.se/Body/m_pop.htm#pop
 */
function popLeftX_(lst: any): any {
  return lst.shift();
}

popLeftX_.lispSource = [Symbol.for('define'), [Symbol.for('pop-left!_'), Symbol.for('lst')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('shift')]];

/**
 * Pop an element off the end of a list.
 *
 * Similar to [`Array.prototype.pop()` in JavaScript][js:pop].
 *
 * [js:pop]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop
 */
function popRightX_(lst: any): any {
  return lst.pop();
}

popRightX_.lispSource = [Symbol.for('define'), [Symbol.for('pop-right!_'), Symbol.for('lst')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('pop')]];

/**
 * Push an element onto the beginning of a list.
 *
 * Similar to [`push` in Common Lisp][cl:push].
 *
 * [cl:push]: http://clhs.lisp.se/Body/m_push.htm#push
 */
function pushLeftX_(lst: any, x: any): any {
  lst.unshift(x);
  return lst;
}

pushLeftX_.lispSource = [Symbol.for('define'), [Symbol.for('push-left!_'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('lst')];

/**
 * Push an element onto the end of a list.
 *
 * Similar to [`Array.prototype.push()` in JavaScript][js:push].
 *
 * [js:push]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push
 */
function pushRightX_(lst: any, x: any): any {
  lst.push(x);
  return lst;
}

pushRightX_.lispSource = [Symbol.for('define'), [Symbol.for('push-right!_'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('lst'), Symbol.for('push'), Symbol.for('x')], Symbol.for('lst')];

/**
 * Return the length of a list.
 *
 * Similar to [`length` in Racket][rkt:length] and
 * [`length` in Common Lisp][cl:length].
 *
 * [rkt:length]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._length%29%29
 * [cl:length]: http://clhs.lisp.se/Body/f_length.htm#length
 */
function length_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
    return linkedListLength(lst);
  } else {
    return lst.length;
  }
}

length_.lispSource = [Symbol.for('define'), [Symbol.for('length_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list-link?'), Symbol.for('lst')], [Symbol.for('linked-list-length'), Symbol.for('lst')], [Symbol.for('array-list-length'), Symbol.for('lst')]]];

/**
 * Return the last element of a list.
 *
 * Similar to [`last` in Racket][rkt:last].
 *
 * [rkt:last]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last%29%29
 */
function last_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
    return linkedListLast(lst);
  } else {
    return lst[lst.length - 1];
  }
}

last_.lispSource = [Symbol.for('define'), [Symbol.for('last_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('linked-list-link?'), Symbol.for('lst')], [Symbol.for('linked-list-last'), Symbol.for('lst')], [Symbol.for('array-list-last'), Symbol.for('lst')]]];

/**
 * Return the last pair of a list.
 *
 * Similar to [`last-pair` in Racket][rkt:last-pair] and
 * [`last` in Common Lisp][cl:last].
 *
 * [rkt:last-pair]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last-pair%29%29
 * [cl:last]: http://clhs.lisp.se/Body/f_last.htm#last
 */
function lastPair_(lst: any): any {
  if (!Array.isArray(lst)) {
    return undefined;
  } else if (Array.isArray(lst) && (lst.length === 0)) {
    return lst;
  } else if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
    let current: any = lst;
    let result: any = undefined;
    while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.')) && !((): any => {
      const x: any = current[current.length - 1];
      return Array.isArray(x) && (x.length === 0);
    })()) {
      current = current[current.length - 1];
    }
    return result;
  } else {
    const n: any = lst.length - 1;
    if (n === 0) {
      return lst;
    } else {
      return lst.slice(n);
    }
  }
}

lastPair_.lispSource = [Symbol.for('define'), [Symbol.for('last-pair_'), Symbol.for('lst')], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('array?'), Symbol.for('lst')]], Symbol.for('undefined')], [[Symbol.for('null?'), Symbol.for('lst')], Symbol.for('lst')], [[Symbol.for('linked-list-link?'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('current'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('linked-list-link?'), Symbol.for('current')], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('linked-list-tail'), Symbol.for('current')]]]], [Symbol.for('set!'), Symbol.for('current'), [Symbol.for('linked-list-tail'), Symbol.for('current')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('array-list-drop'), Symbol.for('lst'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('lst')], 1]]]]];

/**
 * Return the last cdr of a list, i.e., the terminating empty list.
 */
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

lastCdr_.lispSource = [Symbol.for('define'), [Symbol.for('last-cdr_'), Symbol.for('lst')], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('array?'), Symbol.for('lst')]], Symbol.for('undefined')], [[Symbol.for('linked-list-link?'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('while'), [Symbol.for('linked-list-link?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('linked-list-tail'), Symbol.for('result')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('quote'), []]]]];

/**
 * Set the CAR of a list.
 *
 * Similar to [`set-car!` in Racket][rkt:set-car].
 *
 * [rkt:set-car]: https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html#%25_idx_416
 */
function setCarX_(x: any, y: any): any {
  if (x.length > 0) {
    x[0] = y;
  }
  return undefined;
}

setCarX_.lispSource = [Symbol.for('define'), [Symbol.for('set-car!_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-length'), Symbol.for('x')], 0], [Symbol.for('aset!'), Symbol.for('x'), 0, Symbol.for('y')]], Symbol.for('undefined')];

/**
 * Set the CDR of a list.
 *
 * Similar to [`set-cdr!` in Racket][rkt:set-cdr].
 *
 * [rkt:set-cdr]: https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html#%25_idx_418
 */
function setCdrX_(x: any, y: any): any {
  if (Array.isArray(x) && (x.length === 0)) {
  } else if (x === y) {
    if (Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.'))) {
      x[x.length - 1] = y;
    } else {
      x.push(Symbol.for('.'));
      x.push(y);
    }
  } else {
    while (x.length > 1) {
      x.pop();
    }
    if (Array.isArray(y)) {
      for (let z of y) {
        x.push(z);
      }
    } else {
      x.push(Symbol.for('.'));
      x.push(y);
    }
  }
  return undefined;
}

setCdrX_.lispSource = [Symbol.for('define'), [Symbol.for('set-cdr!_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('x')]], [[Symbol.for('eq?'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('linked-list-link?'), Symbol.for('x')], [Symbol.for('aset!'), Symbol.for('x'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('x')], 1], Symbol.for('y')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('*cons-dot*')], [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('y')]]]], [Symbol.for('else'), [Symbol.for('while'), [Symbol.for('>'), [Symbol.for('array-length'), Symbol.for('x')], 1], [Symbol.for('pop-right!'), Symbol.for('x')]], [Symbol.for('cond'), [[Symbol.for('array?'), Symbol.for('y')], [Symbol.for('for'), [[Symbol.for('z'), Symbol.for('y')]], [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('z')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('*cons-dot*')], [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('y')]]]]], Symbol.for('undefined')];

/**
 * Whether something is the empty list.
 *
 * Similar to [`null?` in Racket][rkt:nullp].
 *
 * [rkt:nullp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._null~3f%29%29
 */
function nullp_(x: any): any {
  return Array.isArray(x) && (x.length === 0);
}

nullp_.lispSource = [Symbol.for('define'), [Symbol.for('null?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('='), [Symbol.for('array-length'), Symbol.for('x')], 0]]];

/**
 * Whether something is an array list.
 *
 * An array list is a list implemented in terms of an array.
 * It corresponds roughly to the
 * [`ArrayList` class in Java][java:ArrayList].
 *
 * [java:ArrayList]: https://docs.oracle.com/javase/8/docs/api/java/util/ArrayList.html
 */
function arrayListP_(x: any): any {
  return Array.isArray(x);
}

arrayListP_.lispSource = [Symbol.for('define'), [Symbol.for('array-list?_'), Symbol.for('x')], [Symbol.for('array?'), Symbol.for('x')]];

/**
 * Return the length of an array list.
 */
function arrayListLength_(lst: any): any {
  return lst.length;
}

arrayListLength_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-length_'), Symbol.for('lst')], [Symbol.for('array-length'), Symbol.for('lst')]];

/**
 * Return the first element of an array list.
 */
function arrayListFirst_(lst: any): any {
  return lst[0];
}

arrayListFirst_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-first_'), Symbol.for('lst')], [Symbol.for('array-first'), Symbol.for('lst')]];

/**
 * Return the second element of an array list.
 */
function arrayListSecond_(lst: any): any {
  return lst[1];
}

arrayListSecond_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-second_'), Symbol.for('lst')], [Symbol.for('array-second'), Symbol.for('lst')]];

/**
 * Return the third element of an array list.
 */
function arrayListThird_(lst: any): any {
  return lst[2];
}

arrayListThird_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-third_'), Symbol.for('lst')], [Symbol.for('array-third'), Symbol.for('lst')]];

/**
 * Return the fourth element of an array list.
 */
function arrayListFourth_(lst: any): any {
  return lst[3];
}

arrayListFourth_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-fourth_'), Symbol.for('lst')], [Symbol.for('array-fourth'), Symbol.for('lst')]];

/**
 * Return the fifth element of an array list.
 */
function arrayListFifth_(lst: any): any {
  return lst[4];
}

arrayListFifth_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-fifth_'), Symbol.for('lst')], [Symbol.for('array-fifth'), Symbol.for('lst')]];

/**
 * Return the sixth element of an array list.
 */
function arrayListSixth_(lst: any): any {
  return lst[5];
}

arrayListSixth_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-sixth_'), Symbol.for('lst')], [Symbol.for('array-sixth'), Symbol.for('lst')]];

/**
 * Return the seventh element of an array list.
 */
function arrayListSeventh_(lst: any): any {
  return lst[6];
}

arrayListSeventh_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-seventh_'), Symbol.for('lst')], [Symbol.for('array-seventh'), Symbol.for('lst')]];

/**
 * Return the eighth element of an array list.
 */
function arrayListEighth_(lst: any): any {
  return lst[7];
}

arrayListEighth_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-eighth_'), Symbol.for('lst')], [Symbol.for('array-eighth'), Symbol.for('lst')]];

/**
 * Return the ninth element of an array list.
 */
function arrayListNinth_(lst: any): any {
  return lst[8];
}

arrayListNinth_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-ninth_'), Symbol.for('lst')], [Symbol.for('array-ninth'), Symbol.for('lst')]];

/**
 * Return the tenth element of an array list.
 */
function arrayListTenth_(lst: any): any {
  return lst[9];
}

arrayListTenth_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-tenth_'), Symbol.for('lst')], [Symbol.for('array-tenth'), Symbol.for('lst')]];

/**
 * Return the last element of an array list.
 */
function arrayListLast_(lst: any): any {
  return lst[lst.length - 1];
}

arrayListLast_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-last_'), Symbol.for('lst')], [Symbol.for('array-last'), Symbol.for('lst')]];

/**
 * Return the `n`-th element of an array list.
 */
function arrayListNth_(n: any, lst: any): any {
  return (lst as any)[n];
}

arrayListNth_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-nth_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Return the `n`-th CDR of an array list.
 */
function arrayListNthcdr_(n: any, lst: any): any {
  if (n === 0) {
    return lst;
  } else {
    return lst.slice(n);
  }
}

arrayListNthcdr_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-nthcdr_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('array-drop'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Return the CDR of an array list.
 */
function arrayListCdr_(lst: any): any {
  return lst.slice(1);
}

arrayListCdr_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-cdr_'), Symbol.for('lst')], [Symbol.for('array-rest'), Symbol.for('lst')]];

/**
 * Return the tail of an array list.
 */
function arrayListRest_(lst: any): any {
  return lst.slice(1);
}

arrayListRest_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-rest_'), Symbol.for('lst')], [Symbol.for('array-list-cdr'), Symbol.for('lst')]];

/**
 * Take the `n` first elements from an array list.
 */
function arrayListTake_(lst: any, n: any): any {
  const n1: any = lst.length - n;
  if (n1 === 0) {
    return lst;
  } else {
    return lst.slice(0, -n1);
  }
}

arrayListTake_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-take_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-take'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Return the list obtained by dropping
 * the first `n` elements from an array list.
 */
function arrayListDrop_(lst: any, n: any): any {
  if (n === 0) {
    return lst;
  } else {
    return lst.slice(n);
  }
}

arrayListDrop_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-drop_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-drop'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Return the list obtained by dropping
 * the last `n` elements from an array list.
 */
function arrayListDropRight_(lst: any, n: any): any {
  if (n === 0) {
    return lst;
  } else {
    return lst.slice(0, -n);
  }
}

arrayListDropRight_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-drop-right_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-drop-right'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Reverse the order of an array list.
 * Returns a new array list.
 */
function arrayListReverse_(lst: any): any {
  return lst.reverse();
}

arrayListReverse_.lispSource = [Symbol.for('define'), [Symbol.for('array-list-reverse_'), Symbol.for('lst')], [Symbol.for('array-reverse'), Symbol.for('lst')]];

/**
 * Whether something is a dotted list.
 *
 * Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
 *
 * [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
 */
function dottedListP_(x: any): any {
  return Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && !((): any => {
    const x1: any = lastCdr(x);
    return Array.isArray(x1) && (x1.length === 0);
  })();
}

dottedListP_.lispSource = [Symbol.for('define'), [Symbol.for('dotted-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('cons-dot?'), [Symbol.for('aget'), Symbol.for('x'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('x')], 2]]], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]]]];

/**
 * Whether something is a linked list.
 *
 * An linked list is a list implemented as a chain of links.
 * It corresponds roughly to the
 * [`LinkedList` class in Java][java:LinkedList].
 *
 * [java:LinkedList]: https://docs.oracle.com/javase/8/docs/api/java/util/LinkedList.html
 */
function linkedListP_(x: any): any {
  return Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && ((): any => {
    const x1: any = lastCdr(x);
    return Array.isArray(x1) && (x1.length === 0);
  })();
}

linkedListP_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('cons-dot?'), [Symbol.for('aget'), Symbol.for('x'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('x')], 2]]], [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]]];

/**
 * Whether something is a linked list link.
 */
function linkedListLinkP_(x: any): any {
  return Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.'));
}

linkedListLinkP_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-link?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('cons-dot?'), [Symbol.for('aget'), Symbol.for('x'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('x')], 2]]]]];

/**
 * Whether something is a linked pair.
 */
function linkedPairP_(x: any): any {
  return Array.isArray(x) && (x.length === 3) && (x[1] === Symbol.for('.'));
}

linkedPairP_.lispSource = [Symbol.for('define'), [Symbol.for('linked-pair?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('cons-dot?'), [Symbol.for('aget'), Symbol.for('x'), 1]]]];

/**
 * Return the CAR of a linked list link.
 */
function linkedListLinkCar_(x: any): any {
  return x[0];
}

linkedListLinkCar_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-link-car_'), Symbol.for('x')], [Symbol.for('array-first'), Symbol.for('x')]];

/**
 * Return the CDR of a linked list link.
 */
function linkedListLinkCdr_(x: any): any {
  return x[x.length - 1];
}

linkedListLinkCdr_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-link-cdr_'), Symbol.for('x')], [Symbol.for('array-last'), Symbol.for('x')]];

/**
 * Return the CAR of a linked pair.
 */
function linkedPairCar_(x: any): any {
  return x[0];
}

linkedPairCar_.lispSource = [Symbol.for('define'), [Symbol.for('linked-pair-car_'), Symbol.for('x')], [Symbol.for('array-first'), Symbol.for('x')]];

/**
 * Return the CDR of a linked pair.
 */
function linkedPairCdr_(x: any): any {
  return x[2];
}

linkedPairCdr_.lispSource = [Symbol.for('define'), [Symbol.for('linked-pair-cdr_'), Symbol.for('x')], [Symbol.for('array-third'), Symbol.for('x')]];

/**
 * Return the length of a linked list.
 */
function linkedListLength_(lst: any): any {
  let len: any = 0;
  let current: any = lst;
  while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
    len = len + (lst.length - 2);
    current = current[current.length - 1];
  }
  return len;
}

linkedListLength_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-length_'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('len'), 0], [Symbol.for('define'), Symbol.for('current'), Symbol.for('lst')], [Symbol.for('while'), [Symbol.for('linked-list-link?'), Symbol.for('current')], [Symbol.for('set!'), Symbol.for('len'), [Symbol.for('+'), Symbol.for('len'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]]], [Symbol.for('set!'), Symbol.for('current'), [Symbol.for('linked-list-tail'), Symbol.for('current')]]], Symbol.for('len')];

/**
 * Return the first element of a linked list.
 */
function linkedListFirst_(lst: any): any {
  return lst[0];
}

linkedListFirst_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-first_'), Symbol.for('lst')], [Symbol.for('array-first'), Symbol.for('lst')]];

/**
 * Return the second element of a linked list.
 */
function linkedListSecond_(lst: any): any {
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
}

linkedListSecond_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-second_'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), 1, Symbol.for('lst')]];

/**
 * Return the third element of a linked list.
 */
function linkedListThird_(lst: any): any {
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
}

linkedListThird_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-third_'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), 2, Symbol.for('lst')]];

/**
 * Return the fourth element of a linked list.
 */
function linkedListFourth_(lst: any): any {
  let i: any = 3;
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
}

linkedListFourth_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-fourth_'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), 3, Symbol.for('lst')]];

/**
 * Return the fifth element of a linked list.
 */
function linkedListFifth_(lst: any): any {
  let i: any = 4;
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
}

linkedListFifth_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-fifth_'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), 4, Symbol.for('lst')]];

/**
 * Return the sixth element of a linked list.
 */
function linkedListSixth_(lst: any): any {
  let i: any = 5;
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
}

linkedListSixth_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-sixth_'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), 5, Symbol.for('lst')]];

/**
 * Return the seventh element of a linked list.
 */
function linkedListSeventh_(lst: any): any {
  let i: any = 6;
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
}

linkedListSeventh_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-seventh_'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), 6, Symbol.for('lst')]];

/**
 * Return the eighth element of a linked list.
 */
function linkedListEighth_(lst: any): any {
  let i: any = 7;
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
}

linkedListEighth_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-eighth_'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), 7, Symbol.for('lst')]];

/**
 * Return the ninth element of a linkedd list.
 */
function linkedListNinth_(lst: any): any {
  let i: any = 8;
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
}

linkedListNinth_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-ninth_'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), 8, Symbol.for('lst')]];

/**
 * Return the tenth element of a linkedd list.
 */
function linkedListTenth_(lst: any): any {
  let i: any = 9;
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
}

linkedListTenth_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-tenth_'), Symbol.for('lst')], [Symbol.for('linked-list-nth'), 9, Symbol.for('lst')]];

/**
 * Return the last element of a linked list.
 */
function linkedListLast_(lst: any): any {
  let current: any = lst;
  let result: any = undefined;
  while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.')) && !((): any => {
    const x: any = current[current.length - 1];
    return Array.isArray(x) && (x.length === 0);
  })()) {
    current = current[current.length - 1];
  }
  if (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
    result = current[current.length - 3];
  }
  return result;
}

linkedListLast_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-last_'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('current'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('linked-list-link?'), Symbol.for('current')], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('linked-list-link-cdr'), Symbol.for('current')]]]], [Symbol.for('set!'), Symbol.for('current'), [Symbol.for('linked-list-link-cdr'), Symbol.for('current')]]], [Symbol.for('when'), [Symbol.for('linked-list-link?'), Symbol.for('current')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('aget'), Symbol.for('current'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('current')], 3]]]], Symbol.for('result')];

/**
 * Return the `n`-th element of a linked list.
 */
function linkedListNth_(n: any, lst: any): any {
  let i: any = n;
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
}

linkedListNth_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-nth_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('i'), Symbol.for('n')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('cond'), [[Symbol.for('dotted-pair?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('linked-list-tail'), Symbol.for('lst')]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('array-rest'), Symbol.for('lst')]]]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], [Symbol.for('when'), [Symbol.for('array?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('array-first'), Symbol.for('result')]]], Symbol.for('result')];

/**
 * Return the `n`-th CDR of a linked list.
 */
function linkedListNthcdr_(n: any, lst: any): any {
  return lst[lst.length - 1];
}

linkedListNthcdr_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-nthcdr_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 1]]];

/**
 * Return the list obtained by dropping
 * the first `n` elements from a linked list.
 */
function linkedListDrop_(lst: any, pos: any): any {
  // TODO: Linked lists.
  if (pos === 0) {
    return lst;
  } else {
    return lst.slice(pos);
  }
}

linkedListDrop_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-drop_'), Symbol.for('lst'), Symbol.for('pos')], [Symbol.for('array-drop'), Symbol.for('lst'), Symbol.for('pos')]];

/**
 * Return the list obtained by dropping
 * the last `n` elements from a linked list.
 */
function linkedListDropRight_(lst: any, n: any): any {
  // TODO: Linked lists.
  const n1: any = n + 1;
  if (n1 === 0) {
    return lst;
  } else {
    return lst.slice(0, -n1);
  }
}

linkedListDropRight_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-drop-right_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-drop-right'), Symbol.for('lst'), [Symbol.for('+'), Symbol.for('n'), 1]]];

/**
 * Return the CAR of a linked list.
 */
function linkedListCar_(lst: any): any {
  return lst[0];
}

linkedListCar_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-car_'), Symbol.for('lst')], [Symbol.for('array-first'), Symbol.for('lst')]];

/**
 * Return the CDR of a linked list.
 */
function linkedListCdr_(lst: any): any {
  if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
    return lst[2];
  } else {
    return lst.slice(1);
  }
}

linkedListCdr_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-cdr_'), Symbol.for('lst')], [Symbol.for('cond'), [[Symbol.for('dotted-pair?'), Symbol.for('lst')], [Symbol.for('array-third'), Symbol.for('lst')]], [Symbol.for('else'), [Symbol.for('array-rest'), Symbol.for('lst')]]]];

/**
 * Return the head of a linked list.
 */
function linkedListHead_(lst: any): any {
  // TODO: Rename to `linked-list-link-head`.
  return lst.slice(0, -2);
}

linkedListHead_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-head_'), Symbol.for('lst')], [Symbol.for('drop-right'), Symbol.for('lst'), 2]];

/**
 * Return the tail of a linked list.
 */
function linkedListTail_(lst: any): any {
  // TODO: Rename to `linked-list-link-tail`.
  return lst[lst.length - 1];
}

linkedListTail_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-tail_'), Symbol.for('lst')], [Symbol.for('array-last'), Symbol.for('lst')]];

/**
 * Parse a linked list.
 */
function linkedListParse_(lst: any): any {
  return [linkedListHead_(lst), linkedListTail_(lst)];
}

linkedListParse_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list-parse_'), Symbol.for('lst')], [Symbol.for('values'), [Symbol.for('linked-list-head_'), Symbol.for('lst')], [Symbol.for('linked-list-tail_'), Symbol.for('lst')]]];

/**
 * Make a linked list.
 */
function makeDottedList_(car: any, cdr: any): any {
  // TODO: Rename to `make-linked-list`.
  return listStar_(car, cdr);
}

makeDottedList_.lispSource = [Symbol.for('define'), [Symbol.for('make-dotted-list_'), Symbol.for('car'), Symbol.for('cdr')], [Symbol.for('list-star_'), Symbol.for('car'), Symbol.for('cdr')]];

/**
 * Make a linked pair.
 */
function makePair_(car: any, cdr: any): any {
  // TODO: Rename to `make-linked-pair`.
  return [car, Symbol.for('.'), cdr];
}

makePair_.lispSource = [Symbol.for('define'), [Symbol.for('make-pair_'), Symbol.for('car'), Symbol.for('cdr')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('car')], [Symbol.for('unquote'), Symbol.for('*cons-dot*')], [Symbol.for('unquote'), Symbol.for('cdr')]]]];

/**
 * Whether something is a proper list,
 * i.e., a list that is terminated by
 * the empty list.
 *
 * Similar to [`proper-list?` in Racket][rkt:proper-list-p].
 *
 * [rkt:proper-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#proper-list-p
 */
function properListP_(x: any): any {
  const x1: any = lastCdr(x);
  return Array.isArray(x1) && (x1.length === 0);
}

properListP_.lispSource = [Symbol.for('define'), [Symbol.for('proper-list?_'), Symbol.for('x')], [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]];

/**
 * Whether something is an improper list,
 * i.e., a dotted list.
 *
 * Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
 *
 * [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
 */
function improperListP_(x: any): any {
  return !((): any => {
    const x1: any = lastCdr(x);
    return Array.isArray(x1) && (x1.length === 0);
  })();
}

improperListP_.lispSource = [Symbol.for('define'), [Symbol.for('improper-list?_'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]]];

/**
 * Whether something is a circular list.
 *
 * Similar to [`circular-list?` in Racket][rkt:circular-list-p].
 *
 * [rkt:circular-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#circular-list-p
 */
function circularListP_(x: any): any {
  return Array.isArray(x) && (x.length >= 3) && (x[x.length - 2] === Symbol.for('.')) && (x[x.length - 1] === x);
}

circularListP_.lispSource = [Symbol.for('define'), [Symbol.for('circular-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('linked-list-link?'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('linked-list-tail'), Symbol.for('x')], Symbol.for('x')]]];

/**
 * Convert an array list to a linked list.
 */
function arrayListToLinkedList_(x: any): any {
  return [...x.slice(0, -1), Symbol.for('.'), x[x.length - 1]];
}

arrayListToLinkedList_.lispSource = [Symbol.for('define'), [Symbol.for('array-list->linked-list_'), Symbol.for('x')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('drop-right'), Symbol.for('x'), 1]], [Symbol.for('unquote'), Symbol.for('*cons-dot*')], [Symbol.for('unquote'), [Symbol.for('array-list-last'), Symbol.for('x')]]]]];

/**
 * Convert a linked list to an array list.
 */
function linkedListToArrayList_(x: any): any {
  return [...x.slice(0, -2), x[x.length - 1]];
}

linkedListToArrayList_.lispSource = [Symbol.for('define'), [Symbol.for('linked-list->array-list_'), Symbol.for('x')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('linked-list-head'), Symbol.for('x')]], [Symbol.for('unquote'), [Symbol.for('linked-list-tail'), Symbol.for('x')]]]]];

export {
  append_ as append,
  arrayListToLinkedList_,
  arrayListToLinkedList_ as properListToDottedList_,
  butlast_ as butlast,
  cdr_ as cdr,
  cdr_ as tail,
  cdr_ as tail_,
  consp_ as pairp,
  consp_ as pairp_,
  dottedListP_ as dottedListP,
  dropRight_ as dropRight,
  drop_ as drop,
  drop_ as listTail,
  drop_ as listTail_,
  eighth_ as eighth,
  fifth_ as fifth,
  first_ as car,
  first_ as car_,
  first_ as first,
  first_ as head,
  flatten_ as flatten,
  fourth_ as fourth,
  lastCdr_ as linkedListLastCdr_,
  lastPair_ as lastCons_,
  lastPair_ as linkedListLastCons_,
  lastPair_ as linkedListLastPair_,
  last_ as last,
  length_ as length,
  linkedListToArrayList_,
  linkedListLast_,
  linkedPairP_ as dottedPairP_,
  listStar_ as listStar,
  listp_ as listp,
  listp_,
  listp_ as properListP,
  list_ as list,
  makeList_ as makeList,
  nbutlast_ as nbutlast,
  ninth_ as ninth,
  nth_ as nth,
  nthcdr_ as nthcdr,
  nullp_ as nullp,
  nullp_,
  popLeftX_ as popX,
  popLeftX_ as popX_,
  popLeftX_ as pop,
  popLeftX_ as popLeftX,
  popLeftX_ as popLeft,
  popLeftX_ as popLeft_,
  popLeftX_ as pop_,
  popRightX_ as popRightX,
  popRightX_ as popRight,
  popRightX_ as popRight_,
  pushLeftX_ as pushX,
  pushLeftX_ as pushX_,
  pushLeftX_ as push,
  pushLeftX_ as pushLeftX,
  pushLeftX_ as pushLeft,
  pushLeftX_ as pushLeft_,
  pushLeftX_ as push_,
  pushRightX_ as appendToList,
  pushRightX_ as pushRightX,
  pushRightX_ as pushRight,
  pushRightX_ as pushRight_,
  rest_ as rest,
  reverse_ as reverse,
  second_ as cadr,
  second_ as cadr_,
  second_ as second,
  setCarX_ as setCar_,
  setCdrX_ as setCdr_,
  seventh_ as seventh,
  sixth_ as sixth,
  tenth_ as tenth,
  third_ as third,
  append_,
  arrayListCdr_,
  arrayListDropRight_,
  arrayListDrop_,
  arrayListEighth_,
  arrayListFifth_,
  arrayListFirst_,
  arrayListFourth_,
  arrayListLast_,
  arrayListLength_,
  arrayListNinth_,
  arrayListNth_,
  arrayListNthcdr_,
  arrayListRest_,
  arrayListReverse_,
  arrayListSecond_,
  arrayListSeventh_,
  arrayListSixth_,
  arrayListTake_,
  arrayListTenth_,
  arrayListThird_,
  arrayListP_,
  buildList_,
  butlast_,
  cdr_,
  circularListP_,
  consDotCompiled_,
  consDotF_,
  consDotP_,
  consDot_,
  consp_,
  cons_,
  dottedListP_,
  dropRight_,
  drop_,
  eighth_,
  fifth_,
  first_,
  flatten_,
  fourth_,
  improperListP_,
  lastCdr_,
  lastPair_,
  last_,
  length_,
  linkedListCar_,
  linkedListCdr_,
  linkedListDropRight_,
  linkedListDrop_,
  linkedListEighth_,
  linkedListFifth_,
  linkedListFirst_,
  linkedListFourth_,
  linkedListHead_,
  linkedListLength_,
  linkedListLinkCar_,
  linkedListLinkCdr_,
  linkedListLinkP_,
  linkedListNinth_,
  linkedListNth_,
  linkedListNthcdr_,
  linkedListParse_,
  linkedListSecond_,
  linkedListSeventh_,
  linkedListSixth_,
  linkedListTail_,
  linkedListTenth_,
  linkedListThird_,
  linkedListP_,
  linkedPairCar_,
  linkedPairCdr_,
  linkedPairP_,
  listStar_,
  list_,
  makeDottedList_,
  makeList_,
  makePair_,
  nbutlast_,
  ninth_,
  nth_,
  nthcdr_,
  popLeftX_,
  popRightX_,
  properListP_,
  pushLeftX_,
  pushRightX_,
  rest_,
  reverse_,
  second_,
  setCarX_,
  setCdrX_,
  seventh_,
  sixth_,
  take_,
  tenth_,
  third_
};