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
 * *cons cell*, such as `'(1 . 2)`. Instead, such values are
 * represented as a *dotted list*, which is a list where the
 * penultimate value is the symbol `|.|` (in JavaScript,
 * `Symbol.for('.')`).
 *
 *     > (dotted-list? '(1 . 2))
 *     #t
 *     > (dotted-list? '(1 2 . 3))
 *     #t
 *     > (dotted-list? (list 1 '|.| 2))
 *     #t
 *
 * The simplifying assumption is made that dotted lists are only used
 * to represent values that cannot be expressed without a dot. This
 * permits most list functions to be implemented as simple array
 * operations, although some checks are necessary in places since
 * dotted lists are also implemented as arrays.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

// (require (only-in "./estree"
//                   CallExpression
//                   Identifier
//                   Literal
//                   MemberExpression))

const [lastCdr, range, dottedListSecond, dottedListThird, dottedListFourth, dottedListFifth, dottedListSixth, dottedListSeventh, dottedListEighth, dottedListNinth, dottedListTenth, dottedListLength, dottedListLast]: any[] = ((): any => {
  function lastCdr_(lst: any): any {
    if (!Array.isArray(lst)) {
      return undefined;
    } else if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
      let result: any = lst;
      while (Array.isArray(result) && (result.length >= 3) && (result.at(-2) === Symbol.for('.'))) {
        result = result.at(-1);
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
  function dottedListSecond_(lst: any): any {
    return dottedListRef_(lst, 1);
  }
  function dottedListThird_(lst: any): any {
    return dottedListRef_(lst, 2);
  }
  function dottedListFourth_(lst: any): any {
    return dottedListRef_(lst, 3);
  }
  function dottedListFifth_(lst: any): any {
    return dottedListRef_(lst, 4);
  }
  function dottedListSixth_(lst: any): any {
    return dottedListRef_(lst, 5);
  }
  function dottedListSeventh_(lst: any): any {
    return dottedListRef_(lst, 6);
  }
  function dottedListEighth_(lst: any): any {
    return dottedListRef_(lst, 7);
  }
  function dottedListNinth_(lst: any): any {
    return dottedListRef_(lst, 8);
  }
  function dottedListTenth_(lst: any): any {
    return dottedListRef_(lst, 9);
  }
  function dottedListLength_(lst: any): any {
    let len: any = 0;
    let current: any = lst;
    while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.'))) {
      len = len + (lst.length - 2);
      current = current.at(-1);
    }
    return len;
  }
  function dottedListLast_(lst: any): any {
    let current: any = lst;
    let result: any = undefined;
    while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.')) && !((x: any): any => {
      return Array.isArray(x) && (x.length === 0);
    })(((current.length === 3) && (current[1] === Symbol.for('.'))) ? current[2] : current.slice(1))) {
      current = ((current.length === 3) && (current[1] === Symbol.for('.'))) ? current[2] : current.slice(1);
    }
    if (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.'))) {
      result = current[current.length - 3];
    }
    return result;
  }
  return [lastCdr_, range_, dottedListSecond_, dottedListThird_, dottedListFourth_, dottedListFifth_, dottedListSixth_, dottedListSeventh_, dottedListEighth_, dottedListNinth_, dottedListTenth_, dottedListLength_, dottedListLast_];
})();

/**
 * Whether something is a pair, i.e., a cons cell.
 *
 * Similar to [`pair?` in Racket][rkt:pairp] and
 * [`consp` in Common Lisp][cl:consp].
 *
 * [rkt:pairp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._pair~3f%29%29
 * [cl:consp]: http://clhs.lisp.se/Body/f_consp.htm
 */
function pairp_(x: any): any {
  // All lists except the empty list qualify as pairs.
  return Array.isArray(x) && (x.length > 0);
}

pairp_.fsource = [Symbol.for('define'), [Symbol.for('pair?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>'), [Symbol.for('array-length'), Symbol.for('x')], 0]]];

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

nullp_.fsource = [Symbol.for('define'), [Symbol.for('null?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('='), [Symbol.for('array-length'), Symbol.for('x')], 0]]];

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

listp_.fsource = [Symbol.for('define'), [Symbol.for('list?_'), Symbol.for('x')], [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]];

/**
 * Whether something is a pair or a list.
 *
 * Similar to [`listp` in Common Lisp][cl:listp] and
 * [`listp` in Emacs Lisp], which are not as
 * rigorous as `list?` in Scheme.
 *
 * [cl:listp]: http://clhs.lisp.se/Body/f_listp.htm#listp
 * [el:listp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/List_002drelated-Predicates.html#index-listp
 */
function pairOrListP_(x: any): any {
  return (Array.isArray(x) && (x.length > 0)) || (Array.isArray(x) && (x.length === 0));
}

pairOrListP_.fsource = [Symbol.for('define'), [Symbol.for('pair-or-list?_'), Symbol.for('x')], [Symbol.for('or'), [Symbol.for('pair?'), Symbol.for('x')], [Symbol.for('null?'), Symbol.for('x')]]];

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

list_.fsource = [Symbol.for('define'), [Symbol.for('list_'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')];

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
  // Create a regular list whenever possible;
  // otherwise create a dotted list.
  return [x, ...(Array.isArray(y) ? y : [Symbol.for('.'), y])];
}

cons_.fsource = [Symbol.for('define'), [Symbol.for('cons_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote-splicing'), [Symbol.for('dotted-list-link'), Symbol.for('y')]]]]];

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
    const tailLst: any = args.at(-1);
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

listStar_.fsource = [Symbol.for('define'), [Symbol.for('list-star_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('length'), Symbol.for('args')], 0], undefined], [[Symbol.for('='), [Symbol.for('length'), Symbol.for('args')], 1], [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('tail-lst'), [Symbol.for('last'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('head-lst'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('cond'), [[Symbol.for('pair-or-list?'), Symbol.for('tail-lst')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('head-lst')], [Symbol.for('unquote-splicing'), Symbol.for('tail-lst')]]]], [Symbol.for('else'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('head-lst')], Symbol.for('.'), [Symbol.for('unquote'), Symbol.for('tail-lst')]]]]]]]];

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

buildList_.fsource = [Symbol.for('define'), [Symbol.for('build-list_'), Symbol.for('n'), Symbol.for('proc')], [Symbol.for('map'), Symbol.for('proc'), [Symbol.for('range'), 0, Symbol.for('n')]]];

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

makeList_.fsource = [Symbol.for('define'), [Symbol.for('make-list_'), Symbol.for('k'), Symbol.for('v')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quote'), []]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, Symbol.for('k')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('v')]], Symbol.for('result')]];

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

append_.fsource = [Symbol.for('define'), [Symbol.for('append_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('append'), Symbol.for('acc'), Symbol.for('x')]], [Symbol.for('quote'), []], Symbol.for('args')]];

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

flatten_.fsource = [Symbol.for('define'), [Symbol.for('flatten_'), Symbol.for('lst')], [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('x'), Symbol.for('acc')], [Symbol.for('cond'), [[Symbol.for('pair-or-list?'), Symbol.for('x')], [Symbol.for('append'), Symbol.for('acc'), [Symbol.for('flatten_'), Symbol.for('x')]]], [[Symbol.for('eq?'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('.')]], Symbol.for('acc')], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('acc'), Symbol.for('x')]]]], [Symbol.for('quote'), []], Symbol.for('lst')]];

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

first_.fsource = [Symbol.for('define'), [Symbol.for('first_'), Symbol.for('lst')], [Symbol.for('array-first'), Symbol.for('lst')]];

/**
 * Return the second element of a list.
 *
 * Similar to [`second` in Racket][rkt:second].
 *
 * [rkt:second]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._second%29%29
 */
function second_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListSecond(lst);
  } else {
    return lst[1];
  }
}

second_.fsource = [Symbol.for('define'), [Symbol.for('second_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-second'), Symbol.for('lst')], [Symbol.for('array-second'), Symbol.for('lst')]]];

/**
 * Return the third element of a list.
 *
 * Similar to [`third` in Racket][rkt:third].
 *
 * [rkt:third]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._third%29%29
 */
function third_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListThird(lst);
  } else {
    return lst[2];
  }
}

third_.fsource = [Symbol.for('define'), [Symbol.for('third_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-third'), Symbol.for('lst')], [Symbol.for('array-third'), Symbol.for('lst')]]];

/**
 * Return the fourth element of a list.
 *
 * Similar to [`fourth` in Racket][rkt:fourth].
 *
 * [rkt:fourth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fourth%29%29
 */
function fourth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListFourth(lst);
  } else {
    return lst[3];
  }
}

fourth_.fsource = [Symbol.for('define'), [Symbol.for('fourth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-fourth'), Symbol.for('lst')], [Symbol.for('array-fourth'), Symbol.for('lst')]]];

/**
 * Return the fifth element of a list.
 *
 * Similar to [`fifth` in Racket][rkt:fifth].
 *
 * [rkt:fifth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fifth%29%29
 */
function fifth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListFifth(lst);
  } else {
    return lst[4];
  }
}

fifth_.fsource = [Symbol.for('define'), [Symbol.for('fifth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-fifth'), Symbol.for('lst')], [Symbol.for('array-fifth'), Symbol.for('lst')]]];

/**
 * Return the sixth element of a list.
 *
 * Similar to [`sixth` in Racket][rkt:sixth].
 *
 * [rkt:sixth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._sixth%29%29
 */
function sixth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListSixth(lst);
  } else {
    return lst[5];
  }
}

sixth_.fsource = [Symbol.for('define'), [Symbol.for('sixth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-sixth'), Symbol.for('lst')], [Symbol.for('array-sixth'), Symbol.for('lst')]]];

/**
 * Return the seventh element of a list.
 *
 * Similar to [`seventh` in Racket][rkt:seventh].
 *
 * [rkt:seventh]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._seventh%29%29
 */
function seventh_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListSeventh(lst);
  } else {
    return lst[6];
  }
}

seventh_.fsource = [Symbol.for('define'), [Symbol.for('seventh_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-seventh'), Symbol.for('lst')], [Symbol.for('array-seventh'), Symbol.for('lst')]]];

/**
 * Return the eighth element of a list.
 *
 * Similar to [`eighth` in Racket][rkt:eighth].
 *
 * [rkt:eighth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._eighth%29%29
 */
function eighth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListEighth(lst);
  } else {
    return lst[7];
  }
}

eighth_.fsource = [Symbol.for('define'), [Symbol.for('eighth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-eighth'), Symbol.for('lst')], [Symbol.for('array-eighth'), Symbol.for('lst')]]];

/**
 * Return the ninth element of a list.
 *
 * Similar to [`ninth` in Racket][rkt:ninth].
 *
 * [rkt:ninth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._ninth%29%29
 */
function ninth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListNinth(lst);
  } else {
    return lst[8];
  }
}

ninth_.fsource = [Symbol.for('define'), [Symbol.for('ninth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-ninth'), Symbol.for('lst')], [Symbol.for('array-ninth'), Symbol.for('lst')]]];

/**
 * Return the tenth element of a list.
 *
 * Similar to [`tenth` in Racket][rkt:tenth].
 *
 * [rkt:tenth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._tenth%29%29
 */
function tenth_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListTenth(lst);
  } else {
    return lst[9];
  }
}

tenth_.fsource = [Symbol.for('define'), [Symbol.for('tenth_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-tenth'), Symbol.for('lst')], [Symbol.for('array-tenth'), Symbol.for('lst')]]];

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

cdr_.fsource = [Symbol.for('define'), [Symbol.for('cdr_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-pair?'), Symbol.for('lst')], [Symbol.for('array-third'), Symbol.for('lst')], [Symbol.for('array-rest'), Symbol.for('lst')]]];

/**
 * Return the tail of a list.
 *
 * Similar to [`rest` in Racket][rkt:rest].
 *
 * [rkt:rest]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._rest%29%29
 */
function rest_(lst: any): any {
  if (Array.isArray(lst) && (lst.length === 3) && (lst[1] === Symbol.for('.'))) {
    return lst[2];
  } else {
    return lst.slice(1);
  }
}

rest_.fsource = [Symbol.for('define'), [Symbol.for('rest_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-pair?'), Symbol.for('lst')], [Symbol.for('array-third'), Symbol.for('lst')], [Symbol.for('array-rest'), Symbol.for('lst')]]];

/**
 * Access the list element indicated by
 * one or more `indices`.
 */
function listRef_(lst: any, ...indices: any[]): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListRef_(lst, ...indices);
  } else {
    let result: any = lst;
    for (let i of indices) {
      result = (lst as any)[i];
    }
    return result;
  }
}

listRef_.fsource = [Symbol.for('define'), [Symbol.for('list-ref_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices')], [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('apply'), Symbol.for('dotted-list-ref_'), Symbol.for('lst'), Symbol.for('indices')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('indices')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('array-ref'), Symbol.for('lst'), Symbol.for('i')]]], Symbol.for('result')]]];

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
  return listRef_(lst, n);
}

nth_.fsource = [Symbol.for('define'), [Symbol.for('nth_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('list-ref_'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Set a list position to a given value.
 * Returns a new list.
 */
function listSet_(lst: any, ...indicesAndValue: any[]): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListSet_(lst, ...indicesAndValue);
  } else {
    let result: any = [...lst];
    if (indicesAndValue.length > 2) {
      const [pos, ...indicesAndValue1]: any[] = indicesAndValue;
      (result as any)[pos] = listSet_((result as any)[pos], ...indicesAndValue1);
    } else {
      const [pos, val]: any[] = indicesAndValue;
      (result as any)[pos] = val;
    }
    return result;
  }
}

listSet_.fsource = [Symbol.for('define'), [Symbol.for('list-set_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('apply'), Symbol.for('dotted-list-set_'), Symbol.for('lst'), Symbol.for('indices-and-value')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('lst')]]]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('length'), Symbol.for('indices-and-value')], 2], [Symbol.for('define-values'), [Symbol.for('pos'), Symbol.for('.'), Symbol.for('indices-and-value-1')], Symbol.for('indices-and-value')], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('pos'), [Symbol.for('apply'), Symbol.for('list-set_'), [Symbol.for('array-ref'), Symbol.for('result'), Symbol.for('pos')], Symbol.for('indices-and-value-1')]]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('pos'), Symbol.for('val')], Symbol.for('indices-and-value')], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('pos'), Symbol.for('val')]]], Symbol.for('result')]]];

/**
 * Set a list position to a given value.
 * Modifies the original list.
 */
function listSetX_(lst: any, ...indicesAndValue: any[]): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListSetX_(lst, ...indicesAndValue);
  } else {
    const indices: any = indicesAndValue.slice(0, -1);
    const firstIndices: any = indices.slice(0, -1);
    let lastIndex: any = indices.at(-1);
    const value: any = indicesAndValue.at(-1);
    let lst1: any = lst;
    for (let i of firstIndices) {
      lst1 = (lst1 as any)[i];
    }
    (lst1 as any)[lastIndex] = value;
    return value;
  }
}

listSetX_.fsource = [Symbol.for('define'), [Symbol.for('list-set!_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('apply'), Symbol.for('dotted-list-set!_'), Symbol.for('lst'), Symbol.for('indices-and-value')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), Symbol.for('indices-and-value'), 1]], [Symbol.for('define'), Symbol.for('first-indices'), [Symbol.for('drop-right'), Symbol.for('indices'), 1]], [Symbol.for('define'), Symbol.for('last-index'), [Symbol.for('last'), Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('last'), Symbol.for('indices-and-value')]], [Symbol.for('define'), Symbol.for('lst1'), Symbol.for('lst')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('first-indices')]], [Symbol.for('set!'), Symbol.for('lst1'), [Symbol.for('list-ref'), Symbol.for('lst1'), Symbol.for('i')]]], [Symbol.for('array-set!'), Symbol.for('lst1'), Symbol.for('last-index'), Symbol.for('value')], Symbol.for('value')]]];

/**
 * Return the `n`-th CDR element of a list.
 */
function listTail_(lst: any, n: any): any {
  let result: any = lst;
  let i: any = n;
  while (i > 0) {
    result = ((result.length === 3) && (result[1] === Symbol.for('.'))) ? result[2] : result.slice(1);
    i--;
  }
  return result;
}

listTail_.fsource = [Symbol.for('define'), [Symbol.for('list-tail_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('i'), Symbol.for('n')], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('cdr'), Symbol.for('result')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('result')];

/**
 * Return the `n`-th CDR element of a list.
 *
 * Similar to [`nthcdr` in Common Lisp][cl:nthcdr].
 *
 * [cl:nth]: http://clhs.lisp.se/Body/f_nthcdr.htm#nthcdr
 */
function nthcdr_(n: any, lst: any): any {
  let result: any = lst;
  let i: any = n;
  while (i > 0) {
    result = ((result.length === 3) && (result[1] === Symbol.for('.'))) ? result[2] : result.slice(1);
    i--;
  }
  return result;
}

nthcdr_.fsource = [Symbol.for('define'), [Symbol.for('nthcdr_'), Symbol.for('n'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('i'), Symbol.for('n')], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('cdr'), Symbol.for('result')]], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('result')];

/**
 * Take the `n` first elements from `lst`.
 *
 * Similar to [`take` in Racket][rkt:take].
 *
 * [rkt:take]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._take%29%29
 */
function take_(lst: any, n: any): any {
  return lst.slice(0, -(lst.length - n) || undefined);
}

take_.fsource = [Symbol.for('define'), [Symbol.for('take_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('drop-right'), Symbol.for('lst'), [Symbol.for('-'), [Symbol.for('length'), Symbol.for('lst')], Symbol.for('n')]]];

/**
 * Return the list obtained by dropping
 * the first `n` elements from `lst`.
 *
 * Similar to [`drop` in Racket][rkt:drop].
 *
 * [rkt:drop]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop%29%29
 */
function drop_(lst: any, n: any): any {
  return lst.slice(n);
}

drop_.fsource = [Symbol.for('define'), [Symbol.for('drop_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-drop'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Return the list obtained by dropping
 * the last `n` elements from `lst`.
 *
 * Similar to [`drop-right` in Racket][rkt:drop-right].
 *
 * [rkt:drop-right]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop-right%29%29
 */
function dropRight_(lst: any, n: any): any {
  return lst.slice(0, -n || undefined);
}

dropRight_.fsource = [Symbol.for('define'), [Symbol.for('drop-right_'), Symbol.for('lst'), Symbol.for('n')], [Symbol.for('array-drop-right'), Symbol.for('lst'), Symbol.for('n')]];

/**
 * Reverse the order of a list.
 * Returns a new list.
 *
 * Similar to [`reverse` in Racket][rkt:reverse].
 *
 * [rkt:reverse]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._reverse%29%29
 */
function reverse_(lst: any): any {
  return [...lst].reverse();
}

reverse_.fsource = [Symbol.for('define'), [Symbol.for('reverse_'), Symbol.for('lst')], [Symbol.for('array-reverse'), Symbol.for('lst')]];

/**
 * Reverse the order of a list.
 */
function reversex_(lst: any): any {
  return lst.reverse();
}

reversex_.fsource = [Symbol.for('define'), [Symbol.for('reverse!_'), Symbol.for('lst')], [Symbol.for('array-reverse!'), Symbol.for('lst')]];

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

butlast_.fsource = [Symbol.for('define'), [Symbol.for('butlast_'), Symbol.for('x'), [Symbol.for('n'), 1]], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('x')]]]], [Symbol.for('i'), Symbol.for('n')]], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('>'), [Symbol.for('length'), Symbol.for('result')], 0]], [Symbol.for('pop-right!'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('result')]];

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

nbutlast_.fsource = [Symbol.for('define'), [Symbol.for('nbutlast_'), Symbol.for('x'), [Symbol.for('n'), 1]], [Symbol.for('let'), [[Symbol.for('i'), Symbol.for('n')]], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('>'), [Symbol.for('length'), Symbol.for('x')], 0]], [Symbol.for('pop-right!'), Symbol.for('x')], [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), 1]]], Symbol.for('x')]];

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

popLeftX_.fsource = [Symbol.for('define'), [Symbol.for('pop-left!_'), Symbol.for('lst')], [Symbol.for('array-pop-left!'), Symbol.for('lst')]];

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

popRightX_.fsource = [Symbol.for('define'), [Symbol.for('pop-right!_'), Symbol.for('lst')], [Symbol.for('array-pop-right!'), Symbol.for('lst')]];

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

pushLeftX_.fsource = [Symbol.for('define'), [Symbol.for('push-left!_'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('array-push-left!'), Symbol.for('lst'), Symbol.for('x')]];

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

pushRightX_.fsource = [Symbol.for('define'), [Symbol.for('push-right!_'), Symbol.for('lst'), Symbol.for('x')], [Symbol.for('array-push-right!'), Symbol.for('lst'), Symbol.for('x')]];

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
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListLength(lst);
  } else {
    return lst.length;
  }
}

length_.fsource = [Symbol.for('define'), [Symbol.for('length_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-length'), Symbol.for('lst')], [Symbol.for('array-length'), Symbol.for('lst')]]];

/**
 * Return the last element of a list.
 *
 * Similar to [`last` in Racket][rkt:last].
 *
 * [rkt:last]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last%29%29
 */
function last_(lst: any): any {
  if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    return dottedListLast(lst);
  } else {
    return lst.at(-1);
  }
}

last_.fsource = [Symbol.for('define'), [Symbol.for('last_'), Symbol.for('lst')], [Symbol.for('if'), [Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('dotted-list-last'), Symbol.for('lst')], [Symbol.for('array-last'), Symbol.for('lst')]]];

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
  } else if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    let current: any = lst;
    let result: any = undefined;
    while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.')) && !((x: any): any => {
      return Array.isArray(x) && (x.length === 0);
    })(current.at(-1))) {
      current = current.at(-1);
    }
    return result;
  } else {
    return lst.slice(lst.length - 1);
  }
}

lastPair_.fsource = [Symbol.for('define'), [Symbol.for('last-pair_'), Symbol.for('lst')], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('pair-or-list?'), Symbol.for('lst')]], undefined], [[Symbol.for('null?'), Symbol.for('lst')], Symbol.for('lst')], [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('current'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), undefined], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('dotted-list?'), Symbol.for('current')], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('dotted-list-tail'), Symbol.for('current')]]]], [Symbol.for('set!'), Symbol.for('current'), [Symbol.for('dotted-list-tail'), Symbol.for('current')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('array-drop'), Symbol.for('lst'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 1]]]]];

/**
 * Return the last cdr of a list, i.e., the terminating empty list.
 */
function lastCdr_(lst: any): any {
  if (!Array.isArray(lst)) {
    return undefined;
  } else if (Array.isArray(lst) && (lst.length >= 3) && (lst.at(-2) === Symbol.for('.'))) {
    let result: any = lst;
    while (Array.isArray(result) && (result.length >= 3) && (result.at(-2) === Symbol.for('.'))) {
      result = result.at(-1);
    }
    return result;
  } else {
    return [];
  }
}

lastCdr_.fsource = [Symbol.for('define'), [Symbol.for('last-cdr_'), Symbol.for('lst')], [Symbol.for('cond'), [[Symbol.for('not'), [Symbol.for('pair-or-list?'), Symbol.for('lst')]], undefined], [[Symbol.for('dotted-list?'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('while'), [Symbol.for('dotted-list?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('dotted-list-tail'), Symbol.for('result')]]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('quote'), []]]]];

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

setCarX_.fsource = [Symbol.for('define'), [Symbol.for('set-car!_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('length'), Symbol.for('x')], 0], [Symbol.for('list-set!'), Symbol.for('x'), 0, Symbol.for('y')]], undefined];

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
    if (Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.'))) {
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

setCdrX_.fsource = [Symbol.for('define'), [Symbol.for('set-cdr!_'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('null?'), Symbol.for('x')]], [[Symbol.for('eq?'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('cond'), [[Symbol.for('dotted-list?'), Symbol.for('x')], [Symbol.for('array-set!'), Symbol.for('x'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('x')], 1], Symbol.for('y')]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('.')]], [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('y')]]]], [Symbol.for('else'), [Symbol.for('while'), [Symbol.for('>'), [Symbol.for('array-length'), Symbol.for('x')], 1], [Symbol.for('pop-right!'), Symbol.for('x')]], [Symbol.for('cond'), [[Symbol.for('pair-or-list?'), Symbol.for('y')], [Symbol.for('for'), [[Symbol.for('z'), Symbol.for('y')]], [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('z')]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('x'), [Symbol.for('quote'), Symbol.for('.')]], [Symbol.for('push-right!'), Symbol.for('x'), Symbol.for('y')]]]]], undefined];

/**
 * Whether something is a dotted list.
 *
 * Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
 *
 * [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
 */
function dottedListP_(x: any): any {
  return Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.'));
}

dottedListP_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('eq?'), [Symbol.for('array-nlast'), Symbol.for('x'), 2], [Symbol.for('quote'), Symbol.for('.')]]]];

/**
 * Whether something is a dotted pair.
 */
function dottedPairP_(x: any): any {
  return Array.isArray(x) && (x.length === 3) && (x[1] === Symbol.for('.'));
}

dottedPairP_.fsource = [Symbol.for('define'), [Symbol.for('dotted-pair?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('eq?'), [Symbol.for('array-ref'), Symbol.for('x'), 1], [Symbol.for('quote'), Symbol.for('.')]]]];

/**
 * Whether something is a proper dotted list.
 */
function dottedProperListP_(x: any): any {
  return Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.')) && ((x: any): any => {
    return Array.isArray(x) && (x.length === 0);
  })(lastCdr(x));
}

dottedProperListP_.fsource = [Symbol.for('define'), [Symbol.for('dotted-proper-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('eq?'), [Symbol.for('array-nlast'), Symbol.for('x'), 2], [Symbol.for('quote'), Symbol.for('.')]], [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]]];

/**
 * Whether something is an improper dotted list.
 */
function dottedImproperListP_(x: any): any {
  return Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.')) && !((x: any): any => {
    return Array.isArray(x) && (x.length === 0);
  })(lastCdr(x));
}

dottedImproperListP_.fsource = [Symbol.for('define'), [Symbol.for('dotted-improper-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('array?'), Symbol.for('x')], [Symbol.for('>='), [Symbol.for('array-length'), Symbol.for('x')], 3], [Symbol.for('eq?'), [Symbol.for('array-nlast'), Symbol.for('x'), 2], [Symbol.for('quote'), Symbol.for('.')]], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]]]];

/**
 * Return the head of a dotted list.
 */
function dottedListHead_(lst: any): any {
  return lst.slice(0, -2);
}

dottedListHead_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-head_'), Symbol.for('lst')], [Symbol.for('array-drop-right'), Symbol.for('lst'), 2]];

/**
 * Return the tail of a dotted list.
 */
function dottedListTail_(lst: any): any {
  return lst.at(-1);
}

dottedListTail_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-tail_'), Symbol.for('lst')], [Symbol.for('array-last'), Symbol.for('lst')]];

/**
 * Create a dotted list link.
 */
function dottedListLink_(x: any): any {
  if (Array.isArray(x)) {
    return x;
  } else {
    return [Symbol.for('.'), x];
  }
}

dottedListLink_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-link_'), Symbol.for('x')], [Symbol.for('if'), [Symbol.for('pair-or-list?'), Symbol.for('x')], Symbol.for('x'), [Symbol.for('list'), [Symbol.for('quote'), Symbol.for('.')], Symbol.for('x')]]];

/**
 * Return the CDR of a dotted pair.
 */
function dottedPairCdr_(x: any): any {
  return x[2];
}

dottedPairCdr_.fsource = [Symbol.for('define'), [Symbol.for('dotted-pair-cdr_'), Symbol.for('x')], [Symbol.for('array-third'), Symbol.for('x')]];

/**
 * Parse a dotted list.
 */
function dottedListParse_(lst: any): any {
  return [dottedListHead_(lst), dottedListTail_(lst)];
}

dottedListParse_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-parse_'), Symbol.for('lst')], [Symbol.for('values'), [Symbol.for('dotted-list-head_'), Symbol.for('lst')], [Symbol.for('dotted-list-tail_'), Symbol.for('lst')]]];

/**
 * Return the length of a dotted list.
 */
function dottedListLength_(lst: any): any {
  let len: any = 0;
  let current: any = lst;
  while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.'))) {
    len = len + (lst.length - 2);
    current = current.at(-1);
  }
  return len;
}

dottedListLength_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-length_'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('len'), 0], [Symbol.for('define'), Symbol.for('current'), Symbol.for('lst')], [Symbol.for('while'), [Symbol.for('dotted-list?'), Symbol.for('current')], [Symbol.for('set!'), Symbol.for('len'), [Symbol.for('+'), Symbol.for('len'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]]], [Symbol.for('set!'), Symbol.for('current'), [Symbol.for('dotted-list-tail'), Symbol.for('current')]]], Symbol.for('len')];

/**
 * Access the dotted list element indicated by
 * one or more `indices`.
 */
function dottedListRef_(lst: any, ...indices: any[]): any {
  let result: any = lst;
  for (let i of indices) {
    while (i > 0) {
      if (i < (result.length - 2)) {
        break;
      } else {
        i = i - (result.length - 2);
        result = result.at(-1);
      }
    }
    if (Array.isArray(result)) {
      result = (result as any)[i];
    }
  }
  return result;
}

dottedListRef_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('lst')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('indices')]], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('result')], 2]], [Symbol.for('break')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('result')], 2]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('dotted-list-tail'), Symbol.for('result')]]]]], [Symbol.for('when'), [Symbol.for('pair-or-list?'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('array-ref'), Symbol.for('result'), Symbol.for('i')]]]], Symbol.for('result')];

/**
 * Set a dotted list position to a given value.
 * Returns a new list.
 */
function dottedListSet_(lst: any, ...indicesAndValue: any[]): any {
  if (indicesAndValue.length > 2) {
    const [pos, ...indicesAndValue1]: any[] = indicesAndValue;
    if (pos < (lst.length - 2)) {
      let result: any = [...lst];
      (result as any)[pos] = dottedListSet_((result as any)[pos], ...indicesAndValue1);
      return result;
    } else {
      return [...lst.slice(0, -1), dottedListSet_(lst.at(-1), [pos - (lst.length - 2), ...indicesAndValue1])];
    }
  } else {
    const [pos, val]: any[] = indicesAndValue;
    if (pos < (lst.length - 2)) {
      let result: any = [...lst];
      (result as any)[pos] = val;
      return result;
    } else {
      return [...lst.slice(0, -1), dottedListSet_(lst.at(-1), pos - (lst.length - 2), val)];
    }
  }
}

dottedListSet_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-set_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('length'), Symbol.for('indices-and-value')], 2], [Symbol.for('define-values'), [Symbol.for('pos'), Symbol.for('.'), Symbol.for('indices-and-value-1')], Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('pos'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('lst')]]]], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('pos'), [Symbol.for('apply'), Symbol.for('dotted-list-set_'), [Symbol.for('array-ref'), Symbol.for('result'), Symbol.for('pos')], Symbol.for('indices-and-value-1')]], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('append'), [Symbol.for('array-drop-right'), Symbol.for('lst'), 1], [Symbol.for('list'), [Symbol.for('dotted-list-set_'), [Symbol.for('array-last'), Symbol.for('lst')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), [Symbol.for('-'), Symbol.for('pos'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]]], [Symbol.for('unquote-splicing'), Symbol.for('indices-and-value-1')]]]]]]]]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('pos'), Symbol.for('val')], Symbol.for('indices-and-value')], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('pos'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('lst')]]]], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('pos'), Symbol.for('val')], Symbol.for('result')], [Symbol.for('else'), [Symbol.for('append'), [Symbol.for('array-drop-right'), Symbol.for('lst'), 1], [Symbol.for('list'), [Symbol.for('dotted-list-set_'), [Symbol.for('array-last'), Symbol.for('lst')], [Symbol.for('-'), Symbol.for('pos'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst')], 2]], Symbol.for('val')]]]]]]]];

/**
 * Set a dotted list position to a given value.
 * Modifies the original list.
 */
function dottedListSetX_(lst: any, ...indicesAndValue: any[]): any {
  const indices: any = indicesAndValue.slice(0, -1);
  const indices1: any = indices.slice(0, -1);
  let lastIndex: any = indices.at(-1);
  const value: any = indicesAndValue.at(-1);
  let lst1: any = lst;
  for (let i of indices1) {
    while (i > 0) {
      if (i < (lst1.length - 2)) {
        break;
      } else {
        i = i - (lst1.length - 2);
        lst1 = lst1.at(-1);
      }
    }
    if (Array.isArray(lst1)) {
      lst1 = (lst1 as any)[i];
    }
  }
  while (lastIndex > 0) {
    if (lastIndex < (lst1.length - 2)) {
      break;
    } else {
      lastIndex = lastIndex - (lst1.length - 2);
      lst1 = lst1.at(-1);
    }
  }
  (lst1 as any)[lastIndex] = value;
  return value;
}

dottedListSetX_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-set!_'), Symbol.for('lst'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), Symbol.for('indices-and-value'), 1]], [Symbol.for('define'), Symbol.for('indices1'), [Symbol.for('drop-right'), Symbol.for('indices'), 1]], [Symbol.for('define'), Symbol.for('last-index'), [Symbol.for('last'), Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('last'), Symbol.for('indices-and-value')]], [Symbol.for('define'), Symbol.for('lst1'), Symbol.for('lst')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('indices1')]], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('i'), 0], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst1')], 2]], [Symbol.for('break')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('-'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst1')], 2]]], [Symbol.for('set!'), Symbol.for('lst1'), [Symbol.for('dotted-list-tail'), Symbol.for('lst1')]]]]], [Symbol.for('when'), [Symbol.for('pair-or-list?'), Symbol.for('lst1')], [Symbol.for('set!'), Symbol.for('lst1'), [Symbol.for('list-ref'), Symbol.for('lst1'), Symbol.for('i')]]]], [Symbol.for('while'), [Symbol.for('>'), Symbol.for('last-index'), 0], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('last-index'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst1')], 2]], [Symbol.for('break')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('last-index'), [Symbol.for('-'), Symbol.for('last-index'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('lst1')], 2]]], [Symbol.for('set!'), Symbol.for('lst1'), [Symbol.for('dotted-list-tail'), Symbol.for('lst1')]]]]], [Symbol.for('array-set!'), Symbol.for('lst1'), Symbol.for('last-index'), Symbol.for('value')], Symbol.for('value')];

/**
 * Return the first element of a dotted list.
 */
function dottedListFirst_(lst: any): any {
  return lst[0];
}

dottedListFirst_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-first_'), Symbol.for('lst')], [Symbol.for('array-first'), Symbol.for('lst')]];

/**
 * Return the second element of a dotted list.
 */
function dottedListSecond_(lst: any): any {
  return dottedListRef_(lst, 1);
}

dottedListSecond_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-second_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 1]];

/**
 * Return the third element of a dotted list.
 */
function dottedListThird_(lst: any): any {
  return dottedListRef_(lst, 2);
}

dottedListThird_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-third_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 2]];

/**
 * Return the fourth element of a dotted list.
 */
function dottedListFourth_(lst: any): any {
  return dottedListRef_(lst, 3);
}

dottedListFourth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-fourth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 3]];

/**
 * Return the fifth element of a dotted list.
 */
function dottedListFifth_(lst: any): any {
  return dottedListRef_(lst, 4);
}

dottedListFifth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-fifth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 4]];

/**
 * Return the sixth element of a dotted list.
 */
function dottedListSixth_(lst: any): any {
  return dottedListRef_(lst, 5);
}

dottedListSixth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-sixth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 5]];

/**
 * Return the seventh element of a dotted list.
 */
function dottedListSeventh_(lst: any): any {
  return dottedListRef_(lst, 6);
}

dottedListSeventh_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-seventh_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 6]];

/**
 * Return the eighth element of a dotted list.
 */
function dottedListEighth_(lst: any): any {
  return dottedListRef_(lst, 7);
}

dottedListEighth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-eighth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 7]];

/**
 * Return the ninth element of a dotted list.
 */
function dottedListNinth_(lst: any): any {
  return dottedListRef_(lst, 8);
}

dottedListNinth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-ninth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 8]];

/**
 * Return the tenth element of a dotted list.
 */
function dottedListTenth_(lst: any): any {
  return dottedListRef_(lst, 9);
}

dottedListTenth_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-tenth_'), Symbol.for('lst')], [Symbol.for('dotted-list-ref_'), Symbol.for('lst'), 9]];

/**
 * Return the last element of a dotted list.
 */
function dottedListLast_(lst: any): any {
  let current: any = lst;
  let result: any = undefined;
  while (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.')) && !((x: any): any => {
    return Array.isArray(x) && (x.length === 0);
  })(((current.length === 3) && (current[1] === Symbol.for('.'))) ? current[2] : current.slice(1))) {
    current = ((current.length === 3) && (current[1] === Symbol.for('.'))) ? current[2] : current.slice(1);
  }
  if (Array.isArray(current) && (current.length >= 3) && (current.at(-2) === Symbol.for('.'))) {
    result = current[current.length - 3];
  }
  return result;
}

dottedListLast_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list-last_'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('current'), Symbol.for('lst')], [Symbol.for('define'), Symbol.for('result'), undefined], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('dotted-list?'), Symbol.for('current')], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('dotted-list-cdr'), Symbol.for('current')]]]], [Symbol.for('set!'), Symbol.for('current'), [Symbol.for('dotted-list-cdr'), Symbol.for('current')]]], [Symbol.for('when'), [Symbol.for('dotted-list?'), Symbol.for('current')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list-ref'), Symbol.for('current'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('current')], 3]]]], Symbol.for('result')];

/**
 * Make a dotted list.
 */
function makeDottedList_(car: any, cdr: any): any {
  return listStar_(car, cdr);
}

makeDottedList_.fsource = [Symbol.for('define'), [Symbol.for('make-dotted-list_'), Symbol.for('car'), Symbol.for('cdr')], [Symbol.for('list-star_'), Symbol.for('car'), Symbol.for('cdr')]];

/**
 * Make a dotted pair.
 */
function makePair_(car: any, cdr: any): any {
  return [car, Symbol.for('.'), cdr];
}

makePair_.fsource = [Symbol.for('define'), [Symbol.for('make-pair_'), Symbol.for('car'), Symbol.for('cdr')], [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('car')], Symbol.for('.'), [Symbol.for('unquote'), Symbol.for('cdr')]]]];

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

properListP_.fsource = [Symbol.for('define'), [Symbol.for('proper-list?_'), Symbol.for('x')], [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]];

/**
 * Whether something is an improper list,
 * i.e., a dotted list.
 *
 * Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
 *
 * [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
 */
function improperListP_(x: any): any {
  return !((x: any): any => {
    return Array.isArray(x) && (x.length === 0);
  })(lastCdr(x));
}

improperListP_.fsource = [Symbol.for('define'), [Symbol.for('improper-list?_'), Symbol.for('x')], [Symbol.for('not'), [Symbol.for('null?'), [Symbol.for('last-cdr'), Symbol.for('x')]]]];

/**
 * Whether something is a circular list.
 *
 * Similar to [`circular-list?` in Racket][rkt:circular-list-p].
 *
 * [rkt:circular-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#circular-list-p
 */
function circularListP_(x: any): any {
  return Array.isArray(x) && (x.length >= 3) && (x.at(-2) === Symbol.for('.')) && (x.at(-1) === x);
}

circularListP_.fsource = [Symbol.for('define'), [Symbol.for('circular-list?_'), Symbol.for('x')], [Symbol.for('and'), [Symbol.for('dotted-list?'), Symbol.for('x')], [Symbol.for('eq?'), [Symbol.for('dotted-list-tail'), Symbol.for('x')], Symbol.for('x')]]];

/**
 * Convert an array list to a linked list.
 */
function listToDottedList_(x: any): any {
  return [...x.slice(0, -1), Symbol.for('.'), x.at(-1)];
}

listToDottedList_.fsource = [Symbol.for('define'), [Symbol.for('list->dotted-list_'), Symbol.for('x')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('array-drop-right'), Symbol.for('x'), 1]], Symbol.for('.'), [Symbol.for('unquote'), [Symbol.for('array-last'), Symbol.for('x')]]]]];

/**
 * Convert a linked list to an array list.
 */
function dottedListToList_(x: any): any {
  return [...x.slice(0, -2), x.at(-1)];
}

dottedListToList_.fsource = [Symbol.for('define'), [Symbol.for('dotted-list->list_'), Symbol.for('x')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('dotted-list-head'), Symbol.for('x')]], [Symbol.for('unquote'), [Symbol.for('dotted-list-tail'), Symbol.for('x')]]]]];

export {
  append_ as append,
  buildList_ as buildList,
  butlast_ as butlast,
  cdr_ as cdr,
  cdr_ as tail,
  cdr_ as tail_,
  circularListP_ as circularListP,
  cons_ as cons,
  dottedListP_ as dottedListP,
  dropRight_ as dropRight,
  drop_ as drop,
  eighth_ as eighth,
  fifth_ as fifth,
  first_ as car,
  first_ as car_,
  first_ as first,
  first_ as head,
  first_ as head_,
  flatten_ as flatten,
  fourth_ as fourth,
  improperListP_ as improperListP,
  lastCdr_ as dottedListLastCdr_,
  lastCdr_ as lastCdr,
  lastPair_ as lastCons_,
  lastPair_ as lastPair,
  last_ as last,
  length_ as length,
  listStar_ as listStar,
  listp_ as listp,
  listp_ as properListP,
  list_ as list,
  makeDottedList_ as makeDottedList,
  makeList_ as makeList,
  makePair_ as makePair,
  nbutlast_ as nbutlast,
  ninth_ as ninth,
  nth_ as dottedListNth,
  nth_ as dottedListNth_,
  nth_ as nth,
  nthcdr_ as dottedListNthcdr,
  nthcdr_ as dottedListNthcdr_,
  nthcdr_ as nthcdr,
  nullp_ as nullp,
  pairp_ as consp,
  pairp_ as consp_,
  pairp_ as pairp,
  popLeftX_ as popx,
  popLeftX_ as popx_,
  popLeftX_ as popLeftX,
  popRightX_ as popRightX,
  pushLeftX_ as pushx,
  pushLeftX_ as pushLeftX,
  pushRightX_ as appendToList,
  pushRightX_ as pushRightX,
  rest_ as rest,
  reverse_ as reverse,
  second_ as cadr_,
  second_ as second,
  setCarX_ as setCarX,
  setCdrX_ as setCdrX,
  seventh_ as seventh,
  sixth_ as sixth,
  take_ as take,
  tenth_ as tenth,
  third_ as third,
  append_,
  buildList_,
  butlast_,
  cdr_,
  circularListP_,
  cons_,
  dottedImproperListP_,
  dottedListToList_,
  dottedListEighth_,
  dottedListFifth_,
  dottedListFirst_,
  dottedListFourth_,
  dottedListHead_,
  dottedListLast_,
  dottedListLength_,
  dottedListNinth_,
  dottedListParse_,
  dottedListRef_,
  dottedListSecond_,
  dottedListSetX_,
  dottedListSet_,
  dottedListSeventh_,
  dottedListSixth_,
  dottedListTail_,
  dottedListTenth_,
  dottedListLink_,
  dottedListThird_,
  dottedListP_,
  dottedPairCdr_,
  dottedPairP_,
  dottedProperListP_,
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
  listToDottedList_,
  listRef_,
  listSetX_,
  listSet_,
  listStar_,
  listTail_,
  listp_,
  list_,
  makeDottedList_,
  makeList_,
  makePair_,
  nbutlast_,
  ninth_,
  nth_,
  nthcdr_,
  nullp_,
  pairOrListP_,
  pairp_,
  popLeftX_,
  popRightX_,
  properListP_,
  pushLeftX_,
  pushRightX_,
  rest_,
  reversex_,
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