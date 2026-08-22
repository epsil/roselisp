// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Arrays
 *
 * Array functions.
 *
 * ## Description
 *
 * Functions for working with arrays.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/**
 * Whether something is an array.
 */
function arrayp_(x: any): any {
  return Array.isArray(x);
}

arrayp_.fsource = [Symbol.for('define'), [Symbol.for('array?_'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), Symbol.for('x')]];

/**
 * Return the length of an array.
 */
function arrayLength_(arr: any): any {
  return arr.length;
}

arrayLength_.fsource = [Symbol.for('define'), [Symbol.for('array-length_'), Symbol.for('arr')], [Symbol.for('js/length'), Symbol.for('arr')]];

/**
 * Copy an array.
 */
function arrayCopy_(arr: any): any {
  return [...arr];
}

arrayCopy_.fsource = [Symbol.for('define'), [Symbol.for('array-copy_'), Symbol.for('arr')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('arr')]]]];

/**
 * Return the first element of an array.
 */
function arrayFirst_(arr: any): any {
  return arr[0];
}

arrayFirst_.fsource = [Symbol.for('define'), [Symbol.for('array-first_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 0]];

/**
 * Return the second element of an array.
 */
function arraySecond_(arr: any): any {
  return arr[1];
}

arraySecond_.fsource = [Symbol.for('define'), [Symbol.for('array-second_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 1]];

/**
 * Return the third element of an array.
 */
function arrayThird_(arr: any): any {
  return arr[2];
}

arrayThird_.fsource = [Symbol.for('define'), [Symbol.for('array-third_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 2]];

/**
 * Return the fourth element of an array.
 */
function arrayFourth_(arr: any): any {
  return arr[3];
}

arrayFourth_.fsource = [Symbol.for('define'), [Symbol.for('array-fourth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 3]];

/**
 * Return the fifth element of an array.
 */
function arrayFifth_(arr: any): any {
  return arr[4];
}

arrayFifth_.fsource = [Symbol.for('define'), [Symbol.for('array-fifth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 4]];

/**
 * Return the sixth element of an array.
 */
function arraySixth_(arr: any): any {
  return arr[5];
}

arraySixth_.fsource = [Symbol.for('define'), [Symbol.for('array-sixth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 5]];

/**
 * Return the seventh element of an array.
 */
function arraySeventh_(arr: any): any {
  return arr[6];
}

arraySeventh_.fsource = [Symbol.for('define'), [Symbol.for('array-seventh_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 6]];

/**
 * Return the eight element of an array.
 */
function arrayEighth_(arr: any): any {
  return arr[7];
}

arrayEighth_.fsource = [Symbol.for('define'), [Symbol.for('array-eighth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 7]];

/**
 * Return the ninth element of an array.
 */
function arrayNinth_(arr: any): any {
  return arr[8];
}

arrayNinth_.fsource = [Symbol.for('define'), [Symbol.for('array-ninth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 8]];

/**
 * Return the tenth element of an array.
 */
function arrayTenth_(arr: any): any {
  return arr[9];
}

arrayTenth_.fsource = [Symbol.for('define'), [Symbol.for('array-tenth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 9]];

/**
 * Return the last element of an array.
 */
function arrayLast_(arr: any): any {
  return arr.at(-1);
}

arrayLast_.fsource = [Symbol.for('define'), [Symbol.for('array-last_'), Symbol.for('arr')], [Symbol.for('array-at'), Symbol.for('arr'), -1]];

/**
 * Return the `n`-th element counting from
 * the end of the array.
 */
function arrayNlast_(arr: any, n: any): any {
  return arr.at(-n);
}

arrayNlast_.fsource = [Symbol.for('define'), [Symbol.for('array-nlast_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-at'), Symbol.for('arr'), [Symbol.for('-'), Symbol.for('n')]]];

/**
 * Access the array element indicated by
 * one or more `indices`.
 *
 * Similar to [`array-ref` in Racket][rkt:array-ref],
 * [`aref` in Common Lisp][cl:aref] and
 * [`aget` in ClojureScript][cljs:aget].
 *
 * [rkt:array-ref]: https://docs.racket-lang.org/array/index.html#%28def._%28%28lib._array%2Fmain..rkt%29._array-ref%29%29
 * [cl:aref]: http://clhs.lisp.se/Body/f_aref.htm#aref
 * [cljs:aget]: https://cljs.github.io/api/cljs.core/#aget
 */
function arrayRef_(arr: any, ...indices: any[]): any {
  let result: any = arr;
  for (let i of indices) {
    result = (arr as any)[i];
  }
  return result;
}

arrayRef_.fsource = [Symbol.for('define'), [Symbol.for('array-ref_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('arr')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('indices')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/get'), Symbol.for('arr'), Symbol.for('i')]]], Symbol.for('result')];

/**
 * Return the `i`-th element of the array.
 * Accepts negative values, counting back
 * from the end of the array.
 */
function arrayAt_(arr: any, i: any): any {
  return arr.at(i);
}

arrayAt_.fsource = [Symbol.for('define'), [Symbol.for('array-at_'), Symbol.for('arr'), Symbol.for('i')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('at'), Symbol.for('i')]];

/**
 * Set an array position to a given value.
 * Returns a new array.
 */
function arraySet_(arr: any, ...indicesAndValue: any[]): any {
  let result: any = [...arr];
  if (indicesAndValue.length > 2) {
    const [i, ...indicesAndValue1]: any[] = indicesAndValue;
    (result as any)[i] = arraySet_((result as any)[i], ...indicesAndValue1);
  } else {
    const [i, val]: any[] = indicesAndValue;
    (result as any)[i] = val;
  }
  return result;
}

arraySet_.fsource = [Symbol.for('define'), [Symbol.for('array-set_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('array-copy'), Symbol.for('arr')]], [Symbol.for('cond'), [[Symbol.for('>'), [Symbol.for('length'), Symbol.for('indices-and-value')], 2], [Symbol.for('define-values'), [Symbol.for('i'), Symbol.for('.'), Symbol.for('indices-and-value-1')], Symbol.for('indices-and-value')], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('i'), [Symbol.for('apply'), Symbol.for('array-set_'), [Symbol.for('array-ref'), Symbol.for('result'), Symbol.for('i')], Symbol.for('indices-and-value-1')]]], [Symbol.for('else'), [Symbol.for('define-values'), [Symbol.for('i'), Symbol.for('val')], Symbol.for('indices-and-value')], [Symbol.for('array-set!'), Symbol.for('result'), Symbol.for('i'), Symbol.for('val')]]], Symbol.for('result')];

/**
 * Set the array position indiciated by one or more indices
 * to a given value.
 *
 * Similar to [`array-set!` in Racket][rkt:array-set] and
 * [`aset` in ClojureScript][cljs:aset].
 *
 * [rkt:array-set]: https://docs.racket-lang.org/array/index.html#%28def._%28%28lib._array%2Fmain..rkt%29._array-set%21%29%29
 * [cljs:aset]: https://cljs.github.io/api/cljs.core/#aset
 */
function arraySetX_(arr: any, ...indicesAndValue: any[]): any {
  const indices: any = indicesAndValue.slice(0, -1);
  const firstIndices: any = indices.slice(0, -1);
  const lastIndex: any = indices.at(-1);
  const value: any = indicesAndValue.at(-1);
  let arr1: any = arr;
  for (let i of firstIndices) {
    arr1 = (arr1 as any)[i];
  }
  (arr1 as any)[lastIndex] = value;
  return value;
}

arraySetX_.fsource = [Symbol.for('define'), [Symbol.for('array-set!_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), Symbol.for('indices-and-value'), 1]], [Symbol.for('define'), Symbol.for('first-indices'), [Symbol.for('drop-right'), Symbol.for('indices'), 1]], [Symbol.for('define'), Symbol.for('last-index'), [Symbol.for('last'), Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('last'), Symbol.for('indices-and-value')]], [Symbol.for('define'), Symbol.for('arr1'), Symbol.for('arr')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('first-indices')]], [Symbol.for('set!'), Symbol.for('arr1'), [Symbol.for('array-ref'), Symbol.for('arr1'), Symbol.for('i')]]], [Symbol.for('js/='), [Symbol.for('array-ref'), Symbol.for('arr1'), Symbol.for('last-index')], Symbol.for('value')], Symbol.for('value')];

/**
 * Take the `n` first elements from `arr`.
 */
function arrayTake_(arr: any, n: any): any {
  return arr.slice(0, -(arr.length - n) || undefined);
}

arrayTake_.fsource = [Symbol.for('define'), [Symbol.for('array-take_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-drop-right'), Symbol.for('arr'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('arr')], Symbol.for('n')]]];

/**
 * Return the tail of an array.
 */
function arrayRest_(arr: any): any {
  return arr.slice(1);
}

arrayRest_.fsource = [Symbol.for('define'), [Symbol.for('array-rest_'), Symbol.for('arr')], [Symbol.for('array-drop'), Symbol.for('arr'), 1]];

/**
 * Slice a JavaScript array.
 */
function arraySlice_(arr: any, ...args: any[]): any {
  return arr.slice(...args);
}

arraySlice_.fsource = [Symbol.for('define'), [Symbol.for('array-slice_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('slice'), Symbol.for('args')]];

/**
 * Return the array obtained by dropping
 * the first `n` elements from `arr`.
 */
function arrayDrop_(arr: any, n: any): any {
  return arr.slice(n);
}

arrayDrop_.fsource = [Symbol.for('define'), [Symbol.for('array-drop_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-slice'), Symbol.for('arr'), Symbol.for('n')]];

/**
 * Return the array obtained by dropping
 * the last `n` elements from `arr`.
 */
function arrayDropRight_(arr: any, n: any): any {
  // Edge case: `(array-slice arr 0 (- n))` works well most
  // of the time, but not when `n` is zero, in which case
  // `(array-slice arr 0 0)` returns an empty array (and
  // not the full array, as expected). Hence the `or`
  // expression.
  return (
    // Edge case: `(array-slice arr 0 (- n))` works well most
    // of the time, but not when `n` is zero, in which case
    // `(array-slice arr 0 0)` returns an empty array (and
    // not the full array, as expected). Hence the `or`
    // expression.
    arr.slice(0, -n || undefined)
  );
}

arrayDropRight_.fsource = [Symbol.for('define'), [Symbol.for('array-drop-right_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-slice'), Symbol.for('arr'), 0, [Symbol.for('or'), [Symbol.for('-'), Symbol.for('n')], undefined]]];

/**
 * Concatenate arrays.
 */
function arrayConcat_(...args: any[]): any {
  return [].concat(...args);
}

arrayConcat_.fsource = [Symbol.for('define'), [Symbol.for('array-concat_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), [Symbol.for('quote'), []], Symbol.for('concat'), Symbol.for('args')]];

/**
 * Reverse the order of an array.
 * Returns a new array.
 */
function arrayReverse_(arr: any): any {
  return [...arr].reverse();
}

arrayReverse_.fsource = [Symbol.for('define'), [Symbol.for('array-reverse_'), Symbol.for('arr')], [Symbol.for('array-reverse!'), [Symbol.for('array-copy'), Symbol.for('arr')]]];

/**
 * Reverse the order of an array.
 * Returns a new array.
 */
function arrayReverseX_(arr: any): any {
  return arr.reverse();
}

arrayReverseX_.fsource = [Symbol.for('define'), [Symbol.for('array-reverse!_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('reverse')]];

/**
 * Pop an element off the beginning of an array.
 */
function arrayPopLeftX_(arr: any): any {
  return arr.shift();
}

arrayPopLeftX_.fsource = [Symbol.for('define'), [Symbol.for('array-pop-left!_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('shift')]];

/**
 * Pop an element off the end of an array.
 */
function arrayPopRightX_(arr: any): any {
  return arr.pop();
}

arrayPopRightX_.fsource = [Symbol.for('define'), [Symbol.for('array-pop-right!_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('pop')]];

/**
 * Push an element onto the beginning of an array.
 */
function arrayPushLeftX_(arr: any, x: any): any {
  arr.unshift(x);
  return arr;
}

arrayPushLeftX_.fsource = [Symbol.for('define'), [Symbol.for('array-push-left!_'), Symbol.for('arr'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('arr')];

/**
 * Push an element onto the end of an array.
 */
function arrayPushRightX_(arr: any, x: any): any {
  arr.push(x);
  return arr;
}

arrayPushRightX_.fsource = [Symbol.for('define'), [Symbol.for('array-push-right!_'), Symbol.for('arr'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('push'), Symbol.for('x')], Symbol.for('arr')];

export {
  arrayRef_ as aget,
  arrayRef_ as aget_,
  arrayRef_ as aref,
  arraySet_ as arraySet,
  arraySet_ as aset,
  arraySet_ as aset_,
  arrayAt_,
  arrayConcat_,
  arrayCopy_,
  arrayDropRight_,
  arrayDrop_,
  arrayEighth_,
  arrayFifth_,
  arrayFirst_,
  arrayFourth_,
  arrayLast_,
  arrayLength_,
  arrayNinth_,
  arrayPopLeftX_,
  arrayPopRightX_,
  arrayPushLeftX_,
  arrayPushRightX_,
  arrayRef_,
  arrayRest_,
  arrayReverseX_,
  arrayReverse_,
  arraySecond_,
  arraySetX_,
  arraySet_,
  arraySeventh_,
  arraySixth_,
  arraySlice_,
  arrayTake_,
  arrayTenth_,
  arrayNlast_,
  arrayThird_,
  arrayp_
};