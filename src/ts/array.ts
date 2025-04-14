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
function arrayp_(obj: any): any {
  return Array.isArray(obj);
}

arrayp_.lispSource = [Symbol.for('define'), [Symbol.for('array?_'), Symbol.for('obj')], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), Symbol.for('obj')]];

/**
 * Return the last element of an array.
 */
function arrayLast_(arr: any): any {
  return arr[arr.length - 1];
}

arrayLast_.lispSource = [Symbol.for('define'), [Symbol.for('array-last_'), Symbol.for('arr')], [Symbol.for('aget'), Symbol.for('arr'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('arr')], 1]]];

/**
 * Return the length of an array.
 */
function arrayLength_(arr: any): any {
  return arr.length;
}

arrayLength_.lispSource = [Symbol.for('define'), [Symbol.for('array-length_'), Symbol.for('arr')], [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('arr')]];

/**
 * Return the first element of an array.
 */
function arrayFirst_(lst: any): any {
  return lst[0];
}

arrayFirst_.lispSource = [Symbol.for('define'), [Symbol.for('array-first_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 0]];

/**
 * Return the second element of an array.
 */
function arraySecond_(lst: any): any {
  return lst[1];
}

arraySecond_.lispSource = [Symbol.for('define'), [Symbol.for('array-second_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 1]];

/**
 * Return the third element of an array.
 */
function arrayThird_(lst: any): any {
  return lst[2];
}

arrayThird_.lispSource = [Symbol.for('define'), [Symbol.for('array-third_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 2]];

/**
 * Return the fourth element of an array.
 */
function arrayFourth_(lst: any): any {
  return lst[3];
}

arrayFourth_.lispSource = [Symbol.for('define'), [Symbol.for('array-fourth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 3]];

/**
 * Return the fifth element of an array.
 */
function arrayFifth_(lst: any): any {
  return lst[4];
}

arrayFifth_.lispSource = [Symbol.for('define'), [Symbol.for('array-fifth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 4]];

/**
 * Return the sixth element of an array.
 */
function arraySixth_(lst: any): any {
  return lst[5];
}

arraySixth_.lispSource = [Symbol.for('define'), [Symbol.for('array-sixth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 5]];

/**
 * Return the seventh element of an array.
 */
function arraySeventh_(lst: any): any {
  return lst[6];
}

arraySeventh_.lispSource = [Symbol.for('define'), [Symbol.for('array-seventh_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 6]];

/**
 * Return the eight element of an array.
 */
function arrayEighth_(lst: any): any {
  return lst[7];
}

arrayEighth_.lispSource = [Symbol.for('define'), [Symbol.for('array-eighth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 7]];

/**
 * Return the ninth element of an array.
 */
function arrayNinth_(lst: any): any {
  return lst[8];
}

arrayNinth_.lispSource = [Symbol.for('define'), [Symbol.for('array-ninth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 8]];

/**
 * Return the tenth element of an array.
 */
function arrayTenth_(lst: any): any {
  return lst[9];
}

arrayTenth_.lispSource = [Symbol.for('define'), [Symbol.for('array-tenth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 9]];

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
  if (indices.length === 1) {
    return (arr as any)[indices[0]];
  } else {
    return indices.reduce(function (arr: any, i: any): any {
      return (arr as any)[i];
    }, arr);
  }
}

arrayRef_.lispSource = [Symbol.for('define'), [Symbol.for('array-ref_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices')], [Symbol.for('cond'), [[Symbol.for('='), [Symbol.for('array-length'), Symbol.for('indices')], 1], [Symbol.for('aget'), Symbol.for('arr'), [Symbol.for('first'), Symbol.for('indices')]]], [Symbol.for('else'), [Symbol.for('foldl'), [Symbol.for('lambda'), [Symbol.for('i'), Symbol.for('arr')], [Symbol.for('aget'), Symbol.for('arr'), Symbol.for('i')]], Symbol.for('arr'), Symbol.for('indices')]]]];

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
function arraySet_(arr: any, ...indicesAndValue: any[]): any {
  const value: any = indicesAndValue[indicesAndValue.length - 1];
  const idx: any = indicesAndValue[indicesAndValue.length - 2];
  const indices: any = indicesAndValue.slice(0, -2);
  const arr1: any = arrayRef_(arr, ...indices);
  (arr1 as any)[idx] = value;
  return value;
}

arraySet_.lispSource = [Symbol.for('define'), [Symbol.for('array-set_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('aget'), Symbol.for('indices-and-value'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('indices-and-value')], 1]]], [Symbol.for('define'), Symbol.for('idx'), [Symbol.for('aget'), Symbol.for('indices-and-value'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('indices-and-value')], 2]]], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), Symbol.for('indices-and-value'), 2]], [Symbol.for('define'), Symbol.for('arr1'), [Symbol.for('apply'), Symbol.for('array-ref_'), Symbol.for('arr'), Symbol.for('indices')]], [Symbol.for('aset!'), Symbol.for('arr1'), Symbol.for('idx'), Symbol.for('value')], Symbol.for('value')];

/**
 * Return the array obtained by dropping
 * the first `n` elements from `arr`.
 */
function arrayDrop_(arr: any, n: any): any {
  if (n === 0) {
    return arr;
  } else {
    return arr.slice(n);
  }
}

arrayDrop_.lispSource = [Symbol.for('define'), [Symbol.for('array-drop_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('arr'), Symbol.for('slice'), Symbol.for('n')]]]];

/**
 * Return the array obtained by dropping
 * the last `n` elements from `arr`.
 */
function arrayDropRight_(arr: any, n: any): any {
  if (n === 0) {
    return arr;
  } else {
    return arr.slice(0, -n);
  }
}

arrayDropRight_.lispSource = [Symbol.for('define'), [Symbol.for('array-drop-right_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('arr'), Symbol.for('slice'), 0, [Symbol.for('-'), Symbol.for('n')]]]]];

/**
 * Return the tail of an array.
 */
function arrayRest_(arr: any): any {
  return arr.slice(1);
}

arrayRest_.lispSource = [Symbol.for('define'), [Symbol.for('array-rest_'), Symbol.for('arr')], [Symbol.for('array-drop'), Symbol.for('arr'), 1]];

/**
 * Reverse the order of an array.
 * Returns a new array list.
 */
function arrayReverse_(arr: any): any {
  return arr.reverse();
}

arrayReverse_.lispSource = [Symbol.for('define'), [Symbol.for('array-reverse_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('reverse')]];

/**
 * Take the `n` first elements from `arr`.
 */
function arrayTake_(arr: any, n: any): any {
  const n1: any = arr.length - n;
  if (n1 === 0) {
    return arr;
  } else {
    return arr.slice(0, -n1);
  }
}

arrayTake_.lispSource = [Symbol.for('define'), [Symbol.for('array-take_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-drop-right'), Symbol.for('arr'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('arr')], Symbol.for('n')]]];

export {
  arrayRef_ as aget,
  arrayRef_ as aget_,
  arrayRef_ as aref,
  arraySet_ as arraySet,
  arraySet_ as aset,
  arraySet_ as aset_,
  arrayDropRight_,
  arrayDrop_,
  arrayEighth_,
  arrayFifth_,
  arrayFirst_,
  arrayFourth_,
  arrayLast_,
  arrayLength_,
  arrayNinth_,
  arrayRef_,
  arrayRest_,
  arrayReverse_,
  arraySecond_,
  arraySet_,
  arraySeventh_,
  arraySixth_,
  arrayTenth_,
  arrayTake_,
  arrayThird_,
  arrayp_
};