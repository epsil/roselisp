"use strict";
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.arrayp_ = exports.arrayThird_ = exports.arrayTake_ = exports.arrayTenth_ = exports.arraySixth_ = exports.arraySeventh_ = exports.arraySet_ = exports.arraySecond_ = exports.arrayReverse_ = exports.arrayRest_ = exports.arrayRef_ = exports.arrayNinth_ = exports.arrayLength_ = exports.arrayLast_ = exports.arrayFourth_ = exports.arrayFirst_ = exports.arrayFifth_ = exports.arrayEighth_ = exports.arrayDrop_ = exports.arrayDropRight_ = exports.aset_ = exports.aset = exports.arraySet = exports.aref = exports.aget_ = exports.aget = void 0;
/**
 * Whether something is an array.
 */
function arrayp_(obj) {
    return Array.isArray(obj);
}
exports.arrayp_ = arrayp_;
arrayp_.lispSource = [Symbol.for('define'), [Symbol.for('array?_'), Symbol.for('obj')], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), Symbol.for('obj')]];
/**
 * Return the last element of an array.
 */
function arrayLast_(arr) {
    return arr[arr.length - 1];
}
exports.arrayLast_ = arrayLast_;
arrayLast_.lispSource = [Symbol.for('define'), [Symbol.for('array-last_'), Symbol.for('arr')], [Symbol.for('aget'), Symbol.for('arr'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('arr')], 1]]];
/**
 * Return the length of an array.
 */
function arrayLength_(arr) {
    return arr.length;
}
exports.arrayLength_ = arrayLength_;
arrayLength_.lispSource = [Symbol.for('define'), [Symbol.for('array-length_'), Symbol.for('arr')], [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('arr')]];
/**
 * Return the first element of an array.
 */
function arrayFirst_(lst) {
    return lst[0];
}
exports.arrayFirst_ = arrayFirst_;
arrayFirst_.lispSource = [Symbol.for('define'), [Symbol.for('array-first_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 0]];
/**
 * Return the second element of an array.
 */
function arraySecond_(lst) {
    return lst[1];
}
exports.arraySecond_ = arraySecond_;
arraySecond_.lispSource = [Symbol.for('define'), [Symbol.for('array-second_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 1]];
/**
 * Return the third element of an array.
 */
function arrayThird_(lst) {
    return lst[2];
}
exports.arrayThird_ = arrayThird_;
arrayThird_.lispSource = [Symbol.for('define'), [Symbol.for('array-third_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 2]];
/**
 * Return the fourth element of an array.
 */
function arrayFourth_(lst) {
    return lst[3];
}
exports.arrayFourth_ = arrayFourth_;
arrayFourth_.lispSource = [Symbol.for('define'), [Symbol.for('array-fourth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 3]];
/**
 * Return the fifth element of an array.
 */
function arrayFifth_(lst) {
    return lst[4];
}
exports.arrayFifth_ = arrayFifth_;
arrayFifth_.lispSource = [Symbol.for('define'), [Symbol.for('array-fifth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 4]];
/**
 * Return the sixth element of an array.
 */
function arraySixth_(lst) {
    return lst[5];
}
exports.arraySixth_ = arraySixth_;
arraySixth_.lispSource = [Symbol.for('define'), [Symbol.for('array-sixth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 5]];
/**
 * Return the seventh element of an array.
 */
function arraySeventh_(lst) {
    return lst[6];
}
exports.arraySeventh_ = arraySeventh_;
arraySeventh_.lispSource = [Symbol.for('define'), [Symbol.for('array-seventh_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 6]];
/**
 * Return the eight element of an array.
 */
function arrayEighth_(lst) {
    return lst[7];
}
exports.arrayEighth_ = arrayEighth_;
arrayEighth_.lispSource = [Symbol.for('define'), [Symbol.for('array-eighth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 7]];
/**
 * Return the ninth element of an array.
 */
function arrayNinth_(lst) {
    return lst[8];
}
exports.arrayNinth_ = arrayNinth_;
arrayNinth_.lispSource = [Symbol.for('define'), [Symbol.for('array-ninth_'), Symbol.for('lst')], [Symbol.for('aget'), Symbol.for('lst'), 8]];
/**
 * Return the tenth element of an array.
 */
function arrayTenth_(lst) {
    return lst[9];
}
exports.arrayTenth_ = arrayTenth_;
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
function arrayRef_(arr, ...indices) {
    if (indices.length === 1) {
        return arr[indices[0]];
    }
    else {
        return indices.reduce(function (arr, i) {
            return arr[i];
        }, arr);
    }
}
exports.aget = arrayRef_;
exports.aget_ = arrayRef_;
exports.aref = arrayRef_;
exports.arrayRef_ = arrayRef_;
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
function arraySet_(arr, ...indicesAndValue) {
    const value = indicesAndValue[indicesAndValue.length - 1];
    const idx = indicesAndValue[indicesAndValue.length - 2];
    const indices = indicesAndValue.slice(0, -2);
    const arr1 = arrayRef_(arr, ...indices);
    arr1[idx] = value;
    return value;
}
exports.arraySet = arraySet_;
exports.aset = arraySet_;
exports.aset_ = arraySet_;
exports.arraySet_ = arraySet_;
arraySet_.lispSource = [Symbol.for('define'), [Symbol.for('array-set_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('aget'), Symbol.for('indices-and-value'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('indices-and-value')], 1]]], [Symbol.for('define'), Symbol.for('idx'), [Symbol.for('aget'), Symbol.for('indices-and-value'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('indices-and-value')], 2]]], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), Symbol.for('indices-and-value'), 2]], [Symbol.for('define'), Symbol.for('arr1'), [Symbol.for('apply'), Symbol.for('array-ref_'), Symbol.for('arr'), Symbol.for('indices')]], [Symbol.for('aset!'), Symbol.for('arr1'), Symbol.for('idx'), Symbol.for('value')], Symbol.for('value')];
/**
 * Return the array obtained by dropping
 * the first `n` elements from `arr`.
 */
function arrayDrop_(arr, n) {
    if (n === 0) {
        return arr;
    }
    else {
        return arr.slice(n);
    }
}
exports.arrayDrop_ = arrayDrop_;
arrayDrop_.lispSource = [Symbol.for('define'), [Symbol.for('array-drop_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('arr'), Symbol.for('slice'), Symbol.for('n')]]]];
/**
 * Return the array obtained by dropping
 * the last `n` elements from `arr`.
 */
function arrayDropRight_(arr, n) {
    if (n === 0) {
        return arr;
    }
    else {
        return arr.slice(0, -n);
    }
}
exports.arrayDropRight_ = arrayDropRight_;
arrayDropRight_.lispSource = [Symbol.for('define'), [Symbol.for('array-drop-right_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('cond'), [[Symbol.for('='), Symbol.for('n'), 0], Symbol.for('arr')], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('arr'), Symbol.for('slice'), 0, [Symbol.for('-'), Symbol.for('n')]]]]];
/**
 * Return the tail of an array.
 */
function arrayRest_(arr) {
    return arr.slice(1);
}
exports.arrayRest_ = arrayRest_;
arrayRest_.lispSource = [Symbol.for('define'), [Symbol.for('array-rest_'), Symbol.for('arr')], [Symbol.for('array-drop'), Symbol.for('arr'), 1]];
/**
 * Reverse the order of an array.
 * Returns a new array list.
 */
function arrayReverse_(arr) {
    return arr.reverse();
}
exports.arrayReverse_ = arrayReverse_;
arrayReverse_.lispSource = [Symbol.for('define'), [Symbol.for('array-reverse_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('reverse')]];
/**
 * Take the `n` first elements from `arr`.
 */
function arrayTake_(arr, n) {
    const n1 = arr.length - n;
    if (n1 === 0) {
        return arr;
    }
    else {
        return arr.slice(0, -n1);
    }
}
exports.arrayTake_ = arrayTake_;
arrayTake_.lispSource = [Symbol.for('define'), [Symbol.for('array-take_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-drop-right'), Symbol.for('arr'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('arr')], Symbol.for('n')]]];
