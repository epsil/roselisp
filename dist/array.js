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
exports.arrayp_ = exports.arrayThird_ = exports.arrayNlast_ = exports.arrayTenth_ = exports.arrayTake_ = exports.arraySlice_ = exports.arraySixth_ = exports.arraySeventh_ = exports.arraySet_ = exports.arraySetX_ = exports.arraySecond_ = exports.arrayReverse_ = exports.arrayReverseX_ = exports.arrayRest_ = exports.arrayRef_ = exports.arrayPushRightX_ = exports.arrayPushLeftX_ = exports.arrayPopRightX_ = exports.arrayPopLeftX_ = exports.arrayNinth_ = exports.arrayLength_ = exports.arrayLast_ = exports.arrayFourth_ = exports.arrayFirst_ = exports.arrayFifth_ = exports.arrayEighth_ = exports.arrayDrop_ = exports.arrayDropRight_ = exports.arrayCopy_ = exports.arrayConcat_ = exports.arrayAt_ = exports.aset_ = exports.aset = exports.arraySet = exports.aref = exports.aget_ = exports.aget = void 0;
/**
 * Whether something is an array.
 */
function arrayp_(x) {
    return Array.isArray(x);
}
exports.arrayp_ = arrayp_;
arrayp_.fsource = [Symbol.for('define'), [Symbol.for('array?_'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('Array'), Symbol.for('isArray'), Symbol.for('x')]];
/**
 * Return the length of an array.
 */
function arrayLength_(arr) {
    return arr.length;
}
exports.arrayLength_ = arrayLength_;
arrayLength_.fsource = [Symbol.for('define'), [Symbol.for('array-length_'), Symbol.for('arr')], [Symbol.for('js/length'), Symbol.for('arr')]];
/**
 * Copy an array.
 */
function arrayCopy_(arr) {
    return [...arr];
}
exports.arrayCopy_ = arrayCopy_;
arrayCopy_.fsource = [Symbol.for('define'), [Symbol.for('array-copy_'), Symbol.for('arr')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('arr')]]]];
/**
 * Return the first element of an array.
 */
function arrayFirst_(arr) {
    return arr[0];
}
exports.arrayFirst_ = arrayFirst_;
arrayFirst_.fsource = [Symbol.for('define'), [Symbol.for('array-first_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 0]];
/**
 * Return the second element of an array.
 */
function arraySecond_(arr) {
    return arr[1];
}
exports.arraySecond_ = arraySecond_;
arraySecond_.fsource = [Symbol.for('define'), [Symbol.for('array-second_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 1]];
/**
 * Return the third element of an array.
 */
function arrayThird_(arr) {
    return arr[2];
}
exports.arrayThird_ = arrayThird_;
arrayThird_.fsource = [Symbol.for('define'), [Symbol.for('array-third_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 2]];
/**
 * Return the fourth element of an array.
 */
function arrayFourth_(arr) {
    return arr[3];
}
exports.arrayFourth_ = arrayFourth_;
arrayFourth_.fsource = [Symbol.for('define'), [Symbol.for('array-fourth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 3]];
/**
 * Return the fifth element of an array.
 */
function arrayFifth_(arr) {
    return arr[4];
}
exports.arrayFifth_ = arrayFifth_;
arrayFifth_.fsource = [Symbol.for('define'), [Symbol.for('array-fifth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 4]];
/**
 * Return the sixth element of an array.
 */
function arraySixth_(arr) {
    return arr[5];
}
exports.arraySixth_ = arraySixth_;
arraySixth_.fsource = [Symbol.for('define'), [Symbol.for('array-sixth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 5]];
/**
 * Return the seventh element of an array.
 */
function arraySeventh_(arr) {
    return arr[6];
}
exports.arraySeventh_ = arraySeventh_;
arraySeventh_.fsource = [Symbol.for('define'), [Symbol.for('array-seventh_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 6]];
/**
 * Return the eight element of an array.
 */
function arrayEighth_(arr) {
    return arr[7];
}
exports.arrayEighth_ = arrayEighth_;
arrayEighth_.fsource = [Symbol.for('define'), [Symbol.for('array-eighth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 7]];
/**
 * Return the ninth element of an array.
 */
function arrayNinth_(arr) {
    return arr[8];
}
exports.arrayNinth_ = arrayNinth_;
arrayNinth_.fsource = [Symbol.for('define'), [Symbol.for('array-ninth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 8]];
/**
 * Return the tenth element of an array.
 */
function arrayTenth_(arr) {
    return arr[9];
}
exports.arrayTenth_ = arrayTenth_;
arrayTenth_.fsource = [Symbol.for('define'), [Symbol.for('array-tenth_'), Symbol.for('arr')], [Symbol.for('array-ref'), Symbol.for('arr'), 9]];
/**
 * Return the last element of an array.
 */
function arrayLast_(arr) {
    return arr.at(-1);
}
exports.arrayLast_ = arrayLast_;
arrayLast_.fsource = [Symbol.for('define'), [Symbol.for('array-last_'), Symbol.for('arr')], [Symbol.for('array-at'), Symbol.for('arr'), -1]];
/**
 * Return the `n`-th element counting from
 * the end of the array.
 */
function arrayNlast_(arr, n) {
    return arr.at(-n);
}
exports.arrayNlast_ = arrayNlast_;
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
function arrayRef_(arr, ...indices) {
    let result = arr;
    for (let i of indices) {
        result = arr[i];
    }
    return result;
}
exports.aget = arrayRef_;
exports.aget_ = arrayRef_;
exports.aref = arrayRef_;
exports.arrayRef_ = arrayRef_;
arrayRef_.fsource = [Symbol.for('define'), [Symbol.for('array-ref_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('arr')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('indices')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('js/get'), Symbol.for('arr'), Symbol.for('i')]]], Symbol.for('result')];
/**
 * Return the `i`-th element of the array.
 * Accepts negative values, counting back
 * from the end of the array.
 */
function arrayAt_(arr, i) {
    return arr.at(i);
}
exports.arrayAt_ = arrayAt_;
arrayAt_.fsource = [Symbol.for('define'), [Symbol.for('array-at_'), Symbol.for('arr'), Symbol.for('i')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('at'), Symbol.for('i')]];
/**
 * Set an array position to a given value.
 * Returns a new array.
 */
function arraySet_(arr, ...indicesAndValue) {
    let result = [...arr];
    if (indicesAndValue.length > 2) {
        const [i, ...indicesAndValue1] = indicesAndValue;
        result[i] = arraySet_(result[i], ...indicesAndValue1);
    }
    else {
        const [i, val] = indicesAndValue;
        result[i] = val;
    }
    return result;
}
exports.arraySet = arraySet_;
exports.aset = arraySet_;
exports.aset_ = arraySet_;
exports.arraySet_ = arraySet_;
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
function arraySetX_(arr, ...indicesAndValue) {
    const indices = indicesAndValue.slice(0, -1);
    const firstIndices = indices.slice(0, -1);
    const lastIndex = indices.at(-1);
    const value = indicesAndValue.at(-1);
    let arr1 = arr;
    for (let i of firstIndices) {
        arr1 = arr1[i];
    }
    arr1[lastIndex] = value;
    return value;
}
exports.arraySetX_ = arraySetX_;
arraySetX_.fsource = [Symbol.for('define'), [Symbol.for('array-set!_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('indices-and-value')], [Symbol.for('define'), Symbol.for('indices'), [Symbol.for('drop-right'), Symbol.for('indices-and-value'), 1]], [Symbol.for('define'), Symbol.for('first-indices'), [Symbol.for('drop-right'), Symbol.for('indices'), 1]], [Symbol.for('define'), Symbol.for('last-index'), [Symbol.for('last'), Symbol.for('indices')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('last'), Symbol.for('indices-and-value')]], [Symbol.for('define'), Symbol.for('arr1'), Symbol.for('arr')], [Symbol.for('for'), [[Symbol.for('i'), Symbol.for('first-indices')]], [Symbol.for('set!'), Symbol.for('arr1'), [Symbol.for('array-ref'), Symbol.for('arr1'), Symbol.for('i')]]], [Symbol.for('js/='), [Symbol.for('array-ref'), Symbol.for('arr1'), Symbol.for('last-index')], Symbol.for('value')], Symbol.for('value')];
/**
 * Take the `n` first elements from `arr`.
 */
function arrayTake_(arr, n) {
    return arr.slice(0, -(arr.length - n) || undefined);
}
exports.arrayTake_ = arrayTake_;
arrayTake_.fsource = [Symbol.for('define'), [Symbol.for('array-take_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-drop-right'), Symbol.for('arr'), [Symbol.for('-'), [Symbol.for('array-length'), Symbol.for('arr')], Symbol.for('n')]]];
/**
 * Return the tail of an array.
 */
function arrayRest_(arr) {
    return arr.slice(1);
}
exports.arrayRest_ = arrayRest_;
arrayRest_.fsource = [Symbol.for('define'), [Symbol.for('array-rest_'), Symbol.for('arr')], [Symbol.for('array-drop'), Symbol.for('arr'), 1]];
/**
 * Slice a JavaScript array.
 */
function arraySlice_(arr, ...args) {
    return arr.slice(...args);
}
exports.arraySlice_ = arraySlice_;
arraySlice_.fsource = [Symbol.for('define'), [Symbol.for('array-slice_'), Symbol.for('arr'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), Symbol.for('arr'), Symbol.for('slice'), Symbol.for('args')]];
/**
 * Return the array obtained by dropping
 * the first `n` elements from `arr`.
 */
function arrayDrop_(arr, n) {
    return arr.slice(n);
}
exports.arrayDrop_ = arrayDrop_;
arrayDrop_.fsource = [Symbol.for('define'), [Symbol.for('array-drop_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-slice'), Symbol.for('arr'), Symbol.for('n')]];
/**
 * Return the array obtained by dropping
 * the last `n` elements from `arr`.
 */
function arrayDropRight_(arr, n) {
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
    arr.slice(0, -n || undefined));
}
exports.arrayDropRight_ = arrayDropRight_;
arrayDropRight_.fsource = [Symbol.for('define'), [Symbol.for('array-drop-right_'), Symbol.for('arr'), Symbol.for('n')], [Symbol.for('array-slice'), Symbol.for('arr'), 0, [Symbol.for('or'), [Symbol.for('-'), Symbol.for('n')], undefined]]];
/**
 * Concatenate arrays.
 */
function arrayConcat_(...args) {
    return [].concat(...args);
}
exports.arrayConcat_ = arrayConcat_;
arrayConcat_.fsource = [Symbol.for('define'), [Symbol.for('array-concat_'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('send/apply'), [Symbol.for('quote'), []], Symbol.for('concat'), Symbol.for('args')]];
/**
 * Reverse the order of an array.
 * Returns a new array.
 */
function arrayReverse_(arr) {
    return [...arr].reverse();
}
exports.arrayReverse_ = arrayReverse_;
arrayReverse_.fsource = [Symbol.for('define'), [Symbol.for('array-reverse_'), Symbol.for('arr')], [Symbol.for('array-reverse!'), [Symbol.for('array-copy'), Symbol.for('arr')]]];
/**
 * Reverse the order of an array.
 * Returns a new array.
 */
function arrayReverseX_(arr) {
    return arr.reverse();
}
exports.arrayReverseX_ = arrayReverseX_;
arrayReverseX_.fsource = [Symbol.for('define'), [Symbol.for('array-reverse!_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('reverse')]];
/**
 * Pop an element off the beginning of an array.
 */
function arrayPopLeftX_(arr) {
    return arr.shift();
}
exports.arrayPopLeftX_ = arrayPopLeftX_;
arrayPopLeftX_.fsource = [Symbol.for('define'), [Symbol.for('array-pop-left!_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('shift')]];
/**
 * Pop an element off the end of an array.
 */
function arrayPopRightX_(arr) {
    return arr.pop();
}
exports.arrayPopRightX_ = arrayPopRightX_;
arrayPopRightX_.fsource = [Symbol.for('define'), [Symbol.for('array-pop-right!_'), Symbol.for('arr')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('pop')]];
/**
 * Push an element onto the beginning of an array.
 */
function arrayPushLeftX_(arr, x) {
    arr.unshift(x);
    return arr;
}
exports.arrayPushLeftX_ = arrayPushLeftX_;
arrayPushLeftX_.fsource = [Symbol.for('define'), [Symbol.for('array-push-left!_'), Symbol.for('arr'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('unshift'), Symbol.for('x')], Symbol.for('arr')];
/**
 * Push an element onto the end of an array.
 */
function arrayPushRightX_(arr, x) {
    arr.push(x);
    return arr;
}
exports.arrayPushRightX_ = arrayPushRightX_;
arrayPushRightX_.fsource = [Symbol.for('define'), [Symbol.for('array-push-right!_'), Symbol.for('arr'), Symbol.for('x')], [Symbol.for('send'), Symbol.for('arr'), Symbol.for('push'), Symbol.for('x')], Symbol.for('arr')];
