;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Hash maps
;;;
;;; Functions for working with hash maps.
;;;
;;; ## Description
;;;
;;; An implementation of hash maps, expressed in terms of JavaScript's
;;; [`Map`][js:Map].
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;
;;; [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map

;;; Whether something is a hash map.
;;;
;;; Similar to [`hash?` in Racket][rkt:hashp].
;;;
;;; [rkt:hashp]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash~3f%29%29
(define (hash?_ v)
  (is-a? v Map))

;;; Make a hash map from a list of `(key . value)` pairs.
;;;
;;; Similar to [`make-hash` in Racket][rkt:make-hash].
;;;
;;; [rkt:make-hash]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._make-hash%29%29
(define (make-hash_ (assocs '()))
  (new Map (map flatten assocs)))

;;; Set `key` to `v` in the hash map `ht`.
;;;
;;; Similar to [`hash-set!` in Racket][rkt:hash-set-x].
;;;
;;; [rkt:hash-set-x]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-set%21%29%29
(define (hash-set!_ ht key v)
  (send ht set key v))

;;; Set `key` to `v` in the hash map `ht`,
;;; returning a new hash map.
;;;
;;; Similar to [`hash-set` in Racket][rkt:hash-set].
;;;
;;; [rkt:hash-set]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-set%29%29
(define (hash-set_ ht key v)
  (let ((result (hash-copy ht)))
    (hash-set! result key v)
    result))

;;; Get the value of `key` in the hash map `ht`.
;;; `failure-result`, if specified, is returned
;;; if there is no value.
;;;
;;; Similar to [`hash-ref` in Racket][rkt:hash-ref].
;;;
;;; [rkt:hash-ref]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-ref%29%29
(define (hash-ref_ ht key (failure-result undefined))
  (cond
   ((and (not (eq? failure-result undefined))
         (not (hash-has-key? ht key)))
    failure-result)
   (else
    (send ht get key))))

;;; Whether a hash map has a value for a given key.
;;;
;;; Similar to [`hash-has-key?` in Racket][rkt:hash-has-key-p].
;;;
;;; [rkt-hash-has-key-p]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._hash-has-key~3f%29%29
(define (hash-has-key?_ ht key)
  (send ht has key))

;;; Remove the value for a given key in a hash map
;;;
;;; Similar to [`hash-remove!` in Racket][rkt:hash-remove-x].
;;;
;;; [rkt:hash-remove-x]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-remove%21%29%29
(define (hash-remove_ ht key)
  (let ((result (hash-copy ht)))
    (hash-remove! result key)
    result))

;;; Remove the value for a given key in a hash map,
;;; returning a new hash map.
;;;
;;; Similar to [`hash-remove` in Racket][rkt:hash-remove].
;;;
;;; [rkt:hash-remove]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-remove%29%29
(define (hash-remove!_ ht key)
  (send ht delete key))

;;; Return the number of keys in a hash table.
(define (hash-size_ ht)
  (get-field size ht))

;;; Clone a hash map.
;;;
;;; Similar to [`hash-copy` in Racket][rkt:hash-copy].
;;;
;;; [rkt:hash-copy]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-copy%29%29
(define (hash-copy_ ht)
  (new Map ht))

;;; Delete all entries in a hash map,
;;; returning a new hash map.
;;;
;;; Similar to [`hash-clear` in Racket][rkt:hash-clear].
;;;
;;; [rkt:hash-clear]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-clear%29%29
(define (hash-clear_ ht)
  (new Map))

;;; Delete all entries in a hash map.
;;;
;;; Similar to [`hash-clear!` in Racket][rkt:hash-clear-x].
;;;
;;; [rkt:hash-clear-x]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-clear%21%29%29
(define (hash-clear!_ ht)
  (send ht clear))

;;; Return a list of all the keys in a hash map.
;;;
;;; Similar to [`hash-keys` in Racket][rkt:hash-keys].
;;;
;;; [rkt:hash-keys]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-keys%29%29
(define (hash-keys_ ht)
  `(,@(send ht keys)))

;;; Return a list of all the values in a hash map.
;;;
;;; Similar to [`hash-values` in Racket][rkt:hash-values].
;;;
;;; [rkt:hash-values]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-keys%29%29
(define (hash-values_ ht)
  `(,@(send ht values)))

;;; Convert a hash map to a list of `(key value)` tuples.
(define (hash-entries_ ht)
  `(,@(send ht entries)))

;;; Convert a hash map to a list of `(key . value)` pairs.
;;;
;;; Similar to [`hash->list` in Racket][rkt:hash-to-list].
;;;
;;; [rkt:hash-to-list]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-~3elist%29%29
(define (hash->list_ ht)
  (map (lambda (x)
         (cons (array-first x)
               (array-second x)))
       (hash-entries ht)))

(provide
  hash->list_
  hash-clear!_
  hash-clear_
  hash-copy_
  hash-entries_
  hash-has-key?_
  hash-keys_
  hash-ref_
  hash-remove!_
  hash-remove_
  hash-set!_
  hash-set_
  hash-size_
  hash-values_
  hash?_
  make-hash_)
