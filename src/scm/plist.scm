;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Property lists
;;;
;;; Property lists, also known as plists.
;;;
;;; ## Description
;;;
;;; Various functions for working with property lists.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Whether something is a property list.
;;;
;;; Similar to [`plistp` in Emacs Lisp][el:plistp].
;;;
;;; [el:plistp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html#index-plistp
(define (plist?_ obj)
  ;; Since we permit properties to be any kind of value, it suffices
  ;; to verify that the input is an array of even length.
  (and (array? obj)
       (even? (array-length obj))))

;;; Copy a property list.
(define (plist-copy_ plist)
  `(,@plist))

;;; Return the value of a property in a property list.
;;; Returns `undefined` if not found.
;;;
;;; Similar to [`plist-get` in Emacs Lisp][el:plist-get].
;;;
;;; [el:plist-get]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dget
(define (plist-get_ plist prop)
  (define val undefined)
  (for ((i (range 0 (array-length plist) 2)))
    (when (eq? (aget plist i) prop)
      (set! val (aget plist (+ i 1)))
      (break)))
  val)

;;; Whether a property list contains a given property.
(define (plist-has?_ plist prop)
  (define found #f)
  (for ((i (range 0 (array-length plist) 2)))
    (when (eq? (aget plist i) prop)
      (set! found #t)
      (break)))
  found)

;;; Set the value of a property in a property list.
;;;
;;; Similar to [`plist-put` in Emacs Lisp][el:plist-put].
;;;
;;; [el:plist-put]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dput
(define (plist-set!_ plist prop val)
  (define found #f)
  (for ((i (range 0 (array-length plist) 2)))
    (when (eq? (aget plist i) prop)
      (aset! plist (+ i 1) val)
      (set! found #t)
      (break))
    (unless found
      (push-right! plist prop)
      (push-right! plist val)))
  undefined)

;;; Set the value of a property in a property list,
;;; returning a new property list.
(define (plist-set_ plist prop val)
  (let ((result (plist-copy plist)))
    (plist-set!_ plist prop val)
    result))


;;; Convert a plist to an association list.
(define (plist->alist_ plst)
  (define alst '())
  (for ((i (range 0 (array-list-length plst) 2)))
    (push-right! alst
                 (cons (aget plst i)
                       (aget plst (+ i 1)))))
  alst)

(provide
  (rename-out (plist-get_ plist-ref_))
  (rename-out (plist-has?_ plist-has_))
  plist->alist_
  plist-copy_
  plist-get_
  plist-has?_
  plist-set!_
  plist-set_
  plist?_)
