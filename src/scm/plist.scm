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

(require (only-in "./procedures"
                  keyword->string))
(require (only-in "./util"
                  make-identifier-string))

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
(define (plist-copy_ plst)
  `(,@plst))

;;; Return the value of a property in a property list.
;;; Returns `#u` if not found.
;;;
;;; Similar to [`plist-get` in Emacs Lisp][el:plist-get].
;;;
;;; [el:plist-get]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dget
(define (plist-get_ plst prop)
  (define val #u)
  (for ((i (range 0 (array-length plst) 2)))
    (when (eq? (aget plst i) prop)
      (set! val (aget plst (+ i 1)))
      (break)))
  val)

;;; Whether a property list contains a given property.
(define (plist-has?_ plst prop)
  (define found #f)
  (for ((i (range 0 (array-length plst) 2)))
    (when (eq? (aget plst i) prop)
      (set! found #t)
      (break)))
  found)

;;; Set the value of a property in a property list.
;;;
;;; Similar to [`plist-put` in Emacs Lisp][el:plist-put].
;;;
;;; [el:plist-put]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#index-plist_002dput
(define (plist-set!_ plst prop val)
  (define found #f)
  (for ((i (range 0 (array-length plst) 2)))
    (when (eq? (aget plst i) prop)
      (aset! plst (+ i 1) val)
      (set! found #t)
      (break))
    (unless found
      (push-right! plst prop)
      (push-right! plst val)))
  #u)

;;; Set the value of a property in a property list,
;;; returning a new property list.
(define (plist-set_ plst prop val)
  (let ((result (plist-copy plst)))
    (plist-set!_ plst prop val)
    result))

;;; Iterate over a property list.
(define (plist-iterate_ f plst)
  (for ((i (range 0 (js/length plst) 2)))
    (define prop
      (aget plst i))
    (define val
      (aget plst (+ i 1)))
    (define entry
      (list prop val))
    (f entry)))

;;; Map a function over a property list.
(define (plist-map_ f plst)
  (define result '())
  (plist-iterate_
   (lambda (entry)
     (define-values (prop val)
       (f entry))
     (push-right! result prop)
     (push-right! result val))
   plst)
  result)

;;; Convert a plist to an association list.
(define (plist->alist_ plst)
  (define alst '())
  (for ((i (range 0 (js/length plst) 2)))
    (push-right! alst
                 (cons (aget plst i)
                       (aget plst (+ i 1)))))
  alst)

;;; Convert a property list to a JavaScript object.
(define (plist->object_ plst (options (js/obj)))
  (define result
    (js/obj))
  (for ((i (range 0 (js/length plst) 2)))
    (define prop
      (aget plst i))
    (define val
      (aget plst (+ i 1)))
    (define key
      (~> prop
          (keyword->string _)
          (make-identifier-string _ options)))
    (oset! result key val))
  result)

(provide
  (rename-out (plist->alist_ plist->alist))
  (rename-out (plist-map_ plist-map))
  (rename-out (plist->object_ plist->object))
  (rename-out (plist-copy_ plist-copy))
  (rename-out (plist-get_ plist-get))
  (rename-out (plist-get_ plist-ref_))
  (rename-out (plist-has?_ plist-has?))
  (rename-out (plist-has?_ plist-has_))
  (rename-out (plist-set!_ plist-set!))
  (rename-out (plist-set_ plist-set))
  (rename-out (plist?_ plist?))
  plist->alist_
  plist-map_
  plist->object_
  plist-copy_
  plist-get_
  plist-has?_
  plist-set!_
  plist-set_
  plist?_)
