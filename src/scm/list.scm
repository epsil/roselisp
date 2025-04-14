;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # List functions
;;;
;;; Functions for working with lists.
;;;
;;; ## Description
;;;
;;; Lists are implemented in terms of JavaScript arrays. There is,
;;; however, no direct JavaScript equivalent to the Lisp concept of a
;;; *cons cell*, such as `'(1 . 2)`. Therefore, Roselisp defines a
;;; list type called a linked list, whose constituent links correspond
;;; to the cons cell concept.
;;;
;;; ### Terminology
;;;
;;; The *empty list* is the list `'()`, represented by the JavaScript
;;; array `[]`. The function `null?` returns `#t` only when passed the
;;; empty list.
;;;
;;;     > (null? '())
;;;     #t
;;;
;;; A list with one or more elements is a *nonempty list*. There are
;;; two forms of nonempty lists: *array lists* and *linked lists*. An
;;; array list is a list that is straightforwardly implemented in
;;; terms of a JavaScript array, such as `'(1)`, `'(1 2)` and
;;; `'(1 2 3)`. These correspond to the JavaScript arrays `[1]`,
;;; `[1, 2]` and `[1, 2, 3]`, respectively. The function `array-list?`
;;; returns `#t` when passed an array list.
;;;
;;;     > (array-list? '(1))
;;;     #t
;;;     > (array-list? '(1 2))
;;;     #t
;;;     > (array-list? '(1 2 3))
;;;     #t
;;;
;;; The other type of nonempty list is the *linked list*, which is
;;; composed out of *linked list links*. A linked list link is an
;;; expression such as `'(1 . ())`, which is implemented in terms of
;;; the JavaScript array `[1, Symbol.for('.'), []]`. Here, the
;;; penultimate array element is the special symbol `.` (i.e., `'.`),
;;; which is also referred to as the *cons dot*. The function
;;; `linked-list-link?` returns `#t` when passed a linked list link.
;;;
;;;     > (linked-list-link? '(1 . ()))
;;;     #t
;;;     > (linked-list-link? '(1 2 . ()))
;;;     #t
;;;
;;; By chaining such links together, a sequence of values is obtained.
;;; If the final chain is the empty list, then the resulting structure
;;; is referred to as a *linked list*. For example, the linked list
;;; `'(1 . (2 . ()))` may be considered to represent the same sequence
;;; as the array list `(1 2)`, even though the underlying JavaScript
;;; arrays are different. The function `linked-list?` returns `#t`
;;; when passed a linked list.
;;;
;;;     > (linked-list? '(1 . ()))
;;;     #t
;;;     > (linked-list? '(1 . (2 . ())))
;;;     #t
;;;     > (linked-list? '(1 2 . (3 . ())))
;;;     #t
;;;
;;; However, if the final chain is *not* the empty list, then the
;;; sequence is called a *dotted list*. The function `dotted-list?`
;;; returns `#t` when passed a dotted list.
;;;
;;;     > (dotted-list? '(1 . 2))
;;;     #t
;;;     > (dotted-list? '(1 2 . 3))
;;;     #t
;;;     > (dotted-list? '(1 . (2 . 3)))
;;;     #t
;;;     > (dotted-list? '(1 2 . (3 . 4)))
;;;     #t
;;;
;;; Dotted lists are sometimes referred to as "improper lists", to
;;; distinguish them from "proper lists", which are `()`-terminated.
;;; Thus, array lists and linked lists are proper lists, while dotted
;;; lists are improper. The function `list?` returns `#t` when passed
;;; a proper list, but not when passed an improper list.
;;;
;;;     > (list? '())
;;;     #t
;;;     > (list? '(1))
;;;     #t
;;;     > (list? '(1 . ()))
;;;     #t
;;;     > (list? '(1 . (2 . ())))
;;;     #t
;;;     > (list? '(1 2 . (3 . ())))
;;;     #t
;;;
;;; While array lists are the fastest and are preferable for most
;;; tasks, linked lists are more versatile. For example, a linked list
;;; may be turned into a *circular list*, i.e., a list that loops back
;;; on itself. The function `circular-list?` returns `#t` when passed
;;; a circular list.
;;;
;;;     > (define circ-lst
;;;         '(1 . ()))
;;;     undefined
;;;     > (set-cdr! circ-lst circ-lst)
;;;     undefined
;;;     > (circular-list? circ-lst)
;;;     #t))))
;;;
;;; It should be appreciated that Lisp's "list" concept is not a type,
;;; but a structural property. Whether something is a list or not can
;;; be determined only by inspecting the structure itself. One can
;;; avoid this check by working exclusively with array lists and using
;;; `array-list?` to determine whether something is an array list or
;;; not.
;;;
;;; ### Subtleties
;;;
;;; Note that the function `array-list?` also returns `#t` when passed
;;; a linked list:
;;;
;;;     > (array-list? '(1 . ()))
;;;     #t
;;;
;;; Roselisp allows for the possibility that you simply wants to treat
;;; the list `'(1 . ())` as an array list of three elements, with the
;;; cons dot as the second element. If you wish to distinguish linked
;;; lists from array lists, you should use `linked-list?`, not
;;; `array-list?`. The standard list functions---`cdr`, `nth`,
;;; `first`, `second`, `third`, etc.---treat the input as a linked
;;; list if `linked-list?` returns `#t`, and as an array list
;;; otherwise.
;;;
;;; The term "dotted list" refers exclusively to improper lists.
;;; Therefore, while the linked list `'(1 . ())` does indeed contain
;;; a dot, it is not regarded as a dotted list. The improper list
;;; `'(1 . 2)`, on the other hand, is regarded as dotted.
;;;
;;; ### Cons dot
;;;
;;; TODO: Merge this section into the other text.
;;;
;;; There is no direct analogue to the cons cell concept in
;;; JavaScript. Therefore, Roselisp represents cons cells in terms of
;;; *dotted lists*, which are lists where the penultimate element is
;;; the *cons dot*, the symbol `.` (i.e., `|.|`, or `Symbol.for('.')`
;;; in JavaScript).
;;;
;;; Thus, in Roselisp, the expression `'(1 . 2)` is the same as
;;; `(list 1 '. 2)`, or, in JavaScript, `[1, Symbol.for('.'), 2]`.
;;; A cons cell can be considered from two perspectives: either as a
;;; pair of two elements, or as a list where the second element is the
;;; cons dot.
;;;
;;; Cons cells are implemented as a *dotted list*, i.e., as a
;;; three-element list with a special cons dot symbol as the second
;;; element. Thus, the cons cell `(a . b)` is represented as the list
;;; `(a <cons-dot> b)`, where `<cons-dot>` is a special value defined
;;; in this file. It is just the symbol `.` (also written `|.|`).
;;;
;;; It is possible to have more than one element before the dot, as in
;;; `(a b c . d)`. Thus, dotted lists can be understood as a
;;; generalization of cons cells, with a cons cell being a dotted list
;;; with two elements.
;;;
;;; It is also possible to represent circular lists in terms of dotted
;;; lists. In Roselisp, a circular list is a dotted list containing
;;; itself.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "./estree"
                  CallExpression
                  Identifier
                  Literal
                  MemberExpression))

;;; Cons dot value.
(define cons-dot_ *cons-dot*)

;;; Compiled cons dot value.
(define cons-dot-compiled_
  ;; `Symbol.for('.')`
  (new CallExpression
       (new MemberExpression
            (new Identifier "Symbol")
            (new Identifier "for"))
       (list (new Literal "."))))

;;; `cons-dot` function.
(define (cons-dot-f_)
  *cons-dot*)

;;; Whether a value is the cons dot.
(define (cons-dot?_ obj)
  (eq? obj *cons-dot*))

;;; Create a cons cell whose CAR is `x` and CDR is `y`.
;;;
;;; Similar to [`cons` in Racket][rkt:cons] and
;;; [`cons` in Common Lisp][cl:cons].
;;;
;;; [rkt:cons]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cons%29%29
;;; [cl:cons]: http://clhs.lisp.se/Body/f_cons.htm
(define (cons_ x y)
  ;; Create an array list whenever possible;
  ;; otherwise create a dotted list.
  (cond
   ((array? y)
    `(,x ,@y))
   (else
    `(,x ,*cons-dot* ,y))))

;;; Whether something is a cons cell.
;;;
;;; Similar to [`cons?` in Racket][rkt:consp] and
;;; [`consp` in Common Lisp][cl:consp].
;;;
;;; [rkt:consp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._cons~3f%29%29
;;; [cl:consp]: http://clhs.lisp.se/Body/f_consp.htm
(define (cons?_ obj)
  ;; All lists except the empty list
  ;; qualify as cons cells.
  (and (array? obj)
       (not (null? obj))))

;;; Make a list.
;;;
;;; Similar to [`list` in Racket][rkt:list] and
;;; [`list` in Common Lisp][cl:list].
;;;
;;; [rkt:list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list%29%29
;;; [cl:list]: http://clhs.lisp.se/Body/f_list_.htm
(define (list_ . args)
  args)

;;; Whether something is a list.
;;;
;;; This function checks whether the list is a proper list, i.e.,
;;; whether the list is terminated by the empty list. In that regard,
;;; this function is more similar to [`list?` in Racket][rkt:listp]
;;; than to [`listp` in Common Lisp][cl:listp].
;;;
;;; [rkt:listp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list~3f%29%29
;;; [cl:listp]: http://clhs.lisp.se/Body/f_listp.htm#listp
(define (list?_ x)
  (null? (last-cdr x)))

;;; Make a dotted list. Like `list`, but the final argument
;;; is used as the tail, instead of as the final element.
;;;
;;;     > (list* 1 '())
;;;     '(1)
;;;     > (list* 1 2)
;;;     '(1 . 2)
;;;     > (list* 1 2 3)
;;;     '(1 2 . 3)
;;;
;;; Also known as `cons*`. Similar to
;;; [`list*` in Racket][rkt:list-star].
;;;
;;; [rkt:list-star]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list%2A%29%29
(define (list-star_ . args)
  (cond
   ((= (array-list-length args) 0)
    undefined)
   ((= (array-list-length args) 1)
    (first args))
   (else
    (define tail-lst
      (array-list-last args))
    (define head-lst
      (drop-right args 1))
    (cond
     ;; Make a proper list if possible.
     ((array? tail-lst)
      `(,@head-lst ,@tail-lst))
     ;; If not, make a dotted list.
     (else
      `(,@head-lst ,*cons-dot* ,tail-lst))))))

;;; Make a list of `n` elements. The function `proc` is applied
;;; to the integers from `0` to `n - 1`.
;;;
;;; Similar to [`build-list` in Racket][rkt:build-list].
;;;
;;; [rkt:build-list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._build-list%29%29
(define (build-list_ n proc)
  (map proc (range 0 n)))

;;; Make a list of length `k`, where every element is the value `v`.
;;;
;;; Similar to [`make-list` in Racket][rkt:make-list].
;;;
;;; [rkt:make-list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._make-list%29%29
(define (make-list_ k v)
  (let ((result '()))
    (for ((i (range 0 k)))
      (push-right! result v))
    result))

;;; List concatenation.
;;;
;;; Similar to [`append` in Racket][rkt:append] and
;;; [`append` in Common Lisp][cl:append].
;;;
;;; [rkt:append]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._append%29%29
;;; [cl:append]: http://clhs.lisp.se/Body/f_append.htm#append
(define (append_ . args)
  (foldl (lambda (x acc)
           (append acc x))
         '()
         args))

;;; Flatten an arbitrarily nested list.
;;;
;;; Similar to [`flatten` in Racket][rkt:flatten].
;;;
;;; [rkt:flatten]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._flatten%29%29
(define (flatten_ lst)
  (foldl (lambda (x acc)
           (cond
            ((array? x)
             (append acc (flatten_ x)))
            ((cons-dot? x)
             acc)
            (else
             (push-right! acc x))))
         '()
         lst))

;;; Return the first element of a list.
;;;
;;; Similar to [`first` in Racket][rkt:first].
;;;
;;; [rkt:first]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._first%29%29
(define (first_ lst)
  (array-first lst))

;;; Return the second element of a list.
;;;
;;; Similar to [`second` in Racket][rkt:second].
;;;
;;; [rkt:second]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._second%29%29
(define (second_ lst)
  (if (linked-list? lst)
      (linked-list-second lst)
      (array-list-second lst)))

;;; Return the third element of a list.
;;;
;;; Similar to [`third` in Racket][rkt:third].
;;;
;;; [rkt:third]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._third%29%29
(define (third_ lst)
  (if (linked-list? lst)
      (linked-list-third lst)
      (array-list-third lst)))

;;; Return the fourth element of a list.
;;;
;;; Similar to [`fourth` in Racket][rkt:fourth].
;;;
;;; [rkt:fourth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fourth%29%29
(define (fourth_ lst)
  (if (linked-list? lst)
      (linked-list-fourth lst)
      (array-list-fourth lst)))

;;; Return the fifth element of a list.
;;;
;;; Similar to [`fifth` in Racket][rkt:fifth].
;;;
;;; [rkt:fifth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fifth%29%29
(define (fifth_ lst)
  (if (linked-list? lst)
      (linked-list-fifth lst)
      (array-list-fifth lst)))

;;; Return the sixth element of a list.
;;;
;;; Similar to [`sixth` in Racket][rkt:sixth].
;;;
;;; [rkt:sixth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._sixth%29%29
(define (sixth_ lst)
  (if (linked-list? lst)
      (linked-list-sixth lst)
      (array-list-sixth lst)))

;;; Return the seventh element of a list.
;;;
;;; Similar to [`seventh` in Racket][rkt:seventh].
;;;
;;; [rkt:seventh]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._seventh%29%29
(define (seventh_ lst)
  (if (linked-list? lst)
      (linked-list-seventh lst)
      (array-list-seventh lst)))

;;; Return the eighth element of a list.
;;;
;;; Similar to [`eighth` in Racket][rkt:eighth].
;;;
;;; [rkt:eighth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._eighth%29%29
(define (eighth_ lst)
  (if (linked-list? lst)
      (linked-list-eighth lst)
      (array-list-eighth lst)))

;;; Return the ninth element of a list.
;;;
;;; Similar to [`ninth` in Racket][rkt:ninth].
;;;
;;; [rkt:ninth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._ninth%29%29
(define (ninth_ lst)
  (if (linked-list? lst)
      (linked-list-ninth lst)
      (array-list-ninth lst)))

;;; Return the tenth element of a list.
;;;
;;; Similar to [`tenth` in Racket][rkt:tenth].
;;;
;;; [rkt:tenth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._tenth%29%29
(define (tenth_ lst)
  (if (linked-list? lst)
      (linked-list-tenth lst)
      (array-list-tenth lst)))

;;; Return the tail of a list.
;;;
;;; Interprets `lst` as a linked list of pairs, and returns the second
;;; element of the first pair.
;;;
;;; Similar to [`cdr` in Racket][rkt:cdr] and
;;; [`cdr` in Common Lisp][cl:cdr].
;;;
;;; [rkt:cdr]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cdr%29%29
;;; [cl:cdr]: http://clhs.lisp.se/Body/f_car_c.htm#cdr
(define (cdr_ lst)
  (if (linked-pair? lst)
      (linked-pair-cdr lst)
      (array-list-cdr lst)))

;;; Return the tail of a list.
;;;
;;; Similar to [`rest` in Racket][rkt:rest].
;;;
;;; [rkt:rest]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._rest%29%29
(define (rest_ lst)
  ;; TODO: Handle linked lists.
  ;; TODO: Alias of `cdr`.
  (array-list-rest lst))

;;; Return the `n`-th element of a list.
;;;
;;; Similar to [`nth` in Racket][rkt:nth] and
;;; [`nth` in Common Lisp][cl:nth].
;;;
;;; [rkt:nth]: https://docs.racket-lang.org/collections/collections-api.html#%28def._%28%28lib._data%2Fcollection..rkt%29._nth%29%29
;;; [cl:nth]: http://clhs.lisp.se/Body/f_nth.htm#nth
(define (nth_ n lst)
  (if (linked-list-link? lst)
      (linked-list-nth n lst)
      (array-list-nth n lst)))

;;; Return the `n`-th CDR element of a list.
;;;
;;; Similar to [`nthcdr` in Common Lisp][cl:nthcdr].
;;;
;;; [cl:nth]: http://clhs.lisp.se/Body/f_nthcdr.htm#nthcdr
(define (nthcdr_ n lst)
  (if (and (= (array-length lst) (+ n 2))
           (cons-dot? (aget lst n)))
      (linked-list-nthcdr n lst)
      (array-list-nthcdr n lst)))

;;; Take the `n` first elements from `lst`.
;;;
;;; Similar to [`take` in Racket][rkt:take].
;;;
;;; [rkt:take]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._take%29%29
(define (take_ lst n)
  (array-list-take lst n))

;;; Return the list obtained by dropping
;;; the first `n` elements from `lst`.
;;;
;;; Similar to [`drop` in Racket][rkt:drop].
;;;
;;; [rkt:drop]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop%29%29
(define (drop_ lst n)
  (array-list-drop lst n))

;;; Return the list obtained by dropping
;;; the last `n` elements from `lst`.
;;;
;;; Similar to [`drop-right` in Racket][rkt:drop-right].
;;;
;;; [rkt:drop-right]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop-right%29%29
(define (drop-right_ lst n)
  (array-list-drop-right lst n))

;;; Reverse the order of a list.
;;; Returns a new list.
;;;
;;; Similar to [`reverse` in Racket][rkt:reverse].
;;;
;;; [rkt:reverse]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._reverse%29%29
(define (reverse_ lst)
  (array-list-reverse lst))

;;; Return a list where the last `n` conses have been omitted.
;;;
;;; Similar to [`butlast` in Common Lisp][cl:butlast].
;;;
;;; [cl:butlast]: http://clhs.lisp.se/Body/f_butlas.htm#butlast
(define (butlast_ x (n 1))
  (let ((result `(,@x))
        (i n))
    (while (and (> i 0)
                (> (array-length result) 0))
      (pop-right! result)
      (set! i (- i 1)))
    result))

;;; Return a list where the last `n` conses have been omitted.
;;; Changes the original list.
;;;
;;; Similar to [`nbutlast` in Common Lisp][cl:nbutlast].
;;;
;;; [cl:nbutlast]: http://clhs.lisp.se/Body/f_butlas.htm#nbutlast
(define (nbutlast_ x (n 1))
  (let ((i n))
    (while (and (> i 0)
                (> (array-length x) 0))
      (pop-right! x)
      (set! i (- i 1)))
    x))

;;; Pop an element off the beginning of a list.
;;;
;;; Similar to [`pop` in Common Lisp][cl:pop].
;;;
;;; [cl:pop]: http://clhs.lisp.se/Body/m_pop.htm#pop
(define (pop-left!_ lst)
  (send lst shift))

;;; Pop an element off the end of a list.
;;;
;;; Similar to [`Array.prototype.pop()` in JavaScript][js:pop].
;;;
;;; [js:pop]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop
(define (pop-right!_ lst)
  (send lst pop))

;;; Push an element onto the beginning of a list.
;;;
;;; Similar to [`push` in Common Lisp][cl:push].
;;;
;;; [cl:push]: http://clhs.lisp.se/Body/m_push.htm#push
(define (push-left!_ lst x)
  (send lst unshift x)
  lst)

;;; Push an element onto the end of a list.
;;;
;;; Similar to [`Array.prototype.push()` in JavaScript][js:push].
;;;
;;; [js:push]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push
(define (push-right!_ lst x)
  (send lst push x)
  lst)

;;; Return the length of a list.
;;;
;;; Similar to [`length` in Racket][rkt:length] and
;;; [`length` in Common Lisp][cl:length].
;;;
;;; [rkt:length]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._length%29%29
;;; [cl:length]: http://clhs.lisp.se/Body/f_length.htm#length
(define (length_ lst)
  (if (linked-list-link? lst)
      (linked-list-length lst)
      (array-list-length lst)))

;;; Return the last element of a list.
;;;
;;; Similar to [`last` in Racket][rkt:last].
;;;
;;; [rkt:last]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last%29%29
(define (last_ lst)
  (if (linked-list-link? lst)
      (linked-list-last lst)
      (array-list-last lst)))

;;; Return the last pair of a list.
;;;
;;; Similar to [`last-pair` in Racket][rkt:last-pair] and
;;; [`last` in Common Lisp][cl:last].
;;;
;;; [rkt:last-pair]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last-pair%29%29
;;; [cl:last]: http://clhs.lisp.se/Body/f_last.htm#last
(define (last-pair_ lst)
  (cond
   ((not (array? lst))
    undefined)
   ((null? lst)
    lst)
   ((linked-list-link? lst)
    (define current lst)
    (define result undefined)
    (while (and (linked-list-link? current)
                (not (null? (linked-list-tail current))))
      (set! current (linked-list-tail current)))
    result)
   (else
    (array-list-drop lst (- (array-list-length lst) 1)))))

;;; Return the last cdr of a list, i.e., the terminating empty list.
(define (last-cdr_ lst)
  (cond
   ((not (array? lst))
    undefined)
   ((linked-list-link? lst)
    (define result lst)
    (while (linked-list-link? result)
      (set! result (linked-list-tail result)))
    result)
   (else
    '())))

;;; Set the CAR of a list.
;;;
;;; Similar to [`set-car!` in Racket][rkt:set-car].
;;;
;;; [rkt:set-car]: https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html#%25_idx_416
(define (set-car!_ x y)
  (when (> (array-length x) 0)
    (aset! x 0 y))
  undefined)

;;; Set the CDR of a list.
;;;
;;; Similar to [`set-cdr!` in Racket][rkt:set-cdr].
;;;
;;; [rkt:set-cdr]: https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html#%25_idx_418
(define (set-cdr!_ x y)
  (cond
   ((null? x))
   ((eq? x y)
    (cond
     ((linked-list-link? x)
      (aset! x (- (array-length x) 1) y))
     (else
      (push-right! x *cons-dot*)
      (push-right! x y))))
   (else
    (while (> (array-length x) 1)
      (pop-right! x))
    (cond
     ((array? y)
      (for ((z y))
        (push-right! x z)))
     (else
      (push-right! x *cons-dot*)
      (push-right! x y)))))
  undefined)

;;; Whether something is the empty list.
;;;
;;; Similar to [`null?` in Racket][rkt:nullp].
;;;
;;; [rkt:nullp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._null~3f%29%29
(define (null?_ x)
  (and (array? x)
       (= (array-length x) 0)))

;;; Whether something is an array list.
;;;
;;; An array list is a list implemented in terms of an array.
;;; It corresponds roughly to the
;;; [`ArrayList` class in Java][java:ArrayList].
;;;
;;; [java:ArrayList]: https://docs.oracle.com/javase/8/docs/api/java/util/ArrayList.html
(define (array-list?_ x)
  (array? x))

;;; Return the length of an array list.
(define (array-list-length_ lst)
  (array-length lst))

;;; Return the first element of an array list.
(define (array-list-first_ lst)
  (array-first lst))

;;; Return the second element of an array list.
(define (array-list-second_ lst)
  (array-second lst))

;;; Return the third element of an array list.
(define (array-list-third_ lst)
  (array-third lst))

;;; Return the fourth element of an array list.
(define (array-list-fourth_ lst)
  (array-fourth lst))

;;; Return the fifth element of an array list.
(define (array-list-fifth_ lst)
  (array-fifth lst))

;;; Return the sixth element of an array list.
(define (array-list-sixth_ lst)
  (array-sixth lst))

;;; Return the seventh element of an array list.
(define (array-list-seventh_ lst)
  (array-seventh lst))

;;; Return the eighth element of an array list.
(define (array-list-eighth_ lst)
  (array-eighth lst))

;;; Return the ninth element of an array list.
(define (array-list-ninth_ lst)
  (array-ninth lst))

;;; Return the tenth element of an array list.
(define (array-list-tenth_ lst)
  (array-tenth lst))

;;; Return the last element of an array list.
(define (array-list-last_ lst)
  (array-last lst))

;;; Return the `n`-th element of an array list.
(define (array-list-nth_ n lst)
  (aget lst n))

;;; Return the `n`-th CDR of an array list.
(define (array-list-nthcdr_ n lst)
  (array-drop lst n))

;;; Return the CDR of an array list.
(define (array-list-cdr_ lst)
  (array-rest lst))

;;; Return the tail of an array list.
(define (array-list-rest_ lst)
  (array-list-cdr lst))

;;; Take the `n` first elements from an array list.
(define (array-list-take_ lst n)
  (array-take lst n))

;;; Return the list obtained by dropping
;;; the first `n` elements from an array list.
(define (array-list-drop_ lst n)
  (array-drop lst n))

;;; Return the list obtained by dropping
;;; the last `n` elements from an array list.
(define (array-list-drop-right_ lst n)
  (array-drop-right lst n))

;;; Reverse the order of an array list.
;;; Returns a new array list.
(define (array-list-reverse_ lst)
  (array-reverse lst))

;;; Whether something is a dotted list.
;;;
;;; Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
;;;
;;; [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
(define (dotted-list?_ x)
  (and (array? x)
       (>= (array-length x) 3)
       (cons-dot? (aget x (- (array-length x) 2)))
       (not (null? (last-cdr x)))))

;;; Whether something is a linked list.
;;;
;;; An linked list is a list implemented as a chain of links.
;;; It corresponds roughly to the
;;; [`LinkedList` class in Java][java:LinkedList].
;;;
;;; [java:LinkedList]: https://docs.oracle.com/javase/8/docs/api/java/util/LinkedList.html
(define (linked-list?_ x)
  (and (array? x)
       (>= (array-length x) 3)
       (cons-dot? (aget x (- (array-length x) 2)))
       (null? (last-cdr x))))

;;; Whether something is a linked list link.
(define (linked-list-link?_ x)
  (and (array? x)
       (>= (array-length x) 3)
       (cons-dot? (aget x (- (array-length x) 2)))))

;;; Whether something is a linked pair.
(define (linked-pair?_ x)
  (and (array? x)
       (= (array-length x) 3)
       (cons-dot? (aget x 1))))

;;; Return the CAR of a linked list link.
(define (linked-list-link-car_ x)
  (array-first x))

;;; Return the CDR of a linked list link.
(define (linked-list-link-cdr_ x)
  (array-last x))

;;; Return the CAR of a linked pair.
(define (linked-pair-car_ x)
  (array-first x))

;;; Return the CDR of a linked pair.
(define (linked-pair-cdr_ x)
  (array-third x))

;;; Return the length of a linked list.
(define (linked-list-length_ lst)
  (define len 0)
  (define current lst)
  (while (linked-list-link? current)
    (set! len (+ len (- (array-length lst) 2)))
    (set! current (linked-list-tail current)))
  len)

;;; Return the first element of a linked list.
(define (linked-list-first_ lst)
  (array-first lst))

;;; Return the second element of a linked list.
(define (linked-list-second_ lst)
  (linked-list-nth 1 lst))

;;; Return the third element of a linked list.
(define (linked-list-third_ lst)
  (linked-list-nth 2 lst))

;;; Return the fourth element of a linked list.
(define (linked-list-fourth_ lst)
  (linked-list-nth 3 lst))

;;; Return the fifth element of a linked list.
(define (linked-list-fifth_ lst)
  (linked-list-nth 4 lst))

;;; Return the sixth element of a linked list.
(define (linked-list-sixth_ lst)
  (linked-list-nth 5 lst))

;;; Return the seventh element of a linked list.
(define (linked-list-seventh_ lst)
  (linked-list-nth 6 lst))

;;; Return the eighth element of a linked list.
(define (linked-list-eighth_ lst)
  (linked-list-nth 7 lst))

;;; Return the ninth element of a linkedd list.
(define (linked-list-ninth_ lst)
  (linked-list-nth 8 lst))

;;; Return the tenth element of a linkedd list.
(define (linked-list-tenth_ lst)
  (linked-list-nth 9 lst))

;;; Return the last element of a linked list.
(define (linked-list-last_ lst)
  (define current lst)
  (define result undefined)
  (while (and (linked-list-link? current)
              (not (null? (linked-list-link-cdr current))))
    (set! current (linked-list-link-cdr current)))
  (when (linked-list-link? current)
    (set! result
          (aget current (- (array-length current) 3))))
  result)

;;; Return the `n`-th element of a linked list.
(define (linked-list-nth_ n lst)
  (define i n)
  (define result lst)
  (while (> i 0)
    (cond
     ((dotted-pair? result)
      (set! result (linked-list-tail lst)))
     (else
      (set! result (array-rest lst))))
    (set! i (- i 1)))
  (when (array? result)
    (set! result (array-first result)))
  result)

;;; Return the `n`-th CDR of a linked list.
(define (linked-list-nthcdr_ n lst)
  (aget lst (- (array-length lst) 1)))

;;; Return the list obtained by dropping
;;; the first `n` elements from a linked list.
(define (linked-list-drop_ lst pos)
  ;; TODO: Linked lists.
  (array-drop lst pos))

;;; Return the list obtained by dropping
;;; the last `n` elements from a linked list.
(define (linked-list-drop-right_ lst n)
  ;; TODO: Linked lists.
  (array-drop-right lst (+ n 1)))

;;; Return the CAR of a linked list.
(define (linked-list-car_ lst)
  (array-first lst))

;;; Return the CDR of a linked list.
(define (linked-list-cdr_ lst)
  (cond
   ((dotted-pair? lst)
    (array-third lst))
   (else
    (array-rest lst))))

;;; Return the head of a linked list.
(define (linked-list-head_ lst)
  ;; TODO: Rename to `linked-list-link-head`.
  (drop-right lst 2))

;;; Return the tail of a linked list.
(define (linked-list-tail_ lst)
  ;; TODO: Rename to `linked-list-link-tail`.
  (array-last lst))

;;; Parse a linked list.
(define (linked-list-parse_ lst)
  (values (linked-list-head_ lst)
          (linked-list-tail_ lst)))

;;; Make a linked list.
(define (make-dotted-list_ car cdr)
  ;; TODO: Rename to `make-linked-list`.
  (list-star_ car cdr))

;;; Make a linked pair.
(define (make-pair_ car cdr)
  ;; TODO: Rename to `make-linked-pair`.
  `(,car ,*cons-dot* ,cdr))

;;; Whether something is a proper list,
;;; i.e., a list that is terminated by
;;; the empty list.
;;;
;;; Similar to [`proper-list?` in Racket][rkt:proper-list-p].
;;;
;;; [rkt:proper-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#proper-list-p
(define (proper-list?_ x)
  (null? (last-cdr x)))

;;; Whether something is an improper list,
;;; i.e., a dotted list.
;;;
;;; Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
;;;
;;; [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
(define (improper-list?_ x)
  (not (null? (last-cdr x))))

;;; Whether something is a circular list.
;;;
;;; Similar to [`circular-list?` in Racket][rkt:circular-list-p].
;;;
;;; [rkt:circular-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#circular-list-p
(define (circular-list?_ x)
  (and (linked-list-link? x)
       (eq? (linked-list-tail x) x)))

;;; Convert an array list to a linked list.
(define (array-list->linked-list_ x)
  `(,@(drop-right x 1) ,*cons-dot* ,(array-list-last x)))

;;; Convert a linked list to an array list.
(define (linked-list->array-list_ x)
  `(,@(linked-list-head x) ,(linked-list-tail x)))

(provide
  (rename-out (append_ append))
  (rename-out (array-list->linked-list_ array-list-to-linked-list_))
  (rename-out (array-list->linked-list_ proper-list->dotted-list_))
  (rename-out (butlast_ butlast))
  (rename-out (cdr_ cdr))
  (rename-out (cdr_ tail))
  (rename-out (cdr_ tail_))
  (rename-out (cons?_ pair?))
  (rename-out (cons?_ pair?_))
  (rename-out (dotted-list?_ dotted-list-p))
  (rename-out (dotted-list?_ dotted-list?))
  (rename-out (drop-right_ drop-right))
  (rename-out (drop_ drop))
  (rename-out (drop_ list-tail))
  (rename-out (drop_ list-tail_))
  (rename-out (eighth_ eighth))
  (rename-out (fifth_ fifth))
  (rename-out (first_ car))
  (rename-out (first_ car_))
  (rename-out (first_ first))
  (rename-out (first_ head))
  (rename-out (flatten_ flatten))
  (rename-out (fourth_ fourth))
  (rename-out (last-cdr_ linked-list-last-cdr_))
  (rename-out (last-pair_ last-cons_))
  (rename-out (last-pair_ linked-list-last-cons_))
  (rename-out (last-pair_ linked-list-last-pair_))
  (rename-out (last_ last))
  (rename-out (length_ length))
  (rename-out (linked-list->array-list_ linked-list-to-array-list_))
  (rename-out (linked-list-last_ linked-list-last_))
  (rename-out (linked-pair?_ dotted-pair-p_))
  (rename-out (list-star_ list-star))
  (rename-out (list?_ list?))
  (rename-out (list?_ listp))
  (rename-out (list?_ listp_))
  (rename-out (list?_ proper-list?))
  (rename-out (list_ list))
  (rename-out (make-list_ make-list))
  (rename-out (nbutlast_ nbutlast))
  (rename-out (ninth_ ninth))
  (rename-out (nth_ nth))
  (rename-out (nthcdr_ nthcdr))
  (rename-out (null?_ nullp))
  (rename-out (null?_ nullp_))
  (rename-out (pop-left!_ pop!))
  (rename-out (pop-left!_ pop!_))
  (rename-out (pop-left!_ pop))
  (rename-out (pop-left!_ pop-left!))
  (rename-out (pop-left!_ pop-left))
  (rename-out (pop-left!_ pop-left_))
  (rename-out (pop-left!_ pop_))
  (rename-out (pop-right!_ pop-right!))
  (rename-out (pop-right!_ pop-right))
  (rename-out (pop-right!_ pop-right_))
  (rename-out (push-left!_ push!))
  (rename-out (push-left!_ push!_))
  (rename-out (push-left!_ push))
  (rename-out (push-left!_ push-left!))
  (rename-out (push-left!_ push-left))
  (rename-out (push-left!_ push-left_))
  (rename-out (push-left!_ push_))
  (rename-out (push-right!_ append-to-list))
  (rename-out (push-right!_ push-right!))
  (rename-out (push-right!_ push-right))
  (rename-out (push-right!_ push-right_))
  (rename-out (rest_ rest))
  (rename-out (reverse_ reverse))
  (rename-out (second_ cadr))
  (rename-out (second_ cadr_))
  (rename-out (second_ second))
  (rename-out (set-car!_ set-car_))
  (rename-out (set-cdr!_ set-cdr_))
  (rename-out (seventh_ seventh))
  (rename-out (sixth_ sixth))
  (rename-out (tenth_ tenth))
  (rename-out (third_ third))
  ;; (rename-out (last-cdr_ dotted-list-last-cdr_))
  ;; (rename-out (last-pair_ dotted-list-last-cons_))
  ;; (rename-out (last-pair_ dotted-list-last-pair_))
  ;; (rename-out (linked-list-car_ dotted-list-car_))
  ;; (rename-out (linked-list-cdr_ dotted-list-cdr_))
  ;; (rename-out (linked-list-drop-right_ dotted-list-drop-right_))
  ;; (rename-out (linked-list-drop_ dotted-list-drop_))
  ;; (rename-out (linked-list-head_ dotted-list-head_))
  ;; (rename-out (linked-list-length_ dotted-list-length_))
  ;; (rename-out (linked-list-nth_ dotted-list-nth_))
  ;; (rename-out (linked-list-nthcdr_ dotted-list-nthcdr_))
  ;; (rename-out (linked-list-parse_ dotted-list-parse_))
  ;; (rename-out (linked-list-tail_ dotted-list-tail_))
  ;; (rename-out (linked-list-to-array-list_ dotted-list-to-proper-list_))
  ;; (rename-out (linked-pair-cdr_ dotted-pair-cdr_))
  ;; (rename-out (list?_ proper-list-p))
  ;; (rename-out (list?_ proper-list-p_))
  append_
  array-list->linked-list_
  array-list-cdr_
  array-list-drop-right_
  array-list-drop_
  array-list-eighth_
  array-list-fifth_
  array-list-first_
  array-list-fourth_
  array-list-last_
  array-list-length_
  array-list-ninth_
  array-list-nth_
  array-list-nthcdr_
  array-list-rest_
  array-list-reverse_
  array-list-second_
  array-list-seventh_
  array-list-sixth_
  array-list-take_
  array-list-tenth_
  array-list-third_
  array-list?_
  build-list_
  butlast_
  cdr_
  circular-list?_
  cons-dot-compiled_
  cons-dot-f_
  cons-dot?_
  cons-dot_
  cons?_
  cons_
  dotted-list?_
  drop-right_
  drop_
  eighth_
  fifth_
  first_
  flatten_
  fourth_
  improper-list?_
  last-cdr_
  last-pair_
  last_
  length_
  linked-list->array-list_
  linked-list-car_
  linked-list-cdr_
  linked-list-drop-right_
  linked-list-drop_
  linked-list-eighth_
  linked-list-fifth_
  linked-list-first_
  linked-list-fourth_
  linked-list-head_
  linked-list-last_
  linked-list-length_
  linked-list-link-car_
  linked-list-link-cdr_
  linked-list-link?_
  linked-list-ninth_
  linked-list-nth_
  linked-list-nthcdr_
  linked-list-parse_
  linked-list-second_
  linked-list-seventh_
  linked-list-sixth_
  linked-list-tail_
  linked-list-tenth_
  linked-list-third_
  linked-list?_
  linked-pair-car_
  linked-pair-cdr_
  linked-pair-cdr_
  linked-pair?_
  list-star_
  list?_
  list_
  make-dotted-list_
  make-list_
  make-pair_
  nbutlast_
  ninth_
  nth_
  nthcdr_
  null?_
  pop-left!_
  pop-right!_
  proper-list?_
  push-left!_
  push-right!_
  rest_
  reverse_
  second_
  set-car!_
  set-cdr!_
  seventh_
  sixth_
  take_
  tenth_
  third_)
