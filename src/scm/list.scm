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
;;; *cons cell*, such as `'(1 . 2)`. Instead, such values are
;;; represented as a *dotted list*, which is a list where the
;;; penultimate value is the symbol `|.|` (in JavaScript,
;;; `Symbol.for('.')`).
;;;
;;;     > (dotted-list? '(1 . 2))
;;;     #t
;;;     > (dotted-list? '(1 2 . 3))
;;;     #t
;;;     > (dotted-list? (list 1 '|.| 2))
;;;     #t
;;;
;;; The simplifying assumption is made that dotted lists are only used
;;; to represent values that cannot be expressed without a dot. This
;;; permits most list functions to be implemented as simple array
;;; operations, although some checks are necessary in places since
;;; dotted lists are also implemented as arrays.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; (require (only-in "./estree"
;;                   CallExpression
;;                   Identifier
;;                   Literal
;;                   MemberExpression))

;;; Whether something is a pair, i.e., a cons cell.
;;;
;;; Similar to [`pair?` in Racket][rkt:pairp] and
;;; [`consp` in Common Lisp][cl:consp].
;;;
;;; [rkt:pairp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._pair~3f%29%29
;;; [cl:consp]: http://clhs.lisp.se/Body/f_consp.htm
(define (pair?_ x)
  ;; All lists except the empty list qualify as pairs.
  (and (array? x)
       (> (array-length x) 0)))

;;; Whether something is the empty list.
;;;
;;; Similar to [`null?` in Racket][rkt:nullp].
;;;
;;; [rkt:nullp]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._null~3f%29%29
(define (null?_ x)
  (and (array? x)
       (= (array-length x) 0)))

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

;;; Whether something is a pair or a list.
;;;
;;; Similar to [`listp` in Common Lisp][cl:listp] and
;;; [`listp` in Emacs Lisp], which are not as
;;; rigorous as `list?` in Scheme.
;;;
;;; [cl:listp]: http://clhs.lisp.se/Body/f_listp.htm#listp
;;; [el:listp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/List_002drelated-Predicates.html#index-listp
(define (pair-or-list?_ x)
  (or (pair? x)
      (null? x)))

;;; Make a list.
;;;
;;; Similar to [`list` in Racket][rkt:list] and
;;; [`list` in Common Lisp][cl:list].
;;;
;;; [rkt:list]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list%29%29
;;; [cl:list]: http://clhs.lisp.se/Body/f_list_.htm
(define (list_ . args)
  args)

;;; Create a cons cell whose CAR is `x` and CDR is `y`.
;;;
;;; Similar to [`cons` in Racket][rkt:cons] and
;;; [`cons` in Common Lisp][cl:cons].
;;;
;;; [rkt:cons]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cons%29%29
;;; [cl:cons]: http://clhs.lisp.se/Body/f_cons.htm
(define (cons_ x y)
  ;; Create a regular list whenever possible;
  ;; otherwise create a dotted list.
  `(,x ,@(dotted-list-link y)))

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
   ((= (length args) 0)
    #u)
   ((= (length args) 1)
    (first args))
   (else
    (define tail-lst
      (last args))
    (define head-lst
      (drop-right args 1))
    (cond
     ;; Make a proper list if possible.
     ((pair-or-list? tail-lst)
      `(,@head-lst ,@tail-lst))
     ;; If not, make a dotted list.
     (else
      `(,@head-lst . ,tail-lst))))))

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
            ((pair-or-list? x)
             (append acc (flatten_ x)))
            ((eq? x '|.|)
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
  (if (dotted-list? lst)
      (dotted-list-second lst)
      (array-second lst)))

;;; Return the third element of a list.
;;;
;;; Similar to [`third` in Racket][rkt:third].
;;;
;;; [rkt:third]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._third%29%29
(define (third_ lst)
  (if (dotted-list? lst)
      (dotted-list-third lst)
      (array-third lst)))

;;; Return the fourth element of a list.
;;;
;;; Similar to [`fourth` in Racket][rkt:fourth].
;;;
;;; [rkt:fourth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fourth%29%29
(define (fourth_ lst)
  (if (dotted-list? lst)
      (dotted-list-fourth lst)
      (array-fourth lst)))

;;; Return the fifth element of a list.
;;;
;;; Similar to [`fifth` in Racket][rkt:fifth].
;;;
;;; [rkt:fifth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._fifth%29%29
(define (fifth_ lst)
  (if (dotted-list? lst)
      (dotted-list-fifth lst)
      (array-fifth lst)))

;;; Return the sixth element of a list.
;;;
;;; Similar to [`sixth` in Racket][rkt:sixth].
;;;
;;; [rkt:sixth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._sixth%29%29
(define (sixth_ lst)
  (if (dotted-list? lst)
      (dotted-list-sixth lst)
      (array-sixth lst)))

;;; Return the seventh element of a list.
;;;
;;; Similar to [`seventh` in Racket][rkt:seventh].
;;;
;;; [rkt:seventh]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._seventh%29%29
(define (seventh_ lst)
  (if (dotted-list? lst)
      (dotted-list-seventh lst)
      (array-seventh lst)))

;;; Return the eighth element of a list.
;;;
;;; Similar to [`eighth` in Racket][rkt:eighth].
;;;
;;; [rkt:eighth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._eighth%29%29
(define (eighth_ lst)
  (if (dotted-list? lst)
      (dotted-list-eighth lst)
      (array-eighth lst)))

;;; Return the ninth element of a list.
;;;
;;; Similar to [`ninth` in Racket][rkt:ninth].
;;;
;;; [rkt:ninth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._ninth%29%29
(define (ninth_ lst)
  (if (dotted-list? lst)
      (dotted-list-ninth lst)
      (array-ninth lst)))

;;; Return the tenth element of a list.
;;;
;;; Similar to [`tenth` in Racket][rkt:tenth].
;;;
;;; [rkt:tenth]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._tenth%29%29
(define (tenth_ lst)
  (if (dotted-list? lst)
      (dotted-list-tenth lst)
      (array-tenth lst)))

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
  (if (dotted-pair? lst)
      (array-third lst)
      (array-rest lst)))

;;; Return the tail of a list.
;;;
;;; Similar to [`rest` in Racket][rkt:rest].
;;;
;;; [rkt:rest]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._rest%29%29
(define (rest_ lst)
  (if (dotted-pair? lst)
      (array-third lst)
      (array-rest lst)))

;;; Access the list element indicated by
;;; one or more `indices`.
(define (list-ref_ lst . indices)
  (cond
   ((dotted-list? lst)
    (apply dotted-list-ref_ lst indices))
   (else
    (define result lst)
    (for ((i indices))
      (set! result (array-ref lst i)))
    result)))

;;; Return the `n`-th element of a list.
;;;
;;; Similar to [`nth` in Racket][rkt:nth] and
;;; [`nth` in Common Lisp][cl:nth].
;;;
;;; [rkt:nth]: https://docs.racket-lang.org/collections/collections-api.html#%28def._%28%28lib._data%2Fcollection..rkt%29._nth%29%29
;;; [cl:nth]: http://clhs.lisp.se/Body/f_nth.htm#nth
(define (nth_ n lst)
  (list-ref_ lst n))

;;; Set a list position to a given value.
;;; Returns a new list.
(define (list-set_ lst . indices-and-value)
  (cond
   ((dotted-list? lst)
    (apply dotted-list-set_ lst indices-and-value))
   (else
    (define result
      `(,@lst))
    (cond
     ((> (length indices-and-value) 2)
      (define-values (pos . indices-and-value-1)
        indices-and-value)
      (array-set! result
                  pos
                  (apply list-set_
                         (array-ref result pos)
                         indices-and-value-1)))
     (else
      (define-values (pos val)
        indices-and-value)
      (array-set! result pos val)))
    result)))

;;; Set a list position to a given value.
;;; Modifies the original list.
(define (list-set!_ lst . indices-and-value)
  (cond
   ((dotted-list? lst)
    (apply dotted-list-set!_ lst indices-and-value))
   (else
    (define indices
      (drop-right indices-and-value 1))
    (define first-indices
      (drop-right indices 1))
    (define last-index
      (last indices))
    (define value
      (last indices-and-value))
    (define lst1 lst)
    (for ((i first-indices))
      (set! lst1 (list-ref lst1 i)))
    (array-set! lst1 last-index value)
    value)))

;;; Return the `n`-th CDR element of a list.
(define (list-tail_ lst n)
  (define result lst)
  (define i n)
  (while (> i 0)
    (set! result (cdr result))
    (set! i (- i 1)))
  result)

;;; Return the `n`-th CDR element of a list.
;;;
;;; Similar to [`nthcdr` in Common Lisp][cl:nthcdr].
;;;
;;; [cl:nth]: http://clhs.lisp.se/Body/f_nthcdr.htm#nthcdr
(define (nthcdr_ n lst)
  (define result lst)
  (define i n)
  (while (> i 0)
    (set! result (cdr result))
    (set! i (- i 1)))
  result)

;;; Take the `n` first elements from `lst`.
;;;
;;; Similar to [`take` in Racket][rkt:take].
;;;
;;; [rkt:take]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._take%29%29
(define (take_ lst n)
  (drop-right lst (- (length lst) n)))

;;; Return the list obtained by dropping
;;; the first `n` elements from `lst`.
;;;
;;; Similar to [`drop` in Racket][rkt:drop].
;;;
;;; [rkt:drop]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop%29%29
(define (drop_ lst n)
  (array-drop lst n))

;;; Return the list obtained by dropping
;;; the last `n` elements from `lst`.
;;;
;;; Similar to [`drop-right` in Racket][rkt:drop-right].
;;;
;;; [rkt:drop-right]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop-right%29%29
(define (drop-right_ lst n)
  (array-drop-right lst n))

;;; Reverse the order of a list.
;;; Returns a new list.
;;;
;;; Similar to [`reverse` in Racket][rkt:reverse].
;;;
;;; [rkt:reverse]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._reverse%29%29
(define (reverse_ lst)
  (array-reverse lst))

;;; Reverse the order of a list.
(define (reverse!_ lst)
  (array-reverse! lst))

;;; Return a list where the last `n` conses have been omitted.
;;;
;;; Similar to [`butlast` in Common Lisp][cl:butlast].
;;;
;;; [cl:butlast]: http://clhs.lisp.se/Body/f_butlas.htm#butlast
(define (butlast_ x (n 1))
  (let ((result `(,@x))
        (i n))
    (while (and (> i 0)
                (> (length result) 0))
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
                (> (length x) 0))
      (pop-right! x)
      (set! i (- i 1)))
    x))

;;; Pop an element off the beginning of a list.
;;;
;;; Similar to [`pop` in Common Lisp][cl:pop].
;;;
;;; [cl:pop]: http://clhs.lisp.se/Body/m_pop.htm#pop
(define (pop-left!_ lst)
  (array-pop-left! lst))

;;; Pop an element off the end of a list.
;;;
;;; Similar to [`Array.prototype.pop()` in JavaScript][js:pop].
;;;
;;; [js:pop]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop
(define (pop-right!_ lst)
  (array-pop-right! lst))

;;; Push an element onto the beginning of a list.
;;;
;;; Similar to [`push` in Common Lisp][cl:push].
;;;
;;; [cl:push]: http://clhs.lisp.se/Body/m_push.htm#push
(define (push-left!_ lst x)
  (array-push-left! lst x))

;;; Push an element onto the end of a list.
;;;
;;; Similar to [`Array.prototype.push()` in JavaScript][js:push].
;;;
;;; [js:push]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push
(define (push-right!_ lst x)
  (array-push-right! lst x))

;;; Return the length of a list.
;;;
;;; Similar to [`length` in Racket][rkt:length] and
;;; [`length` in Common Lisp][cl:length].
;;;
;;; [rkt:length]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._length%29%29
;;; [cl:length]: http://clhs.lisp.se/Body/f_length.htm#length
(define (length_ lst)
  (if (dotted-list? lst)
      (dotted-list-length lst)
      (array-length lst)))

;;; Return the last element of a list.
;;;
;;; Similar to [`last` in Racket][rkt:last].
;;;
;;; [rkt:last]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last%29%29
(define (last_ lst)
  (if (dotted-list? lst)
      (dotted-list-last lst)
      (array-last lst)))

;;; Return the last pair of a list.
;;;
;;; Similar to [`last-pair` in Racket][rkt:last-pair] and
;;; [`last` in Common Lisp][cl:last].
;;;
;;; [rkt:last-pair]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._last-pair%29%29
;;; [cl:last]: http://clhs.lisp.se/Body/f_last.htm#last
(define (last-pair_ lst)
  (cond
   ((not (pair-or-list? lst))
    #u)
   ((null? lst)
    lst)
   ((dotted-list? lst)
    (define current lst)
    (define result #u)
    (while (and (dotted-list? current)
                (not (null? (dotted-list-tail current))))
      (set! current (dotted-list-tail current)))
    result)
   (else
    (array-drop lst (- (array-length lst) 1)))))

;;; Return the last cdr of a list, i.e., the terminating empty list.
(define (last-cdr_ lst)
  (cond
   ((not (pair-or-list? lst))
    #u)
   ((dotted-list? lst)
    (define result lst)
    (while (dotted-list? result)
      (set! result (dotted-list-tail result)))
    result)
   (else
    '())))

;;; Set the CAR of a list.
;;;
;;; Similar to [`set-car!` in Racket][rkt:set-car].
;;;
;;; [rkt:set-car]: https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html#%25_idx_416
(define (set-car!_ x y)
  (when (> (length x) 0)
    (list-set! x 0 y))
  #u)

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
     ((dotted-list? x)
      (array-set! x (- (array-length x) 1) y))
     (else
      (push-right! x '|.|)
      (push-right! x y))))
   (else
    (while (> (array-length x) 1)
      (pop-right! x))
    (cond
     ((pair-or-list? y)
      (for ((z y))
        (push-right! x z)))
     (else
      (push-right! x '|.|)
      (push-right! x y)))))
  #u)

;;; Whether something is a dotted list.
;;;
;;; Similar to [`dotted-list?` in Racket][rkt:dotted-list-p].
;;;
;;; [rkt:dotted-list-p]: https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html#dotted-list-p
(define (dotted-list?_ x)
  (and (array? x)
       (>= (array-length x) 3)
       (eq? (array-nlast x 2) '|.|)))

;;; Whether something is a dotted pair.
(define (dotted-pair?_ x)
  (and (array? x)
       (= (array-length x) 3)
       (eq? (array-ref x 1) '|.|)))

;;; Whether something is a proper dotted list.
(define (dotted-proper-list?_ x)
  (and (array? x)
       (>= (array-length x) 3)
       (eq? (array-nlast x 2) '|.|)
       (null? (last-cdr x))))

;;; Whether something is an improper dotted list.
(define (dotted-improper-list?_ x)
  (and (array? x)
       (>= (array-length x) 3)
       (eq? (array-nlast x 2) '|.|)
       (not (null? (last-cdr x)))))

;;; Return the head of a dotted list.
(define (dotted-list-head_ lst)
  (array-drop-right lst 2))

;;; Return the tail of a dotted list.
(define (dotted-list-tail_ lst)
  (array-last lst))

;;; Create a dotted list link.
(define (dotted-list-link_ x)
  (if (pair-or-list? x)
      x
      (list '|.| x)))

;;; Return the CDR of a dotted pair.
(define (dotted-pair-cdr_ x)
  (array-third x))

;;; Parse a dotted list.
(define (dotted-list-parse_ lst)
  (values (dotted-list-head_ lst)
          (dotted-list-tail_ lst)))

;;; Return the length of a dotted list.
(define (dotted-list-length_ lst)
  (define len 0)
  (define current lst)
  (while (dotted-list? current)
    (set! len
          (+ len
             (- (array-length lst) 2)))
    (set! current (dotted-list-tail current)))
  len)

;;; Access the dotted list element indicated by
;;; one or more `indices`.
(define (dotted-list-ref_ lst . indices)
  (define result lst)
  (for ((i indices))
    (while (> i 0)
      (cond
       ((< i (- (array-length result) 2))
        (break))
       (else
        (set! i (- i (- (array-length result) 2)))
        (set! result (dotted-list-tail result)))))
    (when (pair-or-list? result)
      (set! result (array-ref result i))))
  result)

;;; Set a dotted list position to a given value.
;;; Returns a new list.
(define (dotted-list-set_ lst . indices-and-value)
  (cond
   ((> (length indices-and-value) 2)
    (define-values (pos . indices-and-value-1)
      indices-and-value)
    (cond
     ((< pos (- (array-length lst) 2))
      (define result `(,@lst))
      (array-set! result
                  pos
                  (apply dotted-list-set_
                         (array-ref result pos)
                         indices-and-value-1))
      result)
     (else
      (append (array-drop-right lst 1)
              (list
               (dotted-list-set_
                (array-last lst)
                `(,(- pos (- (array-length lst) 2))
                  ,@indices-and-value-1)))))))
   (else
    (define-values (pos val)
      indices-and-value)
    (cond
     ((< pos (- (array-length lst) 2))
      (define result `(,@lst))
      (array-set! result pos val)
      result)
     (else
      (append (array-drop-right lst 1)
              (list
               (dotted-list-set_
                (array-last lst)
                (- pos (- (array-length lst) 2))
                val))))))))

;;; Set a dotted list position to a given value.
;;; Modifies the original list.
(define (dotted-list-set!_ lst . indices-and-value)
  (define indices
    (drop-right indices-and-value 1))
  (define indices1
    (drop-right indices 1))
  (define last-index
    (last indices))
  (define value
    (last indices-and-value))
  (define lst1 lst)
  (for ((i indices1))
    (while (> i 0)
      (cond
       ((< i (- (array-length lst1) 2))
        (break))
       (else
        (set! i (- i (- (array-length lst1) 2)))
        (set! lst1 (dotted-list-tail lst1)))))
    (when (pair-or-list? lst1)
      (set! lst1 (list-ref lst1 i))))
  (while (> last-index 0)
    (cond
     ((< last-index (- (array-length lst1) 2))
      (break))
     (else
      (set! last-index
            (- last-index (- (array-length lst1) 2)))
      (set! lst1 (dotted-list-tail lst1)))))
  (array-set! lst1 last-index value)
  value)

;;; Return the first element of a dotted list.
(define (dotted-list-first_ lst)
  (array-first lst))

;;; Return the second element of a dotted list.
(define (dotted-list-second_ lst)
  (dotted-list-ref_ lst 1))

;;; Return the third element of a dotted list.
(define (dotted-list-third_ lst)
  (dotted-list-ref_ lst 2))

;;; Return the fourth element of a dotted list.
(define (dotted-list-fourth_ lst)
  (dotted-list-ref_ lst 3))

;;; Return the fifth element of a dotted list.
(define (dotted-list-fifth_ lst)
  (dotted-list-ref_ lst 4))

;;; Return the sixth element of a dotted list.
(define (dotted-list-sixth_ lst)
  (dotted-list-ref_ lst 5))

;;; Return the seventh element of a dotted list.
(define (dotted-list-seventh_ lst)
  (dotted-list-ref_ lst 6))

;;; Return the eighth element of a dotted list.
(define (dotted-list-eighth_ lst)
  (dotted-list-ref_ lst 7))

;;; Return the ninth element of a dotted list.
(define (dotted-list-ninth_ lst)
  (dotted-list-ref_ lst 8))

;;; Return the tenth element of a dotted list.
(define (dotted-list-tenth_ lst)
  (dotted-list-ref_ lst 9))

;;; Return the last element of a dotted list.
(define (dotted-list-last_ lst)
  (define current lst)
  (define result #u)
  (while (and (dotted-list? current)
              (not (null? (dotted-list-cdr current))))
    (set! current (dotted-list-cdr current)))
  (when (dotted-list? current)
    (set! result
          (list-ref current (- (array-length current) 3))))
  result)

;;; Make a dotted list.
(define (make-dotted-list_ car cdr)
  (list-star_ car cdr))

;;; Make a dotted pair.
(define (make-pair_ car cdr)
  `(,car . ,cdr))

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
  (and (dotted-list? x)
       (eq? (dotted-list-tail x) x)))

;;; Convert an array list to a linked list.
(define (list->dotted-list_ x)
  `(,@(array-drop-right x 1) . ,(array-last x)))

;;; Convert a linked list to an array list.
(define (dotted-list->list_ x)
  `(,@(dotted-list-head x) ,(dotted-list-tail x)))

(provide
  (rename-out (append_ append))
  (rename-out (build-list_ build-list))
  (rename-out (butlast_ butlast))
  (rename-out (cdr_ cdr))
  (rename-out (cdr_ tail))
  (rename-out (cdr_ tail_))
  (rename-out (circular-list?_ circular-list?))
  (rename-out (cons_ cons))
  (rename-out (dotted-list?_ dotted-list?))
  (rename-out (drop-right_ drop-right))
  (rename-out (drop_ drop))
  (rename-out (eighth_ eighth))
  (rename-out (fifth_ fifth))
  (rename-out (first_ car))
  (rename-out (first_ car_))
  (rename-out (first_ first))
  (rename-out (first_ head))
  (rename-out (first_ head_))
  (rename-out (flatten_ flatten))
  (rename-out (fourth_ fourth))
  (rename-out (improper-list?_ improper-list?))
  (rename-out (last-cdr_ dotted-list-last-cdr_))
  (rename-out (last-cdr_ last-cdr))
  (rename-out (last-pair_ last-cons_))
  (rename-out (last-pair_ last-pair))
  (rename-out (last_ last))
  (rename-out (length_ length))
  (rename-out (list-star_ list*))
  (rename-out (list?_ list?))
  (rename-out (list?_ proper-list?))
  (rename-out (list_ list))
  (rename-out (make-dotted-list_ make-dotted-list))
  (rename-out (make-list_ make-list))
  (rename-out (make-pair_ make-pair))
  (rename-out (nbutlast_ nbutlast))
  (rename-out (ninth_ ninth))
  (rename-out (nth_ dotted-list-nth))
  (rename-out (nth_ dotted-list-nth_))
  (rename-out (nth_ nth))
  (rename-out (nthcdr_ dotted-list-nthcdr))
  (rename-out (nthcdr_ dotted-list-nthcdr_))
  (rename-out (nthcdr_ nthcdr))
  (rename-out (null?_ null?))
  (rename-out (pair?_ cons?))
  (rename-out (pair?_ cons?_))
  (rename-out (pair?_ pair?))
  (rename-out (pop-left!_ pop!))
  (rename-out (pop-left!_ pop!_))
  (rename-out (pop-left!_ pop-left!))
  (rename-out (pop-right!_ pop-right!))
  (rename-out (proper-list?_ proper-list?))
  (rename-out (push-left!_ push!))
  (rename-out (push-left!_ push-left!))
  (rename-out (push-right!_ append-to-list))
  (rename-out (push-right!_ push-right!))
  (rename-out (rest_ rest))
  (rename-out (reverse_ reverse))
  (rename-out (second_ cadr_))
  (rename-out (second_ second))
  (rename-out (set-car!_ set-car!))
  (rename-out (set-cdr!_ set-cdr!))
  (rename-out (seventh_ seventh))
  (rename-out (sixth_ sixth))
  (rename-out (take_ take))
  (rename-out (tenth_ tenth))
  (rename-out (third_ third))
  append_
  build-list_
  butlast_
  cdr_
  circular-list?_
  cons_
  dotted-improper-list?_
  dotted-list->list_
  dotted-list-eighth_
  dotted-list-fifth_
  dotted-list-first_
  dotted-list-fourth_
  dotted-list-head_
  dotted-list-last_
  dotted-list-length_
  dotted-list-ninth_
  dotted-list-parse_
  dotted-list-ref_
  dotted-list-second_
  dotted-list-set!_
  dotted-list-set_
  dotted-list-seventh_
  dotted-list-sixth_
  dotted-list-tail_
  dotted-list-tenth_
  dotted-list-link_
  dotted-list-third_
  dotted-list?_
  dotted-pair-cdr_
  dotted-pair?_
  dotted-proper-list?_
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
  list->dotted-list_
  list-ref_
  list-set!_
  list-set_
  list-star_
  list-tail_
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
  pair-or-list?_
  pair?_
  pop-left!_
  pop-right!_
  proper-list?_
  push-left!_
  push-right!_
  rest_
  reverse!_
  reverse_
  second_
  set-car!_
  set-cdr!_
  seventh_
  sixth_
  take_
  tenth_
  third_)
