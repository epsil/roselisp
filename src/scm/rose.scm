;; SPDX-License-Identifier: MPL-2.0
;;; # Rose trees
;;;
;;; Rose tree implementation.
;;;
;;; ## Description
;;;
;;; Implements a `Rose` class for representing
;;; [rose trees][w:Rose tree]. A rose tree, in this context, is a data
;;; structure that functions as a container or wrapper around an
;;; S-expression. This is useful for annotating the expression with
;;; additional metadata that is not contained in the expression
;;; itself, such as comments and line numbers.
;;;
;;; Also provided here are various utilities for dealing with
;;; S-expressions wrapped in rose trees.
;;;
;;; ### Nodes
;;;
;;; A rose tree node consists of two parts: a *label* and a *forest*.
;;; The label is a value attached to that particular node, while the
;;; forest is a set of outgoing edges from that node to other nodes.
;;; (The forest is so named because it can be considered a multitude
;;; of trees.)
;;;
;;; In addition to these, this implementation equips every node with a
;;; key--value map of properties. This is useful for annotating the
;;; node with metadata.
;;;
;;; ### Forest
;;;
;;; The forest is implemented in its own class, `Forest`. It contains
;;; a list of pointers to other rose tree nodes. An empty forest means
;;; that the node is a leaf node.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;
;;; [w:Rose tree]: https://en.wikipedia.org/wiki/Rose_tree

(require (only-in "./visitor"
                  visit))

;;; Rose tree node class.
(define-class Rose ()
  ;;; The node's value.
  (define/public value undefined)
  ;;; The node's forest.
  (define/public forest)
  ;;; The node's parent node, if any.
  (define/public parent undefined)
  ;;; The node's properties.
  (define/public properties
    (make-hash))

  ;;; Create a new rose tree node with a given value (optional)
  ;;; and a given forest (optional). If not specified, the
  ;;; value defaults to `undefined` and the forest defaults to
  ;;; an empty forest (i.e., a leaf node).
  (define/public (constructor (value undefined) (forest undefined))
    (~> (or forest (new Forest))
        (send _ set-parent this)
        (set-field! forest this _))
    (unless (eq? value undefined)
      (send this set-value value)))

  ;;; Visitor pattern.
  (define/public (accept visitor (recursive #f))
    (define result this)
    ;; If `recursive` is `#t`, visit sub-nodes first.
    ;; If it is `#f`, the visiting of sub-nodes is
    ;; left to the visitor.
    (when recursive
      (define is-new #f)
      ;; Visit sub-nodes, if any.
      (define result-nodes
        (map (lambda (node)
               (define result-node
                 (send node accept visitor))
               ;; If visiting a sub-node produces a new value,
               ;; set `is-new` to `#t`.
               (unless (or is-new (eq? result-node node))
                 (set! is-new #t))
               result-node)
             (send this get-nodes)))
      ;; If visiting the sub-nodes produces new values,
      ;; then create a new containing node to house them in.
      ;; (Note that the results of visiting sub-nodes may
      ;; be a non-`Rose` value.)
      (when is-new
        (set! result
              (new Rose (send this get-value)))
        (for ((result-node result-nodes))
          (send result insert result-node))))
    ;; Visit the containing node.
    (visit visitor result))

  ;;; Clear a node.
  ;;; Deletes the value, empties the forest,
  ;;; and removes all properties.
  (define/public (clear (clear-value #t) (clear-forest #t) (clear-properties #t))
    (when clear-value
      (send this clear-value))
    (when clear-forest
      (send this clear-forest))
    (when clear-properties
      (send this clear-properties))
    this)

  ;;; Clear the value
  ;;; (i.e., set it to `undefined`).
  ;;;
  ;;; Alias for `remove-value`.
  (define/public (clear-value)
    (send this remove-value))

  ;;; Clear the forest.
  (define/public (clear-forest)
    (~> (send this get-forest)
        (send _ clear))
    this)

  ;;; Clear the properties.
  (define/public (clear-properties)
    (hash-clear! (get-field properties this))
    this)

  ;;; Return the list of nodes obtained by
  ;;; dropping the first `n` nodes from
  ;;; the forest.
  (define/public (drop n)
    (~> (send this get-forest)
        (send _ drop n)))

  ;;; Return the forest obtained by dropping
  ;;; the first `n` nodes from the forest.
  (define/public (drop-forest n)
    (~> (send this get-forest)
        (send _ drop-forest n)))

  ;;; Return the list of nodes obtained by dropping
  ;;; the last `n` nodes from the forest.
  (define/public (drop-right n)
    (~> (send this get-forest)
        (send _ drop-right n)))

  ;;; Return the forest obtained by dropping
  ;;; the last `n` nodes from the forest.
  (define/public (drop-right-forest n)
    (~> (send this get-forest)
        (send _ drop-right-forest n)))

  ;;; Return the first node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (first)
    (~> (send this get-forest)
        (send _ first)))

  ;;; Return the second node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (second)
    (~> (send this get-forest)
        (send _ second)))

  ;;; Return the third node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (third)
    (~> (send this get-forest)
        (send _ third)))

  ;;; Return the fourth node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (fourth)
    (~> (send this get-forest)
        (send _ fourth)))

  ;;; Return the fifth node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (fifth)
    (~> (send this get-forest)
        (send _ fifth)))

  ;;; Return the sixth node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (sixth)
    (~> (send this get-forest)
        (send _ sixth)))

  ;;; Return the seventh node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (seventh)
    (~> (send this get-forest)
        (send _ seventh)))

  ;;; Return the eight node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (eight)
    (~> (send this get-forest)
        (send _ eight)))

  ;;; Return the ninth node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (ninth)
    (~> (send this get-forest)
        (send _ ninth)))

  ;;; Return the tenth node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (tenth)
    (~> (send this get-forest)
        (send _ tenth)))

  ;;; Return the last node in the forest,
  ;;; or `undefined` if there is none.
  (define/public (last)
    (~> (send this get-forest)
        (send _ last)))

  ;;; Alias for `for-each-node`.
  (define/public (for-each f)
    (send this for-each-node f))

  ;;; Call `f` on this node and on
  ;;; each node in the forest.
  (define/public (for-each-node f (recursive #t))
    (f this)
    (~> (send this get-forest)
        (send _ for-each-node f recursive))
    this)

  ;;; Get the `n`-th node in the forest.
  ;;; Counting begins at zero.
  ;;;
  ;;; Alias for `nth`.
  (define/public (get n)
    (send this nth n))

  ;;; Get the numerical index of `node` in the forest.
  ;;; Returns `-1` if not found.
  (define/public (get-index node)
    (~> (send this get-forest)
        (send _ get-index node)))

  ;;; Alias for `get-value`.
  ;;;
  ;;; In the literature, the value of a node
  ;;; is sometimes referred to as its "label".
  (define/public (get-label)
    (send this get-value))

  ;;; Get a list of all the nodes in the forest.
  (define/public (get-nodes)
    (~> (send this get-forest)
        (send _ get-nodes)))

  ;;; Get the forest.
  ;;; Returns an instance of `Forest`.
  (define/public (get-forest)
    (get-field forest this))

  ;;; Get the parent node, or `undefined`
  ;;; if this is a root node.
  (define/public (get-parent)
    (get-field parent this))

  ;;; Get the node property `prop`.
  (define/public (get-property prop)
    (~> (get-field properties this)
        (hash-ref _ prop)))

  ;;; Get the node value.
  (define/public (get-value)
    (get-field value this))

  ;;; Whether there is a node property `prop`.
  (define/public (has-property prop)
    (~> (get-field properties this)
        (hash-has-key? _ prop)))

  ;;; Insert `node` into the forest.
  (define/public (insert node)
    (~> (send this get-forest)
        (send _ insert node))
    this)

  ;;; Get the `n`-th node in the forest.
  ;;; Counting begins at zero.
  (define/public (nth n)
    (~> (send this get-forest)
        (send _ nth n)))

  ;;; Remove the node whose index is `n`.
  (define/public (remove-node n)
    (~> (send this get-forest)
        (send _ remove-node n))
    this)

  ;;; Remove the node property `prop`.
  (define/public (remove-property prop)
    (~> (get-field properties this)
        (hash-remove! _ prop))
    this)

  ;;; Remove the value
  ;;; (i.e., set it to `undefined`).
  (define/public (remove-value)
    (send this set-value undefined))

  ;;; Alias for `set-value`.
  ;;;
  ;;; In the literature, the value of a node
  ;;; is sometimes referred to as its "label".
  (define/public (set-label label)
    (send this set-value label))

  ;;; Set the property `prop` to `value`.
  (define/public (set-property prop value)
    (~> (get-field properties this)
        (hash-set! _ prop value))
    this)

  ;;; Set the parent of this node to `node`.
  (define/public (set-parent node)
    (set-field! parent this node)
    this)

  ;;; Set the value of this node to `value`.
  (define/public (set-value value)
    (set-field! value this value)
    this)

  ;;; Return the number of nodes in the forest.
  (define/public (size)
    (~> (send this get-forest)
        (send _ size)))

  ;;; Get the parent node.
  (define/public (parent-node)
    (get-field parent this))

  ;;; Get the first subnode.
  (define/public (first-child)
    (send this first))

  ;;; Get the last subnode.
  (define/public (last-child)
    (send this last))

  ;;; Get the previous sibling.
  (define/public (previous-sibling)
    ;; TODO
    undefined)

  ;;; Get the next sibling.
  (define/public (next-sibling)
    ;; TODO
    undefined)

  ;;; Get the previous node.
  (define/public (previous-node)
    (or (send this previous-sibling)
        (send this parent-node)))

  ;;; Get the next node.
  (define/public (next-node)
    (or (send this next-sibling)
        (send this first-child))))

;;; Rose tree forest class.
(define-class Forest ()
  ;;; Node list.
  ;;; The order is significant.
  (define/public node-list '())
  ;;; The node this forest belongs to.
  (define/public parent undefined)

  ;;; Create a new forest containing `nodes`.
  (define/public (constructor . nodes)
    (for ((node nodes))
      (send this insert node)))

  ;;; Clear the forest.
  (define/public (clear)
    (set-field! node-list this '())
    this)

  ;;; Return the list of nodes obtained by
  ;;; dropping the first `n` nodes.
  (define/public (drop n)
    (~> (get-field node-list this)
        (drop _ n)))

  ;;; Return the forest obtained by dropping
  ;;; the first `n` nodes.
  (define/public (drop-forest n)
    (~> (send this drop n)
        (apply new Forest _)))

  ;;; Return the list of nodes obtained by dropping
  ;;; the last `n` nodes.
  (define/public (drop-right n)
    (~> (get-field node-list this)
        (drop-right _ n)))

  ;;; Return the forest obtained by dropping
  ;;; the last `n` nodes.
  (define/public (drop-right-forest n)
    (~> (send this drop-right n)
        (apply new Forest _)))

  ;;; Return the first node,
  ;;; or `undefined` if there is none.
  (define/public (first)
    (send this nth 0))

  ;;; Return the second node,
  ;;; or `undefined` if there is none.
  (define/public (second)
    (send this nth 1))

  ;;; Return the third node,
  ;;; or `undefined` if there is none.
  (define/public (third)
    (send this nth 2))

  ;;; Return the fourth node,
  ;;; or `undefined` if there is none.
  (define/public (fourth)
    (send this nth 3))

  ;;; Return the fifth node,
  ;;; or `undefined` if there is none.
  (define/public (fifth)
    (send this nth 4))

  ;;; Return the sixth node,
  ;;; or `undefined` if there is none.
  (define/public (sixth)
    (send this nth 5))

  ;;; Return the seventh node,
  ;;; or `undefined` if there is none.
  (define/public (seventh)
    (send this nth 6))

  ;;; Return the eight node,
  ;;; or `undefined` if there is none.
  (define/public (eight)
    (send this nth 7))

  ;;; Return the ninth node,
  ;;; or `undefined` if there is none.
  (define/public (ninth)
    (send this nth 8))

  ;;; Return the tenth node,
  ;;; or `undefined` if there is none.
  (define/public (tenth)
    (send this nth 9))

  ;;; Return the last node,
  ;;; or `undefined` if there is none.
  (define/public (last)
    (send this
          nth
          (- (send this size) 1)))

  ;;; Alias for `for-each-node`.
  (define/public (for-each f)
    (send this for-each-node f))

  ;;; Call `f` on each node in the forest.
  (define/public (for-each-node f (recursive #t))
    (for ((node (get-field node-list this)))
      (cond
       (recursive
        (send node for-each-node f recursive))
       (else
        (f node))))
    this)

  ;;; Get the `n`-th node.
  ;;; Counting begins at zero.
  ;;;
  ;;; Alias for `nth`.
  (define/public (get n)
    (send this nth n))

  ;;; Get the numerical index of `node`.
  ;;; Returns `-1` if not found.
  (define/public (get-index node)
    (js/find-index (lambda (x)
                     (eq? x node))
                   (send this get-nodes)))

  ;;; Get the list of all the nodes.
  (define/public (get-nodes)
    (get-field node-list this))

  ;;; Get the node this forest belongs to.
  (define/public (get-parent)
    (get-field parent this))

  ;;; Insert `node` into the forest.
  (define/public (insert node)
    (cond
     ((is-a? (send node get-value)
             RoseSplice)
      (for ((sub-node (send node drop 0)))
        (send this insert sub-node)))
     (else
      ;; Update `node-list`.
      (push-right! (get-field node-list this)
                   node)
      (send node
            set-parent
            (send this get-parent))))
    this)

  ;;; Get the `n`-th node.
  ;;; Counting begins at zero.
  (define/public (nth n)
    (~> (get-field node-list this)
        (aget _ n)))

  ;;; Remove the node whose index is `n`.
  (define/public (remove-node n)
    (~> (get-field node-list this)
        (send _ splice n 1))
    this)

  ;;; Set the `parent` field, i.e., the node
  ;;; this forest belongs to.
  (define/public (set-parent parent (update-nodes #t))
    (set-field! parent this parent)
    (when update-nodes
      (for ((node (get-field node-list this)))
        (send node set-parent parent)))
    this)

  ;;; Return the number of nodes in the forest.
  (define/public (size)
    (~> (get-field node-list this)
        (get-field length _))))

;;; Special rose tree value.
;;; Used for inserting directly into the forest.
(define-class RoseSplice ())

;;; Whether something is a rose tree node.
(define (rose? obj)
  (is-a? obj Rose))

;;; Whether something is a rose tree forest.
(define (forest? obj)
  (is-a? obj Forest))

;;; Wrap a list of rose tree-wrapped S-expressions in
;;; a `(begin ...)` form.
;;;
;;; Legacy function, but still used in a few places.
(define (begin-wrap-rose nodes)
  (make-list-rose
   `(begin ,@nodes)))

;;; Wrap a list of rose tree-wrapped S-expressions in
;;; a `(begin ...)` form. Does not wrap singleton lists.
;;;
;;; Legacy function, but still used in a few places.
(define (begin-wrap-rose-smart nodes)
  (cond
   ((not (array? nodes))
    nodes)
   ((and (= (array-list-length nodes) 1)
         (array? (~> (first nodes)
                     (send _ get-value)))
         (> (~> (first nodes)
                (send _ get-value)
                (array-list-length _))
            0)
         (eq? (~> (first nodes)
                  (send _ get-value)
                  (first _))
              'begin))
    (first nodes))
   (else
    (begin-wrap-rose nodes))))

;;; Legacy function, but still used in a few places.
(define (begin-wrap-rose-smart-1 nodes)
  (cond
   ((not (array? nodes))
    nodes)
   ((= (array-list-length nodes) 1)
    (first nodes))
   (else
    (begin-wrap-rose nodes))))

;;; Make a rose tree-wrapped S-expression.
;;;
;;; Converts a list or nested list of rose tree nodes
;;; and other values to a rose tree.
(define (make-rose exp (node undefined))
  (define cache
    (make-rose-map node))
  (define indices
    (make-hash))
  (make-rose-helper exp node cache indices))

;;; Helper function for `make-rose`.
(define (make-rose-helper exp node cache indices)
  (cond
   ((hash-has-key? cache exp)
    (define idx
      (or (hash-ref indices exp) 0))
    (define entry
      (hash-ref cache exp))
    (define val
      (aget entry idx))
    (unless (>= idx (- (array-list-length entry) 1))
      (set! idx (+ idx 1)))
    (hash-set! indices exp idx)
    val)
   ((is-a? exp Rose)
    exp)
   ((array? exp)
    ;; We need to create a new list since
    ;; `exp` may be a list of rose tree nodes
    ;; and S-expressions.
    (define lst '())
    (define result
      (new Rose lst))
    (define is-modified #f)
    (define el-node)
    (for ((el exp))
      (set! el-node
            (make-rose-helper el undefined cache indices))
      (unless (eq? (send el-node get-value) el)
        (set! is-modified #t))
      (push-right! lst
                   (send el-node get-value))
      (send result insert el-node))
    ;; If all of the sub-expressions are unchanged,
    ;; use the original list.
    (unless is-modified
      (send result set-value exp))
    (when (is-a? node Rose)
      (set! result
            (transfer-comments node result)))
    (rose-map-set! cache exp result)
    result)
   (else
    (define result
      (new Rose exp))
    (when (is-a? node Rose)
      (set! result
            (transfer-comments node result)))
    (rose-map-set! cache exp result)
    result)))

;;; Insert an S-expression into a rose tree node.
(define (insert-sexp-into-rose exp node (cache (make-hash)))
  (define cache-map
    (if (is-a? cache Rose)
        (make-simple-rose-map cache)
        cache))
  (send node clear-forest)
  (send node set-value exp)
  (when (array? exp)
    (for ((x exp))
      (cond
       ((hash-has-key? cache-map x)
        (send node
              insert
              (hash-ref cache-map x)))
       (else
        (send node
              insert
              (insert-sexp-into-rose
               x (new Rose) cache-map))))))
  node)

;;; Make a map mapping values to rose tree nodes,
;;; but only one level down.
(define (make-simple-rose-map node)
  (define map
    (make-hash))
  (send node
        for-each-node
        (lambda (x)
          (hash-set! map (send x get-value) x)))
  map)

;;; Make a rose tree map.
;;;
;;; Returns a map mapping a value to a list of rose tree nodes
;;; containing that value.
(define (make-rose-map (node undefined))
  (define map
    (make-hash))
  (when node
    (send node
          for-each-node
          (lambda (x)
            (define exp
              (send x get-value))
            (rose-map-set! map exp x))))
  map)

;;; Set or update an entry in a rose tree map.
(define (rose-map-set! map exp node)
  (if (hash-has-key? map exp)
      (push-right! (hash-ref map exp) node)
      (hash-set! map exp (list node)))
  map)

(define (make-rose-nonrecursive exp)
  (cond
   ((array? exp)
    (make-list-rose exp))
   ((is-a? exp Rose)
    exp)
   (else
    (new Rose exp))))

;;; Make a rose tree-wrapped list expression.
(define (make-list-rose (expressions '()))
  (make-sexp-rose expressions))

;;; Make a rose tree-wrapped S-expression.
(define (make-sexp-rose (exp '()))
  (cond
   ((array? exp)
    (define lst '())
    (define node
      (new Rose lst))
    (for ((x exp))
      (cond
       ((is-a? x Rose)
        (push-right! lst (send x get-value))
        (send node insert x))
       (else
        (push-right! lst x)
        (send node insert (wrap-sexp-in-rose x)))))
    node)
   ((is-a? exp Rose)
    exp)
   (else
    (wrap-sexp-in-rose exp))))

;;; Slice a list wrapped in a rose tree.
;;; Returns a new rose tree node containing
;;; the sliced list.
(define (slice-rose node n)
  (define value
    (send node get-value))
  (cond
   ((array? value)
    (define sliced-node
      (new Rose (drop value n)))
    (for ((x (send node drop n)))
      (send sliced-node insert x))
    sliced-node)
   (else
    node)))

;;; Wrap an S-expression in a rose tree.
(define (wrap-sexp-in-rose exp (cache (make-hash)))
  (cond
   ((hash-has-key? cache exp)
    (hash-ref cache exp))
   (else
    (define node
      (new Rose exp))
    (when (array? exp)
      (for ((x exp))
        (send node
              insert
              (wrap-sexp-in-rose x cache))))
    node)))

;;; Transfer comments from `node1` to `node2`.
;;;
;;; The nodes may be rose tree nodes or ESTree nodes.
(define (transfer-comments from to)
  (define from-comments
    (if (is-a? from Rose)
        (send from get-property "comments")
        (get-field comments from)))
  (when from-comments
    (cond
     ((is-a? to Rose)
      (define to-comments
        (or (send to get-property "comments") '()))
      (define comments
        (append from-comments to-comments))
      (send to set-property "comments" comments))
     (else
      (define to-comments
        (or (get-field comments to) '()))
      (define comments
        (append from-comments to-comments))
      (set-field! comments to comments))))
  to)

(provide
  Forest
  Rose
  RoseSplice
  begin-wrap-rose
  begin-wrap-rose-smart
  begin-wrap-rose-smart-1
  forest?
  insert-sexp-into-rose
  make-list-rose
  make-rose
  make-rose-map
  make-rose-nonrecursive
  make-sexp-rose
  make-simple-rose-map
  rose?
  slice-rose
  transfer-comments
  wrap-sexp-in-rose)
