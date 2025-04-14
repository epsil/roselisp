;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Parser
;;;
;;; S-expression parsing.
;;;
;;; ## Description
;;;
;;; A simple S-expression parser that returns an S-expression wrapped
;;; in a rose tree. Metadata that is not part of the S-expression,
;;; such as comments, is stored in the rose tree wrapper instead.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "./constants"
                  quote-sym_
                  quasiquote-sym_
                  unquote-sym_
                  unquote-splicing-sym_))
(require (only-in "./rose"
                  Rose
                  make-list-rose))

;;; Parse a string of Lisp code and return an S-expression.
(define (read input)
  (read-sexp input))

;;; Parse a string of Lisp code and return an S-expression.
(define (read-sexp str)
  (~> (read-rose str)
      (send _ get-value)))

;;; Parse a string of Lisp code and return an S-expression
;;; wrapped in a rose tree.
(define (read-rose str (options (js-obj)))
  ;; Parsing is implemented in two stages: a lexical analysis stage
  ;; (`tokenize`) and a syntax analysis stage (`parse-rose`).
  ;; The lexical analysis stage converts a string to a stream of
  ;; tokens, which is represented as an array of Lisp symbols.
  ;; The syntax analysis stage converts the token stream to a rose
  ;; tree, which contains an S-expression that can be evaluated.
  (~> (tokenize str options)
      (parse-rose _ options)))

;;; Convert a string of Lisp code to an array of tokens.
;;; For example, the string:
;;;
;;;     '((lambda (x) x) "Lisp")'
;;;
;;; Is converted to the array:
;;;
;;;     [s`(`, s`(`, s`lambda`, s`(`, s`x`, s`)`, s`x`, s`)`, 'Lisp', s`)`]
;;;
;;; The output of this function is passed to `parse-rose`.
(define (tokenize str (options (js-obj)))
  (define comments
    (oget options "comments"))
  (when (eq? comments undefined)
    (set! comments #t))
  (define pos 0)
  (define len
    (array-list-length str))
  (define char "")
  (define buffer "")
  (define result '())
  (define state "start")
  (while (not (eq? state "stop"))
    (cond
     ((eq? state "start")
      (set! state
            (if (= len 0)
                "stop"
                "read")))
     ((eq? state "read")
      (cond
       ((>= pos len)
        (set! state "stop"))
       (else
        (set! char (aget str pos))
        (cond
         ((is-whitespace char)
          (set! pos (+ pos 1)))
         ((eq? char "(")
          (push-right! result (new SymbolToken char))
          (set! pos (+ pos 1)))
         ((eq? char ")")
          (push-right! result (new SymbolToken char))
          (set! pos (+ pos 1)))
         ((eq? char "\"")
          (set! state "string")
          (set! pos (+ pos 1)))
         ((is-comment char)
          (set! state "comment"))
         ((eq? char "'")
          (push-right! result (new SymbolToken char))
          (set! pos (+ pos 1)))
         ((eq? char "`")
          (push-right! result (new SymbolToken char))
          (set! pos (+ pos 1)))
         ((eq? char ",")
          (cond
           ((and (< pos len)
                 (eq? (aget str (+ pos 1)) "@"))
            (define next-token
              (aget str (+ pos 1)))
            (push-right! result
                         (new SymbolToken
                              (string-append
                               char next-token)))
            (set! pos (+ pos 2)))
           (else
            (push-right! result (new SymbolToken char))
            (set! pos (+ pos 1)))))
         (else
          (set! state "symbol")
          (set! buffer (string-append buffer char))
          (set! pos (+ pos 1)))))))
     ((eq? state "symbol")
      (set! char (aget str pos))
      (cond
       ((or (>= pos len)
            (regexp-match (regexp "\\s") char)
            (eq? char ")"))
        (define num
          (parse-float buffer))
        (if (not (is-NaN num))
            (push-right! result (new NumberToken num))
            (push-right! result (new SymbolToken buffer)))
        (set! buffer "")
        (set! state "read"))
       (else
        (set! buffer (string-append buffer char))
        (set! pos (+ pos 1)))))
     ((eq? state "string")
      (set! char (aget str pos))
      (cond
       ((>= pos len)
        (push-right! result (new StringToken buffer))
        (set! buffer "")
        (set! state "read"))
       ((eq? char "\\")
        (cond
         ((< pos len)
          (define next-token
            (aget str (+ pos 1)))
          (cond
           ((eq? next-token "n")
            (set! buffer (string-append buffer "\n")))
           ((eq? next-token "t")
            (set! buffer (string-append buffer "\t")))
           ((eq? next-token "r")
            (set! buffer (string-append buffer "\r")))
           (else
            (set! buffer (string-append buffer next-token))))
          (set! pos (+ pos 2)))
         (else
          (set! pos (+ pos 1)))))
       ((eq? char "\"")
        (set! pos (+ pos 1))
        (push-right! result (new StringToken buffer))
        (set! buffer "")
        (set! state "read"))
       (else
        (set! buffer (string-append buffer char))
        (set! pos (+ pos 1)))))
     ((eq? state "comment")
      (set! char (aget str pos))
      (cond
       ((>= pos len)
        (when comments
          (push-right! result
                       (new LeadingCommentToken
                            (remove-indentation buffer))))
        (set! buffer "")
        (set! state "stop"))
       ((eq? char "\n")
        (while (and (eq? char "\n")
                    (< pos len))
          (set! buffer (string-append buffer char))
          (set! pos (+ pos 1))
          (set! char (aget str pos)))
        ;; Skip past indentation on the next line and see if there
        ;; is another leading comment; if so, merge it into this.
        (while (is-indentation char)
          (set! pos (+ pos 1))
          (set! char (aget str pos)))
        (unless (is-comment char)
          ;; Exit `comment` state.
          (when comments
            (push-right! result
                         (new LeadingCommentToken
                              (remove-indentation buffer))))
          (set! buffer "")
          (set! state "read")))
       (else
        (set! buffer (string-append buffer char))
        (set! pos (+ pos 1)))))))
  result)

;;; Take the array of tokens produced by `tokenize` and make a
;;; rose tree that corresponds to the structure of the Lisp code.
;;;
;;; For example,
;;;
;;;     [s`(`, s`(`, s`lambda`, s`(`, s`x`, s`)`, s`x`, s`)`, 'Lisp', s`)`]
;;;
;;; is transformed into a rose tree containing the value:
;;;
;;;     [[s`lambda`, [s`x`], s`x`], 'Lisp']
;;;
;;; Which corresponds to:
;;;
;;;     ((lambda (x) x) "Lisp")
;;;
;;; The output of this function is a S-expression wrapped in a
;;; rose tree.
(define (parse-rose tokens (options (js-obj)))
  ;; In order to implement this function in a non-recursive way, a
  ;; stack is needed to keep track of expressions and their
  ;; subexpressions. Each stack entry is a list
  ;;`(expression value expression-node value-node)`, where
  ;; `expression` is an expression and `value` is the value-part
  ;; of the expression, i.e., the part to insert subexpressions
  ;; into. The other two values are the corresponding rose tree
  ;; nodes. In most cases, the value and the expression are one
  ;; and the same, but for some expressions, like
  ;; `(quote (1 2))`, the value is a subexpression. When `3` is
  ;; added to this expression, the result should be
  ;; `(quote (1 2 3))`, not `(quote (1 2) 3)`.
  (define stack '())
  ;; Comments for the current node.
  (define comments '())
  ;; Pointer to the value on the top of the stack.
  (define parent-exp)
  ;; Pointer to the rose tree node for the top stack value.
  (define parent-exp-node)
  ;; The current expression, i.e., the expression most recently popped
  ;; off the stack. The final value of this variable is the return
  ;; value.
  (define current-exp)
  ;; Rose tree node for `current-exp`.
  (define current-exp-node)
  ;; The value-part of the current expression
  ;; (i.e., the insertion point).
  (define current-val)
  ;; Rose tree node for `current-val`.
  (define current-val-node)
  (for ((i (range 0 (array-list-length tokens))))
    (define token
      (aget tokens i))
    (cond
     ;; Comments.
     ((is-a? token CommentToken)
      (when (is-a? token LeadingCommentToken)
        (push-right! comments token)))
     ;; Symbolic values.
     ((is-a? token SymbolToken)
      (define token-string
        (send token get-value))
      (cond
       ;; Opening parenthesis.
       ((eq? token-string "(")
        (set! current-exp '())
        (set!-values (current-exp-node comments)
                     (attach-comments current-exp
                                      comments
                                      options))
        (push-right! stack
                     (list current-exp
                           current-exp
                           current-exp-node
                           current-exp-node))
        (set! parent-exp current-exp)
        (set! parent-exp-node current-exp-node))
       ;; Closing parenthesis.
       ((eq? token-string ")")
        (set!-values (current-exp
                      current-val
                      current-exp-node
                      current-val-node)
                     (pop-right! stack))
        (when (and (array? current-val)
                   (>= (array-list-length current-val) 3)
                   (eq? (aget current-val
                              (- (array-list-length current-val) 2))
                        (string->symbol ".")))
          (aset! current-val
                 (- (array-list-length current-val) 2)
                 (cons-dot))
          (~> current-val-node
              (send get (- (array-list-length current-val) 2))
              (send set-value (cons-dot))))
        (cond
         ((> (array-list-length stack) 0)
          (define entry
            (array-list-last stack))
          (set! parent-exp (aget entry 1))
          (set! parent-exp-node (aget entry 3)))
         (else
          (set! parent-exp undefined)
          (set! parent-exp-node undefined)))
        (when parent-exp
          (push-right! parent-exp current-exp)
          (send parent-exp-node insert current-exp-node)))
       ;; Quoting.
       ((hash-has-key? operator-symbols token-string)
        (define sym
          (hash-ref operator-symbols token-string))
        (cond
         ((< i (- (array-list-length tokens) 1))
          (let ((next (aget tokens (+ i 1))))
            (set! i (+ i 1))
            (cond
             ((and (is-a? next SymbolToken)
                   (eq? (send next get-value) "("))
              (set! current-exp '())
              (set! current-exp-node (new Rose current-exp))
              (define exp-node
                (make-list-rose
                 (list sym current-exp-node)))
              (define exp
                (send exp-node get-value))
              (set!-values (exp-node comments)
                           (attach-comments exp-node
                                            comments
                                            options))
              (push-right! stack
                           (list exp
                                 current-exp
                                 exp-node
                                 current-exp-node))
              (set! parent-exp current-exp)
              (set! parent-exp-node
                    current-exp-node))
             (else
              (if (is-a? next SymbolToken)
                  (set! next (string->symbol
                              (send next get-value)))
                  (set! next (send next get-value)))
              (set! current-exp-node (make-list-rose (list sym next)))
              (set! current-exp (send current-exp-node get-value))
              (set!-values (current-exp-node comments)
                           (attach-comments current-exp-node
                                            comments
                                            options))
              (when parent-exp
                (push-right! parent-exp current-exp)
                (send parent-exp-node
                      insert current-exp-node))))))
         (else
          (set! current-exp sym)
          (set!-values (current-exp-node comments)
                       (attach-comments current-exp
                                        comments
                                        options))
          (when parent-exp
            (push-right! parent-exp current-exp)
            (send parent-exp-node
                  insert current-exp-node)))))
       ;; Other symbolic values.
       (else
        (set! current-exp
              (string->symbol (send token get-value)))
        (set!-values (current-exp-node comments)
                     (attach-comments current-exp
                                      comments
                                      options))
        (when parent-exp
          (push-right! parent-exp current-exp)
          (send parent-exp-node insert current-exp-node)))))
     ;; Non-symbolic values.
     (else
      (set! current-exp (send token get-value))
      (set!-values (current-exp-node comments)
                   (attach-comments current-exp
                                    comments
                                    options))
      (when parent-exp
        (push-right! parent-exp current-exp)
        (send parent-exp-node insert current-exp-node)))))
  current-exp-node)

;;; Take the array of tokens produced by `tokenize` and make a
;;; nested array that corresponds to the structure of the Lisp code.
;;;
;;; The output of this function is a fully valid S-expression which
;;; can be evaluated in a Lisp environment.
(define (parse-sexp tokens (options (js-obj)))
  (~> (parse-rose tokens options)
      (send get-value)))

;;; Remove indentation from a multi-line string.
(define (remove-indentation str)
  (regexp-replace (regexp "^[^\\S\\r\\n]+$" "gm")
                  str
                  ""))

;;; Whether a character is indentation
;;; (i.e., tabs or spaces, but not newlines).
(define (is-indentation char)
  ;; Newlines are whitespace, but not indentation.
  (regexp-match (regexp "^[^\\S\\r\\n]+$") char))

;;; Whether a character is whitespace
;;; (i.e., tabs, spaces or newlines).
(define (is-whitespace char)
  (regexp-match (regexp "^\\s$") char))

;;; Whether a character is a newline.
;;;
;;; Our concept of newline is that of a
;;; [Unix newline][w:Unix text files]
;;; (i.e., LF "Line Feed", U+000A).
;;;
;;; [w:Unix text files]: https://en.wikipedia.org/wiki/Text_file#Unix_text_files
(define (is-newline char)
  (eq? char "\n"))

;;; Whether a character is a comment character
;;; (i.e., `;`).
(define (is-comment char)
  (eq? char ";"))

;;; Attach comments to a rose tree node, conditional on options.
;;; Returns the resulting node and an empty list of comments.
(define (attach-comments node comments (options (js-obj)))
  (define comments-option
    (oget options "comments"))
  (when (eq? comments-option undefined)
    (set! comments-option #t))
  (define result
    (if (is-a? node Rose)
        node
        (new Rose node)))
  (when (and comments-option
             comments
             (> (array-list-length comments) 0))
    (send result set-property "comments" comments))
  (values result '()))

;;; Whether `comment` is a `;;`-comment (level 2),
;;; a `;;;`-comment (level 3), or some other level.
(define (get-comment-level comment)
  (define str
    (if (string? comment)
        comment
        (get-field value comment)))
  (~> str
      (regexp-match (regexp "^[ ]*;*") _)
      (ann _ Any)
      (first _)
      (string-trim _)
      (string-length _)))

;;; Get the comment level, e.g., 2 for a `;;`-comment,
;;; 3 for a `;;;`-comment, etc.
(define (is-comment-level comment level)
  (= (get-comment-level comment) level))

;;; Map of operator symbols.
;;; Used by `parse-rose`.
(define operator-symbols
  (make-hash
   `(("'" . ,quote-sym_)
     ("`" . ,quasiquote-sym_)
     ("," . ,unquote-sym_)
     (",@" . ,unquote-splicing-sym_))))

;;; Token class.
(define-class Token ()
  ;;; The type of the token.
  (define/public tag)

  ;;; The value of the token.
  (define/public value)

  ;;; Make a token.
  (define/public (constructor (value undefined) (tag "token"))
    (send this set-value value)
    (send this set-tag tag))

  ;;; Get the token tag
  ;;; (i.e., its type).
  (define/public (get-tag)
    (get-field tag this))

  ;;; Get the value of the token.
  (define/public (get-value)
    (get-field value this))

  ;;; Set the token tag
  ;;; (i.e., its type).
  (define/public (set-tag tag)
    (set-field! tag this tag))

  ;;; Set the value of the token.
  (define/public (set-value value)
    (set-field! value this value)))

;;; Token class for representing comments.
(define-class CommentToken (Token)
  (define/public (constructor value (tag "comment"))
    (super value tag)))

;;; Token class for representing leading comments.
(define-class LeadingCommentToken (CommentToken)
  (define/public (constructor value)
    (super value "leading-comment")))

;;; Token class for representing trailing comments.
(define-class TrailingCommentToken (CommentToken)
  (define/public (constructor value)
    (super value "trailing-comment")))

;;; Token class for representing numbers.
(define-class NumberToken (Token)
  (define/public (constructor value)
    (super value "number")))

;;; Token class for representing strings.
(define-class StringToken (Token)
  (define/public (constructor value)
    (super value "string")))

;;; Token class for representing symbols.
(define-class SymbolToken (Token)
  (define/public (constructor value)
    (super value "symbol")))

(provide
  CommentToken
  LeadingCommentToken
  NumberToken
  StringToken
  SymbolToken
  Token
  TrailingCommentToken
  get-comment-level
  is-comment-level
  parse-rose
  parse-sexp
  read
  read-rose
  read-sexp
  tokenize)
