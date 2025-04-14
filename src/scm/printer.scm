;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # ESTree printer implementation
;;;
;;; Printer code for ESTree trees, rose trees and raw S-expressions.
;;;
;;; ## Description
;;;
;;; TODO: Better description
;;;
;;; This is a partial, incomplete printer for ESTree trees, with
;;; limited support for TSESTree extensions. With further work, it
;;; might be developed into a complete implementation.
;;;
;;; Why not use an external library? For example, [recast][npm:recast]
;;; could be an option as far as basic ESTree trees are concerned. The
;;; challenge is that we also need to emit comments, and for the sake
;;; of TypeScript support, the printer must be able to understand type
;;; annotations in the form of [TSESTree][doc:typescript-estree]
;;; extensions. However, comments and type annotations are both
;;; nonstandard extensions to the [ESTree
;;; specification][github:estree], and are not supported by recast.
;;;
;;; It might be possible to replace the whole lot with
;;; [Prettier][www:prettier], which does support both comments and
;;; types. However, it has proven difficult to [hook
;;; into][doc:prettier:plugins] Prettier's [API][doc:prettier:api] in
;;; the right way.
;;;
;;; Thus we are currently left with the option of writing our own
;;; implementation. It is incomplete, and possibly buggy, but at least
;;; there is a test suite that attempts to salvage the situation
;;; somewhat.
;;;
;;; ## External links
;;;
;;; -   [The ESTree spec][github:estree]
;;; -   [TSESTree details][doc:typescript-estree]
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;
;;; [npm:recast]: https://www.npmjs.com/package/recast
;;; [doc:typescript-estree]: https://typescript-eslint.io/packages/typescript-estree/
;;; [github:estree]: https://github.com/estree/estree
;;; [www:prettier]: https://prettier.io/
;;; [doc:prettier:plugins]: https://prettier.io/docs/en/plugins#printers
;;; [doc:prettier:api]: https://prettier.io/docs/en/api

(require (only-in "./estree"
                  BlockComment
                  CallExpression
                  ClassDeclaration
                  Comment
                  ImportSpecifier
                  LeadingComment
                  Node
                  TSAnyKeyword
                  TSArrayType
                  TSUnionType
                  TrailingComment
                  VariableDeclarator
                  estree?
                  estree-type?
                  estree-type))
(require (only-in "./rose"
                  rose?))
(require (only-in "./visitor"
                  make-visitor
                  visit))

;;; `Doc` type.
;;;
;;; A `Doc` is a string, a `DocCommand`, or a list of `Doc`s.
(define-type Doc
  (U String
     (Listof Doc)
     DocCommand))

;;; `DocCommand` class.
;;;
;;; Used for implementing more complicated constructs,
;;; such as `indent` and `group`.
(define-class DocCommand ()
  (define/public type)
  (define/public args)

  (define/public (constructor type . args)
    (set-field! type this type)
    (set-field! args this args)))

;;; Empty string.
(define empty "")

;;; Space.
(define space " ")

;;; Newline.
(define line "\n")

;;; `Doc` command `literalline`.
(define literalline
  (new DocCommand "literalline"))

;;; `Doc` command `align`.
(define (align offset doc (options (js-obj)))
  (new DocCommand "align" offset doc options))

;;; `Doc` command `indent`.
(define (indent doc (options (js-obj)))
  (new DocCommand "indent" doc options))

;;; `Doc` command `noindent`.
(define (noindent doc (options (js-obj)))
  (new DocCommand "noindent" doc options))

;;; `Doc` command `join`.
;;;
;;; Join a list of documents with a separator.
(define (join sep docs)
  (define result '())
  (for ((i (range 0 (array-list-length docs))))
    (unless (= i 0)
      (push-right! result sep))
    (push-right! result (aget docs i)))
  result)

;;; `Doc` command `group`.
;;;
;;; Makes a document group.
(define (group doc (options (js-obj)))
  (new DocCommand "group" doc options))

;;; Get the type of a `Doc` object.
(define (doc-type doc)
  (cond
   ((string? doc)
    "string")
   ((array? doc)
    "array")
   ((is-a? doc DocCommand)
    (get-field type doc))
   (else
    "undefined")))

;;; Unwrap a `Doc` command.
(define (doc-value doc)
  (cond
   ((is-a? doc DocCommand)
    (first (get-field args doc)))
   (else
    doc)))

;;; Unwrap a `Doc` command and print it to a string.
(define (doc-value-string doc)
  (print-doc (doc-value doc)))

;;; Whether a `Doc` object should break across multiple lines.
(define (doc-should-break? doc)
  (cond
   ((is-a? doc DocCommand)
    (oget (array-list-last (get-field args doc)) "should-break"))
   (else
    #f)))

;;; Whether a `Doc` object contains any comments.
(define (doc-has-comments? doc)
  (cond
   ((is-a? doc DocCommand)
    (oget (array-list-last (get-field args doc)) "has-comments"))
   (else
    #f)))

;;; Wrap a `Doc` object in a pair of parentheses.
(define (doc-wrap doc (options (js-obj)) (settings (js-obj)))
  (define open
    (or (oget settings "open")
        "("))
  (define close
    (or (oget settings "close")
        ")"))
  (define offset
    (string-length open))
  (cond
   ((or (oget options "has-comments")
        (doc-has-comments? doc))
    (print-doc
     (list
      open
      line
      (align offset doc)
      line
      close)
     options))
   (else
    (print-doc
     (list open doc close)))))

;;; Print comments of an ESTree node and attach them
;;; to a `Doc` object.
(define (attach-comments result node (options (js-obj)))
  (define comments-option
    (oget options "comments"))
  (define comments
    (get-field comments node))
  (define code
    (doc-value-string result))
  (define leading-comments "")
  (define trailing-comments "")
  (when (or (eq? comments-option #f)
            (not comments)
            (= (array-list-length comments) 0))
    (return result))
  (for ((i (range 0 (length comments))))
    (define comment
      (aget comments i))
    (cond
     ((is-a? comment BlockComment)
      (define block-comment
        (make-block-comment
         (get-field original-text comment)))
      (when (and (= i (- (array-list-length comments) 1))
                 (eq? code ""))
        (set! block-comment
              (regexp-replace (regexp "\\n*$")
                              block-comment
                              "")))
      (set! leading-comments
            (string-append
             leading-comments
             block-comment
             (if (or (eq? code "")
                     (regexp-match (regexp "\\n*$")
                                   block-comment))
                 empty
                 line))))
     ((is-a? comment LeadingComment)
      (define leading-comment
        (make-line-comment
         (get-field original-text comment)))
      (when (and (= i (- (array-list-length comments) 1))
                 (eq? code ""))
        (set! leading-comment
              (regexp-replace (regexp "\\n$")
                              leading-comment
                              "")))
      (set! leading-comments
            (string-append
             leading-comments
             leading-comment
             (if (or (eq? code "")
                     (regexp-match (regexp "\\n$")
                                   leading-comment))
                 empty
                 line))))
     ((is-a? comment TrailingComment)
      (define trailing-comment
        (make-line-comment
         (get-field original-text comment)))
      (set! trailing-comments
            (string-append
             trailing-comments
             space
             trailing-comment)))))
  (group
   (list leading-comments
         code
         trailing-comments)
   (js-obj "should-break" #t
           "has-comments" #t)))

;;; Make a line comment.
(define (make-line-comment text)
  (define-values (_ content trailing-newlines)
    (regexp-match (regexp "^([\\s\\S]*?)([\\n]*)$")
                  text))
  (string-append
   (~> content
       (string-split "\n")
       (map (lambda (x)
              (regexp-replace (regexp "^")
                              x
                              (if (eq? x "")
                                  "//"
                                  "// ")))
            _)
       (string-join "\n"))
   trailing-newlines))

;;; Make a block comment.
(define (make-block-comment text)
  (define-values (_ content trailing-newlines)
    (regexp-match (regexp "^([\\s\\S]*?)([\\n]*)$")
                  text))
  (string-append
   "/**"
   line
   (~> content
       (string-split "\n")
       (map (lambda (x)
              (regexp-replace (regexp "^")
                              x
                              (if (eq? x "")
                                  " *"
                                  " * ")))
            _)
       (string-join "\n"))
   line
   " */"
   trailing-newlines))

;;; Whether an expression is "simple", i.e., does not
;;; need to be wrapped in parentheses when printed.
(define (estree-simple? exp)
  (memq? (estree-type exp)
         '("Literal"
           "Identifier"
           "ThisExpression"
           "CallExpression"
           "NewExpression"
           "UnaryExpression"
           "ArrayExpression"
           "ObjectExpression"
           "MemberExpression")))

;;; Whether an expression is "complex", i.e., needs
;;; to be wrapped in parentheses when printed.
(define (estree-complex? exp)
  (memq? (estree-type exp)
         '("FunctionExpression"
           "ArrowFunctionExpression"
           "FunctionDeclaration"
           "TSAsExpression")))

;;; Whether an expression is a string literal.
(define (estree-string-literal? exp)
  (and (estree-type? exp "Literal")
       (string? (get-field value exp))))

;;; Whether an ESTree node has any comments.
(define (estree-has-comments? node)
  (> (array-list-length (get-field comments node)) 0))

;;; Whether an ESTree node has any block comments.
(define (estree-has-block-comment? node)
  (findf (lambda (comment)
           (is-a? comment BlockComment))
         (get-field comments node)))

;;; Whether an ESTree node has any leading comments.
(define (estree-has-leading-comment? node)
  (findf (lambda (comment)
           (is-a? comment LeadingComment))
         (get-field comments node)))

;;; Whether an ESTree node has any trailing comments.
(define (estree-has-trailing-comment? node)
  (findf (lambda (comment)
           (is-a? comment TrailingComment))
         (get-field comments node)))

;;; Print an ESTree node or an S-expression.
(define (print obj (options (js-obj)))
  (cond
   ((estree? obj)
    (print-estree obj options))
   (else
    (print-sexp obj options))))

;;; Print an ESTree node.
(define (print-estree node (options (js-obj)))
  (print-to-string node options))

;;; Print a rose tree.
(define (print-rose node (options (js-obj)))
  (print-sexp (send node get-value) options))

;;; Print an S-expression.
(define (print-sexp exp (options (js-obj)))
  (write-to-string exp options))

;;; Print an S-expression as an expression
;;; that can be evaluated.
(define (print-sexp-as-expression exp (options (js-obj)))
  (print-sexp exp
              (js-obj-append
               options
               (js-obj "quoteToplevel" #t))))

;;; Print an S-expression to a string.
(define (write-to-string obj (options (js-obj)))
  (define result
    (write-to-doc obj options))
  (unless (oget options "doc")
    (set! result (print-doc result options)))
  result)

;;; Print an S-expression to a `Doc` object.
(define (write-to-doc obj (options (js-obj)))
  (define doc-option
    (oget options "doc"))
  (define pretty-option
    (oget options "pretty"))
  (define quote-toplevel-option
    (oget options "quoteToplevel"))
  (define visitor
    (make-visitor
     `(
       ;; Rose tree.
       (,rose?
        ,(lambda (obj)
           (write-to-doc (send obj get-value) options)))
       ;; Symbol.
       (,symbol?
        ,(lambda (obj)
           (list
            (if quote-toplevel-option
                "'"
                empty)
            (if (cons-dot? obj)
                "."
                (symbol->string obj)))))
       ;; Boolean.
       (,boolean?
        ,(lambda (obj)
           (if obj
               "#t"
               "#f")))
       ;; String.
       (,string?
        ,(lambda (obj)
           (list
            "\""
            (~> obj
                (regexp-replace
                 (regexp "\\\\" "g") _ "\\\\")
                (regexp-replace
                 (regexp "\"" "g") _ "\\\"")
                (string-split line)
                (join literalline _))
            "\"")))
       ;; Function.
       (,procedure?
        ,(lambda (obj)
           "#<procedure>"))
       ;; List or cons cell.
       (,array?
        ,(lambda (obj)
           (define op
             (first obj))
           (define spec
             (and pretty-option
                  (send pretty-print-map get op)))
           (define result
             (cond
              ((procedure? spec)
               (spec obj options))
              ((number? spec)
               (pretty-print-with-offset spec obj options))
              (else
               (pretty-print-form obj options))))
           (when quote-toplevel-option
             (set! result (list "'" result)))
           result))
       ;; Other values.
       (,(const #t)
        ,(lambda (obj)
           (string-append obj ""))))))
  (define result
    (visit visitor obj))
  result)

;;; Pretty-print a list expression.
;;;
;;; Helper function for `write-to-doc`.
(define (pretty-print-form form options)
  (list
   "("
   (join " "
         (map (lambda (x)
                (write-to-doc
                 x
                 (js-obj-append
                  options
                  (js-obj "quoteToplevel"
                          #f))))
              form))
   ")"))

;;; Pretty-print a list expression with an indentation offset.
;;;
;;; Helper function for `write-to-doc`.
(define (pretty-print-with-offset offset form options)
  (define pretty-option
    (oget options "pretty"))
  (unless pretty-option
    (return (write-to-string form options)))
  (unless (array? form)
    (return (write-to-string form options)))
  (define op
    (first form))
  (define elements
    (map (lambda (x)
           (write-to-doc
            x
            (js-obj-append
             options
             (js-obj "quoteToplevel"
                     #f))))
         form))
  (define elements1
    (take elements (+ offset 1)))
  (define elements2
    (drop elements (+ offset 1)))
  (define result
    (list
     (join space elements1)
     (if (> (array-list-length elements2) 0)
         (list
          line
          (indent
           (join line elements2)))
         empty)))
  (set! result (list "(" result ")"))
  result)

;;; Pretty-print a `cond` expression.
(define (pretty-print-cond form options)
  (list
   "("
   (write-to-doc (first form) options)
   line
   (align
    1
    (join
     line
     (map (lambda (x)
            (list
             "("
             (write-to-doc (first x) options)
             line
             (align
              1
              (join
               line
               (map (lambda (x1)
                      (write-to-doc
                       x1
                       (js-obj-append
                        options
                        (js-obj "quoteToplevel"
                                #f))))
                    (rest x))))
             ")"))
          (rest form))))
   ")"))

;;; Pretty-print an `if` expression.
(define (pretty-print-if form options)
  (list
   "("
   (join " "
         (map (lambda (x)
                (write-to-doc x options))
              (take form 2)))
   line
   (align 4
          (join
           line
           (map (lambda (x)
                  (write-to-doc x options))
                (drop form 2))))
   ")"))

;;; Pretty-print a `module` expression.
(define (pretty-print-module form options)
  (define no-module-form-option
    (oget options "noModuleForm"))
  (cond
   (no-module-form-option
    (join
     (list line line)
     (map (lambda (x)
            (write-to-doc x options))
          (drop form 3))))
   (else
    (list
     "("
     (join " "
           (map (lambda (x)
                  (write-to-doc x options))
                (take form 3)))
     line
     (indent
      (join
       (list line line)
       (map (lambda (x)
              (write-to-doc x options))
            (drop form 3))))
     ")"))))

;;; Map of pretty printing specifications.
;;;
;;; Somewhat similar to [`(declare (indent indent-spec))`
;;; in GNU Emacs][emacs:declare].
;;;
;;; [emacs:declare]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Indenting-Macros.html
(define pretty-print-map
  (make-hash
   `((as~> . 2)
     (begin . 0)
     (catch . 2)
     (class . 1)
     (cond . ,pretty-print-cond)
     (define . 1)
     (define/public . 1)
     (do . 1)
     (finally . 0)
     (fn . 1)
     (for . 1)
     (if . ,pretty-print-if)
     (js/arrow . 1)
     (js/function . 1)
     (js/while . 1)
     (lambda . 1)
     (let*-values . 1)
     (let-values . 1)
     (module . ,pretty-print-module)
     (provide . 0)
     (try . 0)
     (unless . 1)
     (when . 1)
     (while . 1))))

;;; Print a `Doc` object to a string.
(define (print-doc doc (options (js-obj)))
  (~> doc
      (print-doc-to-doc-list options)
      (print-doc-list-to-string options)))

;;; Print a `Doc` object to a `Doc` list.
(define (print-doc-to-doc-list doc (options (js-obj)))
  (define dtype
    (doc-type doc))
  (cond
   ((eq? dtype "string")
    (join line (string-split doc line)))
   ((eq? dtype "array")
    (define result '())
    (for ((x doc))
      (define x-result
        (print-doc-to-doc-list x options))
      (if (array? x-result)
          (set! result (append result x-result))
          (push-right! result x-result)))
    result)
   ((eq? dtype "align")
    (define args
      (get-field args doc))
    (define offset
      (first args))
    (define contents
      (second args))
    (define contents-printed
      (filter (lambda (x)
                (not (eq? x empty)))
              (print-doc-to-doc-list
               contents options)))
    (define result '())
    (define indentation
      (string-repeat " " offset))
    (when (> (array-list-length contents-printed) 0)
      (push-right! result indentation))
    (for ((i (range 0 (array-list-length contents-printed))))
      (define current
        (aget contents-printed i))
      (define next
        (if (< i (- (array-list-length contents-printed) 1))
            (aget contents-printed (+ i 1))
            empty))
      (push-right! result current)
      (when (and (eq? current line)
                 (not (eq? next line))
                 (not (eq? next empty)))
        (push-right! result indentation)))
    result)
   ((eq? dtype "indent")
    (define args (get-field args doc))
    (define contents (first args))
    (define offset
      (or (oget options "indent") 2))
    (print-doc-to-doc-list
     (new DocCommand "align" offset contents options)
     options))
   ((eq? dtype "group")
    (define args (get-field args doc))
    (define contents (first args))
    (define contents-printed
      (print-doc-to-doc-list contents options))
    contents-printed)
   (else
    (list doc))))

;;; Print a `Doc` list to a string.
(define (print-doc-list-to-string doc (options (js-obj)))
  (define dtype
    (doc-type doc))
  (cond
   ((eq? dtype "string")
    doc)
   ((eq? dtype "array")
    (string-join
     (map (lambda (x)
            (print-doc-list-to-string x options))
          doc)
     empty))
   ((eq? dtype "literalline")
    line)
   (else
    empty)))

;;; Print an ESTree node to a string.
(define (print-to-string node (options (js-obj)))
  (print-doc (print-node node options) options))

;;; Print an ESTree node to a `Doc` object.
(define (print-node node (options (js-obj)))
  (visit print-visitor node options))

;;; Visitor function for printing ESTree nodes.
(define (print-visitor node options)
  (define type
    (estree-type node))
  (define comments
    (oget options "comments"))
  (define printer
    (or (hash-ref printer-map type)
        default-printer))
  (define result
    (printer node options))
  (when comments
    (set! result
          (attach-comments result node options)))
  result)

;;; Print an `ExpressionStatement` ESTree node to a `Doc` object.
(define (print-expression-statement node (options (js-obj)))
  (list
   (print-node (get-field expression node) options)
   ";"))

;;; Print a `ReturnStatement` ESTree node to a `Doc` object.
(define (print-return-statement node (options (js-obj)))
  (define argument
    (get-field argument node))
  (cond
   (argument
    (define argument-printed
      (print-node argument options))
    (when (doc-should-break? argument-printed)
      (set! argument-printed
            (list
             "("
             line
             (indent argument-printed)
             line
             ")")
            options))
    (list
     "return"
     space
     argument-printed
     ";"))
   (else
    (list "return" ";"))))

;;; Print a `YieldExpression` ESTree node to a `Doc` object.
(define (print-yield-expression node (options (js-obj)))
  (list
   "yield"
   (if (get-field argument node)
       (list
        space
        (print-node (get-field argument node) options))
       empty)))

;;; Print a `ThrowStatement` ESTree node to a `Doc` object.
(define (print-throw-statement node (options (js-obj)))
  (list
   "throw"
   space
   (print-node (get-field argument node) options)
   ";"))

;;; Print an `AwaitExpression` ESTree node to a `Doc` object.
(define (print-await-expression node (options (js-obj)))
  (list
   "await"
   space
   (print-node (get-field argument node) options)))

;;; Print a `BreakStatement` ESTree node to a `Doc` object.
(define (print-break-statement node (options (js-obj)))
  (list
   "break"
   (if (get-field label node)
       (list
        space
        (print-node (get-field label node) options))
       empty)
   ";"))

;;; Print a `ContinueStatement` ESTree node to a `Doc` object.
(define (print-continue-statement node (options (js-obj)))
  (list
   "continue"
   (if (get-field label node)
       (list
        space
        (print-node (get-field label node) options))
       empty)
   ";"))

;;; Print a `ThisExpression` ESTree node to a `Doc` object.
(define (print-this-expression node (options (js-obj)))
  "this")

;;; Print an `Identifier` ESTree node to a `Doc` object.
(define (print-identifier node (options (js-obj)))
  (define language
    (oget options "language"))
  (define no-implicit-any
    (oget options "noImplicitAny"))
  (define type_
    (get-field typeAnnotation node))
  (when (and no-implicit-any (not type_))
    (set! type_ (new TSAnyKeyword)))
  (list
   (get-field name node)
   (if (and (eq? language "TypeScript")
            type_)
       (list
        (if (get-field optional node) "?:" ":")
        space
        (print-node type_ options))
       empty)))

;;; Print a `Literal` ESTree node to a `Doc` object.
(define (print-literal node (options (js-obj)))
  (define value
    (get-field value node))
  (cond
   ((string? value)
    (print-string-literal node options))
   ((eq? value #t)
    "true")
   ((eq? value #f)
    "false")
   ((eq? value js/null)
    "null")
   ((eq? value undefined)
    "undefined")
   (else
    (string-append value ""))))

;;; Print a string `Literal` ESTree node to a `Doc` object.
;;;
;;; Helper function for `print-literal`.
(define (print-string-literal node (options (js-obj)))
  (define str
    (~> node
        (get-field value _)
        (regexp-replace (regexp "\\\\" "g") _ "\\\\")
        (regexp-replace (regexp "'" "g") _ "\\'")
        (regexp-replace (regexp "\\n" "g") _ "\\n")))
  (list "'" str "'"))

;;; Print a template string.
;;;
;;; Helper function for `print-template-element` and
;;; `print-template-literal`.
(define (print-template-string str)
  (list
   "`"
   (~> str
       (string-split line)
       (join literalline _))
   "`"))

;;; Print a `TemplateElement` ESTree node to a `Doc` object.
(define (print-template-element node (options (js-obj)))
  (define str
    (get-field raw (get-field value node)))
  (print-template-string str))

;;; Print a `TemplateLiteral` ESTree node to a `Doc` object.
(define (print-template-literal node (options (js-obj)))
  (define str
    (get-field raw
               (get-field value
                          (first
                           (get-field quasis node)))))
  (print-template-string str))

;;; Print a `TaggedTemplateExpression` ESTree node to a `Doc` object.
(define (print-tagged-template-expression node (options (js-obj)))
  (define tag
    (get-field tag node))
  (define tag-printed
    (print-node tag options))
  (define quasi
    (get-field quasi node))
  (define quasi-printed
    (print-node quasi options))
  (list tag-printed quasi-printed))

;;; Print a `UnaryExpression` ESTree node to a `Doc` object.
(define (print-unary-expression node (options (js-obj)))
  (define prefix
    (get-field prefix node))
  (define operator
    (get-field operator node))
  (define operator-printed operator)
  (define argument
    (get-field argument node))
  (define argument-printed
    (print-node argument options))
  (unless (estree-simple? argument)
    (set! argument-printed
          (doc-wrap argument-printed options)))
  (cond
   (prefix
    (list
     operator-printed
     (if (or (eq? operator-printed "delete")
             (eq? operator-printed "typeof"))
         space
         empty)
     argument-printed))
   (else
    (list
     argument-printed
     operator-printed))))

;;; Print a `BinaryExpression` ESTree node to a `Doc` object.
(define (print-binary-expression node (options (js-obj)))
  (define type_
    (estree-type node))
  (define operator
    (get-field operator node))
  (define operator-printed
    operator)
  (define left
    (get-field left node))
  (define left-printed
    (print-node
     left options))
  (define left-printed-str
    (doc-value-string left-printed))
  (define right
    (get-field right node))
  (define right-printed
    (print-node
     right options))
  (define right-printed-str
    (doc-value-string right-printed))
  (define should-break
    (or (doc-should-break? left-printed)
        (doc-should-break? right-printed)))
  (define is-multiline-string-literal
    (and (estree-string-literal? left)
         (regexp-match "\\n$" (get-field value left))))
  (define is-multiline-binary-expression
    (and (not is-multiline-string-literal)
         (estree-type? left "BinaryExpression")
         (estree-string-literal? (get-field right left))
         (regexp-match (regexp "\\n$")
                       (~> left
                           (get-field right _)
                           (get-field value _)))))
  (define is-multiline-string
    (or is-multiline-string-literal
        is-multiline-binary-expression))
  (define result)
  (unless (or (estree-simple? left)
              (and (estree-type? left type_)
                   (eq? (get-field operator left) operator)))
    (set! left-printed-str
          (doc-wrap left-printed-str
                    (js-obj-append
                     options
                     (js-obj "has-comments"
                             (doc-has-comments?
                              left-printed))))))
  (unless (or (estree-simple? right)
              (and (estree-type? right type_)
                   (eq? (get-field operator right) operator)
                   (memq? operator '("+" "*" "&&" "||"))))
    (set! right-printed-str
          (doc-wrap right-printed-str
                    (js-obj-append
                     options
                     (js-obj "has-comments"
                             (doc-has-comments?
                              right-printed))))))
  (cond
   (should-break
    (set! result (list
                  "("
                  line
                  (align 1 left-printed-str)
                  (if (estree-has-trailing-comment? left)
                      (list
                       line
                       (align 1 operator))
                      (list space operator))
                  line
                  (align 1 right-printed-str)
                  line
                  ")")))
   (is-multiline-string
    (set! result (list
                  left-printed-str
                  space
                  operator
                  line
                  (indent right-printed-str))))
   (else
    (set! result (list
                  left-printed-str
                  space
                  operator
                  space
                  right-printed-str))))
  (group result
         (js-obj "should-break" should-break)))

;;; Print a `LogicalExpression` ESTree node to a `Doc` object.
(define (print-logical-expression node (options (js-obj)))
  (print-binary-expression node options))

;;; Print an `AssignmentExpression` ESTree node to a `Doc` object.
(define (print-assignment-expression node (options (js-obj)))
  ;; TODO: Break up statement if one of the sides have comments.
  (define language
    (oget options "language"))
  (define operator
    (get-field operator node))
  (define operator-printed operator)
  (define left
    (get-field left node))
  (define left-printed
    (print-node left
                options))
  (define right
    (get-field right node))
  (define right-printed
    (print-node right
                (js-obj-append
                 options
                 (js-obj "noImplicitAny" #f))))
  (define result
    (list
     left-printed
     space
     operator
     (if (doc-has-comments? right-printed)
         (list line
               (indent right-printed))
         (list space
               right-printed))))
  (when (estree-type? left "ObjectPattern")
    (set! result
          (doc-wrap result options)))
  result)

;;; Print an `AssignmentPattern` ESTree node to a `Doc` object.
(define (print-assignment-pattern node (options (js-obj)))
  (print-node
   (new VariableDeclarator
        (get-field left node)
        (get-field right node))
   options))

;;; Print a `CallExpression` ESTree node to a `Doc` object.
(define (print-call-expression node (options (js-obj)))
  (define callee
    (get-field callee node))
  (define callee-type
    (estree-type callee))
  (define callee-printed
    (print-node callee options))
  (define args
    (get-field arguments node))
  (define args-printed
    (map (lambda (x)
           (print-node x options))
         args))
  (define optional
    (get-field optional node))
  (when (estree-complex? callee)
    (set! callee-printed
          (doc-wrap callee-printed options)))
  (list
   callee-printed
   (if optional
       "?."
       empty)
   "("
   (join (list "," space) args-printed)
   ")"))

;;; Print a `SequenceExpression` ESTree node to a `Doc` object.
(define (print-sequence-expression node (options (js-obj)))
  (define expressions
    (get-field expressions node))
  (define expressions-printed
    (map (lambda (x)
           (print-node x options))
         expressions))
  (define result)
  (set! result
        (join (list "," space)
              expressions-printed))
  (when (> (array-list-length expressions) 1)
    (set! result
          (doc-wrap result options)))
  result)

;;; Print a `BlockStatement` ESTree node to a `Doc` object.
(define (print-block-statement node (options (js-obj)))
  (define body
    (get-field body node))
  (define body-modified
    (begin
      (when (and (get-field comments node)
                 (> (array-list-length body) 0))
        (set-field! comments
                    (first body)
                    (append (get-field comments node)
                            (or (get-field comments (first body))
                                '())))
        (set! (get-field comments node) '()))
      (get-field body node)))
  (define body-indented
    (indent
     (~> body-modified
         (map (lambda (x)
                (print-node x options))
              _)
         (join line _))))
  (define body-printed
    (print-doc body-indented))
  (list
   "{"
   line
   body-indented
   (if (eq? body-printed "")
       empty
       line)
   "}"))

;;; Print a `MemberExpression` ESTree node to a `Doc` object.
(define (print-member-expression node (options (js-obj)))
  (define language
    (oget options "language"))
  (define object
    (get-field object node))
  (define object-type
    (estree-type object))
  (define object-printed
    (print-node object options))
  (define property
    (get-field property node))
  (define property-printed
    (print-node property options))
  (define computed
    (get-field computed node))
  (define optional
    (get-field optional node))
  (when (or (not (estree-simple? object))
            (eq? object-type "ObjectExpression"))
    ;; If the object expression is complicated, wrap it in
    ;; parentheses.
    (set! object-printed
          (doc-wrap object-printed options)))
  (cond
   (computed
    ;; Kludge: prevent errors with expressions like
    ;; `x[y]`, where `y` is `any`-typed.
    ;; TODO: Move this code into the compiler.
    (when (and (eq? language "TypeScript")
               (not (memq? (estree-type property)
                           '("Literal"
                             "UnaryExpression"
                             "BinaryExpression"))))
      (set! object-printed
            (doc-wrap
             (list object-printed
                   " as any")
             options)))
    (list
     object-printed
     "["
     property-printed
     "]"))
   (else
    (list
     object-printed
     (if optional
         "?."
         ".")
     property-printed))))

;;; Print an `UpdateExpression` ESTree node to a `Doc` object.
(define (print-update-expression node (options (js-obj)))
  (print-unary-expression node options))

;;; Print a `SpreadElement` ESTree node to a `Doc` object.
(define (print-spread-element node (options (js-obj)))
  (define language
    (oget options "language"))
  (define no-implicit-any
    (oget options "noImplicitAny"))
  (define argument
    (get-field argument node))
  (define argument-printed
    (print-node argument
                (js-obj-append
                 options
                 (js-obj "noImplicitAny" #f))))
  (define type_
    (get-field typeAnnotation node))
  (when (and no-implicit-any (not type_))
    (set! type_ (new TSArrayType (new TSAnyKeyword))))
  (unless (estree-simple? argument)
    (set! argument-printed
          (doc-wrap argument-printed options)))
  (list
   "..."
   argument-printed
   (if (and (eq? language "TypeScript")
            type_)
       (list
        ":"
        space
        (print-node type_ options))
       empty)))

;;; Print a `RestElement` ESTree node to a `Doc` object.
(define (print-rest-element node (options (js-obj)))
  (print-spread-element node options))

;;; Print a function declaration or function expression to a
;;; `Doc` object. Also handles arrow functions.
(define (print-function node (options (js-obj)) (settings (js-obj)))
  (define language
    (oget options "language"))
  (define arrow
    (oget settings "arrow"))
  (define async_
    (get-field async node))
  (define return-type-setting
    (oget settings "returnType"))
  (define return-type
    (if (string? return-type-setting)
        return-type-setting
        (get-field returnType node)))
  (define return-type-printed
    (cond
     ((string? return-type)
      return-type)
     (return-type
      (print-ts-type return-type options))
     (async_
      "Promise<any>")
     (else
      "any")))
  (list
   (if async_
       (list "async" space)
       empty)
   (if arrow
       empty
       (list "function" space))
   (if (get-field id node)
       (print-node (get-field id node)
                   options)
       empty)
   "("
   (~> (get-field params node)
       (map (lambda (x)
              (print-node x
                          (js-obj-append
                           options
                           (js-obj "noImplicitAny" #t))))
            _)
       (join (list "," space) _))
   ")"
   (if (and (eq? language "TypeScript")
            (not (eq? return-type-printed "")))
       (list ":" space return-type-printed)
       empty)
   (if arrow
       (list space "=>" space)
       space)
   (print-node (get-field body node)
               options)))

;;; Print a `FunctionDeclaration` ESTree node to a `Doc` object.
(define (print-function-declaration node (options (js-obj)))
  (print-function node options))

;;; Print a `FunctionExpression` ESTree node to a `Doc` object.
(define (print-function-expression node (options (js-obj)))
  (print-function node options))

;;; Print a `ArrowFunctionExpression` ESTree node to a `Doc` object.
(define (print-arrow-function-expression node (options (js-obj)))
  (print-function node options (js-obj "arrow" #t)))

;;; Print a `VariableDeclaration` ESTree node to a `Doc` object.
(define (print-variable-declaration node (options (js-obj)))
  (list
   (get-field kind node)
   space
   (~> (get-field declarations node)
       (map (lambda (x)
              (print-node x options))
            _)
       (join (list "," space) _))
   ";"))

;;; Print a `VariableDeclarator` ESTree node to a `Doc` object.
(define (print-variable-declarator node (options (js-obj)))
  (define language
    (oget options "language"))
  (define id
    (get-field id node))
  (define id-printed
    (print-node id
                (js-obj-append
                 options
                 (js-obj "noImplicitAny" #t))))
  (cond
   ((get-field init node)
    (define init
      (get-field init node))
    (define init-printed
      (print-node init
                  (js-obj-append
                   options
                   (js-obj "noImplicitAny" #f))))
    (list
     id-printed
     space
     "="
     (if (doc-has-comments? init-printed)
         (list line
               (indent init-printed))
         (list space
               init-printed))))
   (else
    id-printed)))

;;; Print an `IfStatement` ESTree node to a `Doc` object.
(define (print-if-statement node (options (js-obj)))
  (define test
    (get-field test node))
  (define test-printed
    (print-node
     test options))
  (define test-printed-str
    (if (estree-type? test "AssignmentExpression")
        (doc-wrap (doc-value-string test-printed)
                  options)
        (doc-value-string test-printed)))
  (define consequent
    (get-field consequent node))
  (define consequent-printed
    (print-node
     consequent options))
  (define alternate
    (get-field alternate node))
  (define result
    (string-append
     "if ("
     test-printed-str
     ")"
     (if (doc-should-break? consequent-printed)
         line
         space)
     (doc-value-string consequent-printed)))
  (when alternate
    (define alternate-printed
      (print-node
       alternate options))
    (set! result
          (string-append
           result
           " else "
           (doc-value-string alternate-printed))))
  result)

;;; Print a `ConditionalExpression` ESTree node to a `Doc` object.
(define (print-conditional-expression node (options (js-obj)))
  (define test
    (get-field test node))
  (define test-printed
    (print-node test options))
  (define consequent
    (get-field consequent node))
  (define consequent-printed
    (print-node consequent options))
  (define alternate
    (get-field alternate node))
  (define alternate-printed
    (print-node alternate options))
  (define result)
  (unless (estree-simple? test)
    (set! test-printed
          (doc-wrap test-printed options)))
  (unless (estree-simple? consequent)
    (set! consequent-printed
          (doc-wrap consequent-printed options)))
  (unless (or (estree-type? alternate "SequenceExpression")
              (estree-simple? alternate))
    (set! alternate-printed
          (doc-wrap alternate-printed options)))
  (list
   test-printed
   space
   "?"
   space
   consequent-printed
   space
   ":"
   space
   alternate-printed))

;;; Print a `WhileStatement` ESTree node to a `Doc` object.
(define (print-while-statement node (options (js-obj)))
  (define test
    (get-field test node))
  (define test-printed
    (print-node test options))
  (define body
    (get-field body node))
  (define body-printed
    (print-node body options))
  (when (estree-type? test "AssignmentExpression")
    (set! test-printed
          (doc-wrap test-printed options)))
  (list
   "while"
   space
   "("
   test-printed
   ")"
   space
   body-printed))

;;; Print a `DoWhileStatement` ESTree node to a `Doc` object.
(define (print-do-while-statement node (options (js-obj)))
  (define test
    (get-field test node))
  (define test-printed
    (print-node test options))
  (define body
    (get-field body node))
  (define body-printed
    (print-node body options))
  (when (estree-type? test "AssignmentExpression")
    (set! test-printed
          (doc-wrap test-printed options)))
  (list
   "do"
   space
   body-printed
   space
   "while"
   space
   "("
   test-printed
   ")"
   ";"))

;;; Print a `ForStatement` ESTree node to a `Doc` object.
(define (print-for-statement node (options (js-obj)))
  (define init
    (get-field init node))
  (define init-printed
    (~> (print-node init options)
        (print-doc options)
        (regexp-replace (regexp ";$") _ "")))
  (define test
    (get-field test node))
  (define test-printed
    (print-doc (print-node test options) options))
  (define update
    (get-field update node))
  (define update-printed
    (~> (print-node update options)
        (print-doc options)
        (regexp-replace (regexp ";$") _ "")))
  (define body
    (get-field body node))
  (define body-printed
    (print-node body options))
  (list
   "for"
   space
   "("
   init-printed
   ";"
   space
   test-printed
   ";"
   space
   update-printed
   ")"
   space
   body-printed))

;;; Print a `ForOfStatement` ESTree node to a `Doc` object.
(define (print-for-of-statement node (options (js-obj)))
  (define language
    (oget options "language"))
  (define left
    (get-field left node))
  (define left-printed
    (~> (print-node left options)
        (print-doc options)
        (regexp-replace (regexp ";$") _ "")))
  (define right
    (get-field right node))
  (define right-printed
    (print-node right options))
  (define body
    (get-field body node))
  (define body-printed
    (print-node body options))
  (define result-str)
  (when (eq? language "TypeScript")
    (set! left-printed
          (regexp-replace (regexp ": any$")
                          left-printed
                          "")))
  (list
   "for"
   space
   "("
   left-printed
   space
   "of"
   space
   right-printed
   ")"
   space
   body-printed))

;;; Print a `ForInStatement` ESTree node to a `Doc` object.
(define (print-for-in-statement node (options (js-obj)))
  (define language
    (oget options "language"))
  (define left
    (get-field left node))
  (define left-printed
    (~> (print-node left options)
        (print-doc options)
        (regexp-replace (regexp ";$") _ "")))
  (define right
    (get-field right node))
  (define right-printed
    (print-node right options))
  (define body
    (get-field body node))
  (define body-printed
    (print-node body options))
  (list
   "for"
   space
   "("
   left-printed
   space
   "in"
   space
   right-printed
   (if (eq? language "TypeScript")
       " as any[]"
       empty)
   ")"
   space
   "{"
   line
   (indent body-printed)
   line
   "}"))

;;; Print a `TryStatement` ESTree node to a `Doc` object.
(define (print-try-statement node (options (js-obj)))
  (define block
    (get-field block node))
  (define block-printed
    (print-node block options))
  (define handler
    (get-field handler node))
  (define finalizer
    (get-field finalizer node))
  (define result
    (list "try" space block-printed))
  (when handler
    (define handler-param
      (get-field param handler))
    (define handler-param-printed
      (if handler-param
          (print-node handler-param options)
          #f))
    (define handler-body-printed
      (print-node (get-field body handler) options))
    (set! result
          (append
           result
           (list
            space
            "catch"
            space)
           (if handler-param
               (list
                "("
                handler-param-printed
                ")"
                space)
               '())
           (list
            "{"
            line
            (indent handler-body-printed)
            line
            "}"))))
  (when finalizer
    (define finalizer-printed
      (print-node finalizer options))
    (set! result
          (append
           result
           (list
            space
            "finally"
            space
            finalizer-printed))))
  result)

;;; Print a `ClassDeclaration` ESTree node to a `Doc` object.
(define (print-class-declaration node (options (js-obj)))
  (define id
    (get-field id node))
  (define body
    (get-field body node))
  (define body-indented
    (indent (print-node body options)))
  (define body-printed
    (print-doc body-indented options))
  (define super-class
    (get-field superClass node))
  (list
   "class"
   space
   (if id
       (list
        (print-node id options)
        space)
       empty)
   (if super-class
       (list
        "extends"
        space
        (print-node super-class options)
        space)
       empty)
   "{"
   line
   body-indented
   (if (eq? body-printed "")
       empty
       line)
   "}"))

;;; Print a `ClassExpression` ESTree node to a `Doc` object.
(define (print-class-expression node (options (js-obj)))
  (print-class-declaration
   (new ClassDeclaration
        js/null
        (get-field body node)
        (get-field superClass node))
   options))

;;; Print a `ClassBody` ESTree node to a `Doc` object.
(define (print-class-body node (options (js-obj)))
  (~> node
      (get-field body _)
      (map (lambda (x)
             (print-node x options))
           _)
      (join (list line line) _)))

;;; Print a `PropertyDefinition` ESTree node to a `Doc` object.
(define (print-property-definition node (options (js-obj)))
  (define language
    (oget options "language"))
  (define key
    (get-field key node))
  (define value
    (get-field value node))
  (define static-flag
    (get-field static node))
  (define accessibility
    (get-field accessibility node))
  (list
   (if (and (eq? language "TypeScript")
            (eq? accessibility "private"))
       (list "private" space)
       empty)
   (if static-flag (list "static" space) empty)
   (print-node key options)
   (if (eq? language "TypeScript")
       (list ":" space "any")
       empty)
   (if value
       (list
        space
        "="
        space
        (print-node value options))
       empty)
   ";"))

;;; Print a `MethodDefinition` ESTree node to a `Doc` object.
(define (print-method-definition node (options (js-obj)))
  (define language
    (oget options "language"))
  (define key
    (get-field key node))
  (define key-printed
    (print-node key options))
  (define key-printed-str
    (print-doc key-printed options))
  (define value
    (get-field value node))
  (define value-printed
    (~> value
        (print-function
         _
         options
         (js-obj "returnType"
                 (if (eq? key-printed-str
                          "constructor")
                     ""
                     "any")))
        (print-doc _ options)
        (regexp-replace (regexp "^function ") _ "")))
  (define static-flag
    (get-field static node))
  (define computed-flag
    (get-field computed node))
  (define generator-flag
    (get-field generator value))
  (define accessibility
    (get-field accessibility node))
  (list
   (if (and (eq? language "TypeScript")
            (eq? accessibility "private"))
       (list "private" space)
       empty)
   (if static-flag
       (list "static" space)
       empty)
   (if generator-flag
       "*"
       empty)
   (if computed-flag
       (list "[" key-printed "]")
       key-printed)
   value-printed))

;;; Print an `ArrayExpression` ESTree node to a `Doc` object.
(define (print-array-expression node (options (js-obj)))
  (define language
    (oget options "language"))
  (define no-implicit-any
    (oget options "noImplicitAny"))
  (define type_
    (get-field typeAnnotation node))
  (define printed-expressions '())
  (define should-break #f)
  (define printed-exp)
  (define result)
  (for ((exp (get-field elements node)))
    (if exp
        (set! printed-exp
              (print-node exp
                          (js-obj-append
                           options
                           (js-obj "noImplicitAny" #f))))
        (set! printed-exp empty))
    (push-right! printed-expressions
                 (doc-value-string printed-exp))
    (when (doc-should-break? printed-exp)
      (set! should-break #t)))
  (cond
   (should-break
    (set! result
          (list
           "["
           line
           (~> printed-expressions
               (map (lambda (x)
                      (align 1 x))
                    _)
               (join (list "," line) _))
           line
           "]")))
   (else
    (set! result
          (list
           "["
           (join (list "," space)
                 printed-expressions)
           "]"))))
  (when (and no-implicit-any
             (not type_))
    (set! type_
          (new TSArrayType (new TSAnyKeyword))))
  (when (and type_
             (eq? language "TypeScript"))
    (set! result
          (append
           result
           (list
            ":"
            space
            (print-node type_ options)))))
  (when should-break
    (set! result
          (group result (js-obj "should-break" should-break))))
  result)

;;; Print an `ArrayPattern` ESTree node to a `Doc` object.
(define (print-array-pattern node (options (js-obj)))
  (print-array-expression node options))

;;; Print a `NewExpression` ESTree node to a `Doc` object.
(define (print-new-expression node (options (js-obj)))
  (list
   "new"
   space
   (print-node (new CallExpression
                    (get-field callee node)
                    (get-field arguments node))
               options)))

;;; Print an `ImportDeclaration` ESTree node to a `Doc` object.
(define (print-import-declaration node (options (js-obj)))
  (define specifiers
    (get-field specifiers node))
  (define source
    (get-field source node))
  (cond
   ((and (= (array-list-length specifiers) 1)
         (not (estree-type? (first specifiers)
                            "ImportSpecifier")))
    (list
     "import"
     space
     (print-node (first specifiers) options)
     space
     "from"
     space
     (print-node source options)
     ";"))
   (else
    (list
     "import"
     space
     "{"
     line
     (~> specifiers
         (map (lambda (x)
                (print-node x options))
              _)
         (join (list "," line) _)
         (indent _))
     line
     "}"
     space
     "from"
     space
     (print-node source options)
     ";"))))

;;; Print an `ImportSpecifier` ESTree node to a `Doc` object.
(define (print-import-specifier node (options (js-obj)))
  (define local
    (get-field local node))
  (define local-printed
    (print-doc (print-node local options) options))
  (define imported
    (get-field imported node))
  (define imported-printed
    (print-doc (print-node imported options) options))
  (cond
   ((eq? local-printed imported-printed)
    local-printed)
   (else
    (list
     local-printed
     space
     "as"
     space
     imported-printed))))

;;; Print an `ImportDefaultSpecifier` ESTree node to a `Doc` object.
(define (print-import-default-specifier node (options (js-obj)))
  (print-node (get-field local node) options))

;;; Print an `ImportNamespaceSpecifier` ESTree node to a `Doc` object.
(define (print-import-namespace-specifier node (options (js-obj)))
  (list
   "*"
   space
   "as"
   space
   (print-node (get-field local node) options)))

;;; Print an `ExportNamedDeclaration` ESTree node to a `Doc` object.
(define (print-export-named-declaration node (options (js-obj)))
  (define specifiers
    (get-field specifiers node))
  (define specifiers-printed
    (~> specifiers
        (map (lambda (x)
               (print-node x options))
             _)
        (join (list "," line) _)
        (indent)
        (print-doc options)))
  (list
   "export"
   space
   "{"
   line
   specifiers-printed
   (if (eq? specifiers-printed "")
       empty
       line)
   "}"
   ";"))

;;; Print an `ExportSpecifier` ESTree node to a `Doc` object.
(define (print-export-specifier node (options (js-obj)))
  (print-import-specifier
   (new ImportSpecifier
        (get-field local node)
        (get-field exported node))
   options))

;;; Print an `ExportAllDeclaration` ESTree node to a `Doc` object.
(define (print-export-all-declaration node (options (js-obj)))
  (list
   "export"
   space
   "*"
   space
   "from"
   space
   (print-node (get-field source node) options)
   ";"))

;;; Print an `ObjectExpression` ESTree node to a `Doc` object.
(define (print-object-expression node (options (js-obj)))
  (define properties
    (get-field properties node))
  (list
   "{"
   (if (= (array-list-length properties) 0)
       empty
       (list
        line
        (~> properties
            (map (lambda (x)
                   (print-node x options))
                 _)
            (join (list "," line) _)
            (indent))
        line))
   "}"))

;;; Print an `ObjectPattern` ESTree node to a `Doc` object.
(define (print-object-pattern node (options (js-obj)))
  (list
   "{"
   (~> (get-field properties node)
       (map (lambda (prop)
              (print-assignment-property prop options))
            _)
       (join (list "," space) _))
   "}"))

;;; Print an `AssignmentProperty` ESTree node to a `Doc` object.
(define (print-assignment-property node (options (js-obj)))
  (define options1
    (js-obj-append
     options
     (js-obj "noImplicitAny" #f)))
  (define key
    (get-field key node))
  (define key-printed
    (print-node key options1))
  (define key-printed-str
    (print-doc key-printed options1))
  (define value
    (get-field value node))
  (define value-printed
    (print-node value options1))
  (define value-printed-str
    (print-doc value-printed options1))
  (if (eq? key-printed-str value-printed-str)
      key-printed
      (list key-printed ":" space value-printed)))

;;; Print a `Property` ESTree node to a `Doc` object.
(define (print-property node (options (js-obj)))
  (define language
    (oget options "language"))
  (define key
    (get-field key node))
  (define key-printed
    (print-node key options))
  (define value
    (get-field value node))
  (define value-printed
    (print-node value options))
  (when (get-field computed node)
    (set! key-printed
          (list
           "["
           key-printed
           (if (eq? language "TypeScript")
               (list space "as any")
               empty)
           "]")))
  (list key-printed
        ":"
        space
        value-printed))

;;; Print a `Program` ESTree node to a `Doc` object.
(define (print-program node (options (js-obj)))
  (~> (get-field body node)
      (map (lambda (x)
             (print-node x options))
           _)
      (join (list line line) _)))

;;; Print a `SwitchStatement` ESTree node to a `Doc` object.
(define (print-switch-statement node (options (js-obj)))
  (define discriminant
    (get-field discriminant node))
  (define discriminant-printed
    (print-node discriminant options))
  (define cases
    (get-field cases node))
  (define cases-printed
    (~> cases
        (map (lambda (x)
               (print-node x options))
             _)
        (join line _)
        (indent _)))
  (list
   "switch"
   space
   "("
   discriminant-printed
   ")"
   space
   "{"
   line
   cases-printed
   line
   "}"))

;;; Print a `SwitchCase` ESTree node to a `Doc` object.
(define (print-switch-case node (options (js-obj)))
  (define test
    (get-field test node))
  (define test-printed
    (cond
     (test
      (list
       "case"
       space
       (print-node test options)))
     (else
      "default")))
  (define consequent
    (get-field consequent node))
  (define is-block-statement
    (and (= (array-list-length consequent) 1)
         (first consequent)
         (estree-type? (first consequent) "BlockStatement")))
  (define consequent-printed
    (map (lambda (x)
           (print-node x options))
         consequent))
  (cond
   (is-block-statement
    (set! consequent-printed
          (list
           space
           (first consequent-printed))))
   (else
    (set! consequent-printed
          (list
           line
           (~> consequent-printed
               (join line _)
               (indent _))))))
  (list
   test-printed
   ":"
   consequent-printed))

;;; Print a `TSAsExpression` TSESTree node to a `Doc` object.
(define (print-ts-as-expression node (options (js-obj)))
  (define expression
    (get-field expression node))
  (define expression-printed
    (print-node expression options))
  (define type-annotation
    (get-field typeAnnotation node))
  (define type-annotation-printed
    (print-ts-type type-annotation options))
  (list
   expression-printed
   space
   "as"
   space
   type-annotation-printed))

;;; Print TSESTree type to a `Doc` object.
(define (print-ts-type node (options (js-obj)))
  (define type_
    (estree-type node))
  (cond
   ((hash-has-key? printer-map type_)
    (print-node node options))
   (else
    "any")))

;;; Print a `TSAnyKeyword` TSESTree node to a `Doc` object.
(define (print-ts-any-keyword node (options (js-obj)))
  "any")

;;; Print a `TSVoidKeyword` TSESTree node to a `Doc` object.
(define (print-ts-void-keyword node (options (js-obj)))
  "void")

;;; Print a `TSUndefinedKeyword` TSESTree node to a `Doc` object.
(define (print-ts-undefined-keyword node (options (js-obj)))
  "undefined")

;;; Print a `TSBooleanKeyword` TSESTree node to a `Doc` object.
(define (print-ts-boolean-keyword node (options (js-obj)))
  "boolean")

;;; Print a `TSNumberKeyword` TSESTree node to a `Doc` object.
(define (print-ts-number-keyword node (options (js-obj)))
  "number")

;;; Print a `TSStringKeyword` TSESTree node to a `Doc` object.
(define (print-ts-string-keyword node (options (js-obj)))
  "string")

;;; Print a `TSArrayType` TSESTree node to a `Doc` object.
(define (print-ts-array-type node (options (js-obj)))
  (define element-type
    (get-field elementType node))
  (define result
    (print-node element-type options))
  (when (estree-type? element-type "TSUnionType")
    (set! result
          (doc-wrap result options)))
  (list result "[]"))

;;; Print a `TSTupleType` TSESTree node to a `Doc` object.
(define (print-ts-tuple-type node (options (js-obj)))
  (define element-types
    (get-field elementTypes node))
  (list
   "["
   (join (list "," space)
         (map (lambda (x)
                (print-node x options))
              element-types))
   "]"))

;;; Print a `TSUnionType` TSESTree node to a `Doc` object.
(define (print-ts-union-type node (options (js-obj)))
  (~> (get-field types node)
      (map (lambda (x)
             (define result
               (print-node x options))
             (when (estree-type? x "TSUnionType")
               (set! result
                     (doc-wrap result options)))
             result)
           _)
      (join (list space "|" space) _)))

;;; Print a `TSFunctionType` TSESTree node to a `Doc` object.
(define (print-ts-function-type node (options (js-obj)))
  (list
   "("
   (~> (get-field params node)
       (map (lambda (x)
              (print-node x options))
            _)
       (join (list "," space) _))
   ")"
   space
   "=>"
   space
   (print-node (get-field returnType node) options)))

;;; Print a `TSTypeAliasDeclaration` TSESTree node to a `Doc` object.
(define (print-ts-type-alias-declaration node (options (js-obj)))
  (list
   "type"
   space
   (print-node (get-field id node) options)
   space
   "="
   space
   (print-node (get-field typeAnnotation node) options)
   ";"))

;;; Print a `TSTypeAnnotation` TSESTree node to a `Doc` object.
(define (print-ts-type-annotation node (options (js-obj)))
  (print-node (get-field typeAnnotation node) options))

;;; Print a `TSLiteralType` TSESTree node to a `Doc` object.
(define (print-ts-literal-type node (options (js-obj)))
  (print-node (get-field literal node)
              (js-obj-append
               options
               (js-obj "noImplicitAny" #f))))

;;; Print a `TSTypeReference` TSESTree node to a `Doc` object.
(define (print-ts-type-reference node (options (js-obj)))
  (define name
    (get-field typeName node))
  (define params
    (get-field typeParameters node))
  (list
   (print-node name
               (js-obj-append
                options
                (js-obj "noImplicitAny" #f)))
   (if params
       (print-node params
                   (js-obj-append
                    options
                    (js-obj "noImplicitAny" #f)))
       empty)))

;;; Print a `TSTypeParameterInstantiation` TSESTree node to a `Doc` object.
(define (print-ts-type-parameter-instantiation node (options (js-obj)))
  (define params
    (get-field params node))
  (list
   "<"
   (~> params
       (map (lambda (x)
              (print-ts-type
               x
               (js-obj-append
                options
                (js-obj "noImplicitAny" #f))))
            _)
       (join "," _))
   ">"))

;;; Print an `XRawJavaScript` ESTree extension node to a `Doc` object.
(define (print-x-raw-javascript node (options (js-obj)))
  (define str
    (get-field js node))
  (when (regexp-match (regexp "^function \\(") str)
    (set! str (doc-wrap str)))
  str)

;;; Default printer.
;;;
;;; Returns the empty string.
(define (default-printer node (options (js-obj)))
  empty)

;;; Mapping from node types to printer functions.
(define printer-map
  (make-hash
   `(("ArrayExpression" . ,print-array-expression)
     ("ArrayPattern" . ,print-array-pattern)
     ("ArrowFunctionExpression" . ,print-arrow-function-expression)
     ("AssignmentExpression" . ,print-assignment-expression)
     ("AssignmentPattern" . ,print-assignment-pattern)
     ("AwaitExpression" . ,print-await-expression)
     ("BinaryExpression" . ,print-binary-expression)
     ("BlockStatement" . ,print-block-statement)
     ("BreakStatement" . ,print-break-statement)
     ("CallExpression" . ,print-call-expression)
     ("ClassBody" . ,print-class-body)
     ("ClassDeclaration" . ,print-class-declaration)
     ("ClassExpression" . ,print-class-expression)
     ("ConditionalExpression" . ,print-conditional-expression)
     ("ContinueStatement" . ,print-continue-statement)
     ("DoWhileStatement" . ,print-do-while-statement)
     ("ExportAllDeclaration" . ,print-export-all-declaration)
     ("ExportNamedDeclaration" . ,print-export-named-declaration)
     ("ExportSpecifier" . ,print-export-specifier)
     ("ExpressionStatement" . ,print-expression-statement)
     ("ForInStatement" . ,print-for-in-statement)
     ("ForOfStatement" . ,print-for-of-statement)
     ("ForStatement" . ,print-for-statement)
     ("FunctionDeclaration" . ,print-function-declaration)
     ("FunctionExpression" . ,print-function-expression)
     ("Identifier" . ,print-identifier)
     ("IfStatement" . ,print-if-statement)
     ("ImportDeclaration" . ,print-import-declaration)
     ("ImportDefaultSpecifier" . ,print-import-default-specifier)
     ("ImportNamespaceSpecifier" . ,print-import-namespace-specifier)
     ("ImportSpecifier" . ,print-import-specifier)
     ("Literal" . ,print-literal)
     ("LogicalExpression" . ,print-logical-expression)
     ("MemberExpression" . ,print-member-expression)
     ("MethodDefinition" . ,print-method-definition)
     ("NewExpression" . ,print-new-expression)
     ("ObjectExpression" . ,print-object-expression)
     ("ObjectPattern" . ,print-object-pattern)
     ("Program" . ,print-program)
     ("Property" . ,print-property)
     ("PropertyDefinition" . ,print-property-definition)
     ("RestElement" . ,print-rest-element)
     ("ReturnStatement" . ,print-return-statement)
     ("SequenceExpression" . ,print-sequence-expression)
     ("SpreadElement" . ,print-spread-element)
     ("SwitchStatement" . ,print-switch-statement)
     ("SwitchCase" . ,print-switch-case)
     ("TSAnyKeyword" . ,print-ts-any-keyword)
     ("TSArrayType" . ,print-ts-array-type)
     ("TSAsExpression" . ,print-ts-as-expression)
     ("TSBooleanKeyword" . ,print-ts-boolean-keyword)
     ("TSFunctionType" . ,print-ts-function-type)
     ("TSLiteralType" . ,print-ts-literal-type)
     ("TSNumberKeyword" . ,print-ts-number-keyword)
     ("TSStringKeyword" . ,print-ts-string-keyword)
     ("TSTupleType" . ,print-ts-tuple-type)
     ("TSTypeAliasDeclaration" . ,print-ts-type-alias-declaration)
     ("TSTypeAnnotation" . ,print-ts-type-annotation)
     ("TSTypeParameterInstantiation" . ,print-ts-type-parameter-instantiation)
     ("TSTypeReference" . ,print-ts-type-reference)
     ("TSUndefinedKeyword" . ,print-ts-undefined-keyword)
     ("TSUnionType" . ,print-ts-union-type)
     ("TSVoidKeyword" . ,print-ts-void-keyword)
     ("TaggedTemplateExpression" . ,print-tagged-template-expression)
     ("TemplateElement" . ,print-template-element)
     ("TemplateLiteral" . ,print-template-literal)
     ("ThisExpression" . ,print-this-expression)
     ("ThrowStatement" . ,print-throw-statement)
     ("TryStatement" . ,print-try-statement)
     ("UnaryExpression" . ,print-unary-expression)
     ("UpdateExpression" . ,print-update-expression)
     ("VariableDeclaration" . ,print-variable-declaration)
     ("VariableDeclarator" . ,print-variable-declarator)
     ("WhileStatement" . ,print-while-statement)
     ("YieldExpression" . ,print-yield-expression)
     ("XRawJavaScript" . ,print-x-raw-javascript))))

(provide
  (rename-out (print-node print-estree-node))
  (rename-out (print-sexp-as-expression print-as-expression))
  print
  print-estree
  print-node
  print-rose
  print-sexp
  print-sexp-as-expression
  write-to-string)
