;; SPDX-License-Identifier: MPL-2.0
;;; # Command-line interface
;;;
;;; Simple command-line interface.
;;;
;;; ## Description
;;;
;;; This file defines and invokes a `main` function that reads input
;;; from the command line and takes appropriate action. Options
;;; parsing is done with [`minimist`][npm:minimist].
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;
;;; [npm:minimist]: https://www.npmjs.com/package/minimist

(require "minimist")
(require build-options "minimist-options")
(require (only-in "./decompiler"
                  decompile-files!))
(require (only-in "./language"
                  compile-files!
                  interpret-files
                  interpret-string))
(require (only-in "./repl"
                  repl))

;;; Command line options. Passed to
;;; [`minimist-options`][npm:minimist-options]
;;; for parsing.
;;;
;;; [npm:minimist-options]: https://www.npmjs.com/package/minimist-options
(define cli-options
  (js-obj
   "camelCase" (js-obj
                "alias" "camel-case"
                "default" #t
                "type" "boolean")
   "comments" (js-obj
               "default" #t
               "type" "boolean")
   "compile" (js-obj
              "default" #f
              "alias" "c"
              "type" "boolean")
   "decompile" (js-obj
                "default" #f
                "alias" "d"
                "type" "boolean")
   "eval" (js-obj
           "default" ""
           "alias" "e"
           "type" "string")
   "fevalBindings" (js-obj
                    "alias" "feval-bindings"
                    "default" #f
                    "type" "boolean")
   "finlineFunctions" (js-obj
                       "alias" "finline-functions"
                       "default" #f
                       "type" "boolean")
   "help" (js-obj
           "default" #f
           "alias" "h"
           "type" "boolean")
   "indent" (js-obj
             "default" 2
             "type" "number")
   "language" (js-obj
               "type" "string")
   "outDir" (js-obj
             "alias" "out-dir"
             "type" "string"
             "default" ".")
   "quick" (js-obj
            "default" #f
            "alias" "q"
            "type" "boolean")
   "repl" (js-obj
           "default" #f
           "alias" "i"
           "type" "boolean")))

;;; Help message. Displayed when the program
;;; is invoked with `-h` or `--help`.
(define help-message
  "Lisp interpreter and transpiler in JavaScript

REPL:

  roselisp

Interpret a file:

  roselisp input.scm

Compile a file to JavaScript:

  roselisp -c input.scm

Compile a file to TypeScript:

  roselisp -c --language TypeScript input.scm

Options:

  --compile   Compiles one or more files (short form -c).
              Otherwise, the default is interpretation.
  --quick     Incremental compilation (short form -q).
              Only compiles a file if the input file is
              newer than the output file.
  --eval      Evaluate an expression (short form -e).
              For example, roselisp -e \"(+ 1 1)\"
              evaluates the expression (+ 1 1) and
              prints the result to standard output.
              Otherwise interprets it (default).
  --indent    The number of spaces to indent
              (default: 2).
  --language  Language: JavaScript or TypeScript
              (default: JavaScript).
  --out-dir   Output directory for compiled files
              (default: same directory).")

;;; Normalize CLI options.
(define (normalize-cli-options options)
  (define language-option
    (or (oget options "language")
        ""))
  (define inline-functions-option
    (oget options "finlineFunctions"))
  (define language
    (if (regexp-match (regexp "^TypeScript$" "i")
                      language-option)
        "TypeScript"
        "JavaScript"))
  (js-obj-append
   options
   (js-obj "language" language
           "inlineFunctions" inline-functions-option)))

;;; Normalize compilation options.
(define (normalize-compilation-options options)
  (define eval-bindings-option
    (oget options "fevalBindings"))
  (js-obj-append
   options
   (js-obj "eval" eval-bindings-option)))

;;; `main` function. Invoked when the program is
;;; run from the command line.
(: main (-> Void))
(define (main)
  (define flags
    (normalize-cli-options
     (minimist
      (~> (get-field argv process)
          (send slice 2))
      ((ann build-options Any)
       cli-options))))
  (define input
    (get-field _ flags))
  (define compile-flag
    (get-field compile flags))
  (define decompile-flag
    (get-field decompile flags))
  (define eval-flag
    (get-field eval flags))
  (define repl-flag
    (get-field repl flags))
  (define help-flag
    (get-field help flags))
  (cond
   (help-flag
    (display help-message))
   (eval-flag
    (display (interpret-string eval-flag)))
   ((or repl-flag
        (= (array-list-length input) 0))
    (repl))
   (decompile-flag
    (decompile-files! input flags))
   (compile-flag
    (define compilation-options
      (normalize-compilation-options flags))
    (compile-files! input compilation-options))
   (else
    (interpret-files input))))

;; Invoke the `main` function.
(main)
