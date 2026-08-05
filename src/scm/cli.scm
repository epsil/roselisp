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
  (js/obj
   :case (js/obj
          :default "camelcase"
          :type "string")
   :comments (js/obj
              :default #t
              :type "boolean")
   :compile (js/obj
             :alias "c"
             :default #f
             :type "boolean")
   :decompile (js/obj
               :alias "d"
               :default #f
               :type "boolean")
   :eval (js/obj
          :alias "e"
          :default ""
          :type "string")
   :fcommonjs (js/obj
               :default #f
               :type "boolean")
   :fes-module-interop (js/obj
                        :alias "fes-module-interop"
                        :default #f
                        :type "boolean")
   :feval-bindings (js/obj
                    :alias "feval-bindings"
                    :default #f
                    :type "boolean")
   :finline-functions (js/obj
                       :alias "finline-functions"
                       :default #f
                       :type "boolean")
   :fsemicolon (js/obj
                :default #t
                :type "boolean")
   :help (js/obj
          :alias "h"
          :default #f
          :type "boolean")
   :indent (js/obj
            :default 2
            :type "number")
   :language (js/obj
              :default "javascript"
              :type "string")
   :optimize (js/obj
              :default #t
              :type "boolean")
   :out-dir (js/obj
             :alias "out-dir"
             :default "."
             :type "string")
   :quick (js/obj
           :alias "q"
           :default #f
           :type "boolean")
   :repl (js/obj
          :alias "i"
          :default #f
          :type "boolean")))

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

  roselisp -c --language typescript input.scm

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
  --language  Language: javascript or typescript
              (default: javascript).
  --out-dir   Output directory for compiled files
              (default: same directory).")

;;; Normalize CLI options.
(define (normalize-cli-options options)
  (define language
    (string-downcase (oget options :language)))
  (js/obj-append
   options
   (js/obj :language language)))

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
        (= (js/length input) 0))
    (repl))
   (decompile-flag
    (decompile-files! input flags))
   (compile-flag
    (compile-files! input flags))
   (else
    (interpret-files input))))

;; Invoke the `main` function.
(main)
