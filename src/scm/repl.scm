;; SPDX-License-Identifier: MPL-2.0
;;; # REPL
;;;
;;; Read--eval--print loop (REPL).
;;;
;;; ## Description
;;;
;;; This file defines a very simple read--eval--print loop
;;; ([REPL][w:REPL]), i.e., an interactive language shell.
;;; Reading is done with Node's [`readline`][node:readline]
;;; library.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;
;;; [w:REPL]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
;;; [node:readline]: https://nodejs.org/api/readline.html

(require (only-in "process"
                  stdin
                  stdout))
(require readline "readline")
(require (only-in "./constants"
                  version))
(require (only-in "./env"
                  LispEnvironment))
(require (only-in "./language"
                  (interpret eval_)
                  lang-environment
                  print-sexp-as-expression))
(require (only-in "./parser"
                  read))

;;; REPL prompt.
(define repl-prompt "> ")

;;; Message displayed when starting the REPL.
(define initial-repl-message
  (string-append
   ";; Roselisp version " version ".\n"
   ";; Type ,h for help and ,q to quit."))

;;; Help message displayed by the REPL's `help` command.
(define repl-help-message
  "Enter an S-expression to evaluate it.
Use the up and down keys to access previous expressions.

Type ,q to quit.")

;;; Start a simple REPL.
;;;
;;; The REPL reads from standard input using Node's
;;; [`readline`][node:readline] module.
;;;
;;; [node:readline]: https://nodejs.org/api/readline.html
(: repl (-> Void))
(define (repl)
  (define rl
    (send readline
          createInterface
          (js-obj "input" stdin
                  "output" stdout)))
  (define quit-flag #f)
  (define (quit)
    (unless quit-flag
      (set! quit-flag #t)
      (send rl close)))
  (define env
    (make-repl-environment
     `((exit ,quit "function")
       (help ,help "function")
       (quit ,quit "function"))))
  ;; Read-eval-print loop
  (define (loop-f . args)
    (define (callback x)
      (cond
       ((or quit-flag
            (is-quit-cmd? x))
        (quit))
       ((is-help-cmd? x)
        (help)
        (loop-f))
       (else
        (define result undefined)
        ;; Read (R).
        (define exp
          (read x))
        ;; Evaluate (E).
        (try
          (set! result
                (eval_ exp env))
          ;; Print (P).
          (print-value result)
          (catch Error e
            (display e)))
        ;; Loop (L).
        (loop-f))))
    (send rl question repl-prompt callback))
  (display initial-repl-message)
  (loop-f))

;;; Make an environment for the REPL.
(define (make-repl-environment bindings)
  (new LispEnvironment
       bindings
       lang-environment))

;;; Whether `str` is a command for quitting the REPL.
(define (is-help-cmd? str)
  (memq? str
         '(",h"
           ",help"
           "(help)")))

;;; Whether `str` is a command for quitting the REPL.
(define (is-quit-cmd? str)
  (memq? str
         '(",q"
           ",quit"
           "(quit)"
           ",exit"
           "(exit)")))

;;; Print a value.
(define (print-value x (options (js-obj)))
  (display
   (print-sexp-as-expression x options)))

;;; Display help message.
(define (help)
  (display repl-help-message))

(provide
  repl)
