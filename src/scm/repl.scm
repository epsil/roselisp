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
(require (only-in "./equal"
                  equal?_))
(require (only-in "./env"
                  LispEnvironment
                  with-environment))
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

;;; Read utility.
(define (r input)
  (~> input
      (string-append "(" _ ")")
      (read _)))

;;; Eval utility.
(define (e input (env (make-repl-environment)))
  (~> input
      (map (lambda (exp)
             (let ((result #u))
               (try
                 (set! result
                       (eval_ exp env))
                 (catch Error err
                   (display err)))
               result))
           _)))

;;; Read--Eval utility.
(define (re input (env (make-repl-environment)))
  (~> input
      (r _)
      (e _ env)))

;;; Print utility.
(define (p input)
  (~> input
      (map print-sexp-as-expression _)
      (string-join _ "\n")))

;;; Read--Eval--Print utility.
(define (rep input (env (make-repl-environment)))
  (~> input
      (re _ env)
      (p _)))

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
          (js/obj :input stdin
                  :output stdout)))
  (define quit-flag #f)
  (define (quit!)
    (unless quit-flag
      (set! quit-flag #t)
      (send rl close)))
  (define env
    (make-repl-environment
     `((exit ,quit! '(-> Any * Any))
       (help ,help '(-> Any * Any))
       (quit ,quit! '(-> Any * Any)))))
  ;; Read-eval-print loop
  (define (loop-f . args)
    (define (callback x)
      (define exp
        (r x))
      (cond
       ((or quit-flag
            (quit-cmd? exp))
        (quit!))
       ((help-cmd? exp)
        (help)
        (loop-f))
       (else
        (with-environment
         env
         (lambda ()
           ;; Read (R), Evaluate (E), Print (P).
           (~> exp
               (e _ env)
               (p _)
               (display _))))
        (loop-f))))
    (send rl question repl-prompt callback))
  (display initial-repl-message)
  (loop-f))

;;; Make an environment for the REPL.
(define (make-repl-environment (bindings '()))
  (new LispEnvironment
       bindings
       lang-environment))

;;; Whether `exp` is a command for quitting the REPL.
(define (help-cmd? exp)
  (or (equal?_ exp '((unquote h)))
      (equal?_ exp '((unquote help)))
      (equal?_ exp '((help)))))

;;; Whether `exp` is a command for quitting the REPL.
(define (quit-cmd? exp)
  (or (equal?_ exp '((unquote q)))
      (equal?_ exp '((unquote quit)))
      (equal?_ exp '((quit)))
      (equal?_ exp '((unquote exit)))
      (equal?_ exp '((exit)))
      (equal?_ exp '((unquote x)))))

;;; Print a value.
(define (print-value x (options (js/obj)))
  (~> x
      (print-sexp-as-expression _ options)
      (display _)))

;;; Display help message.
(define (help)
  (display repl-help-message))

(provide
  r
  re
  rep
  repl)
