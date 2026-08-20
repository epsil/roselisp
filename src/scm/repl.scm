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
                  with-environment
                  with-environment-f))
(require (only-in "./language"
                  (interpret eval_)
                  lang-environment
                  load_
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
(define (e input (env (make-interactive-environment)))
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
(define (re input (env (make-interactive-environment)))
  (~> input
      (r _)
      (e _ env)))

;;; Print utility.
(define (p input)
  (~> input
      (map print-sexp-as-expression _)
      (string-join _ "\n")))

;;; Read--Eval--Print utility.
(define (rep input (env (make-interactive-environment)))
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
  (define print-flag #t)
  (define quit-flag #f)
  (define (help)
    (set! print-flag #f)
    (display repl-help-message))
  (define (quit!)
    (set! print-flag #f)
    (set! quit-flag #t)
    (send rl close))
  (define interactive-env
    (make-interactive-environment
     (js/obj :help help
             :quit quit!)))
  ;; Read-eval-print loop
  (define (loop-f . args)
    (define (callback x)
      (with-environment
       interactive-env
       ;; Read (R), Evaluate (E), Print (P).
       (set! print-flag #t)
       (define result
         (~> x
             (r _)
             (rewrite-expression _)
             (e _ interactive-env)
             (p _)))
       (when print-flag
         (display result))
       (unless quit-flag
         (loop-f))))
    (send rl question repl-prompt callback))
  (display initial-repl-message)
  (loop-f))

;;; Make an environment for the REPL.
(define (make-interactive-environment (options (js/obj)))
  (define help_ (oget options :help))
  (define quit_ (oget options :quit))
  (define parent-env
    (new LispEnvironment
         `((exit ,quit_ '(-> Any * Any))
           (help ,help_ '(-> Any * Any))
           (quit ,quit_ '(-> Any * Any))
           (load ,load_ '(-> Any * Any)))
         lang-environment))
  (define env
    (new LispEnvironment
         '()
         parent-env))
  env)

;;; Whether `exp` is a command for quitting the REPL.
(define (help-cmd? exp)
  (equal?_ exp '((help))))

;;; Whether `exp` is a command for quitting the REPL.
(define (quit-cmd? exp)
  (or (equal?_ exp '((quit)))
      (equal?_ exp '((exit)))))

;;; Print a value.
(define (print-value x (options (js/obj)))
  (~> x
      (print-sexp-as-expression _ options)
      (display _)))

;;; Rewrite `(unquote ...)` expressions to regular
;;; function calls.
(define (rewrite-expression exp)
  (match exp
    ((list (list 'unquote x) y ...)
     (cond
      ((eq? x 'h)
       (set! x 'help))
      ((memq? x '(x q))
       (set! x 'quit)))
     `((,x ,@y)))
    (_
     exp)))

(provide
  r
  re
  rep
  repl)
