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
(require (only-in "./util"
                  copy-into-array!))

(declare-macro with-environment)

;;; REPL class.
;;;
;;; Encapsulates a [`readline`][node:readline] instance that reads
;;; from standard input.
;;;
;;; [node:readline]: https://nodejs.org/api/readline.html
(define-class REPL ()
  ;;; REPL prompt.
  (define prompt "> ")

  ;;; REPL state.
  (define state "prompt")

  ;;; REPL user input.
  (define input "")

  ;;; REPL history.
  ;;;
  ;;; A list of strings. The latest entry is stored
  ;;; at the beginning of the list.
  (define history '())

  ;;; Flag for printing values.
  (define print-flag #t)

  ;;; Flag for quitting.
  (define quit-flag #f)

  ;;; `readline` instance.
  (define rl #u)

  ;;; `readline` history.
  (define rl-history '())

  ;;; The REPL environment.
  (define env)

  ;;; Message displayed when starting the REPL.
  (define startup-message
    (string-append
     ";; Roselisp version " version ".\n"
     ";; Type ,h for help and ,q to quit."))

  ;;; Help message displayed by the REPL's `help` command.
  (define help-message
    ";; Enter an S-expression to evaluate it.
;; Use the up and down keys to access previous expressions.
;;
;; Type ,q to quit.")

  ;;; Create a new REPL.
  (define (constructor)
    (set-field! env
                this
                (make-interactive-environment
                 (js/obj :help
                         (lambda ()
                           (set-field! print-flag this #f)
                           (display (get-field help-message this)))
                         :quit
                         (lambda ()
                           (set-field! print-flag this #f)
                           (set-field! quit-flag this #t))))))

  ;;; Start the REPL.
  (define/public (start)
    ;; Initialize the `readline` instance.
    (set-field! rl
                this
                (send readline
                      createInterface
                      (js/obj :input stdin
                              :output stdout)))
    (send (get-field rl this)
          on
          "line"
          (lambda (input)
            (with-environment
             (get-field env this)
             (set-field! print-flag this #t)
             (cond
              ((eq? (get-field state this) "read")
               (set-field! input
                           this
                           (string-append (get-field input this)
                                          "\n"
                                          input))
               (set-field! state this "prompt"))
              (else
               (set-field! input this input)))
             (define result
               (send this
                     rep
                     (get-field input this)
                     (get-field env this)))
             (cond
              ((get-field quit-flag this)
               (send (get-field rl this) close))
              ((eq? (get-field state this) "read")
               (send this read-line))
              (else
               (when (get-field print-flag this)
                 (display result))
               (send this read-line))))))
    (send (get-field rl this)
          on
          "history"
          (lambda (x)
            (set-field! rl-history this x)))
    ;; Start the read--eval--print loop.
    (send this print-startup-message)
    (send this read-line))

  ;;; Read--eval--print method.
  (define/public (rep input (env (get-field env this)))
    ;; Ignore leading whitespace.
    (when (regexp-match (regexp "^\\s*$") input)
      (set-field! state this "read")
      (return ""))
    ;; Parse user input into expressions. Note the plural: we allow
    ;; for multiple expressions to be entered at a single prompt,
    ;; so that what is processed here is not a single expression,
    ;; but rather a list of expressions.
    (define parsed-expressions '())
    (try
      (set! parsed-expressions
            (~> input
                (string-append "(" _ ")")
                (read _)))
      (catch Error err
        (cond
         ;; If we are reading a multi-line expression,
         ;; then continue listening for user input.
         ((eq? (get-field message err) "eof")
          (set-field! state this "read")
          (return ""))
         (else
          (throw err)))))
    ;;; Update the REPL history.
    (push-left! (get-field history this) input)
    ;; Synchronize the `readline` instance's history with the
    ;; REPL history. This is needed in order to handle multi-line
    ;; expressions correctly, because otherwise the `readline`
    ;; instance will create one history entry per line.
    (copy-into-array! (get-field history this)
                      (get-field rl-history this))
    ;; Rewrite the expressions slightly in order to handle REPL
    ;; shortcuts such as `,h` and `,q`.
    (define rewritten-expressions
      (rewrite-expressions parsed-expressions))
    ;; Evaluate the expressions one by one, producing a list
    ;; of values.
    (define evaluated-expressions
      (map (lambda (exp)
             (let ((result #u))
               (unless (get-field quit-flag this)
                 (try
                   (set! result
                         (eval_ exp env))
                   (catch Error err
                     (display err))))
               result))
           rewritten-expressions))
    ;; Continue unless the user has quit. If the user has quit,
    ;; the return value is just the empty string.
    (define result "")
    (unless (get-field quit-flag this)
      ;; Print the values, producing a list of value strings.
      (define printed-expressions
        (map print-sexp-as-expression
             evaluated-expressions))
      ;; Concatenate the value strings into a single string,
      ;; with each value string on its own line.
      (set! result
            (string-join printed-expressions "\n")))
    result)

  ;;; Read a line of user input.
  (define (read-line)
    (send (get-field rl this)
          setPrompt
          (if (eq? (get-field state this) "read")
              ""
              (get-field prompt this)))
    (send (get-field rl this) prompt))

  ;;; Print startup message.
  (define (print-startup-message)
    (display (get-field startup-message this))))

;;; Whether `exp` is a command for quitting the REPL.
(define (help-cmd? exp)
  (equal?_ exp '((help))))

;;; Whether `exp` is a command for quitting the REPL.
(define (quit-cmd? exp)
  (or (equal?_ exp '((quit)))
      (equal?_ exp '((exit)))))

;;; Rewrite `(unquote ...)` expressions to regular
;;; function calls.
(define (rewrite-expressions exp)
  (match exp
    ((list (list 'unquote x) y ...)
     (cond
      ((eq? x 'h)
       (set! x 'help))
      ((memq? x '(ex x q))
       (set! x 'quit)))
     `((,x ,@y)))
    (_
     exp)))

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

;;; Read--eval--print function.
(define (rep input)
  (send (new REPL) rep input))

;;; Read--eval--print--loop function.
(: repl (-> Void))
(define (repl)
  (send (new REPL) start))

(provide
  REPL
  rep
  repl)
