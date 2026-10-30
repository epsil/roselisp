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

(require (only-in "child_process"
                  execSync))
(require (only-in "fs"
                  readFileSync
                  writeFileSync))
(require (only-in "os"
                  homedir))
(require (only-in "path"
                  join))
(require (only-in "process"
                  chdir
                  cwd
                  stdin
                  stdout))
(require readline "readline")
(require (only-in "./constants"
                  package-name
                  version))
(require (only-in "./core"
                  (interpret eval_)
                  (load_ load)
                  lang-environment
                  load-module
                  print-sexp
                  print-sexp-as-expression))
(require (only-in "./env"
                  LispEnvironment
                  current-environment
                  with-environment))
(require (only-in "./equal"
                  equal?_))
(require (only-in "./parser"
                  read))
(require (only-in "./util"
                  copy-into-array!))

;;; REPL class.
;;;
;;; Encapsulates a [`readline`][node:readline] instance that reads
;;; from standard input.
;;;
;;; [node:readline]: https://nodejs.org/api/readline.html
(define-class REPL ()
  ;;; Flag for printing values.
  (define/public print-flag #t)

  ;;; Flag for quitting.
  (define/public quit-flag #f)

  ;;; The REPL environment.
  (define/public env)

  ;;; The toplevel environment.
  (define/public toplevel)

  ;;; The current module, if any.
  (define/public current-module #u)

  ;;; REPL options.
  (define options
    (object))

  ;;; REPL prompt string.
  (define prompt-str "> ")

  ;;; REPL state.
  (define state "prompt")

  ;;; REPL user input.
  (define input "")

  ;;; REPL history.
  ;;;
  ;;; A list of strings, with the latest entry stored
  ;;; at the beginning of the list.
  (define history '())

  ;;; `readline` instance.
  (define rl #u)

  ;;; `readline` history.
  (define rl-history '())

  ;;; Absolute path to `~/.roselisp_history`, where
  ;;; persistent history is stored.
  (define history-file
    (join (homedir)
          (string-append "." package-name "_history")))

  ;;; Message displayed when starting the REPL.
  (define startup-message
    (string-append
     ";; Roselisp version " version ".\n"
     ";; Type ,h for help and ,q to quit."))

  ;; Whether to echo user input.
  (define echo #t)

  ;;; Create a new REPL.
  (define (constructor (options (object)))
    (set-field! options this (send this normalize-options options))
    (set-field! env this (make-interactive-environment this))
    (set-field! toplevel this (get-field env this)))

  ;;; Start the REPL.
  (define/public (start (options (get-field options this)))
    (set-field! echo this (not (object-ref options :sparse-repl)))
    (when (object-ref options :repl-history)
      (set-field! history this (send this read-history))
      (set-field! rl-history this (list-copy (get-field history this))))
    ;; Initialize the `readline` instance.
    (set-field! rl
                this
                (send readline
                      createInterface
                      (object
                       :input
                       (or (object-ref options :input)
                           stdin)
                       :history
                       (get-field rl-history this)
                       :output
                       (if (get-field echo this)
                           (or (object-ref options :output)
                               stdout)
                           #u))))
    ;; Register a listener for the `line` event that will
    ;; receive a single line of user input and take
    ;; appropriate action.
    (send (get-field rl this)
          on
          "line"
          (lambda (input)
            (with-repl
             this
             (thunk
              (with-environment
               (get-field env this)
               (thunk
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
                  (send this prompt))
                 (else
                  (when (and (get-field print-flag this)
                             (not (eq? result "")))
                    (display result))
                  (send this prompt)))))))))
    ;; Register a listener for the `close` event that will
    ;; terminate the output properly.
    (send (get-field rl this)
          on
          "close"
          (lambda ()
            (unless (get-field echo this)
              (send stdout write "\n"))))
    ;; Start the read--eval--print loop.
    (send this print-startup-message)
    (send this prompt))

  ;;; Read--eval--print method.
  (define/public (rep input (env (get-field env this)))
    ;; Ignore leading whitespace.
    (when (regexp-match (regexp "^\\s*$") input)
      (set-field! state this "read")
      (return ""))
    ;; Parse user input into expressions. Note the plural: we allow
    ;; for multiple expressions to be entered at a single prompt
    ;; (e.g., `> (+ 1 1) (+ 2 2)`), so that what is processed here
    ;; is not a single expression, but rather a list of expressions.
    (define parsed-expressions '())
    (try
      (set! parsed-expressions
            (~> input
                (string-append "(" _ ")")
                (read _)))
      (catch Error e
        (cond
         ;; If we are reading a multi-line expression,
         ;; then continue listening for user input.
         ((eq? (get-field message e) "eof")
          (set-field! state this "read")
          (return ""))
         (else
          (throw e)))))
    ;; Update the REPL history.
    (push-left! (get-field history this) input)
    ;; Synchronize the `readline` instance's history with the
    ;; REPL history. This is needed in order to handle multi-line
    ;; expressions correctly, because otherwise the `readline`
    ;; instance will create one history item per line.
    (copy-into-array! (get-field history this)
                      (get-field rl-history this))
    ;; Rewrite the expressions slightly in order to handle REPL
    ;; shortcuts such as `,h` and `,q`.
    (define rewritten-expressions
      (rewrite-expressions parsed-expressions))
    ;; Evaluate the expressions one by one, producing a list
    ;; of values.
    (define evaluated-expressions '())
    (for ((exp rewritten-expressions))
      (when (get-field quit-flag this)
        (break))
      (try
        (push-right! evaluated-expressions
                     (eval_ exp
                            env
                            (get-field options this)))
        (catch _ e
          (display e))))
    ;; Continue unless the user has quit. If the user has quit,
    ;; the return value is just the empty string.
    (define result "")
    (unless (get-field quit-flag this)
      (when (object-ref (get-field options this) :repl-history)
        (send this write-history-entry input))
      ;; Print the values, producing a list of value strings.
      (define printed-expressions '())
      (for ((x evaluated-expressions))
        (cond
         ;; Don't print `#u`. FIXME: A better way to determine this
         ;; would be to check if the original expression called a
         ;; function whose function type returns `Void`.
         ((undefined? x)
          (continue))
         (else
          (push-right! printed-expressions
                       (print-sexp-as-expression x)))))
      ;; Concatenate the value strings into a single string,
      ;; with each value string on its own line.
      (set! result
            (string-join printed-expressions "\n")))
    result)

  ;;; Normalize options.
  (define (normalize-options options)
    (define result
      (object-copy options))
    result)

  ;;; Print startup message.
  (define (print-startup-message)
    (display (get-field startup-message this)))

  ;;; Read a line of user input.
  (define (prompt)
    (define prompt-str
      (if (eq? (get-field state this) "read")
          ""
          (get-field prompt-str this)))
    (send (get-field rl this)
          setPrompt
          prompt-str)
    (unless (get-field echo this)
      (send stdout write prompt-str))
    (send (get-field rl this) prompt))

  ;;; Read persistent history.
  (define (read-history (str ""))
    (define history str)
    (try
      (set! history
            (readFileSync (get-field history-file this)
                          (object :encoding "utf8")))
      (catch))
    (string->history history))

  ;;; Write persistent history.
  (define (write-history (history (get-field history this)))
    (try
      (writeFileSync (get-field history-file this)
                     (history->string history)
                     (object :encoding "utf8"))
      (catch)))

  ;;; Append an entry to the persistent history.
  (define (write-history-entry entry)
    (try
      (writeFileSync (get-field history-file this)
                     (string-append entry "\n")
                     (object :encoding "utf8"
                             :flag "a"))
      (catch ))))

;;; List current bindings.
(define (binding) : Void
  (define env
    (get-field env (current-repl)))
  (for ((entry (send env entries)))
    (define sym
      (first entry))
    (define sym-printed
      (print-sexp sym))
    (define-values (val)
      (second entry))
    (define val-printed
      (print-sexp-as-expression val))
    (display sym-printed "\t\t" val-printed)))

;;; Change the current directory.
(define (cd dir) : Void
  (try
    (chdir dir)
    (pwd)
    (catch _ e
      (display e))))

;;; Edit a file.
;;;
;;; Uses the system's default editor as specified with the environment
;;; variable `$EDITOR`; if that is not defined, then the command
;;; `editor` is invoked instead.
(define (edit . args) : Void
  (define cmd
    (string-join (cons "${EDITOR:-editor}" args)
                 " "))
  (try
    (execSync cmd (object :shell "/bin/bash"
                          :stdio "inherit"))
    (catch _ e
      (display e))))

;;; Enter a module.
(define (enter! (x #u)) : Void
  (cond
   ((undefined? x)
    (define m
      (get-field current-module (current-repl)))
    (when m
      (enter! (join (get-field module-path m)
                    (get-field name m)))))
   ((not x)
    (toplevel))
   (else
    (define m
      (load-module x (current-environment)))
    (when m
      (set-field! env
                  (current-repl)
                  (send m get-export-environment))))))

;;; Display help message.
(define (help) : Void
  (set-field! print-flag (current-repl) #f)
  (display
   ";; Enter an S-expression to evaluate it.
;; Use the up and down keys to access previous expressions.
;;
;; Type ,q to quit."))

;;; List the contents of the current directory.
(define (ls . args) : Void
  (apply shell (cons "ls" args)))

;;; Print the current directory.
(define (pwd) : Void
  (display (cwd)))

;;; Execute a shell command.
(define (shell . args) : Void
  (define cmd
    (string-join args " "))
  (try
    (execSync cmd (object :stdio "inherit"))
    (catch _ e
      (display e))))

;;; Go back to the toplevel.
(define (toplevel) : Void
  (set-field! env
              (current-repl)
              (get-field toplevel (current-repl))))

;;; Quit the REPL.
(define (quit) : Void
  (set-field! print-flag (current-repl) #f)
  (set-field! quit-flag (current-repl) #t))

;;; Read--eval--print function.
(define (rep input)
  (send (new REPL) rep input))

;;; Read--eval--print--loop function.
(: repl (-> Void))
(define (repl (options (object)))
  (send (new REPL options) start))

;;; Rewrite `(unquote ...)` expressions to regular
;;; function calls.
(define (rewrite-expressions exp)
  (match exp
    ((list (list 'unquote x) y ...)
     ;; Aliases.
     (cond
      ((eq? x 'b)
       (set! x 'binding))
      ((memq? x '(e ed))
       (set! x 'edit))
      ((memq? x '(en enter))
       (set! x 'enter!))
      ((eq? x 'h)
       (set! x 'help))
      ((eq? x 'l)
       (set! x 'load))
      ((eq? x 'm)
       (set! x 'module))
      ((memq? x '(req r))
       (set! x 'require))
      ((memq? x '(ex x q))
       (set! x 'quit)))
     ;; Rewrite an expression like `,cd ..`
     ;; to `(cd "..")`.
     `((,x
        ,@(map (lambda (x)
                 (if (symbol? x)
                     (symbol->string x)
                     (js/to-string x)))
               y))))
    ((list)
     '((help)))
    (_
     exp)))

;;; Make an environment for the REPL.
(define (make-interactive-environment repl (bindings '()))
  (define parent lang-environment)
  (define env
    (new LispEnvironment
         `((binding ,binding '(-> Void))
           (bindings ,binding '(-> Void))
           (cd ,cd '(-> Void))
           (edit ,edit '(-> Any * Any))
           (enter! ,enter! (-> Any Void))
           (exit ,quit '(-> Any * Any))
           (help ,help (-> Void))
           (load ,load '(-> Any * Any))
           (ls ,ls '(-> Any * Any))
           (module ,enter! (-> Any Void))
           (pwd ,pwd '(-> Void))
           (quit ,quit (-> Void))
           (shell ,shell '(-> Void))
           (toplevel ,toplevel (-> Void))
           ,@bindings)
         parent))
  (define env1
    (new LispEnvironment
         '()
         env))
  env1)

;;; Pointer to the current REPL instance.
(define current-repl-pointer #u)

;;; Force the thunk `f` with `current-repl-pointer`
;;; bound to `repl`. The previous value of
;;; `current-repl-pointer` is restored afterwards.
(define (with-repl repl f)
  (define result #u)
  (define tmp current-repl-pointer)
  (try
    (set! current-repl-pointer repl)
    (set! result (force f))
    (finally
      (set! current-repl-pointer tmp)))
  result)

;;; Return the current REPL instance.
;;; Used by REPL meta-commands to
;;; communicate with the REPL.
(define (current-repl)
  current-repl-pointer)

;;; Convert a history object to a string.
;;; The reverse of `string->history`.
(define (history->string history)
  (~> history
      (reverse _)
      (string-join _ "\n")))

;;; Convert a string to a history object.
;;;
;;; The input to this function is a string of entered lines, separated
;;; by newlines, with the latest entry at the end of the string. The
;;; output is a list of strings, one per expression, in the opposite
;;; order (i.e., with the latest entry at the beginning of the list,
;;; which is what `readline` expects). Expressions spanning multiple
;;; lines are parsed as a single expression, so there might be fewer
;;; entries in the output than there are lines in the input.
(define (string->history str)
  ;; Note that since the format is so simple, this function has to be
  ;; fairly error-tolerant so it can deal gracefully with syntax
  ;; errors in the user input. With a more robust format for
  ;; persistent history (e.g., serializing the history as an
  ;; S-expression), this would not be needed at all; nevertheless, we
  ;; have opted to go with a simple but brittle format for the sake of
  ;; convention.
  (define result '())
  (define space-regexp
    (regexp "^\\s*$"))
  (define buffer "")
  (define exp #u)
  (define lines
    (string-split str "\n"))
  (for ((line lines))
    (set! buffer
          (if (eq? buffer "")
              line
              (string-append buffer "\n" line)))
    (try
      (read buffer)
      (push-left! result buffer)
      (set! buffer "")
      (catch Error e
        (unless (eq? (get-field message e) "eof")
          (unless (regexp-match space-regexp buffer)
            (push-left! result buffer))
          (set! buffer "")))))
  result)

(provide
  REPL
  history->string
  rep
  repl
  string->history)
