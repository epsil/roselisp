;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Environment
;;;
;;; Environment class.
;;;
;;; ## Description
;;;
;;; This file defines the `Environment` class used for implementing
;;; the language environment.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "./lookup"
                  lookup-js-value))
(require (only-in "./thunk"
                  force
                  Thunk
                  thunk?))

;;; Environment.
;;;
;;; In its simplest form, an environment is a mapping from keys to
;;; values, with a pointer to an (optional) parent environment.
;;; Subclasses of this class, like {@link TypedEnvironment}, might add
;;; additional semantics, but the core idea is the same.
(define-class Environment ()
  ;;; The table of bindings.
  (define/public table (make-hash))

  ;;; The parent environment, if any.
  (define/public parent)

  ;;; Create a new environment.
  ;;;
  ;;; The environment may be initialized with `entries`,
  ;;; which is a list of `(key value)` tuples. The
  ;;; parent environment may be specified with `parent`.
  (define/public (constructor (entries '())
                              (parent undefined))
    (send this add-entries entries)
    (set-field! parent this parent))

  ;;; Add a list of entries `((key value) ...)` to the environment.
  (define/public (add-entries entries)
    (for ((entry entries))
      (send this set-entry entry))
    this)

  ;;; Clone the environment.
  ;;;
  ;;; Creates a new environment object with its own table.
  ;;; The `parent` pointer, however, points to the same
  ;;; environment as the original environment.
  (define/public (clone)
    (define env
      (new (ann (get-field constructor this)
                Any)))
    (set-field! table
                env
                (hash-copy
                 (get-field table this)))
    (set-field! parent
                env
                (get-field parent this))
    env)

  ;;; Clone the environment deeply.
  ;;;
  ;;; Like `.clone()`, but the `parent` environment is
  ;;; cloned too.
  (define/public (clone-deep)
    (define env
      (send this clone))
    (define parent
      (get-field parent env))
    (when parent
      (~> (send parent clone-deep)
          (set-field! parent env _)))
    env)

  ;;; Combine two environments.
  ;;;
  ;;; Returns a new environment.
  (define/public (combine env)
    ;; FIXME: Does not support combining an untyped environment
    ;; with a typed one.
    (define entries
      (append (send this entries)
              (send env entries)))
    (define result
      (new (ann (get-field constructor this) Any)
           entries
           (get-field parent this)))
    result)

  ;;; Combine one environment into another.
  ;;;
  ;;; Changes the current environment.
  (define/public (combine-into env)
    (send this
          add-entries
          (send env entries))
    this)

  ;;; Return a list of table entries.
  ;;; Each entry is a tuple `(key value)`.
  (define/public (entries)
    (hash-entries (get-field table this)))

  ;;; Find an environment frame binding `key`.
  ;;; Returns `not-found` if not found.
  (define/public (find-frame key (options (js-obj)))
    (define not-found
      (oget options "notFound"))
    (define filter
      (oget options "filter"))
    (define env this)
    (while (and env
                (or (and filter
                         (not (filter env)))
                    (not (send env has-local key))))
      (set! env
            (get-field parent env)))
    (if env
        env
        not-found))

  ;;; Delete the binding for `key`, if any.
  (define/public (delete key)
    (define env
      (send this find-frame key))
    (when env
      (send env delete-local key))
    this)

  ;;; Delete the local binding for `key`, if any.
  (define/public (delete-local key)
    (hash-remove! (get-field table this) key)
    this)

  ;;; Get the value of `key`, or `undefined`
  ;;; if there is no binding.
  (define/public (get key
                      (not-found undefined))
    (define-values (value found)
      (send this get-tuple key))
    (if found
        value
        not-found))

  ;;; Get the entry of `key`, which is a list `(key binding)`.
  ;;; If there is no binding, `not-found` is returned.
  (define/public (get-entry key
                            (not-found undefined))
    (define-values (value found)
      (send this get-tuple key))
    (if found
        (list key value)
        not-found))

  ;;; Get the environment frames of an environment.
  ;;;
  ;;; Returns a list of `Environment` objects that are accessible by
  ;;; following the `parent` pointer. The list also includes the
  ;;; current environment, which is the first element in the list.
  (define/public (get-frames (options (js-obj)))
    (define offset
      (or (oget options "offset") 0))
    (define frames '())
    (define env this)
    (while (and env
                (not (memq? env frames)))
      (push-right! frames env)
      (set! env (get-field parent env)))
    (drop frames offset))

  ;;; Get the binding for `key` as a tuple `(value found)`.
  (define/public (get-tuple key)
    (define env
      (send this find-frame key))
    (if env
        (send env get-local-tuple key)
        (values undefined #f)))

  ;;; Get the binding defined by the current environment frame,
  ;;; if any.
  (define/public (get-local key (not-found undefined))
    (define-values (value found)
      (send this get-local-tuple key))
    (if found
        value
        not-found))

  ;;; Get the binding defined by the current environment frame,
  ;;; if any, as a tuple `(binding found)`.
  (define/public (get-local-tuple key)
    (define found
      (send this has-local key))
    (define value
      (if found
          (hash-ref (get-field table this) key)
          undefined))
    (values value found))

  ;;; Get the parent environment, if any.
  (define/public (get-parent)
    (get-field parent this))

  ;;; Get the value of `key`, or `not-found`
  ;;; if there is no binding.
  (define/public (get-value key
                            (not-found undefined))
    ;; Alias for `.get`.
    (send this get key not-found))

  ;;; Whether `key` is bound in the environment,
  ;;; or in a parent environment.
  (define/public (has key)
    (define env
      (send this find-frame key))
    (if env
        #t
        #f))

  ;;; Whether `key` is bound in the current environment frame.
  (define/public (has-local key)
    (~> this
        (get-field table _)
        (hash-has-key? _ key)))

  ;;; Map a function over the environment.
  (define/public (map f)
    (new (ann (get-field constructor this) Any)
         (send (send this entries) map f)
         (get-field parent this)))

  ;;; Set `key` to `value` in the environment.
  (define/public (set key value)
    (define env
      (send this
            find-frame
            key
            (js-obj "notFound" this)))
    (send env set-local key value))

  ;;; Add an entry to the current environment frame.
  (define/public (set-entry entry)
    (define-values (key binding)
      entry)
    (send this set-local key binding))

  ;;; Set `key` to `value` in the current environment frame.
  (define/public (set-local key value)
    (define table
      (get-field table this))
    (hash-set! table key value)
    this)

  ;;; Set the parent environment.
  (define/public (set-parent parent)
    (set-field! parent this parent)
    this)

  ;;; Set `key` to `value` in the environment.
  (define/public (set-value key value)
    ;; Alias for `.set`.
    (send this set key value)))

;;; Typed environment.
;;;
;;; Each value is tagged with a "type" like `'variable'`
;;; or `'macro'`.
(define-class TypedEnvironment (Environment)
  ;;; Get the binding defined by the current environment frame,
  ;;; if any.
  (define/public (get key (not-found undefined))
    (send this get-untyped-value key not-found))

  ;;; Get the local binding defined by the current environment frame,
  ;;; if any.
  (define/public (get-local key (not-found undefined))
    (send this get-untyped-local-value key not-found))

  ;;; Get the type of `key`. If there is no binding,
  ;;; return `"undefined"`.
  (define/public (get-type key)
    (define-values (_ typ)
      (send this get-typed-value key))
    typ)

  ;;; Get the typed value of `key`, which is a tuple
  ;;; `(value type)`. If there is no binding, return
  ;;; `not-found`.
  (define/public (get-typed-value key
                                  (not-found
                                   (list undefined "undefined")))
    ;; The same as `super.get`, except that
    ;; `not-found` defaults to `(undefined "undefined")`.
    (send super get key not-found))

  (define/public (get-typed-local-value key
                                        (not-found
                                         (list undefined "undefined")))
    ;; The same as `super.get-local`, except that
    ;; `not-found` defaults to `(undefined "undefined")`.
    (send super get-local key not-found))

  ;;; Get the untyped value of `key`. If there is no binding,
  ;;; return `not-found`.
  (define/public (get-untyped-value key
                                    (not-found undefined))
    (define-values (value typ)
      (send this get-typed-value key))
    (if (eq? typ "undefined")
        not-found
        value))

  ;;; Get the untyped local value of `key`. If there is no binding,
  ;;; return `not-found`.
  (define/public (get-untyped-local-value key
                                          (not-found undefined))
    (define-values (value typ)
      (send this get-typed-local-value key))
    (if (eq? typ "undefined")
        not-found
        value))

  ;;; Set `key` to `value` with type `type` in the environment.
  (define/public (set key value (type "variable"))
    ;; Alias for `.set-typed-value`.
    (send this set-typed-value key value type))

  ;;; Add an entry `(key value type)` or `(key (value type))`
  ;;; to the environment.
  (define/public (set-entry entry)
    (cond
     ((= (array-list-length entry) 3)
      (define-values (key value type)
        entry)
      (send this set-local key value type))
     (else
      (define-values (key binding)
        entry)
      (define-values (value type)
        binding)
      (send this set-local key value type)))
    this)

  ;;; Set `key` to `value` with type `type` in
  ;;; the current environment frame.
  (define/public (set-local key value (type "variable"))
    (send super set-local key (list value type)))

  ;;; Set `key` to `value` with type `type` in
  ;;; the current environment frame.
  (define/public (set-typed-value key value (type "variable"))
    (define env
      (send this
            find-frame
            key
            (js-obj "notFound" this)))
    (cond
     ((is-a? env TypedEnvironment)
      (send env set-local key value type))
     (else
      (send key set-local value))))

  ;;; Set `key` to `value` with type `type` in
  ;;; the current environment frame.
  (define/public (set-value key value (type "variable"))
    ;; Alias for `.set`.
    (send this set key value type)))

;;; Thunked environment.
;;;
;;; A typed environment storing thunks that are forced upon request.
(define-class ThunkedEnvironment (TypedEnvironment)
  ;;; Get the binding defined by the current environment frame,
  ;;; if any, as a tuple `(binding found)`.
  (define/public (get-local-tuple key)
    (define tuple
      (send this get-unforced-local-tuple key))
    (define-values (binding found)
      tuple)
    (when found
      (define-values (val typ)
        binding)
      (when (thunk? val)
        (set! val (force val))
        (send this set-local key val typ)
        (set! binding (list val typ))
        (set! tuple (list binding found))))
    tuple)

  (define/public (get-unforced-tuple key)
    (cond
     ((send this has-local key)
      (send this get-unforced-local-tuple key))
     (else
      (for ((frame (send this get-frames (js-obj "offset" 1))))
        (when (send frame has-local key)
          (return
           (if (is-a? frame ThunkedEnvironment)
               (send frame get-unforced-local-tuple key)
               (send frame get-local-tuple key)))))
      (values undefined #f))))

  (define/public (get-unforced-local-tuple key)
    (send super get-local-tuple key))

  ;;; Get the type of `key`. If there is no binding,
  ;;; return `"undefined"`.
  (define/public (get-type key)
    ;; Obtain the type without forcing the thunk.
    (define tuple
      (send this get-unforced-tuple key))
    (define-values (binding found)
      tuple)
    (cond
     (found
      (define-values (_ typ)
        binding)
      typ)
     (else
      "undefined"))))

;;; Lisp environment.
;;;
;;; A typed, thunked environment.
(define-class LispEnvironment (ThunkedEnvironment))

;;; Environment class for stacking environments.
;;;
;;; Each stacked environment is treated like a frame in the containing
;;; environment. The environment at the top of the stack---i.e., the
;;; first element of the underlying array---is tried first, with the
;;; other environments serving as parent environments.
(define-class EnvironmentStack (TypedEnvironment)
  ;;; Environment stack.
  ;;;
  ;;; `stack[0]` is the top of the stack, i.e., the environment
  ;;; that is tried first.
  (define/public stack '())

  ;;; Create an environment stack.
  ;;;
  ;;; `args` is a list of environments. `args[0]` becomes the top of
  ;;; the stack, i.e., the environment that is tried first.
  (define/public (constructor . args)
    (super)
    (set-field! stack this args))

  ;;; Clone the environment stack.
  (define/public (clone)
    (define env
      (send super clone))
    (set-field! stack
                env
                `(,@(get-field stack this)))
    env)

  ;;; Find an environment frame binding `key`.
  ;;; Returns `not-found` if not found.
  (define/public (find-frame key (options (js-obj)))
    (define not-found
      (oget options "notFound"))
    (define inherited-options
      (js-obj-append
       options
       (js-obj "notFound" #f)))
    (define env
      (send this
            find-local-frame
            key
            inherited-options))
    (unless env
      (define parent
        (get-field parent this))
      (when parent
        (set! env
              (send parent
                    find-frame
                    key
                    inherited-options))))
    (if env
        env
        not-found))

  ;;; Find a local environment frame binding `key`.
  ;;; Returns `not-found` if not found.
  (define/public (find-local-frame key (options (js-obj)))
    (define not-found
      (oget options "notFound"))
    (define filter
      (oget options "filter"))
    (define inherited-options
      (js-obj-append
       options
       (js-obj "notFound" #f)))
    (define result not-found)
    (define environments
      (get-field stack this))
    (for ((env environments))
      (define frame
        (send env find-frame key inherited-options))
      (when frame
        (set! result frame)
        (break)))
    result)

  ;;; Get all the environment frames of all the environments
  ;;; on the stack.
  (define/public (get-frames (options (js-obj)))
    (define frames '())
    (define offset
      (or (oget options "offset") 0))
    (define environments
      (get-field stack this))
    (define parent
      (get-field parent this))
    (when parent
      (push-right! environments parent))
    (for ((env environments))
      (for ((f (send env get-frames)))
        (unless (memq? f frames)
          (cond
           ((is-a? f EnvironmentStack)
            (set! frames
                  (append frames
                          (send f get-frames))))
           (else
            (push-right! frames f))))))
    (drop frames offset))

  ;;; Get the binding for `key` as a tuple `(value found)`.
  (define/public (get-tuple key)
    (define env
      (send this find-frame key))
    (if env
        (send env get-tuple key)
        (values undefined #f)))

  ;;; Get the local binding for `key` as a tuple `(value found)`.
  (define/public (get-local-tuple key)
    (define env
      (send this find-local-frame key))
    (if env
        (send env get-tuple key)
        (values undefined #f)))

  ;;; Whether the stack contains an environment that binds `key`.
  (define/public (has-local key)
    ;; This could have been implemented in terms of
    ;; `find-local-frame`, but the following is faster since it
    ;; doesn't concern itself with the finer details of which
    ;; particular frame of which particular environment contains the
    ;; binding.
    (define result #f)
    (for ((env (get-field stack this)))
      (when (send env has key)
        (set! result #t)
        (break)))
    result)

  ;;; Whether the environment stack contains a
  ;;; particular environment.
  (define/public (has-environment env)
    (when (eq? env this)
      (return #t))
    (for ((x (get-field stack this)))
      (cond
       ((eq? x env)
        (return #t))
       ((and (is-a? x EnvironmentStack)
             (send x has-environment env))
        (return #t))))
    (cond
     ((and (get-field parent this)
           (is-a? (get-field parent this)
                  EnvironmentStack))
      (send (get-field parent this)
            has-environment
            env))
     (else
      #f)))

  ;;; Map a function over the environment stack.
  (define/public (map f)
    (apply new
           EnvironmentStack
           (map (lambda (env)
                  (send env map f))
                (get-field stack this))))

  ;;; Set `key` to `value` in the first
  ;;; environment in the stack.
  (define/public (set-local key value (type "variable"))
    (define env
      (first (get-field stack this)))
    (when env
      (cond
       ((is-a? env TypedEnvironment)
        (send env set-local key value type))
       (else
        (send key set-local value))))
    this))

;;; Compose environments left-to-right.
(define-class EnvironmentPipe (TypedEnvironment)
  (define/public environments '())

  ;;; Create an environment pipe.
  (define/public (constructor . args)
    (super)
    (set-field! environments this args))

  ;;; Get the binding for `key` as a tuple `(value found)`.
  (define/public (get-tuple key)
    (send this get-local-tuple key))

  ;;; Get the local binding for `key` as a tuple `(value found)`.
  (define/public (get-local-tuple key)
    (define current-key key)
    (define last-key key)
    (define last-env)
    (define found #t)
    (for ((env (get-field environments this)))
      (cond
       ((send env has current-key)
        (set! last-env env)
        (set! last-key current-key)
        (set! current-key
              (send env get current-key)))
       (else
        (set! found #f)
        (break))))
    (cond
     (found
      (send last-env get-tuple last-key))
     ((get-field parent this)
      (~> this
          (get-field parent _)
          (send _ get-tuple current-key)))
     (else
      (values undefined #f))))

  (define/public (has key)
    (define-values (value found)
      (send this get-tuple key))
    found)

  (define/public (has-local key)
    (define-values (value found)
      (send this get-local-tuple key))
    found))

;;; Compose environments right-to-left.
(define-class EnvironmentComposition (EnvironmentPipe)
  ;;; Create an environment composition.
  (define/public (constructor . args)
    (apply super (reverse args))))

;;; Dynamic environment, retrieving values
;;; on the basis of some lookup function.
(define-class DynamicEnvironment (TypedEnvironment)
  ;;; Lookup function.
  (define/public lookup-f)

  ;;; Typing function.
  (define/public typing-f)

  ;;; Create a dynamic environment.
  (define/public (constructor lookup-f
                              (typing-f
                               (const "variable")))
    (super)
    (set-field! lookup-f this lookup-f)
    (set-field! typing-f this typing-f))

  ;;; Get the binding defined by the dynamic environment,
  ;;; if any, as a tuple `(binding found)`.
  (define/public (get-local-tuple key)
    (define-values (value found)
      ((get-field lookup-f this) key))
    (cond
     (found
      (values
       (list value
             ((get-field typing-f this) value))
       #t))
     (else
      (values undefined #f))))

  ;;; Whether `key` is bound by the dynamic environment.
  (define/public (has-local key)
    (define-values (_ found)
      (send this get-local-tuple key))
    found))

;;; JavaScript environment.
;;;
;;; A dynamic environment that looks up JavaScript values.
(define-class JavaScriptEnvironment (DynamicEnvironment)
  (define/public (constructor)
    (super lookup-js-value)))

;;; Pointer to the current environment.
;;; Used by {@link currentEnvironment}.
(define current-environment-pointer undefined)

;;; Return the current environment.
(define (current-environment_)
  current-environment-pointer)

;;; Return an empty environment.
(define (empty-environment)
  (new LispEnvironment))

;;; Return the default environment.
;;;
;;; The default environment is defined as follows: use
;;; the current environment if there is one, and if not,
;;; use the empty environment.
(define (default-environment)
  (or (current-environment_)
      (empty-environment)))

;;; Run `f` with `currentEnvironmentPointer` bound to `env`.
;;; This makes the current environment available through the
;;; function {@link currentEnvironment}. The original value
;;; of `currentEnvironmentPointer` is restored afterwards.
(define (with-environment env f)
  ;; TODO: It would be much faster to implement this as
  ;; a macro.
  (define result undefined)
  (define tmp current-environment-pointer)
  (try
    (set! current-environment-pointer env)
    (set! result (f))
    (finally
      (set! current-environment-pointer tmp)))
  result)

;;; Make an environment.
(define (make-environment (variables undefined)
                          (parent undefined)
                          (is-lisp-2 #f))
  (new LispEnvironment variables parent))

;;; Extend the environment `env` with `parent` as its parent
;;; environment. This means that if a variable, function or macro
;;; is not defined in `env`, it is looked up in `parent` instead.
;;;
;;; If `env` already has a parent environment, `parent` becomes the
;;; parent of that environment.  An environment is effectively a
;;; linked list of environment frames. This function simply creates
;;; a linked list containing of the frames of both environments,
;;; in such a way that no frame is listed more than once.
(define (extend-environment env parent)
  ;; Create an array where each environment frame is
  ;; listed only once. This prevents cycles when the array
  ;; is converted to a linked list, which is how we tie
  ;; the two environments together.
  (define frames '())
  (for ((frame (environment-frames env)))
    (unless (memq? frame frames)
      (push-right! frames frame)))
  (for ((frame (environment-frames parent)))
    (unless (memq? frame frames)
      (push-right! frames frame)))
  (link-environment-frames frames)
  env)

;;; Return an array of the frames in an environment.
(define (environment-frames env)
  (define frames
    (list env))
  (if (get-field parent env)
      (append frames
              (environment-frames
               (get-field parent env)))
      frames))

;;; Convert an array of environment frames to a linked list
;;; by modifying each frame's `parent` property to point to
;;; the next frame in the array.
(define (link-environment-frames frames)
  (define first-frame undefined)
  (for ((i (range (- (array-list-length frames) 1) -1 -1)))
    (define frame
      (aget frames i))
    (set-field! parent frame first-frame)
    (set! first-frame frame))
  first-frame)

;;; Prefix a set of bindings.
(define (prefix-bindings prefix bindings)
  (map (lambda (x)
         (define prefixed-sym
           (string->symbol
            (string-append
             prefix
             (symbol->string
              (first x)))))
         (append (list prefixed-sym)
                 (rest x)))
       bindings))

(provide
  (rename-out (current-environment_ current-environment))
  DynamicEnvironment
  Environment
  EnvironmentComposition
  EnvironmentPipe
  EnvironmentStack
  JavaScriptEnvironment
  LispEnvironment
  ThunkedEnvironment
  TypedEnvironment
  current-environment-pointer
  current-environment_
  default-environment
  empty-environment
  environment-frames
  extend-environment
  link-environment-frames
  make-environment
  prefix-bindings
  with-environment)
