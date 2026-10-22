;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # TypeScript
;;;
;;; TypeScript constructs.
;;;
;;; ## Description
;;;
;;; Various TypeScript constructs. Note that these make a tangible
;;; difference only if the output language is TypeScript.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "./env"
                  current-compilation-options))

;;; Expand a `(ts/as ...)` expression.
(define-macro (ts/as_ exp typ)
  exp)

(provide
  ts/as_)
