#lang typed/racket/base

(require racket/match
         racket/function
         racket/path
         racket/runtime-path
         racket/set
         threading

         "../private/interop.rkt")

(provide output-directory
         logging?
         current-source-file
         main-source-file
         FFI-CALL-ID
         test-environment?

         racketscript-dir
         racketscript-compiler-dir
         racketscript-runtime-dir
         cache-directory

         jsruntime-module-path
         jsruntime-core-module

         primitive-modules
         get-primitive-module
         ignored-module-imports-in-boot
         ignored-undefined-identifier?

         skip-arity-checks?)

;;; ---------------------------------------------------------------------------
(define FFI-CALL-ID '#%js-ffi)

(: output-directory (Parameter Path-String))
(define output-directory  (make-parameter "js-build"))

(: current-source-file (Parameter (Option (U Path Symbol))))
(define current-source-file (make-parameter #f))

(: main-source-file (Parameter (Option Path)))
(define main-source-file (make-parameter #f))

(: cache-directory (-> Path))
(define (cache-directory)
  (let ([dir (build-path (output-directory) "cache")])
    (if (directory-exists? dir)
        dir
        (begin (make-directory dir)
               dir))))

;; Path to the main compiler module
(define-runtime-path racketscript-main-module "main.rkt")

(: racketscript-compiler-dir Path)
(define racketscript-compiler-dir
  (cast (path-only racketscript-main-module) Path))

(: racketscript-runtime-dir Path)
(define racketscript-runtime-dir
  (build-path racketscript-compiler-dir "runtime"))

(: racketscript-dir Path)
;; Root directory of Racketscript project
(define racketscript-dir
  (~> racketscript-compiler-dir
      (build-path _ "..")
      (simplify-path _)))

;;; ---------------------------------------------------------------------------

(: jsruntime-core-module (Parameter String))
;; Name of kernel/core module object used in JavaScript environment
(define jsruntime-core-module (make-parameter "$rjs_core"))

(: jsruntime-module-path (-> Symbol Path))
(define (jsruntime-module-path mod)
  (let ([mod-name (match mod
                    ['core "core.js"]
                    [_ #:when (set-member? primitive-modules mod)
                       (let ([prim-mod (get-primitive-module mod)])
                         (string-append
                          (substring (symbol->string prim-mod) 2)
                          ".rkt.js"))])])
    (path->complete-path
     (build-path (output-directory)
                 "runtime"
                 mod-name))))

;;; ---------------------------------------------------------------------------

(: test-environment? (Parameter Boolean))
(define test-environment? (make-parameter #f))

(: logging? (Parameter Boolean))
(define logging? (make-parameter #t))

(: ignored-module-imports-in-boot (Setof Path))
;; Ignore these imports in primtive modules.
(define ignored-module-imports-in-boot
  (set
   (build-path racketscript-dir "private" "interop.rkt")))

(: ignored-undefined-identifiers (Listof Identifier))
(define ignored-undefined-identifiers
  (list #'#%js-ffi))

(: ignored-undefined-identifier? (-> Identifier Boolean))
(define (ignored-undefined-identifier? id)
  (ormap (curry free-identifier=? id) ignored-undefined-identifiers))

(: primitive-modules (Setof (U Symbol Path)))
(define primitive-modules
  (set '#%kernel
       '#%runtime
       '#%core
       '#%main
       '#%utils
       '#%paramz
       '#%unsafe
       '#%utils
       '#%flfxnum
       '#%futures
       '#%extfl
       '#%place-struct
       '#%network
       '#%builtin
       '#%boot
       '#%foreign
       '#%place
       (build-path racketscript-runtime-dir "lib.rkt")))

(: primitive-module-reexported-by (HashTable Symbol Symbol))
;; These modules (key) can essentially be treated as aliases, as
;; they simply re-export the provided symbols of another module (value).
(define primitive-module-reexported-by
  (hash
   ;; The '#%kernel module combines '#%core, '#%runtime, and '#%main
   ;; See: racket's boot/kernel.rkt
   ;; TODO: We should consider splitting kernel.rkt up to mirror racket's codebase.
   '#%runtime '#%kernel
   '#%main '#%kernel
   '#%core '#%kernel))

(: get-primitive-module (-> Symbol Symbol))
(define (get-primitive-module symb)
  (hash-ref primitive-module-reexported-by symb (λ () symb)))


;;; ---------------------------------------------------------------------------

(: skip-arity-checks? (Parameter Boolean))
(define skip-arity-checks? (make-parameter #f))
