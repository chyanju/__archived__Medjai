#lang rosette
; a simplified version of cairo_run.py
(require racket/cmdline
    (prefix-in tokamak: "./papyrus/tokamak.rkt")
    (prefix-in program: "./papyrus/program.rkt")
    (prefix-in memory: "./papyrus/memory.rkt")
    (prefix-in runner: "./papyrus/runner.rkt")
)

; parse command line arguments
(define arg-cname null)
(command-line
  #:program "cairo-run.rkt"
  #:once-any
  [("--cname") p-cname "path to a compiled Cairo program (.json)"
    (begin
      (set! arg-cname p-cname)
    )
  ]
)
(when (null? arg-cname) (tokamak:error "cname should not be null."))

(define args (make-hash (list
    (cons 'program arg-cname)
    (cons 'program-input null)
)))

(define program (program:load-program (hash-ref args 'program)))
(define initial-memory (memory:make-memory
    #:prime (program:program-prime program)))
(tokamak:log "initial memory data is: ~a" (memory:memory-data initial-memory))

(define runner (runner:make-runner
    #:prog program #:mem initial-memory))
(runner:initialize-segments runner)
(define end (runner:initialize-main-entrypoint runner))
(tokamak:log "end is: ~a" end)
(define program-input (let ([pi (hash-ref args 'program-input)])
    (if (null? pi) (make-hash) pi)
))
(runner:initialize-vm
    runner
    (make-hash (list (cons 'program-input program-input)))
)
(runner:run-until-pc runner end)

(tokamak:log "final memory data is: ~a" (memory:memory-data initial-memory))