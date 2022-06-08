#lang rosette
; a simplified version of cairo_run.py
(require
    (prefix-in tokamak: "./papyrus/tokamak.rkt")
    (prefix-in program: "./papyrus/program.rkt")
    (prefix-in memory: "./papyrus/memory.rkt")
    (prefix-in runner: "./papyrus/runner.rkt")
)

(define args (make-hash (list
    (cons 'program "./examples/test_compiled.json")
    (cons 'program-input null)
)))

(define program (program:load-program (hash-ref args 'program)))
(define initial-memory (memory:make-memory
    #:prime (program:program-prime program)))
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