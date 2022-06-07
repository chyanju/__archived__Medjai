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

(define prog (program:load-program (hash-ref args 'program)))
; (define initmem (memory:make-memory))
; (define rn (runner:make-runner #:prog prog #:mem initmem))
; (runner:initialize-segments rn)
; (define end (runner:initialize-main-entrypoint rn))
; (define program-input (let ([pi (hash-ref args 'program-input)])
;     (if (null? pi) (make-hash) pi)
; ))
; (runner:initialize-vm rn (make-hash (list (cons 'program-input program-input))))