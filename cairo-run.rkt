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
)))

(define prog (program:load-program (hash-ref args 'program)))
(define initmem (memory:make-memory))
(define rn (runner:make-runner #:prog prog #:mem initmem))
(runner:initialize-segments rn)
(define end (runner:initialize-main-entrypoint rn))
; (fixme) runner