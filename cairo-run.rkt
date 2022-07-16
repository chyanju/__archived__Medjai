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

;; TODO/fixme: hardcode the symbolic inputs
(define-symbolic* bal integer?)
(define-symbolic* amt integer?)
;(tokamak:log "assuming bal low >0")
;(assume (> (modulo bal (program:program-prime program)) 0))
;(tokamak:log "assuming bal low < 2**127")
;(assume (< (modulo bal (program:program-prime program)) 170141183460469231731687303715884105728))
;(tokamak:log "assuming amt low >0")
;(assume (> (modulo amt (program:program-prime program)) 0))
;(tokamak:log "assuming amt low < 2**127")
;(assume (< (modulo amt (program:program-prime program)) 170141183460469231731687303715884105728))
;(define amt 0)
;(define amt 3618502788666131213697322783095070105623107215331596699973092056135872020481)
;(tokamak:log "bal mod p: ~a" (modulo bal (program:program-prime program)))
;(tokamak:log "amt mod p: ~a" (modulo amt (program:program-prime program)))
(memory:memory-set!
  initial-memory
  (memory:rv 0 (+ 6 (program:program-main program)))
  (modulo bal (program:program-prime program)))
(memory:memory-set!
  initial-memory
  (memory:rv 0 (+ 10 (program:program-main program)))
  (modulo amt (program:program-prime program)))

(let ([mdl (verify (runner:run-until-pc runner end))])
  (tokamak:log "Model for verification below, (unsat) means \"no bugs found\"\n~a" mdl))

; (tokamak:log "final memory data is: ~a" (memory:memory-data initial-memory))

; Prints for checking correctness of erc20simple.cairo
;(define (val->str val)
;  (if (memory:rv? val)
;    (~a (memory:rv-seg val) ":" (memory:rv-off val))
;    (~a val)))
;
;(for ([i 60])
;  (let ([val (memory:data-ref (memory:memory-data initial-memory) (memory:rv 1 i))])
;    (displayln (~a "1 : " i " = " (val->str val)))))
;(for ([i 3])
;  (let ([val (memory:data-ref (memory:memory-data initial-memory) (memory:rv 2 i))])
;    (displayln (~a "2 : " i " = " (val->str val)))))