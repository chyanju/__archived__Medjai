#lang rosette
; this module stores all context related components, including:
;   |- RunContext
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in memory: "./memory.rkt")
)
(provide (all-defined-out))

; ============================ ;
; ======== RunContext ======== ;
; ============================ ;
; (RunContext)
(struct context (
    mem ; (memory) memory
    pc ap fp ; rv / int
    prime ; int
) #:mutable #:transparent #:reflection-name 'context)

; raw constructor
(define (new-context #:mem mem #:pc pc #:ap ap #:fp fp #:prime prime)
    ; return
    (context mem pc ap fp prime)
)
; constructor
(define (make-context #:mem mem #:pc pc #:ap ap #:fp fp #:prime prime)
    (tokamak:typed mem memory:memory?)
    (tokamak:typed pc memory:rv? integer?)
    (tokamak:typed ap memory:rv? integer?)
    (tokamak:typed fp memory:rv? integer?)
    (tokamak:typed prime integer?)
    ; return
    (new-context #:mem mem #:pc pc #:ap ap #:fp fp #:prime prime)
)

(define (get-instruction-encoding p)
    (tokamak:typed p context?)
    (let ([mem (context-mem p)][pc (context-pc p)][prime (context-prime p)])
        (define instruction-encoding (memory:memory-ref mem pc))
        (assert (integer? instruction-encoding)
            (format "instruction should be an int, got: ~a." instruction-encoding))
        (define imm-addr (memory:rvmod (memory:rvadd pc 1) prime))
        (define optional-imm (let ([imm0 (memory:memory-ref mem imm-addr)])
            (if (integer? imm0) imm0 null)
        ))
        (values instruction-encoding optional-imm)
    )
)