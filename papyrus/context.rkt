#lang rosette
; this module stores all context related components, including:
;   |- RunContext
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in memory: "./memory.rkt")
    (prefix-in instruction: "./instruction.rkt")
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

(define (compute-dst-addr p instruction)
    (tokamak:typed p context?)
    (tokamak:typed instruction instruction:instruction?)
    (define base-addr (let ([sym (instruction:instruction-dst instruction)])
        (cond
            [(equal? 'ap sym) (context-ap p)]
            [(equal? 'fp sym) (context-fp p)]
            [else (tokamak:error "invalid dst-register value.")]
        )
    ))
    ; return
    (memory:rvmod (memory:rvadd base-addr (instruction:instruction-off0 instruction)) (context-prime p))
)

(define (compute-op0-addr p instruction)
    (tokamak:typed p context?)
    (tokamak:typed instruction instruction:instruction?)
    (define base-addr (let ([sym (instruction:instruction-op0 instruction)])
        (cond
            [(equal? 'ap sym) (context-ap p)]
            [(equal? 'fp sym) (context-fp p)]
            [else (tokamak:error "invalid op0-register value.")]
        )
    ))
    ; return
    (memory:rvmod (memory:rvadd base-addr (instruction:instruction-off1 instruction)) (context-prime p))
)

(define (compute-op1-addr p instruction op0)
    (tokamak:typed p context?)
    (tokamak:typed instruction instruction:instruction?)
    (tokamak:typed op0 memory:rv? integer?)
    (define base-addr (let ([sym (instruction:instruction-op1 instruction)])
        (cond
            [(equal? 'ap sym) (context-ap p)]
            [(equal? 'fp sym) (context-fp p)]
            [(equal? 'imm sym)
                (assert (equal? 1 (instruction:instruction-off2 instruction)) "in immediate mode, off2 should be 1.")
                (context-pc p)
            ]
            [(equal? 'op0 sym)
                (assert (! (null? op0)) "op0 must be known in double dereference.")
                op0
            ]
            [else (tokamak:error "invalid op1-register value.")]
        )
    ))
    ; return
    (memory:rvmod (memory:rvadd base-addr (instruction:instruction-off2 instruction)) (context-prime p))
)