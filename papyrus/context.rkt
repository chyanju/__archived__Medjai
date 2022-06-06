#lang rosette
; this module stores all context related components, including:
;   |- RunContext
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in config: "./config.rkt")
)
(provide (all-defined-out))

; ============================ ;
; ======== RunContext ======== ;
; ============================ ;
; (RunContext)
(struct context (
    mem ; (memory) MemoryDict
    pc ap fp ; MaybeRelocatable
    prime ; int
) #:mutable #:transparent #:reflection-name 'context)

; (get_instruction_encoding)
(define (get-instenc p)
    (tokamak:typed p context?)
    (let ([mem (context-mem p)] [pc (context-pc p)] [prime (context-prime p)])
        (define instenc (memory-ref mem pc))
        (tokamak:typed instenc bv?)
        (define imm-addr (modulo (+ 1 pc) prime))
        (define optional-imm (let ([i0 (memory-ref mem imm-addr])
            (if (not (bv? i0)) null i0)
        ))
        ; return
        (values instenc optional-imm)
    )
)