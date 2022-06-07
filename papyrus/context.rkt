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