#lang rosette
; this module stores all vm related components, including:
;   |- VirtualMachine
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in comp: "./comp.rkt")
    (prefix-in inst: "./inst.rkt")
    (prefix-in enc: "./enc.rkt")
)

; ================================ ;
; ======== VirtualMachine ======== ;
; ================================ ;
; (VirtualMachine)
(struct vm (
    context ; (run_context) context
) #:mutable #:transparent #:reflection-name 'vm)

(define (decode-currinst p)
    (tokamak:typed p vm?)
    (let ([context (vm-context p)])
        (define-values (instenc imm) (comp:get-instenc context))
        (define inst (enc:decode-inst instenc imm))
        ; return
        inst
    )
)