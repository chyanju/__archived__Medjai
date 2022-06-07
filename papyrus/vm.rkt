#lang rosette
; this module stores all vm related components, including:
;   |- VirtualMachine
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in context: "./context.rkt")
    (prefix-in memory: "./memory.rkt")
)
(provide (all-defined-out))

; ================================ ;
; ======== VirtualMachine ======== ;
; ================================ ;
; (VirtualMachine)
(struct vm (
    prog ; (program) program
    cntx ; (run_context) context
    hlocals ; (fixme) (hint_locals) null
    slocals ; (fixme) (static_locals) null
    brunners ; (fixme) (builtin_runners) null
    pbase ; (program_base) null
    ; from VirtualMachineBase
    mem ; (validated_memory) memory
    ; stateful vars
    currstep ; (current_step) int
) #:mutable #:transparent #:reflection-name 'vm)

; (fixme) dramatically simplified
(define (make-vm #:prog prog #:cntx cntx #:hlocals hlocals
                 #:slocals [slocals null] #:brunners [brunners null] #:pbase [pbase null])
    (define pbase0 (if (null? pbase) (context:context-pc cntx) pbase))
    (define brunners0 (if (null? brunners) (make-hash) brunners))
    (define mem (context:context-mem cntx))
    (define currstep 0)
    ; return
    (vm prog cntx hlocals slocals brunners0 pbase0 mem currstep)
)

(define (validate-existing-memory p)
    (tokamak:typed p vm?)
    (memory:validate-existing-memory (vm-mem p))
)

; (define (decode-currinst p)
;     (tokamak:typed p vm?)
;     (let ([context (vm-context p)])
;         (define-values (instenc imm) (comp:get-instenc context))
;         (define inst (enc:decode-inst instenc imm))
;         ; return
;         inst
;     )
; )