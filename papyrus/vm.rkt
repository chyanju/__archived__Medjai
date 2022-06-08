#lang rosette
; this module stores all vm related components, including:
;   |- VirtualMachine
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in context: "./context.rkt")
    (prefix-in memory: "./memory.rkt")
    (prefix-in program: "./program.rkt")
    (prefix-in encode: "./encode.rkt")
    (prefix-in instruction: "./instruction.rkt")
)
(provide (all-defined-out))

; ================================ ;
; ======== VirtualMachine ======== ;
; ================================ ;
; (VirtualMachineBase) -> (VirtualMachine)
(struct vm (

    ; VirtualMachineBase
    prime ; int
    brunners ; (fixme) (builtin_runners) dict[str,BuiltinRunner]
    excopes ; (exec_scopes) list[dict]
    hints ; (fixme) dict[rv,list[CompiledHint]]
    hpi ; (fixme) (hint_pc_and_index) dict[int,tuple[rv,int]]
    idi ; (fixme) (instruction_debug_info) dict[rv,InstructionLocation]
    dfc ; (debug_file_contents) dict[str,str]
    ema ; (fixme) (error_message_attributes) list[VmAttributeScope]
    prog ; (program) program
    mem ; (validated_memory) memory
    autodd ; (fixme) (auto_deduction) dict[int,list[tuple[Rule,tuple]]]
    slocals ; (static_locals) dict[str,Any]

    ; VirtualMachine
    cntx ; (run_context) context
    acaddrs ; (fixme) (accessed_addresses) set[rv]
    trace ; (fixme) list[TraceEntry[rv]]
    currstep ; (current_step) int
    skipiexec ; (skip_instruction_execution) bool

) #:mutable #:transparent #:reflection-name 'vm)

; raw constructor
(define (new-vm
    ; VirtualMachineBase
    #:prime prime #:brunners brunners #:excopes excopes #:hints hints
    #:hpi hpi #:idi idi #:dfc dfc #:ema ema
    #:prog prog #:mem mem #:autodd autodd #:slocals slocals
    ; VirtualMachine
    #:cntx cntx #:acaddrs acaddrs #:trace trace
    #:currstep currstep #:skipiexec skipiexec
    )
    ; return
    (vm prime brunners excopes hints hpi idi dfc ema prog mem autodd slocals
        cntx acaddrs trace currstep skipiexec)
)

; constructor
(define (make-vm #:prog prog #:cntx cntx #:hlocals hlocals #:slocals [slocals null]
                 #:brunners [brunners null] #:pbase [pbase null])
    (tokamak:typed prog program:program?)
    (tokamak:typed cntx context:context?)
    (tokamak:typed hlocals hash?)
    (tokamak:typed slocals hash? null?)
    (tokamak:typed brunners hash? null?)
    (tokamak:typed pbase memory:rv? integer? null?)
    (define cntx0 cntx)
    (define pbase0 (if (null? pbase) (context:context-pc cntx) pbase))
    (define brunners0 (if (null? brunners) (make-hash) brunners))
    ; base init starts ==>
    (define prime0 (program:program-prime prog))
    ; brunners is fine
    (define excopes0 null)
    ; (fixme) call enter_scope
    (define hints0 (make-hash))
    (define hpi0 (make-hash))
    (define idi0 (make-hash))
    (define dfc0 (make-hash))
    (define ema0 null)
    (define prog0 prog)
    (define mem0 (context:context-mem cntx))
    ; (fixme) tell apart StrippedProgram and Program
    (when (program:program? prog0) (load-program prime0 prog0 pbase0))
    (define autodd0 (make-hash))
    (define slocals0 (if (! (null? slocals))
        slocals
        (make-hash (list
            (cons 'PRIME prime0)
            (cons 'fadd (lambda (a b p) (modulo (+ a b) p)))
            ; (fixme) add the remainings
        ))
    ))
    ; <== base init ends
    (define acaddrs0 null)
    (define trace0 null)
    (define currstep0 0)
    (define skipiexec0 #f)
    ; return
    (new-vm
        #:prime prime0 #:brunners brunners0 #:excopes excopes0 #:hints hints0 #:hpi hpi0
        #:idi idi0 #:dfc dfc0 #:ema ema0 #:prog prog0 #:mem mem0 #:autodd autodd0 #:slocals slocals0
        #:cntx cntx0 #:acaddrs acaddrs0 #:trace trace0 #:currstep currstep0 #:skipiexec skipiexec0
    )
)

; (VirtualMachineBase.load_program)
; yeah the vm also has a load_program method
(define (load-program self.prime program program-base)
    (tokamak:typed self.prime integer?)
    (tokamak:typed program program:program?)
    (tokamak:typed program-base memory:rv? integer?)
    (assert (equal? self.prime (program:program-prime program))
        (format "unexpected prime for loaded program: ~a != ~a."
        (program:program-prime program) self.prime))
    ; (fixme) skipped a few
)

(define (step p)
    (tokamak:typed p vm?)
    (tokamak:log "vm step")
    ; (fixme) hint execution is skipped
    (define instruction (decode-current-instruction p))
    (run-instruction p instruction)
    (tokamak:log "instruction is: ~a" instruction)
)

(define (decode-current-instruction p)
    (tokamak:typed p vm?)
    (let ([cntx (vm-cntx p)])
        (define-values (instruction-encoding imm)
            (context:get-instruction-encoding cntx))
        (define instruction (decode-instruction p instruction-encoding #:imm imm))
        ; return
        instruction
    )
)

(define (decode-instruction p encoded-inst #:imm [imm null])
    (tokamak:typed p vm?)
    (tokamak:typed encoded-inst integer?)
    (tokamak:typed imm integer? null?)
    (encode:decode-instruction encoded-inst #:imm imm)
)

(define (run-instruction p instruction)
    (tokamak:typed p vm?)
    (tokamak:typed instruction instruction:instruction?)

)

(define (compute-operands p instruction)
    (tokamak:typed p vm?)
    (tokamak:typed instruction instruction:instruction?)

)