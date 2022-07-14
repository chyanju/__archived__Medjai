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
    math/number-theory
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
    ; (fixme) TODO should use functions instead of hash for symexec
    (define hints0 (make-hash))
    (define hpi0 (make-hash))
    (define idi0 (make-hash))
    (define dfc0 (make-hash))
    (define ema0 null)
    (define prog0 prog)
    (define mem0 (context:context-mem cntx))
    ; (fixme) tell apart StrippedProgram and Program
    (when (program:program? prog0) (load-program prime0 prog0 pbase0))
    (define autodd0 (lambda (seg) empty))
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
    (tokamak:log "instruction is: ~a" instruction)
    (run-instruction p instruction)
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
    ; (fixme) will call compute-operands
    (define-values (operands operands-mem-addresses)
        (compute-operands p instruction))
    ; (fixme) skipped a lot here
    ; update registers
    (update-registers p instruction operands)
    (set-vm-currstep! p (+ 1 (vm-currstep p)))
)

(define (update-registers p instruction operands)
    (tokamak:typed p vm?)
    (tokamak:typed instruction instruction:instruction?)
    (tokamak:typed operands instruction:operands?)
    ; update fp
    (let ([fp (context:context-fp (vm-cntx p))]
          [fp-update (instruction:instruction-fp instruction)]
          [ap (context:context-ap (vm-cntx p))])
        (cond
            [(equal? 'ap+2 fp-update)
                (context:set-context-fp! (vm-cntx p)
                    (memory:rvadd ap 2))
            ]
            [(equal? 'dst fp-update)
                (context:set-context-fp! (vm-cntx p)
                    (instruction:operands-dst operands))
            ]
            [(! (equal? 'regular fp-update))
                (tokamak:error "invalid fp_update value")
            ]
        )
    )

    ; update ap
    (let ([ap (context:context-ap (vm-cntx p))]
          [ap-update (instruction:instruction-ap instruction)])
        (cond
            [(equal? 'add ap-update)
                (when (null? (instruction:operands-res operands))
                    (tokamak:error "Res.UNCONSTRAINED cannot be used with ApUpdate.ADD"))
                (tokamak:log "first rvmod")
                (context:set-context-ap! (vm-cntx p)
                    (memory:rvmod (memory:rvadd ap (instruction:operands-res operands)) (vm-prime p)))
            ]
            [(equal? 'add1 ap-update)
                (context:set-context-ap! (vm-cntx p)
                    (memory:rvadd ap 1))
            ]
            [(equal? 'add2 ap-update)
                (context:set-context-ap! (vm-cntx p)
                    (memory:rvadd ap 2))
            ]
            [(! (equal? 'regular ap-update))
                (tokamak:error "invalid ap_update value")
            ]
        )
    )
    (tokamak:log "second rvmod")
    (context:set-context-ap! (vm-cntx p)
        (memory:rvmod (context:context-ap (vm-cntx p)) (vm-prime p)))

    ; update pc
    (let ([pc (context:context-pc (vm-cntx p))]
          [pc-update (instruction:instruction-pc instruction)])
        (cond
            [(equal? 'regular pc-update)
                (context:set-context-pc! (vm-cntx p)
                    (memory:rvadd pc (instruction:instruction-size instruction)))
            ]
            [(equal? 'jump pc-update)
                (when (null? (instruction:operands-res operands))
                    (tokamak:error "Res.UNCONSTRAINED cannot be used with PcUpdate.JUMP"))
                (context:set-context-pc! (vm-cntx p)
                    (instruction:operands-res operands))
            ]
            [(equal? 'jump-rel pc-update)
                (when (null? (instruction:operands-res operands))
                    (tokamak:error "Res.UNCONSTRAINED cannot be used with PcUpdate.JUMP_REL"))
                (when (! (integer? (instruction:operands-res operands)))
                    (tokamak:error "PureValueError: jump rel"))
                (context:set-context-pc! (vm-cntx p)
                    (memory:rvadd pc (instruction:operands-res operands)))
            ]
            [(equal? 'jnz pc-update)
                (if (is-zero p (instruction:operands-dst operands))
                    (context:set-context-pc! (vm-cntx p)
                        (memory:rvadd pc (instruction:instruction-size instruction)))
                    (context:set-context-pc! (vm-cntx p)
                        (memory:rvadd pc (instruction:operands-op1 operands)))
                )
            ]
            [else (tokamak:error "invalid pc_update value")]
        )
    )
    (tokamak:log "third rvmod, context-pc: ~a" (context:context-pc (vm-cntx p)))
    (context:set-context-pc! (vm-cntx p)
        (memory:rvmod (context:context-pc (vm-cntx p)) (vm-prime p)))
)

(define (is-zero p value)
    (tokamak:log "is-zero value: ~a" value)
    (tokamak:typed p vm?)
    ; the method itself has implicit assertions of value
    ; return
    (cond
        [(integer? value) (equal? value 0)]
        [(and (memory:rv? value) (memory:rvge (memory:rv-off value) 0)) #f]
        [else (tokamak:error "PureValueError: jmp != 0")]
    )
)

(define (compute-res p instruction op0 op1)
    (tokamak:typed p vm?)
    (tokamak:typed instruction instruction:instruction?)
    (tokamak:typed op0 memory:rv? integer?)
    (tokamak:typed op1 memory:rv? integer?)
    (let ([res (instruction:instruction-res instruction)])
        (cond
            [(equal? 'op1 res) op1]
            [(equal? 'add res) (memory:rvmod (memory:rvadd op0 op1) (vm-prime p))]
            ; [(equal? 'add res) (modulo (+ op0 op1) (vm-prime p))]
            [(equal? 'mul res)
                ;(when (|| (memory:rv? op0) (memory:rv? op1))
                ;    (tokamak:error "PuerValueError: *"))
              (let ([op0 (if (memory:rv? op0) (memory:rv-off op0) op0)]
                    [op1 (if (memory:rv? op1) (memory:rv-off op1) op1)])
                (modulo (* op0 op1) (vm-prime p)))]
            [(equal? 'unconstrained res) #f]
            ; [(equal? 'unconstrained res) null]
            [else (tokamak:error "invalid res value")]
        )
    )
)

(define (add-auto-deduction-rule p segment rule)
  (tokamak:typed p vm?)
  ;; TODO typecheck segment
  ;; TODO typecheck rule

  (define old-autodd (vm-autodd p))
  (define (new-autodd seg)
    (if (equal? seg segment)
      (cons rule (old-autodd seg))
      (old-autodd seg)))
  
  (set-vm-autodd! p new-autodd))

(define (deduce_memory_cell p addr)
    (tokamak:typed p vm?)
    (tokamak:typed addr memory:rv?)

    (tokamak:log "deducing memory cell at: ~a." addr)

    (define seg (memory:rv-seg addr))
    (define rules ((vm-autodd p) seg))

    ;; TODO return null instead of false
    (ormap
      (lambda (rule)
        ;; rule should return #f if it does not match
        (rule vm addr))
      rules))

;; compute x / y % prime
(define (div-mod x y prime)
  (let ([yinv (modular-inverse y prime)])
    (modulo (* x yinv) prime)))

(define (deduce_op0 p instruction dst op1)
  (tokamak:log "deduce_op0 opcode: ~a, res: ~a"
               (instruction:instruction-opcode instruction)
               (instruction:instruction-res instruction))
  (let ([opcode (instruction:instruction-opcode instruction)]
        [instr-res (instruction:instruction-res instruction)]
        [prime (context:context-prime (vm-cntx p))])
  (tokamak:log "deduce_op0 condition: ~a"
               (and (equal? opcode 'assert-eq)
                    (equal? instr-res 'add)
                    dst
                    op1))
    (cond
      [(equal? opcode 'call)
        (list
          (memory:rvadd (context:context-pc (vm-cntx p))
                        (instruction:instruction-size instruction))
          #f)]
      [(and (equal? opcode 'assert-eq)
            (equal? instr-res 'add)
            dst
            op1)
        (list
          (memory:rvmod (memory:rvsub dst op1) prime)
          dst)]
      [(and (equal? opcode 'assert-eq)
            (equal? instr-res 'mul)
            (integer? dst)
            (integer? op1)
            (not (equal? 0 op1)))
        (list
          (div-mod dst op1 prime)
          dst)]
      [else '(#f #f)])))

(define (deduce_op1 p instruction dst op0)
  (let ([opcode (instruction:instruction-opcode instruction)]
        [instr-res (instruction:instruction-res instruction)]
        [prime (context:context-prime (vm-cntx p))])
    (cond
    ; TODO
      [(and (equal? opcode 'assert-eq)
            (equal? instr-res 'op1)
            dst)
        (list dst dst)]
      [(and (equal? opcode 'assert-eq)
            (equal? instr-res 'add)
            dst
            op0)
        (list
          (memory:rvmod (memory:rvsub dst op0) prime)
          dst)]
      [(and (equal? opcode 'assert-eq)
            (equal? instr-res 'mul)
            (integer? dst)
            (integer? op0)
            (not (equal? op0 0)))
        (list
          (div-mod dst op0 prime)
          dst)]
      [else '(#f #f)])))

(define (symint)
  (define-symbolic* x integer?)
  x)

(define (compute-operands p instruction)
    (tokamak:typed p vm?)
    (tokamak:typed instruction instruction:instruction?)

    (tokamak:log "calling compute-operands, memory data is: ~a." (memory:memory-data (vm-mem p)))

    (define dst-addr (context:compute-dst-addr (vm-cntx p) instruction))
    (define dst (memory:memory-ref (vm-mem p) dst-addr))
    (tokamak:log "dst-addr is: ~a." dst-addr)
    (tokamak:log "dst is: ~a." dst)

    (define op0-addr (context:compute-op0-addr (vm-cntx p) instruction))
    (tokamak:log "op0-addr is: ~a." op0-addr)
    (define op0
      (let ([t0 (memory:memory-ref (vm-mem p) op0-addr)])
        (tokamak:log "original op0 is: ~a." t0)
        t0))

    (define op1-addr (context:compute-op1-addr (vm-cntx p) instruction op0))
    (tokamak:log "op1-addr is: ~a." op1-addr)
    (define op1
      (let ([t0 (memory:memory-ref (vm-mem p) op1-addr)])
        (tokamak:log "original op1 is: ~a." t0)
        t0))

    (define res #f)

    (define should-update-dst (not dst))
    (define should-update-op0 (not op0))
    (define should-update-op1 (not op1))

    ;; TODO/fixme these are hard-coded values for hints
    (when (equal? (context:context-pc (vm-cntx p)) (memory:rv 0 87))
      (set! dst 1))
    (when (equal? (context:context-pc (vm-cntx p)) (memory:rv 0 95))
      (set! dst 0))
    (when (equal? (context:context-pc (vm-cntx p)) (memory:rv 0 128))
      (set! op0 0)
      (set! op1 0)
      (set! dst 0))
    (when (equal? (context:context-pc (vm-cntx p)) (memory:rv 0 129))
      (set! op0 0)
      (set! op1 0)
      (set! dst 0))

    (when (not op0)
      (let* ([op0+res (deduce_op0 p instruction dst op1)]
             [new-op0 (first op0+res)]
             [new-res (second op0+res)])
        (set! op0 new-op0)
        (when (not res) (set! res new-res))))
    (tokamak:log "op0 is: ~a." op0)

    (when (not op1)
      (let* ([op1+res (deduce_op1 p instruction dst op0)]
             [new-op1 (first op1+res)]
             [new-res (second op1+res)])
        (set! op1 new-op1)
        (when (not res) (set! res new-res))))
    (tokamak:log "op1 is: ~a." op1)

    ; (fixme) res may become not null when you call any deduce methods above
    (when (not res) (set! res (compute-res p instruction op0 op1)))
    (tokamak:log "res is: ~a." res)

    ; deduce dst
    (define dst0
      (if dst
        dst
        ; else, update
         (let ([opcode (instruction:instruction-opcode instruction)])
           (cond
             [(and (equal? 'assert-eq opcode) res) res]
             [(equal? 'call opcode) (context:context-fp (vm-cntx p))]
             [else dst]))))

    ; force pulling dst from memory for soundness
    (define dst1
      (if dst0
        dst0
        (memory:memory-ref (vm-mem p) dst-addr)))

    (when should-update-dst
        (memory:memory-set! (vm-mem p) dst-addr dst1))
    (when should-update-op0
        (memory:memory-set! (vm-mem p) op0-addr op0))
    (when should-update-op1
        (memory:memory-set! (vm-mem p) op1-addr op1))

    ; return
    (values
        (instruction:make-operands #:dst dst1 #:op0 op0 #:op1 op1 #:res res)
        (list dst-addr op0-addr op1-addr)
    )

)