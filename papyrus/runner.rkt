#lang rosette
; this module stores all runner related components, including:
;   |- CairoRunner
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in memory: "./memory.rkt")
    (prefix-in config: "./config.rkt")
    (prefix-in program: "./program.rkt")
    (prefix-in context: "./context.rkt")
    (prefix-in vm: "./vm.rkt")
)
(provide (all-defined-out))

; ============================= ;
; ======== CairoRunner ======== ;
; ============================= ;
; (CairoRunner)
(struct runner (
    prog ; (program) program
    mem ; (memory) memory
    pbase ; (program_base) rv or null (init)
    ebase ; (execution_base) rv or null (init)
    initfp ; (initial_fp) rv or null (init)
    initap ; (initial_ap) rv or null (init)
    initpc ; (initial_pc) rv or null (init)
    finalpc ; (final_pc) rv or null (init)
    vm ; vm or null (init)
    brunners ; (fixme) (builtin_runners) null
) #:mutable #:transparent #:reflection-name 'runner)

(define (make-runner #:prog prog #:mem mem)
    (tokamak:typed prog program:program?)
    (tokamak:typed mem memory:memory?)
    ; return
    (runner prog mem null null null null null null null null)
)

(define (topc p lop)
    (tokamak:typed p runner?)
    (tokamak:typed lop string? integer?)
    ; return
    (if (string? lop)
        ; (fixme)
        ; (program:get-label (runner-prog p) lop)
        0
        lop
    )
)

(define (initialize-segments p #:pbase [pbase null])
    (tokamak:typed p runner?)
    (tokamak:typed pbase memory:rv? null?)
    (let ([mem (runner-mem p)])
        ; program segment
        (set-runner-pbase! p
            (if (null? pbase)
                (memory:add-segment mem)
                pbase
            )
        )
        ; execution segment
        (set-runner-ebase! p (memory:add-segment mem))
        ; builtin segment
        ; (fixme) add it later
    )
)

(define (initialize-main-entrypoint p)
    (tokamak:typed p runner?)
    ; (fixme) extremely simplified
    (define stack (list ))
    (let ([prog (runner-prog p)][mem (runner-mem p)])
        (define return-fp (memory:add-segment mem))
        (define main (program:program-main prog))
        ; return
        (initialize-function-entrypoint p main stack #:return-fp return-fp)
    )
)

(define (initialize-function-entrypoint p entrypoint args #:return-fp [return-fp (memory:rv 0 0)])
    (tokamak:typed p runner?)
    (tokamak:typed entrypoint string? integer?)
    (tokamak:typed args list?)
    (tokamak:typed return-fp memory:rv? integer?)
    (let ([mem (runner-mem p)])
        (define end (memory:add-segment mem))
        (define stack (append args (list return-fp end)))
        (initialize-state p entrypoint stack)
        (set-runner-initfp! p (memory:rvadd (runner-ebase p) (length stack)))
        (set-runner-initap! p (memory:rvadd (runner-ebase p) (length stack)))
        (set-runner-finalpc! p end)
        ; return
        end
    )
)

(define (initialize-state p entrypoint stack)
    (tokamak:typed p runner?)
    (tokamak:typed entrypoint string? integer?)
    (tokamak:typed stack list?)
    (set-runner-initpc! p (memory:rvadd (runner-pbase p) (topc p entrypoint)))
    ; (fixme) load program
    ; (fixme) load data
)

(define (initialize-vm p hint-locals #:static-locals [static-locals null])
    (tokamak:typed p runner?)
    (tokamak:typed hint-locals hash?)
    (tokamak:typed static-locals hash? null?)
    (define cntx (context:make-context
        #:mem (runner-mem p)
        #:pc (runner-initpc p)
        #:ap (runner-initap p)
        #:fp (runner-initfp p)
        #:prime (program:program-prime (runner-prog p))
    ))
    (define sl (if (null? static-locals) (make-hash) static-locals))
    (define vm (vm:make-vm
        #:prog (runner-prog p) #:cntx cntx #:hlocals hint-locals
        #:slocals sl #:brunners (runner-brunners p) #:pbase (runner-pbase p)
    ))
    (set-runner-vm! p vm)
    ; (fixme) skipped a few
)