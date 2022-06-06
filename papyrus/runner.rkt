#lang rosette
; this module stores all runner related components, including:
;   |- CairoRunner
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in memory: "./memory.rkt")
    (prefix-in config: "./config.rkt")
)

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
    finapc ; (final_pc) rv or null (init)
) #:mutable #:transparent #:reflection-name 'runner)

(define (make-runner #:prog prog #:mem mem)
    (tokamak:typed prog program:program?)
    (tokamak:typed mem memory:memory?)
    ; return
    (runner prog mem null null null null null null)
)

; (initialize_segments)
(define (init-segs p #:pbase [pbase null])
    (tokamak:typed p runner?)
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

; (initialize_main_entrypoint)
(define (init-mainent p)
    (tokamak:typed p runner?)
    ; (fixme) extremely simplified
    (define stack (list ))
    (let ([prog (runner-prog p)][mem (runner-mem p)])
        (define retfp (memory:add-segment mem))
        (define main (program:program-main prog))
        ; return
        (init-funent p main stack #:)
    )
)

; (initialize_function_entrypoint)
(define (init-funent p ent args #:retfp [retfp (memory:rv 0 0)])
    (tokamak:typed p runner?)
    (tokamak:typed ent string? integer?)
    (tokamak:typed args list?)
    (let ([mem (runner-mem p)])
        (define end (memory:add-segment mem))
        (define stack (append args (list retfp end)))
        (init-state p ent stack)
        (set-runner-initfp! p (+ (runner-ebase p) (length stack)))
        (set-runner-initap! p (+ (runner-ebase p) (length stack)))
        (set-runner-finalpc! p end)
        ; return
        end
    )
)

(define (init-state p ent stack)
    (tokamak:typed p runner?)
    (tokamak:typed ent string? integer?)
    (tokamak:typed stack list?)
    (set-runner-initpc! p (+ (runner-pbase p) (topc ent)))
    ; (fixme) load program
    ; (fixme) load data
)

(define (topc p lp)
    (tokamak:typed p runner?)
    (tokamak:typed lp string? integer?)
    ; return
    (if (string? lp)
        (program:get-label (runner-prog p) lp)
        lp
    )
)