#lang rosette
; this module stores all program related components, including:
;   |- Program
(require json
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in cfg: "./config.rkt")
)
(provide (all-defined-out))

; ========================= ;
; ======== Program ======== ;
; ========================= ;
; (Program)
(struct program (
    prime ; int
    data ; list of int
    hints ; (fixme) null
    builtins ; list of string
    mscope ; (fixme) (mainscope) null
    ids ; (fixme) (identifiers) null
    refmgr ; (fixme) (reference_manager) null
    attrs ; (fixme) (attributes) null
    dbg ; (fixme) (debug_info) null
    ; from ProgramBase
    main ; (fixme) null
) #:mutable #:transparent #:reflection-name 'program)

(define (load-program jspath)
    (tokamak:typed jspath string?)
    (define js0 (string->jsexpr (file->string jspath)))
    ; return
    (program
        (hash-ref js0 'prime)
        (hash-ref js0 'data)
        (hash-ref js0 'hints)
        (hash-ref js0 'builtins)
        (hash-ref js0 'main_scope)
        (hash-ref js0 'identifiers)
        (hash-ref js0 'reference_manager)
        (hash-ref js0 'attributes)
        (hash-ref js0 'debug_info)
        0
    )
)
