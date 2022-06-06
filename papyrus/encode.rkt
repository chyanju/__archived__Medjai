#lang rosette
; this module simulates the functionalities as in encode.py
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in config: "./config.rkt")
)

; global constants
(define DST-REG-BIT 0)
(define OP0-REG-BIT 1)
(define OP1-IMM-BIT 2)
(define OP1-FP-BIT 3)
(define OP1-AP-BIT 4)
(define RES-ADD-BIT 5)
(define RES-MUL-BIT 6)
(define PC-JUMP-ABS-BIT 7)
(define PC-JUMP-REL-BIT 8)
(define PC-JNZ-BIT 9)
(define AP-ADD-BIT 10)
(define AP-ADD1-BIT 11)
(define OPCODE-CALL-BIT 12)
(define OPCODE-RET-BIT 13)
(define OPCODE-ASSERT-EQ-BIT 14)
; (define RESERVED-BIT 15)

; (decode_instruction)
(define (decode-inst enc imm)
    (tokamak:typed enc bv?)
    (tokamak:typed imm bv? null?)
    (let-values ([(flags off0-enc off1-enc off2-enc) (inst:decode-instvals enc)])
        (define dst-reg (if (bvand 1 (bvashr flags (bv DST-REG-BIT config:bvsize))) 'fp 'ap))
        (define op0-reg (if (bvand 1 (bvashr flags (bv OP0-REG-BIT config:bvsize))) 'fp 'ap))

        ; get op1
        (define op1-addr-pat (list
            (bvand 1 (bvashr flags (bv OP1-IMM-BIT config:bvsize)))
            (bvand 1 (bvashr flags (bv OP1-AP-BIT config:bvsize)))
            (bvand 1 (bvashr flags (bv OP1-FP-BIT config:bvsize)))
        ))
        (define op1-addr (cond
            [(equal? op1-addr-pat (list config:bvone config:bvzero config:bvzero)) 'imm]
            [(equal? op1-addr-pat (list config:bvzero config:bvone config:bvzero)) 'ap]
            [(equal? op1-addr-pat (list config:bvzero config:bvzero config:bvone)) 'fp]
            [(equal? op1-addr-pat (list config:bvzero config:bvzero config:bvzero)) 'op0]
            [else (tokamak:error "unrecognized op1-addr-pat.")]
        ))

        (define imm0 (cond
            [(equal? 'imm op1-addr)
                (assert (not (null? imm)) "op1-addr is Op1Addr.IMM, but no immediate given.")
                imm
            ]
            [else null]
        ))

        ; get pc_update
        (define pc-update-pat (list
            (bvand 1 (bvashr flags (bv PC-JUMP-ABS-BIT config:bvsize)))
            (bvand 1 (bvashr flags (bv PC-JUMP-REL-BIT config:bvsize)))
            (bvand 1 (bvashr flags (bv PC-JNZ-BIT config:bvsize)))
        ))
        (define pc-upate (cond
            [(equal? pc-update-pat (list config:bvone config:bvzero config:bvzero)) 'jump]
            [(equal? pc-update-pat (list config:bvzero config:bvone config:bvzero)) 'jump-rel]
            [(equal? pc-update-pat (list config:bvzero config:bvzero config:bvone)) 'jnz]
            [(equal? pc-update-pat (list config:bvzero config:bvzero config:bvzero)) 'regular]
            [else (tokamak:error "unrecognized pc-update-pat")]
        ))

        ; get res
        (define res-pat (list
            (bvand 1 (bvashr flags (bv RES-ADD-BIT config:bvsize)))
            (bvand 1 (bvashr flags (bv RES-MUL-BIT config:bvsize)))
        ))
        (define res (cond
            [(equal? res-pat (list config:bvone config:bvzero)) 'add]
            [(equal? res-pat (list config:bvzero config:bvone)) 'mul]
            [(equal? res-pat (list config:bvzero config:bvzero))
                (if (equal? 'jnz pc-update) 'unconstrained 'op1)
            ]
            [else (tokamak:error "unrecognized res-pat")]
        ))
        ; JNZ opcode means res must be UNCONSTRAINED
        (when (equal? 'jnz pc-update) (assert (equal? 'unconstrained res)))

        ; get opcode
        (define opcode-pat (list
            (bvand 1 (bvashr flags (bv OPCODE-CALL-BIT config:bvsize)))
            (bvand 1 (bvashr flags (bv OPCODE-RET-BIT config:bvsize)))
            (bvand 1 (bvashr flags (bv OPCODE-ASSERT-EQ-BIT config:bvsize)))
        ))
        (define opcode (cond
            [(equal? opcode-pat (list config:bvone config:bvzero config:bvzero)) 'call]
            [(equal? opcode-pat (list config:bvzero config:bvone config:bvzero)) 'ret]
            [(equal? opcode-pat (list config:bvzero config:bvzero config:bvone)) 'assert-eq]
            [(equal? opcode-pat (list config:bvzero config:bvzero config:bvzero)) 'nop]
            [else (tokamak:error "unrecognized opcode-pat")]
        ))

        ; get ap_update
        (define ap-update-pat (list
            (bvand 1 (bvashr flags (bv AP-ADD-BIT config:bvsize)))
            (bvand 1 (bvashr flags (bv AP-ADD1-BIT config:bvsize)))
        ))
        (define ap-update (cond
            [(equal? ap-update-pat (list config:bvone config:bvzero)) 'add]
            [(equal? ap-update-pat (list config:bvzero config:bvone)) 'add1]
            [(equal? ap-update-pat (list config:bvzero config:bvzero))
                (if (equal? 'call opcode) 'add2 'regular)
            ]
            [else (tokamak:error "unrecognized ap-update-pat")]
        ))
        ; CALL opcode means ap_update must be ADD2
        (when (equal? 'call opcode) (assert (equal? 'add2 ap-update)))

        ; get fp_update
        (define fp-update
            (if (equal? 'call opcode)
                'ap+2
                (if (equal? 'ret opcode)
                    'dst
                    'regular
                )
            )
        )

        ; return
        (inst:inst
            (bvsub off0-enc (bv (expt 2 (- inst:OFFSET-BITS 1)) config:bvsize)) ; off0
            (bvsub off1-enc (bv (expt 2 (- inst:OFFSET-BITS 1)) config:bvsize)) ; off1
            (bvsub off2-enc (bv (expt 2 (- inst:OFFSET-BITS 1)) config:bvsize)) ; off2
            imm0 ; imm
            dst-reg ; dst
            op0-reg ; op0
            op1-addr ; op1
            res ; res
            pc-upate ; pc
            ap-update ; ap
            fp-update ; fp
            opcode ; opcode
        )
    )
)