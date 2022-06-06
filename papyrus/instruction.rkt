#lang rosette
; this module stores all instruction related components, including:
;   |- Instruction
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in config: "./config.rkt")
)

; global constants
(define OFFSET-BITS 16)
(define N-FLAGS 15)

; (Instruction)
(struct inst (
    off0 off1 off2 ; int
    imm ; int or null
    dst ; (dst_register) Register: 'ap, 'fp
    op0 ; (op0_register) Register: 'ap, 'fp
    op1 ; (op1_addr) Op1Addr: 'imm, 'ap, 'fp, 'op0
    res ; Res: 'op1, 'add, 'mul, 'unconstrained
    pc  ; (pc_update) PcUpdate: 'regular, 'jump, 'jump-rel, 'jnz
    ap  ; (ap_update) ApUpdate: 'regular, 'add, 'add1, 'add2
    fp  ; (fp_update) FpUpdate: 'regular, 'ap+2, 'dst
    opcode ; Opcode: 'nop, 'assert-eq, 'call, 'ret
) #:mutable #:transparent #:reflection-name 'inst)

(define (get-size p)
    (tokamak:typed p inst?)
    (if (not (null? (inst-imm p))) 2 1)
)

; (decode_instruction_values)
(define (decode-instvals enc)
    (tokamak:typed enc bv?)
    (assert (bvsle config:bvzero enc))
    (assert (bvslt
        enc
        (bv (expt 2 (+ N-FLAGS (* 3 OFFSET-BITS))) config:bvsize)
    ))
    (define off0 (bvand
        enc
        (bv (- (expt 2 OFFSET-BITS) 1) config:bvsize)
    ))
    (define off1 (bvand
        (bvashr enc (bv OFFSET-BITS config:bvsize))
        (bv (- (expt 2 OFFSET-BITS) 1) config:bvsize)
    ))
    (define off2 (bvand
        (bvashr enc (bv (* 2 OFFSET-BITS) config:bvsize))
        (bv (- (expt 2 OFFSET-BITS) 1) config:bvsize)
    ))
    (define flags-val (bvashr enc (bv (* 3 OFFSET-BITS) config:bvsize)))
    ; return
    (values flags-val off0 off1 off2)
)