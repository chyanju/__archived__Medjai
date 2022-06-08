#lang rosette
; this module stores all memory related components, including:
;   |- MemoryDict
;   |- RelocatableValue
;   |- MemorySegmentManager
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in config: "./config.rkt")
)
(provide (all-defined-out))

; ================================== ;
; ======== RelocatableValue ======== ;
; ================================== ;
(define SEGMENT-BITS 16)
(define OFFSET-BITS 47)
; (RelocatableValue)
(struct rv (
    seg ; (segment_index) int
    off ; (offset) int
) #:mutable #:transparent #:reflection-name 'rv)

(define (rvadd self other)
    (tokamak:typed self rv?)
    (tokamak:typed other integer?)
    ; return
    (rv (rv-seg self) (+ (rv-off self) other))
)

(define (rvsub self other)
    (tokamak:typed self rv?)
    (tokamak:typed other rv? integer?)
    (cond
        [(integer? other) (rv (rv-seg self) (- (rv-off self) other))]
        [else
            ; other is also rv
            (assert (equal? (rv-seg self) (rv-seg other)) "rv-sub segs mismatch")
            (- (rv-off self) (rv-off other))
        ]
    )
)

(define (rvmod self other)
    (tokamak:typed self rv?)
    (tokamak:typed other integer?)
    ; return
    (rv (rv-seg self) (modulo (rv-off self) other))
)

(define (rveq self other)
    (tokamak:typed self rv?)
    (tokamak:typed other rv?)
    ; return
    (&&
        (equal? (rv-seg self) (rv-seg other))
        (equal? (rv-off self) (rv-off other))
    )
)

(define (rvlt self other)
    (tokamak:typed self rv?)
    (tokamak:typed other rv? integer?)
    (cond
        [(integer? other) #f]
        [else ; rv
            (let ([seg0 (rv-seg self)][seg1 (rv-seg other)]
                  [off0 (rv-off self)][off1 (rv-off other)])
                (if (< seg0 seg1)
                    #t
                    (if (equal? seg0 seg1)
                        (if (< off0 off1) #t #f)
                        #f
                    )
                )
            )
        ]
    )
)

; pass on, no need to check types
(define (rvle self other) (|| (rvlt self other) (rveq self other)))
(define (rvge self other) (! (rvlt self other)))
(define (rvgt self other) (! (rvle self other)))

; ========================================================================= ;
; ======== MemoryDict + MemorySegmentManager + ValidatedMemoryDict ======== ;
; ========================================================================= ;
; (MemoryDict + MemorySegmentManager + ValidatedMemoryDict)
(struct memory (

    ; MemoryDict
    data ; vector of vectors
    frozen ; (_frozen) bool
    rrules ; (relocation_rules) dict[int,rv]

    ; MemorySegmentManager
    ; (memory) is data, no need to include
    prime ; int
    nsegs ; (n_segments) int
    ntsegs ; (n_temp_segments) int
    ssizes ; (_segment_sizes) dict[int,int]
    susizes ; (_segment_used_sizes) dict[int,int] or null
    pmoffs ; (public_memory_offsets) dict[int,list[tup[int,int]]]

    ; ValidatedMemoryDict
    vrules ; (fixme) (__validation_rules) dict[int,list[tuple[ValidationRule,tuple]]]
    vaddrs ; (fixme) (__validated_addresses) set[rv]

) #:mutable #:transparent #:reflection-name 'memory)

; raw constructor
(define (new-memory
    ; MemoryDict
    #:data data #:frozen frozen #:rrules rrules
    ; MemorySegmentManager
    #:prime prime #:nsegs nsegs #:ntsegs ntsegs #:ssizes ssizes
    #:susizes susizes #:pmoffs pmoffs
    ; ValidatedMemoryDict
    #:vrules vrules #:vaddrs vaddrs
    )
    ; return
    (memory data frozen rrules prime nsegs ntsegs ssizes susizes pmoffs vrules vaddrs)
)

; constructor
(define (make-memory
    #:values [values null] ; MemoryDict
    #:prime prime ; MemorySegmentManager
    )
    (tokamak:typed values list? null?)
    (tokamak:typed prime integer?)

    ; (fixme) you need a correct way to convert values into data
    (when (! (null? values)) (tokamak:error "not implemented"))
    (define data0 (list->vector (for/list ([_ (range config:segcap)]) null)))

    ; return
    (new-memory
        #:data data0
        #:frozen #f
        #:rrules (make-hash)
        #:prime prime
        #:nsegs 0
        #:ntsegs 0
        #:ssizes (make-hash)
        #:susizes null
        #:pmoffs null
        #:vrules (make-hash) ; (fixme)
        #:vaddrs null ; (fixme)
    )
)

; MemoryDict method, internal core method
(define (data-ref p addr)
    (tokamak:typed p vector?) ; p is the memory data
    (tokamak:typed addr rv? integer?) ; for integer, treat it as seg=0
    (define seg0 (if (integer? addr) 0 (rv-seg addr)))
    (define off0 (if (integer? addr) addr (rv-off addr)))
    (define l0 (vector-ref p seg0))
    (when (null? l0) (tokamak:error "l0 is null, given addr: ~a." addr))
    (define l1 (vector-ref l0 off0))
    (when (null? l1) (tokamak:error "l1 is null, given addr: ~a." addr))
    ; return
    l1
)

; MemoryDict method, internal core method
(define (data-set-default! p key [default null])
    (tokamak:typed p vector?) ; p is the memory data
    (tokamak:typed key rv? integer?) ; for integer, treat it as seg=0
    (tokamak:typed default rv? integer?)
    (define seg0 (if (integer? key) 0 (rv-seg key)))
    (define off0 (if (integer? key) key (rv-off key)))
    (define l0 (let ([t0 (vector-ref p seg0)])
        (cond
            [(null? t0)
                (vector-set! p (list->vector (for/list ([_ (range config:offcap)]) null)))
                (vector-ref p seg0)
            ]
            [else t0]
        )
    ))
    (vector-set! l0 off0 default)
    ; return
    default
)

; MemoryDict method
(define (check-element num name)
    (tokamak:typed num rv? integer?)
    (tokamak:typed name string?)
    (when (&& (integer? num) (< num 0))
        (tokamak:error "~a must be nonnegative, got: ~a." name num))
)

; MemoryDict method
(define (verify-same-value addr current value)
    (tokamak:typed addr rv? integer?)
    (tokamak:typed current rv? integer?)
    (tokamak:typed value rv? integer?)
    ; (fixme) this may cause exception if current or value is rv
    ;         since equal? is not overloaded here
    (when (! (equal? current value))
        (tokamak:error "inconsistency memory error, addr: ~a, current: ~a, value: ~a." addr current value))
)

; MemoryDict method
(define (memory-ref p addr)
    (tokamak:typed p memory?)
    (check-element addr "memory address")
    (define value (data-ref (memory-data p) addr))
    ; return
    (relocate-value value)
)

; MemoryDict method
(define (memory-set! p addr value)
    (tokamak:typed p memory?)
    (when (memory-frozen p)
        (tokamak:error "memory is frozen and cannot be changed."))
    (check-element addr "memory address")
    (check-element value "memory value")
    (when (&& (rv? addr) (< (rv-off addr) 0))
        (tokamak:error "offset of rv must be nonnegative, got: ~a." (rv-off addr)))
    (define current (data-set-default! (memory-data p) addr value))
    (verify-same-value addr current value)
)

; MemoryDict method
(define (relocate-value value)
    (tokamak:typed value rv? integer?)
    (cond
        [(integer? value) value]
        [else (let ([seg (rv-seg value)])
            (cond
                [(>= seg 0) value]
                [else (tokamak:error "not implemented")]
            )
        )]
    )
)

; MemoryDict method
(define (validate-existing-memory p)
    (tokamak:typed p memory?)
    ; (fixme) add implementation
)

; MemorySegmentManager method
(define (add-segment p #:size [size null])
    (tokamak:typed p memory?)
    (tokamak:typed size integer? null?)
    (let ([segment-index (memory-nsegs p)])
        (set-memory-nsegs! p (+ 1 segment-index))
        (when (! (null? size))
            ; (fixme) call finalize
            (void)
        )
        ; return
        (rv segment-index 0)
    )
)