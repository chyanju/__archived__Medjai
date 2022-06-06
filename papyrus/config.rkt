#lang rosette
; this module stores all global configurations of the tool
(provide (all-defined-out))

(define bvsize 256) ; default bv size for representing integer
(define bvzero (bv 0 bvsize))
(define bvone (bv 1 bvsize))

(define segcap 20) ; maximum number of segments in memdict
(define offcap 20) ; maximum number of offsets in each segment in memdict