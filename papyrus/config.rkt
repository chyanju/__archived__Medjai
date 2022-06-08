#lang rosette
; this module stores all global configurations of the tool
(provide (all-defined-out))

(define segcap 10) ; maximum number of segments in memdict
(define offcap 10) ; maximum number of offsets in each segment in memdict