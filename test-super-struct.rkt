#lang rosette/safe

(struct animal (size color))
; (struct dog animal (name location))
(struct dog (name location) #:super struct:animal)

(define dog0 (dog 9 "red" "bob" "sb"))