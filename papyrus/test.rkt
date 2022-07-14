#lang racket

(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in context: "./context.rkt")
    (prefix-in memory: "./memory.rkt")
    (prefix-in program: "./program.rkt")
    (prefix-in encode: "./encode.rkt")
    (prefix-in instruction: "./instruction.rkt")
    math/number-theory
)

(define (hint-87-helper ap fp op1)
  (+ ap fp op1))

"hello"
(hint-87-helper 5 6 7)
