#lang rosette
(require rosette/lib/angelic)
;; my-x is either 00 or 01
(define my-x (choose* (bv 0 2) (bv 1 2)))
(define (check x)
  (cond
    [(equal? x (bv 0 2))
     (displayln "checked 00")]
    [(equal? x (bv 1 2))
     (displayln "checked 01")]
    [(equal? x (bv 2 2))
     (displayln "checked 10")]
    [(equal? x (bv 3 2))
     (displayln "checked 11")]))
(displayln "Rosette uses rewrite rules to speed up sym exec in some cases")
(check my-x)
(displayln "")
(displayln "The rewrite rules aren't perfect though")
(displayln "In this case, rosette checks 11, even though my-x can be at most 10")
(define my-y (choose* (bv 0 2) (bv 1 2)))
(set! my-x (bvadd my-y my-y))
(check my-x)
(displayln "")
(displayln "We can use for/all with #:exhaustive to force this optimization")
(for/all ([y my-y #:exhaustive])
  (begin
    (set! my-x (bvadd y y))
    (check my-x)))
(displayln "")
(displayln "for/all #:exhaustive needs an (ite ...) as input to work properly though")
(set! my-x (bvadd my-y my-y))
(for/all ([x my-x #:exhaustive])
  (check my-x))