#lang racket
(provide (all-defined-out))
(define screen%
    (class object%
        (define/public (render state) (void)) ; state -> image
        (define/public (key-handler state key) (void)) ; state -> state
        (super-new)))
