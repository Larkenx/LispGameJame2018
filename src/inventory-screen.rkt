#lang racket
(require "screen.rkt" 2htdp/image)
(provide inventory-screen%)

(define inventory-screen%
    (class screen%
        (define/override (render state)
            (rectangle 100 100 "solid" "red"))
        ; key event handler for player input while viewing in-game display
        (define/override (key-handler state key)
            state)

        (super-new)))
