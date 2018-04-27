#lang racket
(require "point.rkt" "screen.rkt" "inventory-screen.rkt" 2htdp/image thing)
(provide game-screen%)
(define game-screen%
    (class screen%
        (init-field TILE_SIZE WIDTH HEIGHT)
        (define width-in-characters (quotient WIDTH TILE_SIZE))
        (define height-in-characters (quotient HEIGHT TILE_SIZE))
        (define/public (get-width-in-characters) width-in-characters)
        (define/public (get-height-in-characters) height-in-characters)

        ; draw a character at a location with given colors on the given image
        (define (draw-char c x y fg bg canvas)
          (let ([char (overlay
                       (text/font c TILE_SIZE fg "Menlo" 'script 'normal 'normal #f)
                       (rectangle TILE_SIZE TILE_SIZE "solid" "black"))])
            (place-image/align char (* x TILE_SIZE) (* y TILE_SIZE) 'left 'top canvas)))

        (define (draw-entity e canvas)
            (define p (thing-get e 'location))
            (draw-char (thing-get e 'character) (pt-x p) (pt-y p) (thing-get e 'color) "transparent" canvas))


        (define/override (render state)
        ; basic idea for a render:
        ; - render viewport of map using tile-at method on state
        ; - render the player, and non-player-characters
            (define canvas (rectangle WIDTH HEIGHT "solid" "black"))
            (define player (send state get-player))
            (define draw-@ (recenter this (pt 0 0)))
            (for* ([xi (in-range width-in-characters)]
                   [yi (in-range height-in-characters)])
                (define x/y (recenter this (- (thing-get player 'location) (pt xi yi))))
                (define tile (send state tile-at (pt-x x/y) (pt-y x/y)))
                (set! canvas (draw-char (thing-get tile 'character)  xi yi (thing-get tile 'color) "transparent" canvas)))
            (draw-char (thing-get player 'character) (pt-x draw-@) (pt-y draw-@) (thing-get player 'color) "transparent"  canvas))

        ; key event handler for player input while viewing in-game display
        (define/override (key-handler state key)
            (define player (send state get-player))
            (define location (thing-get player 'location))
            (case key
                [("numpad4" "h" "left") (send state try-move player (- location (pt -1 0)))]
                [("numpad6" "l" "right") (send state try-move player (- location (pt 1 0)))]
                [("numpad2" "j" "down") (send state try-move player (- location (pt 0 1)))]
                [("numpad8" "k" "up") (send state try-move player (- location (pt 0 -1)))]
                [("numpad7" "y") (send state try-move player (- location (pt -1 -1)))]
                [("numpad9" "u") (send state try-move player (- location (pt 1 -1)))]
                [("numpad1" "b") (send state try-move player (- location (pt -1 1)))]
                [("numpad3" "n") (send state try-move player (- location (pt 1 1)))]
                [else state]))

        (super-new)))
