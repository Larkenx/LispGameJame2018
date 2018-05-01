#lang racket
(require "point.rkt" "screen.rkt" "inventory-screen.rkt" "map-generation.rkt" 2htdp/image thing)
(provide game-screen% text/menlo text/menlo/small)
(define (text/menlo s color) (text/font s 28 color "Menlo" 'script 'normal 'normal #f))
(define (text/menlo/small s color) (text/font s 20 color "Menlo" 'script 'normal 'normal #f))
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
                   (text/font c 28 fg "Menlo" 'script 'normal 'normal #f)
                   (rectangle TILE_SIZE TILE_SIZE "solid" bg))])
        (place-image/align char (* x TILE_SIZE) (* y TILE_SIZE) 'left 'top canvas)))

    (define (draw-entity e state canvas)
      (define p (thing-get e 'location))
      (define x/y (recenter this (- (thing-get (send state get-player) 'location) (pt (pt-x p) (pt-y p)))))
      (define tile (send state tile-at (pt-x x/y) (pt-y x/y)))
      (draw-char (thing-get e 'character) (pt-x x/y) (pt-y x/y) (thing-get e 'color) (thing-get tile 'background-color) canvas))


    (define/override (render state)
      
      (define player (send state get-player))
      (define p (thing-get player 'location))
      (define draw-@ (recenter this (pt 0 0)))
      (define canvas (rectangle WIDTH HEIGHT "solid" "black"))
      ; right-panel to hold stats, items held
      (define separator (rectangle (* TILE_SIZE .25) HEIGHT "solid" (make-color 175 199 193)))
      (define hp (string-append
                  "HP: "
                  (number->string (thing-get player 'health))
                  " / "
                  (number->string (thing-get player 'max-health))))
      (define HUD
        (let ([panel (rectangle (* TILE_SIZE 15) (- HEIGHT 180) "solid" (color 14 58 57))]
              [get-gem (λ (f) (if (f (thing-get player 'gems)) "◆ " "◇ "))])
          (place-image/align
           (above/align 'left
                        (text/menlo "Merlin" (make-color 175 199 193)) ; name of character
                        (text/menlo hp "white") ; hp / maxhp
                        (text/menlo (string-append "DEF: " (number->string (thing-get player 'defense))) "white")
                        (beside (text/menlo "Gems: " "white") ; gems collected
                                (text/menlo (get-gem first) (make-color 0 146 251))
                                (text/menlo (get-gem second) "green")
                                (text/menlo (get-gem third) "red")
                                (text/menlo (get-gem fourth)"white"))) 
           10 10 'left 'top panel)))
      (define logs-viewer
        (let ([result (rectangle (* TILE_SIZE 15) 180 "solid" (color 14 58 57))]
              [logs (send state get-logs)])
          (place-image/align
           (text/menlo/small (foldr (λ (log r) (string-append r "\n" log)) "" logs) "white")
           0 0 'left 'top
           result)))
      
      (define (draw-tile x y tile c)
        (let ([items (thing-get tile 'items)])
          (cond
            [(not (empty? items)) (draw-char (thing-get (first items) 'character) x y (thing-get (first items) 'color) (thing-get tile 'background-color) c)]
            [else (draw-char (thing-get tile 'character)  x y (thing-get tile 'color) (thing-get tile 'background-color) c)])))
            
          
      ; render all of the tiles in the viewport
      (for* ([xi (in-range width-in-characters)]
             [yi (in-range height-in-characters)])
        (define x/y (recenter this (- (thing-get player 'location) (pt xi yi))))
        (define tile (send state tile-at (pt-x x/y) (pt-y x/y)))
        (set! canvas (draw-tile xi yi tile canvas)))
      ; render the player to the viewport
      (set! canvas (draw-char (thing-get player 'character)
                              (pt-x draw-@) (pt-y draw-@)
                              (thing-get player 'color)
                              (thing-get (send state tile-at (pt-x p) (pt-y p)) 'background-color)
                              canvas))

      
      ; render all other npcs to the viewport
      (for ([npc (get-npcs)])
        (set! canvas (draw-entity npc state canvas)))

      ; render a separator and panel next to the game-viewport for a HUD
      (beside
       canvas
       separator
       (above HUD logs-viewer)))
       

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
        [("numpad3" "n") (send state try-move player (- location (pt 1 1)))])
        ;[("q") (send state kill-player)]
        ;[("w") (send state collect-all-gems)])
      (send state update)
      state)

    (super-new)))
