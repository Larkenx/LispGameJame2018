#lang racket
(require thing 2htdp/image "point.rkt" "game-screen.rkt" "map-generation.rkt" "entities.rkt")
(provide (all-defined-out))

(define world%
  (class object%
    (define player
        (make-thing entity
            [character "@"]
            [color "yellow"]
            [name "player"]
            [attack 10]
            [defense 10]
            [health 100]))

    (define/public (get-player) player)
    ; holds all the log messages
    (define log-messages '())
    ; add a new message to the log
    (define/public (log-message msg)
        (set! log-messages (cons log-messages msg)))

    (define/public (get-logs) log-messages)



    ; define the active screen for the big bang to render
    (define active-screen (new game-screen% [WIDTH 800] [HEIGHT 600] [TILE_SIZE 32]))
    (define/public (update-screen new-screen)
        (set! active-screen new-screen))

    ; functions to assist with rendering...?
    (define/public (tile-at x y)
        (get-tile x y))

    ; move entity to target space
    ; if there is another entity on the target space,
    ; the entity will interact with that entity
    (define/public (try-move entity target)
        (define tile (tile-at (pt-x target) (pt-y target)))
        (cond
            [(not (thing-get tile 'walkable)) (void)]
            [else (thing-set! entity 'location target)])
        this)

    ; big bang functions
    (define end-game-condition #f) ; world% -> boolean
    (define end-game-state void) ; -> world%
    (define/public (get-render s) (send active-screen render s))
    (define/public (get-key-handler s k)  (send active-screen key-handler s k))
    (define/public (get-end-game-condition) end-game-condition)
    (define/public (get-end-game-state) end-game-state)
    (super-new)))
