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
                  [health 20]
                  [max-health 20]
                  [gems (list #f #f #f #f)]))

    (define/public (get-player) player)
    
    ; holds all the log messages
    (define log-messages '())
    ; add a new message to the log
    (define/public (log-message msg)
      (set! log-messages (cons msg log-messages)))

    (define/public (get-logs)
      (if (<= (length log-messages) 6)
          log-messages
          (take log-messages 6)))


    ; define the active screen for the big bang to render
    (define active-screen (new game-screen% [WIDTH 800] [HEIGHT 608] [TILE_SIZE 32]))
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
      (define others
        (filter
         ; Only get ones at the target location that aren't me
         (λ (thing) (and (not (eqv? thing entity))
                         (= (thing-get thing 'location) target)))
         ; Include the player and all npcs
         (cons player (get-npcs))))
      (cond
        [(not (thing-get tile 'walkable)) (void)]
        [(null? others)         
         ; move to the new location
         (thing-set! entity 'location target)
         ; Pick up an item from the ground
         (define (pick-up item)
           (when (string=? "player" (thing-get entity 'name))
             (thing-call item 'on-pick-up item entity this)         
             (printf (foldr (λ (g r) (string-append (if g " true " " false ") r)) "" (thing-get entity 'gems))))
                  
           (send this log-message (format "~a picked up ~a" (thing-get entity 'name) (thing-get item 'name)))
           (thing-set! entity 'inventory (cons item (thing-get entity 'inventory)))
           (thing-set! tile 'items (remove item (thing-get tile 'items))))
             
         ; Drop an item from the inventory
         (define (drop item)
           (send this log-message (format "~a dropped ~a" (thing-get entity 'name) (thing-get item 'name)))
           (thing-set! entity 'inventory (remove item (thing-get entity 'inventory)))
           (thing-set! tile 'items (cons item (thing-get tile 'items)))
           (thing-call item 'on-drop item entity this))
         
         ; Consume a consumable item from the ground
         (define (consume item)
           (send this log-message (format "~a consumed ~a" (thing-get entity 'name) (thing-get item 'name)))
           (thing-set! tile 'items (remove item (thing-get tile 'items)))
           (thing-call item 'on-pick-up item entity this))

         (for ([item (in-list (thing-get tile 'items))])
           (pick-up item))]

         
        [else
         (for ([other (in-list others)])
           (send this attack entity other))])
      this)

    (define/public (attack entity other)
      ; Do the damage
      (define damage
        (max 0 (- (random (max 1 (thing-get entity 'attack)))
                  (random (max 1 (thing-get other 'defense))))))
      (thing-set! other 'health (- (thing-get other 'health) damage))
      
      ; Log a message
      (send this log-message
            (format "~a attacked ~a, did ~a damage"
                    (thing-get entity 'name)
                    (thing-get other 'name)
                    damage)))

    (define/public (update)
      (update-npcs this))

    (define (collected-all-gems)
      (foldr  (λ (v r) (and v r)) #true (thing-get player 'gems)))
    
    ; big bang functions
    (define (end-game-condition)
      (or (<= (thing-get player 'health) 0) (collected-all-gems)))

    (define/public (kill-player)
      (thing-set! player 'health 0))

    (define/public (collect-all-gems)
      (thing-set! player 'gems (list #t #t #t #t)))

    ; render last image
    (define (render-death i)
      (overlay
       (overlay
        (text/menlo "You died!" "red")
        (overlay         
         (rectangle 400 200 "solid" (color 14 58 57))
         (rectangle 410 210 "solid" (make-color 175 199 193))))        
       i))

    (define (render-victory i)
      (overlay
       (overlay
        ; (text/font c 28 fg "Menlo" 'script 'normal 'normal #f)
        (text/menlo/small (string-append "You find the last gem and insert it into your teleporter...\n"
                                   "You activate your teleporter and escaped the wilderness!")
                                   (make-color 0 146 251))
        (overlay         
         (rectangle 730 200 "solid" (color 14 58 57))
         (rectangle 740 210 "solid" (make-color 175 199 193))))        
       i))

    ; world% -> boolean
    (define end-game-state void) ; -> world%
    (define/public (get-render s) (send active-screen render s))
    (define/public (get-key-handler s k)  (send active-screen key-handler s k))
    (define/public (get-end-game-condition) (end-game-condition))
    (define/public (get-end-game-image)
      (if (and (> (thing-get player 'health) 0) (collected-all-gems))
          (render-victory (send active-screen render this))
          (render-death (send active-screen render this))))
    (define/public (get-end-game-state) end-game-state)
    (super-new)))
