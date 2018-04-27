#lang racket

(provide (all-defined-out))

(require thing "point.rkt")

; All entities have:
; - a location on the map
; - attack and defense strengths
; - hitpoints
; - an inventory
(define-thing entity
  [character "x"]
  [color "white"]
  [location (pt 0 0)]
  [inventory '()]
  [view-range 5])

(define-thing enemy entity
    [name "enemy"]
    [color "red"]
    [attack 10]
    [defense 10]
    [health 10]
    [(act self world) (void)])

(define-thing wandering-enemy enemy
    [(act self world)
    (send world try-move
        self
        (+ (thing-get 'location)
             (pt (- (random 3) 1)
                 (- (random 3) 1))))])

; A seeking enemy runs towards the player (heedless of walls) 50% of the time
; The other 50% of the time they are identical to a wandering enemy
(define-thing seeking-enemy wandering-enemy
  [(act self world)
   (cond
     ; 50/50 of a seeking move
     [(= 0 (random 2))
      (define player-pt (thing-get (send world get-player) 'location))
      (define self-pt (thing-get self 'location))
      (define dir (unit (- player-pt self-pt)))
      (send world try-move
            self
            (+ self-pt
               (inexact->exact (round (pt-x dir)))
               (inexact->exact (round (pt-y dir)))))]
     ; Otherwise, wander
     [else
      (thing-call wandering-enemy 'act self world)])])

; A fleeing enemy moves the exact opposite of a seeking enemy
(define-thing fleeing-enemy wandering-enemy
  [(act self world)
   (cond
     ; 50/50 of a fleeing move
     [(= 0 (random 2))
      (define player-pt (thing-get (send world get-player) 'location))
      (define self-pt (thing-get self 'location))
      (define dir (unit (- player-pt self-pt)))
      (send world try-move
            self
            (- self-pt
               (inexact->exact (round (pt-x dir)))
               (inexact->exact (round (pt-y dir)))))]
     ; Otherwise, wander
     [else
      (thing-call wandering-enemy 'act self world)])])
