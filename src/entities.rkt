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
