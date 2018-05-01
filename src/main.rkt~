#lang racket
(require 2htdp/image 2htdp/universe "world.rkt")

(define world
  (new world%))

(big-bang (new world%)
  (name "Racket Roguelike")
  (to-draw (λ (s) (send s get-render s)))
  (on-key (λ (s k) (send s get-key-handler s k)))
  (stop-when (λ (s) (send s get-end-game-condition))))
