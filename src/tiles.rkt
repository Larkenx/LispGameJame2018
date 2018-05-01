#lang racket
(require thing 2htdp/image)
(provide (all-defined-out))

; ===== Basic tile definitions =====

(define-thing tile
  [character " "]
  [color "black"]
  [items '()]
  [lighting 'dark]    ; Dark: Invisible; Fog: Only show tile, not NPC or item; Lit: Everything
  [walkable #f]       ; Can the player walk on this tile?
  [solid #f]
  [background-color (color 14 58 57)]) 

(define-thing empty tile
  [walkable #t])

(define-thing grass tile
  [character "."]
  [color "green"]
  [walkable #t])

(define-thing sand tile
  [character "."]
  [color "goldenrod"]
  [walkable #t])

(define-thing flower1 tile
  [character ","]
  [color "lightgreen"]
  [walkable #t])

(define-thing flower2 tile
  [character ";"]
  [color "green"]
  [walkable #t])

(define-thing flower3 tile
  [character "'"]
  [color "green"]
  [walkable #t])

(define-thing mountain tile
  [character "^"]
  [color (make-color 193 171 137)]
  [solid #t])

(define-thing mountain-peak tile
  [character "^"]
  [color "white"]
  [solid #t])

(define-thing wall tile
  [solid #t]
  [character "#"]
  [color "white"])

(define-thing water tile
  [character "≈"]
  ;[background-color (make-color 0 71 188)]
  [color (make-color 0 146 251)])

(define-thing shallow-water water
  [character "~"]
  [walkable #t])

(define-thing forest-tree tile
  [solid #t]
  [character "♠"]
  [color "green"])

(define-thing jungle-tree tile
  [solid #t]
  [character "♠"]
  [color "limegreen"])