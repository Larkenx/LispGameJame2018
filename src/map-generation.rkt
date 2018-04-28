#lang racket
(require noise thing "point.rkt" ) ; 2htdp/image)
(provide get-tile)
(struct level-definition (tile-gen npcs items))
(define levels (make-hasheq))
(define current-depth (make-parameter 0))
; Generate a new level if one does not exist
(define (get-level depth)
  (unless (hash-has-key? levels depth)
    (define level (make-hash))
    (hash-set! level 'seed (random))
    (hash-set! level 'npcs '())
    (hash-set! level 'gen (level-definition surface nothing nothing))
    (hash-set! levels depth level))
  (hash-ref levels depth))

(define (get-tile x y)
  (define current-level (get-level (current-depth)))
  (define seed (hash-ref current-level 'seed))

  ; If the tile doesn't already exist, generate it
  (unless (hash-has-key? current-level (pt x y))
    ; Get the new tile
    ; Copy the tile here so that they don't share state
    (define new-tile
      (let ([base-tile ((level-definition-tile-gen (hash-ref current-level 'gen)) seed x y)])
        (make-thing base-tile)))
    (hash-set! current-level (pt x y) new-tile))

  ; Return the tile (newly generated or not)
  (hash-ref current-level (pt x y)))

; The surface level with grass, water, and trees
(define (surface seed x y)
  ;                               snowy-level sea-level beach-level forest-level size freq redistribution
  ; (create-3d-simplex-with-water 6             .5           .6          1.2     100   3       4))
  (define sea-level .5)
  (define coast-level .7)
  (define beach-level .75)
  (define forest-level 2)
  (define jungle-level 3)
  (define desert-level 4)
  (define arctic-level 5)
  (define mountain-level 6)
  (define size 100)
  (define redistribution 3)
  (define freq 3)
  (define (get-elevation x y)
    (expt (abs (- 1
                  (+ (simplex (* freq (/ x size)) (* freq (/ y size)) seed)
                     (* 0.5 (simplex (* (* 2 freq) (/ x size))(* (* 2 freq) (/ y size)) seed))
                     (* 0.25 (simplex (* (* 4 freq) (/ x size))(* (* 2 freq) (/ y size)) seed)))
                  )) redistribution))
  (define e (get-elevation x y))
  (define water? (<= e sea-level))
  (define coast? (<= e coast-level))
  (define beach? (<= e beach-level))
  (define forest? (<= e forest-level))
  (define jungle? (<= e jungle-level))
  (define desert? (<= e desert-level))
  (define arctic? (<= e arctic-level))
  (define mountain? (<= e mountain-level))
  (cond
    [water? (make-thing water)]
    [coast? (make-thing shallow-water)]
    [beach? (make-thing sand)]
    [forest? (if (>= 5 (random 100))
                 (make-thing forest-tree)
                 (make-thing grass))]
    [jungle? (if (>= 5 (random 100))
                 (make-thing jungle-tree)
                 (make-thing grass))]
    [desert? (make-thing sand)]
    [mountain? (make-thing mountain)]
    [else    (make-thing empty)]))

; ===== Basic tile definitions =====

(define-thing tile
  [character " "]
  [color "black"]
  [items '()]
  [lighting 'dark]    ; Dark: Invisible; Fog: Only show tile, not NPC or item; Lit: Everything
  [walkable #f]       ; Can the player walk on this tile?
  [solid #f])         ; Does this tile block light?

(define-thing empty tile
  [walkable #t])

(define-thing grass tile
  [character "."]
  [color "brown"]
  [walkable #t])

(define-thing sand tile
  [character "."]
  [color "goldenrod"]
  [walkable #t])

(define-thing flower tile
  [character "\'"]
  [color "green"]
  [walkable #t])

(define-thing flower2 tile
  [character ","]
  [color "green"]
  [walkable #t])

(define-thing flower3 tile
  [character "`"]
  [color "green"]
  [walkable #t])

(define-thing mountain tile
  [character "^"]
  [color "white"]
  [solid #t])

(define-thing wall tile
  [solid #t]
  [character "#"]
  [color "white"])

(define-thing water tile
  [character "~"]
  [color "blue"])

(define-thing shallow-water tile
  [character "~"]
  [color "lightblue"])

(define-thing forest-tree tile
  [solid #t]
  [character "♠"]
  [color "green"])

(define-thing jungle-tree tile
  [solid #t]
  [character "♠"]
  [color "limegreen"])

(define (nothing seed x y) #f)
(define (real->natural n)
  (define (helper n acc)
    (cond
      [(>= acc n) acc]
      [(= acc 255) 255]
      [else (helper n (add1 acc))]))
  (helper n 0))

(require 2htdp/image)
(define (render-surface size)
  (define TILE_SIZE 5)
  (define (draw-char c x y fg bg canvas)
    (let ([char (overlay
                 (text/font c TILE_SIZE fg "Menlo" 'script 'normal 'normal #f)
                 (rectangle TILE_SIZE TILE_SIZE "solid" "black"))])
      (place-image/align char (* x TILE_SIZE) (* y TILE_SIZE) 'left 'top canvas)))
  (define img (rectangle size size "solid" "black"))
  (for* ([x (in-range size)]
         [y (in-range size)])
    (define tile (get-tile x y))
    (set! img (draw-char (thing-get tile 'character) x y (thing-get tile 'color) "transparent" img)))
  img)

(define (render-surface2 size)
  (define TILE_SIZE 2)
  (define img (rectangle size size "solid" "black"))
  (for* ([x (in-range size)]
         [y (in-range size)])
    (define tile (get-tile x y))
    (set! img
          (place-image/align
           (rectangle TILE_SIZE TILE_SIZE "solid" (thing-get tile 'color))
           x y 'left 'top img)))
  img)

;(save-image (render-surface 1000) "rendered-ascii-surface.png")
          
;
;
;(define (create-3d-simplex size freq)
;  (plot3d
;   (surface3d (λ (x y)
;                (* size ; moving it from decimal to 100's
;                   (expt (abs (- 1
;                                 (+ (simplex (* freq (/ x size)) (* freq (/ y size)))
;                                    (* 0.5 (simplex (* (* 2 freq) (/ x size))(* (* 2 freq) (/ y size))))
;                                    (* 0.25 (simplex (* (* 4 freq) (/ x size))(* (* 2 freq) (/ y size)))))
;                                 ))
;                         2))) 0 size 0 size #:line-style 'transparent)))
;

;(require plot)
;(define (create-3d-simplex-with-water snowy-level sea-level beach-level forest-level size freq redistribution)
;  (define seed (random 256))
;  (define (get-elevation x y)
;    (expt (abs (- 1
;                  (+ (simplex (* freq (/ x size)) (* freq (/ y size)) seed)
;                     (* 0.5 (simplex (* (* 2 freq) (/ x size))(* (* 2 freq) (/ y size)) seed))
;                     (* 0.25 (simplex (* (* 4 freq) (/ x size))(* (* 2 freq) (/ y size)) seed)))
;                  )) redistribution))
;  (plot3d
;   (list (surface3d (λ (x y) (* size (get-elevation x y))) 0 size 0 size #:color '(53 102 79) #:line-style 'transparent) ; land
;         (surface3d (λ (x y) ; snow
;                      (let ([e (get-elevation x y)])
;                        (if (> e snowy-level) (add1 (* size e)) 0))) 0 size 0 size #:line-style 'transparent)
;         (surface3d (λ (x y) ; water
;                      (let ([e (get-elevation x y)])
;                        (if (< e sea-level) (* size sea-level) 0))) 0 size 0 size #:color '(62 96 193) #:line-style 'transparent)
;         (surface3d (λ (x y) ; beach
;                      (let ([e (get-elevation x y)])
;                        (if (< e beach-level) (add1 (* size e)) 0))) 0 size 0 size #:color '(87 121 242) #:line-style 'transparent)
;         (surface3d (λ (x y) ; forest
;                      (let ([e (get-elevation x y)])
;                        (if (< e forest-level) (add1 (* size e)) 0))) 0 size 0 size #:color '(116 169 99) #:line-style 'transparent))))
;
;(parameterize ([plot-width 1300] [plot-height 900] [plot-x-label #f] [plot-y-label #f] [plot-new-window? #t])
;  (create-3d-simplex-with-water 6 .5 .4 1.2 100 3 2))
