#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(struct game (width height walls player enemies)  #:transparent)

(struct player (char x y hp maxhp str items)  #:transparent)

(struct enemy (char x y hp str items) #:extra-constructor-name make-enemy #:transparent)

(define TILE_SIZE 32)

; List -> any/c
; returns a random element from a list
(define (get-random-value ls)
  (cond
    [(empty? ls) #f]
    [else (list-ref ls (random 0 (length ls)))]))

;; Map Generation
; constructs and returns hash table containing lookup table for walls
; Number -> Hash
(define (create-walls n)
  (let ([walls (make-hash)])
    (begin
      (for ([y (in-range (+ n 1))])
        (for ([x (in-range (+ n 1))])
          (if (or (= x 0) (= y 0) (= x n) (= y n)) 
              (hash-set! walls (make-posn x y) #t)
              void)))
      walls)))

;; Drawing / GUI
; String String Number Number Image -> Image
(define (draw-char c fg x y img)
  (let ([char (overlay
               (text/font c TILE_SIZE fg "Menlo" 'script 'normal 'normal #f)
               (rectangle TILE_SIZE TILE_SIZE "solid" "black"))])
    (place-image/align char (* x TILE_SIZE) (* y TILE_SIZE) 'left 'top img)))

(define (draw-player p img)
  (draw-char (player-char p) "yellow" (player-x p) (player-y p) img))

(define (draw-enemy e img)
  (draw-char (enemy-char e) "red" (enemy-x e) (enemy-y e) img))

(define (draw-all-enemies state img)
  (let ([enemies (game-enemies state)])
    (foldr (λ (e i) (draw-enemy e i)) img enemies)))

; State, Image -> Image
(define (draw-map state img)
  (let ([walls (game-walls state)]
        [width (game-width state)]
        [height (game-height state)]
        [drawn-image img])
    (begin
      (for ([y (in-range height)])
        (for ([x (in-range width)])
          (if (hash-has-key? walls (make-posn x y))
              (set! drawn-image (draw-char  "#" "gray" x y drawn-image))
              (set! drawn-image (draw-char  "." "orange" x y drawn-image)))))
      drawn-image)))

;(define (draw-map2 state img)
;  (let* ([walls (game-walls state)]
;         [width (game-width state)]
;         [height (game-height state)]
;         [tile-mapping (foldr (λ (v l) (append l v)) '() (build-list height
;                                                                     (λ (y) (build-list width
;                                                                                        (λ (x)
;                                                                                          (let ([p (make-posn x y)])
;                                                                                            (if (hash-has-key? walls p)
;                                                                                                (list p "#")
;                                                                                                (list p "."))))))))])
;    (foldr
;     (λ (v i)
;       (let ([p (first v)] [char (second v)])
;         (draw-char char (if (string=? char "#" ) "gray" "orange") (posn-x p) (posn-y p) i))) img tile-mapping)))
 



(define (HUD-background-panel state)
  (rectangle 200 (* TILE_SIZE (game-height state)) "solid" "black"))

(define (draw-hud state)
  (let* ([p (game-player state)]
         [hp (player-hp p)]
         [maxhp (player-maxhp p)]
         [hp-ratio (/ hp maxhp)]
         [hp-bar-size 80])
    (place-image/align
     (beside
      (text/font "Health " 12 "white" "Menlo" 'script 'normal 'normal #f)
      (overlay/align 'left 'top
                     (rectangle (* hp-bar-size (if (> hp-ratio 0) hp-ratio 0)) 10 "solid" "red")
                     (rectangle hp-bar-size 10 "solid" "crimson"))
      (text/font (string-append " " (number->string hp) "/" (number->string maxhp)) 12 "white" "Menlo" 'script 'normal 'normal #f))
    
     25 25 'left 'top
     (HUD-background-panel state))))

; background for game display
(define (background state)
  (rectangle (* TILE_SIZE (game-width state)) (* TILE_SIZE (game-height state)) "solid" "black"))
(define (draw-game-display state)
  (draw-player (game-player state)
               (draw-all-enemies state
                                 (draw-map state
                                           (background state)))))
; render the entire display
(define (render state)
  (beside
   (draw-game-display state)
   (draw-hud state)))

;; Handling player state
; State, Number -> State
(define (damage-enemy state target dmg)
  (let ([all-enemies (game-enemies state)])
    (struct-copy game state
                 (enemies (map (λ (e) (if (eq? e target) (struct-copy enemy target [hp (- (enemy-hp target) dmg)]) e)) all-enemies)))))

; State -> State
(define (move-player state dx dy)
  (let* ([walls (game-walls state)]
         [p (game-player state)]
         [x (player-x p)]
         [y (player-y p)]
         [nx (+ x dx)]
         [ny (+ y dy)])
    (if (not (hash-has-key? walls (make-posn nx ny)))
        ; no wall blocking the tile, but there might be an enemy...
        (let* ([all-enemies (game-enemies state)]
               [possible-enemies (filter (λ (e) (and (= (enemy-x e) nx) (= (enemy-y e) ny))) all-enemies)])
          (if (>= (length possible-enemies) 1)
              (damage-enemy state (first possible-enemies) (player-str p))
              (struct-copy game state
                           (player (struct-copy player p [x nx] [y ny])))))
        state)))

; Given the surrounding obstacles, the x & y position,
; it will return a list of possible movement posns at that x,y position
; as vectors
; State, Number, Number -> Listof Posn
(define (viable-movements state x y)
  (let ([all-possible (list
                       (make-posn 1 1)
                       (make-posn -1 -1)
                       (make-posn 0 1)
                       (make-posn 1 0)
                       (make-posn -1 0)
                       (make-posn 0 -1)
                       (make-posn 1 -1)
                       (make-posn -1 1))]
        [walls (game-walls state)]
        [width (game-width state)]
        [enemies (game-enemies state)]
        [height (game-height state)])
    (filter (λ (p)
              (let ([px (posn-x p)]
                    [py (posn-y p)])
                (and (not (hash-has-key? walls p))
                     (> px 0) (<= px width) (> py 0) (<= py height)
                     (not (foldr
                           (λ (e collision) (or collision (and (= (enemy-x e) px) (= (enemy-y e) py)))) #f enemies)))))
            (map (λ (a) (make-posn (+ x (posn-x a)) (+ y (posn-y a)))) all-possible))))

; State, Enemy -> Boolean
(define (enemy-can-attack-player state enemy)
  (let ([all-possible (list
                       (make-posn 1 1)
                       (make-posn -1 -1)
                       (make-posn 0 1)
                       (make-posn 1 0)
                       (make-posn -1 0)
                       (make-posn 0 -1)
                       (make-posn 1 -1)
                       (make-posn -1 1))]
        [x (enemy-x enemy)]
        [y (enemy-y enemy)]
        [player (game-player state)])
    (ormap
     (λ (p) (and (= (posn-x p) (player-x player)) (= (player-y player) (posn-y p))))
     (map
      (λ (a) (make-posn (+ x (posn-x a)) (+ y (posn-y a)))) all-possible))))

; State, Number -> State
(define (damage-player state dmg)
  (let ([p (game-player state)])
    (struct-copy game state (player (struct-copy player p [hp (- (player-hp p) dmg)])))))


; State -> State
(define (move-enemy state enemy)
  (let* ([walls (game-walls state)]
         [x (enemy-x enemy)]
         [y (enemy-y enemy)]
         [enemies (game-enemies state)]
         [all-but-this-enemy (filter (λ (e) (not (equal? e enemy))) enemies)]
         [possible-movements (viable-movements state x y)]
         [player-in-range (enemy-can-attack-player state enemy)])
    (if player-in-range
        (damage-player state (enemy-str enemy))
        (struct-copy game state (enemies (append all-but-this-enemy
                                                 (list
                                                  ; creates new enemy
                                                  (cond
                                                    [(> (length possible-movements) 0)
                                                     (let* ([p (get-random-value possible-movements)] [px (posn-x p)] [py (posn-y p)])
                                                       (make-enemy (enemy-char enemy) px py (enemy-hp enemy) (enemy-str enemy) (enemy-items enemy)))]  
                                                    [else enemy]))))))))

; move-enemies: State -> State
; Takes the state and returns a new state with
; all of the enemies having taken their move
(define (move-enemies state)
  (let ([enemies (game-enemies state)])
    (foldr (λ (e st) (move-enemy st e)) state enemies)))

; remove-dead-enemies: State -> State
; Removes all enemies with hp <= 0
(define (remove-dead-enemies state)
  (struct-copy game state (enemies (filter (λ (e) (not (<= (enemy-hp e) 0))) (game-enemies state)))))

; turn: Takes the state after the player makes a move,
; and progresses the state of the game by letting all of the actors
; take a move!
; State -> State
(define (turn state)
  ; TODO: remove dead enemies from the state, *before* they take their turn :)
  (move-enemies (remove-dead-enemies state)))

; Key Event Handler
; State, String -> State
(define (key-handler state k)
  (cond
    ; vim movement keys
    [(key=? k "l") (turn (move-player state 1 0))]
    [(key=? k "h") (turn (move-player state -1 0))]
    [(key=? k "k") (turn (move-player state 0 -1))]
    [(key=? k "j") (turn (move-player state 0 1))]
    [(key=? k "y") (turn (move-player state -1 -1))]
    [(key=? k "u") (turn (move-player state 1 -1))]
    [(key=? k "b") (turn (move-player state -1 1))]
    [(key=? k "n") (turn (move-player state 1 1))]
    [(key=? k "-") (struct-copy game state (player (struct-copy player (game-player state) [hp (sub1 (player-hp (game-player state)))])))]
    [else state]))

;(struct player (char x y hp maxhp str items))

(define (game-over state)
  (or (<= (player-hp (game-player state)) 0)
      (empty? (game-enemies state))))

(define init-state
  (game 16 16
        (create-walls 15)
        (player "@" 1 1 10 10 1 '())
        (list (enemy "r" 6 7 1 1 '()) (enemy "g" 3 7 1 1 '()))))

(big-bang init-state (to-draw render) (on-key key-handler) (stop-when game-over))