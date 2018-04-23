#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(struct game (width height walls player enemies))

(struct player (char x y hp str items))

(struct enemy (char x y hp str items))

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
               (text/font c 16 fg "Menlo" 'script 'normal 'normal #f)
               (rectangle 16 16 "solid" "black"))])
    (place-image/align char (* x 16) (* y 16) 'left 'top img)))

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

(define (draw-map2 state img)
  (let* ([walls (game-walls state)]
         [width (game-width state)]
         [height (game-height state)]
         [tile-mapping (foldr (λ (v l) (append l v)) '() (build-list height
                                                                     (λ (y) (build-list width
                                                                                        (λ (x)
                                                                                          (let ([p (make-posn x y)])
                                                                                            (if (hash-has-key? walls p)
                                                                                                (list p "#")
                                                                                                (list p "."))))))))])
    (foldr
     (λ (v i)
       (let ([p (first v)] [char (second v)])
         (draw-char char (if (string=? char "#" ) "gray" "orange") (posn-x p) (posn-y p) i))) img tile-mapping)))
 

(define (background state)
  (rectangle (* 16 (game-width state)) (* 16 (game-height state)) "solid" "black"))

(define (render state)
  (draw-player (game-player state)
               (draw-all-enemies state
                                 (draw-map2 state
                                            (background state)))))

; Handling player state
; State -> State
(define (move-player state dx dy)
  (let* ([walls (game-walls state)]
         [p (game-player state)]
         [x (player-x p)]
         [y (player-y p)]
         [nx (+ x dx)]
         [ny (+ y dy)])
    (if (not (hash-has-key? walls (make-posn nx ny)))
        (struct-copy game state
                     (player (struct-copy player p [x nx] [y ny])))
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

; Enemy, Posn -> Enemy
; Returns an enemy 
;(define (move-enemy-helper
; State -> State
(define (move-enemy state enemy)
  (let* ([walls (game-walls state)]
         [x (enemy-x enemy)]
         [y (enemy-y enemy)]
         [enemies (game-enemies state)]
         [all-but-this-enemy (filter (λ (e) (not (equal? e enemy))) enemies)]
         [possible-movements (viable-movements state x y)])
    (cond
      [(> (length possible-movements) 0)
       
       ]
      [else state])))
        
        
    


; move-enemies: State -> State
; Takes the state and returns a new state with
; all of the enemies having taken their move
(define (move-enemies state)
  (let ([enemies (game-enemies state)])
    (foldr (λ (e st) (move-enemy st e)) state enemies)))
  

; turn: Takes the state after the player makes a move,
; and progresses the state of the game by letting all of the actors
; take a move!
; State -> State
(define (turn state)
  (move-enemies state))



; Key Event Handler
; State, String -> State
(define (key-handler state k)
  (cond
    ; vim movement keys
    [(key=? k "l") (move-player state 1 0)]
    [(key=? k "h") (move-player state -1 0)]
    [(key=? k "k") (move-player state 0 -1)]
    [(key=? k "j") (move-player state 0 1)]
    [(key=? k "y") (move-player state -1 -1)]
    [(key=? k "u") (move-player state 1 -1)]
    [(key=? k "b") (move-player state -1 1)]
    [(key=? k "n") (move-player state 1 1)]
    [else state]))



(define init-state
  (game 10 10
        (create-walls 9)
        (player "@" 1 1 1 10 '())
        (list (enemy "r" 6 7 1 1 '()) (enemy "g" 3 7 1 1 '()))))

(render init-state)

(big-bang init-state (to-draw render) (on-key key-handler))