#lang racket
(require plot noise)
(define sea-level .3)
(define coast-level .5)
(define beach-level 1)
(define forest-level 6)
(define jungle-level 3)
(define desert-level 4)
(define snowy-level 5)
(define mountain-level 6)
(define size 100)
(define redistribution 6)
(define freq 3)

(define (create-3d-simplex-with-water)
  (define seed (random 256))
  (define (get-elevation x y)
    (expt (abs (- 1
                  (+ (simplex (* freq (/ x size)) (* freq (/ y size)) seed)
                     (* 0.5 (simplex (* (* 2 freq) (/ x size))(* (* 2 freq) (/ y size)) seed))
                     (* 0.25 (simplex (* (* 4 freq) (/ x size))(* (* 2 freq) (/ y size)) seed)))
                  )) redistribution))
  (plot3d
   (list (surface3d (λ (x y) (* size (get-elevation x y))) 0 size 0 size #:color '(53 102 79) #:line-style 'transparent) ; land
         (surface3d (λ (x y) ; snow;
                      (let ([e (get-elevation x y)])
                        (if (> e snowy-level) (add1 (* size e)) 0))) 0 size 0 size #:line-style 'transparent)
         (surface3d (λ (x y) ; water
                      (let ([e (get-elevation x y)])
                        (if (< e sea-level) (* size sea-level) 0))) 0 size 0 size #:color '(62 96 193) #:line-style 'transparent)
         (surface3d (λ (x y) ; forest
                      (let ([e (get-elevation x y)])
                        (if (< e forest-level) (add1 (* size e)) 0))) 0 size 0 size #:color '(116 169 99) #:line-style 'transparent))))

(parameterize ([plot-width 1300] [plot-height 900] [plot-x-label #f] [plot-y-label #f] [plot-new-window? #t])
  (create-3d-simplex-with-water))