#lang racket
(require images/flomap)

(struct pt (x y))
(struct cpt pt (c))

(define (voronoi width height distance seeds)
  (flomap->bitmap
   (build-flomap*
    3 width height
    (λ (x y)
      (call-with-values
       (thunk
        (for/fold ([min-distance +inf.0] [min-color #f])
                  ([seed (in-list seeds)])
          (define new-distance (distance (pt x y) seed))
          (if (< new-distance min-distance)
              (values new-distance (cpt-c seed))
              (values min-distance min-color))))
       (λ (distance color) color))))))

(define (random-seeds width height count)
  (for/list ([i (in-range count)])
    (cpt (random width) (random height)
         (vector (random) (random) (random)))))

(define (distance p1 p2)
  (expt (+ (expt (- (pt-x p1) (pt-x p2)) 2)
           (expt (- (pt-y p1) (pt-y p2)) 2))
    0.5))

(voronoi
 400 400
 distance
 (random-seeds 400 400 10))