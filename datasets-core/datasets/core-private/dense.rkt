#lang racket/base
(require (only-in racket/flonum fl* fl+ fl- fl/ flsqrt)
         (only-in racket/vector vector-map))
(provide draw-rows draw-vector map-rows rows->columns select-columns permute linspace row-times
         rows-times orthonormal-columns)

(define (draw-vector n draw) (for/vector #:length n ([_ (in-range n)]) (draw)))

(define (draw-rows m n draw) (draw-vector m (λ () (draw-vector n draw))))

(define (select-columns rows indices)
  (for/list ([j indices])
    (for/vector #:length (vector-length rows) ([row (in-vector rows)]) (vector-ref row j))))

(define (rows->columns rows n) (select-columns rows (in-range n)))

(define (permute v order)
  (for/vector #:length (vector-length order) ([i (in-vector order)]) (vector-ref v i)))

(define (linspace start stop count #:endpoint? endpoint?)
  (define divisions (if endpoint? (sub1 count) count))
  (define step (if (positive? divisions) (fl/ (fl- stop start) (exact->inexact divisions)) 0.0))
  (for/vector #:length count ([i (in-range count)])
    (if (and endpoint? (> count 1) (= i (sub1 count)))
        stop
        (fl+ (fl* (exact->inexact i) step) start))))

(define (row-dot u v)
  (for/fold ([s 0.0]) ([a (in-vector u)] [b (in-vector v)]) (fl+ s (fl* a b))))

(define (row-times row b width)
  (for/vector #:length width ([k (in-range width)])
    (for/fold ([s 0.0]) ([x (in-vector row)] [b-row (in-vector b)])
      (fl+ s (fl* x (vector-ref b-row k))))))

(define (rows-times rows b width) (map-rows (λ (row) (row-times row b width)) rows))

(define (map-rows f rows)
  (for/vector #:length (vector-length rows) ([row (in-vector rows)]) (f row)))

(define (orthonormal-columns columns)
  (for/fold ([q '()] #:result (reverse q)) ([column (in-list columns)])
    (define (project-out v)
      (for/fold ([v v]) ([e (in-list q)])
        (define c (row-dot e v))
        (vector-map (λ (x y) (fl- x (fl* c y))) v e)))
    (define v (project-out (project-out column)))
    (define norm (flsqrt (row-dot v v)))
    (cons (vector-map (λ (x) (fl/ x norm)) v) q)))
