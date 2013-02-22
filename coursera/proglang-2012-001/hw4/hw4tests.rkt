#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 0 5 1))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-n-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-zero-only)
  (place-repeatedly (open-window) 0.5 (stream-add-zero dan-then-dog) 27))


;; My Tests
(equal? nums (list 0 1 2 3 4 5))
(equal? (sequence 0 5 2) (list 0 2 4))
(equal? (sequence 0 5 5) (list 0 5))
(equal? (sequence 0 5 6) (list 0))
(equal? (sequence 0 0 1) (list 0))
(equal? (sequence 3 2 1) (list))

files
(equal? (string-append-map (list) ".jpg") (list))

(equal? funny-test (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16))

;(one-visual-test)

;(visual-zero-only)

(define vec1 (vector (cons 1 2) #t (cons (cons 9 8) 7) 6 "hi" (cons "ok" #f)))
(equal? (vector-assoc 1 vec1) (cons 1 2))
(equal? (vector-assoc (cons 9 8) vec1) (cons (cons 9 8) 7))
(equal? (vector-assoc (cons 9 7) vec1) #f)

(define vec2 (vector (cons 1 2) (cons #t 3) (cons (cons 9 8) 7) (cons 6 "hi") (cons "ok" #f)))
(define lst2 (vector->list vec2))
(define f10 (cached-assoc lst2 2))
(f10 1)
(f10 1)
(f10 2)
(f10 (cons 9 8))
(f10 (cons 9 8))
(f10 1)
(f10 (cons 9 7))
(f10 "ok")
(f10 "ok")
(f10 (cons 9 8))
(f10 1)
(f10 "ok")
(f10 (cons 9 8))

(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))

