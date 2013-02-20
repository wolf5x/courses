#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; pur your code below

;; problem 1
(define (sequence low high stride)
  (cond
    [(<= low high) (cons low (sequence (+ low stride) high stride))]
    [#t null]))

;; problem 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

;; problem 3
(define (list-nth-mod lst n)
    (cond
      [(< n 0) (error "list-nth-mod: negative number")]
      [(null? lst) (error "list-nth-mod: empty list")]
      [(= (remainder n (length lst)) 0) (car lst)]
      [#t (list-nth-mod (cdr lst) (- (remainder n (length lst)) 1))]))

;; problem 4
(define (stream-for-n-steps s n)
  (cond 
    [(> n 0) (let ([pr (s)])
	       (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))]
    [#t null]))

;; problem 5
(define (funny-number-stream)
  (letrec ([f (lambda (x) 
		(cons (cond
			[(= 0 (remainder x 5)) (- 0 x)]
			[#t x])
		      (lambda () (f (+ x 1)))))])
    (f 1)))








