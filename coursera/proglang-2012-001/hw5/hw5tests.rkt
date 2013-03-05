#lang racket

(require "hw5.rkt")

(define lst_rkt
  (list (int 3) (int 4) (int 9)))

(define lst_mupl
  (apair (int 30) (apair (int 40) (apair (int 90) (aunit)))))

; test for racketlist->mupllist
(racketlist->mupllist lst_rkt)

; test for mupllist->racketlist
(mupllist->racketlist lst_mupl)
(mupllist->racketlist (racketlist->mupllist lst_rkt))

; test for ifaunit
(eval-exp 
  (ifaunit (int 2) (int 3) (aunit)))

; test for add
(eval-exp 
  (add (int 2000) (int 13)))

; test for mlet*
(define test_mlet*
  (mlet* (list (cons "_x" (int 33)) (cons "_y" (int 66))) 
	 (add (var "_x") (var "_y"))))
test_mlet*
(eval-exp test_mlet*)

; test for ifgreater
(eval-exp (ifgreater (int 0) (int 7) (int 1) (int -1)))
(eval-exp (ifgreater (int 7) (int 0) (int 1) (int -1)))
;(eval-exp (ifgreater (int 7) (aunit) (int 1) (int -1)))

; test for ifeq
(eval-exp (ifeq (int 0) (int 5) (int 2) (int -2)))
(eval-exp (ifeq (int 5) (int 0) (int 2) (int -2)))
(eval-exp (ifeq (int 5) (int 5) (int 2) (int -2)))

; test for mupl-map
(define add7
  (fun #f "x"
       (add (var "x") (int 7))))
(eval-exp (call add7 (int 13)))

(define map-add7
  (call mupl-map add7))
(eval-exp map-add7)

(define mapped
  (eval-exp (call map-add7 (racketlist->mupllist lst_rkt))))
mapped
(mupllist->racketlist mapped)

; test for mupl-mapAddN
(eval-exp (call (call mupl-mapAddN (int 77))
		(racketlist->mupllist lst_rkt)))


; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
    (eval-exp (call (call mupl-mapAddN (int 7))
		    (racketlist->mupllist lst_rkt)))))
test1

; test for compute-free-vars
;(compute-free-vars (mlet "v1" (add (int 1) (int 2)) (aunit)))
(printf "TEST compute-free-vars:~n~n")

(define olde
  (mlet "v1" (add (int 11) (int 22))
	(mlet "v2" (add (var "v1") (int 33))
	      (call (fun "f1" "a1"
			 (ifgreater (var "v1") (var "a1")
				    (mlet "v1" (add (var "v1") (var "a1"))
					  (call (var "f1") (var "v1")))
				    (call (fun #f "a2"
					       (ifgreater (var "v2") (var "a2")
							  (call (var "f1") (var "a1"))
							  (aunit)))
					  (var "v2"))))
		    (var "v1")))))
(define newe (compute-free-vars olde))
newe

; for eval-exp-c
(printf "TEST eval-exp-c:~n~n")
(eval-exp-c olde)
(equal? (eval-exp olde)
	(eval-exp-c olde))
(define test2
  (mupllist->racketlist
    (eval-exp (call (call mupl-mapAddN (int 7))
		    (racketlist->mupllist lst_rkt)))))
(equal? test1 test2)
