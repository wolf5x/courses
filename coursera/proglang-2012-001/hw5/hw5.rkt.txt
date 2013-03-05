;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist l)
  (cond [(null? l) (aunit)]
	[(pair? l) (apair (car l) (racketlist->mupllist (cdr l)))]
	[#t (error "bad racket list")]))

(define (mupllist->racketlist l)
  (cond [(aunit? l) null]
	[(apair? l) (cons (apair-e1 l) (mupllist->racketlist (apair-e2 l)))]
	[#t (error "bad MUPL list")]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
	[(equal? (car (car env)) str) (cdr (car env))]
	[#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
	 (envlookup env (var-string e))]
	[(add? e) 
	 (let ([v1 (eval-under-env (add-e1 e) env)]
	       [v2 (eval-under-env (add-e2 e) env)])
	   (if (and (int? v1)
		    (int? v2))
	     (int (+ (int-num v1) 
		     (int-num v2)))
	     (error "MUPL addition applied to non-number")))]
	;; CHANGE add more cases here
	[(int? e) 
	 (if (integer? (int-num e))
	   e
	   (error "MUPL int-num must be a number"))]
	[(fun? e) 
	 (if (and (or (false? (fun-nameopt e)) (string? (fun-nameopt e)))
		  (string? (fun-formal e)))
	   (closure env e)
	   (error "MUPL fun's args type invalid"))]
	[(ifgreater? e)
	 (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
	       [v2 (eval-under-env (ifgreater-e2 e) env)])
	   (if (and (int? v1) (int? v2))
	     (if (> (int-num v1) (int-num v2)) 
	       (eval-under-env (ifgreater-e3 e) env)
	       (eval-under-env (ifgreater-e4 e) env))
	     (error "MUPL ifgreater applied to non-number")))]
	[(mlet? e)
	 (let ([s (mlet-var e)]
	       [v (eval-under-env (mlet-e e) env)])
	   (if (string? s)
	     (eval-under-env (mlet-body e) (cons (cons s v) env))
	     (error "MUPL mlet-var must be a string")))]
	[(apair? e)
	 (apair (eval-under-env (apair-e1 e) env)
		(eval-under-env (apair-e2 e) env))]
	[(fst? e)
	 (let ([v (eval-under-env (fst-e e) env)])
	   (if (apair? v)
	     (apair-e1 v)
	     (error "MUPL fst applied to non-pair")))]
	[(snd? e)
	 (let ([v (eval-under-env (snd-e e) env)])
	   (if (apair? v)
	     (apair-e2 v)
	     (error "MUPL snd applied to non-pair")))]
	[(aunit? e) e]
	[(isaunit? e)
	 (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
	[(call? e)
	 (let ([cl (eval-under-env (call-funexp e) env)]
	       [arg (eval-under-env (call-actual e) env)])
	   (if (closure? cl)
	     (letrec ([cfun (closure-fun cl)]
		      [s1 (fun-nameopt cfun)]
		      [benv (cons (cons (fun-formal cfun) arg)
				  (closure-env cl))])
	       (eval-under-env (fun-body cfun)
			       (cond 
				 [(string? s1) (cons (cons s1 cl) benv)]
				 [(false? s1) benv])))
	     (error "MUPL call applied to non-closure")))]
	[(closure? e) e]
	[#t (error "bad MUPL expression")]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

; tricky and ugly
(define (ifaunit e1 e2 e3) 
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) 
  (cond 
    [(null? lstlst) e2]
    [(list? lstlst) (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))]))

(define (ifeq e1 e2 e3 e4) 
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
	 (ifgreater (var "_x") (var "_y") 
		    e4
		    (ifgreater (var "_y") (var "_x") 
			       e4
			       e3))))

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "lambda" "l"
	    (ifaunit (var "l")
		     (aunit)
		     (apair (call (var "f") (fst (var "l"))) 
			    (call (var "lambda") (snd (var "l"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
	(fun #f "i"
	     (call (var "map")
		   (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec 
    ([helper 
       ; Return a pair holding the transformed exp and its free-var set.
       ; Doesn't typecheck the struct members.
       (lambda (e)
	 (cond
	   [(var? e) 
	    (cons e (set (var-string e)))]
	   [(int? e) 
	    (cons e (set))]
	   [(add? e)
	    (let ([r1 (helper (add-e1 e))]
		  [r2 (helper (add-e2 e))])
	      (cons (add (car r1) (car r2))
		    (set-union (cdr r1) (cdr r2))))]
	   [(ifgreater? e)
	    (let ([r1 (helper (ifgreater-e1 e))]
		  [r2 (helper (ifgreater-e2 e))]
		  [r3 (helper (ifgreater-e3 e))]
		  [r4 (helper (ifgreater-e4 e))])
	      (cons (ifgreater (car r1) (car r2) (car r3) (car r4))
		    (set-union (cdr r1) (cdr r2) (cdr r3) (cdr r4))))]
	   [(fun? e)
	    (letrec ([s1 (fun-nameopt e)]
		     [s2 (fun-formal e)]
		     [r (helper (fun-body e))]
		     [fvs (set-remove (set-remove (cdr r) s1) s2)])
	      (cons (fun-challenge s1 s2 (car r) fvs) 
		    fvs))]
	   [(call? e)
	    (let ([r1 (helper (call-funexp e))]
		  [r2 (helper (call-actual e))])
	      (cons (call (car r1) (car r2))
		    (set-union (cdr r1) (cdr r2))))]
	   [(mlet? e)
	    (let ([s (mlet-var e)]
		  [r1 (helper (mlet-e e))]
		  [r2 (helper (mlet-body e))])
	      (cons (mlet s (car r1) (car r2))
		    (set-remove (set-union (cdr r1) (cdr r2)) s)))]
	   [(apair? e)
	    (let ([r1 (helper (apair-e1 e))]
		  [r2 (helper (apair-e2 e))])
	      (cons (apair (car r1) (car r2))
		    (set-union (cdr r1) (cdr r2))))]
	   [(fst? e)
	    (let ([r (helper (fst-e e))])
	      (cons (fst (car r))
		    (cdr r)))]
	   [(snd? e)
	    (let ([r (helper (snd-e e))])
	      (cons (snd (car r))
		    (cdr r)))]
	   [(aunit? e)
	    (cons e (set))]
	   [(isaunit? e)
	    (let ([r (helper (isaunit-e e))])
	      (cons (isaunit-e (car r))
		    (cdr r)))]
	   [#t (error "bad MUPL expression to compute free-vars")]))])
    (car (helper e))))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes

; Placed outside for testing.
; This could be a local function inside eval-under-env-c.
(define (build-free-var-env st env)
  (cond 
    [(set-empty? st) null]
    [(null? env) (error "unbound free-vars in env" st)]
    [(set-member? st (caar env))
     (cons (car env) 
	   (build-free-var-env (set-remove st (caar env))
			       (cdr env)))]
    [#t (build-free-var-env st (cdr env))]))

; Compare to the previous eval-under-env,
; we only need to modify the 'fun' case to the 'fun-challenge' case, 
; and reimplement the 'call' case.
(define (eval-under-env-c e env)
  (cond [(var? e) 
	 (envlookup env (var-string e))]
	[(add? e) 
	 (let ([v1 (eval-under-env-c (add-e1 e) env)]
	       [v2 (eval-under-env-c (add-e2 e) env)])
	   (if (and (int? v1)
		    (int? v2))
	     (int (+ (int-num v1) 
		     (int-num v2)))
	     (error "MUPL addition applied to non-number")))]
	;; CHANGE add more cases here
	[(int? e) 
	 (if (integer? (int-num e))
	   e
	   (error "MUPL int-num must be a number"))]
	[(fun-challenge? e) 
	 (if (and (or (false? (fun-challenge-nameopt e)) (string? (fun-challenge-nameopt e)))
		  (string? (fun-challenge-formal e)))
	   (closure (build-free-var-env (fun-challenge-freevars e) env) 
		    e)
	   (error "MUPL fun's args type invalid"))]
	[(ifgreater? e)
	 (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
	       [v2 (eval-under-env-c (ifgreater-e2 e) env)])
	   (if (and (int? v1) (int? v2))
	     (if (> (int-num v1) (int-num v2)) 
	       (eval-under-env-c (ifgreater-e3 e) env)
	       (eval-under-env-c (ifgreater-e4 e) env))
	     (error "MUPL ifgreater applied to non-number")))]
	[(mlet? e)
	 (let ([s (mlet-var e)]
	       [v (eval-under-env-c (mlet-e e) env)])
	   (if (string? s)
	     (eval-under-env-c (mlet-body e) (cons (cons s v) env))
	     (error "MUPL mlet-var must be a string")))]
	[(apair? e)
	 (apair (eval-under-env-c (apair-e1 e) env)
		(eval-under-env-c (apair-e2 e) env))]
	[(fst? e)
	 (let ([v (eval-under-env-c (fst-e e) env)])
	   (if (apair? v)
	     (apair-e1 v)
	     (error "MUPL fst applied to non-pair")))]
	[(snd? e)
	 (let ([v (eval-under-env-c (snd-e e) env)])
	   (if (apair? v)
	     (apair-e2 v)
	     (error "MUPL snd applied to non-pair")))]
	[(aunit? e) e]
	[(isaunit? e)
	 (if (aunit? (eval-under-env-c (isaunit-e e) env)) (int 1) (int 0))]
	[(call? e)
	 (let ([cl (eval-under-env-c (call-funexp e) env)]
	       [arg (eval-under-env-c (call-actual e) env)])
	   (if (closure? cl)
	     (letrec ([cfun (closure-fun cl)]
		      [s1 (fun-challenge-nameopt cfun)]
		      [benv (cons (cons (fun-challenge-formal cfun) arg)
				  (closure-env cl))])
	       (eval-under-env-c (fun-challenge-body cfun)
				 (cond 
				   [(string? s1) (cons (cons s1 cl) benv)]
				   [(false? s1) benv])))
	     (error "MUPL call applied to non-closure")))]
	[(closure? e) e]
	[#t (error "bad MUPL expression")]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
