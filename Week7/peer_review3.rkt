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

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist vs)
  (if (empty? vs)
      (aunit)
      (apair (car vs) (racketlist->mupllist (cdr vs)))))

(define (mupllist->racketlist vs)
  (if (aunit? vs)
      null
      (cons (apair-e1 vs) (mupllist->racketlist (apair-e2 vs)))))

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
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let* ([var (mlet-var e)]
                [v (eval-under-env (mlet-e e) env)]
                [new-env (cons (cons var v) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MULP fst applied to a non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MULP snd applied to a non-pair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [a (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (let* ([f (closure-fun c)]
                      [f-name (fun-nameopt f)]
                      [env-w-arg (cons (cons (fun-formal  f) a) (closure-env c))])
                 (if (eq? f-name #f)
                     (eval-under-env (fun-body f) env-w-arg)
                     (eval-under-env (fun-body f) (cons (cons f-name c) env-w-arg))))        
               (error "MUPL can call only a function")))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lst e2)
  (if (null? lst)
      e2
      (mlet (car (car lst)) (cdr (car lst)) (mlet* (cdr lst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun "map" "f"
       (fun "iterate" "l"
            (ifaunit (var "l")
                     (aunit)
                     (apair (call (var "f") (fst (var "l")))
                            (call (var "iterate") (snd (var "l"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (fun  #f "l"
                   (call (call (var "map") (fun #f "x" (add (var "x") (var "i")))) (var "l"))))))
        
;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (define (collect-free-vars free-vars e)
    (cond [(var? e) (set-add free-vars (var-string e))]
          [(add? e) (collect-free-vars (collect-free-vars free-vars (add-e1 e)) (add-e2 e))]
          [(int? e) free-vars]
          [(ifgreater? e)
           (collect-free-vars (collect-free-vars (collect-free-vars (collect-free-vars free-vars (ifgreater-e1 e)) (ifgreater-e2 e)) (ifgreater-e3 e)) (ifgreater-e4 e))]
          [(mlet? e) (set-remove (collect-free-vars (collect-free-vars free-vars (mlet-body e)) (mlet-e e)) (mlet-var e))]
          [(apair? e) (collect-free-vars (collect-free-vars free-vars (apair-e1 e)) (apair-e2 e))]
          [(fst? e) (collect-free-vars free-vars (fst-e e))]
          [(snd? e) (collect-free-vars free-vars (snd-e e))]
          [(aunit? e) free-vars]
          [(isaunit? e) (collect-free-vars free-vars (isaunit-e e))]
          [(fun? e) (set-remove (set-remove (collect-free-vars free-vars (fun-body e)) (fun-nameopt e)) (fun-formal e))]
          [(call? e) (collect-free-vars (collect-free-vars free-vars (call-funexp e)) (call-actual e))]
          [(closure? e) (collect-free-vars free-vars (closure-fun e))]
          [#t free-vars]))
  (define (replace-e e)
    (cond [(var? e) e]
          [(add? e) (add (replace-e (add-e1 e)) (replace-e (add-e2 e)))]
          [(int? e) e]
          [(ifgreater? e)
           (ifgreater (replace-e (ifgreater-e1 e)) (replace-e (ifgreater-e2 e))
                      (replace-e (ifgreater-e3 e)) (replace-e (ifgreater-e4 e)))]
          [(mlet? e) (mlet (mlet-var e) (replace-e (mlet-e e)) (replace-e (mlet-body e)))]
          [(apair? e) (apair (replace-e (apair-e1 e)) (replace-e (apair-e2 e)))]
          [(fst? e) (fst (replace-e (fst-e e)))]
          [(snd? e) (snd (replace-e (snd-e e)))]
          [(aunit? e) e]
          [(isaunit? e) (isaunit (replace-e (isaunit-e e)))]
          [(fun? e) (fun-challenge (fun-nameopt e) (replace-e (fun-formal e)) (replace-e (fun-body e)) (collect-free-vars (set) e))]
          [(call? e) (call (replace-e (call-funexp e)) (replace-e (call-actual e)))]
          [(closure? e) (closure (closure-env e) (replace-e (closure-fun e)))]
          [#t e]))
  (replace-e e))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
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
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let* ([var (mlet-var e)]
                [v (eval-under-env-c (mlet-e e) env)]
                [new-env (cons (cons var v) env)])
           (eval-under-env-c (mlet-body e) new-env))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MULP fst applied to a non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MULP snd applied to a non-pair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(fun-challenge? e)
         (let ([vars (fun-challenge-freevars e)])
           (closure (filter (lambda (pr) (set-member? vars (car pr))) env) e))]
        [(call? e)
         (let ([c (eval-under-env-c (call-funexp e) env)]
               [a (eval-under-env-c (call-actual e) env)])
           (if (closure? c)
               (let* ([f (closure-fun c)]
                      [f-name (fun-challenge-nameopt f)]
                      [env-w-arg (cons (cons (fun-challenge-formal  f) a) (closure-env c))])
                 (if (eq? f-name #f)
                     (eval-under-env-c (fun-challenge-body f) env-w-arg)
                     (eval-under-env-c (fun-challenge-body f) (cons (cons f-name c) env-w-arg))))        
               (error "MUPL can call only a function")))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
