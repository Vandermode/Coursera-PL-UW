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

(define (racketlist->mupllist ls)
  (cond [(null? ls) (aunit)]
        [(pair? ls) (apair (car ls) (racketlist->mupllist (cdr ls)))]
        [#t (error "Expected a racket list")]))


(define (mupllist->racketlist ls)
  (cond [(aunit? ls) null]
        [(apair? ls) (cons (apair-e1 ls) (mupllist->racketlist (apair-e2 ls)))]
        [#t (error "Expected a MUPL list")]))

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
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([a (eval-under-env (ifgreater-e1 e) env)]
             [b (eval-under-env (ifgreater-e2 e) env)])
         (if (and (int? a) (int? b))
             (eval-under-env (if (> (int-num a) (int-num b)) (ifgreater-e3 e) (ifgreater-e4 e)) env)
             (error "ifgreater expects two int-s")))]
        [(mlet? e) (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
        [(call? e)
         (let ([clo (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? clo)
               (let* ([f (closure-fun clo)]
                      [fname (fun-nameopt f)]
                      [ext-env (cons
                                (cons (fun-formal f) arg) ;; Add name of argument to environment
                                (if fname
                                    (cons (cons fname clo) (closure-env clo)) ;; Add name of function to enviroment if it is not annonymous, plus previous env
                                    (closure-env clo)))]) ;; Add closure environment
               (eval-under-env (fun-body f) ext-env))
                                                        
               (error "call expects a function as its first argument")))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([a (eval-under-env (fst-e e) env)])
           (if (apair? a) (eval-under-env (apair-e1 a) env) (error "fst expects an apair")))]
        [(snd? e)
         (let ([a (eval-under-env (snd-e e) env)])
           (if (apair? a) (eval-under-env (apair-e2 a) env) (error "snd expects an apair")))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(aunit? e) e]
        
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
  (let ([bind (car lstlst)])
    (mlet (car bind) (cdr bind) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 ;; Eval and bind e1
        (mlet "_y" e2 ;; Eval and bind 2
              (ifgreater (var "_x") (var "_y") e4 ;; if _x > _y, not equal, e4
                         (ifgreater (var "_y") (var "_x") e4 e3))))) ;; if _y > _x, not equal, e4, else equal, e3

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "map-recursive" "xs"
            (ifaunit (var "xs") (aunit) (apair (call (var "f") (fst (var "xs"))) (call (var "map-recursive") (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
       (fun #f "i" (fun #f "ints" (call (call (var "map") (fun #f "int" (add (var "int") (var "i")))) (var "ints"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
