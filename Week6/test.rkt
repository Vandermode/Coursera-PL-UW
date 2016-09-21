; Programming Languages, Dan Grossman
; Section 6: Datatype Programming in Racket Without Structs

#lang racket
(provide (all-defined-out))

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)
(struct bool (b) #:transparent)
(struct eq-num (e1 e2) #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)

(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e)
         (let ([v (eval-exp (negate-e e))])
           (if (const? v)
               (const (- (const-int v)))
               (error "negate applied to non-number")))]
        [(add? e) (let ([v1 (eval-exp (add-e1 e))]
                        [v2 (eval-exp (add-e2 e))])
                    (if (and (const? v1) (const? v2))
                        (const (+ (const-int v1) (const-int v2)))
                        (error "add applied to non-number")))]                    
        [(multiply? e) (let ([v1 (eval-exp (multiply-e1 e))]
                             [v2 (eval-exp (multiply-e2 e))])
                         (if (and (const? v1) (const? v2))
                             (const (* (const-int v1) (const-int v2)))
                             (error "mutiply applied to non-number")))]
        [(bool? e) e]
        [(eq-num? e)
         (let ([v1 (eval-exp (eq-num-e1 e))]
               [v2 (eval-exp (eq-num-e2 e))])
           (if (and (const? v1) (const? v2))
               (bool (= (const-int v1) (const-int v2)))
               (error "eq-num applied to non-number")))]
        [(if-then-else? e)
         (let ([v-test (eval-exp (if-then-else-e1 e))])
           (if (bool? v-test)
               (if (bool-b v-test)
                   (eval-exp (if-then-else-e2 e))
                   (eval-exp (if-then-else-e2 e)))
               (error "if-then-else applied to non-boolean")))]               
        [#t (error "eval-exp expected an exp")]))

(define a-test (eval-exp (multiply (negate (add (const 2) (const 2))) (const 7))))

(define (andalso e1 e2)
  (if-then-else e1 e2 (bool #f)))

(define (double e)
  (multiply e (const 2)))

(define (list-product es)
  (if (null? es)
      (const 1)
      (multiply (car es) (list-product (cdr es)))))


  