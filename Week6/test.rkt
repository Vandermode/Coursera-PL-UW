; Programming Languages, Dan Grossman
; Section 5: Racket Introduction

; always make this the first (non-comment, non-blank) line of your file
#lang racket

; not needed here, but a workaround so we could write tests in a second file
; see getting-started-with-Racket instructions for more explanation
(provide (all-defined-out))

; basic definitions
(define s "hello")

(define x 3) ; val x = 3
(define y (+ x 2)) ; + is function, call it here

(define cube1
  (lambda (x)
    (* x (* x x)))) ; x * (x * x)

(define cube2
  (lambda (x)
    (* x x x)))

; syntax sugar for cube2
(define (cube3 x)
  (* x x x))

(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

(define (pow2 x)
  (lambda (y)
    (pow1 x y)))

; sum all the numbers in a list
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))));

; append
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

; map
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f (cdr xs)))))

; factorial
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

; Dynamic Typing
(define xs (list 4 5 6))
(define ys (list (list 4 5) (list 4 6) 4 5))
(define zs (list #f "hi" 3))

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))

(define (sum2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum2 (cdr xs)))
          (if (list? (car xs))
              (+ (sum2 (car xs)) (sum2 (cdr xs)))
              (sum2 (cdr xs))))))

(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [(list? (car xs)) (+ (sum3 (car xs)) (sum3 (cdr xs)))]
        [#f (sum3 (cdr xs))]))
  
(define (count-false xs)
  (cond [(null? xs) 0]
        [(car xs) (count-false (cdr xs))]
        [#t (+ 1 (count-false (cdr xs)))]))

; local binding
(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list is empty")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

(define (silly-double x)
  (let ([x (+ x 3)]
        [y (+ x 2)])
    (+ x y -5)))

(define (silly-double2 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)])
    (+ x y -8)))

(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda(z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))

(define (silly-mod2 x)
  (letrec ([even? (lambda(x) (if (zero? x) #t (odd? (- x 1))))]
           [odd? (lambda(x) (if (zero? x) #f (even? (- x 1))))])
    (if (even? x) 0 1)))

(define (my-if-bad e1 e2 e3)
  (if e1 e2 e3))

; not terminate
(define (fact-bad x)
  (my-if-bad (= x 0)
             1
             (* x (fact-bad (- x 1)))))

; e2 and e3 should be zero-argument functions (delays evaluation)
(define (my-if-ok e1 e2 e3)
  (if e1 (e2) (e3)))

(define (fact-ok x)
  (my-if-ok (= x 0)
            (lambda() 1)
            (lambda() (* x (fact-ok (- x 1))))))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

; define stream
; 1 1 1 1 1...
(define ones (lambda () (cons 1 ones)))

; 1 2 3 4 5...
(define (acc x) (lambda () (cons x (acc (+ x 1)))))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; 2 4 8 16...
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 1))))

(define (fib1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fib1 (- x 1))
         (fib1 (- x 2)))))

(define fib2
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                       1
                                       (+ (f (- x 1))
                                          (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))
    
                        
                  
                      