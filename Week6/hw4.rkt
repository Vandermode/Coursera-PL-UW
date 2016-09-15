
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence low hi stride)
  (if (> low hi)
      null
      (cons low (sequence (+ low stride) hi stride))))

; 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix))
       xs))
         
; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; 4
(define ones (lambda () (cons 1 ones)))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next (s)])        
        (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

; 5
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))
                              
; 6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

; 7
(define (stream-add-zero s)
  (let ([next (s)])    
    (lambda () (cons (cons 0 (car next)) (stream-add-zero (cdr next))))))

; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                            (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (n)
                (if (= n len) #f                    
                    (letrec ([vs-ref (vector-ref vec n)])                  
                      (cond [(and (pair? vs-ref) (equal? (car vs-ref) v)) vs-ref]
                            [#t (f (+ n 1))]))))])
    (f 0)))
                        
; 10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [pos 0]
           [change-pos (lambda () (set! pos (remainder (+ pos 1) (vector-length memo))))]
           [f (lambda (v)
                (or (vector-assoc v memo)
                    (let ([ans (assoc v xs)])
                      (and ans
                           (begin
                             (vector-set! memo pos ans)
                             (change-pos)
                             ans)))))])
    f))
                           
