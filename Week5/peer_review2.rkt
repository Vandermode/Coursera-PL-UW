#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> 0 (- high low)) '()]
        [#t (cons low (sequence (+ low stride) high stride))]))



(define (string-append-map l suffix)
  (map (λ (s) (string-append s suffix))
       l ))

(define (list-nth-mod l n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? l) (error "list-nth-mod: empty list")]
        [#t (car (list-tail l (remainder n (length l))))]))


;; For testing
(define (ones)
  (letrec ([f (λ (x)
                (cons x (λ () (f x))))])
    (f 1)))

                
(define (stream-for-n-steps s n)
  (let ([val (car (s))]
        [func (cdr (s))])
    (cond [(= n 0) null]
          [#t (cons
               val
               (stream-for-n-steps
                func
                (- n 1)))])))


(define (funny-number-stream)
  (letrec ([f (λ (x)
                (cons (if (= 0 (remainder x 5))
                          (- 0 x)
                          x)
                      (λ () (f (+ 1 x)))))])
    (f 1)))


(define (dan-then-dog)
  (letrec ([f (λ (x)
                (cons (if (= 0 (remainder x 2))
                          "dan.jpg"
                          "dog.jpg")
                      (λ ()
                        (f (+ x 1)))))])
    (f 0)))


(define (stream-add-zero s)
  (λ ()
    (cons (cons 0
                (car (s)))
          (stream-add-zero (cdr (s))))))
  
              
(define (cycle-lists xl yl)
  (letrec ([f (λ (x)
                (cons  ;; (pair, #proc)
                 (cons
                  (list-nth-mod xl x)
                  (list-nth-mod yl x))
                 (λ () (f (+ x 1)))))])
    (λ () (f 0))))

(define lx '(0 1 2 3 4))
(define ly '(3 2 1))
(define f (cycle-lists lx ly))
              
(define (vector-assoc x vec)
  (letrec ([len (vector-length vec)]
           [f (λ (n)
                (if (= n len)
                    #f
                    (let ([curr (vector-ref vec n)])
                      (cond
                        [(not (pair? curr)) (f (+ n 1))]
                        [(equal? x (car curr)) curr]
                        [#t (f (+ n 1))]))))])
    (f 0)))

(define tv (vector (cons 1 12) (cons 2 12)))
(define tv0 (vector (cons 1 12) (cons 2 12) (cons 3 12) (cons 4 12)))
(define tl (list (cons 1 12) (cons 2 12) (cons 3 12) (cons 4 12)))




(define (++ n) (+ n 1))
(define mod remainder)

(define (cached-assoc lx n)
  (letrec ([cache (make-vector n #f)]
           [cache-index 0]
           [f (λ (v)
                (begin
                  ;; Check the cache
                  (print '("Checking cache for val: " v))
                  (define result (vector-assoc v cache))
                  (if result
                      result
                      (begin
                         (print '("Value " v " not found in cache."))
                         
                         ;; Put value in the cache
                         (vector-set! cache cache-index v)
                         
                         ;; Increment the index, wrap value within n
                         (set! cache-index (mod (++ cache-index) n))
                         (assoc v lx)))))])
    (if (null? lx)
        #f
        (λ (v) (f v)))))
                      

                  
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([r1 e1]
              [f (λ ()
                   (let ([r2 e2])
                     (if (< r1 r2)
                     (f)
                     #t)))])
       (f))]))
