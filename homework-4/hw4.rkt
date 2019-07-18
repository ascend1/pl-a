#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1

(define (sequence l h s)
  (if (> l h)
      null
      (cons l (sequence (+ l s) h s))))

; 2

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; 3

(define (list-nth-mod xs n)
  (if (null? xs)
      (error "list-nth-mod: empty list")
      (if (< n 0)
          (error "list-nth-mod: negative number")
          (car (list-tail xs (remainder n (length xs)))))))

; 4

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

; 5

(define (funny-number-stream)
  (define (fns x)
    (cons (if (= (remainder x 5) 0)
              (- 0 x)
              x)
          (lambda () (fns (+ x 1)))))
  (fns 1))

; 6

(define (dan-then-dog)
  (define (dtd x)
    (cons x (lambda () (dtd (if (string=? x "dan.jpg")
                                "dog.jpg"
                                "dan.jpg")))))
  (dtd "dan.jpg"))

; 7

(define (stream-add-zero s)
  (lambda ()
    (let ([pr (s)])
      (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))

; 8

(define (cycle-lists xs ys)
  (define (cl i)
    (cons (cons (list-nth-mod xs i)
                (list-nth-mod ys i))
          (lambda () (cl (+ i 1)))))
  (lambda () (cl 0)))

; 9

(define (vector-assoc v vec)
  (define (helper i)
    (if (< i (vector-length vec))
        (let ([vi (vector-ref vec i)])
          (cond [(not (pair? vi)) (helper (+ i 1))]
                [(equal? v (car vi)) vi]
                [else (helper (+ i 1))]))
        #f))
  (helper 0))

; 10

(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define next 0)
  (lambda (v)
    (let ([cached-res (vector-assoc v cache)])
      (if cached-res
          (cdr cached-res)
          (let ([res (assoc v xs)])
            (begin (vector-set! cache next (cons v res))
                   (set! next (if (= (- (vector-length cache) next) 1)
                                  0
                                  (+ next 1)))
                   res))))))

; 11

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
              [helper (lambda () (if (< e2 v1) (helper) #t))])
      (helper))]))