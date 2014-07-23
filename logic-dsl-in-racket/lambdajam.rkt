#lang racket
(provide (all-defined-out))

(define (var n) n)

(define (var? n) (number? n))

;; (define (ext-s x v s) (cons (cons x v) s))
(define (ext-s x v s)
  (and (not (occur? x v s)) (cons (cons x v) s)))

(define (occur? x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (eqv? x v))
     ((pair? v) (or (occur? x (car v) s)
                    (occur? x (cdr v) s)))
     (else #f))))

(define (walk u s)
  (let ((pr (and (var? u) (assv u s))))
    (if pr (walk (cdr pr) s) u)))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
     ((eqv? u v) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else #f))))

(define (== u v)
  ;; goal:: state->[state*]
  (lambda (s/c) ;; <- a state
    (let ((s (unify u v (car s/c))))
      (if s (list (cons s (cdr s/c))) '()))))

(define (call/empty-state g)
  (g (cons '() 0)))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (add1 c))))))

(define (disj g1 g2)
  (lambda (s/c) ($append (g1 s/c) (g2 s/c))))

(define (conj g1 g2)
  (lambda (s/c) ($append-map g2 (g1 s/c))))

;; stream -> stream -> stream
(define ($append $1 $2)
  (cond
   ((procedure? $1) (lambda () ($append $2 ($1))))
   ((null? $1) $2)
   (else (cons (car $1) ($append (cdr $1) $2)))))

;; (state -> [state]) -> [state] -> state
(define ($append-map g $)
  (cond
   ((procedure? $)
    (lambda () ($append-map g ($))))
   ((null? $) '())
   (else ($append (g (car $)) ($append-map g (cdr $))))))

(define (appo l s o)
  (disj
   (conj (== 1 '()) (== s o))
   (call/fresh
   (lambda (a)
     (call/fresh
      (lambda (d)
        (conj
         (== (cons a d) 1)
         (call/fresh
          (lambda (res)
            (conj
             (== (cons a res) o)
             (lambda (s/c)
               (lambda ()
                 ((appo d s res) s/c)))))))))))))

(define (hot-dogs meal)
  (disj
   (== meal 'dog)
   (call/fresh
    (lambda (res)
      (conj
       (== (cons 'hot res) meal)
       (hot-dogs res))))))

(define (hot-dogs meal)
  (disj
   (== meal 'dog)
   (call/fresh
    (lambda (res)
      (conj
       (== (cons 'hot res) meal)
       (lambda (s/c)
         (lambda ()
           ((hot-dogs res) s/c))))))))

(define (hot-dogs meal)
  (disj
   (== meal 'dog)
   (call/fresh
    (lambda (res)
      (conj
       (lambda (s/c)
         (lambda ()
           ((hot-dogs res) s/c)))
       (== (cons 'hot res) meal))))))
