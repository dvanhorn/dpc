#lang class3

(define-class a%
  (fields x)
  (define/public (x! v) 
    (set-field! x v)))

(define a (a% 1))

a


(a . x! 5)

a

(define-class author%
  (fields name books)
  (constructor (n) (fields n empty))
  (define/public (register b)
    (set-field! books (cons b (field books)))))
(define-class book%
  (fields author title)
  (constructor (a t)
               (fields a t)
               (send a register this)))

(define mf (author% "Matthias"))
mf
(define htdp (book% mf "HtDP"))
htdp mf

