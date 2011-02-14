#lang class2

;; A Range is one of
;; - (new range% Number Number)
;; Interp: represents the range between `lo' and `hi'
;;         including `lo', but *not* including `hi'

;; - (new hi-range% Number Number)
;; Interp: represents the range between `lo' and `hi'
;;         including `hi', but *not* including `lo'

;; - (new union-range% Range Range)
;; Interp: including all the numbers in both ranges


;; and implements IRange:

;; The IRange interface includes:
;; - in-range? : Number -> Boolean
;; determine if the given number is in this range
;; - overlap? : Range -> Boolean 
;; determine if the given range overlaps with this range
;; - largest-integer : Range -> Number or False
;; find the largest integer included in the range, or false if no integers are included.

(define-class range%
  (fields lo hi)
  (check-expect ((range% 0 1) . in-range? 0) true)
  (check-expect ((range% 0 1) . in-range? 0.5) true)
  (check-expect ((range% 0 1) . in-range? 1) false)
  (check-expect ((range% 0 1) . in-range? 2) false)
  (check-expect ((range% 0 1) . in-range? -2) false)
  (define/public (in-range? n)
    (and (>= n (field lo)) (< n (field hi))))
  
  (check-expect ((range% 2 3) . overlap? (range% 1 2)) false)
  (check-expect ((range% 2 3) . overlap? (range% 3 3)) false)
  (check-expect ((range% 2 3) . overlap? (range% 2.5 2.5)) true)
  (check-expect ((range% 2 3) . overlap? (range% 1 3)) true)
  (define/public (overlap? r)
    (or (and (< (field lo) (r . hi)) (< (r . hi) (field hi)))
        (and (<= (r . lo) (field lo)) (< (field lo) (r . hi)))))
  
  (check-expect ((range% 2 3) . largest-integer) 2)
  (check-expect ((range% 2 3.0000000001) . largest-integer) 3)
  (check-expect ((range% 2.1 3) . largest-integer) false)
  (define/public (largest-integer)
    (local [(define f (floor (field hi)))
            (define f* (cond [(= f (field hi)) (sub1 f)]
                             [else f]))]
      (cond [(>= f* (field lo)) f*]
            [else false])))
  
  (define/public (union r)
    (union-range% this r)))

(define-class hi-range%
  (fields lo hi)
  (check-expect ((hi-range% 0 1) . in-range? 0) false)
  (check-expect ((hi-range% 0 1) . in-range? 0.5) true)
  (check-expect ((hi-range% 0 1) . in-range? 1) true)
  (check-expect ((hi-range% 0 1) . in-range? 2) false)
  (check-expect ((hi-range% 0 1) . in-range? -2) false)
  (define/public (in-range? n)
    (and (> n (field lo)) (<= n (field hi))))
  
  (check-expect ((hi-range% 2 3) . largest-integer) 3)
  (check-expect ((hi-range% 2 3.0000000001) . largest-integer) 3)
  (check-expect ((hi-range% 2.1 3) . largest-integer) 3)
  (check-expect ((hi-range% 2 2.9) . largest-integer) false)
  (define/public (largest-integer)
    (local [(define f (floor (field hi)))]
      (cond [(> f (field lo)) f]
            [else false])))
  
  (define/public (union r)
    (union-range% this r)))



(define-class union-range%
  (fields left right)
  (define/public (in-range? n)
    (or ((field left) . in-range? n)
        ((field right) . in-range? n)))
  (define/public (largest-integer)
    (local [(define f1 ((field left) . largest-integer))
            (define f2 ((field right) . largest-integer))]
      (cond [(false? f1) f2]
            [(false? f2) f1]
            [else (max f1 f2)])))
  (define/public (union r)
    (union-range% this r)))

(define r1 (range% 0 1))
(define r2 (hi-range% 0 1))
(define r3 (range% 2 4))
(define r4 (hi-range% 2.1 2.2))

(check-expect (r4 . largest-integer) false)
(check-expect (r2 . union r4 . largest-integer) 1)
(check-expect (r4 . union r2 . largest-integer) 1)
(check-expect (r4 . union r4 . largest-integer) false)

(check-expect (r1 . union r2 . union r3 . in-range? 3) true)

(check-expect (r1 . union r2 . in-range? 1) true)
(check-expect (r1 . union r2 . in-range? 0) true)
(check-expect (r1 . union r2 . largest-integer) 1)