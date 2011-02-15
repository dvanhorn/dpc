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

(define-class range%
  (fields lo hi)
  (check-expect ((range% 0 1) . in-range? 0) true)
  (check-expect ((range% 0 1) . in-range? 0.5) true)
  (check-expect ((range% 0 1) . in-range? 1) false)
  (check-expect ((range% 0 1) . in-range? 2) false)
  (check-expect ((range% 0 1) . in-range? -2) false)
  (define/public (in-range? n)
    (and (>= n (field lo)) (< n (field hi))))
    
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
    
  (define/public (union r)
    (union-range% this r)))



(define-class union-range%
  (fields left right)
  (define/public (in-range? n)
    (or ((field left) . in-range? n)
        ((field right) . in-range? n)))
  (define/public (union r)
    (union-range% this r)))

(define r1 (range% 0 1))
(define r2 (hi-range% 0 1))
(define r3 (range% 2 4))

(check-expect (r1 . union r2 . union r3 . in-range? 3) true)

(check-expect (r1 . union r2 . in-range? 1) true)
(check-expect (r1 . union r2 . in-range? 0) true)
