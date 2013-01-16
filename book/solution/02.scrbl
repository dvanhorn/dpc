#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct check-expect))
          (for-label (except-in class/0 define-struct check-expect)))

@title[#:tag "Home_on_the_Range_solution"]{Solution:
@secref{Home_on_the_Range}}

This is a solution for the @secref{Home_on_the_Range} exercise.

@#reader scribble/comment-reader
(racketmod
  class/0
;; A Range is one of
;; - (new range% Number Number)
;; Interp: represents the range between `lo' and `hi'
;;         including `lo', but *not* including `hi'
;; and implements IRange.

;; The IRange interface includes:
;; - in-range? : Number -> Boolean                      
;;   determine if the given number is in this range
;; - union : Range -> Range                             
;;   produce the range containing this and the given range

(define-class range%                                    
  (fields lo hi)
  (check-expect ((range% 0 1) #,dot in-range? 0) true)      
  (check-expect ((range% 0 1) #,dot in-range? 1) false)
  (define (in-range? n)                          
    (and (>= n (send this lo)) (< n (send this hi))))
    
  (define (union r)
    (union-range% this r)))

;; Part 2:

;; A Range is one of
;; - (new range% Number Number)
;;   Interp: represents the range between `lo' and `hi'
;;           including `lo', but *not* including `hi'
;; - (new hi-range% Number Number)                      
;;   Interp: represents the range between `lo' and `hi'
;;           including `hi', but *not* including `lo'
;; and implements IRange.

(define-class hi-range%                                 
  (fields lo hi)
  (check-expect ((hi-range% 0 1) #,dot in-range? 0) false)  
  (check-expect ((hi-range% 0 1) #,dot in-range? 0.5) true)
  (check-expect ((hi-range% 0 1) #,dot in-range? 1) true)
  (define (in-range? n)                          
    (and (> n (send this lo)) (<= n (send this hi))))
    
  (define (union r)
    (union-range% this r)))

;; Part 3:

;; A Range is one of
;; - (new range% Number Number)
;;   Interp: represents the range between `lo' and `hi'
;;           including `lo', but *not* including `hi'
;; - (new hi-range% Number Number)                      
;;   Interp: represents the range between `lo' and `hi'
;;           including `hi', but *not* including `lo'
;; - (new union-range% Range Range)                     
;;   Interp: including all the numbers in both ranges
;; and implements IRange.

(define-class union-range%                              
  (fields left right)
  (define (in-range? n)                          
    (or ((send this left) #,dot in-range? n)
        ((send this right) #,dot in-range? n)))

  (define (union r)                              
    (union-range% this r)))

(define r1 (range% 0 1))
(define r2 (hi-range% 0 1))
(define r3 (range% 2 4))

;; union + in-range? test                               
(check-expect (r1 #,dot union r2 #,dot union r3 #,dot in-range? 3) 
              true)

;; testing all the union methods                        
(check-expect (r1 #,dot union r2 #,dot in-range? 1) true)
(check-expect (r2 #,dot union r1 #,dot in-range? 0) true)

)