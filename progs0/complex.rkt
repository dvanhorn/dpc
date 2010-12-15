#lang class0
(require (only-in racket for in-range unless))

(define-class complex%
  (fields real imag)
  
  (define/public (=? n)
    (and (= (field real)
            (send n real))
         (= (field imag)
            (send n imag))))
    
  (define/public (plus n)
    (new complex% 
         [real (+ (field real) (send n real))]
         [imag (+ (field imag) (send n imag))]))
    
  (define/public (minus n)
    (new complex% 
         [real (- (field real) (send n real))]
         [imag (- (field imag) (send n imag))]))
    
  (define/public (times n)
    (new complex%
         [real (- (* (field real) (send n real))
                  (* (field imag) (send n imag)))]
         [imag (+ (* (field imag) (send n real))
                  (* (field real) (send n imag)))]))
  
  (define/public (div n)
    (new complex%
         [real (/ (+ (* (field real) (send n real))
                     (* (field imag) (send n imag)))
                  (+ (sqr (send n real))
                     (sqr (send n imag))))]
         [imag (/ (- (* (field imag) (send n real))
                     (* (field real) (send n imag)))
                  (+ (sqr (send n real))
                     (sqr (send n imag))))]))
    
  (define/public (sq)
    (times this))
  
  ;; -> Number
  (define/public (mag)
    (sqrt (+ (sqr (field real))
             (sqr (field imag)))))
  
  (define/public (sqroot)
    (new complex% 
         [real (sqrt (/ (+ (mag) (field real)) 2))]
         [imag (* (sqrt (/ (- (mag) (field real)) 2))
                  (if (negative? (field imag))
                      -1
                      +1))]))
  
  (define/public (to-number)
    (make-rectangular (field real)
                      (field imag))))

; Some example Complex values.
(define c-1  (new complex% [real -1] [imag 0]))
(define c0+0 (new complex% [real 0] [imag 0]))
(define c2+3 (new complex% [real 2] [imag 3]))
(define c3+5 (new complex% [real 4] [imag 5]))
; Verify the imaginary unit property.
(check-expect (send c-1 mag) 1)
(check-expect (send c-1 sqroot) 
              (new complex% 
                   [real 0]
                   [imag 1]))
(check-expect (send (send (send c-1 sqroot) sq) =? c-1)
              true)
(check-expect (send (send (new complex% [real 0] [imag 1]) sq) =? c-1)
              true)
; Arithmetic on complex numbers.
(check-expect (send c0+0 =? c0+0)
              true)
(check-expect (send c0+0 =? c2+3)
              false)
(check-expect (send (send c2+3 plus c3+5) =?
                    (new complex% [real 6] [imag 8]))
              true)
(check-expect (send (send c2+3 minus c3+5) =?
                    (new complex% [real -2] [imag -2]))
              true)
(check-expect (send (send c2+3 times c3+5) =?
                    (new complex% [real -7] [imag 22]))
              true)
(check-expect (send (send c2+3 div c3+5) =?
                    (new complex% [real 23/41] [imag 2/41]))
              true)
(check-expect (send (new complex% [real 3] [imag 4]) mag)              
              5)



(define (random-complex)
  (new complex% 
       [real (- (random 100) 50)]
       [imag (- (random 100) 50)]))

(define epsilon 0.00001)

#;
(for ([i (in-range 50000)])
  (let ((r (random-complex)))
    (unless (=~ (send (send r sqroot) to-number)
                (sqrt (send r to-number))
                epsilon)
      (error "BARF" i r))))

(for ([i (in-range 50000)])
  (let ((n (random-complex))
        (d (random-complex)))    
    (unless (zero? (send d to-number))
      (unless (= (send (send n div d) to-number)
                 (/ (send n to-number)
                    (send d to-number)))
        (error "HURL" i n d)))))



(check-within 3+4.4i 3 1)
;(=~ 3+4.4i 3 1)


