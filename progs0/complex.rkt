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
         (+ (field real) (send n real))
         (+ (field imag) (send n imag))))
  
  (define/public (minus n)
    (new complex% 
         (- (field real) (send n real))
         (- (field imag) (send n imag))))
  
  (define/public (times n)
    (new complex%
         (- (* (field real) (send n real))
            (* (field imag) (send n imag)))
         (+ (* (field imag) (send n real))
            (* (field real) (send n imag)))))
  
  (define/public (div n)
    (new complex%
         (/ (+ (* (field real) (send n real))
               (* (field imag) (send n imag)))
            (+ (sqr (send n real))
               (sqr (send n imag))))
         (/ (- (* (field imag) (send n real))
               (* (field real) (send n imag)))
            (+ (sqr (send n real))
               (sqr (send n imag))))))
  
  (define/public (sq)
    (times this))
  
  ;; -> Number
  (define/public (mag)
    (sqrt (+ (sqr (field real))
             (sqr (field imag)))))
  
  (define/public (sqroot)
    (new complex% 
         (sqrt (/ (+ (mag) (field real)) 2))
         (* (sqrt (/ (- (mag) (field real)) 2))
                  (if (negative? (field imag))
                      -1
                      +1))))
  
  (define/public (to-number)
    (make-rectangular (field real)
                      (field imag))))

; Some example Complex values.
(define c-1  (new complex% -1 0))
(define c0+0 (new complex% 0 0))
(define c2+3 (new complex% 2 3))
(define c3+5 (new complex% 4 5))
; Verify the imaginary unit property.
(check-expect (send c-1 mag) 1)
(check-expect (send c-1 sqroot) 
              (new complex% 0 1))
(check-expect (send (send (send c-1 sqroot) sq) =? c-1)
              true)
(check-expect (send (send (new complex% 0 1) sq) =? c-1)
              true)
; Arithmetic on complex numbers.
(check-expect (send c0+0 =? c0+0)
              true)
(check-expect (send c0+0 =? c2+3)
              false)
(check-expect (send (send c2+3 plus c3+5) =?
                    (new complex% 6 8))
              true)
(check-expect (send (send c2+3 minus c3+5) =?
                    (new complex% -2 -2))
              true)
(check-expect (send (send c2+3 times c3+5) =?
                    (new complex% -7 22))
              true)
(check-expect (send (send c2+3 div c3+5) =?
                    (new complex% 23/41 2/41))
              true)
(check-expect (send (new complex% 3 4) mag)              
              5)



(define (random-complex)
  (new complex% 
       (- (random 100) 50)
       (- (random 100) 50)))

(define epsilon 0.00001)

#;
(for ([i (in-range 50000)])
  (let ((r (random-complex)))
    (unless (=~ (send (send r sqroot) to-number)
                (sqrt (send r to-number))
                epsilon)
      (error "BARF" i r))))

(for ([i (in-range 50000)])
  (let ((r (random-complex)))
    (unless (< (magnitude (- (send (send r sqroot) to-number)
                             (sqrt (send r to-number))))
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



