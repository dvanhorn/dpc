#lang class0
;; Rubric for each method:
;; Contract - 2pt
;; Purpose  - 1pt
;; Tests    - 2pt
;; Code     - 3pt
;; ==============
;; Subtotal - 8pt x 9 = 72pt

;; Rubric for each function:
;; Contract - 2pt
;; Purpose  - 1pt
;; Tests    - 2pt
;; Code     - 3pt
;; ==============
;; Subtotal - 8pt x 9 = 72pt

;; Overall assignment:
;; Style              - 20pt
;; Data Defn (class)  - 10pt
;; Data Defn (struct) -  5pt
;; =========================
;; Subtotal           - 35pt

;; Methods        - 72
;; Functions      - 72
;; Overall        - 35
;; ====================
;; Total          - 179pt

;; Guidelines:

;; If someone repeatedly makes the same mistake, eg:
;; Self Complex -> Complex
;; instead of
;; Complex -> Complex
;; Treat it like they did this on only half of the problems.
;; So if Self Complex -> Complex is worth 1 out of 2 contract
;; points, you'd get 9+9/2 (= 13.5 ~ 14) points for contracts
;; on methods.

;; Style points are up to the grader's discretion, but should
;; follow the style guidelines on the web page.

;; (Let us know if you have further guidelines we should add.)

;; We put some alternative solutions in; let us know if you find others.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complex Structure

;; A Complex is a (make-cpx Real Real).
;; Interp: real and imaginary parts.
(define-struct cpx (real imag))

;; Complex Complex -> Boolean
;; Is the given complex equal to this one?
(define (=? n m)
  (and (= (cpx-real n)
          (cpx-real m))
       (= (cpx-imag n)
          (cpx-imag m))))

;; Complex Complex -> Complex
;; Add the given complex to this one.
(define (plus n m)
  (make-cpx (+ (cpx-real n) (cpx-real m))
            (+ (cpx-imag n) (cpx-imag m))))

;; Complex Complex -> Complex
;; Subtract the given complex from this one.
(define (minus n m)
  (make-cpx (- (cpx-real n) (cpx-real m))
            (- (cpx-imag n) (cpx-imag m))))

;; Complex Complex -> Complex
;; Multiply the given complex by this one.
(define (times n m)
  (make-cpx (- (* (cpx-real n) (cpx-real m))
               (* (cpx-imag n) (cpx-imag m)))
            (+ (* (cpx-imag n) (cpx-real m))
               (* (cpx-real n) (cpx-imag m)))))

;; Complex Complex -> Complex
;; Divide this complex by the given one.
(define (div n m)
  (make-cpx (/ (+ (* (cpx-real n) (cpx-real m))
                  (* (cpx-imag n) (cpx-imag m)))
               (+ (sqr (cpx-real m))
                  (sqr (cpx-imag m))))
            (/ (- (* (cpx-imag n) (cpx-real m))
                  (* (cpx-real n) (cpx-imag m)))
               (+ (sqr (cpx-real m))
                  (sqr (cpx-imag m))))))

;; Complex -> Complex
;; Multiply this complex by itself.
(define (sq n)
  (times n n))

;; Complex -> Number
;; Compute the magnitude of this complex.
(define (mag n)
  (sqrt (+ (sqr (cpx-real n))
           (sqr (cpx-imag n)))))

;; Complex -> Complex
;; Compute the square root of this complex.
(define (sqroot n)
  (make-cpx (sqrt (/ (+ (mag n) (cpx-real n)) 2))
            (* (sqrt (/ (- (mag n) (cpx-real n)) 2))
               (if (negative? (cpx-imag n))
                   -1
                   +1))))

;; Complex -> Number
;; Convert this complex to a Racket complex number.
(define (to-number n)
  (+ (cpx-real n) 
     (* +i (cpx-imag n))))

;; Alternative:
;; OK, this relies on knowing about `make-rectangular', which
;; we didn't tell them about.
;; Give kudos for reading documentation.
#;
(define (to-number n)
  (make-rectangular (cpx-real n)
                    (cpx-imag n)))


; Some example Complex values.
(define cpx-1  (make-cpx -1 0))
(define cpx0+0 (make-cpx 0 0))
(define cpx2+3 (make-cpx 2 3))
(define cpx3+5 (make-cpx 4 5))
; Verify the imaginary unit property.
(check-expect (mag cpx-1) 1)
(check-expect (sqroot cpx-1) 
              (make-cpx 0 1))
(check-expect (sq (sqroot cpx-1)) cpx-1)
(check-expect (sq (make-cpx 0 1)) cpx-1)
; Arithmetic on complex numbers.
(check-expect (=? cpx0+0 cpx0+0)
              true)
(check-expect (=? cpx0+0 cpx2+3)
              false)
(check-expect (plus cpx2+3 cpx3+5)
              (make-cpx 6 8))
(check-expect (minus cpx2+3 cpx3+5) 
              (make-cpx -2 -2))
(check-expect (times cpx2+3 cpx3+5)
              (make-cpx -7 22))
(check-expect (div cpx2+3 cpx3+5)
              (make-cpx 23/41 2/41))
(check-expect (mag (make-cpx 3 4))
              5)
(check-expect (to-number cpx2+3) 2+3i)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complex Class

;; A Complex is a (new complex% Real Real).
;; Interp: real and imaginary parts.

(define-class complex%
  (fields real imag)
  
  ;; Complex -> Boolean
  ;; Is the given complex equal to this one?
  (define/public (=? n)
    (and (= (field real)
            (send n real))
         (= (field imag)
            (send n imag))))
  
  ;; Complex -> Complex
  ;; Add the given complex to this one.
  (define/public (plus n)
    (new complex% 
         (+ (field real) (send n real))
         (+ (field imag) (send n imag))))
  
  ;; Complex -> Complex
  ;; Subtract the given complex from this one.
  (define/public (minus n)
    (new complex% 
         (- (field real) (send n real))
         (- (field imag) (send n imag))))
  
  ;; Complex -> Complex
  ;; Multiply the given complex by this one.
  (define/public (times n)
    (new complex%
         (- (* (field real) (send n real))
            (* (field imag) (send n imag)))
         (+ (* (field imag) (send n real))
            (* (field real) (send n imag)))))
  
  ;; Complex -> Complex
  ;; Divide this complex by the given one.
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
  
  ;; -> Complex
  ;; Multiply this complex by itself.
  (define/public (sq)
    (times this))
  
  ;; Alternative:
  ;; OK, but point out `this' solution.
  #;
  (define/public (sq)
    (times (new complex%
                (field real)
                (field imag))))
  
  ;; Alternative:
  ;; Not OK, should lose 2pts for no code re-use.
  #;
  (define/public (times n)
    (new complex%
         (- (* (field real) (field real))
            (* (field imag) (field imag)))
         (+ (* (field imag) (field real))
            (* (field real) (field imag)))))          
  
  ;; -> Number
  ;; Compute the magnitude of this complex.
  (define/public (mag)
    (sqrt (+ (sqr (field real))
             (sqr (field imag)))))
  
  ;; -> Complex
  ;; Compute the square root of this complex.
  (define/public (sqroot)
    (new complex% 
         (sqrt (/ (+ (mag) (field real)) 2))
         (* (sqrt (/ (- (mag) (field real)) 2))
            (if (negative? (field imag))
                -1
                +1))))
  
  ;; -> Number
  ;; Convert this complex to a Racket complex number.
  (define/public (to-number)
    (+ (field real) 
       (* +i (field imag))))  
  
  ;; Alternative: 
  ;; OK, this relies on knowing about `make-rectangular', which
  ;; we didn't tell them about.
  ;; Give kudos for reading documentation.
  #; 
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
(check-expect (send (send c-1 sqroot) sq) c-1)
(check-expect (send (new complex% 0 1) sq) c-1)
; Arithmetic on complex numbers.
(check-expect (send c0+0 =? c0+0)
              true)
(check-expect (send c0+0 =? c2+3)
              false)
(check-expect (send c2+3 plus c3+5) 
              (new complex% 6 8))
(check-expect (send c2+3 minus c3+5)
              (new complex% -2 -2))
(check-expect (send c2+3 times c3+5)
              (new complex% -7 22))
(check-expect (send c2+3 div c3+5)
              (new complex% 23/41 2/41))
(check-expect (send (new complex% 3 4) mag)              
              5)
(check-expect (send c2+3 to-number) 2+3i)


#|
;; Fooling around with testing properties.
(require (only-in racket for in-range unless))

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
|#



