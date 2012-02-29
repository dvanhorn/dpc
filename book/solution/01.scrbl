#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct check-expect))
          (for-label (except-in class/0 define-struct check-expect)))

@title[#:tag "Complex_with_class_solution"]{Solution:
@secref{Complex_with_class}}

This is a solution for the @secref{Complex_with_class} exercise.

@#reader scribble/comment-reader
(racketmod
  class/0
  ;;==========================================================
  ;; Complex Structure

  ;; A Complex is a (make-complex Real Real).
  ;; Interp: real and imaginary parts.
  (define-struct complex (real imag))
  
  ;; =? : Complex Complex -> Boolean
  ;; Are the complexes equal?
  (check-expect (=? (make-complex 0 0) (make-complex 0 0)) true)
  (check-expect (=? (make-complex 0 0) (make-complex 2 3)) false)
  (define (=? n m)
    (and (= (complex-real n)
            (complex-real m))
         (= (complex-imag n)
            (complex-imag m))))
  
  ;; plus : Complex Complex -> Complex
  ;; Add the complexes.
  (check-expect (plus (make-complex 2 3) (make-complex 4 5))
                (make-complex 6 8))
  (define (plus n m)
    (make-complex (+ (complex-real n) (complex-real m))
                  (+ (complex-imag n) (complex-imag m))))
  
  ;; minus : Complex Complex -> Complex
  ;; Subtract the complexes.
  (check-expect (minus (make-complex 2 3) (make-complex 4 5))
                (make-complex -2 -2))
  (define (minus n m)
    (make-complex (- (complex-real n) (complex-real m))
                  (- (complex-imag n) (complex-imag m))))
  
  ;; times : Complex Complex -> Complex
  ;; Multiply the complexes.
  (check-expect (times (make-complex 2 3) (make-complex 4 5))
                (make-complex -7 22))
  (define (times n m)
    (make-complex (- (* (complex-real n) (complex-real m))
                     (* (complex-imag n) (complex-imag m)))
                  (+ (* (complex-imag n) (complex-real m))
                     (* (complex-real n) (complex-imag m)))))
  
  ;; div : Complex Complex -> Complex
  ;; Divide the complexes.
  (check-expect (div (make-complex 2 3) (make-complex 4 5))
                (make-complex 23/41 2/41))
  (define (div n m)
    (make-complex (/ (+ (* (complex-real n) (complex-real m))
                        (* (complex-imag n) (complex-imag m)))
                     (+ (sqr (complex-real m))
                        (sqr (complex-imag m))))
                  (/ (- (* (complex-imag n) (complex-real m))
                        (* (complex-real n) (complex-imag m)))
                     (+ (sqr (complex-real m))
                        (sqr (complex-imag m))))))
  
  ;; sq : Complex -> Complex
  ;; Multiply the complex by itself.
  (check-expect (sq (make-complex 0 1)) 
                (make-complex -1 0))
  (define (sq n)
    (times n n))
  
  ;; mag : Complex -> Number
  ;; Compute the magnitude of the complex.
  (check-expect (mag (make-complex -1 0)) 1)
  (check-expect (mag (make-complex 3 4)) 5)
  (define (mag n)
    (sqrt (+ (sqr (complex-real n))
             (sqr (complex-imag n)))))
  
  ;; sqroot : Complex -> Complex
  ;; Compute the square root of the complex.
  (check-expect (sqroot (make-complex -1 0)) 
                (make-complex 0 1))
  (define (sqroot n)
    (make-complex (sqrt (/ (+ (mag n) (complex-real n)) 2))
                  (* (sqrt (/ (- (mag n) (complex-real n)) 2))
                     (if (negative? (complex-imag n))
                         -1
                         +1))))
  
  ;; to-number : Complex -> Number
  ;; Convert the complex to a Racket complex number.
  (check-expect (to-number (make-complex 2 3)) 2+3i)
  (define (to-number n)
    (+ (complex-real n) 
       (* +i (complex-imag n))))
  
  ;; Alternative:
  ;; OK, this relies on knowing about `make-rectangular'.
  ;; (define (to-number n)
  ;;   (make-rectangular (complex-real n)
  ;;                     (complex-imag n)))
  
    
  ;;==========================================================
  ;; Complex Class
  
  ;; A Complex is a (new complex% Real Real).
  ;; Interp: real and imaginary parts.
  
  (define-class complex%
    (fields real imag)
    
    ;; =? : Complex -> Boolean
    ;; Is the given complex equal to this one?
    (check-expect (send (new complex% 0 0) =? (new complex% 0 0)) true)
    (check-expect (send (new complex% 0 0) =? (new complex% 2 3)) false)
    (define (=? n)
      (and (= (send this real)
              (send n real))
           (= (send this imag)
              (send n imag))))
    
    ;; plus : Complex -> Complex
    ;; Add the given complex to this one.
    (check-expect (send (new complex% 2 3) plus (new complex% 4 5))
                  (new complex% 6 8))
    (define (plus n)
      (new complex% 
           (+ (send this real) (send n real))
           (+ (send this imag) (send n imag))))
    
    ;; minus : Complex -> Complex
    ;; Subtract the given complex from this one.
    (check-expect (send (new complex% 2 3) minus (new complex% 4 5))
                  (new complex% -2 -2))
    (define (minus n)
      (new complex% 
           (- (send this real) (send n real))
           (- (send this imag) (send n imag))))
    
    ;; times : Complex -> Complex
    ;; Multiply the given complex by this one.
    (check-expect (send (new complex% 2 3) times (new complex% 4 5))
                  (new complex% -7 22))
    (define (times n)
      (new complex%
           (- (* (send this real) (send n real))
              (* (send this imag) (send n imag)))
           (+ (* (send this imag) (send n real))
              (* (send this real) (send n imag)))))
    
    ;; div : Complex -> Complex
    ;; Divide this complex by the given one.
    (check-expect (send (new complex% 2 3) div (new complex% 4 5))
                  (new complex% 23/41 2/41))
    (define (div n)
      (new complex%
           (/ (+ (* (send this real) (send n real))
                 (* (send this imag) (send n imag)))
              (+ (sqr (send n real))
                 (sqr (send n imag))))
           (/ (- (* (send this imag) (send n real))
                 (* (send this real) (send n imag)))
              (+ (sqr (send n real))
                 (sqr (send n imag))))))
    
    ;; sq : -> Complex
    ;; Multiply this complex by itself.
    (check-expect (send (new complex% 0 1) sq) 
                  (new complex% -1 0))
    (define (sq)
      (send this times this))
  
    ;; Alternative:
    ;; OK, but `this' solution is preferred.
    ;; (define (sq)
    ;;   (times (new complex%
    ;;               (send this real)
    ;;               (send this imag))))
  
    ;; Alternative:
    ;; Not OK: no code re-use.  
    ;; (define (times n)
    ;;   (new complex%
    ;;        (- (* (send this real) (send this real))
    ;;           (* (send this imag) (send this imag)))
    ;;        (+ (* (send this imag) (send this real))
    ;;           (* (send this real) (send this imag)))))
  
    ;; mag : -> Number
    ;; Compute the magnitude of this complex.
    (check-expect (send (new complex% -1 0) mag) 1)
    (check-expect (send (new complex% 3 4) mag) 5)
    (define (mag)
      (sqrt (+ (sqr (send this real))
               (sqr (send this imag)))))
    
    ;; sqroot : -> Complex
    ;; Compute the square root of this complex.
    (check-expect (send (new complex% -1 0) sqroot) 
                  (new complex% 0 1))
    (define (sqroot)
      (new complex% 
           (sqrt (/ (+ (send this mag) (send this real)) 2))
           (* (sqrt (/ (- (send this mag) (send this real)) 2))
              (if (negative? (send this imag))
                  -1
                  +1))))
  
    ;; to-number : -> Number
    ;; Convert this complex to a Racket complex number.
    (check-expect (send (new complex% 2 3) to-number) 2+3i)
    (define (to-number)
      (+ (send this real) 
         (* +i (send this imag)))))
)
