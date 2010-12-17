#lang scribble/manual
@(require "../unnumbered.rkt")
@(require scribble/eval
          racket/sandbox
          (for-label lang/htdp-intermediate-lambda)
          #;(for-label class0))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate-lambda sqr / + sqrt)))
   ;(the-eval '(require lang/htdp-intermediate-lambda))
   ;(the-eval '(require class0))
    #;(call-in-sandbox-context 
     the-eval 
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))
    the-eval))


@title[#:tag "assign01"]{1/12: Complex}

Due: 1/12.

Language: ISL+λ + class0.

@itemlist[#:style 'ordered 
 @item{@bold{The @racket[complex%] class.}
       
        For this exercise, you will develop a class-based representation
        of complex numbers, which are used in several fields, including:
        engineering, electromagnetism, quantum physics, applied mathematics, 
        and chaos theory.        
        
        A @emph{complex number} is a number consisting of a real and 
        imaginary part. It can be written in the mathematical notation @emph{a+bi},
        where @emph{a} and @emph{b} are real numbers, and @emph{i} is the standard
        imaginary unit with the property @emph{i@superscript{2} = −1}.
        
        @margin-note{You can read more about the sophisticated number system of Racket
        in the 
        @other-manual['(lib "scribblings/guide/guide.scrbl")]
        section on
        @secref["numbers" #:doc
                          '(lib "scribblings/guide/guide.scrbl")].}
        Complex numbers are so useful, it turns out they are included
        in the set of numeric values that Racket supports.          
        The Racket notation for writing down complex numbers is @racket[5+3i], 
        where this number has a real part of @racketresult[5] and an 
        imaginery part of @racketresult[3]; @racket[4-2i] has a real 
        part of @racketresult[4] and imaginary part of @racketresult[-2].
        (Notice that complex numbers
        @emph{generalize} the real numbers since any real number can be
        expressed as a complex number with an imaginery part of 
        @racketresult[0].)        
        Arithmetic operations on
        complex numbers work as they should, so for example, you can
        add, subtract, multiply, and divide complex numbers.  (One thing
        you can't do is @emph{order} the complex numbers, so @racket[<]
        and friends work only on real numbers.)
        
        @examples[#:eval the-eval
                         @code:comment{Verify the imaginary unit property.}
                         (sqr (sqrt -1))
                         (sqr 0+1i)
                         @code:comment{Arithmetic on complex numbers.}
                         (+ 2+3i 4+5i)
                         (- 2+3i 4+5i)
                         (* 2+3i 4+5i)
                         (/ 2+3i 4+5i)
                         @code:comment{Complex numbers can't be ordered.}
                         (< 1+2i 2+3i)
                         @code:comment{Real numbers are complex numbers with an imaginary part of 0,}
                         @code:comment{so you can perform arithmetic with them as well.}
                         (+ 2+3i 2)
                         (- 2+3i 2)
                         (* 2+3i 2)
                         (/ 2+3i 2)
			 (magnitude 3+4i)]
 
        @(begin0 
           "" 
           (the-eval '(require class0))
           (the-eval 
            `(begin 
               (define-struct cpx (real imag))
               (define (liftb f) 
	         (lambda (x y) 
                   (f (to-number x) (to-number y))))
               (define (lift2 f)
	         (lambda (x y)
                   (from-number (f (to-number x) (to-number y)))))
               (define (lift1 f)
	         (lambda (x)
                   (from-number (f (to-number x)))))
               (define (from-number n)
                 (make-cpx (real-part n) (imag-part n)))
               (define (to-number c)
                 (+ (cpx-real c) (* +1i (cpx-imag c))))
               (define =? (liftb =))
               (define plus (lift2 +))
               (define minus (lift2 -))
               (define times (lift2 *))
               (define div (lift2 /))
               (define sq (lift1 sqr))
               (define sqroot (lift1 sqrt))))
           (the-eval
            `(define-class complex%
	       (fields real imag)                 
	       (define/private (n) (+ (field real) (* +i (field imag))))
	       
	       (define/public (=? c)
		 (= (n) (send c to-number)))
	       
	       (define/public (plus c)
		 (from-number (+ (n) (send c to-number))))
	       
	       (define/public (minus c)
		 (from-number (- (n) (send c to-number))))
	       
	       (define/public (times c)
		 (from-number (* (n) (send c to-number))))
	       
	       (define/public (div c)
		 (from-number (/ (n) (send c to-number))))
	       
	       (define/public (sq)
		 (from-number (sqr (n))))
	       
	       (define/public (sqroot)
		 (from-number (sqrt (n))))
	       
	       (define/public (mag)
		 (magnitude (n)))

	       (define/private (from-number c)
		 (new complex% 
		      (real-part c)
		      (imag-part c)))
	       
	       (define/public (to-number) (n)))))
        
        Supposing your language was impoverished and didn't support
        complex numbers, you should be able to build them yourself 
        since complex numbers are easily represented as a pair of real 
        numbers---the real and imaginary parts.
        
        Design a structure-based data representation for @tt{Complex}
        values.
        Design the functions @racket[=?], @racket[plus], @racket[minus],
        @racket[times], @racket[div], @racket[sq], @racket[mag], and @racket[sqroot].
        Finally, design a utility function
        @racket[to-number] which can convert @tt{Complex} values into
        the appropriate Racket complex number.  Only the code and tests
        for @racket[to-number] should use Racket's complex (non-real)
        numbers and arithmetic since the point is to build these things
        for yourself.  However, you can use Racket to double-check your 
        understanding of complex arithmetic.
        
	For mathematical definitions of complex number operations, see
	the Wikipedia entries on
	@link["http://en.wikipedia.org/wiki/Complex_number"]{complex numbers}
	and the
	@link["http://en.wikipedia.org/wiki/Square_root#Principal_square_root_of_a_complex_number"]{square root of a complex number}.

        @examples[#:eval the-eval
                         (define c-1  (make-cpx -1 0))
                         (define c0+0 (make-cpx 0 0))                         
                         (define c2+3 (make-cpx 2 3))
                         (define c3+5 (make-cpx 4 5))
                         (=? c0+0 c0+0)
                         (=? c0+0 c2+3)
                         (=? (plus c2+3 c3+5)
                             (make-cpx 6 8))]
        
        Develop a class-based data representation for @tt{Complex}
        values.
        Add accessor methods for extracting the @racket[real] and @racket[imag] 
        parts.
        Develop the methods @racket[=?], @racket[plus], @racket[minus],
        @racket[times], @racket[div], @racket[sq], @racket[mag], @racket[sqroot] and
        @racket[to-number].
        
        @examples[#:eval the-eval
                         @code:comment{Some example Complex values.}
			 (define c-1  (new complex% -1 0))
			 (define c0+0 (new complex% 0 0))
			 (define c2+3 (new complex% 2 3))
			 (define c3+5 (new complex% 4 5))
                         @code:comment{Verify the imaginary unit property.}
			 (send c-1 mag)
			 (send c-1 sqroot) 
			 (send (send (send c-1 sqroot) sq) =? c-1)
			 (send (send (new complex% 0 1) sq) =? c-1)
                         @code:comment{Arithmetic on complex numbers.}
			 (send c0+0 =? c0+0)
			 (send c0+0 =? c2+3)
			 (send (send c2+3 plus c3+5) =?
			       (new complex% 6 8))
			 (send (send c2+3 minus c3+5) =?
			       (new complex% -2 -2))
			 (send (send c2+3 times c3+5) =?
			       (new complex% -7 22))
			 (send (send c2+3 div c3+5) =?
			       (new complex% 23/41 2/41))
			 (send (new complex% 3 4) mag)]}
 
 @item{@bold{SVN basics}
        
        Check in your assigment.}]
