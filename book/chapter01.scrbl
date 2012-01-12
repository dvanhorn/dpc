#lang scribble/manual
@(require scribble/eval
	  class/utils
          racket/sandbox
          #;(for-label lang/htdp-intermediate-lambda)
  	  (for-label (only-in lang/htdp-intermediate-lambda define-struct))
          (for-label (except-in class/0 define-struct))
	  (for-label class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    ;(the-eval '(require lang/htdp-intermediate-lambda))
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    #|(call-in-sandbox-context 
     the-eval 
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))|#
    the-eval))


@title{Objects = Function + Data}

@section{Functional rocket}

Here's a review of the first program we wrote last semester.  It's the
program to launch a rocket, written in the Beginner Student Language:

@racketblock[
(require 2htdp/image)
(require 2htdp/universe)

@code:comment{Use the rocket key to insert the rocket here.}
(define ROCKET (bitmap class/0/rocket.png))

(define WIDTH 100)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

@code:comment{A World is a Number.}
@code:comment{Interp: distance from the ground in AU.}

@code:comment{render : World -> Scene}
(check-expect (render 0) 
	      (place-image ROCKET (/ WIDTH 2) HEIGHT MT-SCENE))
(define (render h)
  (place-image ROCKET
               (/ WIDTH 2)
               (- HEIGHT h)
               MT-SCENE))

@code:comment{next : World -> World}
(check-expect (next 0) 7)
(define (next h)
  (+ h 7))

(big-bang 0
          (on-tick next)
          (to-draw render))
]

It's a shortcoming of our documentation system that we can't define
@racket[ROCKET] to be the rocket image directly, but as you can see
this did the right thing:

@(the-eval `(define ROCKET (bitmap ,(string-append (path->string (collection-path "class/0")) "/rocket.png"))))
@(the-eval '(define WIDTH 100))
@(the-eval '(define HEIGHT 300))
@(the-eval '(define MT-SCENE (empty-scene WIDTH HEIGHT)))
@(the-eval
'(define-class world% 
  (fields height)
  (define/public (on-tick)
    (new world% (add1 (field height))))
  (define/public (to-draw)
    (place-image ROCKET
    		 (/ WIDTH 2)
		 (- HEIGHT (field height))
		 MT-SCENE))))

@interaction[#:eval the-eval
ROCKET
]

You can use the same trick, or you can copy this rocket image and
paste it directly into DrRacket.

@section{Object-oriented rocket}

You'll notice that there are two significant components to this
program.  There is the @emph{data}, which in this case is a number
representing the height of the rocket, and the @emph{functions} that
operate over that class of data, in this case @racket[next] and
@racket[render].

This should be old-hat programming by now.  But in this class, we are
going to explore a new programming paradigm that is based on
@emph{objects}.  Objects are an old programming concept that first
appeared in the 1950s just across the Charles river.  As a first
approximation, you can think of an @emph{object} as the coupling
together of the two significant components of our program (data and
functions) into a single entity: an object.

Since we are learning a new programming language, you will no longer
be using BSL and friends.  Instead, select
@menuitem["Language"]{Choose Language...} in DrRacket, then select the
"Use the language declared in the source" option and add the following
to the top of your program:

@racketmod[class/0]

The way to define a class is with @racket[define-class]:
@racketblock[
(define-class world%
  (fields height))
]

This declares a new class of values, namely @racket[world%] objects.
(By convention, we will use the @tt{%} suffix for the
name of classes.)  For the moment, @racket[world%] objects consist
only of data: they have one @emph{field}, the @racket[height] of the
rocket.

Like a structure definition, this class definition defines a new kind
of data, but it does not make any particular instance of that data.
To make an new instance of a particular class, you use the
@racket[new] syntax, which takes a class name and expressions that
produce a value for each field of the new object.  Since a
@racket[world%] has one field, @racket[new] takes the shape:
@interaction[#:eval the-eval
(new world% 7)]

This creates a @racket[world%] representing a rocket with height @racket[7].

In order to access the data, we can invoke the @racket[height]
accessor method.  Methods are like functions for objects and they are
called (or @emph{invoked}) by using the @racket[send] form like so: 

@interaction[#:eval the-eval
(send (new world% 7) height)]

This suggests that we can now re-write the data definition for Worlds:
@racketblock[
@code:comment{A World is a (new world% Number).}
@code:comment{Interp: height represents distance from the ground in AU.}
]

To add functionality to our class, we define @emph{methods} using the
@racket[define/public] form.  In this case, we want to add two methods
@racket[on-tick] and @racket[to-draw]: 

@racketblock[ 
@code:comment{A World is a (new world% Number).}  
@code:comment{Interp: height represents distance from the ground in AU.}
(define-class world% 
  (fields height)

  @code:comment{on-tick : ...}
  (define/public (on-tick ...) ...)

  @code:comment{to-draw : ...}
  (define/public (to-draw ...) ...))
]

We will return to the contracts and code, but now that we've seen how
to define methods, let's look at how to @emph{invoke} them in order to
actually compute something.  To call a defined method, we again use
the @racket[send] form, which takes an object, a method name, and any
inputs to the method:

@racketblock[
(send (new world% 7) on-tick ...)
]

This will call the @racket[on-tick] method of the object created with
@racket[(new world% 7)].  This is analogous to calling the
@racket[next] function on the world @racket[7].  The ellided code
(@racket[...]) is where we would write additional inputs to the
method, but it's not clear what further inputs are needed, so now
let's turn to the contract and method signatures for @racket[on-tick]
and @racket[to-draw].

When we designed the functional analogues of these methods, the
functions took as input the world on which they operated, i.e. they
had contracts like:

@racketblock[
@code:comment{tick : World -> World}
@code:comment{render : World -> Scene}]

But in an object, the data and functions are packaged together.
Consequently, the method does not need to take the world input; that
data is already a part of the object and the values of the fields are
accessible using the @racket[field] form.

That leads us to the following method signatures:

@racketblock[
@code:comment{A World is a (new world% Number).}  
@code:comment{Interp: height represents distance from the ground in AU.}
(define-class world% 
  (fields height)

  @code:comment{on-tick : -> World}
  (define/public (on-tick) ...)

  @code:comment{to-draw : -> Scene}
  (define/public (to-draw) ...))
]

Since we have contracts and have seen how to invoke methods, we can
now formulate test cases:

@racketblock[
(check-expect (send (new world% 7) on-tick)
	      (new world% 8))
(check-expect (send (new world% 7) to-draw)
	      (place-image ROCKET
	      		   (/ WIDTH 2)
			   (- HEIGHT 7)
			   MT-SCENE))
]

Finally, we can write the code from our methods:

@racketblock[
@code:comment{A World is a (new world% Number).}  
@code:comment{Interp: height represents distance from the ground in AU.}
(define-class world% 
  (fields height)

  @code:comment{on-tick : -> World}
  (define/public (on-tick)
    (new world% (add1 (field height))))

  @code:comment{to-draw : -> Scene}
  (define/public (to-draw)
    (place-image ROCKET
    		 (/ WIDTH 2)
		 (- HEIGHT (field height))
		 MT-SCENE)))]

At this point, we can construct @racket[world%] objects and invoke
methods.

@examples[#:eval the-eval
(new world% 7)
(send (new world% 7) on-tick)
(send (new world% 80) to-draw)
]

But if we want to see an animation, we need to have our program
interact with the @racket[big-bang] system.  Since we are now writing
programs in an object-oriented style, we have a new @racket[big-bang]
system that uses an interface more suited to objects.  To import this
OO-style @racket[big-bang], add the following to the top of your
program:

@racketblock[
(require class/universe)]

In the functional setting, we had to explicitly give a piece of data
representing the state of the initial world and list which functions
should be used for each event in the system.  In other words, we had
to give both data and functions to the @racket[big-bang] system.  In
an object-oriented system, the data and functions are already packaged
together, and thus the @racket[big-bang] form takes a single argument:
an object that both represents the initial world and implements the
methods needed to handle system events such as @racket[to-draw] and
@racket[on-tick].  (Our choice of method names was important; had we
used the names @racket[render] and @racket[next], for example,
@racket[big-bang] would not have known how to make the animation
work.)

So to launch our rocket, we simply do the following:

@racketblock[
(big-bang (new world% 0))]

Our complete program is:

@racketmod[
class/0
(require 2htdp/image)
(require class/universe)

@code:comment{Use the rocket key to insert the rocket here.}
(define ROCKET (bitmap class/0/rocket.png))

(define WIDTH 100)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

@code:comment{A World is a (new world% Number).}  
@code:comment{Interp: height represents distance from the ground in AU.}
(define-class world% 
  (fields height)

  @code:comment{on-tick : -> World}
  (define/public (on-tick)
    (new world% (add1 (field height))))

  @code:comment{to-draw : -> Scene}
  (define/public (to-draw)
    (place-image ROCKET
    		 (/ WIDTH 2)
		 (- HEIGHT (field height))
		 MT-SCENE)))

(check-expect (send (new world% 7) on-tick)
	      (new world% 8))
(check-expect (send (new world% 7) to-draw)
	      (place-image ROCKET
	      		   (/ WIDTH 2)
			   (- HEIGHT 7)
			   MT-SCENE))

@code:comment{Run, program, run!}
(big-bang (new world% 0))]

@section{Landing and taking off}

Let's now revise our program so that the rocket first descends toward
the ground, lands, then lifts off again.  Our current representation
of a world is insufficient since it's ambiguous whether we are going
up or down.  For example, if the rocket is at @racket[42], are we
landing or taking off?  There's no way to know.  We can revise our
data definition to included a representation of this missing
information.  A simple solution is to add a number representing the
velocity of the rocket.  When it's negative, we are moving toward the
ground.  When positive, we are taking off.

In the functional approach, this new design criterion motivated the
use of compound data, which are represented with structures.  With
objects, atomic and compound data are represented the same way; all
that changes are the number of fields contained in each object.

Our revised class definition is then:

@(the-eval
'(define-class world%
  (fields height velocity)))

@racketblock[
;; A World is a (new world% Number Number).
;; Interp: height represents the height of the rocket in AU,
;; velocity represents the speed of the rocket in AU/seconds.
(define-class world%
  (fields height velocity)
  ...)]

When constructing @racket[world%] objects now give two arguments in
addition to the class name:

@interaction[#:eval the-eval
(new world% 7 -1)]

Both components can be accessed through the accessor methods:

@interaction[#:eval the-eval
(send (new world% 7 -1) height)
(send (new world% 7 -1) velocity)]

And within method definitions, we can refer to the values of fields
using the @racket[field] form.  The signatures for our methods don't
change, and rockets should render just the same as before, however we
need a new behavior for @racket[on-tick].  When a world ticks, we want
its new height to be calculated based on its current height and
velocity, and its new velocity is the same as the old velocity, unless
the rocket has landed, in which case we want to flip the direction of
the velocity.

First, let's make some test cases for the new @racket[on-tick]:

@racketblock[
(check-expect (send (new world% 0 1) on-tick)
              (new world% 1 1))
(check-expect (send (new world% 10 -2) on-tick)
              (new world% 8 -2))
(check-expect (send (new world% 0 -1) on-tick)
              (new world% -1 1))
]

Based on this specification, we revise the @racket[on-tick] method
definition as follows:

@racketblock[
(define-class world%
  ...
  ;; on-tick : -> World 
  (define/public (on-tick)
    (new world%
         (+ (field velocity) (field height))
         (cond [(<= (field height) 0) (abs (field velocity))]
               [else (field velocity)])))
  ...)]

Giving us an overall program of:

@#reader scribble/comment-reader
(racketmod
class/0
(require 2htdp/image)
(require class/universe)

(define ROCKET (bitmap class/0/rocket.png))

(define WIDTH 100)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

;; A World is a (new world% Number Number).
;; Interp: height represents the height of the rocket in AU,
;; velocity represents the speed of the rocket in AU/seconds.
(define-class world%
  (fields height velocity)
  
  ;; on-tick : -> World 
  (define/public (on-tick)
    (new world%
         (+ (field velocity) (field height))
         (cond [(<= (field height) 0) (abs (field velocity))]
               [else (field velocity)])))
  
  ;; to-draw : -> Scene
  (define/public (to-draw)
    (place-image ROCKET
                 (/ WIDTH 2) 
                 (- HEIGHT (field height))
                 (empty-scene WIDTH HEIGHT))))

(check-expect (send (new world% 0 1) to-draw)
              (place-image ROCKET 
                           (/ WIDTH 2) 
                           HEIGHT 
                           MT-SCENE))

(check-expect (send (new world% 0 1) on-tick)
              (new world% 1 1))
(check-expect (send (new world% 10 -2) on-tick)
              (new world% 8 -2))
(check-expect (send (new world% 0 -1) on-tick)
              (new world% -1 1))

(big-bang (new world% HEIGHT -1)))

@section{Adding a moon}

Adding more components is straightforward.

Here is a complete program that includes an orbiting moon:

@#reader scribble/comment-reader
(racketmod
class/0
(require 2htdp/image)
(require class/universe)

(define ROCKET (bitmap class/0/rocket.png))
(define MOON (circle 20 "solid" "blue"))

(define WIDTH 300)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

; A World is a (new world% Number Number Number).
; Interp: height represents distance from the ground in AU.
; velocity represents the speed of the rocket in AU/sec.
; moon-height represents the height of the moon.
(define-class world%
  (fields height velocity moon-height)
  
  ;; on-tick : -> World 
  (define/public (on-tick)
    (new world%
         (+ (field velocity) (field height))
         (cond [(= (field height) 0) (abs (field velocity))]
               [else (field velocity)])
         (modulo (+ 5 (field moon-height)) 200)))
  
  ;; to-draw : -> Scene
  (define/public (to-draw)
    (place-image MOON
                 (/ WIDTH 3) 
                 (field moon-height)
                 (place-image ROCKET
                              (/ WIDTH 2) 
                              (- HEIGHT (field height))
                              MT-SCENE))))

(check-expect (send (new world% 0 1 100) to-draw)
              (place-image MOON
                           (/ WIDTH 3) 
                           100
                           (place-image ROCKET 
                                        (/ WIDTH 2) 
                                        HEIGHT 
                                        MT-SCENE)))

(check-expect (send (new world% 0 1 100) on-tick)
              (new world% 1 1 105))
(check-expect (send (send (new world% 10 -2 100) on-tick) height) 8)

(big-bang (new world% HEIGHT -1 100))
)

@section{Exercise}

@subsection[#:tag "assign_complex"]{Complex, with class.}
       
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
           (the-eval '(require class/0))
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
                         (define c4+5 (make-cpx 4 5))
                         (=? c0+0 c0+0)
                         (=? c0+0 c2+3)
                         (=? (plus c2+3 c4+5)
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
			 (define c4+5 (new complex% 4 5))
                         @code:comment{Verify the imaginary unit property.}
			 (send c-1 mag)
			 (send c-1 sqroot) 
			 (send (send (send c-1 sqroot) sq) =? c-1)
			 (send (send (new complex% 0 1) sq) =? c-1)
                         @code:comment{Arithmetic on complex numbers.}
			 (send c0+0 =? c0+0)
			 (send c0+0 =? c2+3)
			 (send (send c2+3 plus c4+5) =?
			       (new complex% 6 8))
			 (send (send c2+3 minus c4+5) =?
			       (new complex% -2 -2))
			 (send (send c2+3 times c4+5) =?
			       (new complex% -7 22))
			 (send (send c2+3 div c4+5) =?
			       (new complex% 23/41 2/41))
			 (send (new complex% 3 4) mag)]
 


