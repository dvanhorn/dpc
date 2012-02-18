#lang scribble/manual
@(require scribble/eval
	  class/utils
          (for-label (except-in class/0 check-expect))
	  (for-label (only-in lang/htdp-intermediate-lambda check-expect))
	  (for-label class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@(the-eval
`(begin
   (define CLOCK-SPEED  1/30) ; SEC/TICK
   (define SATELLITE-SPEED 1) ; AU/SEC
   (define ROCKET-SPEED 1)    ; AU/SEC
   (define DELTA-X (* CLOCK-SPEED SATELLITE-SPEED)) ; AU/TICK
   (define DELTA-Y (* CLOCK-SPEED ROCKET-SPEED)) ; AU/TICK

   (define WIDTH 100)   ; PX
   (define HEIGHT 200)  ; PX
   (define SATELLITE-Y (quotient HEIGHT 4))
   (define MT-SCENE (empty-scene WIDTH HEIGHT))
   (define SATELLITE (circle 30 "solid" "red"))
   (define ROCKET (bitmap ,(string-append (path->string (collection-path "class/0")) "/rocket.png")))))

@(the-eval
'(define-class takeoff%
   (fields dist)
   ;; next : -> Rocket
   ;; Compute next position of this ascending rocket after one tick of time.
   (define (next)
     (new takeoff% (+ (send this dist) DELTA-Y)))

   ;; render : -> Scene
   ;; Render this rocket as a scene.
   (define (render)
     (send this draw-on MT-SCENE))
   
   ;; draw-on : Scene -> Scene
   ;; Draw this rocket on to scene.
   (define (draw-on scn)
     (overlay/align/offset "center" "bottom"
			   ROCKET
			   0 (add1 (send this dist))
			   scn)))
)

@(the-eval
'(define-class landing%
   (fields dist)
   ;; next : -> Rocket
   ;; Compute next position of this descending rocket after one tick of time.
   (define (next)
     (cond [(< (send this dist) DELTA-Y) (new takeoff% 0)]
	   [else (new landing% (- (send this dist) DELTA-Y))]))

   ;; render : -> Scene
   ;; Render this rocket as a scene.
   (define (render)
     (send this draw-on MT-SCENE))
   
   ;; draw-on : Scene -> Scene
   ;; Draw this rocket on to scene.
   (define (draw-on scn)
     (overlay/align/offset "center" "bottom"
			   ROCKET
			   0 (add1 (send this dist))
			   scn)))
)

@(the-eval
'(define-class satellite%
  (fields dist)

  ;; next : -> Satellite
  ;; Move this satellite distance travelled in one tick.
  (define (next)     
    (local [(define n (+ (send this dist) DELTA-X))]                        
      (new satellite% (cond [(> n WIDTH) (- n WIDTH)]
                            [else n]))))
  
  ;; render : -> Scene
  ;; Render this satellite as a scene.
  (define (render)
    (send this draw-on MT-SCENE))

  ;; draw-on : Scene -> Scene
  ;; Draw this satellite on scene.
  (define (draw-on scn)
    (send this draw-on/offset -1
	  (send this draw-on/offset 0
		(send this draw-on/offset 1
		      MT-SCENE))))

  ;; draw-on/offset : Number Scene -> Scene
  ;; Draw this satellite on scene with given day offset.
  (define (draw-on/offset d scn)
    (place-image SATELLITE
		 (+ (send this dist) (* d WIDTH))
		 SATELLITE-Y
		 scn)))
)

@(the-eval
'(define-class space%
  (fields rocket satellite)
  (define (on-tick)
    (new space% 
	 (send (send this rocket) next)
	 (send (send this satellite) next)))
  
  (define (to-draw)
    (send (send this rocket) draw-on
	  (send (send this satellite) draw-on
		MT-SCENE)))))


@title{Revisiting the Rocket}

@section{Landing and taking off}

Let's now revise our @secref{Object-oriented_rocket} program so that
the rocket first descends toward the ground, lands, then lifts off
again.  Our current representation of a world is insufficient since
it's ambiguous whether we are going up or down.  For example, if the
rocket is at @racket[42], are we landing or taking off?  There's no
way to know.  We can revise our data definition to included a
representation of this missing information.  As we hear this revised
description, the idea of a union data definition should jump out: ``a
rocket is either landing or taking off.''  Let's re-develop our
program with this new design.

Our revised class definition is then:

@classblock{
;; A World is a Rocket.

;; A Rocket is one of:
;; - (new takeoff% Number)
;; - (new landing% Number)

;; Interp: distance from the ground to base of rocket in AU,
;; either taking off or landing.

(define-class takeoff%
   (fields dist)
   ...)
(define-class landing%
   (fields dist)
   ...)
}


The signatures for our methods don't change, however we now have two
sets of methods to implement: those for rockets taking off, and those
for landing rockets.

First, let's make some test cases for the @racket[next] method.  We
expect that a rocket taking off works just as before:

@classblock{
(check-expect (send (new takeoff% 10) next)
              (new takeoff% (+ 10 DELTA-Y)))
}

However, when landing we expect the rocket to be descending toward the
ground.  For simplicity, let's specify the rocket descends as fast as
it ascends:

@classblock{
(check-expect (send (new landing% 100) next)
              (new takeoff% (- 100 DELTA-Y)))
}

There is an important addition case though.  When the rocket is
descending and gets close to the ground, we want it to land.  So when
the rocket is descending and less than @racket[DELTA-Y] units from the
ground, we want its next state to be on the ground, ready to lift off:

@classblock{
(check-expect (send (new landing% (sub1 DELTA-Y)) next)
              (new takeoff% 0))
}

Based on these examples, we can now define the @racket[next] method in
the @racket[landing%] and @racket[takeoff%] classes:

@filebox[@racket[takeoff%]]{
@classblock{
;; next : -> Rocket
;; Compute next position of this ascending rocket after one tick of time.
(define (next)
  (new takeoff% (+ (send this dist) DELTA-Y)))
}}

@filebox[@racket[landing%]]{
@classblock{
;; next : -> Rocket
;; Compute next position of this descending rocket after one tick of time.
(define (next)
  (cond [(< (send this dist) DELTA-Y) (new takeoff% 0)]
        [else (new landing% (- (send this dist) DELTA-Y))]))
}}

Now let's turn to the remaining methods such as @racket[render].  When
rendering a rocket, it's clear that it doesn't matter whether the
rocket is landing or taking off; it will be drawn the same.  This
leads to having two @emph{identical} definitions of the
@racket[render] method in both @racket[takeoff%] and
@racket[landing%].  Since the method relies upon the helper method
@racket[draw-on], we likewise have two identical definitions of
@racket[draw-on] in @racket[takeoff%] and @racket[landing%].

@filebox[@elem{@racket[landing%] and @racket[takeoff%]}]{
@classblock{
  ;; render : -> Scene
  ;; Render this rocket as a scene.
  (define (render)
    (send this draw-on MT-SCENE))
 
  ; draw-on : Scene -> Scene
  ; Draw this rocket on to scene.
  (define (draw-on scn)
    (overlay/align/offset "center" "bottom"
                          ROCKET
                          0 (add1 (send this dist))
                          scn))
}}

This duplication of code is unsettling, but for now let's just live
with the duplication.  We could abstract the code by defining a
@emph{function} and calling the function from both methods, but as we
try to focus on object-oriented designs, let's instead recognize
there's a need for an object-oriented abstraction mechnaism here and
revisit the issue later in the chapter on @secref{Inheritance}.

We can experiment and see ascending rockets climb and descending
rockets land:

@examples[#:eval the-eval
(send (new landing% 5) next)
(send (new takeoff% 5) next)
(send (new landing% 0) next)
(send (new landing% (quotient HEIGHT 2)) render)
]

Implementing the needed methods for a @racket[big-bang] animation is
straightforward:

@filebox[@elem{@racket[landing%] and @racket[takeoff%]}]{
@classblock{
(define (on-tick) (send this next))
(define (to-draw) (send this render))
}}

And to run the animation, just start @racket[big-bang] with a landing
rocket:

@classblock{
(big-bang (new landing% HEIGHT))
}

@section{Adding a satellite}

Let's now add an orbiting satellite.  To do so, let's first forget
about rockets and make an satellite animation.  The satellite is
represented by a class with a single field---a number giving the
distance from the date line, which we'll draw at the left of the
screen, to the center of the satellite.  When the satellite gets to
the edge of the screen, it will wrap around starting over again at the
date line.

@classblock{
(define CLOCK-SPEED  1/30) ; SEC/TICK
(define SATELLITE-SPEED 1) ; AU/SEC
(define DELTA-X (* CLOCK-SPEED SATELLITE-SPEED)) ; AU/TICK

(define SATELLITE (circle 30 "solid" "red"))
(define WIDTH 100)   ; PX
(define HEIGHT 200)  ; PX
(define SATELLITE-Y (quotient HEIGHT 4))
(define MT-SCENE (empty-scene WIDTH HEIGHT))

;; A World is a Satellite.

;; A Satellite is a (new satellite% Number).
;; Interp: distance in AU from date line to center of satellite.

(define-class satellite%
  (fields dist)

  ;; next : -> Satellite
  ;; Move this satellite distance travelled in one tick.
  (define (next)     
    (local [(define n (+ (send this dist) DELTA-X))]                        
      (new satellite% (cond [(> n WIDTH) (- n WIDTH)]
                            [else n])))))
}

Drawing the satellite is a little more tricky that the rocket because
the satellite can appear on both the left and right side of the screen
as it passes over the date line.  A simple trick to manage this is to
draw @emph{three} satellites, each a full ``day'' behind and ahead of
the current satellite, thus when the satellite is just past the date
line, the day ahead image appears on the right, and when the satellite
approaches the end of the day, the day behind satellite appears on the
left.  To accomodate this, we define a helper method that draws the
satellite at given day offsets.

@filebox[@racket[satellite%]]{
@classblock{
;; render : -> Scene
;; Render this satellite as a scene.
(define (render)
  (send this draw-on MT-SCENE))

;; draw-on : Scene -> Scene
;; Draw this satellite on scene.
(define (draw-on scn)
  (send this draw-on/offset -1
        (send this draw-on/offset 0
              (send this draw-on/offset 1
                    MT-SCENE))))

;; draw-on/offset : Number Scene -> Scene
;; Draw this satellite on scene with given day offset.
(define (draw-on/offset d scn)
  (place-image SATELLITE
               (+ (send this dist) (* d WIDTH))
               SATELLITE-Y
               scn))
}}

@examples[#:eval the-eval
(send (new satellite% 0) next)
(send (new satellite% (quotient WIDTH 2)) render)
(send (new satellite% 0) render)]

We can now add the needed methods to animate satellites with
@racket[big-bang]:

@filebox[@racket[satellite%]]{
@classblock{
(define (on-tick) (send this next))
(define (to-draw) (send this render))
}}

And then animate a satellite with:

@classblock{
(big-bang (new satellite% 0))
}

Now we have animations of rockets and of satellites, but putting the
pieces together is simple.  We need to revise our data definition.
Let's make a new class of compound that @emph{contains} a rocket and a
satellite and implement the methods needed to make an animation:

@classblock{
;; A World is a (new space% Rocket Satellite).
(define-class space%
  (fields rocket satellite)

  (define (on-tick)
    (new space% 
	 (send (send this rocket) next)
	 (send (send this satellite) next)))
  
  (define (to-draw)
    (send (send this rocket) draw-on
	  (send (send this satellite) draw-on
		MT-SCENE))))

}

@examples[#:eval the-eval
(send (new space% 
	   (new landing% (quotient HEIGHT 2))
	   (new satellite% (quotient WIDTH 3)))
      to-draw)
]

Finally, to animate the whole thing, we just call @racket[big-bang]
with an initial @racket[space%] object:

@classblock{
(big-bang (new space%
	       (new landing% HEIGHT)
               (new satellite% 0)))
}


