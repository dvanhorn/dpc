#lang scribble/manual
@(require "../web/utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class1 check-expect define-struct ... length
				numerator denominator))
	  (for-label 2htdp/image)
	  (for-label (only-in test-engine/racket-tests check-expect))
	  (for-label class1/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class2))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "lec18"]{3/10: Overriding}

@verbatim|{
Is the test really on the 22nd? -- yes.

Inheritance and overriding

  Why do we use inheritance?
    1. Abstracting common code
    2. Managing the type system (in other languages)
    3. Extend existing system -- often overused, but very useful

  Example: points
    color-point% extends point%
      Use overriding to implement mv correctly for color-point%
    color-point% contains point%
      1. Class relationships aren't explicit
      2. Must manually delegate every inherited behavior

  Use mutation?
  Use update methods?
  Use copying + mutation?
    Performance analysis is hard and is best done empirically

  Dispatch
    Call the method most specific to the _object_
    e.g. double-move calls mv which dispatches to the appropriate method body

  Example: worlds
    big-bang expects many behaviors that we usually don't want to specify

  super to dispatch one level up
    Jumping >1 level would very often be a bad idea and break modularity

  With inheritance, data definitions are always _open_
    e.g. A Point is one of
          - (point% Num Num)
          - (color-point% Color Num Num)
          - ...

  Overriding lets you parametrize behavior of methods by supplying new behavior
    Powerful, just like higher-order functions in FP
    But power comes with responsibility: must respect behavioral subtyping!
}|

Overriding in Java:

@verbatim|{
class Point {
    Integer x;
    Integer y;

    public Point(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    public Point mv(Integer dx, Integer dy) {
	return new Point(this.x + dx, this.y + dy);
    }

    public Point double_move(Integer dxy) {
	return this.mv(dxy,dxy);
    }

}

class CPoint extends Point {
    String color;
    
    public CPoint(String color, Integer x, Integer y) {
	super(x,y);
	this.color = color;
    }

    public CPoint mv(Integer dx, Integer dy) {
	return new CPoint(this.color, this.x-dx, this.y-dy);
    }
    
}

}|

Overriding in @racket[class4]:

@codeblock{
#lang class4

;; A Point is (point% Number Number)
;; and implements 
;; mv : Number Number -> Point
;; moves the point by this much in each direction
;; double-move : Number -> Point
;; move both x and y by the given amount
(define-class point%
  (fields x y)
  (define/public (mv dx dy)
    (point% (+ dx (field x)) 
            (+ dy (field y))))
  (define/public (double-move dxy)
    (mv dxy dxy))
  
  (check-expect (send (point% 1 2) mv 3 3) (point% 4 5))
  (check-expect (send (point% 1 2) double-move 3) (point% 4 5))
  )

(define-class color-point%
  (super point%)
  (fields color)
  (define/public (mv dx dy)
    (color-point% (field color) 
                  (+ dx (field x))
                  (+ dy (field y)))))

(check-expect (send (color-point% "red" 1 2) mv 2 2)
              (color-point% "red" 3 4))
}

Delegation in @racket[class4]:

@codeblock{
#lang racket



;; A Point is (point% Number Number)
;; and implements 
;; mv : Number Number -> Point
;; moves the point by this much in each direction
(define-class point%
  (fields x y)
  (define/public (mv dx dy)
    (point% (+ dx (field x))
            (+ dy (field y))))
  (check-expect (send (point% 1 2) mv 3 3) (point% 4 5))
  )

(define-class color-point
  (fields color pt)
  (define/public (mv dx dy)
    (color-point% (field color)
                  (send (field pt) mv dx dy))))
}

A default World:

@codeblock{
#lang class4
(require class4/universe 2htdp/image)

;; A DWorld is (default-world%)
;; and implements
;; on-tick : DWorld -> DWorld
;; to-draw : DWorld -> Scene
;; ...
(define-class default-world%
  (define/public (on-tick)
    this)
  (define/public (to-draw)
    (empty-scene 300 300
                 )
    )
  )

(define-class circle-world%
  (super default-world%)
  (define/public (to-draw)
    (overlay (circle 20 "solid" "red")
             (empty-scene 300 300))))

(big-bang (circle-world%))
}