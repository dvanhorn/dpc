#lang scribble/manual

@title[#:tag "lec21"]{3/21: Under the Hood: Implementing OO}

@verbatim|{
News:

- Homework due Friday
- Exam 2, 110 WVH, 6-9pm
- Dan is holding Sam's office hours today
- We are compiling class notes for the exam just like last time.
  Notes will be up later today.

Pulling back the veil from object-oriented programming.

We will implement OO in ISL+lambda.

In Fundies 1 we saw a way to implement the language we were

We could write a Java compiler, which would help us program Java, but
the JLS is like 900 pages long.

Structs with functions in them.  That would be similar to having
objects because we'd have data with functionality bundled together.

We could also implement dictionary and use those to associate values
and fields and values and methods.

We could use built-in Racket objects.  That would be similar to what
we've done, and would be similar to structure and functions.

We could use functions to represents objects.

We could write an interpreter for class1 in ISL+.

We are going to start out by using functions to represent objects.
Ask yourself:  WHAT IS AN OBJECT?

- Data + functions - this is the "how they are made" or "what they are
  contructed out of".

Another view is: what do objects do?

- Objects respond to messages.

What are messages?  Messages are names.

So let's write something that responds to things that are names.
Let's make an example of something that responds to messages.  As an
example, we'll make a square object that responds to the messages:

- side
- area

If we make a square-10 object, what's its contract:

;; square-10 : Message -> Number

How should we represent messages?  Symbols.

;; A Message is a Symbol

;; square-10 : Message -> Number
(define (square-10 msg)
  (cond [(symbol=? msg 'side) 10]
        [(symbol=? msg 'area) 100]
        [else "message not understood"]))

(check-expect (square-10 'side) 10)
(check-expect (square-10 'area) 100)
(check-error  (square-10 'bad))

How would we write a square-5 object?

;; square-5 : Message -> Number
(define (square-5 msg)
  (cond [(symbol=? msg 'side) 5]
        [(symbol=? msg 'area) 25]
        [else "message not understood"]))

(check-expect (square-5 'side) 5)
(check-expect (square-5 'area) 25)
(check-error  (square-5 'bad))

Now we have two simple objects that look very similar.  Let's
abstract.

;; A Square is a Message -> Number
;; square% : Number -> Square
(define (square% side)
  (local [(define (the-square msg)
            (cond [(symbol=? msg 'side) side]
                  [(symbol=? msg 'area) (sqr side)]
                  [else (error "message not understood")])))
    the-square))

(define square-10 (square% 10))
(define square-5  (square% 5))

Why is what've done a little weird considering we said that an object
is data plus functions.

Depending on your perspective:

- We only have data.
- We only have functions.

Our messages are always just a symbol.  But what about arguments?

But there's something else.  Where did the data go?

The trick is that when we produce the square function, it remembers
the values it can see, like side, when it was created.  So the data is
remember in the function.  

*Functions are really code plus data.*

Basically a list of fields that map to values, plus code.

That's why we are able to implement objects so easily.

But how do we do inheritance?

Let's create another kind of shape -- let's create circles.

;; A Circle is a Message -> Number
;; Number -> Circle
(define (circle% radius)
  (local [(define (the-circle msg)
            (cond [(symbol=? msg 'radius) radius]
                  [(symbol=? msg 'area) (* radius radius pi)]
                  [else (error "message not understood")]))]
    the-circle))

(define circle-2 (circle% 2))
(check-expect (circle-2 'radius) 2)
(check-within (circle-2 'area) (* 2 2 pi) 0.0001)

There's a lot of repeated code here.  How can we abstract something
common to both of these definitions.

We have different code for handling message, but all of the objects
have the same code for the message that is not understood.

(define (dumb-object msg)
  (error "message not understood"))

This is an object -- not a constructor for an object.

Let's write a constructor for this:

(define (dumb%)
  (local [(define (the-dumb-object msg)
            (error "message not understood"))]
    the-dumb-object))

Now how could we use this?

(define (square% side super)
  (local [(define (the-square msg)
            (cond [(symbol=? msg 'side) side]
                  [(symbol=? msg 'area) (sqr side)]
                  [else ((super) msg)]))]
    the-square))

This is odd -- we can make squares with different super classes.
Let's fix that.

(define (square% side super)
  (local [(define (the-square msg)
            (cond [(symbol=? msg 'side) side]
                  [(symbol=? msg 'area) (sqr side)]
                  [else ((dumb%) msg)]))]
    the-square))

dumb% is actually object%.

(define (object%)
  (local [(define (the-dumb-object msg)
            (error "message not understood"))]
    the-dumb-object))

How many times are we going to create an object% objecct?  How do we
have it happen only once.

(define (square% side super)
  (local [(define super (object%))
          (define (the-square msg)
            (cond [(symbol=? msg 'side) side]
                  [(symbol=? msg 'area) (sqr side)]
                  [else (super msg)]))]
    the-square))

And likewise for circle:

(define (circle% radius)
  (local [(define super (object%))
          (define (the-circle msg)
            (cond [(symbol=? msg 'radius) radius]
                  [(symbol=? msg 'area) (* radius radius pi)]
                  [else (super msg)]))]
    the-circle))

We've now abstracted out the behavior of the error message.

But, let's come back to the observation that we can add methods to
object% that every object will now understand.

For example, we could add =?, but that sounds hard.  Let's do
something really simple:

(define (object%)
  (local [(define (the-dumb-object msg)
            (cond [(symbol=? msg 'hi) "Howdy"]
                  [else (error "message not understood")]))]
    the-dumb-object))


But we've broken the contract.  So we'll say instead that an object
can respond to any message and produce anything.

OK, let's see how it works:

(check-expect ((square% 10) 'hi) "Howdy")

Look!  We have inheritance!  All istances of subclasses of object%
understand the hi message!

This almost everything that is going on under the hood in classN,
Java, Ruby, etc.

But does this do overridding?

(define (square% side)
  (local [(define super (object%))
          (define (the-square msg)
            (cond [(symbol=? msg 'side) side]
                  [(symbol=? msg 'area) (sqr side)]
                  ;; Overriding the hi method.
                  [(symbol=? msg 'hi) "Good day, sir."]
                  [else (super msg)]))]
    the-square))

Great, but none of our messages take arguments.  How can we do that?
We could change our representation of a messages to include arguments.

;; A Message is a (make-msg Symbol [Listof Anything]).
(define-struct msg (name args))

;; Symbol Message -> Boolean
(define (msg-is? sym msg)
  (symbol=? sym (msg-name msg)))

(define (object%)
  (local [(define (the-dumb-object msg)
            (cond [(msg-is? 'hi msg) "Howdy"]
                  [else (error "message not understood"))))]
    the-dumb-object))

(check-expect ((object%) (make-msg 'hi empty)) "Howdy")

Another approach, revert back to Message = Symbol.

;; An Object is a Message [Listof Symbol] -> Anything

(define (object%)
  (local [(define (the-dumb-object msg args)
            (cond [(msg-is? 'hi msg) "Howdy"]
                  [else (error "message not understood"))))]
    the-dumb-object))

Another approach, return a function that takes the arguments.

Suppose we want to add a multiply method to circles.

(define (circle% radius)
  (local [(define super (object%))
          (define (the-circle msg)
            (cond [(symbol=? msg 'radius) radius]
                  [(symbol=? msg 'area) (* radius radius pi)]
                  [(symbol=? msg 'multiply)
                   (lambda (factor)
                     (circle% (* factor radius)))]
                  [else (super msg)]))]
    the-circle))

These contracts suck.  We really want to talk about the contract of
each method that is supported by an object.

;; A Circle is a Object that implements
;; 'radius -> Number
;; 'area -> Number
;; 'multiply -> (Number -> Circle)


(((circle% 10) 'multiply) 4) => circle with radius 40

(check-expect ((((circle% 10) 'multiply) 4) 'radius) 40)

So we've got classes, objects, inheritance, overriding, and basically
everything you'd want in a class system.

We might like to have a nice notation to make it more convenient to
write programs in this style, but this is really all that is going on.

Suppose we add a field to square%, called area, which is computed at
construction time and store away in the field.

(define (square% side)
  (local [(define super (object%))
          (define area (sqr side))
          (define (the-square msg)
            (cond [(symbol=? msg 'side) side]
                  [(symbol=? msg 'area) area]
                  ;; Overriding the hi method.
                  [(symbol=? msg 'hi) "Good day, sir."]
                  [else (super msg)]))]
    the-square))

So we can write constructors that do computation.

What about this?

(define (square% side)
  (local [(define super (object%))
          (define area (sqr side))
          (define (this msg)
            (cond [(symbol=? msg 'side) side]
                  [(symbol=? msg 'area) area]
                  ;; Overriding the hi method.
                  [(symbol=? msg 'hi) "Good day, sir."]
                  [else (super msg)]))]
    this))

Let's redefine area to use.  So in class1 we might write (sqr (this
. side)).  We'll if we pick our names better, it should become
obvious.

(define (square% side)
  (local [(define super (object%))
          (define (this msg)
            (cond [(symbol=? msg 'side) side]
                  [(symbol=? msg 'area) (sqr (this 'side))]
                  ;; Overriding the hi method.
                  [(symbol=? msg 'hi) "Good day, sir."]
                  [else (super msg)]))]
    this))

Now I want to step back and look at a different approach for doing
this.

Guiding principle: Data + Functions

;; A Method is a Function.
;; An Object is a (make-obj [Listof Any] [Listof Method]).

(define-struct obj (fields methods))

But the "is a Function" contract is not very useful, but really we
don't know what the contract on a method is until later.

Let's create our simple square-10 object:

(define square-10 (make-obj (list ...) (list ...)))

What should go in these lists?

(define square-10 
  (make-obj (list 10) 
            (list 
              ;; side : -> Number
              (lambda () 10)
              ;; area : -> Number
              (lambda () 100))))

(check-expect ((first (obj-methods square-10))) 10)
(check-expect ((second (obj-methods square-100))) 100)

What's wrong with this?  Nothing is called by name.

Methods can't access the fields!  What the hell is going on here?

Here's an idea: pass the object itself to the methods.

(define square-10 
  (make-obj (list 10) 
            (list 
              ;; side : -> Number
              (lambda (itself) 
                (first (object-fields itself)))
              ;; area : -> Number
              (lambda (itself) 
                (* (first (obj-fields itself))
                   (first (obj-fields itself)))))))

(check-expect ((first (obj-methods square-10)) square-10) 10)
(check-expect ((second (obj-methods square-100)) square-10) 100)

(define square-10 
  (make-obj (list 10) 
            (list 
              ;; side : -> Number
              (lambda (itself) 
                (first (object-fields itself)))
              ;; area : -> Number
              (lambda (itself) 
                (* ((first (obj-methods itself)) itself)
                   ((first (obj-methods itself)) itself))))))

A better name for itself: this!

It's annoying to program like this, but we can abstract this

;; Object Name -> Anything
(define (send obj meth)
  ...)

Why would you ever do this?  Every single object-oriented language
you've programmed in works like this: it has a table of data and
functions and those functions take as its first object the object
itself.

Python makes you write self as the first argument, which is just
exposing this implementation detail.

Why would you do one or the other?

The functional style is slow, but easy.

The structural style is fast, but hard. 


Here's a question: where in the methods do we need to refer to
square-10?  Nowhere.  Thus we can easily lift the methods out of the
definition for square-10.

(define square-methods
  (list 
    ;; side : -> Number
    (lambda (itself) 
      (first (object-fields itself)))
    ;; area : -> Number
    (lambda (itself) 
      (* ((first (obj-methods itself)) itself)
         ((first (obj-methods itself)) itself)))))


(define square-10 
  (make-obj (list 10) square-methods))

(define square-5
  (make-obj (list 5) square-methods))

(define (square% side)
  (make-obj (list side) square-methods))

}|