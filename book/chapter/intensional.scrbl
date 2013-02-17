#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 check-expect define-struct ... length
                                numerator denominator))
          (for-label 2htdp/image)
          (for-label (only-in test-engine/racket-tests check-expect))
          (for-label class/universe))

@title{Intensional equality}

Mutation exposes yet another sense in which two things may be consider
@emph{"the same"}: two things are the same if mutating one mutates the
other.

How can we tell if two posns are two different names for the same
thing, or if they're two different posns with the same contents?  

For example:

@codeblock{
(define p1 (posn% 3 4))
(define p2 (posn% 3 4))
}

or 

@codeblock{
(define p1 (posn% 3 4))
(define p2 p1)
}

These are very different in the presence of mutation.  We have a way
of testing this: @r[eq?].  Similarly, the @r[equal?] function checks
structural equality.  

But that didn't shed any light on the question.  Is there a way we can
check this @emph{in} our language?

Answer: yes.  Do some operation to one of them that changes the state
of the object, and see if it @emph{also} happens to the other one.

Drawback: you can't necessarily undo this operation.  

@codeblock{
(define (really-the-same? p1 p2)
  ....)
}

Now we need some operation to perform on @r[p1].  

@filebox[@tt{posn%}]{
@codeblock{
;; -> (posn% 'black-eye Number)
(define (punch!)
  (begin
    (set-field! x 'black-eye)
    this))
}
}

@racketblock[
(define sam (posn% 3 4))
(send sam punch!)
]

"I punched him so hard, I punched him right out of the data
defintion."

Now we can define @r[really-the-same?].

@codeblock{
(define (really-the-same? p1 p2)
  (begin
    (send p1 punch!)
    (symbol? (send p2 x))))
}

@racketblock[
(really-the-same? p1 p2)
p1
]

Now @r[p1] is permanently broken, and can't be undone.  So
@r[really-the-same?] is a very problematic, and you should use
@r[eq?], which uses DrRacket's internal knowledge of where things came
from to answer this question without changing the objects.  

Question:

@racketblock[
(eq? p1 (send p1 punch!))]

Produces true.

@racketblock[
(send p2 =? (send p2 punch!))
]

Produces true (or crashes).


@racketblock[
(send (send p3 punch!) =? p3)
]

Produces true (or crashes).

Question:
Does intensional equality imply structural equality?  Yes.
