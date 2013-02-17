#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 check-expect define-struct ... length
                                numerator denominator))
          (for-label 2htdp/image)
          (for-label (only-in test-engine/racket-tests check-expect))
          (for-label class/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/2))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title{Circular Data}

Books & Authors

Books have:
title : String
author : Author

Authors have:
name : String
books : [Listof Book]

As data def:
@codeblock{
;; A Book is (book% String Author)
;; An Author is (author% String [Listof Book])
}

Can we make an @tt{Author}?
@codeblock{
(author% "Rose" empty)
(book% "Reign of Roquet" (author% "Rose" empty))
}

But this is wrong: the Rose has written a book, but the author object
doesn't know about it.

Do we need books to know the author? Yes.

We've seen this before with graph structure.  We represented graphs as
association lists, using symbolic names. 
@codeblock{
;; A Book is (book% String Author)
(define-class book%
  (fields title author))

;; An Author is (author% String [Listof Book])
(define-class author%
  (fields name books))

(define rose  (author% "Rose" empty))
(define reign (book% "Reign of Roquet" rose))
}
But:
@codeblock{
reign
rose
}

Question: Does @r[reign] contain a copy of @r[rose], or are they all
the same @r[rose]?  Answer: always the same, because we use the name
@r[rose], we didn't construct a new one.

Let's add a new method for modifying the author after a book is
written:
@classblock{
(define (add-book b)
  (set-field! books (cons b (this . books))))
}

Now we change our example:
@classblock{
(define rose  (author% "Rose" empty))
(define reign (book% "Reign of Roquet" rose))
(rose . add-book reign)
}

@(the-eval
   '(module b class/3
      (provide book% author% rose reign)
      ;; A Book is (book% String Author)
      (define-class book%
        (fields title author))
      
      ;; An Author is (author% String [Listof Book])
      (define-class author%
        (fields name books)
        (define (add-book b)
          (set-field! books (cons b (send this books)))))

      
      (define rose  (author% "Rose" empty))
      (define reign (book% "Reign of Roquet" rose))
      (send rose add-book reign)))

@(the-eval '(require 'b))

How does it print? 

@interaction[#:eval the-eval
rose
reign
]

See graph-style printing.

But every times we construct a book with an author, we want to use
@r[add-book].  So, let's use the constructor.  

@codeblock{
(constructor (t a)
  (fields t a)
  (send a add-book this))
}

In the first expression, we @emph{cannot} use @r[this], and we must produce
the result using @r[fields].  Later, we can use @r[this], and we get the
desired result. 
