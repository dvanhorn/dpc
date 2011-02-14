#lang scribble/manual
@require["../utils.rkt" (for-label class3)]

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class3))
    the-eval))

@title[#:style 'quiet]{Class 3}

@defmodulelang[class3]

@defform[(require module-name ...)]{
Imports all the modules named @racket[module-name]s.}

@defform[(provide id ...)]{
Makes all of the @r[id]s available to other modules.}


@defform/subs[#:literals (fields constructor implements define/public define/private super)
              (define-class class-name 
                super-spec
		implements-spec
                fields-spec
                constructor-spec
                method-spec ...)
              ([super-spec code:blank
                           (super super-name)]
	       [implements-spec code:blank
	       			(implements interface-name ...)]
               [fields-spec code:blank
                            (fields field-name ...)]
               [constructor-spec code:blank
                            (constructor (arg ...) body ...)]
               [method-spec (define/public (method-name arg ...)
                              body)
                            (define/private (method-name arg ...)
                              body)])]{

Defines a new class named @racket[class-name], just as with
@racketmodname[class2].  However, the body of each method may use
@r[set-field!] to change the value of a field.  For example.

@interaction[#:eval the-eval
(define-class c%
  (fields x)
  (define/public (double-x!) (set-field! x (* (field x) 2))))
(define c (c% 5))
(send c x)
(send c double-x!)
(send c x)]
}

@defform[(set-field! f e)]{
Changes the value of field @r[f] to be the result of @r[e].
}

@deftogether[
[@defform[(super class-or-interface-name)]
 @defform[(implements interface-name ...)]
 @defform[(constructor (args ...) body)]]]{
See @racket[define-class] and @racket[define-interface].}

@deftogether[
[@defidform[this]
 @defform[(fields id ...)]
 @defform[(define/public (method-name id ...) body)]
 @defform[(define/private (method-name id ...) body)]
 @defform[(new class-name arg ...)]
 @defform[(field field-name)]
 @defform[(send object message arg ...)]]]{
These have the same meaning as in @racketmodname[class0].}

@defform/subs[#:literals (super)
(define-interface interface-name 
  (super super-interface) ... 
  (method-name ...))
	      ()]{This has the same meaning as in @racketmodname[class1].}

}

@section[#:style (list 'toc-hidden 'hidden)]{Object-oriented Universe (class 3)}

@defmodule[class3/universe]

@deftogether[
[@defproc[(big-bang [obj World]) World]
 @defproc[(universe [obj Universe]) Universe]]]{
These have the same meaning as in @racketmodname[class0/universe].}
