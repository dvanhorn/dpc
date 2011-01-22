#lang scribble/manual
@require[(for-label class1)]

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class1))
    the-eval))

@title[#:style 'quiet]{Class 1}

@defmodulelang[class1]

@defform[(require module-name ...)]{
Imports all the modules named @racket[module-name]s.}

@defform/subs[#:literals (fields implements define/public define/private super)
              (define-class class-name 
                super-spec
		implements-spec
                fields-spec
                method-spec ...)
              ([super-spec code:blank
                           (super super-name)]
	       [implements-spec code:blank
	       			(implements interface-name ...)]
               [fields-spec code:blank
                            (fields field-name ...)]
               [method-spec (define/public (method-name arg ...)
                              body)
                            (define/private (method-name arg ...)
                              body)])]{

Defines a new class named @racket[class-name], much like as with
@racketmodname[class0], however a class may declare @racket[super-name]
as a super class of @racket[class-name] by using @racket[(super
super-name)].  A class may also declare to implement a number of
interfaces using @racket[(implements interface-name ...)].

When a class definition declares a super class, it inherits all of the
super class's fields and methods.  The constructor for the class takes
values for its fields followed by values for the fields of the super
class.

@interaction[#:eval the-eval
(define-class c%
  (fields x)
  (define/public (m) 'm!))
(define-class d%
  (super c%)
  (fields y)
  (define/public (n) 'n!))
(define d (new d% 'y! 'x!))
(send d m)
(send d n)
(send d x)
(send d y)]


When a class definition declares to implement any interfaces, it must
define all the methods of the interfaces, otherwise an error is
signalled.}

@deftogether[
[@defform[(super class-name)]
 @defform[(implements interface-name ...)]]]{
See @racket[define-class].}

@deftogether[
[@defidform[this]
 @defform[(fields id ...)]
 @defform[(define/public (method-name id ...) body)]
 @defform[(define/private (method-name id ...) body)]
 @defform[(new class-name arg ...)]
 @defform[(field field-name)]
 @defform[(send object message arg ...)]]]{
These have the same meaning as in @racketmodname[class0].}

@defform/subs[(define-interface interface-name (method-name ...))
	      ()]{
Defines a new interface named @racket[interface-name].
Classes declaring to implement @racket[interface-name] must
provide methods implementing each of the @racket[method-name]s.}

@section[#:style 'hidden]{Object-oriented Universe (class 1)}

@defmodule[class1/universe]

@deftogether[
[@defproc[(big-bang [obj World]) World]
 @defproc[(universe [obj Universe]) Universe]]]{
These have the same meaning as in @racketmodname[class0/universe].}
