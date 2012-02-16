#lang scribble/manual
@require[(for-label class/2)]

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/2))
    the-eval))

@title[#:style 'quiet]{Class 2}

@defmodulelang[class/2]

@defform[(require module-name ...)]{
Imports all the modules named @racket[module-name]s.}

@defform[(provide id ...)]{
Makes all of the @racket[id]s available to other modules.}


@defform/subs[#:literals (fields constructor implements define super)
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
               [method-spec (define (method-name arg ...)
                              body)])]{

Defines a new class named @racket[class-name], much like as with
@racketmodname[class/0], however a class may declare @racket[super-name]
as a super class of @racket[class-name] by using @racket[(super
super-name)].  A class may also declare to implement a number of
interfaces using @racket[(implements interface-name ...)].

When a class definition declares a super class, it inherits all of the
super class's fields and methods.  

If a method definition defines the same method name as a method in the
super class, then that method definition @emph{overrides} the
definition in the super class.  The super class method can be called with 

If no @racket[constructor-spec] is provided, the default constructor simply
accepts as many  arguments as the class has fields, and initializes
them in positional order, with subclass arguments before superclass
arguments.  

If a @racket[constructor-spec] is provided, then constructing the object
takes as many arguments as the @racket[constructor-spec] has @racket[args].  The
provided values are bound to the @racket[args], and the @racket[body] of the
constructor is run.  The constructor must use the @racket[fields] form to
produce values to use to initialize the fields of the object.  

The constructor for the class takes
values for its fields followed by values for the fields of the super
class.

@interaction[#:eval the-eval
(define-class c%
  (fields x)
  (define (m) 'm!))
(define-class d%
  (super c%)
  (fields y)
  (define (n) 'n!))
(define d (new d% 'y! 'x!))
(send d m)
(send d n)
(send d x)
(send d y)]

@interaction[#:eval the-eval
(define-class mul%
  (fields a b prod)
  (constructor (x y)
    (fields x y (* x y))))
(new mul% 5 7)]

When a class definition declares to implement any interfaces, it must
define all the methods of the interfaces, otherwise an error is
signalled.}

@deftogether[
[@defform[(super class-or-interface-name)]
 @defform[(implements interface-name ...)]
 @defform[(constructor (args ...) body)]]]{
See @racket[define-class] and @racket[define-interface].}

@deftogether[
[@defidform[this]
 @defidform[|.|]
 @defform[(fields id ...)]
 @defform[(define (method-name id ...) body)]
 @defform[(new class-name arg ...)]
 @defform[(field field-name)]
 @defform[(send object message arg ...)]]]{
These have the same meaning as in @racketmodname[class/1].}

@defform/subs[#:literals (super)
(define-interface interface-name 
  (super super-interface) ... 
  (method-name ...))
	      ()]{
Defines a new interface named @racket[interface-name].
Classes declaring to implement @racket[interface-name] must
provide methods implementing each of the @racket[method-name]s, 
as well as all methods declared in any of the @racket[super-interface]s.  
The @racket[super-interface]s must name previously defined interfaces.  
}

