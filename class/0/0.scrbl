#lang scribble/manual

@(define-syntax-rule (def-racket define-id)
  (begin
    (require (for-label (only-in lang/htdp-intermediate-lambda define)))
    (define define-id (racket define))))
@(def-racket define-id)

@require[(for-label class/universe
		    (except-in class/0 check-expect)
		    (only-in lang/htdp-intermediate-lambda check-expect)
		    (prefix-in 2htdp: 2htdp/universe)
		    (except-in 2htdp/image image?))]

@title[#:style 'quiet]{Class 0}

@defmodulelang[class/0]

The @racket[class/0] language is the most basic version of the class
system.  In essence, it extends @secref[#:doc '(lib
"scribblings/htdp-langs/htdp-langs.scrbl")]{intermediate-lam} with the
ability to define classes, construct objects that are instances of
classes, and to send such objects messages to call methods.

@defform[(require module-name ...)]{
Imports all the modules named @racket[module-name]s.}

@defform/subs[#:literals (fields define)
	      (define-class class-name 
                 field-names
		 method-or-test ...)
	      ([field-names code:blank
                            (fields field-name ...)]
               [method-or-test method test]
	       [method (define (method-name variable ...) body)])]{

Defines a new class named @racket[class-name] with fields
@racket[field-name]s and methods @racket[method-name]s.
The class has one additional method for each field name
@racket[field-name], which access the field values.

A method is defined with @racket[(define (method-name variable ...)
body)].  Such a definition extends the set of messages every instance
of @racket[class-name] understands.  When an object is sent a
@racket[method-name] message and some values, the @racket[body] of the
method definition is evaluated with the values of the arguments in
place of the @racket[variable]s.  Within a method, @racket[this]
refers to the current object, i.e. the object whose method was called,
and @racket[(field field-name)] refers to the value of the field named
@racket[field-name] of the current object.

A @racket[test] can be any form of @racket[check-expect].
Conceptually, each @racket[test] is lifted out of the class definition
and does not exist inside any instance of @racket[class-name],
therefore @racket[test]s cannot reference fields using @racket[field]
or use @racket[this].}

@defform[(new class-name expression ...)]{ 

Constructs an object that is an instance of @racket[class-name] and
whose fields are the values of each @racket[expression].  The number
of @racket[expressions] must be equal to the number of fields in
@racket[class-name]'s definition.}

@defform[(send object method-name expression ...)]{ 

Sends @racket[object] the message @racket[method-name] with the values
of the @racket[expression]s as arguments, that is, call
@racket[object]'s method named @racket[method-name] with the given
arguments.}

@defform[(define (method-name variable ...) body)]{
Within a class definition, this form defines a method.
Outside of a class definition, this form defines a function;
it is equivalent to @|define-id| in ISL/λ.}

@deftogether[
[@defidform[this]
 @defform[(fields id ...)]
 @defform[(field field-name)]]]{

See @racket[define-class].  These forms are not allowed outside of a
method definition.

@deftogether[
[@defform[(define/public (method-name variable ...) body)]
 @defform[(define/private (method-name variable ...) body)]]]{
These forms are deprecated and should not be used in new code.
Use @racket[define] instead.}

}

