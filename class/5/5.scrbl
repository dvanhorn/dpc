#lang scribble/manual
@require["../utils.rkt" (for-label class5)]

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class5))
    the-eval))

@title[#:style 'quiet]{Class 5}

@defmodulelang[class5]

@defform[(require module-name ...)]{
Imports all the modules named @racket[module-name]s.}

@defform[(provide id ...)]{
Makes all of the @r[id]s available to other modules.}

@defform/subs[#:literals (fields constructor implements define/public
                                 define/private super check-expect)
              (class 
                name-spec
                super-spec
		implements-spec
                fields-spec
                constructor-spec
                method/test-spec ...)
              ([name-spec code:blank
                          (name class-name)]
               [super-spec code:blank     
                           (super super-expr super-interface-name)]
	       [implements-spec code:blank
	       			(implements interface-name ...)]
               [fields-spec code:blank
                            (fields field-name ...)]
               [constructor-spec code:blank
                            (constructor (arg ...) body ...)]
               [method-spec (define/public (method-name arg ...)
                              body)
                            (check-expect actual expected)])]{

Defines a class value.  Inherits the methods and fields specified in
        @r[super-interface-name], and is a subclass of the class that
        @r[super-expr] evaluates to.
}



@defform/subs[#:literals ()
              (define-class class-name body-forms ...) ()]{
The same as @r[(define class-name (class (name class-name) body-forms ...))].
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
 @defform[(new class-name arg ...)]
 @defform[(field field-name)]
 @defform[(send object message arg ...)]]]{
These have the same meaning as in @racketmodname[class0].}

@defform/subs[#:literals (super)
(define-interface interface-name 
  (super super-interface) ... 
  (method-name ...)
  (field-name ...))
	      ()]{Describes a class with the given methods and fields.}

}

@section[#:style (list 'toc-hidden 'hidden)]{Object-oriented Universe (class 5)}

@defmodule[class4/universe]

@deftogether[
[@defproc[(big-bang [obj World]) World]
 @defproc[(universe [obj Universe]) Universe]]]{
These have the same meaning as in @racketmodname[class0/universe].}
