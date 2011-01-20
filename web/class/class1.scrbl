#lang scribble/manual

@require[(for-label class1)]

@title[#:style 'quiet]{Class 1}

@defmodulelang[class1]

@defform[(require module-name ...)]{
Imports all the modules named @racket[module-name]s.}

@defform/subs[#:literals (fields define/public define/private super)
              (define-class class-name 
                super-spec
                fields-spec
                method-spec ...)
              ([super-spec code:blank
                           (super super-name)]
               [fields-spec code:blank
                            (fields field-name ...)]
               [method-spec (define/public (method-name arg ...)
                              body)
                            (define/private (method-name arg ...)
                              body)])]{

blah.}

@defform/subs[#:literals ()
              (define-interface interface-name (method-name ...))
	      ()]{
blah.}