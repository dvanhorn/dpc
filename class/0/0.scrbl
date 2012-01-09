#lang scribble/manual

@require[(for-label class/universe
		    class/0
		    (prefix-in 2htdp: 2htdp/universe)
		    (except-in 2htdp/image image?))]

@title[#:style 'quiet]{Class 0}

@defmodulelang[class/0]

@defform[(require module-name ...)]{
Imports all the modules named @racket[module-name]s.}

@defform/subs[#:literals (fields define/public define/private)
	      (define-class class-name 
                 fields-spec
		 method-spec ...)
	      ([fields-spec code:blank
			    (fields field-name ...)]
	       [method-spec (define/public (method-name arg ...)
			      body)
			    (define/private (method-name arg ...)
			      body)])]{

Defines a new class named @racket[class-name] with fields
@racket[field-name]s and methods @racket[method-name]s.
The class has one additional method for each field name
@racket[field-name], which access the field values.

Methods defined with @racket[define/public] are accessible both inside
and outside of the class definition, while methods defined with
@racket[define/private] are only accessible within the class
definition.

To refer to a field within the class definition, use @racket[(field
field-name)].

Methods may be invoked within the class definition using the function
call syntax @racket[(method-name arg #,(racketidfont "..."))], but
must be invoked with @racket[send] from oustide the class definition
as in @racket[(send object method-name arg #,(racketidfont "..."))].

The name @racket[this] is implicitly bound to the current object,
i.e. the object whose method was called.

To construct an instance of @racket[class-name], use @racket[(new
class-name arg #,(racketidfont "..."))] with as many arguments as there are fields in the
class.}

@deftogether[
[@defidform[this]
 @defform[(fields id ...)]
 @defform[(define/public (method-name id ...) body)]
 @defform[(define/private (method-name id ...) body)]]]{
See @racket[define-class].}

@defform[(new class-name arg ...)]{Creates an object which is an instance of
@racket[class-name] with @racket[arg]s as the values for the fields.}

@defform[(field field-name)]{References the value of the field named
@racket[field-name].}

@defform[(send object message arg ...)]{ 

Send @racket[object] the message @racket[message] with arguments
@racket[arg]s, that is, invoke @racket[object]'s @racket[message]
method with the given arguments, @racket[arg]s.}
