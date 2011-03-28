#lang racket
(require syntax/parse (for-template racket/base racket/class) unstable/syntax)
(provide (all-defined-out))



(define-struct class-name (id flds methods)
  #:property prop:procedure 
  (λ (inst stx) 
    (syntax-parse stx
      [(_ args ...)
       #`(make-object #,(class-name-id inst) args ...)]
      [i:id
       #`(lambda a (apply make-object #,(class-name-id inst) a))])))

(define-syntax-class cls-name
  #:description "a class name"
  #:opaque
  (pattern name:id
           #:attr v (syntax-local-value #'name (λ () #f))
           #:fail-unless 
           (class-name? (attribute v)) 
           "expected a class name"
           #:attr fields (class-name-flds (attribute v))
           #:attr methods (map syntax-local-introduce (class-name-methods (attribute v)))
           #:with real-name (class-name-id (attribute v))))

(define-struct (interface-name class-name) ()
        #:property prop:procedure 
        (λ (inst stx) 
          (raise-syntax-error #f 
                              "interface names may not be used as expressions" 
                        stx)))

(define-syntax-class ifc-name
  #:description "an interface name"
  #:opaque
  (pattern name:id
           #:attr v (syntax-local-value #'name (λ () #f))
           #:fail-unless 
           (interface-name? (attribute v))
           "expected an interface name"
           #:with real-name (class-name-id (attribute v))
           #:with (fields ...) (map syntax-local-introduce (class-name-flds (attribute v)))
           #:with (field-internals ...) (for/list ([f (class-name-flds (attribute v))])
                                          (format-id f "_~a" f))
           #:with (methods ...) (map syntax-local-introduce (class-name-methods (attribute v)))
           ))

(define-struct class-wrapper (val)
  #:property prop:custom-write
  (λ (v p w?)
    (if w? 
        (write (class-wrapper-val v) p)
        (print (class-wrapper-val v) p)))
  #:property prop:procedure (λ (v . args) (apply make-object (class-wrapper-val v) args)))

