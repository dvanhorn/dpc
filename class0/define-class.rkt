#lang racket

(provide (all-defined-out))
(provide send define/public this)

(require (prefix-in isl+: (only-in lang/htdp-intermediate-lambda define)))

(provide (rename-out [define define/value]))

(require racket/stxparam racket/splicing 
         (for-syntax syntax/parse racket/splicing racket/list
                     "define-class-helper.rkt"))
(require (prefix-in r: racket))
(define-syntax object% (class-name #'r:object%))

(define-syntax field (λ (stx) (raise-syntax-error #f "can only be used inside define-class" stx)))
(define-syntax fields (λ (stx) (raise-syntax-error #f "can only be used inside define-class" stx)))
(define-syntax super (λ (stx) (raise-syntax-error #f "can only be used inside define-class" stx)))
(define-syntax implements (λ (stx) (raise-syntax-error #f "can only be used inside define-class" stx)))

(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_ name:id (meths:id ...))
     #:with -ifc (datum->syntax #f (syntax-e #'name))
     #'(begin (define-syntax name (interface-name #'-ifc))
              (define -ifc (interface () meths ...)))]))

(define-syntax (define-class stx)
  (define-syntax-class (member-name names)
    (pattern name:id
             #:fail-when (eq? (syntax-e #'name) 'field) "class members may not be named `field'" 
             #:fail-when (eq? (syntax-e #'name) 'define/public) "class members may not be named `define/public'"
             #:fail-when (memf (λ (id) (eq? (syntax-e id) (syntax-e #'name))) names) "duplicate class member name"))
  (define-syntax-class (member-def names)
    #:literals (define/public define)
    (pattern ((~or define/public define/private define) ((~var f (member-name names)) x:id ...)  e:expr))
    [pattern (define (~var f (member-name names)) e:expr)])
  
 (syntax-parse stx #:literals (super implements fields)
   [(define-class class%
      (~optional (super super%:cls-name)
                 #:defaults ([super%.real-name #'r:object%]))
      (~optional (implements i%:ifc-name ...)
                 #:defaults ([(i%.real-name 1) null]))
      (~optional (fields (~var fld (member-name null)) ...)
                 #:defaults ([(fld 1) null]))
      (~var <definition> (member-def (syntax->list #'(fld ...))))
      ...)
    #:with -class% (datum->syntax #f (syntax-e #'class%))
    (with-syntax ([field (datum->syntax stx 'field)]
                  [(the-fld ...)
                   (generate-temporaries (syntax (fld ...)))])
      (quasisyntax
       (begin
         (define-syntax class% (class-name #'-class%))
         (r:define -class%
           (r:class/derived #,stx (class% super%.real-name (i%.real-name ...) #f)
             (r:inspect #f)
             
             (r:init-field [(the-fld fld)] ...)
             (r:super-new)
             (r:define/public (fld) the-fld)
             ...
             (splicing-let-syntax
              ([field (λ (stx)
                        (syntax-parse stx
                          [(_ arg) (let ([r (assf (λ (id) (free-identifier=? id #'arg))
                                                  (list (list #'fld #'the-fld) ...))])
                                     (if r
                                         (second r)
                                         (raise-syntax-error #f "no field by that name" stx #'arg)))]))])                           
              <definition>
              ...))))))]))

(define-syntax (new stx)
  (syntax-parse stx
    [(_ cls:cls-name . args)
     #'(r:new cls.real-name . args)]))

