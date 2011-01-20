#lang racket
(provide (all-defined-out))
(provide new send define/public define/private this field fields)

(require (prefix-in isl+: (only-in lang/htdp-intermediate-lambda define)))
(require (only-in "../class0/define-class.rkt" field fields))
(require racket/stxparam racket/splicing 
         (for-syntax syntax/parse racket/splicing racket/list
                     unstable/syntax
                     "define-class-helper.rkt"))
(require (prefix-in r: racket))

(define-syntax object% (class-name #'r:object% null null))

(define-syntax (super stx) 
  (raise-syntax-error #f "can only be used inside define-class" stx))
(define-syntax (implements stx)
  (raise-syntax-error #f "can only be used inside define-class" stx))

(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_ name:id (meths:id ...))
     #:with -ifc (datum->syntax #f (syntax-e #'name))
     #'(begin (define-syntax name (interface-name #'-ifc))
              (define -ifc (interface () meths ...)))]))

(define-syntax (define-class stx)
  (define-syntax-class (member-name names)
    (pattern name:id
             #:fail-when 
             (eq? (syntax-e #'name) 'field) 
             "class members may not be named `field'" 
             #:fail-when 
             (eq? (syntax-e #'name) 'define/public) 
             "class members may not be named `define/public'"
             #:fail-when
             (memf (λ (id) (eq? (syntax-e id) (syntax-e #'name))) names) 
             "duplicate class member name"))
  (define-syntax-class (member-def names)
    #:literals (define/public define/private)
    (pattern ((~or define/public #;define/private) 
              ((~var f (member-name names)) x:id ...) e:expr)))
  
  (syntax-parse stx #:literals (super implements fields)
    [(define-class class%
       (~optional (super super%:cls-name)
                  #:defaults ([super%.real-name #'r:object%]
                              [super%.fields null]
                              [super%.methods null]))
       (~optional (implements i%:ifc-name ...)
                  #:defaults ([(i%.real-name 1) null]
                              #;
                              [(i%.methods 1) null]))
       (~optional (fields (~var fld (member-name null)) ...)
                  #:defaults ([(fld 1) null]))
       (~var <definition> (member-def (syntax->list #'(fld ...))))
       ...)
     #:with -class% (datum->syntax #f (syntax-e #'class%))
     (with-syntax* ([field (datum->syntax stx 'field)]
                    [(the-fld ...)
                     (generate-temporaries (syntax (fld ...)))]
                    [(super-methods ...) (attribute super%.methods)]
                    [(meths ...) #'(super-methods ... <definition>.f ...)]
                    [(inherit-fld ...)
                     (map reverse (attribute super%.fields))]
                    ;; the real names
                    [(all-field-names ...)
                     (append (map second (attribute super%.fields))
                             (syntax->list #'(the-fld ...)))]
                    ;; the external names
                    [(all-flds ...)
                     (append (map first (attribute super%.fields))
                             (syntax->list #'(fld ...)))])
       #;
       (define all-fields (map list
                               (syntax->list #'(all-flds ...))                               
                               (syntax->list #'(all-field-names ...))))
       (quasisyntax
        (begin
          (define-syntax class% (class-name #'-class% 
                                            (list (list #'all-flds #'all-field-names) ...)
                                            (list #'meths ...)))
          (r:define -class%
            (r:class/derived #,stx (class% #;r:object% super%.real-name (i%.real-name ...) #f)
              (r:inspect #f)
              
              (r:inherit-field inherit-fld) ...
              (r:init-field [(the-fld fld)] ...)
              (r:inherit super-methods) ...
              (r:super-new)
              (r:define/public (fld) the-fld)
              ...
              (splicing-let-syntax
               ([field 
                  (λ (stx)
                    (syntax-parse stx
                      [(_ arg) 
                       (let ([r (assf (λ (id) (eq? id (syntax-e #'arg)))
                                      (list (list 'all-flds #'all-field-names) ...))])
                         (if r
                             (second r)
                             (raise-syntax-error #f 
                                                 "no field by that name" 
                                                 stx 
                                                 #'arg)))]))])
               (void)
               <definition>
               ...))))))]))

(define-syntax (new stx)
  (syntax-parse stx
    [(_ cls:cls-name . args)
     #'(r:make-object cls.real-name . args)]))

