#lang racket
(provide (all-defined-out))
(provide new send define/public define/private define/override this field fields set-field!)

(require (prefix-in isl+: (only-in lang/htdp-intermediate-lambda define)))
(require (prefix-in isl+: (only-in "../class1/test-engine/racket-tests.rkt"
                                   check-expect check-within 
                                   check-error check-member-of
                                   check-range)))
(require (only-in "../class0/define-class.rkt" fields))
(require racket/stxparam racket/splicing 
         (for-syntax syntax/parse racket/splicing racket/list
                     unstable/syntax racket/syntax
                     "define-class-helper.rkt")
         "define-class-helper.rkt")
(require (prefix-in r: racket))

(define top-class (make-class-wrapper r:object%))

;; for field construction
(define-struct field-wrapper (vals))

(define-syntax set-field!
  (lambda (stx)
    (raise-syntax-error #f "can only be used inside define-class" stx)))
(define-syntax field
  (lambda (stx)
    (raise-syntax-error #f "can only be used inside define-class" stx)))

(define (wrap . x) (make-field-wrapper x))

(define-syntax object% (class-name #'r:object% null null))

(define-syntax super (make-rename-transformer #'r:super))
(define-syntax (constructor stx) 
  (raise-syntax-error #f "can only be used inside define-class" stx))
(define-syntax (name stx) 
  (raise-syntax-error #f "can only be used inside define-class" stx))
(define-syntax (implements stx)
  (raise-syntax-error #f "can only be used inside define-class" stx))

(define-syntax (define-interface stx)
  (syntax-parse stx #:literals (super)
    [(_ name:id (super super-ifc:ifc-name) ... (meths:id ...) (~optional (fields:id ...) #:defaults ([(fields 1) null])))
     #:with -ifc (datum->syntax #f (syntax-e #'name))
     #'(begin (define-syntax name (interface-name #'-ifc
                                                   (list #'super-ifc.fields ... ... #'fields ...)
                                                   (list #'meths ...)))
              (define -ifc (interface (super-ifc.real-name ...) meths ... fields ...)))]))

(define-syntax-rule (define-class nm . body)
  (define nm (class (name nm) . body)))

(define-syntax (class stx)
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
    (pattern ((~literal define/public) 
              ((~var f (member-name names)) x:id ...) e:expr)             
             #:with def (with-syntax ([super (datum->syntax #'e 'super)])
                          #'(define (f x ...) (let-syntax ([super (syntax-parser
                                                                   [(_ args (... ...))
                                                                    #'(r:super f args (... ...))])])
                                                e)))))
  
  (syntax-parse stx #:literals (super implements fields constructor name
                                      isl+:check-expect 
                                      isl+:check-within
                                      isl+:check-error
                                      isl+:check-member-of
                                      isl+:check-range)
    [(class
       (~optional (name nm:id) #:defaults ([nm #'#f]))
       (~optional (super super%:expr super-ifc%:ifc-name)
                  #:defaults ([super% #'top-class]
                              [(super-ifc%.fields 1) null]
                              [(super-ifc%.field-internals 1) null]
                              [(super-ifc%.methods 1) null]))
       (~optional (implements i%:ifc-name ...)
                  #:defaults ([(i%.real-name 1) null]))
       (~optional (fields (~var fld (member-name null)) ...)
                  #:defaults ([(fld 1) null]))
       (~optional (constructor (cargs:id ...)
                   cbody:expr cextra-body:expr ...)
                   #:defaults ([(cargs 1) (generate-temporaries #`(fld ... super-ifc%.fields ...))]
                               [cbody #`(#,(syntax-local-introduce (datum->syntax #f 'fields)) cargs ...)]
                               [(cextra-body 1) null]))
       (~or (~var <definition> (member-def (syntax->list #'(fld ...))))
            (~and ce (~or (isl+:check-expect <act> <exp>)
                          (isl+:check-within <act> <exp> <rng>)
                          (isl+:check-error <act> <msg>)
                          (isl+:check-error <act>)
                          (isl+:check-member-of <act> <exp0> <exp1> ...)
                          (isl+:check-range <act> <lo> <hi>))))
       ...)
     ;#:with -class% (datum->syntax #f (syntax-e #'class%))
     (with-syntax* ([field (syntax-local-introduce (datum->syntax #f 'field))]
                    [set-field! (syntax-local-introduce (datum->syntax #f 'set-field!))]
                    [fake-super (syntax-local-introduce (datum->syntax #f 'super))]

                    [fake-this (syntax-local-introduce (datum->syntax #f 'this))]
                    [fields (syntax-local-introduce (datum->syntax #f 'fields))]
                    [(all-flds ...) #'(fld ... super-ifc%.fields ...)]
                    [(field-internals ...) (for/list ([f (syntax->list #'(fld ...))])
                                             (format-id f "_~a" f))]
                    [(all-field-internals ...) #'(field-internals ... super-ifc%.field-internals ...)]
                    [(super-methods/inherit ...) (for/list ([m (syntax->list #'(super-ifc%.methods ... super-ifc%.fields ...))]
                                                            #:when (not (memf (λ (e) (eq? (syntax-e m) (syntax-e e)))
                                                                              (syntax->list #'(<definition>.f ...)))))
                                                   m)]
                    [(super-methods/over ...) (for/list ([m (syntax->list #'(super-ifc%.methods ... super-ifc%.fields ...))]
                                                            #:when (memf (λ (e) (eq? (syntax-e m) (syntax-e e)))
                                                                         (syntax->list #'(<definition>.f ...))))
                                                   m)]
                    [(meths/new ...) (for/list ([m (syntax->list #'(<definition>.f ...))]
                                                #:when (not (memf (λ (e) (eq? (syntax-e m) (syntax-e e)))
                                                                  (syntax->list #'(super-methods/over ...)))))
                                                   m)]
                    [(meths ...) #'(super-methods/inherit ... super-methods/over ... <definition>.f ...)]
                    [over (if (free-identifier=? #'super% #'top-class)
                              #'r:define/public ;; don't override if we're the top class
                              #'r:define/override)])
       (quasisyntax/loc stx
        (begin
          ce ...
          #;(define-syntax class% (class-name #'-class% 
                                            (list (list #'all-flds #'all-field-names) ...)
                                            (list #'meths ...)))
          (make-class-wrapper
           (r:class/derived #,stx (nm (class-wrapper-val super%)
                                      ((interface* () ([prop:custom-print-quotable 'never]))
                                       r:writable<%> i%.real-name ...) #f)
                            (r:inspect #f)
                            
                            (r:inherit-field super-ifc%.field-internals) ...
                            
                            (r:init cargs ...)
                            (r:field (field-internals (void)) ...)
                            (r:let-values ([(fld ... super-ifc%.fields ...)
                                            (let-syntax ([fields (make-rename-transformer #'wrap)]
                                                         [fake-this
                                                          (λ (stx)
                                                            (raise-syntax-error 
                                                             #f 
                                                             "`this' may not be used before initialization"
                                                             stx))])
                                              (let ([v cbody])
                                                (if (field-wrapper? v)
                                                    (apply values (field-wrapper-vals v))
                                                    (error 
                                                     'class% 
                                                     "constructor must use `fields' to produce result"))))])
                                          (r:set! field-internals fld) ...
                                          (r:super-make-object super-ifc%.fields ...))
                            (r:inherit super-methods/inherit) ...
                            (r:override super-methods/over) ...              
                            (r:define/public (fld) field-internals) ...
                            (splicing-let-syntax
                             ([set-field!
                               (λ (stx)
                                 (syntax-parse stx
                                   [(_ arg expr)
                                    (let ([r (memf (λ (id) (eq? (syntax-e id) (syntax-e #'arg)))
                                                   (syntax->list #'(all-flds ...)))])
                                      (if r
                                          (with-syntax ([rs (format-id #'arg "_~a" #'arg)])
                                            (syntax/loc stx (set! rs expr)))
                                          (raise-syntax-error #f 
                                                              "no field by that name" 
                                                              stx 
                                                              #'arg)))]))]
                              [field 
                               (λ (stx)
                                 (syntax-parse stx
                                   [(_ arg) 
                                    (let ([r (memf (λ (id) (eq? (syntax-e id) (syntax-e #'arg)))
                                                   (syntax->list #'(all-flds ...)))])
                                      (if r
                                          (with-syntax ([rs (format-id #'arg "_~a" #'arg)])
                                            (syntax/loc stx rs))
                                          (raise-syntax-error #f 
                                                              "no field by that name" 
                                                              stx 
                                                              #'arg)))]))])
                             (void)
                             (over (internal-write p)                                   
                                   (for ([i (list field-internals ...)])
                                     (fprintf p " ~v" i))
                                   #,(if (free-identifier=? #'super% #'top-class)
                                         #'(void)
                                         #'(r:super internal-write p)))
                             (over (custom-write p)
                                   (fprintf p "(object:~a" (if 'nm 'nm 'class%))
                                   (internal-write p)
                                   (fprintf p ")"))
                             
                             (over (custom-display p) (custom-write p))
                             
                             (public meths/new) ...
                             <definition>.def
                             ...
                             (begin cextra-body ...)))))))]))

(define-syntax (new stx)
  (syntax-parse stx
    [(_ cls:expr . args)
     #'(r:make-object (class-wrapper-val cls) . args)]))

(define-syntax (instanceof stx)
  (syntax-parse stx
    [(_ e:expr cls:expr)
     #'(r:is-a? e (class-wrapper-val cls))]))