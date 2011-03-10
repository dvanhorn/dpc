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
                     unstable/syntax
                     "define-class-helper.rkt"))
(require (prefix-in r: racket))

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

(define-syntax (super stx) 
  (raise-syntax-error #f "can only be used inside define-class" stx))
(define-syntax (constructor stx) 
  (raise-syntax-error #f "can only be used inside define-class" stx))
(define-syntax (implements stx)
  (raise-syntax-error #f "can only be used inside define-class" stx))

(define-syntax (define-interface stx)
  (syntax-parse stx #:literals (super)
    [(_ name:id (super super-ifc:ifc-name) ... (meths:id ...))
     #:with -ifc (datum->syntax #f (syntax-e #'name))
     #'(begin (define-syntax name (interface-name #'-ifc))
              (define -ifc (interface (super-ifc.real-name ...) meths ...)))]))

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
    #:literals (define/public define/private define/override)
    (pattern ((~or define/public) 
              ((~var f (member-name names)) x:id ...) e:expr)
             #:with def #'(define (f x ...) e)))
  
  (syntax-parse stx #:literals (super implements fields constructor
                                      isl+:check-expect 
                                      isl+:check-within
                                      isl+:check-error
                                      isl+:check-member-of
                                      isl+:check-range)
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
       (~optional (constructor (cargs:id ...)
                   cbody:expr cextra-body:expr ...)
                   #:defaults ([(cargs 1) (generate-temporaries #`(fld ... . #,(map second (attribute super%.fields))))]
                               [cbody #`(#,(datum->syntax stx 'fields) cargs ...)]
                               [(cextra-body 1) null]))
       (~or (~var <definition> (member-def (syntax->list #'(fld ...))))
            (~and ce (~or (isl+:check-expect <act> <exp>)
                          (isl+:check-within <act> <exp> <rng>)
                          (isl+:check-error <act> <msg>)
                          (isl+:check-error <act>)
                          (isl+:check-member-of <act> <exp0> <exp1> ...)
                          (isl+:check-range <act> <lo> <hi>))))
       ...)
     #:with -class% (datum->syntax #f (syntax-e #'class%))
     (with-syntax* ([field (datum->syntax stx 'field)]
                    [set-field! (datum->syntax stx 'set-field!)]
                    [fake-this (datum->syntax stx 'this)]
                    [fields (datum->syntax stx 'fields)]
                    [(the-fld ...)
                     (generate-temporaries (syntax (fld ...)))]
                    [(the-fld2 ...)
                     (generate-temporaries (syntax (fld ...)))]
                    [(super-methods/inherit ...) (for/list ([m (attribute super%.methods)]
                                                            #:when (not (memf (λ (e) (free-identifier=? m e))
                                                                              (syntax->list #'(<definition>.f ...)))))
                                                   m)]
                    [(super-methods/over ...) (for/list ([m (attribute super%.methods)]
                                                            #:when (memf (λ (e) (free-identifier=? m e))
                                                                         (syntax->list #'(<definition>.f ...))))
                                                   m)]
                    [(meths/new ...) (for/list ([m (syntax->list #'(<definition>.f ...))]
                                                #:when (not (memf (λ (e) (free-identifier=? m e))
                                                                  (syntax->list #'(super-methods/over ...)))))
                                                   m)]
                    [(meths ...) #'(super-methods/inherit ... super-methods/over ... <definition>.f ...)]
                    [(inherit-fld ...)
                     (map second (attribute super%.fields))]
                    ;; the real names
                    [(all-field-names ...)
                     (append (map second (attribute super%.fields))
                             (syntax->list #'(the-fld ...)))]
                    ;; the external names
                    [(all-flds ...)
                     (append (map first (attribute super%.fields))
                             (syntax->list #'(fld ...)))]
                    [over (if (free-identifier=? #'super%.real-name #'r:object%)
                              #'r:define/public ;; don't override if we're the top class
                              #'r:define/override)])
       (quasisyntax
        (begin
          ce ...
          (define-syntax class% (class-name #'-class% 
                                            (list (list #'all-flds #'all-field-names) ...)
                                            (list #'meths ...)))
          (r:define -class%
            (r:class/derived #,stx (class% #;r:object% super%.real-name 
                                           ((interface* () ([prop:custom-print-quotable 'never]))
                                            r:writable<%> i%.real-name ...) #f)
              (r:inspect #f)
              
              (r:inherit-field inherit-fld) ...
                   ;(r:field the-fld) ...
              (r:init cargs ...)
              (r:field (the-fld (void)) ...)
              (r:let-values ([(the-fld2 ... inherit-fld ...)
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
                            (r:set! the-fld the-fld2) ...
                            (r:super-make-object inherit-fld ...))
              (r:inherit super-methods/inherit) ...
              (r:override super-methods/over) ...
              #;(r:super-new)
              (r:define/public (fld) the-fld)
              ...
	      (splicing-let-syntax
	       ([set-field!
		 (λ (stx)
                    (syntax-parse stx
                      [(_ arg expr)
                       (let ([r (assf (λ (id) (eq? id (syntax-e #'arg)))
                                      (list (list 'all-flds #'all-field-names) ...))])
                         (if r
                             (with-syntax ([rs (second r)])
                               (syntax/loc stx (set! rs expr)))
                             (raise-syntax-error #f 
                                                 "no field by that name" 
                                                 stx 
                                                 #'arg)))]))]
                [field 
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
               (over (custom-write p) 
                (fprintf p "(object:~a" 'class%)
                (for ([i (list #,@(append (syntax->list #'(the-fld ...))
                                          (map second (attribute super%.fields))))])
                  (fprintf p " ~v" i))
                (fprintf p ")"))
               (over (custom-display p) (custom-write p))
	       
               (public meths/new) ...
               <definition>.def
               ...
               (begin cextra-body ...)))))))]))

(define-syntax (new stx)
  (syntax-parse stx
    [(_ cls:cls-name . args)
     #'(r:make-object cls.real-name . args)]))
