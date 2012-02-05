#lang racket/base
(require (except-in lang/htdp-intermediate-lambda define require #%module-begin
                    define-struct image? #%app
                    check-expect check-within check-error check-range check-member-of)
         "../test-engine/racket-tests.rkt"
         "../private/utils.rkt"
         (only-in racket/class send this define/private define/public)
         (prefix-in isl+: 
                    (combine-in
                     (only-in lang/htdp-intermediate-lambda define)
                     (only-in "../test-engine/racket-tests.rkt"
                              check-expect check-within 
                              check-error check-member-of
                              check-range)))
         racket/splicing          
         (for-syntax syntax/parse racket/list
                     racket/syntax racket/base
                     "../private/utils-stxtime.rkt")
         (prefix-in r: racket))

(provide (all-from-out lang/htdp-intermediate-lambda)
	 quote |.| define require provide
         all-defined-out only-in all-from-out except-in
         (except-out (all-from-out "../test-engine/racket-tests.rkt")
                     test))

(provide (rename-out [-module-begin #%module-begin]
                     [-define-struct define-struct]
                     [my-app #%app]))
(provide new send field fields this
         define-class define-interface super implements)

(define-syntax (define-class stx)  
  (syntax-parse stx #:literals (super implements fields)
    [(define-class class%
       (~optional (super super%:cls-name)
                  #:defaults ([super%.real-name #'r:object%]
                              [super%.fields null]
                              [super%.methods null]))
       ~!
       (~optional (implements i%:ifc-name ...)
                  #:defaults ([(i%.real-name 1) null]))
       ~!       
       ;; workaround for PR 12537
       (~describe #:opaque "a field declaration" f:fields-spec)
       ~!
       (~or (~var <definition> (member-def (syntax->list #'(f.fld ...))))
            ce:test)
       ...)
     #:fail-when (check-duplicate-identifier (syntax->list #'(f.fld ...)))
     "duplicate field name"
     #:fail-when (check-duplicate-identifier (syntax->list #'(<definition>.f ...)))
     "duplicate method name"
     #:with -class% (datum->syntax #f (syntax-e #'class%))
     (with-syntax* ([field (datum->syntax stx 'field)]
                    [(the-fld ...)
                     (generate-temporaries (syntax (f.fld ...)))]
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
                             (syntax->list #'(f.fld ...)))]
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
            (r:class/derived #,stx (class% super%.real-name 
                                           ((r:interface* () ([prop:custom-print-quotable 'never]))
                                            r:writable<%> i%.real-name ...) #f)
              (r:inspect #f)
              
              (r:inherit-field inherit-fld) ...
              (r:init-field [(the-fld f.fld)] ...)
              (r:inherit super-methods) ...
              (r:super-new)
              (r:define/public (f.fld) the-fld)
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
               (over (custom-write p) 
                (fprintf p "(new ~a" 'class%)
                (for ([i (list #,@(append (syntax->list #'(the-fld ...))
                                          (map second (attribute super%.fields))))])
                  (fprintf p " ~v" i))
                (fprintf p ")"))
               (over (custom-display p) (custom-write p))
	       
               <definition>.def
               ...))))))]))



                  
   