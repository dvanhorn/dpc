#lang racket/base
(require (except-in lang/htdp-intermediate-lambda define require #%module-begin
                    define-struct image? 
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
         define require provide
         all-defined-out only-in all-from-out except-in
         (except-out (all-from-out "../test-engine/racket-tests.rkt")
                     test))

(provide new send field fields this
         define-class)
;; REMOVE -- see bug 16
(provide define/public define/private)

(provide (rename-out [-module-begin #%module-begin]
                     [-define-struct define-struct]))

(define-syntax (define-class stx)    
  (syntax-parse stx
    [(define-class class%
       ;; workaround for PR 12537
       (~describe #:opaque "a field declaration" f:fields-spec) ~!
       (~or (~var <definition> (member-def (syntax->list #'(f.fld ...))))
            ce:test) ...)
     #:fail-when (check-duplicate-identifier (syntax->list #'(f.fld ...)))
     "duplicate field name"
     #:fail-when (check-duplicate-identifier (syntax->list #'(<definition>.f ...)))
     "duplicate method name"
     #:with -class% (datum->syntax #f (syntax-e #'class%))
     (with-syntax ([field (datum->syntax stx 'field)]
                   [(the-fld ...) (generate-temporaries #'(f.fld ...))])
       (quasisyntax
        (begin          
          ce ...
          (define-syntax class% (class-name #'-class% 
                                            (list (list #'f.fld #'the-fld) ...)
                                            (list #'<definition>.f ...)))
          (r:define -class%
            (r:class/derived #,stx (class% r:object% 
                                           (r:writable<%>
                                            (r:interface* () ([prop:custom-print-quotable 'never])))
                                           #f)
              (r:inspect #f)
              
              (r:init-field [(the-fld f.fld)] ...)
              (r:super-new)
              (r:define/public (f.fld) the-fld)
              ...
              (splicing-let-syntax
               ([field (λ (stx)
                         (syntax-parse stx
                           [(_ arg) 
                            (let ([r (assf (λ (id) (eq? id (syntax-e #'arg)))
                                           (list (list 'f.fld #'the-fld) ...))])
                              (if r
                                  (second r)
                                  (raise-syntax-error #f 
                                                      "no field by that name" 
                                                      stx 
                                                      #'arg)))]))])
               (void)
               (r:define/public (custom-write p) 
                (fprintf p "(new ~a" 'class%)
                (for ([i (list #,@(syntax->list #'(the-fld ...)))])
                  (fprintf p " ~v" i))
                (fprintf p ")"))
               (r:define/public (custom-display p) (custom-write p))
	       
               <definition>.def
               ...))))))]))

                  
    
