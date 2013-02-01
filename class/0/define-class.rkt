#lang racket
;; TODO after 2012: Kill function application syntax for method calls
;; within method definitions and bring back only after inheritance.
(provide (all-defined-out))
(provide send this)

(require (prefix-in isl+: (only-in lang/htdp-intermediate-lambda define))
         (prefix-in isl+: (only-in "../test-engine/racket-tests.rkt"
                                   check-expect check-within 
                                   check-error check-member-of
                                   check-range)))

(require racket/stxparam racket/splicing 
         (for-syntax syntax/parse racket/splicing racket/list
                     "define-class-helper.rkt"))
(require (prefix-in r: racket))

(define-syntax object% (class-name #'r:object%))

(define-syntax (fields stx) 
  (raise-syntax-error #f "can only be used inside define-class" stx))

(define-syntax (define-class stx)
  (define-syntax-class (member-name names)
    (pattern name:id
             #:fail-when 
             (eq? (syntax-e #'name) 'define) 
             "class members may not be named `define'"
             #:fail-when 
             (memf (λ (id) (eq? (syntax-e id) (syntax-e #'name))) names)
             "duplicate class member name"))
  (define-syntax-class (member-def names)
    #:literals (define)
    (pattern (define ((~var f (member-name names)) x:id ...) e:expr)
             #:with def #'(define/public (f x ...) e)))
  
  (syntax-parse stx #:literals (super #;implements fields 
                                      isl+:check-expect 
                                      isl+:check-within
                                      isl+:check-error
                                      isl+:check-member-of
                                      isl+:check-range)
    [(define-class class%      
       (~optional (fields (~var fld (member-name null)) ...)
                  #:defaults ([(fld 1) null]))
       (~or (~var <definition> (member-def (syntax->list #'(fld ...))))
            (~and ce (~or (isl+:check-expect <act> <exp>)
                          (isl+:check-within <act> <exp> <rng>)
                          (isl+:check-error <act> <msg>)
                          (isl+:check-error <act>)
                          (isl+:check-member-of <act> <exp0> <exp1> ...)
                          (isl+:check-range <act> <lo> <hi>))))
       ...)
     #:with -class% (datum->syntax #f (syntax-e #'class%))
     (with-syntax ([field (datum->syntax stx 'field)]
                   [(the-fld ...)
                    (generate-temporaries (syntax (fld ...)))])
       (quasisyntax
        (begin          
          ce ...
          (define-syntax class% (class-name #'-class%))          
          (r:define -class%
            (r:class/derived #,stx (class% r:object% 
                                           (r:writable<%>
                                            (interface* () ([prop:custom-print-quotable 'never])))
                                           #f)
              (r:inspect #f)
              
              (r:init-field [(the-fld fld)] ...)
              (r:super-new)
              (r:define/public (fld) the-fld)
              ...
              (void)
              (define/public (custom-write p) 
               (fprintf p "(new ~a" 'class%)
               (for ([i (list #,@(syntax->list #'(the-fld ...)))])                                  
                 (fprintf p " ~v" i))
               (fprintf p ")"))
              (define/public (custom-display p) (custom-write p))
	       
              <definition>.def
              ...)))))]))

(define-syntax (new stx)
  (syntax-parse stx
    [(_ cls:cls-name . args)
     #'(r:make-object cls.real-name . args)]))

