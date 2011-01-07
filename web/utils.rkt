#lang racket
(provide internal if-internal scheme-from-file)
(define-syntax (internal stx)
  (syntax-case stx ()
    [(internal . forms)
     (if (getenv "STUDENTS")
	 #'(begin)
	 #'(begin . forms))]))

(define-syntax (if-internal stx)
  (syntax-case stx ()
    [(if-internal e1 e2)
     (if (getenv "STUDENTS")
	 #'e2
	 #'e1)])) 

(require (for-syntax racket syntax/strip-context))


(define-for-syntax (check-tab l filename)
  (when (regexp-match #rx"[\t]" l)
    (error 'from-file "file ~s contains tab characters" filename)))

(define-for-syntax (extract-from-file filename start end)
  (parameterize ([read-accept-reader #t])
  (call-with-input-file filename
    (λ (port)
        (let loop ([in? #f])
          (let ([l (read-line port 'any)])
            (cond
             [(eof-object? l)
              '()]
             [(and (not in?) 
                   (regexp-match start l)
                   (regexp-match end l))
              (list l "\n")]
             [(and (not in?) (regexp-match start l))
              (check-tab l filename)
              (list* l "\n" (loop #t))]
             [(and in? (regexp-match end l))
              (check-tab l filename)
              (list l "\n")]
             [in?
              (check-tab l filename)
              (list* l "\n" (loop #t))]
             [else
              (loop #f)])))))))
             
(require scribble/manual)

(define-syntax (scheme-from-file stx)
  (syntax-case stx ()
    [(_ fname #:start start #:end end)
     (let* ([l (extract-from-file (syntax-e #'fname) 
                                  (syntax-e #'start)
                                  (if (eq? (syntax-e #'end) 'same)
                                      (syntax-e #'start)
                                      (syntax-e #'end)))])
       #`(codeblock #:indent 0 #,(datum->syntax stx (car l)) #,@(cdr l)))]))

#;

(define-syntax (scheme-from-file stx)
  (syntax-case stx ()
    [(_ fname #:start start #:end end SCH)
     (let* ([l (extract-from-file (syntax-e #'fname) 
                                  (syntax-e #'start)
                                  (if (eq? (syntax-e #'end) 'same)
                                      (syntax-e #'start)
                                      (syntax-e #'end)))]
            [s (if (eq? (syntax-e #'end) 'same)
                   (string-append (apply string-append l)
                                  "\ncode:blank")
                   (apply string-append l))]
            [p (open-input-string s)]
            [os 
             (with-handlers ((exn:fail:read?
                              (λ (x)
                                (fprintf
                                 (current-error-port)
                                 "error reading from ~s, start ~s end ~s\n ~a"
                                 (syntax-e #'fname)
                                 (syntax-e #'start)
                                 (syntax-e #'end)
                                 (exn-message x)))))
               (port-count-lines! p)
               (let loop ([x null])
                 (let ([v (read-syntax (syntax-e #'fname) p)])
                   (if (eof-object? v)
                       (reverse x)
                       (loop (cons (replace-context #'fname v)
                                   x))))))])
       #`(begin (SCH . #,os)
                #;
                (make-styled-paragraph '(".1in") "vspace*")))]
    [(_ fname #:start start #:end end)
     #'(scheme-from-file fname #:start start #:end end schemeblock0)]
    ))