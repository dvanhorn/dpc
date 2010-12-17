#lang racket
(provide internal if-internal)
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