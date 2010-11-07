#lang racket/base
(require "define-class.rkt"
         lang/htdp-intermediate-lambda
         test-engine/racket-tests)
(provide (all-from-out "define-class.rkt")
         define test
         (except-out (all-from-out lang/htdp-intermediate-lambda) define))