#lang racket/base
(require "define-class.rkt"
         (except-in lang/htdp-intermediate-lambda define require)
         test-engine/racket-tests)
(provide (all-from-out "define-class.rkt")
         (all-from-out lang/htdp-intermediate-lambda)
         define test require provide
         all-defined-out only-in all-from-out)
