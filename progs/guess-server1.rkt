#lang class/1
(provide thinking-of%)
(require class/universe)

(define-class thinking-of%
  (fields n)

  ;; on-msg : IWorld SExp -> Universe
  ;; Mail response to guess from given world
  
  (check-expect ((new thinking-of% 7) . on-msg iworld1 "Bogus")
                (new thinking-of% 7))
  (check-expect ((new thinking-of% 7) . on-msg iworld1 5)
                (make-bundle (new thinking-of% 7)
                             (list (make-mail iworld1 "too small"))
                             empty))  
  (define (on-msg iw msg)
    (cond [(and (number? msg) (real? msg))
           (make-bundle this
                        (list (make-mail iw (this . guess msg)))
                        empty)]
          [else this]))

  ;; A Response is one of:
  ;; - "too big"
  ;; - "too small"
  ;; - "just right"

  ;; guess : Real -> Response
  ;; Respond to a given guess
  (check-expect ((new thinking-of% 7) . guess 5) "too small")
  (check-expect ((new thinking-of% 7) . guess 9) "too big")
  (check-expect ((new thinking-of% 7) . guess 7) "just right")
  (define (guess m)
    (cond [(< m (this . n)) "too small"]
          [(> m (this . n)) "too big"]
          [(= m (this . n)) "just right"])))
