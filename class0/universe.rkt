#lang racket

(require 2htdp/universe)
(require racket/class)
(provide (rename-out [big-bang* big-bang]))
(provide (except-out (all-from-out 2htdp/universe) big-bang))

(define-syntax-rule 
  (m on-event i)
  (if (method-in-interface? 'on-event i)
      (λ (w . args)
        (send/apply w on-event args))
      (λ (w . _) w)))
                           

(define (big-bang* o)
  (let ((i (object-interface o)))
    (big-bang o
              (on-tick
               (if (method-in-interface? 'on-tick i)
                   (λ (w)
                     (send w on-tick))
                   (λ (w) w))
               (if (method-in-interface? 'tick-rate i)
                   (send o tick-rate)
                   1/28))
              (to-draw 
               (if (method-in-interface? 'to-draw i)
                   (λ (w)
                     (send w to-draw))
                   (error "Must supply a to-draw method.")))
              (on-key (m on-key i))
              (on-release (m on-release i))
              (on-mouse (m on-mouse i))
              (stop-when
               (if (method-in-interface? 'stop-when i)
                   (λ (w)
                     (send w stop-when))
                   (λ (w) false)))
              ;; check-with
              (record? (if (method-in-interface? 'record? i)
                           (send o record?)
                           false))
              (state (if (method-in-interface? 'state i)
                         (send o state)
                         false))
              (name (if (method-in-interface? 'name i)
                        (send o name)
                        "World")))))
    
   
                           
