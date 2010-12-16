#lang racket

(require 2htdp/universe)


(define-syntax-rule 
  (m on-key)
  
  
  (if (method-in-interface? 'on-key i)
                          (λ (w k)
                            (send w on-key k))
                          (λ (w k) w))))))
                           

(define (big-bang* o)
  (let ((i (object-interface o)))
    (big-bang o
              (if (method-in-interface? 'on-tick i)
                           (λ (w)
                             (send w on-tick))
                           (λ (w) w))
                       (if (method-in-interface? 'tick-rate i)
                           (send w tick-rate)
                           1/28))
              (to-draw (if (method-in-interface? 'to-draw i)
                           (λ (w)
                             (send w to-draw))
                           (error "Must supply a to-draw method.")))
              (on-key (m on-key))
    ))
    
   
                           
