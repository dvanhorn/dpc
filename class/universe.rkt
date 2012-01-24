#lang racket

(require 2htdp/universe)
(require racket/class "tick-rate.rkt")
(provide (rename-out [big-bang* big-bang]
                     [universe* universe]))
(provide (except-out (all-from-out 2htdp/universe) big-bang universe) tick-rate)

(define-syntax m
  (syntax-rules ()
    [(m on-event)
     (λ (w . args)
       (if (method-in-interface? 'on-event (object-interface w))
           (send/apply w on-event args)
           w))]
    [(m on-event default)
     (λ (w . args)
       (if (method-in-interface? 'on-event (object-interface w))
           (send/apply w on-event args)
           (default w)))]))




(define (universe* o)
  (let ((i (object-interface o)))
    (universe o
              (on-new 
               (if (method-in-interface? 'on-new i)
                   (λ (u iw)
                     (send u on-new iw))
                   (error "Must supply an on-new method.")))
              
              (on-msg
               (if (method-in-interface? 'on-msg i)
                   (λ (u iw msg)
                     (send u on-msg iw msg))
                   (error "Must supply an on-msg method.")))
              
              (on-tick
               (m on-tick 
                  (λ (u)
                    (make-bundle u empty empty)))
               (if (method-in-interface? 'tick-rate i)
                   (send o tick-rate)
                   (if (method-in-interface? 'on-tick i)
                       1/28
                       1000000)))
              
              (on-disconnect
               (m on-disconnect
                  (λ (u)
                    (make-bundle u empty empty))))
              
              (state
               (if (method-in-interface? 'state i)
                   (send o state)
                   false))
              
              (to-string
               (if (method-in-interface? 'to-string i)
                   (λ (u)
                     (send u to-string))
                   (λ (u)
                     (format "~a" u))))
              
              (check-with
               (if (method-in-interface? 'check-with i)
                   (λ (u)
                     (send u check-with))
                   (λ (u)
                     true))))))                            
  
(define (big-bang* o)
  (let ((i (object-interface o)))
    (if (method-in-interface? 'register i)
	(big-bang o
		  (on-tick 
                   (m on-tick)
		   (if (method-in-interface? 'tick-rate i)
		       (send o tick-rate)
		       1/28))
		  (to-draw 
		   (if (method-in-interface? 'to-draw i)
		       (λ (w)
			  (send w to-draw))
		       (error "Must supply a to-draw method.")))
		  (on-key (m on-key))
		  (on-release (m on-release))
                  (on-mouse (m on-mouse))
                  (stop-when
                   (m stop-when (λ _ false))
                   (m last-image
                      (λ (w)
                        (send w to-draw))))
		  ;; check-with
		  (record? (if (method-in-interface? 'record? i)
			       (send o record?)
			       false))
		  (state (if (method-in-interface? 'state i)
			     (send o state)
			     false))
		  (name (if (method-in-interface? 'name i)
			    (send o name)
			    "World"))
		  (register (send o register))
                  (on-receive (m on-receive)))
                                    
	(big-bang o
		  (on-tick
		   (m on-tick)
		   (if (method-in-interface? 'tick-rate i)
		       (send o tick-rate)
		       1/28))
		  (to-draw 
		   (if (method-in-interface? 'to-draw i)
		       (λ (w)
			  (send w to-draw))
		       (error "Must supply a to-draw method.")))
                  (on-key (m on-key))
                  (on-release (m on-release))
                  (on-mouse (m on-mouse))
                  (stop-when
                   (m stop-when (λ (w) false))
                   (m last-image
                      (λ (w)
                        (send w to-draw))))
                  ;; check-with
		  (record? (if (method-in-interface? 'record? i)
			       (send o record?)
			       false))
		  (state (if (method-in-interface? 'state i)
			     (send o state)
			     false))
		  (name (if (method-in-interface? 'name i)
			    (send o name)
			    "World"))))))



