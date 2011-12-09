#lang racket

(require 2htdp/universe)
(require racket/class)
(provide (rename-out [big-bang* big-bang]
                     [universe* universe]))
(provide (except-out (all-from-out 2htdp/universe) big-bang universe))

(define-syntax-rule 
  (m on-event i)
  (if (method-in-interface? 'on-event i)
      (λ (w . args)
	 (send/apply w on-event args))
      (λ (w . _) w)))

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
               (if (method-in-interface? 'on-tick i)                   
                   (λ (u)
                     (send u on-tick))
                   (λ (u)
                     (make-bundle u empty empty)))
               (if (method-in-interface? 'tick-rate i)
                   (send o tick-rate)
                   (if (method-in-interface? 'on-tick i)
                       1/28
                       1000000)))
              
              (on-disconnect
               (if (method-in-interface? 'on-disconnect i)
                   (λ (u iw)
                     (send u on-disconnect iw))
                   (λ (u iw)
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
                       (λ (w) false))
                   (if (method-in-interface? 'last-image i)
                       (λ (w)
                         (send w last-image))
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
                  (on-receive (if (method-in-interface? 'on-receive i)
                                  (λ (w m)
                                    (send w on-receive m))
                                  (λ (w m) w))))
                                    
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
                       (λ (w) false))
                   (if (method-in-interface? 'last-image i)
                       (λ (w)
                         (send w last-image))
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



