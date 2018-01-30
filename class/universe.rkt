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

;; Any -> Bundle
(define (bundlize v)
  (cond [(bundle? v) v]
        [else (make-bundle v empty empty)]))


(define (universe* o)
  (let ((i (object-interface o)))
    (universe o
              (on-new                
               (λ (u iw)
                 (if (method-in-interface? 'on-new (object-interface u))
                     (bundlize (send u on-new iw))                  
                     (make-bundle u empty empty))))
              
              (on-msg
               (λ (u iw msg)
                 (if (method-in-interface? 'on-msg (object-interface u))
                     (bundlize (send u on-msg iw msg))
                     (make-bundle u empty empty))))

              (on-tick
               (if (method-in-interface? 'on-tick i)
                   (λ (u)
                     (bundlize (send u on-tick)))                   
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
               (λ (u)
                 (if (method-in-interface? 'to-string (object-interface u))
                     (send u to-string)                   
                     (format "~a" u))))
              
              (check-with               
               (λ (u)
                 (if (method-in-interface? 'check-with (object-interface u))
                     (send u check-with)
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



