#lang class1
(require class1/universe)


;; A Universe is a (new universe% [U #f Number] [U #f IWorld] [U #f IWorld]).

(define-class universe%
  (fields number
          picker
          guesser)
  
  ;; is the given world the picker?
  (define/public (picker? iw)
    (and (iworld? (field picker))
         (iworld=? iw (field picker))))
  
  ;; is the given world the guesser?
  (define/public (guesser? iw)
    (and (iworld? (field guesser))
         (iworld=? iw (field guesser))))
    
  (define/public (on-new iw)
    (cond [(false? (field picker))
           (make-bundle
            (new universe% false iw false)
            (list (make-mail iw "pick a number"))
            empty)]          
          [(false? (field guesser))
           (make-bundle
            (new universe% (field number) (field picker) iw)
            empty
            empty)]          
          [else
           (make-bundle this empty (list iw))]))
  
  (define/public (on-msg iw m)
    (cond [(and (picker? iw)
                (false? (field number)))           
           (make-bundle
            (new universe% m (field picker) (field guesser))
            empty
            empty)]
          [(picker? iw) ;; already picked a number
           (make-bundle this empty empty)]
          [(and (guesser? iw)
                (number? (field number)))
           (make-bundle this 
                        (list (make-mail iw (respond m (field number))))
                        empty)]
          [(guesser? iw)
           (make-bundle this
                        (list (make-mail iw "no number"))
                        empty)]
          #;
          [else
           (make-bundle this empty empty)])))
  

(define (respond guess number)
  (cond [(< guess number) "too small"]
        [(> guess number) "too big"]
        [else "just right"]))

(universe (new universe% false false false))