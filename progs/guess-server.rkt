#lang class/1
(require class/universe)

;; A Universe is a (new universe% [U #f Number] [U #f IWorld] [U #f IWorld]).
(define-class universe%
  (fields number
          picker
          guesser)
  
  ;; is the given world the picker?
  (define (picker? iw)
    (and (iworld? (this . picker))
         (iworld=? iw (this . picker))))
  
  ;; is the given world the guesser?
  (define (guesser? iw)
    (and (iworld? (this . guesser))
         (iworld=? iw (this . guesser))))
    
  (define (on-new iw)
    (cond [(false? (this . picker))
           (make-bundle
            (new universe% false iw false)
            (list (make-mail iw "pick a number"))
            empty)]          
          [(false? (this . guesser))
           (make-bundle
            (new universe% (this . number) (this . picker) iw)
            empty
            empty)]          
          [else
           (make-bundle this empty (list iw))]))
  
  (define (on-msg iw m)
    (cond [(and (picker? iw)
                (false? (this . number)))           
           (make-bundle
            (new universe% m (this . picker) (this . guesser))
            empty
            empty)]
          [(picker? iw) ;; already picked a number
           (make-bundle this empty empty)]
          [(and (guesser? iw)
                (number? (this . number)))
           (make-bundle this 
                        (list (make-mail iw (respond m (this . number))))
                        empty)]
          [(guesser? iw)
           (make-bundle this
                        (list (make-mail iw "no number"))
                        empty)])))
  

(define (respond guess number)
  (cond [(< guess number) "too small"]
        [(> guess number) "too big"]
        [else "just right"]))

(universe (new universe% false false false))