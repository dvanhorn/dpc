#lang class1
;; ==========================================================
;; Parametric lists

;; A [Listof X] implements list<%>.
(define-interface list<%>
  [;; X -> [Listof X]
   ;; Add the given element to the front of the list.
   cons   
   ;; -> [Listof X]
   ;; Produce the empty list.
   empty 
   ;; -> Nat
   ;; Count the number of elements in this list.
   length
   ;; [Listof X] -> [Listof X]
   ;; Append the given list to the end of this list.
   append
   ;; -> [Listof X]
   ;; Reverse the order of elements in this list.
   reverse
   ;; [X -> Y] -> [Listof Y]
   ;; Construct the list of results of applying the function
   ;; to elements of this list.
   map
   ;; [X -> Boolean] -> [Listof X]
   ;; Construct the list of elements in this list that 
   ;; satisfy the predicate.
   filter
   ;; [X Y -> Y] Y -> Y
   ;; For elements x_0...x_n, (f x_0 ... (f x_n b)).
   foldr   
   ;; [X Y -> Y] Y -> Y
   ;; For elements x_0...x_n, (f x_n ... (f x_0 b)).
   foldl])

(define-class list%
  (define/public (length)
    (send this foldr (λ (x s) (add1 s)) 0))
  
  (define/public (append ls)
    (send this foldr
          (λ (x ls) (send ls cons x))
          ls))
  
  (define/public (reverse)
    (send this foldl 
          (λ (x ls) (send ls cons x))
          (send this empty)))
  
  (define/public (map f)
    (send this foldr 
          (λ (x ls) (send ls cons (f x)))
          (send this empty)))
  
  (define/public (filter p)
    (send this foldr
          (λ (x ls)
            (cond [(p x) (send ls cons x)]
                  [else ls]))
          (send this empty))))


;; ==========================================================
;; [Listof X] implemented as a recursive union.

;; A [RListof X] is one of:
;; - (new empty%)
;; - (new cons% X [RListof X])

(define-class rlist%
  (super list%)
  (define/public (empty)
    (new empty%))
  (define/public (cons x)
    (new cons% x this)))

(define-class cons%
  (super rlist%)
  (implements list<%>)
  (fields first rest)
    
  (define/public (foldr c b)
    (c (field first)
       (send (field rest) foldr c b)))
  
  (define/public (foldl c b)
    (send (field rest) foldl c 
          (c (field first) b))))

(define-class empty%  
  (super rlist%)
  (implements list<%>) 
  (define/public (foldr c b) b)  
  (define/public (foldl c b) b))


;; ==========================================================
;; [Listof X] implemented as a wrapper.

;; A [WListof X] is a (new wlist% [RacketListof X])
;; A [RacketListof X] is one of:
;; - empty
;; - (cons X [RacketListof X])

(define ls:foldl foldl)
(define ls:foldr foldr)
(define ls:cons cons)
(define ls:empty empty)

(define-class wlist%
  (super list%)
  (implements list<%>)
  (fields ls)
  (define/public (cons x)
    (new wlist% (ls:cons x (field ls))))
  (define/public (empty)
    (new wlist% ls:empty))
  (define/public (foldl c b)
    (ls:foldl c b (field ls)))
  (define/public (foldr c b)
    (ls:foldr c b (field ls))))


;; ==========================================================
;; Test cases

(define wmt (new wlist% empty))
(define wls (new wlist% (list 2 3 4)))
(define rmt (new empty%))
(define rls (new cons% 2 (new cons% 3 (new cons% 4 rmt))))

(check-expect (send wls length) 3)
(check-expect (send rls length) 3)

(check-expect (send wls append wmt) wls)
(check-expect (send wmt append wls) wls)
(check-expect (send wls append wls)
              (new wlist% (list 2 3 4 2 3 4)))

(check-expect (send rls append rmt) rls)
(check-expect (send rmt append rls) rls)
(check-expect (send rls append rls)
              (new cons% 2 (new cons% 3 (new cons% 4 rls))))

(check-expect (send wmt reverse) wmt)
(check-expect (send wls reverse) (new wlist% (list 4 3 2)))

(check-expect (send rmt reverse) rmt)
(check-expect (send rls reverse) 
              (new cons% 4 (new cons% 3 (new cons% 2 rmt))))

(check-expect (send wmt map add1) wmt)
(check-expect (send wls map add1) 
              (new wlist% (list 3 4 5)))

(check-expect (send rmt map add1) rmt)
(check-expect (send rls map add1)
              (new cons% 3 (new cons% 4 (new cons% 5 rmt))))
              
(check-expect (send rmt filter even?) rmt)
(check-expect (send rls filter even?)
              (new cons% 2 (new cons% 4 rmt)))

(check-expect (send wmt filter even?) wmt)
(check-expect (send wls filter even?)
              (new wlist% (list 2 4)))

(check-expect (send wmt foldr cons empty) empty)
(check-expect (send wls foldr cons empty) (list 2 3 4))

(check-expect (send rmt foldr cons empty) empty)
(check-expect (send rls foldr cons empty) (list 2 3 4))

(check-expect (send wmt foldl cons empty) empty)
(check-expect (send wls foldl cons empty) (list 4 3 2))

(check-expect (send rmt foldl cons empty) empty)
(check-expect (send rls foldl cons empty) (list 4 3 2))

;; Inter-representation append!
(check-expect (send wls append rls)
              (new cons% 2 (new cons% 3 (new cons% 4 rls))))
(check-expect (send rls append wls)
              (new wlist% (list 2 3 4 2 3 4)))

