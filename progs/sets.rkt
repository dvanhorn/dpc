#lang racket
(require racket/class)
(require test-engine/racket-tests)

;; -----------------------------------------------------------------------
;; Set library

;; This library uses equivalence relations, represented as 
;; functional classes with method equivalent? : [X X -> Boolean] 
;; to define set operations.

;; This library does not construct functions.

;; It is useful for representing sets of values.

;; A [Set X] implements:
(define set-interface
  (interface () 
    empty?    ;; -> Boolean
    contains? ;; X -> Boolean
    remove    ;; X -> [Set X]
    union     ;; [Set X] -> [Set X]
    intersect ;; [Set X] -> [Set X]
    =?        ;; [Set X] -> Boolean
    subset?   ;; [Set X] -> Bolean
    map       ;; [Map X Y] [Equiv Y] -> [Set Y]
    length    ;; -> Nat
    fold      ;; [Combine X Y] Y -> Y
    filter    ;; [Pred X] -> [Set X]
    #;choose))  ;; -> X


(define-syntax define-interface
  (syntax-rules ()
    [(define-interface i (id ...))
     (define i (interface () id ...))]))

(define-syntax define-class 
  (syntax-rules (super implements fields)
    [(define-class class%
       (super super%)
       (implements i%)
       ...
       (fields fld ...)
       . <definitions>)
     (define class%
       (class* super% (i% ...)
         (inspect #f)
         (super-new)
         (init-fields [(the-fld fld)] ...)
         . <definitions>))]))
         
    

#;#;
(define-interface i (id ...))
(define-class set% 
  (super id)
  (implements id)
  ...
  (fields id ...)
  
  (define/public (m a ...) b)
  (define (f a ...) b)
  (define x v)
  ...)

  
  
    
;; A [Set X] is one of:
;; - (new mt-set% [= [Equiv X]])
;; - (new cons-set% [elem X] [rest [Set X]]),
;;   where elem is *not* an element in rest.
      
;; Functional classes

;; A [Map X Y] implements:
(define map-interface
  (interface () apply)) ;; X -> Y

;; A [Combine X Y] implements:
(define combine-interface
  (interface () combine)) ;; X Y -> Y
    
;; A [Equiv X] implements:
(define equiv-interface
  (interface () equivalent?)) ;; X X -> Boolean

;; A [Pred X] implements:
(define pred-interface
  (interface () pred?)) ;; X -> Boolean


  

(define set% 
  (class* object% ()
    (super-new)
    (init-field [(the-= =)])
    
    ;; -> [Equiv X]
    (define/public (=) the-=)
 
    ;; [Set X] -> Boolean
    (define/public (=? s)
      (and (send this subset? s)
           (send s subset? this)))
    
    ;; [Set X] -> [Set X]
    (define/public (union s)
      (send this fold 
            (new (class* object% (combine-interface)
                   (super-new)
                   ;; X [Set X] -> [Set X]
                   (define/public (combine x xs)
                     (send xs add x))))
            s))
    
    ;; -> Nat
    (define/public (length)
      (send this fold 
            (new (class* object% (combine-interface)
                   (super-new)
                   ;; X Nat -> Nat
                   (define/public (combine x n)
                     (add1 n))))
            0))
    
    ;; [Pred X] -> [Set X]
    (define/public (filter f)
      (send this fold
            (new (class* object% (combine-interface)
                   (super-new)
                   ;; X [Set X] -> [Set X]
                   (define/public (combine x xs)
                     (if (send f pred? x)
                         (send xs add x)
                         xs))))
            (new mt-set% [= the-=])))
    
    ;; [Map X Y] [Equiv Y] -> [Set Y]
    (define/public (map f =)
      (send this fold 
            (new (class* object% (combine-interface)
                   (super-new)
                   ;; X [Set Y] -> [Set Y]
                   (define/public (combine x ys)
                     (send ys add (send f apply x)))))
            (new mt-set% [= =])))))

(define mt-set%
  (class* set% (set-interface)
    (super-new)
    (define/public (empty?) true)
    (define/public (contains? x) false)
    (define/public (remove x)
      this)
    (define/public (add x)
      (new cons-set% [elem x] [rest this]))
    (define/public (intersect s)
      this)
    (define/public (subset? s)
      true)
    (define/public (fold f b)
      b)))
      

(define cons-set%
  (class* set% (set-interface)
    (init-field [(the-elem elem)] [(the-rest rest)])
    (super-new [= (send the-rest =)])
    (inherit =)
    
    ;; -> Boolean
    (define/public (empty?) false)
    
    ;; X -> Boolean
    (define/public (contains? x)
      (or (send (=) equivalent? the-elem x)
          (send the-rest contains? x)))
    
    ;; X -> [Set X]
    (define/public (remove x)
      (if (send (=) equivalent? x the-elem)
          the-rest
          (new this% 
               [elem the-elem]
               [rest (send the-rest remove x)])))
    
    ;; [Set X] -> [Set X]
    (define/public (intersect s)
      (if (send s contains? the-elem)
          (new this%
               [elem the-elem]
               [rest (send the-rest intersect s)])
          (send the-rest intersect s)))
    
    ;; [Set X] -> Boolean
    (define/public (subset? s)
      (and (send s contains? the-elem)
           (send the-rest subset? s)))
    
    ;; X -> [Set X]
    (define/public (add x)
      (cond [(contains? x) this]
            [else
             (new this% [elem x] [rest this])]))
    
    ;; [Combine X Y] Y -> Y
    (define/public (fold f b)
      (send f combine the-elem (send the-rest fold f b)))))
  
  
;; [Equiv Nat]
(define number-equiv
  (new (class* object% (equiv-interface)
         (super-new)
         (define/public (equivalent? n1 n2)
           (= n1 n2)))))

(check-expect (send number-equiv equivalent? 10 10)
              true)
(check-expect (send number-equiv equivalent? 10 11)
              false)

;; [Equiv X] X ... -> [Set X]
;; Convenient constructor for sets.
(define (set = . xs)
  (cond [(empty? xs) (new mt-set% [= =])]
        [else (send (apply set = (rest xs)) add (first xs))]))
 

(check-expect (send (set number-equiv) empty?) true)
(check-expect (send (set number-equiv) contains? 5) false)
(check-expect (send (send (set number-equiv) remove 5) empty?) true)
(check-expect (send (send (set number-equiv) union (set number-equiv)) empty?) true)
(check-expect (send (send (set number-equiv) intersect (set number-equiv)) empty?) true)


(check-expect (send (set number-equiv 1) =? (set number-equiv 1)) true)
(check-expect (send (set number-equiv 1) empty?) false)
(check-expect (send (set number-equiv 1) contains? 5) false)
(check-expect (send (set number-equiv 1 5 2) contains? 5) true)
(check-expect (send (send (set number-equiv 1) remove 5) empty?) false)
(check-expect (send (send (set number-equiv 1) union (set number-equiv)) empty?) false)
(check-expect (send (send (set number-equiv 1) intersect (set number-equiv)) empty?) true)

(check-expect (send (set number-equiv 1 1 1) =? 
                    (set number-equiv 1)) 
              true)
(check-expect (send (set number-equiv 1) =? 
                    (set number-equiv 1 1 1)) 
              true)
(check-expect (send (set number-equiv 1 2 3) =? 
                    (set number-equiv 3 2 1)) 
              true)

(check-expect (send (set number-equiv 1 2 3 4) fold 
                    (new (class* object% (combine-interface)
                           (super-new)
                           (define/public (combine x s)
                             (+ x s))))
                    0)
              10)


;; [Equiv Nat]
(define =%5
  (new (class* object% (equiv-interface)
         (super-new)
         (define/public (equivalent? n1 n2)
           (= (modulo n1 5) (modulo n2 5))))))

(check-expect (send (set =%5 1 2 3) =? 
                    (set =%5 6 7 8)) true)

;; [Equiv [Set X]]
(define s= 
  (new (class* object% (equiv-interface)
         (super-new)
         (define/public (equivalent? s1 s2)
           (send s1 =? s2)))))

;; Comparing [Set [Set Nat]]
(check-expect (send (set s= (set number-equiv 1 2 3) (set number-equiv 3 2 1)) =?
                    (set s= (set number-equiv 1 2 3)))
              true)

;; [Map Nat Nat]
(define add1-map
  (new (class* object% (map-interface)
         (super-new)
         (define/public (apply n)
           (add1 n)))))

(check-expect (send (send (set number-equiv 1 2 3 6 7 8) map add1-map number-equiv) contains? 1) false)
(check-expect (send (send (set number-equiv 1 2 3 6 7 8) map add1-map number-equiv) contains? 2) true)
(check-expect (send (send (set number-equiv 1 2 3 6 7 8) map add1-map =%5) length) 3)

(test)