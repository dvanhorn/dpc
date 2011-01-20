#lang class0

;; Rubric:

;; Per method:
;; Contract  - 2pt
;; Purpose   - 1pt
;; Tests     - 2pt
;; Code      - 3pt
;; ===============
;; Subtotal  - 8pt x 8 = 64pt

;; Overall:
;; Style     - 20pt
;; Data Defn - 10pt
;; ================
;; Subtotal  - 30pt

;; Methods   - 64pt
;; Overall   - 30pt
;; ================
;; Total     - 94pt

;; Comments:

;; The `max' method is particularly tricky. Be accepting of various solutions.

;; General Guidelines:

;; If someone repeatedly makes the same mistake, eg:
;;   Self Complex -> Complex
;; instead of
;;   Complex -> Complex
;; treat it like they did this on only half of the problems.
;; So if Self Complex -> Complex is worth 1 out of 2 contract
;; points, you'd get 9+9/2 (= 13.5 ~ 14) points for contracts
;; on methods.

;; Style points are up to the grader's discretion, but should
;; follow the style guidelines on the web page.

;; (Let us know if you have further guidelines we should add.)

;; ==========================================================
;; Lists of Numbers

;; A LoN is one of:
;; - (new empty%)
;; - (new cons% Number LoN)
;;
;; length : -> Nat
;; Count the number of elements in this list.
;;
;; append LoN : -> LoN
;; Append the elements of this list with the given list.
;;
;; sum : -> Number
;; Add all the elements of this list.
;;
;; prod : -> Number
;; Multiply all the elements of this list.
;;
;; contains? : Number -> Boolean
;; Is the given number an element in this list?
;;
;; reverse : LoN
;; Reverse the elements of this list.
;;
;; map : (Number -> Number) -> Number
;; Apply given function to each element of this list and
;; construct a list of the results
;;
;; A NeLoN is one of:
;; - (new cons% Number (new empty%))
;; - (new cons% Number NeLoN)
;;
;; max : -> Number
;; Find the largest number in this non-empty list.

(define-class empty%
  (define/public (length) 0)
  (define/public (append ls) ls)
  (define/public (sum) 0)
  (define/public (prod) 1)
  (define/public (contains? n) false)
  (define/public (reverse) this)
  (define/public (reverse-acc ls) ls)
  (define/public (map f) this)
  (define/public (max) 'aint-defined))

(define-class cons%
  (fields first rest)
  (define/public (length) 
    (add1 (send (field rest) length)))
  (define/public (append ls)
    (new cons% 
         (field first)
         (send (field rest) append ls)))
  (define/public (sum)
    (+ (field first)
       (send (field rest) sum)))
  (define/public (prod)
    (* (field first)
       (send (field rest) prod)))
  (define/public (contains? n)
    (or (= n (field first))
        (send (field rest) contains? n)))        
  (define/public (reverse)
    (reverse-acc (new empty%)))
  (define/public (reverse-acc ls)
    (send (field rest) reverse-acc 
          (new cons% (field first) ls)))
  (define/public (map f)
    (new cons% (f (field first))
         (send (field rest) map f)))
  (define/public (max)
    (local [(define mr (send (field rest) max))]
      (cond [(or (symbol? mr) 
                 (> (field first) mr))
             (field first)]
            [else mr]))))

(define mt (new empty%))
(define ls (new cons% 4 (new cons% 3 (new cons% 2 mt))))
(check-expect (send mt length) 0)
(check-expect (send ls length) 3)
(check-expect (send mt append ls) ls)
(check-expect (send ls append mt) ls)
(check-expect (send mt sum) 0)
(check-expect (send ls sum) 9)
(check-expect (send mt prod) 1)
(check-expect (send ls prod) 24)
(check-expect (send ls contains? 5) false)
(check-expect (send ls contains? 2) true)
(check-expect (send mt reverse) mt)
(check-expect (send ls reverse)
              (new cons% 2 (new cons% 3 (new cons% 4 mt))))
(check-expect (send ls max) 4)
(check-expect (send mt map add1) mt)
(check-expect (send ls map add1)
              (new cons% 5 (new cons% 4 (new cons% 3 mt))))
