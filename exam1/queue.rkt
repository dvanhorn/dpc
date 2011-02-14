#lang class2

;; invariant: front is empty IFF queue is empty
;; A [Queue X] is (queue% [Listof X] [Listof X])

(define-class queue%
  (fields front back)
  ;; Add element to the back
  ;; X -> [Queue X]
  (define/public (enq i)
    (cond [(empty? (field front))
           (queue% (list i) empty)]
          [else
           (queue% (field front) (cons i (field back)))]))
  ;; get the first element
  ;;  -> X
  ;; assumes this queue is non-empty
  (define/public (head)
    (first (field front)))
  ;; drop the first element
  ;;  -> [Queue X]
  ;; assumes this queue is non-empty
  (define/public (deq)
    (cond 
      [(empty? (rest (field front)))
       (queue% (reverse (field back)) empty)]
      [else (queue% (rest (field front)) (field back))]))
  ;; is this queue empty?
  ;; emp? : -> Boolean
  (define/public (emp?)
    (empty? (field front)))
  ;; convert the queue to a list
  ;; -> [Listof X] 
  (define/public (to-list)
    (append (field front)
            (reverse (field back)))))

(define q (queue% '(1 2 3) empty))
(define eq (queue% empty empty))

(check-expect (q . emp?) false)
(check-expect (q . head) 1)
(check-expect (eq . emp?) true)
(check-expect (eq . enq 1) (queue% '(1) '()))
(check-expect (eq . enq 1 . deq) eq)
(check-expect (q . enq 5) (queue% '(1 2 3) '(5)))
(check-expect (q . enq 5 . deq) (queue% '(2 3) '(5)))

(check-expect (q . to-list) '(1 2 3))
(check-expect (q . enq 5 . to-list) '(1 2 3 5))
(check-expect (q . enq 5 . deq . to-list) '(2 3 5))