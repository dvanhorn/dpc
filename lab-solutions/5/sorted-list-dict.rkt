#lang class2

(provide empty-sorted-list-dict)

(require "dict.rkt")

; A [SortedListDict V] is one of:
;  - (sld-empty%)
;  - (sld-cons Key V [SortedListDict V])
;
; Invariant: The keys are sorted (increasing).

(define-class sld-empty%
  (implements dict<%>)

  (check-expect ((new sld-empty%) . has-key? 1) false)

  (define/public (has-key? k)
    false)

  (define/public (lookup k)
    (error "Empty SortedListDict doesn't contain key" k))

  (check-expect ((new sld-empty%) . set 1 "one" . lookup 1) "one")

  (define/public (set k v)
    (new sld-cons% k v this))

  (define/public (size)
    0)

  )

(define-class sld-cons%
  (implements dict<%>)
  (fields key value rest)

  (check-expect ((new sld-cons% 1 "one" (new sld-empty%)) . has-key? 1) true)
  (check-expect ((new sld-cons% 1 "one" (new sld-empty%)) . has-key? 2) false)

  (define/public (has-key? k)
    (cond
      [(< k (field key)) false]
      [(= k (field key)) true]
      [else              ((field rest) . has-key? k)]))

  (check-expect ((new sld-cons% 1 "one" (new sld-empty%)) . lookup 1) "one")
  (check-error  ((new sld-cons% 1 "one" (new sld-empty%)) . lookup 2))

  (define/public (lookup k)
    (cond
      [(< k (field key)) (error "Key not found:" k)]
      [(= k (field key)) (field value)]
      [else              ((field rest) . lookup k)]))

  (check-expect ((new sld-empty%) . set 1 "one" . set 2 "two" . lookup 1) "one")
  (check-expect ((new sld-empty%) . set 1 "one" . set 2 "two" . lookup 2) "two")
  (check-expect ((new sld-empty%) . set 1 "one" . set 1 "uno" . lookup 1) "uno")
  (check-error  ((new sld-empty%) . set 1 "one" . set 1 "uno" . lookup 3))
  (check-expect ((new sld-empty%) . set 1 "one" . set 2 "two")
                (new sld-cons% 1 "one" (new sld-cons% 2 "two"(new sld-empty%))))
  (check-expect ((new sld-empty%) . set 2 "two" . set 1 "one")
                (new sld-cons% 1 "one" (new sld-cons% 2 "two"(new sld-empty%))))

  (define/public (set k v)
    (cond
      [(< k (field key)) (new sld-cons% k v this)]
      [(= k (field key)) (new sld-cons% k v (field rest))]
      [else              (new sld-cons%
                              (field key)
                              (field value)
                              ((field rest) . set k v))]))

  (define/public (size)
    (+ 1 ((field rest) . size)))

  )

; empty-sorted-list-dict : [SortedListDict V]
(define empty-sorted-list-dict
  (new sld-empty%))
