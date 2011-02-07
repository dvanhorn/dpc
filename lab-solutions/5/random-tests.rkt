#lang class2

(require "dict.rkt"
         "list-dict.rkt"
         "sorted-list-dict.rkt"
         "tree-dict.rkt")

; some-nat : -> Nat
(define (some-nat)
  (random 100))

; do : Nat (Any -> Any) -> Any
; Run f n times
(define (do n f)
  (if (zero? n)
    'done
    (f (do (- n 1) f))))

;
; IList
;

; set-randomly : Nat [IList Nat] -> [IList Nat]
(define (set-randomly n d)
  (if (zero? n)
    d
    ((set-randomly (- n 1) d) . set (some-nat) (some-nat))))

; has-key?-set : [IDict V] Key V -> Boolean
(define (has-key?-set d k v)
  (d . set k v . has-key? k))

; has-key?-lookup : [IDict V] Key V -> Boolean
(define (has-key?-lookup d k v)
  (equal? v (d . set k v . lookup k)))

;
; ListDict
;

; some-ld : -> [ListDict Nat]
(define (some-ld)
  (set-randomly 20 empty-list-dict))

(do 1000
  (λ (_)
    (check-expect (has-key?-set (some-ld) (some-nat) (some-nat))
                  true)))
(do 1000
  (λ (_)
    (check-expect (has-key?-lookup (some-ld) (some-nat) (some-nat))
                  true)))

;
; SortedListDict
;

; some-sld : -> [SortedListDict Nat]
(define (some-sld)
  (set-randomly 20 empty-sorted-list-dict))

(do 1000
  (λ (_)
    (check-expect (has-key?-set (some-sld) (some-nat) (some-nat))
                  true)))
(do 1000
  (λ (_)
    (check-expect (has-key?-lookup (some-sld) (some-nat) (some-nat))
                  true)))

;
; TreeDict
;

; some-td : -> [TreeDict Nat]
(define (some-td)
  (set-randomly 20 empty-tree-dict))

(do 1000
  (λ (_)
    (check-expect (has-key?-set (some-td) (some-nat) (some-nat))
                  true)))
(do 1000
  (λ (_)
    (check-expect (has-key?-lookup (some-td) (some-nat) (some-nat))
                  true)))
