#lang class2

(provide empty-tree-dict)

(require "dict.rkt")

; A [TreeDict V] is one of:
;  - (td-leaf%)
;  - (td-node% Key V [TreeDict V] [TreeDict V])
; 
; Invariant: The keys form a BST.

(define-class td-leaf%

  (define/public (has-key? k)
    false)

  (define/public (lookup k)
    (error "Empty TreeDict doesn't contain key" k))

  (define/public (set k v)
    (new td-node% k v this this))

  (define/public (size)
    0)

  (define/public (extend that)
    that)

  )

(define-class td-node%
  (fields key value left right)

  (define/public (has-key? k)
    (cond
      [(< k (field key)) ((field left) . has-key? k)]
      [(= k (field key)) true]
      [(> k (field key)) ((field right) . has-key? k)]))

  (define/public (lookup k)
    (cond
      [(< k (field key)) ((field left) . lookup k)]
      [(= k (field key)) (field value)]
      [(> k (field key)) ((field right) . lookup k)]))

  (define/public (set k v)
    (cond
      [(< k (field key)) (new td-node%
                              (field key)
                              (field value)
                              ((field left) . set k v)
                              (field right))]
      [(= k (field key)) (new td-node%
                              (field key)
                              v
                              (field left)
                              (field right))]
      [(> k (field key)) (new td-node%
                              (field key)
                              (field value)
                              (field left)
                              ((field right) . set k v))]))

  (define/public (size)
    (+ 1 ((field left) . size) ((field right) . size)))

  (define/public (extend that)
    ((field left) . extend (field right)
                  . extend that
                  . set (field key) (field value)))

  )

; empty-tree-dict : [TreeDict V]
(define empty-tree-dict
  (new td-leaf%))
