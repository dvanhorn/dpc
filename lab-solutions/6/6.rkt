#lang class3
(require (only-in racket begin))

;
; List
;

; A [List X] is either:
;  - (empty%)
;  - [NonEmptyList X]

; A [NonEmptyList X] is a (cons% X [List X])

(define-class empty%

  ; -> Boolean
  (define/public (empty?)
    true)

  ; -> Nat
  (define/public (length)
    0)

  )

(define-class cons%
  (fields first rest)

  ; -> Boolean
  (define/public (empty?)
    false)

  ; -> Nat
  (define/public (length)
    (+ 1 ((field rest) . length)))

  ; X ->
  ; Effect: Add x to the front
  (define/public (insert-front! x)
    (local [(define old (field first))]
      (begin
        (set-field! first x)
        (set-field! rest (cons% old (field rest))))))

  ; X ->
  ; Effect: Add x to the end
  (define/public (insert-end! x)
    (if ((field rest) . empty?)
      (set-field! rest (cons% x (empty%)))
      ((field rest) . insert-end! x)))

  ; [List X] ->
  ; Effect: Append xs to the end
  (define/public (append! xs)
    (if ((field rest) . empty?)
      (set-field! rest xs)
      ((field rest) . append! xs)))

  ; X ->
  ; Effect: Remove the ith element
  ; Assume: length ≥ i+1
  (define/public (remove! i)
    (if (zero? i)
      (begin
        (set-field! first ((field rest) . first))
        (set-field! rest  ((field rest) . rest)))
      ((field rest) . remove (- i 1))))

  )

(define xs (cons% "b" (cons% "c" (empty%))))
(xs . insert-front! "a")
;(check-expect xs (cons% "a" (cons% "b" (cons% "c" (empty%)))))
(xs . insert-end! "d")
;(check-expect xs (cons% "a" (cons% "b" (cons% "c" (cons% "d" (empty%))))))

;
; CList
;

; A [CList X] is a (clist% X [CList X] [CList X])
(define-class clist%
  (fields data left right)

  (constructor (x)
    (fields x 'invalid 'invalid)
    (set-field! left  this)
    (set-field! right this))

  ; X ->
  ; Effect: Insert x as a new node in the list
  (define/public (insert! x)
    (local [(define c (clist% x this (field right)))]
      (set-field! right c)))

  ; -> [CList X]
  ; Effect: Extract this node out as its own list
  (define/public (extract!)
    (begin
      ((field left)  . set-right! (field right))
      ((field right) . set-left!  (field left))
      (set-field! left  this)
      (set-field! right this)
      (clist% (field data))))

  ; [CList X] ->
  ; Effect: Update left
  (define/public (set-left! c)
    (set-field! left c))

  ; [CList X] ->
  ; Effect: Update right
  (define/public (set-right! c)
    (set-field! right c))

  ; [CList X] ->
  ; Effect: Join the two lists
  (define/public (join! that)
    (local [(define old-this-left  (this . left))
            (define old-that-right (that . right))]
      (begin
        (this . set-left!  that)
        (that . set-right! this)
        (send old-this-left  set-right! old-that-right)
        (send old-that-right set-left!  old-this-left))))

  )

;(define a (clist% 0))
;(define b (clist% 1))
;(define c (clist% 2))
;
;a b c
;
;(a . join! b)
;
;a b c
;
;(b . join! c)
;
;a b c
