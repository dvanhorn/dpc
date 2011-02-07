#lang class1

; An [IDict K V] implements:
(define-interface dict<%>
  [
   ; K -> V
   ; The value for the given key
   ; Assume the key exists
   lookup

   ; K V -> [IDict K V]
   ; Set the given key-value mapping
   set

   ; K -> [IDict K V]
   ; Remove the key and its value from the dictionary
   ; Assume the key exists
   remove

   ; -> Number
   ; The number of mappings in the dictionary
   size

   ; K (V -> V) -> [IDict K V]
   ; Update the value for the given key by the given function
   ; Assume the key exists
   update

   ; [IDict K V] -> [IDict K V]
   ; Extend this dictionary with all the mappings from the given dictionary
   extend

   ; (V -> V) -> [IDict K V]
   ; Map the given function over the values
   map

   ; K -> Boolean
   ; Does the given key exist?
   has-key?

   ; V -> Boolean
   ; Does the given value exist?
   has-value?

   ; -> [Listof K]
   ; The keys in the dictionary
   keys

   ; -> [Listof V]
   ; The values in the dictionary
   values
   ])

; A [ListDict K V] is one of:
; - (dempty%)
; - (dcons% K V [ListDict K V])

(define empty-dict (dempty%))

(define-class dempty%

  (define/public (set k v)
    (dcons% k v this))

  (define/public (has-key? k)
    false)

  )

(define-class dcons%
  (fields key value rest)

  (define/public (lookup k)
    (if (equal? k (field key))
      (field value)
      ((field rest) . lookup k)))

  (define/public (set k v)
    (if (equal? k (field key))
      (dcons% (field key) v (field rest))
      (dcons% (field key) (field value) ((field rest) . set k v))))

  (define/public (has-key? k)
    (if (equal? k (field key))
      true
      ((field rest) . has-key? k)))

  (define/public (remove k)
    (if (equal? k (field key))
      (field rest)
      (dcons% (field key) (field value) ((field rest) . remove k))))

  )
