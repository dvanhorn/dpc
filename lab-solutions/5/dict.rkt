#lang class2

(provide dict<%>)

; A Key is a Nat

; An [IDict V] implements:
(define-interface dict<%>
  [; Key -> Boolean
   ; Does the given key exist?
   has-key?

   ; Key -> V
   ; The value mapped to by the given key
   ; Assumption: the key exists
   lookup

   ; Key V -> [IDict V]
   ; Set the given key-value mapping
   set

   ; -> Nat
   ; The number of mappings
   size

   ; [IDict V] -> [IDict V]
   ; Extend this with all the mappings in that
   extend
   ])
