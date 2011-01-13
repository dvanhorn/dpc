#lang class0
(require 2htdp/universe)
(require 2htdp/image)

(define GRID-SIZE 10)

;; A Posn is a Complex.
(define posn-x real-part)
(define posn-y imag-part)


(define-struct cycle (dir trail))
;; A Cycle is a (make-cycle Dir (cons Posn [Listof Posn])).
;; A Dir is one of 1 -1 +1i -1i.


;; Cycle -> Cycle
(define (step c)
  (make-cycle (cycle-dir c)
              (cons (+ (cycle-dir c)
                       (first (cycle-trail c)))
                    (cycle-trail c))))

;; Cycle KeyEvent -> Cycle
(define (press c k)
  (make-cycle (cond [(key=? k "down") +1i]
                    [(key=? k "up")   -1i]
                    [(key=? k "left") -1]
                    [(key=? k "right") +1]
                    [else (cycle-dir c)])
              (cycle-trail c)))

(define (render c)
  (foldl (λ (t scn)
           (place-image (square GRID-SIZE "solid" "orange")
                        (* GRID-SIZE (posn-x t))
                        (* GRID-SIZE (posn-y t))
                        scn))
         (empty-scene 200 200)
         (cycle-trail c)))

(big-bang (make-cycle -1 (list 20+20i))
          (on-tick step 1/10)
          (to-draw render)
          (on-key press))

        
         


