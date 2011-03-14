#lang racket
(provide exercise-counter
         lab:section)
(require scribble/base)

(define (exercise-counter)
  (let [(i 0)]
    (λ xs
      (set! i (+ 1 i))
      (apply nested #:style "exercise"
                    (bold (format "Exercise ~s." i))
                    " "
                    xs))))

(define (lab:section . args)
  ; TODO Report bug: #:style has no effect with section
  ; OR   Did I stupidly confuse "lab-heading" with "labheading"...?
  ;(apply section #:style "lab-heading" args)
  (elem (first args) #:style "labheading"))
