#lang scribble/manual

@title[#:tag "lec24"]{4/4: Assorted Topics}

@verbatim|{

Announcements:

 - Tournament in Final Exam Time
 - We don't know when the exam will be.

Derivative in Multiple Languages

 - In ISL:

 (define ep 0.000001)
 ;; [Number -> Number] -> [Number -> Number]
 ;; Compute the derivative of f
 (define (deriv f)
   (lambda (x)
     (/ (- (f (+ x ep))
           (f (- x ep)))
        (* 2 ep))))

 - In class5:

   
   





}|