#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct check-expect))
          (for-label (except-in class/1 define-struct check-expect)))

@title[#:tag "soln03"]{Solution: Shapes}

This is a solution for the @seclink["assign_shapes"]{Shapes} exercise.

@codeblock{
#lang class/1

;; Solution to part 1.
#;
(define-class rect% 
  (fields width height)                
  (define (bba)                 
    (* (this . width) (this . height))))

#;
(define-class circ% 
  (fields radius)                      
  (define (bba)                 
    (sqr (* 2 (this . radius)))))

;; A Shape implements:
;; width : -> Number                         
;; height : -> Number
;; Compute the {width,height} of this shape. 

;; Solution to part 2.
(define-class shape% 
  (define (bba)
    (* (this . width) (this . height))))     

(define-class rect% 
  (super shape%)                             
  (fields width height))                     
  
(define-class circ% 
  (super shape%)                             
  (fields radius)
  (define (width)                     
    (* 2 (this . radius)))
  (define (height)                    
    (this . width)))

;; Solution to part 3.   
(define-class square%
  (super shape%)                             
  (fields width)                             
  (define (height)                    
    (this . width)))

(check-expect ((new rect% 3 4) . bba) 12)
(check-expect ((new circ% 1.5) . bba)  9)
(check-expect ((new square% 5) . bba) 25)
}