#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct check-expect))
          (for-label (except-in class/1 define-struct check-expect)))

@title[#:tag "soln03"]{Solution: Shapes}

This is a solution for the @seclink["assign_shapes"]{Shapes} exercise.

@#reader scribble/comment-reader
(racketmod
class/1
(define-class rect% 
  (fields width height)                
  (define/public (bba)                 
    (* (field width) (field height))))
  
(define-class circ% 
  (fields radius)                      
  (define/public (bba)                 
    (sqr (* 2 (field radius)))))

;; Part 2 [10pt]

;; implements:
;; width : -> Number                         
;; height : -> Number
;; Compute the {width,height} of this shape. 

(define-class shape% 
  (define/public (bounding-box-area)
    (* (this . width) (this . height))))     

(define-class rect% 
  (super shape%)                             
  (fields width height))                     
  
(define-class circ% 
  (super shape%)                             
  (fields radius)
  (define/public (width)                     
    (* 2 (field radius)))
  (define/public (height)                    
    (width)))

;; Part 3 [4pt]
    
(define-class square%
  (super shape%)                             
  (fields width)                             
  (define/public (height)                    
    (width)))

(check-expect ((square% 5) . bba) 25)        
)