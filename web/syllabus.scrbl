#lang scribble/manual

@title{Syllabus}

@(define (number->string2 n)
   (let ((r (number->string n)))
     (cond [(= (string-length r) 1)
            (string-append "0" r)]
           [else r])))

@(define (syllabus-row i reading)
   (list (number->string i)
         (itemlist (item (secref (string-append "lec" (number->string2 (- (* 2 i) 2)))))
                   (item (secref (string-append "lec" (number->string2 (- (* 2 i) 1))))))
         reading
         (secref (string-append "lab" (number->string2 i)))
         (secref (string-append "assign" (number->string2 i)))))

@(tabular (apply list 
                 (list "" "Lectures " "Readings " "Lab " "Assignment")
                 (list "1"    
                       @itemlist{@item{@secref{lec01}}}
                       "Ch. 1"
                       @secref{lab01}
                       @secref{assign01})
                 (for/list ([i (in-range 2 16)])
                   (syllabus-row i "??"))))
