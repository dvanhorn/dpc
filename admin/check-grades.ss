#!/bin/bash
#| -*- scheme -*-
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module check-grades mzscheme
  (require (lib "string.ss")
           (lib "file.ss")
           (lib "list.ss")
           (lib "cmdline.ss")
           (lib "xml.ss" "xml")
           (only (lib "misc.ss" "swindle")
                 regexp-case))

  (define STOP-ON-FAIL? #f)
  (define ORIG-DIR #f)
  (define HTML-DIR #f)
  (define GRADED-FILES null)
  
  ;; check+grade+htmlize : path path path -> number
  ;; This thing computes a file's grade.  This is done using special markup --
  ;; this markup is anything that follows ";>" (or "//>" for java etc).  The
  ;; markup itself can contain <+N> or <-N> to add to the grade (which begins at
  ;; 0), or <*N%> to factor the final grade by some percentage.  It returns a
  ;; second value that is a list of xexpr things that make the marked html
  ;; result.
  (define (check+grade+htmlize graded orig html-file)
    (define grade 0)
    (define factor 1)
    (define markup-re ";+>")
    (define markup-only-re (regexp (string-append "^[ \t]*" markup-re)))
    (define line-markup-re
      (regexp (string-append "^(.*?)([ \t]*?)(" markup-re ".*?)? *$")))
    (define in  (make-reader graded))
    (define oin (make-reader orig))
    (define html '())
    (define (html! x . style)
      (unless (equal? x "")
        (set! html (cons (if (null? style)
                             x `(span ((class ,(format "~a" (car style)))) ,x))
                         html))))
    ;; Main Loop
    (let loop ([line (in)] [oline (and oin (oin))])
      (cond
       [(and (eof-object? line) (or (not oin) (eof-object? oline))) 'done]
       [(eof-object? line) (error 'check+grade+htmlize "premature eof in ~a" graded)]
       ;; skip ;>-lines (or //>) in original file (student names etc)
       [(and (string? oline) (regexp-match markup-only-re oline))
        (loop line (oin))]
       [else
        (regexp-case line
          [(line-markup-re prefix space markup)
           (html! (string-append prefix space))
           (let loop ([str (or markup "")])
             (regexp-case str
               [(#rx"^(.*?)(<[^@<> \t]*[0-9][^@<> ]*>)(.*)$" prefix markup rest)
                (html! prefix 'comment)
                (html! markup 'grade)
                (regexp-case markup
                  [(#rx"^<([+-][0-9]+)>$" g)
                   (set! grade (+ grade (string->number g)))]
                  [(#rx"^<[*]([0-9]+)%>$" f)
                   (set! factor (* factor (/ (string->number f) 100)))]
                  [else
                   (error 'check+grade+htmlize "bad grade markup in ~s: ~e" graded str)])
                (loop rest)]
               [else (html! str 'comment)]))
           (html! "\n")
           ;; (printf ">>> ~s ~s\n    ~s\n" prefix markup oline)
           (cond
            [(not oin) (loop (in) oin)] ; no original to compare against
            ;; continue if lines are equal ignoring meta markup
            [(equal? prefix oline) (loop (in) (oin))]
            ;; skip markup-only lines and empty lines in graded file
            [(regexp-match #rx"^[ \t]*$" prefix) (loop (in) oline)]
            [else (error 'check+grade+htmlize "content mismatch: ~a:~a, ~a:~a"
                         graded (sub1 (in 'get-line-num))
                         orig (sub1 (oin 'get-line-num)))])])]))
    (in 'close) (when oin (oin 'close))
    (let ([grade (inexact->exact (round-up (* grade factor)))])
      (when html-file
        (with-output-to-file html-file
          (lambda () (write-html (path->string (file-name-from-path graded))
                                 grade
                                 (reverse html)))
          'truncate))
      grade))
  
  ;; Utility for get-grade
  (define (make-reader f)
    (let ([p (open-input-file f)])
      (port-count-lines! p)
      (lambda args
        (if (null? args)
            (let ([line (read-line p 'any)])
              (if (string? line) (regexp-replace #rx" +$" line "") line))
            (case (car args)
              [(close) (close-input-port p)]
              [(get-line-num) (let-values ([(line col pos) (port-next-location p)])
                                line)])))))

  (define (round-up x)
    (floor (+ 0.5 x)))
  
  (define (write-html team grade marked)
    (define title `("Graded file for " ,team))
    (define style
      (string-append
       "\n.comment { font-weight: bold; background-color: #FFC0C0; }\n"
       ".grade { font-weight: bold; background-color: #FFFF60;"
       " border: solid thin red; }\n"))
    (write-xml/content
     (xexpr->xml
      `(html
        (head (title ,@title) (style ((type "text/css")) ,(make-comment style)))
        (body ((bgcolor "white"))
              (h1 ,@title)
              "Computed grade for this submission: "
              (b ,(format "~a" grade))
              (pre ,@marked))))))
  
  ;; process-all-graded-files : (list-of path) -> void
  (define (process-all-graded-files graded-files)
    (let loop ([files graded-files]
               [grades null])
      (cond [(pair? files)
             (let ([grade (process-one-graded-file (car files))])
               (if (or grade (not STOP-ON-FAIL?))
                   (loop (cdr files)
                         (cons (cons (car files) grade) grades))
                   (printf "** Stopping after first failure~n")))]
            [(null? files)
             (record-grades (reverse grades))])))
  
  ;; record-grades : (list-of (cons path number)) -> void
  (define (record-grades grades)
    (define body
      (map (lambda (gp)
             (define team
               (path->string
                (path-replace-suffix
                 (file-name-from-path (car gp)) "")))
             (define team-html (string-append team ".html"))
             (define grade (cdr gp))
             (if grade
                 `(p (a ((href ,team-html))
                        ,team)
                     " "
                     (b ,(format "~a" grade)))
                 `(p ,team
                     " "
                     (b ((style "color: red")) "error"))))
           (sort grades
                 (lambda (a b) (string<? (car a) (car b))))))
    (when HTML-DIR
      (with-output-to-file (build-path HTML-DIR "index.html")
        (lambda ()
          (write-xml/content
           (xexpr->xml
            `(html
              (head (title "Grade summary"))
              (body ((bgcolor "white"))
                    (h1 "Grade summary")
                    ,@body)))))
        'truncate)))
  
  ;; process-one-graded-file : path -> (maybe number)
  ;; Returns grade on success
  (define (process-one-graded-file graded-file)
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (printf "ERROR: ~a~n" (exn-message e))
                       #f)])
      (process graded-file)))
  
  ;; process : path -> (cons string number)
  (define (process graded-file)
    (define filename (path->string (file-name-from-path graded-file)))
    (define orig-file (build-path ORIG-DIR filename))
    (define html-file
      (and HTML-DIR
           (build-path HTML-DIR (path-replace-suffix filename ".html"))))
    (unless (file-exists? orig-file)
      (error 'process "No original version for ~a" filename))
    (when (equal? (normalize-path graded-file) (normalize-path orig-file))
      (error 'process "Original and graded files are the same!"))
    (let* ([grade (check+grade+htmlize graded-file orig-file html-file)]
           [users (regexp-split #rx"[+]" (second (regexp-match #rx"(.*)[.]scm" filename)))])
      (for-each (lambda (u) (printf " (~a ~s)~n" u grade)) users)
      grade))

  (define (go/k)
    (unless (and ORIG-DIR (directory-exists? ORIG-DIR))
      (printf "You must specify a directory of original files.~n")
      (printf "Type 'check-grades.ss -h' for command syntax.~n")
      (exit 1))
    (printf "(\n")
    (process-all-graded-files GRADED-FILES)
    (printf ")\n"))

  (define (go/command-line)
    (command-line "check-grades.ss" (current-command-line-arguments)
      (once-each
       ["-s"
        "Stop on first failure"
        (set! STOP-ON-FAIL? #t)]
       ["-o" original-dir
        "Directory containing original (ungraded) submissions"
        (set! ORIG-DIR original-dir)]
       ["-x" html-dir
        "Directory for optional html output. Clobbers files w/o warning."
        (set! HTML-DIR html-dir)])
      (args graded-files
            (set! GRADED-FILES graded-files)))
    (go/k))

  (define (go/mred get-file-list get-directory)
    (set! GRADED-FILES
          (map path->string
               (get-file-list "Files to check, grade, and convert to html")))
    (set! ORIG-DIR
          (path->string
           (get-directory "Directory of ORIGINAL (ungraded) submissions")))
    (let ([path (get-directory "Directory for HTML output")])
      (when path
        (set! HTML-DIR (path->string path))))
    (go/k))


  (let-values ([(get-file-list get-directory)
                (with-handlers ([exn:fail? (lambda (e) (values #f #f))])
                  (values
                   (dynamic-require '(lib "mred.ss" "mred")
                                    'get-file-list)
                   (dynamic-require '(lib "mred.ss" "mred")
                                    'get-directory)))])
    (if (and get-file-list get-directory)
        (go/mred get-file-list get-directory)
        (go/command-line)))
  
  )
