#lang racket

;; expect lst look like:
;; '(module racketjs-lang-test "racketjs-lang.rkt" body ...)
(define (transform lst)
  (list
   'module
   (second lst)
   (third lst)
   (cons
    'enable-js
    (rest
     (rest
      (rest lst))))))

(define (exp) 
  (define r (read (open-input-file "racketjs-lang-test.rkt")))
  
  (parameterize ([current-namespace (make-base-namespace)]
  		 [read-accept-reader #t])
    (let ([t (transform r)])
      (expand t))))
