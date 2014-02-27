#lang racket

(require racket/stxparam racket/splicing)

 (define-syntax-parameter rendering-js #f)

(define-syntax (if-js stx)
  (syntax-case stx ()
    [(_ js no-js)
         (if (syntax-parameter-value #'rendering-js)
             #'js
             #'no-js)]))

(define-syntax-rule (enable-js body ...)
  (splicing-syntax-parameterize ([rendering-js #t])
				body ...))

(provide
 #%module-begin
 #%datum
 #%app
 #%top

 begin
 lambda
 
 let
 let*
 if
 and
 or
 define

 values
 list
 car
 cdr
 rest
 empty?
 empty
 first
 symbol?
 +
 cons
 number?
 string?
 boolean?
 pair?

 enable-js
 if-js
 rendering-js)
