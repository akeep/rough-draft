;;; Copyright (c) 2012 Andrew W. Keep
;;; See the accompanying file Copyright for detatils

(library (rough-draft helpers)
  (export construct-name)
  (import (rnrs))

  (define-syntax define-who
    (lambda (x)
      (syntax-case x ()
        [(_ name e)
         (with-syntax ([who (datum->syntax #'name 'who)])
           #'(define name
               (let ()
                 (define who 'name)
                 e)))]
        [(define-who (name . args) e0 e1 ...)
         #'(define-who name (lambda args e0 e1 ...))])))

  (define-who construct-name
    (lambda (tid . args)
      (define ->string
        (lambda (x)
          (let f ([x x] [converted? #f])
            (cond
              [(string? x) x]
              [(identifier? x) (symbol->string (syntax->datum x))]
              [(symbol? x) (symbol->string x)]
              [(char? x) (list->string (list x))]
              [(number? x) (number->string x)]
              [(not converted?) (f (syntax->datum x) #t)]
              [else (error who "unable to convert to string" x)]))))
      (datum->syntax tid
        (string->symbol
          (apply string-append
            (map ->string args)))))))
