(library (rough-draft assertions)
  (export assertion-module)
  (import
    (rnrs)
    (rough-draft helpers)
    (only (chezscheme) module export with-implicit format format-condition?))
  
  (define-syntax define-equivalence-assertions
    (lambda (x)
      (syntax-case x ()
        [(_ tid record-assertion record-error record-exception (name names ...))
         (with-syntax ([(assert-name ...)
                        (map (lambda (x) (construct-name #'tid "assert-" x))
                          #'(name names ...))]
                       [(friendly-name ...)
                        (map (lambda (x)
                               (let ([str (symbol->string (syntax->datum x))])
                                 (let ([last-char (fx- (string-length str) 1)])
                                   (if (char=? (string-ref str last-char) #\?)
                                       (datum->syntax x
                                         (string->symbol
                                           (substring str 0 last-char)))
                                       x))))
                          #'(name names ...))]
                       [(name ...) #'(name names ...)])
           #'(begin
               (define-syntax assert-name
                 (syntax-rules ()
                   [(_ ?actual ?expected)
                    (begin
                      (record-assertion)
                      (let ([expected ?expected] [actual ?actual])
                        (unless (name actual expected)
                          (record-error
                            (format "~s is not ~s to ~s"
                              actual 'friendly-name expected)
                            #'?actual))))]))
               ...
               (export assert-name ...)))])))

  (define-syntax define-predicate-assertions
    (lambda (x)
      (syntax-case x ()
        [(_ tid record-assertion record-error record-exception (name names ...))
         (with-syntax ([(assert-name ...)
                        (map (lambda (x) (construct-name #'tid "assert-" x))
                          #'(name names ...))]
                       [(friendly-name ...)
                        (map (lambda (x)
                               (let ([str (symbol->string (syntax->datum x))])
                                 (let ([last-char (fx- (string-length str) 1)])
                                   (if (char=? (string-ref str last-char) #\?)
                                       (datum->syntax x
                                         (string->symbol
                                           (substring str 0 last-char)))
                                       x))))
                          #'(name names ...))])
           #'(begin
               (define-syntax assert-name
                 (syntax-rules ()
                   [(_ ?actual)
                    (begin
                      (record-assertion)
                      (let ([actual ?actual])
                        (unless (name actual)
                          (record-error
                            (format "~s is not a ~s" actual 'friendly-name)
                            #'?actual))))]))
               ...
               (export assert-name ...)))])))

  (define-syntax assertion-module
    (lambda (x)
      (syntax-case x ()
        [(_ name record-assertion record-error record-exception)
         (with-implicit (name assert-true assert-error)
           #'(module name (assert-true assert-error)
               (define-equivalence-assertions
                 name record-assertion record-error record-exception
                 (eq? eqv? equal? = < > <= >= fx=? fx<? fx>? fx<=? fx>=?
                  fl=? fl<? fl>? fl<=? fl>=? string=? char=?))
               (define-predicate-assertions
                 name record-assertion record-error record-exception
                 (boolean? sybmol? string?  identifier? char?))
               (define-syntax assert-true
                 (syntax-rules ()
                   [(_ ?actual)
                    (let ([actual ?actual])
                      (record-assertion)
                      (unless actual
                        (record-error
                          (format "~s is not a true value" actual)
                          #'?actual)))]))
               (define-syntax assert-error
                 (syntax-rules ()
                   [(_ ?actual ?msg)
                    (let ([msg ?msg])
                      (record-assertion)
                      (guard (c [else
                                  (let ([actual-msg
                                          (or (and (format-condition? c)
                                                   (apply format (condition-message c)
                                                     (condition-irritants c)))
                                              (and (message-condition? c)
                                                   (condition-message c)))])
                                    (unless (string=? actual-msg msg)
                                      (record-exception "~s is not the expected execption message ~s"
                                        #'?actual)))])
                        (let ([actual ?actual])
                          (record-error (format "~s instead of exception" actual) #'?actual))))]))))]))))