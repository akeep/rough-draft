;;; Copyright (c) 2012 Andrew W. Keep
;;; See the accompanying file Copyright for detatils

(library (rough-draft unit-test)
  (export define-test-suite)
  (import
    (rnrs)
    (rough-draft assertions)
    (only (chezscheme) warningf import trace-lambda trace-define with-implicit))

  (define-syntax define-test-suite 
    (lambda (x)
      (define who 'define-test-suite)
      (define define-test
        (lambda (test)
          (syntax-case test ()
            [(_ name e0 e1 ...)
             (with-implicit (name record-assertion record-error record-exception)
               #'(name (lambda (record-assertion record-error record-exception)
                         (assertion-module name
                           record-assertion record-error record-exception)
                         (import name)
                         e0 e1 ...)))]
            [test (syntax-violation who "invalid test syntax" x #'test)])))
      (syntax-case x ()
        [(_ name test tests ...)
         (with-syntax ([((test-name test-proc) ...)
                        (map define-test #'(test tests ...))])
           #'(define name
               (let ([als (list (cons 'test-name test-proc) ...)])
                 (lambda (test-suite-message run-test tests-to-run)
                   (test-suite-message 'name
                     (lambda ()
                       (for-each (lambda (as) (run-test (car as) (cdr as)))
                         (if (eq? tests-to-run #t)
                             als
                             (fold-right
                               (lambda (test-to-run ls)
                                 (cond
                                   [(assq test-to-run als) =>
                                    (lambda (as) (cons as ls))]
                                   [else
                                    (warningf 'name
                                      "no test ~s named in test suite"
                                      test-to-run)
                                    ls]))
                               '() tests-to-run)))))))))]))))

