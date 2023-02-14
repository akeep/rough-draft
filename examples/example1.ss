;;; Copyright (c) 2012 Andrew W. Keep
;;; See the accompanying file Copyright for detatils

(import
  (rnrs)
  (rough-draft unit-test)
  (rough-draft console-test-runner))

(define-test-suite foo
  (define-test first-test
    (define x 10)
    (assert-eqv? x 10)
    (assert-eq? (string->symbol "foo") 'foo))
  (define-test second-test
    (let ([y (list #\f #\o #\o)])
      (assert-string=? (list->string y) "foo")
      (assert-char=? (car y) #\f))))

(define-test-suite bar
  (define-test third-test
    (assert-equal? '(2 3) (cdr '(1 2 3)))))

(run-test-suite foo)
(run-test-suite bar)

(run-test foo first-test)

(run-tests foo second-test first-test)

(run-test-suites foo bar)
