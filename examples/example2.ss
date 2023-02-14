(import
  (rnrs)
  (rough-draft unit-test)
  (rough-draft console-test-runner))

(define-test-suite foo
  (define-test test-car
    (assert-equal? 1 (car '(1 2 3)))))

(define-test-suite bar
  (define-test test-cdr
    (assert-equal? '(2 3) (cdr '(1 2 3 42)))))

(run-test-suite bar)

(run-test-suites foo bar)
