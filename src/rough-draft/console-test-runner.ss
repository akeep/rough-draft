;;; Copyright (c) 2012 Andrew W. Keep
;;; See the accompanying file Copyright for detatils

(library (rough-draft console-test-runner)
  (export run-test run-tests run-test-suite run-test-suites)
  (import (rnrs)
    (only (chezscheme) syntax->annotation annotation-stripped annotation-source
      source-object-sfd open-source-file source-object-bfp with-output-to-string
      format printf display-condition void))

  (define-record-type suite-info
    (nongenerative)
    (fields
      (mutable test-count)
      (mutable failure-count)
      (mutable assertion-count)
      (mutable error-count)
      (mutable exception-count))
    (protocol (lambda (new) (lambda () (new 0 0 0 0 0)))))

  (define syntax->expression-information
    (lambda (stx)
      (let ([a (syntax->annotation stx)])
        (if a
            (let ([expr (annotation-stripped a)] [s (annotation-source a)])
              (let ([p (open-source-file (source-object-sfd s))])
                (if p
                    (let f ([fp (source-object-bfp s)] [line 1] [col 1])
                      (if (zero? fp)
                          (format "~s at line ~d, char ~d" expr line col)
                          (let ([c (read-char p)] [fp (- fp 1)])
                            (cond
                              [(char=? c #\newline) (f fp (+ line 1) 1)]
                              [else (f fp line (+ col 1))]))))
                    (format "~s at char ~d" expr (source-object-bfp s)))))
            (format "~s" (syntax->datum stx))))))

  (define test-runner
    (lambda (suite-info)
      (lambda (name p)
        (suite-info-test-count-set! suite-info
          (fx+ (suite-info-test-count suite-info) 1))
        (printf "  Testing ~s ..." name)
        (let ([asserts 0] [errors 0] [exceptions 0])
          (guard (c [else
                      (set! exceptions (fx+ exceptions 1))
                      (printf "~&    ~a~%"
                        (with-output-to-string
                          (lambda () (display-condition c))))])
            (p (lambda () (set! asserts (fx+ asserts 1)))
               (lambda (msg stx)
                 (set! errors (fx+ errors 1))
                 (printf "~&    Error: ~a in ~a~%" msg
                   (syntax->expression-information stx)))
               (lambda (msg stx)
                 (set! exceptions (fx+ exceptions 1))
                 (printf "~&    Exception: ~a in ~a~%" msg
                   (syntax->expression-information stx)))))
          (when (or (> errors 0) (> exceptions 0))
            (suite-info-failure-count-set! suite-info 
              (fx+ (suite-info-failure-count suite-info) 1)))
          (suite-info-assertion-count-set! suite-info
            (fx+ (suite-info-assertion-count suite-info) asserts))
          (suite-info-error-count-set! suite-info
            (fx+ (suite-info-error-count suite-info) errors))    
          (suite-info-exception-count-set! suite-info
            (fx+ (suite-info-exception-count suite-info) exceptions))
          (if (or (> errors 0) (> exceptions 0))
              (printf "~&    ~d assertion~:p, ~d error~:p, ~d exception~:p~%"
                asserts errors exceptions)
              (printf "~s~%" #t))))))

  (define-syntax run-test
    (syntax-rules ()
      [(_ suite test) ($run-tests suite (list 'test))]))

  (define-syntax run-tests
    (syntax-rules ()
      [(_ suite test tests ...) ($run-tests suite (list 'test 'tests ...))]))

  (define $run-tests
    (lambda (test-suite tests)
      (let ([suite-info (make-suite-info)])
        (test-suite
          (lambda (name th)
            (printf "Running ~{~s~^, ~} test~p from ~s test suite ...~%"
              tests (length tests) name)
            (th)
            (printf "Ran ~d test~:p with ~d assertion~:p, "
              (suite-info-test-count suite-info)
              (suite-info-assertion-count suite-info))
            (printf "~d test~:p failed (~d error~:p, ~d exception~:p)~%"
              (suite-info-failure-count suite-info)
              (suite-info-error-count suite-info)
              (suite-info-exception-count suite-info)))
          (test-runner suite-info) tests))))

  (define $run-test-suite
    (lambda (test-suite)
      (let ([suite-info (make-suite-info)])
        (test-suite
          (lambda (name th)
            (printf "Running ~s test suite ...~%" name)
            (th)
            (printf "Ran ~d test~:p with ~d assertion~:p, "
              (suite-info-test-count suite-info)
              (suite-info-assertion-count suite-info))
            (printf "~d test~:p failed (~d error~:p, ~d exception~:p)~%"
              (suite-info-failure-count suite-info)
              (suite-info-error-count suite-info)
              (suite-info-exception-count suite-info)))
          (test-runner suite-info) #t)
        suite-info)))

  (define map-+
    (lambda (f records)
      (let loop ([records records] [sum 0])
        (if (null? records)
            sum
            (loop (cdr records) (+ (f (car records)) sum))))))
  
  (define run-test-suites
    (lambda test-suites
      (let loop ([test-suites test-suites] [suite-info* '()])
        (if (null? test-suites)
            (begin
              (printf "Summary: Ran ~d test suite~:p with ~d test~:p and ~d assertion~:p, "
                (length suite-info*)
                (map-+ suite-info-test-count suite-info*)
                (map-+ suite-info-assertion-count suite-info*))
              (printf "~d test~:p failed (~d error~:p, ~d exception~:p)~%"
                (map-+ suite-info-failure-count suite-info*)
                (map-+ suite-info-error-count suite-info*)
                (map-+ suite-info-exception-count suite-info*)))
            (loop (cdr test-suites) (cons ($run-test-suite (car test-suites)) suite-info*))))))

  (define run-test-suite
    (lambda (test-suite)
      ($run-test-suite test-suite)
      (void))))
