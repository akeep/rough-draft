Rough Draft
============

This is a simple Scheme unit testing framework that I hand rolled to replace my
previous home grown testing framework.  The old one was even more basic if you
can believe it.  The new framework allows tests to have arbitrary Scheme code
in addition to assertions.  The test runner is also separated from the test
framework so it is possible to run the tests with a variety of different test
runners.

There are currently two interesting libraries here, one for defining test
groups and one with a test runner in it.  The current test runner expects to be
run on the console.

Defining Tests
===============

Tests can be defined within the ``define-test-suite`` form.  This form takes a
name for the test suite and a list of test definitions defined with
``define-test``.  Each test definition has a name for the test and a set of
definitions and expressions that can include assertions.  For example, the
``foo`` test suite can be define as follows.

    (import (rough-draft unit-test))
    (define-test-suite foo
      (define-test first-test
        (define x 10)
        (assert-eqv? x 10)
        (assert-eq? (string->symbol "foo") 'foo))
      (define-test second-test
        (let ([y (list #\f #\o #\o)])
          (assert-string=? (list->string y) "foo")
          (assert-char=? (car y) #\f))))

Assertions can only be used within the body of a test definition. Assertion
definitons break down into a few categories.  Comparison assertions take two
arguments, first a test expression that produces a value and then an expected
value.  The current list of comparison assertions is ``assert-eq?``,
``assert-equal?``, ``assert-eqv?``, ``assert-<``, ``assert-<=``, ``assert-=``,
``assert->``, ``assert->=``,  ``assert-char=?``, ``assert-fl<=?``,
``assert-fl<?``, ``assert-fl=?``, ``assert-fl>=?``, ``assert-fl>?``,
``assert-fx<=?``, ``assert-fx<?``, ``assert-fx=?``, ``assert-fx>=?``,
``assert-fx>?``, and ``assert-string=?``.  The second category are type
checking assertions that take a single argument that will be tested for type
based on the assertion.  The current list of type checking assertions is
``assert-boolean?``, ``assert-char?``, ``assert-identifier?``,
``assert-string?``, and ``assert-sybmol?``.  Finally there are two special
assertions.  The ``assert-true`` assertion succeeds if the expression it is
passed is a true value.  The ``assert-error`` takes an expression expected to
throw an exception and an expected exception message.

Running Tests
==============

Test are run using a test runner.  Currently the only test runner provided is
the console-test-runner.  It produces results that are easy to read on a
standard unix terminal.  Tests can be run individually or as a complete set as
follows.

    (import (rough-draft test-runner))
    (run-test-suite foo)
    Running foo test suite ...
      Testing first-test ...#t
      Testing second-test ...#t
    Ran 2 tests with 4 assertions, 0 tests failed (0 errors, 0 exceptions)

    (run-test foo first-test)
    Running first-test test from foo test suite ...
      Testing first-test ...#t
    Ran 1 test with 2 assertions, 0 tests failed (0 errors, 0 exceptions)

    (run-tests foo second-test first-test)
    Running second-test, first-test tests from foo test suite ...
      Testing second-test ...#t
      Testing first-test ...#t
    Ran 2 tests with 4 assertions, 0 tests failed (0 errors, 0 exceptions)

The exmples from this readme are in the ``examples`` subdirectory and can be
run from the command line with Chez Scheme using:

    scheme --libdirs "../src" --program example1.ss

The test makes use of features that require Chez Scheme 8.4.  The test can also
be run using Petite Chez Scheme 8.4, simply replace ``scheme`` in the above
line with ``petite``.


