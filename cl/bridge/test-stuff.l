(defpackage :test-stuff
  (:use :common-lisp)
  (:export :expect :expect-equal :expect-tree-equal :expect-error :fail :tests
           :*failures* :run-tests :spew-trace-on-failure))

(in-package :test-stuff)

;; There are fancier unit-test ideas at
;; http://freesw.onshored.com/wwwdist/xptest/, but for the time being,
;; the below suffices.

(defparameter *testing* t
  "When true, tests defined by TESTS will get evaluated.")

(defparameter *failures* ()
  "Messages from tests that failed on the last run.")

(defun fail (&rest format-args)
  (push (apply #'format  (cons nil format-args)) *failures*)
  nil)

(defmacro expect (test-form)
  "Evaluates TEST-FORM and returns the result.  But: if the result is
nil, prints a message to that effect, which includes the text of
TEST-FORM."
  `(if (not ,test-form)
       (fail "~S returned nil"
              ',test-form)
     t))

(defmacro expect-equal (expected-form actual-form)
  "Evaluates both FORMS.  If they are not equal, prints a message to
that effect."
  `(expect-tree-equal ,expected-form ,actual-form))

(defmacro expect-tree-equal (expected-form actual-form)
  "Evaluates both FORMS.  If they are not tree-equal, prints a message to
that effect."
  (let ((exp-value (gensym))
        (act-value (gensym)))
    `(let ((,exp-value ,expected-form)
           (,act-value ,actual-form))
       (if (not (tree-equal ,exp-value ,act-value))
           (fail "from ~S, got ~S, but expected ~S"
                 ',actual-form
                 ,act-value
                 ,exp-value)
         t))))

(defmacro expect-error (test-form)
  "Evaluates the TEST-FORM.  If doing so does *not* cause an error,
prints a message to that effect."
  (let ((got-error (gensym)))
    `(let ((,got-error nil))
       (handler-case ,test-form 
                     (error (condition) 
                            (declare (ignore condition))
                            (setf ,got-error t)))
       (if (not ,got-error)
           (fail "~S should have caused an error." ',test-form)
         t))))

(defmacro run-tests (suite-name &rest names-of-test-functions)
  "Runs all the named functions, in order.  If any of them calls FAIL
\(which see\), reports that fact by printing a message on standard output."
  (let ((some-test-sym (gensym)))
    `(progn
       (setf *failures* '())

       (dolist (,some-test-sym ',names-of-test-functions *failures*)
         (format t ".") (finish-output)
         (funcall (symbol-function ,some-test-sym)))

       (format t "~&")

       (if (null *failures*)
           (format t "~&~S tests passed.~&" ,suite-name)
         (format t "FAILURES in ~S: ~S~&" ,suite-name (reverse *failures*)))

       (null *failures*))))

(defmacro spew-trace-on-failure (&rest forms)
  "Evaluates forms, while capturing output from traced functions.  If,
after evaluating, *failures* is non-nil (i.e., if one of the forms
called FAIL), prints the trace output."
  (let ((diag (gensym))
        (initial-failures (gensym)))
    `(let* ((,diag (make-string-output-stream))
            (*trace-output* ,diag)
            (,initial-failures (length *failures*)))
       ,@forms
       (if (> (length *failures*)
              ,initial-failures)
           (format t "Trace of failed call: ~S~&"
                   (get-output-stream-string ,diag))))))
