(defpackage mp-float/tests/main
  (:use :cl
        :mp-float
        :rove))
(in-package :mp-float/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :mp-float)' in your Lisp.

(deftest rump
  ;; f(a, b) = 333.75 * b^6 + a^2 * (11 * a^2 * b^2 - b^6 - 121 * b^4 - 2) + 5.5 * b^8 + a/(2b)
  ;; a = 77617, b = 33096
  ;; Answer: -0.82739605...
  (let* ((ans -0.82739605)
         (a (make-mp-float :significand 77617))
         (b (make-mp-float :significand 33096))
         (term1 (mp* (double->mp-float 333.75)
                     (mp-expt b 6)))
         (term2 (mp* (mp-expt a 2)
                     (mp- (mp- (mp- (mp* (make-mp-float :significand 11)
                                         (mp* (mp* a a)
                                              (mp* b b)))
                                    (mp-expt b 6))
                               (mp* (make-mp-float :significand 121)
                                    (mp-expt b 4)))
                          (make-mp-float :significand 2))))
         (term3 (mp* (double->mp-float 5.5)
                     (mp-expt b 8)))
         (term4 (mp/ a
                     (mp* (make-mp-float :significand 2)
                          b)))
         (result (mp+ (mp+ term1 term2) (mp+ term3 term4)))
         (result-double (mp-float->double result)))
    (ok (< (abs (/ (- result-double ans) ans))
           0.0001))))
