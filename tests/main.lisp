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
    (let* ((a (make-mp-float :significand 77617))
           (b (make-mp-float :significand 33096))
           (term1 (mp* (double->mp-float 333.75)
                       (mp* (mp* b b)
                            (mp* (mp* b b)
                                 (mp* b b)))))
           (term2 (mp* (mp* a a)
                       (mp- (mp- (mp- (mp* (make-mp-float :significand 11)
                                           (mp* (mp* a a)
                                                (mp* b b)))
                                      (mp* (mp* b b)
                                           (mp* (mp* b b)
                                                (mp* b b))))
                                 (mp* (make-mp-float :significand 121)
                                      (mp* (mp* b b)
                                           (mp* b b))))
                            (make-mp-float :significand 2))))
           (term3 (mp* (double->mp-float 5.5)
                       (mp* (mp* (mp* b b)
                                 (mp* b b))
                            (mp* (mp* b b)
                                 (mp* b b)))))
           (term4 (mp/ a
                       (mp* (make-mp-float :significand 2)
                            b)))
           (result (mp+ (mp+ term1 term2) (mp+ term3 term4))))
      (ok (< (abs (/ (- (mp-float->double result)
                        -0.82739605)
                     -0.82739605))
             0.0001))))
