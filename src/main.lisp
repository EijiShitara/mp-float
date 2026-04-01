(uiop:define-package mp-float
  (:use #:cl)
  (:export #:mp-float
           #:mp-normalize
           #:mp+
           #:mp-
           #:mp*
           #:mp/
           #:mp-float->double
           #:double->mp-float
           #:mp-sqrt))
(in-package #:mp-float)

(defstruct mp-float
  (significand 0 :type integer)
  (exponent    0 :type integer)
  (precision 256 :type (integer 1)))

(defun mp-normalize (f)
  "Normalize the significand to the precision's bit"
  (with-slots (significand exponent precision) f
    (let* ((bits (integer-length (abs significand)))
           (shift (- bits precision)))
      (if (> shift 0)
          (make-mp-float :significand (ash significand (- shift))
                         :exponent (+ exponent shift)
                         :precision precision)
          f))))

(defun mp+ (a b)
  "加算: 指数部を揃えて仮数部を加算"
  (let* ((ea (mp-float-exponent a))
         (eb (mp-float-exponent b))
         (sa (mp-float-significand a))
         (sb (mp-float-significand b))
         (prec (max (mp-float-precision a) (mp-float-precision b))))
    (multiple-value-bind (big-s big-e)
        (if (>= ea eb)
            (values (+ (ash sa (- ea eb)) sb) eb)
            (values (+ sa (ash sb (- eb ea))) ea))
      (mp-normalize
       (make-mp-float :significand big-s
                      :exponent    big-e
                      :precision   prec)))))

(defun mp- (a b)
  (mp+ a
       (make-mp-float :significand (- (mp-float-significand b))
                      :exponent (mp-float-exponent b)
                      :precision (mp-float-precision b))))

(defun mp* (a b)
  "乗算: 仮数部の積・指数部の和"
  (mp-normalize
   (make-mp-float
    :significand (* (mp-float-significand a) (mp-float-significand b))
    :exponent    (+ (mp-float-exponent a) (mp-float-exponent b))
    :precision   (max (mp-float-precision a) (mp-float-precision b)))))

(defun mp/ (a b)
  "除算: a / b
     整数仮数を left-shift して sa/sb の精度を precision ビット確保する。
     value = (sa * 2^shift / sb) * 2^(ea - eb - shift)"
  (when (zerop (mp-float-significand b))
    (error "Division by zero in mp/"))
  (let* ((sa   (mp-float-significand a))
         (sb   (mp-float-significand b))
         (ea   (mp-float-exponent a))
         (eb   (mp-float-exponent b))
         (prec (max (mp-float-precision a) (mp-float-precision b)))
         ;; 2ビット余裕を持たせて丸め誤差を抑える
         (shift (+ prec 2))
         ;; ash sa shift で分子を拡大し、整数除算で quotient を得る
         (sig  (round (ash sa shift) sb))
         (exp  (- (- ea eb) shift)))
    (mp-normalize
     (make-mp-float :significand sig
                    :exponent   exp
                    :precision  prec))))

(defun mp-float->double (f)
  "mp-float を double-float に変換する。
     value = significand * 2^exponent なので scale-float で指数部を適用する。"
  (let ((sig (mp-float-significand f))
        (exp (mp-float-exponent f)))
    (if (zerop sig)
        0.0d0
        ;; (float sig 1.0d0) で bignum → double（精度は落ちるが近似として十分）
        ;; scale-float で * 2^exp を適用
        (scale-float (float sig 1.0d0) exp))))

(defun double->mp-float (d &key (precision 256))
  "double-float を mp-float に変換する。
     decode-float で仮数部・指数部を取り出し、precision ビットの整数仮数に変換する。"
  (let ((d (float d 1.0d0)))
    (if (zerop d)
        (make-mp-float :significand 0 :exponent 0 :precision precision)
        (multiple-value-bind (sig exp sign)
            (decode-float d)
          ;; decode-float: d = sign * sig * 2^exp, sig ∈ [0.5, 1.0)
          ;; → 整数仮数: sig-int ≈ sig * 2^precision
          ;; → 実際の指数: exp - precision
          (let* ((sig-int   (round (scale-float sig precision)))
                 (signed-sig (* (round sign) sig-int)))
            (mp-normalize
             (make-mp-float :significand signed-sig
                            :exponent   (- exp precision)
                            :precision  precision)))))))

(defun mp-sqrt (x &key (iterations 50))
  "ニュートン法による平方根"
  (let* ((prec (mp-float-precision x))
         ;; 初期値: double-float で近似
         (x0-d (sqrt (mp-float->double x)))
         (r (double->mp-float x0-d :precision prec)))
    (dotimes (_ iterations r)
      ;; r = (r + x/r) / 2
      (setf r (mp* (mp+ r (mp/ x r))
                   (double->mp-float 0.5d0 :precision prec))))))
