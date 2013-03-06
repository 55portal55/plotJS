; complex number routines because my scheme and scheme2js lack them

(define (make-complex r i)
  (list r i))

(define (get-real z)
  (car z))

(define (get-imaginary z)
  (cadr z))

(define (mag z)
  (let
    ((r (get-real z))
     (i (get-imaginary z)))
    (let
      ((m (sqrt (+ (* r r) (* i i)))))
      (if (or (is-infinite m) (nan? m))
        INFINITY
        m))))

(define (arg z) ; phase between -pi .. pi
  (let
    ((r (get-real z))
     (i (get-imaginary z)))
    (if (or (>= r INFINITY) (>= i INFINITY))
      0.0
      (if (> r 0.0)
        (atan (/ i r))
        (if (< r 0.0)
          (if (>= i 0.0)
            (+ (atan (/ i r)) PI)
            (- (atan (/ i r)) PI))
          ; r = 0
          (if (> i 0.0)
            (/ PI 2.0)
            (if (< i 0.0)
              (- (/ PI 2.0))
              ; i = 0
              ; really undefined
              PI)))))))

(define (complex-add z1 z2)
  (let
    ((z1r (get-real z1))
     (z1i (get-imaginary z1))
     (z2r (get-real z2))
     (z2i (get-imaginary z2)))
    (make-complex (+ z1r z2r) (+ z1i z2i))))

(define (complex-subtract z1 z2)
  (let
    ((z1r (get-real z1))
     (z1i (get-imaginary z1))
     (z2r (get-real z2))
     (z2i (get-imaginary z2)))
    (make-complex (- z1r z2r) (- z1i z2i))))

(define (complex-minus z)
  (complex-multiply z (make-complex -1.0 0.0)))

(define (complex-multiply z1 z2)
  (let
    ((z1r (get-real z1))
     (z1i (get-imaginary z1))
     (z2r (get-real z2))
     (z2i (get-imaginary z2)))
    (make-complex (- (* z1r z2r) (* z1i z2i)) (+ (* z1r z2i) (* z1i z2r)))))

(define (complex-divide z1 z2)
  (let
    ((z1r (get-real z1))
     (z1i (get-imaginary z1))
     (z2r (get-real z2))
     (z2i (get-imaginary z2)))
    (make-complex
      (/ (+ (* z1r z2r) (* z1i z2i))
         (+ (* z2r z2r) (* z2i z2i)))
      (/ (- (* z1i z2r) (* z1r z2i))
         (+ (* z2r z2r) (* z2i z2i))))))

(define (complex-sqrt z)
  (let
    ((a (get-real z))
     (b (get-imaginary z)))
    (let
      ((modulus (sqrt (+ (* a a) (* b b)))))
      (let
        ((gamma (sqrt (/ (+ a modulus) 2.0)))
         (delta (* (sgn b) (sqrt (/ (- modulus a) 2.0)))))
        (make-complex gamma delta)))))

(define (complex-exp z)
  (let
    ((x (get-real z))
     (y (get-imaginary z)))
    (let
      ((etoa (exp x)))
      (make-complex (* etoa (cos y)) (* etoa (sin y))))))

(define (complex-sin z)
  (let
    ((x (get-real z))
     (y (get-imaginary z)))
    (make-complex (* (sin x) (cosh y)) (* (cos x) (sinh y)))))

(define (complex-cos z)
  (let
    ((x (get-real z))
     (y (get-imaginary z)))
    (make-complex (* (cos x) (cosh y)) (* (sin x) (sinh y)))))

(define (complex-tan z)
  (let
    ((x (get-real z))
     (y (get-imaginary z))
     (term (complex-exp
       (complex-multiply (make-complex 2.0 0.0)
         (complex-multiply (make-complex 0.0 1.0) z)))))
    (complex-divide
      (complex-subtract
        term
        (make-complex 1.0 0.0))
      (complex-multiply
        (make-complex 0.0 1.0)
        (complex-add
          term
          (make-complex 1.0 0.0))))))

(define (complex-log z) ; multifunction restricted to principle value
  ; not defined for z = 0.0
  (make-complex (log (mag z)) (arg z)))

(define (complex-power z w)
  (complex-exp (complex-multiply w (complex-log z))))

(define (complex-abs z)
  (complex-sqrt (complex-multiply z z)))

(define (complex-asin z)
  (complex-multiply
    (make-complex 0.0 -1.0)
    (complex-log
      (complex-add
        (complex-multiply (make-complex 0.0 1.0) z)
        (complex-sqrt
          (complex-subtract
            (make-complex 1.0 0.0)
            (complex-multiply z z)))))))

(define (complex-acos z)
  (complex-subtract
    (make-complex (/ PI 2.0) 0.0)
    (complex-asin z)))

(define (complex-atan z)
  (complex-divide
    (complex-subtract
      (complex-log
        (complex-add
          (make-complex 1.0 0.0)
          (complex-multiply (make-complex 1.0 0.0) z)))
      (complex-log
        (complex-subtract
          (make-complex 1.0 0.0)
          (complex-multiply (make-complex 1.0 0.0) z))))
    (complex-multiply
      (make-complex 2.0 0.0)
      (make-complex 0.0 1.0))))
