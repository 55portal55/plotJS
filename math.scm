; miscellaneous math routines

(define INFINITY 1.0e40)
(define PI 3.1415926535897932)
(define E 2.71828182845904524)

(define (nan? x)
  (not (= x x))) ; cute trick

(define (is-infinite x) (or (>= x INFINITY) (<= x (- INFINITY))))

(define (sgn x)
  (if (< x 0.0)
    -1.0
    (if (= x 0.0)
      0.0
      1.0)))

(define (sinc x) ; TODO complex
  (if (= x 0.0)
    1.0
    (/ (sin x) x)))

(define (log2 x)
  (/ (log x) (log 2.0)))
 
(define (log10 x)
  (/ (log x) (log 10.0)))

(define (sinh x)
  (/ (- (exp x) (exp (- x))) 2.0))

(define (cosh x)
  (/ (+ (exp x) (exp (- x))) 2.0))

(define (atan2 y x) ; algorithm from wikipedia
  (let
    ((HALF-PI (/ PI 2.0)))
    (cond
      ((> x 0.0) (atan (/ y x)))
      ((and (>= y 0.0) (< x 0.0)) (+ (atan (/ y x)) PI))
      ((and (< y 0.0) (< x 0.0)) (- (atan (/ y x)) PI))
      ((and (> y 0.0) (= x 0.0)) HALF-PI)
      ((and (< y 0.0) (= x 0.0)) (- HALF-PI))
      (else ; really undefined
        0.0))))
