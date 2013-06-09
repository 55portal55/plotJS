; domain coloring routines

(define (distort x)
  (let
    ((x (- x 1.0)))
    (sqrt (- 1.0 (* x x))))) ; left half of a semi-circle

(define (contour-saturation x)
  (* 0.5 (- 1.0 (cos (* 2.0 PI (+ x 0.5))))))

(define (contour-value x)
  (if (< x 0.5)
    1.0
    (- 1.0 (expt (cos (* 2.0 PI (+ x 0.25))) 2.0))))

(define (scale-color x factor)
  (+ (* x factor) (- 1.0 factor)))

(define (grid-saturation x p)
  (- 1.0 (expt (cos (* 2.0 PI x)) p)))

(define (grid-value x p)
  (- (expt (cos (* 2.0 PI x)) p)))

(define (clip x)
  (if (< x 0.0)
    0.0
    (if (>= x 1.0)
      0.99999999
      x)))
  
(define (cartesian->spherical vec)
  (let
    ((x (car vec))
     (y (cadr vec))
     (z (car (cddr vec))))
    (let
      ((latitude (acos z)) ; is in the range 0 .. pi
       (longitude (+ (atan2 y x) PI))) ; is in the range 0 .. 2 * pi
      (set! latitude (/ latitude PI)) ; normalize to 0 .. 1
      (set! longitude (/ longitude (* 2.0 PI))) ; normalize to 0 .. 1
      (list longitude latitude))))

(define (equatorial-plane->unit-sphere X Y)
  (let
    ((x (/ (* 2.0 X) (+ 1.0 (* X X) (* Y Y)))) ; TODO factor out denominator
     (y (/ (* 2.0 Y) (+ 1.0 (* X X) (* Y Y))))
     (z (/ (+ -1.0 (* X X) (* Y Y)) (+ 1.0 (* X X) (* Y Y)))))
    (list x y z)))

(define spot-threshold 0.5) ; settable in the animation version of plotJS

(define (procedural-map x y stereo-type)
  (cond
    ((eq? stereo-type 'smooth-spots)
      (+ (/ (+ (sin (* 2.0 PI x)) (sin (* 2.0 PI y))) 4.0) 0.5))
    ((eq? stereo-type 'hard-spots)
      (let
        ((offset (* 4.0 2.0 PI clock)))
        (if (< (+ (/ (* (sin (+ offset (* 20.0 2.0 PI x)))
                        (sin (+ offset (* 20.0 2.0 PI y)))) 0.5) 0.5)
          spot-threshold)
          1.0
          0.0)))
    ((eq? stereo-type 'vertical-stripes)
      (if (= (remainder (inexact->exact (truncate (* x 6.0))) 2) 0)
        0.0
        1.0))
    ((eq? stereo-type 'diagonal-stripes)
      (if (= (remainder (inexact->exact (truncate (* (+ x y) 6.0))) 2) 0)
        0.0
        1.0))
    ((eq? stereo-type 'smooth-diagonal-stripes)
      (let
        ((z (* (+ x y) 6.0)))
        (contour-value (- z (truncate z)))))
    (else
      0.0)))

(define (complex-color i j c color-type)
  (let
    ((x (car c))
     (y (cadr c))
     (color-type (car color-type))
     (stereo-type (cadr color-type))
     (m (mag c))
     (value 0.0)
     (phase 0.0)
     (saturation 0.0))

    ; determine hue
    (set! phase (/ (+ (/ (arg c) PI) 1.0) 2.0))
    ; shift phase
    (set! phase (+ phase (/ 1.0 2.0)))
    (if (>= phase 1.0)
      (set! phase (- phase 1.0)))

    (if (or (eq? color-type 'contour) (eq? color-type 'grid))
      (begin
        (set! value (log2 (+ m 1.0)))
        (set! value (- value (floor value)))) ; color range 0.0 .. 1.0
      (if (eq? color-type 'spots)
        (let
          ((newx (* 1.0 x))
           (newy (* 1.0 y)))
          (set! newx (- newx (floor newx)))
          (set! newy (- newy (floor newy)))
          (set! value (+ (/ (+ (sin (* 2.0 PI newx))
                               (sin (* 2.0 PI newy)))
                           4.0) 0.5))
          (set! saturation 0.0)
          (set! phase 0.0))
        (if (eq? color-type 'stereo) ; procedural kind
          (let
            ((new '())
             (vec '()))
            (set! vec (equatorial-plane->unit-sphere x y))
            (set! new (cartesian->spherical vec))
            (set! value (procedural-map (car new) (cadr new) stereo-type))
            (set! saturation 0.0)
            (set! phase 0.0))
          (if (eq? color-type 'tile)
            (begin
            ; (set! saturation (/ (+ (- x (floor x)) (- y (floor y))) 2.0))
              (set! saturation (- x (floor x)))
              (set! value (- y (floor y)))
)
;             (set! saturation (/ (+ (- x (floor x)) (- y (floor y))) 2.0))
;             (set! value 0.9999))
            (begin ; classic
              (set! value (/ (log10 (+ m 1.0)) 40.0))
              (set! value (distort value)))))))

    (if (eq? color-type 'contour)
      (begin
        (set! saturation (scale-color (contour-saturation value) 0.7))
        (set! value (scale-color (contour-value value) 0.3)))
      (if (eq? color-type 'grid)
        (let
          ((newx (* 1.0 x))
           (newy (* 1.0 y)))
          (set! value (clip (+ (scale-color (grid-value value 80.0) 0.5)
                               (scale-color (grid-value value 2.0) 0.5))))
          (set! saturation (* (grid-saturation (- newx (floor newx)) 16.0)
                              (grid-saturation (- newy (floor newy)) 16.0))))
        (if (eq? color-type 'classic)
          (begin ; magnitude is value 0.0 .. 1.0
            (if (> value 0.5)
              (begin
                (set! saturation (- 1.0 (* (- value 0.5) 2.0)))
                (set! value 0.99999999))
              (begin
                (set! saturation 0.99999999)
                (set! value (* value 2.0))))))))

    (plot-pixel i j (hsv->rgb phase saturation value))))

(define (fxy-color z i j)
  (plot-pixel i j (hsv->rgb z 1.0 1.0)))

(define MAGENTA (+ 255 (* 255 256 256)))
(define CYAN (+ (* 255 256) (* 255 256 256)))
(define WHITE (+ 255 (* 255 256) (* 255 256 256)))
(define OFF-WHITE (+ 255 (* 255 256) (* 230 256 256)))

(define (real-color color i j)
  (plot-pixel i j color))
