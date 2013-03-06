; application environment
; although not obvious, the application is designed using a read/eval/print
; loop (REPL) model.

(define examples '(
  (
    0.0
    0.0
    1.0
    1
    (classic na)
    "(sin (/ 1 z))"
  )
  (
    0.0
    0.0
    2000.0
    1
    (classic na)
    "z"
  )
  (
    0.0
    0.0
    60.0
    1
    (contour na)
    "z"
  )
  (
    -0.5
    0.0
    4.0
    6
    (classic na)
    "(+ (^ z 2) c)" ; mandelbrot
  )
  (
    -0.5
    0.0
    4.0
    9
    (classic na)
    "(+ (^ z 2) c)" ; mandelbrot
  )
  (
    -0.5
    0.0
    4.0
    7
    (stereo smooth-spots)
    "(+ (^ z 2) c)" ; mandelbrot
  )
  (
    -0.5
    0.0
    4.0
    4
    (stereo vertical-stripes)
    "(+ (^ z 2) c)" ; mandelbrot
  )
  (
    0.0
    0.0
    5.0
    5
    (grid na)
    "(* (+ 1 i) (tan z))"
  )
  (
    0.0
    0.0
    5.0
    5
    (spots na)
    "(* (+ 1 i) (tan z))"
  )
  (
    0.0
    0.0
    3.0
    3
    (classic na)
    "(* (- (^ z 2) i) (+ (* 2 (^ z 2)) (* 2 i)))"
  )
  (
    0.0
    0.0
    3.0
    1
    (stereo diagonal-stripes)
    "(* (- (^ z 2) i) (+ (* 2 (^ z 2)) (* 2 i)))"
  )
  (
    0.0
    0.0
    3.0
    1
    (stereo smooth-diagonal-stripes)
    "(* (- (^ z 2) i) (+ (* 2 (^ z 2)) (* 2 i)))"
  )
  (
    0.0
    0.0
    5.0
    5
    (classic na)
    "(/ (* i (* 2 z)) (exp (/ (* 2 z) (sin (* i (* 2 z))))))"
  )
  (
    0.0
    0.0
    0.12
    9
    (contour na)
    "(/ (* i (* 2 z)) (exp (/ (* 2 z) (sin (* i (* 2 z))))))"
  )
  (
    0.0
    0.0
    2.0
    9
    (classic na)
    "(* (+ 1 i) (sin z))"
  )
  (
    0.25
    0.25
    2.0
    2
    (contour na)
    "(sqrt (+ (/ (* (- (* (* z 4) 2) 1) (^ (- (- (* z 4) 2) i) 2)) (+ (^ (* z 4) 2) 2 (* 2 i))) c))"
  )
  (
    0.25
    0.25
    2.0
    1
    (stereo vertical-stripes)
    "(+ (/ (* (- (* (* z 4) 2) 1) (^ (- (- (* z 4) 2) i) 2)) (+ (^ (* z 4) 2) 2 (* 2 i))) c)"
  )
  (
    0.0
    0.0
    1.0
    1
    (grid na)
    "(/ 1 z)"
  )
  (
    0.0
    0.0
    7.0
    1
    (classic na)
    "(^ (^ (^ z z) z) z)"
  )
  (
    0.0
    0.0
    2.5
    1
    (classic na)
    "(sqrt (- 1 (* x x)))"
  )
  (
    0.0
    0.0
    2.5
    1
    (classic na)
    "(abs (* (cos (* 6 pi x)) (exp (- (^ (* 2.0 x) 2)))))"
  )
  (
    0.0
    0.0
    2.0
    1
    (classic na)
    "(* 0.5 (/ 4 pi) (+ (sin (* 2 pi x))
       (* (/ 1 3) (sin (* 6 pi x)))
       (* (/ 1 5) (sin (* 10 pi x)))
       (* (/ 1 7) (sin (* 14 pi x)))))"
  )
  (
    0.0
    0.0
    2.0
    1
    (classic na)
    "(+ (cos (* 2 pi x)) (cos (* 2 pi y)))"
  )
  (
    0.0
    0.0
    2.0
    1
    (classic na)
    "(* 100 (+ (^ x 2) (^ y 2)))"
  )
  (
    0.0
    0.0
    2.0
    1
    (classic na)
    "(* x y)"
  )
  (
    0.0
    0.0
    2.0
    1
    (classic na)
    "(* (sinc (* 10 x)) (sinc (* 10 y)))"
  )
))

; globals

(define exampleidx (- (length examples) 1))
(define centerx 0.0) ; real axis coordinate
(define centery 0.0) ; imaginary axis coordinate
(define width 1.0) ; real axis view width
(define iterations 1)
(define colortype '(classic na))
(define fn "") ; function text to read
(define zoomfactor 0.6)

(define (get-example n)
  (let
    ((example (list-ref examples n)))
  example))

(define (increxample)
  (set! exampleidx (+ exampleidx 1))
  (if (= exampleidx (length examples))
    (set! exampleidx 0))
  (let
    ((example (get-example exampleidx)))
    (set! centerx (car example)) ; real axis coordinate
    (set! centery (car (cdr example))) ; imaginary axis coordinate
    (set! width (car (cddr example))) ; real axis view width
    (set! iterations (car (cddr (cdr example))))
    (set! colortype (car (cddr (cddr example))))
    (set! fn (car (cddr (cddr (cdr example))))))) ; function text

(define (colortypeget)
  (if (eq? (car colortype) 'classic)
    "classic"
    (if (eq? (car colortype) 'contour)
      "contour"
      (if (eq? (car colortype) 'grid)
        "grid"
        (if (eq? (car colortype) 'stereo)
          "stereo"
          (if (eq? (car colortype) 'spots)
            "spots"
            (if (eq? (car colortype) 'tile)
              "tile"
              "none")))))))

(define (colortypeset)
  (if (eq? (car colortype) 'classic)
    (set! colortype '(contour na))
    (if (eq? (car colortype) 'contour)
      (set! colortype '(grid na))
      (if (eq? (car colortype) 'grid)
        (set! colortype '(stereo smooth-spots))
        (if (eq? (car colortype) 'stereo)
          (set! colortype '(spots na))
          (if (eq? (car colortype) 'spots)
          ; (set! colortype '(tile na))
            (set! colortype '(classic na))
            (if (eq? (car colortype) 'tile)
              (set! colortype '(classic na))
              (set! colortype '(classic na)))))))))

(define (colortype2get)
  (if (eq? (cadr colortype) 'smooth-spots)
    "smooth spots"
    (if (eq? (cadr colortype) 'vertical-stripes)
      "vert stripes"
      (if (eq? (cadr colortype) 'diagonal-stripes)
        "diag stripes"
        (if (eq? (cadr colortype) 'smooth-diagonal-stripes)
          "smooth diag stripes"
          (if (eq? (cadr colortype) 'na)
            "na"
            "na"))))))

(define (colortype2set)
  (let
    ((type1 (car colortype))
     (type2 (cadr colortype)))
    (if (eq? type1 'stereo)
      (if (eq? type2 'na)
        (set! type2 'smooth-spots)
        (if (eq? type2 'smooth-spots)
          (set! type2 'vertical-stripes)
          (if (eq? type2 'vertical-stripes)
            (set! type2 'diagonal-stripes)
            (if (eq? type2 'diagonal-stripes)
              (set! type2 'smooth-diagonal-stripes)
              (if (eq? type2 'smooth-diagonal-stripes)
                (set! type2 'smooth-spots)
                (set! type2 'smooth-spots)))))))
    (set! colortype (list type1 type2))))

(define (read-f s) ; read a function from a string
  (set! read-buffer (string->list s))
  (readx))

(define (plotmain)
  (plotter centerx centery width iterations colortype (read-f fn)))

(define (mouseClick x y)
  (set! centerx
    (+ (upper-left-x centerx width) (* width (/ (exact->inexact x) WIDTH))))
  (let
    ((height (* HEIGHT (/ width WIDTH))))
    (set! centery
      (+ (upper-left-y centery height)
         (* height (/ (exact->inexact y) HEIGHT))))))
