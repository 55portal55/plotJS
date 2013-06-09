
;;; function plotter

(define WIDTH 320) ; display resolution
(define HEIGHT 320) ; display resolution

(define (complex-function f iterations r i step x y)
  (let
    ((cr (+ r (* (exact->inexact x) step) (jitter step)))
     (ci (+ i (* (exact->inexact y) step) (jitter step))))
    (evaluate-iterations f iterations cr ci cr ci)))

(define (complex-plotter f iterations color-type r i step width height)
  (do ((y 0 (+ y 1))) ((= y height))
    (do ((x 0 (+ x 1))) ((= x width))
      (complex-color x y (complex-function f iterations r i step x y) color-type))))

(define (range y fx1 fx2 delta)
  (if (or (nan? fx1) (nan? fx2) (is-infinite fx1) (is-infinite fx2))
    #f
    (if (<= fx1 fx2)
      (and (> y (- fx1 delta)) (< y (+ fx2 delta)))
      (and (< y (+ fx1 delta)) (> y (- fx2 delta))))))

(define (fxy-function f r i step x y)
  (let
    ((new-x (+ r (* (exact->inexact x) step)))
     (new-y (+ i (* (exact->inexact y) step))))
    (evaluate-fxy f new-x new-y)))

(define (range-step fx1 fx2 step)
  (let
    ((slope (/ (abs (- fx2 fx1)) step)))
    (if (> slope 100.0)
      step
      (/ step 2.0))))

(define (real-function f lookup r i step x y)
  (let
    ((new-x (+ r (* (exact->inexact x) step)))
     (new-y (+ i (* (exact->inexact y) step))))
    (let
      ((fx1 (evaluate-real f lookup new-x x))
       (fx2 (evaluate-real f lookup (+ new-x step) (+ x 1))))
      (if (range new-y fx1 fx2 (range-step fx1 fx2 step))
        'point
        'no-point))))

(define (fxy-plotter f iterations color-type r i step width height)
  (do ((y 0 (+ y 1))) ((= y height))
    (do ((x 0 (+ x 1))) ((= x width))
      (let
        ((z (fxy-function f r i step x y)))
        (fxy-color (- z (floor z)) x y)))))

(define (real-plotter f iterations color-type r i step width height)
  (init-f-lookup)
  (let
    ((x-axis-f (read-f "0"))
     (y-axis-f (read-f "(* 1e20 x)")))
    (do ((y 0 (+ y 1))) ((= y height))
      (do ((x 0 (+ x 1))) ((= x width))
        (let
          ((plot-f (real-function f f-lookup r i step x y))
           (plot-x-axis-f (real-function x-axis-f x-axis-lookup r i step x y))
           (plot-y-axis-f (real-function y-axis-f y-axis-lookup r i step x y)))
          (if (eq? plot-f 'point)
            (real-color MAGENTA x y)
            (if (eq? plot-x-axis-f 'point)
              (real-color CYAN x y)
              (if (eq? plot-y-axis-f 'point)
                (real-color CYAN x y)
                (real-color OFF-WHITE x y)))))))))

(define (upper-left-x center-x width)
  (- center-x (* width 0.5)))

(define (upper-left-y center-y width)
  (- center-y (* width 0.5)))

(define (get-step image-size width)
  (/ width image-size))
  
(define (flatten x)
  (letrec (
    (flatten-help
      (lambda (x y)
        (cond
          ((null? x) y)
          ((symbol? x) (cons x y))
          ((number? x) (cons x y))
          ((string? x) (cons x y))
          ((char? x) (cons x y))
          (else
            (flatten-help
              (car x)
              (flatten-help (cdr x) y)))))))
    (flatten-help x '())))

(define (f-complex? f)
  (let
    ((l (flatten f)))
    (or (memq 'z l) (memq 'i l) (memq 'c l))))

(define (f-xy? f)
  (let
    ((l (flatten f)))
    (or (memq 'y l))))

(define (plotter center-x center-y width iterations color-type f)
  (let
     ((image-width WIDTH) ; display resolution
      (image-height HEIGHT)) ; display resolution
    (display-size image-width image-height) ; used when pure scheme non js
    (let
      ((height (* image-height (/ width image-width))))
      ; determine if 2D 3D or complex function type
      (if (f-complex? f)
        (complex-plotter
          f
          iterations
          color-type
          (upper-left-x center-x width)
          (upper-left-y center-y height)
          (get-step image-width width)
          image-width image-height)
        (if (f-xy? f)
          (fxy-plotter
            f
            iterations
            color-type
            (upper-left-x center-x width)
            (upper-left-y center-y height)
            (get-step image-width width)
            image-width image-height)
          (real-plotter
            f
            iterations
            color-type
            (upper-left-x center-x width)
            (upper-left-y center-y height)
            (get-step image-width width)
            image-width image-height))))))
