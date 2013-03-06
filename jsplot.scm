
(define (plot x y r g b)
  (plotRGB ctx x y r g b)) ; interface to javascript

(define (plot-pixel x y value)
  (let
    ((r (remainder value 256))
     (g (remainder (quotient value 256) 256))
     (b (remainder (quotient value (* 256 256)) 256)))
    (plot x y r g b)))
