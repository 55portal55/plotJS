; convert hue saturation value to a 24 bit r g b value.

(define (hsv->rgb h s v)
  (let
    ((r 0.0)
     (g 0.0)
     (b 0.0))
    (if (= s 0.0) ; achromatic (grey)
      (begin
        (set! r v)
        (set! g v)
        (set! b v))
      (let
        ((h (* h 6.0))) ; sector 0 .. 5
        (let
          ((sector (inexact->exact (floor h))))
          (let
            ((f (- h sector))) ; fractional part of h
            (let
              ((p (* v (- 1.0 s)))
               (q (* v (- 1.0 (* s f))))
               (t (* v (- 1.0 (* s (- 1.0 f))))))
              (case sector
                ((0)
                  (set! r v)
                  (set! g t)
                  (set! b p))
                ((1)
                  (set! r q)
                  (set! g v)
                  (set! b p))
                ((2)
                  (set! r p)
                  (set! g v)
                  (set! b t))
                ((3)
                  (set! r p)
                  (set! g q)
                  (set! b v))
                ((4)
                  (set! r t)
                  (set! g p)
                  (set! b v))
                (else ; 5
                  (set! r v)
                  (set! g p)
                  (set! b q))))))))
    (let
      ((r (inexact->exact (floor (* 255.0 r))))
       (g (inexact->exact (floor (* 255.0 g))))
       (b (inexact->exact (floor (* 255.0 b)))))
      (+ r (* g 256) (* b 256 256)))))
