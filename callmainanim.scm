; used only when running standalone scheme and animating

; get data passed from the controlling script

(set! WIDTH (car data))
(set! HEIGHT (cadr data))
(define frame (cadr (cdr data)))
(define n-frames (cadr (cddr data)))
(set! clock (/ (exact->inexact frame) (exact->inexact n-frames)))

; this provided example uses hard spots and other following parameters

; distort clock for threshold - allocates more time to equal valued spots
;           /
;        --
;      /
;     /
;  --
;/
(define threshold-clock (+ (* 0.07 (sin (* 2.0 2.0 PI clock))) clock))
(set! spot-threshold (+ 0.5 (* 2.0 (cos (* 2.0 PI (+ 0.5 threshold-clock))))))

(increxample)

(set! centerx 0.0)
(set! centery 0.0)
;(set! width (* 2.0 PI))
(set! width 4.0)
(set! iterations 2)
(set! colortype '(stereo hard-spots))
;(set! fn "(tan z)")
(set! fn "(tan (* i (/ 1.0 z)))")

(plotmain)
