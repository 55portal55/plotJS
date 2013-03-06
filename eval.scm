; evaluation routines

(define (my-add mode x y)
  (if (eq? mode 'complex)
    (complex-add x y)
    (+ x y)))

(define (my-subtract mode x y)
  (if (eq? mode 'complex)
    (complex-subtract x y)
    (- x y)))

(define (my-multiply mode x y)
  (if (eq? mode 'complex)
    (complex-multiply x y)
    (* x y)))

(define (my-divide mode x y)
  (if (eq? mode 'complex)
    (complex-divide x y)
    (/ x y)))

(define (my-power mode x y)
  (if (eq? mode 'complex)
    (complex-power x y)
    (expt x y)))

(define (my-minus mode x)
  (if (eq? mode 'complex)
    (complex-minus x)
    (- x)))

(define (my-sin mode x)
  (if (eq? mode 'complex)
    (complex-sin x)
    (sin x)))

(define (my-cos mode x)
  (if (eq? mode 'complex)
    (complex-cos x)
    (cos x)))

(define (my-tan mode x)
  (if (eq? mode 'complex)
    (complex-tan x)
    (tan x)))

(define (my-exp mode x)
  (if (eq? mode 'complex)
    (complex-exp x)
    (exp x)))

(define (my-asin mode x)
  (if (eq? mode 'complex)
    (complex-asin x)
    (asin x)))

(define (my-acos mode x)
  (if (eq? mode 'complex)
    (complex-acos x)
    (acos x)))

(define (my-atan mode x)
  (if (eq? mode 'complex)
    (complex-atan x)
    (atan x)))

(define (my-sqrt mode x)
  (if (eq? mode 'complex)
    (complex-sqrt x)
    (sqrt x)))

(define (my-log mode x)
  (if (eq? mode 'complex)
    (complex-log x)
    (log x)))

(define (my-abs mode x)
  (if (eq? mode 'complex)
    (complex-abs x)
    (abs x)))

(define (my-sinc mode x) ; TODO complex
  (sinc x))

;;; eval routines

(define (eval-list mode function x y cx cy lst)
  (if (null? lst)
    (if (or (eq? function '+) (eq? function '-))
      (if (eq? mode 'complex)
        (make-complex 0.0 0.0)
        0.0)
      (if (eq? mode 'complex)
        (make-complex 1.0 0.0)
        1.0))
    (let
      ((e (evaluate mode (car lst) x y cx cy)))
      (cond
        ((eq? function '+)
          (my-add mode e (eval-list mode function x y cx cy (cdr lst))))
        ((eq? function '-)
          (my-subtract mode e (eval-list mode function x y cx cy (cdr lst))))
        ((eq? function '*)
          (my-multiply mode e (eval-list mode function x y cx cy (cdr lst))))
        (else ; /
          (my-divide mode e (eval-list mode function x y cx cy (cdr lst))))))))

(define (evaluate mode lst x y cx cy)
  (cond
    ((null? lst)
      (if (eq? mode 'complex)
        (make-complex 0.0 0.0)
        0.0))
    ((number? lst)
      (if (eq? mode 'complex)
        (make-complex lst 0.0)
        lst))
    ((symbol? lst)
      (cond
        ((eq? lst 'x)
          (if (eq? mode 'complex)
            (make-complex x 0.0)
            x))
        ((eq? lst 'y)
          (if (eq? mode 'complex)
            (make-complex y 0.0)
            y))
        ((eq? lst 'z) (make-complex x y))
        ((eq? lst 'c) (make-complex cx cy))
        ((eq? lst 'i) (make-complex 0.0 1.0))
        ((eq? lst 'e)
          (if (eq? mode 'complex)
            (make-complex E 0.0)
            E))
        ((eq? lst 'pi)
          (if (eq? mode 'complex)
            (make-complex PI 0.0)
            PI))
        (else
          (if (eq? mode 'complex)
            (make-complex 0.0 0.0)
            0.0))))
    ((pair? lst)
      (let
        ((function (car lst)))
        (if (or
          (eq? function '+)
          (and (eq? function '-) (not (= (length lst) 2)))
          (eq? function '*)
          (eq? function '/))
          (eval-list mode function x y cx cy (cdr lst))
          (let
            ((a (make-complex 0.0 0.0))
             (b (make-complex 0.0 0.0)))
            (set! a (evaluate mode (cadr lst) x y cx cy))
            (if (eq? function '^)
              (begin
                (set! b (evaluate mode (car (cddr lst)) x y cx cy))
                (my-power mode a b))
              (cond
                ((eq? function '-) (my-minus mode a)) ; unary minus
                ((eq? function 'sin) (my-sin mode a))
                ((eq? function 'cos) (my-cos mode a))
                ((eq? function 'tan) (my-tan mode a))
                ((eq? function 'exp) (my-exp mode a))
                ((eq? function 'asin) (my-asin mode a))
                ((eq? function 'acos) (my-acos mode a))
                ((eq? function 'atan) (my-atan mode a))
                ((eq? function 'sqrt) (my-sqrt mode a))
                ((eq? function 'log) (my-log mode a))
                ((eq? function 'abs) (my-abs mode a))
                ((eq? function 'sinc) (my-sinc mode a))
                (else
                  (if (eq? mode 'complex)
                    (make-complex 0.0 0.0)
                    0.0))))))))
    (else
      (if (eq? mode 'complex)
        (make-complex 0.0 0.0)
        0.0))))

(define x-axis-lookup (make-vector 321))
(define y-axis-lookup (make-vector 321))
(define f-lookup (make-vector 321))

(define (init-f-lookup)
  (do ((i 0 (+ i 1))) ((= i 321))
    (begin
      (vector-set! x-axis-lookup i #f)
      (vector-set! y-axis-lookup i #f)
      (vector-set! f-lookup i #f))))

(define (memo-set! f i x y)
  (vector-set! f i (list x y)))

(define (memo-lookup f i)
  (vector-ref f i))

(define (evaluate-fxy f x y)
  (evaluate 'real f x y 0.0 0.0))

(define (evaluate-real f lookup x i)
  (let
    ((fx (memo-lookup lookup i)))
    (if fx
      (cadr fx)
      (let
        ((fx (evaluate 'real f x 0.0 0.0 0.0)))
        (set! fx (- fx)) ; negate f(x)
        (memo-set! lookup i x fx)
        fx))))

(define (evaluate-iterations f iterations x y cx cy)
  (if (= iterations 0)
    (make-complex x y)
    (let
      ((z (evaluate 'complex f x y cx cy)))
      (evaluate-iterations f (- iterations 1) (car z) (cadr z) cx cy))))
