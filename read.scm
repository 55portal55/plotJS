; my version of a scheme reader adapted to read from a list of characters

(define read-buffer '())

(define (eof-objectx? ch)
  (if (char? ch)
    (= (char->integer ch) 0)
    #f))

(define (errorx x y)
'())

(define (read-charx)
  (if (null? read-buffer)
    (integer->char 0)
    (let
      ((ch (car read-buffer)))
      (set! read-buffer (cdr read-buffer))
      ch)))

(define (read-char-portx port)
  (if (null? read-buffer)
    (integer->char 0)
    (let
      ((ch (car read-buffer)))
      (set! read-buffer (cdr read-buffer))
      ch)))
      
(define readx
  (lambda args

    (letrec (

      (port
        (if (null? args)
          'stdin
          (car args)))

      (token-type '())

      (ch #\space)

      (previous-ch #\space)

      (TAB-CHAR (integer->char 9))

      (CARRIAGE-RETURN (integer->char 13))

      (NEW-LINE (integer->char 10))

      (white-space (list #\space NEW-LINE TAB-CHAR CARRIAGE-RETURN))

      (readin
        (lambda ()
        (letrec (

          (get-token
            (lambda ()

              (letrec (

                (token '())
                (token-list '())
                (token-new '())
                (token-tail '())

                (char-alpha-numeric?
                  (lambda ()
                    (or (char-alphabetic? ch) (char-numeric? ch))))

                (skip-past-alpha
                  (lambda ()
                    (let loop ()
                      (if (char-alpha-numeric?)
                        (begin
                          (get-ch)
                          (skip-past-alpha)
                          (loop))))))

                (get-ch
                  (lambda ()
                    (if (not (eq? port 'stdin))
                      (set! ch (read-char-portx port))
                      (set! ch (read-charx)))
                    (if (memq ch white-space)
                      (set! ch #\space))))

                (get-ch-no-newline
                  (lambda ()
                    (if (not (eq? port 'stdin))
                      (set! ch (read-char-portx port))
                      (set! ch (read-charx)))))
    
                (skip-line
                  (lambda ()
                    (if (or (char=? ch NEW-LINE) (char=? ch CARRIAGE-RETURN))
                      (get-ch)
                      (begin
                        (get-ch-no-newline)
                        (skip-line)))))
    
                (skip-blanks
                  (lambda ()
                    (cond
                      ((eof-objectx? ch))
                      ((char=? ch #\;)
                        (skip-line)
                        (skip-blanks))
                      ((memq ch white-space)
                        (get-ch)
                        (skip-blanks))
                      (else
                        '()))))

                (replace-last-ch
                  (lambda ()
                    (set-car! token-tail ch)
                    (set! previous-ch ch)
                    (get-ch)))

                (append-to-list
                  (lambda ()
                    (set! token-new (list ch))
                    (if (not (null? token-list))
                      (begin
                        (set-cdr! token-tail token-new)
                        (set! token-tail (cdr token-tail)))
                      (begin
                        (set! token-list token-new)
                        (set! token-tail token-new)))
                    (set! previous-ch ch)
                    (get-ch)))

                (get-vector
                  (lambda ()
                    (get-ch)
                    (if (member ch '(#\t #\f)) ; boolean true or false
                      (begin
                        (set! token-type 'BOOLEAN-TOK)
                        (if (char=? ch #\t)
                          (set! token #t)
                          (set! token #f))
                        (get-ch))
                      (if (char=? ch #\\)
                        (begin
                          (set! token-type 'CHAR-TOK)
                          (get-ch)
                          (set! token ch)
                          (let
                            ((first-ch ch))
                            (get-ch)
                            (case first-ch
                              ((#\s)
                                (if (char=? ch #\p)
                                  (begin
                                    (set! token #\space)
                                    (skip-past-alpha))))
                              ((#\n)
                                (if (char=? ch #\e)
                                  (begin
                                    (set! token NEW-LINE)
                                    (skip-past-alpha)))))))
                        (set! token-type 'VECTOR-TOK)))))

                (get-string
                  (lambda ()
                    (get-ch) ; after open double quote
                    (let loop ()
                      (if (not (or (char=? ch #\") (eof-objectx? ch)))
                        (begin
                          (append-to-list)
                          (if (char=? previous-ch #\\) ; escape char
                            (replace-last-ch))
                          (loop))))
                    (get-ch) ; after close double quote
                    (set! token-type 'STRING-TOK)
                    (set! token (list->string token-list))))

                (get-symbol
                  (lambda ()
                    (set! token-type 'SYMBOL-TOK)
                    (let loop ()
                      (if (not (or (memq ch white-space)
                                   (memq ch '(#\( #\)))
                                   (eof-objectx? ch)))
                        (begin
                          (append-to-list)
                          (loop))))
                    (set! token (list->string token-list))
                    (if (or (char-numeric? (string-ref token 0))
                            (and (>= (string-length token) 2)
                                 (char=? (string-ref token 0) #\-)
                                 (char-numeric? (string-ref token 1))))
                      (set! token (string->number token))
                      (set! token (string->symbol token))))))

                (skip-blanks)
                (if (eof-objectx? ch)
                  (begin
                    (set! token-type 'EOF-TOK)
                    (set! token ch))
                  (let
                    ((match (assq ch '(
                       (#\( OPAR-TOK) (#\) CPAR-TOK) (#\. DOT-TOK)
                       (#\' TIC-TOK) (#\# VECTOR-TOK) (#\" STRING-TOK)))))
                    (if match
                      (cond
                        ((member (cadr match)
                          '(OPAR-TOK CPAR-TOK DOT-TOK TIC-TOK))
                          (set! token-type (cadr match))
                          (set! token ch)
                          (get-ch))
                        ((eq? (cadr match) 'VECTOR-TOK)
                          (get-vector))
                        ((eq? (cadr match) 'STRING-TOK)
                          (get-string)))
                      (get-symbol))))
; (display "get-token: ") (write token) (newline)
                token)))

          (exit-bad-sexpr
            (lambda (sexpr)
              (errorx "bad sexpr" sexpr)))

          (read-dot-token
            (lambda ()
              (let
                ((token (get-token)))
                (if (not (or (eq? token-type 'SYMBOL-TOK)
                             (eq? token-type 'STRING-TOK)))
                  (exit-bad-sexpr token))
                (get-token)
                (if (not (eq? token-type 'CPAR-TOK))
                  (exit-bad-sexpr "expecting close paren"))
                token)))

          (read-list
            (lambda ()
              (let
                ((lst '())
                 (tail '())
                 (new '()))
                (let loop ((token (get-token)))
                  (if (not (or (eq? token-type 'CPAR-TOK)
                               (eq? token-type 'EOF-TOK)))
                    (begin
                      (cond
                        ((eq? token-type 'OPAR-TOK)
                          (set! new (list (read-list))))
                        ((eq? token-type 'DOT-TOK)
                          (set-cdr! tail (read-dot-token))
                          (set! token-type 'WAS-DOT-TOK))
                        ((eq? token-type 'EOF-TOK)
                          (exit-bad-sexpr "unexpected end of file"))
                        ((eq? token-type 'TIC-TOK)
                          (set! new (list (cons 'quote (list (readin))))))
                        ((eq? token-type 'VECTOR-TOK)
                          (set! new (list (read-vector))))
                        (else
                          (set! new (list token))))
                      (if (not (eq? token-type 'WAS-DOT-TOK))
                        (begin
                          (if (not (null? lst))
                            (begin
                              (set-cdr! tail new)
                              (set! tail (cdr tail)))
                            (begin
                              (set! lst new)
                              (set! tail new)))
                          (loop (get-token)))
                        (begin
                          (set! token-type 'CPAR-TOK))))))
                lst)))

          (read-vector
            (lambda ()
              (get-token)
              (if (eq? token-type 'OPAR-TOK)
                (list->vector (read-list))
                (exit-bad-sexpr "expecting open paren")))))

        (let
          ((token (get-token)))
          (cond
            ((eq? token-type 'OPAR-TOK)
              (read-list))
            ((eq? token-type 'TIC-TOK)
              (cons 'quote (list (readin))))
            ((eq? token-type 'VECTOR-TOK)
              (read-vector))
            ((eq? token-type 'CPAR-TOK)
              (exit-bad-sexpr "unexpected close paren"))
            (else
              token)))))))
  (readin))))

; test harness

;(define (loop)
;  (let
;    ((sexpr (readx)))
;    (if (not (eof-objectx? sexpr))
;      (begin
;        (write sexpr)
;        (newline)
;        (loop)))))
;  
;(loop)
