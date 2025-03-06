(define fd-read-u8
  (let ((bv (make-bytevector 1)))
    (lambda (fd)
      (let ((n (fd-read fd bv 0 1)))
        (if (and (fixnum? n) (fx>? n 0))
          (bytevector-u8-ref bv 0)
          (eof-object))))))

(define (loop-fd-read-u8 fd)
  (do ((b (fd-read-u8 fd) (fd-read-u8 fd)))
      ((eof-object? b))
    (format (console-output-port) "read one byte: ~s\n" b)
    (flush-output-port (console-output-port))))

(define (loop-port-read-u8 binary-input-port)
  (do ((b (get-u8 binary-input-port) (get-u8 binary-input-port)))
      ((eof-object? b))
    (format (console-output-port) "read one byte: ~s\n" b)
    (flush-output-port (console-output-port))))

(define (loop-port-read-char textual-input-port)
  (do ((ch (get-char textual-input-port) (get-char textual-input-port)))
      ((eof-object? ch))
    (format (console-output-port) "read one char: ~s\n" ch)
    (flush-output-port (console-output-port))))

(define j1 {{echo0 abc; sleep 1; echo def; sleep 1} | $(loop-fd-read-u8 (sh-fd 0))})
(define j2 {{echo0 abc; sleep 1; echo def; sleep 1} | $(loop-port-read-u8 (sh-stdin))})
(define j3 {{echo0 abc; sleep 1; echo def; sleep 1} | $(loop-port-read-char (current-input-port))})

#;(sh-run/i j1)
#;(sh-run/i j2)
#;(sh-run/i j3)

#;(sh-run j1)
#;(sh-run j2)
#;(sh-run j3)
