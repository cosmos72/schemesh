;; usage: (source-file-descriptor* path length crc)
(define source-file-descriptor* (record-constructor (record-rtd (source-file-descriptor "" 0))))
      
;; (define sfd (source-file-descriptor* "/dev/tty" 0 0))
(define sfd (make-source-file-descriptor "/dev/tty" (open-bytevector-input-port #vu8())))

(define esyntax (make-annotation 'syntax-case (make-source-object sfd 1 12 1 2) 'syntax-case)) ; chars [1, 12), line 1, column 2

(define earg (make-annotation 'arg (make-source-object sfd 13 16 1 14) 'arg)) ; chars [13, 16), line 1, column 14

(define esyntax-arg (make-annotation (list esyntax earg) (make-source-object sfd 0 17 1 1) '(syntax-case arg))) ; chars [0, 17), line 1, column 1

(compile esyntax-arg)
; raises condition:
; Exception: invalid syntax (syntax-case arg) at line 1, char 1 of /dev/tty

