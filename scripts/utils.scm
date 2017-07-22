;;;
;;; utils.scm
;;;
;;; Some helper procedures.
;;;

(define (truncate input-list max-length)
  (let do-truncate ((output '()) (input input-list) (remaining max-length))
    (cond
     ((or (eq? remaining 0) (null? input))
      (reverse output))
     (else
      (do-truncate (cons (car input) output) (cdr input) (- remaining 1))))))

(define (->string object)
  (define port (open-output-string))
  (write object port)
  (get-output-string port))

(define (int-list->string command)
  (define expr-len (length command))
  (define str (make-string expr-len))
  (let populate ((index (- expr-len 1)) (char-list command))
    (cond
     ((eq? index -1) str)
     (else
      (string-set! str index (integer->char (car char-list)))
      (populate (- index 1) (cdr char-list))))))

(define (string->int-list str)
  (define strlen (string-length str))
  (let populate ((index (- strlen 1)) (command '()))
    (cond
     ((eq? index -1) command)
     (else
      (populate (- index 1)
                (cons
                 (char->integer (string-ref str index))
                 command))))))

(define (del-assq term alist)
  (let remove ((left alist) (result '()))
    (cond
     ((null? left) result)
     ((eq? (caar left) term)
      (remove (cdr left) result))
     (else
      (remove (cdr left) (cons (car left) result))))))
