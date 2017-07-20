;;;
;;; utility.scm
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
