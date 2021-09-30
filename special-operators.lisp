;; Quote

;; Error: 
; (1 x z 3 y)
'(1 x 3 y) ; Valid: atoms are not evaluated.
(quote (1 x 3 y))

;; Backquote, comma, and comma-at

`(cons ,(+ 1 3) x)
`(progn ,@(list '(print 1) '(print 2) '(print 3)))

;; Classical operators

(defun print-if-not (x)
  (if (not x)
    (format t "NOT ~A~%" 'x)))


(let ((x 3)) ; let is also a special operator
  `(setf y ,x))

(progn  ; progn executes a block of expressions
  (print 1)
  (print 2))

;; There are lots more. The point is just to show that these are the building
;; blocks of LISP programs. Everything else is macros and functions.
