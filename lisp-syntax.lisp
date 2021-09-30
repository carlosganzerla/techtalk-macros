;;;; Basic Lisp syntax

;;; Atoms
1
3
;; Error on evaluating just this: key
:key

(defparameter *foo* "im a variable")
*foo*

;;; Lists
(list 1 3 *foo* :key)

(+ 3 4 2 1 5) ; Calling function
(+ 1)
(+)

(let ((lst (list 1 2 3 4 5)))
  (print (car lst))
  (print (cdr lst))
  (print (cadr lst))
  (setf (car lst) "setting the head")
  (print lst)
  (setf lst "bulbassauro")
  (print lst)
  nil)

(list 1 2 3 (list 3 4 5 6))

;;; Functions
(defun say-hi (name)
  (format t "HI ~A~%" name))

(defun print-dots-iter (len)
  (do ((i 0 (+ i 1)))
      ((>= i len) len)
      (format t ".")))
