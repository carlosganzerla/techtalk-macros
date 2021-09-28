;;;; Basic Lisp syntax

;;; Atoms
1
3
key
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
(defun print-dots-rec (x)
  (if (> x 0)
      (progn
        (format t ".")
        (print-dots-rec (- x 1)))
      nil))

(defun print-dots-iter (len)
  (do ((i 0 (+ i 1)))
      ((>= i len) nil)
      (format t ".")))

(defun ocurrences-rec (lst sym count)
  (if (null lst)
      count
      (ocurrences-rec (cdr lst) sym (if (eql (car lst) sym)
                                        (+ count 1)
                                        count))))
(defun occurrences-iter (lst sym)
  (let ((cnt 0))
    (dolist (e lst)
      (if (eql e sym)
          (setf cnt (+ cnt 1))
          nil))
    cnt))
