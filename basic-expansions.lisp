;; Built-in macros

(or (+ 1 2) 1 :keyword 'e (print "carlo"))

(and (+ 1 2) :keyword 'e (print "carlo"))

(defun print-if-not (x)
  (unless x
    (format t "NOT ~A~%" 'x)))


(dolist (e (list 5 4 3 2 1))
  (print e))



;; Creating our own macros

(at-least-2 1 2 3 (print "hello good old friend!"))

(at-least-2 1 nil nil (print "hello good old friend!"))

(at-least-2-fn 1 2 3 (print "hello good old friend!"))

(or (and 1 2)
    (and 1 3)
    (and 2 3))

(dolist (e1 lst)
  (dolist (e2 (cdr lst))
    (setf res (cons `(and ,e1 ,e2) res))
    (setf lst (cdr lst))))
`(or ,@res)  

(defmacro at-least-2 (form1 form2 &rest forms)
  (let ((lst (append (list form1 form2) forms)))
    `(or ,@(combine-2 lst (lambda (e1 e2)
                            `(and ,e1 ,e2))))))

(defun combine-2 (lst fn)
  (let ((result nil))
    (do* ((iter lst (cdr iter))
          (head (car lst) (car iter)))
      ((not iter) (nreverse result))
      (dolist (i (cdr iter))
        (setf result (cons (funcall fn head i) result))))))

(defun at-least-2-fn (form1 form2 &rest forms)
  (let ((lst (append (list form1 form2) forms)))
    (some #'identity (combine-2 lst (lambda (e1 e2)
                                      (and e1 e2))))))

