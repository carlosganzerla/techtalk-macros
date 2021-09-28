;;; Built-in macros

(or (+ 1 2) 1 :keyword 'e (print "carlo"))

(and (+ 1 2) :keyword 'e (print "carlo"))

(dolist (e (list 5 4 3 2 1))
  (print e))

(let ((x 3))
  (unless x
    (format t "~A is falsy~%" 'x)))



;;; Creating our own macros
(defmacro my-if (condition if-form else-form)
  `(cond (,condition ,if-form)
         (t ,else-form)))

;; Tip to macro design: picture the expansion
(or (and 1 2)
    (and 1 3)
    (and 1 4)
    (and 2 3)
    (and 2 4)
    (and 3 4))

(defun combine-2 (lst fn)
  (let ((result nil))
    (do* ((iter lst (cdr iter))
          (head (car lst) (car iter)))
      ((not iter) (nreverse result))
      (dolist (i (cdr iter))
        (setf result (cons (funcall fn head i) result))))))

(defmacro at-least-2 (form1 form2 &rest forms)
  (let ((lst (append (list form1 form2) forms)))
    `(or ,@(combine-2 lst (lambda (e1 e2)
                            `(and ,e1 ,e2))))))

(defun at-least-2-fn (form1 form2 &rest forms)
  (let ((lst (append (list form1 form2) forms)))
    (some #'identity (combine-2 lst (lambda (e1 e2)
                                      (and e1 e2))))))

(let ((x 999))
 (at-least-2 1 x 3 (print "hello good old friend!")))

(at-least-2 1 nil nil (print "hello good old friend!"))

(at-least-2-fn 1 2 3 (print "hello good old friend!"))
