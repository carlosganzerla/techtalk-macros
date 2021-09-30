;;; Built-in macros

(and (+ 1 2) :keyword 'e (print "carlo"))

(let ((x 3))
  (unless x
    (format t "~A is falsy~%" 'x)))

(or (+ 1 2) 1 :keyword 'e (print "carlo"))

(dolist (e (list 5 4 3 2 1))
  (print e))

;; I want to port JS falsy stuff to lisp: How to do it?
(defun is-js-truthy (val)
  (not (or (not val)
           (eql "" val)
           (eql 0 val))))

;; If we didn't have macros, a good way to do it is a HOF
(defun js-if-fn (condition if-fn &optional (else-fn (lambda () nil)))
  (if (is-js-truthy condition)
      (funcall if-fn)
      (funcall else-fn)))

;; Unfortunately, it kinda sucks having to keep wrapping stuff on functions
(js-if-fn (print 1)
          (lambda () (print "aint 0"))
          (lambda () 'zero))

(js-if-fn (print 0)
          (lambda () (print "aint 0"))
          (lambda () 'zero))


;; Tip to macro design: picture the expansion
;; Here we are extending the syntax in terms of a known function:
(defmacro js-if (condition if-form else-form)
  `(if (is-js-truthy ,condition)
      ,if-form
      ,else-form))


(js-if (print 1)
       (print "aint 0")
       'zero)

(js-if (print 0)
       (print "aint 0")
       'zero)
