;;;; Macro design issues examples

;; Unintended variable capture
(defmacro ntimes-bad (n &rest body)
  `(do ((x 0 (+ x 1)))
       ((>= x ,n))
       ,@body))

(let ((x 10))
  (ntimes-bad 5 (setf x (+ x 1)))
  x)

;; Multiple Evaluation
(defmacro ntimes-bad-2 (n &rest body)
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
         ((>= ,g ,n))
         ,@body)))

(let ((v 10))
  (ntimes-bad-2 (setf v (- v 1))
    (princ ".")))
