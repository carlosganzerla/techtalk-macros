;;;; Defining utility macros

;; How to solve the gensym issue in a practical way
(defmacro with-gensyms (syms &body body)
  "This macro will bind gensyms to SYMS"
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

(with-gensyms (x y z))

;; Running block by index
(defmacro nth-expr (n &rest exprs)
  "This macro will execute the nth 0 index expression on EXPRS"
  (if (integerp n)
      (nth n exprs)
      `(case ,n
         ,@(let ((i -1))
             (mapcar #'(lambda(x) `(,(incf i) ,x)) exprs)))))

(nth-expr 2 (print 1) (print 2) (print 3))

(let ((i 2))
  (nth-expr i (print 1) (print 2) (print 3)))

;; Repeat blocks N times
(defmacro ntimes (n &body body)
  "This macro will execute the BODY N times"
  (with-gensyms (times rec-fun)
                `(let ((,times ,n))
                   (labels ((,rec-fun (n)
                              (when (> n 0)
                                ,@body
                                (,rec-fun (- n 1)))))
                     (,rec-fun ,times)))))

(let ((x 3))
  (ntimes 6
          (incf x)
          (format t "Incrementing ~A! ~A is now ~A~%" 'x 'x x)))


;; Preserve original values
(defmacro retain (params &body body)
  "The supplied PARAMS will return to their original value after the
   evaluation of BODY"
  `((lambda ,params ,@body) ,@params))

(let ((i 3) (j 5))
  (retain (i j) 
          (print i)
          (setf i 32)
          (print i)
          (print j)
          (setf j -3)
          (print j))
  (print i)
  (print j)
  nil)


(defmacro my-double (x)
  "Doubles the value of X"
  (with-gensyms (val)
                `(let ((,val ,x))
                   (setf ,val (* ,val 2)))))

(let ((x 1) (y 3) (lst (list 3 2 3 4)))
  (my-double x)
  (my-double y)
  (my-double (car lst))
  (print x)
  (print y)
  (print lst)
  nil)
