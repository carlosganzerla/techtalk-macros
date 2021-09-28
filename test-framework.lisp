;;;; Example from Practical Common Lisp by Peter Seibel.

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;; Unit under test example

(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt)
      elt)) 

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))


(defun filter (fn lst)
  (remove-if (lambda (x) (not (funcall fn x))) lst))

;;; Test fixture
(deftest test-compress ()
  (check
    (equal (compress (list 1 1 3 3 5 1 1)) 
           '((2 . 1) (2 . 3) 5 (2 . 1)))
    (eql (compress nil) nil)
    (equal (compress '(1 2 3)) '(1 2 3))
    (equal (compress '(x x x x x x)) '((6 . x)))
    (equal (compress '(a a b b c c)) '((2 . a) (2 . b) (2 . c)))))


;;; Test fixture
(deftest test-filter ()
  (check
    (equal (filter #'identity (list 1 2 3)) (list 1 2 3))
    (eql (filter (lambda (x) (> x 6)) (list 1 2 3)) nil) 
    (equal (filter #'car (list '(1 3 4) (list nil 3) nil)) '((1 3 4))) 
    (equal (filter #'symbolp (list 'y '(3 x 4) 3 5 '(1 2 3))) (list 'y))))

;;; Master test fixture
(deftest test-all ()
  (combine-results
    (test-filter)
    (test-compress)))

;; Run tests
(test-all)
