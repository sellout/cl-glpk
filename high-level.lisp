(in-package :cl-glpk)

;;; This high-level interface provides a macro that can be used as follows
;;;
;;;     (make-linear-program
;;;      :maximize (+ (* (- 12 2) x1) (* 6 x2) (* 4 x3))
;;;      :subject-to ((<= (+ x1 x2 x3) 100)
;;;                   (<= (+ (* (+ 2 2) x2) (* 10 x1) (* 5 x3)) (+ 200 400))
;;;                   (<= (+ (* 2 x1) (* 2 x2) (* 6 x3)) 300))
;;;      :bounds ((>= x3 10)))
;;;
;;; Features:
;;; – coefficients & bounds can be arbitrary lisp forms
;;; - terms in constraints don't have to be in same order as in the objective
;;;   function
;;; - (+ (* 4 x)) can be abbreviated as (* 4 x)
;;; - (* 1 x) can be abbreviated as x
;;; - not all variables need to occur in every constraint
;;; - variables can be left out of bounds list, indicating free variables
;;; - instead of :LOWER and :UPPER, use (<= lb (+ (* 4 x) ...) ub) [if there is
;;;   only one bound, you must have the variable before the bound. EG,
;;;   (>= x lb), not (<= lb x)?]
;;;
;;; TODO:
;;; - integrate bounds with constraints (either discover that it's just as fast
;;;   to combine them, or add code to separate out bounds before expansion)

(defun standardize-equation (form)
  "Converts all equations [(+ (* 4 x) (* 7 y)), x, (* 4 x), etc.] into the form
   ((4 x) (7 y)) to make our processing simpler."
  (mapcar (lambda (term)
            (if (listp term)
                (cdr term)
                (list 1 term)))
          (if (listp form)
              (cdr form)            ; get rid of the +
              (list form))))

(defun get-specified-bounds (lower upper)
  (if lower
      (if upper
          (if (eq upper lower) +glp-fx+ +glp-db+)
          +glp-lo+)
      (if upper
          +glp-up+
          +glp-fr+)))

(defun get-bounds (list)
  (mapcar (lambda (constraint)
            (let ((comparator (car constraint)))
              (ecase (length constraint)
                (3 (ecase comparator
                     (= `(list ',(second constraint)
                               ,(third constraint)
                               ,(third constraint)))
                     (<= `(list ',(second constraint) nil ,(third constraint)))
                     (>= `(list ',(second constraint) ,(third constraint) nil))
                     ((< > /=) (error "Invalid comparator"))))
                (4 (ecase comparator
                     (<= `(list ',(third constraint)
                                ,(second constraint)
                                ,(fourth constraint)))
                     (>= `(list ',(third constraint)
                                ,(fourth constraint)
                                ,(second constraint)))
                     ((< > = /=) (error "Invalid comparator")))))))
          list))

;;; FIXME: this is pretty experimental & exploratory, so there's variable
;;;        capture, multiple expansion, … need to fix all that.
(defmacro make-linear-program
    (direction objective-function &key subject-to bounds)
  `(let* ((variables ',(mapcar #'second
                               (standardize-equation objective-function)))
          (constraint-bounds (list ,@(get-bounds subject-to)))
          (bounds-bounds (list ,@(get-bounds bounds)))
          (constraint-coefficients
           (list ,@(mapcar (lambda (constraint)
                             `(list ,@(mapcar #'first
                                              (standardize-equation (second constraint)))))
                           subject-to))))
     (make-instance
      'glpk:linear-problem
      :rows (mapcar (lambda (constraint)
                      (destructuring-bind (form lower upper) constraint
                        (declare (ignore form))
                        (list (string (gensym))
                              (get-specified-bounds lower upper)
                              (or lower 0)
                              (or upper 0))))
                    constraint-bounds)
      :columns (mapcar (lambda (var)
                         (let ((binding (assoc var bounds-bounds)))
                           (if binding
                               (destructuring-bind (name lower upper) binding
                                 (list (string name)
                                       (get-specified-bounds lower upper)
                                       (or lower 0)
                                       (or upper 0)))
                               (list (string var) +glp-fr+ 0 0))))
                       variables)
      :constraints (loop for constraint in constraint-bounds
                      for row from 0
                      appending (loop for product in (standardize-equation (car constraint))
                                   for col from 0
                                   collecting  (list (1+ row)
                                                     (progn
                                                       (format nil "~a - ~a - ~a~%"
                                                               (second product)
                                                               (position (second product) variables)
                                                               variables)
                                                       (1+ (position (second product) variables)))
                                                     (elt (elt constraint-coefficients row)
                                                          col))))
      :objective (list ,@(mapcar #'first
                                 (standardize-equation objective-function)))
      :direction (if (eq ,direction :maximize) +glp-max+ +glp-min+))))
