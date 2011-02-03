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
;;; - (* 1 x) can be abbreviated as x in constraints
;;; - not all variables need to occur in every constraint
;;; - variables can be left out of bounds list, indicating free variables
;;; - instead of :LOWER and :UPPER, use (<= lb (+ (* 4 x) ...) ub) [if there is
;;;   only one bound, you must have the variable before the bound. EG,
;;;   (>= x lb), not (<= lb x)?]
;;; 
;;; TODO:
;;; - abbreviate (* 1 x) as x in objective function
;;; - integrate bounds with constraints (either discover that it's just as fast
;;;   to combine them, or add code to separate out bounds before expansion)
;;; - abbreviate (+ (* 4 x)) as (* 4 x)

(defun get-specified-bounds (lower upper)
  (if lower
      (if upper
          (if (eq upper lower) :fixed :double-bound)
          :lower-bound)
      (if upper
          :upper-bound
          :free)))

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
  `(let* ((variables (mapcar #'third (cdr ',objective-function)))
          (constraint-bounds (list ,@(get-bounds subject-to)))
          (bounds-bounds (list ,@(get-bounds bounds)))
          (constraint-coefficients
           (list ,@(mapcar (lambda (constraint)
                             `(list ,@(mapcar (lambda (product)
                                                (typecase product
                                                  (list (second product))
                                                  (t 1)))
                                              (cdadr constraint))))
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
                               (list (string var) :free 0 0))))
                       variables)
      :constraints (loop for constraint in constraint-bounds
                      for row from 0
                      appending (loop for product in (cdar constraint)
                                   for col from 0
                                   collecting (typecase product
                                                (symbol (list (1+ row)
                                                              (1+ (position product variables))
                                                              1))
                                                (cons (list (1+ row)
                                                            (1+ (position (third product) variables))
                                                            (elt (elt constraint-coefficients row)
                                                                 col))))))
      :objective (list ,@(mapcar #'second (cdr objective-function)))
      :direction (if (eq ,direction :maximize) :max :min))))
