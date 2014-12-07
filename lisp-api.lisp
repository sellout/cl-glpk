;;; Copyright (c) 2007, Kai Kaminski.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-glpk)

(cffi:define-foreign-library libglpk
  (t (:default "libglpk")))

(cffi:use-foreign-library libglpk)

;;; class definition and basic amenities (print-object etc)
(defclass linear-problem ()
  ((_problem :reader _problem)))

(defmethod print-object ((lp linear-problem) stream)
  (print-unreadable-object (lp stream :type t)
    (format stream "~a / ~a"
	    (name lp)
	    (direction lp))))

(defmethod initialize-instance :after
    ((lp linear-problem) &key name direction rows columns constraints objective &allow-other-keys)
  (let ((_problem  (glp-create-prob)))
    (setf (slot-value lp '_problem) _problem)

    (when direction
      (setf (direction lp) direction))

    (when name
      (setf (name lp) name))

    (when columns
      (setf (columns lp) columns))

    (when rows
      (setf (rows lp) rows))

    (when constraints
      (setf (constraints lp) constraints))

    (when objective
      (setf (objective lp) objective))

    (finalize lp (lambda ()
		   (glp-delete-prob _problem)))))

;;; Accessors
(defmethod name ((lp linear-problem))
  (glp-get-prob-name (_problem lp)))

(defmethod (setf name) (name (lp linear-problem))
  (glp-set-prob-name (_problem lp) name)
  name)

(autowrap:define-enum-from-constants (direction "GLP-")
  +glp-min+
  +glp-max+)

(defmethod direction ((lp linear-problem))
  (autowrap:enum-key '(:enum (direction))
                     (glp-get-obj-dir (_problem lp))))

(defmethod (setf direction) (direction (lp linear-problem))
  (glp-set-obj-dir (_problem lp)
                   (autowrap:enum-value '(:enum (direction))
                                        direction))
  direction)

(autowrap:define-foreign-enum 'var-type 0
  (list (cons :free +glp-fr+)
        (cons :lower-bound +glp-lo+)
        (cons :upper-bound +glp-up+)
        (cons :double-bounded +glp-db+)
        (cons :fixed +glp-fx+)))

(autowrap:define-foreign-alias 'var-type '(:enum (var-type)))

(defmethod (setf columns) (columns (lp linear-problem))
; This docstring is commented out, because Emacs/Slime chokes on it otherwise.
;  "If the current number of columns is 0, this function simply adds
;all the specified columns. Otherwise it asserts that
;(= (LENGTH COLUMNS) (NUMBER-OF-COLUMNS LP)) and sets names and bounds of the columns."
  (let* ((_lp (_problem lp))
	 (num-cols (glp-get-num-cols _lp))
	 (new-num-cols (length columns)))
    (if (= 0 num-cols)
	(glp-add-cols _lp new-num-cols)
	(assert (= num-cols new-num-cols)))

    (iter (for column in columns)
	  (for k from 1)
	  (destructuring-bind (name type lower-bound upper-bound)
	      column
	    (glp-set-col-name _lp k name)
	    (glp-set-col-bnds _lp k
                           (autowrap:enum-value '(:enum (var-type)) type)
			   (coerce lower-bound 'double-float)
			   (coerce upper-bound 'double-float))))
    columns))

(defmethod (setf rows) (rows (lp linear-problem))
  (let* ((_lp (_problem lp))
	 (num-rows (glp-get-num-rows _lp))
	 (new-num-rows (length rows)))
    (if (= 0 num-rows)
	(glp-add-rows _lp new-num-rows)
	(assert (= num-rows new-num-rows)))

    (iter (for row in rows)
	  (for k from 1)
	  (destructuring-bind (name type lower-bound upper-bound)
	      row
	    (glp-set-row-name _lp k name)
	    (glp-set-row-bnds _lp k
                              (autowrap:enum-value '(:enum (var-type)) type)
                              (coerce lower-bound 'double-float)
                              (coerce upper-bound 'double-float))))
    rows))

(defmethod number-of-rows ((lp linear-problem))
  (glp-get-num-rows (_problem lp)))

(defmethod number-of-columns ((lp linear-problem))
  (glp-get-num-cols (_problem lp)))

(defmethod (setf constraints) (constraints (lp linear-problem))
  (autowrap:with-many-alloc ((is :int (1+ (length constraints)))
                             (js :int (1+ (length constraints)))
                             (coefs :double (1+ (length constraints))))
    (iter (for (i j coef) in constraints)
	  (for k from 1)
	  (assert (<= i (number-of-rows lp)))
	  (assert (<= j (number-of-columns lp)))
	  (assert (/= coef 0d0))
	  (setf (autowrap:c-aref is k :int) i)
	  (setf (autowrap:c-aref js k :int) j)
	  (setf (autowrap:c-aref coefs k :double) (coerce coef 'double-float)))
    (glp-load-matrix (_problem lp) (length constraints) is js coefs)
    constraints))

(defmethod (setf objective) (objective (lp linear-problem))
  (assert (<= (length objective) (number-of-columns lp)))
  (iter (for coef in objective)
	(for k from 1)
	(glp-set-obj-coef (_problem lp) k (coerce coef 'double-float)))
  objective)


;;; Solvers

(autowrap:define-foreign-enum 'return-code 0
  (list (cons :ok 0)
        (cons :invalid-basis +glp-ebadb+)
        (cons :singular-matrix +glp-esing+)
        (cons :ill-conditioned-matrix +glp-econd+)
        (cons :invalid-bounds +glp-ebound+)
        (cons :solver-failed +glp-efail+)
        (cons :objective-lower-limit-reached +glp-eobjll+)
        (cons :objective-upper-limit-reached +glp-eobjul+)
        (cons :iteration-limit-exceeded +glp-eitlim+)
        (cons :time-limit-exceeded +glp-etmlim+)
        (cons :no-primal-feasible-solution +glp-enopfs+)
        (cons :no-dual-feasible-solution +glp-enodfs+)
        (cons :root-lp-optimum-not-provided +glp-eroot+)
        (cons :search-terminated-by-application +glp-estop+)
        (cons :relative-mip-gap-tolerance-reached +glp-emipgap+)
        (cons :no-primal/dual-feasible-solution +glp-enofeas+)
        (cons :no-convergence +glp-enocvg+)
        (cons :numerical-instability +glp-einstab+)
        (cons :invalid-data +glp-edata+)
        (cons :result-out-of-range +glp-erange+)))

(autowrap:define-foreign-alias 'return-code '(:enum (return-code)))

(defmethod simplex ((lp linear-problem))
  (autowrap:with-alloc (parm 'glp-smcp)
    (glp-init-smcp parm)
    (autowrap:enum-key '(:enum (return-code))
                       (glp-simplex (_problem lp) parm))))


;;; Query functions

(defmethod objective-value ((lp linear-problem))
  (glp-get-obj-val (_problem lp)))

(defmethod column-primal-value ((lp linear-problem) column)
  (glp-get-col-prim (_problem lp) column))

(defmethod column-dual-value ((lp linear-problem) column)
  (glp-get-col-dual (_problem lp) column))

(defmethod row-primal-value ((lp linear-problem) row)
  (glp-get-row-prim (_problem lp) row))

(defmethod row-dual-value ((lp linear-problem) row)
  (glp-get-row-dual (_problem lp) row))

;;; Utility functions
(defun array/list->constraints (constraints)
  "Takes a two-dimensional array or the corresponding nested list and
  returns a list of 3-tupels of the form (row column coefficient).

Example: '((1 9) (7 4)) => ((1 1 1) (1 2 9) (2 1 7) (2 2 4))"
  (typecase constraints
    (cons
     (array/list->constraints (make-array (list (length constraints)
						(length (car constraints)))
					  :initial-contents constraints)))

    (array
     (iter (for i from 0 below (array-dimension constraints 0))
	   (appending (iter (for j from 0 below (array-dimension constraints 1))
			    (for value = (coerce (aref constraints i j) 'double-float))
			    (when (/= 0.0d0 value)
			      (collect (list (1+ i) (1+ j) value)))))))))
