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
  (let ((_problem  (%create-prob)))    
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
		   (%delete-prob _problem)))))

;;; Accessors
(defmethod name ((lp linear-problem))
  (%get-prob-name (_problem lp)))

(defmethod (setf name) (name (lp linear-problem))
  (%set-prob-name (_problem lp) name)
  name)

(defmethod direction ((lp linear-problem))
  (%get-obj-dir (_problem lp)))

(defmethod (setf direction) (direction (lp linear-problem))
  (%set-obj-dir (_problem lp) direction)
  direction)

(defmethod (setf columns) (columns (lp linear-problem))
; This docstring is commented out, because Emacs/Slime chokes on it otherwise.
;  "If the current number of columns is 0, this function simply adds
;all the specified columns. Otherwise it asserts that
;(= (LENGTH COLUMNS) (NUMBER-OF-COLUMNS LP)) and sets names and bounds of the columns."
  (let* ((_lp (_problem lp))
	 (num-cols (%get-num-cols _lp))
	 (new-num-cols (length columns)))
    (if (= 0 num-cols)
	(%add-cols _lp new-num-cols)
	(assert (= num-cols new-num-cols)))
    
    (iter (for column in columns)
	  (for k from 1)
	  (destructuring-bind (name type lower-bound upper-bound)
	      column
	    (%set-col-name _lp k name)
	    (%set-col-bnds _lp k type
			   (coerce lower-bound 'double-float)
			   (coerce upper-bound 'double-float))))
    columns))

(defmethod (setf rows) (rows (lp linear-problem))
  (let* ((_lp (_problem lp))
	 (num-rows (%get-num-rows _lp))
	 (new-num-rows (length rows)))
    (if (= 0 num-rows)
	(%add-rows _lp new-num-rows)
	(assert (= num-rows new-num-rows)))

    (iter (for row in rows)
	  (for k from 1)
	  (destructuring-bind (name type lower-bound upper-bound)
	      row
	    (%set-row-name _lp k name)
	    (%set-row-bnds _lp k type
			   (coerce lower-bound 'double-float)
			   (coerce upper-bound 'double-float))))
    rows))

(defmethod number-of-rows ((lp linear-problem))
  (%get-num-rows (_problem lp)))

(defmethod number-of-columns ((lp linear-problem))
  (%get-num-cols (_problem lp)))

(defmethod (setf constraints) (constraints (lp linear-problem))
  (let ((is (foreign-alloc :int :count (1+ (length constraints))))
	(js (foreign-alloc :int :count (1+ (length constraints))))
	(coefs (foreign-alloc :double :count (1+ (length constraints)))))
    (iter (for (i j coef) in constraints)
	  (for k from 1)
	  (assert (<= i (number-of-rows lp)))
	  (assert (<= j (number-of-columns lp)))
	  (assert (/= coef 0d0))
	  (setf (mem-aref is :int k) i)
	  (setf (mem-aref js :int k) j)
	  (setf (mem-aref coefs :double k) (coerce coef 'double-float)))
    (%load-matrix (_problem lp) (length constraints) is js coefs)
    constraints))

(defmethod (setf objective) (objective (lp linear-problem))
  (assert (<= (length objective) (number-of-columns lp)))
  (iter (for coef in objective)
	(for k from 1)
	(%set-obj-coef (_problem lp) k (coerce coef 'double-float)))
  objective)


;;; Solvers

(defmethod simplex ((lp linear-problem))
  (%simplex (_problem lp)))


;;; Query functions

(defmethod objective-value ((lp linear-problem))
  (%get-obj-val (_problem lp)))

(defmethod column-primal-value ((lp linear-problem) column)
  (%get-col-prim (_problem lp) column))


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
