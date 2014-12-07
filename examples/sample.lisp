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

;;; This file corresponds to EXAMPLES/SAMPLE.C in the GLPK distribution.

;;; The linear problem, which is solved by the functions in this file,
;;; is described in Section 1.3 of the GLPK manual.

;;; Currently there are two implementations:

;;; sample              -- uses the low-level FFI bindings directly
;;; sample-medium-level -- uses a lispier interface (see lisp-api.lisp)

(in-package #:cl-glpk)

(defun sample ()
  "An implementation using the FFI bindings directly."
  (let ((lp (glp-create-prob)))
    (autowrap:with-many-alloc ((is :int 10)
                               (js :int 10)
                               (as :double 10)
                               (parm 'glp-smcp))

    (glp-set-prob-name lp "sample")
    (glp-set-obj-dir lp +glp-max+)

    (glp-add-rows lp 3)
    (glp-set-row-name lp 1 "p")
    (glp-set-row-bnds lp 1 +glp-up+ 0d0 100d0)
    (glp-set-row-name lp 2 "q")
    (glp-set-row-bnds lp 2 +glp-up+ 0d0 600d0)
    (glp-set-row-name lp 3 "r")
    (glp-set-row-bnds lp 3 +glp-up+ 0d0 300d0)

    (glp-add-cols lp 3)
    (glp-set-col-name lp 1 "x1")
    (glp-set-col-bnds lp 1 +glp-lo+ 0d0 0d0)
    (glp-set-obj-coef lp 1 10d0)
    (glp-set-col-name lp 2 "x2")
    (glp-set-col-bnds lp 2 +glp-lo+ 0d0 0d0)
    (glp-set-obj-coef lp 2 6d0)
    (glp-set-col-name lp 3 "x3")
    (glp-set-col-bnds lp 3 +glp-lo+ 0d0 0d0)
    (glp-set-obj-coef lp 3 4d0)

    (iter (for k from 1)
	  (for a in '(1d0 1d0 1d0 10d0 4d0 5d0 2d0 2d0 6d0))
	  (setf (autowrap:c-aref is k :int) (ceiling k 3))
	  (setf (autowrap:c-aref js k :int) (1+ (mod (1- k) 3)))
	  (setf (autowrap:c-aref as k :double) a))

    (glp-load-matrix lp 9 is js as)
    (glp-init-smcp parm)
    (glp-simplex lp parm))

    (format t "z = ~a; x1 = ~a; x2 = ~a; x3 = ~a~%"
	    (glp-get-obj-val lp)
	    (glp-get-col-prim lp 1)
	    (glp-get-col-prim lp 2)
	    (glp-get-col-prim lp 3))

    (glp-delete-prob lp)))

(defun sample-medium-level ()
  "Implemented using the Lisp API."
  (let ((lp (make-instance 'linear-problem
			   :rows '(("p" :upper-bound 0 100)
				   ("q" :upper-bound 0 600)
				   ("r" :upper-bound 0 300))
			   :columns '(("x1" :lower-bound 0 0)
				      ("x2" :lower-bound 0 0)
				      ("x3" :lower-bound 0 0))
			   :constraints '((1 1 1.0d0)
					  (1 2 1.0d0)
					  (1 3 1.0d0)
					  (2 1 10.0d0)
					  (2 2 4.0d0)
					  (2 3 5.0d0)
					  (3 1 2.0d0)
					  (3 2 2.0d0)
					  (3 3 6.0d0))
			   :objective '(10 6 4)
			   :direction :max)))
    (simplex lp)
    (format t "z = ~a, x1 = ~a, x2 = ~a, x3 = ~a~%"
	    (objective-value lp)
	    (column-primal-value lp 1)
	    (column-primal-value lp 2)
	    (column-primal-value lp 3))))

(defun sample-high-level ()
  "Implemented using the MAKE-LINEAR-PROGRAM macro."
  (let ((lp (make-linear-program
             :maximize (+ (* 10 x1) (* 6 x2) (* 4 x3))
             :subject-to ((<= (+ x1 x2 x3) 100)
                          (<= (+ (* 10 x1) (* 4 x2) (* 5 x3)) 600)
                          (<= (+ (* 2 x1) (* 2 x2) (* 6 x3)) 300))
             :bounds ((>= x1 0)
                      (>= x2 0)
                      (>= x3 0)))))
    (simplex lp)
    (format t "z = ~a, x1 = ~a, x2 = ~a, x3 = ~a~%"
	    (objective-value lp)
	    (column-primal-value lp 1)
	    (column-primal-value lp 2)
	    (column-primal-value lp 3))))
