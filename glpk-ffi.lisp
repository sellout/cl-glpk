;;; Copyright (c) 2002-2007, Kai Kaminski.  All rights reserved.

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

(in-package #:cl-glpk)

(defcenum problem-class
  (:lp 100)      ;; linear programming
  (:mip 101))    ;; mixed integer programming

(defcenum optimization-direction-flag
  (:min 120)
  (:max 121))

;; type of auxiliary/structural variable
(defcenum variable-type
  (:free 110)           ;; /* free variable */
  (:lower-bound 111)    ;; /* variable with lower bound */	
  (:upper-bound 112)	;; /* variable with upper bound */	
  (:double-bound 113)	;; /* double-bounded variable */	
  (:fixed 114))		;; /* fixed variable */

(defcenum basis-status
  (:undefined 130)
  (:valid 131))

(defcenum primal-basic-solution-status
  (:undefined 132)
  :feasible
  :infeasible
  :no-feasible-solution-exists)

(defcenum dual-basic-solution-status
  (:undefined 136)
  :feasible
  :infeasible
  :no-feasible-solution-exists)

(defcenum variable-status
  (:basic 140)
  :non-basic-on-lower-bound
  :non-basic-on-upper-bound
  :non-basic-free
  :non-basic-fixed)

(defcenum interior-point-solution-status
  (:undefined 150)
  :optimal)

(defcenum structural-variable-kind
  (:continuous 160)
  :integer)

(defcenum integer-solution-status
  (:undefined 170)
  :optimal
  :feasible
  :no-integer-solution-exists)

(defcenum lpx-status
  (:optimal 180)
  :feasible
  :infeasible
  :no-feasible
  :unbounded
  :undefined)

(defcenum solver-exit-code
  (:ok 200)
  :empty
  :bad-basis
  :infeasible
  :fault
  :objective-lower-limit-reached
  :objective-upper-limit-reached
  :iteration-limit-exhausted
  :time-limit-exhausted
  :no-feasible-solution
  :numerical-instability
  :sing
  :no-convergence
  :no-primal-feasible-solution
  :no-dual-feasible-solution)


;; todo: lispify names; in particular, use longer names
(defcenum control-parameter
  (:msglev 300) ;; /* lp->msg_lev */
  (:scale 301)	;; /* lp->scale */
  (:dual 302)	;; /* lp->dual */
  (:price 303)	;; /* lp->price */
  (:relax 304)	;; /* lp->relax */
  (:tolbnd 305) ;; /* lp->tol_bnd */
  (:toldj 306)	;; /* lp->tol_dj */
  (:tolpiv 307) ;; /* lp->tol_piv */
  (:round 308)	;; /* lp->round */
  (:objll 309)	;; /* lp->obj_ll */
  (:objul 310)	;; /* lp->obj_ul */
  (:itlim 311)	;; /* lp->it_lim */
  (:itcnt 312)	;; /* lp->it_cnt */
  (:tmlim 313)	;; /* lp->tm_lim */
  (:outfrq 314) ;; /* lp->out_frq */
  (:outdly 315) ;; /* lp->out_dly */
  (:branch 316) ;; /* lp->branch */
  (:btrack 317) ;; /* lp->btrack */
  (:tolint 318) ;; /* lp->tol_int */
  (:tolobj 319) ;; /* lp->tol_obj */
  (:mpsinfo 320) ;; /* lp->mps_info */
  (:mpsobj 321)	 ;; /* lp->mps_obj */
  (:mpsorig 322) ;; /* lp->mps_orig */
  (:mpswide 323) ;; /* lp->mps_wide */
  (:mpsfree 324) ;; /* lp->mps_free */
  (:mpsskip 325) ;; /* lp->mps_skip */
  (:lptorig 326) ;; /* lp->lpt_orig */
  (:presol 327)	 ;; /* lp->presol */
  (:binarize 328) ;; /* lp->binarize */
  (:usecuts 329)) ;; /* lp->use_cuts */

(defcenum cut-types
  (:cover     #x01)  ;/* mixed cover cuts */
  (:clique    #x02)  ;/* clique cuts */
  (:gomory    #x04)  ;/* Gomory's mixed integer cuts */
  (:all       #xff)) ; /* all cuts */

(defcfun ("_glp_lpx_create_prob" %create-prob) :pointer)
;; /* create problem object */

(defcfun ("_glp_lpx_set_prob_name" %set-prob-name)
    :void
  (problem :pointer)
  (name :string))
;;/* assign (change) problem name */

(defcfun ("_glp_lpx_set_obj_name" %set-obj-name)
    :void
  (problem :pointer)
  (name :string))
;; /* assign (change) objective function name */

(defcfun ("_glp_lpx_set_obj_dir" %set-obj-dir)
    :void
  (problem :pointer)
  (direction optimization-direction-flag))
;;/* set (change) optimization direction flag */

(defcfun ("_glp_lpx_add_rows" %add-rows)
    :int
  (problem :pointer)
  (new-rows :int))
;;/* add new rows to problem object */

(defcfun ("_glp_lpx_add_cols" %add-cols)
    :int
  (problem :pointer)
  (new-columns :int))
;;/* add new columns to problem object */

(defcfun ("_glp_lpx_set_row_name" %set-row-name)
    :void
  (problem :pointer)
  (index :int)
  (name :string))
;;/* assign (change) row name */

(defcfun ("_glp_lpx_set_col_name" %set-col-name)
    :void
  (problem :pointer)
  (index :int)
  (name :string))
;;/* assign (change) column name */

(defcfun ("_glp_lpx_set_row_bnds" %set-row-bnds)
    :void
  (problem :pointer)
  (index :int)
  (type variable-type)
  (lower-bound :double)
  (upper-bound :double))
;;/* set (change) row bounds */

(defcfun ("_glp_lpx_set_col_bnds" %set-col-bnds)
    :void
  (problem :pointer)
  (index :int)
  (type variable-type)
  (lower-bound :double)
  (upper-bound :double))
;; /* set (change) column bounds */

(defcfun ("_glp_lpx_set_obj_coef" %set-obj-coef)
    :void
  (problem :pointer)
  (index :int)
  (coefficient :double))
;; /* set (change) obj. coefficient or constant term */

(defcfun ("_glp_lpx_set_mat_row" %set-mat-row)
    :void
  (problem :pointer)
  (row-index :int)
  (length :int)
  (column-indices :pointer) ;; int array
  (values :pointer)) ;; double array
;; set (replace) row of the constraint matrix

(defcfun ("_glp_lpx_set_mat_col" %set-mat-col)
    :void
  (problem :pointer)
  (column-index :int)
  (length :int)
  (row-indices :pointer) ;; int array
  (values :pointer)) ;; double array
;; /* set (replace) column of the constraint matrix */


(defcfun ("_glp_lpx_load_matrix" %load-matrix)
    :void
  (problem :pointer)
  (number-of-entries :int)
  (rows :pointer)
  (columns :pointer)
  (values :pointer))
;; void lpx_load_matrix(LPX *lp, int ne, int ia[], int ja[], double ar[]);
;; /* load (replace) the whole constraint matrix */

(defcfun ("_glp_lpx_del_rows" %del-rows)
    :void
  (problem :pointer)
  (number-of-rows :int)
  (rows :pointer))
;; /* delete specified rows from problem object */

(defcfun ("_glp_lpx_del_cols" %del-cols)
    :void
  (problem :pointer)
  (number-of-columns :int)
  (columns :pointer))
;; /* delete specified columns from problem object */

(defcfun ("_glp_lpx_delete_prob" %delete-prob)
    :void
  (problem :pointer))
;; /* delete problem object */

(defcfun ("_glp_lpx_get_prob_name" %get-prob-name)
    :string
  (problem :pointer))
;; /* retrieve problem name */

(defcfun ("_glp_lpx_get_obj_name" %get-obj-name)
    :string
  (problem :pointer))
;; /* retrieve objective function name */

(defcfun ("_glp_lpx_get_obj_dir" %get-obj-dir)
    optimization-direction-flag
  (problem :pointer))
;;/* retrieve optimization direction flag */

(defcfun ("_glp_lpx_get_num_rows" %get-num-rows)
    :int
  (problem :pointer))
;;/* retrieve number of rows */

(defcfun ("_glp_lpx_get_num_cols" %get-num-cols)
    :int
  (problem :pointer))
;;/* retrieve number of columns */

(defcfun ("_glp_lpx_get_row_name" %get-row-name)
    :string
  (problem :pointer)
  (index :int))
;;/* retrieve row name */

(defcfun ("_glp_lpx_get_col_name" %get-col-name)
    :string
  (problem :pointer)
  (index :int))
;;/* retrieve column name */

(defcfun ("_glp_lpx_get_row_type" %get-row-type)
    variable-type
  (problem :pointer)
  (index :int))
;;/* retrieve row type */

(defcfun ("_glp_lpx_get_row_lb" %get-row-lb)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row lower bound */

(defcfun ("_glp_lpx_get_row_ub" %get-row-ub)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row upper bound */

#|
(defcfun ("_glp_lpx_get_row_bnds" %get-row-bnds))   /* obsolete */
void lpx_get_row_bnds(LPX *lp, int i, int *typx, double *lb,
      double *ub);
/* obtain row bounds */
|#

(defcfun ("_glp_lpx_get_col_type" %get-col-type)
    variable-type
  (problem :pointer)
  (index :int))
;; /* retrieve column type */

(defcfun ("_glp_lpx_get_col_lb" %get-col-lb)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column lower bound */

(defcfun ("_glp_lpx_get_col_ub" %get-col-ub)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column upper bound */

#|
(defcfun ("_glp_lpx_get_col_bnds" %get-col-bnds))   /* obsolete */
void lpx_get_col_bnds(LPX *lp, int j, int *typx, double *lb,
      double *ub);
/* obtain column bounds */
|#

(defcfun ("_glp_lpx_get_obj_coef" %get-obj-coef)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve obj. coefficient or constant term */

(defcfun ("_glp_lpx_get_num_nz" %get-num-nz)
    :int
  (problem :pointer))
;;/* retrieve number of constraint coefficients */

(defcfun ("_glp_lpx_get_mat_row" %get-mat-row)
    :int
  (problem :pointer)
  (index :int)
  (indices :pointer)  ; array of int
  (values :pointer))  ; array of double
;;/* retrieve row of the constraint matrix */

(defcfun ("_glp_lpx_get_mat_col" %get-mat-col)
    :int
  (problem :pointer)
  (index :int)
  (indices :pointer) ; array of int
  (values :double))  ; array of double
;;/* retrieve column of the constraint matrix */

(defcfun ("_glp_lpx_create_index" %create-index)
    :void
  (problem :pointer))

(defcfun ("_glp_lpx_find_row" %find-row)
    :int
  (problem :pointer)
  (name :string))

(defcfun ("_glp_lpx_find_col" %find-col)
    :int
  (problem :pointer)
  (name :string))

(defcfun ("_glp_lpx_delete_index" %delete-index)
    :void
  (problem :pointer))

(defcfun ("_glp_lpx_scale_prob" %scale-prob)
    :void
  (problem :pointer))
;;/* scale problem data */

(defcfun ("_glp_lpx_unscale_prob" %unscale-prob)
    :void
  (problem :pointer))
;;/* unscale problem data */

(defcfun ("_glp_lpx_std_basis" %std-basis)
    :void
  (problem :pointer))
;;/* construct standard initial LP basis */

(defcfun ("_glp_lpx_adv_basis" %adv-basis)
    :void
  (problem :pointer))
;;/* construct advanced initial LP basis */

(defcfun ("_glp_lpx_cpx_basis" %cpx-basis)
    :void
  (problem :pointer))
;;/* construct Bixby's initial LP basis */

(defcfun ("_glp_lpx_set_row_stat" %set-row-stat)
    :void
  (problem :pointer)
  (index :int)
  (status variable-status))

(defcfun ("_glp_lpx_set_col_stat" %set-col-stat)
    :void
  (problem :pointer)
  (index :int)
  (status variable-status))

(defcfun ("_glp_lpx_simplex" %simplex)
    solver-exit-code
  (problem :pointer))

(defcfun ("_glp_lpx_exact" %exact)
    solver-exit-code
  (problem :pointer))
;;/* easy-to-use driver to the exact simplex method */

(defcfun ("_glp_lpx_get_status" %get-status)
    lpx-status
  (problem :pointer))
;;/* retrieve generic status of basic solution */

(defcfun ("_glp_lpx_get_prim_stat" %get-prim-stat)
    primal-basic-solution-status
  (problem :pointer))
;;/* retrieve primal status of basic solution */

(defcfun ("_glp_lpx_get_dual_stat" %get-dual-stat)
    dual-basic-solution-status
  (problem :pointer))
;;/* retrieve dual status of basic solution */

(defcfun ("_glp_lpx_get_obj_val" %get-obj-val)
    :double
  (problem :pointer))
;; /* retrieve objective value (basic solution) */

(defcfun ("_glp_lpx_get_row_stat" %get-row-stat)
    variable-status
  (problem :pointer)
  (index :int))
;;/* retrieve row status (basic solution) */

(defcfun ("_glp_lpx_get_row_prim" %get-row-prim)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row primal value (basic solution) */

(defcfun ("_glp_lpx_get_row_dual" %get-row-dual)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row dual value (basic solution) */

#|
(defcfun ("_glp_lpx_get_row_info" %get-row-info))   /* obsolete */
void lpx_get_row_info(LPX *lp, int i, int *tagx, double *vx,
      double *dx);
/* obtain row solution information */
|#

(defcfun ("_glp_lpx_get_col_stat" %get-col-stat)
    variable-status
  (problem :pointer)
  (index :int))
;;/* retrieve column status (basic solution) */

(defcfun ("_glp_lpx_get_col_prim" %get-col-prim)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column primal value (basic solution) */

(defcfun ("_glp_lpx_get_col_dual" %get-col-dual)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column dual value (basic solution) */

#|
(defcfun ("_glp_lpx_get_col_info" %get-col-info))   /* obsolete */
void lpx_get_col_info(LPX *lp, int j, int *tagx, double *vx,
      double *dx);
/* obtain column solution information */
|#

;; TODO: This function returns the index of some non-basic variable
;; which causes primal unboundedness, and 0 if it doesn't know
;; one. We'd probably prefer to return :UNKNOWN in the latter case.
(defcfun ("_glp_lpx_get_ray_info" %get-ray-info)
    :int
  (problem :pointer))
;;/* determine what causes primal unboundness */

#| TODO: Define the type for LPXKKT and finish this definition.
(defcfun ("_glp_lpx_check_kkt" %check-kkt)
    :void
  (problem :pointer)
  ())
void lpx_check_kkt(LPX *lp, int scaled, LPXKKT *kkt);
/* check Karush-Kuhn-Tucker conditions */
|#

(defcfun ("_glp_lpx_warm_up" %warm-up)
    solver-exit-code
  (problem :pointer))
;;/* "warm up" LP basis */

#| TODO: Implement these
(defcfun ("_glp_lpx_eval_tab_row" %eval-tab-row))
int lpx_eval_tab_row(LPX *lp, int k, int ind[], double val[]);
/* compute row of the simplex table */

(defcfun ("_glp_lpx_eval_tab_col" %eval-tab-col))
int lpx_eval_tab_col(LPX *lp, int k, int ind[], double val[]);
/* compute column of the simplex table */

(defcfun ("_glp_lpx_transform_row" %transform-row))
int lpx_transform_row(LPX *lp, int len, int ind[], double val[]);
/* transform explicitly specified row */

(defcfun ("_glp_lpx_transform_col" %transform-col))
int lpx_transform_col(LPX *lp, int len, int ind[], double val[]);
/* transform explicitly specified column */

(defcfun ("_glp_lpx_prim_ratio_test" %prim-ratio-test))
int lpx_prim_ratio_test(LPX *lp, int len, int ind[], double val[],
      int how, double tol);
/* perform primal ratio test */

(defcfun ("_glp_lpx_dual_ratio_test" %dual-ratio-test))
int lpx_dual_ratio_test(LPX *lp, int len, int ind[], double val[],
      int how, double tol);
/* perform dual ratio test */
|#

(defcfun ("_glp_lpx_interior" %interior)
    solver-exit-code
  (problem :pointer))
;;/* easy-to-use driver to the interior point method */

(defcfun ("_glp_lpx_ipt_status" %ipt-status)
    interior-point-solution-status
  (problem :pointer))
;;/* retrieve status of interior-point solution */

(defcfun ("_glp_lpx_ipt_obj_val" %ipt-obj-val)
    :double
  (problem :pointer))
;;/* retrieve objective value (interior point) */

(defcfun ("_glp_lpx_ipt_row_prim" %ipt-row-prim)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row primal value (interior point) */

(defcfun ("_glp_lpx_ipt_row_dual" %ipt-row-dual)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row dual value (interior point) */

(defcfun ("_glp_lpx_ipt_col_prim" %ipt-col-prim)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column primal value (interior point) */

(defcfun ("_glp_lpx_ipt_col_dual" %ipt-col-dual)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column dual value (interior point) */

;; TODO: Figure out what values for class are valid.
(defcfun ("_glp_lpx_set_class" %set-class)
    :void
  (problem :pointer)
  (class :int))
;;/* set (change) problem class */

;; TODO: See ..._set_class
(defcfun ("_glp_lpx_get_class" %get-class)
    :int
  (problem :pointer))
;;/* retrieve problem class */

(defcfun ("_glp_lpx_set_col_kind" %set-col-kind)
    :void
  (problem :pointer)
  (index :int)
  (kind structural-variable-kind))
;;/* set (change) column kind */

(defcfun ("_glp_lpx_get_col_kind" %get-col-kind)
    structural-variable-kind
  (problem :pointer)
  (index :int))
;;/* retrieve column kind */

(defcfun ("_glp_lpx_get_num_int" %get-num-int)
    :int
  (problem :pointer))
;;/* retrieve number of integer columns */

(defcfun ("_glp_lpx_get_num_bin" %get-num-bin)
    :int
  (problem :pointer))
;;/* retrieve number of binary columns */

(defcfun ("_glp_lpx_integer" %integer)
    :int
  (problem :pointer))
;;/* easy-to-use driver to the branch-and-bound method */

(defcfun ("_glp_lpx_intopt" %intopt)
    solver-exit-code
  (problem :pointer))
;;/* easy-to-use driver to the branch-and-bound method */

(defcfun ("_glp_lpx_mip_status" %mip-status)
    integer-solution-status
  (problem :pointer))
;;/* retrieve status of MIP solution */

(defcfun ("_glp_lpx_mip_obj_val" %mip-obj-val)
    :double
  (problem :pointer))
;;/* retrieve objective value (MIP solution) */

(defcfun ("_glp_lpx_mip_row_val" %mip-row-val)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row value (MIP solution) */

(defcfun ("_glp_lpx_mip_col_val" %mip-col-val)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column value (MIP solution) */

#|
(defcfun ("_glp_lpx_check_int" %check-int))      /* undocumented */
void lpx_check_int(LPX *lp, LPXKKT *kkt);
/* check integer feasibility conditions */
|#

(defcfun ("_glp_lpx_reset_parms" %reset-parms)
    :void
  (problem :pointer))
;;/* reset control parameters to default values */

(defcfun ("_glp_lpx_set_int_parm" %set-int-parm)
    :void
  (problem :pointer)
  (parameter control-parameter)
  (value :int))
;;/* set (change) integer control parameter */

(defcfun ("_glp_lpx_get_int_parm" %get-int-parm)
    :int
  (problem :pointer)
  (parameter control-parameter))
;;/* query integer control parameter */

(defcfun ("_glp_lpx_set_real_parm" %set-real-parm)
    :void
  (problem :pointer)
  (parameter control-parameter)
  (value :double))
;;/* set (change) real control parameter */

(defcfun ("_glp_lpx_get_real_parm" %get-real-parm)
    :double
  (problem :pointer)
  (parameter control-parameter))
;;/* query real control parameter */

(defcfun ("_glp_lpx_read_mps" %read-mps)
    :pointer ; LPX *
  (filename :string))
;;/* read problem data in fixed MPS format */

;; TODO: Maybe convert the return value to a proper status code
(defcfun ("_glp_lpx_write_mps" %write-mps)
    :int
  (problem :pointer)
  (filename :string))
;;/* write problem data in fixed MPS format */

;; TODO: see ...write_mps
(defcfun ("_glp_lpx_read_bas" %read-bas)
    :int
  (problem :pointer)
  (filename :string))
;;/* read LP basis in fixed MPS format */

;; TODO: see ...write_mps
(defcfun ("_glp_lpx_write_bas" %write-bas)
    :int
  (problem :pointer)
  (filename :string))
;;/* write LP basis in fixed MPS format */

(defcfun ("_glp_lpx_read_freemps" %read-freemps)
    :pointer ; LPX *
  (filename :string))
;;/* read problem data in free MPS format */

;; TODO: see ...write_mps
(defcfun ("_glp_lpx_write_freemps" %write-freemps)
    :int
  (problem :pointer)
  (filename :string))
;;/* write problem data in free MPS format */

(defcfun ("_glp_lpx_read_cpxlp" %read-cpxlp)
    :pointer ; LPX *
  (filename :string))
;;/* read problem data in CPLEX LP format */

(defcfun ("_glp_lpx_write_cpxlp" %write-cpxlp)
    :int
  (problem :pointer)
  (filename :string))
;;/* write problem data in CPLEX LP format */

(defcfun ("_glp_lpx_read_model" %read-model)
    :pointer ; LPX *
  (model :string)
  (data :string)
  (output :string))
;;/* read LP/MIP model written in GNU MathProg language */

(defcfun ("_glp_lpx_print_prob" %print-prob)
    :int
  (problem :pointer)
  (filename :string))
;;/* write problem data in plain text format */

(defcfun ("_glp_lpx_print_sol" %print-sol)
    :int
  (problem :pointer)
  (filename :string))
;;/* write LP problem solution in printable format */

(defcfun ("_glp_lpx_print_sens_bnds" %print-sens-bnds)
    :int
  (problem :pointer)
  (filename :string))
;;/* write bounds sensitivity information */

(defcfun ("_glp_lpx_print_ips" %print-ips)
    :int
  (problem :pointer)
  (filename :string))
;;/* write interior point solution in printable format */

(defcfun ("_glp_lpx_print_mip" %print-mip)
    :int
  (problem :pointer)
  (filename :string))
;; /* write MIP problem solution in printable format */

#|

typedef struct
{     /* this structure contains results reported by the routines which
         checks Karush-Kuhn-Tucker conditions (for details see comments
         to those routines) */
      /*--------------------------------------------------------------*/
      /* xR - A * xS = 0 (KKT.PE) */
      double pe_ae_max;
      /* largest absolute error */
      int    pe_ae_row;
      /* number of row with largest absolute error */
      double pe_re_max;
      /* largest relative error */
      int    pe_re_row;
      /* number of row with largest relative error */
      int    pe_quality;
      /* quality of primal solution:
         'H' - high
         'M' - medium
         'L' - low
         '?' - primal solution is wrong */
      /*--------------------------------------------------------------*/
      /* l[k] <= x[k] <= u[k] (KKT.PB) */
      double pb_ae_max;
      /* largest absolute error */
      int    pb_ae_ind;
      /* number of variable with largest absolute error */
      double pb_re_max;
      /* largest relative error */
      int    pb_re_ind;
      /* number of variable with largest relative error */
      int    pb_quality;
      /* quality of primal feasibility:
         'H' - high
         'M' - medium
         'L' - low
         '?' - primal solution is infeasible */
      /*--------------------------------------------------------------*/
      /* A' * (dR - cR) + (dS - cS) = 0 (KKT.DE) */
      double de_ae_max;
      /* largest absolute error */
      int    de_ae_col;
      /* number of column with largest absolute error */
      double de_re_max;
      /* largest relative error */
      int    de_re_col;
      /* number of column with largest relative error */
      int    de_quality;
      /* quality of dual solution:
         'H' - high
         'M' - medium
         'L' - low
         '?' - dual solution is wrong */
      /*--------------------------------------------------------------*/
      /* d[k] >= 0 or d[k] <= 0 (KKT.DB) */
      double db_ae_max;
      /* largest absolute error */
      int    db_ae_ind;
      /* number of variable with largest absolute error */
      double db_re_max;
      /* largest relative error */
      int    db_re_ind;
      /* number of variable with largest relative error */
      int    db_quality;
      /* quality of dual feasibility:
         'H' - high
         'M' - medium
         'L' - low
         '?' - dual solution is infeasible */
      /*--------------------------------------------------------------*/
      /* (x[k] - bound of x[k]) * d[k] = 0 (KKT.CS) */
      double cs_ae_max;
      /* largest absolute error */
      int    cs_ae_ind;
      /* number of variable with largest absolute error */
      double cs_re_max;
      /* largest relative error */
      int    cs_re_ind;
      /* number of variable with largest relative error */
      int    cs_quality;
      /* quality of complementary slackness:
         'H' - high
         'M' - medium
         'L' - low
         '?' - primal and dual solutions are not complementary */
} LPXKKT;
|#