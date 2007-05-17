(in-package #:cl-glpk)

(defun load-glpk ()
  #+darwin (load-foreign-library "libglpk.dylib")
  #- (or darwin) (error "LOAD-GLPK (in LOAD.LISP) has not been implemented for your platform."))
