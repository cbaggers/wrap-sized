(in-package #:wrap-sized)

(declaim (ftype (function (unsigned-byte integer) integer) wrap-signed)
         (inline wrap-signed))
#+sbcl
(defun wrap-signed (size integer)
  (declare (type unsigned-byte size)
           (type integer integer))
  (sb-c::mask-signed-field size integer))

#+ccl
(progn
  (defun wrap-signed (size integer)
    (declare (optimize (speed 3) (safety 1) (debug 1))
             (type unsigned-byte size)
             (type integer integer))
    (let* ((tmp (ldb (byte size 0) integer))
           (p (1- size)))
      (dpb tmp (byte 8 0) (- (ash (logand (ash -1 p) tmp) (- p))))))
  (define-compiler-macro wrap-signed (&whole whole size integer)
    (if (typep size 'unsigned-byte)
        (let* ((gval (gensym "val"))
               (gtmp (gensym "tmp"))
               (p (1- size)))
          `(let ((,gval ,integer))
             (declare (optimize (speed 3) (safety 1) (debug 1))
                      (type integer ,gval))
             (let* ((,gtmp (ldb (byte ,size 0) ,gval)))
               (the (signed-byte ,size)
                    (dpb ,gtmp (byte 8 0)
                         (- (ash (logand ,(ash -1 p) ,gtmp) ,(- p))))))))
        whole)))

#-(or ccl sbcl)
(progn
  (defun wrap-signed (size integer)
    (declare (optimize (speed 3) (safety 1) (debug 1))
             (type unsigned-byte size)
             (type integer integer))
    (let* ((r (expt 2 size))
           (hr (ceiling r 2)))
      (- (mod (+ integer hr) r) hr)))
  (define-compiler-macro wrap-signed (&whole whole size integer)
    (if (typep size 'unsigned-byte)
        (let* ((gval (gensym "integer"))
               (r (expt 2 size))
               (hr (ceiling r 2)))
          `(let ((,gval ,integer))
             (declare (optimize (speed 3) (safety 1) (debug 1))
                      (inline mod)
                      (type integer ,gval))
             (the (signed-byte ,size) (- (mod (+ ,gval ,hr) ,r) ,hr))))
        whole)))

(declaim (ftype (function (unsigned-byte integer) unsigned-byte)
                wrap-unsigned)
         (inline wrap-unsigned))

#+sbcl
(defun wrap-unsigned (size integer)
  (declare (type unsigned-byte size)
           (type integer integer))
  (sb-c::mask-field (byte size 0) integer))

#-sbcl
(defun wrap-unsigned (size integer)
  (declare (type unsigned-byte size)
           (type integer integer))
  (ldb (byte size 0) integer))

;;------------------------------------------------------------

(defmacro iop (size (func &rest args))
  (assert (typep size 'unsigned-byte))
  (let ((gargs (loop :for i :below (length args) :collect (gensym))))
    `(let ,(loop
              :for arg :in args
              :for garg :in gargs
              :collect (list garg arg))
       (declare
        (type (signed-byte ,size) ,@gargs)
        (inline wrap-signed))
       (the (signed-byte ,size) (wrap-signed ,size (,func ,@gargs))))))

(defmacro uop (size (func &rest args))
  (assert (typep size 'unsigned-byte))
  (let ((gargs (loop :for i :below (length args) :collect (gensym))))
    `(let ,(loop
              :for arg :in args
              :for garg :in gargs
              :collect (list garg arg))
       (declare
        (type (unsigned-byte ,size) ,@gargs)
        (inline wrap-unsigned))
       (the (unsigned-byte ,size) (wrap-unsigned ,size (,func ,@gargs))))))

;;------------------------------------------------------------
