(in-package #:wrap-sized)

(declaim (ftype (function (unsigned-byte integer) integer) wrap-signed)
         (inline wrap-signed))
#+sbcl
(defun wrap-signed (size integer)
  (declare (type unsigned-byte size)
           (type integer integer))
  (sb-c::mask-signed-field size integer))

#-sbcl
(progn
  (defun wrap-signed (size integer)
    (declare (unsigned-byte integer))
    (let* ((r (expt 2 size))
           (hr (ceiling r 2)))
      (- (mod (+ hr integer) r) hr)))
  (define-compiler-macro wrap-signed (size integer)
    (let* ((r (expt 2 size))
           (hr (ceiling r 2)))
      `(- (mod (+ ,hr ,integer) ,r) ,hr))))

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
