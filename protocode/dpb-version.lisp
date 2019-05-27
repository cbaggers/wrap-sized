(in-package #:wrap-sized)

(progn
  (declaim
   (inline wrap-signed-0)
   (ftype (function (unsigned-byte integer) integer) wrap-signed-0))
  (defun wrap-signed-0 (size integer)
    (declare (optimize (speed 3) (safety 1) (debug 1))
             (type unsigned-byte size)
             (type integer integer))
    (let* ((tmp (ldb (byte size 0) integer))
           (p (1- size)))
      (dpb tmp (byte 8 0) (- (ldb (byte 1 p) tmp)))))

  (declaim
   (inline wrap-signed-1)
   (ftype (function (unsigned-byte integer) integer) wrap-signed-1))
  (defun wrap-signed-1 (size integer)
    (declare (optimize (speed 3) (safety 1) (debug 1))
             (type unsigned-byte size)
             (type integer integer))
    (let* ((tmp (ldb (byte size 0) integer))
           (p (1- size)))
      (dpb tmp (byte 8 0) (- (ash (logand (ash -1 p) tmp) (- p))))))

  (declaim
   (inline wrap-signed-2)
   (ftype (function (unsigned-byte integer) integer) wrap-signed-2))
  (defun wrap-signed-2 (size integer)
    (declare (optimize (speed 3) (safety 1) (debug 1))
             (type unsigned-byte size)
             (type integer integer))
    (let* ((r (expt 2 size))
           (hr (ceiling r 2)))
      (- (mod (+ integer hr) r) hr))))

;;
;; Don't compiler these until you have tested without them. Inlining
;; should be enough but for some implementations it helps
;;
#+nil
(progn
  (define-compiler-macro wrap-signed-0 (&whole whole size integer)
    (if (typep size 'unsigned-byte)
        (let* ((gval (gensym "val"))
               (gtmp (gensym "tmp"))
               (p (1- size)))
          `(let ((,gval ,integer))
             (declare (optimize (speed 3) (safety 1) (debug 1))
                      (type integer ,gval))
             (let* ((,gtmp (ldb (byte ,size 0) ,gval)))
               (the (signed-byte ,size)
                    (dpb ,gtmp (byte 8 0) (- (ldb (byte 1 ,p) ,gtmp)))))))
        whole))
  (define-compiler-macro wrap-signed-1 (&whole whole size integer)
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
        whole))
  (define-compiler-macro wrap-signed-2 (&whole whole size integer)
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


(defun test ()
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((c 10000)
         (arr (make-array c :element-type 'fixnum))
         (res0 0)
         (res1 0)
         (res2 0))
    (declare (type fixnum res0 res1 res2))
    (dotimes (z c)
      (setf (aref arr z) (- (random 800) 400)))

    (dotimes (z 2)
      (print "-----")
      (time
       (loop
          :for i :below 10000
          :do (loop
                 :for j :below c
                 :do (incf res0 (wrap-signed-0 8 (aref arr j))))))
      (print "-and-")
      (time
       (loop
          :for i :below 10000
          :do (loop
                 :for j :below c
                 :do (incf res1 (wrap-signed-1 8 (aref arr j))))))
      (print "-and-")
      (time
       (loop
          :for i :below 10000
          :do (loop
                 :for j :below c
                 :do (incf res2 (wrap-signed-2 8 (aref arr j)))))))

    (print "----------- LESS WORK WITHOUT INLINING ----------")
    (locally
        (declare (notinline wrap-signed-0 wrap-signed-1 wrap-signed-2))
      (dotimes (z 2)
        (time
         (loop
            :for i :below 1000
            :do (loop
                   :for j :below c
                   :do (incf res0 (wrap-signed-0 8 (aref arr j))))))
        (print "-and-")
        (time
         (loop
            :for i :below 1000
            :do (loop
                   :for j :below c
                   :do (incf res1 (wrap-signed-1 8 (aref arr j))))))
        (print "-and-")
        (time
         (loop
            :for i :below 1000
            :do (loop
                   :for j :below c
                   :do (incf res2 (wrap-signed-2 8 (aref arr j))))))
        (print "-----")))
    (locally (declare (optimize (speed 1) (safety 3) (debug 3)))
      (let ((results (list res0 res1 res2)))
        (print results)
        (assert (every (lambda (x) (= (first results) x))
                       (rest results)))))))
