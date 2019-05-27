(in-package #:wrap-sized)

(progn
  (defun wrap-signed-0 (size integer)
    (declare (type unsigned-byte size)
             (type integer integer))
    (let* ((tmp (ldb (byte size 0) integer))
           (p (1- size)))
      (dpb tmp (byte 8 0) (- (ldb (byte 1 p) tmp)))))
  (define-compiler-macro wrap-signed-0 (&whole whole size integer)
    (if (typep size 'unsigned-byte)
        (let* ((gtmp (gensym "tmp"))
               (p (1- size)))
          `(let* ((,gtmp (ldb (byte ,size 0) ,integer)))
             (the (signed-byte ,size)
                  (dpb ,gtmp (byte 8 0) (- (ldb (byte 1 ,p) ,gtmp))))))
        whole))

  (defun wrap-signed-1 (size integer)
    (declare (type unsigned-byte size)
             (type integer integer))
    (let* ((tmp (ldb (byte size 0) integer))
           (p (1- size)))
      (dpb tmp (byte 8 0) (- (ash (logand (ash -1 p) tmp) (- p))))))
  (define-compiler-macro wrap-signed-1 (&whole whole size integer)
    (if (typep size 'unsigned-byte)
        (let* ((gtmp (gensym "tmp"))
               (p (1- size)))
          `(let* ((,gtmp (ldb (byte ,size 0) ,integer)))
             (the (signed-byte ,size)
                  (dpb ,gtmp (byte 8 0)
                       (- (ash (logand ,(ash -1 p) ,gtmp) ,(- p)))))))
        whole))

  (defun wrap-signed-2 (size integer)
    (declare (type unsigned-byte size)
             (type integer integer))
    (let* ((r (expt 2 size))
           (hr (ceiling r 2)))
      (- (mod (+ hr integer) r) hr)))
  (define-compiler-macro wrap-signed-2 (&whole whole size integer)
    (if (typep size 'unsigned-byte)
        (let* ((r (expt 2 size))
               (hr (ceiling r 2)))
          `(the (signed-byte ,size) (- (mod (+ ,hr ,integer) ,r) ,hr)))
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
                 :do (incf res2 (wrap-signed-2 8 (aref arr j))))))
      (print "-----"))

    (print "----------- WITHOUT INLINING ----------")
    (locally
        (declare (notinline wrap-signed-0 wrap-signed-1 wrap-signed-2))
      (dotimes (z 2)
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
                   :do (incf res2 (wrap-signed-2 8 (aref arr j))))))
        (print "-----")))
    (list res0 res1)))
