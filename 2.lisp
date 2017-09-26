(defun read-file (file-name &optional (delimiter '(#\newline)))
  (let ((ret "")
	(next-line (coerce delimiter 'string)))
    (with-open-file (in file-name)
      (do ((line (read-line in nil nil) (read-line in nil nil)))
	  ((null line) ret)
	(setf ret (concatenate 'string ret line next-line))))))

(defparameter test (read-file "resource/hightemp.txt"))

;; 10
(defun my-count (item sequence &optional (test #'eql))
  (reduce (lambda (count elm) (if (funcall test elm item)
				  (1+ count)
				  count))
	  sequence
	  :initial-value 0))

(defun wc (string &optional (delimiter #\newline))
  (my-count delimiter (coerce string 'list)))

;; 11
(defun my-replace (old new sequence &key (test #'eql))  
  (map (type-of sequence)
       (lambda (x) (if (funcall test x old) new x))
       sequence))

(defun tab-space (string)
  (my-replace #\tab #\space string))

;; 12
(ql:quickload :split-sequence)
(use-package :split-sequence)

(defun cut (n string)
  (mapcar (lambda (x) (nth n x))
	    (mapcar (lambda (x) (split-sequence #\tab x))
			(remove "" (split-sequence #\newline string) :test #'string=))))

(defun split-col (string)
  (let ((col1 (cut 0 string))
	(col2 (cut 1 string)))    
    (with-open-file (out "output/col1.txt"
			 :direction :output
			 :if-exists :supersede)
      (dolist (elm col1)
	(princ elm out)
	(fresh-line out)))
    (with-open-file (out "output/col2.txt"
			 :direction :output
			 :if-exists :supersede)
      (dolist (elm col2)
	(princ elm out)
	(fresh-line out)))))

;; 13
(defun my-merge (field1 field2)
  (let ((list1 (split-sequence #\newline field1))
	(list2 (split-sequence #\newline field2))
	(ret ""))
    (loop for i1 in list1
	  for i2 in list2
	  do (setf ret (concatenate 'string ret (format nil "~a~a~a~%" i1 #\tab i2))))
    ret))

(defun merge-col1-col2 ()
  (with-open-file (out "output/mergec-col1-col2.txt"
		       :direction :output
		       :if-exists :supersede)      
    (format out "~a" (my-merge (read-file "output/col1.txt") (read-file "output/col2.txt")))))

;; 14
(defun head (num file)
  (with-open-file (in file)
      (dotimes (n num)	
	(format t "~A"(read-line in))
	(fresh-line))))

;; 15
(defun tail (num file)
  (let ((buf nil))
    (with-open-file (in file)
      (do ((line (read-line in nil nil)
		 (read-line in nil nil)))
	  ((null line) buf)
	(push line buf)))
    (format t "~{~A~^~%~}" (reverse (subseq buf 0 num)))))

;; 16
(defun split (n file)
  (let* ((string (read-file file))
	 (line-num (wc string))
	 (lines (split-sequence #\newline string))
	 (ret nil))
    (mapcar (lambda (x) (format nil "~{~A~^~%~}" x))
	    (dotimes (x n (reverse ret))
	      (let ((buf nil))
		(push (dotimes (y (truncate line-num n) (reverse buf))
			(when lines
			  (push (pop lines) buf)))
		      ret))))))

;; 17
(defun cut-sort-uniq (n string)
  (remove-duplicates (cut n string) :test #'string=))

;; 18
(defun zip (seq1 seq2)
  (loop for e1 in seq1
	for e2 in seq2
	collect (cons e1 e2)))

(defun numeriacl-rev-sort (string)
  (let ((nums (mapcar #'read-from-string (cut 2 string))))
    (format nil "~{~A~^~%~}" (mapcar #'cdr (reverse (sort (zip nums (split-sequence #\newline string)) (lambda (x y) (< (car x) (car y)))))))))

;; 19
(defun freq-sort (string)
  (let ((lines (cut 0 string)))
    (mapcar #'cdr
	    (sort (mapcar (lambda (x) (cons (count x lines :test #'string=)
					    x))
			  lines)
		  (lambda (x y) (< (car x) (car y)))))))
