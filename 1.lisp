;; 00
(defun string-reverse (string)
  (reverese string))

(defun string-reverse (string)
  (labels ((my-reverse (list acc)
	     (if list
		 (my-reverse (cdr list) (cons (car list) acc))
		 acc)))
    (coerce (my-reverse (coerce string 'list) nil) 'string)))

;; 01
(defun one-three-five-seven (string)
  (let ((list (coerce string 'list)))
    (coerce (loop for i from 0 to 6 by 2
		  collect (nth i list))
	    'string)))

;; 02
(defun alternate-concat (str1 str2)
  (labels ((inner-loop1 (str1 str2 acc)
	     (if str1
		 (inner-loop2 (cdr str1) str2 (cons (car str1) acc))
		 (reverse acc)))
	   (inner-loop2 (str1 str2 acc)
	     (if str2
		 (inner-loop1 str1 (cdr str2) (cons (car str2) acc))
		 (reverse acc))))
    (coerce (inner-loop1 (coerce str1 'list) (coerce str2 'list) nil) 'string)))

;; 03
(defparameter test-string "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics.")

(defun split-sequense (delimeter sequence &optional (test #'equal))
  (labels ((inner-loop (sequence tmp acc)
	     (cond ((null sequence) (reverse (cons (reverse tmp) acc)))
		   ((funcall test (car sequence) delimeter) (inner-loop (cdr sequence)
									nil
									(cons (reverse tmp) acc)))
		   (t (inner-loop (cdr sequence)
				  (cons (car sequence) tmp)
				  acc)))))
    (inner-loop sequence nil nil)))

(defun length-word (string)
  (let ((word-list (split-sequense #\space (coerce string 'list))))
    (mapcar #'length (mapcar (lambda (x) (remove #\, (remove #\. x))) word-list))))

;; 04
(defparameter test-string "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can.")

(defun two-sometimes-one (string)
  (let ((word-list (split-sequense #\space (coerce string 'list)))
	(one-list '(0 4 5 6 7 8 14 15 18)))
    (loop for i from 0
	  for word in word-list
	  collect (cons (coerce (subseq word 0 (if (find i one-list)
						   1
						   2))
				'string)
			i))))

;; 05

;; easy implement, but maybe not efficient
(defparameter test-string "I am an NLPer")

(defun split-num (sequence n)  
  (loop for i from 0 below (length sequence) by n
	collect (subseq sequence i (min (+ i n) (length sequence)))))

(defun n-gram (sequence n)
  (split-num sequence n))

(split-num test-string 2)
(mapcar (lambda (x)
	  (mapcar (lambda (y) (coerce y 'string))
		  x))
	(split-num (split-sequense #\space (coerce test-string 'list)) 2))

;; 06
(defparameter test-string1 "paraparaparadise")
(defparameter test-string2 "paragraph")

(let ((x (n-gram test-string1 2))
      (y (n-gram test-string2 2)))
  (format t "~A~%" (remove-duplicates (union x y :test #'string=) :test #'string=))
  (format t "~A~%" (remove-duplicates (intersection  x y :test #'string=) :test #'string=))
  (format t "~A~%" (remove-duplicates (set-difference  x y :test #'string=) :test #'string=))) 

;; 07
(defun print-when (x y z)
  (format nil "~A時の~Aは~A" x y z))

(print-when 12 "気温" 22.4)

;; 08
(defun cipher (string)
  (map 'string (lambda (x) (if (lower-case-p x)
			       (code-char (- 219 (char-code x)))
			       x))
       string))

;; 09
(defparameter test-text "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind .")

(defun random-sort (list)
  (let ((mapper nil)
	(ret (make-array (length list))))
    (loop for i = (random (length list))
	  when (not (find i mapper))
	    do (push i mapper)
	  when (= (length mapper) (length list))
	    do (return))
    (dolist (index mapper (coerce ret 'list))
      (setf (aref ret index) (pop list)))))

(defun typoglycemia (string)
  (labels ((get-enough-long-words (words acc)
	     (cond ((null words) acc)
		   ((> (length (car words)) 4) (get-enough-long-words (cdr words)
								      (cons (car words) acc)))
		   (t (get-enough-long-words (cdr words) acc)))))    
    (let* ((word-list (mapcar (lambda (x) (coerce x 'string))
			      (split-sequense #\space (coerce string 'list))))
	   (enough-long (random-sort (get-enough-long-words word-list nil)))
	   (ret-str ""))
      (dolist (word
	       (append (list (car word-list))
		       (mapcar (lambda (x) (if (> (length x) 4)
					       (pop enough-long)
					       x))
			       (subseq word-list 1 (1- (length word-list))))
		       (list (car (reverse word-list))))
	       (subseq ret-str 1))
	   (setf ret-str (concatenate 'string ret-str " " word))))))
