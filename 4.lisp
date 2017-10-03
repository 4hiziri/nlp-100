;; 30
(ql:quickload :split-sequence)
(use-package :split-sequence)


;; ((surface . "生れ") (base . "生れる") (pos . "動詞") (pos1 . "連用形"))n
(defun read-mecab (file)  
  (labels ((parse-morpheme (line)
	     (let* ((morpheme-analysis (split-sequence #\tab line))		    
		    (surface           (first morpheme-analysis))
		    (analysis          (split-sequence #\, (second morpheme-analysis))))
	       (list (cons 'surface surface)
		     (cons 'base    (nth 6 analysis))
		     (cons 'pos     (nth 0 analysis))
		     (cons 'pos1    (nth 1 analysis)))))
	   (read-morpheme (line)
	     (if (string= line "EOS")
		 nil
		 (parse-morpheme line))))
    (let ((list-sentences nil)
	  (temp-sentence nil))
      (with-open-file (in file)        
	(do ((line (read-line in nil nil) (read-line in nil nil)))
	    ((null line) (reverse list-sentences))
	  (let ((morpheme (read-morpheme line)))
	    (if morpheme
		(push morpheme temp-sentence)
		(progn (push (reverse temp-sentence) list-sentences)
		       (setf temp-sentence nil)))))))))

(defparameter mecab (read-mecab "resource/neko.txt.mecab"))

;; 31
(defun assoc-val (key alist)
  (cdr (assoc key alist)))

(defun find-mecab (key val mecab-list)
  (remove nil
	  (mapcar (lambda (sentense)
		    (find-if (lambda (morpheme)
			       (string= (assoc-val key morpheme)
					val))
			     sentense))
		  mecab-list)))

(setf surfaces (mapcar (lambda (x) (assoc-val 'surface x))
		       (find-mecab 'pos "動詞" mecab)))

;; 32
(setf bases (mapcar (lambda (x) (assoc-val 'base x))
		    (find-mecab 'pos "動詞" mecab)))

;; 33
(setf sa-hen-setuzoku (remove-if-not (lambda (x) (string= (assoc-val 'pos1 x)
							  "サ変接続"))
				     (find-mecab 'pos "名詞" mecab)))

;; 34
(defun find-phrase (pattern sentence)
  (let ((pos (search pattern
	  sentence
	  :test (lambda (x y) (string= x (assoc-val 'pos y))))))
    (when pos (subseq sentence pos (+ pos (length pattern))))))

(setf noun-phrase
      (mapcar (lambda (x) (concatenate 'string
                                       (assoc-val 'surface (nth 0 x))
				       (assoc-val 'surface (nth 1 x))
				       (assoc-val 'surface (nth 2 x))))
	      (remove-if-not (lambda (x) (and (string= (assoc-val 'base (nth 1 x))
						       "の")
					      (string= (assoc-val 'pos1 (nth 1 x))
						       "連体化")))
			     (mapcar (lambda (x) (find-phrase '("名詞" "助詞" "名詞") x))
				     mecab))))

;; 35
(defun find-longest-nouns (sentence)
  (flet ((inner-remove-nil-and-one-element (list)
	   (remove-if (lambda (x) (or (= (length x) 0)
				      (= (length x) 1)))
		      list)))
    (let ((series nil)
	  (tmp nil))
      (dolist (word sentence (inner-remove-nil-and-one-element (reverse series)))
	(if (string= (assoc-val 'pos word)
		     "名詞")
	    (push word tmp)
	    (progn (push (reverse tmp) series)
		   (setf tmp nil)))))))

(defun connect-surface (list)
  (reduce (lambda (x y) (concatenate 'string x y))
	  (mapcar (lambda (x) (assoc-val 'surface x)) list)))

(setf connected-nouns (mapcar (lambda (sentence)
				(mapcar #'connect-surface sentence))
			      (remove nil (mapcar #'find-longest-nouns mecab))))

;; 36
;; make-hash
(defun count-freq-base-words (mecab-list)
  (flet ((inner-word-base (list)
	   (apply #'append (mapcar (lambda (sentense)
				     (mapcar (lambda (word)
					       (assoc-val 'base word))
					     sentense))
				   list))))
    (let ((count (make-hash-table :test #'equal)))
      (dolist (word (inner-word-base mecab-list) count)
	(if (gethash word count)
	    (incf (gethash word count))
	    (setf (gethash word count) 1))))))

(defun hash-alist (table)
  (let ((ret-alist nil))
    (maphash (lambda (k e)
	       (push (cons k e) ret-alist))
	     table)
    ret-alist))

(defun sort-freq-alist (freq-alist)
  (sort freq-alist
	(lambda (x y) (> (cdr x) (cdr y)))))

(setf freq-sorted (sort-freq-alist(hash-alist (count-freq-base-words mecab))))

;; 37
(defun print-top-10 (freq-alist)
  (dolist (word-freq (subseq freq-alist 0 10))
    (format t "~A: ~A" (cdr word-freq) (car word-freq))
    (fresh-line)))

;; set autoscale y
;; plot "data.dat" using 0:2:xtic(1) with boxes notitle

(defun print-gnuplot-histgram (mecab-list num)
  (let ((freq-alist (sort-freq-alist (hash-alist (count-freq-words mecab-list)))))
    (dolist (pair (subseq freq-alist 0 num))
      (format t "~A ~A~%" (car pair) (cdr pair)))))

;; 38
;; 種類数?
(defun count-freq-words (mecab-list)
  (let ((words-list (apply #'append mecab-list))
	(count (make-hash-table :test #'equalp)))
      (dolist (word words-list count)
	(if (gethash word count)
	    (incf (gethash word count))
	    (setf (gethash word count) 1)))))

;; 39
(defun rule-of-zipf (mecab-list)
  (let ((alist (sort-freq-alist (hash-alist (count-freq-base-words mecab-list)))))
    (loop for i from 1
	  for pair in alist
	  do
	  (format t "~A ~A~%" i (cdr pair)))))

(defun write-to-file (file mecab-list)
  (with-open-file (out file
		       :direction :output
		       :if-exists :supersede)
    (let ((*standard-output* out))
      (rule-of-zipf mecab-list))))
