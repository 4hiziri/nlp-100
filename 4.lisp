;; 30
(ql:quickload :split-sequence)
(use-package :split-sequence)


;; ((surface . "生れ") (base . "生れる") (pos . "動詞") (pos1 . "連用形"))
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
  (labels ((inner-loop (rest tmp acc))))
  (let ((series nil)
	(tmp nil))
    (dolist (word sentence)
      (if ))))
