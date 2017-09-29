;; 30
(ql:quickload :split-sequence)
(use-package :split-sequence)

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
	    ((null line) list-sentences)
	  (let ((morpheme (read-morpheme line)))
	    (if morpheme
		(push morpheme temp-sentence))))))))
;; fucking way
