;; 40
(ql:quickload :split-sequence)
(use-package :split-sequence)

(defclass Morph ()
  ((surface :accessor surface
	    :initform ""
	    :initarg :surface)
   (base :accessor base
	 :initform ""
	 :initarg :base)
   (pos :accessor pos
	:initform ""
	:initarg :pos)
   (pos1 :accessor pos1
	 :initform ""
	 :initarg :pos1)))

(defmethod print-object ((Morph morph) out)
  (print-unreadable-object (Morph out :type t :identity t)
    (format out
	    "~S~T~S,~S,~S"
	    (surface morph)         
	    (pos morph)
	    (pos1 morph)
	    (base morph))))

(defun read-mecab (file)  
  (labels ((parse-morpheme (line)
	     (let* ((morpheme-analysis (split-sequence #\tab line))		    
		    (surface           (first morpheme-analysis))
		    (analysis          (split-sequence #\, (second morpheme-analysis))))
	       (make-instance 'Morph
			      :surface surface
			      :base (nth 6 analysis)
			      :pos (nth 0 analysis)
			      :pos1 (nth 1 analysis))))
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
