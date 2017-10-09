;; 50
(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(use-package :split-sequence)

(defun read-file (file-name &optional (delimiter '(#\newline)))
  (let ((ret "")
	(next-line (coerce delimiter 'string)))
    (with-open-file (in file-name)
      (do ((line (read-line in nil nil) (read-line in nil nil)))
	  ((null line) ret)
	(setf ret (concatenate 'string ret line next-line))))))

(defparameter *nlp* (read-file "resource/nlp.txt"))

;; Like Automaton
(defun split-to-sentence (strings)
  (labels ((next-capital (chars tmp acc)
	     (if chars
		 (if (upper-case-p (car chars))
		     (find-end-char chars nil (cons (reverse (cddr tmp)) acc))
		     (find-end-char (cdr chars) (cons (car chars) tmp) acc))
		 (cons (reverse (cdr tmp)) acc)))
	   (next-space (chars tmp acc)
	     (if chars
		 (if (char= (car chars) #\space)
		     (next-capital (cdr chars) (cons (car chars) tmp) acc)
		     (find-end-char (cdr chars) (cons (car chars) tmp) acc))
		 (cons (reverse tmp) acc)))
	   (find-end-char (chars tmp acc)
	     (if chars
		 (if (member (car chars) '(#\. #\; #\: #\? #\!))
		     (next-space (cdr chars) (cons (car chars) tmp) acc)
		     (find-end-char (cdr chars) (cons (car chars) tmp) acc))
		 (cons (reverse tmp) acc))))
    (mapcar (lambda (x) (substitute #\space #\newline x))
	    (mapcar (lambda (x) (coerce x 'string))
		    (reverse (find-end-char (coerce strings 'list) nil nil))))))

(defparameter *sentences* (split-to-sentence *nlp*))

;; 51
(defun split-to-word (sentence)
  (split-sequence #\space sentence))

(defparameter *words* (mapcar #'split-to-word *sentences*))

;; 52
;; I can't find lisp implementation.

;; 53

