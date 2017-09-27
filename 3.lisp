;; 20
(ql:quickload :cl-ppcre)


(defun read-json ()
  (with-open-file (in "resource/jawiki-country.json")
    (let ((ret nil))
      (do ((line (read-line in nil nil) (read-line in nil nil)))
	  ((null line) ret)
	(push (list
	       (cons :title (read-from-string (ppcre:regex-replace "\"title\": " (ppcre:scan-to-strings "\"title\": \".*\"" line) "")))
	       (cons :text (read-from-string (ppcre:regex-replace-all "\\\\n" (ppcre:regex-replace "\"text\": " (ppcre:scan-to-strings "\"text\": \".*\"," line) "") (format nil "~%")))))
	      ret)))))

(defun search-text (title)
  (let ((json (read-json)))
    (cdr (assoc :text (find-if (lambda (x) (string= (cdr (assoc :title x)) title))
			       json)))))

(defparameter england (search-text "イギリス"))

;; 21
(defun find-category (text)
  (ppcre:all-matches-as-strings "\\[\\[Category:.*" text))

;; 22
(defun get-category (text)
  (let ((categories (find-category text)))
    (mapcar (lambda (x) (subseq x 1 (1- (length x))))
	    (mapcar (lambda (x) (ppcre:scan-to-strings ":.*?]" x)) categories))))

;; 23
(defun find-section (text)
  (let ((sectionts (ppcre:all-matches-as-strings "===*.+?===*" text)))
    (mapcar (lambda (x) (cons (1- (length (ppcre:scan-to-strings "=+" x)))
			      x))
	    sectionts)))

;; 24
(defun find-ref (text)
  (mapcar (lambda (x) (subseq x 1 (1- (length x))))
	  (mapcar (lambda (x) (ppcre:scan-to-strings ":.*?\\|" x)) (ppcre:all-matches-as-strings "\\[File.*?\\]" text))))

;; 25
(ql:quickload :split-sequence)
(use-package :split-sequence)

(defun get-info (text)
  (flet ((strip-field-line (x)
	   (subseq x 0 (1- (length x))))
	 (remove-bracket (x)
	   (subseq x 2 (- (length x) 2))))
    (let ((template (ppcre:scan-to-strings "{{基礎情報(.*\\n)*}}" text)))
      (mapcar (lambda (x)
		(when x
		  (let* ((field-line (split-sequence #\= (subseq (print (strip-field-line x)) 1)))
			 (field-name (remove #\space (first field-line)))
			 (field-val (remove #\space (second field-line))))
		    (cons field-name field-val))))
	      (ppcre:all-matches-as-strings "\\|.* = ([^=]*\\n)*" (remove-bracket template))))))
