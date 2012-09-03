;; Utils I found usefull

(in-package :lj-downloader)

;стырил отсюда http://common-lisp.net/project/clbuild/mirror/ironclad/util.lisp
(defun byte-array-to-hex-string (vector &key (start 0) end (element-type 'base-char))
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (declare (type (vector (unsigned-byte 8)) vector)
           (type fixnum start)
           (type (or cl:null fixnum) end)
           (optimize (speed 3) (safety 1)))
  (let* ((end (or end (length vector)))
         (length (- end start))
         (hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (loop with string = (ecase element-type
                          ;; so that the compiler optimization can jump in
                          (base-char (make-string (* length 2)
                                                  :element-type 'base-char))
                          (character (make-string (* length 2)
                                                  :element-type 'character)))
       for i from start below end
       for j from 0 below (* length 2) by 2
       do (let ((byte (aref vector i)))
            (declare (optimize (safety 0)))
            (setf (aref string j)
                  (aref hexdigits (ldb (byte 4 4) byte))
                  (aref string (1+ j))
                  (aref hexdigits (ldb (byte 4 0) byte))))
       finally (return string))))

(defun md5_hex (input)
  (byte-array-to-hex-string (md5:md5sum-sequence input)))

(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value)) 

(defun string-split (string delimiter)
  "Splits string into list of strings with delimeter provided"
  (loop for i = 0  then (1+ j)
       as j = (position delimiter string :start i)
       collect (subseq string i j)
       while j))


(defun hashize (list table)
  "Converts list into hashtable where elements on odd position used as keys
and elements on even position used as values"
  (when (> (length list) 1)
    (setf (gethash (intern (car list)) table)
	  (car (cdr list)))
    (hashize (cdr (cdr list)) table)))

(defun last* (lst)
  (cond ((listp lst) (car (reverse lst)))
	(t lst)))

(defun first* (lst)
  (cond ((listp lst) (car lst))
	(t lst)))

(defun all-but-last (lst)
  (cond ((listp lst) (reverse (cdr (reverse lst))))
	(t lst)))

(defun unscreen (keyword)
  (let ((tag (write-to-string keyword)))
    (subseq tag 2 (1- (length tag)))))

(defun escape (content)
  (cond ((stringp content) (concatenate 'string
					"<![CDATA["
					content
					"]]>"))
	((numberp content) (write-to-string content))
	(t "UNSUPPORTED TYPE OF CONTENT")))

(defun alist-to-xml (alist stream &optional indent)
  (if alist
      (progn 
	(if (listp (cdar alist))
	    (progn
	      (format stream "<~A> ~%" (caar alist))
	      (if indent (format stream "~@T"))
	      (alist-to-xml (cdar alist) stream indent)
	      (format stream "</~A> ~%" (caar alist)))
	    (list-to-tag stream (car alist)))
	(alist-to-xml (rest alist) stream indent))))
	 
(defun list-to-tag (stream lst)
  (format stream "<~A> ~A </~A> ~%" (car lst) (cdr lst) (car lst)))

(defun cons-to-xml (cell)
  (when cell
    (let ((name (unscreen (car cell))))
      (concatenate 'string
		   (start-tag name)
		   (escape  (cdr cell))
		   (end-tag name)))))

;Peter Seibel's with-gensyms
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro call-if (what expr)
  (with-gensyms (result)
      `(let ((result ,expr))
	 (if (not (eq result nil))
	     (,what result)))))
		 
