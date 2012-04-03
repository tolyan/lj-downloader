(ql:quickload "s-xml-rpc")
(ql:quickload "cl-store")
(ql:quickload "flexi-streams")

(defun lj-call (name &optional struct)
  (s-xml-rpc:xml-rpc-call (s-xml-rpc:encode-xml-rpc-call 
			   (concatenate 'string 
					"LJ.XMLRPC." 
					(string-downcase (string name)))
			   struct)
			  :host  "www.livejournal.com"
			  :url "/interface/xmlrpc"))

(defmacro lj-struct-call (name &rest args)
  `(lj-call ,name 
	   (s-xml-rpc:xml-rpc-struct ,@args)))

(defmacro get-struct-elem (struct path)
  (labels ((expand-get (arg1 arg2)
	     (let ((result 's-xml-rpc:get-xml-rpc-struct-member))
	       (if (> (length arg2) 1)
		   (list result (list 'car (expand-get struct 
							(all-but-last arg2))) 
			 (last* arg2))
		   (list result arg1 (first* arg2))))))
   `(,@(expand-get struct path))))
   
(defmacro  with-auth-call (call user pwd &rest args)
  (let ((challenge (get-struct-elem (get-challenge) (:|challenge|))))
    `(lj-struct-call ,call  ,@args
			   :|username| ,user
			   :|auth_method| "challenge"
			   :|auth_challenge| ,challenge
			   :|auth_response| (md5_hex
					     (concatenate  'string
							   ,challenge
							   (md5_hex ,pwd))))))
(defmacro with-clear-auth-call (call user pwd &rest args)
  `(lj-struct-call ,call ,@args
		   :|username| ,user
		   :|auth_method| "clear"
		   :|password| , pwd))


(defmacro get-event (user pwd event-id &optional (journal user))
    `(with-clear-auth-call 'getevents ,user ,pwd 
				   :|selecttype| "one" :|itemid| ,event-id
				   :|ver| 1 :|lineendings| "unix"
				   :|journal| ,journal))

(defun get-last-event (user pwd &optional (journal user))
  (get-event user pwd -1 journal)) 

(defun get-comments (user pwd event-id &optional (journal user))
  (let ((ditemid (get-ditemid (get-event user pwd event-id journal))))
    (with-clear-auth-call 'getcomments user pwd
			  :|ditemid| ditemid
			  :|expand_strategy| "expand_all"
			  :|ver| 1 :|lineendings| "unix"
			  :|journal| journal)))

(defun store-events (dir user pwd start-id  journal)
  (let ((id start-id) 
	(last-id (get-struct-elem 
			(get-last-event user pwd journal) (:|events| :|itemid|))))
     (loop while (< id last-id) do
	 (progn 
	   (let ((output (get-full-post user pwd id journal)))
	     (when output 
	       (with-open-file (file (concatenate 'string
						 dir
						 (write-to-string id)
						 ".list")
				     :direction :output
				     :if-does-not-exist :create
				     :if-exists :overwrite)
		 (print output file))))
	   (print id)
	   (incf id)))))

(defun get-ditemid (event)
  (+
   (* 256 (get-struct-elem event (:|events| :|itemid|)))
   (get-struct-elem event (:|events| :|anum|))))

(defun event-alist (event)
 (labels ((event-pretty (event args) 
	    (when event
	      (loop for x in args 
		 collect (cons x (get-utf-8-text event (:|events| x)))))))
   (event-pretty event '(:|reply_count| 
			 :|event_timestamp| 
			 :|url|
			 :|anum| 
			 :|logtime| 
;			 :|props|
			 :|eventtime| 
			 :|event| 
			 :|subject| 
			 :|itemid|))))

(defmacro get-utf-8-text (struct path)
  (with-gensyms (value)
  `(let ((value (get-struct-elem ,struct ,path)))
    (if (and (typep value 'vector) (not (typep value 'simple-array)))
	 (flexi-streams:octets-to-string value
				  :external-format :utf-8)
	 value))))

(defun comments-alist (comments)
  "Converts comments xml-rpc-struct into simple alist tree structure"
  (labels ((comments-pre (comment args)
    (when comment
      (cons
       (loop for x in args 
	  collect (cons x (get-utf-8-text comment (x))))
       (comments-alist (get-struct-elem comment (:|children|)))))))
    (loop for struct in comments
       collect (comments-pre struct '(:|datepostunix|
				      :|is_loaded|
				      :|is_show|
				      :|datepost|
				      :|postername|
				      :|dtalkid|
				      :|level|
				      :|subject|
				      :|body|
				      :|state|
				      :|posterid|)))))

(defun get-full-post (usr pwd itemid journal)
  (let ((event (get-event usr pwd itemid journal)))
    (when (get-utf-8-text event (:|events|))
      (cons (event-alist event)
	    (comments-alist
	     (get-struct-elem (get-comments usr pwd itemid journal)
			      (:|comments|)))))))

(let ((stream (make-string-output-stream)))
  (defun alist-to-xml-stream (alist &optional (level 0) &key (pretty t))
    (write-tab-line (start-tag (car alist)) level stream) 
    (if (listp (cdr alist))
	(alist-to-xml-stream (cdr alist) (1+ level) :pretty pretty) 
	(write-tab-line (escape (cdr alist)) (1+ level) stream))
    (write-tab-line (end-tag (car alist)) level stream)
    (get-output-stream-string stream)
    (close stream))
  
  (defun close-stream ()
    (close stream))
  
   (defun flush-stream ()
    (get-output-stream-string stream)))

		 
