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
		   (list result (list 'car (prepare-get struct 
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


(defun store-events (where user pwd start-id &optional journal)
  (let ((id start-id) 
	(event (get-event user pwd start-id journal))
	(last-id (get-struct-elem 
			(get-last-event user pwd journal) (:|events| :|itemid|))))
     (loop while (< id last-id) do
	 (progn 
	   (cl-store:store event (concatenate 'string
					      where  ;TODO add trailing slash check
					      (write-to-string id)
					      "-lj.ser")) 
	   (print id)
	   (incf id)
	   (setf event (get-event user pwd id journal)))))) ;TODO fix journal value binding

(defun get-last-event (user pwd &optional (journal user))
  (get-event user pwd -1 journal)) 

(defun get-ditemid (event)
  (+
   (* 256 (get-struct-elem event (:|events| :|itemid|)))
   (get-struct-elem event (:|events| :|anum|))))


(defmacro get-utf-8-text (struct path)
  `(flexi-streams:octets-to-string (get-struct-elem ,struct ,path)
				  :external-format :utf-8))