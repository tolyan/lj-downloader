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
   `(,@(prepare-get struct path)))
	    
(defun prepare-get (struct path)
  (let ((result 's-xml-rpc:get-xml-rpc-struct-member))
    (if (> (length path) 1)
	(list result (list 'car (prepare-get struct (all-but-last path))) (last* path))
	(list result struct (first* path)))))
  

(defmacro  with-auth-call (call user pwd &rest args)
  (let ((challenge (get-struct-elem (get-challenge) (:|challenge|))))
    `(lj-struct-call ,call  ,@args
			   :|username| ,user
			   :|auth_method| "challenge"
			   :|auth_challenge| ,challenge
			   :|auth_response| ,(md5_hex
					     (concatenate  'string
							   challenge
							   (md5_hex pwd))))))


(defmacro get-utf-8-text (struct path)
  `(flexi-streams:octets-to-string (get-struct-elem ,struct ,path)
				  :external-format :utf-8))

(defun get-ditemid (event)
  (+
   (* 256 (get-struct-elem event (:|events| :|itemid|)))
   (get-struct-elem event (:|events| :|anum|))))