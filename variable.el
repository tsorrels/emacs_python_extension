(defun create-variable (symbol fields methods &optional variables)
  "Generate a list specially formated list to represent a variable.
The first element of the list is a string that represents the variable symbol.
The second element is a list of strings which are the variable's fields.
The third element is a list of strings which are the variable's methods."
  (let (empty-list)
    (cons symbol (cons fields (cons methods (cons variables empty-list))))))

(defun get-variable-fields (variable)
  "Getter to return the proper list that represents the fields for a variable"
  (car (cdr variable)))

(defun get-variable-methods (variable)
  "Getter to return the proper list that represents the methods for a variable"
  (car (cdr (cdr variable))))

(defun get-variable-variables (variable)
  "Getter to return the proper list that represents the nested variables for a variable"
  (car (cdr (cdr (cdr variable)))))

(defun get-variable-symbol (variable)
  "Getter to return the string that is the symbol for the variable"
  (car variable))

(defun parse-variable-line (line)
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (parse-variable)))

(defun parse-variable ()
  (let (symbol
	fields-string
	fields
	methods-string
	methods
	variable
	variables)
  
  ;;; remove beginning '('
  (forward-char)

  ;;; parse symbol
  (setq saved-point (point))
  (search-forward ";")
  (setq symbol (buffer-substring saved-point (- (point) 1)))
  
  ;;; parse fields
  ;;;(forward-char)
  (setq saved-point (point))
  (search-forward ";")
  (setq fields-string (buffer-substring saved-point (- (point) 1)))
  (setq fields (parse-basic-members fields-string))
  
  ;;; parse methods
  ;;;(forward-char)
  (setq saved-point (point))    
  (search-forward ";")
  (setq methods-string (buffer-substring saved-point (- (point) 1)))
  (setq methods (parse-basic-members methods-string))
  
  ;;; parse variables
  ;;;(forward-char)
  (while (not (= (char-after) ?\)))
    (cond ((= (char-after) ?,) (forward-char))
	  ((= (char-after) ?\()
	   (setq variable  (parse-variable))
	   (if (eq variables nil)
	       (setq variables (list variable))		
	     (setq variables (append variables (list variable)))))))

  ;;; eat ')'
  (forward-char)
  (create-variable symbol fields methods variables)))


(defun parse-variable-dep (line)
  (let (pruned-string
	current-char
	current-string
	split-strings
	symbol
	fields
	fields-string
	methods
	methods-string
	variables-line-pair
	variable
	variables )

     ;;; remove beginning '('
    (setq pruned-string (substring line 1 nil))

    ;;; parse symbol
    (setq split-strings (split-string-at-char pruned-string ";"))
    (setq symbol (car split-strings))
    (setq pruned-string (car (cdr split-strings)))

    ;;; parse fields
    (setq split-strings (split-string-at-char pruned-string ";"))
    (let ((fields-string (car split-strings)))
      (setq fields (parse-basic-members fields-string)))
    (setq pruned-string (car (cdr split-strings))) ;;; or nth 2 split-strings

    ;;; parse methods
    (setq split-strings (split-string-at-char pruned-string ";"))
    (let ((methods-string (car split-strings)))
      (setq methods (parse-basic-members methods-string)))
    (setq pruned-string (car (cdr split-strings))) ;;; or nth 2 split-strings

    ;;; parse variables
    (setq current-char (string-to-char pruned-string))
    (while (not (char-equal current-char (string-to-char ")")))
      (cond ( (char-equal current-char (string-to-char "("))
	      (setq variables-line-pair (parse-variable pruned-string))
	      (setq variable (car variables-line-pair))
	      (if (eq variables nil)
		  (setq variables (list variable))		
		(setq variables (append variables (list variable))))
 	      (setq pruned-string (car (cdr variables-line-pair))))
	    ( (char-equal current-char (string-to-char ","))
	      (setq pruned-string (substring pruned-string 1 nil))))
      (setq current-char (string-to-char pruned-string))) ;;; eat ','
    (setq pruned-string (substring pruned-string 1 nil)) ;;; eat ')'
    (setq returned-var (create-variable symbol fields methods variables))
    (list returned-var pruned-string)))
    ;;;variables))

(defun parse-basic-members (string)
  (let (members)
    (if (string-equal "" string)
	(setq members nil)
      (setq members (split-string string ",")))
    members))
  


(defun get-string-at-char-dep(string char)
  (let (pruned-string
	(returned-string ""))
    (setq pruned-string string)
    (while (and (> (length pruned-string) 0)
		(not (char-equal (string-to-char pruned-string) char)))
      ())
    )
  )
  

(defun split-string-at-char-dep (string char)
  (let (words
	first-string
	last-string
	rest-of-words)
    (setq words (split-string string char))
    (setq first-string (car words))
    (setq rest-of-words (cdr words))
    (setq last-string (car rest-of-words))
    (setq rest-of-words (cdr rest-of-words))
    
    ;;; rebuild other splits into second string
    (while rest-of-words
      (setq last-string (concat last-string char (car rest-of-words)))
      (setq rest-of-words (cdr rest-of-words)))
    (list first-string last-string)))
