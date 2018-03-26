(defun get-symbol-and-set-globals-dep (subsequent-call typed-symbol)
  (let (symbol-components
	typing-new-variable
	variable-symbol
	typed-member-symbol
	next-member-symbol)
    (setq symbol-components (split-string typed-symbol "\\."))
    (if (< (length symbol-components) 2)
	(setq typing-new-variable t)
      (progn
	(setq variable-symbol (car symbol-components))
	(setq typed-member-symbol (car (cdr symbol-components)))))
    (if typing-new-variable
	(progn
	  (setq next-variable (get-matching-variable subsequent-call
						     typed-symbol
						     current-variable-global
						     global-scope))
	  (if next-variable
	      (progn
		(setq current-variable-global next-variable)
		(setq current-field-global nil)
		(setq current-method-global nil)
		(get-variable-symbol next-variable))))
      (progn ;;; else typed symbol includes a '.' accessor token
	(if (not subsequent-call)
	    (progn
	    ;;; start by finding the right variable
	      (setq next-variable (get-matching-variable subsequent-call
							 variable-symbol
							 current-variable-global
							 global-scope))
	      (if next-variable
		  (progn
		    (setq current-variable-global next-variable)
		    (setq current-field-global nil)
		    (setq current-method-global nil)))))
	(if current-variable-global
	    (progn
	      (setq next-member-symbol (get-matching-member-and-set-globals
					current-variable-global
					typed-member-symbol
					subsequent-call))
	      (concat variable-symbol "." next-member-symbol)))))))







(defun get-matching-variable-dep (subsequent-call typed-symbol current-var scope)
  (if subsequent-call
      (get-next-matching typed-symbol
			 scope
			 (get-variable-symbol current-var)
			 'compare-symbol-with-variable)
    (get-first-matching typed-symbol scope 'compare-symbol-with-variable)))


(defun get-first-matching-member-dep (typed-member-symbol list-of-members)
  (let (returned-member-symbol
	current-list
	current-member)
    (setq current-list list-of-members)
    (while current-list
      (setq current-member (car current-list))
      (if (string-prefix-p typed-member-symbol current-member)
	  (progn
	    (setq returned-member-symbol current-member)
	    (setq current-list nil))
	(setq current-list (cdr current-list))))
    returned-member-symbol))
    

(defun get-matching-member-and-set-globals-dep (current-variable
					    typed-member-symbol
					    subsequent-call)
  (let ((fields (get-variable-fields current-variable))
	(methods (get-variable-methods current-variable))
	current-field
	current-method
	returned-member)
    (if (not subsequent-call) ;;; just get first matching field or method
	(progn
	  (setq current-field (get-first-matching typed-member-symbol
						  fields
						  'compare-symbol-with-member))
	  (setq returned-member current-field)
	  ;;; if no matching field, try matching a method
	  (if (not current-field)
	      (progn
		(setq current-method (get-first-matching typed-member-symbol
						      methods
						   'compare-symbol-with-member))
		(setq returned-member current-method))))

       ;;; this is a subsequent call
      ;;; if this is a subsequent call, but there was no previous match
      (cond ((and (eq current-field-global nil) (eq current-method-global nil))
	    (setq returned-member nil))

	    ;;; if field is set and method is nil
	    ((and (not (eq current-field-global nil)) (eq current-method-global
							   nil))
	    (progn
	      (setq current-field (get-next-matching typed-member-symbol
						     fields
						     current-field-global
						   'compare-symbol-with-member))
	      (if current-field
		  ;;; if found next matching field
		  (setq returned-member current-field)
		;;; if no more matching fields, match a method
		(setq current-method (get-first-matching
				      typed-member-symbol
				      methods
				      'compare-symbol-with-member))
		(if current-method
		    ;;; if found a matching method
		    (setq returned-member current-method)
		  ;;; if no matching method, try beginning part of fields
		  (setq current-field (get-first-matching
				       typed-member-symbol
				       fields
				       'compare-symbol-with-member))
		  (setq returned-member current-field)))))





(defun get-next-matching-dep (typed-symbol list previous-var comparator)
  "First, widdles down list of variables in scope to remove all variables that appear before an occurance of a variables that has a symbol that matches the symbol in previous-variable.  Next, removes the matching variable from the list.  Then, calls get-first-matching variable using the trimmed list.  If get-first-matching-variable returns nil, re-call get-first-matching-variable with the original scope to search the front of the list. 
THIS FUNCTION LOOPS INFINITELY IF previous-variable IS NOT IN SCOPE."
  (let ((local-list list)
	(finding-variable t)
	current-variable
	returned-variable)    					
    (while (and finding-variable local-list)
      ; widdle down list until you find the current variable
      (setq current-variable (car local-list))
      (if (funcall comparator previous-var current-variable t)
	  (setq finding-variable nil))
      (setq local-list (cdr local-list))) ;advance list pointer
    (if local-list
	(setq returned-variable (get-first-matching typed-symbol
						    local-list
						    comparator)))
    returned-variable))






(defun get-first-matching-dep (typed-symbol list comparator)
  "Loops through scope to find the first variable that has a symbol for which typed-symbol is a prefix.  Returns nil if there is no match."
  (let ((local-list list)
	(returned-var nil)
	current-var)
    (while local-list
      (setq current-var (car local-list))
      (if (funcall comparator typed-symbol current-var)
	  (progn
	    (setq returned-var current-var)
	    (setq local-list nil)) ; break loop
	(setq local-list (cdr local-list))))
    returned-var))

	    ;;; if field is nil and method is set
	    ((and (eq current-field-global nil) (not (eq current-method-global
							   nil)))
	    (progn
	      (setq current-method (get-next-matching typed-member-symbol
						     methods
						     current-method-global
						   'compare-symbol-with-member))
	      (if current-method
		  ;;; if found next matching field
		  (setq returned-member current-method)
		;;; if no more matching methods, match a field
		(setq current-field (get-first-matching
				      typed-member-symbol
				      fields
				      'compare-symbol-with-member))
		(if current-field
		    ;;; if found a matching field
		    (setq returned-member current-field)
		  ;;; if no matching method, try beginning part of fields
		  (setq current-method (get-first-matching
				       typed-member-symbol
				       methods
				       'compare-symbol-with-member))
		  (setq returned-member current-method)))))
	    
	    ;;; if both are set
	    ((t) ;;; we are in a bad state.  Reset and return nil.
	    (setq returned-member nil))))
    (setq current-field-global current-field)
    (setq current-method-global current-method)
    returned-member))





(defun parse-csv-elements-dep (buffer terminator)
  (let (element elements)
    (setq element (extract-text-up-to-char buffer ","))
    (while element
      (setq elements (cons element elements))
      (setq element (extract-text-up-to-char buffer ",")))
    (setq element (extract-text-up-to-char buffer terminator))
    (setq elements (cons element elements))
    (elements)))
	  

(defun parse-fields-dep (buffer)
  (parse-csv-elements buffer ";"))
  
  
(defun parse-methods-dep (buffer)
  (parse-csv-elements buffer "\n"))


(defun parse-line-dep (line)
  (let (components symbol fields methods)
    (setq components (split-string line ";"))
    (setq symbol (car components))
    (let ((fields-string (nth 1 components)))
      (if (string-equal "" fields-string)
	  (setq fields nil)
	(setq fields (split-string fields-string ","))))
    (let ((methods-string (nth 2 components)))
      (if (string-equal "" methods-string)
	  (setq methods nil)
	(setq methods (split-string methods-string ","))))
    (create-variable symbol fields methods)))

