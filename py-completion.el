
(load (concat default-directory "variable.el"))

(defvar global-scope ())

(defvar current-variable-global (car global-scope))

(defvar current-field-global nil)

(defvar current-method-global nil)

(defvar typed-symbol-global nil )

(defvar initial-point-global nil)

;(defvar parser-exe-name "/home/tsorrels/Documents/repos/emacs_python_extension/parse_variables.py")

(defvar parser-exe-name (concat default-directory "parse_variables.py"))



(defun reset-globals ()
  (setq current-variable-global nil)
  (setq current-field-global nil)
  (setq current-method-global nil))
    

(defun insert-into-global-scope (var)
  (setq global-scope (cons var global-scope)))

(defun insert-variable (var scope)
  (setq scope (cons var scope)))


(defun get-variable (symbol scope)
  "Retreives the variable that has a matching symbol from the scope.
The variable returned is a specially formated list.
The first element of the list is a string that represents the variable symbol.
The second element is a list of strings which are the variable's fields.
The third element is a list of strings which are the variable's methods.
This method exists to use as a tool in testing."
  (let ((list-ptr scope) var var-symbol)
    (while list-ptr
      (setq var (car list-ptr))
      (setq var-symbol (car var))
      (if (string-equal symbol var-symbol)
	  (setq list-ptr nil)
	(setq list-ptr (cdr list-ptr))))
    var))


(defun add-variables-to-global-scope (variables)
  (setq global-scope (append variables global-scope)))


(defun parse-symbol (buffer)
  (extract-text-up-to-char buffer ";"))


(defun parse-csv-elements (buffer terminator)
  (let (element elements)
    (setq element (extract-text-up-to-char buffer ","))
    (while element
      (setq elements (cons element elements))
      (setq element (extract-text-up-to-char buffer ",")))
    (setq element (extract-text-up-to-char buffer terminator))
    (setq elements (cons element elements))
    (elements)))
	  

(defun parse-fields (buffer)
  (parse-csv-elements buffer ";"))
  
  
(defun parse-methods (buffer)
  (parse-csv-elements buffer "\n"))


(defun parse-line (line)
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


(defun parse-variables-buffer (buffer)
  (let (variables variable text lines)
    (set-buffer buffer)
    (setq text (delete-and-extract-region (point-min) (point-max)))
    (setq lines (split-string text "\n"))
    (while lines
      (let ((line (car lines)))
	(if (string-equal "" line)
	    (setq lines (cdr lines)) ;;; pass
	  
	  (setq variable (parse-line line))
	  (setq variables (cons variable variables))
	  (setq lines (cdr lines)))))
    variables))
    

(defun run-parser ()
  (let ((buffer (generate-new-buffer "parser")))
    (save-excursion	
      (call-process-region (point-min) (point-max) parser-exe-name nil buffer)
      (let ((variables (parse-variables-buffer buffer)))
	(add-variables-to-global-scope variables )))))


(defun compare-symbol-with-variable (typed-symbol variable &optional exact)
  (let (comparator)
    (cond (exact
	   (setq comparator 'string-equal))
	  (t
	   (setq comparator 'string-prefix-p)))
    (if (funcall comparator typed-symbol (get-variable-symbol variable))
	t
      nil)))

(defun compare-symbol-with-member (typed-symbol member &optional exact)
  (let (comparator)
    (cond (exact
	   (setq comparator 'string-equal))
	  (t
	   (setq comparator 'string-prefix-p)))
    (if (funcall comparator typed-symbol member)
	t
      nil)))


(defun get-first-matching (typed-symbol list comparator)
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

	
(defun get-next-matching (typed-symbol list previous-var comparator)
  "First, widdles down list of variables in scope to remove all variables that appear before an occurance of a variables that has a symbol that matches the symbol in previous-variable.  Next, removes the matching variable from the list.  Then, calls get-first-matching variable using the trimmed list.  If get-first-matching-variable returns nil, re-call get-first-matching-variable with the original scope to search the front of the list. 
THIS FUNCTION LOOPS INFINITELY IF previous-variable IS NOT IN SCOPE."
  (let ((local-list list)
	(finding-variable t)
	current-variable
	returned-variable)    					
    (while finding-variable
      ; widdle down list until you find the current variable
      (setq current-variable (car local-list))
      (if (funcall comparator previous-var current-variable t)
	  (setq finding-variable nil))
      (setq local-list (cdr local-list))) ;advance list pointer
    (setq returned-variable (get-first-matching typed-symbol
						local-list
						comparator))
    returned-variable))


(defun insert-symbol-into-buffer (typed-symbol full-symbol)
  (let ((symbol-length (length typed-symbol)))	
    (let ((text-to-insert (substring full-symbol symbol-length nil)))
      (insert text-to-insert))))


(defun get-matching-variable (subsequent-call typed-symbol current-var scope)
  (if subsequent-call
      (get-next-matching typed-symbol
			 scope
			 (get-variable-symbol current-var)
			 'compare-symbol-with-variable)
    (get-first-matching typed-symbol scope 'compare-symbol-with-variable)))


(defun get-first-matching-member (typed-member-symbol list-of-members)
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
    

(defun get-matching-member-and-set-globals (current-variable
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
    

(defun reset-buffer ()
  "Removes text inserted during last autocomplete and resets point.
Accesses global variable 'initial-point-global' and 'point."
  (delete-and-extract-region initial-point-global (point))
  (goto-char initial-point-global))


(defun get-symbol-and-set-globals (subsequent-call typed-symbol)
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


(defun get-typed-symbol ()
  (let (saved-point
	symbol-start-points
	typed-symbol)
    (setq saved-point (point))
    (setq symbol-start-point (re-search-backward "[
[:space:]]"  nil t))
    (if (eq nil symbol-start-point) ;;; this typed symbol is at beginning of buf
	(setq symbol-start-point (point-min))
      ;;; if successful, point is left on white space character
      (setq symbol-start-point (+ symbol-start-point 1)))

    (setq typed-symbol (buffer-substring symbol-start-point saved-point))
    (goto-char saved-point)
    typed-symbol))

	

(defun complete-symbol-helper (subsequent-call)
  (let (typed-symbol
	next-symbol
	typing-new-variable)
    (if subsequent-call
	(reset-buffer) ;;; reset buffer
      (reset-globals)) ;;; if this is a new call, reset state
    (setq typed-symbol (get-typed-symbol)) ;;; get typed-symbol
    (if typed-symbol
	(progn
	  (setq initial-point-global (point)) ;;; save point before auto-compl
	  (setq typed-symbol-global typed-symbol)
	  (setq next-symbol (get-symbol-and-set-globals subsequent-call
							typed-symbol))
	  (insert-symbol-into-buffer typed-symbol
				     next-symbol)))))

  
(defun complete-symbol ()
  "Interactive function that serves as a wrapper for the autocompletion functionality.  The function determines whether this is a subsequent call to complete-symbol command by comparing last-command with this-command and then calls complete-symbol-helper, passing nil or t for subsequent-call."
  (interactive)
  ;(funcall 'elisp-mode)
 
  (let (subsequent-call)
					;(if (eq last-command this-command)
    (if last-command
	(if (eq last-command 'complete-symbol)
	    (setq subsequent-call t)))
    (complete-symbol-helper subsequent-call)))
    ;(funcall 'python-mode))


  
(defun parse-variable ()
  (run-parser)
  (message "Ran parse-variable"))


(defun newline-parse-variable ()
  (interactive)
  (newline)
  ;(indent-for-tab-command)
  (parse-variable))

(define-key python-mode-map (kbd "<backtab>") 'complete-symbol)
(define-key python-mode-map (kbd "RET") 'newline-parse-variable)
