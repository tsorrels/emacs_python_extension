
(load (concat default-directory "variable.el"))

(defvar current-member-type nil)

(defvar global-scope ())

(defvar typed-symbol-global nil )

(defvar initial-point-global nil)

(defvar current-symbol-global nil)

(defvar parser-exe-name (concat default-directory "parse_variables.py"))


(defun reset-globals ()
  (setq current-member-type nil)
  (setq current-symbol-global nil)
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
  (let (returned-variable current-variable)
    (while scope
      (setq current-variable (car scope))
      (setq var-symbol (get-variable-symbol current-variable))
      (if (string-equal symbol var-symbol)
	  (progn
	    (setq scope nil)
	    (setq returned-variable current-variable))
	(setq scope (cdr scope))))
    returned-variable))


(defun add-variables-to-global-scope (variables)
  "Enforce uniqueness.  If a variable is already in global scope, this function cannot add it again."
  (while variables
    (setq variable (car variables))
    (setq variable-symbol (get-variable-symbol variable))    
    (setq returned-variable (get-variable variable-symbol global-scope))
    (if returned-variable
	nil      
      (setq global-scope (cons variable global-scope)))
    (setq variables (cdr variables))))


(defun parse-symbol (buffer)
  (extract-text-up-to-char buffer ";"))


(defun parse-variables-buffer (buffer)
  (let (variables
	variable-line-pair
	variable
	text
	lines)
    (set-buffer buffer)
    (setq text (delete-and-extract-region (point-min) (point-max)))
    (setq lines (split-string text "\n"))
    (while lines
      (let ((line (car lines)))
	(if (string-equal "" line)
	    (setq lines (cdr lines)) ;;; pass
	  
	  (setq variable-line-pair (parse-variable line))
	  (setq variable (car variable-line-pair))
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


(defun get-first-matching (typed-symbol list previous-var comparator &optional exact)
  "Loops through scope to find the first variable that has a symbol for which typed-symbol is a prefix.  Returns nil if there is no match."
  (let ((local-list list)
	(returned-var nil)
	current-var)
    (while local-list
      (setq current-var (car local-list))
      (if (funcall comparator typed-symbol current-var exact)
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
    (while (and finding-variable local-list)
      ; widdle down list until you find the current variable
      (setq current-variable (car local-list))
      (if (funcall comparator previous-var current-variable t)
	  (setq finding-variable nil))
      (setq local-list (cdr local-list))) ;advance list pointer
    (if local-list
	(setq returned-variable (get-first-matching typed-symbol
						    local-list
						    previous-var
						    comparator)))
    returned-variable))


(defun insert-symbol-into-buffer (typed-symbol full-symbol)
  (let ((symbol-length (length typed-symbol)))	
    (let ((text-to-insert (substring full-symbol symbol-length nil)))
      (insert text-to-insert))))


(defun get-root-variable (symbol scope)
  (get-first-matching typed-symbol scope nil 'compare-symbol-with-variable))


(defun get-matching-variable (symbols scope)
  (let (current-symbol
	current-scope
	local-symbols
	current-variable
	returned-variable
	missed-last-symbol)
    (setq local-symbols symbols)
    (setq current-scope scope)
    (while local-symbols
      (setq next-symbol (car local-symbols))
      (if missed-last-symbol
	  (setq current-symbol (concat current-symbol "." next-symbol))
	(setq current-symbol (car local-symbols)))
      (setq current-variable (get-first-matching current-symbol
						 current-scope
						 nil
						 'compare-symbol-with-variable
						 t))
      (if (eq nil current-variable)
	  (setq missed-last-symbol t)
	(setq missed-last-symbol nil)
	(setq current-scope (get-variable-variables current-variable)))
      (setq local-symbols (cdr local-symbols)))
    current-variable))


(defun get-first-matching-member (current-variable typed-member-symbol)
  (let (symbol)
    ;;; look through fields    
    (setq symbol (get-first-matching typed-member-symbol
				       (get-variable-fields current-variable)
				       nil
				     'compare-symbol-with-member))
    (setq current-member-type 'field)
    ;;; look through methods
    (if (not symbol)
	(progn
	  (setq symbol (get-first-matching typed-member-symbol
				     (get-variable-methods current-variable)
				     nil
				     'compare-symbol-with-member))
	  (setq current-member-type 'method)))

    ;;; look through variables   
    (if (not symbol)
	(progn
	  (setq variable (get-first-matching typed-member-symbol
			       	       (get-variable-variables current-variable)
				       nil
				       'compare-symbol-with-variable))
	  (if variable
	      (progn
		(setq symbol (get-variable-symbol variable))
		(setq current-member-type 'variable))
	    (setq current-member-type nil))))
    symbol))


(defun get-getter (member-type-a member-type-b)
  (if (eq member-type-a member-type-b)
      'get-next-matching
    'get-first-matching))


(defun build-search-groups (variable member-type)
  (let (search-groups)
    (setq fields-search-group (list (get-variable-fields variable)
				    'compare-symbol-with-member
				    'field
				    (get-getter 'field member-type)))
    (setq methods-search-group (list (get-variable-methods variable)
				     'compare-symbol-with-member
				     'method
				     (get-getter 'method member-type)))
    (setq variables-search-group (list (get-variable-variables variable)
				       'compare-symbol-with-variable
				       'variable
				       (get-getter 'variable member-type)))

    
    (if (eq 'variable member-type)
	(setq search-groups (list variables-search-group)))
    
    (if (eq 'method member-type)
	(progn 
	  (setq search-groups (list variables-search-group))
	  (setq search-groups (cons methods-search-group
				    search-groups))))
    (if (eq 'field member-type)
	(progn 
	  (setq search-groups (list variables-search-group))
	  (setq search-groups (cons fields-search-group
				    (cons methods-search-group
					  search-groups)))))
    search-groups))
          
	
(defun get-next-matching-member (current-variable typed-symbol current-symbol)
  (let (symbol)
    (setq search-groups (build-search-groups current-variable 
					     current-member-type))

    (while search-groups
      (setq search-group (car search-groups))
      (setq search-list (nth 0  search-group))
      (setq comparator (nth 1 search-group))
      (setq member-type (nth 2 search-group))
      (setq getter (nth 3 search-group))
      (setq returned-match (funcall getter typed-symbol
			           search-list
			           current-symbol
			           comparator))
      ;;; TODO: this is a hack
      (if (and returned-match (eq 'variable member-type))
	  (setq symbol (get-variable-symbol returned-match))
	(setq symbol returned-match))
      (if symbol
	  (progn
	    (setq current-member-type member-type)
	    (setq search-groups nil))
	(setq search-groups (cdr search-groups))))

    (if (not symbol)
	(setq symbol (get-first-matching-member current-variable typed-symbol)))

    symbol))      


(defun get-matching-member-and-set-globals (current-variable
					    typed-member-symbol
					    subsequent-call)
  (let (returned-member-symbol)
    (if (not subsequent-call) ;;; just get first matching field or method
	(setq returned-member-symbol (get-first-matching-member
				      current-variable
				      typed-member-symbol)))
    (if subsequent-call
	(setq returned-member-symbol (get-next-matching-member
				      current-variable
				      typed-member-symbol
				      current-symbol-global)))
    (setq current-symbol-global returned-member-symbol)
    returned-member-symbol))
	  
	

(defun reset-buffer ()
  "Removes text inserted during last autocomplete and resets point.
Accesses global variable 'initial-point-global' and 'point."
  (delete-and-extract-region initial-point-global (point))
  (goto-char initial-point-global))


(defun rebuild-symbol (symbols completed-symbol)
  (let (returned-symbol
	current-symbol
	remaining-symbols)
      (setq remaining-symbols symbols)

      ;;; do once
      (while remaining-symbols
	(setq current-symbol (car remaining-symbols))
	(setq returned-symbol (concat returned-symbol current-symbol "."))
	(setq remaining-symbols (cdr remaining-symbols)))
      (setq returned-symbol (concat returned-symbol completed-symbol))
    returned-symbol))


(defun get-complete-symbols (typed-symbol)
  (setq symbol-components (split-string typed-symbol "\\."))
  (reverse (cdr (reverse symbol-components))))


(defun get-last-symbol (typed-symbol)
  (setq symbol-components (split-string typed-symbol "\\."))
  (car (reverse symbol-components)))
  

(defun get-symbol-and-set-globals (subsequent-call typed-symbol)
  (let (symbol-components
	complete-symbols
	last-symbol
	current-variable
	next-member-symbol)
    (setq complete-symbols (get-complete-symbols typed-symbol))
    (setq last-symbol (get-last-symbol typed-symbol))
    (setq root-variable (create-variable nil nil nil global-scope))
    ;;; just find the variable
    (if complete-symbols
	;;; if there are complete symbols, go get the variable
	(setq current-variable (get-matching-variable
				complete-symbols
				global-scope))
      ;;; if no completed symbols just use 'root variable' with global-scope
      (setq current-variable root-variable))
    (setq next-member-symbol (get-matching-member-and-set-globals
			      current-variable
			      last-symbol
			      subsequent-call))
    (setq current-symbol-global next-member-symbol)
    (if next-member-symbol
	(rebuild-symbol complete-symbols next-member-symbol)
      typed-symbol)))
    

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
	next-symbol)
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
  (let (subsequent-call)
    (if last-command
	(if (or (eq last-command 'complete-symbol)
		(eq last-command 'debug-on-entry))
	    (setq subsequent-call t)))
    (complete-symbol-helper subsequent-call)))

  
(defun parse-variables ()
  ;;; TODO: move global-scope reset
  (setq global-scope nil)
  (run-parser))


(defun newline-parse-variable ()
  (interactive)
  (newline)
  (indent-for-tab-command)
  (parse-variables))


(define-key python-mode-map (kbd "<backtab>") 'complete-symbol)
(define-key python-mode-map (kbd "RET") 'newline-parse-variable)
