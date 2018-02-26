(defun insert-variable-into-buffer-dep (typed-symbol-local variable)
  (let ((symbol-length (length typed-symbol-local))
	(variable-symbol (car variable)))
    (let ((text-to-insert (substring variable-symbol symbol-length nil)))
      (insert text-to-insert))))


(defun get-next-matching-variable-dep (typed-symbol scope previous-variable)
  "First, widdles down list of variables in scope to remove all variables that appear before an occurance of a variables that has a symbol that matches the symbol in previous-variable.  Next, removes the matching variable from the list.  Then, calls get-first-matching variable using the trimmed list.  If get-first-matching-variable returns nil, re-call get-first-matching-variable with the original scope to search the front of the list. 
THIS FUNCTION LOOPS INFINITELY IF previous-variable IS NOT IN SCOPE."
  (let ((variable-symbol (car previous-variable))
	(variables-list scope)
	(finding-variable t)
	current-variable
	current-symbol
	returned-variable)    					
    (while finding-variable
      ; widdle down list until you find the current variable
      (setq current-variable (car variables-list))
      (setq current-symbol (car current-variable))
      (if (string-equal current-symbol variable-symbol)
	  (setq finding-variable nil))
      (setq variables-list (cdr variables-list))) ;advance list pointer
    (setq returned-variable (get-first-matching-variable typed-symbol
							 variables-list))
    (if (eq returned-variable nil)
	; loop back to the beginning of the scope
	(setq returned-variable (get-first-matching-variable typed-symbol
							     scope)))
    returned-variable))

(defun get-first-matching-variable-dep (typed-symbol-local scope)
  "Loops through scope to find the first variable that has a symbol for which typed-symbol is a prefix.  Returns nil if there is no match."
  (let ((local-scope scope)
	(returned-var nil)
	current-var
	current-symbol)
    (while local-scope
      (setq current-var (car local-scope))
      (setq current-symbol (car current-var))
      (if (string-prefix-p typed-symbol-local current-symbol t)
	  (progn
	    (setq returned-var current-var)
	    (setq local-scope nil)) ; break loop
	(setq local-scope (cdr local-scope))))
    ;returned-symbol))
    returned-var))

(defun extract-text-up-to-char-dep (buffer char)
  (let (begin end)
    (beginning-of-buffer)
    (setq begin point)
    (search-forward char nil t)
    (setq end point)
    (delete-and-extract-region begin end)))



(defun get-matching-symbol-dep (subsequent-call typed-symbol current-var scope )
  (let (returned-symbol
	returned-variable)
    (if (check-complete-member typed-symbol)
	nil
      (progn
	(setq returned-variable (get-matching-variable subsequent-call
						     typed-symbol
						     current-variable-global
						     global-scope))
	(setq returned-symbol (get-variable-symbol returned-variable))))
    returned-symbol))


(defun symbol-has-accessor-dep(typed-symbol)
  (let (components)
    (setq components (split-string typed-symbol "\\."))
    (if (eq (length components) 2)
	(progn
	  (if (not (string-equal (car components) ""))
	      t))
      nil)))
