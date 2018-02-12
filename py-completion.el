
(defvar global-scope ())

(defvar current-variable-global (car global-scope))

(defvar typed-symbol-global nil )

(defvar initial-point-global nil)

(defvar parser-exe-name "/home/tsorrels/Documents/repos/emacs_python_extension/parse_variables.py")

(defun create-variable (symbol fields methods)
  "Generate a list specially formated list to represent a variable.
The first element of the list is a string that represents the variable symbol.
The second element is a list of strings which are the variable's fields.
The third element is a list of strings which are the variable's methods."
  (let (empty-list)
    (cons symbol (cons fields (cons methods empty-list)))))

(defun insert-into-global-scope (var)
  (setq global-scope (cons var global-scope)))

(defun insert-variable (var scope)
  (setq scope (cons var scope)))


(defun get-variable (symbol scope)
  "Retreives the variable that has a matching symbol from the scope.
The variable returned is a specially formated list.
The first element of the list is a string that represents the variable symbol.
The second element is a list of strings which are the variable's fields.
The third element is a list of strings which are the variable's methods."
  (let ((list-ptr scope) var var-symbol)
    (while list-ptr
      (setq var (car list-ptr))
      (setq var-symbol (car var))
      (if (string-equal symbol var-symbol)
	  (progn
	    (setq list-ptr nil)
	    (message "found variable."))
	(progn
	  (setq list-ptr (cdr list-ptr))
	  (message "didn't find variable."))))      
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


(defun extract-text-up-to-char (buffer char)
  (let (begin end)
    (beginning-of-buffer)
    (setq begin point)
    (search-forward char nil t)
    (setq end point)
    (delete-and-extract-region begin end)))


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


(defun list-variables ()
  (interactive)
  (message (car (car global-scope))))
  ;(message "Ran list-variables"))


(defun fetch-variable ()
  (interactive)
  (if (eq last-command this-command)
      ; use current typed-symbol-global
      ; get next matching symbol
      (progn
	(nil))
	
					; get new symbol
					; get first matching symbol
    (let ((typed-symbol-global (thing-at-point 'word)))
      ()
    )
    ))


(defun get-first-matching-variable (typed-symbol-local scope)
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
	
(defun get-next-matching-variable (typed-symbol scope previous-variable)
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

(defun insert-variable-into-buffer (typed-symbol-local variable)
  (let ((symbol-length (length typed-symbol-local))
	(variable-symbol (car variable)))
    (let ((text-to-insert (substring variable-symbol symbol-length nil)))
      (insert text-to-insert))))

(defun get-matching-variable (subsequent-call typed-symbol current-var scope)
  (if subsequent-call
      (get-next-matching-variable typed-symbol scope current-var)
    (get-first-matching-variable typed-symbol scope)))


(defun reset-buffer ()
  "Removes text inserted during last autocomplete and resets point.
Accesses global variable 'initial-point-global' and 'point."
  (delete-and-extract-region initial-point-global (point))
  (goto-char initial-point-global))

(defun complete-symbol ()
  (interactive)
  (let (typed-symbol-local
	subsequent-call
	next-variable)
    (if (eq last-command this-command)
	(progn
	  (setq subsequent-call t)
	  (reset-buffer)))
    (setq typed-symbol-local (thing-at-point 'word)) ; get typed-symbol
    (if typed-symbol-local
	(progn
	  (setq initial-point-global (point))
	  (setq typed-symbol-global typed-symbol-local)
	  (setq next-variable (get-matching-variable subsequent-call
						     typed-symbol-local
						     current-variable-global
						     global-scope))	  
	  ;;; replace word
	  ;;; save typed symbol
	  ;;; save returned symbol/variable
	  ;;; save point before auto-completion
	  (if next-variable
	      (progn
		(setq current-variable-global next-variable)
		(insert-variable-into-buffer typed-symbol-local
					     next-variable)))))))

  
  
(defun parse-variable ()
  (run-parser)
  (message "Ran parse-variable"))


(defun newline-parse-variable ()
  (interactive)
  (newline)
  (indent-for-tab-command)
  (parse-variable))

(define-key python-mode-map (kbd "<backtab>") 'complete-symbol)
(define-key python-mode-map (kbd "RET") 'newline-parse-variable)
