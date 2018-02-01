

(defvar global-scope ())

(defvar parser-exe-name "python2 /home/tsorrels/Documents/repos/emacs_python_extension/test_variable_output.txt/parse_variables.py")

(defun create-variable (symbol fields methods)
  "Generate a list specially formated list to represent a variable.
The first element of the list is a string that represents the variable symbol.
The second element is a list of strings which are the variable's fields.
The third element is a list of strings which are the variable's methods."
  (let (empty-list)
    (cons symbol (cons fields (cons methods empty-list)))))

(defun insert-variable (var scope)
  (setq scope (cons var scope)))

(defun get-variable (symbol scope)
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

(defun parse-variable ()
  (message "Ran parse-variable"))

(defun newline-parse-variable ()
  (interactive)
  (newline)
  (parse-variable))

(global-set-key (kbd "RET") 'newline-parse-variable)


(defun add-variables-to-global-scope ()
  )



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
  (let ((output-buffer (generate-new-buffer "parser")))
    (save-excursion	
      (call-process-region point-min point-max parser-exe-name nil output-buffer)
      (let ((variables (parse-variables-buffer (output-buffer))))
	(add-variables-to-global-scope (variables))))))
      
