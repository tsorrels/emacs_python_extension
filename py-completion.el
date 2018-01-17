

(defvar global-scope ())

(defun create-variable (symbol fields methods)
  (cons symbol (cons fields methods)))

(defun insert-variable (var scope)
  (setq scope (cons scope var)))

(defun get-variable (symbol scope)
  (let ((list-ptr scope))
    (while list-ptr
      (let ((var car list-ptr))
	(if (eq symbol (car list-ptr))
	    (progn
	      (setq list-ptr nil)
	      var)
	  (setq list-ptr (cdr list-ptr)))))))

(defun parse-variable ()
  (message "Ran parse-variable"))

(defun newline-parse-variable ()
  (interactive)
  (newline)
  (parse-variable))

(global-set-key (kbd "RET") 'newline-parse-variable)
