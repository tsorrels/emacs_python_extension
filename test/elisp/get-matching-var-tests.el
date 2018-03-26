(funcall 'python-mode)

(load (concat default-directory "py-completion.el"))

(funcall 'lisp-mode)

(require 'ert)

(ert-deftest get-matching-variable_single-variable_compound-symbol ()
  ""
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	scope
	returned-variable)
    (setq variable1 (create-variable "var.variable1" fields1 methods1))
    (setq symbols '("var" "variable1"))
    (setq scope (insert-variable variable1 scope))
    (setq returned-variable (get-matching-variable symbols scope))
    (should (string-equal "var.variable1"
			  (get-variable-symbol returned-variable)))))




(ert-deftest get-matching-variable_single-variable_compound-symbol ()
  ""
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	scope
	returned-variable)
    (setq variable2 (create-variable "var2.variable2" fields1 methods1))
    (setq variable1 (create-variable "var1.variable1"
				     fields1
				     methods1
				     (list variable2)))
    (setq symbols '("var1" "variable1" "var2" "variable2"))
    (setq scope (insert-variable variable1 scope))
    (setq returned-variable (get-matching-variable symbols scope))
    (should (string-equal "var2.variable2"
			  (get-variable-symbol returned-variable)))))

