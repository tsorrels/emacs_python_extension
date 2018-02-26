(funcall 'python-mode)

(load (concat default-directory "py-completion.el"))

(require 'ert)

(ert-deftest get-first-matching-variable-one-variable-in-scope ()
  ""
  (let ((fields '("field1" "field2")) 
	(methods '("method1" "method2"))
	variable
	scope
	returned-variable)    
    (setq variable (create-variable "var1" fields methods))
    (setq scope (insert-variable variable scope))
    (setq returned-variable (get-first-matching-variable "va" scope))
    (should (string-equal "var1" (car returned-variable)))))

(ert-deftest get-first-matching-variable-two-variable-in-scope ()
  "Tests get-first-matching-variable with a scope containing two variables, both which match the typed symbol.  Variables are pre-pended when inserted into a scope, so the last variable added to the scope is the first to match."
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	scope
	returned-variable)
    (let ((fields2 '("field1" "field2")) 
	  (methods2 '("method1" "method2"))
	  variable2
	  scope
	  returned-variable)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq variable2 (create-variable "var2" fields1 methods1))
      (setq scope (insert-variable variable1 scope))
      (setq scope (insert-variable variable2 scope))
      (setq returned-variable (get-first-matching-variable "va" scope))
      (should (string-equal "var2" (car returned-variable))))))

(ert-deftest get-first-matching-variable-second-is-match ()
  "Tests get-first-matching-variable with two variables, the last in the scope which matches the typed-symbol"
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1)
    (let ((fields2 '("field1" "field2")) 
	  (methods2 '("method1" "method2"))
	  variable2
	  scope
	  returned-variable)    
      (setq variable1 (create-variable "a-var" fields1 methods1))
      (setq variable2 (create-variable "b-var" fields2 methods2))
      (setq scope (insert-variable variable1 scope))
      (setq scope (insert-variable variable2 scope))
      (setq returned-variable (get-first-matching-variable "a-" scope))
      (should (string-equal "a-var" (car returned-variable))))))


(ert-deftest get-first-matching-variable-no-match ()
  ""
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	scope
	returned-variable)
    (let ((fields2 '("field1" "field2")) 
	  (methods2 '("method1" "method2"))
	  variable2
	  scope
	  returned-variable)    
      (setq variable1 (create-variable "a-var" fields1 methods1))
      (setq variable2 (create-variable "b-var" fields1 methods1))
      (setq scope (insert-variable variable1 scope))
      (setq scope (insert-variable variable2 scope))
      (setq returned-variable (get-first-matching-variable "c-" scope))
      (should (eq nil returned-variable)))))


(ert-deftest get-first-matching-variable-null-scope ()
  "Test get-first-matching-variable returns null when searching for a match for a given symbol when no variables are in scope (scope is nil)"
  (let (scope
	returned-variable)    
    (setq returned-variable (get-first-matching-variable "va" scope))
    (should (eq nil returned-variable))))


(ert-deftest get-next-matching-variable-previous-var-second-next-matches ()
  "Tests get-next-matching-variable with scope of three variables, all match typed symbol.  Previous variable is variable2, positioned in the middle of the scope, and get-next-matching-variable should thus return variable1, located at the end of the scope."
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(fields2 '("field1" "field2")) 
	(methods2 '("method1" "method2"))
	variable3
	(fields3 '("field1" "field2")) 
	(methods3 '("method1" "method2"))
	variable2
	scope
	returned-variable)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq variable2 (create-variable "var2" fields2 methods2))
      (setq variable3 (create-variable "var3" fields3 methods3))
      (setq scope (insert-variable variable1 scope))
      (setq scope (insert-variable variable2 scope))
      (setq returned-variable (get-next-matching-variable "v" scope variable2))
      (should (string-equal "var1" (car returned-variable)))))

(ert-deftest get-next-matching-variable-prev-variable-first-next-matches ()
  "Tests get-next-matching-variable with scope of three variables, all match typed symbol.  Previous variable is variable3, positioned in the beginning of the scope, and get-next-matching-variable should thus return variable2, positioned next and in the middle of the three-variable scope."
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(fields2 '("field1" "field2")) 
	(methods2 '("method1" "method2"))
	variable2
	(fields3 '("field1" "field2")) 
	(methods3 '("method1" "method2"))
	variable3
	scope
	returned-variable)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq variable2 (create-variable "var2" fields2 methods2))
      (setq variable3 (create-variable "var3" fields3 methods3))
      (setq scope (insert-variable variable1 scope))
      (setq scope (insert-variable variable2 scope))
      (setq scope (insert-variable variable3 scope))
      (setq returned-variable (get-next-matching-variable "v" scope variable3))
      (should (string-equal "var2" (car returned-variable)))))


(ert-deftest get-next-matching-variable-prev-variable-last-second-matches ()
  "Tests get-next-matching-variable with scope of three variables, all match typed symbol.  Previous variable is variable3, positioned in the beginning of the scope, and get-next-matching-variable should thus return variable2, positioned next and in the middle of the three-variable scope."
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(fields2 '("field1" "field2")) 
	(methods2 '("method1" "method2"))
	variable2
	(fields3 '("field1" "field2")) 
	(methods3 '("method1" "method2"))
	variable3
	scope
	returned-variable)    
      (setq variable1 (create-variable "a-var1" fields1 methods1))
      (setq variable2 (create-variable "a-var2" fields2 methods2))
      (setq variable3 (create-variable "b-var3" fields3 methods3))
      (setq scope (insert-variable variable1 scope))
      (setq scope (insert-variable variable2 scope))
      (setq scope (insert-variable variable3 scope))
      (setq returned-variable (get-next-matching-variable "a-" scope variable1))
      (should (string-equal "a-var2" (car returned-variable)))))
