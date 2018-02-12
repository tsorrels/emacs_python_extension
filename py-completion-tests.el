(funcall 'python-mode)

(load (concat default-directory "py-completion.el"))

(require 'ert)

(ert-deftest test-scope-exists ()
  "Tests whether the global scope list exists."
  (setq global-scope nil)
  (should (eq nil global-scope)))

(ert-deftest test-insert-variable-into-scope ()
  "Adds variable to the global scope."
  (let ((fields '("field1" "field2")) 
	(methods '("method1" "method2")))
    (let ((variable (create-variable "var1" fields methods))
	  scope)
      (setq scope (insert-variable variable scope))
      (let ((var (get-variable "var1" scope)))
	(should (string-equal "var1" (car var)))))))


(ert-deftest test-parse-line-no-fields-no-methods ()
  "Test parse-line with no fields and no methods."
  (let ((line "variable0;;") variable)
    (setq variable (parse-line line))
    (should (string-equal "variable0" (car variable)))
    (should (eq nil (nth 1 variable)))
    (should (eq nil (nth 2 variable)))))

(ert-deftest test-parse-line-one-field ()
  "Test parse-line with one field and no methods."
  (let ((line "variable1;field1;") variable)
    (setq variable (parse-line line))
    (should (string-equal "variable1" (car variable)))
    (should (string-equal "field1" (car (nth 1 variable))))
    (should (eq nil (nth 2 variable)))))

(ert-deftest test-parse-line-one-field-one-method ()
  "Test parse-line with one field and one method."
  (let ((line "variable1;field1;method1") variable)
    (setq variable (parse-line line))
    (should (string-equal "variable1" (car variable)))
    (should (string-equal "field1" (car (nth 1 variable))))
    (should (string-equal "method1" (car (nth 2 variable))))))

(ert-deftest test-parse-line-multiple-fields-and-methods ()
  "Test parse-line with two fields and three methods."
  (let ((line "variable1;field1,field2;method1,method2,method3") variable)
    (setq variable (parse-line line))
    (should (string-equal "variable1" (car variable)))
    (should (string-equal "field1" (car (nth 1 variable))))
    (should (string-equal "field2" (nth 1 (nth 1 variable))))
    (should (string-equal "method1" (car (nth 2 variable))))
    (should (string-equal "method2" (nth 1 (nth 2 variable))))
    (should (string-equal "method3" (nth 2 (nth 2 variable))))))

(ert-deftest test-parse-variables-buffer ()
  ""
  (let ((buffer (generate-new-buffer "test-output-buffer"))
	variables)
    (set-buffer buffer)
    (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test_variable_output.txt" nil nil nil)
    (setq variables (parse-variables-buffer buffer))
    (let ((variable (nth 2 variables)))
      (should (string-equal "variable0" (car variable))))
    (let ((variable (nth 1 variables)))
      (should (string-equal "variable1" (car variable)))
      (let ( (fields (nth 1 variable))
	     (methods (nth 2 variable)) )
	(should (string-equal "field1" (car fields)))
	(should (string-equal "method1" (car methods)))))
    (let ((variable (nth 0 variables)))
      (should (string-equal "variable2" (car variable)))
      (let ( (fields (nth 1 variable))
	     (methods (nth 2 variable)) )
	(should (string-equal "field1" (nth 0 fields)))
	(should (string-equal "field2" (nth 1 fields)))
	(should (string-equal "method1" (nth 0 methods)))
	(should (string-equal "method2" (nth 1 methods)))))))

(ert-deftest test-run-parser ()
  ""
  (setq global-scope nil)
  (let ((current-buffer (generate-new-buffer "test-text-buffer"))
	(variables nil))
    (set-buffer current-buffer)
    (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test/test_input_script.py" nil nil nil)
    (run-parser)
    (let ((var1 (get-variable "Threads" global-scope))
	  (var2 (get-variable "lock" global-scope))
	  (var3 (get-variable "connections" global-scope)))
      (should (string-equal "Threads" (car var1)))
      (should (string-equal "lock" (car var2)))
      (should (string-equal "connections" (car var3))))))


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


(ert-deftest insert-variable-into-buffer ()
  ""
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	scope
	final-text
	(current-buffer (generate-new-buffer "test-text-buffer"))
	(initial-buffer-text "variable1 = 3\nvariable2 = va"))
	
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (setq scope (insert-variable variable1 scope))
    (insert-variable-into-buffer "va" variable1)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))    
    (should (string-equal final-text (concat initial-buffer-text "riable1")))))


(ert-deftest reset-buffer ()
  ""
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	scope
	final-text
	(current-buffer (generate-new-buffer "test-text-buffer"))
	(initial-buffer-text "variable1 = 3\nvariable2 = va"))	
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (setq scope (insert-variable variable1 scope))
    (setq initial-point-global (point))
    (insert-variable-into-buffer "va" variable1)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (insert final-text)
    (should (string-equal final-text (concat initial-buffer-text "riable1")))
    (reset-buffer)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text initial-buffer-text))))


(ert-deftest complete-symbol ()
  ""
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(initial-buffer-text "variable1 = 3\nvariable2 = va")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text
	returned-variable)
    (setq global-scope nil)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (insert-into-global-scope variable1)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    ;(setq final-text (thing-at-point 'word))
    ;(should (string-equal final-text "va"))
    ;(setq returned-variable (get-matching-variable nil "va" nil global-scope))
    ;(should (string-equal "variable1" (car returned-variable)))

    ;))
    (complete-symbol)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text "riable1")))))
