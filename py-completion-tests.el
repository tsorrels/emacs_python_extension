(funcall 'python-mode)

(load (concat default-directory "py-completion.el"))

(funcall 'lisp-mode)


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

(ert-deftest test-parse-line-import-socket ()
  "Test parse-line with two fields and three methods."
  (let ((line "variable1;field1,field2;method1,method2,method3") variable)
    (setq variable (parse-line line))
    (should (string-equal "variable1" (car variable)))
    (should (string-equal "field1" (car (nth 1 variable))))
    (should (string-equal "field2" (nth 1 (nth 1 variable))))
    (should (string-equal "method1" (car (nth 2 variable))))
    (should (string-equal "method2" (nth 1 (nth 2 variable))))
    (should (string-equal "method3" (nth 2 (nth 2 variable))))))

(ert-deftest test-parse-line-import-test-module ()
  "Test parse-line with two fields and three methods."
  (let ((line "test;test_module;") variable)
    (setq variable (parse-line line))
    (should (string-equal "test" (get-variable-symbol variable)))
    (should (string-equal "test_module" (car (get-variable-fields variable))))))

(ert-deftest test-get-typed-symbol ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import test.test_module\nte")
	(typed-symbol "te")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	typed-symbol)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (setq typed-symbol (get-typed-symbol))
    (should (string-equal "te" typed-symbol))))


(ert-deftest test-get-typed-symbol-after-parse ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import test.test_module")
	(typed-symbol "te")
	(current-buffer (generate-new-buffer "test-text-buffer")))
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (newline-parse-variable)
    (insert typed-symbol)
    (setq typed-symbol (get-typed-symbol))
    (should (string-equal "te" typed-symbol))))



(ert-deftest test-parse-variables-buffer-socket-variable ()
  ""
  (let ((buffer (generate-new-buffer "test-output-buffer"))
	variables
        variable
        fields
        current-field
        methods
        current-method
        found-field
        found-method)   
    (set-buffer buffer)
    (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test/import_socket_parser_output.txt" nil nil nil)
    (setq variables (parse-variables-buffer buffer))
    (setq variable (car variables)) ;there should be only one variable
    (should (string-equal "socket" (get-variable-symbol variable)))
    (setq fields (get-variable-fields variable))
    (setq methods (get-variable-methods variable))
    (while fields
      (setq current-field (car fields))
      (if (string-equal "EAI_SYSTEM" current-field)
	  (progn
	    (setq found-field current-field)
	    (setq fields nil))
	(setq fields (cdr fields))))
    (while methods
      (setq current-method (car methods))
      (if (string-equal "inet_pton"  current-method)
	  (progn
	    (setq found-method current-method)
	    (setq methods nil))
	(setq methods (cdr methods))))     
    (should (string-equal "socket" (get-variable-symbol variable)))
    (should (string-equal "EAI_SYSTEM" found-field))
    (should (string-equal "inet_pton" found-method))))


(ert-deftest test-parse-variables-buffer-test-module-variable ()
  ""
  (let ((buffer (generate-new-buffer "test-output-buffer"))
	variables
        variable
        fields
        current-field
        found-field
        found-method)   
    (set-buffer buffer)
    (insert "test;;")
    (setq variables (parse-variables-buffer buffer))
    (setq variable (car variables)) ;there should be only one variable
    (should (string-equal "test" (get-variable-symbol variable)))
    (setq fields (get-variable-fields variable))
    (should (eq fields nil))))


(ert-deftest test-parse-variables-buffer-test-package-variable ()
  ""
  (let ((buffer (generate-new-buffer "test-output-buffer"))
	variables
        variable
        fields
        current-field
        found-field
        found-method)   
    (set-buffer buffer)
    (insert "test.test_module;TestClass,variable;test_function")
    (setq variables (parse-variables-buffer buffer))
    (setq variable (car variables)) ;there should be only one variable
    (should (string-equal "test.test_module" (get-variable-symbol variable)))
    (setq fields (get-variable-fields variable))
    (while fields
      (setq current-field (car fields))
      (if (string-equal "TestClass" current-field)
	  (progn
	    (setq found-field current-field)
	    (setq fields nil))
	(setq fields (cdr fields))))
    (should (string-equal "test.test_module" (get-variable-symbol variable)))
    (should (string-equal "TestClass" found-field))))

   
(ert-deftest test-parse-variables-buffer-multiple-variables ()
  ""
  (let ((buffer (generate-new-buffer "test-output-buffer"))
        sock
	variables)
    (set-buffer buffer)
    (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test/test_variable_parser_output.txt" nil nil nil)
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
      (let ( (fields (get-variable-fields variable))
	     (methods (get-variable-methods variable)) )
	(should (string-equal "field1" (nth 0 fields)))
	(should (string-equal "field2" (nth 1 fields)))
	(should (string-equal "method1" (nth 0 methods)))
	(should (string-equal "method2" (nth 1 methods)))))))


(ert-deftest test-run-parser-full-script ()
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
      (should (string-equal "Threads" (get-variable-symbol var1)))
      (should (string-equal "lock" (get-variable-symbol var2)))
      (should (string-equal "connections" (get-variable-symbol var3))))))

(ert-deftest test-run-parser-import-script ()
  ""
  (setq global-scope nil)
  (let ((current-buffer (generate-new-buffer "test-text-buffer"))
	(variables nil))
    (set-buffer current-buffer)
    (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test/test_import_script.py" nil nil nil)
    (run-parser)
    (let ((sock (get-variable "socket" global-scope))
	  fields
	  methods
	  found-field
	  found-method
	  current-field
	  current-method)
      (setq fields (get-variable-fields sock))
      (setq methods (get-variable-methods sock))
      (should (string-equal "socket" (get-variable-symbol sock)))
      (while fields
	(setq current-field (car fields))
	(if (string-equal "EAI_SYSTEM" current-field)
	    (progn
	      (setq found-field current-field)
	      (setq fields nil))
	  (setq fields (cdr fields))))
      (while methods
	(setq current-method (car methods))
	(if (string-equal "inet_pton"  current-method)
	    (progn
	      (setq found-method current-method)
	      (setq methods nil))
	  (setq methods (cdr methods))))     
      (should (string-equal "socket" (get-variable-symbol sock)))
      (should (string-equal "EAI_SYSTEM" found-field))
      (should (string-equal "inet_pton" found-method)))))


(ert-deftest insert-symbol-into-buffer ()
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
    (insert-symbol-into-buffer "va" (get-variable-symbol variable1))
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
    (insert-symbol-into-buffer "va" (get-variable-symbol variable1))
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (insert final-text)
    (should (string-equal final-text (concat initial-buffer-text "riable1")))
    (reset-buffer)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text initial-buffer-text))))


(ert-deftest complete-symbol-helper-not-subsequent-two-variables-declared ()
  ""
  (reset-globals)
  (setq global-scope nil)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	a-variable2
	(initial-buffer-text "variable1 = 3\nvariable2 = va")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text
	returned-variable)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (setq a-variable2 (create-variable "a-variable2" nil nil))
    (insert-into-global-scope a-variable2)
    (insert-into-global-scope variable1)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text "riable1")))))


(ert-deftest complete-symbol-helper-not-subsequent-typed-accessor-match-feild ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(initial-buffer-text "variable1 = 3\nvariable2 = variable1.")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text
	returned-variable)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (insert-into-global-scope variable1)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text "field1")))))



(ert-deftest run-parser-complete-symbol-import-module-typing-variable ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import test.test_module")
	(typed-symbol "te")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (newline-parse-variable)
    (insert typed-symbol)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n";\t"
					     typed-symbol
					     "st.test_module")))))



(ert-deftest run-parser-complete-symbol-import-module-typing-field ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import test.test_module")
 	(current-buffer (generate-new-buffer "test-text-buffer"))
	(typed-symbol "test.test_module.t")
	final-text
	returned-variable)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (newline-parse-variable)
    (insert typed-symbol)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n"
					     "st.test_module.test_function")))))



(ert-deftest complete-symbol ()
  ""
  (let ((initial-buffer-text "var1 = 1")
	(typed-symbol "va")

	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text)    
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    
    (newline-parse-variable)
    (insert typed-symbol)
    (complete-symbol)

    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n"
					     typed-symbol
					     "r1")))))
