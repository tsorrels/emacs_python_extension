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
  (let ((initial-buffer-text "import test.test_package.test_module_a")
	(typed-symbol "te")
	(current-buffer (generate-new-buffer "test-text-buffer")))
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (newline-parse-variable)
    (insert typed-symbol)
    (setq typed-symbol (get-typed-symbol))
    (should (string-equal "te" typed-symbol))))


(ert-deftest test-parse-variables-buffer_socket-variable ()
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
    ;(insert "import socket\n")
    (insert-file-contents (concat default-directory "test/parser_output/import_socket_parser_output.txt") nil nil nil)
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
    (insert "(test;;;)")
    (setq variables (parse-variables-buffer buffer))
    (setq variable (car variables)) ;there should be only one variable
    (should (string-equal "test"  (get-variable-symbol variable)))
    (setq fields (get-variable-fields variable))
    (should (eq fields nil))))


(ert-deftest test-parse-variables-buffer_test-package-variable ()
  ""
  (let ((buffer (generate-new-buffer "test-output-buffer"))
	variables
        variable
        fields
        current-field
        found-field
        found-method)   
    (set-buffer buffer)
    (insert "(test.test_module;TestField,variable;test_function;)")
    (setq variables (parse-variables-buffer buffer))
    (setq variable (car variables)) ;there should be only one variable
    (should (string-equal "test.test_module" (get-variable-symbol variable)))
    (setq fields (get-variable-fields variable))
    (while fields
      (setq current-field (car fields))
      (if (string-equal "TestField" current-field)
	  (progn
	    (setq found-field current-field)
	    (setq fields nil))
	(setq fields (cdr fields))))
    (should (string-equal "test.test_module" (get-variable-symbol variable)))
    (should (string-equal "TestField" found-field))))

   
(ert-deftest test-parse-variables-buffer_multiple-simple-variables ()
  ""
  (let ((buffer (generate-new-buffer "test-output-buffer"))
        sock
	variables)
    (set-buffer buffer)
    (insert-file-contents (concat default-directory "test/parser_output/test_variable_parser_output.txt") nil nil nil)
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



(ert-deftest test-run-parser_duplicate-variables_only-one-in-scope ()
  ""
  (setq global-scope nil)
  (let ((current-buffer (generate-new-buffer "test-text-buffer"))
	(variables nil))
    (set-buffer current-buffer)
    (insert "variable1 = 1\nvariable2 = 1\nvariable1 = 1\n\n")
    (run-parser)
    (setq first-variable (nth 0 global-scope))
    (setq second-variable (nth 1 global-scope))
    (should (eq 2 (length global-scope)))
    (should (string-equal "variable2" (get-variable-symbol first-variable)))
    (should (string-equal "variable1" (get-variable-symbol second-variable)))))



(ert-deftest test-run-parser_import-socket ()
  ""
  (setq global-scope nil)
  (let ((current-buffer (generate-new-buffer "test-text-buffer"))
	(variables nil))
    (set-buffer current-buffer)
    (insert "import socket")
    (run-parser)
    (let ((sock (get-variable "socket" global-scope))
	  fields
	  methods
	  found-field
	  found-method
	  found-variable
	  current-field
	  current-method)
      (setq fields (get-variable-fields sock))
      (setq methods (get-variable-methods sock))
      (setq variables (get-variable-variables sock))
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
      (while variables
	(setq current-variable (car variables))
	(if (string-equal "warnings" (get-variable-symbol current-variable))
	    (progn
	      (setq found-variable current-variable)
	      (setq variables nil))
	  (setq variables (cdr variables))))     


      (should (string-equal "warnings" (get-variable-symbol found-variable)))      
      (should (string-equal "socket" (get-variable-symbol sock)))
      (should (string-equal "EAI_SYSTEM" found-field))
      (should (string-equal "inet_pton" found-method)))))



(ert-deftest test-run-parser_import-module ()
  ""
  (setq global-scope nil)
  (let ((current-buffer (generate-new-buffer "test-text-buffer"))
	(variables nil))
    (set-buffer current-buffer)
    (insert "import test_module_a")
    (run-parser)
    (let ((module (get-variable "test_module_a" global-scope))
	  fields
	  methods
	  found-field
	  found-method
	  found-variable
	  current-field
	  current-method)
      (setq fields (get-variable-fields module))
      (setq methods (get-variable-methods module))
      (setq variables (get-variable-variables module))
      (should (string-equal "test_module_a" (get-variable-symbol module)))
      (while fields
	(setq current-field (car fields))
	(if (string-equal "test_module_a_variable" current-field)
	    (progn
	      (setq found-field current-field)
	      (setq fields nil))
	  (setq fields (cdr fields))))
      (while methods
	(setq current-method (car methods))
	(if (string-equal "test_module_a_function" current-method)
	    (progn
	      (setq found-method current-method)
	      (setq methods nil))
	  (setq methods (cdr methods))))
      (while variables
	(setq current-variable (car variables))
	(if (string-equal "TestModuleA" (get-variable-symbol current-variable))
	    (progn
	      (setq found-variable current-variable)
	      (setq variables nil))
	  (setq variables (cdr variables))))     


      (should (string-equal "TestModuleA" (get-variable-symbol found-variable)))      
      (should (string-equal "test_module_a" (get-variable-symbol module)))
      (should (string-equal "test_module_a_variable" found-field))
      (should (string-equal "test_module_a_function" found-method)))))




(ert-deftest test-insert-symbol-into-buffer ()
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

(ert-deftest test-insert-symbol-into-buffer_double-index ()
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


(ert-deftest complete-symbol-helper_not-subsequent_two-variables-declared ()
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
    (should (string-equal "variable1" current-symbol-global))
    (should (eq 'variable current-member-type))
    (should (string-equal final-text (concat initial-buffer-text "riable1")))))


(ert-deftest complete-symbol-helper_not-subsequent_typed-accessor_match-feild ()
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
    (should (string-equal "field1" current-symbol-global))
    (should (eq 'field current-member-type))
    (should (string-equal final-text (concat initial-buffer-text "field1")))))



(ert-deftest complete-symbol-helper_is-subsequent_match-next-feild ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(initial-buffer-text "variable1 = 3\nvariable2 = variable1.field1")
	(expected-final-text "variable1 = 3\nvariable2 = variable1.field2")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text
	returned-variable)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (insert-into-global-scope variable1)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (setq current-member-type 'field)
    (setq current-symbol-global "field1")
    ;(setq typed-symbol-global "f")
    (setq initial-point-global 37)
    (complete-symbol-helper t)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal "field2" current-symbol-global))
    (should (eq 'field current-member-type))    
    (should (string-equal final-text expected-final-text))))



(ert-deftest complete-symbol-helper_is-subsequent_match-same-feild ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((fields1 '("field1")) 
	(methods1 '("method1" "method2"))
	variable1
	(initial-buffer-text "variable1 = 3\nvariable2 = variable1.field1")
	(expected-final-text "variable1 = 3\nvariable2 = variable1.field1")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text
	returned-variable)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (insert-into-global-scope variable1)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (setq current-member-type 'field)
    (setq current-symbol-global "field1")
    ;(setq typed-symbol-global "f")
    (setq initial-point-global 38)
    (complete-symbol-helper t)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal "field1" current-symbol-global))
    (should (eq 'field current-member-type))    
    (should (string-equal final-text expected-final-text))))


(ert-deftest complete-symbol-helper_is-subsequent_match-same-variable ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((fields1 '("field1")) 
	(methods1 '("method1" "method2"))
	variable1
	(initial-buffer-text "variable1 = 3\nvariable2 = variable1")
	(expected-final-text "variable1 = 3\nvariable2 = variable1")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text
	returned-variable)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (insert-into-global-scope variable1)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (setq current-member-type 'variable)
    (setq current-symbol-global "variable1")
    ;(setq typed-symbol-global "f")
    (setq initial-point-global 30)
    (complete-symbol-helper t)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal "variable1" current-symbol-global))
    (should (eq 'variable current-member-type))
    (should (string-equal "var" typed-symbol-global))
    (should (string-equal final-text expected-final-text))))



(ert-deftest complete-symbol-helper_is-subsequent_match-different-mem-type ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((fields1 '("a_field1" "field2")) 
	(methods1 '("a_method1" "method2"))
	variable1
	(initial-buffer-text "variable1 = 3\nvariable2 = variable1.a_field1")
	(expected-final-text "variable1 = 3\nvariable2 = variable1.a_method1")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text
	returned-variable)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (insert-into-global-scope variable1)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (setq current-member-type 'field)
    (setq current-symbol-global "a_field1")
    (setq initial-point-global 38)
    (complete-symbol-helper t)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (eq current-member-type 'method))
    (should (string-equal current-symbol-global "a_method1"))
    (should (string-equal final-text expected-final-text))))


(ert-deftest complete-symbol-helper_is-subsequent_match-method ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let (
	(fields1 '("a_field1" "field2")) 
	(methods1 '("a_method1" "method2"))
	variable1
	(fields2 '("a_field1" "field2")) 
	(methods2 '("a_method1" "method2"))
	variable2
	(root-fields '("a_field1" "field2")) 
	(root-methods '("a_method1" "method2"))
	root-variables 	
	root-variable
	(initial-buffer-text "variable1 = 3\nvariable2 = variable1.a_field1")
	(expected-final-text "variable1 = 3\nvariable2 = variable1.a_method1")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text
	returned-variable)
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (setq variable2 (create-variable "variable2" fields2 methods2))
    (setq root-variables (list variable1 variable2))
    (setq root-variable (create-variable "root-variable"
					 root-fields
					 root-methods
					 root-variables))    
    (insert-into-global-scope variable1)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (setq current-member-type 'field)
    (setq current-symbol-global "a_field1")
    ;(setq typed-symbol-global "f")
    (setq initial-point-global 38)
    (complete-symbol-helper t)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (eq current-member-type 'method))
    (should (string-equal current-symbol-global "a_method1"))
    (should (string-equal final-text expected-final-text))))


(ert-deftest complete-symbol-helper_is-subsequent_two-variables_match-var ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let (
	(fields1 '("a_field1" "field2")) 
	(methods1 '("a_method1" "method2"))
	variable1
	(fields2 '("a_field1" "field2")) 
	(methods2 '("a_method1" "method2"))
	variable2
	(initial-buffer-text "variable1 = 3\nvariable2 = variable1")
	(expected-final-text "variable1 = 3\nvariable2 = variable2")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text
	returned-variable)
    (setq variable2 (create-variable "variable2" fields2 methods2))
    (setq variable1 (create-variable "variable1" fields1 methods1))
    (insert-into-global-scope variable2)
    (insert-into-global-scope variable1)

    (set-buffer current-buffer)
    (insert initial-buffer-text)
    (setq current-member-type 'variable)
    (setq current-symbol-global "variable1")
    (setq initial-point-global 30)
    (complete-symbol-helper t)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (eq current-member-type 'variable))
    (should (string-equal current-symbol-global "variable2"))
    (should (string-equal final-text expected-final-text))))




(ert-deftest run-parser-complete-symbol_import-package_typing-variable ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import test.test_module")
	(typed-symbol "te")
	(current-buffer (generate-new-buffer "test-text-buffer"))
	final-text)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    ;;; (newline-parse-variable)
    ;;; newline-parse-variables calls (indent-for-tab) command, which inserts
    ;;; whitespace that is something other than \t.  That makes it hard
    ;;; to do string comparrison in the should statements at the end.
    ;;; So for now I am bypassing (newline-parse-variable).
    (newline)
    (parse-variables)
    (insert typed-symbol)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n"
					     typed-symbol
					     "st.test_module")))))



(ert-deftest run-parser-complete-symbol_import-package_typing-field ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import test.test_package.test_module_a")
 	(current-buffer (generate-new-buffer "test-text-buffer"))
	(typed-symbol "test.test_package.test_module_a.f")
	final-text
	returned-variable)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    ;;; (newline-parse-variable)
    (newline)
    (parse-variables)
    (insert typed-symbol)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n"
					     "test.test_package.test_module_a.field")))))


(ert-deftest run-parser-complete-symbol_import-package_typing-variable ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import test.test_package.test_module_a")
 	(current-buffer (generate-new-buffer "test-text-buffer"))
	(typed-symbol "test.test_package.test_module_a.Tes")
	final-text
	returned-variable)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    ;;; (newline-parse-variable)
    (newline)
    (parse-variables)
    (insert typed-symbol)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n"
					     "test.test_package.test_module_a.TestModuleA")))))



(ert-deftest run-parser-complete-symbol_import-socket_typing-function ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import socket")
 	(current-buffer (generate-new-buffer "test-text-buffer"))
	(typed-symbol "socket.socketpa")
	final-text
	returned-variable)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    ;;; (newline-parse-variable)
    (newline)
    (parse-variables)
    (insert typed-symbol)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n"
					     typed-symbol
					     "ir")))))


(ert-deftest run-parser-complete-symbol_import-socket_typing-variable ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import socket")
 	(current-buffer (generate-new-buffer "test-text-buffer"))
	(typed-symbol "socket.warnin")
	final-text
	returned-variable)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    ;;; (newline-parse-variable)
    (newline)
    (parse-variables)
    (insert typed-symbol)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n"
					     typed-symbol
					     "gs")))))


(ert-deftest run-parser-complete-symbol_import-module_typing-variable ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import test_module_a")
 	(current-buffer (generate-new-buffer "test-text-buffer"))
	(typed-symbol "test_module_a.T")
	final-text
	returned-variable)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    ;;; (newline-parse-variable)
    (newline)
    (parse-variables)
    (insert typed-symbol)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n"
					     typed-symbol
					     "estModuleA")))))



(ert-deftest run-parser-complete-symbol_import-module_typing-nested-variable ()
  ""
  (reset-globals)
  (setq global-scope nil)  
  (let ((initial-buffer-text "import test_module_a")
 	(current-buffer (generate-new-buffer "test-text-buffer"))
	(typed-symbol "test_module_a.TestModuleA.fun")
	final-text
	returned-variable)
    (set-buffer current-buffer)
    (insert initial-buffer-text)
    ;;; (newline-parse-variable)
    (newline)
    (parse-variables)
    (insert typed-symbol)
    (complete-symbol-helper nil)
    (setq final-text (delete-and-extract-region (point-min) (point-max)))
    (should (string-equal final-text (concat initial-buffer-text
					     "\n"
					     typed-symbol
					     "ction_a")))))


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

    ;;; this test hardcodes five spaces, which is what the command
    ;;; (indent-for-tab-command) probably inserts into the buffer in python-
    ;;; mode.  If (indent-for-tab-command) inserts something other than
    ;;; five spaces, the final string comparrison below will fail.
    (should (string-equal final-text (concat initial-buffer-text
					     "\n     "
					     typed-symbol
					     "r1")))))
