(funcall 'python-mode)

(load (concat default-directory "py-completion.el"))

(require 'ert)


;;; non-subsequent call tests
(ert-deftest test-not-subsequent-no-accessor ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-symbol)    
    (setq variable1 (create-variable "var1" fields1 methods1))
    (insert-into-global-scope variable1)
    (setq returned-symbol (get-symbol-and-set-globals nil "va"))
    (should (string-equal "var1"  returned-symbol))
    (should (string-equal "var1" (get-variable-symbol current-variable-global)))
    (should (eq  nil  current-field-global))
    (should (eq  nil current-method-global))))

(ert-deftest test-not-subsequent-has-accessor-no-member-symbol ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-symbol)    
    (setq variable1 (create-variable "var1" fields1 methods1))
    (insert-into-global-scope variable1)
    (setq returned-symbol (get-symbol-and-set-globals nil "var1."))
    (should (string-equal "var1.field1"  returned-symbol))
    (should (string-equal "var1" (get-variable-symbol current-variable-global)))
    (should (string-equal "field1" current-field-global))
    (should (eq  nil current-method-global))))

(ert-deftest test-not-subsequent-has-accessor-field-match ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-symbol)    
    (setq variable1 (create-variable "var1" fields1 methods1))
    (insert-into-global-scope variable1)
    (setq returned-symbol (get-symbol-and-set-globals nil "var1.f"))
    (should (string-equal "var1.field1"  returned-symbol))
    (should (string-equal "var1" (get-variable-symbol current-variable-global)))
    (should (string-equal "field1" current-field-global))
    (should (eq  nil current-method-global))))

(ert-deftest test-not-subsequent-has-accessor-method-match ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-symbol)    
    (setq variable1 (create-variable "var1" fields1 methods1))
    (insert-into-global-scope variable1)
    (setq returned-symbol (get-symbol-and-set-globals nil "var1.m"))
    (should (string-equal "var1.method1"  returned-symbol))
    (should (string-equal "var1" (get-variable-symbol current-variable-global)))
    (should (eq nil current-field-global))
    (should (string-equal "method1" current-method-global))))

;;; tests with subsequent call set
(ert-deftest test-is-subsequent-no-accessor ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(fields2 '("field1" "field2")) 
	(methods2 '("method1" "method2"))
	variable2
	(fields3 '("field1" "field2")) 
	(methods3 '("method1" "method2"))
	variable3
	returned-symbol)    
    (setq variable1 (create-variable "var1" fields1 methods1))
    (setq variable2 (create-variable "var2" fields1 methods1))
    (setq variable3 (create-variable "var3" fields1 methods1))
    (insert-into-global-scope variable3)
    (insert-into-global-scope variable2)
    (insert-into-global-scope variable1)
    (setq current-variable-global variable2)
    (setq returned-symbol (get-symbol-and-set-globals t "va"))
    (should (string-equal "var3"  returned-symbol))
    (should (string-equal "var3" (get-variable-symbol current-variable-global)))
    (should (eq nil current-field-global))
    (should (eq nil current-method-global))))


(ert-deftest test-is-subsequent-has-accessor-no-member-symbol ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(fields2 '("field1" "field2")) 
	(methods2 '("method1" "method2"))
	variable2
	(fields3 '("field1" "field2")) 
	(methods3 '("method1" "method2"))
	variable3
	returned-symbol)    
    (setq variable1 (create-variable "var1" fields1 methods1))
    (setq variable2 (create-variable "var2" fields1 methods1))
    (setq variable3 (create-variable "var3" fields1 methods1))
    (insert-into-global-scope variable3)
    (insert-into-global-scope variable2)
    (insert-into-global-scope variable1)
    (setq current-variable-global variable2)
    (setq current-method-global "method1")
    (setq returned-symbol (get-symbol-and-set-globals t "var2."))
    (should (string-equal "var2.method2"  returned-symbol))
    (should (string-equal "var2" (get-variable-symbol current-variable-global)))
    (should (eq nil current-field-global))
    (should (string-equal "method2"  current-method-global))))



(ert-deftest test-is-subsequent-has-accessor-field-match ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(fields2 '("field1" "field2")) 
	(methods2 '("method1" "method2"))
	variable2
	(fields3 '("field1" "field2")) 
	(methods3 '("method1" "method2"))
	variable3
	returned-symbol)    
    (setq variable1 (create-variable "var1" fields1 methods1))
    (setq variable2 (create-variable "var2" fields1 methods1))
    (setq variable3 (create-variable "var3" fields1 methods1))
    (insert-into-global-scope variable3)
    (insert-into-global-scope variable2)
    (insert-into-global-scope variable1)
    (setq current-variable-global variable2)
    (setq current-field-global "field1")
    (setq returned-symbol (get-symbol-and-set-globals t "var2.f"))
    (should (string-equal "var2.field2"  returned-symbol))
    (should (string-equal "var2" (get-variable-symbol current-variable-global)))
    (should (string-equal "field2" current-field-global))
    (should (eq nil current-method-global))))
