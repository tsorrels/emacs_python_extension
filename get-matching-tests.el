(funcall 'python-mode)

(load (concat default-directory "py-completion.el"))

(funcall 'lisp-mode)

(require 'ert)

(ert-deftest get-first-matching-compare-variables ()
  ""
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
	returned-variable
	comparator)
    (setq comparator 'compare-symbol-with-variable)
    (setq variable1 (create-variable "a-var1" fields1 methods1))
    (setq variable2 (create-variable "a-var2" fields2 methods2))
    (setq variable3 (create-variable "b-var3" fields3 methods3))
    (setq scope (insert-variable variable3 scope))
    (setq scope (insert-variable variable2 scope))
    (setq scope (insert-variable variable1 scope))
    (setq returned-variable (get-first-matching "a" scope comparator))
    (should (string-equal "a-var1" (get-variable-symbol returned-variable)))))


(ert-deftest get-first-matching-compare-members ()
  ""
  (let ((fields '("field1" "field2")) 
	returned-member
	comparator)
    (setq comparator 'compare-symbol-with-member)
    (setq returned-member (get-first-matching "f" fields comparator))
    (should (string-equal "field1" returned-member))))


(ert-deftest get-first-matching-compare-members-no-match ()
  ""
  (let ((fields '("field1" "field2")) 
	returned-member
	comparator)
    (setq comparator 'compare-symbol-with-member)
    (setq returned-member (get-first-matching "a" fields comparator))
    (should (eq nil returned-member))))



(ert-deftest get-next-matching-compare-variables-next-matches ()
  ""
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
	scope
	returned-variable
	comparator)
    (setq comparator 'compare-symbol-with-variable)
    (setq variable1 (create-variable "a-var1" fields1 methods1))
    (setq variable2 (create-variable "a-var2" fields2 methods2))
    (setq variable3 (create-variable "a-var3" fields3 methods3))
    (setq scope (insert-variable variable3 scope))
    (setq scope (insert-variable variable2 scope))
    (setq scope (insert-variable variable1 scope))
    (setq returned-variable (get-next-matching "a"
					       scope
					       (get-variable-symbol variable1)
					       comparator))
    (should (string-equal "a-var2" (get-variable-symbol returned-variable)))))



(ert-deftest get-next-matching-compare-variables-first-in-list-matches ()
  ""
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
	scope
	returned-variable
	comparator)
    (setq comparator 'compare-symbol-with-variable)
    (setq variable1 (create-variable "a-var1" fields1 methods1))
    (setq variable2 (create-variable "a-var2" fields2 methods2))
    (setq variable3 (create-variable "b-var3" fields3 methods3))
    (setq scope (insert-variable variable3 scope))
    (setq scope (insert-variable variable2 scope))
    (setq scope (insert-variable variable1 scope))
    (setq returned-variable (get-next-matching "a"
					       scope
					       (get-variable-symbol variable2)
					       comparator))
    (should (string-equal "a-var1" (get-variable-symbol returned-variable)))))



(ert-deftest get-next-matching-compare-variables-last-in-list-matches ()
  ""
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
	scope
	returned-variable
	comparator)
    (setq comparator 'compare-symbol-with-variable)
    (setq variable1 (create-variable "a-var1" fields1 methods1))
    (setq variable2 (create-variable "a-var2" fields2 methods2))
    (setq variable3 (create-variable "b-var3" fields3 methods3))
    (setq scope (insert-variable variable1 scope))
    (setq scope (insert-variable variable2 scope))
    (setq scope (insert-variable variable3 scope))
    (setq returned-variable (get-next-matching "a"
					       scope
					       (get-variable-symbol variable2)
					       comparator))
    (should (string-equal "a-var1" (get-variable-symbol returned-variable)))))



(ert-deftest get-next-matching-compare-methods-first-in-list-matches ()
  ""
  (reset-globals)
  (let (
	(methods '("method1" "_method2" "method3" "_method4" "_method5"))
	returned-variable
	comparator)
    (setq comparator 'compare-symbol-with-member)
    (setq returned-variable (get-next-matching "m"
					       methods
					       "method3"
					       comparator))
    (should (string-equal "method1"  returned-variable))))


(ert-deftest get-next-matching-compare-methods-last-in-list-matches ()
  ""
  (reset-globals)
  (let (
	(methods '("method1" "_method2" "method3" "_method4" "_method5"))
	returned-variable
	comparator)
    (setq comparator 'compare-symbol-with-member)
    (setq returned-variable (get-next-matching "_m"
					       methods
					       "_method4"
					       comparator))
    (should (string-equal "_method5"  returned-variable))))
