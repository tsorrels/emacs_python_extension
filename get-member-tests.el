(funcall 'python-mode)

(load (concat default-directory "py-completion.el"))

(require 'ert)

(defvar variable1 nil)

(defvar variable2 nil)

(defvar variable3 nil)

;;; all tests test the get-matching-member-and-set-globals function


(ert-deftest test-init ()
  "Runs first to setup variables used in subsequent tests."
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	(fields2 '("field1" "field2")) 
	(methods2 '("method1" "method2"))
	variable2
	(fields3 '("field1" "field2")) 
	(methods3 '("method1" "method2")))
    (setq variable1 (create-variable "var1" fields1 methods1))
    (setq variable2 (create-variable "var2" fields2 methods2))
    (setq variable3 (create-variable "var3" fields3 methods3))
    t))

    
;;; non-subsequent call tests
(ert-deftest test-not-subsequent-field-match-first ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-field)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq returned-field (get-matching-member-and-set-globals variable1
								"f"
								nil))
      (should (string-equal "field1"  returned-field))
      (should (string-equal "field1"  current-field-global))
      (should (eq  nil current-method-global))))

(ert-deftest test-not-subsequent-method-match-first ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	scope
	returned-field)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq returned-field (get-matching-member-and-set-globals variable1
								"m"
								nil))
      (should (string-equal "method1"  returned-field))
      (should (string-equal "method1"  current-method-global))
      (should (eq  nil current-field-global))))

(ert-deftest test-not-subsequent-no-match ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	scope
	returned-field)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq returned-field (get-matching-member-and-set-globals variable1
								"h"
								nil))
      (should (eq nil returned-field))
      (should (eq nil current-method-global))
      (should (eq nil current-field-global))))


;;; non-subsequent call tests
(ert-deftest test-subsequent-all-globals-nil ()
  "Tests a subsequent call with neither the current global field or method set, indicating the last call did not find a match."
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-field)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq returned-field (get-matching-member-and-set-globals variable1
								"f"
								t))
      (should (eq nil returned-field))
      (should (eq nil current-field-global))
      (should (eq  nil current-method-global))))

(ert-deftest test-subsequent-field-set-match-next-field ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-field)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq current-field-global "field1")   
      (setq returned-field (get-matching-member-and-set-globals variable1
								"f"
								t))
      (should (string-equal "field2"  returned-field))
      (should (string-equal "field2"  current-field-global))
      (should (eq  nil current-method-global))))

(ert-deftest test-subsequent-field-set-match-second-method ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "f-method2"))
	variable1
	returned-member)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq current-field-global "field2")   
      (setq returned-member (get-matching-member-and-set-globals variable1
								"f"
								t))
      (should (string-equal "f-method2"  returned-member))
      (should (eq nil current-field-global))
      (should (string-equal "f-method2" current-method-global))))

(ert-deftest test-subsequent-field-set-no-methods-match-first-field ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-member)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq current-field-global "field2")   
      (setq returned-member (get-matching-member-and-set-globals variable1
								"f"
								t))
      (should (string-equal "field1"  returned-member))
      (should (string-equal "field1"  current-field-global))
      (should (eq nil current-method-global))))

(ert-deftest test-subsequent-method-set-match-next-method ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-field)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq current-method-global "method1")   
      (setq returned-field (get-matching-member-and-set-globals variable1
								"m"
								t))
      (should (string-equal "method2"  returned-field))
      (should (string-equal "method2"  current-method-global))
      (should (eq  nil current-field-global))))


(ert-deftest test-subsequent-method-set-match-second-field ()
  (reset-globals)
  (let ((fields1 '("field1" "m-field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-member)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq current-method-global "method2")   
      (setq returned-member (get-matching-member-and-set-globals variable1
								"m"
								t))
      (should (string-equal "m-field2"  returned-member))
      (should (string-equal "m-field2"  current-field-global))
      (should (eq nil current-method-global))))

(ert-deftest test-subsequent-method-set-no-fields-match-first-method ()
  (reset-globals)
  (let ((fields1 '("field1" "field2")) 
	(methods1 '("method1" "method2"))
	variable1
	returned-member)    
      (setq variable1 (create-variable "var1" fields1 methods1))
      (setq current-method-global "method2")   
      (setq returned-member (get-matching-member-and-set-globals variable1
								"m"
								t))
      (should (string-equal "method1"  returned-member))
      (should (eq nil  current-field-global))
      (should (string-equal "method1" current-method-global))))
