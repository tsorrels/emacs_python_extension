
;;; (require 'py-completion.el)

;;; (kill-all-local-variables)

(load "~/Documents/repos/emacs_python_extension/py-completion.el")

(require 'ert)

(ert-deftest test-scope-exists ()
  "Tests whether the global scope list exists."
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
  
  )
