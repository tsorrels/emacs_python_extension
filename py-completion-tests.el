
;;; (require 'py-completion.el)

(kill-all-local-variables)

(load "~/Documents/repos/emacs_python_extension/py-completion.el")

(require 'ert)

(ert-deftest test-scope-exists ()
  "Tests whether the global scope list exists."
  (should (eq nil global-scope)))

(ert-deftest test-insert-variable-into-scope ()
  "Adds variable to the global scope."
  (let ((fields '(field1 field2)) (methods '(method1 method2)))
    (let ((variable (create-variable 'var1 fields methods)))
      (let ((scope nil))
	(insert-variable variable scope)
	(let ((var (get-variable 'var1 scope)))
	  (should (eq 'var1 (car var))))))))
