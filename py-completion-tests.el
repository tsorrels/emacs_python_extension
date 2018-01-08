
;;; (require 'py-completion.el)
(load "~/Documents/repos/emacs_python_extension/py-completion.el")

(require 'ert)

(ert-deftest test-scope-exists ()
  "Tests whether the global scope list exists."
  (should (eq nil global-scope)))
