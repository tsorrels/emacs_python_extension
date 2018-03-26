(funcall 'python-mode)

(load (concat default-directory "py-completion.el"))

(require 'ert)


;;; all tests test the get-matching-member-and-set-globals function

    
(ert-deftest rebuild-symbol_one-complete-symbol_empty-accessor ()
  (setq next-member-symbol "field1")
  (setq rebuilt-symbol (rebuild-symbol (list "variable1") next-member-symbol))
  (should (string-equal "variable1.field1" rebuilt-symbol)))

(ert-deftest rebuild-symbol_two-complete-symbol_empty-accessor ()
  (setq next-member-symbol "field1")
  (setq rebuilt-symbol (rebuild-symbol '("var1" "var2") next-member-symbol))
  (should (string-equal "var1.var2.field1" rebuilt-symbol)))

(ert-deftest rebuild-symbol_no-complete-symbols ()
  (setq next-member-symbol "field1")
  (setq rebuilt-symbol (rebuild-symbol nil next-member-symbol))
  (should (string-equal "field1" rebuilt-symbol)))


(ert-deftest get-complete-symbols_no-accessor ()
  (setq complete-symbols (get-complete-symbols "var1"))
  (should (eq nil complete-symbols)))


(ert-deftest get-complete-symbols_one-variable_no-member ()
  (setq complete-symbols (get-complete-symbols "var1."))
  (should (string-equal "var1" (car complete-symbols))))


(ert-deftest get-complete-symbols_two-variables_typed-member ()
  (setq complete-symbols (get-complete-symbols "var1.var2.f1"))
  (should (string-equal "var1" (car complete-symbols)))
  (should (string-equal "var2" (car (cdr complete-symbols)))))


(ert-deftest get-complete-symbols_none-complete ()
  (setq complete-symbols (get-complete-symbols "var"))
  (should (eq nil  complete-symbols)))


(ert-deftest get-last-symbol ()
  (setq last-symbol (get-last-symbol "var1.var2.f1"))
  (should (string-equal "f1" last-symbol)))


(ert-deftest get-last-symbol ()
  (setq last-symbol (get-last-symbol "var1.var2.f1"))
  (should (string-equal "f1" last-symbol)))


(ert-deftest get-last-symbol_empty-symbol ()
  (setq last-symbol (get-last-symbol "var1.var2."))
  (should (string-equal "" last-symbol)))


(ert-deftest get-last-symbol_no-accessor ()
  (setq last-symbol (get-last-symbol "var"))
  (should (string-equal "var" last-symbol)))





(ert-deftest run-symbol-helpers_two-complete-symbol_empty-accessor ()
  (setq typed-symbol "var")
  (setq complete-symbols (get-complete-symbols typed-symbol))
  (setq last-symbol (get-last-symbol "var"))
  (setq next-member-symbol "variable1")
  (setq rebuilt-symbol (rebuild-symbol complete-symbols next-member-symbol))
  (should (eq nil complete-symbols))  
  (should (string-equal last-symbol "var"))  
  (should (string-equal "variable1" rebuilt-symbol)))
