(load (concat default-directory "variable.el"))
;;;(load (concat default-directory "../../variable.el"))

(require 'ert)


(ert-deftest test-parse-variable-line_no-fields-no-methods-no-variables ()
  "Test parse-line with no fields and no methods."
  (let ((line "(variable0;;;)")
	variable)
    (setq variable (parse-variable-line line))
    (setq symbol (get-variable-symbol variable))
    (setq fields (get-variable-fields variable))
    (setq methods (get-variable-methods variable))
    (should (string-equal "variable0" symbol))
    (should (eq nil fields))
    (should (eq nil methods))))



(ert-deftest parse-variables_no-nested-variables ()
  "Test parse-variable for variable with multiple fields, multiple methods, and no nested variables"
  (let (line
	variable)
    (setq line "(var1;f1,f2;m1,m2;)")
    (setq variable (parse-variable-line line))
    (setq symbol (get-variable-symbol variable))
    (setq fields (get-variable-fields variable))
    (setq methods (get-variable-methods variable))
    (setq variables (get-variable-variables variable))

    (should (string-equal "f1" (car fields)))
    (should (string-equal "f2" (nth 1 fields)))
    (should (string-equal "m1" (car methods)))
    (should (string-equal "m2" (nth 1 methods)))
    (should (eq nil variables))
    
    (should (string-equal "var1" symbol)))))



(ert-deftest parse-variables_one-single-nested-variable ()
  "Test parse-variable for variable with multiple fields, multiple methods, and no nested variables"
  (let (line
	variable
	nested-variable)
    (setq line "(var1;f11,f12;m11,m12;(var2;f21,f22;m21,m22;))")
    (setq variable (parse-variable-line line))
    
    (setq symbol (get-variable-symbol variable))
    (setq fields (get-variable-fields variable))
    (setq methods (get-variable-methods variable))
    (setq variables (get-variable-variables variable))
    (setq nested-variable (car variables))

    (should (string-equal "var1" symbol) )
    (should (string-equal "f11" (car fields)))
    (should (string-equal "f12" (nth 1 fields)))
    (should (string-equal "m11" (car methods)))
    (should (string-equal "m12" (nth 1 methods)))
    
    (setq symbol (get-variable-symbol nested-variable))
    (setq fields (get-variable-fields nested-variable))
    (setq methods (get-variable-methods nested-variable))
    (setq variables (get-variable-variables nested-variable))

    (should (string-equal "var2" symbol) )
    (should (string-equal "f21" (car fields)))
    (should (string-equal "f22" (nth 1 fields)))
    (should (string-equal "m21" (car methods)))
    (should (string-equal "m22" (nth 1 methods)))
    (should (eq nil variables))))





(ert-deftest parse-variables_two-single-nested-variable ()
  "Test parse-variable for variable with multiple fields, multiple methods, and no nested variables"
  (let (line
	variable
	nested-variable
	second-nested-variable)
    (setq line "(var1;f11,f12;m11,m12;(var2;f21,f22;m21,m22;),(var3;f31;m31;))")
    (setq variable (parse-variable-line line))
    (setq pruned-line (car (cdr variable-line-pair)))
    
    (setq symbol (get-variable-symbol variable))
    (setq fields (get-variable-fields variable))
    (setq methods (get-variable-methods variable))
    (setq variables (get-variable-variables variable))
    (setq nested-variable (car variables))
    (setq second-nested-variable (car (cdr variables)))

    (should (string-equal "var1" symbol) )
    (should (string-equal "f11" (car fields)))
    (should (string-equal "f12" (nth 1 fields)))
    (should (string-equal "m11" (car methods)))
    (should (string-equal "m12" (nth 1 methods)))
    
    (setq symbol (get-variable-symbol nested-variable))
    (setq fields (get-variable-fields nested-variable))
    (setq methods (get-variable-methods nested-variable))
    (setq variables (get-variable-variables nested-variable))

    (should (string-equal "var2" symbol) )
    (should (string-equal "f21" (car fields)))
    (should (string-equal "f22" (nth 1 fields)))
    (should (string-equal "m21" (car methods)))
    (should (string-equal "m22" (nth 1 methods)))
    (should (eq nil variables))  

    (setq symbol (get-variable-symbol second-nested-variable))
    (setq fields (get-variable-fields second-nested-variable))
    (setq methods (get-variable-methods second-nested-variable))
    (setq variables (get-variable-variables second-nested-variable))

    (should (string-equal "var3" symbol) )
    (should (string-equal "f31" (car fields)))
    (should (string-equal "m31" (car methods)))
    (should (eq nil variables))))




(ert-deftest parse-variables_double-nested-variable ()
  "Test parse-variable for variable with multiple fields, multiple methods, and no nested variables"
  (let (line
	variable
	nested-variable
	nested-variables
	second-nested-variable)
    (setq line "(var1;f11,f12;m11,m12;(var2;f21,f22;m21,m22;(var3;f31;m31;)))")
    (setq variable(parse-variable-line line))
    (setq pruned-line (car (cdr variable-line-pair)))
    
    (setq symbol (get-variable-symbol variable))
    (setq fields (get-variable-fields variable))
    (setq methods (get-variable-methods variable))
    (setq variables (get-variable-variables variable))
    
    (setq nested-variable (car variables))
    (setq nested-variables (get-variable-variables nested-variable))

    (setq second-nested-variable (car nested-variables))

    (should (string-equal "var1" symbol) )
    (should (string-equal "f11" (car fields)))
    (should (string-equal "f12" (nth 1 fields)))
    (should (string-equal "m11" (car methods)))
    (should (string-equal "m12" (nth 1 methods)))
    
    (setq symbol (get-variable-symbol nested-variable))
    (setq fields (get-variable-fields nested-variable))
    (setq methods (get-variable-methods nested-variable))
    (setq variables (get-variable-variables nested-variable))

    (should (string-equal "var2" symbol) )
    (should (string-equal "f21" (car fields)))
    (should (string-equal "f22" (nth 1 fields)))
    (should (string-equal "m21" (car methods)))
    (should (string-equal "m22" (nth 1 methods)))

    (setq symbol (get-variable-symbol second-nested-variable))
    (setq fields (get-variable-fields second-nested-variable))
    (setq methods (get-variable-methods second-nested-variable))
    (setq variables (get-variable-variables second-nested-variable))

    (should (string-equal "var3" symbol) )
    (should (string-equal "f31" (car fields)))
    (should (string-equal "m31" (car methods)))
    (should (eq nil variables))))





(ert-deftest parse-variable-line_double-nested-variable ()
  "Test parse-variable for variable with multiple fields, multiple methods, and no nested variables"
  (let (line
	variable
	nested-variable
	nested-variables
	second-nested-variable)
    (setq line "(var1;f11,f12;m11,m12;(var2;f21,f22;m21,m22;(var3;f31;m31;)))")
    (setq variable (parse-variable-line line))
    
    (setq symbol (get-variable-symbol variable))
    (setq fields (get-variable-fields variable))
    (setq methods (get-variable-methods variable))
    (setq variables (get-variable-variables variable))
    
    (setq nested-variable (car variables))
    (setq nested-variables (get-variable-variables nested-variable))

    (setq second-nested-variable (car nested-variables))

    (should (string-equal "var1" symbol) )
    (should (string-equal "f11" (car fields)))
    (should (string-equal "f12" (nth 1 fields)))
    (should (string-equal "m11" (car methods)))
    (should (string-equal "m12" (nth 1 methods)))
    
    (setq symbol (get-variable-symbol nested-variable))
    (setq fields (get-variable-fields nested-variable))
    (setq methods (get-variable-methods nested-variable))
    (setq variables (get-variable-variables nested-variable))

    (should (string-equal "var2" symbol) )
    (should (string-equal "f21" (car fields)))
    (should (string-equal "f22" (nth 1 fields)))
    (should (string-equal "m21" (car methods)))
    (should (string-equal "m22" (nth 1 methods)))

    (setq symbol (get-variable-symbol second-nested-variable))
    (setq fields (get-variable-fields second-nested-variable))
    (setq methods (get-variable-methods second-nested-variable))
    (setq variables (get-variable-variables second-nested-variable))

    (should (string-equal "var3" symbol) )
    (should (string-equal "f31" (car fields)))
    (should (string-equal "m31" (car methods)))
    (should (eq nil variables))))
