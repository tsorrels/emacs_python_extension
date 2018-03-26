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
  (let ((line "(variable1;field1,field2;method1,method2,method3;)") variable)
    (setq variable (parse-line line))
    (should (string-equal "variable1" (get-variable-symgol variable)))
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

