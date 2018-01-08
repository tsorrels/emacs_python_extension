(require 'ert)

(ert-deftest addition-test1()
  (let
      ((y 3))
    (should (= (+ 1 2) y))))


(ert-deftest addition-test2()
  (setq y 3)
  (should (= (+ 1 2) y)))

(defvar z 3)

(ert-deftest addition-test3()
  (should (= (+ 1 2) z)))

(defvar zlist ())

(ert-deftest addition-test4()
  (should (eq zlist nil)))


(+ 1 2)

(let ((x 1))
  (= 1 x))
