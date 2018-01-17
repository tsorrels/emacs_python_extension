(require 'ert)

(load "~/Documents/repos/emacs_python_extension/py-completion.el")


(newline-parse-variable)

(global-set-key (kbd "RET") 'newline-parse-variable)

(kbd "RET")



E/

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

(let ((variable '((field) (method))))
  (let ((var (car variable)))
    (let ((function (nthcdr 1 variable))
      function)))




  (lookup-key (current-global-map) "\C-x\C-f")

  (lookup-key (current-global-map) (kbd "C-x C-f"))
  (lookup-key (current-global-map) (kbd "1"))
  (lookup-key (current-global-map) (kbd "RET"))
(insert-char A)

(newline)


A

  (current-global-map)
  (local-global-map)
(last-command)
  
