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
  
(eq "variable0" "variable0")

(eq 1 1)

(eq 'variable0 'variable0)

(string-equal "variable0" "variable0")


(cons 1 ())


(load "~/Documents/repos/emacs_python_extension/py-completion.el")


(car (create-variable "var1" '("1" "2") '(1)))

(cons nil )

(insert-variable '("variable" nil nil) nil)

(car (car (let ((list-ptr (insert-variable (create-variable "variable" nil nil) nil))) list-ptr)))

('("variable" nil nil) nil)

()

(insert-variable(create-variable "var1" nil nil) nil)
(get-variable "var1" (insert-variable(create-variable "var1" nil nil) nil))
(get-variable "var1" (insert-variable(create-variable "var1" '("field1") nil) nil))


(let ((output-buffer (generate-new-buffer "test-output-buffer"))
      variables)
  (set-buffer output-buffer)
  (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test_variable_output.txt" nil nil nil)
  (set-buffer output-buffer)
  (setq text (delete-and-extract-region (point-min) (- (point-max) 1)))
  (setq lines (split-string text "\n"))
  (let ((line (car lines)))
    (parse-line line)))




(let ((output-buffer (generate-new-buffer "test-output-buffer"))
      variables)
  (set-buffer output-buffer)
  (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test_variable_output.txt" nil nil nil)
  (set-buffer output-buffer)
  (setq text (delete-and-extract-region (point-min) (point-max)))
  (setq lines (split-string text "\n"))
  (while lines
    (let ((line (car lines)))
      (if (string-equal "" line)
	  (setq lines (cdr lines)) ;;; pass
	
	(setq variable (parse-line line))
	(setq variables (cons variable variables))
	(setq lines (cdr lines)))))
  variables)



(let ((output-buffer (generate-new-buffer "test-output-buffer"))
      variables)
  (set-buffer output-buffer)
  (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test_variable_output.txt" nil nil nil)
  (parse-variables-buffer output-buffer))


(let ((buffer (generate-new-buffer "test-output-buffer"))
      variables)
  (set-buffer buffer)
  (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test_variable_output.txt" nil nil nil)
  (let (variables variable text lines)
    (set-buffer buffer)
    (setq text (delete-and-extract-region (point-min) (point-max)))
    (setq lines (split-string text "\n"))
    (while lines
      (let ((line (car lines)))
	(if (string-equal "" line)
	    (setq lines (cdr lines)) ;;; pass
	  
	  (setq variable (parse-line line))
	  (setq variables (cons variable variables))
	  (setq lines (cdr lines)))))
    variables))

(let ((buffer (generate-new-buffer "test-output-buffer"))
      variables)
  (set-buffer buffer)
  (insert-file-contents "/home/tsorrels/Documents/repos/emacs_python_extension/test_variable_output.txt" nil nil nil)
  (parse-variables-buffer buffer))



  ;(setq variables (parse-variables-buffer output-buffer)))
 
(point)

(point-min)

(point-max)


(let ((list1 '("a" "b"))
      (list2 '("b" "c" "d")))
  (append list1 list2))
