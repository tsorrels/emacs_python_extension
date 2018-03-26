python2 -m unittest test.test_variable_parser
python2 -m unittest test.test_import_parser
emacs -batch -l ert -l ./test/elisp/py-completion-tests.el -f ert-run-tests-batch-and-exit
emacs -batch -l ert -l ./test/elisp/variable-tests.el -f ert-run-tests-batch-and-exit
emacs -batch -l ert -l ./test/elisp/get-matching-tests.el -f ert-run-tests-batch-and-exit
emacs -batch -l ert -l ./test/elisp/rebuild-symbol-tests.el -f ert-run-tests-batch-and-exit
emacs -batch -l ert -l ./test/elisp/get-matching-var-tests.el -f ert-run-tests-batch-and-exit


