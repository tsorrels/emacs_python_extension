(defun create-variable (symbol fields methods)
  "Generate a list specially formated list to represent a variable.
The first element of the list is a string that represents the variable symbol.
The second element is a list of strings which are the variable's fields.
The third element is a list of strings which are the variable's methods."
  (let (empty-list)
    (cons symbol (cons fields (cons methods empty-list)))))


(defun get-variable-fields (variable)
  "Getter to return the proper list that represents the fields for a variable"
  (car (cdr variable)))

(defun get-variable-methods (variable)
  "Getter to return the proper list that represents the methods for a variable"
  (car (cdr (cdr variable))))

(defun get-variable-symbol (variable)
  "Getter to return the string that is the symbol for the variable"
  (car variable))

(defun parse-variable (line)
  ()
  )
