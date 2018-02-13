# emacs_python_extension

A simple Emacs extension that continuously parses python code and offers basic code completion. IN BETA: currently offers only completion of variables defined in the document. 

To load:
In Emacs (in python-mode), 


Emacs will update symbols/variables in scope with every strike of 'RET'. To auto-complete, strike '<backtab>' (mapped to 'shift-tab' by default). Continuously striking '<backtab>' will loop through all variables that the partially completed symbol ended just prior to the cursor matches to.


![UI View](/doc/screen_initial.png)

The following screenshots show the change to the current line in the buffer following five stricks of '<backtab>'

![UI View](/doc/screen_auto_complete_1.png)
![UI View](/doc/screen_auto_complete_2.png)
![UI View](/doc/screen_auto_complete_3.png)
![UI View](/doc/screen_auto_complete_4.png)
![UI View](/doc/screen_auto_complete_1.png)


TODO:
- auto-complete only variables in scope
- ensure symbols/variables added to scope are unique
- present auto-completion in alphabetical order
- add python keywords to scope
- parse classes/constructors/fields, auto-complete defined fields and methods
- nest scopes
- optimize search
- pull variables into scope from import statements
- create package/install
- test script check to see if all processes exited with 0 status
- refactor code into seperate files
