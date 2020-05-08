# Shorthand

A POC for solving the namespace problem in Emacs:

1. Start an Emacs you don't care about, like `emacs -Q -l shorthand.el`
2. Find `magnar-string.el` and load with M-x eval-buffer;
3. Find test.el and evaluate the two forms there with C-x C-e;
4. Try C-h f and check there's no "s-" pollution;
5. Check that all the functions are namespaced under "magnar-string-"
6. It should work. Eldoc should also work.  Anything else will
   probably crash your Emacs.

Yes, there are problems with autoloads and byte-compilation: this is a
POC.


