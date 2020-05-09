# Shorthand.el

Simple-minded namespacing in Emacs:

1. Do this on an Emacs you don't care about, since this advises basic
   functions;
2. Load `shorthand.el` (or byte-compile and load it);
3. Load `magnar-string.el` or byte-compile it and load `magnar-string.elc`;
4. Try C-h f and check there's no "s-" pollution; Not even the `s-`
   symbols are interned;
5. Check that all the functions are namespaced under "magnar-string-";
6. Open test.el, and play around there.  Open test2.el and play around
   with magnar-string.el under a different "mstring-" prefix;
7. Evaluating code should work.  Eldoc should also work.  Xref (`M-.`)
   is broken.  Anything else might breaks spectacularly;

## Under the hood

Read `shorthand.el`: it's less than 50 loc.  The idea is to keep only
one obarray, but instruments `read` to not pollute it with symbols
that with the shorthands for other longer named symbols.
