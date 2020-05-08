(defun test ()
  (and (eq 'magnar-string-lines (intern-soft "s-lines"))
       (s-lines "It\nWorked")))

(test)


;; Local Variables:
;; shorthand-shorthands: (("^s-" . "magnar-string-"))
;; End:
