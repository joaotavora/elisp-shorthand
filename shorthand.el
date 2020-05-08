;;; shorthand.el --- namespacing system  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: languages, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Read README.md for now.

;;; Code:

(require 'cl-lib)

(defvar shorthand-shorthands nil)
(defvar shorthand-unconditional-reader-magic nil)
(put 'shorthand-shorthands 'safe-local-variable #'consp)

(defun shorthand--expand-shorthand (form)
  (cl-typecase form
    (cons (setcar form (shorthand--expand-shorthand (car form)))
          (setcdr form (shorthand--expand-shorthand (cdr form))))
    (vector (cl-loop for i from 0 for e across form
                     do (aset form i (shorthand--expand-shorthand e))))
    (symbol (let* ((name (symbol-name form)))
              (cl-loop for (short-pat . long-pat) in shorthand-shorthands
                       when (string-match short-pat name)
                       do (setq name (replace-match long-pat t nil name)))
              (setq form (intern name))))
    (string) (number)
    (t
     ;; this bit is most definitely not working correctly.
     (cond ((byte-code-function-p form)
            (cl-loop for e across form do (shorthand--expand-shorthand e)))
           (t (message "[shorthand--expand-shorthand] got unknown %s"
                       (type-of form))))))
  form)

(defun shorthand-read-wrapper (wrappee &rest stuff)
  (if (or shorthand-shorthands shorthand-unconditional-reader-magic)
      (shorthand--expand-shorthand
       (let ((obarray (obarray-make))) (apply wrappee stuff)))
    (apply wrappee stuff)))

(defun shorthand-intern-soft-wrapper (wrappee name &rest stuff)
  (let ((res (apply wrappee name stuff)))
    (or res (cl-loop
             for (short-pat . long-pat) in shorthand-shorthands
             thereis (apply wrappee
                            (replace-regexp-in-string short-pat
                                                      long-pat name)
                            stuff)))))

(defun shorthand-load-wrapper (wrappee file &rest stuff)
  ;; TODO: we certainly don't want to do this, but I think autoloads
  ;; break for some reason.
  (let ((shorthand-shorthands nil))
    (apply wrappee file stuff)))

(advice-add 'read        :around #'shorthand-read-wrapper)
(advice-add 'intern-soft :around #'shorthand-intern-soft-wrapper)
(advice-add 'load        :around #'shorthand-load-wrapper)

(provide 'shorthand)
;;; shorthand.el ends here
