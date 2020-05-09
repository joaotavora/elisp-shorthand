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

;;; Code:

(require 'cl-lib)

(defvar shorthand-shorthands nil)
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
    (t       (message "[shorthand] unexpectged %s" (type-of form))))
  form)

(defun shorthand-read-wrapper (wrappee stream &rest stuff)
  (if (and load-file-name (string-match "\\.elc$" load-file-name))
      (apply wrappee stream stuff)
    (shorthand--expand-shorthand
     (let ((obarray (obarray-make))) (apply wrappee stream stuff)))))

(defun shorthand-intern-soft-wrapper (wrappee name &rest stuff)
  (let ((res (apply wrappee name stuff)))
    (or res (cl-loop
             for (short-pat . long-pat) in shorthand-shorthands
             thereis (apply wrappee
                            (replace-regexp-in-string short-pat
                                                      long-pat name)
                            stuff)))))

(defun shorthand-load-wrapper (wrappee file &rest stuff)
  (let (file-local-shorthands)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (hack-local-variables)
        (setq file-local-shorthands shorthand-shorthands)))
    (let ((shorthand-shorthands file-local-shorthands))
      (apply wrappee file stuff)))))

(advice-add 'read        :around #'shorthand-read-wrapper)
(advice-add 'intern-soft :around #'shorthand-intern-soft-wrapper)
(advice-add 'load        :around #'shorthand-load-wrapper)

(provide 'shorthand)
;;; shorthand.el ends here
