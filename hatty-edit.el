;;; cursorfree.el --- Edit and navigate through hats -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.0.0
;; Package-Requires: ((emacs "25.1") (hatty "0.2.0"))
;; Created: 06 Sep 2024

;; cursorfree.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; cursorfree.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'hatty)
(require 'evil)
(require 'dash)
(require 'multiple-cursors)


;;;; Instruction interpreter:

(cl-defstruct cursorfree--environment
  (instruction-stack nil) (value-stack nil))

(defun cursorfree--make-environment (instructions &optional value-stack)
  (make-cursorfree--environment
   :instruction-stack instructions
   :value-stack value-stack))

(defun cursorfree--clone-environment (environment)
  (make-cursorfree--environment
   :value-stack (cursorfree--environment-value-stack environment)
   :instruction-stack (cursorfree--environment-instruction-stack environment)))

(defun cursorfree--push-instruction (environment instruction)
  (push instruction (cursorfree--environment-instruction-stack environment)))

(defun cursorfree--push-instructions (environment instructions)
  (dolist (instruction (reverse instructions))
    (cursorfree--push-instruction environment instruction)))

(defun cursorfree--pop-instruction (environment)
  (pop (cursorfree--environment-instruction-stack environment)))

(defun cursorfree--push-value (environment value)
  (declare (indent defun))
  (push value (cursorfree--environment-value-stack environment)))

(defun cursorfree--push-value-pure (environment value)
  (declare (indent defun))
  (let ((new-environment (cursorfree--clone-environment environment)))
    (cursorfree--push-value new-environment value)
    new-environment))

(defun cursorfree--push-values (environment values)
  (declare (indent defun))
  (dolist (value (reverse values))
    (cursorfree--push-value environment value)))

(defun cursorfree--pop-value (environment)
  (declare (indent defun))
  (cl-destructuring-bind (head . tail)
      (cursorfree--environment-value-stack environment)
    (setf (cursorfree--environment-value-stack environment) tail)
    head))

(defun cursorfree--pop-values (environment n)
  (declare (indent defun))
  (let ((acc nil))
    (dotimes (i n (reverse acc))
      (push (cursorfree--pop-value environment) acc))))

(defun cursorfree--peek-value (environment)
  (declare (indent defun))
  (car (cursorfree--environment-value-stack environment)))

(defun cursorfree-define-instruction-pure (name instruction)
  (declare (indent defun))
  (put name 'cursorfree--instruction instruction)
  name)

(defun cursorfree-define-instruction (name instruction)
  (declare (indent defun))
  (cursorfree-define-instruction-pure name
    (lambda (environment)
      (let ((mutable-environment (cursorfree--clone-environment environment)))
        (funcall instruction mutable-environment)
        mutable-environment))))

(defun cursorfree--get-instruction (name)
  (let ((instruction
         (get name 'cursorfree--instruction)))
    (unless instruction
      ;; TODO: Custom error symbol?
      (signal 'error (list "No such cursorfree instruction: " name)))
    instruction))

(defun cursorfree--step (environment)
  (let* ((new-environment (cursorfree--clone-environment environment))
         (instruction (cursorfree--pop-instruction new-environment)))
    (funcall instruction new-environment)))

(defun cursorfree--evaluate-environment (environment)
  "Evaluate ENVIRONMENT and return the final value stack."
  (declare (indent defun))
  (while (cursorfree--environment-instruction-stack environment)
    (setq environment (cursorfree--step environment)))
  (cursorfree--environment-value-stack environment))

(defun cursorfree--evaluate (instructions)
  (cursorfree--evaluate-environment
    (cursorfree--make-environment instructions)))

;;; Core macros and functions for defining instructions

;; The core instructions are what all other words build upon.

(defun cursorfree--lisp-funcall-list (function environment)
  (setf (cursorfree--environment-value-stack environment)
        (funcall function (cursorfree--environment-value-stack environment))))

(defun cursorfree--lisp-funcall-1 (function arity environment)
  (cursorfree--lisp-funcall-list
   (lambda (value-stack)
     (cons
      (apply function (reverse (take arity value-stack)))
      (nthcdr arity value-stack)))
   environment))

(defun cursorfree--lisp-funcall-0 (function arity environment)
  (cursorfree--lisp-funcall-list
   (lambda (value-stack)
     (apply function (reverse (take arity value-stack)))
     (nthcdr arity value-stack))
   environment))

(defun cursorfree--lisp-funcall-n (function arity environment)
  (cursorfree--lisp-funcall-list
   (lambda (value-stack)
     (append
      (apply function (reverse (take arity value-stack)))
      (nthcdr arity value-stack)))
   environment))

(defun cursorfree-defun-internal (instruction-name args body)
  (eval `(defun ,instruction-name ,args ,@body)))

(defun cursorfree-define-lisp-instruction-impl (funcaller instruction-name args body)
  (declare (indent defun))
  (cursorfree-defun-internal instruction-name args body)
  (let ((environment-function
         (lambda (environment)
           (funcall funcaller
                    instruction-name
                    (length args)
                    environment))))
    (cursorfree-define-instruction
      instruction-name
      environment-function)
    (put instruction-name
         'cursorfree--environment-function
         environment-function)))

(defmacro cursorfree-defmodifier (instruction-name args &rest body)
  (declare (indent defun))
  (cursorfree-define-lisp-instruction-impl #'cursorfree--lisp-funcall-1 instruction-name args body)
  `(identity ',instruction-name))

(defmacro cursorfree-defaction (instruction-name args &rest body)
  (declare (indent defun))
  (cursorfree-define-lisp-instruction-impl #'cursorfree--lisp-funcall-0 instruction-name args body)
  `(identity ',instruction-name))

(defmacro cursorfree-defmodifier-multi (instruction-name args &rest body)
  (declare (indent defun))
  (cursorfree-define-lisp-instruction-impl #'cursorfree--lisp-funcall-n instruction-name args body)
  `(identity ',instruction-name))

(defun cursorfree--on-environment (function environment)
  (funcall (get function 'cursorfree--environment-function) environment))

;;; Rest
(defun cursorfree--markify-region (region)
  (cons (if (markerp (car region))
            (car region)
          (move-marker (make-marker) (car region)))
        (if (markerp (cdr region))
            (cdr region)
          (move-marker (make-marker) (cdr region)))))

(defun cursorfree--bounds-of-thing-at (thing position)
  (save-excursion
    (goto-char position)
    (cursorfree--markify-region
     (bounds-of-thing-at-point thing))))

(defun cursorfree--make-target (content-region)
  (cursorfree--markify-region content-region))

(defun cursorfree--make-target-from-hat (character &optional color shape)
  (cursorfree--make-target
   (hatty-locate-token-region character color shape)))

(defun cursorfree--pusher (value)
  "Return instruction pushing VALUE to the value stack."
  (lambda (environment)
    (cursorfree--push-value-pure environment value)))

(defun cursorfree--deletion-region (target)
  (cursorfree--markify-region
   (save-excursion
     (goto-char (cdr target))
     (if (/= 0 (skip-chars-forward "[:space:]\n"))
         (cons (car target) (point))
       (goto-char (car target))
       (skip-chars-backward "[:space:]\n")
       (cons (point) (cdr target))))))

(defun cursorfree--target-string (target)
  (buffer-substring (car target) (cdr target)))

(defun cursorfree--target-delete (target)
  (delete-region (car target) (cdr target)))

(defun cursorfree--insert-at (position string)
  (save-excursion
    (goto-char position)
    (insert string)))

(cursorfree-defaction cursorfree--target-select (target)
  "Set active region to TARGET."
  (set-mark (car target))
  (goto-char (cdr target)))

(cursorfree-defaction cursorfree--target-jump-beginning (target)
  "Move point to beginning of TARGET."
  (goto-char (car target)))

(cursorfree-defaction cursorfree--target-jump-end (target)
  "Move point to end of TARGET."
  (goto-char (cdr target)))

(cursorfree-defaction cursorfree--target-indent (target)
  "Indent TARGET."
  (indent-region (car target) (cdr target)))


(cursorfree-defaction cursorfree--target-chuck (target)
  "Delete TARGET and indent the resulting text."
  (cursorfree--target-delete (cursorfree--deletion-region target))
  (cursorfree--target-indent
   (cursorfree--bounds-of-thing-at 'line (car target))))

(cursorfree-defaction cursorfree--target-bring (target)
  (insert (cursorfree--target-string target)))

(cursorfree-defaction cursorfree--target-overwrite (target string)
  (cursorfree--target-delete target)
  (cursorfree--insert-at (car target) string))

(cursorfree-defaction cursorfree--target-move (target)
  (cursorfree--target-bring target)
  (cursorfree--target-chuck target))

(cursorfree-defaction cursorfree--target-swap (target1 target2)
  (let ((string1 (cursorfree--target-string target1))
        (string2 (cursorfree--target-string target2)))
    (cursorfree--target-overwrite target1 string2)
    (cursorfree--target-overwrite target2 string1)))

(cursorfree-defaction cursorfree--target-change (target)
  (cursorfree--target-delete target)
  (goto-char (car target)))

(cursorfree-defaction cursorfree--target-clone (target)
  (cursorfree--insert-at (cdr target) (cursorfree--target-string target)))

(cursorfree-defaction cursorfree--target-copy (target)
  (copy-region-as-kill (car target) (cdr target)))

(cursorfree-defaction cursorfree--target-comment (target)
  (comment-region (car target) (cdr target)))

(cursorfree-defaction cursorfree--target-uncomment (target)
  (uncomment-region (car target) (cdr target)))

(cursorfree-defaction cursorfree--target-narrow (target)
  (narrow-to-region (car target) (cdr target)))

(cursorfree-defaction cursorfree--target-fill (target)
  (fill-region (car target) (cdr target)))

(cursorfree-defaction cursorfree--target-capitalize (target)
  (capitalize-region (car target) (cdr target)))

(cursorfree-defaction cursorfree--target-upcase (target)
  (upcase-region (car target) (cdr target)))

(cursorfree-defaction cursorfree--target-downcase (target)
  (downcase-region (car target) (cdr target)))

(cursorfree-defaction cursorfree--target-crown (target)
  (save-excursion
    (cursorfree--target-jump-beginning target)
    (recenter 0)))

(cursorfree-defaction cursorfree--target-center (target)
  (save-excursion
    (cursorfree--target-jump-beginning target)
    (recenter nil)))

(cursorfree-defaction cursorfree--target-bottom (target)
  (save-excursion
    (cursorfree--target-jump-beginning target)
    (recenter -1)))

(cursorfree-defaction cursorfree--target-wrap-parentheses (target parenthesis)
  (save-excursion
    (goto-char (car target))
    (insert parenthesis)
    (goto-char (cdr target))
    (insert
     (pcase parenthesis
       (?\( ?\))
       (?\[ ?\])
       (?< ?>)
       (?{ ?})
       (_ parenthesis)))))

(defvar cursorfree-dwim-follow-alist
  `((org-mode . org-open-at-point)
    (Info-mode . Info-try-follow-nearest-node)
    (help-mode . push-button)
    (dired-mode . dired-find-file)
    (compilation-mode . compile-goto-error)
    (grep-mode . compile-goto-error)
    (eww-mode . ,(lambda ()
                   (if (get-text-property (point) 'eww-form)
                       (eww-submit)
                    (eww-follow-link))))))

(defun cursorfree--dwim-follow ()
  "Try to follow the thing at point."
  (if-let ((follow-action (alist-get major-mode cursorfree-dwim-follow-alist)))
      (funcall follow-action)))

(cursorfree-defaction cursorfree--target-pick (target)
  (save-excursion
    (goto-char (car target))
    (cursorfree--dwim-follow)))

(defvar cursorfree-actions
  `(("select" . ,(cursorfree--get-instruction 'cursorfree--target-select))
    ("copy" . ,(cursorfree--get-instruction 'cursorfree--target-copy))
    ("chuck" . ,(cursorfree--get-instruction 'cursorfree--target-chuck))
    ("bring" . ,(cursorfree--get-instruction 'cursorfree--target-bring))
    ("move" . ,(cursorfree--get-instruction 'cursorfree--target-move))
    ("swap" . ,(cursorfree--get-instruction 'cursorfree--target-swap))
    ("clone" . ,(cursorfree--get-instruction 'cursorfree--target-clone))
    ("jump" . ,(cursorfree--get-instruction 'cursorfree--target-jump-beginning))
    ("pre" . ,(cursorfree--get-instruction 'cursorfree--target-jump-beginning))
    ("post" . ,(cursorfree--get-instruction 'cursorfree--target-jump-end))
    ("change" . ,(cursorfree--get-instruction 'cursorfree--target-change))
    ("comment" . ,(cursorfree--get-instruction 'cursorfree--target-comment))
    ("uncomment" . ,(cursorfree--get-instruction 'cursorfree--target-uncomment))
    ("indent" . ,(cursorfree--get-instruction 'cursorfree--target-indent))
    ("narrow" . ,(cursorfree--get-instruction 'cursorfree--target-narrow))
    ("wrap" . ,(cursorfree--get-instruction 'cursorfree--target-wrap-parentheses))
    ("filler" . ,(cursorfree--get-instruction 'cursorfree--target-fill))
    ("title" . ,(cursorfree--get-instruction 'cursorfree--target-capitalize))
    ("upcase" . ,(cursorfree--get-instruction 'cursorfree--target-upcase))
    ("downcase" . ,(cursorfree--get-instruction 'cursorfree--target-downcase))
    ("crown" . ,(cursorfree--get-instruction 'cursorfree--target-crown))
    ("center" . ,(cursorfree--get-instruction 'cursorfree--target-center))
    ("bottom" . ,(cursorfree--get-instruction 'cursorfree--target-bottom))
    ("pick" . ,(cursorfree--get-instruction 'cursorfree--target-pick))))

(defun cursorfree--skip-forward-from (position string)
  (save-excursion
    (goto-char position)
    (skip-chars-forward string)
    (point-marker)))

(defun cursorfree--skip-backward-from (position string)
  (save-excursion
    (goto-char position)
    (skip-chars-backward string)
    (point-marker)))

(cursorfree-defmodifier-multi cursorfree--find-occurrences (string)
  (save-excursion
    (let ((length (length string))
          matches)
      (goto-char (point-min))
      (while (search-forward string nil t)
        (push (cursorfree--markify-region
               (cons (- (point) length) (point)))
              matches))
      matches)))

(cursorfree-defmodifier cursorfree--paint-left (target)
  (cons (cursorfree--skip-backward-from (car target) "^[:space:]\n")
        (cdr target)))

(cursorfree-defmodifier cursorfree--paint-right (target)
  (cons (car target)
        (cursorfree--skip-forward-from (cdr target) "^[:space:]\n")))

(cursorfree-defmodifier cursorfree--paint (target)
  (cursorfree--paint-right (cursorfree--paint-left target)))

(cursorfree-defmodifier cursorfree--trim (target)
  (cons (cursorfree--skip-forward-from (car target) "[:space:]\n")
        (cursorfree--skip-backward-from (cdr target) "[:space:]\n")))

(cursorfree-defmodifier cursorfree--inner-parenthesis (region delimiter)
  (save-excursion
    ;; evil-inner-double-quote uses the location of point for the
    ;; expansion.  Put point at the beginning of the region.
    (goto-char (car region))
    (let ((expanded
           (funcall
            (cl-case delimiter
              (?\( #'evil-inner-paren)
              (?\[ #'evil-inner-bracket)
              (?< #'evil-inner-angle)
              (?{ #'evil-inner-curly)
              (?\" #'evil-inner-double-quote)
              (?\' #'evil-inner-single-quote)))))
      (cons (car expanded) (cadr expanded)))))

(cursorfree-defmodifier cursorfree--inner-parenthesis-any (region)
  (-max-by (-on #'> #'car)
           ;; Filter out whenever the evil-inner-*-quote messes up the
           ;; region
           (--filter (<= (car it) (car region))
                     (--keep (condition-case nil
                                 (cursorfree--inner-parenthesis region it)
                               (error nil))
                             '(?< ?{ ?\( ?\[ ?\" ?\')))))

(cursorfree-define-instruction 'cursorfree--inner-parenthesis-dwim
  (lambda (environment)
    (let ((head (cursorfree--peek-value environment)))
      (if (characterp head)
          (cursorfree--on-environment #'cursorfree--inner-parenthesis environment)
        (cursorfree--on-environment #'cursorfree--inner-parenthesis-any environment)))))

(defun cursorfree--targets-join (targets)
  (cursorfree--markify-region
   (cons (apply #'min (mapcar #'car targets))
         (apply #'max (mapcar #'cdr targets)))))

(cursorfree-defmodifier cursorfree--past (target1 target2)
  (cursorfree--targets-join (list target1 target2)))

(defun cursorfree--make-infix (instruction)
  "Return INSTRUCTION as an infix function.

Upon evaluation, this inserts the original INSTRUCTION under the
top instruction of the instruction stack."
  (lambda (environment)
    (let ((next-instruction (cursorfree--pop-instruction environment)))
      (cursorfree--push-instruction environment instruction)
      (cursorfree--push-instruction environment next-instruction))))

(cursorfree-defmodifier cursorfree--current-selection ()
  (region-bounds))

(defvar cursorfree-modifiers
  `(("paint" . ,(cursorfree--get-instruction 'cursorfree--paint))
    ("leftpaint" . ,(cursorfree--get-instruction 'cursorfree--paint-left))
    ("rightpaint" . ,(cursorfree--get-instruction 'cursorfree--paint-right))
    ("trim" . ,(cursorfree--get-instruction 'cursorfree--trim))
    ("past" . ,(cursorfree--make-infix (cursorfree--get-instruction 'cursorfree--past)))
    ("selection" . ,(cursorfree--get-instruction 'cursorfree--current-selection))
    ("every instance" . ,(cursorfree--get-instruction 'cursorfree--find-occurrences))
    ("inside" . ,(cursorfree--get-instruction 'cursorfree--inner-parenthesis-dwim))))

;;; cursorfree.el ends soon
(provide 'cursorfree)
;;; cursorfree.el ends here
