;;; hatty-edit.el --- Interface for hatty            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.0.0
;; Package-Requires: ((emacs "25.1") (hatty "0.2.0"))
;; Created: 06 Sep 2024

;; hatty-edit.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; hatty-edit.el is distributed in the hope that it will be useful,
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

(cl-defstruct he--environment
  (instruction-stack nil) (value-stack nil))

(defun he--make-environment (instructions &optional value-stack)
  (make-hatty-edit--environment
   :instruction-stack instructions
   :value-stack value-stack))

(defun he--clone-environment (environment)
  (make-hatty-edit--environment
   :value-stack (he--environment-value-stack environment)
   :instruction-stack (he--environment-instruction-stack environment)))

(defun he--push-instruction (environment instruction)
  (push instruction (he--environment-instruction-stack environment)))

(defun he--push-instructions (environment instructions)
  (dolist (instruction (reverse instructions))
    (he--push-instruction environment instruction)))

(defun he--pop-instruction (environment)
  (pop (he--environment-instruction-stack environment)))

(defun he--push-value (environment value)
  (declare (indent defun))
  (push value (he--environment-value-stack environment)))

(defun he--push-value-pure (environment value)
  (declare (indent defun))
  (let ((new-environment (he--clone-environment environment)))
    (he--push-value new-environment value)
    new-environment))

(defun he--push-values (environment values)
  (declare (indent defun))
  (dolist (value (reverse values))
    (he--push-value environment value)))

(defun he--pop-value (environment)
  (declare (indent defun))
  (cl-destructuring-bind (head . tail)
      (he--environment-value-stack environment)
    (setf (he--environment-value-stack environment) tail)
    head))

(defun he--pop-values (environment n)
  (declare (indent defun))
  (let ((acc nil))
    (dotimes (i n (reverse acc))
      (push (he--pop-value environment) acc))))

(defun he--peek-value (environment)
  (declare (indent defun))
  (car (he--environment-value-stack environment)))

(defun he--define-instruction-pure (name instruction)
  (declare (indent defun))
  (put name 'he--instruction instruction)
  name)

(defun he--define-instruction (name instruction)
  (declare (indent defun))
  (he--define-instruction-pure name
    (lambda (environment)
      (let ((mutable-environment (he--clone-environment environment)))
        (funcall instruction mutable-environment)
        mutable-environment))))

(defun he--get-instruction (name)
  (let ((instruction
         (get name 'he--instruction)))
    (unless instruction
      ;; TODO: Custom error symbol?
      (signal 'error (list "No such hatty-edit instruction: " name)))
    instruction))

(defun he--step (environment)
  (let* ((new-environment (he--clone-environment environment))
         (instruction (he--pop-instruction new-environment)))
    (funcall instruction new-environment)))

(defun he--evaluate-environment (environment)
  "Evaluate ENVIRONMENT and return the final value stack."
  (declare (indent defun))
  (while (he--environment-instruction-stack environment)
    (setq environment (he--step environment)))
  (he--environment-value-stack environment))

(defun he--evaluate (instructions)
  (he--evaluate-environment
    (he--make-environment instructions)))

;;; Core macros and functions for defining instructions

;; The core instructions are what all other words build upon.

(defun he--lisp-funcall-1 (function arity environment)
  (let ((arguments (reverse (he--pop-values environment arity))))
    (he--push-value environment
      (apply function arguments))))

(defun he--lisp-funcall-0 (function arity environment)
  (let ((arguments (reverse (he--pop-values environment arity))))
    (apply function arguments)))

(defun he--lisp-funcall-n (function arity environment)
  (let ((arguments (reverse (he--pop-values environment arity))))
    (he--push-values environment
      (apply function arguments))))

(defun he--defun-internal (instruction-name args body)
  (eval `(defun ,instruction-name ,args ,@body)))

(defun he--define-lisp-instruction-impl (funcaller instruction-name args body)
  (declare (indent defun))
  (he--defun-internal instruction-name args body)
  (let ((environment-function
         (lambda (environment)
           (funcall funcaller
                    instruction-name
                    (length args)
                    environment))))
    (he--define-instruction
      instruction-name
      environment-function)
    (put instruction-name
         'he--environment-function
         environment-function)))

(defmacro he--define-lisp-instruction-1 (instruction-name args &rest body)
  (declare (indent defun))
  (he--define-lisp-instruction-impl #'he--lisp-funcall-1 instruction-name args body)
  `(identity ',instruction-name))

(defmacro he--define-lisp-instruction-0 (instruction-name args &rest body)
  (declare (indent defun))
  (he--define-lisp-instruction-impl #'he--lisp-funcall-0 instruction-name args body)
  `(identity ',instruction-name))

(defmacro he--define-lisp-instruction-n (instruction-name args &rest body)
  (declare (indent defun))
  (he--define-lisp-instruction-impl #'he--lisp-funcall-n instruction-name args body)
  `(identity ',instruction-name))

(defun he--on-environment (function environment)
  (funcall (get function 'he--environment-function) environment))

;;; Rest
(defun he--markify-region (region)
  (cons (if (markerp (car region))
            (car region)
          (move-marker (make-marker) (car region)))
        (if (markerp (cdr region))
            (cdr region)
          (move-marker (make-marker) (cdr region)))))

(defun he--bounds-of-thing-at (thing position)
  (save-excursion
    (goto-char position)
    (he--markify-region
     (bounds-of-thing-at-point thing))))

(defun he--make-target (content-region)
  (he--markify-region content-region))

(defun he--make-target-from-hat (character &optional color shape)
  (he--make-target
   (hatty-locate-token-region character color shape)))

(defun he--pusher (value)
  "Return instruction pushing VALUE to the value stack."
  (lambda (environment)
    (he--push-value-pure environment value)))

(defun he--make-thing-modifier (thing)
  ;; Expands from car of region
  (let ((function (lambda (target)
                    (he--bounds-of-thing-at thing (car target)))))
    `(,function lisp-funcall)))

(defun he--deletion-region (target)
  (he--markify-region
   (save-excursion
     (goto-char (cdr target))
     (if (/= 0 (skip-chars-forward "[:space:]\n"))
         (cons (car target) (point))
       (goto-char (car target))
       (skip-chars-backward "[:space:]\n")
       (cons (point) (cdr target))))))

(defun he--target-string (target)
  (buffer-substring (car target) (cdr target)))

(defun he--target-delete (target)
  (delete-region (car target) (cdr target)))

(defun he--insert-at (position string)
  (save-excursion
    (goto-char position)
    (insert string)))

(he--define-lisp-instruction-0 he--target-select (target)
  "Set active region to TARGET."
  (set-mark (car target))
  (goto-char (cdr target)))

(he--define-lisp-instruction-0 he--target-jump-beginning (target)
  "Move point to beginning of TARGET."
  (goto-char (car target)))

(he--define-lisp-instruction-0 he--target-jump-end (target)
  "Move point to end of TARGET."
  (goto-char (cdr target)))

(he--define-lisp-instruction-0 he--target-indent (target)
  "Indent TARGET."
  (indent-region (car target) (cdr target)))


(he--define-lisp-instruction-0 he--target-chuck (target)
  "Delete TARGET and indent the resulting text."
  (he--target-delete (he--deletion-region target))
  (he--target-indent
   (he--bounds-of-thing-at 'line (car target))))

(he--define-lisp-instruction-0 he--target-bring (target)
  (insert (he--target-string target)))

(he--define-lisp-instruction-0 he--target-overwrite (target string)
  (he--target-delete target)
  (he--insert-at (car target) string))

(he--define-lisp-instruction-0 he--target-bring-overwrite (target-to target-from)
  (he--target-overwrite
   target-to
   (he--target-string target-from)))

(he--define-lisp-instruction-0 he--target-move (target)
  (he--target-bring target)
  (he--target-chuck target))

(he--define-lisp-instruction-0 he--target-swap (target1 target2)
  (let ((string1 (he--target-string target1))
        (string2 (he--target-string target2)))
    (he--target-overwrite target1 string2)
    (he--target-overwrite target2 string1)))

(he--define-lisp-instruction-0 he--target-change (target)
  (he--target-delete target)
  (goto-char (car target)))

(he--define-lisp-instruction-0 he--target-clone (target)
  (he--insert-at (cdr target) (he--target-string target)))

(he--define-lisp-instruction-0 he--target-copy (target)
  (copy-region-as-kill (car target) (cdr target)))

(he--define-lisp-instruction-0 he--target-comment (target)
  (comment-region (car target) (cdr target)))

(he--define-lisp-instruction-0 he--target-uncomment (target)
  (uncomment-region (car target) (cdr target)))

(he--define-lisp-instruction-0 he--target-narrow (target)
  (narrow-to-region (car target) (cdr target)))

(he--define-lisp-instruction-0 he--target-fill (target)
  (fill-region (car target) (cdr target)))

(he--define-lisp-instruction-0 he--target-capitalize (target)
  (capitalize-region (car target) (cdr target)))

(he--define-lisp-instruction-0 he--target-upcase (target)
  (upcase-region (car target) (cdr target)))

(he--define-lisp-instruction-0 he--target-downcase (target)
  (downcase-region (car target) (cdr target)))

(he--define-lisp-instruction-0 he--target-crown (target)
  (save-excursion
    (he--target-jump-beginning target)
    (recenter 0)))

(he--define-lisp-instruction-0 he--target-center (target)
  (save-excursion
    (he--target-jump-beginning target)
    (recenter nil)))

(he--define-lisp-instruction-0 he--target-bottom (target)
  (save-excursion
    (he--target-jump-beginning target)
    (recenter -1)))

(he--define-lisp-instruction-0 he--target-wrap-parentheses (target parenthesis)
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

(defvar he-actions
  `(("select" . ,(he--get-instruction 'he--target-select))
    ("copy" . ,(he--get-instruction 'he--target-copy))
    ("chuck" . ,(he--get-instruction 'he--target-chuck))
    ("bring" . ,(he--get-instruction 'he--target-bring))
    ("move" . ,(he--get-instruction 'he--target-move))
    ("swap" . ,(he--get-instruction 'he--target-swap))
    ("clone" . ,(he--get-instruction 'he--target-clone))
    ("jump" . ,(he--get-instruction 'he--target-jump-beginning))
    ("pre" . ,(he--get-instruction 'he--target-jump-beginning))
    ("post" . ,(he--get-instruction 'he--target-jump-end))
    ("change" . ,(he--get-instruction 'he--target-change))
    ("comment" . ,(he--get-instruction 'he--target-comment))
    ("uncomment" . ,(he--get-instruction 'he--target-uncomment))
    ("indent" . ,(he--get-instruction 'he--target-indent))
    ("narrow" . ,(he--get-instruction 'he--target-narrow))
    ("wrap" . ,(he--get-instruction 'he--target-wrap-parentheses))
    ("filler" . ,(he--get-instruction 'he--target-fill))
    ("title" . ,(he--get-instruction 'he--target-capitalize))
    ("upcase" . ,(he--get-instruction 'he--target-upcase))
    ("downcase" . ,(he--get-instruction 'he--target-downcase))
    ("crown" . ,(he--get-instruction 'he--target-crown))
    ("center" . ,(he--get-instruction 'he--target-center))
    ("bottom" . ,(he--get-instruction 'he--target-bottom))))

(defun he--skip-forward-from (position string)
  (save-excursion
    (goto-char position)
    (skip-chars-forward string)
    (point-marker)))

(defun he--skip-backward-from (position string)
  (save-excursion
    (goto-char position)
    (skip-chars-backward string)
    (point-marker)))

(he--define-lisp-instruction-n he--find-occurrences (string)
  (save-excursion
    (let ((length (length string))
          matches)
      (goto-char (point-min))
      (while (search-forward string nil t)
        (push (he--markify-region
               (cons (- (point) length) (point)))
              matches))
      matches)))

(he--define-lisp-instruction-1 he--paint-left (target)
  (cons (he--skip-backward-from (car target) "^[:space:]\n")
        (cdr target)))

(he--define-lisp-instruction-1 he--paint-right (target)
  (cons (car target)
        (he--skip-forward-from (cdr target) "^[:space:]\n")))

(he--define-lisp-instruction-1 he--paint (target)
  (he--paint-right (he--paint-left target)))

(he--define-lisp-instruction-1 he--trim (target)
  (cons (he--skip-forward-from (car target) "[:space:]\n")
        (he--skip-backward-from (cdr target) "[:space:]\n")))

(he--define-lisp-instruction-1 he--inner-parenthesis (region delimiter)
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

(he--define-lisp-instruction-1 he--inner-parenthesis-any (region)
  (-max-by (-on #'> #'car)
           ;; Filter out whenever the evil-inner-*-quote messes up the
           ;; region
           (--filter (<= (car it) (car region))
                     (--keep (condition-case nil
                                 (he--inner-parenthesis region it)
                               (error nil))
                             '(?< ?{ ?\( ?\[ ?\" ?\')))))

(he--define-instruction 'he--inner-parenthesis-dwim
  (lambda (environment)
    (let ((head (he--peek-value environment)))
      (if (characterp head)
          (he--on-environment #'he--inner-parenthesis environment)
        (he--on-environment #'he--inner-parenthesis-any environment)))))

(defun he--targets-join (targets)
  (he--markify-region
   (cons (apply #'min (mapcar #'car targets))
         (apply #'max (mapcar #'cdr targets)))))

(he--define-lisp-instruction-1 he--past (target1 target2)
  (he--targets-join (list target1 target2)))

(defun he--make-infix (instruction)
  "Return INSTRUCTION as an infix function.

Upon evaluation, this inserts the original INSTRUCTION under the
top instruction of the instruction stack."
  (lambda (environment)
    (let ((next-instruction (he--pop-instruction environment)))
      (he--push-instruction environment instruction)
      (he--push-instruction environment next-instruction))))

(he--define-lisp-instruction-1 he--current-selection ()
  (region-bounds))

(defvar he-modifiers
  `(("paint" . ,(he--get-instruction 'he--paint))
    ("leftpaint" . ,(he--get-instruction 'he--paint-left))
    ("rightpaint" . ,(he--get-instruction 'he--paint-right))
    ("trim" . ,(he--get-instruction 'he--trim))
    ("past" . ,(he--make-infix (he--get-instruction 'he--past)))
    ("selection" . ,(he--get-instruction 'he--current-selection))
    ("every instance" . ,(he--get-instruction 'he--find-occurrences))
    ("inside" . ,(he--get-instruction 'he--inner-parenthesis-dwim))))

;;; hatty-edit.el ends soon
(provide 'hatty-edit)

;; Local Variables:
;; read-symbol-shorthands: (("he-" . "hatty-edit-"))
;; End:
;;; hatty-edit.el ends here
