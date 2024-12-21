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

(defun cursorfree--apply-on-stack (function stack)
  (let* ((arity (car (func-arity function)))
         (args (reverse (take arity stack)))
         (tail (nthcdr arity stack)))
    (cons (apply function args) tail)))

(defun cursorfree--to-action (function)
  (lambda (environment)
    (let* ((e (cursorfree--clone-environment environment))
           (values (cursorfree--environment-value-stack e)))
      (setf (cursorfree--environment-value-stack e)
            (cursorfree--apply-on-stack function values))
      (cursorfree--pop-value e) ; Ignore return value
      e)))

(defun cursorfree--to-modifier (function)
  (lambda (environment)
    (let* ((e (cursorfree--clone-environment environment))
           (values (cursorfree--environment-value-stack e)))
      (setf (cursorfree--environment-value-stack e)
            (cursorfree--apply-on-stack function values))
      e)))

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

(defun cursorfree--target-select (target)
  "Set active region to TARGET."
  (set-mark (car target))
  (goto-char (cdr target)))

(defun cursorfree--target-jump-beginning (target)
  "Move point to beginning of TARGET."
  (goto-char (car target)))

(defun cursorfree--target-jump-end (target)
  "Move point to end of TARGET."
  (goto-char (cdr target)))

(defun cursorfree--target-indent (target)
  "Indent TARGET."
  (indent-region (car target) (cdr target)))


(defun cursorfree--target-chuck (target)
  "Delete TARGET and indent the resulting text."
  (cursorfree--target-delete (cursorfree--deletion-region target))
  (cursorfree--target-indent
   (cursorfree--bounds-of-thing-at 'line (car target))))

(defun cursorfree--target-bring (target)
  (insert (cursorfree--target-string target)))

(defun cursorfree--target-overwrite (target string)
  (cursorfree--target-delete target)
  (cursorfree--insert-at (car target) string))

(defun cursorfree--target-move (target)
  (cursorfree--target-bring target)
  (cursorfree--target-chuck target))

(defun cursorfree--target-swap (target1 target2)
  (let ((string1 (cursorfree--target-string target1))
        (string2 (cursorfree--target-string target2)))
    (cursorfree--target-overwrite target1 string2)
    (cursorfree--target-overwrite target2 string1)))

(defun cursorfree--target-change (target)
  (cursorfree--target-delete target)
  (goto-char (car target)))

(defun cursorfree--target-clone (target)
  (cursorfree--insert-at (cdr target) (cursorfree--target-string target)))

(defun cursorfree--target-copy (target)
  (copy-region-as-kill (car target) (cdr target)))

(defun cursorfree--target-comment (target)
  (comment-region (car target) (cdr target)))

(defun cursorfree--target-uncomment (target)
  (uncomment-region (car target) (cdr target)))

(defun cursorfree--target-narrow (target)
  (narrow-to-region (car target) (cdr target)))

(defun cursorfree--target-fill (target)
  (fill-region (car target) (cdr target)))

(defun cursorfree--target-capitalize (target)
  (capitalize-region (car target) (cdr target)))

(defun cursorfree--target-upcase (target)
  (upcase-region (car target) (cdr target)))

(defun cursorfree--target-downcase (target)
  (downcase-region (car target) (cdr target)))

(defun cursorfree--target-crown (target)
  (save-excursion
    (cursorfree--target-jump-beginning target)
    (recenter 0)))

(defun cursorfree--target-center (target)
  (save-excursion
    (cursorfree--target-jump-beginning target)
    (recenter nil)))

(defun cursorfree--target-bottom (target)
  (save-excursion
    (cursorfree--target-jump-beginning target)
    (recenter -1)))

(defun cursorfree--target-wrap-parentheses (target parenthesis)
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

(defun cursorfree--target-pick (target)
  (save-excursion
    (goto-char (car target))
    (cursorfree--dwim-follow)))

(defvar cursorfree-actions
  `(("select" . ,(cursorfree--to-action #'cursorfree--target-select))
    ("copy" . ,(cursorfree--to-action #'cursorfree--target-copy))
    ("chuck" . ,(cursorfree--to-action #'cursorfree--target-chuck))
    ("bring" . ,(cursorfree--to-action #'cursorfree--target-bring))
    ("move" . ,(cursorfree--to-action #'cursorfree--target-move))
    ("swap" . ,(cursorfree--to-action #'cursorfree--target-swap))
    ("clone" . ,(cursorfree--to-action #'cursorfree--target-clone))
    ("jump" . ,(cursorfree--to-action #'cursorfree--target-jump-beginning))
    ("pre" . ,(cursorfree--to-action #'cursorfree--target-jump-beginning))
    ("post" . ,(cursorfree--to-action #'cursorfree--target-jump-end))
    ("change" . ,(cursorfree--to-action #'cursorfree--target-change))
    ("comment" . ,(cursorfree--to-action #'cursorfree--target-comment))
    ("uncomment" . ,(cursorfree--to-action #'cursorfree--target-uncomment))
    ("indent" . ,(cursorfree--to-action #'cursorfree--target-indent))
    ("narrow" . ,(cursorfree--to-action #'cursorfree--target-narrow))
    ("wrap" . ,(cursorfree--to-action #'cursorfree--target-wrap-parentheses))
    ("filler" . ,(cursorfree--to-action #'cursorfree--target-fill))
    ("title" . ,(cursorfree--to-action #'cursorfree--target-capitalize))
    ("upcase" . ,(cursorfree--to-action #'cursorfree--target-upcase))
    ("downcase" . ,(cursorfree--to-action #'cursorfree--target-downcase))
    ("crown" . ,(cursorfree--to-action #'cursorfree--target-crown))
    ("center" . ,(cursorfree--to-action #'cursorfree--target-center))
    ("bottom" . ,(cursorfree--to-action #'cursorfree--target-bottom))
    ("pick" . ,(cursorfree--to-action #'cursorfree--target-pick))))

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

(defun cursorfree--paint-left (target)
  (cons (cursorfree--skip-backward-from (car target) "^[:space:]\n")
        (cdr target)))

(defun cursorfree--paint-right (target)
  (cons (car target)
        (cursorfree--skip-forward-from (cdr target) "^[:space:]\n")))

(defun cursorfree--paint (target)
  (cursorfree--paint-right (cursorfree--paint-left target)))

(defun cursorfree--trim (target)
  (cons (cursorfree--skip-forward-from (car target) "[:space:]\n")
        (cursorfree--skip-backward-from (cdr target) "[:space:]\n")))

(defun cursorfree--inner-parenthesis (region delimiter)
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

(defun cursorfree--inner-parenthesis-any (region)
  (-max-by (-on #'> #'car)
           ;; Filter out whenever the evil-inner-*-quote messes up the
           ;; region
           (--filter (<= (car it) (car region))
                     (--keep (condition-case nil
                                 (cursorfree--inner-parenthesis region it)
                               (error nil))
                             '(?< ?{ ?\( ?\[ ?\" ?\')))))

(defun cursorfree--inner-parenthesis-dwim (environment)
  (let* ((head (cursorfree--peek-value environment)))
    (funcall (if (characterp head)
                 (cursorfree--to-modifier #'cursorfree--inner-parenthesis)
               (cursorfree--to-modifier #'cursorfree--inner-parenthesis-any))
             environment)))

(defun cursorfree--targets-join (targets)
  (cursorfree--markify-region
   (cons (apply #'min (mapcar #'car targets))
         (apply #'max (mapcar #'cdr targets)))))

(defun cursorfree--past (target1 target2)
  (cursorfree--targets-join (list target1 target2)))

(defun cursorfree--make-infix (instruction)
  "Return INSTRUCTION as an infix function.

Upon evaluation, this inserts the original INSTRUCTION under the
top instruction of the instruction stack."
  (lambda (environment)

    (let* ((e (cursorfree--clone-environment environment))
           (next-instruction (cursorfree--pop-instruction e)))
      (cursorfree--push-instruction e instruction)
      (cursorfree--push-instruction e next-instruction)
      e)))

(defun cursorfree--current-selection ()
  (region-bounds))

(defvar cursorfree-modifiers
  `(("paint" . ,(cursorfree--to-modifier #'cursorfree--paint))
    ("leftpaint" . ,(cursorfree--to-modifier #'cursorfree--paint-left))
    ("rightpaint" . ,(cursorfree--to-modifier #'cursorfree--paint-right))
    ("trim" . ,(cursorfree--to-modifier #'cursorfree--trim))
    ("past" . ,(cursorfree--make-infix (cursorfree--to-modifier #'cursorfree--past)))
    ("selection" . ,(cursorfree--to-modifier #'cursorfree--current-selection))
    ("inside" . cursorfree--inner-parenthesis-dwim)))

;;; cursorfree.el ends soon
(provide 'cursorfree)
;;; cursorfree.el ends here
