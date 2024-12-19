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

(defun he--debug (instructions)
  (let* ((debug-buffer (generate-new-buffer "*hatty-edit debug*"))
         (state (he--make-environment instructions))
         (state-history (list))
         (format-state (lambda (state)
                         (format "Data:\n%s\n\nInstructions:\n%s"
                                 (mapconcat (lambda (v) (format "%s" v))
                                            (he--environment-value-stack state) "\n")
                                 (mapconcat (lambda (v) (format "%s" v))
                                            (he--environment-instruction-stack state) "\n")))))
    (switch-to-buffer debug-buffer)
    (insert (funcall format-state state))

    (local-set-key (kbd "SPC")
                   (lambda ()
                     (interactive)
                     (push (he--clone-environment state) state-history)
                     (let ((tail (cdr (he--environment-instruction-stack state))))
                       (he--step state)
                       (while (and
                               (he--environment-instruction-stack state)
                               (not (eq (he--environment-instruction-stack state) tail)))
                         (he--step state)))
                     (insert "\n-----------------\n" (funcall format-state state))))

    (local-set-key (kbd "i")
                   (lambda ()
                     (interactive)
                     (push (he--clone-environment state) state-history)
                     (he--step state)
                     (push (he--clone-environment state) state-history)
                     (insert "\n-----------------\n" (funcall format-state state))))

    (local-set-key (kbd "u")
                   (lambda ()
                     (interactive)
                     (setq state (pop state-history))
                     (insert "\n-----------------\n" (funcall format-state state))))

    (local-set-key (kbd "q")
                   #'kill-this-buffer)))

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
  (let ((internal-name
         (intern (concat "hatty-edit-i--"
                         (symbol-name instruction-name)))))
    (eval `(defun ,internal-name ,args ,@body))
    internal-name))

(defmacro he--define-lisp-instruction-1 (instruction-name args &rest body)
  (declare (indent defun))
  (let* ((internal-function
          (he--defun-internal instruction-name args body))
         (environment-function
          (lambda (environment)
            (he--lisp-funcall-1 internal-function
                                (length args)
                                environment))))
    (he--define-instruction
      instruction-name
      environment-function)
    (put internal-function
         'he--environment-function
         environment-function)
    `(identity ',instruction-name)))

(defmacro he--define-lisp-instruction-0 (instruction-name args &rest body)
  (declare (indent defun))
  (let* ((internal-function
          (he--defun-internal instruction-name args body))
         (environment-function
          (lambda (environment)
            (he--lisp-funcall-0 internal-function
                              (length args)
                              environment))))
    (he--define-instruction
      instruction-name
      environment-function)
    (put internal-function
         'he--environment-function
         environment-function)
    `(identity ',instruction-name)))

(defmacro he--define-lisp-instruction-n (instruction-name args &rest body)
  (declare (indent defun))
  (let* ((internal-function
          (he--defun-internal instruction-name args body))
         (environment-function
          (lambda (environment)
            (he--lisp-funcall-n internal-function
                              (length args)
                              environment))))
    (he--define-instruction
      instruction-name
      environment-function)
    (put internal-function
         'he--environment-function
         environment-function)
    `(identity ',instruction-name)))

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
  (lambda (environment)
    (he--push-value-pure environment
      (he--make-target
       (hatty-locate-token-region character color shape)))))

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

(he--define-lisp-instruction-0 target-select (target)
  "Set active region to TARGET."
  (set-mark (car target))
  (goto-char (cdr target)))

(he--define-lisp-instruction-0 target-jump-beginning (target)
  "Move point to beginning of TARGET."
  (goto-char (car target)))

(he--define-lisp-instruction-0 target-jump-end (target)
  "Move point to end of TARGET."
  (goto-char (cdr target)))

(defun he--target-delete (target)
  (delete-region (car target) (cdr target)))

(he--define-lisp-instruction-0 target-indent (target)
  "Indent TARGET."
  (indent-region (car target) (cdr target)))


(he--define-lisp-instruction-0 target-chuck (target)
  "Delete TARGET and indent the resulting text."
  (he--target-delete (he--deletion-region target))
  (he-i--target-indent
   (he--bounds-of-thing-at 'line (car target))))

(defun he--insert-at (position string)
  (save-excursion
    (goto-char position)
    (insert string)))

(he--define-lisp-instruction-0 target-insert (target string)
  (he--insert-at (car target) string))

(he--define-lisp-instruction-0 target-bring (target)
  (insert (he--target-string target)))

(he--define-lisp-instruction-0 target-overwrite (target string)
  (he--target-delete target)
  (he-i--target-insert target string))

(he--define-lisp-instruction-0 target-bring-overwrite (target-to target-from)
  (he-i--target-overwrite
   target-to
   (he--target-string target-from)))

(he--define-lisp-instruction-0 target-move (target)
  (he-i--target-bring target)
  (he-i--target-chuck target))

(he--define-lisp-instruction-0 target-swap (target1 target2)
  (let ((string1 (he--target-string target1))
        (string2 (he--target-string target2)))
    (he-i--target-overwrite target1 string2)
    (he-i--target-overwrite target2 string1)))

(he--define-lisp-instruction-0 target-change (target)
  (he--target-delete target)
  (goto-char (car target)))

(he--define-lisp-instruction-0 target-clone (target)
  (he--insert-at (cdr target) (he--target-string target)))

(he--define-lisp-instruction-0 target-copy (target)
  (copy-region-as-kill (car target) (cdr target)))

(he--define-lisp-instruction-0 target-comment (target)
  (comment-region (car target) (cdr target)))

(he--define-lisp-instruction-0 target-uncomment (target)
  (uncomment-region (car target) (cdr target)))

(he--define-lisp-instruction-0 target-narrow (target)
  (narrow-to-region (car target) (cdr target)))

(he--define-lisp-instruction-0 target-fill (target)
  (fill-region (car target) (cdr target)))

(he--define-lisp-instruction-0 target-wrap-parentheses (target parenthesis)
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


;; Remove everything interspersing list of targets
(defvar he-actions
  `(("select" . (,(he--get-instruction 'target-select)))
    ("copy" . (,(he--get-instruction 'target-copy)))
    ("chuck" . (,(he--get-instruction 'target-chuck)))
    ("bring" . (,(he--get-instruction 'target-bring)))
    ("move" . (,(he--get-instruction 'target-move)))
    ("swap" . (,(he--get-instruction 'target-swap)))
    ("clone" . (,(he--get-instruction 'target-clone)))
    ("jump" . (,(he--get-instruction 'target-jump-beginning)))
    ("pre" . (,(he--get-instruction 'target-jump-beginning)))
    ("post" . (,(he--get-instruction 'target-jump-end)))
    ("change" . (,(he--get-instruction 'target-change)))
    ("comment" . (,(he--get-instruction 'target-comment)))
    ("uncomment" . (,(he--get-instruction 'target-uncomment)))
    ("indent" . (,(he--get-instruction 'target-indent)))
    ("narrow" . (,(he--get-instruction 'target-narrow)))
    ("wrap" . (,(he--get-instruction 'target-wrap-parentheses)))
    ("filler" . (,(he--get-instruction 'target-fill)))))

;;;; Default modifiers:

(he--define-lisp-instruction-1 skip-forward-from (position string)
  (save-excursion
    (goto-char position)
    (skip-chars-forward string)
    (point-marker)))

(he--define-lisp-instruction-1 skip-backward-from (position string)
  (save-excursion
    (goto-char position)
    (skip-chars-backward string)
    (point-marker)))

(he--define-lisp-instruction-n find-occurrences (string)
  (save-excursion
    (let ((length (length string))
          matches)
      (goto-char (point-min))
      (while (search-forward string nil t)
        (push (he--markify-region
               (cons (- (point) length) (point)))
              matches))
      matches)))

(he--define-lisp-instruction-1 paint-left (target)
  (cons (he-i--skip-backward-from (car target) "^[:space:]\n")
        (cdr target)))

(he--define-lisp-instruction-1 paint-right (target)
  (cons (car target)
        (he-i--skip-forward-from (cdr target) "^[:space:]\n")))

(he--define-lisp-instruction-1 paint (target)
  (he-i--paint-right (he-i--paint-left target)))

(he--define-lisp-instruction-1 trim (target)
  (cons (he-i--skip-forward-from (car target) "[:space:]\n")
        (he-i--skip-backward-from (cdr target) "[:space:]\n")))

(he--define-lisp-instruction-1 inner-parenthesis (region delimiter)
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

(he--define-lisp-instruction-1 inner-parenthesis-any (region)
  (-max-by (-on #'> #'car)
           ;; Filter out whenever the evil-inner-*-quote messes up the
           ;; region
           (--filter (<= (car it) (car region))
                     (--keep (condition-case nil
                                 (he-i--inner-parenthesis region it)
                               (error nil))
                             '(?< ?{ ?\( ?\[ ?\" ?\')))))

(he--define-instruction 'inner-parenthesis-dwim
  (lambda (environment)
    (let ((head (he--peek-value environment)))
      (if (characterp head)
          (he--on-environment #'he-i--inner-parenthesis environment)
        (he--on-environment #'he-i--inner-parenthesis-any environment)))))

(defun he--targets-join (targets)
  (he--markify-region
   (cons (apply #'min (mapcar #'car targets))
         (apply #'max (mapcar #'cdr targets)))))

(he--define-lisp-instruction-1 past (target1 target2)
  (he-i--targets-join (list target1 target2)))

(he--define-instruction 'make-infix
  (lambda (environment)
    (let ((function (he--pop-value environment))
          (next-instruction (he--pop-instruction environment)))
      (he--push-instructions environment function)
      (he--push-instruction environment next-instruction))))

(defun he--make-infix (instruction)
  "Return INSTRUCTION as an infix function.

Upon evaluation, this inserts the original INSTRUCTION under the
top instruction of the instruction stack."
  (lambda (environment)
    (let ((next-instruction (he--pop-instruction environment)))
      (he--push-instruction environment instruction)
      (he--push-instruction environment next-instruction))))

(he--define-lisp-instruction-1 current-selection ()
  (region-bounds))

(defvar he-modifiers
  `(("paint" . (,(he--get-instruction 'paint)))
    ("leftpaint" . (,(he--get-instruction 'paint-left)))
    ("rightpaint" . (,(he--get-instruction 'paint-right)))
    ("trim" . (,(he--get-instruction 'trim)))
    ("past" . (,(he--make-infix (he--get-instruction 'past))))
    ("selection" . (,(he--get-instruction 'current-selection)))
    ("every instance" . (,(he--get-instruction 'find-occurrences)))
    ("inside" . (,(he--get-instruction 'inner-parenthesis-dwim)))))


;;; hatty-edit.el ends soon
(provide 'hatty-edit)

;; Local Variables:
;; read-symbol-shorthands: (("he-" . "hatty-edit-"))
;; End:
;;; hatty-edit.el ends here
