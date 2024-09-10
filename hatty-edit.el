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

;;; TODO
;;;; Commentary
;;;; Docstrings
;;;; TODO file

;;; Code:

(require 'hatty)
(require 'multiple-cursors)


;;;; Instruction interpreter:

(cl-defstruct hatty-edit--environment
  (instruction-stack nil) (value-stack nil))

(defvar hatty-edit--environment (make-hatty-edit--environment))

(defun hatty-edit--push-instruction (environment instruction)
  (push instruction (hatty-edit--environment-instruction-stack environment)))

(defun hatty-edit--push-instructions (environment instructions)
  (dolist (instruction (reverse instructions))
    (hatty-edit--push-instruction environment instruction)))

(defun hatty-edit--pop-instruction (environment)
  (pop (hatty-edit--environment-instruction-stack environment)))

(defun hatty-edit--push-value (environment value)
  (push value (hatty-edit--environment-value-stack environment)))

(defun hatty-edit--push-values (environment values)
  (dolist (value (reverse values))
    (hatty-edit--push-value environment value)))

(defun hatty-edit--pop-value (environment)
  (pop (hatty-edit--environment-value-stack environment)))

(defun hatty-edit--pop-values (environment n)
  (let ((acc nil))
    (dotimes (i n)
      (push (pop (hatty-edit--environment-value-stack environment))
            acc))
    (reverse acc)))

(defun hatty-edit--lift-stack-function (stack-function)
  (hatty-edit--make-instruction
   `(amalgamate-stack
     push ,stack-function
     funcall
     unlist)))

(defun hatty-edit--evaluate (instructions)
  (hatty-edit--evaluate-environment
   (make-hatty-edit--environment
    :instruction-stack instructions)))

(defun hatty-edit--define-macro (name macro)
  (declare (indent defun))
  (put name 'hatty-edit--macro macro)
  name)

(defun hatty-edit--get-macro (name)
  (let ((macro
         (get name 'hatty-edit--macro)))
    (unless macro
      ;; TODO: Custom error symbol?
      (signal 'error (list "No such hatty-edit macro: " name)))
    macro))

(defun hatty-edit--define-simple-macro (macro-name forms)
  (declare (indent defun))
  (hatty-edit--define-macro macro-name
    (lambda (environment)
      (hatty-edit--push-instructions environment forms))))

(defun hatty-edit--make-instruction (forms)
  (declare (indent defun))
  (list forms))

(defun hatty-edit--evaluate-environment (environment)
  (declare (indent defun))
  (while (hatty-edit--environment-instruction-stack environment)
    (let ((instruction (hatty-edit--pop-instruction environment)))
      (pcase instruction
        ((pred listp) (hatty-edit--push-instructions environment instruction))
        ('nop)
        ('drop (hatty-edit--pop-value environment))
        ('swap (hatty-edit--push-values environment
                (reverse (hatty-edit--pop-values environment 2))))
        ('eval (funcall (hatty-edit--pop-value environment)))
        ('list (hatty-edit--push-value environment
                                       (hatty-edit--pop-values
                                        environment
                                        (hatty-edit--pop-value environment))))
        ('unlist (hatty-edit--push-values
                  environment
                  (hatty-edit--pop-value environment)))
        ('amalgamate-stack
         (setf (hatty-edit--environment-value-stack environment)
               (list (hatty-edit--environment-value-stack environment))))
        ('apply (hatty-edit--push-value
                 environment
                 (apply (hatty-edit--pop-value environment)
                        (hatty-edit--pop-value environment))))
        ('funcall (hatty-edit--push-value
                   environment
                   (funcall (hatty-edit--pop-value environment)
                            (hatty-edit--pop-value environment))))
        ('map (let ((function (hatty-edit--pop-value environment))
                    (values (hatty-edit--pop-value environment)))
                (hatty-edit--push-value environment
                 (apply #'append
                  (mapcar (lambda (value)
                            (hatty-edit--environment-value-stack
                             (hatty-edit--evaluate-environment
                               (make-hatty-edit--environment
                                ;; Wrap operation in list: If it is a
                                ;; single symbol, it will become a
                                ;; valid program.  If it is a list,
                                ;; evaluation of that list will push
                                ;; it onto the instruction stack,
                                ;; avoiding mutation of the original
                                ;; list.
                                :instruction-stack (list function)
                                :value-stack (list value)))))
                          values)))))
        (_ (funcall (hatty-edit--get-macro instruction) environment)))))
  environment)

(hatty-edit--define-simple-macro 'eval-quoted
  '(swap push 1 list swap
    map
    unlist))

(hatty-edit--define-macro 'push
  (lambda (environment)
    (hatty-edit--push-value environment
     (hatty-edit--pop-instruction environment))))

;;;; Targets, modifiers, actions:

(defun hatty-edit--markify-region (region)
  (cons (if (markerp (car region))
            (car region)
          (move-marker (make-marker) (car region)))
        (if (markerp (cdr region))
            (cdr region)
          (move-marker (make-marker) (cdr region)))))

(defun hatty-edit--bounds-of-thing-at (thing position)
  (save-excursion
    (goto-char position)
    (hatty-edit--markify-region
     (bounds-of-thing-at-point thing))))

(cl-defun hatty-edit--make-target (content-region)
  `(push ,(hatty-edit--markify-region
           (cons (car content-region)
                 (cdr content-region)))))

(defun hatty-edit--make-target-from-hat (character &optional color shape)
  (hatty-edit--make-target
   (hatty-edit--bounds-of-thing-at
    'symbol
    (hatty-locate character color shape))))

(defun hatty-edit--make-multiple-cursors-action (function)
  (let* ((stack-function
           (lambda (regions)
             (when regions
               ;; Clear any previous multiple cursors
               (multiple-cursors-mode 0)
               (multiple-cursors-mode 1)

               (funcall function (car regions))
               (dolist (region (cdr regions))
                 (mc/create-fake-cursor-at-point)
                 (funcall function region)))
             nil)))
    (hatty-edit--lift-stack-function stack-function)))

(defun hatty-edit--make-thing-modifier (thing)
  ;; Expands from car of region
  (let ((function (lambda (target)
                    (hatty-edit--bounds-of-thing-at thing (car target)))))
    (hatty-edit--make-instruction
      `((funcall ,function)))))

(defun hatty-edit--map-all-cursors (function)
  "Perform FUNCTION on point and all fake cursors.
Return a list of the return value of FUNCTION applied with point
and mark set to the corresponding cursor.  If there are no fake
cursors, return a single value instead of a list."
  (let ((result (cons (funcall function)
                      (mapcar (lambda (fake-cursor)
                                (save-mark-and-excursion
                                  (mc/restore-state-from-overlay fake-cursor)
                                  (funcall function)))
                              (mc/all-fake-cursors)))))
    (if (eq (cdr result) nil)
        (car result)
      result)))

(defun hatty-edit--default-target ()
  (hatty-edit--map-all-cursors
   (if (region-active-p)
       (lambda ()
         (hatty-edit--make-target
          (cons (region-beginning)
                (region-end))))
     (lambda ()
       (hatty-edit--make-target
        (bounds-of-thing-at-point 'symbol))))))

(defun hatty-edit--make-parallel-modifier (modifier)
  (hatty-edit--lift-stack-function
   (lambda (regions)
     (mapcar modifier regions))))

(defun hatty-edit--make-parallel-action (action)
  (hatty-edit--lift-stack-function
   (lambda (regions)
     (mapcar action regions)
     nil)))


;;;; Default actions:

(defun hatty-edit--deletion-region (region)
  (save-excursion
    (goto-char (cdr region))
    (skip-chars-forward "[:space:]\n")
    (if (/= (point) (cdr region))
        (cons (car region) (point))
      (goto-char (car region))
      (skip-chars-backward "[:space:]\n")
      (cons (point) (cdr region)))))

(defvar hatty-edit-actions
  `(("select" .
     ,(hatty-edit--make-multiple-cursors-action
       (lambda (region)
         (set-mark (car region))
         (goto-char (cdr region)))))
    ("chuck" .
     ,(hatty-edit--make-instruction
        `((funcall
           ,(lambda (region)
              (let ((deletion-region (hatty-edit--deletion-region region)))
                (kill-region (car deletion-region) (cdr deletion-region))))))))
    ("bring" .
     ,(hatty-edit--make-instruction
        `((list 2)
          (apply ,(lambda (source destination)
                    (unless destination
                      (setq destination (cons (point) (point))))
                    (save-excursion
                      (goto-char (car destination))
                      (delete-region (car destination) (cdr destination))
                      ;; Make sure to move point when inserting
                      (insert-before-markers (buffer-substring (car source) (cdr source)))))))))
    ("move" .
     ,(hatty-edit--make-instruction
        `((list 2)
          (apply ,(lambda (source destination)
                    ;; Use default target here?
                    (unless destination
                      (setq destination (cons (point) (point))))
                    (save-excursion
                      (goto-char (car destination))
                      (delete-region (car destination) (cdr destination))
                      ;; Make sure to move point when inserting
                      (insert-before-markers (buffer-substring (car source) (cdr source)))
                      (let ((deletion-region (hatty-edit--deletion-region source)))
                        (delete-region (car deletion-region) (cdr deletion-region)))))))))
    ("swap" .
     ,(hatty-edit--make-instruction
        `((list 2)
          (apply ,(lambda (first second)
                    (let ((first-string (buffer-substring (car first) (cdr first)))
                          (second-string (buffer-substring (car second) (cdr second))))
                      (delete-region (car first) (cdr first))
                      (delete-region (car second) (cdr second))
                      (save-excursion
                        (goto-char (car first))
                        (insert second-string)
                        (goto-char (car second))
                        (insert first-string))))))))
    ("pre" .
     ,(hatty-edit--make-multiple-cursors-action
       (lambda (region)
         (goto-char (car region)))))
    ("post" .
     ,(hatty-edit--make-multiple-cursors-action
       (lambda (region)
         (goto-char (cdr region)))))
    ("change" .
     ,(hatty-edit--make-multiple-cursors-action
       (lambda (region)
         (goto-char (car region))
         (kill-region (car region) (cdr region)))))
    ("comment" .
     ,(hatty-edit--make-instruction
        '((amalgamate-stack)
          (const ((funcall (lambda (region)
                             (comment-region (car region)
                                             (cdr region))))))
          (map))))
    ("uncomment" .
     ,(hatty-edit--make-parallel-action
       (lambda (region)
         (uncomment-region (car region) (cdr region)))))))

;;;; Default modifiers:

(defvar hatty-edit-modifiers
  `(("paint" . ,(hatty-edit--make-parallel-modifier
                 (lambda (region)
                   (hatty-edit--markify-region
                    (cons (save-excursion
                            (goto-char (car region))
                            (skip-chars-backward "^[:space:]\n")
                            (point))
                          (save-excursion
                            (goto-char (cdr region))
                            (skip-chars-forward "^[:space:]\n")
                            (point)))))))
    ("past" . ,(hatty-edit--make-instruction
                 `((list 2)
                   (apply ,(lambda (first-target second-target)
                             (hatty-edit--markify-region
                              (cons (min (car first-target)
                                         (car second-target))
                                    (max (cdr first-target)
                                         (cdr second-target)))))))))))

;;; hatty-edit.el ends soon
(provide 'hatty-edit)
;;; hatty-edit.el ends here
