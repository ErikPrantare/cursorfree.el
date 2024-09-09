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
;;;; Tests
;;;; Docstrings

;;; Code:

(require 'hatty)
(require 'multiple-cursors)


;;;; Instruction interpreter:

(cl-defstruct hatty-edit--environment
  (evaluation-queue nil) (value-stack nil))

(defvar hatty-edit--environment (make-hatty-edit--environment))

(defun hatty-edit--push-calculation (environment calculation)
  (make-hatty-edit--environment
   :evaluation-queue (append (hatty-edit--environment-evaluation-queue environment)
                             (list calculation))
   :value-stack (hatty-edit--environment-value-stack environment)))

(defun hatty-edit--remove-calculation (environment)
  (make-hatty-edit--environment
   :evaluation-queue (cdr (hatty-edit--environment-evaluation-queue environment))
   :value-stack (hatty-edit--environment-value-stack environment)))

(defun hatty-edit--push-value (environment value)
  (make-hatty-edit--environment
   :evaluation-queue (hatty-edit--environment-evaluation-queue environment)
   :value-stack (append (hatty-edit--environment-value-stack environment)
                        (list value))))

(defun hatty-edit--peek-values (environment n)
  (take n (hatty-edit--environment-value-stack environment)))

(defun hatty-edit--remove-values (environment n)
  (make-hatty-edit--environment
   :evaluation-queue (hatty-edit--environment-evaluation-queue environment)
   :value-stack (let ((value-stack (hatty-edit--environment-value-stack environment)))
                  (dotimes (i n) (pop value-stack)))))

(defun hatty-edit--pop-values (environment n)
  (cons (hatty-edit--peek-values environment n)
        (hatty-edit--remove-values environment n)))

(defun hatty-edit--evaluate-environment (environment)
  (let ((previous nil))
    (while (and (hatty-edit--environment-evaluation-queue environment)
                (not (eq environment previous)))
      (setq previous environment)
      (setq environment
            (funcall (car (hatty-edit--environment-evaluation-queue environment))
                     environment))))
  environment)

(defun hatty-edit--evaluate-environment-debug (environment)
  (let ((previous nil))
    (while (and (hatty-edit--environment-evaluation-queue environment)
                (not (eq environment previous)))
      (insert (format "\n%s\n" (hatty-edit--environment-value-stack environment)))
      (setq previous environment)
      (setq environment
            (funcall (car (hatty-edit--environment-evaluation-queue environment))
                     environment))))
  (insert (format "\n%s" environment))
  environment)

(defun hatty-edit--push-constant (value)
  (lambda (environment)
    (hatty-edit--remove-calculation
     (hatty-edit--push-value environment value))))

(defun hatty-edit--lift-stack-function (stack-function)
  (lambda (environment)
    (make-hatty-edit--environment
     :evaluation-queue (cdr (hatty-edit--environment-evaluation-queue environment))
     :value-stack (funcall stack-function
                           (hatty-edit--environment-value-stack environment)))))

(defun hatty-edit--evaluate (calculations)
  (hatty-edit--evaluate-environment
   (make-hatty-edit--environment
    :evaluation-queue calculations)))

(defun hatty-edit--evaluate-debug (calculations)
  (hatty-edit--evaluate-environment-debug
   (make-hatty-edit--environment
    :evaluation-queue calculations)))

;;;; Targets, modifiers, actions:

;; Computation model:

;; All values are targets.  Modifiers and actions are evaluators.

(defun hatty-edit--bounds-of-thing-at (thing position)
  (save-excursion
    (goto-char position)
    (bounds-of-thing-at-point thing)))

(cl-defun hatty-edit--make-target (content-region)
  (hatty-edit--push-constant
   (cons (move-marker (make-marker) (car content-region))
         (move-marker (make-marker) (cdr content-region)))))

(defun hatty-edit--make-target-from-hat (character &optional color shape)
  (hatty-edit--make-target
   (hatty-edit--bounds-of-thing-at
    'symbol
    (hatty-locate character color shape))))

(defun hatty-edit--make-contiguous-target (first-target second-target)
  "Make smallest target covering FIRST-TARGET and SECOND-TARGET."
  (hatty-edit--make-target
   (cons (min (car first-beginning) (car second-beginning))
         (max (cdr first-end) (cdr second-end)))))

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
                 (funcall function region))))))
    (hatty-edit--lift-stack-function stack-function)))

(defun hatty-edit--make-thing-modifier (thing)
  ;; Expands from car of region
  (lambda (environment)
    (let* ((popped (hatty-edit--pop-values environment 1))
           (popped-environment (car popped))
           (popped-value (cdr popped)))
      (hatty-edit--remove-calculation
       (hatty-edit--push-value
        (hatty-edit--bounds-of-thing-at thing
                                        (car target)))))))

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

(defvar hatty-edit-actions
  `(("select" .
     ,(hatty-edit--make-multiple-cursors-action
       (lambda (region)
         (set-mark (car region))
         (goto-char (cdr region)))))
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
     ,(hatty-edit--make-parallel-action
       (lambda (region)
         (comment-region (car region) (cdr region)))))
    ("uncomment" .
     ,(hatty-edit--make-parallel-action
       (lambda (region)
         (uncomment-region (car region) (cdr region)))))))

;;;; Default modifiers:

(defvar hatty-edit-modifiers
  `(("paint" . ,(hatty-edit--make-parallel-modifier
                 (lambda (region)
                   (cons (save-excursion
                           (goto-char (car region))
                           (skip-chars-backward "^[:space:]\n")
                           (point))
                         (save-excursion
                           (goto-char (cdr region))
                           (skip-chars-forward "^[:space:]\n")
                           (point))))))))

;;; hatty-edit.el ends soon
(provide 'hatty-edit)
;;; hatty-edit.el ends here
