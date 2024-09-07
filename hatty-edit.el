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

;;; TODO
;;;; Commentary
;;;; Tests
;;;; Docstrings

(require 'hatty)
(require 'multiple-cursors)

(defun hatty-edit--bounds-of-thing-at (thing position)
  (save-excursion
    (goto-char position)
    (bounds-of-thing-at-point thing)))

(cl-defstruct hatty-edit--target
  content-region)

(cl-defun hatty-edit--make-target (&key content-region)
  (make-hatty-edit--target
   :content-region (cons (move-marker (make-marker) (car content-region))
                         (move-marker (make-marker) (cdr content-region)))))

(defun hatty-edit--make-target-from-hat (character &optional color shape)
  (hatty-edit--make-target
   :content-region (hatty-edit--bounds-of-thing-at
                    'symbol
                    (hatty-locate character color shape))))

(defun hatty-edit--make-contiguous-target (first-target second-target)
  "Make smallest target covering FIRST-TARGET and SECOND-TARGET."
  (cl-destructuring-bind (first-beginning . first-end) first-target
    (cl-destructuring-bind (second-beginning . second-end) second-target
      (hatty-edit--make-target
       :content-region (cons (min first-beginning second-beginning)
                             (max first-end second-end))))))

(cl-defstruct hatty-edit--modifier
  modifier-function
  list-modifier-function)

(defun hatty-edit--make-modifier (function &optional list-function)
  (unless list-function
    (setq list-function (lambda (target-list)
                          (cl-loop
                           for target in target-list
                           for modified = (funcall function target)
                           if (consp modified) append modified
                           else collect modified))))
  (make-hatty-edit--modifier
   :modifier-function function
   :list-modifier-function list-function))

(defun hatty-edit--make-basic-modifier (function &optional list-function)
  ;; Only apply modifier on content
  (make-hatty-edit--modifier
   :modifier-function
   (lambda (target)
     (hatty-edit--make-target
      :content-region (funcall function (hatty-edit--target-content-region target))))
   :list-modifier-function
   (if list-function
       (lambda (targets)
         (let ((new-region (funcall
                            list-function
                            (mapcar #'hatty-edit--target-content-region targets))))
           (if (proper-list-p new-region)
               (mapcar (lambda (region)
                         (hatty-edit--make-target
                          :content-region region))
                       new-region)
             (hatty-edit--make-target
                          :content-region new-region)))))))

(cl-defstruct hatty-edit--action
  action-function
  list-action-function)

(defun hatty-edit--make-action (function &optional list-function)
  (unless list-function
    (setq list-function (lambda (target-list)
                          (dolist (target target-list)
                            (funcall function target)))))
  (make-hatty-edit--action
   :action-function function
   :list-action-function list-function))

(defun hatty-edit--make-content-action (function &optional list-function)
  (let* ((target-function
           (lambda (target)
             (funcall function
                      (hatty-edit--target-content-region target))))
          (target-list-function
           (if list-function
               (lambda (target-list)
                 (funcall list-function (mapcar #'hatty-edit--target-content-region
                                                target-list)))
             (lambda (target-list)
               (dolist (target target-list)
                 (funcall target-function target))))))
    (make-hatty-edit--action
     :action-function target-function
     :list-action-function target-list-function)))

(defun hatty-edit--make-multiple-cursor-content-action (function)
  (let* ((list-function
           (lambda (regions)
             (when regions
               ;; Clear any previous multiple cursors
               (multiple-cursors-mode 0)
               (multiple-cursors-mode 1)

               (funcall function (car regions))
               (dolist (region (cdr regions))
                 (mc/create-fake-cursor-at-point)
                 (funcall function region))))))
    (hatty-edit--make-content-action function list-function)))

(defun hatty-edit--make-thing-modifier (thing)
  ;; Expands from car of region
  (hatty-edit--make-modifier
   (lambda (target)
     (hatty-edit--make-target
      :content-region
      (hatty-edit--bounds-of-thing-at thing
                                      (car (hatty-edit--target-content-region target)))))))

(defun hatty-edit--apply-modifier (modifier target)
  (cond
   ((listp target)
    (funcall (hatty-edit--modifier-list-modifier-function modifier) target))
   (t
    (funcall (hatty-edit--modifier-modifier-function modifier) target))))

(defun hatty-edit--apply-action (action target)
  (cond
   ((listp target)
    (funcall (hatty-edit--action-list-action-function action) target))
   (t
    (funcall (hatty-edit--action-action-function action) target))))

(defun hatty-edit--perform-hatty-action (target modifiers action)
  (dolist (modifier modifiers)
     (setq target (hatty-edit--apply-modifier modifier target)))
  (hatty-edit--apply-action action target))

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
  (if (region-active-p)
      (hatty-edit--map-all-cursors
       (lambda ()
         (hatty-edit--make-target
          :content-region (cons (region-beginning)
                                (region-end)))))
    (hatty-edit--map-all-cursors
     (lambda ()
       (hatty-edit--make-target
        :content-region (bounds-of-thing-at-point 'symbol))))))

;;;; Default actions:

(defvar hatty-edit-actions
  `(("select" .
     ,(hatty-edit--make-multiple-cursor-content-action
       (lambda (region)
         (set-mark (car region))
         (goto-char (cdr region)))))
    ("pre" .
     ,(hatty-edit--make-multiple-cursor-content-action
       (lambda (region)
         (goto-char (car region)))))
    ("post" .
     ,(hatty-edit--make-multiple-cursor-content-action
       (lambda (region)
         (goto-char (cdr region)))))
    ("change" .
     ,(hatty-edit--make-multiple-cursor-content-action
       (lambda (region)
         (goto-char (car region))
         (kill-region (car region) (cdr region)))))
    ("comment" .
     ,(hatty-edit--make-content-action
       (lambda (region)
         (comment-region (car region) (cdr region)))))
    ("uncomment" .
     ,(hatty-edit--make-content-action
       (lambda (region)
         (uncomment-region (car region) (cdr region)))))))

;;;; Default modifiers:

(defvar hatty-edit-modifiers
  `(("paint" . ,(hatty-edit--make-basic-modifier
                 (lambda (region)
                   (cons (save-excursion
                           (goto-char (car region))
                           (skip-chars-backward "^[:space:]")
                           (point))
                         (save-excursion
                           (goto-char (cdr region))
                           (skip-chars-forward "^[:space:]")
                           (point))))))
    ("fill" . ,(hatty-edit--make-basic-modifier
                 #'identity
                 (lambda (regions)
                   (cons (apply #'min (mapcar #'car regions))
                         (apply #'max (mapcar #'cdr regions))))))))

;;; hatty-edit.el ends soon
(provide 'hatty-edit)
;;; hatty-edit.el ends here
