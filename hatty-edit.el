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
  (cl-destructuring-bind (head . tail)
      (hatty-edit--environment-value-stack environment)
    (setf (hatty-edit--environment-value-stack environment) tail)
    head))

(defun hatty-edit--pop-values (environment n)
  (let ((acc nil))
    (dotimes (i n)
      (push (hatty-edit--pop-value environment) acc))
    (reverse acc)))

(defun hatty-edit--lift-stack-function (stack-function)
  `(amalgamate-stack
    push ,stack-function
    lisp-funcall
    unstack))

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

(defun hatty-edit--define-composed-macro (macro-name forms)
  (declare (indent defun))
  (hatty-edit--define-macro macro-name
    (lambda (environment)
      (hatty-edit--push-instructions environment forms))))

(defun hatty-edit--evaluate-environment (environment)
  (declare (indent defun))
  (while (hatty-edit--environment-instruction-stack environment)
    (let ((instruction (hatty-edit--pop-instruction environment)))
      (pcase instruction
        ((pred listp) (hatty-edit--push-instructions environment instruction))
        (_ (funcall (hatty-edit--get-macro instruction) environment)))))
  environment)

(hatty-edit--define-macro 'push
  (lambda (environment)
    (hatty-edit--push-value environment
     (hatty-edit--pop-instruction environment))))

(hatty-edit--define-macro 'unpush
  (lambda (environment)
    (hatty-edit--push-instruction
     environment
     (hatty-edit--pop-value environment))))

(hatty-edit--define-macro 'drop
  (lambda (environment)
    (hatty-edit--pop-value environment)))

(hatty-edit--define-composed-macro 'swap
  (hatty-edit--lift-stack-function
   (lambda (stack)
     (cl-destructuring-bind (a b) (take 2 stack)
       `(,b ,a . ,(nthcdr 2 stack))))))

(hatty-edit--define-composed-macro 'swapd
  (hatty-edit--lift-stack-function
   (lambda (stack)
     (cl-destructuring-bind (a b c) (take 3 stack)
       `(,a ,c ,b . ,(nthcdr 3 stack))))))

(hatty-edit--define-composed-macro 'dup
  (hatty-edit--lift-stack-function
   (lambda (stack)
     (cl-destructuring-bind (a) (take 1 stack)
       `(,a ,a . ,(nthcdr 1 stack))))))

(hatty-edit--define-composed-macro 'dupd
  (hatty-edit--lift-stack-function
   (lambda (stack)
     (cl-destructuring-bind (a b) (take 2 stack)
       `(,a ,b ,b . ,(nthcdr 2 stack))))))

(hatty-edit--define-composed-macro 'rollup
  (hatty-edit--lift-stack-function
   (lambda (stack)
     (cl-destructuring-bind (a b c) (take 3 stack)
       `(,b ,c ,a . ,(nthcdr 3 stack))))))

(hatty-edit--define-composed-macro 'rolldown
  (hatty-edit--lift-stack-function
   (lambda (stack)
     (cl-destructuring-bind (a b c) (take 3 stack)
       `(,c ,a ,b . ,(nthcdr 3 stack))))))

(hatty-edit--define-composed-macro 'nop '())

(hatty-edit--define-composed-macro 'lisp-eval
  '(nop
    push nil
    swap
    lisp-apply
    drop))

(hatty-edit--define-macro 'stack
  (lambda (environment)
    (hatty-edit--push-value environment
                            (hatty-edit--pop-values
                             environment
                             (hatty-edit--pop-value environment)))))

;; Deprecated
(hatty-edit--define-composed-macro 'list '(stack))

(hatty-edit--define-macro 'unstack
  (lambda (environment)
    (hatty-edit--push-values
     environment
     (hatty-edit--pop-value environment))))

(hatty-edit--define-macro 'amalgamate-stack
  (lambda (environment)
    (setf (hatty-edit--environment-value-stack environment)
          (list (hatty-edit--environment-value-stack environment)))))

(hatty-edit--define-macro 'lisp-apply
  (lambda (environment)
    (hatty-edit--push-value
     environment
     (apply (hatty-edit--pop-value environment)
            (hatty-edit--pop-value environment)))))

(hatty-edit--define-composed-macro 'lisp-apply-n
  (hatty-edit--lift-stack-function
   (lambda (stack)
     (let* ((arity (pop stack))
            (function (pop stack))
            (arguments (take arity stack))
            (return (nthcdr arity stack)))
       (push (apply function arguments) return)
       return))))

(hatty-edit--define-macro 'lisp-funcall
  (lambda (environment)
    (hatty-edit--push-value
     environment
     (funcall (hatty-edit--pop-value environment)
              (hatty-edit--pop-value environment)))))

(hatty-edit--define-composed-macro 'flatten
  '(push append lisp-apply))

(hatty-edit--define-composed-macro 'map
  '(nop
    push (lambda (function substack)
           (mapcar (lambda (value)
                     (hatty-edit--environment-value-stack
                      (hatty-edit--evaluate-environment
                        (make-hatty-edit--environment
                         ;; Wrap operation in list: If it is a single symbol, it
                         ;; will become a valid program.  If it is a list,
                         ;; evaluation of that list will push it onto the
                         ;; instruction stack, avoiding mutation of the original
                         ;; list.
                         :instruction-stack (list function)
                         :value-stack (list value)))))
                   substack))
    push 2 lisp-apply-n
    flatten))

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
    `(push ,function lisp-funcall)))

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

(hatty-edit--define-composed-macro 'cons
  '(push cons push 2 lisp-apply-n))

(hatty-edit--define-composed-macro 'car
  '(push car lisp-funcall))

(hatty-edit--define-composed-macro 'cdr
  '(push cdr lisp-funcall))

(hatty-edit--define-composed-macro 'uncons
  `(dup car swap cdr swap))

(hatty-edit--define-composed-macro 'set-mark
  '(push set-mark lisp-funcall drop))

(hatty-edit--define-composed-macro 'goto-char
  '(push goto-char lisp-funcall drop))

(hatty-edit--define-composed-macro 'target-select
  '(uncons set-mark goto-char))

(hatty-edit--define-composed-macro 'target-deletion-region
  '(push hatty-edit--deletion-region lisp-funcall))

(hatty-edit--define-composed-macro 'target-delete
  '(uncons push delete-region push 2 lisp-apply-n drop))

(hatty-edit--define-composed-macro 'target-chuck
  '(nop
    target-deletion-region target-delete))

(hatty-edit--define-composed-macro 'target-string
  '(uncons push buffer-substring push 2 lisp-apply-n))

(hatty-edit--define-composed-macro 'insert
  '(push insert lisp-funcall drop))

(hatty-edit--define-composed-macro 'insert-at
  `(nop
    push ,(lambda (string position)
            (save-excursion
              (goto-char position)
              (insert string)))
    push 2 lisp-apply-n drop))

(hatty-edit--define-composed-macro 'target-insert
  '(swap car swap insert-at))

(hatty-edit--define-composed-macro 'target-bring
  '(target-string insert))

(hatty-edit--define-composed-macro 'target-overwrite
  '(swap dup target-delete swap target-insert))

(hatty-edit--define-composed-macro 'target-bring-overwrite
  '(target-string target-overwrite))

(hatty-edit--define-composed-macro 'target-move
  '(dup target-string insert target-chuck))

(hatty-edit--define-composed-macro 'target-swap
  '(                                ; t1 t2 (top of stack to the left)
    dup target-string swap          ; t1 s1 t2
    rollup dupd                     ; s1 t2 t2 t1
    swap target-string              ; s2 s1 t2 t2
    rollup                          ; s1 t2 s2 t1
    target-overwrite target-overwrite))

(defvar hatty-edit-actions
  `(("select" . target-select)
    ("chuck" . target-chuck)
    ("bring" . target-bring)
    ("move" . target-move)
    ("swap" . target-swap)
    ("pre" . (car goto-char))
    ("post" . (cdr goto-char))
    ("change" . (dup target-delete car goto-car))
    ("comment" .
     (amalgamate-stack
      push (nop
            push ,(lambda (region)
                    (comment-region (car region)
                                    (cdr region)))
            lisp-funcall drop)
      map))
    ("uncomment" .
     (amalgamate-stack
      push (nop
            push ,(lambda (region)
                    (uncomment-region (car region)
                                      (cdr region)))
            lisp-funcall drop)
      map))))

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
    ("past" .
     (nop
      push 2 list
      push
      ,(lambda (first-target second-target)
         (hatty-edit--markify-region
          (cons (min (car first-target)
                     (car second-target))
                (max (cdr first-target)
                     (cdr second-target)))))
      apply))))

;;; hatty-edit.el ends soon
(provide 'hatty-edit)
;;; hatty-edit.el ends here
