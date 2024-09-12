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
;;;; Debugger (step-through)
;;;; goto-def

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
    (,stack-function)
    lisp-funcall
    unstack))

(defun hatty-edit--evaluate (instructions)
  (hatty-edit--environment-value-stack
   (hatty-edit--evaluate-environment
     (make-hatty-edit--environment
      :instruction-stack instructions))))

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

;; TODO: Do not halt on missing delimiters!
(hatty-edit--define-macro '->
  (lambda (environment)
    (let (parameters mapping body)
      ;; Read parameters until : (colon).
      (push (hatty-edit--pop-instruction environment) parameters)
      (while (not (eq (car parameters) ':))
        (push (hatty-edit--pop-instruction environment) parameters))

      ;; Pop delimiting : (colon), reverse (first parameter -> top of
      ;; stack).
      (pop parameters)
      (setq parameters (reverse parameters))

      ;; Read parameter values from stack.
      (dolist (parameter parameters)
        (push (cons parameter (hatty-edit--pop-value environment))
              mapping))

      ;; Read body until .. (period period).  We don't use single
      ;; period, as that clashes with the syntax of cons cells in
      ;; Elisp.
      (push (hatty-edit--pop-instruction environment) body)
      (while (not (eq (car body) '..))
        (push (hatty-edit--pop-instruction environment) body))

      ;; Pop delimiting .. (period period), reverse (first instruction
      ;; -> top of stack).
      (pop body)
      (setq body (reverse body))

      ;; Substitute top-level occurences of parameters, push new body
      ;; to instruction stack.
      (hatty-edit--push-instructions
       environment
       (mapcar (lambda (instruction)
                 (alist-get instruction mapping instruction))
               body)))))

(defun hatty-edit--define-compound-macro (macro-name forms)
  (declare (indent defun))
  (hatty-edit--define-macro macro-name
    (lambda (environment)
      (hatty-edit--push-instructions environment forms))))

(define-obsolete-function-alias
  'hatty-edit--define-composed-macro
  'hatty-edit--define-compound-macro
  "0.0.0")

(defun hatty-edit--evaluate-environment (environment)
  (declare (indent defun))
  (while (hatty-edit--environment-instruction-stack environment)
    (let ((instruction (hatty-edit--pop-instruction environment)))
      (pcase instruction
        ((pred symbolp) (funcall (hatty-edit--get-macro instruction) environment))
        (_ (hatty-edit--push-value environment instruction)))))
  environment)

(defun hatty-edit--define-rewrite-macro (name stack-before stack-after)
  "Create macro rewriting top of stack.

All elements in STACK-AFTER must occur in STACK-BEFORE."
  (declare (indent defun))
  ;; STACK-AFTER needs to be reversed to be pushed in the correct
  ;; order.
  (hatty-edit--define-compound-macro name
    `(-> ,@stack-before : ,@(reverse stack-after) ..)))

(dolist (definition '((drop (x) ())
                      (swap (x y) (y x))
                      (swapd (x y z) (x z y))
                      (dup (x) (x x))
                      (dupd (x y) (x y y))
                      (rollup (x y z) (y z x))
                      (rolldown (x y z) (z x y))))
  (apply 'hatty-edit--define-rewrite-macro
         definition))

(hatty-edit--define-macro 'nop #'identity)

(hatty-edit--define-compound-macro 'lisp-eval-n
  '(lisp-apply-n drop))

(hatty-edit--define-compound-macro 'lisp-eval
  '(0 lisp-eval-n))

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

;; Figure out the interdependencies of these three...
(hatty-edit--define-macro 'lisp-apply
  (lambda (environment)
    (let ((function (hatty-edit--pop-value environment)))
      (hatty-edit--push-value
       environment
       ;; Function symbols may be wrapped in a stack, so they are
       ;; treated like literals.
       (apply (if (functionp function) function (car function))
              (hatty-edit--pop-value environment))))))

(hatty-edit--define-composed-macro 'lisp-apply-n
  (hatty-edit--lift-stack-function
   (lambda (stack)
     (let* ((arity (pop stack))
            (function (pop stack))
            (arguments (take arity stack))
            (return (nthcdr arity stack)))
       ;; Function symbols may be wrapped in a stack, so they are
       ;; treated like literals.
       (push (apply (if (functionp function) function (car function))
                    arguments)
             return)
       return))))

(hatty-edit--define-macro 'lisp-funcall
  (lambda (environment)
    (let ((function (hatty-edit--pop-value environment)))
      (hatty-edit--push-value
       environment
       ;; Function symbols may be wrapped in a stack, so they are
       ;; treated like literals.
       (funcall (if (functionp function) function (car function))
                (hatty-edit--pop-value environment))))))

(hatty-edit--define-compound-macro 'flatten
  '((append) lisp-apply))

;; TODO: Consider making environments first-class citizens
(hatty-edit--define-composed-macro 'map
  `(,(lambda (function substack)
       (mapcar (lambda (value)
                 (hatty-edit--environment-value-stack
                  (hatty-edit--evaluate-environment
                    (make-hatty-edit--environment
                     ;; Wrap operation in list: If it is a single
                     ;; symbol, it will become a valid program.  If it
                     ;; is a list, evaluation of that list will push it
                     ;; onto the instruction stack, avoiding mutation
                     ;; of the original list.
                     :instruction-stack function
                     :value-stack (list value)))))
               substack))
    2 lisp-apply-n
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
  `(,(hatty-edit--markify-region
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
    (hatty-edit--markify-region
     (if (/= (point) (cdr region))
         (cons (car region) (point))
       (goto-char (car region))
       (skip-chars-backward "[:space:]\n")
       (cons (point) (cdr region))))))

(defun hatty-edit--lift-lisp-function (name)
  `((,name)
    ,(car (func-arity name))
    lisp-apply-n))

(defun hatty-edit--lift-lisp-effect (lisp-name)
  `(,@(hatty-edit--lift-lisp-function lisp-name) drop))

(dolist (entry '((cons . cons)
                 (car . car)
                 (cdr . cdr)))
  (hatty-edit--define-compound-macro (car entry)
    (hatty-edit--lift-lisp-function (cdr entry))))

(dolist (entry '((set-mark . set-mark)
                 (goto-char . goto-char)))
  (hatty-edit--define-compound-macro (car entry)
    (hatty-edit--lift-lisp-effect (cdr entry))))

(hatty-edit--define-composed-macro 'uncons
  `(dup car swap cdr swap))

(hatty-edit--define-composed-macro 'target-select
  '(uncons set-mark goto-char))

(hatty-edit--define-composed-macro 'target-deletion-region
  '((hatty-edit--deletion-region) lisp-funcall))

(hatty-edit--define-composed-macro 'target-delete
  '(uncons (delete-region) 2 lisp-eval-n))

(hatty-edit--define-composed-macro 'target-chuck
  '(target-deletion-region target-delete))

(hatty-edit--define-composed-macro 'target-string
  '(uncons (buffer-substring) 2 lisp-apply-n))

(hatty-edit--define-composed-macro 'insert
  '((insert) 1 lisp-eval-n))

(hatty-edit--define-composed-macro 'insert-at
  `(,(lambda (string position)
       (save-excursion
         (goto-char position)
         (insert string)))
    2 lisp-eval-n))

(hatty-edit--define-composed-macro 'target-insert
  '(swap car swap insert-at))

(hatty-edit--define-composed-macro 'target-bring
  '(target-string insert))

(hatty-edit--define-composed-macro 'target-overwrite
  '(swap dup target-delete swap target-insert))

(hatty-edit--define-compound-macro 'target-bring-overwrite
  '(target-string target-overwrite))

(hatty-edit--define-compound-macro 'target-move
  '(-> t : t target-bring t target-chuck ..))

(hatty-edit--define-compound-macro 'target-swap
  '(-> t1 t2 :
       t1 t2 target-string
       t2 t1 target-string
       target-overwrite
       target-overwrite ..))

(hatty-edit--define-compound-macro 'multiple-cursors-do
  '(-> f :
       amalgamate-stack
       f (hatty-edit--multiple-cursors-do)
       2 lisp-eval-n ..))

(hatty-edit--define-compound-macro 'do-all
  '(-> f : amalgamate-stack f map drop ..))

(defun hatty-edit--multiple-cursors-do (function values)
  "Parallelize side effects on point, mark and active region."
  (when values
    ;; Clear any previous multiple cursors
    (multiple-cursors-mode 0)
    (multiple-cursors-mode 1)

    (hatty-edit--evaluate-environment
      (make-hatty-edit--environment
       :instruction-stack function
       :value-stack (list (car values))))
    (dolist (value (cdr values))
      (mc/create-fake-cursor-at-point)
      (hatty-edit--evaluate-environment
        (make-hatty-edit--environment
         :instruction-stack function
         :value-stack (list value))))))

(defvar hatty-edit-actions
  `(("select" . ((target-select) multiple-cursors-do))
    ("chuck" . ((target-chuck) do-all))
    ("bring" . target-bring)
    ("move" . target-move)
    ("swap" . target-swap)
    ("pre" . ((car goto-char) multiple-cursors-do))
    ("post" . (cdr goto-char))
    ("change" . ((-> t : t target-delete t car goto-char ..)
                 multiple-cursors-do))
    ("comment" .
     (amalgamate-stack
      (uncons (comment-region) 2 lisp-eval-n)
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

(hatty-edit--define-compound-macro 'find-occurrences
  `(nop
    push ,(lambda (string)
            (save-excursion
              (let ((length (length string))
                    matches)
                (goto-char (point-min))
                (while (search-forward string nil t)
                  (push (hatty-edit--markify-region
                         (cons (- (point) length) (point)))
                        matches))
                matches)))
    lisp-funcall))

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
      lisp-apply))
    ("every instance" . (target-string find-occurrences unstack))))

;;; hatty-edit.el ends soon
(provide 'hatty-edit)
;;; hatty-edit.el ends here
