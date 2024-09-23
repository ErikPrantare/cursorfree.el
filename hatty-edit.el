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
;;;; Commentary.
;;;; Docstrings.
;;;; TODO file.
;;;; Debugger.  Better visualizer for hatty-edit--step.
;;;; goto-def of words.
;;;; rename stack -> stackn, value-stack -> stack.

;;; Code:

(require 'hatty)
(require 'multiple-cursors)


;;;; Instruction interpreter:

(cl-defstruct he--environment
  (instruction-stack nil) (value-stack nil))

(defun he--make-environment (instructions)
  (make-hatty-edit--environment
   :instruction-stack instructions))

(defun he--push-instruction (environment instruction)
  (push instruction (he--environment-instruction-stack environment)))

(defun he--push-instructions (environment instructions)
  (dolist (instruction (reverse instructions))
    (he--push-instruction environment instruction)))

(defun he--pop-instruction (environment)
  (pop (he--environment-instruction-stack environment)))

(defun he--push-value (environment value)
  (push value (he--environment-value-stack environment)))

(defun he--push-values (environment values)
  (dolist (value (reverse values))
    (he--push-value environment value)))

(defun he--pop-value (environment)
  (cl-destructuring-bind (head . tail)
      (he--environment-value-stack environment)
    (setf (he--environment-value-stack environment) tail)
    head))

(defun he--pop-values (environment n)
  (let ((acc nil))
    (dotimes (i n (reverse acc))
      (push (he--pop-value environment) acc))))

(defun he--lift-stack-function (stack-function)
  `(amalgamate-stack
    (,stack-function)
    lisp-funcall
    unstack))

(defun he--define-macro (name macro)
  (declare (indent defun))
  (put name 'he--macro macro)
  name)

(defun he--get-macro (name)
  (let ((macro
         (get name 'he--macro)))
    (unless macro
      ;; TODO: Custom error symbol?
      (signal 'error (list "No such hatty-edit macro: " name)))
    macro))

(defun he--define-compound-macro (macro-name forms)
  (declare (indent defun))
  (he--define-macro macro-name
    (lambda (environment)
      (he--push-instructions environment forms))))

(define-obsolete-function-alias
  'he--define-composed-macro
  'he--define-compound-macro
  "0.0.0")

(defun he--step (environment)
  (let ((instruction (he--pop-instruction environment)))
    (if (symbolp instruction)
        (funcall (he--get-macro instruction) environment)
      (he--push-value environment instruction)))
  environment)

(defun he--evaluate-environment (environment)
  (declare (indent defun))
  (while (he--environment-instruction-stack environment)
    (he--step environment))
  (he--environment-value-stack
   environment))

(defun he--evaluate (instructions)
  (he--evaluate-environment
    (he--make-environment instructions)))

;; NOTE: Use the wrapper he--lambda instead!
(he--define-macro '->
  (lambda (environment)
    (let (parameters mapping body)
      ;; Read parameters until : (colon).
      (push (he--pop-instruction environment) parameters)
      (while (not (eq (car parameters) ':))
        (push (he--pop-instruction environment) parameters))

      ;; Pop delimiting : (colon), reverse (first parameter -> top of
      ;; stack).
      (pop parameters)
      (setq parameters (reverse parameters))

      ;; Read parameter values from stack.
      (dolist (parameter parameters)
        (push (cons parameter (he--pop-value environment))
              mapping))

      ;; Read body until .. (period period).  We don't use single
      ;; period, as that clashes with the syntax of cons cells in
      ;; Elisp.
      (push (he--pop-instruction environment) body)
      (while (not (eq (car body) '..))
        (push (he--pop-instruction environment) body))

      ;; Pop delimiting .. (period period), reverse (first instruction
      ;; -> top of stack).
      (pop body)
      (setq body (reverse body))

      ;; Substitute top-level occurences of parameters, push new body
      ;; to instruction stack.
      (he--push-instructions
       environment
       (mapcar (lambda (instruction)
                 (alist-get instruction mapping instruction))
               body)))))

(defmacro he--lambda (args &rest body)
  (declare (indent defun))
  `'(-> ,@args : ,@body ..))

(defun he--define-rewrite-macro (name stack-before stack-after)
  "Create macro rewriting top of stack.

All elements in STACK-AFTER must occur in STACK-BEFORE."
  (declare (indent defun))
  ;; STACK-AFTER needs to be reversed to be pushed in the correct
  ;; order.
  (he--define-compound-macro name
    `(-> ,@stack-before : ,@(reverse stack-after) ..)))

(dolist (definition '((drop (x) ())
                      (swap (x y) (y x))
                      (swapd (x y z) (x z y))
                      (dup (x) (x x))
                      (dupd (x y) (x y y))
                      (rollup (x y z) (y z x))
                      (rolldown (x y z) (z x y))))
  (apply 'he--define-rewrite-macro
         definition))

(he--define-compound-macro 'nop '())

(he--define-macro 'value-stack
  (lambda (environment)
    (he--push-value environment
     (he--environment-value-stack environment))))

(he--define-macro 'clear-stack
  (lambda (environment)
    (setf (he--environment-value-stack environment) nil)))

;; (P) (S) ... -> (E) ... Take program (P) and push environment (E)
;; having (P) as its program and (S) as its initial state.
(he--define-compound-macro 'make-subenvironment
  `(,(lambda (program stack)
       (make-hatty-edit--environment
        :instruction-stack program
        :value-stack stack))
    2 lisp-apply-n))

(he--define-macro 'quote
  (lambda (environment)
    (he--push-value
     environment
     (he--pop-instruction environment))))

(he--define-compound-macro 'nil
  '(quote nil))

(he--define-compound-macro 't
  '(quote t))

(he--define-macro 'drop-stack
  (lambda (environment)
    (setf (he--environment-value-stack environment) nil)))

(he--define-compound-macro 'replace-stack
  (he--lambda (s) drop-stack s unstack))

;; TODO: Rewrite to use dip
(he--define-compound-macro 'save-excursion
  `(-> program :
       value-stack
       program
       make-subenvironment
       ,(lambda (subenvironment)
          (save-excursion
            (he--evaluate-environment subenvironment)))
       lisp-funcall
       replace-stack ..))

(he--define-compound-macro 'lisp-eval-n
  '(lisp-apply-n drop))

(he--define-compound-macro 'lisp-eval
  '(0 lisp-eval-n))

(he--define-macro 'stack
  (lambda (environment)
    (he--push-value environment
                            (he--pop-values
                             environment
                             (he--pop-value environment)))))

(he--define-macro 'unstack
  (lambda (environment)
    (he--push-values
     environment
     (he--pop-value environment))))

(he--define-compound-macro 'amalgamate-stack
  `(value-stack ,@(he--lambda (s) drop-stack s)))

;; Figure out the interdependencies of these three...
(he--define-macro 'lisp-apply
  (lambda (environment)
    (let ((function (he--pop-value environment)))
      (he--push-value
       environment
       ;; Function symbols may be wrapped in a stack, so they are
       ;; treated like literals.
       (apply (if (functionp function) function (car function))
              (he--pop-value environment))))))

(he--define-composed-macro 'lisp-apply-n
  (he--lift-stack-function
   (lambda (stack)
     (let* ((arity (pop stack))
            (function (pop stack))
            (arguments (reverse (take arity stack)))
            (return (nthcdr arity stack)))
       ;; Function symbols may be wrapped in a stack, so they are
       ;; treated like literals.
       (push (apply (if (functionp function) function (car function))
                    arguments)
             return)
       return))))

(he--define-macro 'lisp-funcall
  (lambda (environment)
    (let ((function (he--pop-value environment)))
      (he--push-value
       environment
       ;; Function symbols may be wrapped in a stack, so they are
       ;; treated like literals.
       (funcall (if (functionp function) function (car function))
                (he--pop-value environment))))))

(he--define-compound-macro 'flatten
  '((append) lisp-apply))

;; TODO: Consider making environments first-class citizens
(he--define-compound-macro 'map
  `(,(lambda (substack function)
       (mapcar (lambda (value)
                 (he--evaluate-environment
                   (make-hatty-edit--environment
                    ;; Wrap operation in list: If it is a single
                    ;; symbol, it will become a valid program.  If it
                    ;; is a list, evaluation of that list will push it
                    ;; onto the instruction stack, avoiding mutation
                    ;; of the original list.
                    :instruction-stack function
                    :value-stack (list value))))
               substack))
    2 lisp-apply-n
    flatten))

(he--define-compound-macro 'map-stack
  (he--lambda (f) amalgamate-stack f map unstack))

;;;; Targets, modifiers, actions:

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

(cl-defun he--make-target (content-region)
  `(,(he--markify-region
      (cons (car content-region)
            (cdr content-region)))))

(defun he--make-target-from-hat (character &optional color shape)
  (he--make-target
   (he--bounds-of-thing-at
    'symbol
    (hatty-locate character color shape))))

(defun he--make-multiple-cursors-action (function)
  (let* ((stack-function
           (lambda (regions)
             (when regions
               ;; Clear any previous multiple cursors
               (multiple-cursors-mode 0)

               (funcall function (car regions))
               (when (cdr regions) (multiple-cursors-mode 1))
               (dolist (region (cdr regions))
                 (mc/create-fake-cursor-at-point)
                 (funcall function region)))
             nil)))
    (he--lift-stack-function stack-function)))

(defun he--make-thing-modifier (thing)
  ;; Expands from car of region
  (let ((function (lambda (target)
                    (he--bounds-of-thing-at thing (car target)))))
    `(,function lisp-funcall)))

(defun he--map-all-cursors (function)
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

(defun he--default-target ()
  (he--map-all-cursors
   (if (region-active-p)
       (lambda ()
         (he--make-target
          (cons (region-beginning)
                (region-end))))
     (lambda ()
       (he--make-target
        (bounds-of-thing-at-point 'symbol))))))

(defun he--make-parallel-action (action)
  (he--lift-stack-function
   (lambda (regions)
     (mapcar action regions)
     nil)))


;;;; Default actions:

(defun he--deletion-region (region)
  (save-excursion
    (goto-char (cdr region))
    (skip-chars-forward "[:space:]\n")
    (he--markify-region
     (if (/= (point) (cdr region))
         (cons (car region) (point))
       (goto-char (car region))
       (skip-chars-backward "[:space:]\n")
       (cons (point) (cdr region))))))

(defun he--lift-lisp-function (name arity)
  `((,name)
    ,arity
    lisp-apply-n))

(defun he--lift-lisp-effect (name arity)
  `(,@(he--lift-lisp-function name arity) drop))

(dolist (entry '((cons cons 2)
                 (car car 1)
                 (cdr cdr 1)
                 (min min 2)
                 (max max 2)
                 (point point-marker 0)))
  (he--define-compound-macro (car entry)
    (apply #'he--lift-lisp-function (cdr entry))))

(dolist (entry '((set-mark set-mark 1)
                 (goto-char goto-char 1)))
  (he--define-compound-macro (car entry)
    (apply #'he--lift-lisp-effect (cdr entry))))

(he--define-composed-macro 'uncons
  `(dup cdr swap car))

(he--define-composed-macro 'target-select
  '(uncons set-mark goto-char))

(he--define-composed-macro 'target-deletion-region
  '((he--deletion-region) lisp-funcall))

(he--define-composed-macro 'target-delete
  '(uncons (delete-region) 2 lisp-eval-n))

(he--define-composed-macro 'target-chuck
  '(target-deletion-region target-delete))

(he--define-composed-macro 'target-string
  '(uncons (buffer-substring) 2 lisp-apply-n))

(he--define-composed-macro 'insert
  '((insert) 1 lisp-eval-n))

(he--define-composed-macro 'insert-at
  `(,(lambda (string position)
       (save-excursion
         (goto-char position)
         (insert string)))
    2 lisp-eval-n))

(he--define-composed-macro 'target-insert
  '(swap car insert-at))

(he--define-composed-macro 'target-bring
  '(target-string insert))

(he--define-composed-macro 'target-overwrite
  '(swap dup target-delete swap target-insert))

(he--define-compound-macro 'target-bring-overwrite
  '(target-string target-overwrite))

(he--define-compound-macro 'target-move
  '(-> t : t target-bring t target-chuck ..))

(he--define-compound-macro 'target-swap
  '(-> t1 t2 :
       t1 t2 target-string
       t2 t1 target-string
       target-overwrite
       target-overwrite ..))

(he--define-compound-macro 'multiple-cursors-do
  '(-> f :
       amalgamate-stack
       f (he--multiple-cursors-do)
       2 lisp-eval-n ..))

(he--define-compound-macro 'do-all
  '(-> f : amalgamate-stack f map drop ..))

(he--define-compound-macro 'thing-expand
  '(car swap (he--bounds-of-thing-at) 2 lisp-apply-n))

(defun he--multiple-cursors-do (function values)
  "Parallelize side effects on point, mark and active region."
  (when values
    ;; Clear any previous multiple cursors
    (multiple-cursors-mode 0)
    (multiple-cursors-mode 1)

    (he--evaluate-environment
      (make-hatty-edit--environment
       :instruction-stack function
       :value-stack (list (car values))))
    (dolist (value (cdr values))
      (mc/create-fake-cursor-at-point)
      (he--evaluate-environment
        (make-hatty-edit--environment
         :instruction-stack function
         :value-stack (list value))))))

(defvar he-actions
  `(("select" . ((target-select) multiple-cursors-do))
    ("chuck" . ((target-chuck) do-all))
    ("bring" . (target-bring))
    ("move" . (target-move))
    ("swap" . (target-swap))
    ("pre" . ((car goto-char) multiple-cursors-do))
    ("post" . ((cdr goto-char) multiple-cursors-do))
    ("change" . ((-> t : t target-delete t car goto-char ..)
                 multiple-cursors-do))
    ("unwrap" . ((t ->
                    t (my/surrounding-inner) thing-expand target-string
                    t (my/surrounding-outer) thing-expand target-delete
                    t swap target-insert ..)
                 do-all))
    ("comment" .
     ((uncons (comment-region) 2 lisp-eval-n) do-all))
    ("uncomment" .
     ((uncomment-region) do-all))))

;;;; Default modifiers:

(he--define-compound-macro 'find-occurrences
  `(,(lambda (string)
       (save-excursion
         (let ((length (length string))
               matches)
           (goto-char (point-min))
           (while (search-forward string nil t)
             (push (he--markify-region
                    (cons (- (point) length) (point)))
                   matches))
           matches)))
    lisp-funcall))

(he--define-compound-macro 'paint-right
  `(uncons
    swap
    (goto-char
     ,(lambda ()
        (skip-chars-forward "^[:space:]\n"))
     lisp-eval
     point)
    save-excursion

    swap
    cons))

(he--define-compound-macro 'paint-left
  `(uncons
    (goto-char
     ,(lambda ()
        (skip-chars-backward "^[:space:]\n"))
     lisp-eval
     point)
    save-excursion

    cons))

(defvar he-modifiers
  `(("leftpaint" . ((paint-left) map-stack))
    ("rightpaint" . ((paint-right) map-stack))
    ("paint" . ((paint-left paint-right) map-stack))
    ("past" .
     (-> t1 t2 :
         t1 cdr t2 cdr max
         t1 car t2 car min
         cons ..))
    ("every instance" . (target-string find-occurrences unstack))))

(provide 'hatty-edit)

;;; hatty-edit.el ends soon
;; Local Variables:
;; read-symbol-shorthands: (("he-" . "hatty-edit-"))
;; End:
;;; hatty-edit.el ends here
