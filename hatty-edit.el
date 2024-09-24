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

(defun he--debug (instructions)
  (let* ((debug-buffer (generate-new-buffer "*hatty-edit debug*"))
         (state (he--make-environment instructions))
         (state-history (list))
         (format-state (lambda (state)
                         (format "Data:\n%s\n\nInstructions:\n%s"
                                 (he--environment-value-stack state)
                                 (he--environment-instruction-stack state)))))
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

;;; Level 0: Core words

;; The core words are what all other words build upon.

(he--define-macro 'lisp-funcall-n
  (lambda (environment)
    (let* ((arity (he--pop-value environment))
           (function (he--pop-value environment))
           (arguments (reverse (he--pop-values environment arity))))
      ;; Function symbols may be wrapped in a stack, so they are
      ;; treated like literals.
      ;; TODO: Use \\ to escape them instead?
      (he--push-value
       environment
       (apply (if (functionp function) function (car function))
              arguments)))))

(he--define-macro 'stack
  (lambda (environment)
    (he--push-value environment
     (he--environment-value-stack environment))))

(he--define-macro 'drop-stack
  (lambda (environment)
    (setf (he--environment-value-stack environment) nil)))

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
  `'(-> ,@(reverse args) : ,@body ..))

(he--define-macro 'eval
  (lambda (environment)
    (he--push-instructions
     environment
     (he--pop-value environment))))

(he--define-macro '\\
  (lambda (environment)
    (he--push-value
     environment
     (he--pop-instruction environment))))

(he--define-macro 'unstack
  (lambda (environment)
    (he--push-values
     environment
     (he--pop-value environment))))

;;; Level 1: Convenience words

(he--define-compound-macro 'dip
  (he--lambda (v f) f eval v))

(he--define-compound-macro 'amalgamate-stack
  '(stack (drop-stack) dip))

(he--define-compound-macro 'replace-stack
  '((drop-stack) dip unstack))

(he--define-compound-macro 'lisp-apply-stack
  '((amalgamate-stack) dip
    lisp-funcall
    unstack))

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

(he--define-compound-macro 'nil
  '(\\ nil))

(he--define-compound-macro 't
  '(\\ t))

;; (P) (S) ... -> (E) ... Take program (P) and push environment (E)
;; having (P) as its program and (S) as its initial state.
(he--define-compound-macro 'make-subenvironment
  `(,(lambda (stack program)
       (make-hatty-edit--environment
        :instruction-stack program
        :value-stack stack))
    2 lisp-funcall-n))

(he--define-compound-macro 'save-excursion
  `((stack)
    dip
    make-subenvironment
    ,(lambda (subenvironment)
       (save-excursion
         (he--evaluate-environment subenvironment)))
    lisp-funcall
    replace-stack))

(he--define-compound-macro 'lisp-apply
  `(,(lambda (arguments function) (apply function arguments))
    2 lisp-funcall-n))

(he--define-compound-macro 'lisp-funcall
  '(1 lisp-funcall-n))

(he--define-compound-macro 'lisp-eval-n
  '(lisp-funcall-n drop))

(he--define-compound-macro 'lisp-eval
  '(0 lisp-eval-n))

(he--define-compound-macro 'flatten
  '(\\ append lisp-apply))

;; TODO: Implement with more basic primitives
(he--define-compound-macro 'map
  `(,(lambda (substack function)
       (mapcar (lambda (value)
                 (he--evaluate-environment
                   (make-hatty-edit--environment
                    :instruction-stack function
                    :value-stack (list value))))
               substack))
    2 lisp-funcall-n
    flatten))

(he--define-compound-macro 'map-stack
  '((amalgamate-stack) dip map unstack))

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
  `(,(lambda (regions)
     (mapcar action regions)
     nil)
    lisp-apply-stack))


;;;; Default actions:

(defun he--deletion-region (region)
  (he--markify-region
   (save-excursion
     (goto-char (cdr region))
     (if (/= 0 (skip-chars-forward "[:space:]\n"))
         (cons (car region) (point))
       (goto-char (car region))
       (skip-chars-backward "[:space:]\n")
       (cons (point) (cdr region))))))

(dolist (entry '((cons cons 2)
                 (car car 1)
                 (cdr cdr 1)
                 (min min 2)
                 (max max 2)
                 (* * 2)
                 (+ + 2)
                 (- - 2)
                 (/ / 2)
                 (point point-marker 0)
                 (append append 2)))
  (he--define-compound-macro (car entry)
    `(\\ ,(car entry) ,(caddr entry) lisp-funcall-n)))

(dolist (entry '((set-mark set-mark 1)
                 (goto-char goto-char 1)))
  (he--define-compound-macro (car entry)
    `(\\ ,(car entry) ,(caddr entry) lisp-eval-n)))

(he--define-compound-macro 'cleave
  (he--lambda (object f g)
    \\ object f eval
    \\ object g eval))

(he--define-compound-macro 'spread
  (he--lambda (object1 object2 f)
    \\ object1 f eval
    \\ object2 f eval))

(he--define-compound-macro 'uncons
  `((car) (cdr) cleave))

(he--define-compound-macro 'keep
  `(dupd swap (eval) dip))

(he--define-compound-macro 'target-select
  '(uncons goto-char set-mark))

(he--define-compound-macro 'target-deletion-region
  '((he--deletion-region) lisp-funcall))

(he--define-compound-macro 'target-delete
  '(uncons (delete-region) 2 lisp-eval-n))

(he--define-compound-macro 'target-chuck
  '(target-deletion-region target-delete))

(he--define-compound-macro 'target-string
  '(uncons (buffer-substring) 2 lisp-funcall-n))

(he--define-compound-macro 'insert
  '(\\ insert 1 lisp-eval-n))

(he--define-compound-macro 'insert-at
  `(((goto-char) dip insert) save-excursion))

(he--define-compound-macro 'target-insert
  '((car) dip insert-at))

(he--define-compound-macro 'target-bring
  '(target-string insert))

(he--define-compound-macro 'target-overwrite
  '(((target-delete) keep) dip target-insert))

(he--define-compound-macro 'target-bring-overwrite
  '(target-string target-overwrite))

(he--define-compound-macro 'target-move
  '((target-bring) (target-chuck) cleave))

(he--define-compound-macro 'target-swap
  (he--lambda (t1 t2)
       t1 t2 target-string
       t2 t1 target-string
       target-overwrite
       target-overwrite))

(he--define-compound-macro 'multiple-cursors-do
  '((amalgamate-stack) dip
    \\ he--multiple-cursors-do 2 lisp-eval-n))

(he--define-compound-macro 'do-all
  '(-> f : amalgamate-stack f map drop ..))

(he--define-compound-macro 'thing-expand
  '(car \\ he--bounds-of-thing-at 2 lisp-funcall-n))

(defun he--multiple-cursors-do (values function)
  "Parallelize side effects on point, mark and active region."
  (when values
    ;; Clear any previous multiple cursors
    (multiple-cursors-mode 0)

    (he--evaluate-environment
      (make-hatty-edit--environment
       :instruction-stack function
       :value-stack (list (car values))))

    (when (cdr values)
      (multiple-cursors-mode 1)
      (dolist (value (cdr values))
        (mc/create-fake-cursor-at-point)
        (he--evaluate-environment
          (make-hatty-edit--environment
           :instruction-stack function
           :value-stack (list value)))))))

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
     ((uncons \\ comment-region 2 lisp-eval-n) do-all))
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
