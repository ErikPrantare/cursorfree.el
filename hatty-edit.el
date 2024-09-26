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
  (declare (indent defun))
  (push value (he--environment-value-stack environment)))

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

(defun he--define-instruction (name instruction)
  (declare (indent defun))
  (put name 'he--instruction instruction)
  name)

(defun he--get-instruction (name)
  (let ((instruction
         (get name 'he--instruction)))
    (unless instruction
      ;; TODO: Custom error symbol?
      (signal 'error (list "No such hatty-edit instruction: " name)))
    instruction))

(defun he--define-compound-instruction (instruction-name forms)
  (declare (indent defun))
  (he--define-instruction instruction-name
    (lambda (environment)
      (he--push-instructions environment forms))))

(defmacro he--define-lisp-instruction (instruction-name args &rest body)
  "Define instruction as a lisp function.

ARGS must be a finite list of arguments to be popped from the
stack.  The return value will be pushed back onto the stack."
  (declare (indent defun))
  (let ((internal-name
         (intern (concat "hatty-edit-i--"
                         (symbol-name instruction-name)))))
    `(progn
       (defun ,internal-name ,args ,@body)
       (he--define-instruction
         ',instruction-name
         (lambda (environment)
           (he--push-value environment
             (apply #',internal-name
                    (reverse
                     (he--pop-values environment ,(length args))))))))))

(defun he--step (environment)
  (let ((instruction (he--pop-instruction environment)))
    (if (symbolp instruction)
        (funcall (he--get-instruction instruction) environment)
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

(he--define-instruction 'lisp-funcall-n
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

(he--define-instruction 'stack
  (lambda (environment)
    (he--push-value environment
     (he--environment-value-stack environment))))

(he--define-instruction 'drop-stack
  (lambda (environment)
    (setf (he--environment-value-stack environment) nil)))

;; NOTE: Use the wrapper he--lambda instead!
(he--define-instruction '->
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

(he--define-instruction 'eval
  (lambda (environment)
    (he--push-instructions
     environment
     (he--pop-value environment))))

(he--define-instruction '\\
  (lambda (environment)
    (he--push-value
     environment
     (he--pop-instruction environment))))

(he--define-instruction 'unstack
  (lambda (environment)
    (he--push-values
     environment
     (he--pop-value environment))))

;;; Level 1: Convenience words

(he--define-compound-instruction 'dip
  (he--lambda (v f) f eval v))

(he--define-compound-instruction 'amalgamate-stack
  '(stack (drop-stack) dip))

(he--define-compound-instruction 'replace-stack
  '((drop-stack) dip unstack))

(he--define-compound-instruction 'lisp-apply-stack
  '((amalgamate-stack) dip
    lisp-funcall
    unstack))

(defun he--define-rewrite-instruction (name stack-before stack-after)
  "Create instruction rewriting top of stack.

All elements in STACK-AFTER must occur in STACK-BEFORE."
  (declare (indent defun))
  ;; STACK-AFTER needs to be reversed to be pushed in the correct
  ;; order.
  (he--define-compound-instruction name
    `(-> ,@stack-before : ,@(reverse stack-after) ..)))

(dolist (definition '((drop (x) ())
                      (swap (x y) (y x))
                      (swapd (x y z) (x z y))
                      (dup (x) (x x))
                      (dupd (x y) (x y y))
                      (rollup (x y z) (y z x))
                      (rolldown (x y z) (z x y))))
  (apply 'he--define-rewrite-instruction
         definition))

(he--define-compound-instruction 'nop '())

(he--define-compound-instruction 'nil
  '(\\ nil))

(he--define-compound-instruction 't
  '(\\ t))

;; (P) (S) ... -> (E) ... Take program (P) and push environment (E)
;; having (P) as its program and (S) as its initial state.
(he--define-compound-instruction 'make-subenvironment
  `(,(lambda (stack program)
       (make-hatty-edit--environment
        :instruction-stack program
        :value-stack stack))
    2 lisp-funcall-n))

(he--define-compound-instruction 'save-excursion
  `((stack)
    dip
    make-subenvironment
    ,(lambda (subenvironment)
       (save-excursion
         (he--evaluate-environment subenvironment)))
    lisp-funcall
    replace-stack))

(he--define-compound-instruction 'lisp-apply
  `(,(lambda (arguments function) (apply function arguments))
    2 lisp-funcall-n))

(he--define-compound-instruction 'lisp-funcall
  '(1 lisp-funcall-n))

(he--define-compound-instruction 'lisp-eval-n
  '(lisp-funcall-n drop))

(he--define-compound-instruction 'lisp-eval
  '(0 lisp-eval-n))

(he--define-compound-instruction 'flatten
  '(\\ append lisp-apply))

;; TODO: Implement with more basic primitives
(he--define-compound-instruction 'map
  `(,(lambda (substack function)
       (mapcar (lambda (value)
                 (he--evaluate-environment
                   (make-hatty-edit--environment
                    :instruction-stack function
                    :value-stack (list value))))
               substack))
    2 lisp-funcall-n
    flatten))

(he--define-compound-instruction 'map-stack
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
                 (mark mark-marker 0)
                 (append append 2)))
  (he--define-compound-instruction (car entry)
    `(\\ ,(car entry) ,(caddr entry) lisp-funcall-n)))

(dolist (entry '((set-mark set-mark 1)
                 (goto-char goto-char 1)))
  (he--define-compound-instruction (car entry)
    `(\\ ,(car entry) ,(caddr entry) lisp-eval-n)))

(he--define-compound-instruction 'cleave
  (he--lambda (object f g)
    \\ object f eval
    \\ object g eval))

(he--define-compound-instruction 'spread
  (he--lambda (object1 object2 f)
    \\ object1 f eval
    \\ object2 f eval))

(he--define-compound-instruction 'uncons
  `((car) (cdr) cleave))

(he--define-compound-instruction 'keep
  `(dupd swap (eval) dip))

(he--define-lisp-instruction target-select (target)
  (set-mark (car target))
  (goto-char (cdr target)))

(he--define-lisp-instruction target-deletion-region (target)
  (he--deletion-region target))

(he--define-lisp-instruction target-delete (target)
  (delete-region (car target) (cdr target)))

(he--define-compound-instruction 'target-chuck
  '(target-deletion-region target-delete))

(he--define-compound-instruction 'target-string
  '(uncons (buffer-substring) 2 lisp-funcall-n))

(he--define-compound-instruction 'insert
  '(\\ insert 1 lisp-eval-n))

(he--define-compound-instruction 'insert-at
  `(((goto-char) dip insert) save-excursion))

(he--define-compound-instruction 'target-insert
  '((car) dip insert-at))

(he--define-compound-instruction 'target-bring
  '(target-string insert))

(he--define-compound-instruction 'target-overwrite
  '(((target-delete) keep) dip target-insert))

(he--define-compound-instruction 'target-bring-overwrite
  '(target-string target-overwrite))

(he--define-compound-instruction 'target-move
  '((target-bring) (target-chuck) cleave))

(he--define-compound-instruction 'target-swap
  (he--lambda (t1 t2)
       t1 t2 target-string
       t2 t1 target-string
       target-overwrite
       target-overwrite))

(he--define-compound-instruction 'multiple-cursors-do
  '((amalgamate-stack) dip
    \\ he--multiple-cursors-do 2 lisp-eval-n))

(he--define-compound-instruction 'do-all
  '(-> f : amalgamate-stack f map drop ..))

(he--define-compound-instruction 'thing-expand
  '(car \\ he--bounds-of-thing-at 2 lisp-funcall-n))

(defun he--multiple-cursors-map (function)
  (let ((return nil))
    (push (he--evaluate function) return)
    (mc/for-each-fake-cursor
     (push (he--evaluate function) return))
    (apply #'append return)))

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
     ((\\ uncomment-region 1 lisp-eval-n) do-all))
    ("narrow" .
     (uncons \\ narrow-to-region 2 lisp-eval-n))))

;;;; Default modifiers:

(he--define-compound-instruction 'skip-forward
  `(,(lambda (position string)
       (save-excursion
         (goto-char position)
         (skip-chars-forward string)
         (point-marker)))
    2 lisp-funcall-n))

(he--define-compound-instruction 'skip-backward
  `(,(lambda (position string)
       (save-excursion
         (goto-char position)
         (skip-chars-backward string)
         (point-marker)))
    2 lisp-funcall-n))

(he--define-compound-instruction 'find-occurrences
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

(he--define-compound-instruction 'paint-right
  `(uncons
    "^[:space:]\n"
    skip-forward
    cons))

(he--define-compound-instruction 'paint-left
  `(uncons
    ("^[:space:]\n" skip-backward)
    dip
    cons))

(he--define-compound-instruction 'trim-right
  `(uncons
    "[:space:]\n"
    skip-backward
    cons))

(he--define-compound-instruction 'trim-left
  `(uncons
    ("[:space:]\n" skip-forward)
    dip
    cons))

(defun he--every-thing (thing)
  (save-excursion
    (let ((things nil))
      (goto-char (point-min))
      (when (bounds-of-thing-at-point thing)
        (push (bounds-of-thing-at-point thing) things))
      (forward-thing thing)
      (while (< (point) (point-max))
        (push (bounds-of-thing-at-point thing) things)
        (forward-thing thing))
      things)))

(he--define-compound-instruction 'raise-lisp-function
  `(,(lambda (f)
       `(,f lisp-funcall))
    lisp-funcall))

(he--define-lisp-instruction inner-parenthesis (region delimiter)
  (let ((expanded
         (pcase delimiter
           ((or ?\( ?\))
            (evil-inner-paren 1 (car region) (cdr region)))
           ((or ?{ ?})
            (evil-inner-curly 1 (car region) (cdr region)))
           ((or ?\[ ?\])
            (evil-inner-bracket 1 (car region) (cdr region)))
           ((or ?< ?>)
            (evil-inner-angle 1 (car region) (cdr region))))))
    (cons (car expanded) (cadr expanded))))

(he--define-lisp-instruction curry (f)
  `(-> x : \\ x \\ ,f eval))

(he--define-lisp-instruction inner-parenthesis-any (region)
  (-max-by (-on #'> #'car)
           (--keep (condition-case nil
                       (he-i--inner-parenthesis region it)
                     (error nil))
                   '(?< ?{ ?\( ?\[))))

(he--define-lisp-instruction if (condition then else)
  (if condition then else))

(he--define-compound-instruction 'inner-parenthesis-dwim
  '(dup \\ characterp lisp-funcall
    (inner-parenthesis)
    (inner-parenthesis-any)
    if eval))

(defvar he-modifiers
  `(("leftpaint" .
     ((paint-left) map-stack))
    ("rightpaint" .
     ((paint-right) map-stack))
    ("paint" .
     ((paint-left paint-right) map-stack))
    ("trim" .
     ((trim-left trim-right) map-stack))
    ("past" .
     (-> t1 t2 :
         t1 cdr t2 cdr max
         t1 car t2 car min
         cons ..))
    ("selection" .
     ((mark point cons) \\ he--multiple-cursors-map lisp-funcall unstack))
    ("every instance" .
     (target-string find-occurrences unstack))
    ("every line" .
     (,(lambda () (he--every-thing 'line))
      0 lisp-funcall-n
      unstack))
    ("inside" . (inner-parenthesis-dwim))))

(provide 'hatty-edit)

;;; hatty-edit.el ends soon
;; Local Variables:
;; read-symbol-shorthands: (("he-" . "hatty-edit-")
;; End:
;;; hatty-edit.el ends here
