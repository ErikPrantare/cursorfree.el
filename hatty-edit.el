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

(defun he--define-compound-instruction (instruction-name forms)
  (declare (indent defun))
  (he--define-instruction instruction-name
    (lambda (environment)
      (he--push-instructions environment forms))))

(defun he--step (environment)
  (let ((instruction (he--pop-instruction environment)))
    (if (symbolp instruction)
        (funcall (he--get-instruction instruction) environment)
      (he--push-value-pure environment instruction))))

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

;;; Level 0: Core instructions

;; The core instructions are what all other words build upon.

(defun he--lisp-funcall (function arity environment)
  (let ((arguments (reverse (he--pop-values environment arity))))
    (he--push-value environment
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
            (he--lisp-funcall internal-function
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

(defun he--lisp-funcall-n-impl (environment)
  (let ((arity (he--pop-value environment))
        (function (he--pop-value environment)))
    (he--lisp-funcall function arity environment)))

(he--define-instruction 'lisp-funcall-n #'he--lisp-funcall-n-impl)

(he--define-instruction 'stack
  (lambda (environment)
    (he--push-value environment
     (he--environment-value-stack environment))))

(he--define-instruction 'instructions
  (lambda (environment)
    (he--push-value environment
     (he--environment-instruction-stack environment))))

(he--define-instruction 'replace-stack
  (lambda (environment)
    (setf (he--environment-value-stack environment)
          (he--pop-value environment))))

(he--define-instruction 'replace-instructions
  (lambda (environment)
    (setf (he--environment-instruction-stack environment)
          (he--pop-value environment))))

;; NOTE: Use the wrapper he--lambda if possible!
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

      ;; Substitute occurences of parameters, push new body
      ;; to instruction stack.
      (he--push-instructions
       environment
       (--tree-map (alist-get it mapping it) body)))))

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
    (he--push-value environment
      (he--pop-instruction environment))))

(he--define-instruction 'unstack
  (lambda (environment)
    (he--push-values
     environment
     (he--pop-value environment))))

;;; Level 1: Allow defining instructions as if they were lisp functions

(he--define-instruction 'lisp-eval-n
  (lambda (environment)
    (he--lisp-funcall-n-impl environment)
    (he--pop-value environment)))

(defmacro he--define-lisp-instruction-0 (instruction-name args &rest body)
  (declare (indent defun))
  (let ((internal-function
         (he--defun-internal instruction-name args body)))
    `(he--define-compound-instruction ',instruction-name
       '(\\ ,internal-function ,(length args) lisp-eval-n))))

(defmacro he--define-lisp-instruction-n (instruction-name args &rest body)
  (declare (indent defun))
  (let ((internal-function
         (he--defun-internal instruction-name args body)))
    `(he--define-compound-instruction ',instruction-name
       '(\\ ,internal-function ,(length args) lisp-funcall-n unstack))))

(he--define-lisp-instruction-1 lisp-apply (xs f)
  (apply f xs))

;;; Level 2: Convenience

(he--define-lisp-instruction-0 drop (x))

(he--define-lisp-instruction-1 nil ()
  nil)

(he--define-lisp-instruction-1 t ()
  t)

(he--define-compound-instruction 'dip
  (he--lambda (v f) f eval v))

(he--define-compound-instruction 'amalgamate-stack
  '(stack (drop-stack) dip))

(he--define-compound-instruction 'drop-stack
  '(nil replace-stack))

(he--define-lisp-instruction-n swap (x y)
  (list x y))

(he--define-lisp-instruction-n dup (x)
  (list x x))

(he--define-lisp-instruction-1 make-subenvironment (values instructions)
  (make-hatty-edit--environment
   :instruction-stack instructions
   :value-stack values))

(he--define-lisp-instruction-1 evaluate-subenvironment (subenvironment)
  (he--evaluate-environment subenvironment))

;; TODO: "evaluate-inside-lisp" function?


(he--define-instruction 'save-excursion
  (lambda (environment)
    (setf (he--environment-value-stack environment)
          (save-excursion
            (he--evaluate-environment
             (he--make-environment
              (he--pop-value environment)
              (he--environment-value-stack environment)))))))

(he--define-instruction 'save-mark-and-excursion
  (lambda (environment)
    (setf (he--environment-value-stack environment)
          (save-mark-and-excursion
            (he--evaluate-environment
             (he--make-environment
              (he--pop-value environment)
              (he--environment-value-stack environment)))))))

(he--define-lisp-instruction-1 lisp-funcall (x f)
  (funcall f x))

(he--define-lisp-instruction-0 lisp-eval (f)
  (funcall f))

(he--define-lisp-instruction-0 flatten (xs)
  (apply #'append xs))

(he--define-lisp-instruction-1 map (xs f)
  (--mapcat (he--evaluate-environment
              (make-hatty-edit--environment
               :instruction-stack f
               :value-stack (list it)))
            xs))

(he--define-compound-instruction 'map-stack
  '((amalgamate-stack) dip map unstack))

(dolist (entry '((cons cons 2)
                 (car car 1)
                 (cdr cdr 1)
                 (* * 2)
                 (+ + 2)
                 (- - 2)
                 (/ / 2)
                 (point point-marker 0)
                 (mark mark-marker 0)
                 (append append 2)))
  (he--define-compound-instruction (car entry)
    `(\\ ,(cadr entry) ,(caddr entry) lisp-funcall-n)))

;; Custom definitions to make sure markers are returned if passed as
;; arguments.
(he--define-lisp-instruction-1 min (a b)
  (if (< a b) a b))
(he--define-lisp-instruction-1 max (a b)
  (if (> a b) a b))

(dolist (entry '((set-mark set-mark 1)
                 (goto-char goto-char 1)))
  (he--define-compound-instruction (car entry)
    `(\\ ,(cadr entry) ,(caddr entry) lisp-eval-n)))


(he--define-lisp-instruction-n uncons (cell)
  (list (cdr cell) (car cell)))

(he--define-lisp-instruction-1 curry (f)
  `(-> x : (\\ x \\ ,f eval) ..))

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
   (hatty-locate-token-region character color shape)))

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

(he--define-lisp-instruction-0 target-select (target)
  "Set active region to TARGET."
  (set-mark (car target))
  (goto-char (cdr target)))

(he--define-lisp-instruction-1 target-deletion-region (target)
  (he--deletion-region target))

(he--define-lisp-instruction-1 target-string (target)
  (buffer-substring (car target) (cdr target)))

(he--define-lisp-instruction-0 target-delete (target)
  (delete-region (car target) (cdr target)))

(he--define-lisp-instruction-0 target-indent (target)
  "Indent TARGET."
  (indent-region (car target) (cdr target)))

(he--define-lisp-instruction-1 target-chuck (target)
  "Delete TARGET and indent the resulting text."
  (he-i--target-delete (he-i--target-deletion-region target))
  (he-i--target-indent
   (he--bounds-of-thing-at 'line (car target))))

(he--define-lisp-instruction-0 insert (string)
  (insert string))

(he--define-lisp-instruction-0 insert-at (position string)
  (save-excursion
    (goto-char position)
    (insert string)))

(he--define-lisp-instruction-0 target-insert (target string)
  (he-i--insert-at (car target) string))

(he--define-lisp-instruction-0 target-bring (target)
  (insert (he-i--target-string target)))

(he--define-lisp-instruction-0 target-overwrite (target string)
  (he-i--target-delete target)
  (he-i--target-insert target string))

(he--define-lisp-instruction-0 target-bring-overwrite (target-to target-from)
  (he-i--target-overwrite
   target-to
   (he-i--target-string target-from)))

(he--define-lisp-instruction-0 target-move (target)
  (he-i--target-bring target)
  (he-i--target-chuck target))

(he--define-lisp-instruction-0 target-swap (target1 target2)
  (let ((string1 (he-i--target-string target1))
        (string2 (he-i--target-string target2)))
    (he-i--target-overwrite target1 string2)
    (he-i--target-overwrite target2 string1)))

(he--define-lisp-instruction-0 target-change (target)
  (he-i--target-delete target)
  (goto-char (car target)))

(he--define-lisp-instruction-1 thing-expand (thing target)
  (he--bounds-of-thing-at thing (car target)))

(he--define-lisp-instruction-0 target-clone (target)
  (he-i--insert-at (cdr target) (he-i--target-string target)))

(he--define-lisp-instruction-0 target-copy (target)
  (copy-region-as-kill (car target) (cdr target)))

(defun he--multiple-cursors-map (function)
  (let ((return nil))
    (push (he--evaluate function) return)
    (mc/for-each-fake-cursor
     (save-mark-and-excursion
       (goto-char (overlay-get cursor 'point))
       (set-mark (overlay-get cursor 'mark))
       (push (he--evaluate function) return)))
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

(he--define-compound-instruction 'multiple-cursors-do
  '((amalgamate-stack) dip
    \\ he--multiple-cursors-do 2 lisp-eval-n))

(he--define-compound-instruction 'do-all
  (he--lambda (f) amalgamate-stack f map drop))

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
(he--define-compound-instruction 'crush
  '(dup
    (target-string) map \\ concat lisp-apply
    (targets-join) dip
    target-overwrite))

(defvar he-actions
  `(("select" . ((target-select) multiple-cursors-do))
    ("copy" . (target-copy))
    ("chuck" . ((target-chuck) do-all))
    ("bring" . ((target-bring) curry eval \\ he--multiple-cursors-map lisp-funcall))
    ("move" . (target-move))
    ("swap" . (target-swap))
    ("clone" . ((target-clone) do-all))
    ("jump" . ((car goto-char) multiple-cursors-do))
    ("pre" . ((car goto-char) multiple-cursors-do))
    ("post" . ((cdr goto-char) multiple-cursors-do))
    ("change" . ((target-change)
                 multiple-cursors-do))
    ("unwrap" . ((t ->
                    t (my/surrounding-inner) thing-expand target-string
                    t (my/surrounding-outer) thing-expand target-delete
                    t swap target-insert ..)
                 do-all))
    ("comment" .
     ((uncons \\ comment-region 2 lisp-eval-n) do-all))
    ("uncomment" .
     ((uncons \\ uncomment-region 2 lisp-eval-n) do-all))
    ("indent" . ((target-indent) do-all))
    ("narrow" .
     (uncons \\ narrow-to-region 2 lisp-eval-n))
    ("wrap" . ((target-wrap-parentheses) curry eval do-all))
    ("crush" . (amalgamate-stack \\ reverse lisp-funcall crush))
    ("fillout" . (dup target-string (paint) dip target-overwrite))
    ("filler" . (((car goto-char \\ fill-paragraph lisp-eval) save-excursion) do-all))))

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

(he--define-compound-instruction 'paint
  '(paint-left paint-right))

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

(he--define-compound-instruction 'trim
  '(trim-left trim-right))

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

(he--define-lisp-instruction-1 if (condition then else)
  (if condition then else))

(he--define-instruction 'inner-parenthesis-dwim
  (lambda (environment)
    (let ((head (he--peek-value environment)))
      (if (characterp head)
          (he--on-environment #'he-i--inner-parenthesis environment)
        (he--on-environment #'he-i--inner-parenthesis-any environment)))))

(he--define-lisp-instruction-1 nthcdr (list n)
  (nthcdr n list))

(he--define-compound-instruction 'on-instructions
  `(instructions
    (2 nthcdr ; Remove this instruction and the following eval
       swap
       make-subenvironment
     evaluate-subenvironment
     replace-instructions)
    eval))

(he--define-compound-instruction 'map-next
  `(instructions
    (,(he--lambda (f dummy dummy) \\ map-stack \\ f \\ \\)
     make-subenvironment
     evaluate-subenvironment
     replace-instructions)
    eval))

(he--define-lisp-instruction-1 targets-join (targets)
  (he--markify-region
   (cons (apply #'min (mapcar #'car targets))
         (apply #'max (mapcar #'cdr targets)))))

(he--define-compound-instruction 'past
  '(\\ list 2 lisp-funcall-n targets-join))

(he--define-compound-instruction 'make-infix
  (he--lambda (f) (((f eval) unstack) dip) on-instructions))

(defvar he-modifiers
  `(("leftpaint" .
     ((paint-left) map-stack))
    ("rightpaint" .
     ((paint-right) map-stack))
    ("paint" .
     ((paint) map-stack))
    ("trim" .
     ((trim) map-stack))
    ("past" . ((past) make-infix))
    ("and past" . (past))
    ("all past" . (amalgamate-stack targets-join))
    ("selection" .
     ((mark point cons) \\ he--multiple-cursors-map lisp-funcall unstack))
    ("every instance" .
     (target-string find-occurrences unstack))
    ("every line" .
     (,(lambda () (he--every-thing 'line))
      0 lisp-funcall-n
      unstack))
    ("inside" . (inner-parenthesis-dwim))
    ("map" . (map-next))))


;;; hatty-edit.el ends soon
(provide 'hatty-edit)

;; Local Variables:
;; read-symbol-shorthands: (("he-" . "hatty-edit-"))
;; End:
;;; hatty-edit.el ends here
