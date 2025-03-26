;;; cursorfree.el --- Edit and navigate through hats -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1") (hatty "0.2.0"))
;; Created: 06 Sep 2024

;; cursorfree.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; cursorfree.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a command structure for editing and
;; navigating text using hatty.el.  A command is created as a sequence
;; of instructions, functions taking a `cursorfree-environment' as
;; input and output.  These functions may add or modify the value
;; stack in the environment, or perform side effects informed by the
;; contents of the value stack.

;; To evaluate a sequence of instructions, use `cursorfree-evaluate'.
;; See `cursorfree-actions' and `cursorfree-modifiers' for a list of
;; predefined instructions.

;;; Code:

(require 'hatty)
(require 'evil)
(require 'dash)
(require 'multiple-cursors)

;;;; Instruction interpreter:

(cl-defstruct cursorfree-environment
  "Environment for executing cursorfree programs.

The environment is made up of a value stack and an instruction
stack.  The value stack may be regarded as the the \"memory\" of
the environment: This is for example where information about the
targets to be acted upon is stored.

The instruction stack is a sequence of functions, taking as input
the environment and outputting a transformed environment.

When evaluating an environment (see
`cursorfree-evaluate-environment'), the top instruction in the
instruction stack will be evaluated with the current environment
to yield the next environment (see `cursorfree--step').  This
will be repeated until there are no instructions left."
  (instruction-stack nil) (value-stack nil))

(defun cursorfree--make-environment (instructions &optional value-stack)
  "Create an environment with INSTRUCTIONS and VALUE-STACK.

See `cursorfree-environment' for information about environments."
  (make-cursorfree-environment
   :instruction-stack instructions
   :value-stack value-stack))

(defun cursorfree--clone-environment (environment)
  "Create a shallow copy of ENVIRONMENT."
  (make-cursorfree-environment
   :value-stack (cursorfree-environment-value-stack environment)
   :instruction-stack (cursorfree-environment-instruction-stack environment)))

(defun cursorfree--push-instruction (environment instruction)
  "Add INSTRUCTION to the instruction stack ENVIRONMENT."
  (push instruction (cursorfree-environment-instruction-stack environment)))

(defun cursorfree--push-instructions (environment instructions)
  "Add INSTRUCTIONS to the instruction stack ENVIRONMENT.

The first instruction in INSTRUCTIONS will end up at the top of
the instruction stack."
  (dolist (instruction (reverse instructions))
    (cursorfree--push-instruction environment instruction)))

(defun cursorfree--pop-instruction (environment)
  "Remove and return the top instruction in ENVIRONMENT."
  (pop (cursorfree-environment-instruction-stack environment)))

(defun cursorfree--push-value (environment value)
  "Add VALUE to the value stack of ENVIRONMENT."
  (declare (indent defun))
  (push value (cursorfree-environment-value-stack environment)))

(defun cursorfree--push-value-pure (environment value)
  "Return environment with VALUE added to ENVIRONMENT."
  (declare (indent defun))
  (let ((new-environment (cursorfree--clone-environment environment)))
    (cursorfree--push-value new-environment value)
    new-environment))

(defun cursorfree--push-values (environment values)
  "Add VALUES to the value stack of ENVIRONMENT.

The first value in VALUES will end up at the top of the value
stack."
  (declare (indent defun))
  (dolist (value (reverse values))
    (cursorfree--push-value environment value)))

(defun cursorfree--pop-value (environment)
  "Remove and return the top value in ENVIRONMENT."
  (declare (indent defun))
  (pop (cursorfree-environment-value-stack environment)))

(defun cursorfree--pop-values (environment n)
  "Remove and return the top N values in ENVIRONMENT.

The top value of the value stack will end up at the beginning of
the returned list."
  (declare (indent defun))
  (let ((acc nil))
    (dotimes (i n (reverse acc))
      (push (cursorfree--pop-value environment) acc))))

(defun cursorfree--peek-value (environment)
  "Return the top value in ENVIRONMENT."
  (declare (indent defun))
  (car (cursorfree-environment-value-stack environment)))

(defun cursorfree--step (environment)
  "Evaluate the next instruction of ENVIRONMENT.

This will return the top instruction of ENVIRONMENT applied to
ENVIRONMENT with that instruction removed."
  (let* ((new-environment (cursorfree--clone-environment environment))
         (instruction (cursorfree--pop-instruction new-environment)))
    (funcall instruction new-environment)))

(defun cursorfree-evaluate-environment (environment)
  "Evaluate ENVIRONMENT and return the final value stack.

This will step through ENVIRONMENT with `cursorfree--step' until
there are no instructions left, at wich point it returns the
final stack of values."
  (declare (indent defun))
  (while (cursorfree-environment-instruction-stack environment)
    (setq environment (cursorfree--step environment)))
  (cursorfree-environment-value-stack environment))

(defun cursorfree-evaluate (instructions)
  "Evaluate INSTRUCTIONS and return the final value stack.

This creates an initial environment with empty value stack, upon which
`cursorfree-evaluate-environment' is invoked."
  (cursorfree-evaluate-environment
    (cursorfree--make-environment instructions)))

(defun cursorfree--apply-on-stack (function stack)
  "Apply FUNCTION to the top elements of STACK.

The arity of FUNCTION will be assumed to be the car of
`func-arity'.  The function is evaluated with the top values of
STACK, with the top elements applied as the first arguments.
Returns the unapplied elements of STACK with the return value of
FUNCTION on top."
  (let* ((arity (car (func-arity function)))
         (args (take arity stack))
         (tail (nthcdr arity stack)))
    (cons (apply function args) tail)))

(defun cursorfree-make-action (function)
  "Translate FUNCTION into an instruction not producing any value.

The resulting instruction will read the top elements of the value
stack to supply arguments for FUNCTION.  The read arguments will
not remain on the value stack."
  (lambda (environment)
    (let* ((e (cursorfree--clone-environment environment))
           (values (cursorfree-environment-value-stack e)))
      (setf (cursorfree-environment-value-stack e)
            (cursorfree--apply-on-stack function values))
      (cursorfree--pop-value e) ; Ignore return value
      e)))

(defun cursorfree-make-parallel-action (function)
  "Make instruction applying FUNCTION to each element of the stack."
  (lambda (environment)
    (let* ((e (cursorfree--clone-environment environment))
           (values (cursorfree-environment-value-stack e)))
      (setf (cursorfree-environment-value-stack e) nil)
      (dolist (value values)
        (funcall function value))
      e)))

(defun cursorfree-make-multi-cursor-action (function)
  "Translate FUNCTION into an instruction using multiple cursors.

FUNCTION will be applied on each element of the stack.  For each
target, a new cursor will be created."
  (lambda (environment)
    (let* ((e (cursorfree--clone-environment environment))
           (values (cursorfree-environment-value-stack e)))
      (setf (cursorfree-environment-value-stack e) nil)

      (multiple-cursors-mode 0)

      ;; Only create new cursors for non-final elements.
      (while (cdr values)
        ;; Error?  No issue, just try again with the next element.
        (condition-case e
            (funcall function (car values))
          (:success (multiple-cursors-mode 1)
                    (mc/create-fake-cursor-at-point))
          (error nil))
        (pop values))
      (when values (funcall function (car values)))
      e)))

(defun cursorfree-make-modifier (function)
  "Translate FUNCTION to an instruction producing a value.

The resulting instruction will read the top elements of the value
stack to supply arguments for FUNCTION.  The result of invoking
FUNCTION will be put back on the value stack.  The read arguments
will not remain on the stack."
  (lambda (environment)
    (let* ((e (cursorfree--clone-environment environment))
           (values (cursorfree-environment-value-stack e)))
      (setf (cursorfree-environment-value-stack e)
            (cursorfree--apply-on-stack function values))
      e)))

(defun cursorfree-make-flattening-modifier (function)
  "Translate FUNCTION to an instruction producing multiple values.

The resulting instruction will act as if `cursorfree-make-modifier'
was used, but assumes that the function returns a list.  Each element
of the list will be pushed onto the value stack, with the first
element of the list pushed first."
  (lambda (environment)
    (let* ((e (cursorfree--clone-environment environment))
           (values (cursorfree-environment-value-stack e)))
      (setf (cursorfree-environment-value-stack e)
            (cursorfree--apply-on-stack function values))
      (cursorfree--push-values e (cursorfree--pop-value e))
      e)))


;; TODO: Insert buffer info
(defun cursorfree--markify-region (region)
  "Return REGION with the endpoints as markers."
  (unless (consp region)
    (error "Invalid argument %s in cursorfree--markify-region" region))
  (cons (if (markerp (car region))
            (car region)
          (move-marker (make-marker) (car region)))
        (if (markerp (cdr region))
            (cdr region)
          (move-marker (make-marker) (cdr region)))))

(defun cursorfree--bounds-of-thing-at (thing position)
  "Return bounds of THING at POSITION."
  (save-excursion
    (goto-char position)
    (if-let ((bounds (bounds-of-thing-at-point thing)))
        (cursorfree--markify-region bounds))))

(cl-defstruct cursorfree-region-target
  content-region)

(defun cursorfree--make-target (content-region)
  "Return a target spanning CONTENT-REGION."
  (make-cursorfree-region-target
   :content-region (cursorfree--markify-region content-region)))

;; TODO remove
(defun cursorfree--on-content-region (region-target f)
  "Apply F to the content region of REGION-TARGET."
  (declare (indent defun))
  (unless (cursorfree-region-target-p region-target)
    (error (format "Type error: %s is not of type cursorfree-region-target." region-target)))
  (funcall f (cursorfree-region-target-content-region region-target)))

(defun cursorfree--make-target-from-hat (character &optional color shape)
  "Return target spanning a token.

The token is indexed by CHARACTER, COLOR and SHAPE, as specified
by `hatty-locate-token-region'."
  (cursorfree--make-target
   (hatty-locate-token-region character color shape)))

(defun cursorfree--pusher (value)
  "Return instruction pushing VALUE to the value stack."
  (lambda (environment)
    (cursorfree--push-value-pure environment value)))

;;;; Core functions

(defun cursorfree--target-get (target)
  "Return the content referred to by TARGET."
  (pcase target
    ('kill-ring (current-kill 0 nil))
    ((pred characterp) target)
    ((pred stringp) target)
    ((pred cursorfree-region-target-p)
     (let ((region (cursorfree--content-region target)))
       (buffer-substring-no-properties (car region) (cdr region))))
    (_ (error (format "No method for getting content of target %s" target)))))

(defun cursorfree--target-put (target content)
  "Put CONTENT into TARGET."
  (pcase target
    ('kill-ring (kill-new content))
    ((pred cursorfree-region-target-p)
     (cursorfree--on-content-region target
       (lambda (region)
         (cursorfree--region-delete region)
         (cursorfree--insert-at (car region) (if (characterp content)
                                                 (string content)
                                               content)))))
    (_ (error (format "No method for writing content to target %s" target)))))

;;;; End of core functions

;; TODO: Introduce region-target abstraction layer?

(defun cursorfree--content-region (target)
  "Return region of the content referred to by TARGET."
  (cursorfree-region-target-content-region target))

(defun cursorfree--deletion-region (target)
  "Return region that should be removed if deleting TARGET."
  (cursorfree--markify-region
   (cursorfree--on-content-region target
     (lambda (region)
       (save-excursion
         (goto-char (cdr region))
         (if (/= 0 (skip-chars-forward "[:space:]\n"))
             (cons (car region) (point))
           (goto-char (car region))
           (skip-chars-backward "[:space:]\n")
           (cons (point) (cdr region))))))))

;; TODO deprecate
(defalias #'cursorfree--target-string #'cursorfree--target-get)

(defun cursorfree--region-delete (region)
  "Delete REGION."
  (delete-region (car region) (cdr region)))

(defun cursorfree-target-pulse (target)
  "Temporarily highlight TARGET."
  (when (cursorfree-region-target-p target)
    (let ((region (cursorfree--content-region target)))
      (pulse-momentary-highlight-region (car region) (cdr region)))))

(defun cursorfree--insert-at (position string)
  "Insert STRING at POSITION."
  (save-excursion
    (goto-char position)
    (insert string)
    (cursorfree-target-pulse (cons position (+ position (length string))))))

(defun cursorfree-target-select (target)
  "Set active region to TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (set-mark (car region))
      (goto-char (cdr region)))))

(defun cursorfree-target-jump-beginning (target)
  "Move point to beginning of TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (goto-char (car region)))))

(defun cursorfree-target-jump-end (target)
  "Move point to end of TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (goto-char (cdr region)))))

(defun cursorfree-target-indent (target)
  "Indent TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (indent-region (car region) (cdr region)))))

(defun cursorfree-target-copy (target)
  "Copy TARGET to kill ring."
  (kill-new (cursorfree--target-get target))
  (cursorfree-target-pulse target))

(defun cursorfree-target-chuck (target)
  "Delete TARGET and indent the resulting text."
  (cursorfree--region-delete (cursorfree--deletion-region target))
  (if-let ((indentation-region (cursorfree--bounds-of-thing-at 'line (car target))))
      (cursorfree-target-indent indentation-region)))

(defmacro cursorfree--for-each-cursor (&rest body)
  "Evaluate BODY for each cursor."
  `(mc/for-each-cursor-ordered
    (mc/restore-state-from-overlay cursor)
    ,@body
    (mc/store-current-state-in-overlay cursor)))

(defun cursorfree-target-bring (target)
  "Insert TARGET at point."
  (cursorfree--for-each-cursor
   (insert (cursorfree--target-get target)))
  (cursorfree-target-pulse target))

;; TODO remove old "move" and "bring".

;; TODO deprecate
(defalias #'cursorfree--target-overwrite #'cursorfree--target-put)

(defun cursorfree-target-move (target)
  "Insert TARGET at point and delete TARGET."
  (cursorfree-target-bring target)
  (cursorfree-target-chuck target))

(defun cursorfree-target-swap (target1 target2)
  "Swap the contents of TARGET1 and TARGET2."
  (let ((string1 (cursorfree--target-get target1))
        (string2 (cursorfree--target-get target2)))
    (cursorfree--target-put target1 string2)
    (cursorfree--target-put target2 string1)))

(defun cursorfree-target-pull (target-from target-to)
  "Overwrite TARGET-TO with the contents of TARGET-FROM."
  (cursorfree--target-put target-to
                          (cursorfree--target-get target-from)))

(defun cursorfree-target-change (target)
  "Move point to TARGET and delete its contents."
  (cursorfree--on-content-region target
    (lambda (region)
      (cursorfree--region-delete region)
      (goto-char (car region)))))

;; TODO: Don't move point
(defun cursorfree-target-clone (target)
  "Insert another copy of TARGET after itself."
  (cursorfree--target-put
   target
   (concat
    (cursorfree--target-get target)
    (cursorfree--target-get target))))

(defmacro cursorfree--simple-content-function (name docstring function)
  "Define function with NAME that performs FUNCTION on a target.

FUNCTION will be invoked with the bounds of the content region of the
target.  Afterwards, the region will be pulsed."
  (declare (indent defun))
  `(defun ,name (target)
     (cursorfree--on-content-region target
       (lambda (region)
         (,function (car region) (cdr region))))
     (cursorfree-target-pulse target)))

(cursorfree--simple-content-function cursorfree-target-comment
  "Comment out TARGET."
  comment-region)

(cursorfree--simple-content-function cursorfree-target-uncomment
  "Uncomment TARGET."
  uncomment-region)

(cursorfree--simple-content-function cursorfree-target-narrow
  "Narrow region to TARGET."
  narrow-to-region)

(cursorfree--simple-content-function cursorfree-target-fill
  "Fill the paragraphs in TARGET."
  fill-region)

(cursorfree--simple-content-function cursorfree-target-capitalize
  "Capitalize the first character of each word in TARGET."
  capitalize-region)

(cursorfree--simple-content-function cursorfree-target-upcase
  "Convert TARGET to upper case."
  upcase-region)

(cursorfree--simple-content-function cursorfree-target-downcase
  "Convert TARGET to lower case."
  downcase-region)

(defun cursorfree--clamp-line ()
  "Move point to within window if outside."
  (let* ((current-column (current-column))
         (current-position (point))
         (top-position
          (progn
            (move-to-window-line 0)
            (move-to-column current-column)
            (point)))
         (bottom-position
          (progn
            (move-to-window-line -1)
            (move-to-column current-column)
            (point))))
    (goto-char
     (max top-position (min bottom-position current-position)))))

(defun cursorfree-target-crown (target)
  "Scroll window so TARGET is at the top."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (cursorfree-target-jump-beginning region)
        (recenter 0))
      (cursorfree--clamp-line))))

(defun cursorfree-target-center (target)
  "Scroll window so TARGET is in the center."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (cursorfree-target-jump-beginning region)
        (recenter nil))
      (cursorfree--clamp-line))))

(defun cursorfree-target-bottom (target)
  "Scroll window so TARGET is at the bottom."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (cursorfree-target-jump-beginning region)
        (recenter -1))
      (cursorfree--clamp-line))))

(defun cursorfree-target-drink (target)
  "Insert an empty line before TARGET and put point on it."
  (cursorfree--on-content-region target
    (lambda (region)
      (goto-char (car region))
      (beginning-of-line)
      (insert ?\n)
      (backward-char))))

(defun cursorfree-target-pour (target)
  "Insert an empty line after TARGET and put point on it."
  (cursorfree--on-content-region target
    (lambda (region)
      (goto-char (cdr region))
      (end-of-line)
      (insert ?\n))))

(defun cursorfree-target-wrap-parentheses (parenthesis target)
  "Wrap TARGET with characters specified by PARENTHESIS.

Insert PARENTHESIS before TARGET.  If PARENTHESIS is some type of
parenthesis, insert the matching right version at the end of
TARGET.  Otherwise, insert PARENTHESIS instead."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (insert parenthesis)
        (goto-char (cdr region))
        (insert
         (pcase parenthesis
           (?\( ?\))
           (?\[ ?\])
           (?< ?>)
           (?{ ?})
           (_ parenthesis)))))))

(defvar cursorfree-dwim-follow-alist
  `((org-mode . org-open-at-point)
    (org-agenda-mode . org-agenda-switch-to)
    (Info-mode . Info-try-follow-nearest-node)
    (dired-mode . dired-find-file)
    (compilation-mode . compile-goto-error)
    (grep-mode . compile-goto-error)
    (occur-mode . occur-mode-goto-occurrence)
    (eww-mode . ,(lambda ()
                   (if (get-text-property (point) 'eww-form)
                       (eww-submit)
                     (eww-follow-link)))))
  "Alist for mapping major mode to function for following at point.

Used in `cursorfree-dwim-follow' for determining how to follow
whatever thing point is located on.")

(defun cursorfree-dwim-follow ()
  "Try to follow the thing at point.
If point is at a button, push it.  Otherwise, use the current major
mode to look up the function in `cursorfree-dwim-follow-alist'."
  (if (button-at (point))
      (push-button)
    (if-let ((follow-action
              (alist-get major-mode cursorfree-dwim-follow-alist)))
        (funcall follow-action))))

(defun cursorfree-target-pick (target)
  "Try to follow the thing at TARGET.

This function calls on `cursorfree-dwim-follow' to attempt to
follow the thing at TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (goto-char (car region))
      (cursorfree-dwim-follow))))

;; TODO: Errors on invocation?
(defun cursorfree-target-fuse (target)
  "Remove all whitespace within TARGET."
  (let ((content (cursorfree--target-get target)))
    (cursorfree--target-put
     target
     (with-temp-buffer
       (insert content)
       (replace-regexp (rx (or whitespace "\n")) "" nil (point-min) (point-max))
       (buffer-string)))))

(defun cursorfree-target-join (target)
  "Remove all newlines within TARGET."
  (let ((content (cursorfree--target-get target)))
    (cursorfree--target-put
     target
     (with-temp-buffer
       (insert content)
       (replace-regexp (rx "\n" (zero-or-more " ")) "" nil (point-min) (point-max))
       (buffer-string)))))

(defun cursorfree-target-break (target)
  "Insert newline before TARGET."
  (save-excursion
    (let ((region (cursorfree--content-region target)))
      (goto-char (car region))
      (newline)
      (indent-region (car region) (cdr region)))))

(defun cursorfree-target-help (target)
  "Run `display-local-help' at the start of TARGET.

This may, for example, be used for displaying warning from eglot."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (display-local-help)
        (cursorfree-target-pulse region)))))

(defun cursorfree-target-occur (target)
    "List occurrences of TARGET in the current buffer."
    (occur (rx (literal (cursorfree--target-get target)))))

(defun cursorfree-target-unwrap-parentheses (target)
    "Remove parentheses or quotation around TARGET."
    (cursorfree-target-pull
     (cursorfree-inner-parenthesis-any target)
     (cursorfree-outer-parenthesis-any target)))

(defvar cursorfree-actions
  `(("select" . ,(cursorfree-make-multi-cursor-action #'cursorfree-target-select))
    ("copy" . ,(cursorfree-make-action #'cursorfree-target-copy))
    ("chuck" . ,(cursorfree-make-parallel-action #'cursorfree-target-chuck))
    ("bring" . ,(cursorfree-make-action #'cursorfree-target-bring))
    ("move" . ,(cursorfree-make-action #'cursorfree-target-move))
    ("pull" . ,(cursorfree-make-action #'cursorfree-target-pull))
    ("swap" . ,(cursorfree-make-action #'cursorfree-target-swap))
    ("clone" . ,(cursorfree-make-action #'cursorfree-target-clone))
    ("jump" . ,(cursorfree-make-multi-cursor-action #'cursorfree-target-jump-beginning))
    ("pre" . ,(cursorfree-make-multi-cursor-action #'cursorfree-target-jump-beginning))
    ("post" . ,(cursorfree-make-multi-cursor-action #'cursorfree-target-jump-end))
    ("change" . ,(cursorfree-make-multi-cursor-action #'cursorfree-target-change))
    ("comment" . ,(cursorfree-make-action #'cursorfree-target-comment))
    ("uncomment" . ,(cursorfree-make-action #'cursorfree-target-uncomment))
    ("indent" . ,(cursorfree-make-action #'cursorfree-target-indent))
    ("narrow" . ,(cursorfree-make-action #'cursorfree-target-narrow))
    ("wrap" . ,(cursorfree-make-action #'cursorfree-target-wrap-parentheses))
    ("unwrap" . ,(cursorfree-make-action #'cursorfree-target-unwrap-parentheses))
    ("filler" . ,(cursorfree-make-action #'cursorfree-target-fill))
    ("title" . ,(cursorfree-make-action #'cursorfree-target-capitalize))
    ("upcase" . ,(cursorfree-make-action #'cursorfree-target-upcase))
    ("downcase" . ,(cursorfree-make-action #'cursorfree-target-downcase))
    ("crown" . ,(cursorfree-make-action #'cursorfree-target-crown))
    ("center" . ,(cursorfree-make-action #'cursorfree-target-center))
    ("bottom" . ,(cursorfree-make-action #'cursorfree-target-bottom))
    ("pick" . ,(cursorfree-make-action #'cursorfree-target-pick))
    ("fuse" . ,(cursorfree-make-action #'cursorfree-target-fuse))
    ("join" . ,(cursorfree-make-action #'cursorfree-target-join))
    ("flash" . ,(cursorfree-make-action #'cursorfree-target-pulse))
    ("help" . ,(cursorfree-make-action #'cursorfree-target-help))
    ("drink" . ,(cursorfree-make-action #'cursorfree-target-drink))
    ("pour" . ,(cursorfree-make-action #'cursorfree-target-pour))
    ("occur" . ,(cursorfree-make-action #'cursorfree-target-occur)))
  "Alist mapping spoken utterance to action.

An action is an instruction that is only evaluated for its
effects, and do not add values to the value stack.")

(defun cursorfree--skip-forward-from (position string)
  "Move point forward from POSITION until reaching char in STRING."
  (save-excursion
    (goto-char position)
    (skip-chars-forward string)
    (point-marker)))

(defun cursorfree--skip-backward-from (position string)
  "Move point backward from POSITION until reaching char in STRING."
  (save-excursion
    (goto-char position)
    (skip-chars-backward string)
    (point-marker)))

(defun cursorfree-paint-left (target)
  "Expand TARGET leftwards until the next whitespace."
  (let ((region (cursorfree--content-region target)))
    (cursorfree--make-target
     (cons (cursorfree--skip-backward-from (car region) "^[:space:]\n")
           (cdr region)))))

(defun cursorfree-paint-right (target)
  "Expand TARGET rightwards until the next whitespace."
  (let ((region (cursorfree--content-region target)))
    (cursorfree--make-target
     (cons (car region)
           (cursorfree--skip-forward-from (cdr region) "^[:space:]\n")))))

(defun cursorfree-paint (target)
  "Expand TARGET leftwards and rightwards until the next whitespace."
  (cursorfree-paint-right (cursorfree-paint-left target)))

(defun cursorfree-trim (target)
  "Shrink TARGET until there is no whitespace to the left or right."
  (let ((region (cursorfree--content-region target)))
    (cursorfree--make-target
     (cons (cursorfree--skip-forward-from (car region) "[:space:]\n")
           (cursorfree--skip-backward-from (cdr region) "[:space:]\n")))))

(defun cursorfree-inner-parenthesis (delimiter target)
  "Expand TARGET to fill the insides of DELIMITER.

This function will match parentheses and quotation marks to the
left and right."
  (save-excursion
    ;; evil-inner-double-quote uses the location of point for the
    ;; expansion.  Put point at the beginning of the region.
    (goto-char (car (cursorfree--content-region target)))
    (let ((expanded
           (funcall
            (cl-case delimiter
              (?\( #'evil-inner-paren)
              (?\[ #'evil-inner-bracket)
              (?< #'evil-inner-angle)
              (?{ #'evil-inner-curly)
              (?\" #'evil-inner-double-quote)
              (?\' #'evil-inner-single-quote)
              (?\` #'evil-inner-back-quote)))))
      (cursorfree--make-target (cons (car expanded) (cadr expanded))))))

(defun cursorfree-outer-parenthesis (delimiter target)
  "Expand TARGET to contain the closest DELIMITER.

This function will match parentheses and quotation marks to the
left and right."
  (save-excursion
    ;; evil-outer-double-quote uses the location of point for the
    ;; expansion.  Put point at the beginning of the region.
    (goto-char (car (cursorfree--content-region target)))
    (let ((expanded
           (funcall
            (cl-case delimiter
              (?\( #'evil-a-paren)
              (?\[ #'evil-a-bracket)
              (?< #'evil-an-angle)
              (?{ #'evil-a-curly)
              (?\" #'evil-a-double-quote)
              (?\' #'evil-a-single-quote)
              (?\` #'evil-a-back-quote)))))
      (cursorfree--make-target (cons (car expanded) (cadr expanded))))))

(defun cursorfree-inner-parenthesis-any (target)
  "Expand TARGET to fill the insides of the closest delimiters.

This function tries different parentheses and quotations to
figure out whichever is closest."
  (-max-by (-on #'> (lambda (target) (car (cursorfree--content-region target))))
           ;; Filter out whenever the evil-inner-*-quote messes up the
           ;; region (it selects the next region if not currently in a
           ;; quote)
           (--filter (<= (car (cursorfree--content-region it))
                         (car (cursorfree--content-region target)))
                     (--keep (condition-case nil
                                 (cursorfree-inner-parenthesis it target)
                               (error nil))
                             '(?< ?{ ?\( ?\[ ?\" ?\' ?\`)))))

(defun cursorfree-outer-parenthesis-any (target)
  "Expand TARGET to contain the closest delimiters.

This function tries different parentheses and quotations to
figure out whichever is closest."
  (-max-by (-on #'> (lambda (target) (car (cursorfree--content-region target))))
           ;; Filter out whenever the evil-inner-*-quote messes up the
           ;; region (it selects the next region if not currently in a
           ;; quote)
           (--filter (<= (car (cursorfree--content-region it))
                         (car (cursorfree--content-region target)))
                     (--keep (condition-case nil
                                 (cursorfree-outer-parenthesis it target)
                               (error nil))
                             '(?< ?{ ?\( ?\[ ?\" ?\' ?\`)))))

(defun cursorfree-inner-parenthesis-dwim (environment)
  "Expand a target to fill the insides of some delimiter.

This will read the top value of ENVIRONMENT.  If this is a
character, the next element is assumed to be a target to be
expanded until the delimiter given by the character.  Otherwise,
assumes that the top element was a target and expands it to the
nearest matching pairs of delimiters."
  (let ((head (cursorfree--peek-value environment)))
    (funcall (cursorfree-make-modifier
              (if (characterp head)
                  #'cursorfree-inner-parenthesis
                #'cursorfree-inner-parenthesis-any))
             environment)))

(defun cursorfree-outer-parenthesis-dwim (environment)
  "Expand a target to contain some delimiter.

This will read the top value of ENVIRONMENT.  If this is a
character, the next element is assumed to be a target to be
expanded until the delimiter given by the character.  Otherwise,
assumes that the top element was a target and expands it to the
nearest matching pairs of delimiters."
  (let ((head (cursorfree--peek-value environment)))
    (funcall (cursorfree-make-modifier
              (if (characterp head)
                  #'cursorfree-outer-parenthesis
                #'cursorfree-outer-parenthesis-any))
             environment)))

(defun cursorfree--targets-join (targets)
  "Return the smallest target that can fit all TARGETS."
  (cursorfree--make-target
   (cons (apply #'min (mapcar (lambda (target) (car (cursorfree--content-region target)))
                              targets))
         (apply #'max (mapcar (lambda (target) (cdr (cursorfree--content-region target)))
                              targets)))))

(defun cursorfree-past (target1 target2)
  "Return the smallest target that can fit TARGET1 and TARGET2."
  (cursorfree--targets-join (list target1 target2)))

(defun cursorfree-current-selection ()
  "Return the active region as a target."
  ;; TODO: Handle noncontiguous selections?
  (cursorfree--make-target
   (car (region-bounds))))

(defun cursorfree-thing-to-modifier (thing)
  "Translate THING to an instruction extending a target to THING.

The extension is done from the beginning of the target.  See
`bounds-of-thing-at-point' for more information about the builtin
thing-at-point functionalities."
  (cursorfree-make-modifier
   (lambda (target)
     (cursorfree--make-target
      (cursorfree--bounds-of-thing-at thing (car (cursorfree--content-region target)))))))

(defun cursorfree-everything ()
  "Return a target referring to the full content of the buffer.

This function respects narrowing."
  (cursorfree--make-target
   (cons (point-min) (point-max))))

(defun cursorfree-visible ()
  "Return a target referring to the visible portion of the buffer."
  (save-excursion
    (let (beginning end)
      (move-to-window-line 0)
      (beginning-of-visual-line)
      (setq beginning (point))
      (move-to-window-line -1)
      (end-of-visual-line)
      (setq end (point))
      (cursorfree--make-target
       (cons beginning end)))))

(defun cursorfree-line-right (target)
  "Extend TARGET to include the next newline."
  (save-excursion
    (goto-char (cdr (cursorfree--content-region target)))
    (unless (search-forward "\n" nil t)
      (goto-char (point-max)))
    (cursorfree--make-target (cons (car (cursorfree--content-region target)) (point)))))

(defun cursorfree-line-left (target)
  "Extend TARGET to start after the previous newline."
  (save-excursion
    (goto-char (car (cursorfree--content-region target)))
    (if (search-backward "\n" nil t)
        (forward-char) ; Jump over the searched for newline
      (goto-char (point-min)))
    (cursorfree--make-target (cons (point) (cdr (cursorfree--content-region target))))))

(defun cursorfree-line (target)
  "Extend TARGET to fill the full line."
  (cursorfree-line-left (cursorfree-line-right target)))

(defun cursorfree-row (index)
  "Return the line with number INDEX as a target."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- index))
    (cursorfree-line (cursorfree--make-target (cons (point) (point))))))

(defun cursorfree-this ()
  "Return an empty region located at point."
  (cursorfree--make-target (cons (point) (point))))

(defun cursorfree-extend-right (target1 target2)
  "Return target extending TARGET2 to the end of TARGET1."
  (cursorfree--make-target
   (cons (car (cursorfree--make-target target2))
         (max (cdr (cursorfree--make-target target2))
              (cdr (cursorfree--make-target target1))))))

(defun cursorfree-every-instance (target)
  "Return a list of every occurrence of TARGET."
  (when (/= (- (car target) (cdr target)) 0)
    (let ((string (cursorfree--target-get target))
          (matches '()))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward string nil t)
          (push (cursorfree--make-target (cons (match-beginning 0) (match-end 0)))
                matches)))
      (reverse matches))))

(defun cursorfree-dup (environment)
  "Duplicate the top value in the value stack of ENVIRONMENT."
  (cursorfree--push-value-pure environment
    (cursorfree--peek-value environment)))

(defun cursorfree-kill-ring ()
  "Return symbol indicating the kill ring.

When read from as a target, this will return the top element of the
kill ring."
  'kill-ring)

(defvar cursorfree-modifiers
  `(("paint" . ,(cursorfree-make-modifier #'cursorfree-paint))
    ("leftpaint" . ,(cursorfree-make-modifier #'cursorfree-paint-left))
    ("rightpaint" . ,(cursorfree-make-modifier #'cursorfree-paint-right))
    ("trim" . ,(cursorfree-make-modifier #'cursorfree-trim))
    ("past" . ,(cursorfree-make-modifier #'cursorfree-past))
    ("selection" . ,(cursorfree-make-modifier #'cursorfree-current-selection))
    ("inside" . cursorfree-inner-parenthesis-dwim)
    ("outside" . cursorfree-outer-parenthesis-dwim)
    ("line" . ,(cursorfree-make-modifier #'cursorfree-line))
    ("rightline" . ,(cursorfree-make-modifier #'cursorfree-line-right))
    ("leftline" . ,(cursorfree-make-modifier #'cursorfree-line-left))
    ("block" . ,(cursorfree-thing-to-modifier 'paragraph))
    ("link" . ,(cursorfree-thing-to-modifier 'url))
    ;; ("word" . ,(cursorfree-thing-to-modifier 'word))
    ("sentence" . ,(cursorfree-thing-to-modifier 'sentence))
    ("everything" . ,(cursorfree-make-modifier #'cursorfree-everything))
    ("visible" . ,(cursorfree-make-modifier #'cursorfree-visible))
    ("row" . ,(cursorfree-make-modifier #'cursorfree-row))
    ("this" . ,(cursorfree-make-modifier #'cursorfree-this))
    ("right" . ,(cursorfree-make-modifier #'cursorfree-extend-right))
    ("every instance" . ,(cursorfree-make-flattening-modifier #'cursorfree-every-instance))
    ("dupe" . cursorfree-dup)))

;;; cursorfree.el ends soon
(provide 'cursorfree)
;;; cursorfree.el ends here
