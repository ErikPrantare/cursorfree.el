;;; cursorfree.el --- Edit and navigate through hats -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1") (hatty "1.3.0"))
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

(defun cursorfree--reverse-argument-order (function args)
  (put function 'cursorfree--reverse-argument-order t))

(setf (alist-get 'cursorfree--reverse-argument-order defun-declarations-alist)
      (list #'cursorfree--reverse-argument-order))

(defun cursorfree--apply (function args)
  (when (and (symbolp function)
             (get function 'cursorfree--reverse-argument-order))
    (setq args (reverse args)))
  (apply function args))

(defun cursorfree--apply-on-stack (function stack)
  "Apply FUNCTION to the top elements of STACK.
Returns the unapplied elements of STACK with the return value of
FUNCTION on top.

The arity of FUNCTION is read from the cdr of `func-arity'.  The
function is evaluated with the top values of STACK, with the top
elements applied as the first arguments.  &rest arguments are
supported."
  (let* ((arity (cdr (func-arity function)))
         (args (if (eq arity 'many) stack (take arity stack)))
         (tail (if (eq arity 'many) '() (nthcdr arity stack))))
    (cons (cursorfree--apply function args) tail)))

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

(defun cursorfree--multiple-cursors-do (function targets)
  (multiple-cursors-mode 0)

  ;; Only create new cursors for non-final elements.
  (while (cdr targets)
    ;; Error?  No issue, just try again with the next element.
    (condition-case e
        (funcall function (car targets))
      (:success (multiple-cursors-mode 1)
                (mc/create-fake-cursor-at-point))
      (error nil))
    (pop targets))
  ;; Finally, do it once with the real cursor
  (when targets (funcall function (car targets))))

(defun cursorfree-make-multi-cursor-action (function)
  "Translate FUNCTION into an instruction using multiple cursors.

FUNCTION will be applied on each element of the stack.  For each
target, a new cursor will be created."
  (lambda (environment)
    (let* ((e (cursorfree--clone-environment environment))
           (values (cursorfree-environment-value-stack e)))
      (setf (cursorfree-environment-value-stack e) nil)
      (cursorfree--multiple-cursors-do function values)
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

(cl-defstruct cursorfree--anonymous-target
  put get)

(cl-defstruct cursorfree--region-target
  content-region buffer)

(cl-defun cursorfree--make-target
    (content-region &key (constructor #'make-cursorfree--region-target))
  "Return a target spanning CONTENT-REGION.

CONSTRUCTOR specifies the constructor to use.  It is assumed that it
may be invoked equivalently to `make-cursorfree--region-target'."
  (let* ((region (cursorfree--markify-region content-region))
         (buffer (marker-buffer (car region))))
    (funcall constructor
     :content-region region
     :buffer buffer)))

(defun cursorfree--content-region (target)
  "Return region of the content referred to by TARGET."
  (cursorfree--region-target-content-region target))

(cl-defgeneric cursorfree--target-buffer (target)
  "Get the buffer associated with TARGET.

Defaults to the current buffer."
  (current-buffer))

(cl-defgeneric cursorfree--target-buffer ((target cursorfree--region-target))
  "Get the buffer associated with `cursorfree--region-target' TARGET."
  (cursorfree--region-target-buffer target))

(defun cursorfree--target-window (target)
  "Get the window associated with `cursorfree--region-target' TARGET."
  (get-buffer-window (cursorfree--target-buffer target)))

(defun cursorfree--on-content-region (region-target f)
  "Apply F to the content region of REGION-TARGET."
  (declare (indent defun))
  (unless (cursorfree--region-target-p region-target)
    (error (format "Type error: %s is not of type cursorfree--region-target." region-target)))
  (with-selected-window (cursorfree--target-window region-target)
    (with-current-buffer (cursorfree--target-buffer region-target)
      (let ((region (cursorfree--content-region region-target)))
        (funcall f region)))))

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

(cl-defgeneric cursorfree--target-get (target)
  "Return the content referred to by TARGET."
  (_ (error (format "No method for getting content of target %s" target))))

(cl-defmethod cursorfree--target-get ((target string))
  target)

(cl-defmethod cursorfree--target-get ((target integer))
  ;; TODO: Encode characters as singleton strings instead
  (string target))

(cl-defmethod cursorfree--target-get ((target cursorfree--anonymous-target))
  (funcall (cursorfree--anonymous-target-get target)))

(cl-defmethod cursorfree--target-get ((target cursorfree--region-target))
  (with-current-buffer (cursorfree--target-buffer target)
    (buffer-substring-no-properties (car (cursorfree--content-region target))
                                    (cdr (cursorfree--content-region target)))))

(cl-defgeneric cursorfree--target-put (target content)
  "Put CONTENT into TARGET."
  (_ (error (format "No method for writing content to target %s" target))))

(cl-defmethod cursorfree--target-put ((target cursorfree--anonymous-target) content)
  (funcall (cursorfree--anonymous-target-put target) content))

(cl-defmethod cursorfree--target-put ((target cursorfree--region-target) content)
  (with-current-buffer (cursorfree--target-buffer target)
    (cursorfree--region-delete (cursorfree--content-region target))
    (cursorfree--insert-at (car (cursorfree--content-region target)) content)))

;;;; End of core functions

;; TODO: Introduce region-target abstraction layer?

(defun cursorfree--deletion-region (target)
  "Return region that should be removed if deleting TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (cdr region))
        (cursorfree--markify-region
         (if (/= 0 (skip-chars-forward "[:space:]\n"))
             (cons (car region) (point))
           (goto-char (car region))
           (skip-chars-backward "[:space:]\n")
           (cons (point) (cdr region))))))))

(defun cursorfree--region-delete (region)
  "Delete REGION."
  (with-current-buffer (marker-buffer (car region))
    (delete-region (car region) (cdr region))))

(defun cursorfree-target-pulse (target)
  "Temporarily highlight TARGET."
  (when (cursorfree--region-target-p target)
    (cursorfree--on-content-region target
      (lambda (region)
        (pulse-momentary-highlight-region (car region) (cdr region))))))

(defun cursorfree--insert-at (marker string)
  "Insert STRING at MARKER."
  (save-excursion
    (set-buffer (marker-buffer marker))
    (goto-char marker)
    (insert string)
    (cursorfree-target-pulse (cons marker (+ marker (length string))))))

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
      (select-window (get-buffer-window (marker-buffer (car region)) 'visible))
      (goto-char (car region)))))

(defun cursorfree-target-jump-end (target)
  "Move point to end of TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (select-window (get-buffer-window (marker-buffer (car region)) 'visible))
      (goto-char (cdr region)))))

(defun cursorfree-target-indent (target)
  "Indent TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (indent-region (car region) (cdr region)))))

(defun cursorfree-target-copy (target)
  "Copy TARGET to kill ring."
  (cursorfree--target-put (cursorfree-kill-ring)
                          (cursorfree--target-get target))
  (cursorfree-target-pulse target))

(defun cursorfree-target-chuck (&rest targets)
  "Delete TARGETS and indent the resulting text."
  (dolist (target targets)
    (cursorfree--region-delete (cursorfree--deletion-region target))
    (cursorfree-target-indent (cursorfree-line target))))

(defmacro cursorfree--for-each-cursor (&rest body)
  "Evaluate BODY for each cursor."
  `(mc/for-each-cursor-ordered
    (mc/restore-state-from-overlay cursor)
    ,@body
    (mc/store-current-state-in-overlay cursor)))

(defun cursorfree-target-bring (source &rest targets)
  "Overwrite TARGETS with SOURCE.

If no targets are given, overwrite `cursorfree-this' instead."
  (setq targets (or targets (list (cursorfree-this))))
  (dolist (target targets)
    (cursorfree--target-put target (cursorfree--target-get source))
    (cursorfree-target-pulse target)))

(defun cursorfree-target-move (source &rest targets)
  "Overwrite TARGETS with SOURCE, then delete SOURCE.

If no targets are given, overwrite `cursorfree-this' instead."
  (setq targets (or targets (list (cursorfree-this))))
  (apply #'cursorfree-target-bring (cons source targets))
  (cursorfree-target-chuck source))

(defun cursorfree-target-swap (target1 target2)
  "Swap the contents of TARGET1 and TARGET2."
  (let ((string1 (cursorfree--target-get target1))
        (string2 (cursorfree--target-get target2)))
    (cursorfree--target-put target1 string2)
    (cursorfree--target-put target2 string1)))

(cl-defgeneric cursorfree--target-change (target))

(cl-defmethod cursorfree--target-change ((target cursorfree--region-target))
  (cursorfree--on-content-region target
    (lambda (region)
      (cursorfree--region-delete region)
      (goto-char (car region)))))

(defun cursorfree-target-change (&rest targets)
  "Move point to TARGET and delete its contents."
  (let (region-targets other-targets)
    (dolist (target targets)
      (if (cursorfree--region-target-p target)
          (push target region-targets)
        (push target other-targets)))
    (cursorfree--multiple-cursors-do #'cursorfree--target-change
                                     region-targets)
    (dolist (target other-targets)
      (cursorfree--target-change target))))

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
        (goto-char (car region))
        (recenter 0))
      (cursorfree--clamp-line))))

(defun cursorfree-target-center (target)
  "Scroll window so TARGET is in the center."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (recenter nil))
      (cursorfree--clamp-line)))
  (cursorfree--clamp-line))

(defun cursorfree-target-bottom (target)
  "Scroll window so TARGET is at the bottom."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (recenter -1))
      (cursorfree--clamp-line)))
  (cursorfree--clamp-line))

(defun cursorfree-target-drink (target)
  "Insert an empty line before TARGET and put point on it."
  (cursorfree-target-jump-beginning target)
  (beginning-of-line)
  (newline)
  (backward-char))

(defun cursorfree-target-pour (target)
  "Insert an empty line after TARGET and put point on it."
  (cursorfree-target-jump-end target)
  (end-of-line)
  (newline))

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
  (if (and (button-at (point)) (button-get (button-at (point)) 'action))
      (push-button)
    (if-let ((follow-action
              (alist-get major-mode cursorfree-dwim-follow-alist)))
        (funcall follow-action))))

(defun cursorfree-target-pick (target)
  "Try to follow the thing at TARGET.

This function calls on `cursorfree-dwim-follow' to attempt to
follow the thing at TARGET."
  (with-selected-window (cursorfree--target-window target)
    (let ((region (cursorfree--content-region target)))
      (cursorfree--on-content-region target
        (lambda (region)
          (goto-char (car region))
          (cursorfree-dwim-follow))))))

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
    (cursorfree-target-bring
     (cursorfree-inner-parenthesis-any target)
     (cursorfree-outer-parenthesis-any target)))

(defvar cursorfree-actions
  `(("select" . ,(cursorfree-make-multi-cursor-action #'cursorfree-target-select))
    ("copy" . ,(cursorfree-make-action #'cursorfree-target-copy))
    ("chuck" . ,(cursorfree-make-action #'cursorfree-target-chuck))
    ("bring" . ,(cursorfree-make-action #'cursorfree-target-bring))
    ("move" . ,(cursorfree-make-action #'cursorfree-target-move))
    ("swap" . ,(cursorfree-make-action #'cursorfree-target-swap))
    ("clone" . ,(cursorfree-make-action #'cursorfree-target-clone))
    ("jump" . ,(cursorfree-make-multi-cursor-action #'cursorfree-target-jump-beginning))
    ("pre" . ,(cursorfree-make-multi-cursor-action #'cursorfree-target-jump-beginning))
    ("post" . ,(cursorfree-make-multi-cursor-action #'cursorfree-target-jump-end))
    ("change" . ,(cursorfree-make-action #'cursorfree-target-change))
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
    ("break" . ,(cursorfree-make-action #'cursorfree-target-break))
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

(defun cursorfree-paint-left (&optional target)
  "Expand TARGET leftwards until the next whitespace."
  (setq target (or target (cursorfree-this)))
  (cursorfree--on-content-region target
    (lambda (region)
      (cursorfree--make-target
       (cons (cursorfree--skip-backward-from (car region) "^[:space:]\n")
             (cdr region))))))

(defun cursorfree-paint-right (&optional target)
  "Expand TARGET rightwards until the next whitespace."
  (setq target (or target (cursorfree-this)))
  (cursorfree--on-content-region target
    (lambda (region)
      (cursorfree--make-target
       (cons (car region)
             (cursorfree--skip-forward-from (cdr region) "^[:space:]\n"))))))

(defun cursorfree-paint (&optional target)
  "Expand TARGET leftwards and rightwards until the next whitespace."
  (setq target (or target (cursorfree-this)))
  (cursorfree-paint-right (cursorfree-paint-left target)))

(defun cursorfree-trim (&optional target)
  "Shrink TARGET until there is no whitespace to the left or right."
  (setq target (or target (cursorfree-this)))
  (let ((region (cursorfree--content-region target)))
    (cursorfree--make-target
     (cons (cursorfree--skip-forward-from (car region) "[:space:]\n")
           (cursorfree--skip-backward-from (cdr region) "[:space:]\n")))))

(defun cursorfree-inner-parenthesis (delimiter &optional target)
  "Expand TARGET to fill the insides of DELIMITER.

This function will match parentheses and quotation marks to the
left and right."
  (setq target (or target (cursorfree-this)))
  (save-excursion
    ;; evil-inner-double-quote uses the location of point for the
    ;; expansion.  Put point at the beginning of the region.
    (set-buffer (cursorfree--target-buffer target))
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

(defun cursorfree-outer-parenthesis (delimiter &optional target)
  "Expand TARGET to contain the closest DELIMITER.

This function will match parentheses and quotation marks to the
left and right."
  (setq target (or target (cursorfree-this)))
  (save-excursion
    ;; evil-outer-double-quote uses the location of point for the
    ;; expansion.  Put point at the beginning of the region.
    (set-buffer (cursorfree--target-buffer target))
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

(defun cursorfree-inner-parenthesis-any (&optional target)
  "Expand TARGET to fill the insides of the closest delimiters.

This function tries different parentheses and quotations to
figure out whichever is closest."
  (setq target (or target (cursorfree-this)))
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

(defun cursorfree-outer-parenthesis-any (&optional target)
  "Expand TARGET to contain the closest delimiters.

This function tries different parentheses and quotations to
figure out whichever is closest."
  (setq target (or target (cursorfree-this)))
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

(defun cursorfree-past (target1 &optional target2)
  "Return the smallest target that can fit TARGET1 and TARGET2."
  (setq target2 (or target2 (cursorfree-this)))
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
   (lambda (&optional target)
     (setq target (or target (cursorfree-this)))
     (with-current-buffer (cursorfree--target-buffer target)
       (cursorfree--make-target
        (cursorfree--bounds-of-thing-at thing
                                        (car (cursorfree--content-region target))))))))

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

(defun cursorfree-line-right (&optional target)
  "Extend TARGET to include the next newline."
  (setq target (or target (cursorfree-this)))
  (save-excursion
    (set-buffer (cursorfree--target-buffer target))
    (goto-char (cdr (cursorfree--content-region target)))
    (unless (search-forward "\n" nil t)
      (goto-char (point-max)))
    (cursorfree--make-target (cons (car (cursorfree--content-region target)) (point)))))

(defun cursorfree-line-left (&optional target)
  "Extend TARGET to start after the previous newline."
  (setq target (or target (cursorfree-this)))
  (save-excursion
    (set-buffer (cursorfree--target-buffer target))
    (goto-char (car (cursorfree--content-region target)))
    (if (search-backward "\n" nil t)
        (forward-char) ; Jump over the searched for newline
      (goto-char (point-min)))
    (cursorfree--make-target (cons (point) (cdr (cursorfree--content-region target))))))

(defun cursorfree-line (&optional target)
  "Extend TARGET to fill the full line."
  (setq target (or target (cursorfree-this)))
  (cursorfree-line-left (cursorfree-line-right target)))

(defun cursorfree-row (index)
  "Return the line with number INDEX as a target."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- index))
    (cursorfree-line (cursorfree--make-target (cons (point) (point))))))

(cl-defstruct (cursorfree--this-target (:include cursorfree--region-target)))

(cl-defmethod cursorfree--target-put ((target cursorfree--this-target) content)
  (with-current-buffer (cursorfree--target-buffer target)
    (insert content)))

(defun cursorfree-this ()
  "Return an empty region located at point."
  (cursorfree--make-target (cons (point) (point))
                           :constructor #'make-cursorfree--this-target))

(defun cursorfree-extend-right (target1 target2)
  "Return target extending TARGET2 to the end of TARGET1."
  (cursorfree--make-target
   (cons (car (cursorfree--make-target target2))
         (max (cdr (cursorfree--make-target target2))
              (cdr (cursorfree--make-target target1))))))

(defun cursorfree-every-instance (target &optional view)
  "Return a list of every occurrence of TARGET.

If target VIEW is given, only instances inside of it will be matched.
Otherwise, the full buffer is searched."
  (declare (cursorfree--reverse-argument-order))
  (with-current-buffer (cursorfree--target-buffer target)
    (setq view (or view (cursorfree-everything))))
  (with-current-buffer (cursorfree--target-buffer view)
    (let ((search-string (cursorfree--target-get target))
          (matches '()))
      (unless (equal search-string "")
        (save-excursion
          (goto-char (car (cursorfree--content-region view)))
          (while (search-forward search-string (cdr (cursorfree--content-region view)) t)
            (push (cursorfree--make-target (cons (match-beginning 0) (match-end 0)))
                  matches))))
      (nreverse matches))))

(defun cursorfree-dup (environment)
  "Duplicate the top value in the value stack of ENVIRONMENT."
  (cursorfree--push-value-pure environment
    (cursorfree--peek-value environment)))

(cl-defstruct cursorfree--kill-ring-target)

(cl-defmethod cursorfree--target-get ((target cursorfree--kill-ring-target))
  "Return current kill."
  (current-kill 0 nil))

(cl-defmethod cursorfree--target-put ((target cursorfree--kill-ring-target) content)
  "Add CONTENT to the kill ring."
  (kill-new content))

(defun cursorfree-kill-ring ()
  "Return the kill ring as a target."
  (make-cursorfree--kill-ring-target))

(cl-defgeneric cursorfree-next (target)
  "Get next occurrence of TARGET."
  (save-excursion
    (search-forward (cursorfree--target-get target))
    (cursorfree--make-target (cons (match-beginning 0) (match-end 0)))))

(cl-defmethod cursorfree-next ((target cursorfree--region-target))
  (with-current-buffer (cursorfree--target-buffer target)
    (save-excursion
      (goto-char (cdr (cursorfree--content-region target)))
      (search-forward (cursorfree--target-get target))
      (cursorfree--make-target (cons (match-beginning 0) (match-end 0))))))

(cl-defgeneric cursorfree-previous (target)
  "Get previous occurrence of TARGET."
  (save-excursion
    (search-backward (cursorfree--target-get target))
    (cursorfree--make-target (cons (match-beginning 0) (match-end 0)))))

(cl-defmethod cursorfree-previous ((target cursorfree--region-target))
  (with-current-buffer (cursorfree--target-buffer target)
    (save-excursion
      (goto-char (car (cursorfree--content-region target)))
      (search-backward (cursorfree--target-get target))
      (cursorfree--make-target (cons (match-beginning 0) (match-end 0))))))

(defun cursorfree-filter (filter &rest region-targets)
  "Return the REGION-TARGETS that are fully located inside FILTER."
  (let ((filter-region (cursorfree--content-region filter)))
    (seq-filter (lambda (target)
                  (let ((region (cursorfree--content-region target)))
                    (and (<= (car filter-region) (car region))
                         (>= (cdr filter-region) (cdr region)))))
                region-targets)))

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
    ("tail" . ,(cursorfree-make-modifier #'cursorfree-line-right))
    ("head" . ,(cursorfree-make-modifier #'cursorfree-line-left))
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
    ("dupe" . cursorfree-dup)
    ("filter" . ,(cursorfree-make-flattening-modifier #'cursorfree-filter))
    ("clip" . ,(cursorfree-make-modifier #'cursorfree-kill-ring))
    ("next" . ,(cursorfree-make-modifier #'cursorfree-next))
    ("preve" . ,(cursorfree-make-modifier #'cursorfree-previous))))

;;; cursorfree.el ends soon
(provide 'cursorfree)
;;; cursorfree.el ends here
