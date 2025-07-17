;;; cursorfree.el --- Edit and navigate through hats -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.2.0
;; Homepage: https://github.com/ErikPrantare/cursorfree.el
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

(defun cursorfree--reverse-argument-order (function)
  "Mark FUNCTION for reverse reading order.

This will reverse the order in which `cursorfree--apply' applies
arguments.

This function may be used as part of a `declare' form as follows:

  (declare (cursorfree--reverse-argument-order))"
  (put function 'cursorfree--reverse-argument-order t))

(setf (alist-get 'cursorfree--reverse-argument-order defun-declarations-alist)
      (list (lambda (function args)
              `(cursorfree--reverse-argument-order #',function))))

(cl-defun cursorfree--optional-bag-get-match (arglist spec argument)
  (seq-doseq (entry spec)
    (when-let ((matched-argument
                (funcall
                 (eval `(lambda (%) (when ,(car entry) ',(cadr entry))))
                 argument)))
      (cl-return-from cursorfree--optional-bag-get-match
        (cons (seq-position
               (byte-compile-arglist-vars arglist)
               matched-argument)
              entry))))
  nil)

(defun cursorfree--optional-bag (function arglist &rest spec)
  `(progn
     (put #',function 'cursorfree--arglist ',arglist)
     (put #',function 'cursorfree--optional-bag-spec ',spec)))

(setf (alist-get 'cursorfree--optional-bag defun-declarations-alist)
      (list #'cursorfree--optional-bag))

(defun cursorfree--apply (function args)
  "Call FUNCTION with ARGS as arguments.

If function has been marked with `cursorfree--reverse-argument-order',
reverse that order of ARGS."
  (when (and (symbolp function)
             (get function 'cursorfree--reverse-argument-order))
    (setq args (reverse args)))
  (apply function args))

(defun cursorfree--densify-alist (indexed-args)
  (when-let ((max-index (caar (seq-sort-by #'car #'> indexed-args))))
    (let ((argument-list (make-list (1+ max-index) nil)))
      (dolist (indexed-arg indexed-args)
        (setf (seq-elt argument-list (car indexed-arg)) (cdr indexed-arg)))
      argument-list)))

(defun cursorfree--get-positional-indices (function args)
  (when (and (symbolp function)
             (get function 'cursorfree--reverse-argument-order))
    (setq args (reverse args)))
  (seq-map-indexed (lambda (arg index) (cons index arg)) args))

(defun cursorfree--apply-on-stack (function stack)
  "Apply FUNCTION to the top elements of STACK.
Returns the unapplied elements of STACK with the return value of
FUNCTION on top.

The arity of FUNCTION is read from the cdr of `func-arity'.  The
function is evaluated with the top values of STACK, with the top
elements applied as the first arguments.  &rest arguments are
supported."
  (let* ((optional-bag-spec
         (when (symbolp function)
           (copy-sequence (get function 'cursorfree--optional-bag-spec))))
         (optional-bag-p (when optional-bag-spec t))
         (arg-map '()))
    (when optional-bag-spec
      (while-let ((match (and (seq-first stack)
                              (seq-first optional-bag-spec)
                              (cursorfree--optional-bag-get-match
                               (get function 'cursorfree--arglist)
                               optional-bag-spec
                               (seq-first stack)))))
        (push (cons (car match) (pop stack)) arg-map)
        ;; Remove matching entry, so we don't match on it again
        (setq optional-bag-spec
              (delete (cdr match) optional-bag-spec))))

    (let* ((arity (if optional-bag-p
                      (car (func-arity function))
                    (cdr (func-arity function))))
           (args (if (eq arity 'many) stack (take arity stack)))
           (tail (if (eq arity 'many) '() (nthcdr arity stack))))
      (setq arg-map (append arg-map (cursorfree--get-positional-indices function args)))
      (cons (apply function (cursorfree--densify-alist arg-map)) tail))))

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
  "Apply FUNCTION to each target in TARGETS.
Create a new cursor each time.

Each target is assumed to be in the same buffer.

If invoking FUNCTION causes an error, no cursor is created."
  (when targets
    (cursorfree--on-content-region (car targets)
      ;; We do not actually use region, we only invoke the above function
      ;; to ensure that everything is performed in the correct context.
      (lambda (region)
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
        (funcall function (car targets))))))

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

(defun cursorfree--ensure-marker-region (region)
  "Return REGION, with the endpoints turned into markers as needed."
  (unless (consp region)
    (error "Invalid argument %s in cursorfree--ensure-marker-region" region))
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
        (cursorfree--ensure-marker-region bounds))))

(cl-defstruct cursorfree--region-target
  "Target referring to CONTENT-REGION inside of BUFFER.
CONTENT-REGION is a cons cell of markers."
  content-region buffer deletion-region)

(cl-defstruct cursorfree--parallel-target
  targets)

(defun cursorfree--normalize-target (target)
  "Turn TARGET into a parallel if it is a non-singleton sequence.

If TARGET is not a sequence or is a sequence with a single element,
return that element.  Otherwise, return the targets of the sequence as
a `cursorfree--parallel-target'."
  (cond
   ((not (seqp target)) target)
   ((length= target 1) (seq-first target))
   (t (make-cursorfree--parallel-target :targets (seq-into target 'list)))))

(cl-defun cursorfree--make-target (content-region
                                   &key
                                   deletion-region
                                   (constructor #'make-cursorfree--region-target))
  "Return a target spanning CONTENT-REGION in the current buffer.

DELETION-REGION specified the region to remove if this target is
deleted.  If nil, the region will be guessed.

CONSTRUCTOR specifies the constructor to use.  It is assumed that it
may be invoked equivalently to `make-cursorfree--region-target', and
constructs a target inheriting from `cursorfree--region-target'."
   (let* ((region (cursorfree--ensure-marker-region content-region))
         (buffer (marker-buffer (car region)))
         (deletion (cursorfree--ensure-marker-region
                    (or deletion-region
                        (save-excursion
                          (set-buffer buffer)
                          (goto-char (cdr content-region))
                          (cursorfree--ensure-marker-region
                           (if (/= 0 (skip-chars-forward "[:space:]\n"))
                               (cons (car content-region) (point))
                             (goto-char (car content-region))
                             (skip-chars-backward "[:space:]\n")
                             (cons (point) (cdr content-region)))))))))
    (funcall constructor
             :content-region region
             :buffer buffer
             :deletion-region deletion)))

(defun cursorfree--content-region (target)
  "Return region of the content referred to by TARGET."
  (cursorfree--region-target-content-region target))

(defun cursorfree-target-buffer (&optional target)
  "Get the buffer associated with TARGET.

If TARGET is nil or omitted, default to the target returned by
`cursorfree-this'.

To override this function for new target types, implement a method for
`cursorfree--target-buffer'."
  (cursorfree--target-buffer (or target (cursorfree-this))))

(cl-defgeneric cursorfree--target-buffer (target)
  "Get the buffer associated with TARGET.

Defaults to the current buffer."
  (current-buffer))

(cl-defmethod cursorfree--target-buffer ((target cursorfree--region-target))
  "Get the buffer associated with `cursorfree--region-target' TARGET."
  (cursorfree--region-target-buffer target))

(cl-defmethod cursorfree--target-buffer ((window window))
  "Get the buffer of WINDOW."
  (window-buffer window))

(cl-defmethod cursorfree--target-buffer ((buffer buffer))
  "Return BUFFER."
  buffer)

(defun cursorfree--target-window (target)
  "Get the window showing the buffer of TARGET.
If no window shows the buffer of TARGET, return nil."
  (get-buffer-window (cursorfree--target-buffer target)))

(cl-defgeneric cursorfree--on-content-region (target f)
  (declare (indent defun))
  (error (format "Type error: %s has no content region" target)))

(cl-defmethod cursorfree--on-content-region ((target cursorfree--region-target) f)
  "Apply F to the content region of TARGET."
  (with-selected-window (window-normalize-window (cursorfree--target-window target))
    (with-current-buffer (cursorfree--target-buffer target)
      (let ((region (cursorfree--content-region target)))
        (funcall f region)))))

(cl-defmethod cursorfree--on-content-region ((target cursorfree--parallel-target) f)
  (let ((result '()))
    (seq-doseq (target (cursorfree--parallel-target-targets target))
      (push (cursorfree--on-content-region target f)
            result))
   ;;HACK: Always wrap the results in a parallel-target.  In this way,
   ;;if f returns a target, we will automatically promote the result
   ;;into a parallel target.
    (make-cursorfree--parallel-target
     :targets (nreverse result))))

(cl-defgeneric cursorfree--on-content-region-cursor-effect (target f)
  (declare (indent defun))
  (error (format "Type error: %s has no content region" target)))

(cl-defmethod cursorfree--on-content-region-cursor-effect ((target cursorfree--region-target) f)
  (cursorfree--on-content-region target f))

(cl-defmethod cursorfree--on-content-region-cursor-effect ((target cursorfree--parallel-target) f)
  ;; For now, assume that the parallel target is just a parallel of
  ;; region targets
  (cursorfree--multiple-cursors-do
   (lambda (target)
     (cursorfree--on-content-region target
       (lambda (region)
         (funcall f region))))
   (cursorfree--parallel-target-targets target)))

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
  (error (format "No method for getting content of target %s" target)))

(cl-defmethod cursorfree--target-get ((target string))
  "Return TARGET."
  target)

(cl-defmethod cursorfree--target-get ((target integer))
  "Convert TARGET to a string of length one."
  ;; TODO: Encode characters as singleton strings instead
  (string target))

(cl-defmethod cursorfree--target-get ((target window))
  "Return TARGET."
  target)

(cl-defmethod cursorfree--target-get ((target buffer))
  "Return TARGET."
  target)

(cl-defmethod cursorfree--target-get ((target cursorfree--region-target))
  "Return the buffer substring of TARGET."
  (with-current-buffer (cursorfree--target-buffer target)
    (buffer-substring-no-properties (car (cursorfree--content-region target))
                                    (cdr (cursorfree--content-region target)))))

(cl-defgeneric cursorfree--target-put (target content)
  "Put CONTENT into TARGET."
  (error (format "No method for writing %S to target %S" content target)))

(cl-defmethod cursorfree--target-put ((target window) (content buffer))
  (set-window-buffer target content))

(cl-defmethod cursorfree--target-put ((target window) (content window))
  (cursorfree--target-put target (window-buffer content)))

(cl-defmethod cursorfree--target-put ((target cursorfree--region-target) (content string))
  "Remove region of TARGET and insert CONTENT.

TARGET will be modified to cover the region containing CONTENT."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (insert content)
        ;; We are careful to make sure TARGET still refers to the
        ;; region with the new content after insertion.  We delete
        ;; after we insert so that markers occurring directly after
        ;; TARGET don't end up before the inserted content.
        (if (= (car region) (cdr region))
            ;; If the region is empty, we need to manually move the
            ;; end of the target region.  We do not need to delete the
            ;; empty target.
            (move-marker (cdr region) (+ (cdr region) (length content)))
          ;; Otherwise, the end marker of the region will be moved but
          ;; the beginning will not, so we have to compensate here.
          (delete-region (+ (car region) (length content)) (cdr region)))))))

(cl-defmethod cursorfree--target-put ((target cursorfree--parallel-target) content)
  (seq-doseq (target (cursorfree--parallel-target-targets target))
    (cursorfree--target-put target content)))

(cl-defstruct (cursorfree--this-target (:include cursorfree--region-target))
  "Target indicating the \"the currently active thing\".  The meaning
of this is generally context dependent.  For example, when dealing
with regions, it denotes point, but when dealing with windows, it
denotes the currently selected window.

Generic functions may be overridden to provide specialized behavior
for \"this\".")

(defun cursorfree-this ()
  "Return an empty region located at point.

The returned target this of type `cursorfree--this-target'.  Generic
functions can be overloaded on this type to give more
context-dependent behavior for whatever \"this\" means."
  (cursorfree--make-target
   (cons (point) (point))
   :deletion-region (cons (point) (point))
   :constructor #'make-cursorfree--this-target))

(cl-defmethod cursorfree--target-put ((target cursorfree--this-target) (content string))
  "Insert CONTENT at point in the buffer of TARGET."
  ;; Insert as if usual region target
  (cl-call-next-method)
  ;; Put point after the inserted text, given that point actually was
  ;; located at the corresponding region.
  (cursorfree--on-content-region target
    (lambda (region)
      (when (= (point) (car region))
        (goto-char (cdr region))))))

(cl-defmethod cursorfree--target-put ((target cursorfree--this-target) (content buffer))
  (cursorfree--target-put (cursorfree--target-window target) content))

(cl-defmethod cursorfree--target-put ((target cursorfree--this-target) (content window))
  (cursorfree--target-put target (window-buffer content)))

;;;; End of core functions

;; TODO: Introduce region-target abstraction layer?

(defvar cursorfree--target-that nil
  "The target of the last operation.")
(defvar cursorfree--target-source nil
  "The source target of the last operation.")

(defun cursorfree--deletion-region (target)
  "Return region that should be removed if deleting TARGET."
  (cursorfree--region-target-deletion-region target))

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
  (cursorfree--on-content-region-cursor-effect target
    (lambda (region)
      (set-mark (car region))
      (goto-char (cdr region)))))

;; TODO: Take multiple targets here instead of when transforming to
;; modifier later.
(defun cursorfree-target-jump (target)
  "Go to TARGET."
  (cursorfree--target-jump target))

(cl-defgeneric cursorfree--target-jump (target)
  "Jump to target.

The meaning of \"jump\" is left ambiguous to allow targets of
different types to be jumped to.  See the implemented methods for
examples."
  (error (format "No method for jumping to %s" target)))

(cl-defmethod cursorfree--target-jump ((target cursorfree--region-target))
  "Move point to beginning of TARGET.
If a window displays the buffer of TARGET, select it."
  (cursorfree--on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (car region)))))

;; TODO rethink how best to eliminate duplicated code
;; w.r.t. cursorfree--region-target.  Same for
;; cursorfree--target-jump-end.
(cl-defmethod cursorfree--target-jump ((target cursorfree--parallel-target))
  (cursorfree--on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (car region)))))

(cl-defmethod cursorfree--target-jump ((target window))
  (select-window target))

(cl-defgeneric cursorfree--target-jump-beginning (target)
  (cursorfree--target-jump target))

(defun cursorfree-target-jump-beginning (target)
  (cursorfree--target-jump-beginning target))

(cl-defgeneric cursorfree--target-jump-end (target)
  (cursorfree--target-jump target))

(cl-defmethod cursorfree--target-jump-end ((target cursorfree--region-target))
  "Move point to end of TARGET.
If a window displays the buffer of TARGET, select it."
  (cursorfree--on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (cdr region)))))

(cl-defmethod cursorfree--target-jump-end ((target cursorfree--parallel-target))
  (cursorfree--on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (cdr region)))))

(defun cursorfree-target-jump-end (target)
  (cursorfree--target-jump-end target))

(defun cursorfree-target-indent (target)
  "Indent TARGET."
  (cursorfree--on-content-region-cursor-effect target
    (lambda (region)
      (indent-region (car region) (cdr region)))))

(defun cursorfree-target-copy (target)
  "Copy TARGET to kill ring."
  (cursorfree-target-bring target (cursorfree-kill-ring))
  (setq cursorfree--target-that target)
  (cursorfree-target-pulse target))

(cl-defgeneric cursorfree-target-delete (target)
  "Delete TARGET."
  (error (format "No method for deleting target %s" target)))

(cl-defmethod cursorfree-target-delete ((target cursorfree--region-target))
  (cursorfree--region-delete (cursorfree--deletion-region target))
  (cursorfree-target-indent (cursorfree-line target)))

(cl-defmethod cursorfree-target-delete ((targets cursorfree--parallel-target))
  (seq-doseq (target (cursorfree--parallel-target-targets targets))
    (cursorfree-target-delete target)))

(cl-defmethod cursorfree-target-delete ((target window))
  (delete-window target))

(defun cursorfree-target-chuck (&rest targets)
  "Delete TARGETS and indent the resulting text."
  (dolist (target targets)
    (cursorfree-target-delete target)))

(defmacro cursorfree--for-each-cursor (&rest body)
  "Evaluate BODY for each cursor."
  `(mc/for-each-cursor-ordered
    (mc/restore-state-from-overlay cursor)
    ,@body
    (mc/store-current-state-in-overlay cursor)))

(defun cursorfree-target-bring (source &rest targets)
  "Overwrite TARGETS with SOURCE.

If no targets are given, overwrite `cursorfree-this' instead."
  (let ((target (cursorfree--normalize-target (or targets (cursorfree-this)))))
    (cursorfree--target-put target (cursorfree--target-get source))
    (cursorfree-target-pulse target)
    (setq cursorfree--target-that target)
    (setq cursorfree--target-source source)))

(cl-defgeneric cursorfree--target-move (source target)
  (cursorfree-target-bring source target)
  (cursorfree-target-delete source))

(cl-defmethod cursorfree--target-move ((source window) target)
  (cursorfree--target-put target source)
  (with-selected-window source
    (previous-buffer)))

(defun cursorfree-target-move (source &rest targets)
  "Overwrite TARGETS with SOURCE, then delete SOURCE.

If no targets are given, overwrite `cursorfree-this' instead."
  (setq targets (or targets (list (cursorfree-this))))
  (dolist (target targets)
    (cursorfree--target-move source target)))

(defun cursorfree-target-swap (target1 target2)
  "Swap the contents of TARGET1 and TARGET2."
  (let ((string1 (cursorfree--target-get target1))
        (string2 (cursorfree--target-get target2)))
    (cursorfree--target-put target1 string2)
    (cursorfree--target-put target2 string1)))

(cl-defgeneric cursorfree--target-change (target)
  "Change TARGET interactively.")

(cl-defmethod cursorfree--target-change ((target cursorfree--region-target))
  "Remove contents of TARGET and put point there."
  (cursorfree--on-content-region target
    (lambda (region)
      (cursorfree--region-delete region)
      (goto-char (car region)))))

(cl-defmethod cursorfree--target-change ((target cursorfree--parallel-target))
  "Remove contents of TARGET and put points there."
  (cursorfree--multiple-cursors-do
   #'cursorfree--target-change
   (cursorfree--parallel-target-targets target)))

(defun cursorfree-target-change (&rest targets)
  "Move point to TARGETS and delete its contents."
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
  "Define function with NAME applying FUNCTION on targets.
Use DOCSTRING for the new function.

For each argument, the defined function NAME invokes FUNCTION on the
content region.  Afterwards, the region will be pulsed."
  (declare (indent defun))
  `(defun ,name (&rest targets)
     ,docstring
     (dolist (target targets)
       (cursorfree--on-content-region target
         (lambda (region)
           (,function (car region) (cdr region))))
       (cursorfree-target-pulse target))))

(cursorfree--simple-content-function cursorfree-target-comment
  "Comment out TARGETS."
  comment-region)

(cursorfree--simple-content-function cursorfree-target-uncomment
  "Uncomment TARGETS."
  uncomment-region)

(cursorfree--simple-content-function cursorfree-target-narrow
  "Narrow region to the last element of TARGETS."
  narrow-to-region)

(cursorfree--simple-content-function cursorfree-target-fill
  "Fill the paragraphs in TARGETS."
  fill-region)

(cursorfree--simple-content-function cursorfree-target-capitalize
  "Capitalize the first character of each word in TARGETS."
  capitalize-region)

(cursorfree--simple-content-function cursorfree-target-upcase
  "Convert TARGETS to upper case."
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

(defun cursorfree-target-crown (&optional target)
  "Scroll window so TARGET is at the top."
  (setq target (or target (cursorfree-this)))
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (recenter 0))
      (cursorfree--clamp-line))))

(defun cursorfree-target-center (&optional target)
  "Scroll window so TARGET is in the center."
  (setq target (or target (cursorfree-this)))
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (recenter nil))
      (cursorfree--clamp-line)))
  (cursorfree--clamp-line))

(defun cursorfree-target-bottom (&optional target)
  "Scroll window so TARGET is at the bottom."
  (setq target (or target (cursorfree-this)))
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (recenter -1))
      (cursorfree--clamp-line)))
  (cursorfree--clamp-line))

(defun cursorfree-target-drink (&optional target)
  "Insert an empty line before TARGET and put point on it."
  (setq target (or target (cursorfree-this)))
  (cursorfree--on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (car region))
      (beginning-of-line)
      ;; If this function is invoked in the context of a saved
      ;; excursion, and point is at the beginning of line, not
      ;; inserting before markers would put the saved point at the
      ;; new, preceeding, line.
      (insert-before-markers "\n")
      (backward-char))))

(defun cursorfree-target-pour (&optional target)
  "Insert an empty line after TARGET and put point on it."
  (setq target (or target (cursorfree-this)))
  (cursorfree--on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (cdr region))
      (end-of-line)
      (newline))))

(defun cursorfree-target-drop (&optional target)
  (save-excursion
    (cursorfree-target-drink target)))

(defun cursorfree-target-float (&optional target)
  (save-excursion
    (cursorfree-target-pour target)))

(defun cursorfree-target-puff (&optional target)
   (cursorfree-target-float target)
   (cursorfree-target-drop target))

(defun cursorfree-target-wrap (parenthesis &rest targets)
  "Wrap TARGETS with characters specified by PARENTHESIS.

Insert PARENTHESIS before TARGETS.  If PARENTHESIS is some type of
parenthesis, insert the matching right version at the end of TARGETS.
Otherwise, insert PARENTHESIS instead."
  (dolist (target targets)
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
             (_ parenthesis))))))))

(make-obsolete 'cursorfree-target-wrap-parenthesis
               'cursorfree-target-wrap
               "0.2.0")

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
                     (eww-follow-link))))
    ;; TODO: Bug report for making this mode part of the public API
    (xref--xref-buffer-mode . xref-goto-xref))
  "Alist mapping major mode to function for following at point.

Used in `cursorfree-dwim-follow' for determining how to follow
whatever thing point is located on.")

(defun cursorfree-dwim-follow ()
  "Try to follow the thing at point.
If point is at a button, push it.  Otherwise, use the current major
mode to look up the function in `cursorfree-dwim-follow-alist'."
  ;; The extra check that the button contains an action was used for
  ;; eww.  Check if this was fixed in Emacs 30.
  (if (and (button-at (point)) (button-get (button-at (point)) 'action))
      (push-button)
    (if-let ((follow-action
              (alist-get major-mode cursorfree-dwim-follow-alist)))
        (funcall follow-action))))

(defun cursorfree-target-pick (&optional target)
  "Try to follow the thing at TARGET.

This function calls on `cursorfree-dwim-follow' to attempt to
follow the thing at TARGET."
  (setq target (or target (cursorfree-this)))
  (with-selected-window (cursorfree--target-window target)
    (let ((region (cursorfree--content-region target)))
      (cursorfree--on-content-region target
        (lambda (region)
          (goto-char (car region))
          (cursorfree-dwim-follow))))))

;; TODO: Errors on invocation?
(defun cursorfree-target-fuse (target)
  "Remove all whitespace within TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (replace-regexp (rx (or whitespace "\n")) ""
                        nil (car region) (cdr region))))))

(defun cursorfree-target-join (target)
  "Join TARGET into one line."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (join-line nil (car region) (cdr region))))))

(defun cursorfree-target-break (target)
  "Insert newline before TARGET."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (newline)
        (indent-region (car region) (cdr region))))))

(defun cursorfree-target-help (target)
  "Run `display-local-help' at the start of TARGET.

This may, for example, be used for displaying warning from eglot."
  (cursorfree--on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (display-local-help)
        (cursorfree-target-pulse region)))))

(defun cursorfree-target-occur (target &optional window-or-buffer)
  "List occurrences of TARGET in WINDOW-OR-BUFFER.

If WINDOW-OR-BUFFER is a buffer, look for occurrences in that buffer.
If it is a window, look in the buffer current to that window.  If it
is nil or omitted, look instead in the buffer of TARGET.  If TARGET
has no associated buffer, use the current buffer instead."
  (declare (cursorfree--optional-bag ((or (windowp %) (bufferp %)) window-or-buffer)))
  (with-current-buffer (cond
                        ((windowp window-or-buffer) (window-buffer window-or-buffer))
                        ((bufferp window-or-buffer) window-or-buffer)
                        (t (window-normalize-buffer (cursorfree--target-buffer target))))
    (occur (rx (literal (cursorfree--target-get target))))))

(defun cursorfree-target-unwrap-parentheses (target)
  "Remove parentheses or quotation around TARGET."
  (cursorfree-target-bring
   (cursorfree-inner-parenthesis-dwim target)
   (cursorfree-outer-parenthesis-dwim target)))

(defvar cursorfree-actions
  `(("select" . ,(cursorfree-make-action #'cursorfree-target-select))
    ("copy" . ,(cursorfree-make-action #'cursorfree-target-copy))
    ("chuck" . ,(cursorfree-make-action #'cursorfree-target-chuck))
    ("bring" . ,(cursorfree-make-action #'cursorfree-target-bring))
    ("move" . ,(cursorfree-make-action #'cursorfree-target-move))
    ("swap" . ,(cursorfree-make-action #'cursorfree-target-swap))
    ("clone" . ,(cursorfree-make-action #'cursorfree-target-clone))
    ("jump" . ,(cursorfree-make-action #'cursorfree-target-jump))
    ("pre" . ,(cursorfree-make-action #'cursorfree-target-jump-beginning))
    ("post" . ,(cursorfree-make-action #'cursorfree-target-jump-end))
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
    ("drop" . ,(cursorfree-make-action #'cursorfree-target-drop))
    ("float" . ,(cursorfree-make-action #'cursorfree-target-float))
    ("puff" . ,(cursorfree-make-action #'cursorfree-target-puff))
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

(defun cursorfree--trim-left (target)
  (cursorfree--on-content-region target
    (lambda (region)
      (cursorfree--make-target
       (cons (cursorfree--skip-forward-from (car region) "[:space:]\n")
             (cdr region))))))

(defun cursorfree--trim-right (target)
  (cursorfree--on-content-region target
    (lambda (region)
      (cursorfree--make-target
       (cons (car region)
             (cursorfree--skip-backward-from (cdr region) "[:space:]\n"))))))

(defun cursorfree-trim (&optional target)
  "Shrink TARGET until there is no whitespace to the left or right."
  (setq target (or target (cursorfree-this)))
  (cursorfree--trim-left (cursorfree--trim-right target)))

(defun cursorfree--inner-parenthesis (region delimiter)
  "Expand REGION to fill the insides of DELIMITER.

This function will match parentheses and quotation marks to the
left and right."
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
              (?\' #'evil-inner-single-quote)
              (?\` #'evil-inner-back-quote)))))
      (cons (car expanded) (cadr expanded)))))

(defun cursorfree--outer-parenthesis (region delimiter)
  "Expand REGION to contain the closest DELIMITER.

This function will match parentheses and quotation marks to the
left and right."
  (save-excursion
    ;; evil-outer-double-quote uses the location of point for the
    ;; expansion.  Put point at the beginning of the region.
    (goto-char (car region))
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
      (cons (car expanded) (cadr expanded)))))

(defun cursorfree--parenthesis-expansion-impl (target parenthesis expansion-function)
  "Common interface for expanding to inner and outer parentheses"
  (setq target (or target (cursorfree-this)))
  (cursorfree--on-content-region target
    (lambda (region)
      (thread-last
        (ensure-list (or parenthesis '(?< ?{ ?\( ?\[ ?\" ?\' ?\`)))
        (seq-keep (lambda (parenthesis)
                    (condition-case nil
                        (funcall expansion-function region parenthesis)
                      (error nil))))
        ;; Filter out whenever the evil-inner-*-quote messes up the
        ;; region (it selects the next region if not currently in a
        ;; quote)
        (seq-filter (lambda (expanded)
                      (<= (car expanded) (car region))))
        ;; Pick the result with the tightest bounds
        (seq-sort-by #'car #'>)
        car
        cursorfree--make-target))))

(defun cursorfree-inner-parenthesis-dwim (&optional target parenthesis)
  "Expand TARGET to fill the insides of PARENTHESIS.

TARGET defaults to the target returned by `cursorfree-this'.

If PARENTHESIS is given, expand target until it reaches corresponding
matching parentheses on both sides.  Otherwise, try to guess which
parenthesis is intended."
  (declare (cursorfree--optional-bag
            ((characterp %) parenthesis)
            ((or (cursorfree--region-target-p %)
                 (cursorfree--parallel-target-p %))
             target)))
  (cursorfree--parenthesis-expansion-impl
   target
   parenthesis
   #'cursorfree--inner-parenthesis))

(defun cursorfree-outer-parenthesis-dwim (&optional target parenthesis)
  "Expand TARGET to contain enclosing PARENTHESIS.

TARGET defaults to the target returned by `cursorfree-this'.

If PARENTHESIS is given, expand target until it reaches corresponding
matching parentheses on both sides.  Otherwise, try to guess which
parenthesis is intended."
  (declare (cursorfree--optional-bag
            ((characterp %) parenthesis)
            ((or (cursorfree--region-target-p %)
                 (cursorfree--parallel-target-p %))
             target)))
  (cursorfree--parenthesis-expansion-impl
   target
   parenthesis
   #'cursorfree--outer-parenthesis))

(defun cursorfree--targets-hull (&rest targets)
  "Return the smallest target that can fit all TARGETS."
  (when targets
    ;; Make sure target gets created in correct buffer (max and min do
    ;; not return the corresponding marker, but a new integer instead)
    (with-current-buffer (cursorfree--target-buffer (car targets))
      (cursorfree--make-target
       (cons (apply #'min (mapcar (lambda (target) (car (cursorfree--content-region target)))
                                  targets))
             (apply #'max (mapcar (lambda (target) (cdr (cursorfree--content-region target)))
                                  targets)))
       :deletion-region
       (cons (apply #'min (mapcar (lambda (target) (car (cursorfree--deletion-region target)))
                                  targets))
             (apply #'max (mapcar (lambda (target) (cdr (cursorfree--deletion-region target)))
                                  targets)))))))

(defun cursorfree-past (target1 &optional target2)
  "Return the smallest target that can fit TARGET1 and TARGET2."
  (setq target2 (or target2
                    (with-current-buffer (cursorfree--target-buffer target1)
                      (cursorfree-this))))
  (cursorfree--targets-hull target1 target2))

(defun cursorfree-current-selection ()
  "Return the active region as a target."
  ;; TODO: Handle noncontiguous selections?
  (cursorfree--make-target
   (car (region-bounds))))

(defun cursorfree-thing-to-modifier (thing)
  "Translate THING to an instruction extending a target to THING.

The extension is done from the beginning of the target.  See
`bounds-of-thing-at-point' for more information about the builtin
`thing-at-point' functionalities."
  (cursorfree-make-modifier
   (lambda (&optional target)
     (setq target (or target (cursorfree-this)))
     (with-current-buffer (cursorfree--target-buffer target)
       (cursorfree--make-target
        (cursorfree--bounds-of-thing-at thing
                                        (car (cursorfree--content-region target))))))))

(defun cursorfree-everything (&optional window-or-buffer)
  "Return a target referring to the full content of the buffer.

This function respects narrowing."
  (declare (cursorfree--optional-bag
            ((or (bufferp %) (windowp %)) window-or-buffer)))
  (let ((in-buffer (cond
                    ((windowp window-or-buffer) (window-buffer window-or-buffer))
                    ((bufferp window-or-buffer) window-or-buffer)
                    (t (current-buffer)))))
    (with-current-buffer in-buffer
      (cursorfree--make-target
       (cons (point-min) (point-max))))))

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
    (let ((line-right (cursorfree--targets-hull target (cursorfree-this))))
      (cursorfree--targets-hull target (cursorfree--trim-right line-right)))))

(defun cursorfree-line-left (&optional target)
  "Extend TARGET to start after the previous newline."
  (setq target (or target (cursorfree-this)))
  (save-excursion
    (set-buffer (cursorfree--target-buffer target))
    (goto-char (car (cursorfree--content-region target)))
    (unless (search-backward "\n" nil t)
      (goto-char (point-min)))
    (let ((line-left (cursorfree--targets-hull target (cursorfree-this))))
      (cursorfree--targets-hull target (cursorfree--trim-left line-left)))))

(defun cursorfree-line (&optional target)
  "Extend TARGET to fill the full line."
  (setq target (or target (cursorfree-this)))
  (cursorfree-line-left (cursorfree-line-right target)))

(defun cursorfree-block (&optional target)
  (setq target (or target (cursorfree-this)))
  (save-excursion
    (cursorfree--on-content-region target
      (lambda (region)
        (goto-char (car region))
        (unless (re-search-backward "\n[:blank:]*\n" nil t)
          (goto-char (point-min)))
        (skip-chars-forward  "\n[:blank:]")
        (let ((start (point)))
          (goto-char (cdr region))
          (unless (re-search-forward "\n[:blank:]*\n" nil t)
            (goto-char (point-max)))
          (cursorfree-trim
           (cursorfree--make-target (cons start (point)))))))))

(defun cursorfree-row (index)
  "Return the visible line modulo 100 equal to INDEX as a target."
  (save-excursion
    (let* ((first-line (line-number-at-pos (window-start)))
           (last-line (line-number-at-pos (window-end)))
           (guess (+ (- first-line (% first-line 100)) index)))
      (when (< guess first-line)
        (setq guess (+ 100 guess)))
      (when (> guess last-line)
        (user-error "No line module 100 equal to %s" index))
      (goto-char (point-min))
      (forward-line (1- guess))
      (cursorfree-line (cursorfree--make-target (cons (point) (point)))))))

(defun cursorfree-every-instance (target &optional view)
  "Return a parallel target of every occurrence of TARGET.

If target VIEW is a region target, only instances inside of it will be
matched.  If it is a window, search within the buffer of that window.
Otherwise, search the buffer of TARGET."
  (declare (cursorfree--reverse-argument-order))
  (setq view
        (cond
         ((cursorfree--region-target-p view) view)
         ((windowp view) (cursorfree-everything view))
         (t (cursorfree-everything (cursorfree--target-buffer target)))))
  (list (make-cursorfree--parallel-target
         :targets
         (cursorfree--on-content-region view
           (lambda (view-region)
             (let ((search-string (cursorfree--target-get target))
                   (matches '()))
               (unless (equal search-string "")
                 (save-excursion
                   (goto-char (car view-region))
                   (while (search-forward search-string (cdr view-region) t)
                     (push (cursorfree--make-target (cons (match-beginning 0) (match-end 0)))
                           matches))))
               (nreverse matches)))))))

(cl-defstruct cursorfree--kill-ring-target)

(cl-defmethod cursorfree--target-get ((_ cursorfree--kill-ring-target))
  "Return current kill."
  (current-kill 0 nil))

(cl-defmethod cursorfree--target-put ((_ cursorfree--kill-ring-target) content)
  "Add CONTENT to the kill ring."
  (kill-new content))

(defun cursorfree-kill-ring ()
  "Return the kill ring as a target."
  (make-cursorfree--kill-ring-target))

(cl-defstruct cursorfree--primary-selection-target)

(cl-defmethod cursorfree--target-get ((_ cursorfree--primary-selection-target))
  "Return primary selection."
  (gui-get-primary-selection))

(cl-defmethod cursorfree--target-put ((_ cursorfree--primary-selection-target) content)
  "Put CONTENT into primary selection."
  (gui-set-selection nil content))

(defun cursorfree-primary-selection ()
  "Return the primary selection as a target."
  (make-cursorfree--primary-selection-target))

(defun cursorfree-next (target)
  (cursorfree--next target))

(defun cursorfree-previous (target)
  (cursorfree--previous target))

(cl-defgeneric cursorfree--next (target)
  "Get next occurrence of TARGET."
  (save-excursion
    (search-forward (cursorfree--target-get target))
    (cursorfree--make-target (cons (match-beginning 0) (match-end 0)))))

(cl-defmethod cursorfree--next ((target cursorfree--region-target))
  "Get the next literal occurence of contents of TARGET."
  (with-current-buffer (cursorfree--target-buffer target)
    (save-excursion
      (goto-char (cdr (cursorfree--content-region target)))
      (search-forward (cursorfree--target-get target))
      (cursorfree--make-target (cons (match-beginning 0) (match-end 0))))))

(cl-defgeneric cursorfree--previous (target)
  "Get previous occurrence of TARGET."
  (save-excursion
    (search-backward (cursorfree--target-get target))
    (cursorfree--make-target (cons (match-beginning 0) (match-end 0)))))

(cl-defmethod cursorfree--previous ((target cursorfree--region-target))
  "Get the previous literal occurence of contents of TARGET."
  (with-current-buffer (cursorfree--target-buffer target)
    (save-excursion
      (goto-char (car (cursorfree--content-region target)))
      (search-backward (cursorfree--target-get target))
      (cursorfree--make-target (cons (match-beginning 0) (match-end 0))))))

(defun cursorfree-make-parallel (&rest targets)
  "Make a parallel target out of TARGETS.

See `cursorfree--parallel-target' for more information on parallel
targets."
  (make-cursorfree--parallel-target :targets targets))

(defun cursorfree-end (&optional window-or-buffer)
  (declare (cursorfree--optional-bag
            ((or (bufferp %) (windowp %)) window-or-buffer)))
  (let ((in-buffer (cond
                    ((windowp window-or-buffer) (window-buffer window-or-buffer))
                    ((bufferp window-or-buffer) window-or-buffer)
                    (t (current-buffer)))))
    (with-current-buffer in-buffer
      (cursorfree--make-target (cons (point-max) (point-max))))))

(defun cursorfree-that ()
  cursorfree--target-that)

(defun cursorfree-source ()
  cursorfree--target-source)

(defvar cursorfree-modifiers
  `(("paint" . ,(cursorfree-make-modifier #'cursorfree-paint))
    ("leftpaint" . ,(cursorfree-make-modifier #'cursorfree-paint-left))
    ("rightpaint" . ,(cursorfree-make-modifier #'cursorfree-paint-right))
    ("trim" . ,(cursorfree-make-modifier #'cursorfree-trim))
    ("past" . ,(cursorfree-make-modifier #'cursorfree-past))
    ("selection" . ,(cursorfree-make-modifier #'cursorfree-current-selection))
    ("inside" . ,(cursorfree-make-modifier #'cursorfree-inner-parenthesis-dwim))
    ("outside" . ,(cursorfree-make-modifier #'cursorfree-outer-parenthesis-dwim))
    ("line" . ,(cursorfree-make-modifier #'cursorfree-line))
    ("tail" . ,(cursorfree-make-modifier #'cursorfree-line-right))
    ("head" . ,(cursorfree-make-modifier #'cursorfree-line-left))
    ("block" . ,(cursorfree-make-modifier #'cursorfree-block))
    ("link" . ,(cursorfree-thing-to-modifier 'url))
    ;; ("word" . ,(cursorfree-thing-to-modifier 'word))
    ("token" . ,(cursorfree-thing-to-modifier 'hatty-token))
    ("sentence" . ,(cursorfree-thing-to-modifier 'sentence))
    ("everything" . ,(cursorfree-make-modifier #'cursorfree-everything))
    ("visible" . ,(cursorfree-make-modifier #'cursorfree-visible))
    ("row" . ,(cursorfree-make-modifier #'cursorfree-row))
    ("this" . ,(cursorfree-make-modifier #'cursorfree-this))
    ("every instance" . ,(cursorfree-make-flattening-modifier #'cursorfree-every-instance))
    ("clip" . ,(cursorfree-make-modifier #'cursorfree-kill-ring))
    ("primary" . ,(cursorfree-make-modifier #'cursorfree-primary-selection))
    ("next" . ,(cursorfree-make-modifier #'cursorfree-next))
    ("preve" . ,(cursorfree-make-modifier #'cursorfree-previous))
    ("smash" . ,(cursorfree-make-modifier #'cursorfree-make-parallel))
    ("end" . ,(cursorfree-make-modifier #'cursorfree-end))
    ("that" . ,(cursorfree-make-modifier #'cursorfree-that))
    ("source" . ,(cursorfree-make-modifier #'cursorfree-source))))

;;; cursorfree.el ends soon
(provide 'cursorfree)
;;; cursorfree.el ends here
