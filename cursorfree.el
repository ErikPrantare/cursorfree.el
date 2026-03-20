;;; cursorfree.el --- Complex editing through voice  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.2.0
;; Homepage: https://github.com/ErikPrantare/cursorfree.el
;; Package-Requires: ((emacs "29.1"))
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

;; This package provides functionality for making complex editing
;; commands possible.  The functionality was made to leverage the
;; expressive power of voice control.  This package also comes with a
;; grammar, defined using the package "phony", that can be hooked up
;; to a voice control engine to control Emacs.

;;; Code:

(defgroup cursorfree nil
  "Functions for text and session manipulation."
  :group 'convenience
  :prefix "cursorfree-"
  :link '(emacs-commentary-link :tag "Commentary" "cursorfree.el"))

(defcustom cursorfree-highlight-deletions-p t
  "Whether to highlight text about to be deleted.
This is useful as visual feedback that the action just performed was the
one intended.  To turn this off, set this option to nil."
  :group 'cursorfree)

;;;; Instruction interpreter:

(defvar cursorfree--last-evaluation-result nil
  "The result of the last call to `cursorfree-evaluate'.")

(defun cursorfree-evaluate (instructions)
  "Apply the composition of INSTRUCTIONS on nil.

For example,

  (cursorfree-evaluate (list #'f #'g #'h))

would be equivalent to

  (h (g (f nil)))"
  (let ((values '()))
    (seq-doseq (instruction instructions)
      (setq values (funcall instruction values)))
    (setq cursorfree--last-evaluation-result values)
    values))

(defun cursorfree--apply-on-stack (function stack)
  "Apply FUNCTION to the top elements of STACK.
Return the rest of the STACK with the return value of FUNCTION on top.

The arity of FUNCTION is read from the cdr of `func-arity'.  The
function is evaluated with the top values of STACK, with the top
elements applied as the first arguments.  &rest arguments are
supported."
  (let* ((arity (cdr (func-arity function)))
         (args (if (eq arity 'many) stack (take arity stack)))
         (tail (if (eq arity 'many) '() (nthcdr arity stack))))
    (cons (apply function args) tail)))

(defun cursorfree-make-action (function)
  "Translate FUNCTION into an instruction not producing any value.

The resulting instruction will read the top elements of the value
stack to supply arguments for FUNCTION.  The read arguments will
not remain on the value stack."
  (lambda (values)
    (seq-rest (cursorfree--apply-on-stack function values))))

(defun cursorfree--multiple-cursors-do (function targets)
  "Apply FUNCTION to each target in TARGETS.
Create a new cursor each time.

Each target is assumed to be in the same buffer.

If invoking FUNCTION causes an error, no cursor is created."
  (when targets
    (when (and (cdr targets)
               (not (fboundp 'mc/create-fake-cursor-at-point)))
      (user-error "Using this operation on parallel targets requires the `multiple-cursors' package"))
    (cursorfree-on-content-region (car targets)
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
  (lambda (values)
    (cursorfree--apply-on-stack function values)))

(defun cursorfree--ensure-marker-region (region)
  "Return REGION, with the endpoints turned into markers as needed."
  (unless (consp region)
    (error "Invalid argument %s in cursorfree--ensure-marker-region" region))
  (let ((beginning (copy-marker (car region) nil))
        (end (copy-marker (cdr region) t)))
    (cons beginning end)))

(defun cursorfree--bounds-of-thing-at (thing position)
  "Return bounds of THING at POSITION."
  (save-excursion
    (goto-char position)
    (if-let ((bounds (bounds-of-thing-at-point thing)))
        (cursorfree--ensure-marker-region bounds))))

(cl-defstruct cursorfree-region-target
  "Target referring to CONTENT-REGION inside of BUFFER.
CONTENT-REGION is a cons cell of markers."
  (content-region nil :type (cons marker marker))
  (deletion-region nil :type (cons marker marker))
  (buffer nil :type buffer)
  (window nil :type window)
  (pre-insertion-string nil :type string)
  (post-insertion-string nil :type string)
  ;; :type plist
  (properties nil :type list))

(cl-defstruct cursorfree-parallel-target
  "Target comprising of a sequence of targets.
Operations applied to a parallel target are generally applied as if it
was applied to each element individually."
  targets)

(defun cursorfree--normalize-target (target)
  "Turn TARGET into a parallel if it is a non-singleton sequence.

If TARGET is not a sequence or is a sequence with a single element,
return that element.  Otherwise, return the targets of the sequence as
a `cursorfree-parallel-target'."
  (cond
   ((not (seqp target)) target)
   ((length= target 1) (seq-first target))
   (t (make-cursorfree-parallel-target :targets (seq-into target 'list)))))

(defun cursorfree--guess-deletion-region (region)
  "Guess the correct deletion region for REGION."
  (save-excursion
    (let (right-candidate
          left-candidate
          right-whitespace
          left-whitespace)
      (when (markerp (car region))
        (set-buffer (marker-buffer (car region))))

      (goto-char (cdr region))
      (skip-chars-forward "[:space:]\n")
      (setq right-candidate (point))
      (setq right-whitespace (buffer-substring
                              (cdr region)
                              right-candidate))

      (goto-char (car region))
      (skip-chars-backward "[:space:]\n")
      (setq left-candidate (point))
      (setq left-whitespace (buffer-substring
                             left-candidate
                             (car region)))
      ;; FIXME? Removing x, we want:
      ;; (f\nx) -> (f)
      ;; but also, removing "x-:"
      ;; (f\nx-y) -> (f\ny)
      ;; Can both cases be reasonably handled?  Or should we leave
      ;; such operations for when we have more structural information,
      ;; e.g. from treesitter?
      (cursorfree--ensure-marker-region
       (cond
        ((length= right-whitespace 0)
         (cons left-candidate (cdr region)))
        ((length= left-whitespace 0)
         (cons (car region) right-candidate))
        ((<= (seq-count (lambda (c) (eql c ?\n)) right-whitespace)
             (seq-count (lambda (c) (eql c ?\n)) left-whitespace))
         (cons (car region) right-candidate))
        (t
         (cons left-candidate (cdr region))))))))

(cl-defun cursorfree-make-target (content-region
                                  &key
                                  deletion-region
                                  buffer
                                  window
                                  pre-insertion-string
                                  post-insertion-string
                                  (constructor #'make-cursorfree-region-target)
                                  properties)
  "Return a target spanning CONTENT-REGION in the current buffer.

CONTENT-REGION must be a cons-cell (BEGINNING . END) of integers or
markers.

DELETION-REGION specified the region to remove if this target is
deleted.  If nil, the deletion region will be guessed.

If BUFFER is specified it will be associated to the new target.  If not,
but the BEGINNING is a marker with a buffer, use that buffer instead.
Otherwise, the current buffer will be associated to it.

If WINDOW is specified it will be associated to the new target.
Otherwise, the associated window will be guessed at the point of
request.

CONSTRUCTOR specifies the constructor to use.  It is assumed that it
may be invoked equivalently to `make-cursorfree-region-target', and
constructs a target inheriting from `cursorfree-region-target'.

If specified, PRE-INSERTION-STRING and POST-INSERTION-STRING specify a
string that should be inserted before or after the target if something
is put before or after it through `cursorfree--put-before' or
`cursorfree--put-after'.  The insertion string is put between the two
targets.

PROPERTIES is a plist of additional properties to associate to the
target."
  (with-current-buffer (window-normalize-buffer
                        (or buffer
                            (and (markerp (car content-region))
                                 (marker-buffer (car content-region)))))
    (let* ((region (cursorfree--ensure-marker-region content-region))
           (buffer (current-buffer))
           (deletion (cursorfree--ensure-marker-region
                      (or deletion-region
                          (cursorfree--guess-deletion-region region)))))
      (funcall constructor
               :content-region region
               :buffer buffer
               :window window
               :deletion-region deletion
               :pre-insertion-string (or pre-insertion-string " ")
               :post-insertion-string (or post-insertion-string " ")
               :properties properties))))

(defun cursorfree-content-region (target)
  "Return region of the content referred to by TARGET."
  (cursorfree-region-target-content-region target))

(defun cursorfree-buffer (&optional target)
  "Get the buffer associated with TARGET.
If no buffer is associated with TARGET, return nil.

If TARGET is nil or omitted, the current buffer is returned instead.

To override this function for new target types, implement a method for
`cursorfree--target-buffer'."
  (if target
      (cursorfree--target-buffer target)
    (current-buffer)))

(cl-defgeneric cursorfree--target-buffer (target)
  "Get the buffer associated with TARGET."
  nil)

(cl-defmethod cursorfree--target-buffer ((target cursorfree-region-target))
  "Get the buffer TARGET is located in."
  (cursorfree-region-target-buffer target))

(cl-defmethod cursorfree--target-buffer ((window window))
  "Get the buffer of WINDOW."
  (window-buffer window))

(cl-defmethod cursorfree--target-buffer ((buffer buffer))
  "Return BUFFER."
  buffer)

(defun cursorfree-window (target)
  "Get the window associated with TARGET.
If TARGET is nil or omitted, return nil.  Otherwise, if TARGET has no
associated window, return a window displaying the buffer associated with
TARGET, or nil if no window is showing that buffer.

To override this function for new target types, implement a method for
`cursorfree--target-window'."
  (cursorfree--target-window target))

(cl-defgeneric cursorfree--target-window (target)
  "Get the window associated with TARGET.

By default, returns a window showing the `cursorfree-buffer' of TARGET,
or nil if no window is showing that buffer."
  (when-let ((buffer (cursorfree-buffer target)))
    (get-buffer-window buffer)))

(cl-defmethod cursorfree--target-window ((target cursorfree-region-target))
  "Get the window TARGET was created in.
If the window is unknown, return a window displaying its buffer instead.
If no window is showing its buffer, return nil."
  (or (oref target window)
      (cl-call-next-method)))

(cl-defmethod cursorfree--target-window ((window window))
  "Return WINDOW."
  window)

(defun cursorfree-window-or-selected (&optional target)
  "Get the window associated with TARGET.
If TARGET is nil or omitted, returned the selected window."
  (if target
      (cursorfree--target-window target)
    (selected-window)))

(cl-defgeneric cursorfree-on-content-region (target f)
  "Apply F to the region associated with TARGET."
  (declare (indent defun))
  (error (format "Type error: %s has no associated region" target)))

(cl-defmethod cursorfree-on-content-region ((target cursorfree-region-target) f)
  "Apply F to the content region of TARGET.

If target has an associated window or buffer, they will first be set
as selected or current respectively."
  (with-selected-window (window-normalize-window (cursorfree-window target))
    (with-current-buffer (window-normalize-buffer (cursorfree-buffer target))
      (let ((region (cursorfree-content-region target)))
        (funcall f region)))))

(cl-defmethod cursorfree-on-content-region ((target cursorfree-parallel-target) f)
  "Apply F to the content region of each TARGET.

The return values are collected into a parallel target."
  (let ((result '()))
    (seq-doseq (target (cursorfree-parallel-target-targets target))
      (push (cursorfree-on-content-region target f)
            result))
    ;;HACK: Always wrap the results in a parallel-target.  In this way,
    ;;if f returns a target, we will automatically promote the result
    ;;into a parallel target.
    (make-cursorfree-parallel-target
     :targets (nreverse result))))

(cl-defmethod cursorfree-on-content-region ((buffer buffer) f)
  "Apply F to the contents of BUFFER."
  (cursorfree-on-content-region (cursorfree-everything buffer) f))

(cl-defmethod cursorfree-on-content-region ((window window) f)
  "Apply F to the contents of the buffer in WINDOW."
  (cursorfree-on-content-region (cursorfree-everything window) f))

(cl-defgeneric cursorfree-on-content-region-cursor-effect (target f)
  "Apply F to the region associated with TARGET.

This function exists for actions that manipulate point and mark.  If
done on a `cursorfree-parallel-target', a new cursor is created for
each target.

For a similar function not creating new cursors, see
`cursorfree-on-content-region'."
  (declare (indent defun))
  (error (format "Type error: %s has no content region" target)))

(cl-defmethod cursorfree-on-content-region-cursor-effect ((target cursorfree-region-target) f)
  "Call `cursorfree-on-content-region' with F and TARGET."
  (cursorfree-on-content-region target f))

(cl-defmethod cursorfree-on-content-region-cursor-effect ((target cursorfree-parallel-target) f)
  "For each target in TARGET, apply F and create a new cursor."
  ;; For now, assume that the parallel target is just a parallel of
  ;; region targets
  (cursorfree--multiple-cursors-do
   (lambda (target)
     (cursorfree-on-content-region target
       (lambda (region)
         (funcall f region))))
   (cursorfree-parallel-target-targets target)))

(defun cursorfree--make-target-from-hat (character &optional color shape)
  "Return target spanning a hatty token.

The token is indexed by CHARACTER, COLOR and SHAPE, as specified
by `hatty-locate-token'."
  (require 'hatty)
  (if-let ((region (hatty-locate-token character color shape)))
      (cursorfree-make-target region)
    (user-error "No such hat: color %s, shape %s, character %c" color shape character)))

(defun cursorfree--pusher (value)
  "Return instruction putting VALUE on the value stack."
  (lambda (values) (cons value values)))

;;;; Core functions

(cl-defgeneric cursorfree-target-get (target)
  "Return the content referred to by TARGET."
  (error (format "No method for getting content of target %s" target)))

(cl-defmethod cursorfree-target-get ((target string))
  "Return TARGET."
  target)

(cl-defmethod cursorfree-target-get ((target window))
  "Return TARGET."
  target)

(cl-defmethod cursorfree-target-get ((target buffer))
  "Return TARGET."
  target)

(cl-defmethod cursorfree-target-get ((target cursorfree-region-target))
  "Return the buffer substring of TARGET."
  (with-current-buffer (cursorfree-buffer target)
    (buffer-substring-no-properties (car (cursorfree-content-region target))
                                    (cdr (cursorfree-content-region target)))))

(cl-defmethod cursorfree-target-get ((target cursorfree-parallel-target))
  "Return the buffer substring of TARGET."
  (seq-map #'cursorfree-target-get (cursorfree-parallel-target-targets target)))

(cl-defgeneric cursorfree-target-put (target content)
  "Put CONTENT into TARGET."
  (error "No method for writing %S to target %S" content target))

(cl-defmethod cursorfree-target-put ((buffer buffer) (content string))
  "Put CONTENT into the `cursorfree-this' of BUFFER."
  (cursorfree-target-put (cursorfree-this buffer) content))

(cl-defmethod cursorfree-target-put ((window window) (content string))
  "Put CONTENT into the `cursorfree-this' of the current buffer of WINDOW."
  ;; FIXME: Why with-selected-window?
  (with-selected-window window
    (cursorfree-target-put (window-buffer window) content)))

(cl-defmethod cursorfree-target-put ((window window) (buffer buffer))
  "Set the current buffer of WINDOW to BUFFER."
  (set-window-buffer window buffer))

(cl-defmethod cursorfree-target-put ((target-window window) (source-window window))
  "Set the buffer of TARGET-WINDOW to the buffer of SOURCE-WINDOW."
  (cursorfree-target-put target-window (window-buffer source-window)))

(cl-defmethod cursorfree-target-put ((target cursorfree-region-target) (content string))
  "Remove region of TARGET and insert CONTENT.

TARGET will be modified to cover the region containing CONTENT."
  (cursorfree-on-content-region target
    (lambda (region)
      (let ((point-inside-target (<= (car region) (point) (cdr region))))
        (cursorfree--region-delete region)
        (cursorfree--insert-at (car region) content)
        (when point-inside-target
          (goto-char (cdr region)))))))

(cl-defmethod cursorfree-target-put ((target cursorfree-region-target) (content list))
  "Join CONTENT with spaces and put into a TARGET."
  (cursorfree-target-put target (string-join content " ")))

(cl-defmethod cursorfree-target-put ((parallel cursorfree-parallel-target) content)
  "Put CONTENT into each element of PARALLEL."
  (seq-doseq (target (oref parallel targets))
    (cursorfree-target-put target content)))

(cl-defmethod cursorfree-target-put ((parallel cursorfree-parallel-target) (content list))
  "Put elements of CONTENT into corresponding elements of PARALLEL.

That is, the first element gets put into the first element, the second
in the second, and so on.  If the lengths don't match, a user error is
signaled."
  (if (not (eq (seq-length (oref parallel targets))
               (seq-length content)))
      (user-error "Mismatching length of put-ed content list and parallel target")
    (seq-mapn #'cursorfree-target-put (oref parallel targets) content)))

(cl-defstruct (cursorfree--this-target (:include cursorfree-region-target))
  "Target indicating the \"the currently active thing\".  The meaning
of this is generally context dependent.  For example, when dealing
with regions, it denotes point, but when dealing with windows, it
denotes the currently selected window.

Generic functions may be overridden to provide specialized behavior
for \"this\".")

(defun cursorfree--collect-this ()
  "Return a list of the active regions of each cursor.

Each element of the list is a cons cell of markers (BEG . END).  If
there is no active region for the cursor, BEG equals END in position."
  ;; TODO: Better handle non-contiguous regions (like rectangles).
  ;; Currently the selection becomes messed up by e.g. "take this".
  (let (regions)
    (cursorfree--for-each-cursor
     (if mark-active
         (setq regions (append (region-bounds) regions))
       (push (cons (point) (point)) regions)))
    regions))

(defun cursorfree-this (&optional window-or-buffer)
  "Return an empty region located at point in WINDOW-OR-BUFFER.
If WINDOW-OR-BUFFER is omitted or nil, use the current buffer.

If region if active, the returned target will span that region instead
of being empty.

If multiple cursors are active, a `cursorfree-parallel-target' will be
returned, covering each cursor.

The returned target, if there is only one cursor, is of type
`cursorfree--this-target'.  Generic functions can be overloaded on this
type to give more context-dependent behavior for whatever \"this\"
means."
  ;; TODO: Move away from "this" as inheriting from region-target.
  ;; Pro: It would more easily allow this to be a parallel, in the
  ;; context of multiple cursors.
  ;; Con: User-defined procedures would need to take special account
  ;; of "this".
  ;; Reconciliation: Two different "this" types?
  ;; The reasoning for including "this" as its own type should be
  ;; documented more thoroughly.
  (let ((in-buffer (if window-or-buffer
                       (cursorfree-buffer window-or-buffer)
                     (current-buffer)))
        ;; FIXME: Might be problematic if the argument is a buffer.
        ;; Say that this function is evaluated from lisp: The given
        ;; buffer might be viewed in another window, providing another
        ;; position for point then in the current excursion.
        (in-window (if window-or-buffer
                       (cursorfree-window window-or-buffer)
                     (selected-window))))
    ;; Needs to have the correct window selected to get the correct
    ;; point, should the same buffer be viewed in multiple windows.
    (with-selected-window (window-normalize-window in-window)
      (with-current-buffer in-buffer
        (let ((regions (cursorfree--collect-this)))
          (cursorfree--normalize-target
           (seq-map (lambda (region)
                      (cursorfree-make-target
                       region
                       :deletion-region region
                       :constructor #'make-cursorfree--this-target
                       :buffer in-buffer
                       :window in-window))
                    regions)))))))

(cl-defmethod cursorfree-target-put ((target cursorfree--this-target) (content string))
  "Insert CONTENT at point in the buffer of TARGET."
  ;; Insert as if usual region target
  (cl-call-next-method)
  ;; Put point after the inserted text, given that point actually was
  ;; located at the corresponding region.
  (cursorfree-on-content-region target
    (lambda (region)
      (when (= (point) (car region))
        (goto-char (cdr region)))
      ;; Is there a multiple-cursors cursor here?  If so, move it as
      ;; well.
      (dolist (cursor (overlays-at (car region)))
        (when (overlay-get cursor 'mc-id)
          (setf (overlay-start cursor) (cdr region))
          (overlay-put cursor 'point (cdr region)))))))

(cl-defmethod cursorfree-target-put ((target cursorfree--this-target) (buffer buffer))
  "Set BUFFER as the current buffer of the window of TARGET."
  (cursorfree-target-put (cursorfree-window target) buffer))

(cl-defmethod cursorfree-target-put ((target cursorfree--this-target) (window window))
  "Set current buffer of window of TARGET to buffer of WINDOW."
  (cursorfree-target-put target (window-buffer window)))

(cl-defgeneric cursorfree--target-put-before (target content)
  "Put CONTENT before TARGET."
  (error "No method for writing %S before target %S" content target))

(cl-defmethod cursorfree--target-put-before ((target cursorfree-region-target) content)
  "Put CONTENT before TARGET.

The pre-insertion string of target is inserted between CONTENT and TARGET."
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (insert content)
        (insert (cursorfree-region-target-pre-insertion-string target))))))

(cl-defgeneric cursorfree--target-put-after (target content)
  "Put CONTENT after TARGET.."
  (error "No method for writing %S after target %S" content target))

(cl-defmethod cursorfree--target-put-after ((target cursorfree-region-target) content)
  "Put CONTENT after TARGET.

The post-insertion string of target is inserted between CONTENT and TARGET."
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (cdr region))
        (insert (cursorfree-region-target-post-insertion-string target))
        (insert content)))))

(cl-defgeneric cursorfree--put (target source)
  "Put the content of SOURCE in TARGET."
  (cursorfree-target-put target (cursorfree-target-get source)))

(cl-defgeneric cursorfree--put-before (target source)
  "Put the content of SOURCE before TARGET."
  (cursorfree--target-put-before target (cursorfree-target-get source)))

(cl-defgeneric cursorfree--put-after (target source)
  "Put the content of SOURCE after TARGET."
  (cursorfree--target-put-after target (cursorfree-target-get source)))

(cl-defmethod cursorfree--put ((target cursorfree-parallel-target) (source cursorfree-parallel-target))
  (let ((target-targets (cursorfree-parallel-target-targets target))
        (source-targets (cursorfree-parallel-target-targets source)))
    (if (not (eq (seq-length target-targets)
                 (seq-length source-targets)))
        (user-error "Mismatching length of parallel targets")
      (seq-mapn #'cursorfree--put target-targets source-targets))))

;;;; End of core functions

(defvar cursorfree--target-that nil
  "The target of the last operation.")
(defvar cursorfree--target-source nil
  "The source target of the last operation.")

(defun cursorfree--deletion-region (target)
  "Return region that should be removed if deleting TARGET."
  (cursorfree-region-target-deletion-region target))

(defun cursorfree--region-delete (region)
  "Delete REGION."
  (with-current-buffer (marker-buffer (car region))
    (delete-region (car region) (cdr region))))

(defun cursorfree-target-pulse (target)
  "Temporarily highlight TARGET."
  (when (cursorfree-region-target-p target)
    (cursorfree-on-content-region target
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
  (cursorfree-on-content-region-cursor-effect target
    (lambda (region)
      (set-mark (car region))
      (goto-char (cdr region)))))

(defun cursorfree-target-jump (target)
  "Go to TARGET."
  (cursorfree--target-jump target))

(cl-defgeneric cursorfree--target-jump (target)
  "Jump to TARGET.

The meaning of \"jump\" is left ambiguous to allow targets of
different types to be jumped to.  See the implemented methods for
examples."
  (error (format "No method for jumping to %s" target)))

(cl-defmethod cursorfree--target-jump ((target cursorfree-region-target))
  "Move point to beginning of TARGET.
If TARGET has an associated window, select it.  Otherwise, display the
associated buffer."
  (cursorfree-on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (car region))))
  (cursorfree--target-jump (or (cursorfree-window target)
                               (cursorfree-buffer target))))

(cl-defmethod cursorfree--target-jump ((target cursorfree-parallel-target))
  "Insert a cursor before each element of TARGET."
  (cursorfree-on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (car region)))))

(cl-defmethod cursorfree--target-jump ((window window))
  "Select WINDOW."
  (select-window window))

(cl-defmethod cursorfree--target-jump ((buffer buffer))
  "Set BUFFER as the current buffer."
  (switch-to-buffer buffer))

(cl-defgeneric cursorfree--target-jump-beginning (target)
  "Jump to the beginning of TARGET.

By default, this is equivalent to `cursorfree--target-jump'."
  (cursorfree--target-jump target))

(defun cursorfree-target-jump-beginning (target)
  "Jump to the beginning of TARGET."
  (cursorfree--target-jump-beginning target))

(cl-defgeneric cursorfree--target-jump-end (target)
  "Jump to the end of TARGET.

By default, this is equivalent to `cursorfree--target-jump'."
  (cursorfree--target-jump target))

(cl-defmethod cursorfree--target-jump-end ((target cursorfree-region-target))
  "Move point to end of TARGET.
If TARGET has an associated window, select it.  Otherwise, display the
associated buffer."
  (cursorfree-on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (cdr region))))
  (cursorfree--target-jump (or (cursorfree-window target)
                               (cursorfree-buffer target))))

(cl-defmethod cursorfree--target-jump-end ((target cursorfree-parallel-target))
  "Put a cursor at the end of every element of TARGET."
  (cursorfree-on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (cdr region)))))

(defun cursorfree-target-jump-end (target)
  "Jump to the end of TARGET."
  (cursorfree--target-jump-end target))

(defun cursorfree-target-indent (target)
  "Indent TARGET."
  (cursorfree-on-content-region target
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

(cl-defmethod cursorfree-target-delete ((target cursorfree-region-target))
  "Delete the deletion region of TARGET.
Try to fix up affected indentation."
  (cursorfree--region-delete (oref target deletion-region))
  (cursorfree-on-content-region (cursorfree-line target)
    (lambda (region)
      (when (derived-mode-p 'prog-mode)
        (indent-region (car region) (cdr region))))))

(cl-defmethod cursorfree-target-delete ((parallel cursorfree-parallel-target))
  "Delete each element of PARALLEL."
  (seq-doseq (target (cursorfree-parallel-target-targets parallel))
    (cursorfree-target-delete target)))

(cl-defmethod cursorfree-target-delete ((window window))
  "Delete WINDOW."
  (delete-window window))

(cl-defmethod cursorfree-target-delete ((buffer buffer))
  "Kill BUFFER."
  (kill-buffer buffer))

(defface cursorfree-deletion-highlight-face
  '((t :background "#620f2a"))
  "Face used to highlight target that will be removed.")

(cl-defgeneric cursorfree--indicate-deletion (target)
  "Indicate that TARGET is about to be deleted.

This can be turned off with `cursorfree-highlight-deletions-p'.  The
highlight color can be customized with
`cursorfree-deletion-highlight-face'."
  ;; TODO: Implement multiple regions for pulsing.  See pulse-faces in Emacs 31.
  (ignore))

(cl-defmethod cursorfree--indicate-deletion ((target cursorfree-region-target))
  "Highlight deletion region of TARGET momentarily."
  (when cursorfree-highlight-deletions-p
    (with-current-buffer (cursorfree--target-buffer target)
      (let* ((region (cursorfree-content-region target))
             (overlay (make-overlay (car region) (cdr region))))
        (overlay-put overlay 'face 'cursorfree-deletion-highlight-face)
        (redisplay t)
        (sleep-for 0.1)
        (delete-overlay overlay)))))

(cl-defmethod cursorfree--indicate-deletion ((target cursorfree-parallel-target))
  "Highlight deletion region of each target in TARGET momentarily."
  ;; FIXME: Can't we just indicate deletion for each element?
  (when cursorfree-highlight-deletions-p
    (let ((overlays '()))
      (cursorfree-on-content-region target
        (lambda (region)
          (let ((overlay (make-overlay (car region) (cdr region))))
            (overlay-put overlay 'face 'cursorfree-deletion-highlight-face)
            (push overlay overlays))))
      (redisplay)
      (sleep-for 0.1)
      (seq-doseq (overlay overlays)
        (delete-overlay overlay)))))

(defun cursorfree-target-chuck (&rest targets)
  "Delete TARGETS and indent the resulting text."
  (let ((target (cursorfree--normalize-target targets)))
    (cursorfree--indicate-deletion target)
    (cursorfree-target-delete target)
    (setq cursorfree--target-that target)))

(defmacro cursorfree--for-each-cursor (&rest body)
  "Evaluate BODY for each cursor."
  (if (fboundp 'mc/for-each-cursor-ordered)
       `(mc/for-each-cursor-ordered
         (mc/restore-state-from-overlay cursor)
         ,@body
         (mc/store-current-state-in-overlay cursor))
     `(progn ,@body)))

(defun cursorfree-target-bring (source &rest targets)
  "Overwrite TARGETS with SOURCE.

If no targets are given, overwrite `cursorfree-this' instead."
  (let ((target (cursorfree--normalize-target (or targets (cursorfree-this)))))
    (cursorfree--target-bring source target)))

(cl-defun cursorfree--target-bring (source target &key putter)
  "Put SOURCE into TARGET using PUTTER.

PUTTER is a function of two arguments, a target and a source.  It is
invoked with SOURCE and TARGET."
  (setq putter (or putter #'cursorfree--put))
  (funcall putter target source)
  (cursorfree-target-pulse target)
  (setq cursorfree--target-that target)
  (setq cursorfree--target-source source))

(defun cursorfree-target-move (source &rest targets)
  "Overwrite TARGETS with SOURCE, then delete SOURCE.

If no targets are given, overwrite `cursorfree-this' instead."
  (let ((target (cursorfree--normalize-target (or targets (cursorfree-this)))))
    (cursorfree--target-move source target)))

(cl-defgeneric cursorfree--target-move (source target &key putter)
  "Put SOURCE into TARGET using PUTTER, then delete SOURCE.

PUTTER is a function of two arguments, a target and a source.  It is
invoked with SOURCE and TARGET."
  (setq putter (or putter #'cursorfree--put))
  (cursorfree--indicate-deletion source)
  (cursorfree--target-bring source target :putter putter)
  (cursorfree-target-delete source))

(cl-defmethod cursorfree--target-move ((window window) target &key putter)
  "Put WINDOW into TARGET with PUTTER.
Switch buffer of WINDOW to its previous buffer."
  (cursorfree--target-bring window target :putter putter)
  (with-selected-window window
    (previous-buffer)))

(cl-defmethod cursorfree--target-move ((buffer buffer) (window window) &key putter)
  "Set the current buffer of WINDOW to BUFFER.

BUFFER is not deleted, so this is equivalent to
`cursorfree--target-bring'."
  ;; We do not want to kill the buffer if you move instead of bring.
  (cursorfree--target-bring buffer window :putter putter))

(defun cursorfree-target-swap (target1 target2)
  "Swap the contents of TARGET1 and TARGET2."
  (cursorfree--target-swap target1 target2))

(cl-defgeneric cursorfree--target-swap (target1 target2)
  "Swap contents of TARGET1 and TARGET2."
  (let ((content1 (cursorfree-target-get target1))
        (content2 (cursorfree-target-get target2)))
    (cursorfree-target-put target1 content2)
    (cursorfree-target-put target2 content1)))

(cl-defmethod cursorfree--target-swap ((window1 window) (window2 window))
  "Swap current buffers between WINDOW1 and WINDOW2."
  (let ((buffer1 (cursorfree--target-buffer window1))
        (buffer2 (cursorfree--target-buffer window2)))
    (cursorfree-target-put window1 buffer2)
    (cursorfree-target-put window2 buffer1)))

(cl-defgeneric cursorfree--target-change (target)
  "Change TARGET interactively.")

(cl-defmethod cursorfree--target-change ((target cursorfree-region-target))
  "Remove contents of TARGET and put point there."
  (cursorfree-target-jump target)
  (when (region-active-p) (deactivate-mark))
  (cursorfree--indicate-deletion target)
  (cursorfree--region-delete (cursorfree-content-region target)))

(cl-defmethod cursorfree--target-change ((target cursorfree-parallel-target))
  "Remove contents of TARGET and put points there."
  (cursorfree--multiple-cursors-do
   #'cursorfree--target-change
   (cursorfree-parallel-target-targets target)))

(defun cursorfree-target-change (&rest targets)
  "Move point to TARGETS and delete its contents."
  (let ((target (cursorfree--normalize-target (or targets (cursorfree-this)))))
    (cursorfree--target-change target)))

;; TODO: Don't move point
(defun cursorfree-target-clone (target)
  "Insert another copy of TARGET after itself."
  (cursorfree--target-clone target))

(cl-defgeneric cursorfree--target-clone (target)
  "Insert another copy of TARGET after itself."
  (cursorfree--target-put
   target
   (concat
    (cursorfree-target-get target)
    (cursorfree-target-get target))))

(cl-defmethod cursorfree--target-clone ((target cursorfree-region-target))
  "Insert another copy of TARGET after itself."
  (cursorfree--target-put-after target (cursorfree-target-get target)))

(defmacro cursorfree--simple-content-function (name docstring function)
  "Define function with NAME applying FUNCTION on targets.
Use DOCSTRING for the new function.

For each argument, the defined function NAME invokes FUNCTION on the
content region.  Afterwards, the region will be pulsed."
  (declare (indent defun))
  `(defun ,name (&rest targets)
     ,docstring
     (dolist (target targets)
       (cursorfree-on-content-region target
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
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (recenter 0))
      (cursorfree--clamp-line))))

(defun cursorfree-target-center (&optional target)
  "Scroll window so TARGET is in the center."
  (setq target (or target (cursorfree-this)))
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (recenter nil))
      (cursorfree--clamp-line))))

(defun cursorfree-target-bottom (&optional target)
  "Scroll window so TARGET is at the bottom."
  (setq target (or target (cursorfree-this)))
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (recenter -1))
      (cursorfree--clamp-line))))

(defun cursorfree-target-drink (&optional target)
  "Insert an empty line before TARGET and put point on it.

TARGET defaults to `cursorfree-this'."
  (unless target (setq target (cursorfree-this)))
  (cursorfree-on-content-region-cursor-effect target
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
  "Insert an empty line after TARGET and put point on it.

TARGET defaults to `cursorfree-this'."
  (unless target (setq target (cursorfree-this)))
  (cursorfree-on-content-region-cursor-effect target
    (lambda (region)
      (goto-char (cdr region))
      (end-of-line)
      (newline-and-indent))))

(defun cursorfree-target-drop (&optional target)
  "Insert an empty line before the line of TARGET.

TARGET defaults to `cursorfree-this'."
  (unless target (setq target (cursorfree-this)))
  (save-excursion
    (cursorfree-target-drink target)))

(defun cursorfree-target-float (&optional target)
  "Insert an empty line after the line of TARGET.

TARGET defaults to `cursorfree-this'."
  (unless target (setq target (cursorfree-this)))
  (save-excursion
    (cursorfree-target-pour target)))

(defun cursorfree-target-puff (&optional target)
  "Insert an empty line before and after the line of TARGET.

TARGET defaults to `cursorfree-this'."
  (unless target (setq target (cursorfree-this)))
  (cursorfree-target-float target)
  (cursorfree-target-drop target))

(defun cursorfree-target-wrap (parenthesis &rest targets)
  "Wrap TARGETS with characters specified by PARENTHESIS.

Insert PARENTHESIS before TARGETS.  If PARENTHESIS is some type of
parenthesis, insert the matching right version at the end of TARGETS.
Otherwise, insert PARENTHESIS instead."
  (dolist (target targets)
    (cursorfree-on-content-region target
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
             (_ parenthesis))))
        (when (= (car region) (point))
          (forward-char))))))

(make-obsolete 'cursorfree-target-wrap-parentheses
               'cursorfree-target-wrap
               "0.2.0")

(defcustom cursorfree-dwim-follow-alist
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
whatever thing point is located on."
  :type '(alist :key-type symbol :value-type function))

(defun cursorfree-dwim-follow ()
  "Try to follow the thing at point.
If point is at a button, push it.  Otherwise, use the current major
mode to look up the function in `cursorfree-dwim-follow-alist'."
  ;; The extra check that the button contains an action was used for
  ;; eww.  Check if this was fixed in Emacs 30.
  (cond
   ((and (button-at (point))
         (button-get (button-at (point)) 'action))
    (push-button))
   ((widget-at (point))
    (widget-apply-action (widget-at (point))))
   ((alist-get major-mode cursorfree-dwim-follow-alist)
    (funcall (alist-get major-mode cursorfree-dwim-follow-alist)))
   (t (user-error "Nothing to follow at %S in %S"
                  (point) (current-buffer)))))

(defun cursorfree-target-pick (&optional target)
  "Try to follow the thing at TARGET.

This function calls on `cursorfree-dwim-follow' to attempt to
follow the thing at TARGET."
  (setq target (or target (cursorfree-this)))
  (cursorfree-on-content-region target
    (lambda (region)
      (goto-char (car region))
      (cursorfree-dwim-follow))))

(defun cursorfree-target-fuse (target)
  "Remove all whitespace within TARGET."
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (replace-regexp (rx (not graphic)) ""
                        nil (car region) (cdr region))))))

(defun cursorfree-target-join (target)
  "Join TARGET into one line."
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (join-line nil (car region) (cdr region))))))

(defun cursorfree-target-break (target)
  "Insert newline before TARGET."
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (newline)
        (indent-region (car region) (cdr region))))))

(defun cursorfree-target-help (target)
  "Run `display-local-help' at the start of TARGET.

This may, for example, be used for displaying warning from eglot."
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (display-local-help)
        (cursorfree-target-pulse region)))))

(defun cursorfree-target-occur (target &optional extent context-lines)
  "List occurrences of TARGET in EXTENT with `occur'.

Occurrences will be searched for in the `cursorfree-buffer' of EXTENT if
it is given and non-nil.  Otherwise, the `cursorfree-buffer' of TARGET
is used.  If that returns nil, the current buffer is used instead.  If
EXTENT is a `cursorfree-region-target', the search will be restricted to
its region.

If CONTEXT-LINES is given, that many lines will be used as context."
  (cursorfree-on-content-region (or extent
                                    (cursorfree-buffer target)
                                    (current-buffer))
    (lambda (search-region)
      (occur (rx (literal (cursorfree-target-get target)))
             context-lines
             ;; TODO: Generalize to multiple regions
             (list search-region)))))

(defun cursorfree-target-unwrap (target)
  "Remove parentheses or quotation around TARGET."
  (cursorfree-target-bring
   (cursorfree-inside target)
   (cursorfree-outside target)))

(defun cursorfree-target-rewrap (character target)
  "Replace parentheses or quotations around TARGET with CHARACTER.

If CHARACTER is a parenthesis of some kind, the corresponding
parentheses will be put on the left and right side."
  (let ((expanded-target (cursorfree-inside target)))
    (cursorfree-target-unwrap expanded-target)
    (cursorfree-target-wrap character expanded-target)))

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
  (cursorfree-on-content-region target
    (lambda (region)
      (cursorfree-make-target
       (cons (cursorfree--skip-backward-from (car region) "^[:space:]\n")
             (cdr region))))))

(defun cursorfree-paint-right (&optional target)
  "Expand TARGET rightwards until the next whitespace."
  (setq target (or target (cursorfree-this)))
  (cursorfree-on-content-region target
    (lambda (region)
      (cursorfree-make-target
       (cons (car region)
             (cursorfree--skip-forward-from (cdr region) "^[:space:]\n"))))))

(defun cursorfree-paint (&optional target)
  "Expand TARGET leftwards and rightwards until the next whitespace."
  (setq target (or target (cursorfree-this)))
  (cursorfree-paint-right (cursorfree-paint-left target)))

(defun cursorfree--trim-left (target)
  "Shrink TARGET until there is no whitespace to the left."
  (cursorfree-on-content-region target
    (lambda (region)
      (cursorfree-make-target
       (cons (cursorfree--skip-forward-from (car region) "[:space:]\n")
             (cdr region))))))

(defun cursorfree--trim-right (target)
  "Shrink TARGET until there is no whitespace to the right."
  (cursorfree-on-content-region target
    (lambda (region)
      (cursorfree-make-target
       (cons (car region)
             (cursorfree--skip-backward-from (cdr region) "[:space:]\n"))))))

(defun cursorfree-trim (&optional target)
  "Shrink TARGET until there is no whitespace to the left or right."
  (setq target (or target (cursorfree-this)))
  (cursorfree--trim-left (cursorfree--trim-right target)))

(defun cursorfree--bounds-outside-parentheses-at-point-impl (left right)
  "Return the region of the parentheses containing point.

LEFT and RIGHT are characters denoting the left and right
parenthesis.

This function does not take certain edge cases into account, such as
parenthesis spanning disjoint string literals.

If no bound is found, nil is returned."
  (let ((old-syntax (syntax-table)))
    (with-syntax-table (copy-syntax-table old-syntax)
      ;; To avoid accidentally recognizing other parentheses, mark
      ;; them as whitespace in the new syntax table.
      (map-char-table
       (lambda (k v)
         ;; (logand (car v) #xff): The lower eight bits of the
         ;; syntax code represents the class.
         (when (or (= (syntax-class-to-char (logand (car v) #xff)) ?<)
                   (= (syntax-class-to-char (logand (car v) #xff)) ?>))
           (set-char-table-range (syntax-table) k (string-to-syntax " "))))
       old-syntax)
      (modify-syntax-entry left (string ?\( right))
      (modify-syntax-entry right (string ?\) left))
      (save-excursion
        (condition-case nil
            (cons (progn
                    (backward-up-list)
                    (point))
                  (progn
                    (forward-list)
                    (point)))
          (error nil))))))

(defun cursorfree--bounds-outside-parentheses-at-point (left right)
  "Return the region of the parentheses containing point.

LEFT and RIGHT are characters denoting the left and right
parenthesis.

If no bound is found, nil is returned."
  (if-let* ((nonsyntactic-region
             (cursorfree--bounds-of-nonsyntactic-region-at-point)))
      (or (with-restriction (car nonsyntactic-region) (cdr nonsyntactic-region)
            (cursorfree--bounds-outside-parentheses-at-point-impl left right))
          (save-excursion
            (goto-char (car nonsyntactic-region))
            (cursorfree--bounds-outside-parentheses-at-point-impl left right)))
    (cursorfree--bounds-outside-parentheses-at-point-impl left right)))

(defun cursorfree--bounds-outside-quote-at-point-impl (quote)
  "Return the region of the quoth containing point.

QUOTE is a character denoting which quote to look for.

This function does not take certain edge cases into account, such as
quotations occurring inside of comments.

If no bound is found, nil is returned."
  (condition-case nil
      (save-excursion
        (while (not (or (eq (char-after) quote)
                        (bobp)))
          (backward-up-list nil t))
        (and (eq (char-after) quote)
             (cons (point)
                   (progn (forward-sexp) (point)))))
    (error nil)))

(defun cursorfree--bounds-outside-quote-at-point (quote)
  "Return the region of the quote containing point.

QUOTE is a character denoting which quote to look for.

If no bound is found, nil is returned."
  (if-let* ((nonsyntactic-region
             (cursorfree--bounds-of-nonsyntactic-region-at-point)))
      (or (let ((current-syntax (syntax-table))
                (region-string (buffer-substring-no-properties
                                (car nonsyntactic-region)
                                (cdr nonsyntactic-region)))
                ;; Position relative to start of nonsyntactic region
                (position (1+ (- (point) (car nonsyntactic-region)))))
            (with-temp-buffer
              (insert region-string)
              (goto-char position)
              (set-syntax-table (copy-syntax-table current-syntax))
              ;; Do not treat comment chars in nonsyntactic region as
              ;; comment chars (e.g., ;; inside a string).
              (map-char-table
               (lambda (k v)
                 ;; (logand (car v) #xff): The lower eight bits of the
                 ;; syntax code represents the class.
                 (when (or (= (syntax-class-to-char (logand (car v) #xff)) ?<)
                           (= (syntax-class-to-char (logand (car v) #xff)) ?>))
                   (set-char-table-range (syntax-table) k (string-to-syntax " "))))
               current-syntax)
              (and-let* ((region (cursorfree--bounds-outside-quote-at-point-impl quote)))
                (cons (+ (car nonsyntactic-region) (1- (car region)))
                      (+ (car nonsyntactic-region) (1- (cdr region)))))))
          (cursorfree--bounds-outside-quote-at-point-impl quote))
    (cursorfree--bounds-outside-quote-at-point-impl quote)))

(defun cursorfree--bounds-outside-character-at-point (character)
  "Return smallest region containing point with CHARACTER on both sides.
If no such region exists, return nil."
  (cl-block nil
    (let (start end)
      (save-excursion
        (skip-chars-backward (string ?^ character))
        (when (bobp)
          (cl-return nil))
        (setq start (1- (point)))
        (skip-chars-forward (string ?^ character))
        (when (eobp)
          (cl-return nil))
        (setq end (1+ (point)))
        (cons start end)))))

(defun cursorfree--bounds-outside-any-at-point ()
  "Return region of closest quotation or parentheses containing point.
If no such region exists, return nil."
  (cl-block nil
    (save-excursion
      (let ((point-before (point)))
        (while (cursorfree--nonsyntactic-p)
          (skip-syntax-backward "^\"\(")
          (when (bobp) (cl-return nil))
          (when-let ((bounds (cursorfree--bounds-outside-at-point (char-before))))
            (when (and (<= (car bounds) point-before (cdr bounds))
                       (eq (point) (1+ (car bounds))))
              (cl-return bounds)))
          (backward-char))))
    (bounds-of-thing-at-point 'list)))

(defun cursorfree--bounds-outside-at-point (&optional delimiter)
  "Return region with DELIMITER containing point.

DELIMITER is a character denoting either a quotation, parenthesis, or
other character.  If omitted, the closest delimiters that are deemed
parentheses or quotes by the current syntax are chosen."
  (if (null delimiter)
      (cursorfree--bounds-outside-any-at-point)
    (pcase (char-syntax delimiter)
      (?\( (cursorfree--bounds-outside-parentheses-at-point
            delimiter
            (cdr (char-table-range (syntax-table) delimiter))))
      (?\) (cursorfree--bounds-outside-parentheses-at-point
            (cdr (char-table-range (syntax-table) delimiter))
            delimiter))
      (?\" (cursorfree--bounds-outside-quote-at-point delimiter))
      (_ (cursorfree--bounds-outside-character-at-point delimiter)))))

(defun cursorfree-outside (&optional target delimiter)
  "Expand TARGET to contain enclosing DELIMITER.

TARGET defaults to the target returned by `cursorfree-this'.

If DELIMITER is given, target is expanded until it reaches corresponding
matching delimiter on both sides.  Otherwise, try to guess which
delimiter is intended."
  (cursorfree--expand-bounds
   (or target (cursorfree-this))
   (lambda ()
     (cursorfree--bounds-outside-at-point delimiter))))

(defun cursorfree-inside (&optional target delimiter)
  "Expand TARGET until  enclosing DELIMITER.

TARGET defaults to the target returned by `cursorfree-this'.

If DELIMITER is given, target is expanded until it reaches corresponding
matching delimiter on both sides.  Otherwise, try to guess which
delimiter is intended."
  (cursorfree-on-content-region
    (cursorfree-outside target delimiter)
    (lambda (region)
      (cursorfree-make-target
       (cons
        (1+ (car region))
        (1- (cdr region)))))))

(defun cursorfree--targets-hull (&rest targets)
  "Return the smallest target that can fit all TARGETS."
  (when targets
    ;; Make sure target gets created in correct buffer (max and min do
    ;; not return the corresponding marker, but a new integer instead)
    (with-current-buffer (cursorfree-buffer (car targets))
      (let* ((leftmost (seq-first
                        (seq-sort-by
                         (lambda (target)
                           (car (cursorfree-content-region target)))
                         #'<
                         targets)))
             (rightmost (seq-first
                         (seq-sort-by
                          (lambda (target)
                            (cdr (cursorfree-content-region target)))
                          #'>
                          targets)))
             (content-region
              (cons (car (cursorfree-content-region leftmost))
                    (cdr (cursorfree-content-region rightmost)))))
        (cursorfree-make-target
         content-region
         :pre-insertion-string
         (cursorfree-region-target-pre-insertion-string leftmost)
         :post-insertion-string
         (cursorfree-region-target-post-insertion-string rightmost))))))

(defun cursorfree-past (target1 &optional target2)
  "Return the smallest target that can fit TARGET1 and TARGET2."
  (setq target2 (or target2
                    (with-current-buffer (cursorfree-buffer target1)
                      (cursorfree-this))))
  (cursorfree--targets-hull target1 target2))

(defun cursorfree-current-selection ()
  "Return the active region as a target."
  ;; TODO: Handle noncontiguous selections?
  (cursorfree-make-target
   (car (region-bounds))))

(make-obsolete #'cursorfree-current-selection #'cursorfree-this
               "0.3.0")

(defun cursorfree--expand-bounds (target bounds-function)
  "Expand the beginning of TARGET using BOUNDS-FUNCTION.

BOUNDS-FUNCTION is a function that takes no arguments and returns a
region (BEG . END).  BOUNDS-FUNCTION is invoked with point at the
beginning of TARGET to get the content region of the returned target.

This function returns nil if BOUNDS-FUNCTION returns nil."
  (cursorfree-on-content-region target
    (lambda (region)
      (save-excursion
        (goto-char (car region))
        (and-let* ((bounds (funcall bounds-function)))
          (cursorfree-make-target bounds))))))

(defun cursorfree--expand-to-thing (thing &optional target)
  "Extend the beginning of TARGET to cover containing THING.

The extension is done from the beginning of the target.  See
`bounds-of-thing-at-point' for more information about the builtin
`thing-at-point' functionalities.

TARGET defaults to `cursorfree-this' if nil or omitted"
  (cursorfree--expand-bounds
   (or target (cursorfree-this))
   (lambda ()
     (bounds-of-thing-at-point thing))))

(defun cursorfree-everything (&optional target)
  "Return a target referring to the full content of the buffer of TARGET.

TARGET defaults to `cursorfree-this'.

This function respects narrowing."
  (unless target (setq target (cursorfree-this)))
  (with-current-buffer (cursorfree-buffer target)
    (cursorfree-make-target
     (cons (point-min) (point-max)))))

(defun cursorfree-visible (&optional window)
  "Return a target referring to the visible portion of the buffer.

WINDOW defaults to the selected window."
  (setq window (or window (selected-window)))
  (with-selected-window window
    (with-current-buffer (window-buffer)
      (save-excursion
        (let (beginning end)
          (move-to-window-line 0)
          (beginning-of-visual-line)
          (setq beginning (point))
          (move-to-window-line -1)
          (end-of-visual-line)
          (setq end (point))
          (cursorfree-make-target
           (cons beginning end)))))))

(defun cursorfree-line-right (&optional target)
  "Extend TARGET to the final non-whitespace character of its line."
  (setq target (or target (cursorfree-this)))
  (let ((target-content-region (cursorfree-content-region target))
        (target-deletion-region (cursorfree--deletion-region target))
        space-length
        content-region
        deletion-region)
    (save-excursion
      (set-buffer (cursorfree-buffer target))
      (goto-char (cdr target-content-region))
      (unless (search-forward "\n" nil t)
        (goto-char (point-max)))
      (setq deletion-region (cons (car target-deletion-region) (point)))
      (skip-chars-backward "[:space:]\n" (cdr target-content-region))
      (setq content-region (cons (car target-content-region) (point)))

      (cursorfree-make-target
       content-region
       :deletion-region deletion-region
       :pre-insertion-string (cursorfree-region-target-pre-insertion-string target)
       :post-insertion-string (cursorfree-region-target-pre-insertion-string (cursorfree-line-left target))))))

(defun cursorfree-line-left (&optional target)
  "Extend TARGET to the first non-whitespace character of its line."
  (setq target (or target (cursorfree-this)))
  (let ((target-content-region (cursorfree-content-region target))
        (target-deletion-region (cursorfree--deletion-region target))
        space-length
        content-region
        deletion-region)
    (save-excursion
      (set-buffer (cursorfree-buffer target))
      (goto-char (car (cursorfree-content-region target)))
      (if (search-backward "\n" nil t)
          (setq deletion-region (cons (1+ (point)) (cdr target-deletion-region)))
        (goto-char (point-min))
        (setq deletion-region (cons (point) (cdr target-deletion-region))))
      (skip-chars-forward "[:space:]\n" (car target-content-region))
      (setq content-region (cons (point) (cdr target-content-region)))
      (setq space-length (abs (- (car deletion-region) (car content-region))))

      (cursorfree-make-target
       content-region
       :deletion-region deletion-region
       :pre-insertion-string (concat "\n" (make-string space-length ?\ ))
       :post-insertion-string (cursorfree-region-target-post-insertion-string target)))))

(defun cursorfree-line (&optional target)
  "Extend TARGET to cover all non-whitespace characters on its line."
  (setq target (or target (cursorfree-this)))
  (cursorfree-line-left (cursorfree-line-right target)))

(defun cursorfree-sentence (&optional target)
  "Extend TARGET to cover its containing sentence.
TARGET defaults to the return value of `cursorfree-this'."
  (cursorfree-on-content-region (or target (cursorfree-this))
    (lambda (region)
      (cursorfree-make-target
       (cursorfree--bounds-of-thing-at 'sentence (car region))))))

(defun cursorfree-token (&optional target)
  "Extend the beginning of TARGET to cover its containing hatty token.
TARGET defaults to the return value of `cursorfree-this'."
  (require 'hatty)
  (cursorfree-on-content-region (or target (cursorfree-this))
    (lambda (region)
      (cursorfree-make-target
       (cursorfree--bounds-of-thing-at 'hatty-token (car region))))))

(defun cursorfree-block (&optional target)
  "Extend TARGET to the smallest region with empty lines on both sides."
  (setq target (or target (cursorfree-this)))
  (save-excursion
    (cursorfree-on-content-region target
      (lambda (region)
        (goto-char (car region))
        (unless (re-search-backward (rx "\n" (* blank) "\n") nil t)
          (goto-char (point-min)))
        (skip-chars-forward  "\n[:blank:]")
        (let ((start (point)))
          (goto-char (cdr region))
          (unless (re-search-forward (rx "\n" (* blank) "\n") nil t)
            (goto-char (point-max)))
          (let ((result (cursorfree-trim
                         (cursorfree-make-target (cons start (point))))))
            (oset result post-insertion-string "\n\n")
            (oset result pre-insertion-string "\n\n")
            result))))))

(defun cursorfree-row (index)
  "Return the line on row INDEX."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- index))
    (cursorfree-line)))

(defun cursorfree-row-modulo-100 (index)
  "Return the visible line modulo 100 equal to INDEX as a target."
  (save-excursion
    (let* ((first-line (line-number-at-pos (window-start)))
           (last-line (line-number-at-pos (window-end)))
           (guess (+ (- first-line (% first-line 100)) index)))
      (when (< guess first-line)
        (setq guess (+ 100 guess)))
      (when (> guess last-line)
        (user-error "No line modulo 100 equal to %s" index))
      (cursorfree-row guess))))

(defun cursorfree-every-instance (target &optional view)
  "Return a parallel target of every occurrence of TARGET.

If target VIEW is a region target, only instances inside of it will be
matched.  If it is a window, search within the buffer of that window.
Otherwise, search the buffer of TARGET."
  (setq view (or view (cursorfree-buffer target)))
  (make-cursorfree-parallel-target
   :targets
   (cursorfree-on-content-region view
     (lambda (view-region)
       (let ((search-string (cursorfree-target-get target))
             (matches '()))
         (unless (equal search-string "")
           (save-excursion
             (goto-char (car view-region))
             (while (search-forward search-string (cdr view-region) t)
               (push (cursorfree-make-target (cons (match-beginning 0) (match-end 0)))
                     matches))))
         (nreverse matches))))))

(cl-defstruct cursorfree--kill-ring-target)

(cl-defmethod cursorfree-target-get ((_ cursorfree--kill-ring-target))
  "Return current kill."
  (current-kill 0 nil))

(cl-defmethod cursorfree-target-put ((_ cursorfree--kill-ring-target) content)
  "Add CONTENT to the kill ring."
  (kill-new content))

(defun cursorfree-kill-ring ()
  "Return the kill ring as a target."
  (make-cursorfree--kill-ring-target))

(cl-defstruct cursorfree--primary-selection-target)

(cl-defmethod cursorfree-target-get ((_ cursorfree--primary-selection-target))
  "Return primary selection."
  (gui-get-primary-selection))

(cl-defmethod cursorfree-target-put ((_ cursorfree--primary-selection-target) content)
  "Put CONTENT into primary selection."
  (gui-set-selection nil content))

(defun cursorfree-primary-selection ()
  "Return the primary selection as a target."
  (make-cursorfree--primary-selection-target))

(defun cursorfree--nonsyntactic-p ()
  "Return non-nil if point is inside a comment or string literal."
  (let ((state (syntax-ppss (point))))
    (or (seq-elt state 3) (seq-elt state 4))))

(defun cursorfree--bounds-of-nonsyntactic-region-at-point (&optional type)
  "Return region of comment or string literal at point.

If TYPE is 'comment, this function looks for a containing comment.  If
TYPE is 'string, this function looks for a string literal instead.  If
TYPE is omitted, nil or 'any, either one is looked for.  If TYPE is
anything else, an error is signalled.

If point is not in a comment or string, nil is returned."
  (unless type (setq type 'any))
  (unless (memq type '(comment string any))
    (error "Argument COMMENT-OR-STRING must be either 'comment, 'string or 'any"))
  (save-excursion
    (let ((state (syntax-ppss (point))))
      (and (cond ((eq type 'string) (seq-elt state 3))
                 ((eq type 'comment) (seq-elt state 4))
                 ((eq type 'any) (or (seq-elt state 3)
                                     (seq-elt state 4))))
           (cons (seq-elt state 8)      ; Start of comment or string
                 (progn
                   ;; Move point to after end of comment or string
                   (parse-partial-sexp (point)
                                       (point-max)
                                       nil
                                       nil
                                       state
                                       'syntax-table)
                   (point)))))))

(defun cursorfree--bounds-of-comment-at-point ()
  "Return bounds of the comment at point.

If there is no comment at point, this function returns nil."
  (cursorfree--bounds-of-nonsyntactic-region-at-point 'comment))

(put 'cursorfree--comment
     'bounds-of-thing-at-point
     #'cursorfree--bounds-of-comment-at-point)

(defun cursorfree-comment (&optional target)
  "Return target extended to contain the comment at its beginning.

If there is no comment at the beginning of target, nil is returned."
  (cursorfree--expand-to-thing
   'cursorfree--comment
   (or target (cursorfree-this))))

(defun cursorfree--bounds-of-string-literal-at-point ()
  (cursorfree--bounds-of-nonsyntactic-region-at-point 'string))

(put 'cursorfree--string-literal
     'bounds-of-thing-at-point
     #'cursorfree--bounds-of-string-literal-at-point)

(defun cursorfree-string-literal (&optional target)
  (cursorfree--expand-to-thing
   'cursorfree--string-literal
   (or target (cursorfree-this))))

(defun cursorfree-next (target)
  "Return next occurrence of the content of TARGET."
  (cursorfree--next target))

(defun cursorfree-previous (target)
  "Return previous occurrence of the content of TARGET."
  (cursorfree--previous target))

(cl-defgeneric cursorfree--next (target)
  "Return next occurrence after point of the content of TARGET."
  (save-excursion
    (search-forward (cursorfree-target-get target))
    (cursorfree-make-target (cons (match-beginning 0) (match-end 0)))))

(cl-defmethod cursorfree--next ((target cursorfree-region-target))
  "Return next occurence of contents of TARGET."
  (with-current-buffer (cursorfree-buffer target)
    (save-excursion
      (goto-char (cdr (cursorfree-content-region target)))
      (search-forward (cursorfree-target-get target))
      (cursorfree-make-target (cons (match-beginning 0) (match-end 0))))))

(cl-defgeneric cursorfree--previous (target)
  "Return previous occurrence after point of the content of TARGET."
  (save-excursion
    (search-backward (cursorfree-target-get target))
    (cursorfree-make-target (cons (match-beginning 0) (match-end 0)))))

(cl-defmethod cursorfree--previous ((target cursorfree-region-target))
  "Return previous occurence of contents of TARGET."
  (with-current-buffer (cursorfree-buffer target)
    (save-excursion
      (goto-char (car (cursorfree-content-region target)))
      (search-backward (cursorfree-target-get target))
      (cursorfree-make-target (cons (match-beginning 0) (match-end 0))))))

(defun cursorfree-make-parallel (&rest targets)
  "Make a parallel target out of TARGETS.

See `cursorfree-parallel-target' for more information on parallel
targets."
  (make-cursorfree-parallel-target :targets targets))

(defun cursorfree-beginning (&optional target)
  "Return empty region target located at beginning of TARGET.

TARGET defaults to `cursorfree-everything'."
  (cursorfree-on-content-region (or target (cursorfree-everything))
    (lambda (region)
      (cursorfree-make-target (cons (car region) (car region))))))

(defun cursorfree-end (&optional target)
  "Return empty region target located at end of TARGET.

TARGET defaults to `cursorfree-everything'."
  (cursorfree-on-content-region (or target (cursorfree-everything))
    (lambda (region)
      (cursorfree-make-target (cons (cdr region) (cdr region))))))

(defun cursorfree-that ()
  "Return the primary target of the previous operation.

For example, if `cursorfree-target-bring' was the previous operation,
this returns the target of that."
  cursorfree--target-that)

(defun cursorfree-source ()
  "Return the source target of the previous operation.

For example, if `cursorfree-target-bring' was the previous operation,
this returns the source of that."
  cursorfree--target-source)

;;; cursorfree.el ends soon
(provide 'cursorfree)
;;; cursorfree.el ends here
