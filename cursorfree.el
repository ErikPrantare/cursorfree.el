;;; cursorfree.el --- Edit and navigate through hats -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.0.0
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
  (cl-destructuring-bind (head . tail)
      (cursorfree-environment-value-stack environment)
    (setf (cursorfree-environment-value-stack environment) tail)
    head))

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

(defun cursorfree-to-action (function)
  "Translate FUNCTION an instruction not producing any value.

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

(defun cursorfree-to-modifier (function)
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

(defun cursorfree--make-target (content-region)
  "Return a target spanning CONTENT-REGION."
  (cursorfree--markify-region content-region))

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

(defun cursorfree--deletion-region (target)
  "Return region that should be removed if deleting TARGET."
  (cursorfree--markify-region
   (save-excursion
     (goto-char (cdr target))
     (if (/= 0 (skip-chars-forward "[:space:]\n"))
         (cons (car target) (point))
       (goto-char (car target))
       (skip-chars-backward "[:space:]\n")
       (cons (point) (cdr target))))))

(defun cursorfree--target-string (target)
  "Return the string referred to by TARGET."
  (buffer-substring-no-properties (car target) (cdr target)))

(defun cursorfree--region-delete (region)
  "Delete REGION."
  (delete-region (car region) (cdr region)))

(defun cursorfree-target-pulse (target)
  "Temporarily highlight TARGET."
  (pulse-momentary-highlight-region (car target) (cdr target)))

(defun cursorfree--insert-at (position string)
  "Insert STRING at POSITION."
  (save-excursion
    (goto-char position)
    (insert string)
    (cursorfree-target-pulse (cons position (+ position (length string))))))

(defun cursorfree-target-select (target)
  "Set active region to TARGET."
  (set-mark (car target))
  (goto-char (cdr target)))

(defun cursorfree-target-jump-beginning (target)
  "Move point to beginning of TARGET."
  (goto-char (car target)))

(defun cursorfree-target-jump-end (target)
  "Move point to end of TARGET."
  (goto-char (cdr target)))

(defun cursorfree-target-indent (target)
  "Indent TARGET."
  (indent-region (car target) (cdr target)))

(defun cursorfree-target-chuck (target)
  "Delete TARGET and indent the resulting text."
  (cursorfree--region-delete (cursorfree--deletion-region target))
  (if-let ((indentation-region (cursorfree--bounds-of-thing-at 'line (car target))))
    (cursorfree-target-indent indentation-region)))

(defun cursorfree-target-bring (target)
  "Insert TARGET at point."
  (insert (cursorfree--target-string target))
  (cursorfree-target-pulse target))

(defun cursorfree--target-overwrite (target string)
  "Overwrite TARGET with STRING."
  (cursorfree--region-delete target)
  (cursorfree--insert-at (car target) string))

(defun cursorfree-target-move (target)
  "Insert TARGET at point and delete TARGET."
  (cursorfree-target-bring target)
  (cursorfree-target-chuck target))

(defun cursorfree-target-swap (target1 target2)
  "Swap the contents of TARGET1 and TARGET2."
  (let ((string1 (cursorfree--target-string target1))
        (string2 (cursorfree--target-string target2)))
    (cursorfree--target-overwrite target1 string2)
    (cursorfree--target-overwrite target2 string1)))

(defun cursorfree-target-change (target)
  "Move point to TARGET and delete its contents."
  (cursorfree--region-delete target)
  (goto-char (car target)))

(defun cursorfree-target-clone (target)
  "Insert another copy of TARGET after itself."
  (cursorfree--insert-at
   (cdr target)
   (cursorfree--target-string target)))

(defun cursorfree-target-copy (target)
  "Copy TARGET to kill ring."
  (copy-region-as-kill (car target) (cdr target))
  (cursorfree-target-pulse target))

(defun cursorfree-target-comment (target)
  "Comment out TARGET."
  (comment-region (car target) (cdr target))
  (cursorfree-target-pulse target))

(defun cursorfree-target-uncomment (target)
  "Uncomment TARGET."
  (uncomment-region (car target) (cdr target))
  (cursorfree-target-pulse target))

(defun cursorfree-target-narrow (target)
  "Narrow region to TARGET."
  (narrow-to-region (car target) (cdr target)))

(defun cursorfree-target-fill (target)
  "Fill the paragraphs in TARGET."
  (fill-region (car target) (cdr target))
  (cursorfree-target-pulse target))

(defun cursorfree-target-capitalize (target)
  "Capitalize the first character of each word in TARGET."
  (capitalize-region (car target) (cdr target)))

(defun cursorfree-target-upcase (target)
  "Convert TARGET to upper case."
  (upcase-region (car target) (cdr target)))

(defun cursorfree-target-downcase (target)
  "Convert TARGET to lower case."
  (downcase-region (car target) (cdr target)))

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
  (save-excursion
    (cursorfree-target-jump-beginning target)
    (recenter 0))
  (cursorfree--clamp-line))

(defun cursorfree-target-center (target)
  "Scroll window so TARGET is in the center."
  (save-excursion
    (cursorfree-target-jump-beginning target)
    (recenter nil))
  (cursorfree--clamp-line))

(defun cursorfree-target-bottom (target)
  "Scroll window so TARGET is at the bottom."
  (save-excursion
    (cursorfree-target-jump-end target)
    (recenter -1))
  (cursorfree--clamp-line))

(defun cursorfree-target-drink (target)
  "Insert an empty line before TARGET and put point on it."
  (goto-char (car target))
  (beginning-of-line)
  (insert ?\n)
  (backward-char))

(defun cursorfree-target-pour (target)
  "Insert an empty line after TARGET and put point on it."
  (goto-char (cdr target))
  (end-of-line)
  (insert ?\n))

(defun cursorfree-target-wrap-parentheses (parenthesis target)
  "Wrap TARGET with characters specified by PARENTHESIS.

Insert PARENTHESIS before TARGET.  If PARENTHESIS is some type of
parenthesis, insert the matching right version at the end of
TARGET.  Otherwise, insert PARENTHESIS instead."
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

(defvar cursorfree-dwim-follow-alist
  `((org-mode . org-open-at-point)
    (Info-mode . Info-try-follow-nearest-node)
    (help-mode . push-button)
    (dired-mode . dired-find-file)
    (compilation-mode . compile-goto-error)
    (grep-mode . compile-goto-error)
    (eww-mode . ,(lambda ()
                   (if (get-text-property (point) 'eww-form)
                       (eww-submit)
                    (eww-follow-link)))))
  "Alist for mapping major mode to function for following at point.

Used in `cursorfree-dwim-follow' for determining how to follow
whatever thing point is located on.")

(defun cursorfree-dwim-follow ()
  "Try to follow the thing at point.

This function may be customized by changing
`cursorfree-dwim-follow-alist'."
  (if-let ((follow-action
            (alist-get major-mode cursorfree-dwim-follow-alist)))
      (funcall follow-action)))

(defun cursorfree-target-pick (target)
  "Try to follow the thing at TARGET.

This function calls on `cursorfree-dwim-follow' to attempt to
follow the thing at TARGET."
  (save-excursion
    (goto-char (car target))
    (cursorfree-dwim-follow)))

(defun cursorfree-target-fuse (target)
  "Remove all whitespace within TARGET."
  (save-excursion
    (replace-regexp (rx (or whitespace "\n")) "" nil (car target) (cdr target))))

(defun cursorfree-target-help (target)
  "Run `display-local-help' at the start of TARGET.

This may, for example, be used for displaying warning from eglot."
  (save-excursion
    (goto-char (car target))
    (display-local-help)
    (cursorfree-target-pulse target)))

(defvar cursorfree-actions
  `(("select" . ,(cursorfree-to-action #'cursorfree-target-select))
    ("copy" . ,(cursorfree-to-action #'cursorfree-target-copy))
    ("chuck" . ,(cursorfree-to-action #'cursorfree-target-chuck))
    ("bring" . ,(cursorfree-to-action #'cursorfree-target-bring))
    ("move" . ,(cursorfree-to-action #'cursorfree-target-move))
    ("swap" . ,(cursorfree-to-action #'cursorfree-target-swap))
    ("clone" . ,(cursorfree-to-action #'cursorfree-target-clone))
    ("jump" . ,(cursorfree-to-action #'cursorfree-target-jump-beginning))
    ("pre" . ,(cursorfree-to-action #'cursorfree-target-jump-beginning))
    ("post" . ,(cursorfree-to-action #'cursorfree-target-jump-end))
    ("change" . ,(cursorfree-to-action #'cursorfree-target-change))
    ("comment" . ,(cursorfree-to-action #'cursorfree-target-comment))
    ("uncomment" . ,(cursorfree-to-action #'cursorfree-target-uncomment))
    ("indent" . ,(cursorfree-to-action #'cursorfree-target-indent))
    ("narrow" . ,(cursorfree-to-action #'cursorfree-target-narrow))
    ("wrap" . ,(cursorfree-to-action #'cursorfree-target-wrap-parentheses))
    ("filler" . ,(cursorfree-to-action #'cursorfree-target-fill))
    ("title" . ,(cursorfree-to-action #'cursorfree-target-capitalize))
    ("upcase" . ,(cursorfree-to-action #'cursorfree-target-upcase))
    ("downcase" . ,(cursorfree-to-action #'cursorfree-target-downcase))
    ("crown" . ,(cursorfree-to-action #'cursorfree-target-crown))
    ("center" . ,(cursorfree-to-action #'cursorfree-target-center))
    ("bottom" . ,(cursorfree-to-action #'cursorfree-target-bottom))
    ("pick" . ,(cursorfree-to-action #'cursorfree-target-pick))
    ("fuse" . ,(cursorfree-to-action #'cursorfree-target-fuse))
    ("flash" . ,(cursorfree-to-action #'cursorfree-target-pulse))
    ("help" . ,(cursorfree-to-action #'cursorfree-target-help))
    ("drink" . ,(cursorfree-to-action #'cursorfree-target-drink))
    ("pour" . ,(cursorfree-to-action #'cursorfree-target-pour)))
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
  (cons (cursorfree--skip-backward-from (car target) "^[:space:]\n")
        (cdr target)))

(defun cursorfree-paint-right (target)
  "Expand TARGET rightwards until the next whitespace."
  (cons (car target)
        (cursorfree--skip-forward-from (cdr target) "^[:space:]\n")))

(defun cursorfree-paint (target)
  "Expand TARGET leftwards and rightwards until the next whitespace."
  (cursorfree-paint-right (cursorfree-paint-left target)))

(defun cursorfree-trim (target)
  "Shrink TARGET until there is no whitespace to the left or right."
  (cons (cursorfree--skip-forward-from (car target) "[:space:]\n")
        (cursorfree--skip-backward-from (cdr target) "[:space:]\n")))

(defun cursorfree-inner-parenthesis (delimiter target)
  "Expand TARGET to fill the insides of DELIMITER.

This function will match parentheses and quotation marks to the
left and right."
  (save-excursion
    ;; evil-inner-double-quote uses the location of point for the
    ;; expansion.  Put point at the beginning of the region.
    (goto-char (car target))
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
      (cursorfree--markify-region (cons (car expanded) (cadr expanded))))))

(defun cursorfree-outer-parenthesis (delimiter target)
  "Expand TARGET to contain the closest DELIMITER.

This function will match parentheses and quotation marks to the
left and right."
  (save-excursion
    ;; evil-outer-double-quote uses the location of point for the
    ;; expansion.  Put point at the beginning of the region.
    (goto-char (car target))
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
      (cursorfree--markify-region (cons (car expanded) (cadr expanded))))))

(defun cursorfree-inner-parenthesis-any (target)
  "Expand TARGET to fill the insides of the closest delimiters.

This function tries different parentheses and quotations to
figure out whichever is closest."
  (-max-by (-on #'> #'car)
           ;; Filter out whenever the evil-inner-*-quote messes up the
           ;; region (it selects the next region if not currently in a
           ;; quote)
           (--filter (<= (car it) (car target))
                     (--keep (condition-case nil
                                 (cursorfree-inner-parenthesis it target)
                               (error nil))
                             '(?< ?{ ?\( ?\[ ?\" ?\' ?\`)))))

(defun cursorfree-outer-parenthesis-any (target)
  "Expand TARGET to contain the closest delimiters.

This function tries different parentheses and quotations to
figure out whichever is closest."
  (-max-by (-on #'> #'car)
           ;; Filter out whenever the evil-inner-*-quote messes up the
           ;; region (it selects the next region if not currently in a
           ;; quote)
           (--filter (<= (car it) (car target))
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
  (let* ((head (cursorfree--peek-value environment)))
    (funcall (if (characterp head)
                 (cursorfree-to-modifier #'cursorfree-inner-parenthesis)
               (cursorfree-to-modifier #'cursorfree-inner-parenthesis-any))
             environment)))

(defun cursorfree-outer-parenthesis-dwim (environment)
  "Expand a target to contain of some delimiter.

This will read the top value of ENVIRONMENT.  If this is a
character, the next element is assumed to be a target to be
expanded until the delimiter given by the character.  Otherwise,
assumes that the top element was a target and expands it to the
nearest matching pairs of delimiters."
  (let* ((head (cursorfree--peek-value environment)))
    (funcall (if (characterp head)
                 (cursorfree-to-modifier #'cursorfree-outer-parenthesis)
               (cursorfree-to-modifier #'cursorfree-outer-parenthesis-any))
             environment)))

(defun cursorfree--targets-join (targets)
  "Return the smallest target that can fit all TARGETS."
  (cursorfree--markify-region
   (cons (apply #'min (mapcar #'car targets))
         (apply #'max (mapcar #'cdr targets)))))

(defun cursorfree-past (target1 target2)
  "Return the smallest target that can fit TARGET1 and TARGET2."
  (cursorfree--targets-join (list target1 target2)))

(defun cursorfree--make-infix (instruction)
  "Return INSTRUCTION as an infix function.

Upon evaluation, this inserts INSTRUCTION under the top
instruction of the instruction stack."
  (lambda (environment)
    (let* ((e (cursorfree--clone-environment environment))
           (next-instruction (cursorfree--pop-instruction e)))
      (cursorfree--push-instruction e instruction)
      (cursorfree--push-instruction e next-instruction)
      e)))

(defun cursorfree-current-selection ()
  "Return the active region as a target."
  (cursorfree--markify-region
   (region-bounds)))

(defun cursorfree-thing-to-modifier (thing)
  "Translate THING to an instruction extending a target to THING.

The extension is done from the beginning of the target.  See
`bounds-of-thing-at-point' for more information about the builtin
thing-at-point functionalities."
  (cursorfree-to-modifier
   (lambda (target)
     (cursorfree--bounds-of-thing-at thing (car target)))))

(defun cursorfree-everything ()
  "Return a target referring to the full content of the buffer.

This function respects narrowing."
  (cursorfree--markify-region
   (cons (point-min) (point-max))))

(defun cursorfree-line-right (target)
  "Extend TARGET to include the next newline."
  (save-excursion
    (goto-char (cdr target))
    (unless (search-forward "\n" nil t)
      (goto-char (point-max)))
    (cursorfree--markify-region (cons (car target) (point)))))

(defun cursorfree-line-left (target)
  "Extend TARGET to start after the previous newline."
  (save-excursion
    (goto-char (car target))
    (if (search-backward "\n" nil t)
        (forward-char) ; Jump over the searched for newline
      (goto-char (point-min)))
    (cursorfree--markify-region (cons (point) (cdr target)))))

(defun cursorfree-line (target)
  "Extend TARGET to fill the full line."
  (cursorfree-line-left (cursorfree-line-right target)))

(defvar cursorfree-modifiers
  `(("paint" . ,(cursorfree-to-modifier #'cursorfree-paint))
    ("leftpaint" . ,(cursorfree-to-modifier #'cursorfree-paint-left))
    ("rightpaint" . ,(cursorfree-to-modifier #'cursorfree-paint-right))
    ("trim" . ,(cursorfree-to-modifier #'cursorfree-trim))
    ("past" . ,(cursorfree-to-modifier #'cursorfree-past))
    ("selection" . ,(cursorfree-to-modifier #'cursorfree-current-selection))
    ("inside" . cursorfree-inner-parenthesis-dwim)
    ("outside" . cursorfree-outer-parenthesis-dwim)
    ("line" . ,(cursorfree-to-modifier #'cursorfree-line))
    ("rightline" . ,(cursorfree-to-modifier #'cursorfree-line-right))
    ("leftline" . ,(cursorfree-to-modifier #'cursorfree-line-left))
    ("block" . ,(cursorfree-thing-to-modifier 'paragraph))
    ("link" . ,(cursorfree-thing-to-modifier 'url))
    ("sentence" . ,(cursorfree-thing-to-modifier 'sentence))
    ("everything" . ,(cursorfree-to-modifier #'cursorfree-everything))))

;;; cursorfree.el ends soon
(provide 'cursorfree)
;;; cursorfree.el ends here
