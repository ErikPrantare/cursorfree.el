;;; cursorfree-phony.el --- Phony bindings for cursorfree  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.3.0
;; Homepage: https://github.com/ErikPrantare/cursorfree.el
;; Package-Requires: ((emacs "29.1") (phony "1.1.0"))

;; cursorfree-phony.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; cursorfree-phony.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides phony voice control rules for cursorfree.

;;; Code:

(require 'cursorfree)
(require 'phony)

(phony-module cursorfree "cursorfree")

(defvar cursorfree-phony--last-evaluation-result nil
  "The result of the last evaluated cursorfree command.")

(phony-define-open-rule cursorfree-modifier
  "A modifier.

Modifiers are functions with one optional parameter.  When
invoked, it should produce a target.")

(phony-defun cursorfree-target
    ;; The element matching is unstructured to avoid grammar inflation
    ;; from inlined rules.  Otherwise, e.g. (<a> "past" <a>) as a
    ;; separate alternative would inline <a> twice.  This issue
    ;; occurred with talon in [2026-03-21 Sat].  Targets are used
    ;; everywhere, so we cannot afford the inlining of targets to be
    ;; expensive.

    ;; Our encoded syntax is:

    ;; target -> initial (* noninitial)
    ;; initial -> (| primitive modifier infix)
    ;; noninitial -> (| (seq infix primitive) (seq (? infix) modifier))

    ;; Inlining will double the occurrence of primitives and
    ;; modifiers, and triple for infix.  This is better than using the
    ;; enforced syntax, which has 2 infixes, 2 primitives, and 6
    ;; modifiers.  Modifiers can become very complex in syntax
    ;; compared to primitives, and especially infixes, so we want to
    ;; minimize the amount of inlined modifiers.
    ((initial cursorfree--initial-target-element)
     (* (rest cursorfree--noninitial-target-element)))
  "Top level rule for matching an arbitrary target.

The target is specified by a sequence SPEC of elements.

SPEC must conform to the following syntax:

  target -> (? infix) component (* infix component)
  component -> (| primitive modifier) (* modifier)

To extend the target grammar, add rule contributions to the rules
`cursorfree-primitive' and `cursorfree-modifier'.

INITIAL contributes one list of elements to SPEC, and REST multiple
lists of elements.  SPEC is computed as the concatenation of all these
lists.

Each element is a list (TYPE VALUE), with TYPE one of `primitive',
`modifier', or `infix'.  VALUE defines the operation of the element, and
is an arbitrary value if TYPE is `primitive', a function taking one
optional parameter if it is a `modifier', and a function accepting
one or two parameters if it is an `infix'.

In the following, VALUE returns the VALUE part of the element, and GET
returns VALUE if the element is a primitive, or invokes VALUE with no
parameters if it is a modifier.

The final value is computed as a sequence of reductions, with a variable
ACC collecting the target.  It initializes ACC depending on the first element:

- `primitive' or `modifier':
    ACC <- (GET (car SPEC))
    SPEC <- (cdr SPEC)
- `infix':
    ACC <- (funcall (VALUE (car SPEC)) (GET (cadr SPEC)))
    SPEC <- (cddr SPEC)

It then looks at the type of the first element of SPEC and applies the
corresponding rule until SPEC is empty:

- `infix':
    ACC <- (funcall (VALUE (car SPEC)) ACC (GET (cadr SPEC)))
    SPEC <- (cddr SPEC)
- `modifier'
    ACC <- (funcall (VALUE (car SPEC)) ACC)
    SPEC <- (cdr SPEC)

The syntax of the target ensures that a `primitive' can never occur as
an alternative during this iteration."
  ;; TODO: Test these semantics.
  :interactive nil
  (let ((target (cursorfree-phony--evaluate-target-elements
                 (apply #'append initial rest))))
    (setq cursorfree-phony--last-evaluation-result target)
    target))

(phony-define-open-rule cursorfree-primitive
  "A primitive.

A primitive is any arbitrary value.")

(phony-define-open-rule cursorfree--target-element
  "List of elements that may occur anywhere in a specification.")

(phony-define-open-rule cursorfree--initial-target-element
  "List of elements that may occur at the start of a specification."
  :alternatives (cursorfree--target-element))

(phony-define-open-rule cursorfree--noninitial-target-element
  "List of elements that may occur after the start of a specification."
  :alternatives (cursorfree--target-element))

(phony-defun cursorfree--primitive-element (cursorfree-primitive)
  "A list of one primitive element."
  :contributes-to cursorfree--initial-target-element
  (list (list 'primitive cursorfree-primitive)))

(phony-defun cursorfree--modifier-element (cursorfree-modifier)
  "A list of one modifier element."
  :contributes-to cursorfree--initial-target-element
  (list (list 'modifier cursorfree-modifier)))

(phony-define-open-rule cursorfree--infix-element
  "A list of one infix element."
  :contributes-to cursorfree--initial-target-element)

(phony-defun cursorfree--infix-element-past "past"
  "Infix element for `cursorfree-past'."
  :contributes-to cursorfree--infix-element
  '((infix cursorfree-past)))

(phony-defun cursorfree--infix-element-and "and"
  "Infix element putting targets into a `cursorfree--parallel'.

The targets sent to the infix function are transformed with
`cursorfree--ensure-parallel' before they are concatenated."
  :contributes-to cursorfree--infix-element
  `((infix ,(lambda (&rest targets)
              (apply #'seq-concatenate
                     'cursorfree--parallel
                     (seq-map #'cursorfree--ensure-parallel targets))))))

(phony-defun cursorfree--infix-primitive ((infix cursorfree--infix-element)
                                          (primitive cursorfree--primitive-element))
  "List of an INFIX element followed by PRIMITIVE element."
  :contributes-to cursorfree--target-element
  (append infix primitive))

(phony-defun cursorfree--maybe-infix-modifier ((? (infix cursorfree--infix-element))
                                               (modifier cursorfree--modifier-element))
  "List of an INFIX element followed by MODIFIER element."
  :contributes-to cursorfree--noninitial-target-element
  (append infix modifier))

(defun cursorfree-phony--evaluate-target-elements (elements)
  "Evaluate ELEMENTS to get a target.

See the rule `cursorfree-target' for the semantics of ELEMENTS."
  (pcase elements
    (`((primitive ,c)) c)
    (`((modifier ,m) . ,xs)
     (cursorfree-phony--evaluate-target-elements
      (cons `(primitive ,(funcall m)) xs)))
    (`((infix ,i) ,x . ,xs)
     (cursorfree-phony--evaluate-target-elements
      (cons `(primitive ,(funcall i (cursorfree-phony--evaluate-target-elements (list x))))
            xs)))
    (`((primitive ,c) (infix ,i) ,x . ,xs)
     (cursorfree-phony--evaluate-target-elements
      (cons `(primitive ,(funcall i c (cursorfree-phony--evaluate-target-elements (list x))))
            xs)))
    (`((primitive ,c) (modifier ,m) . ,xs)
     (cursorfree-phony--evaluate-target-elements
      (cons `(primitive ,(funcall m c)) xs)))
    (_ (error "Invalid target element sequence %S" elements))))

(phony-defun cursorfree--window ("split" (n digit))
  "Window numbered N by N."
  :contributes-to cursorfree-primitive
  (if (and (boundp 'winum-mode)
           winum-mode
           ;; To silence byte-compiler
           (fboundp 'winum-get-window-by-number))
      (winum-get-window-by-number n)
    (user-error "Install winum to make use of numbered windows")))

(phony-defun cursorfree--word ("word" (word word))
  "A WORD."
  :contributes-to cursorfree-primitive
  word)

(phony-defun cursorfree--number ("numb" (n number))
  "A number N."
  :contributes-to cursorfree-primitive
  n)

(phony-defun cursorfree--character ("car" (c any-alphanumeric-key))
  "A character C, given as a string."
  :contributes-to cursorfree-primitive
  (string c))

(phony-define-dictionary cursorfree--procedural-primitive
  "Primitive computed through a function call."
  `(("clip" . cursorfree-kill-ring)
    ("primary" . cursorfree-primary-selection)
    ("that" . ,(lambda () cursorfree--target-that))
    ("source" . ,(lambda () cursorfree--target-source))
    ("its" . ,(lambda () cursorfree-phony--last-evaluation-result))
    ("itself" . ,(lambda () cursorfree-phony--last-evaluation-result))))

(phony-defun cursorfree--resolved-procedural-primitive
    ((primitive cursorfree--procedural-primitive))
  "The result of invoking PRIMITIVE."
  :contributes-to cursorfree-primitive
  (funcall primitive))

(phony-define-dictionary cursorfree--color
  "Color used for `hatty'."
  '(("squash" . yellow)
    ("red" . red)
    ("blue" .  blue)
    ("pink" . pink)
    ("green" . green)))

(phony-define-dictionary cursorfree--shape
  "Shape used for `hatty'."
  '(("bolt" . bolt)
    ("curve" . curve)
    ("fox" . fox)
    ("frame" . frame)
    ("play" . play)
    ("wing" . wing)
    ("hole" . hole)
    ("ex" . ex)
    ("cross" . cross)
    ("glim" . eye)))

(phony-defun cursorfree--hat
    ((? (color cursorfree--color))
     (? (shape cursorfree--shape))
     (char any-alphanumeric-key))
  "Hatty token indexed by COLOR, SHAPE and CHAR.

See documentation for `hatty-locate-token' for more information."
  :contributes-to cursorfree-primitive
  (cursorfree--make-target-from-hat char color shape))

(phony-defun cursorfree--row ("row" (index number))
  "Line number INDEX modulo 100."
  :contributes-to cursorfree-primitive
  (cursorfree-row-modulo-100 index))

(phony-defun cursorfree--long-row ("long row" (index number))
  "Line number INDEX."
  :contributes-to cursorfree-primitive
  (cursorfree-row index))


(phony-define-dictionary cursorfree--destination-modifiers
  "Method for putting a target at a destination."
  `(("to" . nil)
    ("after" . after)
    ("before" . before)))

(phony-defun cursorfree--bring
    ("bring"
     (from cursorfree-target)
     (modifier cursorfree--destination-modifiers)
     (to cursorfree-target))
  "Bring FROM to TO.

Where to put it in relation to TO is given by MODIFIER."
  (cursorfree-do-bring from to modifier))

(phony-defun cursorfree--change
    ("change"
     (target cursorfree-target)
     (? "to" (source cursorfree-target)))
  "Change TARGET to SOURCE.

If SOURCE is not given, TARGET is removed and point is put in its place."
  (if source
      (cursorfree-do-bring source target)
    (cursorfree-do-change target)))

(phony-defun cursorfree--move
    ("move"
     (from cursorfree-target)
     (modifier cursorfree--destination-modifiers)
     (to cursorfree-target))
  "Move FROM to TO.

Where to put it in relation to TO is given by MODIFIER."
  (cursorfree-do-move from to modifier))

(phony-defun cursorfree--swap
    ("swap"
     (from cursorfree-target)
     "with"
     (to cursorfree-target))
  "Swap FROM with TO."
  (cursorfree-swap from to))

(phony-define-open-rule cursorfree-wrapper
  "Function ofthe one argument used to wrap a target with something.")

(phony-defun cursorfree--character-wrapper ((? "car") (character symbol-key))
  "A wrapper that wraps with CHARACTER."
  :contributes-to cursorfree-wrapper
  (apply-partially #'cursorfree-wrap character))

(phony-defun cursorfree--wrap ("wrap" (? (target cursorfree-target)) "with" (wrapper cursorfree-wrapper))
  "Wrap TARGET with WRAPPER.

If TARGET is not given, `cursorfree-this' is used."
  (setq target (or target (cursorfree-this)))
  (funcall wrapper target)
  (setq cursorfree--target-that target))

(phony-defun cursorfree--unwrap ("unwrap" (target cursorfree-target))
  "Remove parentheses or quotation around TARGET."
  (cursorfree-unwrap target)
  (setq cursorfree--target-that target))

(phony-defun cursorfree--rewrap ("rewrap" (? (target cursorfree-target)) "with" (character symbol-key))
  "Replace parentheses or quotation around TARGET with CHARACTER.

If TARGET is not given, `cursorfree-this' is used."
  (setq target (or target (cursorfree-this)))
  (cursorfree-rewrap character target)
  (setq cursorfree--target-that target))

(phony-defun cursorfree--occur
    ("hunt"
     (target cursorfree-target)
     (? "in" (extent cursorfree-target))
     (? "context" (context-lines number)))
  "List occurrences of TARGET.

If EXTENT is given, restrict the search to EXTENT.  If
CONTEXT-LINES is given, show that many lines of context."
  (cursorfree-occur target extent context-lines))

;; We prefer to factor out the verbs of the simple actions, as this
;; avoids unnecessary inlining of cursorfree-target by the speech
;; engine.
(phony-define-dictionary cursorfree--simple-action-verb
  "Verb for a simple single-target action."
  '(("take" . cursorfree-select)
    ("copy" . cursorfree-do-copy)
    ("chuck" . cursorfree-chuck)
    ("bring" . cursorfree-do-bring)
    ("move" . cursorfree-do-move)
    ("clone" . cursorfree-clone)
    ("jump" . cursorfree-jump)
    ("pre" . cursorfree-jump-beginning)
    ("post" . cursorfree-jump-end)
    ("comment" . cursorfree-make-comment)
    ("uncomment" . cursorfree-uncomment)
    ("indent" . cursorfree-indent)
    ("narrow" . cursorfree-narrow)
    ("title" . cursorfree-capitalize)
    ("upcase" . cursorfree-upcase)
    ("downcase" . cursorfree-downcase)
    ("crown" . cursorfree-crown)
    ("center" . cursorfree-center)
    ("bottom" . cursorfree-bottom)
    ("pick" . cursorfree-pick)
    ("fuse" . cursorfree-fuse)
    ("tidy" . cursorfree-tidy)
    ("join" . cursorfree-join)
    ("break" . cursorfree-break)
    ("flash" . cursorfree-pulse)
    ("help" . cursorfree-help)
    ("drink" . cursorfree-drink)
    ("pour" . cursorfree-pour)
    ("drop" . cursorfree-drop)
    ("float" . cursorfree-float)
    ("puff" . cursorfree-puff)))

(phony-defun cursorfree--simple-action
    ((verb cursorfree--simple-action-verb)
     (target cursorfree-target))
  "Invoke VERB on TARGET."
  (funcall verb target))

(phony-defun cursorfree--outside ("outside" (? (delimiter any-alphanumeric-key)))
  "Modifier selecting outside of DELIMITER."
  :contributes-to cursorfree-modifier
  (lambda (&optional target)
    (cursorfree-outside target delimiter)))

(phony-defun cursorfree--inside ("inside" (? (delimiter any-alphanumeric-key)))
  "Modifier selecting inside of DELIMITER."
  :contributes-to cursorfree-modifier
  (lambda (&optional target)
    (cursorfree-inside target delimiter)))

(phony-define-dictionary cursorfree--simple-modifier
  "Modifier taking a single optional target."
  :contributes-to cursorfree-modifier
  '(("paint" . cursorfree-paint)
    ("leftpaint" . cursorfree-paint-left)
    ("rightpaint" . cursorfree-paint-right)
    ("trim" . cursorfree-trim)
    ("line" . cursorfree-line)
    ("tail" . cursorfree-line-right)
    ("head" . cursorfree-line-left)
    ("block" . cursorfree-block)
    ("token" . cursorfree-token)
    ("comment" . cursorfree-comment)
    ("string" . cursorfree-string-literal)
    ("everything" . cursorfree-everything)
    ("visible" . cursorfree-visible)
    ("this" . cursorfree-this)
    ("every instance" . cursorfree-every-instance)
    ("next" . cursorfree-next)
    ("preve" . cursorfree-previous)
    ("beginning" . cursorfree-beginning)
    ("end" . cursorfree-end)
    ("buffer" . cursorfree-buffer)
    ("split" . cursorfree-window-or-selected)
    ("word" . cursorfree-word)
    ("symbol" . cursorfree-symbol)
    ("sentence" . cursorfree-sentence)))

(provide 'cursorfree-phony)
;;; cursorfree-phony.el ends here
