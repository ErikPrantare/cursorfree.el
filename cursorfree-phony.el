;;; cursorfree-phony.el --- Phony bindings for cursorfree  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Package-Requires: ((phony "0.3.0"))

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

;;

;;; Code:

(require 'phony)

(phony-module cursorfree "cursorfree")

(defvar cursorfree--last-evaluation-result nil
  "The result of the last evaluated cursorfree command.")

(phony-define-open-rule cursorfree-modifier
  "Rules that produce a modifier.

Modifiers are functions that take a target as an argument, and return a
target as output.  The target argument is allowed to be optional.")

(phony-defun cursorfree-target
    ;; The element matching is unstructured to avoid grammar inflation
    ;; from inlined rules.  Otherwise, e.g. (<a> "past" <a>) as a
    ;; separate alternative would inline <a> twice more.  This issue
    ;; occurred with talon in [2026-03-21 Sat].  Targets are used
    ;; everywhere, so we cannot afford the inlining of targets to be
    ;; expensive.

    ;; The general problem that we still need to overcome is that a
    ;; primitive, unless it is the first element, needs to be preceded
    ;; by an infix operator.  That is why we still need to
    ;; differentiate between initial elements and noninitial elements.
    ;; They are equivalent, except that noninitial primitives always
    ;; occur as <infix> <primitive>.  Otherwise, <char1> <char2> could
    ;; be interpreted both as two elements, or as element <char1>
    ;; followed by a separate keypress command <char2>.  The second
    ;; form is what is generally intended.

    ;; Current syntax:

    ;; S -> initial noninitial*
    ;; initial -> primitive | modifier | infix
    ;; noninitial -> infix primitive | modifier | infix

    ;; Note how inlining will still double the occurrence of each
    ;; rule, or triple for infix.  This is still better than being a
    ;; bit more precise precise in the syntax structure, which'd be
    ;; something like:

    ;; S -> infix? seq (infix seq)*
    ;; seq -> primitive modifier* | modifier modifier*

    ;; which has 2 infixes, 2 primitives, and 6 (!) modifiers.
    ;; Modifiers can become very complex in syntax compared to
    ;; primitives and infixes, so we want to minimize the amount of
    ;; inlined modifiers.  If inlining was no problem, we would
    ;; probably want an even more complex structure than this.

    ;; TODO: Clarify the "real" syntax.  The current interpretation is
    ;; a bit "ad-hoc".  It works for most cases though.  The real
    ;; syntax could be used to re-parse the less precise one.
    ((initial cursorfree--initial-target-element)
     (* (rest cursorfree--noninitial-target-element)))
  "Top level rule for matching an arbitrary target."
  :interactive nil
  (let ((target (cursorfree--evaluate-target-elements
                 (apply #'append initial rest))))
    (setq cursorfree--last-evaluation-result target)
    target))

(phony-define-open-rule cursorfree-primitive
  "Simple rules that provide some value when evaluated.")

(phony-define-open-rule cursorfree--target-element)

(phony-define-open-rule cursorfree--initial-target-element
  :alternatives (cursorfree--target-element))

(phony-define-open-rule cursorfree--noninitial-target-element
  :alternatives (cursorfree--target-element))

(phony-defun cursorfree--primitive-element (cursorfree-primitive)
  :interactive nil
  :contributes-to cursorfree--initial-target-element
  (list (list 'primitive cursorfree-primitive)))

(phony-defun cursorfree--modifier-element (cursorfree-modifier)
  :interactive nil
  :contributes-to cursorfree--target-element
  (list (list 'modifier cursorfree-modifier)))

(phony-define-open-rule cursorfree--infix-element
  :contributes-to cursorfree--target-element)

(phony-defun cursorfree--infix-element-past "past"
  :interactive nil
  :contributes-to cursorfree--infix-element
  '((infix cursorfree-past)))

(phony-defun cursorfree--infix-element-and "and"
  :interactive nil
  :contributes-to cursorfree--infix-element
  `((infix ,(lambda (&rest targets)
              (apply #'seq-concatenate
                     'cursorfree--parallel
                     (seq-map #'cursorfree--ensure-parallel targets))))))

(phony-defun cursorfree--infix-primitive ((infix cursorfree--infix-element)
                                          (primitive cursorfree--primitive-element))
  :contributes-to cursorfree--target-element
  (append infix primitive))

(defun cursorfree--evaluate-target-elements (elements)
  (pcase elements
    (`((primitive ,c)) c)
    (`((modifier ,m) . ,xs)
     (cursorfree--evaluate-target-elements
      (cons `(primitive ,(funcall m)) xs)))
    (`((infix ,i) ,x . ,xs)
     (cursorfree--evaluate-target-elements
      (cons `(primitive ,(funcall i (cursorfree--evaluate-target-elements (list x))))
            xs)))
    (`((primitive ,c) (infix ,i) ,x . ,xs)
     (cursorfree--evaluate-target-elements
      (cons `(primitive ,(funcall i c (cursorfree--evaluate-target-elements (list x))))
            xs)))
    (`((primitive ,c) (modifier ,m) . ,xs)
     (cursorfree--evaluate-target-elements
      (cons `(primitive ,(funcall m c)) xs)))
    (_ (error "Invalid target element sequence %S" elements))))

(phony-defun cursorfree--window ("split" (n digit))
  :interactive nil
  :contributes-to cursorfree-primitive
  (winum-get-window-by-number n))

(phony-defun cursorfree--word ("word" (word word))
  :interactive nil
  :contributes-to cursorfree-primitive
  word)

(phony-defun cursorfree--number ("numb" (n number))
  :interactive nil
  :contributes-to cursorfree-primitive
  n)

(phony-defun cursorfree--character ("car" (c any-alphanumeric-key))
  :interactive nil
  :contributes-to cursorfree-primitive
  (string c))

(phony-define-dictionary cursorfree--procedural-primitive
  `(("clip" . cursorfree-kill-ring)
    ("primary" . cursorfree-primary-selection)
    ("that" . ,(lambda () cursorfree--target-that))
    ("source" . ,(lambda () cursorfree--target-source))
    ("its" . ,(lambda () cursorfree--last-evaluation-result))
    ("itself" . ,(lambda () cursorfree--last-evaluation-result))))

(phony-defun cursorfree--resolved-procedural-primitive ((primitive cursorfree--procedural-primitive))
  :contributes-to cursorfree-primitive
  (funcall primitive))

(phony-define-dictionary cursorfree--color
  '(("squash" . yellow)
    ("red" . red)
    ("blue" .  blue)
    ("pink" . pink)
    ("green" . green)))

(phony-define-dictionary cursorfree--shape
  '(("bolt" . bolt)
    ("curve" . curve)
    ("fox" . fox)
    ("frame" . frame)
    ("play" . play)
    ("wing" . wing)
    ("hole" . hole)
    ("ex" . ex)
    ("cross" . cross)
    ("I" . eye)))

(phony-defun cursorfree--hat
    ((? (color cursorfree--color))
     (? (shape cursorfree--shape))
     (char any-alphanumeric-key))
  :interactive nil
  :contributes-to cursorfree-primitive
  (cursorfree--make-target-from-hat char color shape))

(phony-defun cursorfree--row ("row" (index number))
  :interactive nil
  :contributes-to cursorfree-primitive
  (cursorfree-row-modulo-100 index))

(phony-defun cursorfree--long-row ("long row" (index number))
  :interactive nil
  :contributes-to cursorfree-primitive
  (cursorfree-row index))


(phony-define-dictionary cursorfree--destination-modifiers
  `(("to" . cursorfree--put)
    ("after" . cursorfree--put-after)
    ("before" . cursorfree--put-before)))

(phony-defun cursorfree--bring
    ("bring"
     (from cursorfree-target)
     (modifier cursorfree--destination-modifiers)
     (to cursorfree-target))
  (cursorfree--target-bring from to :putter modifier))

(phony-defun cursorfree--change
    ("change"
     (target cursorfree-target)
     (? "to" (source cursorfree-target)))
  (if source
      (cursorfree-bring source target)
    (cursorfree-change target)))

(phony-defun cursorfree--move
    ("move"
     (from cursorfree-target)
     (modifier cursorfree--destination-modifiers)
     (to cursorfree-target))
  (cursorfree--target-move from to :putter modifier))

(phony-defun cursorfree--swap
    ("swap"
     (from cursorfree-target)
     "with"
     (to cursorfree-target))
  (cursorfree-swap from to))

(phony-define-open-rule cursorfree-wrapper)

(phony-defun character-wrapper ((? "car") (character symbol-key))
  :interactive nil
  :contributes-to cursorfree-wrapper
  (apply-partially #'cursorfree-wrap character))

(phony-defun cursorfree--wrap ("wrap" (? (target cursorfree-target)) "with" (wrapper cursorfree-wrapper))
  (setq target (or target (cursorfree-this)))
  (funcall wrapper target)
  (setq cursorfree--target-that target))

(phony-defun cursorfree--unwrap ("unwrap" (target cursorfree-target))
  (cursorfree-unwrap target)
  (setq cursorfree--target-that target))

(phony-defun cursorfree--rewrap ("rewrap" (? (target cursorfree-target)) "with" (character symbol-key))
  (setq target (or target (cursorfree-this)))
  (cursorfree-rewrap character target)
  (setq cursorfree--target-that target))

(phony-defun cursorfree--occur
    ("hunt"
     (target cursorfree-target)
     (? "in" (extent cursorfree-target))
     (? "context" (context-lines number)))
  (cursorfree-occur target extent context-lines))

;; We prefer to factor out the verbs of the simple actions, as this
;; avoids unnecessary inlining of cursorfree-target by the speech
;; engine.
(phony-define-dictionary cursorfree--simple-action-verb
  '(("take" . cursorfree-select)
    ("copy" . cursorfree-copy)
    ("chuck" . cursorfree-chuck)
    ("bring" . cursorfree-bring)
    ("move" . cursorfree-move)
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
  (funcall verb target))

(phony-defun cursorfree--outside ("outside" (? (delimiter any-alphanumeric-key)))
  :interactive nil
  :contributes-to cursorfree-modifier
  (lambda (&optional target)
    (cursorfree-outside target delimiter)))

(phony-defun cursorfree--inside ("inside" (? (delimiter any-alphanumeric-key)))
  :interactive nil
  :contributes-to cursorfree-modifier
  (lambda (&optional target)
    (cursorfree-inside target delimiter)))

(phony-define-dictionary cursorfree--simple-modifier
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
    ("sentence" . cursorfree-sentence)))

(provide 'cursorfree-phony)
;;; cursorfree-phony.el ends here
