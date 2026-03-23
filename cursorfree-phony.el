;;; cursorfree-phony.el --- Phony bindings for cursorfree  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(phony-module cursorfree "cursorfree")

(phony-define-open-rule cursorfree-modifier
  "Rules that produce a modifier.

Modifiers are functions that take a target as an argument, and return a
target as output.  The target argument is allowed to be optional.")

(phony-defun cursorfree-target
    ;; The element matching is unstructured to avoid grammar inflation
    ;; from inlined rules.  Otherwise, e.g. (<a> "past" <a>) as a
    ;; separate alternative would inline <a> twice more.  This issue
    ;; occurred with talon in [2026-03-21 Sat].
    ((+ (elements cursorfree--target-element)))
  "Top level rule for matching an arbitrary target."
  :export nil
  (cursorfree--evaluate-target-elements elements))

(phony-defun cursorfree-content ((target cursorfree-target))
  :export nil
  (cursorfree-target-get target))

(phony-defun cursorfree-region ((target cursorfree-target))
  :export nil
  (cursorfree-content-region target))

(phony-define-open-rule cursorfree-constant
  "Simple rules that provide some value when evaluated.")

(phony-define-open-rule cursorfree--target-element)

(phony-defun cursorfree--constant-element (cursorfree-constant)
  :export nil
  :contributes-to cursorfree--target-element
  (list 'constant cursorfree-constant))

(phony-defun cursorfree--modifier-element (cursorfree-modifier)
  :export nil
  :contributes-to cursorfree--target-element
  (list 'modifier cursorfree-modifier))

(phony-defun cursorfree--infix-element-past "past"
  :export nil
  :contributes-to cursorfree--target-element
  '(infix cursorfree-past))

(phony-defun cursorfree--infix-element-and "and"
  :export nil
  :contributes-to cursorfree--target-element
  'and)

(defun cursorfree--evaluate-target-elements (elements)
  (pcase elements
    (`((constant ,c)) c)
    (`((modifier ,m) . ,xs)
     (cursorfree--evaluate-target-elements
      (cons `(constant ,(funcall m)) xs)))
    (`((infix ,_) . ,_)
     (cursorfree--evaluate-target-elements
      (cons `(constant ,(cursorfree-this)) elements)))
    (`((constant ,c) (modifier ,m) . ,xs)
     (cursorfree--evaluate-target-elements
      (cons `(constant ,(funcall m c)) xs)))
    (`((constant ,c) (infix ,i) . ,xs)
     (funcall i c (cursorfree--evaluate-target-elements xs)))
    (`((constant ,c1) and (constant ,c2) . ,xs)
     (cursorfree--evaluate-target-elements
      `((constant ,(seq-concatenate
                     'cursorfree-parallel-target
                     (cursorfree--ensure-parallel c1)
                     (cursorfree--ensure-parallel c2)))
        . ,xs)))
    (_ (error "Invalid target element sequence."))))

(phony-defun cursorfree-window ("split" (n digit))
  :export nil
  :contributes-to cursorfree-constant
  (winum-get-window-by-number n))

(phony-defun cursorfree-word ("word" (word word))
  :export nil
  :contributes-to cursorfree-constant
  word)

(phony-defun cursorfree-number ("numb" (n number))
  :export nil
  :contributes-to cursorfree-constant
  n)

(phony-defun cursorfree-character ("car" (c any-alphanumeric-key))
  :export nil
  :contributes-to cursorfree-constant
  (string c))

(phony-defun cursorfree-that "that"
  :export nil
  :contributes-to cursorfree-constant
  cursorfree--target-that)

(phony-defun cursorfree-source "source"
  :export nil
  :contributes-to cursorfree-constant
  cursorfree--target-source)

(phony-defun cursorfree-its "its"
  :export nil
  :contributes-to cursorfree-constant
  (cursorfree--normalize-target cursorfree--last-evaluation-result))

(phony-defun cursorfree-itself "itself"
  :export nil
  :contributes-to cursorfree-constant
  (cursorfree--normalize-target cursorfree--last-evaluation-result))

(phony-define-dictionary cursorfree-color
  '(("squash" . yellow)
    ("red" . red)
    ("blue" .  blue)
    ("pink" . pink)
    ("green" . green)))

(phony-define-dictionary cursorfree-shape
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

(phony-defun cursorfree-hat
    ((? (color cursorfree-color))
     (? (shape cursorfree-shape))
     (char any-alphanumeric-key))
  :export nil
  :contributes-to cursorfree-constant
  (cursorfree--make-target-from-hat char color shape))

(phony-defun cursorfree-row ("row" (index number))
  :export nil
  :contributes-to cursorfree-constant
  (cursorfree-row-modulo-100 index))

(phony-defun cursorfree-long-row ("long row" (index number))
  :export nil
  :contributes-to cursorfree-constant
  (cursorfree-row index))


(phony-define-dictionary cursorfree-destination-modifiers
  `(("to" . cursorfree--put)
    ("after" . cursorfree--put-after)
    ("before" . cursorfree--put-before)))

(phony-defun cursorfree-bring
    ("bring"
     (from cursorfree-target)
     (modifier cursorfree-destination-modifiers)
     (to cursorfree-target))
  (cursorfree--target-bring from to :putter modifier))

(phony-defun cursorfree-change ("change" (target cursorfree-target) (? "to" (source cursorfree-target)))
  (if source
      (cursorfree-target-bring source target)
    (cursorfree-target-change target)))

(phony-defun cursorfree-move
    ("move"
     (from cursorfree-target)
     (modifier cursorfree-destination-modifiers)
     (to cursorfree-target))
  (cursorfree--target-move from to :putter modifier))

(phony-defun cursorfree-swap ("swap" (from cursorfree-target) "with" (to cursorfree-target))
  (cursorfree-target-swap from to))

(phony-define-open-rule cursorfree-wrapper)

(phony-defun character-wrapper ((? "car") (character symbol-key))
  :export nil
  :contributes-to cursorfree-wrapper
  (apply-partially #'cursorfree-target-wrap character))

(phony-defun cursorfree-wrap ("wrap" (? (target cursorfree-target)) "with" (wrapper cursorfree-wrapper))
  (setq target (or target (cursorfree-this)))
  (funcall wrapper target)
  (setq cursorfree--target-that target))

(phony-defun cursorfree-unwrap ("unwrap" (target cursorfree-target))
  (cursorfree-target-unwrap target)
  (setq cursorfree--target-that target))

(phony-defun cursorfree-rewrap ("rewrap" (? (target cursorfree-target)) "with" (character symbol-key))
  (setq target (or target (cursorfree-this)))
  (cursorfree-target-rewrap character target)
  (setq cursorfree--target-that target))

(phony-defun cursorfree-occur
    ("hunt"
     (target cursorfree-target)
     (? "in" (extent cursorfree-target))
     (? "context" (context-lines number)))
  (cursorfree-target-occur target extent context-lines))

;; We prefer to factor out the verbs of the simple actions, as this
;; avoids unnecessary inlining of cursorfree-target by the speech
;; engine.
(phony-define-dictionary cursorfree--simple-action-verb
  '(("take" . cursorfree-target-select)
    ("copy" . cursorfree-target-copy)
    ("chuck" . cursorfree-target-chuck)
    ("bring" . cursorfree-target-bring)
    ("move" . cursorfree-target-move)
    ("clone" . cursorfree-target-clone)
    ("jump" . cursorfree-target-jump)
    ("pre" . cursorfree-target-jump-beginning)
    ("post" . cursorfree-target-jump-end)
    ("comment" . cursorfree-target-comment)
    ("uncomment" . cursorfree-target-uncomment)
    ("indent" . cursorfree-target-indent)
    ("narrow" . cursorfree-target-narrow)
    ("title" . cursorfree-target-capitalize)
    ("upcase" . cursorfree-target-upcase)
    ("downcase" . cursorfree-target-downcase)
    ("crown" . cursorfree-target-crown)
    ("center" . cursorfree-target-center)
    ("bottom" . cursorfree-target-bottom)
    ("pick" . cursorfree-target-pick)
    ("fuse" . cursorfree-target-fuse)
    ("filler" . cursorfree-target-fill)
    ("join" . cursorfree-target-join)
    ("break" . cursorfree-target-break)
    ("flash" . cursorfree-target-pulse)
    ("help" . cursorfree-target-help)
    ("drink" . cursorfree-target-drink)
    ("pour" . cursorfree-target-pour)
    ("drop" . cursorfree-target-drop)
    ("float" . cursorfree-target-float)
    ("puff" . cursorfree-target-puff)))

(phony-defun cursorfree-simple-action
    ((verb cursorfree--simple-action-verb)
     (target cursorfree-target))
  (funcall (symbol-function verb) target))


(phony-defun cursorfree-phony-outside ("outside" (? (delimiter any-alphanumeric-key)))
  :export nil
  :contributes-to cursorfree-modifier
  (lambda (&optional target)
    (cursorfree-outside target delimiter)))

(phony-defun cursorfree-phony-inside ("inside" (? (delimiter any-alphanumeric-key)))
  :export nil
  :contributes-to cursorfree-modifier
  (lambda (&optional target)
    (cursorfree-inside target delimiter)))

(phony-define-dictionary cursorfree--simple-modifier
  :contributes-to cursorfree-modifier
  `(("paint" . ,#'cursorfree-paint)
    ("leftpaint" . ,#'cursorfree-paint-left)
    ("rightpaint" . ,#'cursorfree-paint-right)
    ("trim" . ,#'cursorfree-trim)
    ("past" . ,#'cursorfree-past)
    ("selection" . ,#'cursorfree-current-selection)
    ("line" . ,#'cursorfree-line)
    ("tail" . ,#'cursorfree-line-right)
    ("head" . ,#'cursorfree-line-left)
    ("block" . ,#'cursorfree-block)
    ("token" . ,#'cursorfree-token)
    ("comment" . ,#'cursorfree-comment)
    ("string" . ,#'cursorfree-string-literal)
    ("everything" . ,#'cursorfree-everything)
    ("visible" . ,#'cursorfree-visible)
    ("this" . ,#'cursorfree-this)
    ("every instance" . ,#'cursorfree-every-instance)
    ("clip" . ,#'cursorfree-kill-ring)
    ("primary" . ,#'cursorfree-primary-selection)
    ("next" . ,#'cursorfree-next)
    ("preve" . ,#'cursorfree-previous)
    ("beginning" . ,#'cursorfree-beginning)
    ("end" . ,#'cursorfree-end)
    ("buffer" . ,#'cursorfree-buffer)
    ("split" . ,#'cursorfree-window-or-selected)))

(provide 'cursorfree-phony)
;;; cursorfree-phony.el ends here
