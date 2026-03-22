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

(phony-define-open-rule cursorfree--initial-instruction)
(phony-define-open-rule cursorfree--noninitial-instruction)

(phony-define-open-rule cursorfree--instruction
  :contributes-to (cursorfree--initial-instruction
                   cursorfree--noninitial-instruction))

(phony-define-open-rule cursorfree-modifier)

(phony-defun cursorfree--modifier-instruction ((modifier cursorfree-modifier))
  :export nil
  :contributes-to cursorfree--instruction
  (cursorfree-make-modifier modifier))

(phony-defun cursorfree-target
    ((initial cursorfree--initial-instruction)
     (* (rest cursorfree--noninitial-instruction)))
  :export nil
  (let ((target (cursorfree--normalize-target
                 (cursorfree-evaluate (cons initial rest)))))
    target))

(phony-defun cursorfree-content ((target cursorfree-target))
  :export nil
  (cursorfree-target-get target))

(phony-defun cursorfree-region ((target cursorfree-target))
  :export nil
  (cursorfree-content-region target))

(phony-define-open-rule cursorfree-constant
  "Simple rules that provide some value when evaluated.")

;; Two constant levels.  This stratification means that compound
;; constants cannot refer to other compound constants, as recursion is
;; not allowed.  Without this stratification, constants would not be
;; able to refer to other constants at all.
;;
;; The compound constant rule is unstructured to avoid grammar
;; inflation from inlined rules.  Otherwise, (<a> "past" <a>) as a
;; separate alternative would in line <a> twice more.  This issue
;; occurred with talon in [2026-03-21 Sat].
(phony-define-open-rule cursorfree--compound-constant-element)

(phony-defun cursorfree--constant-element (cursorfree-constant)
  :export nil
  :contributes-to cursorfree--compound-constant-element
  (list 'constant cursorfree-constant))

(phony-defun cursorfree--modifier-element (cursorfree-modifier)
  :export nil
  :contributes-to cursorfree--compound-constant-element
  (list 'modifier cursorfree-modifier))

(phony-defun cursorfree--past-element "past"
  :export nil
  :contributes-to cursorfree--compound-constant-element
  '(infix cursorfree-past))

(defun cursorfree--evaluate-compound-constant (elements)
  (pcase elements
    (`((constant ,c)) c)
    (`((modifier ,m) . ,xs)
     (cursorfree--evaluate-compound-constant
      (cons `(constant ,(funcall m)) xs)))
    (`((infix ,_) . ,_)
     (cursorfree--evaluate-compound-constant
      (cons `(constant ,(cursorfree-this)) elements)))
    (`((constant ,c) (modifier ,m) . ,xs)
     (cursorfree--evaluate-compound-constant
      (cons `(constant ,(funcall m c)) xs)))
    (`((constant ,c) (infix ,i) . ,xs)
     (funcall i c (cursorfree--evaluate-compound-constant xs)))
    (_ (error "Invalid compound constant sequence."))))

(phony-defun cursorfree-compound-constant
    ((+ (elements cursorfree--compound-constant-element)))
  :export nil
  (cursorfree--evaluate-compound-constant elements))

(phony-defun cursorfree--initial-constant ((target cursorfree-compound-constant))
  :export nil
  :contributes-to cursorfree--initial-instruction
  (cursorfree--pusher target))

(phony-defun cursorfree--noninitial-constant ("and" (target cursorfree-compound-constant))
  :export nil
  :contributes-to cursorfree--noninitial-instruction
  (cursorfree--pusher target))

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

(defmacro cursorfree--define-simple-action (name utterance &rest arguments)
  "

\(fn NAME UTTERANCE [OPTIONS...] FUNCTION)"
  (declare (indent defun))
  (let ((options (butlast arguments))
        (function (car (last arguments))))
    `(phony-defun ,(intern (concat "cursorfree-" (symbol-name name) "-simple-action"))
       (,utterance
        ,(if (plist-get options :optional-argument)
             '(? (target cursorfree-target))
           '(target cursorfree-target)))
       (if target
           (funcall ,function
                    ,(if (plist-get options :use-target-content)
                         '(cursorfree-target-get target)
                       'target))
         (funcall ,function)))))

(cursorfree--define-simple-action take "take"
  #'cursorfree-target-select)
(cursorfree--define-simple-action copy "copy"
  #'cursorfree-target-copy)
(cursorfree--define-simple-action chuck "chuck"
  #'cursorfree-target-chuck)
(cursorfree--define-simple-action bring "bring"
  #'cursorfree-target-bring)
(cursorfree--define-simple-action move "move"
  #'cursorfree-target-move)
(cursorfree--define-simple-action clone "clone"
  #'cursorfree-target-clone)
(cursorfree--define-simple-action jump "jump"
  #'cursorfree-target-jump)
(cursorfree--define-simple-action pre "pre"
  #'cursorfree-target-jump-beginning)
(cursorfree--define-simple-action post "post"
  #'cursorfree-target-jump-end)
(cursorfree--define-simple-action comment "comment"
  #'cursorfree-target-comment)
(cursorfree--define-simple-action uncomment "uncomment"
  #'cursorfree-target-uncomment)
(cursorfree--define-simple-action indent "indent"
  #'cursorfree-target-indent)
(cursorfree--define-simple-action narrow "narrow"
  #'cursorfree-target-narrow)
(cursorfree--define-simple-action title "title"
  #'cursorfree-target-capitalize)
(cursorfree--define-simple-action upcase "upcase"
  #'cursorfree-target-upcase)
(cursorfree--define-simple-action downcase "downcase"
  #'cursorfree-target-downcase)
(cursorfree--define-simple-action crown "crown"
  :optional-argument t
  #'cursorfree-target-crown)
(cursorfree--define-simple-action center "center"
  :optional-argument t
  #'cursorfree-target-center)
(cursorfree--define-simple-action bottom "bottom"
  :optional-argument t
  #'cursorfree-target-bottom)
(cursorfree--define-simple-action pick "pick"
  :optional-argument t
  #'cursorfree-target-pick)
(cursorfree--define-simple-action fuse "fuse"
  #'cursorfree-target-fuse)
(cursorfree--define-simple-action fill "filler"
  #'cursorfree-target-fill)
(cursorfree--define-simple-action join "join"
  #'cursorfree-target-join)
(cursorfree--define-simple-action break "break"
  #'cursorfree-target-break)
(cursorfree--define-simple-action flash "flash"
  #'cursorfree-target-pulse)
(cursorfree--define-simple-action help "help"
  #'cursorfree-target-help)
(cursorfree--define-simple-action drink "drink"
  #'cursorfree-target-drink)
(cursorfree--define-simple-action pour "pour"
  #'cursorfree-target-pour)
(cursorfree--define-simple-action drop "drop"
  #'cursorfree-target-drop)
(cursorfree--define-simple-action float "float"
  #'cursorfree-target-float)
(cursorfree--define-simple-action puff "puff"
  #'cursorfree-target-puff)


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

(defmacro cursorfree--define-simple-modifier (name utterance function)
  (declare (indent defun))
  `(phony-defun ,(intern (concat "cursorfree-" (symbol-name name) "-simple"))
       ,utterance
     :contributes-to cursorfree-modifier
     :export nil
     (apply-partially ,function)))

(cursorfree--define-simple-modifier paint "paint"
  #'cursorfree-paint)
(cursorfree--define-simple-modifier leftpaint "leftpaint"
  #'cursorfree-paint-left)
(cursorfree--define-simple-modifier rightpaint "rightpaint"
  #'cursorfree-paint-right)
(cursorfree--define-simple-modifier trim "trim"
  #'cursorfree-trim)
;; Sunset standalone "past"?
(cursorfree--define-simple-modifier past "past"
  #'cursorfree-past)
(cursorfree--define-simple-modifier selection "selection"
  #'cursorfree-current-selection)
(cursorfree--define-simple-modifier line "line"
  #'cursorfree-line)
(cursorfree--define-simple-modifier tail "tail"
  #'cursorfree-line-right)
(cursorfree--define-simple-modifier head "head"
  #'cursorfree-line-left)
(cursorfree--define-simple-modifier block "block"
  #'cursorfree-block)
(cursorfree--define-simple-modifier token "token"
  #'cursorfree-token)
(cursorfree--define-simple-modifier comment "comment"
  #'cursorfree-comment)
(cursorfree--define-simple-modifier string "string"
  #'cursorfree-string-literal)
(cursorfree--define-simple-modifier everything "everything"
  #'cursorfree-everything)
(cursorfree--define-simple-modifier visible "visible"
  #'cursorfree-visible)
(cursorfree--define-simple-modifier this "this"
  #'cursorfree-this)
(cursorfree--define-simple-modifier every-instance "every instance"
  #'cursorfree-every-instance)
(cursorfree--define-simple-modifier clip "clip"
  #'cursorfree-kill-ring)
(cursorfree--define-simple-modifier primary "primary"
  #'cursorfree-primary-selection)
(cursorfree--define-simple-modifier next "next"
  #'cursorfree-next)
(cursorfree--define-simple-modifier preve "preve"
  #'cursorfree-previous)
(cursorfree--define-simple-modifier beginning "beginning"
  #'cursorfree-beginning)
(cursorfree--define-simple-modifier end "end"
  #'cursorfree-end)
(cursorfree--define-simple-modifier buffer "buffer"
  #'cursorfree-buffer)
(cursorfree--define-simple-modifier split "split"
  #'cursorfree-window-or-selected)

;; TODO add sentence, link.  Or wait until I move scopes here as well.

(provide 'cursorfree-phony)
;;; cursorfree-phony.el ends here
