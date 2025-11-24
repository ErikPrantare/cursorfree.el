;;; cursorfree-phony.el --- Phony bindings for cursorfree  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Erik Präntare

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

(phony-define-open-rule rule/cursorfree--initial-instruction)
(phony-define-open-rule rule/cursorfree--noninitial-instruction)

(phony-define-open-rule rule/cursorfree--instruction
  :contributes-to '(rule/cursorfree--initial-instruction
                    rule/cursorfree--noninitial-instruction))

(phony-define-open-rule rule/cursorfree-modifier)

(defun rule/cursorfree--modifier-instruction (modifier)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree--instruction
            (modifier rule/cursorfree-modifier)))
  (cursorfree-make-modifier modifier))

(defun rule/cursorfree-target (initial rest)
  (declare (phony-rule
            :export nil
            (initial rule/cursorfree--initial-instruction)
            (* (rest rule/cursorfree--noninitial-instruction))))
  (let ((target (cursorfree--normalize-target
                 (cursorfree-evaluate (cons initial rest)))))
    (setq my/cursorfree-it target)
    target))

(defun rule/cursorfree-content (target)
  (declare (phony-rule
            :export nil
            (target rule/cursorfree-target)))
  (cursorfree-target-get target))

(defun rule/cursorfree-region (target)
  (declare (phony-rule
            :export nil
            (target rule/cursorfree-target)))
  (cursorfree-content-region target))

;; Two constant levels.  This stratification means that compound
;; constants cannot refer to other compound constants, as recursion is
;; not allowed.  Without this stratification, constants would not be
;; able to refer to other constants at all.
(phony-define-open-rule rule/cursorfree-compound-constant)

(phony-define-open-rule rule/cursorfree-constant
  :contributes-to 'rule/cursorfree-compound-constant)

(defun rule/cursorfree--initial-constant (target)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree--initial-instruction
            (target rule/cursorfree-compound-constant)))
  (cursorfree--pusher target))

(defun rule/cursorfree--noninitial-constant (target)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree--noninitial-instruction
            "and"
            (target rule/cursorfree-compound-constant)))
  (cursorfree--pusher target))

(defun rule/cursorfree-window (n)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "split"
            (n rule/digit)))
  (winum-get-window-by-number n))

(defun rule/cursorfree-word (word)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "word"
            (word rule/word)))
  word)

(defun rule/cursorfree-number (n)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "numb"
            (n rule/number)))
  n)

(defun rule/cursorfree-character (c)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "car"
            (c rule/any-alphanumeric-key)))
  c)

(defun rule/cursorfree-that ()
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "that"))
  cursorfree--target-that)

(defun rule/cursorfree-source ()
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "source"))
  cursorfree--target-source)

(defun rule/cursorfree-its ()
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "its"))
  (cursorfree--normalize-target cursorfree--last-evaluation-result))

(defun rule/cursorfree-itself ()
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "itself"))
  (cursorfree--normalize-target cursorfree--last-evaluation-result))

(phony-define-dictionary rule/cursorfree-color
  '(("squash" . yellow)
    ("red" . red)
    ("blue" .  blue)
    ("pink" . pink)
    ("green" . green)))

(phony-define-dictionary rule/cursorfree-shape
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

(defun rule/cursorfree-hat (char &optional color shape)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            (? (color rule/cursorfree-color))
            (? (shape rule/cursorfree-shape))
            (char rule/any-alphanumeric-key)))
  (cursorfree--make-target-from-hat char color shape))

(defun rule/cursorfree--zeroary-modifier (modifier)
  "Return MODIFIER invoked with zero arguments.

This rule exists to allow `rule/cursorfree-range' to work with
modifiers which may take the place of a constant."
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            (modifier rule/cursorfree-modifier)))
  (funcall modifier))

(defun rule/cursorfree-range (from to)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-compound-constant
            (? (from rule/cursorfree-constant))
            "past"
            (to rule/cursorfree-constant)))
  (cursorfree-past (or from (cursorfree-this)) to))

(defun rule/cursorfree-row (index)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "row"
            (index rule/number)))
  (cursorfree-row-modulo-100 index))

(defun rule/cursorfree-long-row (index)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-constant
            "long row"
            (index rule/number)))
  (cursorfree-row index))


(phony-define-dictionary rule/cursorfree-destination-modifiers
  `(("to" . cursorfree--put)
    ("after" . cursorfree--put-after)
    ("before" . cursorfree--put-before)))

(defun rule/cursorfree-bring (from to modifier)
  (declare (phony-rule
            "bring"
            (from rule/cursorfree-target)
            (modifier rule/cursorfree-destination-modifiers)
            (to rule/cursorfree-target)))
  (cursorfree--target-bring from to :putter modifier))

(defun rule/cursorfree-change (target &optional source)
  (declare (phony-rule
            "change"
            (target rule/cursorfree-target)
            (? "to"
               (source rule/cursorfree-target))))
  (if source
      (cursorfree-target-bring source target)
    (cursorfree-target-change target)))

(defun rule/cursorfree-move (from to modifier)
  (declare (phony-rule
            "move"
            (from rule/cursorfree-target)
            (modifier rule/cursorfree-destination-modifiers)
            (to rule/cursorfree-target)))
  (cursorfree--target-move from to :putter modifier))

(defun rule/cursorfree-swap (from to)
  (declare (phony-rule
            "swap"
            (from rule/cursorfree-target)
            "with"
            (to rule/cursorfree-target)))
  (cursorfree-target-swap from to))

(phony-define-open-rule rule/cursorfree-wrapper)

(defun rule/character-wrapper (character)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-wrapper
            (character rule/symbol-key)))
  (apply-partially #'cursorfree-target-wrap character))

(defun rule/cursorfree-wrap (wrapper &optional target)
  (declare (phony-rule
            "wrap"
            (? (target rule/cursorfree-target))
            "with"
            (wrapper rule/cursorfree-wrapper)))
  (setq target (or target (cursorfree-this)))
  (funcall wrapper target)
  (setq cursorfree--target-that target))

(defun rule/cursorfree-unwrap (target)
  (declare (phony-rule
            "unwrap"
            (target rule/cursorfree-target)))
  (cursorfree-target-unwrap target)
  (setq cursorfree--target-that target))

(defun rule/cursorfree-rewrap (character &optional target)
  (declare (phony-rule
            "rewrap"
            (? (target rule/cursorfree-target))
            "with"
            (character rule/symbol-key)))
  (setq target (or target (cursorfree-this)))
  (cursorfree-target-rewrap character target)
  (setq cursorfree--target-that target))

(defun rule/cursorfree-occur (target &optional extent)
  (declare (phony-rule
            "hunt"
            (target rule/cursorfree-target)
            (? "in"
               (extent rule/cursorfree-target))))
  (cursorfree-target-occur target extent))

(defmacro rule/cursorfree--define-simple-action (name utterance &rest arguments)
  "

\(fn NAME UTTERANCE [OPTIONS...] FUNCTION)"
  (declare (indent defun))
  (let ((options (butlast arguments))
        (function (car (last arguments))))
    `(defun ,(intern (concat "rule/cursorfree-" (symbol-name name) "-simple")) (target)
       (declare (phony-rule
                 ,utterance
                 ,(if (plist-get options :optional-argument)
                      '(? (target rule/cursorfree-target))
                    '(target rule/cursorfree-target))))
       (if target
           (funcall ,function
                    ,(if (plist-get options :use-target-content)
                         '(cursorfree-target-get target)
                       'target))
         (funcall ,function)))))

(rule/cursorfree--define-simple-action take "take"
  #'cursorfree-target-select)
(rule/cursorfree--define-simple-action copy "copy"
  #'cursorfree-target-copy)
(rule/cursorfree--define-simple-action chuck "chuck"
  #'cursorfree-target-chuck)
(rule/cursorfree--define-simple-action bring "bring"
  #'cursorfree-target-bring)
(rule/cursorfree--define-simple-action move "move"
  #'cursorfree-target-move)
(rule/cursorfree--define-simple-action clone "clone"
  #'cursorfree-target-clone)
(rule/cursorfree--define-simple-action jump "jump"
  #'cursorfree-target-jump)
(rule/cursorfree--define-simple-action pre "pre"
  #'cursorfree-target-jump-beginning)
(rule/cursorfree--define-simple-action post "post"
  #'cursorfree-target-jump-end)
(rule/cursorfree--define-simple-action comment "comment"
  #'cursorfree-target-comment)
(rule/cursorfree--define-simple-action uncomment "uncomment"
  #'cursorfree-target-uncomment)
(rule/cursorfree--define-simple-action indent "indent"
  #'cursorfree-target-indent)
(rule/cursorfree--define-simple-action narrow "narrow"
  #'cursorfree-target-narrow)
(rule/cursorfree--define-simple-action title "title"
  #'cursorfree-target-capitalize)
(rule/cursorfree--define-simple-action upcase "upcase"
  #'cursorfree-target-upcase)
(rule/cursorfree--define-simple-action downcase "downcase"
  #'cursorfree-target-downcase)
(rule/cursorfree--define-simple-action crown "crown"
  :optional-argument t
  #'cursorfree-target-crown)
(rule/cursorfree--define-simple-action center "center"
  :optional-argument t
  #'cursorfree-target-center)
(rule/cursorfree--define-simple-action bottom "bottom"
  :optional-argument t
  #'cursorfree-target-bottom)
(rule/cursorfree--define-simple-action pick "pick"
  :optional-argument t
  #'cursorfree-target-pick)
(rule/cursorfree--define-simple-action fuse "fuse"
  #'cursorfree-target-fuse)
(rule/cursorfree--define-simple-action fill "filler"
  #'cursorfree-target-fill)
(rule/cursorfree--define-simple-action join "join"
  #'cursorfree-target-join)
(rule/cursorfree--define-simple-action break "break"
  #'cursorfree-target-break)
(rule/cursorfree--define-simple-action flash "flash"
  #'cursorfree-target-pulse)
(rule/cursorfree--define-simple-action help "help"
  #'cursorfree-target-help)
(rule/cursorfree--define-simple-action drink "drink"
  #'cursorfree-target-drink)
(rule/cursorfree--define-simple-action pour "pour"
  #'cursorfree-target-pour)
(rule/cursorfree--define-simple-action drop "drop"
  #'cursorfree-target-drop)
(rule/cursorfree--define-simple-action float "float"
  #'cursorfree-target-float)
(rule/cursorfree--define-simple-action puff "puff"
  #'cursorfree-target-puff)


(defun rule/cursorfree-phony-outside (&optional delimiter)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-modifier
            "outside"
            (? (delimiter rule/any-alphanumeric-key))))
  (lambda (&optional target)
    (cursorfree-outer-parenthesis-dwim target delimiter)))

(defun rule/cursorfree-phony-inside (&optional delimiter)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-modifier
            "inside"
            (? (delimiter rule/any-alphanumeric-key))))
  (lambda (&optional target)
    (cursorfree-inner-parenthesis-dwim target delimiter)))

(defmacro rule/cursorfree--define-simple-modifier (name utterance function)
  (declare (indent defun))
  `(defun ,(intern (concat "rule/cursorfree-" (symbol-name name) "-simple")) ()
     (declare (phony-rule
               :contributes-to 'rule/cursorfree-modifier
               :export nil
               ,utterance))
     (apply-partially ,function)))

(rule/cursorfree--define-simple-modifier paint "paint"
  #'cursorfree-paint)
(rule/cursorfree--define-simple-modifier leftpaint "leftpaint"
  #'cursorfree-paint-left)
(rule/cursorfree--define-simple-modifier rightpaint "rightpaint"
  #'cursorfree-paint-right)
(rule/cursorfree--define-simple-modifier trim "trim"
  #'cursorfree-trim)
;; Sunset standalone "past"?
(rule/cursorfree--define-simple-modifier past "past"
  #'cursorfree-past)
(rule/cursorfree--define-simple-modifier selection "selection"
  #'cursorfree-current-selection)
(rule/cursorfree--define-simple-modifier line "line"
  #'cursorfree-line)
(rule/cursorfree--define-simple-modifier tail "tail"
  #'cursorfree-line-right)
(rule/cursorfree--define-simple-modifier head "head"
  #'cursorfree-line-left)
(rule/cursorfree--define-simple-modifier block "block"
  #'cursorfree-block)
(rule/cursorfree--define-simple-modifier token "token"
  #'cursorfree-token)
(rule/cursorfree--define-simple-modifier everything "everything"
  #'cursorfree-everything)
(rule/cursorfree--define-simple-modifier visible "visible"
  #'cursorfree-visible)
(rule/cursorfree--define-simple-modifier this "this"
  #'cursorfree-this)
(rule/cursorfree--define-simple-modifier every-instance "every instance"
  #'cursorfree-every-instance)
(rule/cursorfree--define-simple-modifier clip "clip"
  #'cursorfree-kill-ring)
(rule/cursorfree--define-simple-modifier primary "primary"
  #'cursorfree-primary-selection)
(rule/cursorfree--define-simple-modifier next "next"
  #'cursorfree-next)
(rule/cursorfree--define-simple-modifier preve "preve"
  #'cursorfree-previous)
(rule/cursorfree--define-simple-modifier beginning "beginning"
  #'cursorfree-beginning)
(rule/cursorfree--define-simple-modifier end "end"
  #'cursorfree-end)
(rule/cursorfree--define-simple-modifier buffer "buffer"
  #'cursorfree-buffer)
(rule/cursorfree--define-simple-modifier split "split"
  #'cursorfree-window-or-selected)

;; TODO add sentence, link.  Or wait until I move scopes here as well.

(provide 'cursorfree-phony)
;;; cursorfree-phony.el ends here
