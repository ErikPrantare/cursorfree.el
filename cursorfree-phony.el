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

(phony-define-open-rule rule/cursorfree-modifier
  :contributes-to '(rule/cursorfree--instruction)
  :transformation #'cursorfree-make-modifier)

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
;; not allowed.  Without this stratification, constance would not be
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

(phony-define-dictionary rule/hatty-color
  '(("squash" . yellow)
    ("red" . red)
    ("blue" .  blue)
    ("pink" . pink)
    ("green" . green)))

(phony-define-dictionary rule/hatty-shape
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
            (? (color rule/hatty-color))
            (? (shape rule/hatty-shape))
            (char rule/any-alphanumeric-key)))
  (cursorfree--make-target-from-hat char color shape))

(defun my/range-component (from to)
  (declare (phony-rule
            :export nil
            :contributes-to 'rule/cursorfree-compound-constant
            (from rule/cursorfree-constant)
            "past"
            (to rule/cursorfree-constant)))
  (cursorfree-past from to))

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

(provide 'cursorfree-phony)
;;; cursorfree-phony.el ends here
