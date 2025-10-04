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
(phony-define-open-rule rule/cursorfree--rest-instruction)

(phony-define-open-rule rule/cursorfree--instruction
  :contributes-to '(rule/cursorfree--initial-instruction
                    rule/cursorfree--rest-instruction))

(phony-define-open-rule rule/cursorfree-modifier
  :contributes-to '(rule/cursorfree--instruction)
  :transformation #'cursorfree-make-modifier)

(defvar cursorfree--target-it nil)

(defun rule/cursorfree-target (initial rest)
  (declare (phony-rule
            :export nil
            (initial rule/cursorfree--initial-instruction)
            (* (rest rule/cursorfree--rest-instruction))))
  (let ((target (cursorfree--normalize-target
                 (cursorfree-evaluate (cons initial rest)))))
    (setq my/cursorfree-it target)
    target))

(defun rule/cursorfree-region (target)
  (declare (phony-rule
            :export nil
            (target rule/cursorfree-target)))
  (cursorfree-content-region target))

(phony-define-open-rule rule/cursorfree-constant
  :contributes-to 'rule/cursorfree--instruction
  :transformation #'cursorfree--pusher)

(provide 'cursorfree-phony)
;;; cursorfree-phony.el ends here
