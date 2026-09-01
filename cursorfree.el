;;; cursorfree.el --- Complex editing through voice  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Erik Präntare

;; Author: Erik Präntare <erik@prantare.xyz>
;; Keywords: convenience
;; Version: 0.4.3
;; Homepage: https://github.com/ErikPrantare/cursorfree.el
;; Package-Requires: ((emacs "29.1") (phony "1.0.1"))
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

;; Cursorfree provides functionality for generic and extensible
;; editing commands, along with a grammar for using these commands by
;; voice.

;;; Code:

(require 'cursorfree-core)
(require 'cursorfree-phony)

(provide 'cursorfree)
;;; cursorfree.el ends here
