;;; test.el --- Tests for hatty-edit.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Erik Präntare

;; Author: Erik Präntare <erik@system2>
;; Keywords: convenience

;; hatty-edit.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; hatty-edit.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun hatty-edit--result-should-equal (initial-stack expected-stack instructions)
  "Performing INSTRUCTIONS with INITIAL-STACk yields EXPECTED-STACK."
  (declare (indent defun))
  (should (equal expected-stack
                 (hatty-edit--environment-value-stack
                  (hatty-edit--evaluate-environment
                    (make-hatty-edit--environment
                     :value-stack initial-stack
                     :instruction-stack instructions))))))

(ert-deftest hatty-edit--interpreter-push ()
  "push adds to the stack."
  (hatty-edit--result-should-equal nil '(5)
    '(push 5)))

(ert-deftest hatty-edit--interpreter-drop ()
  "odrop removes from the stack."
  (hatty-edit--result-should-equal '(a b c) '(b c) 
    '(drop)))

(ert-deftest hatty-edit--interpreter-drop-undos-push ()
  "drop undos push."
  (hatty-edit--result-should-equal '(a b c) '(a b c)
    '(push 5
      drop)))

(ert-deftest hatty-edit--interpreter-list ()
  "List construction."
  (hatty-edit--result-should-equal '(5 3 1) '((5 3 1))
    '(nop
      push 3
      list)))

(ert-deftest hatty-edit--interpreter-unlist ()
  "List unwrapping."
  (hatty-edit--result-should-equal '((5 a b) 3 1) '(5 a b 3 1)
    '((unlist))))

(ert-deftest hatty-edit--interpreter-macro-definition ()
  "One can define macros."
  (hatty-edit--define-simple-macro 'sum
    '((list 2)
      (apply +)))
  (let* ((sum '(nop
                push 2 list
                push + apply)))
    (hatty-edit--result-should-equal '(5 3) '(8)
      `(,sum))))

(ert-deftest hatty-edit--interpreter-anonymous-macro ()
  "One can define anonymous macros."
  (hatty-edit--result-should-equal '(5 3) '(8)
    '((nop
       push 2
       list
       push +
       apply))))

(ert-deftest hatty-edit--interpreter-function-invocation ()
  "One may apply functions."
  (hatty-edit--result-should-equal  '(5) '(25)
    '(nop
      push 1 list
      push (lambda (x) (* x x))
      apply)))

(ert-deftest hatty-edit--interpreter-stack-amalgamation ()
  "Turn whole stack into a substack."
  (hatty-edit--result-should-equal '(5 3 1) '((5 3 1))
    '((amalgamate-stack))))

(ert-deftest hatty-edit--interpreter-eval-quoted ()
  "eval-quoted evaluates the top function."
  (hatty-edit--result-should-equal  '(5 3 5) '(3 5)
    '(nop
      push drop
      eval-quoted)))

(ert-deftest hatty-edit--interpreter-map ()
  "map applies function across substack."
  (hatty-edit--result-should-equal  '((5 3 5)) '((25 9 25))
    '(nop
      push (push (lambda (x) (* x x)) funcall)
      map)))

(ert-deftest hatty-edit--substack-evaluation ()
  "Sub stacks are evaluated as programs."
  (hatty-edit--result-should-equal  nil '((25 9 25))
    '(nop
      (push 5 push 3 push 5)
      amalgamate-stack
      push (push (lambda (x) (* x x)) funcall)
      map)))

;;; test.el ends here
