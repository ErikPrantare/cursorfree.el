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

(ert-deftest hatty-edit--interpreter-substack ()
  "Substack (de)construction."
  (hatty-edit--result-should-equal '(5 3 1) '((5 3 1))
    '(nop
      push 3
      stack))
  (hatty-edit--result-should-equal '((5 3 1)) '(5 3 1)
    '(unstack)))

(ert-deftest hatty-edit--interpreter-unstack ()
  "List unwrapping."
  (hatty-edit--result-should-equal '((5 a b) 3 1) '(5 a b 3 1)
    '((unstack))))

(ert-deftest hatty-edit--interpreter-macro-definition ()
  "One can define macros."
  (let ((sum (make-symbol "sum")))
    (hatty-edit--define-composed-macro sum
      '(nop
        push 2 list
        push + lisp-apply))
    (hatty-edit--result-should-equal '(5 3) '(8)
      (list sum))))

(ert-deftest hatty-edit--interpreter-substack ()
  "Substacks evaluate as if flattened onto the instruction stack."
  (hatty-edit--result-should-equal '(5 3) '(8)
    '((nop
       push 2
       stack
       push +
       lisp-apply))))

(ert-deftest hatty-edit--interpreter-function-invocation ()
  "One may apply functions."
  (hatty-edit--result-should-equal  '(5) '(25)
    '(nop
      push 1 stack
      push (lambda (x) (* x x))
      lisp-apply))
  (hatty-edit--result-should-equal  '(5) '(25)
    `(nop
      push ,(lambda (x) (* x x))
      lisp-funcall)))

(ert-deftest hatty-edit--interpreter-stack-amalgamation ()
  "Turn whole stack into a substack."
  (hatty-edit--result-should-equal '(5 3 1) '((5 3 1))
    '((amalgamate-stack))))

(ert-deftest hatty-edit--interpreter-unpush ()
  "unpush puts top value back onto instruction stack."
  (hatty-edit--result-should-equal  '(5 3 5) '(3 5)
    '(nop
      push drop
      unpush)))

(ert-deftest hatty-edit--interpreter-map ()
  "map applies function across substack."
  (hatty-edit--result-should-equal  '((5 3 5)) '((25 9 25))
    '(nop
      push (push (lambda (x) (* x x)) lisp-funcall)
      map)))

(ert-deftest hatty-edit--substack-evaluation ()
  "Substacks are evaluated as programs."
  (hatty-edit--result-should-equal  nil '((25 9 25))
    '(nop
      (push 5 push 3 push 5)
      amalgamate-stack
      push (push (lambda (x) (* x x)) lisp-funcall)
      map)))

(ert-deftest hatty-edit--cons-uncons ()
  "Substacks are evaluated as programs."
  (hatty-edit--result-should-equal  '(5 3) '((5 . 3))
    '(cons))
  (hatty-edit--result-should-equal  '((5 . 3)) '(5 3)
    '(uncons)))

(ert-deftest hatty-edit--swap ()
    "swap, swapd."
    (hatty-edit--result-should-equal  '(5 3) '(3 5)
      '(swap))
    (hatty-edit--result-should-equal  '(5 3 100) '(5 100 3)
      '(swapd)))

(ert-deftest hatty-edit--dup ()
    "dup, dupd."
    (hatty-edit--result-should-equal  '(5 2) '(5 5 2)
      '(dup))
    (hatty-edit--result-should-equal  '(5 100 2) '(5 100 100 2)
      '(dupd)))

(ert-deftest hatty-edit--roll ()
    "rollup, rolldown."
    (hatty-edit--result-should-equal  '(5 2 7 3) '(2 7 5 3)
      '(rollup))
    (hatty-edit--result-should-equal  '(5 2 7 3) '(7 5 2 3)
      '(rolldown)))

(ert-deftest hatty-edit--variable-binding ()
    "-> binds variables until .. (period period).

This only replaces occurences in top-level forms."
    (hatty-edit--result-should-equal  nil '(1 4 9 16 25)
      '(nop
        push (1 2 3 4 5)
        push (-> x : x x push * push 2 lisp-apply-n ..)
        map
        unstack)))


;;;; Editing

(ert-deftest hatty-edit--target-insert ()
  "target-insert."
  (with-temp-buffer
    (hatty-edit--evaluate
     '(push "Success" target-insert))
    (should (string= (buffer-string) "Success"))))

(ert-deftest hatty-edit--target-insert ()
  "target-insert."
  (with-temp-buffer
    (insert "aaa bbb")
    (hatty-edit--evaluate
     `(nop
       push ,(hatty-edit--markify-region
              (cons (+ 4 (point-min))
                    (point-max)))
       push "ccc"
       target-insert))
    (should (string= (buffer-string) "aaa cccbbb"))))

(ert-deftest hatty-edit--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (hatty-edit--evaluate
     `(nop
       push ,(hatty-edit--markify-region
              (cons (+ 4 (point-min))
                    (point-max)))
       push "ccc"
       target-overwrite))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest hatty-edit--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (hatty-edit--evaluate
     `(nop
       push ,(hatty-edit--markify-region
              (cons (+ 4 (point-min))
                    (point-max)))
       push "ccc"
       target-overwrite))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest hatty-edit--target-swap ()
  "target-swap."
  (with-temp-buffer
    (insert "aaa bbb")
    (hatty-edit--evaluate
     `(nop
       push ,(hatty-edit--markify-region
              (cons (+ 4 (point-min))
                    (point-max)))
       push ,(hatty-edit--markify-region
              (cons (point-min)
                    (+ 3 (point-min))))
       target-swap))
    (should (string= (buffer-string) "bbb aaa"))))

(ert-deftest hatty-edit--target-delete ()
  "targets-delete."
  (with-temp-buffer
    (insert "aaa bbb")
    (hatty-edit--evaluate
     `(nop
       push ,(hatty-edit--markify-region
              (cons (+ 2 (point-min))
                    (+ 4 (point-min))))
       target-delete))
    (should (string= (buffer-string) "aabbb"))))

(ert-deftest hatty-edit--target-bring ()
  "target-bring."
  (with-temp-buffer
    (insert "aaa ")
    (goto-char (point-max))
    (hatty-edit--evaluate
     `(nop
       push ,(hatty-edit--markify-region
              (cons (point-min)
                    (+ 3 (point-min))))
       target-bring))
    (should (string= (buffer-string) "aaa aaa"))))

(ert-deftest hatty-edit--target-move ()
  "target-move."
  (with-temp-buffer
    (insert "aaa bbb ")
    (goto-char (point-max))
    (hatty-edit--evaluate
     `(nop
       push ,(hatty-edit--markify-region
              (cons (point-min)
                    (+ 3 (point-min))))
       target-move))
    (should (string= (buffer-string) "bbb aaa"))))

;;; test.el ends here
