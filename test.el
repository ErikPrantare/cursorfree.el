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

(defun hatty-edit--should-equal (expected actual)
  "Performing INSTRUCTIONS with INITIAL-STACk yields EXPECTED-STACK."
  (declare (indent defun))
  (should (equal expected actual)))

;; Deprecated
(defun hatty-edit--result-should-equal (initial-stack expected-stack instructions)
  "Performing INSTRUCTIONS with INITIAL-STACK yields EXPECTED-STACK."
  (declare (indent defun))
  (should (equal expected-stack
                 (hatty-edit--evaluate-environment
                   (make-hatty-edit--environment
                    :value-stack initial-stack
                    :instruction-stack instructions)))))

(ert-deftest hatty-edit--interpreter-push ()
  "Literals and substacks are pushed unto the stack."
  (should (equal '(5)
                 (hatty-edit--evaluate
                  '(5))))
  (should (equal '((a b c) 5 10)
                 (hatty-edit--evaluate
                  '(10 5 (a b c))))))

(ert-deftest hatty-edit--interpreter-drop ()
  "odrop removes from the stack."
  (hatty-edit--result-should-equal '(a b c) '(b c) 
    '(drop)))

(ert-deftest hatty-edit--interpreter-drop-undos-push ()
  "drop undos push."
  (hatty-edit--result-should-equal '(a b c) '(a b c)
    '(5 drop)))

(ert-deftest hatty-edit--substack ()
  "stack, unstack, value-stack."
  (hatty-edit--should-equal '((5 3 1))
      (hatty-edit--evaluate
       '(5 3 1 3 stack)))
  (hatty-edit--should-equal '(5 3 1)
      (hatty-edit--evaluate
       '((5 3 1) unstack)))
  (hatty-edit--should-equal '((5 3 1) 5 3 1)
      (hatty-edit--evaluate
       '(1 3 5 value-stack))))

(ert-deftest hatty-edit--interpreter-unstack ()
  "Pack unwrapping."
  (hatty-edit--result-should-equal '((5 a b) 3 1) '(5 a b 3 1)
    '(unstack)))

(ert-deftest hatty-edit--interpreter-macro-definition ()
  "One can define macros."
  ;; Create uninterned symbol to avoid polluting the global namespace
  (let ((sum (make-symbol "sum")))
    (hatty-edit--define-compound-macro sum
      '((+) 2 lisp-apply-n))
    (hatty-edit--should-equal '(8)
      (hatty-edit--evaluate
       `(5 3 ,sum)))))

(ert-deftest hatty-edit--interpreter-substack ()
  "Substacks evaluate as if flattened onto the instruction stack."
  (hatty-edit--should-equal '(8)
    (hatty-edit--evaluate
     '(5
       3
       2
       stack
       (+)
       lisp-apply))))

(ert-deftest hatty-edit--interpreter-function-invocation ()
  "lisp-apply, lisp-apply-n, lisp-funcall, (lisp-eval..?  For
effectful computation.)."
  (hatty-edit--should-equal '("FirstSecond")
    (hatty-edit--evaluate
     '("First"
       "Second"
       2 stack
      (concat)
      lisp-apply)))

  (hatty-edit--should-equal '(15)
    (hatty-edit--evaluate
     '(5
       3
       (*)
       2
       lisp-apply-n)))

  (hatty-edit--should-equal '(25)
    (hatty-edit--evaluate
     `(5
       (lambda (x) (* x x))
       lisp-funcall)))

  (hatty-edit--should-equal '("olleH")
    (hatty-edit--evaluate
     `("Hello"
       (reverse)
       lisp-funcall))))

(ert-deftest hatty-edit--interpreter-stack-amalgamation ()
  "Turn whole stack into a substack."
  (hatty-edit--should-equal '((5 3 1))
    (hatty-edit--evaluate
     '(1 3 5         
       amalgamate-stack))))

(ert-deftest hatty-edit--interpreter-map ()
  "map, map-stack."
  (hatty-edit--should-equal '((25 9 100))
    (hatty-edit--evaluate
     `((5 3 10)
       (,(lambda (x) (* x x)) lisp-funcall)
       map)))
  (hatty-edit--should-equal '(25 9 100)
    (hatty-edit--evaluate
     `(10 3 5
       (,(lambda (x) (* x x)) lisp-funcall)
       map-stack))))

(ert-deftest hatty-edit--cons ()
  "cons, uncons."
  (should (equal
           '((5 . 3))
           (hatty-edit--evaluate
            '(3 5 cons))))
  (should (equal
           '(5 3)
           (hatty-edit--evaluate
            '((5 . 3) uncons)))))

(ert-deftest hatty-edit--swap ()
    "swap, swapd."
    (hatty-edit--result-should-equal  '(5 3) '(3 5)
      '(swap))
    (hatty-edit--result-should-equal  '(5 3 100) '(5 100 3)
      '(swapd)))

(ert-deftest hatty-edit--dup ()
    "dup, dupd."
    (hatty-edit--should-equal '(5 5 2)
      (hatty-edit--evaluate
       `(2 5 dup)))
    (hatty-edit--should-equal '(5 100 100 2)
      (hatty-edit--evaluate
       `(2 100 5 dupd))))

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
      '((1 2 3 4 5)
        (-> x : x x (*) 2 lisp-apply-n ..)
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
     `(,(hatty-edit--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
       "ccc"
       target-insert))
    (should (string= (buffer-string) "aaa cccbbb"))))

(ert-deftest hatty-edit--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (hatty-edit--evaluate
     `(,(hatty-edit--markify-region
              (cons (+ 4 (point-min))
                    (point-max)))
       "ccc"
       target-overwrite))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest hatty-edit--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (hatty-edit--evaluate
     `(,(hatty-edit--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
       "ccc"
       target-overwrite))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest hatty-edit--target-swap ()
  "target-swap."
  (with-temp-buffer
    (insert "aaa bbb")
    (hatty-edit--evaluate
     `(,(hatty-edit--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
       ,(hatty-edit--markify-region
         (cons (point-min)
               (+ 3 (point-min))))
       target-swap))
    (should (string= (buffer-string) "bbb aaa"))))

(ert-deftest hatty-edit--target-delete ()
  "targets-delete."
  (with-temp-buffer
    (insert "aaa bbb")
    (hatty-edit--evaluate
     `(,(hatty-edit--markify-region
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
     `(,(hatty-edit--markify-region
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
     `(,(hatty-edit--markify-region
         (cons (point-min)
               (+ 3 (point-min))))
       target-move))
    (should (string= (buffer-string) "bbb aaa"))))

(ert-deftest hatty-edit--target-chuck ()
  "target-chuck."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (goto-char (point-max))
    (hatty-edit--evaluate
     `(,(hatty-edit--markify-region
         (cons (+ 4 (point-min))
               (+ 7 (point-min))))
       target-chuck))
    (should (string= (buffer-string) "aaa ccc"))))

;;; test.el ends here
