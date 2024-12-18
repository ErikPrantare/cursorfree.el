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

(defun he--should-equal (expected actual)
  "Performing INSTRUCTIONS with INITIAL-STACk yields EXPECTED-STACK."
  (declare (indent defun))
  (should (equal expected actual)))


(ert-deftest he--push ()
  "Non-symbols are pushed unto the stack.  pusher."
  (should (equal '(5)
                 (he--evaluate
                  '(5))))
  (should (equal '((a b c) 5 10)
                 (he--evaluate
                  '(10 5 (a b c))))))

(ert-deftest he--drop ()
  "drop."
  (he--should-equal '(2 1)
    (he--evaluate
     '(1 2 3 drop))))

(ert-deftest he--substack ()
  "stack, unstack."
  (he--should-equal '(5 3 1)
    (he--evaluate
     '((5 3 1) unstack)))
  (he--should-equal '((5 3 1) 5 3 1)
    (he--evaluate
     '(1 3 5 stack))))

(ert-deftest he--instructions ()
  "instructions, replace-instructions."
  (he--should-equal '((cdr 5 (1337)))
    (he--environment-value-stack
     (he--step
      (he--make-environment
       '(instructions cdr 5 (1337))))))

  (he--should-equal '(3)
    (he--evaluate
     '((5 3 1) (cdr car) replace-instructions))))

(ert-deftest he--unstack ()
  "Pack unwrapping."
  (he--should-equal '(5 a b 3 1)
    (he--evaluate
     `(1 3 (5 a b) unstack))))

(ert-deftest he--instruction-definition ()
  "One can define instructions."
  ;; Create uninterned symbols to avoid polluting the global namespace
  (let ((sum (make-symbol "sum")))
    (he--define-instruction sum
      (lambda (environment)
        (he--push-value
          environment
          (+ (he--pop-value environment)
             (he--pop-value environment)))))
    (he--should-equal '(8)
      (he--evaluate
       `(5 3 ,sum))))

  (let ((sum (make-symbol "sum")))
    (he--define-compound-instruction sum
      '(\\ + 2 lisp-funcall-n))
    (he--should-equal '(8)
      (he--evaluate
       `(5 3 ,sum)))))

(ert-deftest he--function-invocation ()
  "lisp-apply, lisp-funcall-n, lisp-funcall, (lisp-eval..?  For
effectful computation.)."
  (he--should-equal '("FirstSecond")
    (he--evaluate
     '(("First" "Second")
       \\ concat
       lisp-apply)))

  (he--should-equal '("FirstSecond")
    (he--evaluate
     '("First"
       "Second"
       \\ concat
       2
       lisp-funcall-n)))

  (he--should-equal '(25)
    (he--evaluate
     `(5
       (lambda (x) (* x x))
       lisp-funcall)))

  (he--should-equal '("olleH")
    (he--evaluate
     `("Hello"
       \\ reverse
       lisp-funcall))))

(ert-deftest he--stack-amalgamation ()
  "Turn whole stack into a substack."
  (he--should-equal '((5 3 1))
    (he--evaluate
     '(1 3 5
         amalgamate-stack))))

(ert-deftest he--map ()
  "map, map-stack"
  (he--should-equal '((25 9 100))
    (he--evaluate
     `((5 3 10)
       ,(he--lambda (x) x x *)
       map)))
  (he--should-equal '(25 9 100)
    (he--evaluate
     `(10 3 5
          ,(he--lambda (x) x x *)
          map-stack))))

(ert-deftest he--cons ()
  "cons, uncons."
  (should (equal
           '((5 . 3))
           (he--evaluate
            '(5 3 cons))))
  (should (equal
           '(3 5)
           (he--evaluate
            '((5 . 3) uncons)))))

(ert-deftest he--swap ()
  "swap."
  (he--should-equal '(3 5)
    (he--evaluate
     `(3 5 swap))))

(ert-deftest he--dup ()
  "dup."
  (he--should-equal '(5 5 2)
    (he--evaluate
     `(2 5 dup))))

(ert-deftest he--dip ()
  "dip."
  (he--should-equal '("covering" "testtest")
    (he--evaluate
     `("test"
       "covering"
       ,(he--lambda (v) v v \\ concat 2 lisp-funcall-n)
       dip))))

(ert-deftest he--variable-binding ()
  "-> binds variables until .. (period period).

This only replaces occurences in top-level forms."
  (he--should-equal '(1 4 9 16 25)
    (he--evaluate
     '((1 2 3 4 5)
       (-> x : x x \\ * 2 lisp-funcall-n ..)
       map
       unstack)))

  (he--should-equal '(1 4 9 16 25)
    (he--evaluate
     `((1 2 3 4 5)
       ,(he--lambda (x) x x \\ * 2 lisp-funcall-n)
       map
       unstack))))

(ert-deftest he--curry ()
  "curry."
  (he--should-equal '((5 15 25))
    (he--evaluate
     '((1 3 5)
       (\\ * 2 lisp-funcall-n)
       curry
       (5) dip eval
       map))))

(ert-deftest he--subenvironment ()
  "make-subenvironment, evaluate-subenvironment."
  (he--should-equal '(9)
    (he--evaluate
     '((1 3 5)
       (+ +)
       make-subenvironment
       evaluate-subenvironment
       unstack))))

(ert-deftest he--infix ()
  "make-infix."
  (he--should-equal '(9)
    (he--evaluate
     '(2 (+) make-infix 7))))

;;;; Editing

(ert-deftest he--save-excursion ()
  "save-excursion."
  (he--should-equal '(25)
    (he--evaluate
     '(5
       (5 *)
       save-excursion))))

(ert-deftest he--target-insert ()
  "target-insert."
  (with-temp-buffer
    (insert "aaa bbb")
    (he--evaluate
     `(,(he--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
       "ccc"
       target-insert))
    (should (string= (buffer-string) "aaa cccbbb"))))

(ert-deftest he--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (he--evaluate
     `(,(he--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
       "ccc"
       target-overwrite))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest he--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (he--evaluate
     `(,(he--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
       "ccc"
       target-overwrite))
    (should (string= (buffer-string) "aaa ccc"))))
(ert-deftest he--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (he--evaluate
     `(,(he--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
       "ccc"
       target-overwrite))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest he--target-change ()
  "target-change."
  (with-temp-buffer
    (insert "aaa bbb")
    (he--evaluate
     `(,(he--markify-region
         (cons (+ 2 (point-min))
               (+ 5 (point-min))))
       target-change))
    (should (string= (buffer-string) "aabb"))
    (should (= (point) (+ 2 (point-min))))))

(ert-deftest he--target-delete ()
  "targets-delete."
  (with-temp-buffer
    (insert "aaa bbb")
    (he--evaluate
     `(,(he--markify-region
         (cons (+ 2 (point-min))
               (+ 4 (point-min))))
       target-delete))
    (should (string= (buffer-string) "aabbb"))))

(ert-deftest he--target-bring ()
  "target-bring."
  (with-temp-buffer
    (insert "aaa ")
    (goto-char (point-max))
    (he--evaluate
     `(,(he--markify-region
         (cons (point-min)
               (+ 3 (point-min))))
       target-bring))
    (should (string= (buffer-string) "aaa aaa"))))

(ert-deftest he--target-move ()
  "target-move."
  (with-temp-buffer
    (insert "aaa bbb ")
    (goto-char (point-max))
    (he--evaluate
     `(,(he--markify-region
         (cons (point-min)
               (+ 3 (point-min))))
       target-move))
    (should (string= (buffer-string) "bbb aaa"))))

(ert-deftest he--target-chuck ()
  "target-chuck."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (goto-char (point-max))
    (he--evaluate
     `(,(he--markify-region
         (cons (+ 4 (point-min))
               (+ 7 (point-min))))
       target-chuck))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest he--inner-parenthesis ()
  "inner-parenthesis, inner-parenthesis-any, inner-parenthesis-dwim."
  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he--evaluate
     `(,(cons (+ (point-min) 2) (+ (point-min) 3))
       ?\(
       inner-parenthesis
       target-delete))
    (should (string= (buffer-string) "()")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he--evaluate
     `(,(cons (+ (point-min) 2) (+ (point-min) 3))
       ?\[
       inner-parenthesis
       target-delete))
    (should (string= (buffer-string) "([] bbb ccc)")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he--evaluate
     `(,(cons (+ (point-min) 2) (+ (point-min) 3))
       inner-parenthesis-any
       target-delete))
    (should (string= (buffer-string) "([] bbb ccc)")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he--evaluate
     `(,(cons (+ (point-min) 2) (+ (point-min) 3))
       inner-parenthesis-dwim
       target-delete))
    (should (string= (buffer-string) "([] bbb ccc)")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he--evaluate
     `(,(cons (+ (point-min) 2) (+ (point-min) 3))
       ?\(
       inner-parenthesis-dwim
       target-delete))
    (should (string= (buffer-string) "()")))

  (with-temp-buffer
    (insert "(\"aaa\" bbb ccc)")
    (he--evaluate
     `(,(cons (+ (point-min) 2) (+ (point-min) 3))
       inner-parenthesis-dwim
       target-delete))
    (should (string= (buffer-string) "(\"\" bbb ccc)"))))

(ert-deftest he--wrap-parentheses ()
  "wrap-parentheses."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (he--evaluate
     `(,(he--markify-region (cons (+ (point-min) 4) (+ (point-min) 7)))
       ?{
       target-wrap-parentheses))
    (should (string= (buffer-string) "aaa {bbb} ccc")))

  ;; Non-parentheses use same character for both ends
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (he--evaluate
     `(,(he--markify-region (cons (+ (point-min) 4) (+ (point-min) 7)))
       ?$
       target-wrap-parentheses))
    (should (string= (buffer-string) "aaa $bbb$ ccc"))))

(ert-deftest he--targets-join ()
  "target-join."
  (he--should-equal `(,(he--markify-region '(5 . 100)))
    (he--evaluate
     '(((43 . 30)
        (5 . 20)
        (65 . 100)
        (23 . 25))
       targets-join))))

(ert-deftest he--crush ()
  "crush."
  (with-temp-buffer
    (insert "aaa bbb ccc ddd eee")
    (he--evaluate
     `((,(he--markify-region (cons (+ (point-min) 1) (+ (point-min) 2)))
        ,(he--markify-region (cons (+ (point-min) 4) (+ (point-min) 7)))
        ,(he--markify-region (cons (+ (point-min) 12) (+ (point-min) 15))))
       crush))
    (should (string= (buffer-string) "aabbbddd eee"))))

;; Local Variables:
;; read-symbol-shorthands: (("he-" . "hatty-edit-"))
;; End:
;;; test.el ends here
