;;; test.el --- Tests for cursorfree.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience

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

;;

;;; Code:

(cl-defstruct cursorfree--test-buffer-state
  string
  points
  (markers nil))

(cl-defstruct cursorfree--test-parameters
   (before (make-my/test-buffer-state
            :string ""
            :points nil)
           :type my/test-buffer-state)
   (after (make-my/test-buffer-state
            :string ""
            :points nil)
          :type my/test-buffer-state)
   command-form)

(defun cursorfree--run-test (parameters)
  (with-temp-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (insert (cursorfree--test-buffer-state-string
               (cursorfree--test-parameters-before parameters)))
      ;; Assume we only use one point for now
      (goto-char (car (cursorfree--test-buffer-state-points
                       (cursorfree--test-parameters-before parameters))))
      (funcall #'cursorfree-evaluate
               (seq-map #'eval (cursorfree--test-parameters-command-form
                                parameters)))
      (should (equal (buffer-string)
                     (cursorfree--test-buffer-state-string
                      (cursorfree--test-parameters-after parameters))))
      (should (equal (point)
                     (car (cursorfree--test-buffer-state-points
                           (cursorfree--test-parameters-after parameters))))))))

(ert-deftest cursorfree--test-pre ()
  "jump/pre."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This is a test"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "This is a test"
            :points '(9))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 9 10)))
                    (alist-get "pre" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-post ()
  "post."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This is a test"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "This is a test"
            :points '(10))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 9 10)))
                    (alist-get "post" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-change ()
  "change."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "A small brown fox"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "A small  fox"
            :points '(9))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 9 14)))
                    (alist-get "change" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-bring ()
  "bring."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This word will be overwritten"
             :points '(30))
    :after (make-cursorfree--test-buffer-state
            :string "This overwritten will be overwritten"
            :points '(37))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 6 10)))
                    (cursorfree--pusher (cursorfree--make-target (cons 19 30)))
                    (alist-get "bring" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Bringing a  word to point"
             :points '(12))
    :after (make-cursorfree--test-buffer-state
            :string "Bringing a point word to point"
            :points '(17))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 21 26)))
                    (alist-get "bring" cursorfree-actions nil nil #'equal)))))


(ert-deftest cursorfree--test-move ()
  "move."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Moving words is fun"
             :points '(20))
    :after (make-cursorfree--test-buffer-state
            :string "words Moving fun"
            :points '(17))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 14 16)))
                    (cursorfree--pusher (cursorfree--make-target (cons 1 7)))
                    (alist-get "move" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Moving a  to point word"
             :points '(10))
    :after (make-cursorfree--test-buffer-state
            :string "Moving a word to point"
            :points '(14))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 20 24)))
                    (alist-get "move" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-chuck ()
  "chuck."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "I must remove an extraneous extraneous word"
             :points '(44))
    :after (make-cursorfree--test-buffer-state
            :string "I must remove an extraneous word"
            :points '(33))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 18 28)))
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This sentence will be decimated for sure"
             :points '(41))
    :after (make-cursorfree--test-buffer-state
            :string "This will decimated sure"
            :points '(25))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 6 14)))
                    (cursorfree--pusher (cursorfree--make-target (cons 20 22)))
                    (cursorfree--pusher (cursorfree--make-target (cons 33 36)))
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--inner ()
  "inner."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "([aaa] bbb ccc)"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "()"
            :points '(3))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 3 6)))
                    (cursorfree--pusher ?\()
                    (alist-get "inside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "([aaa] bbb ccc)"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "([] bbb ccc)"
            :points '(13))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 3 6)))
                    (cursorfree--pusher ?\[)
                    (alist-get "inside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "([aaa] bbb ccc)"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "([] bbb ccc)"
            :points '(13))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 3 6)))
                    (alist-get "inside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "(\"aaa\" bbb ccc)"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "(\"\" bbb ccc)"
            :points '(1))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 3 6)))
                    (alist-get "inside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--outer-parenthesis ()
  "outer."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "([aaa] bbb ccc)"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string ""
            :points '(1))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 3 6)))
                    (cursorfree--pusher ?\()
                    (alist-get "outside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "([aaa] bbb ccc)"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "(bbb ccc)"
            :points '(10))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 3 6)))
                    (cursorfree--pusher ?\[)
                    (alist-get "outside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "([aaa] bbb ccc)"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "(bbb ccc)"
            :points '(10))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 3 6)))
                    (alist-get "outside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "(\"aaa\" bbb ccc)"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "(bbb ccc)"
            :points '(10))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 3 6)))
                    (alist-get "outside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--wrap ()
  "wrap."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "aaa bbb ccc"
             :points '(12))
    :after (make-cursorfree--test-buffer-state
            :string "aaa {bbb} ccc"
            :points '(14))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 5 8)))
                    (cursorfree--pusher ?\{)
                    (alist-get "wrap" cursorfree-actions nil nil #'equal))))

  ;; Non-parentheses use same character for both ends
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "aaa bbb ccc"
             :points '(12))
    :after (make-cursorfree--test-buffer-state
            :string "aaa $bbb$ ccc"
            :points '(14))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 5 8)))
                    (cursorfree--pusher ?$)
                    (alist-get "wrap" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-join ()
  "join."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This
is
a
little
test"
             :points '(26))
    :after (make-cursorfree--test-buffer-state
            :string "This is a little test"
            :points '(1))
    :command-form '((alist-get "everything" cursorfree-modifiers nil nil #'equal)
                    (alist-get "join" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-fuse ()
  "fuse."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "aaa bbb ccc
ddd"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "aaabbbcccddd"
            :points '(1))
    :command-form '((alist-get "everything" cursorfree-modifiers nil nil #'equal)
                    (alist-get "fuse" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-filter ()
  "filter."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a a a b b a a b a"
             :points '(18))
    :after (make-cursorfree--test-buffer-state
            :string "a a a b b b a"
            :points '(14))
    :command-form '((cursorfree--pusher (cursorfree--make-target (cons 17 18)))
                    (alist-get "every instance" cursorfree-modifiers nil nil #'equal)
                    (cursorfree--pusher (cursorfree--make-target (cons 9 10)))
                    (cursorfree--pusher (cursorfree--make-target (cons 15 16)))
                    (alist-get "past" cursorfree-modifiers nil nil #'equal)
                    (alist-get "filter" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

;;; test.el ends here
