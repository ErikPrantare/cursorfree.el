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
  command-form
  (after (make-my/test-buffer-state
          :string ""
          :points nil)
         :type my/test-buffer-state)
   (before (make-my/test-buffer-state
            :string ""
            :points nil)
           :type my/test-buffer-state)
   (from-same-buffer t
    :documentation "Whether evaluating the command is invariant to the
    selected window.")
   (setup #'ignore))

(defun cursorfree--multiple-cursor-points ()
  (let ((points (list (point))))
    (mc/for-each-fake-cursor
     (push (marker-position (overlay-get cursor 'point)) points))
    points))

(defun cursorfree--setup-test (parameters)
  (delete-region (point-min) (point-max))
  (funcall (oref parameters setup))
  (insert (oref (oref parameters before) string))
  (goto-char (seq-first (oref (oref parameters before) points)))
  (multiple-cursors-mode 0)
  (seq-doseq (point (seq-rest (oref (oref parameters before) points)))
    (mc/create-fake-cursor-at-point)
    (goto-char point)))

(defun cursorfree--test-check-state (parameters)
  (should (equal (buffer-string) (oref (oref parameters after) string)))
  ;; We do not test the order (for now)
  (should (equal (seq-sort #'< (cursorfree--multiple-cursor-points))
                 (seq-sort #'< (oref (oref parameters after) points)))))

(defun cursorfree--run-test (parameters)
  (let ((cursorfree-highlight-deletions-p nil))
    (save-window-excursion
      ;; Side windows cannot become the only window
      (select-window (get-window-with-predicate
                      (lambda (window)
                        (not (window-parameter window 'window-side)))))
      (delete-other-windows)
      (split-window-horizontally)
      (let ((test-buffer (generate-new-buffer "*Test contents*"))
            (alternative-buffer (generate-new-buffer "*Focused buffer*")))
        (unwind-protect
            (progn
              (switch-to-buffer test-buffer)

              (cursorfree--setup-test parameters)
              ;; Backwards compatibility with older tests listing
              ;; instructions instead of providing the elisp form to
              ;; evaluate.
              (if (symbolp (car (cursorfree--test-parameters-command-form parameters)))
                  (eval (cursorfree--test-parameters-command-form parameters))
                (funcall #'cursorfree-evaluate
                         (seq-map #'eval (cursorfree--test-parameters-command-form parameters))))
              (cursorfree--test-check-state parameters)

              (unless (cursorfree--test-parameters-from-same-buffer parameters)
                (cursorfree--setup-test parameters)
                (let ((instructions (seq-map #'eval (cursorfree--test-parameters-command-form parameters))))
                  (other-window 1)
                  (switch-to-buffer alternative-buffer)
                  (funcall #'cursorfree-evaluate instructions)
                  (select-window (get-buffer-window test-buffer))
                  (cursorfree--test-check-state parameters))))
          (kill-buffer test-buffer)
          (kill-buffer alternative-buffer))))))

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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 9 10)))
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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 9 10)))
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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 9 14)))
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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 6 10)))
                    (cursorfree--pusher (cursorfree-make-target (cons 19 30)))
                    (alist-get "bring" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Bringing a  word to point"
             :points '(12))
    :after (make-cursorfree--test-buffer-state
            :string "Bringing a point word to point"
            :points '(17))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 21 26)))
                    (alist-get "bring" cursorfree-actions nil nil #'equal))
    :from-same-buffer t))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Point should remain"
             :points '(13))
    :after (make-cursorfree--test-buffer-state
            :string "Point should remain"
            :points '(13))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 7 13)))
                    (cursorfree--pusher (cursorfree-make-target (cons 7 13)))
                    (alist-get "bring" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Point should be after new word"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "Point should some-long-word after new word"
            :points '(28))
    :command-form '(cursorfree-target-bring
                    "some-long-word"
                    (cursorfree-make-target (cons 14 16))))))

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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 14 16)))
                    (cursorfree--pusher (cursorfree-make-target (cons 1 7)))
                    (alist-get "move" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Moving a  to point word"
             :points '(10))
    :after (make-cursorfree--test-buffer-state
            :string "Moving a word to point"
            :points '(14))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 20 24)))
                    (alist-get "move" cursorfree-actions nil nil #'equal))
    :from-same-buffer t)))

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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 18 28)))
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This sentence will be decimated for sure"
             :points '(41))
    :after (make-cursorfree--test-buffer-state
            :string "This will decimated sure"
            :points '(25))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 6 14)))
                    (cursorfree--pusher (cursorfree-make-target (cons 20 22)))
                    (cursorfree--pusher (cursorfree-make-target (cons 33 36)))
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Removing word\nin text with newline"
             :points '(15))
    :after (make-cursorfree--test-buffer-state
            :string "Removing\nin text with newline"
            :points '(10))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 9 14)))
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Removing word\nin text with newline"
             :points '(15))
    :after (make-cursorfree--test-buffer-state
            :string "Removing word\ntext with newline"
            :points '(15))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 15 17)))
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a\nb\n\nc\nd"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "a\n\nc\nd"
            :points '(1))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 4)))
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a\nb\n\nc\nd"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "a\nb\n\nd"
            :points '(1))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 6 7)))
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-inside ()
  "inside."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "([aaa] bbb ccc)"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "()"
            :points '(3))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 6)))

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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 6)))
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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 6)))
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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 6)))
                    (alist-get "inside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Expanding (around) point"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "Expanding () point"
            :points '(12))
    :command-form '((alist-get "inside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))
    :from-same-buffer t)))

(ert-deftest cursorfree--test-outside ()
  "outside."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "([aaa] bbb ccc)"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string ""
            :points '(1))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 6)))
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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 6)))
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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 6)))
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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 6)))
                    (alist-get "outside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Expanding (around) point"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "Expanding point"
            :points '(11))
    :command-form '((alist-get "outside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))
    :from-same-buffer t)))

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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 5 8)))
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
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 5 8)))
                    (cursorfree--pusher ?$)
                    (alist-get "wrap" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This is a test of multiple words"
             :points '(33))
    :after (make-cursorfree--test-buffer-state
            :string "This (is) a (test) of (multiple) words"
            :points '(39))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 6 8)))
                    (cursorfree--pusher (cursorfree-make-target (cons 11 15)))
                    (cursorfree--pusher (cursorfree-make-target (cons 19 27)))
                    (cursorfree--pusher 40)
                    (alist-get "wrap" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "point should stay inside"
             :points '(7))
    :after (make-cursorfree--test-buffer-state
            :string "point (should) stay inside"
            :points '(8))
    :command-form '(cursorfree-target-wrap ?\( (cursorfree-make-target (cons 7 13))))))

(ert-deftest cursorfree--test-past ()
  "past."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "A section will be removed"
             :points '(26))
    :after (make-cursorfree--test-buffer-state
            :string "A be removed"
            :points '(13))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 3 10)))
                    (cursorfree--pusher (cursorfree-make-target (cons 11 15)))
                    (alist-get "past" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "A section starting at point will be removed"
             :points '(11))
    :after (make-cursorfree--test-buffer-state
            :string "A section will be removed"
            :points '(11))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 23 28)))
                    (alist-get "past" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-join ()
  "join."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This\nis\n a \nlittle \n\n\n  test"
             :points '(16))
    :after (make-cursorfree--test-buffer-state
            :string "This is a little test"
            :points '(14))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 1 5)))
                    (cursorfree--pusher (cursorfree-make-target (cons 25 29)))
                    (alist-get "past" cursorfree-modifiers nil nil #'equal)
                    (alist-get "join" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-fuse ()
  "fuse."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "aaa bbb ccc\nddd"
             :points '(14))
    :after (make-cursorfree--test-buffer-state
            :string "aaabbbcccddd"
            :points '(11))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 1 4)))
                    (cursorfree--pusher (cursorfree-make-target (cons 13 16)))
                    (alist-get "past" cursorfree-modifiers nil nil #'equal)
                    (alist-get "fuse" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--every-instance ()
  "every instance."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a a a b b a a b a"
             :points '(10))
    :after (make-cursorfree--test-buffer-state
            :string "b b b"
            :points '(4))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 11 12)))
                    (alist-get "every instance" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a a a b b a a b a"
             :points '(13))
    :after (make-cursorfree--test-buffer-state
            :string "a a a b b b a"
            :points '(11))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 9 10)))
                    (cursorfree--pusher (cursorfree-make-target (cons 15 16)))
                    (alist-get "past" cursorfree-modifiers nil nil #'equal)
                    (cursorfree--pusher (cursorfree-make-target (cons 13 14)))
                    (alist-get "every instance" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-upcase ()
  "upcase."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This is a test"
             :points '(15))
    :after (make-cursorfree--test-buffer-state
            :string "This IS a test"
            :points '(15))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 6 8)))
                    (alist-get "upcase" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "This is another test over here"
             :points '(31))
    :after (make-cursorfree--test-buffer-state
            :string "THIS is ANOTHER test OVER here"
            :points '(31))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 1 5)))
                    (cursorfree--pusher (cursorfree-make-target (cons 9 16)))
                    (cursorfree--pusher (cursorfree-make-target (cons 22 26)))
                    (alist-get "upcase" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-next ()
  "next."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Word and test word word"
             :points '(9))
    :after (make-cursorfree--test-buffer-state
            :string "Word and test word"
            :points '(9))
    :command-form '((cursorfree--pusher "word")
                    (alist-get "next" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))
    :from-same-buffer t))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Next test next next test next"
             :points '(30))
    :after (make-cursorfree--test-buffer-state
            :string "Next test next test next"
            :points '(25))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 11 15)))
                    (alist-get "next" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-preve ()
  "preve."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "A test test here here test test"
             :points '(18))
    :after (make-cursorfree--test-buffer-state
            :string "A test here here test test"
            :points '(13))
    :command-form '((cursorfree--pusher "test")
                    (alist-get "preve" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))
    :from-same-buffer t))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "A test o test and c test d test"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "A test o and c test d test"
            :points '(1))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 21 25)))
                    (alist-get "preve" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-trim ()
  "trim."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "A simple test    "
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "A simple     "
            :points '(10))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 10 18)))
                    (alist-get "trim" cursorfree-modifiers nil nil #'equal)
                    (alist-get "change" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Another test\n"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "\n"
            :points '(1))
    :command-form '((alist-get "everything" cursorfree-modifiers nil nil #'equal)
                    (alist-get "trim" cursorfree-modifiers nil nil #'equal)
                    (alist-get "change" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-break ()
  "break."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Break this line into two"
             :points '(25))
    :after (make-cursorfree--test-buffer-state
            :string "Break this line \ninto two"
            :points '(26))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 17 21)))
                    (alist-get "break" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-parallel-chuck ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "chucking a parallel target"
             :points '(10))
    :after (make-cursorfree--test-buffer-state
            :string "a target"
            :points '(1))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 1 9)))
                    (cursorfree--pusher (cursorfree-make-target (cons 12 20)))
                    (alist-get "smash" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-parallel-outside ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Getting (the) outside of (a) parallel target"
             :points '(45))
    :after (make-cursorfree--test-buffer-state
            :string "Getting outside of parallel target"
            :points '(35))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 10 13)))
                    (cursorfree--pusher (cursorfree-make-target (cons 27 28)))
                    (alist-get "smash" cursorfree-modifiers nil nil #'equal)
                    (alist-get "outside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-parallel-inside ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Getting (the) outside of (a) parallel target"
             :points '(45))
    :after (make-cursorfree--test-buffer-state
            :string "Getting () outside of () parallel target"
            :points '(41))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 10 13)))
                    (cursorfree--pusher (cursorfree-make-target (cons 27 28)))
                    (alist-get "smash" cursorfree-modifiers nil nil #'equal)
                    (alist-get "inside" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-outside-swap ()
  (cursorfree--run-test
    (make-cursorfree--test-parameters
     :before (make-cursorfree--test-buffer-state
              :string "Bringing (correct) to (word) position"
              :points '(20))
     :after (make-cursorfree--test-buffer-state
             :string "Bringing (word) to (correct) position"
             :points '(17))
     :command-form '((cursorfree--pusher (cursorfree-make-target (cons 11 18)))
                     (alist-get "outside" cursorfree-modifiers nil nil #'equal)
                     (cursorfree--pusher (cursorfree-make-target (cons 24 28)))
                     (alist-get "outside" cursorfree-modifiers nil nil #'equal)
                     (alist-get "swap" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-parallel-pre ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "I will put the cursor before multiple elements"
             :points '(47))
    :after (make-cursorfree--test-buffer-state
            :string "I will put the cursor before multiple elements"
            :points '(8 16 30))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 8 11)))
                    (cursorfree--pusher (cursorfree-make-target (cons 16 22)))
                    (cursorfree--pusher (cursorfree-make-target (cons 30 38)))
                    (alist-get "smash" cursorfree-modifiers nil nil #'equal)
                    (alist-get "pre" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-line ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Simple\nmultiple lines of\ntext"
             :points '(30))
    :after (make-cursorfree--test-buffer-state
            :string "Simple\ntext"
            :points '(12))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 17 22)))
                    (alist-get "line" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Multiple\nlines\nhere"
             :points '(15))
    :after (make-cursorfree--test-buffer-state
            :string "Multiple\n\nhere"
            :points '(10))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 15 15)))
                    (alist-get "line" cursorfree-modifiers nil nil #'equal)
                    (alist-get "change" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-that ()
  "that."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Change brought that "
             :points '(21))
    :after (make-cursorfree--test-buffer-state
            :string "Change brought that "
            :points '(21))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 8 15)))
                    (alist-get "bring" cursorfree-actions nil nil #'equal)
                    (alist-get "that" cursorfree-modifiers nil nil #'equal)
                    (alist-get "change" cursorfree-actions nil nil #'equal))
    :from-same-buffer t)))

(ert-deftest cursorfree--test-source ()
  "source."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "Change source thing "
             :points '(21))
    :after (make-cursorfree--test-buffer-state
            :string "Change  thing source"
            :points '(8))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 8 14)))
                    (alist-get "bring" cursorfree-actions nil nil #'equal)
                    (alist-get "source" cursorfree-modifiers nil nil #'equal)
                    (alist-get "change" cursorfree-actions nil nil #'equal))
    :from-same-buffer t)))

(ert-deftest cursorfree--test-puff ()
  "puff."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string ""
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "

"
            :points '(2))
    :command-form '((alist-get "this" cursorfree-modifiers nil nil #'equal)
                    (alist-get "puff" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-line ()
  "line."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a\n   b"
             :points '(7))
    :after (make-cursorfree--test-buffer-state
            :string "   b"
            :points '(5))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 1 2)))
                    (alist-get "line" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  ;; Putting a line after arbitrary region puts it on the next line instead.
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a line\nanother one"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "another one\na line"
            :points '(1))
    :command-form
    '(cursorfree--target-move
      (cursorfree-line (cursorfree-make-target (cons 1 1)))
      (cursorfree-make-target (cons 9 10))
      :putter #'cursorfree--put-after)))

  ;; Same but before.
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a line\nanother one"
             :points '(1))
    :after (make-cursorfree--test-buffer-state
            :string "another one\na line\n"
            :points '(1))
    :command-form
    '(cursorfree--target-move
      (cursorfree-line (cursorfree-make-target (cons 9 10)))
      (cursorfree-make-target (cons 1 1))
      :putter #'cursorfree--put-before))))

(ert-deftest cursorfree--test-block ()
  "block."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a\nparagraph\nblock\n\nb"
             :points '(7))
    :after (make-cursorfree--test-buffer-state
            :string "b"
            :points '(1))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 1 2)))
                    (alist-get "block" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a\nblock with\nadded space next\n \nb"
             :points '(7))
    :after (make-cursorfree--test-buffer-state
            :string "b"
            :points '(1))
    :command-form '((cursorfree--pusher (cursorfree-make-target (cons 1 2)))
                    (alist-get "block" cursorfree-modifiers nil nil #'equal)
                    (alist-get "chuck" cursorfree-actions nil nil #'equal)))))

(ert-deftest cursorfree--test-beginning-end-of ()
  "cursorfree-beginning, cursorfree-end."
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "abcde"
             :points '(3))
    :after (make-cursorfree--test-buffer-state
            :string "abcde"
            :points '(1))
    :command-form '(cursorfree-target-jump-end (cursorfree-beginning (cursorfree-everything)))))

  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "abcde"
             :points '(3))
    :after (make-cursorfree--test-buffer-state
            :string "abcde"
            :points '(6))
    :command-form '(cursorfree-target-jump-beginning (cursorfree-end (cursorfree-everything))))))

(ert-deftest cursorfree--test-outside-escaped-delimiter ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a(hello ?\\) oop)b"
             :points '(4))
    :after (make-cursorfree--test-buffer-state
            :string "ab"
            :points '(2))
    :command-form '(cursorfree-target-chuck
                    (cursorfree-outer-parenthesis-dwim))
    :setup (lambda () (emacs-lisp-mode)))))

(ert-deftest cursorfree--test-outside-delimiter-in-string ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a(hello \" ) \" oop)b"
             :points '(4))
    :after (make-cursorfree--test-buffer-state
            :string "ab"
            :points '(2))
    :command-form '(cursorfree-target-chuck
                    (cursorfree-outer-parenthesis-dwim))
    :setup (lambda () (emacs-lisp-mode)))))

(ert-deftest cursorfree--test-outside-delimiter-in-comment ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "a(hello \n;;)\n oop)b"
             :points '(4))
    :after (make-cursorfree--test-buffer-state
            :string "ab"
            :points '(2))
    :command-form '(cursorfree-target-chuck
                    (cursorfree-outer-parenthesis-dwim))
    :setup (lambda () (emacs-lisp-mode)))))

(ert-deftest cursorfree--test-outside-in-string ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "(hello \"(a b c)\")"
             :points '(12))
    :after (make-cursorfree--test-buffer-state
            :string "(hello \"\")"
            :points '(9))
    :command-form '(cursorfree-target-chuck
                    (cursorfree-outer-parenthesis-dwim))
    :setup (lambda () (emacs-lisp-mode)))))

(ert-deftest cursorfree--test-outside-in-comment ()
  (cursorfree--run-test
   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "(hello \n ;; (a b c)\n)"
             :points '(15))
    :after (make-cursorfree--test-buffer-state
            :string "(hello \n ;;\n)"
            :points '(12))
    :command-form '(cursorfree-target-chuck
                    (cursorfree-outer-parenthesis-dwim))
    :setup (lambda () (emacs-lisp-mode)))

   (make-cursorfree--test-parameters
    :before (make-cursorfree--test-buffer-state
             :string "(hello \n ;; \"a b c\"\n)"
             :points '(15))
    :after (make-cursorfree--test-buffer-state
            :string "(hello \n ;;\n)"
            :points '(12))
    :command-form '(cursorfree-target-chuck
                    (cursorfree-outer-parenthesis-dwim))
    :setup (lambda () (emacs-lisp-mode)))))

;;; test.el ends here
