;;; test.el --- Tests for cursorfree.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Erik Präntare

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

(ert-deftest cursorfree--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (cursorfree--target-overwrite
     (cursorfree--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
     "ccc")
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest cursorfree--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (cursorfree--evaluate
     `(,(cursorfree--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
       "ccc"
       target-overwrite))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest cursorfree--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (cursorfree--target-overwrite
     (cursorfree--markify-region
      (cons (+ 4 (point-min))
            (point-max)))
     "ccc")
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest cursorfree--target-change ()
  "target-change."
  (with-temp-buffer
    (insert "aaa bbb")
    (cursorfree--target-change
     (cursorfree--markify-region
      (cons (+ 2 (point-min))
            (+ 5 (point-min)))))
    (should (string= (buffer-string) "aabb"))
    (should (= (point) (+ 2 (point-min))))))

(ert-deftest cursorfree--target-bring ()
  "target-bring."
  (with-temp-buffer
    (insert "aaa ")
    (goto-char (point-max))
    (cursorfree--target-bring
     (cursorfree--markify-region
      (cons (point-min)
            (+ 3 (point-min)))))
    (should (string= (buffer-string) "aaa aaa"))))

(ert-deftest cursorfree--target-move ()
  "target-move."
  (with-temp-buffer
    (insert "aaa bbb ")
    (goto-char (point-max))
    (cursorfree--target-move
     (cursorfree--markify-region
      (cons (point-min)
            (+ 3 (point-min)))))
    (should (string= (buffer-string) "bbb aaa"))))

(ert-deftest cursorfree--target-chuck ()
  "target-chuck."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (goto-char (point-max))
    (cursorfree--target-chuck
     (cursorfree--markify-region
      (cons (+ 4 (point-min))
            (+ 7 (point-min)))))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest cursorfree--inner-parenthesis ()
  "inner-parenthesis, inner-parenthesis-any, inner-parenthesis-dwim."
  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (cursorfree--target-chuck
     (cursorfree--inner-parenthesis
      (cursorfree--markify-region
       (cons (+ 2 (point-min))
             (+ 3 (point-min))))
      ?\())
    (should (string= (buffer-string) "()")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (cursorfree--target-delete
     (cursorfree--inner-parenthesis
      (cursorfree--markify-region
       (cons (+ 2 (point-min))
             (+ 3 (point-min))))
      ?\[))
    (should (string= (buffer-string) "([] bbb ccc)")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (cursorfree--target-delete
     (cursorfree--inner-parenthesis-any
      (cursorfree--markify-region
       (cons (+ 2 (point-min))
             (+ 3 (point-min))))))
    (should (string= (buffer-string) "([] bbb ccc)")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (cursorfree--evaluate
     (list
      (cursorfree--pusher
        (cons (+ (point-min) 2) (+ (point-min) 3)))
      #'cursorfree--inner-parenthesis-dwim
      (cursorfree--to-action #'cursorfree--target-chuck)))
    (should (string= (buffer-string) "([] bbb ccc)")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (cursorfree--evaluate
     (list
      (cursorfree--pusher
        (cons (+ (point-min) 2) (+ (point-min) 3)))
      (cursorfree--pusher ?\()
      #'cursorfree--inner-parenthesis-dwim
      (cursorfree--to-action #'cursorfree--target-chuck)))
    (should (string= (buffer-string) "()")))

  (with-temp-buffer
    (insert "(\"aaa\" bbb ccc)")
    (cursorfree--evaluate
     (list
      (cursorfree--pusher
        (cons (+ (point-min) 2) (+ (point-min) 3)))
      #'cursorfree--inner-parenthesis-dwim
      (cursorfree--to-action #'cursorfree--target-chuck)))
    (should (string= (buffer-string) "(\"\" bbb ccc)"))))

(ert-deftest cursorfree--wrap-parentheses ()
  "target-wrap-parentheses."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (cursorfree--target-wrap-parentheses
     (cursorfree--markify-region (cons (+ (point-min) 4) (+ (point-min) 7)))
     ?{)
    (should (string= (buffer-string) "aaa {bbb} ccc")))

  ;; Non-parentheses use same character for both ends
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (cursorfree--target-wrap-parentheses
     (cursorfree--markify-region (cons (+ (point-min) 4) (+ (point-min) 7)))
     ?$)
    (should (string= (buffer-string) "aaa $bbb$ ccc"))))

(ert-deftest cursorfree--targets-join ()
  "cursorfree--targets-join."
  (should (equal (cursorfree--markify-region '(5 . 100))
                 (cursorfree--targets-join
                  '((43 . 30)
                    (5 . 20)
                    (65 . 100)
                    (23 . 25))))))

;;; test.el ends here
