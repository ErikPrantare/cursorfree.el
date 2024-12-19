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

(ert-deftest he--target-insert ()
  "target-insert."
  (with-temp-buffer
    (insert "aaa bbb")
    (he-i--target-insert
     (he--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
     "ccc")
    (should (string= (buffer-string) "aaa cccbbb"))))

(ert-deftest he--target-overwrite ()
  "target-overwrite."
  (with-temp-buffer
    (insert "aaa bbb")
    (he-i--target-overwrite
     (he--markify-region
         (cons (+ 4 (point-min))
               (point-max)))
     "ccc")
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
    (he-i--target-overwrite
     (he--markify-region
      (cons (+ 4 (point-min))
            (point-max)))
     "ccc")
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest he--target-change ()
  "target-change."
  (with-temp-buffer
    (insert "aaa bbb")
    (he-i--target-change
     (he--markify-region
      (cons (+ 2 (point-min))
            (+ 5 (point-min)))))
    (should (string= (buffer-string) "aabb"))
    (should (= (point) (+ 2 (point-min))))))

(ert-deftest he--target-bring ()
  "target-bring."
  (with-temp-buffer
    (insert "aaa ")
    (goto-char (point-max))
    (he-i--target-bring
     (he--markify-region
      (cons (point-min)
            (+ 3 (point-min)))))
    (should (string= (buffer-string) "aaa aaa"))))

(ert-deftest he--target-move ()
  "target-move."
  (with-temp-buffer
    (insert "aaa bbb ")
    (goto-char (point-max))
    (he-i--target-move
     (he--markify-region
      (cons (point-min)
            (+ 3 (point-min)))))
    (should (string= (buffer-string) "bbb aaa"))))

(ert-deftest he--target-chuck ()
  "target-chuck."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (goto-char (point-max))
    (he-i--target-chuck
     (he--markify-region
      (cons (+ 4 (point-min))
            (+ 7 (point-min)))))
    (should (string= (buffer-string) "aaa ccc"))))

(ert-deftest he--inner-parenthesis ()
  "inner-parenthesis, inner-parenthesis-any, inner-parenthesis-dwim."
  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he-i--target-chuck
     (he-i--inner-parenthesis
      (he--markify-region
       (cons (+ 2 (point-min))
             (+ 3 (point-min))))
      ?\())
    (should (string= (buffer-string) "()")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he--target-delete
     (he-i--inner-parenthesis
      (he--markify-region
       (cons (+ 2 (point-min))
             (+ 3 (point-min))))
      ?\[))
    (should (string= (buffer-string) "([] bbb ccc)")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he--target-delete
     (he-i--inner-parenthesis-any
      (he--markify-region
       (cons (+ 2 (point-min))
             (+ 3 (point-min))))))
    (should (string= (buffer-string) "([] bbb ccc)")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he--evaluate
     (list
      (lambda (environment)
        (he--push-value-pure environment
          (cons (+ (point-min) 2) (+ (point-min) 3))))
      (he--get-instruction 'inner-parenthesis-dwim)
      (he--get-instruction 'target-chuck)))
    (should (string= (buffer-string) "([] bbb ccc)")))

  (with-temp-buffer
    (insert "([aaa] bbb ccc)")
    (he--evaluate
     (list
      (lambda (environment)
        (he--push-value-pure environment
          (cons (+ (point-min) 2) (+ (point-min) 3))))
      (lambda (environment)
        (he--push-value-pure environment ?\())
      (he--get-instruction 'inner-parenthesis-dwim)
      (he--get-instruction 'target-chuck)))
    (should (string= (buffer-string) "()")))

  (with-temp-buffer
    (insert "(\"aaa\" bbb ccc)")
    (he--evaluate
     (list
      (lambda (environment)
        (he--push-value-pure environment
          (cons (+ (point-min) 2) (+ (point-min) 3))))
      (he--get-instruction 'inner-parenthesis-dwim)
      (he--get-instruction 'target-chuck)))
    (should (string= (buffer-string) "(\"\" bbb ccc)"))))

(ert-deftest he--wrap-parentheses ()
  "target-wrap-parentheses."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (he-i--target-wrap-parentheses
     (he--markify-region (cons (+ (point-min) 4) (+ (point-min) 7)))
     ?{)
    (should (string= (buffer-string) "aaa {bbb} ccc")))

  ;; Non-parentheses use same character for both ends
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (he-i--target-wrap-parentheses
     (he--markify-region (cons (+ (point-min) 4) (+ (point-min) 7)))
     ?$)
    (should (string= (buffer-string) "aaa $bbb$ ccc"))))

(ert-deftest he--targets-join ()
  "he--targets-join."
  (should (equal (he--markify-region '(5 . 100))
                 (he--targets-join
                  '((43 . 30)
                    (5 . 20)
                    (65 . 100)
                    (23 . 25))))))

;; Local Variables:
;; read-symbol-shorthands: (("he-" . "hatty-edit-"))
;; End:
;;; test.el ends here
