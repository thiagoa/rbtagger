;;; test-helper.el  --- Test helper for rbtagger  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thiago Araújo Silva

;; Author: Thiago Araújo <thiagoaraujos@gmail.com>
;; Maintainer: Thiago Araújo <thiagoaraujos@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'seq)

(load (expand-file-name "support/test-deps-helper.el") nil t)

(defun run-all-tests ()
  "Run all test in an idempotent way."
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert-run-tests-interactively t))

(defun trim-blank-chars (string)
  "Trim spaces and newlines of a STRING."
  (string-join
   (seq-filter
    (lambda (s) (not (equal "" s)))
    (mapcar #'string-trim (split-string string "\n")))
   "\n"))

(defmacro should-error-with-message (message body)
  "Extend should-error with message assertion.
Takes MESSAGE and &BODY."
  `(let ((e (should-error ,body)))
     (should (string-match ,message (error-message-string e)))))

(defmacro test-with-file-contents (fixture term-to-seek &rest body)
  "Load FIXTURE into temp buffer, look for TERM-TO-SEEK, and run BODY."
  (let ((contents (with-temp-buffer
                    (insert-file-contents (concat "fixtures/" fixture))
                    (buffer-string))))
    `(test-with-buffer-contents ,contents ,term-to-seek ,@body)))

(defmacro test-with-buffer-contents (contents term-to-seek &rest body)
  "Load CONTENTS into temp buffer, look for TERM-TO-SEEK, and run BODY."
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))
     (ruby-mode)
     (search-forward ,term-to-seek)
     ,@body))

(defmacro with-temp-generate-tags-hook (&rest body)
  "Generate a temp rbtagger-after-generate-tag-hook and restore it thereafter.
Takes BODY."
  `(progn
     (setq hooks-backup rbtagger-after-generate-tag-hook)
     (setq rbtagger-after-generate-tag-hook nil)
     (add-hook
      'rbtagger-after-generate-tag-hook
      (lambda ,@body
        (setq rbtagger-after-generate-tag-hook hooks-backup)))))

;;; test-helper.el ends here
