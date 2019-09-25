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

(defvar available-ruby-modes '(ruby-mode enh-ruby-mode))

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

(defun join-syms (sym1 sym2 &optional sep)
  "Concat the symbols SYM1 and SYM2 with SEP."
  (or sep (setq sep "-for-"))
  (intern (concat (symbol-name sym1) sep (symbol-name sym2))))

(defmacro rbtagger-file-deftest (test-name test-args &rest body)
  "Dynamically define a file test for each Ruby mode.
See `rbtagger-file-deftest-for-mode' for more information.
Takes TEST-NAME, TEST-ARGS and BODY."
  `(progn
     ,@(mapcar
        (lambda (m)
          (rbtagger-file-deftest-for-mode m test-name test-args body))
        available-ruby-modes)))

(defmacro rbtagger-buffer-deftest (test-name test-args &rest body)
  "Dynamically define a buffer test for each Ruby mode.
See `rbtagger-buffer-deftest-for-mode' for more information.
Takes TEST-NAME, TEST-ARGS and BODY."
  `(progn
     ,@(mapcar
        (lambda (mode)
          (rbtagger-buffer-deftest-for-mode mode test-name test-args body))
        available-ruby-modes)))

(defun rbtagger-file-deftest-for-mode (mode test-name test-args body)
  "Define a file test named TEST-NAME for the major mode MODE.
A file test is a test that will create a temporary buffer with
the contents of the pointed file via `test-with-file-contents'.
M and BODY get delegated to `test-with-file-contents'.
TEST-ARGS are the optional arguments to `ert-deftest'."
  `(ert-deftest ,(join-syms test-name mode) ,test-args
     (test-with-file-contents ,mode ,@body)))

(defun rbtagger-buffer-deftest-for-mode (mode test-name test-args body)
  "Define a buffer test named TEST-NAME for the major mode MODE.
A buffer test is a test that will create a temporary buffer with
the passed string as contents via `test-with-buffer-contents'.  M
and BODY get delegated to `test-with-buffer-contents'.  TEST-ARGS
are the optional arguments to `ert-deftest'."
  `(ert-deftest ,(join-syms test-name mode) ,test-args
     (test-with-buffer-contents ,mode ,@body)))

(progn
  (put 'rbtagger-file-deftest 'lisp-indent-function 2)
  (put 'rbtagger-buffer-deftest 'lisp-indent-function 2))

(defmacro test-with-file-contents (mode fixture term-to-seek &rest body)
  "Load FIXTURE into temp buffer under MODE, look for TERM-TO-SEEK, and run BODY."
  (let ((contents (with-temp-buffer
                    (insert-file-contents (concat "fixtures/" fixture))
                    (buffer-string))))
    `(test-with-buffer-contents ,mode ,contents ,term-to-seek ,@body)))

(defmacro test-with-buffer-contents (mode contents term-to-seek &rest body)
  "Load CONTENTS into temp buffer under MODE, look for TERM-TO-SEEK, and run BODY."
  (let ((mode-code (if (eq mode 'enh-ruby-mode)
                       '(progn
                          (enh-ruby-mode)
                          (erm-wait-for-parse)
                          (font-lock-fontify-buffer))
                     `(,mode))))
    `(with-temp-buffer
       (insert ,contents)
       ,mode-code
       (goto-char (point-min))
       (search-forward ,term-to-seek)
       ,@body)))

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
