;;; rbtgger-test.el  --- Tests for rbtagger.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thiago Araújo Silva

;; Author: Thiago Araújo <thiagoaraujos@gmail.com>
;; Maintainer: Thiago Araújo <thiagoaraujos@gmail.com>
;; Version: 0.0.1

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

(load-file "test-helper.el")
(load-file "../rbtagger.el")

(if noninteractive (configure-test-dependencies))

(local-set-key (kbd "C-c C-r") #'run-all-tests)

(ert-deftest rbtagger-find-candidates-zero-level-nesting ()
  (test-with-file-contents
   "unit/general_example.rb"
   "zero_level_nesting"
   (should (equal () (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-one-level-nesting ()
  (test-with-file-contents
   "unit/general_example.rb"
   "one_level_nesting"
   (should (equal '("One") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-first-two-level-nesting ()
  (test-with-file-contents
   "unit/general_example.rb"
   "first_two_level_nesting"
   (should (equal '("One::Two" "One") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-second-two-level-nesting ()
  (test-with-file-contents
   "unit/general_example.rb"
   "second_two_level_nesting"
   (should (equal '("One::Three" "One") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-three-level-nesting ()
  (test-with-file-contents
   "unit/general_example.rb"
   "three_level_nesting"
   (should (equal '("One::Three::Four" "One::Three" "One")
                  (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-second-one-level-nesting ()
  (test-with-file-contents
   "unit/general_example.rb"
   "second_one_level_nesting"
   (should (equal '("Five") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-third-two-level-nesting ()
  (test-with-file-contents
   "unit/general_example.rb"
   "third_two_level_nesting"
   (should (equal '("Five::Six" "Five") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-module-on-first-of-line ()
  (test-with-file-contents
   "unit/module_on_first_line.rb"
   "module One"
   (should (equal '() (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-module-on-first-of-line-one-level-nesting ()
  (test-with-file-contents
   "unit/module_on_first_line.rb"
   "inside_module"
   (should (equal (rbtagger-find-candidates) '("One")))))

(ert-deftest rbtagger-find-candidates-random-code-mistaken-as-module ()
  (test-with-file-contents
   "unit/edge_case_file.rb"
   "where_the_error_could_happen"
   (should (equal (rbtagger-find-candidates) '("Foo::Bar" "Foo")))))

(ert-deftest rbtagger-find-candidates-ignore-singleton-class ()
  (test-with-file-contents
   "unit/edge_case_file.rb"
   "singleton_class"
   (should (equal (rbtagger-find-candidates) '("Bar")))))

(ert-deftest rbtagger-symbol-at-point-constant ()
  (test-with-buffer-contents
   "module Bat\n  Foobar::Baz.something\nend"
   "Foo"
   (should (equal "Foobar::Baz" (rbtagger-symbol-at-point)))))

(ert-deftest rbtagger-symbol-at-point-symbol ()
  (test-with-buffer-contents
   "module Bat\n  foo(:symbol)\nend"
   "symb"
   (should (equal "symbol" (rbtagger-symbol-at-point)))))

(ert-deftest rbtagger-symbol-at-point-symbol-on-beginning-of-file ()
  (test-with-buffer-contents
   "Module"
   "Modul"
   (should (equal "Module" (rbtagger-symbol-at-point)))))

(ert-deftest rbtagger-symbol-at-point-point-one-char-after-symbol ()
  (test-with-buffer-contents
   "module MyMod"
   "MyMod"
   (should (equal "MyMod" (rbtagger-symbol-at-point)))))

(ert-deftest rbtagger-symbol-at-point-method-call ()
  (test-with-buffer-contents
   "module Bat\n  Bar.my_method\nend"
   "my_method"
   (should (equal "my_method" (rbtagger-symbol-at-point)))))

(ert-deftest rbtagger-symbol-at-point-method-call-with-args ()
  (test-with-buffer-contents
   "module Bat\n  Bar.another_method(1, 2, 3)\nend"
   "another_method"
   (should (equal "another_method" (rbtagger-symbol-at-point)))))

(ert-deftest rbtagger-symbol-at-point-point-on-beginning-of-file-no-symbol ()
  (test-with-buffer-contents
   " Module"
   ""
   (goto-char (point-min))
   (should (equal nil (rbtagger-symbol-at-point)))))

(ert-deftest rbtagger-symbol-at-point-top-level-constant ()
  (test-with-buffer-contents
   "module Bat\n  ::Top::Level.bar\nend"
   "Top"
   (should (equal "Top::Level" (rbtagger-symbol-at-point)))))

(ert-deftest rbtagger-tag-lookup-accuracy ()
  (find-file "fixtures/integration/root.rb")
  (visit-tags-table "TAGS")
  (search-forward "Base")
  (backward-word)
  (call-interactively 'rbtagger-find-definitions)
  (should (string-suffix-p "active_record/base.rb" buffer-file-name))
  (should (looking-at "  class Base")))

(ert-deftest rbtagger-without-rbtagger-tag-lookup-is-not-accurate ()
  (find-file "fixtures/integration/root.rb")
  (visit-tags-table "TAGS")
  (search-forward "Base")
  (backward-word)
  (find-tag (find-tag-default))
  (should (string-suffix-p "base.rb" buffer-file-name))
  (should (looking-at "class Base")))

(ert-deftest rbtagger-generate-tags-invalid-project-dir ()
  (should-error-with-message
   "could not be found"
   (rbtagger-generate-tags "non_existing_directory"
                           "fixtures/bin/ruby_index_tags_success")))

(ert-deftest rbtagger-generate-tags-nil-project-dir ()
  (should-error-with-message
   "could not be found"
   (rbtagger-generate-tags nil "fixtures/bin/ruby_index_tags_success")))

(ert-deftest rbtagger-generate-tags-invalid-binary-path ()
  (should-error-with-message
   "could not be found"
   (rbtagger-generate-tags
    "fixtures"
    "fixtures/bin/i_dont_exist")))

(ert-deftest rbtagger-generate-tags-binary-not-executable ()
  (should-error-with-message
   "is not executable"
   (rbtagger-generate-tags
    "fixtures"
    "fixtures/bin/ruby_index_tags_not_executable")))

(ert-deftest-async rbtagger-generate-tags-success (done)
  (with-temp-generate-tags-hook
   (success project-name)
   (should (equal "fixtures" project-name))
   (should success)
   (let ((log-buffer (get-buffer "*rbtagger-log: fixtures*")))
     (should log-buffer)
     (should (equal
              "Success!"
              (with-current-buffer log-buffer
                (trim-blank-chars (buffer-string))))))
   (funcall done))
  (rbtagger-generate-tags
   "fixtures"
   "fixtures/bin/ruby_index_tags_success"))

(ert-deftest-async rbtagger-generate-tags-error (done)
  (with-temp-generate-tags-hook
   (success project-name)
   (should (equal "fixtures" project-name))
   (should-not success)
   (let ((log-buffer (get-buffer "*rbtagger-error-log: fixtures*")))
     (should log-buffer)
     (should (equal
              "Error!"
              (with-current-buffer log-buffer
                (trim-blank-chars (buffer-string))))))
   (funcall done))
  (rbtagger-generate-tags
   "fixtures"
   "fixtures/bin/ruby_index_tags_error"))

(ert-deftest-async rbtagger-generate-tags-args-should-be-expanded (done)
  (with-temp-generate-tags-hook
   (success project-name)
   (should success)
   (let ((log-buffer (get-buffer "*rbtagger-log: fixtures*")))
     (should log-buffer)
     (should (equal
              (expand-file-name "fixtures")
              (with-current-buffer log-buffer
                (trim-blank-chars (buffer-string))))))
   (funcall done))
  (rbtagger-generate-tags
   "fixtures/../fixtures"
   "fixtures/bin/../bin/ruby_index_tags_echo_args"))

(provide 'rbtagger-test)
;;; rbtagger-test.el ends here
