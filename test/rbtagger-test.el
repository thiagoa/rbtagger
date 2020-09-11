;;; rbtgger-test.el  --- Tests for rbtagger.el  -*- lexical-binding: t; -*-

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

;; More verbose than load-file but prints no messages
(load (expand-file-name "test-helper.el") nil t)
(load (expand-file-name "../rbtagger.el") nil t)

(install-test-deps '(ert-async with-simulated-input enh-ruby-mode))

(load (expand-file-name "support/workarounds.el"))

(local-set-key (kbd "C-c C-r") #'run-all-tests)

(rbtagger-file-deftest rbtagger-find-candidates-zero-level-nesting ()
  "unit/general_example.rb"
  "zero_level_nesting"
  (should (equal () (rbtagger-find-candidates))))

(rbtagger-file-deftest rbtagger-find-candidates-first-two-level-nesting ()
  "unit/general_example.rb"
  "first_two_level_nesting"
  (should (equal '("One::Two" "One") (rbtagger-find-candidates))))

(rbtagger-file-deftest rbtagger-find-candidates-second-two-level-nesting ()
  "unit/general_example.rb"
  "second_two_level_nesting"
  (should (equal '("One::Three" "One") (rbtagger-find-candidates))))

(rbtagger-file-deftest rbtagger-find-candidates-three-level-nesting ()
  "unit/general_example.rb"
  "three_level_nesting"
  (should (equal '("One::Three::Four" "One::Three" "One")
                 (rbtagger-find-candidates))))

(rbtagger-file-deftest rbtagger-find-candidates-second-one-level-nesting ()
  "unit/general_example.rb"
  "second_one_level_nesting"
  (should (equal '("Five") (rbtagger-find-candidates))))

(rbtagger-file-deftest rbtagger-find-candidates-third-two-level-nesting ()
  "unit/general_example.rb"
  "third_two_level_nesting"
  (should (equal '("Five::Six" "Five") (rbtagger-find-candidates))))

(rbtagger-file-deftest rbtagger-find-candidates-module-on-first-of-line ()
  "unit/module_on_first_line.rb"
  "module One"
  (should (equal '() (rbtagger-find-candidates))))

(rbtagger-file-deftest rbtagger-find-candidates-module-on-first-of-line-one-level-nesting ()
  "unit/module_on_first_line.rb"
  "inside_module"
  (should (equal (rbtagger-find-candidates) '("One"))))

(rbtagger-file-deftest rbtagger-find-candidates-random-code-mistaken-as-module ()
  "unit/edge_case_file.rb"
  "where_the_error_could_happen"
  (should (equal (rbtagger-find-candidates) '("Foo::Bar" "Foo"))))

(rbtagger-file-deftest rbtagger-find-candidates-ignore-singleton-class ()
  "unit/edge_case_file.rb"
  "singleton_class"
  (should (equal (rbtagger-find-candidates) '("Bar"))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-constant ()
  "module Bat\n  Foobar::Baz.something\nend"
  "Foo"
  (should (equal "Foobar::Baz" (rbtagger-symbol-at-point))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-symbol ()
  "module Bat\n  foo(:symbol)\nend"
  "symb"
  (should (equal "symbol" (rbtagger-symbol-at-point))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-symbol-on-beginning-of-file ()
  "Module"
  "Modul"
  (should (equal "Module" (rbtagger-symbol-at-point))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-point-one-char-after-symbol ()
  "module MyMod"
  "MyMod"
  (should (equal "MyMod" (rbtagger-symbol-at-point))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-method-call ()
  "module Bat\n  Bar.my_method\nend"
  "my_method"
  (should (equal "my_method" (rbtagger-symbol-at-point))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-method-call-with-bang ()
  "module Bat\n  Bar.my_method!\nend"
  "my_method"
  (should (equal "my_method!" (rbtagger-symbol-at-point))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-predicate-method-call ()
  "module Bat\n  Bar.works?\nend"
  "work"
  (should (equal "works?" (rbtagger-symbol-at-point))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-method-call-with-args ()
  "module Bat\n  Bar.another_method(1, 2, 3)\nend"
  "another_method"
  (should (equal "another_method" (rbtagger-symbol-at-point))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-point-on-beginning-of-file-no-symbol ()
  " Module"
  ""
  (goto-char (point-min))
  (should (equal "" (rbtagger-symbol-at-point))))

(rbtagger-buffer-deftest rbtagger-symbol-at-point-top-level-constant ()
  "module Bat\n  ::Top::Level.bar\nend"
  "Top"
  (should (equal "::Top::Level" (rbtagger-symbol-at-point))))

;; xref-find-definitions would present a list of two tags to choose
;; from: Base and ActiveRecord::Base, while rbtagger jumps straight to
;; ActiveRecord::Base
(ert-deftest rbtagger-find-definitions-lookup-accuracy-with-minor-mode-enabled ()
  (find-file "fixtures/integration/root.rb")
  (rbtagger-mode)
  (visit-tags-table "TAGS")
  (goto-char (point-min))
  (search-forward "Base")
  (backward-word)
  (execute-kbd-macro (kbd "M-."))
  (should (string-suffix-p "active_record/base.rb" buffer-file-name))
  (should (looking-at "  class Base")))

(ert-deftest rbtagger-find-definitions-when-point-is-on-non-symbol-then-types-in-existing-symbol ()
  (find-file "fixtures/integration/root.rb")
  (visit-tags-table "TAGS")
  (goto-char (point-min))
  (search-forward "def")
  (beginning-of-line)
  (with-simulated-input "ActiveRecord::Base RET"
    (call-interactively 'rbtagger-find-definitions))
  (should (string-suffix-p "active_record/base.rb" buffer-file-name))
  (should (looking-at "  class Base")))

(ert-deftest rbtagger-find-definitions-when-point-is-on-non-symbol-then-types-in-nonexisting-symbol ()
  (find-file "fixtures/integration/root.rb")
  (visit-tags-table "TAGS")
  (goto-char (point-min))
  (search-forward "def")
  (beginning-of-line)
  (should-error-with-message
   "No definitions for Bananas found"
   (with-simulated-input "Bananas RET"
     (call-interactively 'rbtagger-find-definitions))))

(ert-deftest rbtagger-find-definitions-with-universal-argument ()
  (find-file "fixtures/integration/root.rb")
  (visit-tags-table "TAGS")
  (goto-char (point-min))
  (search-forward "call")
  (backward-word)
  (let ((current-window (get-buffer-window)))
    (with-simulated-input "ActiveRecord::Base RET"
      (setq current-prefix-arg '(4))
      (call-interactively 'rbtagger-find-definitions))
    (should (equal (get-buffer-window) current-window))
    (should (string-suffix-p "base.rb" buffer-file-name))
    (should (looking-at "  class Base"))))

(ert-deftest rbtagger-find-definitions-other-window ()
  (find-file "fixtures/integration/root.rb")
  (visit-tags-table "TAGS")
  (goto-char (point-min))
  (search-forward "Base")
  (backward-word)
  (let ((current-frame (selected-frame))
        (current-window (get-buffer-window)))
    (call-interactively 'rbtagger-find-definitions-other-window)
    (should (equal (selected-frame) current-frame))
    (should (not (equal (get-buffer-window) current-window)))
    (should (string-suffix-p "base.rb" buffer-file-name))
    (should (looking-at "  class Base"))))

(ert-deftest rbtagger-find-definitions-other-window-with-universal-argument ()
  (find-file "fixtures/integration/root.rb")
  (visit-tags-table "TAGS")
  (goto-char (point-min))
  (search-forward "def")
  (backward-word)
  (let ((current-frame (selected-frame))
        (current-window (get-buffer-window)))
    (with-simulated-input "ActiveRecord::Base RET"
      (setq current-prefix-arg '(4))
      (call-interactively 'rbtagger-find-definitions-other-window))
    (should (equal (selected-frame) current-frame))
    (should (not (equal (get-buffer-window) current-window)))
    (should (string-suffix-p "base.rb" buffer-file-name))
    (should (looking-at "  class Base"))))

(ert-deftest rbtagger-find-definitions-other-window-with-keybinding ()
  (find-file "fixtures/integration/root.rb")
  (visit-tags-table "TAGS")
  (goto-char (point-min))
  (search-forward "Base")
  (backward-word)
  (let ((current-frame (selected-frame))
        (current-window (get-buffer-window)))
    (execute-kbd-macro (kbd "C-c C-."))
    (should (equal (selected-frame) current-frame))
    (should (not (equal (get-buffer-window) current-window)))
    (should (string-suffix-p "base.rb" buffer-file-name))
    (should (looking-at "  class Base"))))

(ert-deftest rbtagger-find-definitions-when-point-is-on-non-symbol-then-types-in-empty-symbol ()
  (find-file "fixtures/integration/root.rb")
  (visit-tags-table "TAGS")
  (goto-char (point-min))
  (search-forward "def")
  (beginning-of-line)
  (should-error-with-message
   "Please, specify a symbol!"
   (with-simulated-input "RET" (call-interactively 'rbtagger-find-definitions))))

(ert-deftest rbtagger-without-rbtaggers-tag-lookup-is-not-accurate ()
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

;;; rbtagger-test.el ends here
