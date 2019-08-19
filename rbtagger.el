;;; rbtagger.el  --- Ruby tagging tools -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thiago Araújo Silva

;; Author: Thiago Araújo <thiagoaraujos@gmail.com>
;; URL: http://www.github.com/thiagoa/rbtagger
;; Version: 0.1
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

(require 'ruby-mode)
(require 'cl-macs)

(defconst rbtagger-module-regex "^[\s]*\\(class\\|module\\) \\([^\s<]+\\)"
  "The regex used to match Ruby modules.")

(defcustom rbtagger-stdout-buffer "*rbtagger-log: %s*"
  "The buffer name for the tags generate output log.
To display the project name, include the %s formatting string."
  :type 'string
  :group 'rbtagger)

(defcustom rbtagger-stderr-buffer "*rbtagger-error-log: %s*"
  "The buffer name for tags generate error log.
To display the project name, include the %s formatting string."
  :type 'string
  :group 'rbtagger)

(defcustom rbtagger-generate-tags-bin (concat (file-name-directory load-file-name)
                                              "bin/ruby_index_tags")
  "The full path to the script responsible for generating the TAGS file.
The script must take an optional directory argument, otherwise it will
use the current directory."
  :type 'string
  :group 'rbtagger)

(defcustom rbtagger-after-generate-tag-hook nil
  "Hooks to run after `rbtagger-generate-tags'."
  :type 'hook
  :group 'rbtagger)

(defun rbtagger-find-definitions ()
  "Find definitions for the Ruby tag a point.
This function calls `xref-find-definitions` with a list of tag
candidates.  It reads the current Ruby buffer and tries to figure
out the current nesting level to build the tag candidates.  It is
assumed that your tags file was parsed with ripper-tags --emacs
and --extra=q options."
  (interactive)
  (let* ((tag (rbtagger-symbol-at-point))
         (top-level-constant-p (string-prefix-p "::" tag))
         (tag (replace-regexp-in-string "^::" "" tag))
         (candidates (if top-level-constant-p () (rbtagger-find-candidates)))
         (candidates (mapcar (lambda (c) (concat c "::" tag)) candidates))
         (candidates (append candidates (list tag)))
         (done nil))
    (while (and (not done) candidates)
      (ignore-errors
        (xref-find-definitions (pop candidates))
        (setq done t)))
    (if (not done) (error (concat "No definitions for " tag " found!")))))

(defun rbtagger-symbol-at-point ()
  "Figure out the Ruby symbol at point."
  (let ((tag (substring-no-properties (thing-at-point 'sexp))))
    (replace-regexp-in-string "^:\\([^:]+\\)" "\\1" tag)))

(defun rbtagger-current-indent-level ()
  "Return the indentation level depending on the current Ruby mode."
  (if (eq major-mode 'enh-ruby-mode)
      enh-ruby-indent-level
    ruby-indent-level))

(defun rbtagger-find-candidates ()
  "Find Ruby modules until nesting level at point.
This is a simple regex-based and indentation-based function to
return a list of Ruby modules.  If point is under modules 'One'
and 'Two', this function will return '(list \"One::Two\"
\"One\")."
  (save-excursion
    (let ((line (line-number-at-pos))
          (indent-level (rbtagger-current-indent-level))
          (last-indent 0)
          symbol
          modules
          nesting)
      (goto-char (point-min))
      (cl-flet ((filter-by-indent (modules current-indent)
                                  (seq-remove
                                   (lambda (tuple)
                                     (let ((module-indent (car tuple)))
                                       (>= module-indent current-indent)))
                                   modules)))
        (while (not (eq (line-number-at-pos) line))
          (let ((found-module (re-search-forward rbtagger-module-regex
                                                 (line-end-position)
                                                 t)))
            (when found-module
              (let* ((current-indent (current-indentation))
                     (symbol (rbtagger-symbol-at-point))
                     (offset (abs (- last-indent current-indent)))
                     found-module)
                (if (<= current-indent last-indent)
                    (dotimes (_ (/ (+ indent-level offset) indent-level))
                      (pop nesting)))
                (setq found-module (append (reverse nesting) (list symbol)))
                (setq modules (filter-by-indent modules current-indent))
                (push (cons current-indent found-module) modules)
                (push symbol nesting)
                (setq last-indent current-indent))))
          (forward-line 1))
        (setq modules (filter-by-indent modules (current-indentation))))
      (mapcar (lambda (tuple)
                (let ((module-name (cdr tuple)))
                  (string-join module-name "::"))) modules))))

(defun rbtagger-generate-tags (project-dir &optional generate-tags-bin)
  "Generate Ruby tags for the current git project.
Takes PROJECT-DIR and optionally GENERATE-TAGS-BIN.  If GENERATE_TAGS-BIN
is not passed, it uses `rbtagger-generate-tags` instead."
  (interactive (list (locate-dominating-file default-directory ".git")))
  (let ((project-dir (expand-file-name (string-remove-suffix "/" project-dir)))
        (generate-tags-bin (or generate-tags-bin rbtagger-generate-tags-bin))
        (generate-tags-bin (expand-file-name generate-tags-bin)))
    (unless (file-directory-p project-dir)
      (error "Project directory could not be found"))
    (unless (file-exists-p generate-tags-bin)
      (error "Binary to generate Ruby tags could not be found"))
    (unless (file-executable-p generate-tags-bin)
      (error "Binary to generate Ruby tags is not executable"))
    (let* ((project-name (file-name-base project-dir))
           (process-name (concat "rbtagger-" project-name))
           (buffer (get-buffer-create (format rbtagger-stdout-buffer project-name)))
           (command (list (file-truename generate-tags-bin) project-dir))
           (stderr (get-buffer-create (format rbtagger-stderr-buffer project-name)))
           (sentinel (rbtagger--sentinel project-name)))
      (mapc (lambda (b) (with-current-buffer b (erase-buffer))) (list buffer stderr))
      (make-process :name process-name
                    :buffer buffer
                    :stderr stderr
                    :command command
                    :sentinel sentinel))))

(defmacro rbtagger--sentinel (project-name)
  "Macro to generate the sentinel to run after rbtagger-generate-tags.
Takes PROJECT-NAME."
  `(lambda (process msg)
     (let ((success (and (memq (process-status process) '(exit signal))
                         (eq (process-exit-status process) 0))))
       (run-hook-with-args 'rbtagger-after-generate-tag-hook success ,project-name)
       (if success
           (message "Ruby tags successfully generated")
         (message "ERROR: Ruby tags generation failed!")))))

(provide 'rbtagger)
;;; rbtagger.el ends here
