;;; rbtagger.el --- Ruby tagging tools -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thiago Araújo Silva
;;
;; Author: Thiago Araújo <thiagoaraujos@gmail.com>
;; Maintainer: Thiago Araújo <thiagoaraujos@gmail.com>
;; URL: http://www.github.com/thiagoa/rbtagger
;; Version: 0.1
;; Package-Requires: ((emacs "25"))
;; Keywords: languages, tools

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RbTagger is an Emacs library based on ctags, ripper-tags, and
;; xref.el.  It indexes your entire Ruby project along with gems and
;; provides smarter than average tag lookup.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'ruby-mode)
(require 'enh-ruby-mode nil 'noerror)

(defconst rbtagger-module-regex "^[\s]*\\(class\\|module\\) \\([^\s<]+\\)"
  "The regex to match Ruby modules.")

(defconst rbtagger-symbol-regex "\\(\\sw\\|\\s_\\|:\\)+"
  "The regex to scan for Ruby symbols.")

(defgroup rbtagger nil
  "ctags-based Emacs utility to index Ruby projects."
  :prefix "rbtagger-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/thiagoa/rbtagger"))

(defcustom rbtagger-stdout-buffer "*rbtagger-log: %s*"
  "The buffer name for `rbtagger-generate-tags` output log.
You can include the project name with the %s format string."
  :type 'string
  :group 'rbtagger)

(defcustom rbtagger-stderr-buffer "*rbtagger-error-log: %s*"
  "The buffer name for `rbtagger-generate-tags` error log.
You can include the project name with the %s format string."
  :type 'string
  :group 'rbtagger)

(defcustom rbtagger-generate-tags-bin (concat (file-name-directory load-file-name)
                                              "bin/ruby_index_tags")
  "The full path to the script that generates the TAGS file.
The script should take a \"directory\" argument or use the
current directory otherwise."
  :type 'string
  :group 'rbtagger)

(defcustom rbtagger-after-generate-tag-hook nil
  "Hooks to run after `rbtagger-generate-tags'."
  :type 'hook
  :group 'rbtagger)

;;;###autoload
(defun rbtagger-find-definitions ()
  "Find definitions for the Ruby symbol at point.
This function reads the current Ruby buffer and builds a tag
candidates list, then it loops through the list and calls
`xref-find-definitions` on each candidate.  It is assumed that
your tags file was parsed with ripper-tags --emacs and --extra=q
options."
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
  "Figure out Ruby symbol at point by scanning current buffer.
An easier way to do this would be to use `symbol-at-point`, but
there are differences between `ruby-mode` and `enh-ruby-mode`
where one will return a full symbol like Foo::Bar and the other
will return just Foo due to syntax table differences.  In
`enh-ruby-mode` syntax table, colon is part of symbols but not in
`ruby-mode`."
  (cl-flet ((not-beginning-of-buffer-p () (not (eq (point) (point-min)))))
    (save-excursion
      (if (and (not-beginning-of-buffer-p)
               (not (looking-at rbtagger-symbol-regex)))
          (backward-char))
      (while (and (not-beginning-of-buffer-p)
                  (looking-at rbtagger-symbol-regex))
        (backward-char))
      (if (not-beginning-of-buffer-p) (forward-char))
      (let ((symbol-start-point (point))
            symbol-end-point
            tag)
        (while (looking-at rbtagger-symbol-regex)
          (forward-char))
        (setq symbol-end-point (point))
        (when (not (eq symbol-start-point symbol-end-point))
          (setq tag (substring-no-properties
                     (buffer-substring symbol-start-point symbol-end-point)))
          (replace-regexp-in-string "^:\\([^:]+\\)" "\\1" tag))))))

(defun rbtagger-current-indent-level ()
  "Return indentation level according to Ruby mode."
  (if (and (eq major-mode 'enh-ruby-mode))
      enh-ruby-indent-level
    ruby-indent-level))

(defun rbtagger-find-candidates ()
  "Find Ruby modules until nesting level at point.
This is a simple regex-based and indentation-based function to
return a list of Ruby modules.  If point is under modules 'One'
and 'Two', for example, this function will return '(list \"One::Two\"
\"One\")."
  (save-excursion
    (let ((line (line-number-at-pos))
          (indent-level (rbtagger-current-indent-level))
          (last-indent 0)
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

(defun rbtagger--sentinel (project-name)
  "Generate the sentinel to run after `rbtagger-generate-tags'.
Takes PROJECT-NAME."
  (lambda (process _msg)
    (let ((success (and (memq (process-status process) '(exit signal))
                        (eq (process-exit-status process) 0))))
      (run-hook-with-args 'rbtagger-after-generate-tag-hook success project-name)
      (if success
          (message "Ruby tags successfully generated")
        (message "ERROR: Ruby tags generation failed!")))))

;;;###autoload
(defun rbtagger-generate-tags (project-dir &optional generate-tags-bin)
  "Generate Ruby tags for the current git project.
Takes PROJECT-DIR and optionally GENERATE-TAGS-BIN.  If GENERATE_TAGS-BIN
is not passed, it uses the `rbtagger-generate-tags` setting."
  (interactive (list (locate-dominating-file default-directory ".git")))
  (let* ((project-dir (expand-file-name (string-remove-suffix "/" project-dir)))
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
      (dolist (b (list buffer stderr))
        (with-current-buffer b (erase-buffer)))
      (make-process :name process-name
                    :buffer buffer
                    :stderr stderr
                    :command command
                    :sentinel sentinel))))

(provide 'rbtagger)
;;; rbtagger.el ends here
