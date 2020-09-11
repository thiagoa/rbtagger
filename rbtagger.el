;;; rbtagger.el --- Ruby tagging tools -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thiago Araújo Silva
;;
;; Author: Thiago Araújo <thiagoaraujos@gmail.com>
;; Maintainer: Thiago Araújo <thiagoaraujos@gmail.com>
;; URL: https://www.github.com/thiagoa/rbtagger
;; Version: 0.3.4
;; Package-Requires: ((emacs "25.1"))
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defconst rbtagger-symbol-syntax-chars "w_'"
  "The syntax chars to find Ruby symbols.
Used as argument to `skip-syntax-forward' and
`skip-syntax-backward'.  w is for word constituents; _ is for
symbol constituents; ' is for expression prefixes.  These chars
work for both `ruby-mode' and `enh-ruby-mode', although the
former considers @ and $ as prefix characters and the latter
doesn't.  There are no use cases for these extra prefix
characters, so we should be safe here.  See Info node `(elisp)
Syntax Class Table'")

(defvar enh-ruby-indent-level)

(defvar rbtagger-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'rbtagger-find-definitions)
    (define-key map (kbd "C-c C-.") 'rbtagger-find-definitions-other-window)
    (define-key map (kbd "C-c M-.") 'rbtagger-find-definitions-other-frame)
    map)
  "Keymap for function `rbtagger-mode'.")

(defgroup rbtagger nil
  "ctags-based Emacs utility to index Ruby projects."
  :prefix "rbtagger-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/thiagoa/rbtagger"))

(defcustom rbtagger-stdout-buffer "*rbtagger-log: %s*"
  "The buffer name for `rbtagger-generate-tags' output log.
You can include the project name with the %s format string."
  :type 'string
  :group 'rbtagger)

(defcustom rbtagger-stderr-buffer "*rbtagger-error-log: %s*"
  "The buffer name for `rbtagger-generate-tags' error log.
You can include the project name with the %s format string."
  :type 'string
  :group 'rbtagger)

(defcustom rbtagger-generate-tags-bin (concat (file-name-directory
                                               (or load-file-name (buffer-file-name)))
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
(defun rbtagger-find-definitions (symbol)
  "Find definitions for the Ruby SYMBOL at point.
This function reads the current Ruby buffer and builds a tag
candidates list, then it loops through the list and calls
`xref-find-definitions' on each candidate.  It is assumed that
your tags file was parsed with ripper-tags --emacs and --extra=q
options."
  (interactive
   (rbtagger--interactive-get-symbol current-prefix-arg))
  (rbtagger--find-definitions symbol :same-window))

;;;###autoload
(defun rbtagger-find-definitions-other-window (symbol)
  "Find definitions for the Ruby SYMBOL at point in another window.
See `rbtagger-find-definitions'."
  (interactive
   (rbtagger--interactive-get-symbol current-prefix-arg))
  (rbtagger--find-definitions symbol :other-window))

;;;###autoload
(defun rbtagger-find-definitions-other-frame (symbol)
  "Find definitions for the Ruby SYMBOL at point in another frame.
See `rbtagger-find-definitions'."
  (interactive
   (rbtagger--interactive-get-symbol current-prefix-arg))
  (rbtagger--find-definitions symbol :other-frame))

(defun rbtagger--interactive-get-symbol (choose-symbol)
  "Process the symbol to use for interactive functions.
If CHOOSE-SYMBOL is passed, lets the user pick from an
interactive list of xref symbols.  Otherwise, uses the symbol at
point."
  (let ((symbol (rbtagger-symbol-at-point)))
    (when (or choose-symbol (string= symbol ""))
      (setq symbol (completing-read (concat "Find definitions of"
                                            (if (string= symbol "")
                                                ""
                                              (concat " (" symbol ")"))
                                            ": ")
                                    (xref-backend-identifier-completion-table 'etags)
                                    nil nil nil
                                    'xref--read-identifier-history
                                    symbol))
      (if (string= symbol "") (error "Please, specify a symbol!")))
    (list symbol)))

(defun rbtagger--find-definitions (symbol where-to-open)
  "The function to actually find the definitions.
Takes SYMBOL and WHERE-TO-OPEN, which can be :same-window, :other-window or :other-frame."
  (let* ((top-level-constant-p (string-prefix-p "::" symbol))
         (symbol (replace-regexp-in-string "^::" "" symbol))
         (candidates (if top-level-constant-p () (rbtagger-find-candidates)))
         (candidates (mapcar (lambda (c) (concat c "::" symbol)) candidates))
         (candidates (append candidates (list symbol)))
         (done nil))
    (while (and (not done) candidates)
      (ignore-errors
        (let ((candidate (pop candidates)))
          (pcase where-to-open
            (:same-window
             (xref-find-definitions candidate))
            (:other-window
             (xref-find-definitions-other-window candidate))
            (:other-frame
             (xref-find-definitions-other-frame candidate))))
        (setq done t)))
    (if (not done) (error (concat "No definitions for " symbol " found!")))))

(defun rbtagger-symbol-at-point ()
  "Figure out Ruby symbol at point by scanning current buffer.
An easier way to do this would be to use `symbol-at-point', but
there are differences between the command `ruby-mode' and
`enh-ruby-mode' where one will return a full symbol like Foo::Bar
and the other will return just Foo due to syntax table
differences.  In `enh-ruby-mode' syntax table, colon is part of
symbols but not in the command `ruby-mode'."
  (let (symbol-start-point symbol-end-point tag)
    (save-excursion
      (skip-syntax-backward rbtagger-symbol-syntax-chars)
      (setq symbol-start-point (point))
      (skip-syntax-forward rbtagger-symbol-syntax-chars)
      (setq symbol-end-point
            (if (equal ?! (char-after (point)))
                (1+ (point)) ;; for enh-ruby-mode, which doesn't recognize "!"
              (point)))
      (setq tag (substring-no-properties
                 (buffer-substring symbol-start-point symbol-end-point)))
      (cond ((string-match "^::[A-Z]" tag) tag) ;; top-level constant
            ((string-prefix-p ":" tag) (substring tag 1)) ;; symbol
            (t tag)))))

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
    (let ((start-pos (line-beginning-position))
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
        (while (not (eq (point) start-pos))
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
        (message (concat "ERROR: Ruby tags generation failed! Please check "
                         (format rbtagger-stderr-buffer project-name)))))))

;;;###autoload
(defun rbtagger-generate-tags (project-dir &optional generate-tags-bin)
  "Generate Ruby tags for the current git project.
Takes PROJECT-DIR and optionally GENERATE-TAGS-BIN.  If GENERATE_TAGS-BIN
is not passed, it uses the `rbtagger-generate-tags' setting."
  (interactive (list (locate-dominating-file default-directory ".git")))
  (let* ((project-dir (or project-dir
                          (error "Project git directory could not be found")))
         (project-dir (expand-file-name (string-remove-suffix "/" project-dir)))
         (generate-tags-bin (or generate-tags-bin rbtagger-generate-tags-bin))
         (generate-tags-bin (expand-file-name generate-tags-bin)))
    (unless (file-directory-p project-dir)
      (error "Project git directory could not be found"))
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

;;;###autoload
(define-minor-mode rbtagger-mode
  "Tag management for Ruby files."
  :lighter " rbtagger"
  :keymap rbtagger-mode-map
  :group 'rbtagger)

(provide 'rbtagger)
;;; rbtagger.el ends here
