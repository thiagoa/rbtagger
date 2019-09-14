;;; test-dependencies-helper.el  --- TODO  -*- lexical-binding: t; -*-

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

(require 'package)
(require 'cl-seq)

(defun install-test-deps-p (deps)
  "Determine if test DEPS should be installed.
WARNING: Incurs side-effects."
  (package-initialize)
  (dolist (dep deps) (require dep nil 'noerror))
  (cl-reduce (lambda (acc dep)
               (or (not (featurep dep)) acc))
             deps
             :initial-value nil))

(defun install-test-deps (deps)
  "Install test DEPS.
WARNING: Incurs side-effects."
  (when (install-test-deps-p deps)
    (let ((melpa '("melpa" . "http://stable.melpa.org/packages/")))
      (if (boundp 'package-archives)
          (add-to-list 'package-archives melpa)
        (setq package-archives (list melpa))))
    (package-refresh-contents)
    (dolist (dep deps)
      (unless (featurep dep) (package-install dep))
      (require dep))))

;;; test-deps-helper.el ends here
