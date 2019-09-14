(add-to-list 'load-path default-directory)

(require 'rbtagger)
(require 'checkdoc)

(with-current-buffer (find-file "rbtagger.el")
  (checkdoc-current-buffer t)
  (with-current-buffer "*Style Warnings*"
    (when (re-search-forward "rbtagger.el:[0-9]+" nil t)
      (let ((contents (substring-no-properties (buffer-string))))
        (message (replace-regexp-in-string "\n\n" "" contents))
        (error (kill-emacs 1))))))
