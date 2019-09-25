(if noninteractive
    ;; Workaround extracted from enh-ruby-mode package:
    ;;
    ;; https://github.com/zenspider/enhanced-ruby-mode/blob/f334c42986e93c60fba144d732becfcbdb13bb7d/test/enh-ruby-mode-test.el#L13-L20
    ;;
    ;; In batch mode, face-attribute returns 'unspecified,
    ;; and it causes wrong-number-of-arguments errors.
    ;; This is a workaround for it.
    (defun erm-darken-color (name)
      (let ((attr (face-attribute name :foreground)))
        (unless (equal attr 'unspecified)
          (color-darken-name attr 20)
          "#000000"))))
