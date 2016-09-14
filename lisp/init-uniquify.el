;;; init-uniquify --- Customization for uniquify
;;; Commentary:
;;; Code:

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'init-uniquify)
;;; init-uniquify ends here
