;;; init-html --- Cusomizations for HTML
;;; Commentary:
;;; Code:

(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

(after-load 'sgml-mode
  (require 'tagedit)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

;; Note: ERB is configured in init-ruby-mode

(provide 'init-html)
;;; init-html ends here
