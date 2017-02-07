;;; init-html --- Cusomizations for HTML
;;; Commentary:
;;; Code:

(add-hook 'web-mode-hook (lambda () (tidy-build-menu web-mode-map)))

;; Enable tagedit
(after-load 'web-mode
  (require 'tagedit)
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))

;; Set default indentations to 2
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; ERB is configured in init-ruby.el

(provide 'init-html)
;;; init-html ends here
