;;; Colourise CSS colour literals
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))


;;; SASS and SCSS
(setq-default scss-compile-at-save nil)



;;; Auto-complete CSS keywords
(after-load 'auto-complete
  (dolist (hook '(css-mode-hook sass-mode-hook scss-mode-hook))
    (add-hook hook 'ac-css-mode-setup)))


;;; Use eldoc for syntax hints
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)


(provide 'init-css)
