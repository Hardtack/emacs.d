;;; init-clojure-cider --- Customizations for cider
;;; Commentary:
;;; Code:
(require 'init-clojure)

(setq nrepl-popup-stacktraces nil)

(after-load 'cider
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (after-load 'auto-complete
    (add-to-list 'ac-modes 'cider-repl-mode))

  (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc)

  ;; nrepl isn't based on comint
  (add-hook 'cider-repl-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))

(after-load 'clojure-mode
  (after-load 'flycheck
    (flycheck-clojure-setup)))


(provide 'init-clojure-cider)
;;; init-clojure-cider ends here
