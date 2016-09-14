;;; init-flycheck --- Customizations for flycheck
;;; Commentary:
;;; Code:
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled new-line)
      flycheck-idle-change-delay 0.5)

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
(setq flycheck-display-errors-delay 0.5)


(provide 'init-flycheck)
;;; init-flycheck ends here
