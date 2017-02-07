;;; init-git --- Git configurations
;;; Commentary:
;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(setq-default
 magit-processs-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (add-hook 'magit-popup-mode-hook 'sanityinc/no-trailing-whitespace)
  ;; git-flow support
  (require 'magit-gitflow)
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))


(provide 'init-git)
;;; init-git ends here
