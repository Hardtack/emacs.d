(require-package 'projectile)
(require 'projectile)

(projectile-global-mode t)

;; Speedbar setup
(require-package 'projectile-speedbar)
(require-package 'sr-speedbar)
(require 'sr-speedbar)
(require 'projectile-speedbar)

;; Redef
(defun projectile-speedbar-toggle ()
  (interactive)
  (if (sr-speedbar-exist-p)
      (sr-speedbar-close)
    (progn
      (projectile-speedbar-open-current-buffer-in-tree)
      (sr-speedbar-select-window))))

(global-set-key (kbd "C-x p") 'projectile-speedbar-toggle)
(setq speedbar-frame-parameters
      '((minibuffer)
        (width . 20)
        (border-width . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (unsplittable . t)
        (left-fringe . 0)))
(setq sr-speedbar-max-width 20)
(setq sr-speedbar-width-console 20)
(setq sr-speedbar-auto-refresh nil)
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-use-images nil)
(setq speedbar-update-flag nil)
;; speed bar left side
(setq sr-speedbar-right-side nil)
;; Auto refresh
(sr-speedbar-refresh-turn-off)
;; Disable expand all (takes too long)
(defun speedbar-expand-line-descendants (&OPTIONAL ARG)
  "Expand &OPTIONAL ARG." ())

(provide 'init-project)
