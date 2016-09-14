;;; init-speedbar --- Customizations for speedbar
;;; Commentary:
;;; Code:
(require 'sr-speedbar)

;; Configurations
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
(defun speedbar-expand-line-descendants (&optional arg)
  "Expand ARG." ())

;; Keys
(unless (fboundp 'my-speedbar-toggle)
  (defalias 'my-speedbar-toggle 'sr-speedbar-toggle))
(global-set-key (kbd "C-x p") 'my-speedbar-toggle)

(defun speedbar-toggle-or-edit-line ()
  "In speedbar, toggle line's expansion if line is directory, or edit it."
  (interactive)
  (let* ((file (speedbar-line-file)))
    (if (or (not file) (file-directory-p file))
        (speedbar-toggle-line-expansion)
      (speedbar-edit-line))))

(add-hook 'speedbar-mode-hook
          (lambda ()
            (local-set-key (kbd "<return>") 'speedbar-toggle-or-edit-line)))

(provide 'init-speedbar)
;;; init-speedbar ends here
