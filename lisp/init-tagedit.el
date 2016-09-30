;;; init-tagedit --- Customizations and utilities for tagedit
;;; Commentary:
;;; Provides customizations like my configuration specific key bindings for tagedit-mode
;;; Code:

(require 'tagedit)

(defun geonu/tagedit-add-paredit-like-keybindings ()
  "Add custom tagedit keys to tagedit-mode-map."
  (interactive)

  ;; paredit lookalikes
  (define-key tagedit-mode-map (kbd "C-)") 'tagedit-forward-slurp-tag)
  (define-key tagedit-mode-map (kbd "C-}") 'tagedit-forward-barf-tag)
  (define-key tagedit-mode-map (kbd "M-r") 'tagedit-raise-tag)
  (define-key tagedit-mode-map (kbd "M-s") 'tagedit-splice-tag)
  (define-key tagedit-mode-map (kbd "M-S") 'tagedit-split-tag)
  (define-key tagedit-mode-map (kbd "M-J") 'tagedit-join-tags)
  (define-key tagedit-mode-map (kbd "C-c C-<backspace>") 'te/kill-current-tag)
  (define-key tagedit-mode-map (kbd "C-%") 'te/goto-tag-match)
  (define-key tagedit-mode-map (kbd "C-^") 'te/goto-tag-begging)
  (define-key tagedit-mode-map (kbd "C-$") 'te/goto-tag-end))
(geonu/tagedit-add-paredit-like-keybindings)


(provide 'init-tagedit)
;;; init-tagedit ends here
