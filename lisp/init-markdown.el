;;; init-markdown --- Customization for markdown
;;; Commentary:
;;; Code:

(after-load 'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))


(after-load 'markdown-mode
  (require 'org)
  ;; I don't want these keys
  (let* ((map markdown-mode-map))
    ;; List editing
    (define-key map (kbd "M-<up>") nil)
    (define-key map (kbd "M-<down>") nil)
    (define-key map (kbd "M-<left>") nil)
    (define-key map (kbd "M-<right>") nil)
    (define-key map (kbd "M-<return>") nil)
    (define-key map (kbd "C-c C-j") nil)
    ;; Subtree editing
    (define-key map (kbd "M-S-<up>") nil)
    (define-key map (kbd "M-S-<down>") nil)
    (define-key map (kbd "M-S-<left>") nil)
    (define-key map (kbd "M-S-<right>") nil))

  ;; Set GFM as default
  (setq auto-mode-alist (cons '("\\.md$" . gfm-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.mdown$" . gfm-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.mdt$" . gfm-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.markdown$" . gfm-mode) auto-mode-alist))

  ;; Table
  (defun cleanup-org-tables ()
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-+-" nil t) (replace-match "-|-"))))

  (add-hook 'markdown-mode-hook 'orgtbl-mode)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))
  ;; Use multimarkdown if available
  (setq multimarkdown-path (executable-find "multimarkdown"))
  (if multimarkdown-path
      (setq markdown-command multimarkdown-path)
    (warn "multimarkdown not found, using default markdown instead.")))
(provide 'init-markdown)
;;; init-markdown ends here
