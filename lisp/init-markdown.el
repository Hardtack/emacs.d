(require-package 'markdown-mode)

(require 'markdown-mode)
(require 'org)
(after-load 'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))

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
  (warn "multimarkdown not found, using default markdown instead."))

(provide 'init-markdown)
