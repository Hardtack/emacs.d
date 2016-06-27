(require-package 'markdown-mode)

(require 'markdown-mode)
(require 'org)
(after-load 'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))

(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))

;; Table
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
