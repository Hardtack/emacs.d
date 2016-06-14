(require-package 'diff-hl)
(require 'diff-hl)
(require 'diff-hl-flydiff)

(defun turn-on-diff-hl-flydiff-mode ()
  "Turn on diff-hl-flydiff-mode."
  (interactive)
  (diff-hl-mode t)
  (diff-hl-flydiff-mode t))
(add-hook 'prog-mode-hook 'turn-on-diff-hl-flydiff-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-flydiff-mode)


(provide 'init-vc)
