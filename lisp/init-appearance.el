;;; init-appearance --- Appearance setup
;;; Commentary:
;;; Code:

;; Display time to mode-line
(display-time-mode 1)

;; Hightlight current line
(global-hl-line-mode 1)

;; Bar-cursor
(setq-default cursor-type 'bar)

;; Zooming
(require 'zoom-frm)
(defun unzoom ()
  "Unzoom."
  (interactive)
  (zoom-in/out 0))
(global-set-key (kbd "C-M-=") 'zoom-in)
(global-set-key (kbd "C-M--") 'zoom-out)
(global-set-key (kbd "C-M-0") 'unzoom)

(provide 'init-appearance)
;;; init-appearance ends here
