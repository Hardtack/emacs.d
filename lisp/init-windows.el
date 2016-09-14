;;; init-windows --- Customizations for window system
;;; Commentary:
;;; Code:
;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)


;; Make "C-x o" prompt for a target window when there are more than 2
(require 'switch-window)
(setq-default switch-window-shortcut-style 'quail)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)


(defun toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (if was-dedicated
        (message "Window no longer dedicated to %s" (buffer-name))
      (message "Window dedicated to %s" (buffer-name)))))

(global-set-key (kbd "C-c <down>") 'toggle-current-window-dedication)


(provide 'init-windows)
;;; init-windows ends here
