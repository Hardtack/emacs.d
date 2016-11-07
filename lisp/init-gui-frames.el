;;; init-gui-frames --- Settings for GUI frames
;;; Commentary:
;;; Provides settings for GUI frames
;;; Code:

;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun sanityinc/maybe-suspend-frame ()
  "Suspend frame unless in macOS."
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key [remap suspend-frame] 'sanityinc/maybe-suspend-frame)

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)


;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))

(provide 'init-gui-frames)
;;; init-gui-frames ends here
