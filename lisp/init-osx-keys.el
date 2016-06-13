(when *is-a-mac*

  (defun smart-beginning-of-line ()
    "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
    (interactive)
    (let ((oldpos (point)))
      (back-to-indentation)
      (and (= oldpos (point))
           (beginning-of-line))))

  ;; Full screen
  (defun toggle-fullscreen-osx ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
  (global-set-key (kbd "<s-return>") 'toggle-fullscreen-osx)
  ;; <C-s-268632070> means ^-⌘-f
  (global-set-key (kbd "<C-s-268632070>") 'toggle-fullscreen-osx)

  ;; Command-arrow
  (global-set-key [home] 'smart-beginning-of-line)
  (global-set-key [end] 'end-of-line)
  (global-set-key "\C-a" 'smart-beginning-of-line)

  ;; Close
  (global-set-key (kbd "s-w") 'kill-this-buffer)

  (setq default-input-method "MacOSX")

  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))

  ;; Undo-tree
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo)

  ;; Resizing
  (global-set-key (kbd "<s-up>") 'beginning-of-buffer)
  (global-set-key (kbd "<s-down>") 'end-of-buffer)
  (global-set-key (kbd "<s-left>") 'beginning-of-line)
  (global-set-key (kbd "<s-right>") 'end-of-line)
  (global-set-key (kbd "<M-s-up>") 'shrink-window)
  (global-set-key (kbd "<M-s-down>") 'enlarge-window)
  (global-set-key (kbd "<M-s-right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "<M-s-left>") 'shrink-window-horizontally)
  (after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )


(provide 'init-osx-keys)
