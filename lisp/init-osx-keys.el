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
  (global-set-key "\C-a" 'smart-beginning-of-line)

  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "<s-up>") 'beginning-of-buffer)
  (global-set-key (kbd "<s-down>") 'end-of-buffer)
  (global-set-key (kbd "<s-left>") 'smart-beginning-of-line)
  (global-set-key (kbd "<s-right>") 'end-of-line)
  (after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )


(provide 'init-osx-keys)
