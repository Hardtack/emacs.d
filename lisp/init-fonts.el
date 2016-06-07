;;; Character sets
(require-package 'dash)
(require 'dash)

(defcustom sanityinc/force-default-font-for-symbols nil
  "When non-nil, force Emacs to use your default font for symbols."
  :type 'boolean)

(defun sanityinc/maybe-use-default-font-for-symbols ()
  "Force Emacs to render symbols using the default font, if so configured."
  (when sanityinc/force-default-font-for-symbols
    (set-fontset-font "fontset-default" 'symbol (face-attribute 'default :family))))

(add-hook 'after-init-hook 'sanityinc/maybe-use-default-font-for-symbols)


;;; Changing font sizes

(require-package 'default-text-scale)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)
(set-face-attribute 'default nil :height 140)


(defun sanityinc/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'sanityinc/maybe-adjust-visual-fill-column)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Menlo")
  (setq korean-font-candidates '("D2Coding" "NanumGothicCoding" "Apple SD Gothic Neo"))
  (let* ((korean-font (-first (lambda (font) (x-list-fonts font)) korean-font-candidates)))
    (when korean-font
      (set-fontset-font t 'hangul (font-spec :name korean-font)))))

(provide 'init-fonts)
