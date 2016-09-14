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

(defcustom universal-font nil
  "Font for universal characters.")
(defcustom roman-font nil
  "Font for roman chracters.")
(defcustom korean-font nil
  "Font for korean characters.")
(defcustom universal-font-candidates '()
  "Font candidates for universal-characters")
(defcustom roman-font-candidates '()
  "Font candidates for roman chracters.")
(defcustom korean-font-candidates '()
  "Font candidates for korean characters.")

(defun geonu/font-exists-p (font)
  "Check the existence of FONT in this machine."
  (if window-system
      (-> (x-list-fonts font) not not)
    nil))

(defun geonu/select-first-existing-font (candidates)
  "Select first installed font from CANDIDATES."
  (-first #'geonu/font-exists-p candidates))

(defun geonu/update-fonts ()
  "Update fonts from customs"
  (interactive)
  (if universal-font
      (progn (set-face-attribute 'default nil :family universal-font)
             (set-fontset-font t 'hangul (font-spec :name universal-font)))
    (progn
      (when roman-font (set-face-attribute 'default nil :family roman-font))
      (when korean-font (set-fontset-font t 'hangul (font-spec :name korean-font))))))

(defun geonu/update-fonts-from-global-candidates ()
  "Update fonts from universal-font-candidates, roman-font-candidates, korean-font-candidates."
  (interactive)
  (setq universal-font (geonu/select-first-existing-font universal-font-candidates))
  (setq roman-font (geonu/select-first-existing-font roman-font-candidates))
  (setq korean-font (geonu/select-first-existing-font korean-font-candidates))
  (geonu/update-fonts))

(defun use-ugly-fixed-universal-font-off ()
  "Don't use ugly fixed-size universal font."
  (interactive)
  (setq universal-font-candidates '())
  (geonu/update-fonts-from-global-candidates))
(defun use-ugly-fixed-universal-font ()
  "Use ugly fixed-size universal font."
  (interactive)
  (when (not (geonu/select-first-existing-font ugly-universal-font-candidates))
    (error "No fixed-sized universal fonts available."))
  (setq universal-font-candidates ugly-universal-font-candidates)
  (geonu/update-fonts-from-global-candidates))
(setq ugly-universal-font-candidates
      '("D2Coding" "NanumGothicCoding"))
(setq universal-font-candidates
      '())
(setq roman-font-candidates
      '("Menlo" "Monaco" "Consolas"))
(setq korean-font-candidates
      '("D2Coding" "NanumGothicCoding" "Apple SD Gothic Neo"))

(geonu/update-fonts-from-global-candidates)
(when window-system (use-ugly-fixed-universal-font))

(provide 'init-fonts)
