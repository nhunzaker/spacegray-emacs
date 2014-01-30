;;; spacegray-theme.el --- Custom face theme for Emacs
;;; Commentary:
;;; Author: Nathan Hunzaker
;;; URL: none
;;; Version: 0.0.1

;;; Code:
(unless (>= 24 emacs-major-version)
  (error "Theme spacegray requires Emacs 24 or later"))

(deftheme spacegray "Spacegray color theme")

(setq underline-minimum-offset 4)

;; Highlighting
(custom-theme-set-faces
 'spacegray

 '(default ((t (:background "#2D2D2D" :foreground "#D4D5CE" ))))
 '(cursor  ((t (:background "#D4D5CE"))))
 '(region  ((t (:background "#4f5b66" :foreground unspecified))))
 '(match   ((t (:background "#4f5b66" :foreground unspecified))))
 '(isearch   ((t (:background "#FCD05E" :foreground "#2D2D2D"))))

 ;; Autocomplete mode
 ;;;;;;;;;;;;;;;;;;;;;
 '(ac-selection-face ((t (:background "#757268" :foreground unspecified))))
 '(ac-gtags-candidate-face ((t (:background "#4f5b66" :foreground unspecified))))
 '(ac-gtags-selection-face ((t (:background "#649CD8" :foreground unspecified))))

 '(info-index-match ((t (:background "#757268" :foreground unspecified))))
 '(popup-menu-selection-face ((t (:background "#757268" :foreground unspecified))))

 ;; Org Mode ;;
 ;;;;;;;;;;;;;;

 '(org-done ((t (:foreground "#9DD69B"))))
 '(org-todo ((t (:foreground "#CF99CF"))))
 '(org-checkbox ((t (:foreground "#FCD05E"))))
 '(org-document-title ((t (:foreground "#D4D5CE" :weight bold))))
 '(org-document-info ((t (:foreground "#757268"))))
 '(org-document-info-keyword ((t (:foreground "#757268" :weight bold))))
 '(org-clock-overlay ((t (:background "#757268" :foreground unspecified))))
 '(org-agenda-clocking ((t (:background "#757268" :foreground unspecified))))

 ;; The mode line is found at the bottom of all frames
 '(mode-line ((t (:background "#1E1E1E" :height 140 :foreground "#D4D5CE" :box (:line-width 10 :color "#1E1E1E" )))))
 '(mode-line-inactive ((t (:background "#262626" :height 140 :foreground "#757268" :box (:line-width 10 :color "#262626" ) ))))

 ;; The Border around the edge of the frame
 '(fringe ((t (:background unspecified))))
 '(vertical-border ((t (:background "#262626" :foreground "#262626" ))))

 ;; Interactively do things
 '(ido-first-match ((t (:foreground "#4f5b66"))))
 '(ido-subdir ((t (:foreground "#4f5b66"))))

 ;; When finding files, this highlights matching
 '(flx-highlight-face ((t (:foreground "#649CD8"))))

 ;; The Selected Line
 '(hl-line ((t :background "#2d2d2d" :underline unspecified)))

 ;; Errors
 '(font-lock-error-face ((t (:background "#F7756F" :foreground "#2D2D2D"))))
 '(font-lock-warning-face ((t (:background unspecified :foreground "#F7756F"))))
 '(flymake-errline  ((t (:background "#F7756F" :foreground "#2D2D2D"))))
 '(flymake-warnline ((t (:background "#F7756F" :foreground "#2D2D2D"))))

 ;; The minibuffer
 '(minibuffer-prompt ((t (:foreground "#fff" :weight bold))))

 ;; Not known so far for anything other than Lisp attributes and
 ;; "this" in javascript
 '(font-lock-builtin-face ((t (:foreground "#FCD05E" :weight bold))))

 ;; Comments
 '(font-lock-comment-face ((t (:slant italic :foreground "#797769"))))
 '(font-lock-doc-face ((t (:foreground "#797769"))))

 ;; Constants
 '(font-lock-constant-face ((t (:foreground "#F7756F"))))

 ;; Function names (not the keyword "function")
 '(font-lock-function-name-face ((t (:foreground "#649CD8" :bold t))))

 ;; Keywords
 '(font-lock-keyword-face ((t (:foreground "#CF99CF"))))

 ;; Strings
 '(font-lock-string-face ((t (:foreground "#9DD69B"))))

 `(font-lock-preprocessor-face ((t (:foreground "#797769" ))))

 ;; Ruby classes, etc...
 '(font-lock-type-face ((t (:foreground "#FCD05E"))))

 ;; Variables, and style selectors
 '(font-lock-variable-name-face ((t (:foreground "#c0c5ce" :weight bold))))

 ;; General UI
 '(link ((t (:underline t))))
 '(link-visited ((t (:underline t))))
 '(button ((t (:underline t))))

 '(header-line ((t (:background unspecified :foreground unspecified))))

 ;; Line Numbering
 '(linum ((t (:foreground "#444" :background nil :underline nil))))

 ;; JS2 MODE ;;
 ;;;;;;;;;;;;;

 '(js2-function-param ((t (:foreground "#c0c5ce"))))
 '(js2-external-variable ((t (:foreground "#FCD05E" :bold t ))))
 '(js2-jsdoc-tag ((t (:foreground "#F7756F" ))))
 '(js2-jsdoc-type ((t (:foreground "#FCD05E" ))))
 '(js2-jsdoc-value ((t (:foreground "#FCD05E" ))))
 '(js2-private-function-call ((t (:foreground "#FCD05E" ))))

 ;; WHITESPACE MODE ;;
 ;;;;;;;;;;;;;;;;;;;;

 `(whitespace-tab ((t (:background nil :foreground nil :underline (:color "#3a3a3a" :style wave)))))
 `(whitespace-indentation ((t (:background nil :foreground "#333" ))))
 `(trailing-whitespace ((t (:background "#F7756F" :foreground "#2b303b"))))
 `(whitespace-trailing ((t (:background "#F7756F" :foreground "#2b303b"))))
 `(whitespace-line ((t (:background nil :foreground "#d33682"))))

 ;; Flyspell
 '(flyspell-incorrect ((t (:background "#2D2D2D" :underline (:color "#F7756F" :style wave) :foreground unspecified))))
 '(flyspell-duplicate ((t (:foreground nil :underline (:color "#F7756F" :style wave)))))

 ;; Errors
 '(flycheck-error ((t (:background "#F7756F" :foreground "#2b303b" :bold t))))
 '(js2-error ((t (:background "#F7756F" :foreground "#2b303b" :bold t))))

 ;; YAML ;;
 ;;;;;;;;;
 `(yaml-tab-face ((t (:background "#F7756F" :foreground "#2b303b"))))

 ;; CSS Mode ;;
 ;;;;;;;;;;;;;
 `(css-selector ((t (:foreground "#F7756F" ))))
 `(css-property ((t (:foreground "#D4D5CE" ))))
 `(css-proprietary-property ((t (:foreground "#D4D5CE" :italic t))))
 `(scss-variable-face ((t (:foreground "#91264a" ))))

 ;; Markdown Mode ;;
 ;;;;;;;;;;;;;;;;;;

 `(markdown-header-face   ((t (:foreground "#DB613B"))))
 `(markdown-header-face-6 ((t (:foreground "#87243C"))))
 `(markdown-header-face-5 ((t (:foreground "#A12B39"))))
 `(markdown-header-face-4 ((t (:foreground "#BA3232"))))
 `(markdown-header-face-3 ((t (:foreground "#D44B39"))))
 `(markdown-header-face-2 ((t (:foreground "#E5653E"))))
 `(markdown-header-face-1 ((t (:foreground "#379A6A" :height 150))))

 `(markdown-bold-face  ((t (:foreground "#ba3232" :bold t))))

 ;; ERC Mode ;;
 ;;;;;;;;;;;;;

 `(erc-nick-msg-face ((t (:foreground "#998"))))
 `(erc-current-nick-face ((t (:foreground "#BA3232"))))
 `(erc-direct-msg-face ((t (:foreground "#555"))))
 `(erc-default-face ((t (:foreground "#D4D5CE"))))
 `(erc-command-indicator-face ((t (:foreground "#D44B39"))))
 `(erc-notice-face ((t (:foreground "#445"))))
 `(erc-button ((t (:foreground "#379A6A"))))
 `(erc-prompt-face ((t (:foreground "#444"))))

 `(show-paren-match ((t (:background unspecified :foreground "#FCD05E" :underline nil :weight bold))))

 ;; Web Mode ;;
 ;;;;;;;;;;;;;;
 `(web-mode-html-tag-face ((t (:foreground "#c0c5ce" ))))
 `(web-mode-html-attr-name-face ((t (:foreground "#FCD05E" ))))
 `(web-mode-current-element-highlight-face ((t (:foreground "#BA3232" ))))
 `(web-mode-block-face ((t (:foreground "#D27B4E" ))))
 `(web-mode-preprocessor-face ((t (:foreground "#797769" ))))
 `(web-mode-variable-name-face ((t (:foreground "#D27B4E" ))))
 `(web-mode-symbol-face ((t (:foreground "#FCD05E" ))))
 `(web-mode-builtin-face ((t (:foreground "#D27B4E" ))))
 `(web-mode-type-face ((t (:foreground "#FCD05E" ))))
 `(web-mode-folded-face ((t (:foreground unspecified :underline (:color "#D27B4E" :style wave)))))


 ;; Smart Parens ;;
;;;;;;;;;;;;;;;;;;
 '(sp-pair-overlay-face ((t (:background "#2D2D2D"))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'spacegray)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; spacegray-theme.el ends here
