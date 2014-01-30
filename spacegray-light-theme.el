;;; spacegray-light.el --- Custom face theme for Emacs
;;; Commentary:
;;; Author: Nathan Hunzaker
;;; URL: none
;;; Version: 0.0.1

(unless (>= 24 emacs-major-version)
  (error "spacegray-theme requires Emacs 24 or later."))

(deftheme spacegray-light "Spacegray color theme")

(setq underline-minimum-offset 4)

;; Highlighting
(custom-theme-set-faces
 'spacegray

 '(default ((t (:background "#EFF1F5" :foreground "#1D3046"))))
 '(cursor  ((t (:background "#BFC5CE"))))
 '(region  ((t (:background "#BFC5CE" :foreground unspecified))))
 '(match   ((t (:background "#D88E5C" :foreground unspecified))))
 '(isearch   ((t (:background "#A4B867" :foreground unspecified))))
 '(comint-highlight-prompt   ((t (:background "#A4B867" :foreground unspecified))))

 '(ac-selection-face ((t (:background "#757268" :foreground unspecified))))
 '(ac-completion-face ((t (:background "#C8564A" :foreground unspecified))))

 '(info-index-match ((t (:background "#757268" :foreground unspecified))))
 '(popup-menu-selection-face ((t (:background "#757268" :foreground unspecified))))
 '(org-clock-overlay ((t (:background "#757268" :foreground unspecified))))
 '(org-agenda-clocking ((t (:background "#757268" :foreground unspecified))))

 ;; The mode line is found at the bottom of all frames
 '(mode-line ((t (:background "#BFC5CE" :height 140 :foreground "#1D3046" :box (:line-width 10 :color "#BFC5CE" )))))
 '(mode-line-inactive ((t (:background "#E6E9F0" :height 140 :foreground "#8C919C" :box (:line-width 10 :color "#E6E9F0" ) ))))

 ;; The Border around the edge of the frame
 '(fringe ((t (:background unspecified))))
 '(vertical-border ((t (:background "#BFC5CE" :foreground "#BFC5CE" ))))

 ;; Interactively do things
 '(ido-first-match ((t (:foreground "#4f5b66"))))
 '(ido-subdir ((t (:foreground "#4f5b66"))))

 ;; When finding files, this highlights matching
 '(flx-highlight-face ((t (:foreground "#C8564A"))))

 ;; The Selected Line
 '(hl-line ((t :background transparent :underline unspecified)))

 ;; Errors
 '(font-lock-error-face ((t (:background "#C8564A" :foreground "#EFF1F5"))))
 '(font-lock-warning-face ((t (:background unspecified :foreground "#C8564A"))))
 '(flymake-errline  ((t (:background "#C8564A" :foreground "#EFF1F5"))))
 '(flymake-warnline ((t (:background "#C8564A" :foreground "#EFF1F5"))))

 ;; The minibuffer
 '(minibuffer-prompt ((t (:foreground "#4f5b66" :weight bold))))

 ;; Not known so far for anything other than Lisp attributes and
 ;; "this" in javascript
 '(font-lock-builtin-face ((t (:foreground "#D88E5C" :weight bold))))

 ;; Comments
 '(font-lock-comment-face ((t (:slant italic :foreground "#9EA2B6"))))
 '(font-lock-doc-face ((t (:foreground "#9EA2B6"))))

 ;; Constants
 '(font-lock-constant-face ((t (:foreground "#C8564A"))))

 ;; Function names (not the keyword "function")
 '(font-lock-function-name-face ((t (:foreground "#86A3BA" :bold t))))

 ;; Keywords
 '(font-lock-keyword-face ((t (:foreground "#B382A8"))))

 ;; Strings
 '(font-lock-string-face ((t (:foreground "#A4B867"))))

 ;; Ruby classes, etc...
 '(font-lock-type-face ((t (:foreground "#D88E5C"))))

 ;; Variables, and style selectors
 '(font-lock-variable-name-face ((t (:foreground "#444" :weight normal))))
 `(web-mode-variable-name-face ((t (:foreground "#A36766" ))))

 '(link ((t (:underline t))))
 '(link-visited ((t (:underline t))))
 '(button ((t (:underline t))))

 '(header-line ((t (:background unspecified :foreground unspecified))))

 ;; Line Numbering
 '(linum ((t (:foreground "#444" :background nil :underline nil))))

 ;; JS2 MODE ;;
 ;;;;;;;;;;;;;

 '(js2-function-param ((t (:foreground "#c0c5ce"))))
 '(js2-external-variable ((t (:foreground "#D88E5C" :weight bold ))))
 '(js2-jsdoc-tag ((t (:foreground "#C8564A" :weight bold))))
 '(js2-jsdoc-type ((t (:foreground "#D88E5C" ))))
 '(js2-jsdoc-value ((t (:foreground "#2b303b" ))))
 '(js2-private-function-call ((t (:foreground "#D88E5C" ))))

 ;; WHITESPACE MODE ;;
 ;;;;;;;;;;;;;;;;;;;;

 `(whitespace-tab ((t (:background nil :foreground nil :underline (:color "#c0c5ce" :style wave)))))
 `(whitespace-indentation ((t (:background nil :foreground "#333" ))))
 `(trailing-whitespace ((t (:background "#C8564A" :foreground "#2b303b"))))
 `(whitespace-trailing ((t (:background "#C8564A" :foreground "#2b303b"))))
 `(whitespace-line ((t (:background nil :foreground "#B382A8"))))

 ;; Flyspell
 '(flyspell-incorrect ((t (:background unspecified :underline "#C8564A" :foreground unspecified))))
 '(flyspell-duplicate ((t (:foreground nil :underline "#e33"))))

 ;; Errors
 '(flycheck-error ((t (:background "#C8564A" :foreground "#2b303b" :bold t))))
 '(js2-error ((t (:background "#C8564A" :foreground "#2b303b" :bold t))))

 ;; YAML ;;
 ;;;;;;;;;
 `(yaml-tab-face ((t (:background "#C8564A" :foreground "#2b303b"))))

 ;; CSS Mode ;;
 ;;;;;;;;;;;;;
 `(css-selector ((t (:foreground "#C8564A" ))))
 `(css-property ((t (:foreground "#1D3046" ))))
 `(css-proprietary-property ((t (:foreground "#1D3046" :italic t))))
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
 `(erc-default-face ((t (:foreground "#1D3046"))))
 `(erc-command-indicator-face ((t (:foreground "#D44B39"))))
 `(erc-notice-face ((t (:foreground "#445"))))
 `(erc-button ((t (:foreground "#A4B867"))))
 `(erc-prompt-face ((t (:foreground "#444"))))

 `(show-paren-match ((t (:background unspecified :foreground unspecified :underline t))))

 ;; Web Mode ;;
 ;;;;;;;;;;;;;;
 `(web-mode-html-tag-face ((t (:foreground "#86A3BA" ))))
 `(web-mode-html-attr-name-face ((t (:foreground "#1D3046" ))))
 `(web-mode-current-element-highlight-face ((t (:foreground "#BA3232" ))))
 `(web-mode-block-face ((t (:foreground "#D27B4E" ))))

 `(font-lock-preprocessor-face ((t (:foreground "#9EA2B6" ))))
 `(web-mode-preprocessor-face ((t (:foreground "#9EA2B6" ))))

  `(web-mode-symbol-face ((t (:foreground "#A4B867" ))))
 `(web-mode-builtin-face ((t (:foreground "#A36766" ))))
 `(web-mode-type-face ((t (:foreground "#D88E5C" ))))
 `(web-mode-folded-face ((t (:foreground unspecified :underline (:color "#D27B4E" :style wave)))))

 ;; Smart Parens ;;
;;;;;;;;;;;;;;;;;;
 '(sp-pair-overlay-face ((t (:background unspecified))))

 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'spacegray-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; spacegray-theme.el ends here
