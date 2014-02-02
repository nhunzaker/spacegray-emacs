;;; spacegray-theme.el --- Custom face theme for Emacs
;;; Commentary:
;;; Author: Nathan Hunzaker
;;; URL: https://github.com/nhunzaker/spacegray-emacs
;;; Version: 0.0.1

;;; Code:
(deftheme spacegray "Spacegray color theme")

(setq underline-minimum-offset 4)

(let ((base00 "#151515")
      (base01 "#202020") (base01-up "#232325") (base01-down "#171717")
      (base02 "#303030")
      (base03 "#505050") (base03-up "#504943")
      (base04 "#b0b0b0")
      (base05 "#d0d0d0")
      (base06 "#e0e0e0")
      (base07 "#f5f5f5")
      (base08 "#ac4142")
      (base09 "#d28445")
      (base10 "#f4bf75")
      (base11 "#90A959")
      (base12 "#75b5aa")
      (base13 "#6a9fb5")
      (base14 "#aa759f")
      (base15 "#8f5536"))

  (let ((background-color base01)
        (focus-color base03)
        (comment-color base03-up)
        (highlight-color base10)
        (text-color base05)
        (error-color base08)
        (warning-color base09))

    (custom-theme-set-faces
     'spacegray

     ;; Basics
     `(cursor ((t (:background ,text-color, :foreground ,background-color))))
     `(default ((t (:background ,background-color :foreground ,text-color ))))
     `(header-line ((t (:background unspecified :foreground unspecified))))
     `(isearch ((t (:background ,highlight-color :foreground ,background-color))))
     `(isearch-fail ((t (:background ,error-color :foreground ,background-color))))
     `(match ((t (:background ,focus-color :foreground unspecified))))
     `(menu ((t (:background ,text-color :foreground ,background-color))))
     `(region ((t (:background ,base02 :foreground unspecified))))
     `(warning ((t (:foreground ,warning-color))))
     `(linum ((t (:foreground ,focus-color :background nil :underline nil))))

     `(button ((t (:underline t))))
     `(link ((t (:underline t))))
     `(link-visited ((t (:underline t))))

     `(hl-line ((t :background unspecified)))
     `(highlight ((t (:background unspecified :foreground unspecified))))
     `(lazy-highlight ((t (:background ,base02 :foreground ,text-color))))

     `(minibuffer-prompt ((t (:foreground ,base07 :weight bold))))
     `(mode-line ((t (:background ,base01-down :height 140 :foreground ,base03 :box (:line-width 10 :color ,base01-down )))))
     `(mode-line-inactive ((t (:background ,base01-up :height 140 :foreground ,focus-color :box (:line-width 10 :color ,base01-up) ))))
     `(mode-line-highlight ((t (:foreground ,base05 :box nil))))

     ;; The Border around the edge of the frame
     `(fringe ((t (:background unspecified))))
     `(vertical-border ((t (:background ,base00 :foreground ,base01-up ))))

     `(font-lock-builtin-face ((t (:foreground ,base10 :weight bold))))
     `(font-lock-comment-face ((t (:slant italic :foreground ,comment-color))))
     `(font-lock-constant-face ((t (:foreground ,base12))))
     `(font-lock-doc-face ((t (:foreground ,comment-color))))
     `(font-lock-error-face ((t (:background ,error-color :foreground ,background-color :weight normal))))
     `(font-lock-function-name-face ((t (:foreground ,base07 :bold t))))
     `(font-lock-keyword-face ((t (:foreground ,base14))))
     `(font-lock-preprocessor-face ((t (:foreground ,focus-color ))))
     `(font-lock-string-face ((t (:foreground ,base11))))
     `(font-lock-type-face ((t (:foreground ,highlight-color))))
     `(font-lock-variable-name-face ((t (:foreground ,text-color :weight bold))))
     `(font-lock-warning-face ((t (:inherit 'warning))))

     ;; Autocomplete mode
     ;;;;;;;;;;;;;;;;;;;;;
     `(ac-selection-face ((t (:background ,base04 :foreground ,background-color))))
     `(ac-candidate-mouse-face ((t (:background ,base05 :foreground ,background-color))))
     `(ac-gtags-candidate-face ((t (:background ,focus-color :foreground ,background-color))))
     `(ac-gtags-selection-face ((t (:background ,base13 :foreground unspecified))))

     `(info-index-match ((t (:background ,base04 :foreground unspecified))))
     `(popup-menu-selection-face ((t (:background ,base04 :foreground ,base02))))

     ;; Org Mode ;;
     ;;;;;;;;;;;;;;
     `(org-date ((t (:foreground ,base11))))
     `(org-date-selected ((t (:foreground ,highlight-color))))
     `(org-footnote ((t (:foreground ,base12 :underline t))))
     `(org-formula ((t (:foreground ,base09))))
     `(org-headline-done ((t (:foreground ,highlight-color))))
     `(org-done ((t (:foreground ,base11))))
     `(org-todo ((t (:foreground ,base14))))
     `(org-checkbox ((t (:foreground ,highlight-color))))
     `(org-document-title ((t (:foreground ,text-color :weight bold))))
     `(org-document-info ((t (:foreground ,base04))))
     `(org-document-info-keyword ((t (:foreground ,base04 :weight bold))))
     `(org-clock-overlay ((t (:background ,base04 :foreground unspecified))))
     `(org-mode-line-clock ((t (:background ,base00 :foreground ,base02))))
     `(org-mode-line-clock-overrun ((t (:background ,error-color :foreground ,background-color))))
     `(org-agenda-clocking ((t (:background ,base04 :foreground unspecified))))

     ;; Interactively do things
     `(ido-first-match ((t (:foreground ,focus-color))))
     `(ido-subdir ((t (:foreground ,focus-color))))
     `(ido-indicator ((t (:background ,base08 :foreground ,highlight-color))))
     `(ido-only-match ((t (:foreground ,base11))))

     `(info-index-match ((t (:background ,text-color :foreground ,background-color))))
     `(info-menu-star ((t (:foreground ,base08))))

     ;; When finding files, this highlights matching
     `(flx-highlight-face ((t (:foreground ,base14))))

     ;; Errors
     `(flymake-errline  ((t (:inherit 'font-lock-error-face))))
     `(flymake-warnline ((t (:inherit font-lock-warning-face))))
     `(flycheck-error ((t (:underline (:color ,error-color :style wave) :bold t))))
     `(flycheck-fringe-error ((t (:foreground ,error-color :bold t))))
     `(flycheck-fringe-warning ((t (:foreground ,warning-color :bold t))))
     `(js2-error ((t (:inherit font-lock-error-face))))
     `(clojure-test-failure-face ((t :inherit `font-lock-error-face)))

     ;; helm
     `(helm-buffer-not-saved ((t (:foreground ,error-color))))
     `(helm-buffer-process ((t (:foreground ,base09))))
     `(helm-buffer-size ((t (:foreground ,base14))))
     `(helm-candidate-size ((t (:background ,highlight-color :foreground ,background-color))))
     `(helm-buffer-saved-out ((t (:background ,base00 :foreground ,base08))))
     `(helm-grep-file ((t (:foreground ,base14 :underline t))))
     `(helm-grep-finish ((t (:foreground ,base11))))
     `(helm-grep-lineno ((t (:foreground ,base09))))
     `(helm-grep-match ((t (:background ,base02 :foreground ,text-color))))
     `(helm-grep-running ((t (:foreground ,base08))))
     `(helm-moccur-buffer ((t (:foreground ,base13 :underline t))))
     `(helm-selection ((t (:background ,base11 :foreground ,background-color :underline t))))
     `(helm-selection-line ((t (:background ,base08 :foreground ,background-color :underline t))))
     `(helm-separator ((t (:foreground ,base08))))
     `(helm-source-header ((t (:background ,base13 :foreground ,background-color :weight bold))))
     `(helm-time-zone-current ((t (:foreground ,base11))))
     `(helm-time-zone-home ((t (:foreground ,base08))))
     `(helm-visible-mark ((t (:background ,base11 :foreground ,background-color))))


     ;; Rainbow Mode
     `(rainbow-delimiters-unmatched-face ((t (:foreground ,error-color))))
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,base04))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,base05))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,base06))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,base07))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,base11))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,highlight-color))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,base09))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,base08))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground ,base15))))

     ;; JS2 MODE ;;
     ;;;;;;;;;;;;;

     `(js2-function-param ((t (:foreground ,text-color))))
     `(js2-external-variable ((t (:foreground ,highlight-color :bold t ))))
     `(js2-jsdoc-tag ((t (:foreground ,base08 ))))
     `(js2-jsdoc-type ((t (:foreground ,highlight-color ))))
     `(js2-jsdoc-value ((t (:foreground ,highlight-color ))))
     `(js2-private-function-call ((t (:foreground ,highlight-color ))))
     `(js2-instance-member ((t (:foreground ,base14 ))))
     `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,base11 ))))
     `(js2-jsdoc-html-tag-name ((t (:foreground ,highlight-color ))))

     ;; WHITESPACE MODE ;;
 ;;;;;;;;;;;;;;;;;;;;

     `(whitespace-tab ((t (:background nil :foreground nil :underline (:color ,base02 :style wave)))))
     `(whitespace-indentation ((t (:background nil :foreground ,base02 ))))
     `(trailing-whitespace ((t (:background ,base08 :foreground ,base02))))
     `(whitespace-trailing ((t (:background ,base08 :foreground ,base02))))
     `(whitespace-line ((t (:background unspecified :foreground unspecified :underline (:color ,base09 :style wave)))))

     ;; Flyspell
     `(flyspell-incorrect ((t (:background ,background-color :underline (:color ,base09 :style wave) :foreground nil))))
     `(flyspell-duplicate ((t (:foreground nil :underline (:color ,base09 :style wave)))))

     ;; YAML ;;
 ;;;;;;;;;
     `(yaml-tab-face ((t (:background ,base08 :foreground ,base02))))

     ;; CSS Mode ;;
 ;;;;;;;;;;;;;
     `(css-selector ((t (:foreground ,base08 ))))
     `(css-property ((t (:foreground ,text-color ))))
     `(css-proprietary-property ((t (:foreground ,text-color :italic t))))
     `(scss-variable-face ((t (:foreground ,base14))))
     `(scss-keyword-face ((t (:foreground ,base14 ))))

     ;; Markdown Mode ;;
     ;;;;;;;;;;;;;;;;;;
     `(markdown-header-face   ((t (:foreground ,base08))))
     `(markdown-header-face-6 ((t (:foreground ,base08))))
     `(markdown-header-face-5 ((t (:foreground ,base09))))
     `(markdown-header-face-4 ((t (:foreground ,highlight-color))))
     `(markdown-header-face-3 ((t (:foreground ,base11))))
     `(markdown-header-face-2 ((t (:foreground ,base12))))
     `(markdown-header-face-1 ((t (:foreground ,base13))))
     `(markdown-bold-face  ((t (:foreground ,base08 :bold t))))
     `(markdown-blockquote-face  ((t (:foreground,comment-color :italic t))))

     ;; ERC Mode ;;
     ;;;;;;;;;;;;;
     `(bg:erc-color-face0 ((t :background ,base07 :foreground ,background-color)))
     `(bg:erc-color-face1 ((t :background ,base02 :foreground ,text-color)))
     `(bg:erc-color-face3 ((t :background ,base07 :foreground ,background-color)))
     `(bg:erc-color-face4 ((t :background ,base08 :foreground ,background-color)))
     `(bg:erc-color-face5 ((t :background ,base09 :foreground ,background-color)))
     `(bg:erc-color-face6 ((t :background ,base14 :foreground ,background-color)))
     `(bg:erc-color-face7 ((t :background ,base09 :foreground ,background-color)))
     `(bg:erc-color-face8 ((t :background ,highlight-color :foreground ,background-color)))
     `(bg:erc-color-face9 ((t :background ,base11 :foreground ,background-color)))
     `(bg:erc-color-face10 ((t :background ,base07 :foreground ,text-color)))
     `(bg:erc-color-face11 ((t :background ,base12 :foreground ,background-color)))
     `(bg:erc-color-face12 ((t :background ,base13 :foreground ,background-color)))
     `(bg:erc-color-face13 ((t :background ,base14 :foreground ,background-color)))
     `(bg:erc-color-face14 ((t :background ,focus-color :foreground ,text-color)))
     `(bg:erc-color-face15 ((t :background ,base07 :foreground ,background-color)))

     `(fg:erc-color-face0 ((t :foreground ,text-color)))
     `(fg:erc-color-face1 ((t :foreground ,base00)))
     `(fg:erc-color-face2 ((t :foreground ,base13)))
     `(fg:erc-color-face3 ((t :foreground ,base11)))
     `(fg:erc-color-face4 ((t :foreground ,base08)))
     `(fg:erc-color-face5 ((t :foreground ,base15)))
     `(fg:erc-color-face6 ((t :foreground ,base14)))
     `(fg:erc-color-face7 ((t :foreground ,base09)))
     `(fg:erc-color-face8 ((t :foreground ,highlight-color)))
     `(fg:erc-color-face9 ((t :foreground ,base11)))
     `(fg:erc-color-face10 ((t :foreground ,text-color)))
     `(fg:erc-color-face11 ((t :foreground ,base12)))
     `(fg:erc-color-face12 ((t :foreground ,base13)))
     `(fg:erc-color-face13 ((t :foreground ,base14)))
     `(fg:erc-color-face14 ((t :foreground ,text-color)))
     `(fg:erc-color-face15 ((t :foreground ,text-color)))

     `(erc-nick-msg-face ((t (:foreground ,base04))))
     `(erc-current-nick-face ((t (:foreground ,base08))))
     `(erc-direct-msg-face ((t (:foreground ,focus-color))))
     `(erc-default-face ((t (:foreground ,text-color))))
     `(erc-fool-face ((t (:foreground ,base13))))
     `(erc-command-indicator-face ((t (:foreground ,base09))))
     `(erc-notice-face ((t (:foreground ,highlight-color))))
     `(erc-button ((t (:foreground ,base12))))
     `(erc-prompt-face ((t (:foreground ,focus-color))))
     `(erc-dangerous-host-face ((t (:background ,error-color :foreground ,background-color :weight bold))))
     `(erc-error-face ((t (:foreground ,error-color :weight bold))))
     `(erc-inverse-face ((t (:background unspecified :foreground unspecified :weight bold))))
     `(erc-input-face ((t (:background unspecified :foreground ,base15 :weight bold))))
     `(erc-keyword-face ((t (:background unspecified :foreground ,base11 :weight bold))))
     `(erc-pal-face ((t (:background unspecified :foreground ,base14 :weight bold))))
     `(show-paren-match ((t (:background unspecified :foreground ,highlight-color :underline nil :weight bold))))

     ;; Web Mode ;;
 ;;;;;;;;;;;;;;
     `(web-mode-html-tag-face ((t (:foreground ,text-color ))))
     `(web-mode-html-attr-name-face ((t (:foreground ,highlight-color ))))
     `(web-mode-current-element-highlight-face ((t (:foreground ,base08 ))))
     `(web-mode-block-face ((t (:foreground ,base09 ))))
     `(web-mode-preprocessor-face ((t (:foreground ,focus-color ))))
     `(web-mode-variable-name-face ((t (:foreground ,base09 ))))
     `(web-mode-symbol-face ((t (:foreground ,highlight-color ))))
     `(web-mode-builtin-face ((t (:foreground ,base09 ))))
     `(web-mode-type-face ((t (:foreground ,highlight-color ))))
     `(web-mode-folded-face ((t (:foreground unspecified :underline (:color ,base09 :style wave)))))


     ;; Smart Parens ;;
     ;;;;;;;;;;;;;;;;;;
     `(sp-pair-overlay-face ((t (:background ,background-color))))

     ;; Magit
     `(magit-blame-header ((t (:background ,base01-down :foreground ,base03 :underline nil :box (:line-width 5 :color ,base01-down)))))
     `(magit-blame-sha1 ((t (:background ,base01-down :foreground ,base09 :underline nil))))
     `(magit-blame-culprit ((t (:background ,base01-down :foreground ,base04 :underline nil))))
     `(magit-blame-time ((t (:background ,base01-down :foreground ,base02 :underline nil))))
     `(magit-blame-subject ((t (:background ,base01-down :foreground ,comment-color :underline nil))))
     `(magit-log-message ((t (:foreground ,base03))))
     `(magit-item-highlight ((t (:background ,focus-color))))
     `(magit-diff-file-header ((t (:background unspecified))))

     ;; multi-mark mode
     `(mm/master-face ((t (:inherit 'lazy-highlight))))
     `(mm/mirror-face ((t (:background ,highlight-color :foreground ,background-color))))

     ;; Misc
     `(compilation-info ((t (:foreground ,base11))))
     `(compilation-mode-line-exit ((t (:foreground ,base11))))
     `(compilation-mode-line-fail ((t (:foreground ,error-color))))
     `(compilation-mode-line-run ((t (:foreground ,warning-color))))
     `(compilation-warning ((t (:foreground ,warning-color))))
     `(compilation-warning-face ((t (:foreground ,warning-color))))

     `(diary ((t (:foreground ,highlight-color))))

     `(dired-marked ((t (:foreground ,highlight-color))))

     `(ediff-current-diff-A ((t (:background ,base11 :foreground ,background-color))))
     `(ediff-current-diff-B ((t (:background ,base14 :foreground ,background-color))))
     `(ediff-current-diff-C ((t (:background ,base13 :foreground ,background-color))))
     `(ediff-current-diff-Ancestor ((t (:background ,highlight-color :foreground ,background-color))))
     `(ediff-even-diff-B ((t (:background ,text-color :foreground ,background-color))))
     `(ediff-even-diff-C ((t (:background ,text-color :foreground ,background-color))))
     `(ediff-fine-diff-A ((t (:background ,error-color :foreground ,background-color))))
     `(ediff-fine-diff-Ancestor ((t (:background ,highlight-color :foreground ,background-color))))
     `(ediff-fine-diff-B ((t (:background ,base11 :foreground ,background-color))))
     `(ediff-fine-diff-C ((t (:background ,base12 :foreground ,background-color))))
     `(ediff-odd-diff-Ancestor ((t (:background ,highlight-color :foreground ,background-color))))
     `(ediff-odd-diff-C ((t (:background ,text-color :foreground ,background-color))))

     `(escape-glyph ((t (:foreground ,base13))))
     `(ffap ((t (:background ,base11, :foreground ,background-color))))
     `(holiday ((t (:background ,base15, :foreground ,background-color))))
     )))

;;;###autoload
(when load-file-name
  (add-to-list `custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'spacegray)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; spacegray-theme.el ends here
