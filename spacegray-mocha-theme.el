;;; spacegray-mocha-theme.el --- Custom face theme for Emacs
;;; Commentary:
;;; Author: Nathan Hunzaker
;;; URL: https://github.com/nhunzaker/spacegray-emacs
;;; Version: 0.0.1

;;; Code:
(deftheme spacegray-mocha "Spacegray Mocha color theme")

(setq underline-minimum-offset 4)

(let ((base00 "#3B3228")
      (base01 "#534636")
      (base02 "#645240")
      (base03 "#7E705A")
      (base04 "#B8AFAD")
      (base05 "#D0C8C6")
      (base06 "#E9E1DD")
      (base07 "#F5EEEB")
      (base08 "#CB6077")
      (base09 "#D28B71")
      (base10 "#F4BC87")
      (base11 "#BEB55B")
      (base12 "#7BBDA4")
      (base13 "#8AB3B5")
      (base14 "#A89BB9")
      (base15 "#BB9584"))

  (let ((background-color base00)
        (focus-color base03)
        (comment-color base03)
        (highlight-color base10)
        (text-color base07)
        (error-color base08)
        (warning-color base09))

    (custom-theme-set-faces
     'spacegray-mocha

     `(default ((t (:background ,background-color :foreground ,text-color ))))
     `(cursor  ((t (:background ,text-color, :foreground ,background-color))))
     `(region  ((t (:background ,background-color :foreground unspecified))))
     `(match   ((t (:background ,focus-color :foreground unspecified))))
     `(highlight ((t (:background ,highlight-color :foreground ,background-color))))
     `(isearch ((t (:background ,highlight-color :foreground ,background-color))))
     `(isearch-fail ((t (:background ,error-color :foreground ,background-color))))
     `(menu ((t (:background ,text-color :foreground ,background-color))))
     `(warning ((t (:foreground ,warning-color))))

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

     ;; The mode line is found at the bottom of all frames
     `(mode-line ((t (:background ,base00 :height 140 :foreground ,focus-color :box (:line-width 10 :color ,base00 )))))
     `(mode-line-inactive ((t (:background ,background-color :height 140 :foreground ,focus-color :box (:line-width 10 :color ,background-color) ))))
     `(mode-line-highlight ((t (:foreground ,base05))))

     ;; The Border around the edge of the frame
     `(fringe ((t (:background unspecified))))
     `(vertical-border ((t (:background ,base00 :foreground ,base02 ))))

     ;; Interactively do things
     `(ido-first-match ((t (:foreground ,focus-color))))
     `(ido-subdir ((t (:foreground ,focus-color))))
     `(ido-indicator ((t (:background ,base08 :foreground ,highlight-color))))
     `(ido-only-match ((t (:foreground ,base11))))

     `(info-index-match ((t (:background ,text-color :foreground ,background-color))))
     `(info-menu-star ((t (:foreground ,base08))))

     ;; When finding files, this highlights matching
     `(flx-highlight-face ((t (:foreground ,base14))))

     ;; The Selected Line (twice to undo any strangeness)
     `(hl-line ((t :background ,base00 :underline unspecified)))
     `(hl-line ((t :background unspecified :underline unspecified)))

     ;; Errors
     `(font-lock-error-face ((t (:background ,error-color :foreground ,background-color))))
     `(font-lock-warning-face ((t (:background unspecified :foreground ,error-color))))
     `(flymake-errline  ((t (:background ,error-color :foreground ,background-color))))
     `(flymake-warnline ((t (:background ,error-color :foreground ,background-color))))
     `(flycheck-error ((t (:background ,error-color :foreground ,base02 :bold t))))
     `(flycheck-fringe-error ((t (:foreground ,error-color :bold t))))
     `(flycheck-fringe-warning ((t (:foreground ,warning-color :bold t))))
     `(js2-error ((t (:background ,error-color :foreground ,base02 :bold t))))

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

     ;; The minibuffer
     `(minibuffer-prompt ((t (:foreground ,base07 :weight bold))))

     ;; Not known so far for anything other than Lisp attributes and
     ;; "this" in javascript
     `(font-lock-builtin-face ((t (:foreground ,base11 :weight bold))))

     ;; Comments
     `(font-lock-comment-face ((t (:slant italic :foreground ,comment-color))))
     `(font-lock-doc-face ((t (:foreground ,comment-color))))

     ;; Constants
     `(font-lock-constant-face ((t (:foreground ,base12))))

     ;; Function names (not the keyword "function")
     `(font-lock-function-name-face ((t (:foreground ,base12 :bold t))))

     ;; Keywords
     `(font-lock-keyword-face ((t (:foreground ,base14))))

     ;; Strings
     `(font-lock-string-face ((t (:foreground ,base11))))

     `(font-lock-preprocessor-face ((t (:foreground ,focus-color ))))

     ;; Ruby classes, etc...
     `(font-lock-type-face ((t (:foreground ,highlight-color))))

     ;; Variables, and style selectors
     `(font-lock-variable-name-face ((t (:foreground ,text-color :weight bold))))

     ;; General UI
     `(link ((t (:underline t))))
     `(link-visited ((t (:underline t))))
     `(button ((t (:underline t))))

     `(header-line ((t (:background unspecified :foreground unspecified))))

     ;; Line Numbering
     `(linum ((t (:foreground ,focus-color :background nil :underline nil))))

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

     `(whitespace-tab ((t (:background nil :foreground nil :underline (:color ,base01 :style wave)))))
     `(whitespace-indentation ((t (:background nil :foreground ,base02 ))))
     `(trailing-whitespace ((t (:background ,base08 :foreground ,base02))))
     `(whitespace-trailing ((t (:background ,base08 :foreground ,base02))))
     `(whitespace-line ((t (:background unspecified :foreground unspecified :underline (:color ,base09 :style wave)))))

     ;; Flyspell
     `(flyspell-incorrect ((t (:background ,background-color :underline (:color ,base09 :style wave) :foreground unspecified))))
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

     ;; multi-mark mode
     `(mm/master-face ((t (:background ,base02, :foreground ,base07 :weight bold))))
     `(mm/mirror-face ((t (:background ,highlight-color, :foreground ,background-color))))


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

     `(lazy-highlight ((t (:background ,base13 :foreground ,background-color))))
     )))

;;;###autoload
(when load-file-name
  (add-to-list `custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'spacegray-mocha)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; spacegray-mocha-theme.el ends here
