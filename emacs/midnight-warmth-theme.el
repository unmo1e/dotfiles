;; foreground : #ffbc90
;; background : #081426
;; color0 #050e1c
;; color1 #8e3744
;; color2 #5d9f3f
;; color3 #f17754
;; color4 #276cc7
;; color5 #b557f0
;; color6 #27c4c7
;; color7 #f8eee8
;; color8 #0b111c
;; color9 #ca4754
;; color10 #829f75
;; color11 #da6d4f
;; color12 #4b87e8
;; color13 #c175f0
;; color14 #61c5c7
;; color15 #f8eee7

(deftheme midnight-warmth
  "midnight warmth theme - for comfortable editing in dark at midnight")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'midnight-warmth
   `(default ((,class (:background "#081426" :foreground "#ffbc90"))))
   `(cursor ((,class (:background "#ffbc90" :foreground "#ffbc90"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#081426"))))
   `(highlight ((,class (:background "#081426" :underline t))))
   ;; background during selection, this tends to be weird so feel free to experiment
   `(region ((t (:background "#ffbc90" :foreground "#081426"))))
   `(secondary-selection ((,class (:background "#081426"))))
   `(isearch ((,class (:background "#081426" :foreground "#276cc7" :weight bold))))
   `(isearch-fail ((t (:foreground "#ffbc90" :background "#8e3744"))))
   `(lazy-highlight ((,class (:background "#081426" :foreground "#f17754" :weight bold))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#050e1c" :foreground "#f8eee8"))))
   `(mode-line-inactive ((,class (:background "#0b111c" :foreground "#f8eee7"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#27c4c7"))))
   `(escape-glyph ((,class (:foreground "#f17754" :weight bold))))
   `(homoglyph ((,class (:foreground "#5d9f3f" :weight bold))))
   ;; Font lock faces, this is where all source file highlighing is done
   `(font-lock-builtin-face ((,class (:foreground "#ffbc90" :weight bold))))
   `(font-lock-comment-face ((,class (:foreground "#5d9f3f"))))
   `(font-lock-constant-face ((,class (:foreground "#5d9f3f"))))
   `(font-lock-function-name-face ((,class (:foreground "#27c4c7"))))
   `(font-lock-keyword-face ((,class (:foreground "#ca4754" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#5d9f3f"))))
   `(font-lock-type-face ((,class (:foreground "#da6d4f" :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground "#f8eee8"))))
   `(font-lock-warning-face ((,class (:foreground "#8e3744" :weight bold))))
   ;; Button and link faces
   `(link ((,class (:foreground "#276cc7" :underline t))))
   `(link-visited ((,class (:foreground "#4b87e8" :underline t))))
   `(button ((,class (:background "#0b111c" :foreground "#f8eee7"))))
   `(header-line ((,class (:background "#081426" :foreground "#da6d4f"))))

   ;; set ansi-colors
   `(ansi-color-black ((t (:foreground "#050e1c"))))
   `(ansi-color-white ((t (:foreground "#f8eee8"))))
   `(ansi-color-red ((t (:foreground "#8e3744"))))
   `(ansi-color-yellow ((t (:foreground "#f17754"))))
   `(ansi-color-green ((t (:foreground "#5d9f3f"))))
   `(ansi-color-cyan ((t (:foreground "#27c4c7"))))
   `(ansi-color-blue ((t (:foreground "#276cc7"))))
   `(ansi-color-magenta ((t (:foreground "#b557f0"))))
   `(ansi-color-bright-black ((t (:foreground "#0b111c"))))
   `(ansi-color-bright-white ((t (:foreground "#f8eee7"))))
   `(ansi-color-bright-red ((t (:foreground "#ca4754"))))
   `(ansi-color-bright-yellow ((t (:foreground "#da6d4f"))))
   `(ansi-color-bright-green ((t (:foreground "#829f75"))))
   `(ansi-color-bright-cyan ((t (:foreground "#61c5c7"))))
   `(ansi-color-bright-blue ((t (:foreground "#4b87e8" ))))
   `(ansi-color-bright-magenta ((t (:foreground "#c175f0"))))
   `(message-separator ((,class (:foreground "#f8eee8" :weight bold))))))

(provide-theme 'midnight-warmth)
