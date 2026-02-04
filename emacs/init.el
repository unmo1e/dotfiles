;; -*- lexical-binding: t -*-

;; stop garbage collection at startup
(setq gc-cons-threshold most-positive-fixnum)

;; Set default encoding for files
;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
;; do this especially on Windows, else python output problem
(set-terminal-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; use space to format regions instead of tabs
(setq-default indent-tabs-mode nil)
;; accept 'y' or 'n' instead of yes/no
(setq use-short-answers t)
;; set newline when moving cursor down
;; (setq next-line-add-newlines t)

;; don't blink cursor
(blink-cursor-mode 0)

;; Set line number to relative globally
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
;; Use Shitft+ArrowKeys to switch windows.
(windmove-default-keybindings)

;; Set the auto generated code to another file
(setq custom-file (concat user-emacs-directory "autogen-custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(when (eq nil (file-directory-p "~/.emacs.d/emacs-backup"))
  (make-directory "~/.emacs.d/emacs-backup"))

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/emacs-backup"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; for (display-fill-column-indicator-mode)
(setq-default display-fill-column-indicator-column 79)

;; Fold long lines
(global-visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Package Management  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)

;; Adding Melpa
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Adding nongnu elpa
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add all packages to install in this list
(setq package-selected-packages
      '(ivy counsel swiper magit company god-mode avy rainbow-delimiters
            feebleline org-download multiple-cursors yasnippet ivy-posframe nerd-icons-dired))
(package-install-selected-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Visual Changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setting font
;; this line is for only changing fontsize
;; (set-face-attribute 'default nil :font "Monospace" :height 160)

(set-frame-font "Rec Mono Casual-14" nil t)
;; Add fallback font
;; (set-fontset-font "fontset-default" 'unicode "Monofur Nerd Font" nil 'prepend)
(set-fontset-font "fontset-default" 'unicode "Symbols Nerd Font Mono" nil 'prepend)
;; give spaces between lines
(setq line-spacing 0.05)

(load-theme 'midnight-warmth 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tab Bar Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the format for our tab-bar
;; we use <menu-bar>, <tabs> and <separator>
;; the last line is for having stuff like display-time and org-time
;; on tab-bar instead of mode-line 
(setq tab-bar-format
      '(tab-bar-format-menu-bar
        tab-bar-format-tabs tab-bar-separator
        tab-bar-format-align-right tab-bar-format-global))

;; hide bar if only single tab
;; (setq tab-bar-show 1)
;; don't show close button
(setq tab-bar-close-button-show nil)
;; new tabs will open dired
(setq tab-bar-new-tab-choice "~/")

;; show tab number
;; (setq tab-bar-tab-hints t)

;; modify how tab-bar looks
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (set-face-attribute 'tab-bar nil
                                  :inherit 'default
                                  :foreground "#ffffff"
                                  :background (face-foreground 'ansi-color-black))
              (set-face-attribute 'tab-bar-tab nil
                                  :inherit 'mode-line-active)
              (set-face-attribute 'tab-bar-tab-inactive nil
                                  :inherit 'mode-line
                                  :background (face-foreground 'ansi-color-black))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Compilation Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add colors in compilation mode
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Feebleline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(feebleline-mode)
(setq feebleline-msg-functions
      '((feebleline-line-number         :post "" :fmt "%5s")
	(feebleline-column-number       :pre ":" :fmt "%-2s")
	(feebleline-file-directory      :face feebleline-dir-face :pre "|  " :post "")
	(feebleline-file-or-buffer-name :face org-link :post "")
	;; name of the major mode
	((lambda () (symbol-name major-mode)) :face feebleline-git-face :pre " |  ")
	(feebleline-file-modified-star  :face dired-warning :pre "  " :post "")
        ;; last entry not visible (bug?) so add empty string
        ((lambda () " ​ ") :face feebleline-dir-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Small Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-key-mode)
(global-set-key (kbd "<escape>") #'god-local-mode)
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
;; (mini-modeline-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ivy, Counsel and Swiper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings for ivy-counsel
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-use-selectable-prompt t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'avy)
(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g M-c") 'avy-goto-char-timer)
(global-set-key (kbd "M-g l") 'avy-goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple-cursors.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)
(define-prefix-command 'unmole-custom-prefix)
(global-set-key (kbd "C-z") 'unmole-custom-prefix)

(when (file-exists-p "~/.emacs.d/.mc-lists.el")
  (load "~/.emacs.d/.mc-lists.el"))

(defun unmole-mc-mark-next-like-this ()
  (interactive)
  (mc/mark-next-like-this 1)
  (mc/cycle-forward))
(add-to-list 'mc/cmds-to-run-once 'unmole-mc-mark-next-like-this)

(defun unmole-mc-skip-to-next-like-this ()
  (interactive)
  (mc/cycle-backward)
  (mc/skip-to-next-like-this)
  (mc/cycle-forward))
(add-to-list 'mc/cmds-to-run-once 'unmole-mc-skip-to-next-like-this)

(global-set-key (kbd "C->") 'unmole-mc-mark-next-like-this)
(global-set-key (kbd "C-M->") 'unmole-mc-skip-to-next-like-this)
(global-set-key (kbd "C-z a") 'mc/mark-all-dwim)
(global-set-key (kbd "C-z l") 'mc/edit-lines)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rainbow Delimiters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(eval-after-load 'rainbow-delimiters
  '(progn
     (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground (face-foreground 'ansi-color-bright-red))
     (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground (face-foreground 'ansi-color-bright-yellow))
     (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground (face-foreground 'ansi-color-bright-green))
     (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground (face-foreground 'ansi-color-bright-cyan))
     (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground (face-foreground 'ansi-color-bright-blue))
     (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground (face-foreground 'ansi-color-bright-magenta))
     (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground (face-foreground 'ansi-color-bright-green))
     (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground (face-foreground 'ansi-color-bright-white))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nerd icons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'nerd-icons)
(require 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy-posframe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-posframe gives floating ivy windows
(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
(ivy-posframe-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Yasnippets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; allow to use 'a' key in dired buffers to open the folders/files in the same buffer rather than make a new one
(put 'dired-find-alternate-file 'disabled nil)
;; show file sizes in human readable format in dired
(setq dired-listing-switches "-alh")
;; move, copy files from one dired buffer to another by default
(setq dired-dwim-target t)
;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)
;; initially hide details in dired
;; use '(' to toggle info
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; use 'k' to kill the dired buffer
;; use 'K' to kill subdir in the buffer (added by 'i')
;; use ',' to go to parent directory
;; use 's' to search files
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "k") 'kill-current-buffer)
     (define-key dired-mode-map (kbd "K") 'dired-kill-subdir)
     (define-key dired-mode-map (kbd ",") 'dired-jump)
     (define-key dired-mode-map (kbd "s") 'swiper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Company ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function to resize window
(defun unmole-resize (key)
  "interactively resize the window" 
  (interactive "cHit '['/']' to enlarge/shrink vertically and '{'/'}' for horizontally")
  (cond
   ((eq key (string-to-char "["))
    (enlarge-window 1)
    (call-interactively 'unmole-resize))
   ((eq key (string-to-char "]"))
    (enlarge-window -1)
    (call-interactively 'unmole-resize))
   ((eq key (string-to-char "{"))
    (shrink-window-horizontally 1)
    (call-interactively 'unmole-resize))
   ((eq key (string-to-char "}"))
    (enlarge-window-horizontally 1)
    (call-interactively 'unmole-resize))
   (t (push key unread-command-events))))

(defconst UNMOLE-DEFAULT-SHELL shell-file-name)
(defun unmole-change-shell ()
  "change shell used by emacs to call async commands"
  (interactive)  
  (let ((menu-map (make-sparse-keymap "Simple Choices")))
    (define-key menu-map [choice-1]
                '(menu-item "Default shell"
                            (lambda ()
                              (interactive)
                              (setq shell-file-name UNMOLE-DEFAULT-SHELL)
                              (message (concat "set shell to " UNMOLE-DEFAULT-SHELL)))))
    
    (define-key menu-map [choice-2]
                '(menu-item "Bash"
                            (lambda ()
                              (interactive)
                              (setq shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
                              (message (concat "set shell to bash")))))
    (tmm-prompt menu-map)))


;; recompile buffer fullscreen
(defun switch-buffer-recompile ()
  "switch to compilation buffer and then recompile"
  (interactive)
  (switch-to-buffer '"*compilation*")
  (recompile))

(defun insert-char-zero-width ()
  "insert the zero width space (used to escape org-mode formatting)"
  (interactive)
  (insert "​"))

(defun compile-elisp ()
  "interpret an emacs-lisp script using compilation-mode"
  (interactive)
  (compile (concat "emacs --script " (read-file-name "emacs --script "))))

(defun unmole-open-file (&optional secondary)
  "Open the file in an external program.
   Code taken from (browse-url-of-dired-file)"
  (interactive "P")
  (let ((tem (read-file-name "Select file : "))
        (browse-url-browser-function
         (if secondary
             browse-url-secondary-browser-function
           browse-url-browser-function))
        ;; Some URL handlers open files in Emacs.  We want to always
        ;; open in a browser, so disable those.
        (browse-url-default-handlers nil))
    (if tem
	(browse-url-of-file (expand-file-name tem))
      (error "No file on this line"))))

(defun unmole-tab-new ()
  "open a new tab and open current directory in dired in it"
  (interactive)
  (setq tab-bar-new-tab-choice default-directory)
  (tab-new)
  ;; change the character of the menu-bar
  (setq tab-bar-menu-bar-button ""))

(defun unmole-grepr ()
  "Search pattern from all files in current directory recursively using grep"
  (interactive)
  (let ((grepr-buffer-name))
    (progn
      (setq grepr-buffer-name (concat "unmole-grepr " (emacs-uptime "%s")))
      (generate-new-buffer grepr-buffer-name)
      (async-shell-command (concat
		"grep --color=always -nH --recursive "
		(read-from-minibuffer "pattern : ")
                " \"" default-directory "\".")
	       grepr-buffer-name)
      (with-current-buffer (get-buffer grepr-buffer-name)
	(compilation-minor-mode)))))

(defun unmole-ripgrepr ()
  "Search pattern from all files in current directory recursively using ripgrep"
  (interactive)
  (let ((ripgrepr-buffer-name))
    (progn
      (setq ripgrepr-buffer-name (concat "unmole-ripgrepr " (emacs-uptime "%s")))
      (generate-new-buffer ripgrepr-buffer-name)
      (async-shell-command (concat "rg -n --column "
			           (read-from-minibuffer "pattern : ") " \""
                                   default-directory "\"")
			   ripgrepr-buffer-name)
      (with-current-buffer (get-buffer ripgrepr-buffer-name)
	(compilation-minor-mode)))))

;; align columns
(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x t n") 'unmole-tab-new)
(global-set-key (kbd "C-x t k") 'tab-close)

(global-set-key (kbd "C-{") 'unmole-resize)
(global-set-key (kbd "C-z g") 'unmole-ripgrepr)
(global-set-key "\C-x&" 'recompile)
;; (global-set-key "\C-x&" 'switch-buffer-recompile)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key "\M-n" 'forward-list)
(global-set-key "\M-p" 'backward-list)
(global-set-key "\M-*" 'unmole-open-file)
(global-set-key (kbd "C-M-SPC") 'pop-to-mark-command)
(global-set-key (kbd "C-z M-s") 'scratch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Resume Garbage Collection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set to 100mb
(setq gc-cons-threshold (* 1024 1024 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Extra Comments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adding to the path in Windows is done as shown
;; (setenv "PATH" (concat (getenv "PATH") "C:\\Program Files (x86)\\GnuWin32\\bin;"))
;; (setq exec-path (append exec-path '("C:\\Program Files (x86)\\GnuWin32\\bin")))
