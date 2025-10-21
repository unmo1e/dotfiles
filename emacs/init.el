;; Set default encoding for files
;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
;; do this especially on Windows, else python output problem
(set-terminal-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq visible-bell 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; use space to format regions instead of tabs
(setq indent-tabs-mode nil)

;; set newline when moving cursor down
(setq next-line-add-newlines t)

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
      '(ivy counsel swiper magit company god-mode eat mini-modeline spacious-padding org-download))
(package-install-selected-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Visual Changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setting font
;; this line is for only changing fontsize
;; (set-face-attribute 'default nil :font "Monospace" :height 160)
(set-frame-font "Comic Code Ligatures-18" nil t)

(load-theme 'midnight-warmth 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Compilation Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add colors in compilation mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Small Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(spacious-padding-mode)
(mini-modeline-mode)
(global-set-key (kbd "<escape>") #'god-local-mode)
(add-hook 'dired-mode-hook 'org-download-enable)

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
;; use ',' to go to parent directory
;; use 's' to search files
(eval-after-load "dired" '(progn
			    (define-key dired-mode-map (kbd ",") 'dired-jump)
			    (define-key dired-mode-map (kbd "s") 'swiper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Company ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function to resize window
(defun v-resize (key)
  "interactively resize the window" 
  (interactive "cHit '['/']' to enlarge/shrink vertically and '{'/'}' for horizontally")
  (cond
   ((eq key (string-to-char "["))
    (enlarge-window 1)
    (call-interactively 'v-resize))
   ((eq key (string-to-char "]"))
    (enlarge-window -1)
    (call-interactively 'v-resize))
   ((eq key (string-to-char "{"))
    (shrink-window-horizontally 1)
    (call-interactively 'v-resize))
   ((eq key (string-to-char "}"))
    (enlarge-window-horizontally 1)
    (call-interactively 'v-resize))
   (t (push key unread-command-events))))

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
  "insert the zero width space (used to escape org-mode formatting)"
  (interactive)
  (compile (concat "emacs --script " (read-from-minibuffer "emacs --script "))))

(defun open-file-windows ()
  "open files in windows, uses powershell"
  (interactive)
  (call-process-shell-command (concat "start " (read-file-name "Select file : ")) nil 0 nil))

(defun grepr ()
  "Search pattern from all files in current directory recursively using grep"
  (interactive)
  (let ((grepr-buffer-name))
    (progn
      (setq grepr-buffer-name (concat "grepr " (emacs-uptime "%s")))
      (generate-new-buffer grepr-buffer-name)
      (async-shell-command (concat
			    "grep --color=always -nH --recursive "
			    (read-from-minibuffer "pattern : ") " .")
			   grepr-buffer-name)
      (with-current-buffer (get-buffer grepr-buffer-name)
	(compilation-minor-mode)))))

;; align columns
(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-{") #'v-resize)
(global-set-key "\C-x&" 'recompile)
;; (global-set-key "\C-x&" 'switch-buffer-recompile)
;; Overwrite suspend frame keybinding to avoid accidental suspend.
(global-set-key "\C-z" 'backward-char)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key "\M-n" 'forward-list)
(global-set-key "\M-p" 'backward-list)

;; some bindings need to check the OS
;; eg, the opening file command depends on OS
(cond ((eq system-type 'windows-nt)
       (global-set-key "\M-*" 'open-file-windows))
      ((eq system-type 'gnu/linux)
       (message "No equivalent for linux yet"))
      (t (message "Emacs is running on another operating system: %s" system-type)))
