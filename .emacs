;; This section is just used to attempt to optimize speed

;; Avoid garbage collection during startup. The GC eats up quite a bit of time, easily
;; doubling the startup time. The trick is to turn up the memory threshold in order to
;; prevent it from running during startup.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Every file opened and loaded by Emacs will run through this list to check for a proper
;; handler for the file, but during startup, it won't need any of them.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; After Emacs startup has been completed, set 'gc-cons-threshold' to
;; 16 MB and reset 'gc-cons-percentage' to its original value.
;; Also reset 'file-name-handler-alist'
(add-hook 'emacs-startup-hook
          '(lambda ()
             (setq gc-cons-threshold (* 16 1024 1024)
                   gc-cons-percentage 0.1
                   file-name-handler-alist file-name-handler-alist-original)
             (makunbound 'file-name-handler-alist-original)))
;; The speed optimizations end here

;; Set basic emacs settings for more comfort
;; Silence audio bell
(setq ring-bell-function 'ignore)
;; Turn off landing apge
(setq inhibit-startup-message t)

;; Disable unwanted built in modes
;; Remove unwanted UI
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)

;; Enable useful built-in modes
;; (ido-mode 1) ;; Basic auto-complete
(column-number-mode t) ;; Column numbers in mode line
(global-display-line-numbers-mode t) ;; Line numbers
(windmove-default-keybindings 'shift) ;; Move between splits easily

;; Disable line numbers in certain modes
(dolist (mode '(eshell-mode-hook
		shell-mode-hook
		term-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Change font
(set-frame-font "Hack" nil t)

;; Set dark theme
;; Only built in themes can be set before the packages are loaded
;; Package themes will be set in teh package specific config
;; (load-theme 'misterioso)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install packages
(straight-use-package 'command-log-mode) ;; clm/toggle-command-log-buffer - logs keypresses and called commands
(straight-use-package 'counsel) ;; Autocomplete that works with ivy
(straight-use-package 'crux) ;; Collection of useful functions
(straight-use-package 'doom-modeline) ;; More modern modeline
(straight-use-package 'doom-themes) ;; Collection of themes
(straight-use-package 'evil) ;; Vim emulation
(straight-use-package 'evil-collection) ;; Vim emulation accross popular emacs packages
(straight-use-package 'evil-numbers) ;; Vim emulation for number commands
(straight-use-package
 '(evil-plugins :type git :host github :repo "tarao/evil-plugins")) ;; Vim emulation that mimics popular vim packages
(straight-use-package 'evil-surround) ;; Vim emultaion extension that mimics vim-surround
(straight-use-package 'general) ;; Easier way to bind keys and set a leader key. Works well with evil-mode
(straight-use-package 'helm) ;; Fuzzy search and completion with bells and whistles
(straight-use-package 'helpful) ;; 
(straight-use-package 'ivy) ;; Minimal fuzzy search and completion
(straight-use-package 'ivy-rich) ;; More detail on ivy and counsel functions
(straight-use-package 'magit) ;; Version control package for git with utility functions
(straight-use-package 'rainbow-delimiters) ;; Highlight nested parenthesis
(straight-use-package 'which-key) ;; Helpful learning buffer that completes commands it is configured for
(straight-use-package 'winum) ;; Allows you to jump to split by number

;; Load packages
(require 'counsel)
(require 'crux)
(require 'general)
(require 'ivy)
(require 'ivy-rich)
(require 'winum)


(ivy-mode 1)
(ivy-rich-mode 1)
(which-key-mode 1)
(winum-mode 1)

;; Package specific configurations
;; ivy config
(setq ivy-user-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Which-key config
(setq which-key-idle-delay 0)

;; Helpful-counsel config
(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

;; doom-theme config
(load-theme 'doom-solarized-dark 1)

;; My Custom functions
(defun custom-goto-emacs-init-file ()
  (interactive)
  (find-file user-init-file))


;; Add useful key bindings
(general-define-key
 "C-." 'custom-goto-emacs-init-file
 "C-<return>" 'crux-smart-open-line-above
 "C-`" 'winum-select-window-by-number
 "C-c F" 'crux-recentf-find-directory
 "C-c M-d" 'crux-duplicate-and-comment-current-line-or-region
 "C-c d" 'crux-duplicate-current-line-or-region
 "C-c f" 'crux-recentf-find-file
 "C-c j" 'crux-top-join-line
 "C-c n" 'cruc-cleanup-buffer-or-region
 "C-c o" 'crux-open-with
 "C-c s" 'swiper
 "C-h C-C" 'helpful-command
 "C-h C-d" 'helpful-at-point
 "C-h F" 'helpful-function
 "C-h f" 'helpful-callable
 "C-h k" 'helpful-key
 "C-h v" 'helpful-variable
 "C-k" 'crux-smart-kill-line
 "C-x 4 t" 'crux-transpose-windows
 "C-x C-f" 'counsel-find-file
 "C-x b" 'counsel-switch-buffer
 "M-x" 'counsel-M-x
"<escape>" 'keyboard-escape-quit)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 "<tab>" 'ivy-alt-done
 "C-l" 'ivy-alt-done
 "C-j" 'ivy-next-line
 "C-k" 'ivy-previous-line)

(general-define-key
 :keymaps 'minibuffer-local-map
 "C-r" 'counsel-minibuffer-history)


