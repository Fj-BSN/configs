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
(straight-use-package 'avy) ;; Jump to places in a buffer
(straight-use-package 'all-the-icons) ;; Fonts for emacs - needed for doom-modeline - the first time using emacs on a new machine, run the command all-the-icons-install-fonts to install teh fonts on the machine OS.
(straight-use-package 'command-log-mode) ;; clm/toggle-command-log-buffer - logs keypresses and called commands
(straight-use-package 'counsel) ;; Autocomplete that works with ivy
(straight-use-package 'crux) ;; Collection of useful functions
(straight-use-package 'doom-modeline) ;; More modern modeline
(straight-use-package 'doom-themes) ;; Collection of themes
(straight-use-package 'evil) ;; Vim emulation 5
(straight-use-package 'evil-collection) ;; Vim emulation accross popular emacs packages
(straight-use-package 'evil-commentary) ;; Vim emulation accross popular emacs packages
(straight-use-package 'evil-numbers) ;; Vim emulation for number commands
(straight-use-package
 '(evil-plugins :type git :host github :repo "tarao/evil-plugins")) ;; Vim emulation that mimics popular vim packages
(straight-use-package 'evil-snipe) ;; Vim emulation for snipe/seek package
(straight-use-package 'evil-surround) ;; Vim emultaion extension that mimics vim-surround
(straight-use-package
 '(evil-unimpaired :type git :host github :repo "zmaas/evil-unimpaired")) ;; Vim emultaion extension that mimics vim-unimpaired
(straight-use-package 'general) ;; Easier way to bind keys and set a leader key. Works well with evil-mode
(straight-use-package 'helm) ;; Fuzzy search and completion with bells and whistles
(straight-use-package 'helpful) ;; Replacement for built-in emacs documentation
(straight-use-package 'hydra) ;; Key bindings for adjusting settings with repeating commands
(straight-use-package 'ivy) ;; Minimal fuzzy search and completion
(straight-use-package 'ivy-rich) ;; More detail on ivy and counsel functions
(straight-use-package 'magit) ;; Version control package for git with utility functions
(straight-use-package 'undo-tree) ;; Advanced undo system
(straight-use-package 'rainbow-delimiters) ;; Highlight nested parenthesis
(straight-use-package 'which-key) ;; Helpful learning buffer that completes commands it is configured for
(straight-use-package 'winum) ;; Allows you to jump to split by number

;; Load packages
(require 'counsel)
(require 'crux)
(require 'evil-little-word)
(require 'general)
(require 'ivy)
(require 'ivy-rich)
(require 'undo-tree)
(require 'winum)

(doom-modeline-mode 1)
(ivy-mode 1)
(ivy-rich-mode 1)
(global-undo-tree-mode 1)
(which-key-mode 1)
(winum-mode 1)

;; Package specific configurations

;; evil configuration
(setq evil-want-C-d-scroll t) ;; Scroll as is standard in vim
(setq evil-want-C-i-jump nil) ;; C-i controls jump list as in default vim
(setq evil-want-C-u-scroll t) ;; Scroll as is standard in vim
(setq evil-want-Y-yank-eol t) ;; Make Y yank to the end of the line, unlike default vim, but in line with other vim commands
(setq evil-want-integration t)
(setq evil-want-keybinding nil) ;; Sets evil keybindings in other modes - evil-collection does this better
;; (setq evil-undo-system 'undo-tree) ;; Sets underlying redo system for evil to use

(evil-mode 1)
(evil-commentary-mode 1)
(evil-surround-mode 1)
(evil-unimpaired-mode 1)
(evil-set-undo-system 'undo-tree)

;; (evil-collection-init) ;; Binds vim-like keys for many major modes

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

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

;; Define custom hydras
(defhydra hydra-text-scale (:timeout 4)
  "Scale Text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("u" (text-scale-set 0) "default")
  ("l" nil "done" :exit t))

;; Add useful global key bindings
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

;; evil keybindings
(general-define-key
 :states '(motion normal)
 "C-a" 'evil-numbers/inc-at-pt
 "C-x" 'evil-numbers/dec-at-pt)

;; Add pairs to evil-unimpaired
(evil-unimpaired-define-pair "e" '(move-text-up . move-text-down) '(normal visual))

;; Create Leader Key
(general-create-definer Fj-BSN/leader-keys
  :keymaps '(normal)
  :prefix "SPC")

(Fj-BSN/leader-keys
  "h"  '(:ignore t :which-key "quick settings")
  "ht"  '(hydra-text-scale/body :which-key "text size")
  "a"  '(:ignore t :which-key "apps")
  "au"  '(:ignore t :which-key "undo-tree")
  "auv" 'undo-tree-visualize
  "aub" 'undo-tree-switch-branch
  "b"  '(:ignore t :which-key "buffer")
  "b<tab>" 'crux-switch-to-previous-buffer ;; this doesn't work
  "bb" 'counsel-switch-buffer
  "bx" 'kill-buffer
  "f"  '(:ignore t :which-key "file")
  "ff" 'counsel-find-file
  "fo" 'crux-open-with
  "fr" 'crux-recentf-find-file
  "fs" 'save-buffer
  "g"  '(:ignore t :which-key "goto")
  "gt." 'custom-goto-emacs-init-file
  "q"  '(:ignore t :which-key "quit")
  "qq" 'save-buffers-kill-terminal
  "s"  '(:ignore t :which-key "search")
  "ss" 'swiper
  "w"  '(:ignore t :which-key "window")
  "wo" 'delete-other-windows
  "wq" 'delete-window
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "j"  '(:ignore t :which-key "jump")
  "jj" 'avy-goto-char-in-line
  "jl" 'avy-goto-line
  "jw" 'avy-goto-word-1
)
