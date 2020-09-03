;;; This section is just used to attempt to optimize speed

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

;; This will bootstrap the straight.el package manager by downloading it from it's github and storing it in the .emacs.d directory so it can be called to install packages
;; https://github.com/raxod502/straight.el

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
;; This is the end of the straight.el bootstrap code
;; This is how straight.el is used to get packages
;; This syntax can be used for a github package that is not in the ususal repositories
;;(el-patch :type git :host github :repo "raxod502/el-patch")

(straight-use-package 'god-mode)
(straight-use-package 'evil)
(straight-use-package
    '(evil-god-state :type git :host github :repo "gridaphobe/evil-god-state"))
(straight-use-package
    '(evil-plugins :type git :host github :repo "tarao/evil-plugins"))
(straight-use-package
    '(evil-unimpared :type git :host github :repo "zmaas/evil-unimpaired"))
(straight-use-package 'evil-commentary)
(straight-use-package 'evil-leader)
(straight-use-package 'evil-numbers)
(straight-use-package 'evil-snipe)
(straight-use-package 'evil-surround)
(straight-use-package 'ace-window)
(straight-use-package 'winum)
(straight-use-package 'magit)
(straight-use-package 'org)
(straight-use-package 'ivy)
(straight-use-package 'hydra)
(straight-use-package 'major-mode-hydra)
(straight-use-package 'company)
(straight-use-package 'doom-themes)
(straight-use-package 'exwm)

;; make sure the exwm code is not loaded when in terminal mode
(when (display-graphic-p)
    (require 'exwm)
	(require 'exwm-config)
	(exwm-config-default))

;; set super key binding 
(define-key key-translation-map (kbd "<home>") #'event-apply-super-modifier)
	
;; Set the default font to something more suited to coding
(set-frame-font "Hack" nil t)

;;sets a darker theme
(load-theme 'deeper-blue)

;;Add some evil specific plug-ins to mimic useful vim plug-ins
(require 'evil-little-word)
(require 'evil-numbers)
(evil-surround-mode)
(evil-unimpaired-mode)
(evil-commentary-mode)
(evil-snipe-mode)

;; Add useful functions to move lines up and down the same way vim-unimpaired does
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; bind the lien moving functions to vim-unimpaired
(evil-unimpaired-define-pair "e" '(move-line-up . move-line-down) '(normal visual))

;; define a key to use god-mode in evil-mode
(evil-define-key 'normal global-map "\\" 'evil-execute-in-god-state)
(evil-define-key 'god global-map [escape] 'evil-god-state-bail)

;; set the inferior lisp process
(setq inferior-lisp-program "clisp.exe")

;; keybindings for ace window/winum package
(global-set-key (kbd "M-p") 'ace-window)
;;(setq winum-keymap
;;    (let ((map (make-sparse-keymap)))
;;      (define-key map (kbd "C-`") 'winum-select-window-by-number)
;;      map))

(evil-mode)
;;(winum-mode)
(ido-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5dd2ef36219b9f109a72da9ea9fba150d0123653f06b44906881824215b16e44" "ecb923cbeddeadab6b60cb64ab56e57a3a910c678480f947ae679a1b824f6de0" "e08833c5dc1ba09647d6f7d2d55579fa25fe1b8c47038513007c41541667d9c8" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
