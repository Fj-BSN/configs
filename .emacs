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

;; Installs the packages I want
;; (straight-use-package 'god-mode)
;; (straight-use-package
;;     '(evil-god-state :type git :host github :repo "gridaphobe/evil-god-state"))
;; define a key to use god-mode in evil-mode
;; (evil-define-key 'normal global-map "\\" 'evil-execute-in-god-state)
;; (evil-define-key 'god global-map [escape] 'evil-god-state-bail)

;; Add some evil specific plug-ins to mimic useful vim plug-ins
;; (straight-use-package
;;     '(evil-plugins :type git :host github :repo "tarao/evil-plugins"))
;; (require 'evil-little-word)
;; (require 'evil-numbers)

;; (straight-use-package 'winum)
;;(setq winum-keymap
;;    (let ((map (make-sparse-keymap)))
;;      (define-key map (kbd "C-`") 'winum-select-window-by-number)
;;      map))
;; (straight-use-package 'evil-numbers)
;; (straight-use-package 'doom-themes)

(straight-use-package 'evil)

(straight-use-packge
 '(evil-unimpaired :type git :host github :repo "zmaas/evil-unimpaired"))

(straight-use-package
 '(evil-commentary :type git :host github :repo "linktohack/evil-commentary"))
  
(straight-use-package
 '(evil-surround :type git :host github :repo "emacs-evil/evil-surround"))

(straight-use-package
 '(evil-snipe :type git :host github :repo "hlissner/evil-snipe"))

(straight-use-package 'magit)

(straight-use-package 'ledger-mode)

(straight-use-package 'org)
(straight-use-package 'org-roam)

(straight-use-package 'general)


;; (straight-use-package 'exwm)

;; ;; make sure the exwm code is not loaded when in terminal mode
;; (when (display-graphic-p)
;;     (require 'exwm)
;; 	(require 'exwm-config)
;; 	(exwm-config-default))


;;Enables modes I want
(evil-mode 1)
(evil-unimpaired-mode 1)
(evil-surround-mode 1)
(evil-commentary-mode 1)
(evil-snipe-mode 1)

(ido-mode 1)

;; set the inferior lisp process
;; (setq inferior-lisp-program "clisp.exe")

;; Disables unwanted GUI stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)

;; Set the default font to something more suited to coding
;; (set-frame-font "Hack" nil t)

;;sets a darker theme
(load-theme 'deeper-blue)

;; Adds functionaliy to move lines
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

(defun go-to-dot-file ()
    (interactive)
   "Open your .emacs file for easy editing"
   (find-file user-init-file))

(defun reload-dot-file ()
    (interactive)
   "Open your .emacs file for easy editing"
   (load-file user-init-file))

;; Extends evil-unimpaired commands
(evil-unimpaired-define-pair "e" '(move-line-up . move-line-down) '(normal visual))
;; Makes it easier to manage window splits
(windmove-default-keybindings)

;; Define some custom commands
(global-set-key (kbd "C-.") 'go-to-dot-file)

;; set super key binding 
(define-key key-translation-map (kbd "<home>") #'event-apply-super-modifier)

;; Define cstom keys sing general
(require 'general)
;; evil bindings
(general-evil-setup)
(general-nmap
 :prefix "SPC"
 :prefix-map 'my-leader-local-map
 "fs"  'save-buffer
 "ff"  'ido-find-file
 "gtd" 'go-to-dot-file
 "gtr" 'reload-dot-file)
