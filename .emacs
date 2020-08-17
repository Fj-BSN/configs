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

;; Set the default font to something more suited to coding
(set-frame-font "Hack" nil t)

;;sets a darker theme
(load-theme 'deeper-blue)

;;set some evil settings
(require 'evil-little-word)
(require 'evil-surround)
(require 'evil-numbers)
(require 'evil-unimpaired)
(require 'evil-commentary)
(require 'evil-snipe)

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
