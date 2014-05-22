;; Save the starting time...
(defvar *init-start-time* (current-time))

(setq load-path
      (append
       (list "~/.emacs.d/elisp/"
	     "~/.local/share/emacs/site-lisp/")
       load-path))


;; C styles
(load-file "~/.emacs.d/c-setup.el")

(require 'xcscope)
(setq cscope-do-not-update-database t)


;; markdown mode
;;(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

					; backups
(load-file "~/.emacs.d/backups.el")

;; saveplace
(load-file "~/.emacs.d/saveplace.el")

;; recentf
(load-file "~/.emacs.d/recentf.el")

;; doxymacs
(load-file "~/.emacs.d/doxymacs.el")

(require 'boxquote)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(define-key esc-map "*" 'revert-buffer)
(define-key esc-map "z" 'compile)

(line-number-mode t)
(column-number-mode t)
(display-time-mode t)

(setq frame-title-format `(,(user-login-name) "@" ,(system-name) "  [%b]" ))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'c-mode-common-hook
          'flyspell-prog-mode)


;; set up a bunch of auto-mode-alist stuff
(load-file "~/.emacs.d/auto-mode-alist-setup.el")

;; Get the end time.
(defvar *init-end-time* (current-time))
;; Print init time.
(message "Init took %d seconds"
         (- (+ (lsh (car *init-end-time*) 16) (cadr *init-end-time*))
	    (+ (lsh (car *init-start-time*) 16) (cadr
						 *init-start-time*))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(user-full-name "Alun Evans")
 '(user-mail-address "alun.evans@xockets.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
