;; Save the starting time...
(defvar *init-start-time* (current-time))

(setq load-path (append
		 (list "~/.emacs.d/elisp/"
		       "~/.local/share/emacs/site-lisp/")
		 load-path))

(require 'xcscope)
(setq cscope-do-not-update-database t)
(put 'narrow-to-region 'disabled nil)
(define-key esc-map "*" 'revert-buffer)
(define-key esc-map "z" 'compile)

(add-to-list 'auto-mode-alist '("/Kconfig$" . kconfig-mode))
(add-to-list 'auto-mode-alist '("/Kconfig\\..*$" . kconfig-mode))
(add-to-list 'auto-mode-alist '("_defconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("/Kbuild$" . makefile-mode))

;; c.f. http://inst.eecs.berkeley.edu/~cs47b/emacs/lisp/cc-mode/cc-styles.el

(defconst xockets-c-style
  '((c-basic-offset . 4)
    (c-auto-newline)
    (comment-multi-line t)
    (c-offsets-alist
     (knr-argdecl-intro . +)
     (knr-argdecl . 0)
     (statement-cont . +)
     (statement-case-open . +)
     (substatement-open . 0)
     (label . 0)
     (case-label . 0)))
  "Xockets C Style for CC-MODE")

;;
;; Linux style as required in Documentation/CodingStyle
;;
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defconst linux-c-style
  '((c-basic-offset . 8)
    (indent-tabs-mode . t)
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only)))
  "Linux C Style for CC-MODE")

;;
;; LwIP Style based on:
;; http://lwip.wikia.com/wiki/Contributing_to_lwIP
;;   Source code style
;;
;; do not use tabs.
;; indentation is two spaces per level (i.e. per tab).
;; end debug messages with a trailing newline (\n).
;; one space between keyword and opening bracket.
;; no space between function and opening bracket.
;; one space and no newline before opening curly braces of a block.
;; closing curly brace on a single line.
;; spaces surrounding assignment and comparisons.
;; use current source code style as further reference.
;;
(defconst lwip-c-style
  '("lwip"
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only)))
  "LwIP C Style for CC-MODE")

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add Xockets style
            (c-add-style
             "XOCKETS" xockets-c-style)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only" linux-c-style)))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
	      (if (and filename
		       (or 
			(string-match "/kernel" filename)
			(file-exists-p (concat
					(file-name-directory filename)
					"Kbuild"))))
		  (progn
		    (message "Kernel mode: %s" filename)
		    (c-set-style "linux-tabs-only"))
		(progn
		  (message "Xockets mode: %s" filename)
		  (c-set-style "XOCKETS")
		  (setq indent-tabs-mode nil))
		))))

;; saveplace: save location in file when saving files
(require 'saveplace)                   ;; get the package
(setq
 save-place-file (format "~/.emacs.d/cache/saveplace.%s"
			 (getenv "HOSTNAME"))
 save-place-limit 10)
(setq-default save-place t)            ;; activate it for all buffers

(if (>= emacs-major-version '22)
    ;;
    ;; savehist: save some history
    (require 'savehist)
    (setq savehist-additional-variables    ;; also save...
	  '(search ring regexp-search-ring);; ... my search entries
	  savehist-autosave-interval 60    ;; save every minute (default: 5 min)
	  savehist-file (format "~/.emacs.d/cache/savehist.%s"
				(getenv "HOSTNAME"))) ;; keep my home clean
	  (savehist-mode t)                ;; do customization before activation
)

;; recentf
(require 'recentf)    ;; save recently used files
(setq recentf-save-file (format "~/.emacs.d/cache/recentf.%s"
				(getenv "HOSTNAME"))
      recentf-max-saved-items 100     ;; max save 100
      recentf-max-menu-items 15)      ;; max 15 in menu
(global-set-key "\C-x\ \C-r" 'recentf-open-files) ;; access it.
(recentf-mode t)                  ;; turn it on


(line-number-mode t)
(column-number-mode t)
(display-time-mode t)

(require 'doxymacs)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

(add-hook 'c-mode-common-hook 'doxymacs-mode)

(load "boxquote")

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

(setq frame-title-format `(,(user-login-name) "@" ,(system-name) "  [%b]" ))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'c-mode-common-hook
          'flyspell-prog-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
