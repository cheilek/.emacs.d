(setq load-path
      (append
       (list "~/.emacs.d/markdown-mode/")
       load-path))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
;;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
