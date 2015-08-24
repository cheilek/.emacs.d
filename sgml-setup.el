
;; Xml Setup

;; zencoding renamed to emmet: http://emmet.io/

;;(require 'emmet-mode)
;;(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes

(add-hook 'sgml-mode-hook
          (lambda ()
	    (setq sgml-basic-offset 4)
            (setq indent-tabs-mode nil)
	    )
	  )

