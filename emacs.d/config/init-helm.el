(require 'helm-config)

(defun helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(helm-source-buffers-list
                       helm-source-files-in-current-dir
                       helm-source-ls-git
                       helm-source-recentf
                       helm-source-file-cache)
                     "*helm mini*"))

(setq helm-for-files-preferred-list '(helm-source-buffers-list
                                      helm-source-ls-git
                                      helm-source-files-in-current-dir
                                      helm-source-recentf
                                      helm-source-bookmarks
                                      helm-source-file-cache
                                      helm-source-locate))


(provide 'init-helm)
