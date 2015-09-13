
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5)
  )

;; gnus+davmail bug, so I have to use pop3 for davmail
;; http://permalink.gmane.org/gmane.emacs.gnus.general/83301
;; but delete all the mails on server is scary
(setq pop3-leave-mail-on-server t)

(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups ))
          )

;; (getenv "HOSTNAME") won't work because $HOSTNAME is not an env variable
;; (system-name) won't work because as Optus required, my /etc/hosts is changed
(setq my-hostname (with-temp-buffer
        (shell-command "hostname" t)
        (goto-char (point-max))
        (delete-char -1)
        (buffer-string))
      )

(defun at-office ()
  (interactive)
  (and (string= my-hostname "RobertzhouxhsMacPro")
       (not (string= my-hostname "homepc"))
       )
  )
(setq user-full-name "Xuehao Zhou"
      user-mail-address (if (at-office) "robertzhouxh@gmail.com" "robertzhouxh@gmail.com")
      )
(setq mm-text-html-renderer 'w3m)
(setq gnus-use-cache t)


(provide 'init-gnus)