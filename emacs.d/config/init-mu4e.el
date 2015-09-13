;; ===> http://docs.offlineimap.org/en/latest/FAQ.html#how-do-i-generate-an-sslcacertfile-file
;; ===> https://wiki.archlinux.org/index.php/OfflineIMAP#Using_GPG

;; ===> http://kirang.in/2014/11/13/emacs-as-email-client-with-offlineimap-and-mu4e-on-osx/
;; ===> https://gist.github.com/areina/3879626
;; ===> http://www.djcbsoftware.nl/code/mu/mu4e.html

;; # 1. Generate certs
;; ➜  Maildir  hostname
;; RobertzhouxhsMacPro
;; ➜  Maildir  export hostname="imap.gmail.com"
;; ➜  Maildir  export cert_path="/Users/robertzhouxh/Maildir"
;; ➜  Maildir  export sslcacertfile=$cert_path/'gmail.cert'
;; ➜  Maildir   openssl s_client -CApath $cert_path -connect ${hostname}:imaps -showcerts \
;; >   | perl -ne 'print if /BEGIN/../END/; print STDERR if /return/' > $sslcacertfile

;; # 2. Verfiy cert
;; ➜ openssl s_client -CAfile $sslcacertfile -connect ${hostname}:imaps 2>&1 </dev/null


;; 3. Generate .authinfo.gpg
;; https://wiki.archlinux.org/index.php/OfflineIMAP#Using_GPG
;; ➜ emacs ~/.authinfo
;; machine imap.gmail.com login areina0@gmail.com port 993 password blabla123bla456(doixdu...)
;; machine smtp.gmail.com login areina0@gmail.com port 587 password blabla123bla456
;; With emacs, to encrypt this file:
;; M-x epa-encrypt-file (generate ~/.authinfo.gpg and remove original).

;; 4. execute in shell:  ➜ offlineimap   <================ haoge
;; 5. ➜ do not use brew install mu
;; /usr/bin/emacs is too old ============> sudo ln -s /Applications/Emacs.app/Contents/MacOS/Emacs /usr/bin/emacs
;; brew install mu --with-emacs --HEAD
;; mu4e is installed with mu

;; 6. Launch
;; ➜ mu index --maildir=~/Maildir
;; This should just be a matter of few minutes. In the meantime, let us configure mu4e to work with Emacs.


(require 'mu4e)

;; Xuehao Zhou
(setq mu4e-mu-binary "/usr/local/bin/mu")

;; default
(setq mu4e-maildir (expand-file-name "~/Maildir"))

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
;; I don't use a signature...
(setq
 user-mail-address "robertzhouxh@gmail.com"
 user-full-name  "Xuehao Zhou"
 message-signature
 (concat
  "周学浩 (Zhou Xuehao)\n"
  "Email: robertzhouxh@gmail.com\n"
  "Blog: http://robertzhouxh.github.io/\n"
  "www.intorobot.com"
  "\n"))

;;; Html rendering
(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command (cond ((fboundp 'w3m)
                                    (lambda ()          ; Use emacs-w3m
                                      (w3m-region (point-min) (point-max))))
                                   ((executable-find "w3m")
                                    "w3m -T text/html") ; Use w3m shell-command
                                   (t (lambda ()        ; Use shr (slow)
                                        (let ((shr-color-visible-luminance-min 75)
                                              shr-width)
                                          (shr-render-region (point-min) (point-max)))))))



;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)


(provide 'init-mu4e)