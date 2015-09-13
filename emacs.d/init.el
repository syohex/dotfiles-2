

(setq user-full-name "Xuehao Zhou")
(setq user-mail-address "robertzhouxh@gmail.com")
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/bin")))
(setq load-path (cons "/usr/local/lib/gtags" load-path))

(setq org-ditaa-jar-path "~/.emacs.d/vendor/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/vendor/plantuml.jar")
(setq plantuml-jar-path "~/.emacs.d/vendor/plantuml.jar")

(setq is-mac (equal system-type 'darwin))

(setenv "GOPATH" (concat (getenv "HOME") "/go"))

;; things that don't come from package managers
(defvar abedra/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(defvar abedra/config-dir (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path abedra/vendor-dir)
(add-to-list 'load-path abedra/config-dir)

(dolist (project (directory-files abedra/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))


(setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *win32* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )
(setq *xemacs* (featurep 'xemacs) )
(setq *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24))) )
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))

(require 'cl)
(require 'package)
(package-initialize)


;; Add melpa to package repos
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("org" . "http://orgmode.org/elpa/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ("melpa" . "http://melpa.org/packages/")
                  ("elpy" . "http://jorgenschaefer.github.io/packages/")))
  (add-to-list 'package-archives source t))

;;; Pin some packages to specific repositories.
(setq package-pinned-packages '((gtags . "marmalade")
                                (magit . "melpa-stable")))


(defvar abedra/packages '(dash                               ;A modern list api for Emacs. No 'cl required
                          chinese-pyim
                          cl-lib
                          ack
                          ag                                 ;do grep
                          bbdb                               ;database
                          expand-region                      ;smart region selection
                          neotree                            ;files tree
                          fullframe
                          dash-at-point
                          ffap                               ;find file at point
                          diminish
                          w3m

                          tabbar
                          multiple-cursors
                          smartrep
                          ace-jump-mode
                          whitespace
                          ibuffer-vc
                          golden-ratio
                          switch-window

                          ; erlang
                          edts

                          ; c/c++ ide
                          anzu
                          company
                          company-c-headers
                          ggtags
                          helm-gtags
                          helm
                          helm-projectile
                          projectile                          ;find file/folder in project
                          undo-tree
                          zygospore
                          smex                                ;M-x interface with ido style

                          ;; evil
                          evil-leader
                          evil-surround
                          evil-paredit
                          evil-nerd-commenter
                          evil-matchit
                          evil

                          ;; modes
                          cmake-mode
                          json-mode
                          web-mode
                          sass-mode
                          less-css-mode
                          coffee-mode
                          js2-mode
                          lua-mode
                          mmm-mode
                          emmet-mode
                          yaml-mode
                          puppet-mode
                          python-mode
                          protobuf-mode
                          writegood-mode

                          ;; Dired
                          dired-details
                          dired-details+
                          dired+
                          dired-rainbow

                          ;; UI
                          smart-mode-line
                          switch-window
                          rainbow-mode
                          rainbow-delimiters
                          monokai-theme
                          sublime-themes
                          zenburn-theme
                          solarized-theme

                          git-gutter
                          git-messenger
                          magit
                          paredit
                          move-text
                          gist
                          htmlize
                          markdown-mode
                          flycheck
                          flycheck-pos-tip
                          flycheck-clojure
                          flx                                 ;fuzzy matching
                          flx-ido                             ;fuzzy matching for ido
                          ido-vertical-mode
                          ido-at-point
                          ido-ubiquitous                      ;use ido nearly everywhere
                          yasnippet
                          smartparens
                          simple-httpd
                          guide-key
                          nodejs-repl
                          restclient
                          highlight-escape-sequences
                          whitespace-cleanup-mode
                          elisp-slime-nav
                          dockerfile-mode
                          clojure-mode
                          clojure-mode-extra-font-locking
                          groovy-mode
                          prodigy
                          cider
                          string-edit

                          ))

(defun abedra/packages-installed-p ()
  (loop for pkg in abedra/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (abedra/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg abedra/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))



;; starting deployment
(require 'dash)
(require 'init-util)

;; built-in plugins
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; simple plugins
(require 'restclient)
(require 'ffap)
(require 'prodigy)
(require 'helm-projectile)


;; init plugins
;;(require 'init-bbdb)
;;(require 'init-helm)
;;(require 'init-ggtags)
;;(require 'init-golden-ratio)
(require 'init-mode-mapps)
(require 'init-bootstrap)
(require 'init-appearance)
(require 'init-tabbar)
(require 'init-ediff)
(require 'init-autoinsert)
(require 'init-ibuffer)
(require 'init-emacs-w3m)
;;(require 'init-gnus)
(require 'init-mu4e)
(require 'init-yasnippet)
(require 'init-whitespace)
(require 'init-undo-tree)
(require 'init-projejctile)
(require 'init-neotree)
(require 'init-mutiple-curesor)
(require 'init-magit)
(require 'init-git-gutter)
(require 'init-mac)
(require 'init-ido)
(require 'init-dash-at-point)
(require 'init-company)
(require 'init-anzu)
(require 'init-ack)
(require 'init-ace-jump-mode)
(require 'init-expand-region)
(require 'init-flycheck)
(require 'init-flyspell)
(require 'init-guide-key)
(require 'init-helm-gtags)
(require 'dired-sort-map)
(require 'init-dired)
(require 'init-clipboard)
(require 'init-misc-modes)
(require 'init-org-mode)
(require 'init-cc-mode)
(require 'init-keybindings)
(require 'init-evil)
(require 'init-edts)
(require 'init-man)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:handled-backends (quote (git hg bzr)))
 '(git-gutter:modified-sign "==")
 '(package-selected-packages
   (quote
    (smart-mode-line evil-indent-textobject evil-surround evil-jumper evil-leader zygospore zenburn-theme yasnippet yaml-mode ws-butler writegood-mode window-numbering whitespace-cleanup-mode web-mode volatile-highlights undo-tree tabbar switch-window sublime-themes string-edit solarized-theme smex smartrep smartparens simple-httpd sass-mode restclient rainbow-mode rainbow-delimiters python-mode puppet-mode protobuf-mode prodigy paredit nodejs-repl neotree multiple-cursors move-text monokai-theme mmm-mode markdown-mode magit lua-mode less-css-mode json-mode js2-mode iedit ido-vertical-mode ido-ubiquitous ido-hacks ido-better-flex ido-at-point ibuffer-vc htmlize highlight-escape-sequences helm-swoop helm-projectile helm-gtags helm-flycheck helm-descbinds header2 guide-key groovy-mode goto-chg google-translate google-this google-c-style golden-ratio git-messenger gist ggtags fullframe flycheck-pos-tip flycheck-clojure flx-ido find-file-in-project expand-region exec-path-from-shell emmet-mode elisp-slime-nav ecb duplicate-thing dtrt-indent dockerfile-mode dired-rainbow dired-details+ dired+ diminish dash-at-point cypher-mode cpputils-cmake company-c-headers company-anaconda comment-dwim-2 coffee-mode cmake-mode clojure-mode-extra-font-locking clean-aindent-mode chinese-pyim bbdb anzu ag ack ace-jump-mode ac-helm)))
 '(pyim-dicts
   (quote
    ((:name "pyim-bigdict" :file "~/githubs/emacs.d/vendor/pyim-bigdict.pyim" :coding utf-8-unix :dict-type pinyin-dict)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(neo-banner-face ((t :inherit shadow)) t)
 '(neo-button-face ((t :inherit dired-directory)) t)
 '(neo-dir-link-face ((t :inherit dired-directory)) t)
 '(neo-expand-btn-face ((t :inherit button)) t)
 '(neo-file-link-face ((t :inherit default)) t)
 '(neo-header-face ((t :inherit shadow)) t)
 '(neo-root-dir-face ((t :inherit link-visited :underline nil)) t))
