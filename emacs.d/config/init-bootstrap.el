;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(winner-mode 1)                                  ;; Undo/redo window configuration with C-c <left>/<right>
(global-subword-mode 1)                          ;; Easily navigate sillycased words
(auto-compression-mode t)                        ;; Transparently open compressed files
(global-font-lock-mode t)                        ;; Enable syntax highlighting for older Emacsen that have it off
(delete-selection-mode 1)                        ;; Remove text in active region if inserting text
(global-auto-revert-mode 1)                      ;; Auto refresh buffers

(setq inhibit-startup-message t)                 ;; No splash screen please ... jeez
;;(setq x-select-enable-clipboard t)
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(setq global-auto-revert-non-file-buffers t)     ;; Also auto refresh dired, but be quiet about it
(setq auto-revert-verbose nil)
(setq echo-keystrokes 0.1)                       ;; Show keystrokes in progress
(setq delete-by-moving-to-trash t)               ;; Move files to trash when deleting
(setq shift-select-mode nil)                     ;; Real emacs knights don't use shift to mark things
(setq jump-char-lazy-highlight-face nil)         ;; Don't highlight matches with jump-char - it's distracting
(setq enable-recursive-minibuffers t)            ;; Allow recursive minibuffers
(setq gc-cons-threshold 20000000)                ;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq org-replace-disputed-keys t)               ;; org-mode: Don't S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq electric-indent-mode nil)                  ;; No electric indent
(setq eval-expression-print-level nil)           ;; Nic says needs to be nil (turned off) so that you can always see what's happening.
(setq org-src-fontify-natively t)                ;; Fontify org-mode code blocks
(setq large-file-warning-threshold 100000000)    ;; Warn only when opening files bigger than 100MB

(set-default 'imenu-auto-rescan t)
(set-default 'indent-tabs-mode nil)              ;; Never insert tabs
(set-default 'indicate-empty-lines t)            ;; Show me empty lines after buffer end
(set-default 'sentence-end-double-space nil)     ;; Sentences do not need double spaces to end. Period.
(setq-default truncate-lines t)                  ;; Don't break lines for me, please


(defalias 'yes-or-no-p 'y-or-n-p)                ;; Answering just 'y' or 'n' will do

;; ----------------------- no buffers when starting ------------------
(setq initial-scratch-message "")    ;; Makes *scratch* empty.
;;Removes *messages* from the buffer.
;;(setq-default message-log-max nil)
;;(kill-buffer "*Messages*")
;; ----------------------- no buffers when starting ------------------


;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top


;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)


;; Emacs若在文件的第一行找到#!声明,则认为该文件为script文件.
;; 通过下面设置可用让Emacs自动給script文件加上可执行文件
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;-----kill ring 长度 
(require 'linum)
(global-linum-mode 1)
(setq line-number-mode t)                            ;; Always display line and column numbers
(setq column-number-mode t)
(add-hook 'org-mode-hook 'linum-mode)
(add-hook 'c++-mode-hook 'linum-mode)
(setq linum-format "%3d ")
(setq fill-column 80)                                ;; Lines should be 80 characters wide, not 72


;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)


;; recentf-mode
(recentf-mode 1)                                     ;; Save a list of recent files visited. (open recent file with C-x f)
(setq recentf-max-saved-items 100)                   ;; just 20 is too recent


;; backup
;; Write backup files to own directory
;; Remove all backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;;Rebind your C-x o key: for switch-window
(global-set-key (kbd "C-x o") 'switch-window)


(provide 'init-bootstrap)