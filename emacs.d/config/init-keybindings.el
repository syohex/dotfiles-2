(require 'init-util)

(setq mac-control-modifier 'control)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)


(global-set-key (kbd "C-c x") 'eshell)
(global-set-key [f4] 'eshell)
(global-set-key (kbd "C-=") 'weibo-timeline)
(global-set-key (kbd "C-x M-d") 'insert-current_time)
(global-set-key (kbd "C-c g") 'goto-line)


(global-set-key (kbd "C-c n") 'neotree-toggle)
(global-set-key (kbd "M-t") 'my-go-to-char)
(global-set-key (kbd "C-c k") 'align-to-equals)    ;; Align Text use "="

;;(define-key global-map (kbd "M-p") 'previous-multiframe-window)
;;(define-key global-map (kbd "M-n") 'other-window)

(define-key global-map (kbd "M-n") 'scroll-other-window)
(define-key global-map (kbd "M-p") 'scroll-other-window-down)


;; ======================== global keybindings =============================
;;(bind-key "C-c G" 'search-github)
;;(bind-key "C-c g" 'search-google)
;;(bind-key "C-c q" 'search-code)
(global-set-key (kbd  "C-c G") 'search-github)
(global-set-key (kbd  "C-c s") 'search-google)
(global-set-key (kbd  "C-c q") 'search-code)


;; Comment/uncomment block
;; (global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-\\") 'dabbrev-expand)


;; buffer
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-S-k") 'nuke-all-buffers)
(global-set-key (kbd "C-c r") 'revert-buffer)


;; line
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))


;; mark
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)


;; misc for global keybinds
;;(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)
(global-set-key (kbd "C-c C-k") 'compile)        ;compile
(global-set-key (kbd "C-c C-v") 'browse-url)     ;browse-url
(global-set-key (kbd "RET") 'newline-and-indent) ;automatically indent when press RET
(global-set-key (kbd "C-u") 'kill-back-to-indentation)
(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)
(global-set-key (kbd "C-u") 'kill-back-to-indentation)
(global-set-key (kbd "C-c o s") 'sudo-reopen-file)


;; 连续滚屏
;;M-n/p 移动本文
;;功能如下
;;(global-set-key (kbd "M-n") 'scroll-up-line)
;;(global-set-key (kbd "M-p") 'scroll-down-line)
(defun hold-line-scroll-up()
  "Scroll the page with the cursor in the same line"
  (interactive)
  (let ((next-screen-context-lines
		 (count-lines
		  (window-start) (window-end))))
    (scroll-up)))

(defun hold-line-scroll-down()
  "Scroll the page with the cursor in the same line"
  (interactive)
  (let ((next-screen-context-lines
		 (count-lines
		  (window-start) (window-end))))
    (scroll-down)))
;;(global-set-key (kbd "C-n") 'hold-line-scroll-up)
;;(global-set-key (kbd "C-p") 'hold-line-scroll-down)


(provide 'init-keybindings)
