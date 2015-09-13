
(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))


;; Show FIXME/TODO/BUG keywords
(defun show-prog-keywords ()
  "highlight additional keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-string-face t))))


(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun align-to-equals ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
     (align-regexp beg end
                   (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 )
    (next-line)))


;; From prelude
(defun site-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "search-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (site-search ,search-engine-url ,search-engine-prompt)))

(install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(install-search-engine "code"       "http://code.dapps.douban.com/hub/search?q="   "Search Code: ")



;;函数来自Purcell，目的是把一些相互依赖的feature的加载顺序理顺，例如feature A依赖于feature B，则可以写成(after-load 'B 'A)
;; after-load
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-get-project-root-directory))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))



;; elisp version of try...catch...finally
(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))


;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;; String process
(defun string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]*$" "" str))


;; Browse current HTML file
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url-generic (concat "file://" (buffer-file-name))))



;; separate OS-specific stuff
;; Example:
;; (tmtxt/in '(darwin) something here) ;macos system
;; (tmtxt/in '(gnu/linux) something here) ;gnu linux system
(defmacro tmtxt/in (systems &rest body)
  "Run BODY if `system-type' is in the list of SYSTEMS."
  (declare (indent 1))
  `(when (member system-type ,systems)
     ,@body))


;; ====================== global functions ===========================

;; sudo reopen
(defun sudo-reopen-file ()
  (interactive)
  (message (concat "/sudo::" buffer-file-name))
  (let* ((file-name (expand-file-name buffer-file-name))
    (sudo-file-name (concat "/sudo::" file-name)))
    (progn
      (setq buffer-file-name sudo-file-name)
      (rename-buffer sudo-file-name)
      (setq buffer-read-only nil)
      (message (concat "File name set to " sudo-file-name)))))


;; ;; google
;; (defun google ()
;;   "Google the selected region if any, display a query prompt otherwise."
;;   (interactive)
;;   (browse-url
;;    (concat
;;     "https://www.google.com.hk/search?ie=utf-8&oe=utf-8&q="
;;     (url-hexify-string (if mark-active
;;          (buffer-substring (region-beginning) (region-end))
;;          (read-string "Google: "))))))


;; delete back to head vs c-k
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

;; open new line
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
    (do-left-margin (and (bolp) (> (current-left-margin) 0)))
    (loc (point-marker))
    ;; Don't expand an abbrev before point.
    (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
        (if do-left-margin (indent-to (current-left-margin)))
        (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))


;; Buffer management for init-keybindings
(defun kill-current-buffer ()
  "Kills the current buffer"
  (interactive)
  (kill-buffer (buffer-name)))
(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
          (buffer-list))
  (delete-other-windows))


;; (un)comment a region
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))


;; goto some char
 (defun my-go-to-char (n char)
   "Move forward to Nth occurence of CHAR.
 Typing `my-go-to-char-key' again will move forwad to the next Nth
 occurence of CHAR."
   (interactive "p\ncGo to char: ")
   (let ((case-fold-search nil))
     (if (eq n 1)
         (progn                            ; forward
           (search-forward (string char) nil nil n)
           (backward-char)
           (while (equal (read-key)
                         char)
             (forward-char)
             (search-forward (string char) nil nil n)
             (backward-char)))
       (progn                              ; backward
         (search-backward (string char) nil nil )
         (while (equal (read-key)
                       char)
           (search-backward (string char) nil nil )))))
   (setq unread-command-events (list last-input-event)))



;;; from Youtube hack vim from emacs global functions.
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; Functions for working with the X clipboard, which is very handy.
(defun yank-to-x-clipboard (&optional start end)
  (interactive "r")
  ;; Force the command to output everything to the default output buffer and don't show it.
  (shell-command-on-region start end "/usr/bin/xsel --clipboard -i > /dev/null" nil nil nil nil)
  (deactivate-mark))

(defun put-from-x-clipboard ()
  (interactive)
  (insert (shell-command-to-string "xsel --clipboard")))

(defun selective-display-increase ()
  (interactive)
  (set-selective-display
   (if selective-display (+ selective-display 1) 1)))

(defun selective-display-decrease ()
  (interactive)
  (when selective-display
    (set-selective-display
     (if (< (- selective-display 1) 1)
         nil
       (- selective-display 1)))))

(defun my-align-single-equals ()
  "Align on the first single equal sign."
  (interactive)
  (align-regexp
   (region-beginning) (region-end)
   "\\(\\s-*\\)=" 1 1 nil))

;;; Helpers for narrowing.
(defun narrow-and-set-normal ()
  "Narrow to the region and, if in a visual mode, set normal mode."
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (if (string= evil-state "visual")
      (progn (evil-normal-state nil)
             (evil-goto-first-line))))

(defun open-current-line-in-codebase-search ()
  "Go to the current file's current line on the codebase site."
  (interactive)
  (let* ((line-num (number-to-string (line-number-at-pos)))
         (file-path (replace-regexp-in-string (project-root) "" (buffer-file-name)))
         (args (concat "http://dox.wayfair.com/source/xref/php/" file-path "#" line-num)))
    (call-process "xdg-open" nil nil nil args)))

;;; From http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list (read-string (format "Wikipedia (%s): " term) nil nil term))))
  (w3m-browse-url (concat
               "http://en.m.wikipedia.org/w/index.php?search="
               search-term)))


(provide 'init-util)
