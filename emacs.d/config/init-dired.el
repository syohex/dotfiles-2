;;; this is my config for dired mode
;;; its target is to replace macos as well as other os's default file explorer application

;;; my required packages
;;;(require 'init-util)


;; Open ssh; or open in su(do).
;;
;;  Normally: C-x C-f /path/to/file
;;  Through ssh: C-x C-f /ssh:username@myhost.univ:/path/to/file
;;  Using sudo: C-x C-f /su::/etc/hosts




;;; Dired Omit Mode
;; omit (not show) files begining with . and #
(setq-default dired-omit-mode t
              dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")

;; delete *.tp from omit mode
(setq-default dired-omit-extensions (remove ".tp" dired-omit-extensions))

;;; some required packages for dired
(require 'dired+)

;;; some minor config

(setq dired-recursive-deletes 'always)  ;always recursively delete dir
(setq dired-recursive-copies 'always) ;always recursively copy dir
;; (dired "~/")             ;open home dir when start
(setq dired-dwim-target t)        ;auto guess default dir when copy/move


;;; delete files by moving to the folder emacs in Trash folder
;;; this path is for MacOS users
;;; for other os, just set the correct path of the trash
(setq delete-by-moving-to-trash t)
(tmtxt/in '(darwin)
  (setq trash-directory "~/.Trash/emacs"))
(tmtxt/in '(gnu/linux)
  (setq trash-directory "~/.local/share/Trash/files/emacs"))


;;; mark file and then move the cursor back
;;; different from the built in dired-mark
;;; dired-mark marks a file and then move the cursor to the next file
;;; tmtxt-dired-mark-backward marks a file but then move the cursor to the
;;; previous file
(defun tmtxt/dired-mark-backward ()
  (interactive)
  (call-interactively 'dired-mark)
  (call-interactively 'dired-previous-line) ;remove this line if you want the
                                        ;cursor to stay at the current line
  (call-interactively 'dired-previous-line))


;;; Mac OS
;;; open file/marked files by default program in mac
;;; http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/
(defun tmtxt/dired-do-shell-open ()
  (interactive)
  (save-window-excursion
    (let ((files (dired-get-marked-files nil current-prefix-arg))
          command)
      ;; the open command
      (tmtxt/in '(darwin)
        (setq command "open "))
      (tmtxt/in '(gnu/linux)
        (setq command "xdg-open "))
      (dolist (file files)
        (setq command (concat command (shell-quote-argument file) " ")))
      (message command)
      ;; execute the command
      (async-shell-command command))))


;; unmount disk in dired
;;http://loopkid.net/articles/2008/06/27/force-unmount-on-mac-os-x
(tmtxt/in '(darwin)           ;MacOS
  (defun tmtxt/dired-do-shell-unmount-device ()
    (interactive)
    (save-window-excursion
      (dired-do-async-shell-command
       "diskutil unmount" current-prefix-arg
       (dired-get-marked-files t current-prefix-arg)))))
(tmtxt/in '(gnu/linux)          ;Linux
  (defun tmtxt/dired-do-shell-unmount-device ()
    (interactive)
    (save-window-excursion
      (dired-do-async-shell-command
       "umount" current-prefix-arg
       (dired-get-marked-files t current-prefix-arg)))))


;;; open current directory in Finder (MacOSX)
;;; can apply to other buffer type (not only dired)
;;; in that case, calling this function will cause Finder to open the directory
;;; that contains the current open file in that buffer
(defun tmtxt/dired-open-current-directory ()
  "Open the current directory in Finder"
  (interactive)
  (save-window-excursion
    (tmtxt/in '(darwin)
      (async-shell-command
       "open ."))
    (tmtxt/in '(gnu/linux)
      (async-shell-command
       "xdg-open ."))))

;;; hide details
(require 'dired-details+
  ;; show sym link target
  (setq dired-details-hide-link-targets nil))

;;; directory first by default
;;; on Mac OS, first install coreutils and findutils, which are the gnu version
;;; of some shell program including ls
;;; sudo port install coreutils findutils
;;; (optional) add this to .bashrc or .zshrc file for them to run in shell
;;; export PATH=/opt/local/libexec/gnubin:$PATH
;;; on ubuntu, no need to do so since it's ship with gnu version ones
;; (tmtxt/in '(darwin)
;;   (require 'ls-lisp)
;;   (setq ls-lisp-use-insert-directory-program t)
;;   (setq insert-directory-program "~/bin/macports/libexec/gnubin/ls"))
(require 'dired-sort-map)
(setq dired-listing-switches "--group-directories-first -alh")

;;; open current directory in terminal
(tmtxt/in '(darwin)

  ;; default terminal application path
  (defvar tmtxt/macos-default-terminal-app-path
    "/Applications/Terminal.app" "The default path to terminal application in MacOS")
  (setq-default tmtxt/macos-default-terminal-app-path "/Volumes/tmtxt/Applications/iTerm.app")

  ;; function to open new terminal window at current directory
  (defun tmtxt/open-current-dir-in-terminal ()
    "Open current directory in dired mode in terminal application.
For MacOS only"
    (interactive)

    (shell-command (concat "open -a "
                           (shell-quote-argument tmtxt/macos-default-terminal-app-path)
                           " "
                           (shell-quote-argument (file-truename default-directory))))))

;;; fast renaming for wdired
(require 'wdired)
  (defun tmtxt/mark-file-name-for-rename ()
    "Mark file name on current line except its extension"
    (interactive)

    ;; get the file file name first
    ;; full-name: full file name
    ;; extension: extension of the file
    ;; base-name: file name without extension
    (let ((full-name (file-name-nondirectory (dired-get-filename)))
          extension base-name)

      ;; check if it's a dir or a file
      ;; TODO not use if, use switch case check for symlink
      (if (file-directory-p full-name)
          (progn
            ;; if file name is directory, mark file name should mark the whole
            ;; file name
            (call-interactively 'end-of-line) ;move the end of line
            (backward-char (length full-name)) ;back to the beginning
            (set-mark (point))
            (forward-char (length full-name)))
        (progn
          ;; if current file is a file, mark file name mark only the base name,
          ;; exclude the extension
          (setq extension (file-name-extension full-name))
          (setq base-name (file-name-sans-extension full-name))
          (call-interactively 'end-of-line)
          (backward-char (length full-name))
          (set-mark (point))
          (forward-char (length base-name))))))

  (defun tmtxt/mark-file-name-forward ()
    "Mark file name on the next line"
    (interactive)
    (deactivate-mark)
    (next-line)
    (tmtxt/mark-file-name-for-rename))

  (defun tmtxt/mark-file-name-backward ()
    "Mark file name on the next line"
    (interactive)
    (deactivate-mark)
    (previous-line)
    (tmtxt/mark-file-name-for-rename))

;;; dired-rainbow
;;; different color for different file type
(require 'dired-rainbow)
(defconst dired-audio-files-extensions
  '("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV")
  "Dired Audio files extensions")

(defconst dired-video-files-extensions
  '("vob" "VOB" "mkv" "MKV" "mpe" "mpg" "MPG" "mp4" "MP4" "ts" "TS" "m2ts"
    "M2TS" "avi" "AVI" "mov" "MOV" "wmv" "asf" "m2v" "m4v" "mpeg" "MPEG" "tp" "webm" "WEBM")
  "Dired Video files extensions")

(defconst dired-archive-files-extensions
  '("rar" "zip" "tar" "gz")
  "Dired Archive files extensions")

(defconst dired-image-files-extensions
  '("jpg" "jpeg" "png" "gif")
  "Dired Image files extensions")

(dired-rainbow-define audio "#329EE8" dired-audio-files-extensions)
(dired-rainbow-define video "#B3CCFF" dired-video-files-extensions)
(dired-rainbow-define archive "#F77896" dired-archive-files-extensions)
(dired-rainbow-define image "#E2E8F7" dired-image-files-extensions)

;;; auto revert dired buffer
(setq global-auto-revert-non-file-buffers t)

;;; finally provide the library
(provide 'init-dired)
