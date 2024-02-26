;;; tmux-cc.el --- control tmux via emacs            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Chunye Wang

;; Author: Chunye Wang <chunywan@xbjlabdpsvr15>
;; Keywords: emulations, terminals

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'seq)
(require 'shell)


(defvar tmux-cc-delimiter-begin "^```\n"
  "begin of a delimter")
(defvar tmux-cc-delimiter-end "^```\n"
  "end of a delimter")

(defvar tmux-cc-target-window "1")
(defvar tmux-cc-target-host nil)
(defvar tmux-cc-search-for-prompt nil)
(defvar tmux-cc-mimic-delay nil
  "To mimic human typing. in milleseconds")

(defvar tmux-cc-shell-bash-history '()
  "the command history, sync with ~/.bash_history")

(defun tmux-cc--convert-keys(strings)
  (seq-map #'(lambda(c) (format "%x" c))
           (with-temp-buffer
             (insert strings)
             (goto-char (point-min))
             (when tmux-cc-search-for-prompt
               (when (re-search-forward shell-prompt-pattern nil t nil)
                 (replace-match "")))
             (buffer-string))))

(defvar-local tmux-cc--shell-process nil
  "the shell process for tmux cc. `tmux-cc-send-keys` send
  command via this process")

(defun tmux-cc--delete-process-on-exit()
  (when (and tmux-cc--shell-process (process-live-p tmux-cc--shell-process))
    (process-send-string tmux-cc--shell-process "exit\n")
    (sleep-for 1)
    (delete-process tmux-cc--shell-process)))

(defun tmux-cc--maybe-start-shell-process ()
  (unless (and tmux-cc--shell-process (process-live-p tmux-cc--shell-process))
    (setq tmux-cc--shell-process
          (start-file-process "tmux-cc-shell" nil "bash" "-i")))
  (unless (and tmux-cc--shell-process (process-live-p tmux-cc--shell-process))
    (error "cannot start shell for tmux-cc"))
  (add-hook 'kill-buffer-hook #'tmux-cc--delete-process-on-exit t t))

(defvar tmux-cc--cmd-buffer "*tmux cc*")
(defun tmux-cc--tmux-cmd (&rest args)
  (let*  ((buffer (cond
                   ((bufferp tmux-cc--cmd-buffer) tmux-cc--cmd-buffer)
                   ((stringp tmux-cc--cmd-buffer) (get-buffer-create
                                                   tmux-cc--cmd-buffer))
                   (:else (error "%S must be a buffer or string" tmux-cc--cmd-buffer))))
          (apply-args (if tmux-cc-target-host
                          `("ssh" nil ,buffer t
                            ,tmux-cc-target-host
                            "tmux" ,@args)
                        `("tmux" nil ,buffer t ,@args))))
    ;;(message "%S" apply-args)
    (apply 'call-process apply-args)))

(defun tmux-cc-send-keys (strings)
  (cond
   ((and nil :hack) (let ()
            (with-temp-file "/tmp/a.cmd"
              (insert strings))
            (call-process "osascript" nil nil nil "-e"
                        (concat
                         "
tell application \"iTerm\"
	tell current window
		tell current tab
			tell current session
				write contents of file \"/tmp/a.cmd\" newline false
			end tell
		end tell
	end tell
end tell"

                         ))))
   (tmux-cc-mimic-delay
    (dolist (c (tmux-cc--convert-keys strings))
      (tmux-cc--tmux-cmd "send-keys" "-t" tmux-cc-target-window "-H" c)
      (sleep-for 0 (random tmux-cc-mimic-delay))))
   (:else
    (let ((ret (apply #'tmux-cc--tmux-cmd
                      `("send-keys" "-t" ,tmux-cc-target-window "-H" ,@(tmux-cc--convert-keys
                                                                                       strings)))))
      (list 'message "call return %S %S" ret (tmux-cc--convert-keys strings))))))

(defun tmux-cc-send-region(begin end)
  (interactive "r")
  (unless mark-active
    (error "no region is selected"))
  (let ((content (buffer-substring begin end)))
    (tmux-cc-send-keys content)))

(defun tmux-cc--begining-of-line()
  (save-excursion
    (goto-char (line-beginning-position))
    (while (and (eq (char-before) ?\n)
                (eq (char-before (- (point) 1)) ?\\))
      (goto-char (- (point) 1))
      (goto-char (line-beginning-position)))
    (point)))

(defun tmux-cc--end-of-line()
  (save-excursion
    (goto-char (line-end-position))
    (while  (eq (char-before) ?\\)
      (goto-char (+ 1 (point)))
      (goto-char (line-end-position)))
    (min (point-max) (+ 1 (point)))))

(defun tmux-cc-send-current-line ()
  (interactive)
  (tmux-cc-send-keys (buffer-substring (tmux-cc--begining-of-line)
                                       (tmux-cc--end-of-line))))


;; (defun tmux-cc--search-end ()
;;   (save-excursion
;;     (when (search-forward-regexp tmux-cc-delimiter-begin nil t nil)
;;       (match-beginning 0))))

;; (defun tmux-cc--search-begin ()
;;   (save-excursion
;;     (when (search-backward-regexp tmux-cc-delimiter-end nil t nil)
;;       (match-end 0))))

;; (defun tmux-cc-select-block ()
;;   (interactive)
;;   (let ((begin (tmux-cc--search-begin))
;;         (end (tmux-cc--search-end)))
;;     (when (and begin end)
;;       (push-mark begin nil t)
;;       (goto-char end))))

;; (defun tmux-cc-shell-read-bash-history ()
;;   (reverse (split-string
;;             (with-temp-buffer
;;               (insert-file-contents "~/.bash_history")
;;               (buffer-substring-no-properties
;;                (point-min)
;;                (point-max)))
;;             "\n"
;;             t)))
;;;###autoload
;; (defun tmux-cc-shell-command (command)
;;   (interactive
;;    (list (read-shell-command
;;           "Tmux CC Shell command: "
;;           nil nil
;;           ;; (progn (setq tmux-cc-shell-bash-history
;;           ;;                  (tmux-cc-shell-read-bash-history)
;;           ;;                  )
;;           ;;            'tmux-cc-shell-bash-history)
;;           (let ((filename (cond (buffer-file-name)
;;                                 ((eq major-mode 'dired-mode) (dired-get-filename nil t)))))
;;             (and filename
;;                  (file-relative-name
;;                   filename))))))
;;   (tmux-cc-send-keys
;;    (concat ;; "cd "
;;     ;;(shell-quote-argument
;; ;;            (directory-file-name default-directory))
;;   ;;         "\n"
;;            command
;;            "\n")))

;;;###autoload
(defun tmux-cc-set-target-window (target-window)
  (interactive "sSet tmux target window: ")
  (setq tmux-cc-target-window target-window))
;;;###autoload
(defun tmux-cc-get-panel ()
  (interactive ;; "stmux panel (empty for current): "
   )
  (let ((tmux-cc--cmd-buffer (concat " *tmux "
                                (format
                                 "%s@%s"
                                 tmux-cc-target-window
                                 tmux-cc-target-host) " *")))
    (with-current-buffer (get-buffer-create tmux-cc--cmd-buffer)
      (erase-buffer)
      (tmux-cc--tmux-cmd "capture-pane" "-p" ;; "-t" target-panel
                         "-S" "-" "-E" "-")
      (save-excursion
        (goto-char (point-min))
        (replace-regexp-in-region "^[[:blank:]]*\n+\\'" "\n"))
      (switch-to-buffer (current-buffer)))))
;;;
(defun tmux-cc-clear ()
  (interactive)
  (tmux-cc-send-keys "clear\n")
  (tmux-cc--tmux-cmd "clear-history"))
;;;
(defun tmux-cc-run-single-command (command)
  (interactive "scommand: ")
  (tmux-cc-send-keys "clear\n")
  (tmux-cc--tmux-cmd "tmux" "clear-history")
  (tmux-cc-send-keys (concat command "\n"))
  (let ((tmux-cc--cmd-buffer (current-buffer)))
    (tmux-cc--tmux-cmd "capture-pane" "-p" ;; "-t" target-panel
                         "-S" "0" "-E" "-")))

;;;###autoload
(defun tmux-cc-bind-command (command &rest args)
  `(lambda ()
     (interactive)
     (tmux-cc--tmux-cmd ,command ,@args)))

(defun convert-key (key)
  (cond
   ((eq key 'up)
    (list "1b" "5b" "41"))
   ((eq key 'down)
    (list "1b" "5b" "42"))
   ((eq key 'right)
    (list "1b" "5b" "43"))
   ((eq key 'left)
    (list "1b" "5b" "44"))
   ((eq key 'home)
    (list "1b" "5b" "48"))
   ((eq key 'end)
    (list "1b" "5b" "31" "3b" "32" "46"))
   ((eq key 'f6)
    (list "3"))
   ((eq key 'f7)
    (list "7"))
   ((integerp key)
    (let ((ascii (logand key #xff)))
      (if (not (= (logand key #x8000000) 0))
          (list "1b" (format "%x" ascii))
        (list (format "%x" ascii)))))
   (t (error "unknown key %S" key ))))
;;;###autoload
(defun tmux-start-special-key ()
  (interactive)
  (message "tmux special key mode start")
  (let (key)
    (while (not (or (eq (setq key
                              (read-key "sending key stroke to tmux,press s-g
(cmd+g or win+g)
to quit .... "))
                        (aref (kbd "s-g") 0))))
      (condition-case err
          (apply #'tmux-cc--tmux-cmd
                 "send-key" "-t"
                 tmux-cc-target-window
                 "-H" (convert-key key))
        (error (warn "%s" (cdr err))))))
  (message "tmux special key mode done"))

(defun tmux-cc-send-keys-from-a-file (file)
  (interactive "fSend keys from a file")
  (message file)
  (with-temp-buffer
    (insert-file-literally file)
    (goto-char (point-min))
    (while (not (eobp))
      (tmux-cc-send-keys
       (concat (buffer-substring
                (line-beginning-position) (line-end-position))
               "\n"))
      (forward-line 1))))

(defun tmux-cc-send-a-key (key)
  (interactive "kKey:")
  (tmux-cc-send-keys key))

(defvar tmux-cc-key-map
  (let ((map (make-sparse-keymap)))
    ;; These bindings roughly imitate those used by Outline mode.
    (define-key map (kbd "C-b") 'tmux-start-special-key)
    (define-key map (kbd "g") 'tmux-cc-get-panel)
    (define-key map (kbd "C-l") 'tmux-cc-clear)
    (define-key map "%"	      (tmux-cc-bind-command "split-window" "-h"))
    (define-key map "0"	      (tmux-cc-bind-command "select-window" "-t"  ":=0"))
    (define-key map "1"	      (tmux-cc-bind-command "select-window" "-t"  ":=1"))
    (define-key map "2"	      (tmux-cc-bind-command "select-window" "-t"  ":=2"))
    (define-key map "3"	      (tmux-cc-bind-command "select-window" "-t"  ":=3"))
    (define-key map "4"	      (tmux-cc-bind-command "select-window" "-t"  ":=4"))
    (define-key map "5"	      (tmux-cc-bind-command "select-window" "-t"  ":=5"))
    (define-key map "6"	      (tmux-cc-bind-command "select-window" "-t"  ":=6"))
    (define-key map "7"	      (tmux-cc-bind-command "select-window" "-t"  ":=7"))
    (define-key map "8"	      (tmux-cc-bind-command "select-window" "-t"  ":=8"))
    (define-key map "9"	      (tmux-cc-bind-command "select-window" "-t"  ":=9"))
    (define-key map "c"	      (tmux-cc-bind-command "new-window"))
    (define-key map "o"	      (tmux-cc-bind-command "last-pane" "-t" ":.+"))
    (define-key map "l"	      (tmux-cc-bind-command "last-window"))
    (define-key map "z"	      (tmux-cc-bind-command "resize-pane" "-Z"))
    (define-key map "q"	      #'tmux-cc-send-a-key)
    (define-key map (kbd "C-c")	      (tmux-cc-bind-command "send-key"
  "-H" "3"))
    map)
  "Keymap for mimic tmux key bindings")
(provide 'tmux-cc)
;;; tmux-cc.el ends here
