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


(defvar tmux-cc-delimiter-begin "^```\n"
  "begin of a delimter")
(defvar tmux-cc-delimiter-end "^```\n"
  "end of a delimter")

(defvar tmux-cc-target-window "1")

(defvar tmux-cc-mimic-delay nil
  "To mimic human typing. in milleseconds")

(defun tmux-cc--convert-keys(strings)
  (seq-map #'(lambda(c) (format "%x" c)) strings))

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

(defun tmux-cc-send-keys (strings)
  (cond
   ((file-remote-p (buffer-file-name))
    (let ((ret (progn
                 (tmux-cc--maybe-start-shell-process)
                 (process-send-string
                  tmux-cc--shell-process
                  (concat (mapconcat #'identity
                                     `("tmux" "send-keys" "-t"
                                       ,(shell-quote-argument
                                         tmux-cc-target-window) "-H"
                                       ,@(tmux-cc--convert-keys strings))
                                     " ") "\n")))))
      (message "call return %S" ret)))
   (tmux-cc-mimic-delay
    (dolist (c (tmux-cc--convert-keys strings))
      (call-process "tmux" nil "*tmux cc*" t
                    "send-keys" "-t" tmux-cc-target-window "-H"  c)
      (sleep-for 0 tmux-cc-mimic-delay)))
   (:else
    (let ((ret (apply #'call-process `("tmux" nil "*tmux cc*" t
                                       "send-keys" "-t" ,tmux-cc-target-window "-H" ,@(tmux-cc--convert-keys
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

(defun tmux-cc-shell-command (command)
  (interactive
   (list (read-shell-command
          "Tmux CC Shell command: "
          nil nil
          (let ((filename (cond (buffer-file-name)
                                ((eq major-mode 'dired-mode) (dired-get-filename nil t)))))
            (and filename
                 (file-relative-name
                  filename))))))
  (tmux-cc-send-keys
   (concat ;; "cd "
    ;;(shell-quote-argument
;;            (directory-file-name default-directory))
  ;;         "\n"
           command
           "\n")))



(provide 'tmux-cc)
;;; tmux-cc.el ends here
