;;; synchronize-git.el --- Synchronize a list of git repositories -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Homepage: https://github.com/kisaragi-hiu/synchronize-git.el
;; Version: 0.9.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience vc


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; I use git as a way to synchronize a bunch of private repositories.
;; As I'm the only user of these repositories, I'd like them to be
;; synchronized so that I can avoid merge conflicts.
;;
;; This previously lived in `kisaragi-extra-functions.el', but I want
;; to manage it with straight.

;;; Code:

(require 's)
(require 'dash)

(defvar synchronize-git-default-repos nil
  "List of folders to synchronize.")

;;;###autoload
(defun synchronize-git (&rest repos)
  "Synchronize REPOS.

This is meant to be used interactively.

Run git pull then git push in REPOS, asynchronously. Display
synchronization status in a new buffer.

REPOS is `synchronize-git-default-repos' by default."
  (interactive synchonize-git-default-repos)
  (let ((shell-file-name "bash")
        (inhibit-read-only t))
    ;; Create the status buffer.
    (with-current-buffer (get-buffer-create "*repo sync*")
      (erase-buffer)
      (special-mode)
      (insert (format "Repo Sync: %s\n\n" (format-time-string "%FT%T%z")))
      ;; Allow `revert-buffer' to work.
      (set (make-local-variable 'revert-buffer-function)
           (lambda (_ignore-auto _noconfirm)
             (apply #'synchronize-git repos))))
    (dolist (repo repos)
      (with-current-buffer (get-buffer-create "*repo sync*")
        (insert (format "Synchronizing %s...\n" repo)))
      (let ((process
             (start-process-shell-command
              "sync" "*repo sync: debug*"
              (format "cd '%s'; git pull && git push" repo))))
        (set-process-filter
         process
         (lambda (process output)
           (with-current-buffer (process-buffer process)
             (insert (s-replace (kbd "C-m") "\n" output)))))
        (set-process-sentinel
         process
         (lambda (process _change)
           ;; don't do anything if `process' hasn't exited
           (-when-let* ((status (and (not (process-live-p process))
                                     (process-exit-status process))))
             (with-current-buffer "*repo sync*"
               (let* ((inhibit-read-only t)
                      (default-directory repo)
                      (dirty? (/= 0 (call-process
                                     "git"
                                     nil nil nil
                                     "diff" "--quiet" "--ignore-submodules"))))
                 (goto-char (point-min))
                 (while (search-forward (format "Synchronizing %s..." repo)
                                        nil t)
                   (cond (dirty?
                          (replace-match (format "Synchronizing %s... has uncommitted changes!" repo)
                                         nil t))
                         ((= status 0)
                          (replace-match (format "Synchronizing %s...done" repo)
                                         nil t))
                         (t
                          (replace-match (format "Synchronizing %s failed: %s" repo status)
                                         nil t)))))))))))
    (when (called-interactively-p 'interactive)
      (display-buffer "*repo sync*"))))

(provide 'synchronize-git)

;;; synchronize-git.el ends here

