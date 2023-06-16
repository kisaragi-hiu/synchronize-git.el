;;; synchronize-git.el --- Synchronize a list of repositories -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Homepage: https://github.com/kisaragi-hiu/synchronize-git.el
;; Version: 0.10.2
;; Package-Requires: ((emacs "24.1") (dash "2.18.1") (s "1.12.0"))
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

;; NOTE: this is pending a rename as it's not specific to Git anymore.

;; I use git as a way to synchronize a bunch of private repositories.
;; As I'm the only user of these repositories, I'd like them to be
;; synchronized so that I can avoid merge conflicts.
;;
;; This previously lived in `kisaragi-extra-functions.el', but I want
;; to manage it with straight.

;;; Code:

(require 's)
(require 'dash)

(defun synchronize-git-kill-buffers ()
  "Kill all repo sync output buffers."
  (interactive)
  (--each (buffer-list)
    (when (eq (buffer-local-value 'major-mode it)
              'synchronize-git-output-mode)
      (kill-buffer it))))

(defconst synchronize-git--type-command-map
  '((git . ("git" "pull"))
    (svn . ("svn" "update"))
    (git-svn . ("git" "svn" "rebase"))))
(ert-deftest synchronize-git--repo-command ()
  (should
   (equal
    (synchronize-git--repo-command "whatever")
    '("git" "pull")))
  (should
   (equal
    (synchronize-git--repo-command '(svn "whatever"))
    '("svn" "update")))
  (should
   (equal
    (synchronize-git--repo-command '(("abc" "def") "whatever"))
    '("abc" "def"))))
(ert-deftest synchronize-git--repo-path ()
  (should
   (equal
    (synchronize-git--repo-path "whatever")
    "whatever"))
  (should
   (equal
    (synchronize-git--repo-path '(abc "whatever"))
    "whatever"))
  (should-error
   (synchronize-git--repo-path 123))
  (should-error
   (synchronize-git--repo-path '("abc")))
  (should-error
   (synchronize-git--repo-path '("abc" 123))))
(defun synchronize-git--validate-repo (repo)
  "Validate REPO is in the right format."
  (cond ((or (stringp repo)
             (not repo))
         repo)
        ((listp repo)
         (let ((type-or-command (car repo))
               (path (cadr repo)))
           (unless (= (length repo) 2)
             (error "REPO %s is of the wrong length" repo))
           (unless (or (stringp path)
                       (not path))
             (error "PATH %s is not a string" path))
           (unless (or (symbolp type-or-command)
                       (and (listp type-or-command)
                            (-all? #'stringp type-or-command)))
             (error "First element of REPO %s is not a symbol type or a command" repo)))
         repo)
        (t (error "REPO %s is neither a string nor a list in the right format" repo))))
(defun synchronize-git--repo-command (repo)
  "Return the command for pulling REPO."
  (let ((repo (synchronize-git--validate-repo repo)))
    (if (stringp repo)
        (alist-get 'git synchronize-git--type-command-map)
      (let ((elem (car repo)))
        (if (symbolp elem)
            (alist-get elem synchronize-git--type-command-map)
          elem)))))
(defun synchronize-git--repo-path (repo)
  "Return the path of REPO."
  (if (stringp repo)
      repo
    (let ((repo (synchronize-git--validate-repo repo)))
      (cadr repo))))

(defvar synchronize-git-default-repos nil
  "A list of repositories to pull from.

Each repository, REPO, has a command and a path.
The command is a list of strings.
The path is a string or nil. If it is nil, REPO is ignored.

REPO can be specified as:
- PATH ::
  act like TYPE is `git'; see below
- (TYPE PATH) where TYPE is a symbol ::
  read `synchronize-git--type-command-map' to get the command
- (COMMAND PATH) where COMMAND is a list ::
  Directly specify the command and the path.")

;;;###autoload
(defun synchronize-git (&rest repos)
  "Pull from all of REPOS.

This is meant to be used interactively.

Run eg. git pull on REPOS, asynchronously. Display status in a
new buffer.

REPOS is `synchronize-git-default-repos' by default."
  (interactive synchronize-git-default-repos)
  (let ((shell-file-name "bash")
        (inhibit-read-only t)
        ;; Remove repos whose path is nil
        (repos (-filter #'synchronize-git--repo-path repos)))
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
      (let* ((path (expand-file-name (synchronize-git--repo-path repo)))
             (command (synchronize-git--repo-command repo))
             (output-buffer (format "*repo sync: %s*" path))
             (default-directory path)
             (process
              (apply #'start-process "sync" output-buffer command)))
        (with-current-buffer (get-buffer-create output-buffer)
          (setq-local major-mode 'synchronize-git-output-mode))
        (with-current-buffer (get-buffer-create "*repo sync*")
          (insert (format "Synchronizing %s...\n" path)))
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
                      (default-directory path)
                      (dirty?
                       (and (member "git" command)
                            (with-temp-buffer
                              (call-process "git" nil '(t nil) nil "status" "--short" "--ignore-submodules")
                              (/= 0 (buffer-size))))))
                 (goto-char (point-min))
                 (while (search-forward (format "Synchronizing %s..." path)
                                        nil t)
                   (cond (dirty?
                          (replace-match (format "Synchronizing %s... has uncommitted changes!" path)
                                         nil t))
                         ((= status 0)
                          (replace-match (format "Synchronizing %s...done" path)
                                         nil t))
                         (t
                          (replace-match (format "Synchronizing %s failed: %s" path status)
                                         nil t)))))))))))
    (when (called-interactively-p 'interactive)
      (display-buffer "*repo sync*"))))

(provide 'synchronize-git)

;;; synchronize-git.el ends here

