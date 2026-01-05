;;; git-sync.el -*- lexical-binding: t; -*-

;; Automatically add, commit, and push when files change.

(defvar autocommit-dir-set '(org-directory)
  "Set of directories for which there is a pending timer job")

(defun autocommit-schedule-commit (dn)
  "Schedule an autocommit (and push) if one is not already scheduled for the given dir."
  (if (null (member dn autocommit-dir-set))
      (progn
        (run-with-idle-timer
         10 nil
         (lambda (dn)
           (setq autocommit-dir-set (remove dn autocommit-dir-set))
           (message (concat "Committing org files in " dn))
           (shell-command (concat "cd " dn " && git commit -m 'Changes on '" (system-name) " at " (current-time-string)))
           (shell-command (concat "cd " dn " && git push & /usr/bin/true")))
         dn)
        (setq autocommit-dir-set (cons dn autocommit-dir-set)))))

(defun autocommit-after-save-hook ()
  "After-save-hook to 'git add' the modified file and schedule a commit and push in the idle loop."
  (let ((fn (buffer-file-name)))
    (message "git adding %s" fn)
    (shell-command (concat "git add " fn))
    (autocommit-schedule-commit (file-name-directory fn))))

;; Set up the autocommit save hook for the current file.process
(add-hook 'after-save-hook 'autocommit-after-save-hook)
