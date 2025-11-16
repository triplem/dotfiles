;;; utility.el -*- lexical-binding: t; -*-

;; some date calculations (used for org-journal)
;; mostly copied from https://emacs.stackexchange.com/questions/43984/convert-between-iso-week-and-a-normal-date
(defun triplem/iso-week-to-time(year week day)
  "Convert ISO year, week, day to elisp time value."
  (apply #'encode-time
         (append '(0 0 0)
                 (-select-by-indices
                  '(1 0 2)
                  (calendar-gregorian-from-absolute (calendar-iso-to-absolute
                                                     (list week day year)))))))

(defun triplem/iso-beginning-of-week(time)
  "Convert ISO year, week to elisp time for first day (Monday) of week."
  (let* ((year (string-to-number (format-time-string "%Y" time)))
         (week (string-to-number (format-time-string "%V" time))))
    (triplem/iso-week-to-time year week calendar-week-start-day)))

(defun triplem/iso-end-of-week(time)
  "Convert ISO year, week to elisp time for last day (Sunday) of week."
  (let* ((year (string-to-number (format-time-string "%Y" time)))
         (week (string-to-number (format-time-string "%V" time))))
    (triplem/iso-week-to-time year week (mod (+ calendar-week-start-day 6) 7))))

;; some methods to be able to ask the user for input and to store input to be able to retriev it for later
;; usages
(defvar triplem/input-history nil
  "List of values previously provided by the user.")


(defun triplem/prompt-for-input ()
  "Prompt the user to select a value from a list or input a new value."
  (let* ((default-values '("Private"))
         (all-values (append triplem/input-history default-values))
         (input (completing-read "Choose or enter a value: "
                                 (delete-dups all-values)
                                 nil nil)))
    (unless (member input triplem/input-history)
      (push input triplem/input-history)) ; Store new value
    input))

;; could use savehist as well...
;; decided against it, since then several other stuff is persisted
(defvar triplem/input-history-file-name (concat doom-user-dir "snippets/input-history.el"))

(defun triplem/save-input-history ()
  "Save the input history to a file."
  (with-temp-file triplem/input-history-file-name
    (insert (format "(setq triplem/input-history '%S)" triplem/input-history))))

(defun triplem/load-input-history ()
  "Load the input history from a file."
  (when (file-exists-p triplem/input-history-file-name)
    (load-file triplem/input-history-file-name)))

;; Load the history when Emacs starts
(triplem/load-input-history)

;; store value on disk if emacs closes
(add-hook 'kill-emacs-hook 'triplem/save-input-history)


;; Close the current buffer and delete the file
;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))


(defun triplem/count-workdays (start-date end-date)
 "Count workdays between START-DATE and END-DATE (excluding weekends).
  Returns negative number if end-date is before start-date."
  (let* ((start (org-time-string-to-time start-date))
         (end (org-time-string-to-time end-date))
         (start-days (time-to-days start))
         (end-days (time-to-days end))
         (workdays 0)
         (direction (if (< start-days end-days) 1 -1)))
    (dotimes (i (abs (- end-days start-days)))
      (let* ((current-days (+ start-days (* direction i)))
             (current-date (calendar-gregorian-from-absolute current-days))
             (day-of-week (calendar-day-of-week current-date)))
        ;; Count if not Saturday (6) or Sunday (0)
        (when (and (not (= day-of-week 0)) (not (= day-of-week 6)))
          (setq workdays (1+ workdays)))))
    (* direction workdays)))
