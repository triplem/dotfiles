;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; load useful utility methods
(load-file (concat doom-user-dir "utility.el"))

;; load personal varialbes
(load-file (concat doom-user-dir "personal.el"))


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name triplem/full-name
      user-mail-address triplem/alias-email)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Adopt font-size based on screen size - i am working on a hdpi display...
;;(when window-system
;; ui splash screen/ dashboard logo
;; images taken from
;; https://github.com/doomemacs/doomemacs/issues/3382
;; https://github.com/eccentric-j/doom-icon/tree/master/cute-doom
(setq fancy-splash-image (concat doom-user-dir "splash/doom-splash-small.png"))
(setq doom-font (font-spec :family "JetBrains Mono" :size 13))

(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; auto-save
(setq auto-save-default t)

;; open in full-screen on linux (gnome?) and maximized on windoof
;(if (eq system-type 'gnu/linux)
  ; something for linux if true
;  (add-hook 'window-setup-hook #'toggle-frame-fullscreen)
  ; optional something if not
  (add-hook 'window-setup-hook #'toggle-frame-maximized)
;)

;; enable word count in modline for GFM, Markdown and Org files
;; show encoding in modeline
(setq doom-modeline-enable-word-count t
      doom-modeline-buffer-encoding t)

;; show time in emacs modeline
(display-time-mode 1)
(setq display-time-24hr-format t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org-mode/")

(setq triplem/org-capture-inbox-file "inbox.org")
(setq +org-capture-inbox-file (expand-file-name triplem/org-capture-inbox-file org-directory))

;; org-mode
(after! org
  (setq org-startup-folded 'show2levels

        ;; show [...] instead of just three dots
        org-ellipsis " [...] "

        ;; show utf-8 bullets correctly
        ;; org-superstar-headline-bullets-list '("?" "?" "?" "?" "?")
        org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("▸" . "▾") ("▹" . "▿") ("▸" . "▾"))
        
        ;; start calendar on monday
        org-agenda-start-on-weekday nil

        ;; log done state change
        org-log-done t

        ;; export org without section numbering
        org-export-with-section-numbers nil

        org-agenda-span 'week

        ;; personal keywords for my workflow
        org-todo-keywords
          '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANC(c@/!)")
            (sequence "MEET(m)" "|" "DONE(d!)" "CANC(c@/!)"))

        ;; personal coloring of keywords
        org-todo-keyword-faces
          '(("TODO" . (:foreground "DarkOrange" :weight bold))
            ("DONE" . (:foreground "gray" :weight bold))
            ("MEET" . (:foreground "deep sky blue"))
            ("CANC" . (:foreground "green"))
            ("WAIT" . (:foreground "orange" :weight bold)))

        ;; some usefule capture templates
        org-capture-templates
          '(("t" "TOOD" entry (file+headline +org-capture-inbox-file "Inbox")
             "** TODO %^{TITLE}\n"
             :prepend t
             :kill-buffer t)
            ("d" "Deadline" entry (file+headline +org-capture-inbox-file "Inbox")
             "** TODO %^{TITLE}\nDEADLINE: <%(org-read-date)>\n"
             :prepend t
             :kill-buffer t)
            ("s" "Scheduled" entry (file+headline +org-capture-inbox-file "Inbox")
             "** TODO %^{TITLE}\nSCHEDULED: <%(org-read-date)>\n"
             :prepend t
             :kill-buffer t)))

  ;; html-export options
;  (setq org-html-head-extra (concat "<style type=\"type/css\">\n"
;                                    (with-temp-buffer
;                                      (insert-file-contents (concat doom-user-dir "org.css"))
;                                      (buffer-string))
;                                    "</style>"))
  (setq org-html-head-extra (concat "<link rel=\"stylesheet\" href=\"" doom-user-dir "org.css" "\"/>"))

  ;; some additional configs for org journal
  (setq org-journal-file-type 'weekly)
  (setq org-journal-start-on-weekday calendar-week-start-day)
  (setq org-journal-file-format "%Y-%V.org")
  (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"MEET\"|TODO=\"WAIT\"")
  (setq org-journal-enable-agenda-integration t)

  (defun org-journal-file-header-func (time)
    "Custom function to create journal header for weekly org-journal files.
     TIME is the start date of the week."
    (let* ((start-of-week (triplem/iso-beginning-of-week time))
           (end-of-week (triplem/iso-end-of-week time))
           (start-date (format-time-string "%Y-%m-%d" start-of-week))
           (end-date (format-time-string "%Y-%m-%d" end-of-week)))
    (concat
     "#+TITLE: Journal " start-date " - " end-date
     "\n#+CATEGORY: Journal W" (format-time-string "%V/%Y" time)
     "\n#+FILETAGS: journal W" (format-time-string "%V/%Y" time)
     "\n#+STARTUP: overview")))

  (setq org-journal-file-header 'org-journal-file-header-func)

  ;; refile settings -- limit to second level
  (setq org-refile-targets
        '((nil :maxlevel . 2)
          (org-agenda-files :maxlevel . 2))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode 1))

;; enable holidays in org-agenda
(after! org-agenda
  (setq org-agenda-include-diary t
        org-agenda-sorting-strategy '(scheduled-down deadline-down)
        org-agenda-custom-commands
     '(("T" "start Today View"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "0d")
                      (org-deadline-warning-days 0)
                      (org-deadline-past-days 0)
                      (org-scheduled-past-days 0)
                      (org-agenda-prefix-format "%(or (org-entry-get (point) \"PARTICIPANT\") \"\")")
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :scheduled today
                                :tag "regeltasks"
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-agenda-skip-scheduled-if-done t)
                       (org-agenda-skip-scheduled-if-deadline-is-shown t)
                       (org-agenda-prefix-format
                         "%(triplem/org-agenda-prefix)") 
                        (org-super-agenda-groups
                        '((:name "Due Soon"
                                 :deadline future
                                 :scheduled future
                                 :order 7)
                          (:name "Overdue"
                                 :deadline past
                                 :scheduled past
                                 :order 8)
                          (:discard (:not (:todo ("TODO" "MEET" "WAIT"))))
                          (:discard (:date today))
                          (:discard (:scheduled today)))))))))))

;; Save the changed buffer after state changes
(advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)

;; Enable visual line mode
(add-hook! org-mode
  :append
  #'visual-line-mode)

;; include Northrhine-Westphalia holidays in agenda view
;; should not run :after org-agenda, because then the US holidays are shown on first calendar call
;; but the configured holidays only on the second call
(use-package! holidays
  :config
  (require 'german-holidays)
  (setq calendar-holidays holiday-german-NW-holidays))

;;(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(add-hook 'calendar-initial-window-hook #'calendar-mark-holidays)

;; adopt calendar to german names
(setq calendar-date-style 'iso
      calendar-week-start-day 1
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                               "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"])

;; add org-contacts package for birthdays and anniversaries...
(use-package! org-contacts
  :after org-agenda)

;; We need to turn on the pretty-code Doom module to get ligatures working.
;; Unfortunately, that also turns on pretty-code-mode, which I don't want - it's
;; a feature that replaces keywords with symbols à la Vim-conceal, which I find
;; needlessly confusing. Luckily we can shut off just pretty-code-mode by
;; setting this variable.
(setq +pretty-code-enabled-modes nil)

;; latex-mode
(setq +latex-viewers '(pdf-tools))

;; config settings for adoc-mode
;; i am not fully convinced that this theme is really nice, but it is better then the default one
(use-package! adoc-mode
  :custom-face
  (adoc-list-face ((t (:foreground "light gray"))))
  (adoc-meta-face ((t (:foreground "gainsboro"))))
  (adoc-complex-replacement-face ((t (:foreground "plum1" :background nil :box nil))))
  (adoc-reference-face ((t (:foreground "light blue")))))

;; enable org-re-reveal
(use-package! org-re-reveal)

;; enable mail only for "home" system
(if (string-match-p "kena" (system-name))
    (load-file (concat doom-user-dir "email.el")))

;; enable spell-checks for DE as well as EN
(after! ispell
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "de_DE,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_US"))

;; save-as
(defun triplem/save-as (filename)
  (interactive "F")
  (save-restriction (widen) (write-region (point-min) (point-max) filename)))

;; query date with org calendar and return value from temp buffer
;; org-time-stamp-inactive prints and returns this value
;; stolen from https://emacs.stackexchange.com/questions/69009/how-to-get-org-time-stamp-to-return-timestamp-rather-than-inserting
(defun triplem/org-time-stamp-string ()
   (with-temp-buffer
      (org-mode)
      (org-time-stamp nil nil)
      (buffer-substring (point-min) (point-max))))

(defun triplem/org-add-participant-on-wait ()
  "Add a PARTICIPANT property when the TODO state becomes WAIT."
  (when (string= org-state "WAIT")
    (let ((participant (read-string "Participant: ")))
      (org-set-property "PARTICIPANT" participant))))

(add-hook 'org-after-todo-state-change-hook #'triplem/org-add-participant-on-wait)

(defun triplem/org-agenda-prefix ()
  (let ((scheduled (org-get-scheduled-time (point)))
        (participant (org-entry-get (point) "PARTICIPANT" t)))
    (concat (if scheduled (format-time-string "%Y-%m-%d " scheduled) "")
            (or participant "")
            " ")))
