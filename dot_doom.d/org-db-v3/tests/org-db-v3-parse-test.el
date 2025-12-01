;;; org-db-v3-parse-test.el --- Tests for org parsing -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-db-v3-parse)

(ert-deftest org-db-v3-test-parse-headlines ()
  "Test parsing headlines to JSON structure."
  (with-temp-buffer
    (insert "* TODO Test Heading :tag1:tag2:\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: test-id\n")
    (insert ":END:\n")
    (insert "Content here.\n")
    (org-mode)

    (let* ((parse-tree (org-element-parse-buffer))
           (headlines (org-db-v3-parse-headlines parse-tree)))

      (should (= (length headlines) 1))
      (let ((hl (car headlines)))
        (should (equal (plist-get hl :title) "Test Heading"))
        (should (equal (plist-get hl :level) 1))
        (should (equal (plist-get hl :todo-keyword) "TODO"))
        (should (equal (plist-get hl :tags) ":tag1:tag2:"))))))

(ert-deftest org-db-v3-test-parse-to-json ()
  "Test parsing buffer to JSON string."
  (with-temp-buffer
    (insert "#+TITLE: Test\n")
    (insert "* Heading\n")
    (insert "[[https://example.com][link]]\n")
    (org-mode)

    (let* ((json-str (org-db-v3-parse-buffer-to-json))
           (data (json-read-from-string json-str)))

      (should (assoc 'headlines data))
      (should (assoc 'links data))
      (should (assoc 'keywords data)))))

(ert-deftest org-db-v3-test-parse-sample-file ()
  "Test parsing sample org file with all features."
  (let ((sample-file (expand-file-name "tests/fixtures/sample.org")))
    (with-temp-buffer
      (insert-file-contents sample-file)
      (org-mode)

      (let* ((parse-tree (org-element-parse-buffer))
             (headlines (org-db-v3-parse-headlines parse-tree))
             (links (org-db-v3-parse-links parse-tree))
             (keywords (org-db-v3-parse-keywords parse-tree))
             (src-blocks (org-db-v3-parse-src-blocks parse-tree)))

        ;; Check headlines
        (should (>= (length headlines) 2))
        (let ((first-hl (car headlines)))
          (should (equal (plist-get first-hl :title) "First Heading"))
          (should (equal (plist-get first-hl :todo-keyword) "TODO"))
          (should (equal (plist-get first-hl :level) 1)))

        ;; Check links
        (should (>= (length links) 1))
        (let ((first-link (car links)))
          (should (equal (plist-get first-link :type) "https"))
          (should (equal (plist-get first-link :path) "//example.com")))

        ;; Check keywords
        (should (>= (length keywords) 3))

        ;; Check src blocks
        (should (>= (length src-blocks) 1))
        (let ((first-src (car src-blocks)))
          (should (equal (plist-get first-src :language) "python")))))))

(ert-deftest org-db-v3-test-parse-links ()
  "Test parsing links from org buffer."
  (with-temp-buffer
    (insert "[[https://example.com][Example Link]]\n")
    (insert "[[file:~/test.org][Local File]]\n")
    (org-mode)

    (let* ((parse-tree (org-element-parse-buffer))
           (links (org-db-v3-parse-links parse-tree)))

      (should (= (length links) 2))
      (let ((link1 (car links)))
        (should (equal (plist-get link1 :type) "https"))
        (should (equal (plist-get link1 :description) "Example Link"))))))

(ert-deftest org-db-v3-test-parse-keywords ()
  "Test parsing keywords from org buffer."
  (with-temp-buffer
    (insert "#+TITLE: Test Title\n")
    (insert "#+AUTHOR: Test Author\n")
    (insert "#+DATE: 2025-10-11\n")
    (org-mode)

    (let* ((parse-tree (org-element-parse-buffer))
           (keywords (org-db-v3-parse-keywords parse-tree)))

      (should (= (length keywords) 3))
      (let ((title-kw (car keywords)))
        (should (equal (plist-get title-kw :key) "TITLE"))
        (should (equal (plist-get title-kw :value) "Test Title"))))))

(ert-deftest org-db-v3-test-parse-src-blocks ()
  "Test parsing source blocks from org buffer."
  (with-temp-buffer
    (insert "#+begin_src python\n")
    (insert "print('hello')\n")
    (insert "#+end_src\n")
    (insert "\n")
    (insert "#+begin_src elisp\n")
    (insert "(message \"test\")\n")
    (insert "#+end_src\n")
    (org-mode)

    (let* ((parse-tree (org-element-parse-buffer))
           (src-blocks (org-db-v3-parse-src-blocks parse-tree)))

      (should (= (length src-blocks) 2))
      (let ((py-block (car src-blocks)))
        (should (equal (plist-get py-block :language) "python"))
        (should (string-match-p "hello" (plist-get py-block :contents)))))))

;;; org-db-v3-parse-test.el ends here
