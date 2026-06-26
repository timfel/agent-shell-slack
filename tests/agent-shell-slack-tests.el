;;; agent-shell-slack-tests.el --- Tests for agent-shell-slack -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:

;; Tests for `agent-shell-slack'.

;;; Code:

(require 'ert)
(require 'agent-shell-slack)

(ert-deftest agent-shell-slack-parse-response ()
  (should (equal (agent-shell-slack--parse-response
                  "timestamp: 1712345678.123456\n<begin_quote>\nhello\nworld\n</end_quote>")
                 '(:timestamp "1712345678.123456" :text "hello\nworld"))))

(ert-deftest agent-shell-slack-latest-response-uses-last-block ()
  (with-temp-buffer
    (insert "timestamp: 1\n<begin_quote>\none\n</end_quote>\n")
    (insert "noise\n")
    (insert "timestamp: 2\n<begin_quote>\ntwo\n</end_quote>\n")
    (should (equal (agent-shell-slack--latest-response (current-buffer))
                   '(:timestamp "2" :text "two")))))

(ert-deftest agent-shell-slack-newer-timestamp-p ()
  (should (agent-shell-slack--newer-timestamp-p "2" nil))
  (should (agent-shell-slack--newer-timestamp-p "2.1" "2.0"))
  (should-not (agent-shell-slack--newer-timestamp-p "2.0" "2.1"))
  (should (agent-shell-slack--newer-timestamp-p "b" "a")))

(ert-deftest agent-shell-slack-request-prompt-includes-message-and-socket ()
  (let ((server-name "/tmp/emacs-server"))
    (should (string-match-p
             (regexp-quote "hello verbatim")
             (agent-shell-slack--request-prompt "hello verbatim")))
    (should (string-match-p
             (regexp-quote "emacsclient --socket-name=/tmp/emacs-server")
             (agent-shell-slack--request-prompt "hello verbatim")))
    (should (string-match-p
             (regexp-quote "Besides telling me here, also respond via self-DM on Slack.")
             (agent-shell-slack--request-prompt "hello verbatim")))))

(provide 'agent-shell-slack-tests)

;;; agent-shell-slack-tests.el ends here
