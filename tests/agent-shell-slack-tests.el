;;; agent-shell-slack-tests.el --- Tests for agent-shell-slack -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:

;; Tests for `agent-shell-slack'.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Allow byte compilation when Emacs only adds this tests directory to `load-path'.
(eval-and-compile
  (let ((package-root
         (locate-dominating-file default-directory "agent-shell-slack.el")))
    (when package-root
      (add-to-list 'load-path package-root))))

(require 'agent-shell-slack)

(ert-deftest agent-shell-slack-parse-response ()
  (should (equal (agent-shell-slack--parse-response
                  "timestamp: 1712345678.123456\n<begin_quote>\nhello\nworld\n</end_quote>")
                 '(:timestamp "1712345678.123456" :text "hello\nworld"))))

(ert-deftest agent-shell-slack-message-text-usable-p ()
  (should (agent-shell-slack--message-text-usable-p "hello"))
  (should-not (agent-shell-slack--message-text-usable-p ""))
  (should-not (agent-shell-slack--message-text-usable-p "\n  \n")))

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
             (regexp-quote "also respond via self-DM on Slack")
             (agent-shell-slack--request-prompt "hello verbatim")))))

(ert-deftest agent-shell-slack-check-prompt-ignores-agent-responses ()
  (let ((prompt (agent-shell-slack--check-prompt)))
    (should (string-match-p
             (regexp-quote "authored by the human user")
             prompt))
    (should (string-match-p
             (regexp-quote "Ignore messages sent by assistants, agents, bots, apps, or yourself")
             prompt))
    (should (string-match-p
             (regexp-quote "no-message")
             prompt))))

(ert-deftest agent-shell-slack-state-is-global ()
  (with-temp-buffer
    (should-not (local-variable-p 'agent-shell-slack--shell-buffer))
    (should-not (local-variable-p 'agent-shell-slack--monitor-buffer))
    (should-not (local-variable-p 'agent-shell-slack--timer))
    (should-not (local-variable-p 'agent-shell-slack--monitor-prompt-ready))))

(ert-deftest agent-shell-slack-tick-waits-for-prompt-ready ()
  (let ((agent-shell-slack-mode t)
        (agent-shell-slack--shell-buffer nil)
        (agent-shell-slack--monitor-buffer nil)
        (agent-shell-slack--monitor-prompt-ready nil)
        queue-called)
    (cl-letf (((symbol-function 'agent-shell-slack--monitor-ready-p)
               (lambda (_buffer) t))
              ((symbol-function 'agent-shell-slack--set-monitor-model)
               (lambda (_target _monitor) nil))
              ((symbol-function 'agent-shell-queue-request)
               (lambda (_prompt)
                 (setq queue-called t))))
      (with-temp-buffer
        (setq agent-shell-slack--shell-buffer (current-buffer)
              agent-shell-slack--monitor-buffer (current-buffer))
        (agent-shell-slack--tick (current-buffer)))
      (should-not queue-called))))

(ert-deftest agent-shell-slack-tick-queues-monitor-request ()
  (let ((agent-shell-slack-mode t)
        (agent-shell-slack--shell-buffer nil)
        (agent-shell-slack--monitor-buffer nil)
        (agent-shell-slack--monitor-prompt-ready t)
        queued-prompt)
    (cl-letf (((symbol-function 'agent-shell-slack--monitor-ready-p)
               (lambda (_buffer) t))
              ((symbol-function 'agent-shell-slack--set-monitor-model)
               (lambda (_target _monitor) nil))
              ((symbol-function 'agent-shell-queue-request)
               (lambda (prompt)
                 (setq queued-prompt prompt))))
      (with-temp-buffer
        (setq agent-shell-slack--shell-buffer (current-buffer)
              agent-shell-slack--monitor-buffer (current-buffer))
        (agent-shell-slack--tick (current-buffer)))
      (should (equal queued-prompt
                     (agent-shell-slack--check-prompt))))))

(ert-deftest agent-shell-slack-handle-monitor-turn-complete-ignores-empty-message ()
  (let ((agent-shell-slack-mode t)
        (agent-shell-slack--shell-buffer nil)
        (agent-shell-slack--monitor-buffer nil)
        (agent-shell-slack--last-timestamp nil)
        queue-called)
    (cl-letf (((symbol-function 'agent-shell-slack--latest-response)
               (lambda (_buffer)
                 '(:timestamp "3" :text "\n \n")))
              ((symbol-function 'agent-shell-queue-request)
               (lambda (_prompt)
                 (setq queue-called t))))
      (with-temp-buffer
        (setq agent-shell-slack--shell-buffer (current-buffer)
              agent-shell-slack--monitor-buffer (current-buffer))
        (agent-shell-slack--handle-monitor-turn-complete
         (current-buffer)))
      (should-not queue-called)
      (should-not agent-shell-slack--last-timestamp))))

(ert-deftest agent-shell-slack-handle-monitor-turn-complete-queues-in-shell-buffer ()
  (let ((agent-shell-slack-mode t)
        (agent-shell-slack--shell-buffer nil)
        (agent-shell-slack--monitor-buffer nil)
        (agent-shell-slack--last-timestamp nil)
        queue-buffer
        queue-prompt)
    (cl-letf (((symbol-function 'agent-shell-slack--latest-response)
               (lambda (_buffer)
                 '(:timestamp "4" :text "hello from slack")))
              ((symbol-function 'agent-shell-queue-request)
               (lambda (prompt)
                 (setq queue-buffer (current-buffer)
                       queue-prompt prompt))))
      (with-temp-buffer
        (setq agent-shell-slack--shell-buffer (current-buffer)
              agent-shell-slack--monitor-buffer (current-buffer))
        (agent-shell-slack--handle-monitor-turn-complete (current-buffer))
        (should (eq queue-buffer (current-buffer)))
        (should (equal queue-prompt
                       (agent-shell-slack--request-prompt "hello from slack")))))
    (should (equal agent-shell-slack--last-timestamp "4"))))

(ert-deftest agent-shell-slack-enable-does-not-require-agent-shell-buffer ()
  (let ((agent-shell-slack--monitor-buffer nil)
        (agent-shell-slack--shell-buffer nil)
        (agent-shell-slack--subscription nil)
        (agent-shell-slack--timer nil)
        (agent-shell-slack--last-timestamp nil)
        (created-buffer (generate-new-buffer " *agent-shell-slack test*"))
        subscriptions
        timers
        message-output)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--resolve-preferred-config)
                   (lambda () '(:buffer-name . "Codex")))
                  ((symbol-function 'agent-shell-slack--read-model-id)
                   (lambda () ""))
                  ((symbol-function 'agent-shell-slack--read-frequency)
                   (lambda () 5))
                  ((symbol-function 'agent-shell-new-temp-shell)
                   (lambda (&rest _args) created-buffer))
                  ((symbol-function 'agent-shell-slack--subscribe-monitor)
                   (lambda (shell-buffer)
                     (push (list :turn-complete shell-buffer) subscriptions)
                     :turn-complete-sub))
                  ((symbol-function 'agent-shell-slack--subscribe-monitor-ready)
                   (lambda (shell-buffer)
                     (push (list :prompt-ready shell-buffer) subscriptions)
                     :prompt-ready-sub))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args)
                     (push args timers)
                     :fake-timer))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-output (apply #'format fmt args)))))
          (with-temp-buffer
            (fundamental-mode)
            (agent-shell-slack--enable))
          (should (eq agent-shell-slack--shell-buffer created-buffer))
          (should (eq agent-shell-slack--monitor-buffer created-buffer))
          (should (equal agent-shell-slack--subscription
                         '(:turn-complete-sub :prompt-ready-sub)))
          (should (= (length subscriptions) 2))
          (should (= (length timers) 2))
          (should (string-match-p
                   (regexp-quote (buffer-name created-buffer))
                   message-output)))
      (when (buffer-live-p created-buffer)
        (kill-buffer created-buffer)))))

(provide 'agent-shell-slack-tests)

;;; agent-shell-slack-tests.el ends here
