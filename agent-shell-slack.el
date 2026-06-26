;;; agent-shell-slack.el --- Slack self-DM bridge for Agent Shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff <tim.felgentreff@oracle.com>
;; URL: https://github.com/timfel/agent-shell-slack.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.50.1") (shell-maker "0.90.1"))
;; Keywords: tools, convenience

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Poll Slack self-DMs through a temporary Agent Shell and forward new messages
;; to the single Agent Shell buffer selected when `agent-shell-slack-mode' is
;; enabled.
;;
;; The package intentionally does not know how to call Slack.  It asks the
;; monitor agent to inspect Slack and return a timestamp plus quoted text.

;;; Code:

(require 'agent-shell)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'server)
(require 'subr-x)

(defconst agent-shell-slack--version "0.1.0")

(defvar agent-shell-slack-mode)

(defvar agent-shell-slack--monitor-buffer nil)
(defvar agent-shell-slack--timer nil)
(defvar agent-shell-slack--subscription nil)
(defvar agent-shell-slack--target-buffer nil)
(defvar agent-shell-slack--last-timestamp nil)
(defvar agent-shell-slack--frequency-minutes nil)
(defvar agent-shell-slack--model-id nil)
(defvar agent-shell-slack--model-set-p nil)

(defconst agent-shell-slack--response-regexp
  (rx "timestamp:" (* blank) (group (+ nonl)) (* space)
      "<begin_quote>" (* space)
      (group (*? anything))
      (* space) "</end_quote>")
  "Regexp matching a monitor response block.")

(declare-function agent-shell--config-option-set-model-id "agent-shell"
                  (&rest args))
(declare-function agent-shell--resolve-preferred-config "agent-shell" ())
(declare-function agent-shell-new-temp-shell "agent-shell" (&rest args))
(declare-function agent-shell-queue-request "agent-shell" (prompt))
(declare-function agent-shell-select-config "agent-shell" (&rest args))
(declare-function agent-shell-status "agent-shell" (&rest args))
(declare-function agent-shell-subscribe-to "agent-shell" (&rest args))
(declare-function agent-shell-unsubscribe "agent-shell" (&rest args))
(declare-function agent-shell-insert "agent-shell" (&rest args))

(defun agent-shell-slack--ensure-agent-shell ()
  "Signal unless the current buffer is an Agent Shell buffer."
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer")))

(defun agent-shell-slack--read-frequency ()
  "Read the polling frequency in minutes."
  (let ((raw (read-number "Slack check frequency in minutes: " 5)))
    (unless (and (numberp raw) (> raw 0))
      (user-error "Frequency must be a positive number"))
    raw))

(defun agent-shell-slack--read-model-id ()
  "Read the optional model id for the monitor shell."
  (string-trim (read-string "Slack monitor model id (blank for default): ")))

(defun agent-shell-slack--socket-name ()
  "Return the current Emacs server socket name for prompt instructions."
  (or (and (boundp 'server-name)
           (stringp server-name)
           (not (string-empty-p server-name))
           server-name)
      "server"))

(defun agent-shell-slack--check-prompt ()
  "Return the prompt sent to the Slack monitor agent."
  (concat
   "Look for the latest Slack self-DM authored by the human user. Ignore "
   "messages sent by assistants, agents, bots, apps, or yourself, including "
   "Slack self-DM responses from this bridge. Respond with only the timestamp "
   "and the human-authored message text in this exact format:\n\n"
   "timestamp: <timestamp>\n"
   "<begin_quote>\n"
   "<text verbatim>\n"
   "</end_quote>\n\n"
   "If there is no human-authored self-DM, respond exactly: no-message\n"
   "Do not add commentary before or after the requested format."))

(defun agent-shell-slack--request-prompt (message-text)
  "Return the target agent request for Slack MESSAGE-TEXT."
  (format
   (concat
    "%s\n\n"
    "You can use emacsclient --socket-name=%s to look for other agent shells "
    "and identify them by their name, working directory, or git branch, if "
    "the user asks about other agent shells.\n\n"
    "Besides telling me here, also respond via self-DM on Slack, prefixed with the robot emoji 🤖.")
   message-text
   (agent-shell-slack--socket-name)))

(defun agent-shell-slack--timestamp-key (timestamp)
  "Return a comparable key for TIMESTAMP."
  (if (and timestamp
           (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" timestamp))
      (string-to-number timestamp)
    timestamp))

(defun agent-shell-slack--newer-timestamp-p (timestamp previous)
  "Return non-nil when TIMESTAMP is newer than PREVIOUS."
  (cond
   ((not previous) t)
   ((not timestamp) nil)
   (t
    (let ((current-key (agent-shell-slack--timestamp-key timestamp))
          (previous-key (agent-shell-slack--timestamp-key previous)))
      (cond
       ((and (numberp current-key) (numberp previous-key))
        (> current-key previous-key))
       (t (string> timestamp previous)))))))

(defun agent-shell-slack--parse-response (text)
  "Parse monitor response TEXT into a plist with :timestamp and :text."
  (when (string-match agent-shell-slack--response-regexp text)
    (list :timestamp (string-trim (match-string 1 text))
          :text (match-string 2 text))))

(defun agent-shell-slack--latest-response (buffer)
  "Return the latest parseable Slack monitor response in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((text (buffer-substring-no-properties (point-min) (point-max)))
            (start 0)
            latest)
        (while (string-match agent-shell-slack--response-regexp text start)
          (setq latest (list :timestamp (string-trim (match-string 1 text))
                             :text (match-string 2 text))
                start (match-end 0)))
        latest))))

(defun agent-shell-slack--monitor-ready-p (buffer)
  "Return non-nil when monitor BUFFER can accept input."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'agent-shell-mode)
              (boundp 'agent-shell--state)
              (map-nested-elt agent-shell--state '(:session :id))
              (eq (agent-shell-status :shell-buffer buffer) 'ready)))))

(defun agent-shell-slack--set-monitor-model (target-buffer monitor-buffer)
  "Apply the requested model to MONITOR-BUFFER for TARGET-BUFFER.
Return non-nil when a model change was started."
  (let ((model-id agent-shell-slack--model-id))
    (when (and (buffer-live-p monitor-buffer)
               model-id
               (not (string-empty-p model-id))
               (not agent-shell-slack--model-set-p))
      (setq agent-shell-slack--model-set-p t)
      (with-current-buffer monitor-buffer
        (agent-shell--config-option-set-model-id
         :model-id model-id
         :on-success
         (lambda ()
           (agent-shell-slack--tick target-buffer))
         :on-failure
         (lambda (error-message _raw-message)
           (message "agent-shell-slack could not set monitor model %s: %s"
                    model-id error-message))))
      t)))

(defun agent-shell-slack--tick (target-buffer)
  "Ask the Slack monitor for the latest self-DM for TARGET-BUFFER."
  (when (buffer-live-p target-buffer)
    (when (and agent-shell-slack-mode
               (eq target-buffer agent-shell-slack--target-buffer))
      (let ((monitor agent-shell-slack--monitor-buffer))
        (when (agent-shell-slack--monitor-ready-p monitor)
          (unless (agent-shell-slack--set-monitor-model
                   target-buffer monitor)
            (with-current-buffer monitor
              (agent-shell-insert
               :text (agent-shell-slack--check-prompt)
               :submit t
               :shell-buffer monitor))))))))

(defun agent-shell-slack--handle-monitor-turn-complete (target-buffer monitor-buffer)
  "Handle a completed Slack monitor turn for TARGET-BUFFER and MONITOR-BUFFER."
  (when (and (buffer-live-p target-buffer)
             (buffer-live-p monitor-buffer))
    (when (and agent-shell-slack-mode
               (eq target-buffer agent-shell-slack--target-buffer))
      (when-let* ((response (agent-shell-slack--latest-response
                             monitor-buffer))
                  (timestamp (plist-get response :timestamp))
                  (text (plist-get response :text))
                  ((agent-shell-slack--newer-timestamp-p
                    timestamp agent-shell-slack--last-timestamp)))
        (setq agent-shell-slack--last-timestamp timestamp)
        (with-current-buffer target-buffer
          (agent-shell-queue-request
           (agent-shell-slack--request-prompt text))
          (message "agent-shell-slack queued Slack self-DM %s in %s"
                   timestamp (buffer-name target-buffer)))))))

(defun agent-shell-slack--subscribe-monitor (target-buffer monitor-buffer)
  "Subscribe TARGET-BUFFER to MONITOR-BUFFER completion events."
  (agent-shell-subscribe-to
   :shell-buffer monitor-buffer
   :event 'turn-complete
   :on-event (lambda (_event)
               (agent-shell-slack--handle-monitor-turn-complete
                target-buffer monitor-buffer))))

(defun agent-shell-slack--subscribe-monitor-ready (target-buffer monitor-buffer)
  "Subscribe TARGET-BUFFER to MONITOR-BUFFER prompt readiness."
  (agent-shell-subscribe-to
   :shell-buffer monitor-buffer
   :event 'prompt-ready
   :on-event (lambda (_event)
               (agent-shell-slack--tick target-buffer))))

(defun agent-shell-slack--enable ()
  "Enable global Slack self-DM polling for the current Agent Shell buffer."
  (agent-shell-slack--ensure-agent-shell)
  (when agent-shell-slack--monitor-buffer
    (agent-shell-slack--disable))
  (let* ((config (or (and (fboundp 'agent-shell--resolve-preferred-config)
                          (agent-shell--resolve-preferred-config))
                     (agent-shell-select-config
                      :prompt "Slack monitor agent: ")))
         (model-id (agent-shell-slack--read-model-id))
         (frequency (agent-shell-slack--read-frequency))
         (target-buffer (current-buffer))
         (monitor-buffer (agent-shell-new-temp-shell
                          :config config
                          :no-display t)))
    (setq agent-shell-slack--target-buffer target-buffer
          agent-shell-slack--monitor-buffer monitor-buffer
          agent-shell-slack--frequency-minutes frequency
          agent-shell-slack--model-id model-id
          agent-shell-slack--model-set-p nil
          agent-shell-slack--last-timestamp nil)
    (with-current-buffer monitor-buffer
      (rename-buffer (generate-new-buffer-name
                      (format " *agent-shell slack monitor: %s*"
                              (buffer-name target-buffer)))))
    (setq agent-shell-slack--subscription
          (list
           (agent-shell-slack--subscribe-monitor
            target-buffer monitor-buffer)
           (agent-shell-slack--subscribe-monitor-ready
            target-buffer monitor-buffer)))
    (setq agent-shell-slack--timer
          (run-at-time
           (* 60 frequency) (* 60 frequency)
           #'agent-shell-slack--tick target-buffer))
    (run-at-time 3 nil #'agent-shell-slack--tick target-buffer)
    (message "agent-shell-slack enabled in %s; monitor %s every %s minutes"
             (buffer-name target-buffer) (buffer-name monitor-buffer)
             frequency)))

(defun agent-shell-slack--disable ()
  "Disable global Slack self-DM polling."
  (when (timerp agent-shell-slack--timer)
    (cancel-timer agent-shell-slack--timer))
  (setq agent-shell-slack--timer nil)
  (when (and agent-shell-slack--subscription
             (buffer-live-p agent-shell-slack--monitor-buffer))
    (with-current-buffer agent-shell-slack--monitor-buffer
      (dolist (subscription agent-shell-slack--subscription)
        (agent-shell-unsubscribe :subscription subscription))))
  (setq agent-shell-slack--subscription nil)
  (when (buffer-live-p agent-shell-slack--monitor-buffer)
    (kill-buffer agent-shell-slack--monitor-buffer))
  (setq agent-shell-slack--monitor-buffer nil
        agent-shell-slack--target-buffer nil
        agent-shell-slack--last-timestamp nil
        agent-shell-slack--model-set-p nil
        agent-shell-slack--frequency-minutes nil
        agent-shell-slack--model-id nil))

;;;###autoload
(define-minor-mode agent-shell-slack-mode
  "Forward Slack self-DMs into one Agent Shell buffer.

Enabling this mode prompts for a monitor agent, optional model id, and polling
frequency.  It starts a single temporary Agent Shell that periodically checks
the latest Slack self-DM.  New Slack messages are queued into the Agent Shell
buffer from which the mode was enabled."
  :global t
  :lighter " Slack"
  :group 'agent-shell
  (if agent-shell-slack-mode
      (condition-case err
          (agent-shell-slack--enable)
        (quit
         (setq agent-shell-slack-mode nil))
        (error
         (setq agent-shell-slack-mode nil)
         (signal (car err) (cdr err))))
    (agent-shell-slack--disable)))

;;;###autoload
(defun agent-shell-slack ()
  "Enable Slack self-DM forwarding in the current Agent Shell buffer."
  (interactive)
  (agent-shell-slack-mode 1))

;;;###autoload
(defun agent-shell-slack-version ()
  "Show `agent-shell-slack' version."
  (interactive)
  (message "agent-shell-slack v%s" agent-shell-slack--version))

(provide 'agent-shell-slack)

;;; agent-shell-slack.el ends here
