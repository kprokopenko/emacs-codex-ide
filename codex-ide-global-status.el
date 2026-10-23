;;; codex-ide-global-status.el --- Global session indicators -*- lexical-binding: t; -*-

;;; Commentary:

;; Optional mode-line counts and navigation across live Codex chats.

;;; Code:

(require 'seq)
(require 'codex-ide-core)
(require 'codex-ide-approvals-data)
(require 'codex-ide-window)

(defface codex-ide-global-status-idle-face
  '((t :weight bold))
  "Face for the global idle chat count."
  :group 'codex-ide)

(defvar codex-ide-global-status--origin nil
  "Marker saved before visiting a request or idle chat.")

(defvar codex-ide-global-status--target nil
  "Chat buffer visited from `codex-ide-global-status--origin'.")

(defconst codex-ide-global-status--mode-line
  '(:eval (codex-ide-global-status--indicator))
  "Entry installed in `global-mode-string' while global status is enabled.")

(defun codex-ide-global-status--sessions ()
  "Return live, non-query sessions with live chat buffers."
  (seq-filter (lambda (session)
                (and (codex-ide--live-session-p session)
                     (not (codex-ide--query-only-session-p session))
                     (buffer-live-p (codex-ide-session-buffer session))))
              codex-ide--sessions))

(defun codex-ide-global-status--pending-requests ()
  "Return (SESSION . REQUEST) pairs for unresolved requests."
  (let (requests)
    (dolist (session (codex-ide-global-status--sessions))
      (dolist (request (codex-ide-approvals-data-unresolved-list session))
        (push (cons session request) requests)))
    requests))

(defun codex-ide-global-status--idle-sessions ()
  "Return live chats whose current status is idle."
  (seq-filter (lambda (session)
                (equal (codex-ide-session-status session) "idle"))
              (codex-ide-global-status--sessions)))

(defun codex-ide-global-status--clear-origin ()
  "Release the saved return marker and chat buffer."
  (when (markerp codex-ide-global-status--origin)
    (set-marker codex-ide-global-status--origin nil))
  (setq codex-ide-global-status--origin nil
        codex-ide-global-status--target nil))

(defun codex-ide-global-status--return ()
  "Return to the saved location when in the visited chat, and report success."
  (when (and codex-ide-global-status--origin
             (eq (current-buffer) codex-ide-global-status--target))
    (let ((buffer (marker-buffer codex-ide-global-status--origin))
          (position (marker-position codex-ide-global-status--origin)))
      (codex-ide-global-status--clear-origin)
      (unless (buffer-live-p buffer)
        (user-error "The original buffer was killed"))
      (codex-ide-display-buffer buffer codex-ide-display-buffer-pop-up-action
				:select t)
      (goto-char position))
    t))

(defun codex-ide-global-status--visit (buffer &optional position)
  "Visit BUFFER at POSITION or its end, saving the current location."
  (let ((origin (unless (eq (current-buffer) buffer) (point-marker))))
    (codex-ide-display-buffer buffer codex-ide-display-buffer-pop-up-action
                              :select t)
    (codex-ide-global-status--clear-origin)
    (setq codex-ide-global-status--origin origin
          codex-ide-global-status--target (and origin buffer))
    (goto-char (if (and (markerp position)
                        (eq (marker-buffer position) buffer))
                   position
                 (point-max)))))

(defun codex-ide-global-status--visit-request ()
  "Visit the oldest unresolved request across live chats."
  (let* ((pending (car (sort (codex-ide-global-status--pending-requests)
                             (lambda (left right)
                               (time-less-p (plist-get (cdr left) :created-at)
                                            (plist-get (cdr right) :created-at))))))
         (session (car pending))
         (request (cdr pending)))
    (unless pending
      (user-error "No pending Codex requests"))
    (codex-ide-global-status--visit
     (codex-ide-session-buffer session)
     (codex-ide-approvals-data-view-get request :start-marker))))

(defun codex-ide-global-status--visit-idle ()
  "Visit an idle chat, asking which one when several are available."
  (let* ((buffers (mapcar #'codex-ide-session-buffer
                          (codex-ide-global-status--idle-sessions)))
         (buffer (pcase buffers
                   ('nil (user-error "No idle Codex chats"))
                   (`(,only) only)
                   (_ (get-buffer
                       (completing-read "Idle Codex chat: "
                                        (mapcar #'buffer-name buffers) nil t))))))
    (codex-ide-global-status--visit buffer)))

;;;###autoload
(defun codex-ide-review-request ()
  "Visit the oldest pending Codex request, or return to the previous buffer."
  (interactive)
  (unless (codex-ide-global-status--return)
    (codex-ide-global-status--visit-request)))

;;;###autoload
(defun codex-ide-visit-idle ()
  "Visit an idle Codex chat, or return to the previous buffer."
  (interactive)
  (unless (codex-ide-global-status--return)
    (codex-ide-global-status--visit-idle)))

;;;###autoload
(defun codex-ide-visit-ready ()
  "Visit a pending request first, otherwise an idle chat, or return.
When already in a chat visited by these commands, restore the previous
buffer and point even if the request was resolved or its status changed."
  (interactive)
  (unless (codex-ide-global-status--return)
    (if (codex-ide-global-status--pending-requests)
        (codex-ide-global-status--visit-request)
      (codex-ide-global-status--visit-idle))))

(defvar codex-ide-global-status--approval-map (make-sparse-keymap)
  "Mouse bindings for the global approval count.")
(define-key codex-ide-global-status--approval-map [mode-line mouse-1]
            #'codex-ide-review-request)

(defvar codex-ide-global-status--idle-map (make-sparse-keymap)
  "Mouse bindings for the global idle count.")
(define-key codex-ide-global-status--idle-map [mode-line mouse-1]
            #'codex-ide-visit-idle)

(defun codex-ide-global-status--indicator ()
  "Return counts for pending requests and idle chats, hiding zero counts."
  (let ((approvals (length (codex-ide-global-status--pending-requests)))
        (idle (length (codex-ide-global-status--idle-sessions))))
    (when (or (> approvals 0) (> idle 0))
      (concat
       (when (> approvals 0)
         (propertize (format " Appr[%d]" approvals)
                     'face 'warning
                     'mouse-face 'mode-line-highlight
                     'help-echo "Click to review a pending Codex request / return"
                     'local-map codex-ide-global-status--approval-map))
       (when (> idle 0)
         (propertize (format " Idle[%d]" idle)
                     'face 'codex-ide-global-status-idle-face
                     'mouse-face 'mode-line-highlight
                     'help-echo "Click to visit an idle Codex chat / return"
                     'local-map codex-ide-global-status--idle-map))))))

(defun codex-ide-global-status--session-event (_event _session _payload)
  "Refresh all mode lines after a session lifecycle event."
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode codex-ide-global-status-mode
  "Show pending request and idle chat counts in every mode line.
Counts cover all live chats across workspaces.  Zero counts are hidden.
Idle includes all chats ready for input, including previously read chats.
Click either count to visit a chat; repeat to return.  This global mode
does not install any keyboard bindings or change notification settings."
  :global t
  :group 'codex-ide
  (if codex-ide-global-status-mode
      (progn
        (add-to-list 'global-mode-string codex-ide-global-status--mode-line t)
        (add-hook 'codex-ide-session-event-hook
                  #'codex-ide-global-status--session-event))
    (setq global-mode-string
          (delete codex-ide-global-status--mode-line global-mode-string))
    (remove-hook 'codex-ide-session-event-hook
                 #'codex-ide-global-status--session-event)
    (codex-ide-global-status--clear-origin))
  (force-mode-line-update t))

(provide 'codex-ide-global-status)
;;; codex-ide-global-status.el ends here
