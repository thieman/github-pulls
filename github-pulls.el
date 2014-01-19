;;; github-pulls.el --- Standalone GitHub pull requests management

;; Copyright (C) 2014 Travis Thieman

;; GitHub: https://github.com/thieman/github-pulls
;; Author: Travis Thieman <travis.thieman@gmail.com>

;; Package: github-pulls
;; Version: 20140118
;; Package-Requires: ((gh "20131223") (deferred "0.3.1") (string-utils "20131022"))
;; Keywords: github pull request gh pr

;; This code is licensed under the WTFPL.

;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                    Version 2, December 2004

;; Copyright (C) 2014 Travis Thieman <travis.thieman@gmail.com>

;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.

;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

;;  0. You just DO WHAT THE FUCK YOU WANT TO.

(require 'gh-auth)
(require 'gh-api)
(require 'gh-pulls)
(require 'gh-repos)
(require 'gh-orgs)
(require 'gh-issue-comments)
(require 'easymenu)
(require 'deferred)
(require 'string-utils)

(defvar github-pulls-api (gh-api-v3 (gh-oauth-authenticator "default") :async t))
(defvar *github-pulls-state* (make-hash-table :test 'equal))
(defvar *github-pulls-last-buffer* nil)

(defun github-pulls-clear-globals ()
  "Reset all globals to their initial states."
  (setq *github-pulls-last-buffer* nil)
  (clrhash *github-pulls-state*))

(defun github-pulls-inl (text)
  "Insert TEXT, followed by a newline at the current point."
  (insert text) (newline))

(defun github-pulls-safe-next-line ()
  "Move to the next line if possible, otherwise do nothing."
  (if (= (line-end-position) (point-max)) nil (forward-line 1)))

(defun github-pulls-delete-line ()
  "Delete the text in the current line without deleting the newline at the end."
  (let ((start (line-beginning-position))
        (end (progn (github-pulls-safe-next-line) (line-beginning-position))))
    (delete-region start end)))

(defun github-pulls-switch-mode (mode-sym)
  (funcall mode-sym))

(defun github-pulls-keys (hashtable)
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys))

;; mode definitions

(defvar github-pulls-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'github-pulls-quit)
    map))

(defun github-pulls-dispatch-ret ()
  (interactive)
  (cond ((equal major-mode 'github-pulls-menu-mode) (github-pulls-menu-select))))

(defvar github-pulls-menu-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map github-pulls-mode-map)
    map))

(defvar github-pulls-mode-keywords
  '((".*\n=+\n" . font-lock-constant-face)  ;; headings
    ("[0-9]+\: " . font-lock-variable-name-face)  ;; track numbers
    ("\[-+\]" . font-lock-builtin-face)  ;; progress bar
    ("\\[.*/.*\\]" . font-lock-variable-name-face)))  ;; track timer

(define-derived-mode github-pulls-mode special-mode "GitHub Pulls"
  (buffer-disable-undo)
  (setq font-lock-defaults '(github-pulls-mode-keywords))
  (setq truncate-lines t))

(define-derived-mode github-pulls-menu-mode github-pulls-mode "GitHub Pulls")

(easy-menu-define github-pulls-mode-menu github-pulls-mode-map
  "GitHub Pulls menu"
  '("GitHub Pulls"))

;; GitHub API calls

(defmacro github-pulls-api-fn (fn-name gh-api gh-fn &rest gh-fn-arg-names)
  (let ((api (gh-api github-pulls-api)))
    `(defun ,fn-name (,@gh-fn-arg-names)
       (,gh-fn ,api ,@gh-fn-arg-names))))

(github-pulls-api-fn github-pulls-personal-repos
                     gh-repos-api gh-repos-user-list)
(github-pulls-api-fn github-pulls-orgs gh-orgs-api gh-orgs-list)
(github-pulls-api-fn github-pulls-org-repos gh-repos-api gh-repos-org-list org)
(github-pulls-api-fn github-pulls-prs gh-pulls-api gh-pulls-list org repo)
(github-pulls-api-fn github-pulls-pr-comments gh-issue-comments-api
                     gh-issue-comments-list org repo pr-id)
(github-pulls-api-fn github-pulls-pr-comment-get gh-issue-comments-api
                     gh-issue-comments-get org repo comment-id)
(github-pulls-api-fn github-pulls-pr-comment-delete gh-issue-comments-api
                     gh-issue-comments-delete org repo comment-id)

(defun github-pulls-pr-comment-create (org repo comment-id comment-body)
  (let ((api (gh-issue-comments-api github-pulls-api))
        (comment (make-instance 'gh-issue-comments-comment :body comment-body)))
    (gh-issue-comments-new api org repo comment-id comment)))

(defun github-pulls-pr-comment-update (org repo comment-id comment-body)
  (let ((api (gh-issue-comments-api github-pulls-api))
        (comment (make-instance 'gh-issue-comments-comment :body comment-body)))
    (gh-issue-comments-update api org repo comment-id comment)))

;; buffer rendering and management

(defun github-pulls-switch-to-buffer ()
  "Create *github-pulls* buffer if it does not exist.  Go to that buffer
   if not currently on it, otherwise go back to the previous buffer."
  (let ((buf (or (get-buffer "*github-pulls*")
                 (generate-new-buffer "*github-pulls*"))))
    (switch-to-buffer buf)))

(defun github-pulls-buffer-set ()
  "Designate the current buffer as the *github-pulls* buffer."
  (let ((buf (or (get-buffer "*github-pulls*")
                 (generate-new-buffer "*github-pulls*"))))
    (set-buffer buf)))

(defun github-pulls-draw-header ()
  (mapc 'github-pulls-inl '("header" "")))

(defun github-pulls-draw-org-buffer (orgs)
  "Turn the current buffer into a fresh GitHub Pulls org select buffer."
  (let ((inhibit-read-only t)
        (org-names (mapcar (lambda (org) (oref org :login)) orgs)))
    (github-pulls-switch-mode 'github-pulls-menu-mode)
    (erase-buffer)
    (github-pulls-draw-header)
    (goto-char (point-max))
    (mapc 'github-pulls-inl '("Your Orgs" "=========" ""))
    (github-pulls-inl (gh-auth-get-username))
    (mapc 'github-pulls-inl org-names)))

(defun github-pulls-init-org-buffer ()
  (deferred:$
    (deferred:call 'github-pulls-orgs)
    (deferred:nextc it
      (lambda (orgs-resp)
        (github-pulls-draw-org-buffer (oref orgs-resp :data))))))

;; interactive commands

;;;###autoload
(defun github-pulls ()
  "Create a new GitHub Pulls buffer, or switch to it if it already exists.
   If already in the buffer, switch to the previous buffer."
  (interactive)
  (let ((exists (not (equal nil (get-buffer "*github-pulls*")))))
    (if exists
      (if (equal (buffer-name) "*github-pulls*")
        (switch-to-buffer *github-pulls-last-buffer*)
        (progn (setq *github-pulls-last-buffer* (current-buffer))
               (github-pulls-switch-to-buffer)))
      (progn (setq *github-pulls-last-buffer* (current-buffer))
             (github-pulls-switch-to-buffer)
             (github-pulls-init-org-buffer)))))

(defun github-pulls-quit ()
  "Quit out of GitHub Pulls, clearing globals and killing the buffer."
  (interactive)
  (github-pulls-clear-globals)
  (kill-buffer "*github-pulls*"))

;;; github-pulls.el ends here
