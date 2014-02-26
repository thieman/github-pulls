;;; github-pulls.el --- Standalone GitHub pull requests management

;; Copyright (C) 2014 Travis Thieman

;; GitHub: https://github.com/thieman/github-pulls
;; Author: Travis Thieman <travis.thieman@gmail.com>

;; Package: github-pulls
;; Version: 20140118
;; Package-Requires: ((gh "20140121") (deferred "0.3.1") (string-utils "20131022") (s "20131223"))
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
(require 's)
(require 'string-utils)

(defvar github-pulls-api (gh-api-v3 (gh-oauth-authenticator "default")))
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
    (define-key map (kbd ".") 'github-pulls-go-back)
    (define-key map (kbd "q") 'github-pulls-quit)
    (define-key map (kbd "RET") 'github-pulls-dispatch-ret)
    map))

(defun github-pulls-dispatch-ret ()
  (interactive)
  (cond ((equal major-mode 'github-pulls-menu-mode) (github-pulls-menu-select))
        ((equal major-mode 'github-pulls-issue-mode) nil)))

(defvar github-pulls-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map github-pulls-mode-map)
    map))

(defvar github-pulls-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map github-pulls-mode-map)
    map))

(defvar github-pulls-mode-keywords
  '(("^>.*$" . font-lock-constant-face)
    (".*\n=+\n" . font-lock-constant-face)  ;; headings
    ("[0-9]+\: " . font-lock-variable-name-face)  ;; track numbers
    ("\[-+\]" . font-lock-builtin-face)  ;; progress bar
    ("\\[.*/.*\\]" . font-lock-variable-name-face)))  ;; track timer

(define-derived-mode github-pulls-mode special-mode "GitHub Pulls"
  (buffer-disable-undo)
  (setq font-lock-defaults '(github-pulls-mode-keywords))
  (setq truncate-lines t))

(define-derived-mode github-pulls-menu-mode github-pulls-mode "GitHub Pulls Navigation")

(define-derived-mode github-pulls-issue-mode github-pulls-mode "GitHub Pulls PR")

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
(github-pulls-api-fn github-pulls-pr gh-pulls-api gh-pulls-get org repo pr-id)
(github-pulls-api-fn github-pulls-pr-comments gh-issue-comments-api
                     gh-issue-comments-list org repo pr-id)
(github-pulls-api-fn github-pulls-pr-comment-get gh-issue-comments-api
                     gh-issue-comments-get org repo comment-id)
(github-pulls-api-fn github-pulls-pr-comment-delete gh-issue-comments-api
                     gh-issue-comments-delete org repo comment-id)

(gh-orgs-list (gh-orgs-api github-pulls-api))

(defun github-pulls-pr-comment-create (org repo comment-id comment-body)
  (let ((api (gh-issue-comments-api github-pulls-api))
        (comment (make-instance 'gh-issue-comments-comment :body comment-body)))
    (gh-issue-comments-new api org repo comment-id comment)))

(defun github-pulls-pr-comment-update (org repo comment-id comment-body)
  (let ((api (gh-issue-comments-api github-pulls-api))
        (comment (make-instance 'gh-issue-comments-comment :body comment-body)))
    (gh-issue-comments-update api org repo comment-id comment)))

;; data access

(defun github-pulls-nav-state ()
  (let ((nav-state (gethash "nav" *github-pulls-state*)))
    (if nav-state
      nav-state
      (progn (puthash "nav" (make-hash-table :test 'equal) *github-pulls-state*)
             (gethash "nav" *github-pulls-state*)))))

(defun github-pulls-current-org ()
  (gethash "org" (github-pulls-nav-state)))

(defun github-pulls-current-repo ()
  (gethash "repo" (github-pulls-nav-state)))

(defun github-pulls-current-issue ()
  (gethash "issue" (github-pulls-nav-state)))

(defun github-pulls-set-current-org (org)
  (puthash "org" org (github-pulls-nav-state)))

(defun github-pulls-set-current-repo (repo)
  (puthash "repo" repo (github-pulls-nav-state)))

(defun github-pulls-set-current-issue (issue)
  (puthash "issue" issue (github-pulls-nav-state)))

(defun github-pulls-nav-level ()
  (cond ((github-pulls-current-issue) "issue-comments")
        ((github-pulls-current-repo) "issue")
        ((github-pulls-current-org) "repo")
        (t "org")))

(defun github-pulls-store-menu-items (items)
  (puthash "current-menu" items *github-pulls-state*))

(defun github-pulls-menu-item (number)
  (elt (gethash "current-menu" *github-pulls-state*) number))

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
  (insert "> ")
  (when (github-pulls-current-org) (insert (github-pulls-current-org) " > "))
  (when (github-pulls-current-repo) (insert (github-pulls-current-repo) " > "))
  (when (github-pulls-current-issue) (insert (format "PR #%d" (github-pulls-current-issue))))
  (newline)
  (newline))

(defun github-pulls-draw-menu-listing (items)
  (let ((formatter (github-pulls-listing-format-string items)))
    (mapc
     (lambda (number)
       (insert (format formatter (+ 1 number) (elt items number)))
       (newline))
     (number-sequence 0 (- (length items) 1)))))

(defun github-pulls-listing-format-string (items)
  "Return listing string formatter for all elements in ITEMS."
  (let ((magnitude (length (number-to-string (length items)))))
    (concat "%0" (number-to-string magnitude) "d: %s")))

(defun github-pulls-prepare-menu-buffer ()
  (github-pulls-switch-mode 'github-pulls-menu-mode)
  (visual-line-mode)
  (erase-buffer)
  (github-pulls-draw-header)
  (goto-char (point-max)))

(defun github-pulls-prepare-issue-buffer ()
  (github-pulls-switch-mode 'github-pulls-issue-mode)
  (visual-line-mode)
  (erase-buffer)
  (github-pulls-draw-header)
  (goto-char (point-max)))

(defun github-pulls-draw-org-buffer (orgs)
  "Turn the current buffer into a fresh GitHub Pulls org select buffer."
  (let* ((inhibit-read-only t)
         (org-names (mapcar (lambda (org) (oref org :login)) orgs))
         (org-names-with-self (cons (gh-auth-get-username) org-names)))
    (github-pulls-prepare-menu-buffer)
    (mapc 'github-pulls-inl '("Your Orgs" "=========" ""))
    (github-pulls-store-menu-items org-names-with-self)
    (github-pulls-draw-menu-listing org-names-with-self)))

(defun github-pulls-draw-repo-buffer (repos)
  "Turn the current buffer into a fresh GitHub Pulls repo select buffer."
  (let ((inhibit-read-only t)
        (repo-names (mapcar (lambda (repo) (oref repo :name)) repos))
        (header-string (format "%s Repos" (github-pulls-current-org))))
    (github-pulls-prepare-menu-buffer)
    (mapc 'github-pulls-inl (list header-string (string-utils-string-repeat "=" (length header-string)) ""))
    (github-pulls-store-menu-items repo-names)
    (github-pulls-draw-menu-listing repo-names)))

(defun github-pulls-draw-issue-buffer (issues)
  "Turn the current buffer into a fresh GitHub Pulls issue select buffer."
  (let ((inhibit-read-only t)
        (issue-ids (mapcar (lambda (issue) (oref issue :number)) issues))
        (issue-titles (mapcar
                       (lambda (issue) (format "%s: %s (Issue #%d)"
                                               (oref (oref issue :user) :login)
                                               (oref issue :title)
                                               (oref issue :number))) issues))
        (header-string (format "%s Open Pull Requests" (github-pulls-current-repo))))
    (github-pulls-prepare-menu-buffer)
    (mapc 'github-pulls-inl (list header-string (string-utils-string-repeat "=" (length header-string)) ""))
    (github-pulls-store-menu-items issue-ids)
    (github-pulls-draw-menu-listing issue-titles)))

(defun github-pulls-draw-issue-comments-buffer (comments)
  "Turn the current buffer into a fresh GitHub Pulls issue comments buffer."
  (let ((inhibit-read-only t))
    (github-pulls-prepare-issue-buffer)
    (github-pulls-draw-current-issue-header)
    (mapc 'github-pulls-draw-issue-comment comments)))

(defun github-pulls-draw-current-issue-header ()
  (let* ((issue (gethash "issue-data" *github-pulls-state*))
         (header-string (format "%s: %s" (oref (oref issue :user) :login) (oref issue :title))))
    (mapc 'github-pulls-inl (list header-string (string-utils-string-repeat "=" (length header-string)) ""))
    (insert (oref issue :body))
    (newline)))

(defun github-pulls-draw-issue-comment (comment) nil)

(defun github-pulls-init-org-buffer ()
  (deferred:$
    (deferred:call 'github-pulls-orgs)
    (deferred:nextc it
      (lambda (orgs-resp)
        (github-pulls-draw-org-buffer (oref orgs-resp :data))))))

(defun github-pulls-init-repo-buffer ()
  (deferred:$
    (deferred:call
      (lambda ()
        (if (equal (github-pulls-current-org) (gh-auth-get-username))
          (github-pulls-personal-repos)
          (github-pulls-org-repos (github-pulls-current-org)))))
    (deferred:nextc it
      (lambda (repos-resp)
        (github-pulls-draw-repo-buffer (oref repos-resp :data))))))

(defun github-pulls-init-issue-buffer ()
  (deferred:$
    (deferred:call
      (lambda ()
        (github-pulls-prs (github-pulls-current-org) (github-pulls-current-repo))))
    (deferred:nextc it
      (lambda (issue-resp)
        (github-pulls-draw-issue-buffer (oref issue-resp :data))))))

(defun github-pulls-init-issue-comments-buffer ()
  (deferred:$
    (deferred:call
      (lambda ()
        (let ((response (github-pulls-pr (github-pulls-current-org)
                                         (github-pulls-current-repo)
                                         (github-pulls-current-issue))))
          (puthash "issue-data" (oref response :data) *github-pulls-state*)
          (github-pulls-pr-comments (github-pulls-current-org)
                                    (github-pulls-current-repo)
                                    (number-to-string (github-pulls-current-issue))))))
    (deferred:nextc it
      (lambda (comment-resp)
        (github-pulls-draw-issue-comments-buffer (oref comment-resp :data))))))

(defun github-pulls-get-menu-item-at-point ()
  (beginning-of-line)
  (re-search-forward "[0-9]+" nil 'move)
  (let ((index (string-to-number (buffer-substring-no-properties (line-beginning-position) (point)))))
    (if index
      (github-pulls-menu-item (- index 1))
      nil)))

(defun github-pulls-init-current-nav-buffer ()
  (cond ((equal (github-pulls-nav-level) "org") (github-pulls-init-org-buffer))
        ((equal (github-pulls-nav-level) "repo") (github-pulls-init-repo-buffer))
        ((equal (github-pulls-nav-level) "issue") (github-pulls-init-issue-buffer))
        ((equal (github-pulls-nav-level) "issue-comments") (github-pulls-init-issue-comments-buffer))))

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

(defun github-pulls-menu-select ()
  "Select an item in the nav menus."
  (interactive)
  (if (equal (line-number-at-pos) 1)
    (github-pulls-go-back)
    (let ((selection (github-pulls-get-menu-item-at-point)))
      (when selection
        (puthash (github-pulls-nav-level) selection (github-pulls-nav-state))
        (github-pulls-init-current-nav-buffer)))))

(defun github-pulls-go-back ()
  "Go back one step in the nav process."
  (interactive)
  (cond ((equal (github-pulls-nav-level) "repo") (remhash "org" (github-pulls-nav-state)))
        ((equal (github-pulls-nav-level) "issue") (remhash "repo" (github-pulls-nav-state)))
        ((equal (github-pulls-nav-level) "issue-comments") (remhash "issue" (github-pulls-nav-state))))
  (github-pulls-init-current-nav-buffer))

;;; github-pulls.el ends here
