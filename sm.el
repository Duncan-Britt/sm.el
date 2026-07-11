;;; sm.el --- Simple UI for managing git submodules -*- lexical-binding: t -*-

;; Author: Duncan Britt <duncanbritt.com>
;; Contact: https://github.com/Duncan-Britt/sm.el/issues
;; URL: https://github.com/Duncan-Britt/sm.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2") (transient "0.12.0"))
;; Keywords: hypermedia, srs, memory

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; TODO see dired.el, specifically look at font-lock-defaults which
;; defines how marked files/dirs get highlighted differently. I can do
;; the same thing for marked repos.

;; NOTE: Maybe this should work with mercurial as well?

;; UI:
;; foo -> main -> up to date
;; |--- bar -> (das3fs9) main -> up to date
;; |--- baz -> (das3fs9) feature -> unpulled changes
;; |--- qux -> feature -> uncommitted changes

;;; Code:
(require 'ewoc)
(require 'vc)

(defun sm-mark ()
  "Mark repo on the line at point."
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (insert "+"))
  (next-line))

;; TODO sm--get-marked-repos
;; TODO sm--repo-at-point
;; TODO sm--checkout-repo
;; TODO sm--pull-repo

;; TODO sm-commit command that prompts to commit
;; unstaged changes in the submodules before commiting the parent.

;; TODO sm-vc-dir: open repo at point in vc dir
;; TODO sm-push: push changes for marked repos or repo at point

;; TODO helper: (sm--get-status repo) - return list (plist? alist?) of
;; - checked out (hash or branch)
;; - branch associated with checked-out
;; - status message

(defun sm-checkout ()
  "Check out marked repos or repo at point."
  (interactive)
  ;; TODO figured out what a "repo" is, e.g. what info is needed in
  ;; that data structure.
  (if-let (marked-repos (sm--get-marked-repos))
      (dolist (repo marked-repos)
        (sm--checkout-repo repo))
    (sm--checkout-repo (sm--repo-at-point))))

(defun sm-pull ()
  "Pull marked repos or repo at point."
  (interactive)
  ;; TODO figured out what a "repo" is, e.g. what info is needed in
  ;; that data structure.
  (if-let (marked-repos (sm--get-marked-repos))
      (dolist (repo marked-repos)
        (sm--pull-repo repo))
    (sm--pull-repo (sm--repo-at-point))))

(defvar sm--buffers nil "List of sm-mode buffers.")

(defun sm--setup-buffer (buf)
  "..."
  (set-buffer (get-buffer-create buf))
  (kill-all-local-variables)
  (let ((buffer-undo-list t)
        (inhibit-read-only t))
    (erase-buffer)))

(defun sm--prepare-status-buffer (name dir)
  "Find a buffer named NAME showing DIR, or create a new one."
  (setq dir (file-name-as-directory (expand-file-name dir)))
  (let* ;; Look for another buffer name NAME visiting the same directory.
      ((buf (save-excursion
              (cl-dolist (buffer sm--buffers)
                (when (buffer-live-p buffer)
                  (set-buffer buffer)
                  (when (and (derived-mode-p 'sm-mode)
                             (string= default-directory dir))
                    (cl-return buffer)))))))
    (or buf
        ;; Create a new buffer named NAME.
	;; We pass a filename to create-file-buffer because it is what
	;; the function expects.
        (with-current-buffer (create-file-buffer (expand-file-name name dir))
          (sm--setup-buffer (current-buffer))
          (setq default-directory dir)
          (current-buffer)))))

(defun sm (dir)
  "Show vcs status for dir including submodules."
  (interactive
   (list
    (file-truename (read-directory-name "Status for directory: "
                                        (vc-root-dir) nil t))))
  (let ((pop-up-windows nil))
    (pop-to-buffer (sm--prepare-status-buffer "*sm*" dir)))
  (if (derived-mode-p 'sm-mode)
      (sm-refresh)
    (sm-mode)))

;; I can explore or use `vc-switch-branch' which does completing read over available branches

(defvar sm--ewoc nil)

(cl-defstruct (sm--repo-info
               (:copier nil)
               (:type list)
               (:constructor
                sm-create-repo-info (rel-path commit branch branch-checked-out? status &optional marked))
               (:conc-name sm--repo-info->))
  rel-path
  commit
  branch
  branch-checked-out?
  status
  marked)
;; up to date? uncommitted changes? unpulled changes?

;; foo -> main -> up to date
;; |--- bar -> (das3fs9) main -> up to date
;; |--- baz -> (das3fs9) feature -> unpulled changes
;; |--- qux -> feature -> uncommitted changes

(defun sm--repo-info:status-msg (repo)
  "Return appropriate status message for REPO."
  (pcase-exhaustive (sm--repo-info->status repo)
    (:up-to-date "up to date")
    (:unpulled-changes "unpulled changes")
    (:uncommitted-changes "uncommitted changes")))

(let ((repo-info (sm-create-repo-info "foo" "ae41jlsk" "feature-1" nil :up-to-date)))
  (sm--repo-info:status-msg repo-info))

(defun sm--repo-render (entry)
  "Render state of submodule ENTRY."
  (insert
   (propertize
    (format "%c" (if (sm--repo-info->marked entry) ?* ? ))
    'face 'sm-mark-indicator-face)
   " └── "
   (propertize
    (format "%s" (sm--repo-info->rel-path entry))
    'face 'sm-repo-path-face)
   " "
   (if (sm--repo-info->branch-checked-out? entry)
       (propertize
        (format "%s" (sm--repo-info->branch entry))
        'face 'sm-checkout-face)
     (propertize
      (format "%s" (sm--repo-info->commit entry))
      'face 'sm-checkout-face)
     (propertize
      (format " (%s)" (sm--repo-info:status-msg entry))
      'face 'sm-status-msg-face))
   "<- "
   (propertize
    (format "%s" (sm--repo-info->)))))

(defun sm--root-dir ()
  "Return VC root of `default-directory', or nil."
  (when-let ((backend (vc-responsible-backend default-directory t)))
    (vc-call-backend backend 'root default-directory)))

(defun sm--project-root-name ()
  "Return name of project root directory."
  (if-let (path (sm--root-dir))
      (file-name-nondirectory (string-trim-right path "/"))
    (user-error "directory not under source control: %s" default-directory)))

(defun sm-headers ()
  "Display the headers *SM* buffer."
  (concat
   (propertize (format "%s" (sm--project-root-name)) 'face 'sm-header)
   "\n"))

(defun sm--project-get-repos ()
  "Return a list of `sm--repo-info's for each git submodule"
  (message "called")
  nil)

(defun sm--busy ()
  "TODO"
  nil)

(defun sm-refresh ()
  "Refresh the contents of the *SM* buffer.
  Throw an error if another update process is in progress."
  (interactive)
  (if (sm--busy)
      (error "Another update process is in progress, cannot run two at a time")
    (ewoc-set-hf sm--ewoc (sm-headers) "")
    (mapc (lambda (entry)
            "TODO update sm--ewoc")
          (sm--project-get-repos))))

(defvar sm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" #'sm-mark)
    (define-key map "g" #'sm-refresh)
    (let ((branch-map (make-sparse-keymap)))
      (define-key map "b" branch-map)
      (define-key branch-map "s" #'sm-switch-branch))
    map)
  "Keymap for directory buffer.")

(define-derived-mode sm-mode special-mode "SM dir"
  "Major mode for SM directory buffers."
  (setq buffer-read-only t)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (setq-local sm--ewoc (ewoc-create #'sm--repo-render))
    ;; (setq-local revert-buffer-function 'vc-dir-revert-buffer-function)
    (setq list-buffers-directory (expand-file-name "*sm*" default-directory))
    (hack-dir-local-variables-non-file-buffer)
    (sm-refresh)))

(provide 'sm)
;;; sm.el ends here
