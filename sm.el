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
;; NOTE: Maybe this should work with mercurial as well?

;;; Code:
(require 'ewoc)
(require 'vc)
(require 'vc-git)

(defgroup sm nil
  "Simple UI for managing git submodules."
  :group 'vc)

(defface sm-header
  '((t :inherit vc-dir-header-value))
  "Face for the project name in the header."
  :group 'sm)

(defface sm-mark-indicator-face
  '((t :inherit dired-mark)) ;; TODO: do I need to require dired?
  "Face for the mark indicator."
  :group 'sm)

(defface sm-repo-path-face
  '((t :inherit dired-directory))
  "Face for submodule paths."
  :group 'sm)

(defface sm-checkout-face
  '((t :inherit vc-dir-status-up-to-date))
  "Face for the checked-out branch or commit."
  :group 'sm)

(defface sm-status-ok-face
  '((t :inherit shadow))
  "Face for status messages when a submodule is up to date."
  :group 'sm)

(defface sm-status-attention-face
  '((t :inherit warning))
  "Face for status messages when a submodule needs attention."
  :group 'sm)

(defface sm-marked-face
  '((t :inherit dired-marked))
  "Face overlaid on marked entries."
  :group 'sm)

(defvar sm--ewoc nil)

(cl-defstruct (sm--repo-info
               (:copier nil)
               (:type list)
               (:constructor
                sm-create-repo-info (rel-path commit branch detached-head? unpulled-changes? uncommitted-changes? &optional marked))
               (:conc-name sm--repo-info->))
  rel-path
  commit
  branch
  detached-head?
  unpulled-changes?
  uncommitted-changes?
  marked?)

(defun sm--repo-status-face (repo)
  (if (or (sm--repo-info->unpulled-changes? repo)
          (sm--repo-info->uncommitted-changes? repo))
      'sm-status-attention-face
    'sm-status-ok-face))

(defun sm--mark-internal (node)
  "Mark ewoc NODE."
  (setf (sm--repo-info->marked? (ewoc-data node)) t)
  (let ((inhibit-read-only t))
    (ewoc-invalidate sm--ewoc node)))

(defun sm-mark ()
  "Mark the repo at point and move to the next entry."
  (interactive)
  (let ((node (ewoc-locate sm--ewoc)))
    (unless node
      (user-error "No repo at point"))
    (sm--mark-internal node)
    (when-let ((next (ewoc-next sm--ewoc node)))
      (ewoc-goto-node sm--ewoc next))))

(defun sm--unmark-internal (node)
  "Unmark ewoc NODE."
  (setf (sm--repo-info->marked? (ewoc-data node)) nil)
  (let ((inhibit-read-only t))
    (ewoc-invalidate sm--ewoc node)))

(defun sm-unmark ()
  "Unmark the repo at point and move to the next entry."
  (interactive)
  (let ((node (ewoc-locate sm--ewoc)))
    (unless node
      (user-error "No repo at point"))
    (sm--unmark-internal node)
    (when-let ((next (ewoc-next sm--ewoc node)))
      (ewoc-goto-node sm--ewoc next))))

(defun sm-mark-all ()
  "Mark all repos."
  (interactive)
  (let ((node (ewoc-nth sm--ewoc 0)))
    (while node
      (sm--mark-internal node)
      (setq node (ewoc-next sm--ewoc node)))))

(defun sm-unmark-all ()
  "Unmark all repos."
  (interactive)
  (let ((node (ewoc-nth sm--ewoc 0)))
    (while node
      (sm--unmark-internal node)
      (setq node (ewoc-next sm--ewoc node)))))

(defun sm--get-marked-repos ()
  "Return a list of marked `sm--repo-info's."
  (let (marked)
    (ewoc-map (lambda (repo)
                (when (sm--repo-info->marked? repo)
                  (push repo marked))
                nil)  ; nil = don't re-render this node
              sm--ewoc)
    (nreverse marked)))

(defun sm--get-marked-ewoc-nodes ()
  "Return a list of ewoc nodes whose repos are marked."
  (let ((marked nil)
        (node (ewoc-nth sm--ewoc 0)))
    (while node
      (when (sm--repo-info->marked? (ewoc-data node))
        (push node marked))
      (setq node (ewoc-next sm--ewoc node)))
    (nreverse marked)))

;; (mapcar #'ewoc-data (sm--get-marked-ewoc-nodes))

(defun sm--branch-switch-internal (node)
  "Switch vc branch of repo at ewoc NODE and update UI."
  (let* ((repo (ewoc-data node))
         (dir (expand-file-name (sm--repo-info->rel-path repo)
                                (sm--root-dir)))
         (default-directory dir)
         (name (vc-read-revision (format-prompt "Switch %s to branch" "latest revisions" (sm--repo-info->rel-path repo))
                                 (list dir)
                                 (vc-responsible-backend dir))))
    (vc-retrieve-tag dir name)
    ;; FIXME isn't vc-retrieve-tag async? does that matter?
    ;; TODO I should deal with this properly
    (setf (sm--repo-info->branch repo) name))
  (ewoc-invalidate sm--ewoc node))

(defun sm-branch-switch-dwim ()
  "Switch branch of marked repos or repo at point.
If multiple repos are marked, completing read of branch names common
among all marked repos, or user-error if there are no options."
  (if-let (marked-nodes (sm--get-marked-ewoc-nodes))
      (progn
        'todo)
    (sm--branch-switch-internal (ewoc-locate sm--ewoc))))

(defun sm-branch-switch ()
  "Switch branch of marked repos or repo at point.
If multiple repos are marked, switch branches one at a time."
  (interactive)
  (if-let (marked-nodes (sm--get-marked-ewoc-nodes))
      (dolist (node marked-nodes)
        (sm--branch-switch-internal node))
    (sm--branch-switch-internal (ewoc-locate sm--ewoc))))

;; TODO sm--pull-repo

;; TODO sm-commit command that prompts to commit
;; unstaged changes in the submodules before commiting the parent.

;; TODO sm-push: push changes for marked repos or repo at point
;; TODO sm-branch-new key: b n

(defun sm-vc-dir ()
  "Open the repo at point in `vc-dir'."
  (interactive)
  (let* ((repo (sm--repo-at-point))
         (dir (expand-file-name (sm--repo-info->rel-path repo)
                                (sm--root-dir))))
    (vc-dir dir)))

(defun sm--repo-at-point ()
  "Return the `sm--repo-info' for the entry at point, or signal an error."
  (let ((node (ewoc-locate sm--ewoc)))
    (unless node
      (user-error "No repo at point"))
    (ewoc-data node)))

(defun sm-pull ()
  "Pull marked repos or repo at point."
  (interactive)
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
  (setq sm--buffers (cl-delete-if-not #'buffer-live-p sm--buffers))
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

(defun sm--branches-containing-head (dir)
  "Return branches containing HEAD in DIR, excluding the detached pseudo-entry."
  (let ((default-directory dir))
    (cl-remove-if (lambda (b) (string-prefix-p "(" b))
                  (process-lines vc-git-program "branch" "--contains"
                                 "HEAD" "--format=%(refname:short)"))))

;; (sm--branches-containing-head "~/code/watch_n_draw_build/watchndraw/")
;;=> ("main")
;; (sm--branches-containing-head "~/code/watch_n_draw_build/directory-slideshow/")
;;=> ("foobranch" "main")

(defun sm--branch-attach-internal (node)
  "Check out a branch containing HEAD for the repo of NODE."
  (let* ((repo (ewoc-data node))
         (dir (expand-file-name (sm--repo-info->rel-path repo) (sm--root-dir)))
         (branches (sm--branches-containing-head dir))
         (branch (pcase branches
                   ('() (user-error "No branch contains this commit"))
                   (`(,b) b)
                   (_ (completing-read (format "Attach %s to branch: "
                                               (sm--repo-info->rel-path repo))
                                       branches nil t)))))
    (let ((default-directory dir))
      (vc-retrieve-tag dir branch))
    ;; FIXME isn't vc-retrieve-tag async? does that matter?
    ;; TODO I should deal with this properly
    (setf (sm--repo-info->branch repo) branch
          (sm--repo-info->detached-head? repo) nil)
    (ewoc-invalidate sm--ewoc node)))

(defun sm-branch-attach ()
  "Check out a branch containing HEAD for the (detached) repo at point or
marked repos."
  (interactive)
  (if-let (marked-nodes (sm--get-marked-ewoc-nodes))
      (dolist (node marked-nodes)
        (sm--branch-attach-internal node))
    (sm--branch-attach-internal (ewoc-locate sm--ewoc))))

(defun sm--repo-info:status-msg (repo)
  "Return appropriate status message for REPO."
  (if (sm--repo-info->detached-head? repo)
      "detached HEAD"
    (pcase-exhaustive (cons (sm--repo-info->unpulled-changes? repo)
                            (sm--repo-info->uncommitted-changes? repo))
      ('(nil) "up to date")
      ('(t) "unpulled changes")
      ('(nil . t) "uncommitted changes")
      ('(t . t) "unpulled & uncommitted changes"))))

;; (let ((repo-info (sm-create-repo-info "foo" "ae41jlsk" "feature-1" nil :up-to-date)))
;;   (sm--repo-info:status-msg repo-info))
;;=> "up to date"

(defvar-local sm--column-widths nil
  "Cons of (PATH-WIDTH . CHECKOUT-WIDTH) for aligning entries.")

(defun sm--compute-column-widths (repos)
  (cons
   (apply #'max 0 (mapcar (lambda (r) (length (sm--repo-info->rel-path r))) repos))
   (apply #'max 0 (mapcar (lambda (r)
                            (length (if (sm--repo-info->detached-head? r)
                                        (sm--repo-info->commit r)
                                      (sm--repo-info->branch r))))
                          repos))))

(defun sm--repo-render (entry)
  "Render state of submodule ENTRY."
  (let ((line (concat (propertize
                       (format "%c" (if (sm--repo-info->marked? entry) ?* ? ))
                       'face 'sm-mark-indicator-face)
                      " └── "
                      (propertize
                       (format (format "%%-%ds" (car sm--column-widths))
                               (sm--repo-info->rel-path entry))
                       'face 'sm-repo-path-face)
                      " "
                      (propertize
                       (format (format "%%-%ds" (cdr sm--column-widths))
                               (if (sm--repo-info->detached-head? entry)
                                   (sm--repo-info->commit entry)
                                 (sm--repo-info->branch entry)))
                       'face 'sm-checkout-face)
                      " <- "
                      (propertize
                       (format "(%s)" (sm--repo-info:status-msg entry))
                       'face (sm--repo-status-face entry)))))
    (when (sm--repo-info->marked? entry)
      (add-face-text-property 0 (length line) 'sm-marked-face nil line))
    (insert line)))

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
   (mapconcat
    (pcase-lambda (`(,cmd . ,desc))
      (let ((key (where-is-internal cmd sm-mode-map t)))
        (concat (propertize (if key (key-description key) "M-x")
                            'face 'help-key-binding)
                " " desc)))
    '((sm-mark          . "mark")
      (sm-unmark        . "unmark")
      (sm-refresh       . "refresh")
      (sm-vc-dir        . "vc-dir")
      (sm-branch-switch . "switch branch")
      (sm-branch-attach . "attach branch")
      (sm-mark-all      . "mark all")
      (sm-unmark-all    . "unmark all"))
    "  ")
   "\n\n"
   (propertize (format "%s" (sm--project-root-name)) 'face 'sm-header)))

(defun sm--busy ()
  "TODO"
  nil)

(defun sm--git-submodule-lines ()
  "Return output lines of `git submodule status --recursive'."
  (process-lines vc-git-program "submodule" "status" "--recursive"))

(defun sm--unpulled-changes-p (dir branch)
  "Return t if remote has unpulled changes, else NIL.
Applies to git repo rooted at DIR."
  (let ((default-directory dir))
    (and (zerop (call-process vc-git-program nil nil nil
                              "rev-parse" "--verify" "--quiet" "@{upstream}"))
         (process-lines vc-git-program "rev-list" "-1" "HEAD..@{upstream}")
         t)))

;; git rev-list -1 HEAD..origin/foobranch


(defun sm--uncommitted-changes-p (dir)
  "Return t if remote has unpulled changes, else NIL.
Applies to git repo rooted at DIR."
  (let ((default-directory dir))
    (and (process-lines vc-git-program "status" "--porcelain") t)))


;; (sm--uncommitted-changes-p "~/code/watch_n_draw_build/Splice-Lang")
;; ;;=> t
;; (sm--uncommitted-changes-p "~/code/watch_n_draw_build/directory-slideshow")
;; ;;=> nil
;; (sm--unpulled-changes-p "~/code/watch_n_draw_build/watchndraw" "main")
;; ;;=> t
;; (sm--unpulled-changes-p "~/code/watch_n_draw_build/word_ladders" "main")
;; ;;=> nil


(defun sm--git-current-branch (dir)
  "Return (BRANCH . DETACHED-HEAD?) for repo DIR.
BRANCH is nil when HEAD is detached."
  (let ((default-directory dir))
    (pcase (process-lines-ignore-status vc-git-program "branch" "--show-current")
      (`(,branch) (cons branch nil))
      ('() (cons nil t)))))

;; (sm--git-current-branch "~/code/watch_n_draw_build/Splice-Lang/")
;;=> ("main")

;;=> ("main")

;;=> (nil)

;;=> (nil)

;;=> (nil)

;; (sm--git-current-branch "~/code/watch_n_draw_build/watchndraw/")
;;=> (nil . t)

;;=> ("main" . t)

;;=> ("(HEAD detached at 667d148)" . t)

;;=> ("(HEAD detached at 667d148)" . t)

;;=> ("(HEAD detached at 667d148)" . t)

;;=> (nil . t)

;;=> ("main" . t)

;; (sm--git-current-branch "~/code/watch_n_draw_build/directory-slideshow/")
;;=> (nil . t)

;;=> ("foobranch" . t)

;;=> ("(HEAD detached at 647df9d)" . t)

;;=> (nil . t)

;;=> ("main" . t)

(defun sm--project-get-repos ()
  "Return a list of `sm--repo-info's for each git submodule, recursively."
  (mapcar
   (lambda (line)
     (pcase-let* ((`(,commit ,rel-path) (split-string (substring line 1)))
                  (dir (expand-file-name rel-path (sm--root-dir)))
                  (`(,branch . ,detached-head?) (sm--git-current-branch dir))
                  (unpulled-changes? (and (not detached-head?) (sm--unpulled-changes-p dir branch)))
                  (uncommitted-changes? (sm--uncommitted-changes-p dir)))
       (sm-create-repo-info rel-path
                            (substring commit 0 8)
                            branch
                            detached-head?
                            unpulled-changes?
                            uncommitted-changes?)))
   (sm--git-submodule-lines)))
;; (let ((default-directory "~/code/watch_n_draw_build/"))
;;   (sm--project-get-repos))
;;=> (("watchndraw" "667d148d" "main" t :up-to-date nil))

(defun sm-refresh ()
  "Refresh the contents of the *SM* buffer.
  Throw an error if another update process is in progress."
  (interactive)
  (if (sm--busy)
      (error "Another update process is in progress, cannot run two at a time")
    (let ((inhibit-read-only t))
      (ewoc-set-hf sm--ewoc (sm-headers) "")
      (ewoc-filter sm--ewoc #'ignore)
      (let ((repos (sm--project-get-repos)))
        (setq sm--column-widths (sm--compute-column-widths repos))
        (ewoc-filter sm--ewoc #'ignore)
        (dolist (repo repos)
          (ewoc-enter-last sm--ewoc repo))))))

(defvar sm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" #'sm-mark)
    (define-key map "M" #'sm-mark-all)
    (define-key map "u" #'sm-unmark)
    (define-key map "U" #'sm-unmark-all)
    (define-key map "g" #'sm-refresh)
    (define-key map (kbd "RET") #'sm-vc-dir)
    (let ((branch-map (make-sparse-keymap)))
      (define-key map "b" branch-map)
      (define-key branch-map "s" #'sm-branch-switch)
      (define-key branch-map "a" #'sm-branch-attach))
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
    (cl-pushnew (current-buffer) sm--buffers)
    (sm-refresh)))

(provide 'sm)
;;; sm.el ends here
