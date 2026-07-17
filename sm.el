;;; sm.el --- Simple UI for managing git submodules -*- lexical-binding: t -*-

;; Author: Duncan Britt <duncanbritt.com>
;; Contact: https://github.com/Duncan-Britt/sm.el/issues
;; URL: https://github.com/Duncan-Britt/sm.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.0"))
;; Keywords: vc, tools

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
;; Should you find yourself developing a software project composed of
;; tightly coupled git submodules, sm.el (Sub-Module) is here to help
;; you by providing visibility into the state of all your submodules
;; and convenience to frequently used git commands, especially when
;; you need to carry out the same action accross multiple git
;; submodules.

;;; Code:
(require 'ewoc)
(require 'vc)
(require 'vc-git)

(defun sm--assoc-delete-all (key alist &optional test)
  "Delete from ALIST all elements whose car is KEY.
Compare keys with TEST.  Defaults to `equal'.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (if (fboundp 'assoc-delete-all)
      (assoc-delete-all key alist test)
    (unless test (setq test #'equal))
    (while (and (consp (car alist))
                (funcall test (caar alist) key))
      (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr))
                 (funcall test (caar tail-cdr) key))
            (setcdr tail (cdr tail-cdr))
          (setq tail tail-cdr))))
    alist))

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

(defface sm-status-busy-face
  '((t :inherit compilation-mode-line-run))
  "Face for status of a submodule with an operation in progress."
  :group 'sm)

(defvar sm--ewoc nil)

(cl-defstruct (sm--repo-info
               (:copier nil)
               (:type list)
               (:constructor
                sm-create-repo-info (rel-path commit branch detached-head? unpulled-changes? uncommitted-changes? &optional marked?))
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

(defun sm--switch-node-to-branch (node branch &optional callback)
  "Asynchronously switch repo at ewoc NODE to BRANCH and update UI."
  (let* ((repo (ewoc-data node))
         (rel-path (sm--repo-info->rel-path repo))
         (dir (expand-file-name rel-path (sm--root-dir))))
    (sm--run-git-on-node
     node "sm-switch" "switching branch" (list "checkout" branch)
     (lambda ()
       (setf (sm--repo-info->commit repo) (sm--head-commit)
             (sm--repo-info->branch repo) branch
             (sm--repo-info->detached-head? repo) nil
             (sm--repo-info->unpulled-changes? repo)
             (sm--unpulled-changes-p dir branch)))
     (format "Switched %s to %s" rel-path branch)
     callback)))

(defun sm--branch-switch-internal (node &optional callback)
  "Prompt for a branch and asynchronously switch the repo at ewoc NODE to it."
  (let* ((repo (ewoc-data node))
         (rel-path (sm--repo-info->rel-path repo))
         (dir (expand-file-name rel-path (sm--root-dir)))
         (branch (completing-read
                  (format "Switch %s to branch: " rel-path)
                  (sm--branch-candidates dir) nil t)))
    (sm--switch-node-to-branch node branch callback)))

(defun sm-branch-switch ()
  "Switch branch of marked repos or repo at point.
If multiple repos are marked, prompt for each in turn."
  (interactive)
  (sm--do-nodes-dwim "switched" #'sm--branch-switch-internal))

(defun sm-branch-switch-dwim ()
  "Switch branch of marked repos or repo at point.
With multiple marked repos, offer branches common to all of them;
if there are none, fall back to prompting per repo."
  (interactive)
  (let ((marked-nodes (sm--get-marked-ewoc-nodes)))
    (if-let ((_ (cdr marked-nodes))
             (common (cl-reduce
                      (lambda (a b) (cl-intersection a b :test #'string=))
                      (mapcar (lambda (node)
                                (sm--branch-candidates
                                 (expand-file-name
                                  (sm--repo-info->rel-path (ewoc-data node))
                                  (sm--root-dir))))
                              marked-nodes))))
        (let ((branch (completing-read "Switch marked repos to branch: "
                                       common nil t)))
          (sm--do-nodes-dwim "switched"
                             (lambda (node cb)
                               (sm--switch-node-to-branch node branch cb))))
      (sm--do-nodes-dwim "switched" #'sm--branch-switch-internal))))

(defun sm--branch-candidates (dir)
  "Return local branch names plus short names of remote branches in DIR."
  (let ((default-directory dir))
    (delete-dups
     (append
      (process-lines vc-git-program "branch" "--format=%(refname:short)")
      (mapcar (lambda (ref)
                ;; "origin/feature" -> "feature"
                (substring ref (1+ (cl-position ?/ ref))))
              (process-lines vc-git-program "for-each-ref"
                             "refs/remotes" "--format=%(refname:short)"
                             "--exclude=refs/remotes/*/HEAD"))))))

(defun sm--create-node-branch (node branch &optional callback)
  "Asynchronously create and switch to BRANCH for the repo at ewoc NODE."
  (let* ((repo (ewoc-data node))
         (rel-path (sm--repo-info->rel-path repo)))
    (sm--run-git-on-node
     node "sm-branch-new" "creating branch" (list "checkout" "-b" branch)
     (lambda ()
       (setf (sm--repo-info->commit repo) (sm--head-commit)
             (sm--repo-info->branch repo) branch
             (sm--repo-info->detached-head? repo) nil
             (sm--repo-info->unpulled-changes? repo) nil))
     (format "Created branch %s in %s" branch rel-path)
     callback)))

(defun sm--branch-new-internal (node &optional callback)
  "Prompt for a name and asynchronously create a new branch for the repo at NODE."
  (let* ((repo (ewoc-data node))
         (rel-path (sm--repo-info->rel-path repo))
         (branch (read-string (format "New branch for %s: " rel-path))))
    (sm--create-node-branch node branch callback)))

(defun sm-branch-new ()
  "Create and switch to a new branch for marked repos or repo at point.
If multiple repos are marked, prompt for each in turn."
  (interactive)
  (sm--do-nodes-dwim "branched" #'sm--branch-new-internal))

(defun sm-branch-new-dwim ()
  "Create and switch to a new branch for marked repos or repo at point.
With multiple marked repos, prompt once and create the same branch
name in all of them."
  (interactive)
  (if (cdr (sm--get-marked-ewoc-nodes))
      (let ((branch (read-string "New branch for marked repos: ")))
        (sm--do-nodes-dwim "branched"
                           (lambda (node cb)
                             (sm--create-node-branch node branch cb))))
    (sm--do-nodes-dwim "branched" #'sm--branch-new-internal)))

(defun sm-vc-dir ()
  "Open the repo at point in `vc-dir'."
  (interactive)
  (let* ((repo (sm--repo-at-point))
         (dir (expand-file-name (sm--repo-info->rel-path repo)
                                (sm--root-dir))))
    (vc-dir dir)))

(defun sm--repo-at-point (&optional pos)
  "Return the `sm--repo-info' for the entry at point or POS."
  (let* ((pos (or pos (point)))
         (node (ewoc-locate sm--ewoc pos)))
    (when (and node
               ;; point in header => locate returns first node,
               ;; but pos is before it
               (>= pos (ewoc-location node))
               ;; point in footer => locate returns last node,
               ;; but pos is at/after the footer start
               (< pos (ewoc-location (ewoc--footer sm--ewoc))))
      (ewoc-data node))))

(defconst sm--log-buffer "*sm-log*"
  "Name of the buffer logging failed sm operations.")

(defun sm--log-failure (operation rel-path err)
  "Append OPERATION failure ERR for REL-PATH to `sm--log-buffer'."
  (with-current-buffer (get-buffer-create sm--log-buffer)
    (special-mode)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format-time-string "[%F %T] ")
              (propertize operation 'face 'error)
              " "
              (propertize rel-path 'face 'sm-repo-path-face)
              "\n" err "\n\n"))))

(defun sm--pull-repo (node &optional callback)
  "Asynchronously pull the repo at ewoc NODE and update UI when done."
  (let* ((repo (ewoc-data node))
         (rel-path (sm--repo-info->rel-path repo)))
    (if (sm--repo-info->detached-head? repo)
        (if callback
            (funcall callback rel-path "detached HEAD")
          (user-error "Cannot pull %s: detached HEAD" rel-path))
      (sm--run-git-on-node
       node "sm-pull" "pulling" '("pull" "--ff-only")
       (lambda ()
         (setf (sm--repo-info->commit repo) (sm--head-commit)
               (sm--repo-info->unpulled-changes? repo) nil))
       (format "Pulled %s" rel-path)
       callback))))

(defun sm--do-nodes-dwim (verb operation)
  "Run OPERATION on marked nodes, or the node at point.
OPERATION is called with (NODE CALLBACK), where CALLBACK must
eventually be called with (REL-PATH ERROR-STRING-OR-NIL).  When
operating on marked nodes, failures are logged to `sm--log-buffer'
and a single summary is messaged using VERB (e.g. \"pulled\")."
  (if-let (marked-nodes (sm--get-marked-ewoc-nodes))
      (let ((total (length marked-nodes))
            (pending (length marked-nodes))
            (failed 0))
        (dolist (node marked-nodes)
          (funcall operation node
                   (lambda (rel-path err)
                     (when err
                       (cl-incf failed)
                       (sm--log-failure verb rel-path err))
                     (cl-decf pending)
                     (when (zerop pending)
                       (if (zerop failed)
                           (message "%s %d repos" (capitalize verb) total)
                         (message "%s %d repos, %d failed (see %s)"
                                  (capitalize verb) (- total failed)
                                  failed sm--log-buffer)
                         (pop-to-buffer sm--log-buffer)))))))
    (funcall operation (ewoc-locate sm--ewoc) nil)))

(defun sm-pull ()
  "Pull marked repos or repo at point."
  (interactive)
  (sm--do-nodes-dwim "pulled" #'sm--pull-repo))

(defun sm--push-repo (node &optional callback)
  "Asynchronously push the repo at ewoc NODE and update UI when done."
  (let* ((repo (ewoc-data node))
         (rel-path (sm--repo-info->rel-path repo)))
    (if (sm--repo-info->detached-head? repo)
        (if callback
            (funcall callback rel-path "detached HEAD")
          (user-error "Cannot push %s: detached HEAD" rel-path))
      (sm--run-git-on-node
       node "sm-push" "pushing" '("push")
       #'ignore
       (format "Pushed %s" rel-path)
       callback))))

(defun sm-push ()
  "Push marked repos or repo at point."
  (interactive)
  (sm--do-nodes-dwim "pushed" #'sm--push-repo))

(defvar sm--buffers nil "List of sm-mode buffers.")

(defun sm--setup-buffer (buf)
  "Setup *SM* buffer."
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

(defun sm--head-commit ()
  "Return the abbreviated HEAD commit of the repo at `default-directory'."
  (substring (car (process-lines vc-git-program "rev-parse" "HEAD")) 0 8))

(defun sm--run-git-on-node (node proc-name busy-label args update-fn success-msg &optional callback)
  "Run git ARGS asynchronously in the repo of ewoc NODE.
PROC-NAME names the process and temp buffer.  BUSY-LABEL is shown in
the entry's status while the operation runs (e.g. \"pulling\").  ..."
  (let* ((repo (ewoc-data node))
         (rel-path (sm--repo-info->rel-path repo))
         (dir (expand-file-name rel-path (sm--root-dir)))
         (sm-buf (current-buffer))
         (default-directory dir))
    (if (sm--dir-busy-p dir)
        (if callback
            (funcall callback rel-path "another operation is in progress")
          (user-error "%s: another operation is in progress" rel-path))
      (let ((proc
             (make-process
              :name (format "%s:%s" proc-name rel-path)
              :buffer (generate-new-buffer (format " *%s*" proc-name))
              :command (cons vc-git-program args)
              :sentinel
              (lambda (proc _event)
                (when (memq (process-status proc) '(exit signal))
                  (let (err)
                    (unwind-protect
                        (if (not (zerop (process-exit-status proc)))
                            (setq err (with-current-buffer (process-buffer proc)
                                        (string-trim (buffer-string))))
                          (let ((default-directory dir))
                            (funcall update-fn)))
                      (setq sm--processes (sm--assoc-delete-all dir sm--processes))
                      (kill-buffer (process-buffer proc))
                      (when (buffer-live-p sm-buf)
                        (with-current-buffer sm-buf
                          (let ((inhibit-read-only t))
                            (ewoc-invalidate sm--ewoc node)))))
                    (if callback
                        (funcall callback rel-path err)
                      (if err
                          (message "git %s failed in %s: %s" (car args) rel-path err)
                        (message "%s" success-msg)))))))))
        (push (list dir proc busy-label) sm--processes)
        (let ((inhibit-read-only t))
          (ewoc-invalidate sm--ewoc node))))))

(defun sm--branch-attach-internal (node &optional callback)
  "Asynchronously check out a branch containing HEAD for the repo of NODE."
  (let* ((repo (ewoc-data node))
         (rel-path (sm--repo-info->rel-path repo))
         (dir (expand-file-name rel-path (sm--root-dir))))
    (if (not (sm--repo-info->detached-head? repo))
        (if callback
            (funcall callback rel-path "not a detached HEAD")
          (user-error "%s is not a detached HEAD" rel-path))
      (let* ((branches (sm--branches-containing-head dir))
             (branch (pcase branches
                       ('() (if callback
                                (funcall callback rel-path
                                         "no branch contains this commit")
                              (user-error "No branch contains this commit")))
                       (`(,b) b)
                       (_ (completing-read
                           (format "Attach %s to branch: " rel-path)
                           branches nil t)))))
        (when branch
          (sm--run-git-on-node
           node "sm-attach" "attaching" (list "checkout" branch)
           (lambda ()
             (setf (sm--repo-info->commit repo) (sm--head-commit)
                   (sm--repo-info->branch repo) branch
                   (sm--repo-info->detached-head? repo) nil))
           (format "Attached %s to %s" rel-path branch)
           callback))))))

(defun sm-branch-attach ()
  "Attach detached HEADs to a branch for marked repos or repo at point."
  (interactive)
  (sm--do-nodes-dwim "attached" #'sm--branch-attach-internal))

(defun sm--repo-info:status-msg (repo)
  "Return appropriate status message for REPO."
  (if-let ((label (sm--dir-busy-label
                   (expand-file-name (sm--repo-info->rel-path repo)
                                     (sm--root-dir)))))
      (concat label "...")
    (if (sm--repo-info->detached-head? repo)
        "detached HEAD"
      (pcase-exhaustive (cons (sm--repo-info->unpulled-changes? repo)
                              (sm--repo-info->uncommitted-changes? repo))
        ('(nil) "up to date")
        ('(t) "unpulled changes")
        ('(nil . t) "uncommitted changes")
        ('(t . t) "unpulled & uncommitted changes")))))

(defun sm--repo-status-face (repo)
  (cond
   ((sm--dir-busy-label (expand-file-name (sm--repo-info->rel-path repo)
                                          (sm--root-dir)))
    'sm-status-busy-face)
   ((or (sm--repo-info->unpulled-changes? repo)
        (sm--repo-info->uncommitted-changes? repo))
    'sm-status-attention-face)
   (t 'sm-status-ok-face)))

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
  (when-let ((backend (vc-responsible-backend default-directory)))
    (vc-call-backend backend 'root default-directory)))

(defun sm--project-root-name ()
  "Return name of project root directory."
  (if-let (path (sm--root-dir))
      (file-name-nondirectory (string-trim-right path "/"))
    (user-error "directory not under source control: %s" default-directory)))

(defun sm-headers ()
  "Render the headers of the *SM* buffer."
  (cl-flet ((render-row (cmds)
              (mapconcat
               (pcase-lambda (`(,cmd . ,desc))
                 (let ((key (where-is-internal cmd sm-mode-map t)))
                   (concat (propertize (if key (key-description key) "M-x")
                                       'face 'help-key-binding)
                           " " desc)))
               cmds
               "  ")))
    (concat
     (render-row '((sm-mark               . "mark")
                   (sm-unmark             . "unmark")
                   (sm-mark-all           . "mark all")
                   (sm-unmark-all         . "unmark all")
                   (sm-refresh            . "refresh")
                   (sm-pull               . "pull")
                   (sm-push               . "push")))
     "\n"
     (render-row '((sm-vc-dir             . "vc-dir")
                   (sm-branch-switch-dwim . "switch branch (dwim)")
                   (sm-branch-switch      . "switch branch")
                   (sm-branch-new-dwim         . "new branch (dwim)")
                   (sm-branch-new         . "new branch")
                   (sm-branch-attach      . "attach branch")))
     "\n\n"
     (propertize (format "%s" (sm--project-root-name)) 'face 'sm-header))))

(defvar sm--processes nil
  "Alist of (DIR PROC LABEL) for in-flight sm git operations.
LABEL is a short present-participle string like \"pulling\".")

(defun sm--prune-processes ()
  "Drop dead processes from `sm--processes'."
  (setq sm--processes
        (cl-delete-if-not (lambda (entry) (process-live-p (nth 1 entry)))
                          sm--processes)))

(defun sm--dir-busy-label (dir)
  "Return the busy LABEL for DIR, or nil if no operation is in flight."
  (nth 2 (sm--dir-busy-p dir)))

(defun sm--dir-busy-p (dir)
  "Return non-nil if a git operation is in flight in DIR."
  (sm--prune-processes)
  (assoc dir sm--processes))

(defun sm--busy ()
  "Return non-nil if any operation is in flight under this project's root."
  (sm--prune-processes)
  (let ((root (expand-file-name (sm--root-dir))))
    (cl-some (lambda (entry) (string-prefix-p root (car entry)))
             sm--processes)))

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

(defun sm--uncommitted-changes-p (dir)
  "Return t if remote has unpulled changes, else NIL.
Applies to git repo rooted at DIR."
  (let ((default-directory dir))
    (and (process-lines vc-git-program "status" "--porcelain") t)))

(defun sm--git-current-branch (dir)
  "Return (BRANCH . DETACHED-HEAD?) for repo DIR.
BRANCH is nil when HEAD is detached."
  (let ((default-directory dir))
    (pcase (process-lines vc-git-program "branch" "--show-current")
      (`(,branch) (cons branch nil))
      ('() (cons nil t)))))

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
    (define-key map "+" #'sm-pull)
    (define-key map "P" #'sm-push)
    (define-key map (kbd "RET") #'sm-vc-dir)
    (let ((branch-map (make-sparse-keymap)))
      (define-key map "b" branch-map)
      (define-key branch-map "s" #'sm-branch-switch-dwim)
      (define-key branch-map "S" #'sm-branch-switch)
      (define-key branch-map "n" #'sm-branch-new-dwim)
      (define-key branch-map "N" #'sm-branch-new)
      (define-key branch-map "a" #'sm-branch-attach))
    map)
  "Keymap for directory buffer.")

(define-derived-mode sm-mode special-mode "SM dir"
  "Major mode for SM directory buffers."
  (setq buffer-read-only t)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (setq-local sm--ewoc (ewoc-create #'sm--repo-render))
    ;; TODO? (setq-local revert-buffer-function 'sm-revert-buffer-function)
    (setq list-buffers-directory (expand-file-name "*sm*" default-directory))
    (hack-dir-local-variables-non-file-buffer)
    (cl-pushnew (current-buffer) sm--buffers)
    (sm-refresh)))

(provide 'sm)
;;; sm.el ends here
