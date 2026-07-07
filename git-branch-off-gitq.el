;;; git-branch-off-gitq.el --- GitQ: categorical query language for git  -*- lexical-binding: t; -*-

;; Provides `gitq': a pipeline query language for navigating git's object graph.
;; Syntax: source step step terminal (whitespace-separated, terminals start with /)
;; Example: (gitq "commits where author contains \"alice\" take 5 /show")

(require 'cl-lib)

;;; Git execution layer

(defun gitq--git (&rest args)
  "Run git with ARGS; return output lines as a list of non-empty strings."
  (if (fboundp 'magit-git-lines)
      (apply #'magit-git-lines args)
    (let ((buf (generate-new-buffer " *gitq-git*")))
      (unwind-protect
          (progn
            (apply #'call-process "git" nil buf nil args)
            (with-current-buffer buf
              (split-string (buffer-string) "\n" t)))
        (kill-buffer buf)))))

(defun gitq--git-string (&rest args)
  "Run git with ARGS; return first line of output or nil."
  (car (apply #'gitq--git args)))

(defun gitq--toplevel ()
  "Return the git toplevel or signal an error."
  (or (if (fboundp 'magit-toplevel)
          (magit-toplevel)
        (let ((s (gitq--git-string "rev-parse" "--show-toplevel")))
          (when s (file-name-as-directory s))))
      (user-error "gitq: not in a git repository")))

;;; Parser helpers

(defun gitq--unquote (str)
  "Strip surrounding double-quotes from STR."
  (if (and (> (length str) 1) (eq (aref str 0) ?\"))
      (substring str 1 (1- (length str)))
    str))

(defun gitq--unregex (str)
  "Extract the pattern from a /pattern/ token."
  (if (and (> (length str) 1) (eq (aref str 0) ?/))
      (substring str 1 (1- (length str)))
    str))

;;; Stage parsers

(defun gitq--expect-no-more (tokens context)
  "Signal an error if TOKENS is non-nil, naming CONTEXT (e.g. a stage keyword).
A terminal always ends the pipeline, so a terminal parser should always
consume every token it is given. Leftover tokens almost always mean a
multi-word value that needed double-quotes to be read as one token, or
a stray extra word after the terminal — both used to be silently
discarded here."
  (when tokens
    (error "gitq: unexpected token '%s' after '%s' (missing quotes around a value?)"
           (car tokens) context)))

(defun gitq--parse-terminal (kw tokens)
  "Parse terminal operation KW with remaining TOKENS."
  (cond
   ((equal kw "show")      (gitq--expect-no-more tokens kw) (list :type 'terminal :op 'show))
   ((equal kw "copy")      (gitq--expect-no-more tokens kw) (list :type 'terminal :op 'copy))
   ((equal kw "insert")    (gitq--expect-no-more tokens kw) (list :type 'terminal :op 'insert))
   ((equal kw "count")     (gitq--expect-no-more tokens kw) (list :type 'terminal :op 'count))
   ((equal kw "remove")    (gitq--expect-no-more tokens kw) (list :type 'terminal :op 'remove))
   ((equal kw "delete")    (gitq--expect-no-more tokens kw) (list :type 'terminal :op 'delete))
   ((equal kw "stage")     (gitq--expect-no-more tokens kw) (list :type 'terminal :op 'stage))
   ((equal kw "branch-off")
    (let* ((name (when (and tokens (string-prefix-p "\"" (car tokens)))
                   (gitq--unquote (pop tokens))))
           (wt   (when (equal (car tokens) "worktree")
                   (pop tokens)
                   (gitq--unquote (pop tokens)))))
      (gitq--expect-no-more tokens kw)
      (list :type 'terminal :op 'branch-off :name name :worktree wt)))
   ((equal kw "amend")
    (cond
     ((equal (car tokens) "no-edit")
      (gitq--expect-no-more (cdr tokens) kw)
      (list :type 'terminal :op 'amend :no-edit t :message nil))
     ((and tokens (string-prefix-p "\"" (car tokens)))
      (gitq--expect-no-more (cdr tokens) kw)
      (list :type 'terminal :op 'amend :no-edit nil
            :message (gitq--unquote (car tokens))))
     (t (gitq--expect-no-more tokens kw)
        (list :type 'terminal :op 'amend :no-edit nil :message nil))))
   ((equal kw "squash")
    (let ((msg (when (and tokens (string-prefix-p "\"" (car tokens)))
                 (gitq--unquote (car tokens)))))
      (gitq--expect-no-more (if msg (cdr tokens) tokens) kw)
      (list :type 'terminal :op 'squash :message msg)))
   ((equal kw "reword")
    (let ((msg (when (and tokens (string-prefix-p "\"" (car tokens)))
                 (gitq--unquote (car tokens)))))
      (gitq--expect-no-more (if msg (cdr tokens) tokens) kw)
      (list :type 'terminal :op 'reword :message msg)))
   ((equal kw "commit")
    (let ((msg (when (and tokens (string-prefix-p "\"" (car tokens)))
                 (gitq--unquote (car tokens)))))
      (gitq--expect-no-more (if msg (cdr tokens) tokens) kw)
      (list :type 'terminal :op 'commit :message msg)))
   ((equal kw "mark")
    (gitq--expect-no-more (cdr tokens) kw)
    (list :type 'terminal :op 'mark :label (when tokens (gitq--unquote (car tokens)))))
   (t (error "gitq: unknown terminal operation '%s'" kw))))

;;; Git data fetchers

(defconst gitq--log-format "%H%x00%ae%x00%an%x00%ai%x00%P%x00%T%x00%s"
  "NUL-delimited log format using git's %x00 escape (safe to pass as CLI arg).")

(defun gitq--parse-commit-line (line)
  "Parse a NUL-delimited commit log LINE into a frame plist, or nil."
  (let ((parts (split-string line "\x00")))
    (when (>= (length parts) 7)
      (let ((sha (nth 0 parts)))
        (unless (string-empty-p sha)
          (list :type 'commit
                :sha     sha
                :email   (nth 1 parts)
                :author  (nth 2 parts)
                :date    (nth 3 parts)
                :parents (split-string (nth 4 parts) " " t)
                :tree    (nth 5 parts)
                :message (nth 6 parts)))))))

(defun gitq--fetch-commits (&optional range)
  "Fetch commits reachable from HEAD (or within RANGE) as frame plists."
  (let* ((fmt  (format "--format=%s" gitq--log-format))
         (args (if range (list "log" fmt range) (list "log" fmt))))
    (delq nil (mapcar #'gitq--parse-commit-line (apply #'gitq--git args)))))

(defun gitq--fetch-commit (sha-or-ref)
  "Fetch a single commit by SHA-OR-REF, returning a frame plist or nil."
  (let ((sha (gitq--git-string "rev-parse" "--verify" sha-or-ref)))
    (when sha
      (car (delq nil
                 (mapcar #'gitq--parse-commit-line
                         (gitq--git "log" "--no-walk"
                                    (format "--format=%s" gitq--log-format)
                                    sha)))))))

(defun gitq--fetch-branches ()
  "Fetch all local branches as ref frame plists."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "^\\([0-9a-f]\\{40,\\}\\) \\(.+\\)$" line)
                    (list :type 'ref :reftype 'branch
                          :sha  (match-string 1 line)
                          :name (match-string 2 line))))
                (gitq--git "for-each-ref"
                           "--format=%(objectname) %(refname:short)"
                           "refs/heads/"))))

(defun gitq--fetch-tags ()
  "Fetch all tags as ref frame plists."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "^\\([0-9a-f]\\{40,\\}\\) \\(.+\\)$" line)
                    (list :type 'ref :reftype 'tag
                          :sha  (match-string 1 line)
                          :name (match-string 2 line))))
                (gitq--git "for-each-ref"
                           "--format=%(objectname) %(refname:short)"
                           "refs/tags/"))))

(defun gitq--fetch-refs ()
  "Fetch all refs as ref frame plists."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "^\\([0-9a-f]\\{40,\\}\\) \\(.+\\)$" line)
                    (list :type 'ref
                          :sha  (match-string 1 line)
                          :name (match-string 2 line))))
                (gitq--git "for-each-ref"
                           "--format=%(objectname) %(refname:short)"))))

(defun gitq--fetch-worktrees ()
  "Fetch all worktrees as worktree frame plists."
  (let (result entry)
    (dolist (line (gitq--git "worktree" "list" "--porcelain"))
      (cond
       ((string-prefix-p "worktree " line)
        (when entry (push entry result))
        (setq entry (list :type 'worktree :path (substring line 9))))
       ((string-prefix-p "HEAD " line)
        (setq entry (plist-put entry :sha (substring line 5))))
       ((string-prefix-p "branch " line)
        (setq entry (plist-put entry :branch
                               (string-remove-prefix "refs/heads/"
                                                     (substring line 7)))))
       ((string= (string-trim line) "detached")
        (setq entry (plist-put entry :detached t)))))
    (when entry (push entry result))
    (nreverse result)))

(defun gitq--fetch-blobs-at (tree-sha &optional path-filter type-filter)
  "Fetch blob/tree entries from TREE-SHA as frame plists."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match
                         "^\\([0-9]+\\) \\(blob\\|tree\\) \\([0-9a-f]+\\)\t\\(.+\\)$"
                         line)
                    (let* ((mode  (match-string 1 line))
                           (ftype (intern (match-string 2 line)))
                           (sha   (match-string 3 line))
                           (path  (match-string 4 line)))
                      (when (or (null type-filter) (eq ftype type-filter))
                        (when (or (null path-filter)
                                  (gitq--path-matches path path-filter))
                          (list :type ftype :sha sha :path path :mode mode))))))
                (gitq--git "ls-tree" "-r" tree-sha))))

(defun gitq--path-matches (path pattern)
  "Return non-nil if PATH matches the glob PATTERN."
  (or (string-match-p (wildcard-to-regexp pattern) path)
      (string-match-p (regexp-quote pattern) path)))

;;; Pipeline step executors

(defun gitq--exec-source (node)
  "Execute source node NODE and return the initial frame list."
  (pcase (plist-get node :source)
    ('commits
     (gitq--fetch-commits (plist-get node :range)))
    ('ref
     (let ((frame (gitq--fetch-commit (plist-get node :ref))))
       (if frame (list frame) nil)))
    ('branches (gitq--fetch-branches))
    ('tags     (gitq--fetch-tags))
    ('refs     (gitq--fetch-refs))
    ('worktree (gitq--fetch-worktrees))
    ('blobs
     (let ((tree (gitq--git-string "rev-parse" "HEAD^{tree}")))
       (when tree (gitq--fetch-blobs-at tree))))
    (src (error "gitq: unknown source '%s'" src))))

(defun gitq--traverse-parents-star (frames &optional plus)
  "Walk parent links from FRAMES, returning all reachable commits.
When PLUS is non-nil, exclude the start frames themselves (`.parent+')."
  (let (result (visited (make-hash-table :test 'equal)))
    (dolist (start frames)
      (let ((start-sha (plist-get start :sha))
            (queue     (list (plist-get start :sha))))
        (while queue
          (let* ((sha (pop queue))
                 (c   (gitq--fetch-commit sha)))
            (unless (gethash sha visited)
              (puthash sha t visited)
              (when c
                ;; .parent* includes start; .parent+ excludes start
                (unless (and plus (equal sha start-sha))
                  (push c result))
                (dolist (p (plist-get c :parents))
                  (unless (gethash p visited)
                    (push p queue)))))))))
    (nreverse result)))

(defun gitq--exec-via (frames node)
  "Traverse morphism in NODE from FRAMES, returning new frames."
  (let ((m (plist-get node :morphism)))
    (pcase m
      ('parent
       (cond
        ((plist-get node :star) (gitq--traverse-parents-star frames))
        ((plist-get node :plus) (gitq--traverse-parents-star frames t))
        ((numberp (plist-get node :index))
         (let ((idx (plist-get node :index)))
           (delq nil (mapcar (lambda (f)
                               (gitq--fetch-commit
                                (nth idx (plist-get f :parents))))
                             frames))))
        (t
         (delq nil
               (apply #'append
                      (mapcar (lambda (f)
                                (mapcar #'gitq--fetch-commit
                                        (plist-get f :parents)))
                              frames))))))
      ('parent-adjoint
       (let* ((target-shas (mapcar (lambda (f) (plist-get f :sha)) frames))
              (all (gitq--fetch-commits)))
         (seq-filter (lambda (c)
                       (seq-some (lambda (p) (member p target-shas))
                                 (plist-get c :parents)))
                     all)))
      ('tree
       (delq nil
             (mapcar (lambda (f)
                       (let ((tree (plist-get f :tree)))
                         (when tree (list :type 'tree :sha tree))))
                     frames)))
      ('tree-entries
       (let ((filter (plist-get node :filter)))
         (apply #'append
                (mapcar (lambda (f)
                          (let ((tree (or (and (eq (plist-get f :type) 'commit)
                                              (plist-get f :tree))
                                         (plist-get f :sha))))
                            (when tree (gitq--fetch-blobs-at tree nil filter))))
                        frames))))
      ('diff
       (let ((ref (plist-get node :ref)))
         (apply #'append
                (mapcar (lambda (f)
                          (let* ((sha   (plist-get f :sha))
                                 (other (or ref (format "%s^" sha)))
                                 (paths (gitq--git "diff-tree" "-r" "--name-only"
                                                   "--no-commit-id" other sha)))
                            (mapcar (lambda (p)
                                      (list :type 'diff :sha sha :path p
                                            :parent-sha other))
                                    paths)))
                        frames))))
      ('diff-hunks
       (apply #'append
              (mapcar (lambda (f)
                        (let* ((sha    (plist-get f :sha))
                               (parent (format "%s^" sha))
                               (text   (string-join
                                        (gitq--git "diff-tree" "-p" "--no-commit-id"
                                                   "-r" parent sha)
                                        "\n")))
                          (gitq--parse-diff-hunks text sha)))
                      frames)))
      ('history
       (apply #'append
              (mapcar (lambda (f)
                        (let* ((path (plist-get f :path))
                               (shas (gitq--git "log" "--follow" "--format=%H" "--" path)))
                          (delq nil
                                (mapcar (lambda (sha)
                                          (let ((c (gitq--fetch-commit sha)))
                                            (when c
                                              (append c (list :path path)))))
                                        shas))))
                      frames)))
      ('commit
       (delq nil
             (mapcar (lambda (f)
                       (gitq--fetch-commit (plist-get f :commit-sha)))
                     frames)))
      (_ (error "gitq: unknown morphism '%s'" m)))))

(defun gitq--parse-diff-hunks (diff-text commit-sha)
  "Parse DIFF-TEXT into a list of hunk frame plists for COMMIT-SHA."
  (let (hunks cur-path)
    (dolist (line (split-string diff-text "\n"))
      (cond
       ((string-match "^diff --git a/.+ b/\\(.+\\)$" line)
        (setq cur-path (match-string 1 line)))
       ((and cur-path
             (string-match
              "^@@ -[0-9,]+ \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" line))
        (let* ((start (string-to-number (match-string 1 line)))
               (count (if (match-string 2 line)
                          (string-to-number (match-string 2 line))
                        1)))
          (push (list :type 'hunk :path cur-path
                      :start-line start :end-line (+ start (max 0 (1- count)))
                      :commit-sha commit-sha)
                hunks)))))
    (nreverse hunks)))

(defun gitq--exec-where (frames node)
  "Filter FRAMES by the conditions in NODE."
  (let ((conds (plist-get node :conditions)))
    (seq-filter (lambda (f)
                  (cl-every (lambda (c) (gitq--eval-condition f c)) conds))
                frames)))

(defun gitq--frame-field (frame field)
  "Extract FIELD (symbol) from FRAME plist."
  (pcase field
    ('sha            (plist-get frame :sha))
    ('message        (plist-get frame :message))
    ('author         (or (plist-get frame :author) (plist-get frame :name)))
    ('email          (plist-get frame :email))
    ('date           (plist-get frame :date))
    ('path           (plist-get frame :path))
    ('name           (plist-get frame :name))
    ('branch         (plist-get frame :branch))
    ('parents-count  (length (plist-get frame :parents)))
    ('modified       (plist-get frame :modified))
    ('staged         (plist-get frame :staged))
    ('untracked      (plist-get frame :untracked))
    (_ (plist-get frame (intern (format ":%s" field))))))

(defun gitq--eval-condition (frame cond)
  "Return non-nil if FRAME satisfies COND plist."
  (let* ((field  (plist-get cond :field))
         (op     (plist-get cond :op))
         (value  (plist-get cond :value))
         (actual (gitq--frame-field frame field)))
    (pcase op
      ('==       (equal actual value))
      ('!=       (not (equal actual value)))
      ('>        (and (numberp actual) (numberp value) (> actual value)))
      ('<        (and (numberp actual) (numberp value) (< actual value)))
      ('>=       (and (numberp actual) (numberp value) (>= actual value)))
      ('<=       (and (numberp actual) (numberp value) (<= actual value)))
      ('contains (and (stringp actual) (stringp value)
                      (string-match-p (regexp-quote value) actual)))
      ('matches  (and (stringp actual) (stringp value)
                      (string-match-p value actual)))
      ('after    (gitq--date-op actual value #'>))
      ('before   (gitq--date-op actual value #'<))
      ('within   (gitq--date-within actual value))
      ('is       (if (eq value t) (not (null actual)) (equal actual value)))
      (_         (error "gitq: unknown where operator '%s'" op)))))

(defun gitq--date-op (date-str ref-str cmp)
  "Compare DATE-STR and REF-STR using CMP function, or return nil on error."
  (ignore-errors
    (funcall cmp
             (float-time (date-to-time date-str))
             (float-time (date-to-time ref-str)))))

(defun gitq--date-within (date-str period-str)
  "Return non-nil if DATE-STR falls within PERIOD-STR of now."
  (when (string-match "^\\([0-9]+\\) +\\(day\\|week\\|month\\|year\\)s?\\b" period-str)
    (let* ((n    (string-to-number (match-string 1 period-str)))
           (unit (match-string 2 period-str))
           (secs (* n (pcase unit ("day" 86400) ("week" 604800)
                              ("month" 2592000) ("year" 31536000) (_ 0))))
           (cutoff (- (float-time) secs)))
      (ignore-errors (>= (float-time (date-to-time date-str)) cutoff)))))

(defun gitq--exec-grep (frames node)
  "Grep blob/commit FRAMES for pattern in NODE, returning line frames."
  (let* ((pattern     (plist-get node :pattern))
         (regex       (plist-get node :regex))
         (path-filter (plist-get node :path-filter)))
    (apply #'append
           (mapcar (lambda (f)
                     (let* ((sha  (plist-get f :sha))
                            (args (append (list "grep" "-n" "--no-color"
                                               (if regex "-E" "-F")
                                               pattern sha)
                                          (when path-filter (list "--" path-filter)))))
                       (delq nil
                             (mapcar (lambda (line)
                                       (when (string-match
                                              "^[^:]+:\\([^:]+\\):\\([0-9]+\\):\\(.*\\)$"
                                              line)
                                         (list :type 'line :sha sha
                                               :path        (match-string 1 line)
                                               :line-number (string-to-number
                                                             (match-string 2 line))
                                               :content     (match-string 3 line)
                                               :commit-sha  sha)))
                                     (apply #'gitq--git args)))))
                   frames))))

(defun gitq--exec-pickaxe (frames node)
  "Filter commit FRAMES to those whose diffs match the pickaxe pattern in NODE."
  (let* ((pattern (plist-get node :pattern))
         (regex   (plist-get node :regex))
         (flag    (if regex "-G" "-S"))
         (shas    (delq nil (mapcar (lambda (f) (plist-get f :sha)) frames))))
    (when shas
      (let ((hits (apply #'gitq--git
                         (append (list "log" flag pattern "--format=%H" "--no-walk")
                                 shas))))
        (seq-filter (lambda (f) (member (plist-get f :sha) hits)) frames)))))

(defun gitq--exec-path (frames node)
  "Filter FRAMES to those whose :path matches the pattern in NODE."
  (let ((pattern (plist-get node :pattern)))
    (seq-filter (lambda (f)
                  (let ((p (plist-get f :path)))
                    (and p (gitq--path-matches p pattern))))
                frames)))

(defun gitq--exec-pick (frames node)
  "Project each frame in FRAMES to only the fields listed in NODE."
  (let ((fields (plist-get node :fields)))
    (mapcar (lambda (f)
              (let (proj)
                (dolist (field fields)
                  (setq proj (plist-put proj field (gitq--frame-field f field))))
                (cons :type (cons 'projection proj))))
            frames)))

(defun gitq--exec-sort (frames node)
  "Sort FRAMES by the field in NODE."
  (let ((field (plist-get node :field))
        (desc  (plist-get node :desc)))
    (sort (copy-sequence frames)
          (lambda (a b)
            (let ((va (or (gitq--frame-field a field) ""))
                  (vb (or (gitq--frame-field b field) "")))
              (if desc (string> va vb) (string< va vb)))))))

(defun gitq--exec-step (frames step)
  "Execute one pipeline STEP against FRAMES, returning new frame list."
  (let ((type (plist-get step :type)))
    (pcase type
      ('via     (gitq--exec-via frames step))
      ('where   (gitq--exec-where frames step))
      ('grep    (gitq--exec-grep frames step))
      ('pickaxe (gitq--exec-pickaxe frames step))
      ('path    (gitq--exec-path frames step))
      ('pick    (gitq--exec-pick frames step))
      ('take    (seq-take frames (plist-get step :n)))
      ('skip    (seq-drop frames (plist-get step :n)))
      ('first   (when frames (list (car frames))))
      ('last    (when frames (list (car (last frames)))))
      ('sort    (gitq--exec-sort frames step))
      (_        frames))))

;;; Terminal operations

(defun gitq--frame-commit-sha (frame)
  "Return the commit SHA for FRAME (direct or via :commit-sha)."
  (or (plist-get frame :commit-sha)
      (when (memq (plist-get frame :type) '(commit ref))
        (plist-get frame :sha))
      (plist-get frame :sha)))

;;; Results display

(defvar gitq-results-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'gitq-results-visit)
    (define-key m (kbd "b")   #'gitq-results-branch-off)
    (define-key m (kbd "c")   #'gitq-results-copy-sha)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for `gitq-results-mode'.")

(define-derived-mode gitq-results-mode special-mode "GitQ"
  "Major mode for displaying gitq pipeline results."
  :interactive nil
  (setq truncate-lines t))

(defun gitq--insert-frame (frame)
  "Insert a human-readable line for FRAME into the current buffer."
  (let ((type  (plist-get frame :type))
        (start (point)))
    (pcase type
      ('commit
       (let* ((sha   (plist-get frame :sha))
              (short (when sha (substring sha 0 (min 8 (length sha))))))
         (insert (propertize (or short "?") 'face 'magit-hash))
         (insert "  ")
         (let ((author (plist-get frame :author)))
           (when author
             (insert (propertize
                      (format "%-20s"
                              (substring author 0 (min 20 (length author))))
                      'face 'magit-log-author))))
         (let ((date (plist-get frame :date)))
           (when date
             (insert (propertize (substring date 0 (min 10 (length date)))
                                 'face 'magit-log-date))
             (insert "  ")))
         (insert (or (plist-get frame :message) ""))))
      ('blob
       (insert (propertize (or (plist-get frame :path) "?") 'face 'magit-filename)))
      ('ref
       (insert (propertize (or (plist-get frame :name) "?")
                           'face 'magit-branch-local))
       (when-let ((sha (plist-get frame :sha)))
         (insert "  ")
         (insert (propertize (substring sha 0 (min 8 (length sha))) 'face 'magit-hash))))
      ('worktree
       (insert (propertize (or (plist-get frame :path) "?") 'face 'magit-filename))
       (when-let ((b (plist-get frame :branch)))
         (insert "  ")
         (insert (propertize b 'face 'magit-branch-local))))
      ('line
       (insert (propertize (or (plist-get frame :path) "?") 'face 'magit-filename))
       (insert ":")
       (insert (propertize (number-to-string (or (plist-get frame :line-number) 0))
                           'face 'shadow))
       (insert ": ")
       (insert (or (plist-get frame :content) "")))
      ('hunk
       (insert (propertize (or (plist-get frame :path) "?") 'face 'magit-filename))
       (insert (format " lines %d-%d"
                       (or (plist-get frame :start-line) 0)
                       (or (plist-get frame :end-line) 0))))
      (_
       ;; projected or unknown — dump key:value pairs
       (let (first)
         (cl-loop for (k v) on frame by #'cddr
                  do (progn
                       (unless first (setq first t))
                       (insert (format "%s:%s " k v)))))))
    (put-text-property start (point) 'gitq-frame frame)
    (put-text-property start (point) 'gitq-sha (gitq--frame-commit-sha frame))
    (insert "\n")))

(defun gitq--render (frames pipeline-str)
  "Render FRAMES into the *gitq* results buffer and return that buffer."
  (with-current-buffer (get-buffer-create "*gitq*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (format "gitq: %s\n\n" pipeline-str)
                          'face 'font-lock-comment-face))
      (if frames
          (dolist (f frames) (gitq--insert-frame f))
        (insert "(no results)\n"))
      (gitq-results-mode)
      (goto-char (point-min)))
    (current-buffer)))

(defun gitq--display (frames pipeline-str)
  "Show FRAMES in the *gitq* results buffer, taking over the whole frame
and selecting its window."
  (pop-to-buffer (gitq--render frames pipeline-str) '(display-buffer-full-frame)))

(defun gitq--preview-display (frames pipeline-str)
  "Show FRAMES in the *gitq* results buffer without selecting its window.
Used to preview a pipeline still being typed in the minibuffer: results
appear right away, above the minibuffer, taking over the whole frame
\(display-buffer-full-frame\) rather than splitting it in half, without
taking focus away from typing.

`display-buffer-full-frame' does its work via `delete-other-windows',
which selects its target window as a side effect regardless of what
was selected before the call -- confirmed directly: it silently stole
window selection away from the active minibuffer read on every preview
tick, so the very next keystroke (e.g. a space right after
autocompleting a token) went to the now-selected *gitq* buffer instead
of the minibuffer. Restoring the previously selected window
immediately afterward keeps focus exactly where typing expects it."
  (let ((previously-selected (selected-window)))
    (display-buffer (gitq--render frames pipeline-str) '(display-buffer-full-frame))
    (when (window-live-p previously-selected)
      (select-window previously-selected))))

(defun gitq-results-visit ()
  "Visit the git object at point in the *gitq* buffer."
  (interactive nil gitq-results-mode)
  (let* ((frame (get-text-property (point) 'gitq-frame))
         (sha   (get-text-property (point) 'gitq-sha))
         (type  (plist-get frame :type)))
    (pcase type
      ('blob (when (and sha (fboundp 'magit-find-file))
               (magit-find-file sha (plist-get frame :path))))
      (_     (when (and sha (fboundp 'magit-show-commit))
               (magit-show-commit sha))))))

(defun gitq-results-branch-off ()
  "Create a branch from the commit at point in the *gitq* buffer."
  (interactive nil gitq-results-mode)
  (let ((sha (get-text-property (point) 'gitq-sha)))
    (unless sha (user-error "No commit at point"))
    (let ((name (read-string "Branch name: ")))
      (gitq--git "checkout" "-b" name sha)
      (when (fboundp 'magit-refresh) (magit-refresh))
      (message "gitq: created branch '%s'" name))))

(defun gitq-results-copy-sha ()
  "Copy the SHA at point to the kill ring."
  (interactive nil gitq-results-mode)
  (let ((sha (get-text-property (point) 'gitq-sha)))
    (if sha
        (progn (kill-new sha)
               (message "gitq: copied %s" (substring sha 0 (min 8 (length sha)))))
      (user-error "No SHA at point"))))

;;; Terminal dispatch

(defun gitq--apply-terminal (frames node pipeline-str)
  "Apply terminal operation from NODE to FRAMES."
  (pcase (plist-get node :op)
    ('show
     (gitq--display frames pipeline-str))
    ('copy
     (let ((sha (gitq--frame-commit-sha (car frames))))
       (if sha
           (progn (kill-new sha)
                  (message "gitq: copied %s" (substring sha 0 (min 8 (length sha)))))
         (user-error "gitq copy: no SHA in result"))))
    ('insert
     (let ((sha (gitq--frame-commit-sha (car frames))))
       (when sha (insert sha))))
    ('count
     (message "gitq: %d result(s)" (length frames)))
    ('branch-off
     (let* ((f    (car frames))
            (sha  (gitq--frame-commit-sha f))
            (name (or (plist-get node :name)
                      (read-string "Branch name: ")))
            (wt   (plist-get node :worktree)))
       (unless sha (user-error "gitq branch-off: no commit in result"))
       (if wt
           (gitq--git "worktree" "add" "-b" name wt sha)
         (gitq--git "checkout" "-b" name sha))
       (when (fboundp 'magit-refresh) (magit-refresh))
       (message "gitq: created branch '%s'" name)))
    ('amend
     (let ((no-edit  (plist-get node :no-edit))
           (msg      (plist-get node :message)))
       (cond
        (no-edit (gitq--git "commit" "--amend" "--no-edit"))
        (msg     (gitq--git "commit" "--amend" "-m" msg))
        (t (if (fboundp 'magit-commit-amend)
               (call-interactively #'magit-commit-amend)
             (gitq--git "commit" "--amend"))))
       (when (fboundp 'magit-refresh) (magit-refresh))))
    ('reword
     (let* ((f    (car frames))
            (sha  (gitq--frame-commit-sha f))
            (msg  (plist-get node :message)))
       (unless sha (user-error "gitq reword: no commit in result"))
       (if msg
           (when (fboundp 'git-branch-off--reword-apply)
             (git-branch-off--reword-apply sha msg))
         (when (fboundp 'git-branch-off-reword)
           (git-branch-off-reword sha)))))
    ('squash
     (let ((msg (plist-get node :message)))
       (message "gitq squash: %d commits%s — use git-branch-off-squash for full support"
                (length frames)
                (if msg (format " → \"%s\"" msg) ""))))
    ('remove
     (let* ((f   (car frames))
            (sha (gitq--frame-commit-sha f)))
       (unless sha (user-error "gitq remove: no commit in result"))
       (when (fboundp 'git-branch-off-remove)
         (git-branch-off-remove sha))))
    ('commit
     (let ((msg (plist-get node :message)))
       (if msg
           (progn
             (gitq--git "commit" "-m" msg)
             (when (fboundp 'magit-refresh) (magit-refresh)))
         (when (fboundp 'magit-commit-create)
           (call-interactively #'magit-commit-create)))))
    ('stage
     (when (fboundp 'magit-stage-modified)
       (magit-stage-modified)))
    ('mark
     (let* ((f     (car frames))
            (sha   (gitq--frame-commit-sha f))
            (label (plist-get node :label)))
       (when (and sha label)
         (gitq--git "notes" "add" "-m" label sha)
         (message "gitq: marked %s with '%s'"
                  (substring sha 0 (min 8 (length sha))) label))))
    (_
     (gitq--display frames pipeline-str))))

;;; Main entry points

(defvar gitq--history nil "Minibuffer history list for `gitq'.")

(defun gitq--exec-nodes (nodes)
  "Execute parsed pipeline NODES (a source, zero or more steps, optional terminal).
Returns (RESULT . TERMINAL-NODE-OR-NIL).  The terminal, if any, is
identified and stripped out but never applied here — callers decide
whether to run it for real (`gitq--apply-terminal') or ignore it
entirely for a read-only preview (`gitq--preview-frames')."
  (let* ((src-node (car nodes))
         (rest     (cdr nodes))
         (last     (car (last rest)))
         (is-term  (and last (eq (plist-get last :type) 'terminal)))
         (steps    (if is-term (butlast rest) rest))
         (terminal (when is-term last))
         (frames   (gitq--exec-source src-node))
         (result   (cl-reduce #'gitq--exec-step steps :initial-value frames)))
    (cons result terminal)))

;;; Flat-syntax pipeline parser (whitespace-separated stages, /terminal keywords)
;;
;; Grammar:
;;   pipeline ::= source step* terminal?
;;   source   ::= "commits" ["in" range-tokens] | "HEAD" | BRANCH
;;               | "branches" | "tags" | "refs" | "worktrees" | "blobs"
;;   step     ::= "via" MORPHISM | "where" conditions | "grep" PATTERN
;;               | "pickaxe" PATTERN ["regex"] | "path" GLOB
;;               | "pick" FIELD[,...] | "take" N | "skip" N
;;               | "first" | "last" | "sort" ["-"]FIELD
;;   terminal ::= "/show" | "/copy" | "/insert" | "/count" | "/branch-off" [NAME]
;;               | "/amend" ["no-edit"|MSG] | "/squash" [MSG] | "/reword" [MSG]
;;               | "/remove" | "/delete" | "/commit" [MSG] | "/stage"
;;               | "/mark" [LABEL] | "/worktree"
;;   conditions ::= condition ("," condition)*
;;   condition  ::= FIELD [OP value]
;;   FIELD      ::= one of `gitq--field-names' (bare word; unlike MORPHISM,
;;                  fields do not take a leading ".")
;;   value      ::= QUOTED | /REGEX/ | NUMBER | BARE-WORD (not a step keyword)
;;
;; Disambiguation rules:
;;   1. Terminals start with / and have no closing /  (/show not /show/).
;;      /regex/ literals have a closing / and cannot be terminals.
;;   2. Step keywords (via where grep pickaxe path pick take skip first last sort)
;;      always start a new stage; they are reserved and cannot appear as unquoted
;;      values. Use quotes when searching for these literal strings:
;;        where message contains "take"   (not: where message contains take)
;;   3. Former terminal identifiers (commit show count remove stage mark …) are
;;      now plain identifiers and can appear freely as where-clause values.
;;   4. In "commits in RANGE", range tokens are consumed until a step keyword,
;;      /terminal, or end of input. Branch names that are step keywords must be
;;      quoted.
;;   5. FIELD is a closed, validated set (`gitq--field-names'); `path' is both
;;      a field and a step keyword — see `gitq--field-names' docstring.

(defconst gitq--flat-step-keywords
  '("via" "where" "grep" "pickaxe" "path" "pick" "take" "skip" "first" "last" "sort")
  "Reserved step keywords in flat-syntax pipelines.
These always start a new stage; quote them when used as string values.")

(defconst gitq--field-names
  '(;; common, cross-frame-type fields
    "sha" "author" "email" "date" "message" "path" "name" "branch"
    "parents-count" "modified" "staged" "untracked"
    ;; present on specific frame types only (tree/ref/diff/hunk/line),
    ;; but referenceable via `gitq--frame-field''s generic fallback —
    ;; included here so `where'/`sort'/`pick' can validate and complete
    ;; them too, instead of only the "well-known" subset above
    "tree" "reftype" "detached" "mode" "parent-sha" "commit-sha"
    "start-line" "end-line" "line-number" "content")
  "The closed set of field names `where', `sort', and `pick' accept.
Used both for completion and for grammar validation: any field token
that is not a member of this list is a parse-time error, not a
silently-matches-nothing where-condition.  Note: `path' collides with
the reserved step keyword of the same name (the standalone `path GLOB'
step) — see `gitq--complete--enclosing-step' and `pick's field-list
loop, both of which are written to disambiguate this correctly.")

(defun gitq--tokenize-flat (str)
  "Tokenize a flat pipeline STR.
Like `gitq--tokenize' but distinguishes /command terminal tokens from
/pattern/ regex literals by looking for a matching closing slash."
  (let (tokens (i 0) (len (length str)))
    (while (< i len)
      (let ((c (aref str i)))
        (cond
         ((memq c '(?\s ?\t ?\n ?\r)) (setq i (1+ i)))
         ((eq c ?\")
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len) (not (eq (aref str i) ?\")))
              (when (eq (aref str i) ?\\) (setq i (1+ i)))
              (setq i (1+ i)))
            ;; Only consume the closing quote if one was actually found --
            ;; this runs on every keystroke via live completion, so an
            ;; in-progress, still-unterminated quote must never error here.
            (when (< i len) (setq i (1+ i)))
            (push (substring str s i) tokens)))
         ((eq c ?/)
          ;; Scan forward to see if there is a matching closing /, without
          ;; crossing into a later quoted string -- otherwise a terminal
          ;; argument like /branch-off "feature/x" misreads the / inside
          ;; the branch name as this token's own closing slash, swallowing
          ;; everything up to it (including the opening quote) as one
          ;; bogus regex-shaped token.
          ;; Found  → /pattern/ regex literal.
          ;; Absent → /command terminal token.
          (let ((j (1+ i)))
            (while (and (< j len)
                        (not (eq (aref str j) ?/))
                        (not (eq (aref str j) ?\")))
              (setq j (1+ j)))
            (if (and (< j len) (eq (aref str j) ?/))
                ;; Regex literal: consume up to and including closing /
                (progn (push (substring str i (1+ j)) tokens)
                       (setq i (1+ j)))
              ;; Command token: consume /alpha-chars
              (let ((s i))
                (setq i (1+ i))
                (while (and (< i len)
                            (let ((d (aref str i)))
                              (or (and (>= d ?a) (<= d ?z))
                                  (and (>= d ?A) (<= d ?Z))
                                  (and (>= d ?0) (<= d ?9))
                                  (memq d '(?- ?_)))))
                  (setq i (1+ i)))
                (push (substring str s i) tokens)))))
         ((eq c ?,) (push "," tokens) (setq i (1+ i)))
         ((and (< (1+ i) len)
               (member (substring str i (+ i 2)) '("==" "!=" ">=" "<=")))
          (push (substring str i (+ i 2)) tokens) (setq i (+ i 2)))
         ((memq c '(?> ?<)) (push (string c) tokens) (setq i (1+ i)))
         ;; Negated field name: -date (used in `sort -date'). Fields are
         ;; bare identifiers now (no leading dot), so this just consumes
         ;; the leading "-" plus a normal bare-word run.
         ((and (eq c ?-) (< (1+ i) len)
               (let ((d (aref str (1+ i))))
                 (or (and (>= d ?a) (<= d ?z)) (and (>= d ?A) (<= d ?Z)) (eq d ?_))))
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len)
                        (let ((d (aref str i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?- ?_ ?/ ?~ ?@ ?{ ?})))))
              (setq i (1+ i)))
            (push (substring str s i) tokens)))
         ((eq c ?.)
          (let ((s i))
            (setq i (1+ i))
            (while (and (< i len)
                        (let ((d (aref str i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?. ?- ?_ ?\[ ?\] ?* ?+))
                              (eq d #x2020))))
              (setq i (1+ i)))
            (push (substring str s i) tokens)))
         ((and (>= c ?0) (<= c ?9))
          (let ((s i))
            (while (and (< i len) (>= (aref str i) ?0) (<= (aref str i) ?9))
              (setq i (1+ i)))
            (push (substring str s i) tokens)))
         ((or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z)) (eq c ?_))
          (let ((s i))
            (while (and (< i len)
                        (let ((d (aref str i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?- ?_ ?/ ?~ ?@ ?{ ?})))))
              (setq i (1+ i)))
            (push (substring str s i) tokens)))
         (t (setq i (1+ i))))))
    (nreverse tokens)))

(defun gitq--flat-step-p (tok)
  "Return non-nil if TOK is a reserved step keyword in flat-syntax mode."
  (member tok gitq--flat-step-keywords))

(defun gitq--flat-terminal-p (tok)
  "Return non-nil if TOK is a /command terminal token (not a /regex/ literal)."
  (and (stringp tok)
       (> (length tok) 1)
       (eq (aref tok 0) ?/)
       ;; A /regex/ ends with /; a /command does not
       (not (eq (aref tok (1- (length tok))) ?/))))

(defun gitq--flat-boundary-p (tok)
  "Return non-nil if TOK is a stage boundary in flat-syntax mode."
  (or (null tok)
      (gitq--flat-step-p tok)
      (gitq--flat-terminal-p tok)))

(defun gitq--flat-parse-where (tokens)
  "Parse where-conditions from flat TOKENS, returning (node . remaining).
Step keywords and /terminals act as stage boundaries and are never consumed
as condition values.  FIELD tokens must be members of `gitq--field-names' —
anything else right after `where' or a comma is an error naming the bad
token, rather than silently ending the clause early or matching nothing
at run time."
  (unless (or (null tokens) (member (car tokens) gitq--field-names)
              (gitq--flat-boundary-p (car tokens)))
    (error "gitq: expected a field name after 'where', got '%s'" (car tokens)))
  (let (conditions)
    (while (and tokens (member (car tokens) gitq--field-names))
      (let* ((field     (intern (pop tokens)))
             (next      (car tokens)))
        (cond
         ;; Bare flag: next token is a boundary, comma, or another field
         ((or (null next) (equal next ",")
              (member next gitq--field-names)
              (gitq--flat-boundary-p next))
          (push (list :field field :op 'is :value t) conditions))
         ;; Operator present
         (t
          (let* ((op-tok (pop tokens))
                 (op     (intern op-tok))
                 (next2  (car tokens)))
            (cond
             ;; Step keyword immediately after an operator that requires a value:
             ;; this is always an error — the keyword must be quoted.
             ((gitq--flat-step-p next2)
              (error
               "gitq: '%s' requires a value; step keyword '%s' must be quoted: \"%s\""
               op-tok next2 next2))
             ;; No value: nil, comma, another field, or /terminal after operator
             ((or (null next2) (equal next2 ",")
                  (member next2 gitq--field-names)
                  (gitq--flat-terminal-p next2))
              (push (list :field field :op op :value t) conditions))
             ;; Normal value
             (t
              (let* ((val-tok (pop tokens))
                     (val     (cond
                               ((string-prefix-p "\"" val-tok) (gitq--unquote val-tok))
                               ((string-prefix-p "/" val-tok)  (gitq--unregex val-tok))
                               ((string-match-p "^[0-9]+$" val-tok)
                                (string-to-number val-tok))
                               (t val-tok))))
                (push (list :field field :op op :value val) conditions))))))))
      (when (equal (car tokens) ",")
        (pop tokens)
        (unless (and tokens (member (car tokens) gitq--field-names))
          (error "gitq: expected a field name after ',' in 'where', got '%s'"
                 (or (car tokens) "end of input")))))
    (cons (list :type 'where :conditions (nreverse conditions)) tokens)))

(defun gitq--flat-parse-source (tokens)
  "Parse source node from flat TOKENS, returning (node . remaining)."
  (let ((kw (pop tokens)))
    (cond
     ((member kw '("commits" "commit"))
      (if (equal (car tokens) "in")
          (let (range-parts)
            (pop tokens)                  ; consume "in"
            (while (and tokens (not (gitq--flat-boundary-p (car tokens))))
              (push (pop tokens) range-parts))
            (cons (list :type 'source :source 'commits
                        :range (apply #'concat (nreverse range-parts)))
                  tokens))
        (cons (list :type 'source :source 'commits :range nil) tokens)))
     ((equal kw "branches") (cons (list :type 'source :source 'branches) tokens))
     ((equal kw "tags")     (cons (list :type 'source :source 'tags)     tokens))
     ((member kw '("worktrees" "worktree"))
      (cons (list :type 'source :source 'worktree) tokens))
     ((equal kw "blobs")    (cons (list :type 'source :source 'blobs)    tokens))
     ((equal kw "refs")     (cons (list :type 'source :source 'refs)     tokens))
     (t (cons (list :type 'source :source 'ref :ref kw) tokens)))))

(defun gitq--flat-parse-via (tokens)
  "Parse via-step morphism from flat TOKENS, returning (node . remaining).
Handles the optional REF argument of .diff without consuming step keywords."
  (let* ((path (pop tokens))
         (node (cond
                ((equal path ".parent")   (list :type 'via :morphism 'parent))
                ((equal path ".parent*")  (list :type 'via :morphism 'parent :star t))
                ((equal path ".parent+")  (list :type 'via :morphism 'parent :plus t))
                ((string-match "^\\.parent\\[\\([0-9]+\\)\\]$" path)
                 (list :type 'via :morphism 'parent
                       :index (string-to-number (match-string 1 path))))
                ((or (equal path ".parent†") (equal path ".parent†"))
                 (list :type 'via :morphism 'parent-adjoint))
                ((equal path ".tree")     (list :type 'via :morphism 'tree))
                ((string-match "^\\.tree\\.entries\\(?:\\[\\(Blob\\|Tree\\)\\]\\)?$" path)
                 (list :type 'via :morphism 'tree-entries
                       :filter (when (match-string 1 path)
                                 (if (equal (match-string 1 path) "Blob") 'blob 'tree))))
                ((equal path ".tree.blobs")    (list :type 'via :morphism 'tree-entries :filter 'blob))
                ((equal path ".tree.subtrees") (list :type 'via :morphism 'tree-entries :filter 'tree))
                ((equal path ".diff")
                 ;; Optional REF: consume only if not a step keyword or /terminal
                 (let ((ref (when (and tokens
                                       (not (gitq--flat-boundary-p (car tokens)))
                                       (not (string-prefix-p "." (car tokens))))
                              (pop tokens))))
                   (list :type 'via :morphism 'diff :ref ref)))
                ((equal path ".diff.hunks") (list :type 'via :morphism 'diff-hunks))
                ((equal path ".history")    (list :type 'via :morphism 'history))
                ((equal path ".commit")     (list :type 'via :morphism 'commit))
                (t (error "gitq: unknown morphism '%s'" path)))))
    (cons node tokens)))

(defun gitq--flat-parse-step (tokens)
  "Parse one step node from flat TOKENS (first token must be a step keyword).
Returns (node . remaining)."
  (let ((kw (pop tokens)))
    (pcase kw
      ("via" (gitq--flat-parse-via tokens))
      ("where" (gitq--flat-parse-where tokens))
      ("grep"
       (let* ((pat-tok (pop tokens))
              (regex   (string-prefix-p "/" pat-tok))
              (pattern (if regex (gitq--unregex pat-tok) (gitq--unquote pat-tok))))
         ;; Inline "path" qualifier removed in flat mode — use a separate path step.
         (cons (list :type 'grep :pattern pattern :regex regex :path-filter nil)
               tokens)))
      ("pickaxe"
       (let* ((pat-tok (pop tokens))
              (regex   (or (string-prefix-p "/" pat-tok)
                           (equal (car tokens) "regex")))
              (pattern (if (string-prefix-p "/" pat-tok)
                           (gitq--unregex pat-tok)
                         (gitq--unquote pat-tok))))
         (when (equal (car tokens) "regex") (pop tokens))
         (cons (list :type 'pickaxe :pattern pattern :regex regex) tokens)))
      ("path"
       (cons (list :type 'path :pattern (gitq--unquote (pop tokens))) tokens))
      ("pick"
       ;; Driven by field-list membership (plus comma), not the generic
       ;; step-keyword boundary check: `path' is both a reserved step
       ;; keyword (the standalone `path GLOB' step) and a legitimate
       ;; field (blob/diff/hunk/line frames all carry a :path key), so
       ;; `pick path, author' must recognize `path' as a field here even
       ;; though it is also a step keyword everywhere else.
       (let (fields)
         (while (and tokens (or (equal (car tokens) ",")
                                (member (car tokens) gitq--field-names)))
           (let ((tok (pop tokens)))
             (unless (equal tok ",")
               (push (intern tok) fields))))
         (cons (list :type 'pick :fields (nreverse fields)) tokens)))
      ("take"
       (cons (list :type 'take :n (string-to-number (pop tokens))) tokens))
      ("skip"
       (cons (list :type 'skip :n (string-to-number (pop tokens))) tokens))
      ("first" (cons (list :type 'first) tokens))
      ("last"  (cons (list :type 'last)  tokens))
      ("sort"
       (let* ((f    (pop tokens))
              (neg  (string-prefix-p "-" f))
              (name (if neg (substring f 1) f)))
         (unless (member name gitq--field-names)
           (error "gitq: unknown field '%s' after 'sort'" name))
         (cons (list :type 'sort :field (intern name) :desc neg) tokens)))
      (_ (error "gitq: unknown step keyword '%s'" kw)))))

(defun gitq--flat-parse-terminal (tok tokens)
  "Parse /terminal token TOK using TOKENS for optional arguments.
Returns (node . remaining)."
  (let ((op-str (substring tok 1)))  ; strip leading /
    (cons (gitq--parse-terminal op-str tokens)
          ;; Terminals consume 0-2 tokens from `tokens' internally via pop,
          ;; but gitq--parse-terminal uses its own local copy of the list.
          ;; Since terminals must be last, pass nil as remaining.
          nil)))

(defun gitq--parse-flat (pipeline-str)
  "Parse a flat pipeline PIPELINE-STR into a list of AST node plists.
Uses whitespace as the stage separator with /terminal syntax.
No pipe character required."
  (let* ((tokens (gitq--tokenize-flat (string-trim pipeline-str)))
         nodes)
    (unless tokens (error "gitq: empty pipeline"))
    ;; Parse source (first stage)
    (let* ((result (gitq--flat-parse-source tokens)))
      (push (car result) nodes)
      (setq tokens (cdr result)))
    ;; Parse steps and terminal
    (while tokens
      (let ((tok (car tokens)))
        (cond
         ((gitq--flat-terminal-p tok)
          (let* ((result (gitq--flat-parse-terminal tok (cdr tokens))))
            (push (car result) nodes)
            (setq tokens nil)))     ; terminal is always last
         ((gitq--flat-step-p tok)
          (let* ((result (gitq--flat-parse-step tokens)))
            (push (car result) nodes)
            (setq tokens (cdr result))))
         (t
          (error "gitq: expected step keyword or /terminal, got '%s'" tok)))))
    (nreverse nodes)))

;;; Completion

(defconst gitq--complete-source-keywords
  '("commits" "branches" "tags" "refs" "worktrees" "blobs" "HEAD")
  "Source keywords offered at the start of a pipeline.")

(defconst gitq--complete-morphisms
  '(".parent" ".parent*" ".parent+" ".tree" ".tree.blobs" ".tree.subtrees"
    ".tree.entries" ".tree.entries[Blob]" ".tree.entries[Tree]"
    ".diff" ".diff.hunks" ".history" ".commit")
  "Morphism paths offered after `via'.")

(defconst gitq--complete-where-operators
  '("==" "!=" ">" "<" ">=" "<=" "contains" "matches" "after" "before" "within" "is")
  "Operators offered after a field name in a where clause.")

(defconst gitq--complete-terminals
  '("/show" "/copy" "/insert" "/count" "/branch-off" "/amend"
    "/squash" "/reword" "/remove" "/delete" "/commit" "/stage" "/mark" "/worktree")
  "Terminal /command keywords.")

(defconst gitq--complete-date-within-examples
  '("1 day" "3 days" "1 week" "2 weeks" "1 month" "3 months" "6 months" "1 year")
  "Example duration values for the `within' where-operator on `date'.
`within' takes a duration (\"N day/week/month/year(s)\"), not a literal
date, so it gets its own candidate set instead of `date's usual list
of real commit dates.")

(defconst gitq--complete-descriptions
  '(;; sources
    ("commits"   . "commits reachable from HEAD")
    ("branches"  . "local branch refs")
    ("tags"      . "tag refs")
    ("refs"      . "all refs (branches, tags, ...)")
    ("worktrees" . "linked worktrees")
    ("blobs"     . "blob/tree entries under HEAD's tree")
    ("HEAD"      . "the current commit")
    ("in"        . "restrict commits to a revision range")
    ;; steps
    ("via"     . "traverse a morphism (parent, tree, diff, ...)")
    ("where"   . "filter by field conditions")
    ("grep"    . "search blob/commit content for a pattern")
    ("pickaxe" . "filter commits whose diff adds/removes a pattern")
    ;; "path" is both the standalone glob-filter step and the file-path
    ;; field (blob/diff/hunk/line frames) -- this lookup is a flat string
    ;; match with no context, so one entry has to cover both meanings.
    ("path"    . "path glob step, or the file-path field")
    ("pick"    . "project onto specific fields")
    ("take"    . "keep the first N results")
    ("skip"    . "drop the first N results")
    ("first"   . "keep only the first result")
    ("last"    . "keep only the last result")
    ("sort"    . "sort by field (prefix with - for descending)")
    ;; morphisms
    (".parent"             . "first parent commit")
    (".parent*"            . "all reachable ancestors, inclusive")
    (".parent+"            . "all reachable ancestors, exclusive")
    (".tree"               . "the commit's tree")
    (".tree.blobs"         . "blob entries in the tree")
    (".tree.subtrees"      . "subtree entries in the tree")
    (".tree.entries"       . "all tree entries")
    (".tree.entries[Blob]" . "blob entries only")
    (".tree.entries[Tree]" . "subtree entries only")
    (".diff"               . "paths changed vs. parent (or REF)")
    (".diff.hunks"         . "line ranges changed vs. parent")
    (".history"            . "commits that touched this path")
    (".commit"             . "resolve to the referenced commit")
    ;; field names
    ("sha"           . "commit SHA")
    ("author"        . "author name")
    ("email"         . "author email")
    ("date"          . "commit date")
    ("message"       . "commit message")
    ("name"          . "ref/branch name")
    ("branch"        . "worktree's branch")
    ("parents-count" . "number of parents")
    ("modified"      . "has modified/unstaged changes")
    ("staged"        . "has staged changes")
    ("untracked"     . "has untracked files")
    ;; field names present on specific frame types only
    ("tree"        . "commit's tree SHA")
    ("reftype"     . "ref kind (branch or tag)")
    ("detached"    . "worktree HEAD is detached")
    ("mode"        . "tree entry file mode")
    ("parent-sha"  . "the ref/SHA a diff was compared against")
    ("commit-sha"  . "commit a hunk/grep line belongs to")
    ("start-line"  . "hunk's first changed line")
    ("end-line"    . "hunk's last changed line")
    ("line-number" . "grep match's line number")
    ("content"     . "grep match's line content")
    ;; where operators
    ("=="       . "equals")
    ("!="       . "not equals")
    (">"        . "greater than")
    ("<"        . "less than")
    (">="       . "greater or equal")
    ("<="       . "less or equal")
    ("contains" . "substring match")
    ("matches"  . "regex match")
    ("after"    . "date is after value")
    ("before"   . "date is before value")
    ("within"   . "date is within \"N day/week/month/year(s)\"")
    ("is"       . "boolean flag is true")
    ;; terminals
    ("/show"       . "display results in the *gitq* buffer")
    ("/copy"       . "copy the SHA of the first result")
    ("/insert"     . "insert the SHA of the first result at point")
    ("/count"      . "show the result count")
    ("/branch-off" . "create a branch from the first result")
    ("/amend"      . "amend HEAD with the first result")
    ("/squash"     . "squash results into one commit")
    ("/reword"     . "reword the first result's commit message")
    ("/remove"     . "remove the first result's commit")
    ("/delete"     . "delete the first result's commit")
    ("/commit"     . "create a commit")
    ("/stage"      . "stage modified files")
    ("/mark"       . "attach a git note label")
    ("/worktree"   . "add a worktree")
    ("no-edit"     . "reuse HEAD's existing commit message"))
  "Short descriptions shown as completion annotations for gitq tokens.")

(defun gitq--complete-refs ()
  "Return local branch and tag names, for contexts expecting a ref."
  (ignore-errors
    (append (gitq--git "branch" "--format=%(refname:short)")
            (gitq--git "tag" "--list"))))

(defun gitq--complete--enclosing-step (ctx)
  "Return the most recent step keyword in CTX, or nil.
CTX is the list of fully-typed tokens so far.  Comma-separated lists
(`where' conditions, `pick' fields) can put arbitrarily many tokens
between the stage keyword that opened them and the token currently
being completed, so callers that need to know \"which stage is this
token part of\" (e.g. deciding whether a field takes a where-operator
next) should use this instead of just looking at the immediately
preceding token.

Walks CTX in order rather than just taking the last step-keyword match,
because `path' is both a step keyword (the standalone `path GLOB' step)
and a field name (blob/diff/hunk/line frames' path). When `path'
appears right after `where'/`pick', a comma, or another field name
(fields may chain without commas too), it is a field reference
continuing that stage, not a fresh `path' step."
  (let (enclosing)
    (dotimes (i (length ctx))
      (let ((tok (nth i ctx)))
        (when (and (member tok gitq--flat-step-keywords)
                   (not (and (member tok gitq--field-names)
                             (member enclosing '("where" "pick"))
                             (or (member (nth (1- i) ctx) '("where" "pick" ","))
                                 (member (nth (1- i) ctx) gitq--field-names)))))
          (setq enclosing tok))))
    enclosing))

(defun gitq--complete-where-values (field op)
  "Return completion candidates for a where-condition's value, or nil.
FIELD is the preceding field token; OP is the where-operator that was
just typed.  Returns nil (free text, no candidates) for fields with no
natural, git-derivable value domain: `message' (arbitrary, often
multi-line text), `parents-count' (an arbitrary integer), and the
boolean flags `modified'/`staged'/`untracked' (used as bare flags — a
literal \"true\"/\"false\" string would never actually match, since the
parser compares values with `equal', so offering either would be
actively misleading)."
  (cond
   ((and (equal field "date") (equal op "within"))
    gitq--complete-date-within-examples)
   ((equal field "author")
    (ignore-errors (delete-dups (gitq--git "log" "--format=%an" "--all"))))
   ((equal field "email")
    (ignore-errors (delete-dups (gitq--git "log" "--format=%ae" "--all"))))
   ((equal field "date")
    (ignore-errors (delete-dups (gitq--git "log" "--format=%ai" "--all"))))
   ((member field '("sha" "commit-sha"))
    (ignore-errors (delete-dups (gitq--git "log" "--format=%h" "--all"))))
   ((equal field "path")
    (ignore-errors (delete-dups (gitq--git "log" "--all" "--name-only" "--format="))))
   ((member field '("name" "branch"))
    (gitq--complete-refs))))

(defun gitq--complete-candidates (input)
  "Return a list of completion candidates for the pipeline string INPUT.
INPUT is everything typed so far; completions extend the last partial word."
  (let* ((trimmed    (string-trim-right input))
         (trailing   (not (equal trimmed input)))  ; trailing whitespace?
         (tokens     (gitq--tokenize-flat trimmed))
         ;; In-progress partial word (nil when trailing whitespace)
         (partial    (unless trailing (car (last tokens))))
         ;; Tokens that are fully typed
         (ctx        (if trailing tokens (butlast tokens)))
         (n          (length ctx))
         (last-ctx   (when (> n 0) (nth (1- n) ctx)))
         (prev-ctx   (when (> n 1) (nth (- n 2) ctx))))
    (cond
     ;; Start of pipeline → source keywords
     ((= n 0)
      gitq--complete-source-keywords)

     ;; After "commits" at position 1 → "in" or steps/terminals
     ((and (= n 1) (equal last-ctx "commits"))
      (cons "in" (append gitq--flat-step-keywords gitq--complete-terminals)))

     ;; After "commits in" → branch and tag names from git
     ((and (equal last-ctx "in") (equal prev-ctx "commits"))
      (gitq--complete-refs))

     ;; After "via" → morphisms
     ((equal last-ctx "via")
      gitq--complete-morphisms)

     ;; After ".diff" (the one morphism with an optional trailing REF
     ;; argument) → offer refs, but also let the user skip straight to
     ;; a step/terminal since the REF is optional.
     ((and (equal last-ctx ".diff") (equal prev-ctx "via"))
      (append (gitq--complete-refs) gitq--flat-step-keywords gitq--complete-terminals))

     ;; After "where" or "," (start of another condition) → field names
     ((or (equal last-ctx "where") (equal last-ctx ","))
      gitq--field-names)

     ;; After a field that is part of a `where' clause → where
     ;; operators.  `sort'/`pick' fields never take one; `path' is also
     ;; a field name but is excluded from THIS check anyway, since the
     ;; enclosing-step guard already requires "where" specifically.
     ((and last-ctx (member last-ctx gitq--field-names)
           (equal (gitq--complete--enclosing-step ctx) "where"))
      gitq--complete-where-operators)

     ;; After "sort" → field names with optional "-" negation prefix
     ((equal last-ctx "sort")
      (append gitq--field-names
              (mapcar (lambda (f) (concat "-" f)) gitq--field-names)))

     ;; After "pick" or pick-comma → field names
     ((or (equal last-ctx "pick")
          (and (equal last-ctx ",") (member "pick" ctx)))
      gitq--field-names)

     ;; After a where-operator → dynamic values (authors, dates, paths,
     ;; refs, sha's, ...) — see `gitq--complete-where-values'.
     ((member last-ctx gitq--complete-where-operators)
      (gitq--complete-where-values (when (> n 1) (nth (- n 2) ctx)) last-ctx))

     ;; After a terminal: only its own optional argument (if any) may
     ;; follow — never more steps/terminals, since a terminal always
     ;; ends the pipeline.
     ((gitq--flat-terminal-p last-ctx)
      (when (equal last-ctx "/amend") '("no-edit")))

     ;; Otherwise → step keywords + terminals, plus "," to continue a
     ;; still-open where/pick comma-list.
     (t (append (and (member (gitq--complete--enclosing-step ctx) '("where" "pick")) '(","))
                gitq--flat-step-keywords gitq--complete-terminals)))))

(defun gitq--current-token (string)
  "Return the in-progress partial token at the end of STRING, or \"\".
Trailing whitespace in STRING means the previous token is already
complete and a new (empty) token is starting."
  (let ((trimmed (string-trim-right string)))
    (if (equal trimmed string)
        (or (car (last (gitq--tokenize-flat trimmed))) "")
      "")))

(defun gitq--affixate (candidates)
  "Return CANDIDATES as (CAND PREFIX SUFFIX) triples for `completing-read'.
SUFFIX is a short description from `gitq--complete-descriptions'.  This
is supplied directly in the completion metadata (rather than left for
Marginalia's category-based classifiers, which have nothing to match
against a bespoke category like ours) so decorations show up with any
`completing-read' front-end — Vertico, Marginalia, or vanilla."
  (mapcar (lambda (c)
            (let* ((key  (if (string-prefix-p "-" c) (substring c 1) c))
                   (desc (cdr (assoc key gitq--complete-descriptions))))
              (list c ""
                    (if desc
                        (propertize (concat "  " desc)
                                    'face 'completions-annotations)
                      ""))))
          candidates))

(defun gitq--token-kind (cand)
  "Return a short category label for completion candidate CAND, or nil.
Reflects gitq's own categorical grammar by checking which fixed
candidate set CAND belongs to: a source keyword, a step keyword, a
morphism path, a field name (optionally `sort'-negated with a leading
\"-\"), a where-operator, or a terminal /command."
  (let ((key (if (string-prefix-p "-" cand) (substring cand 1) cand)))
    (cond
     ((or (equal key "in") (member key gitq--complete-source-keywords)) "source")
     ((member key gitq--flat-step-keywords)       "step")
     ((member key gitq--complete-morphisms)        "morphism")
     ((member key gitq--field-names)      "field")
     ((member key gitq--complete-where-operators)  "operator")
     ((member key gitq--complete-terminals)        "terminal"))))

(with-eval-after-load 'marginalia
  (defvar marginalia-separator)
  (defun gitq--marginalia-annotate (cand)
    "Marginalia annotator for gitq completion candidate CAND.
Registered under the `gitq-token' category so Marginalia users get its
column alignment instead of the plain `gitq--affixate' fallback used
by everyone else.

Deliberately NOT built via the `marginalia--fields'/`marginalia--field'
macros that every built-in annotator uses (e.g. `marginalia-annotate-file')
— those are only visible once Marginalia is actually loaded, which here
only ever happens at runtime via this very `with-eval-after-load'.  A
normal package build (e.g. straight.el's isolated per-package byte-
compilation, which is exactly what triggered this) compiles this file
with Marginalia NOT loaded, so the byte-compiler cannot see they are
macros and instead compiles each field spec as a literal function call
— `(kind :face ...)' becomes a call to a function named `kind', which
does not exist, so the very first annotation attempt fails with
`(void-function kind)'.  Confirmed by byte-compiling this file in
isolation and loading the resulting .elc against a real Marginalia.

This version only reads the public `marginalia-separator' variable and
applies the same `\\='marginalia--align' text property protocol that
`marginalia--align' (Marginalia's own alignment pass) looks for — all
plain runtime data (`propertize'/`concat'/`format'), so there is no
macro expansion left to go stale at compile time.

Two fields are shown: `gitq--token-kind' (padded to a fixed width, so
the second field lines up too) and the description from
`gitq--complete-descriptions'."
    (let* ((key  (if (string-prefix-p "-" cand) (substring cand 1) cand))
           (kind (gitq--token-kind cand))
           (desc (cdr (assoc key gitq--complete-descriptions))))
      (when (or kind desc)
        (concat
         (propertize " " 'marginalia--align t)
         marginalia-separator
         (propertize (format "%-10s" (or kind "")) 'face 'marginalia-type)
         marginalia-separator
         (propertize (or desc "") 'face 'marginalia-documentation)))))
  (add-to-list 'marginalia-annotators
               '(gitq-token gitq--marginalia-annotate builtin none)))

(defun gitq--completion-table (string predicate action)
  "Dynamic `completing-read' collection table for a growing gitq pipeline.
Only the in-progress final token of STRING is completed; earlier tokens
are fixed context.  Candidates for that token come from
`gitq--complete-candidates', which derives them from gitq's categorical
step grammar — so Vertico shows the right set of keywords, morphisms,
fields, operators, or terminals for the current position, and refreshes
it live as each token is finished and the next one begins.  Candidates
are annotated via `gitq--affixate'."
  (cond
   ((eq action 'metadata)
    '(metadata (category . gitq-token)
               (affixation-function . gitq--affixate)))
   ((eq (car-safe action) 'boundaries)
    (cons 'boundaries
          (cons (- (length string) (length (gitq--current-token string))) 0)))
   (t
    (complete-with-action action
                          (gitq--complete-candidates string)
                          (gitq--current-token string)
                          predicate))))

;;; Live preview

(defcustom git-branch-off-gitq-preview-debounce 0.2
  "Seconds of no further input change before gitq (re)previews results."
  :type 'number :group 'git-branch-off)

(defun gitq--preview-frames (input)
  "Return (:ok . FRAMES) if INPUT is a complete pipeline, else nil.
Parses INPUT with `gitq--parse-flat' and executes the source and steps
(via `gitq--exec-nodes'), discarding any terminal — a terminal keyword
is recognized as ending the pipeline, but its action (branch-off,
amend, commit, ...) is never applied here, since this only ever
previews results while the pipeline is still being typed.

Returns nil, with no side effects, if INPUT does not currently parse
or execute cleanly (still mid-token, an unknown keyword, a missing
argument, not inside a repo, ...); the caller should leave whatever
was previously shown as-is in that case.  The (:ok . FRAMES) wrapper
distinguishes \"not ready yet\" (nil) from \"ready, and matched zero
results\" (FRAMES is an empty list) — both are otherwise nil.

A source token gitq doesn't recognize as a keyword (\"commits\", \"HEAD\",
...) parses as a literal ref/branch/commit name instead (see
`gitq--flat-parse-source').  That fallback matches genuine ref lookups,
but it also matches every partial word on the way to typing a real
keyword — e.g. \"commi\" parses just fine as an (unresolvable) ref
while the user is still typing \"commits\".  Left alone, that would
flash an empty \"(no results)\" preview on each such keystroke.  So a
`ref' source is only treated as ready once it actually resolves; final
command execution (`gitq') is untouched and still shows \"(no
results)\" for a genuinely nonexistent ref."
  (ignore-errors
    (let* ((default-directory (gitq--toplevel))
           (nodes    (gitq--parse-flat input))
           (src-node (car nodes)))
      (unless (and (eq (plist-get src-node :source) 'ref)
                   (not (gitq--fetch-commit (plist-get src-node :ref))))
        (cons :ok (car (gitq--exec-nodes nodes)))))))

(defun gitq--read-pipeline (prompt)
  "Read a gitq pipeline with `completing-read', driven by Vertico.
Backed by `gitq--completion-table', so the full candidate set for the
current pipeline position is shown as soon as the prompt opens, and
live-updates as each token is completed and the next one begins.

While typing, on a short debounce (`git-branch-off-gitq-preview-debounce'),
the current input is parsed via `gitq--preview-frames'.  As soon as it
parses into a complete pipeline that also executes without error, the
resulting frames are shown read-only in the *gitq* buffer via
`gitq--preview-display', which does not select that buffer's window —
focus stays in the minibuffer so typing is uninterrupted.  Pressing RET
hands the final string back to the caller, which runs the pipeline for
real, terminal included."
  (let ((mb-buffer  nil)
        (last-input nil)
        (timer      nil))
    (cl-labels
        ((tick ()
           (when (buffer-live-p mb-buffer)
             (with-current-buffer mb-buffer
               (let ((input (minibuffer-contents-no-properties)))
                 (unless (equal input last-input)
                   (setq last-input input)
                   (pcase (gitq--preview-frames input)
                     (`(:ok . ,frames) (gitq--preview-display frames input))))))))
         (schedule ()
           (when timer (cancel-timer timer))
           (setq timer (run-with-timer git-branch-off-gitq-preview-debounce nil #'tick)))
         (setup ()
           (setq mb-buffer (current-buffer))
           (add-hook 'post-command-hook #'schedule nil t)))
      (unwind-protect
          (minibuffer-with-setup-hook #'setup
            (completing-read prompt #'gitq--completion-table nil nil nil 'gitq--history))
        (when timer (cancel-timer timer))))))

;;;###autoload
(defun gitq (pipeline)
  "Execute a GitQ PIPELINE: a whitespace-separated query over git's object graph.

PIPELINE syntax:  source [step...] [/terminal]

Sources:   commits [in RANGE]  HEAD  BRANCH  branches  tags  refs  worktrees  blobs
Steps:     via MORPHISM  where COND[,COND...]  grep PATTERN  pickaxe PATTERN
           path GLOB  pick FIELD[,...]  take N  skip N  first  last  sort [-]FIELD
Terminals: /show  /copy  /insert  /count  /branch-off [NAME]  /amend [no-edit|MSG]
           /squash [MSG]  /reword [MSG]  /remove  /delete  /commit [MSG]
           /stage  /mark [LABEL]

FIELD is a closed, validated set (see `gitq--field-names'); referencing
an unknown field is a parse-time error. Unlike MORPHISM, FIELD has no
leading \".\" (e.g. `where author == \"alice\"', not `where .author ...').

Step keywords are reserved: quote them when used as values.
  CORRECT:  commits where message contains \"take\" take 5 /show
  WRONG:    commits where message contains take take 5 /show  (error)

Context-aware candidates for the current token appear as soon as the
minibuffer opens (via Vertico or any other `completing-read' UI), and
update live as each token is completed and the next one begins.  A
read-only preview of the source and steps (ignoring any terminal) also
appears in the *gitq* buffer as soon as they parse and execute
cleanly, without taking focus away from the minibuffer.

Examples:
  (gitq \"commits take 10 /show\")
  (gitq \"commits where author contains \\\"alice\\\" take 5 /count\")
  (gitq \"HEAD via .parent* where message contains \\\"fix\\\" /show\")
  (gitq \"commits in main..HEAD sort -date /show\")"
  (interactive (list (gitq--read-pipeline "gitq> ")))
  (let* ((default-directory (gitq--toplevel))
         (exec     (gitq--exec-nodes (gitq--parse-flat pipeline)))
         (result   (car exec))
         (terminal (cdr exec)))
    (if terminal
        (gitq--apply-terminal result terminal pipeline)
      (gitq--display result pipeline))))

(provide 'git-branch-off-gitq)
;;; git-branch-off-gitq.el ends here
