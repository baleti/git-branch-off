;;; git-branch-off-gitq.el --- GitQ: categorical query language for git  -*- lexical-binding: t; -*-

;; Provides `gitq': a pipeline query language for navigating git's object graph.
;; Syntax: source step step terminal (whitespace-separated, terminals start with /)
;; Example: (gitq "commits where author contains \"alice\" take 5 /show")

(require 'cl-lib)

;;; Git execution layer

(defun gitq--git (&rest args)
  "Run git with ARGS; return output lines as a list of non-empty strings.
Stderr is discarded, not mixed into the captured buffer -- otherwise a
git error message (e.g. an invalid revision) gets split into lines and
silently returned as if it were real data."
  (if (fboundp 'magit-git-lines)
      (apply #'magit-git-lines args)
    (let ((buf (generate-new-buffer " *gitq-git*")))
      (unwind-protect
          (progn
            (apply #'call-process "git" nil (list buf nil) nil args)
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

(defun gitq--terminal--simple (op)
  "Return a terminal parser for OP that accepts no arguments."
  (lambda (tokens kw)
    (gitq--expect-no-more tokens kw)
    (list :type 'terminal :op op)))

(defun gitq--terminal--optional-msg (op)
  "Return a terminal parser for OP taking one optional quoted message."
  (lambda (tokens kw)
    (let ((msg (when (and tokens (string-prefix-p "\"" (car tokens)))
                 (gitq--unquote (car tokens)))))
      (gitq--expect-no-more (if msg (cdr tokens) tokens) kw)
      (list :type 'terminal :op op :message msg))))

(defun gitq--terminal--parse-branch-off (tokens kw)
  "Parse /branch-off [NAME] [worktree PATH] from TOKENS."
  (let* ((name (when (and tokens (string-prefix-p "\"" (car tokens)))
                 (gitq--unquote (pop tokens))))
         (wt   (when (equal (car tokens) "worktree")
                 (pop tokens)
                 (gitq--unquote (pop tokens)))))
    (gitq--expect-no-more tokens kw)
    (list :type 'terminal :op 'branch-off :name name :worktree wt)))

(defun gitq--terminal--parse-amend (tokens kw)
  "Parse /amend [no-edit|MSG] from TOKENS."
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

(defun gitq--terminal--parse-mark (tokens kw)
  "Parse /mark [LABEL] from TOKENS."
  (gitq--expect-no-more (cdr tokens) kw)
  (list :type 'terminal :op 'mark :label (when tokens (gitq--unquote (car tokens)))))

(defun gitq--terminal--parse-worktree (tokens kw)
  "Parse /worktree [PATH] from TOKENS."
  (let ((path (when (and tokens (string-prefix-p "\"" (car tokens)))
                (gitq--unquote (pop tokens)))))
    (gitq--expect-no-more tokens kw)
    (list :type 'terminal :op 'worktree :path path)))

(defconst gitq--terminals
  (list
   (cons "show"       (gitq--terminal--simple 'show))
   (cons "copy"       (gitq--terminal--simple 'copy))
   (cons "insert"     (gitq--terminal--simple 'insert))
   (cons "count"      (gitq--terminal--simple 'count))
   (cons "remove"     (gitq--terminal--simple 'remove))
   ;; /delete is a true alias of /remove: it parses to the same op, so
   ;; it can never again parse successfully and then fall through the
   ;; executor to a silent no-op (which is what it used to do).
   (cons "delete"     (gitq--terminal--simple 'remove))
   (cons "stage"      (gitq--terminal--simple 'stage))
   (cons "branch-off" #'gitq--terminal--parse-branch-off)
   (cons "amend"      #'gitq--terminal--parse-amend)
   (cons "squash"     (gitq--terminal--optional-msg 'squash))
   (cons "reword"     (gitq--terminal--optional-msg 'reword))
   (cons "commit"     (gitq--terminal--optional-msg 'commit))
   (cons "mark"       #'gitq--terminal--parse-mark)
   (cons "worktree"   #'gitq--terminal--parse-worktree))
  "The terminal registry: NAME -> parser (TOKENS KW) -> terminal node.
Single source of truth for what terminals exist — the completion
candidate list derives from it, so completion can never again offer a
terminal the parser rejects (/worktree was completable, documented,
and listed in the test suite, yet unparseable, before this table
existed).")

(defun gitq--parse-terminal (kw tokens)
  "Parse terminal operation KW with remaining TOKENS via `gitq--terminals'."
  (let ((parser (cdr (assoc kw gitq--terminals))))
    (unless parser
      (error "gitq: unknown terminal operation '%s' (expected one of: %s)"
             kw (mapconcat #'car gitq--terminals ", ")))
    (funcall parser tokens kw)))

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

(defvar gitq--morphisms)                ; registry; defined with the parser below

(defun gitq--exec-via (frames node)
  "Traverse the morphism in NODE from FRAMES, via the morphism registry.
Dispatches to the `:exec' function registered in `gitq--morphisms' for
the node's `:morphism' symbol — the same table the parser and the
type checker read, so the three can never disagree about which
morphisms exist."
  (let* ((m    (plist-get node :morphism))
         (spec (alist-get m gitq--morphisms)))
    (unless spec (error "gitq: internal error: unregistered morphism '%s'" m))
    (funcall (plist-get spec :exec) frames node)))

(defun gitq--via-parent (frames node)
  "Parent morphism: first/indexed/all parents, or */+ ancestor closure."
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

(defun gitq--via-parent-adjoint (frames _node)
  "Adjoint of parent: the commits whose parent is in FRAMES (children-of)."
  (let* ((target-shas (mapcar (lambda (f) (plist-get f :sha)) frames))
         (all (gitq--fetch-commits)))
    (seq-filter (lambda (c)
                  (seq-some (lambda (p) (member p target-shas))
                            (plist-get c :parents)))
                all)))

(defun gitq--via-tree (frames _node)
  "Tree morphism: each commit's tree object."
  (delq nil
        (mapcar (lambda (f)
                  (let ((tree (plist-get f :tree)))
                    (when tree (list :type 'tree :sha tree))))
                frames)))

(defun gitq--via-tree-entries (frames node)
  "Tree-entries morphism: blob/subtree entries, optionally filtered."
  (let ((filter (plist-get node :filter)))
    (apply #'append
           (mapcar (lambda (f)
                     (let ((tree (or (and (eq (plist-get f :type) 'commit)
                                          (plist-get f :tree))
                                     (plist-get f :sha))))
                       (when tree (gitq--fetch-blobs-at tree nil filter))))
                   frames))))

(defun gitq--via-diff (frames node)
  "Diff morphism: paths changed vs. parent (or the node's :ref)."
  (let ((ref (plist-get node :ref)))
    (apply #'append
           (mapcar (lambda (f)
                     (let* ((sha       (plist-get f :sha))
                            ;; A root commit (no parents) has no
                            ;; "sha^" to diff against -- `--root'
                            ;; diffs it against the empty tree
                            ;; instead of erroring on an invalid
                            ;; revision.
                            (no-parent (and (not ref) (null (plist-get f :parents))))
                            (other     (unless no-parent (or ref (format "%s^" sha))))
                            (paths     (if no-parent
                                           (gitq--git "diff-tree" "--root" "-r"
                                                      "--name-only" "--no-commit-id" sha)
                                         (gitq--git "diff-tree" "-r" "--name-only"
                                                    "--no-commit-id" other sha))))
                       (mapcar (lambda (p)
                                 (list :type 'diff :sha sha :path p
                                       :parent-sha other))
                               paths)))
                   frames))))

(defun gitq--via-diff-hunks (frames _node)
  "Diff-hunks morphism: changed line ranges vs. parent."
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

(defun gitq--via-history (frames _node)
  "History morphism: the commits that touched each frame's path."
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

(defun gitq--via-commit (frames _node)
  "Commit morphism: resolve each frame's :commit-sha back to its commit."
  (delq nil
        (mapcar (lambda (f)
                  (gitq--fetch-commit (plist-get f :commit-sha)))
                frames)))

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
  "Project each frame in FRAMES to only the fields listed in NODE.
Projected keys are keywords (`:sha', `:path', ...), matching every other
frame type -- storing bare symbol keys here would silently break any
keyword-based lookup on the result (`gitq--frame-field', `plist-get
frame :sha' in `gitq--frame-commit-sha', the results-buffer RET/b/c
commands, ...)."
  (let ((fields (plist-get node :fields)))
    (mapcar (lambda (f)
              (let (proj)
                (dolist (field fields)
                  (setq proj (plist-put proj (intern (format ":%s" field))
                                        (gitq--frame-field f field))))
                (cons :type (cons 'projection proj))))
            frames)))

(defun gitq--exec-sort (frames node)
  "Sort FRAMES by the field in NODE, using the field's scalar type.
Number fields compare numerically — `sort parents-count' used to crash
with a wrong-type-argument, since every comparison went through
`string<'.  Date fields (git's ISO-8601 %ai format) and everything else
compare lexically, which for ISO dates is chronological order."
  (let* ((field (plist-get node :field))
         (desc  (plist-get node :desc))
         (cmp   (if (eq (gitq--field-type field) 'number)
                    (lambda (a b) (< (if (numberp a) a 0) (if (numberp b) b 0)))
                  (lambda (a b) (string< (if (stringp a) a "")
                                         (if (stringp b) b ""))))))
    (sort (copy-sequence frames)
          (lambda (a b)
            (let ((va (gitq--frame-field a field))
                  (vb (gitq--frame-field b field)))
              (if desc (funcall cmp vb va) (funcall cmp va vb)))))))

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
      ;; The parser can't produce anything else; reaching here means an
      ;; internal inconsistency, which must not silently pass frames
      ;; through unchanged as it used to.
      (_        (error "gitq: internal error: unknown step type '%s'" type)))))

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
           (msg      (plist-get node :message))
           (head     (gitq--git-string "rev-parse" "HEAD"))
           (sel      (and frames (gitq--frame-commit-sha (car frames)))))
       ;; `git commit --amend' only ever rewrites HEAD.  If the pipeline
       ;; selected some other commit, silently amending HEAD instead
       ;; would be doing something different from what the query says.
       (when (and sel head
                  (not (equal (gitq--git-string "rev-parse" sel) head)))
         (user-error "gitq amend: selected commit %s is not HEAD (amend only rewrites HEAD; use /reword for older commits)"
                     (substring sel 0 (min 8 (length sel)))))
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
       ;; A missing backing function must be a loud error, not a
       ;; silently-does-nothing terminal (the /delete lesson).
       (if msg
           (if (fboundp 'git-branch-off--reword-apply)
               (git-branch-off--reword-apply sha msg)
             (user-error "gitq reword: git-branch-off-reword.el is not loaded"))
         (if (fboundp 'git-branch-off-reword)
             (git-branch-off-reword sha)
           (user-error "gitq reword: git-branch-off-reword.el is not loaded")))))
    ('squash
     (let ((msg (plist-get node :message)))
       (message "gitq squash: %d commits%s — use git-branch-off-squash for full support"
                (length frames)
                (if msg (format " → \"%s\"" msg) ""))))
    ('remove
     (let* ((f   (car frames))
            (sha (gitq--frame-commit-sha f)))
       (unless sha (user-error "gitq remove: no commit in result"))
       (if (fboundp 'git-branch-off-remove)
           (git-branch-off-remove sha)
         (user-error "gitq remove: git-branch-off-reword.el is not loaded"))))
    ('commit
     (let ((msg (plist-get node :message)))
       (if msg
           (progn
             (gitq--git "commit" "-m" msg)
             (when (fboundp 'magit-refresh) (magit-refresh)))
         (if (fboundp 'magit-commit-create)
             (call-interactively #'magit-commit-create)
           (user-error "gitq commit: no message given and magit is not loaded (use /commit \"MSG\")")))))
    ('stage
     (if (fboundp 'magit-stage-modified)
         (magit-stage-modified)
       (gitq--git "add" "--update")
       (message "gitq: staged modified files")))
    ('mark
     (let* ((f     (car frames))
            (sha   (gitq--frame-commit-sha f))
            (label (plist-get node :label)))
       (when (and sha label)
         (gitq--git "notes" "add" "-m" label sha)
         (message "gitq: marked %s with '%s'"
                  (substring sha 0 (min 8 (length sha))) label))))
    ('worktree
     (let* ((f   (car frames))
            (sha (gitq--frame-commit-sha f)))
       (unless sha (user-error "gitq worktree: no commit in result"))
       ;; Default path follows the package's worktree convention:
       ;; <repo-root>/.worktree/<full-40-char-hash> (see design.org).
       (let* ((full (or (gitq--git-string "rev-parse" sha) sha))
              (path (or (plist-get node :path)
                        (expand-file-name (concat ".worktree/" full)
                                          (gitq--toplevel)))))
         (gitq--git "worktree" "add" "--detach" path full)
         (when (fboundp 'magit-refresh) (magit-refresh))
         (message "gitq: added worktree at %s" path))))
    (op
     ;; Every parseable terminal has a branch above; falling through
     ;; used to silently degrade to /show, which is how /delete could
     ;; parse fine and then not delete anything.
     (error "gitq: internal error: unhandled terminal operation '%s'" op))))

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
Used for completion (every field that could ever be valid *somewhere*)
and for lexical disambiguation.  Grammar validation of any *particular*
`where'/`sort'/`pick' reference is against the narrower, frame-type-
specific lists below, threaded through the pipeline as `current-fields'
— referencing a field that exists on some other frame type (e.g. `name'
on a `commits' source) is a parse-time error naming exactly which
fields the current frame actually has, not a silently-matches-nothing
where-condition.  Note: `path' collides with the reserved step keyword
of the same name (the standalone `path GLOB' step) — see
`gitq--complete--enclosing-step' and `pick's field-list loop, both of
which are written to disambiguate this correctly.")

(defconst gitq--field-types
  '(("sha" . sha) ("author" . string) ("email" . string) ("date" . date)
    ("message" . string) ("path" . string) ("name" . string) ("branch" . string)
    ("parents-count" . number) ("modified" . flag) ("staged" . flag)
    ("untracked" . flag) ("tree" . sha) ("reftype" . string)
    ("detached" . flag) ("mode" . string) ("parent-sha" . sha)
    ("commit-sha" . sha) ("start-line" . number) ("end-line" . number)
    ("line-number" . number) ("content" . string))
  "Scalar type of each field in `gitq--field-names'.
The structural field-SET typing below answers \"does this frame carry
this field at all?\"; this table answers the next question, \"which
where-operators and sort order make sense on it?\".  Every field name
must appear in both — `gitq--flat-parse-where' and `gitq--exec-sort'
look types up unconditionally.")

(defconst gitq--operator-signatures
  '(("=="       . (string sha date number flag))
    ("!="       . (string sha date number flag))
    (">"        . (number))
    ("<"        . (number))
    (">="       . (number))
    ("<="       . (number))
    ("contains" . (string sha))
    ("matches"  . (string sha))
    ("after"    . (date))
    ("before"   . (date))
    ("within"   . (date))
    ("is"       . (flag)))
  "For each where-operator, the field scalar types it accepts.
A closed list, checked at parse time: an operator not in this table is
an unknown-operator parse error (it used to parse fine and only blow up
— or worse, silently match nothing — when the condition was evaluated),
and applying an operator to a field whose type it doesn't accept (e.g.
`date > \"2020-01-01\"', where `>' compares numbers so the condition was
always silently false) is a parse-time type error suggesting the
operators that DO fit the field.")

(defun gitq--field-type (field)
  "Return the scalar type symbol for FIELD (a string or symbol).
Fields projected into existence by `pick' keep their original names, so
this lookup covers them too.  Unknown fields default to `string' — the
weakest assumption — rather than erroring, because `pick'ed projections
are open-ended."
  (or (cdr (assoc (if (symbolp field) (symbol-name field) field)
                  gitq--field-types))
      'string))

;; Structural field-set typing: each of these is the exact set of
;; fields one particular frame *shape* actually carries, taken directly
;; from where that shape is constructed (`gitq--parse-commit-line',
;; `gitq--fetch-refs', `gitq--fetch-worktrees', `gitq--fetch-blobs-at',
;; the `via'/`grep' executors in `gitq--exec-via'/`gitq--exec-grep').
;; `gitq--parse-flat' threads the currently-active one of these through
;; the whole pipeline as `current-fields', updating it after every
;; source/`via'/`grep'/`pick' stage, and validates every `where'/`sort'/
;; `pick'/`path'/`via' reference against it instead of the flat global
;; union above. This is deliberately structural (keyed by field-set
;; content) rather than nominal (keyed by the runtime `:type' tag),
;; because two DIFFERENT shapes share the tag `tree': a whole tree
;; object from `.tree' (just `:sha') vs. a subtree entry from
;; `.tree.subtrees'/`.tree.entries[Tree]' (`:sha :path :mode', same
;; shape as a blob entry) — the tag alone can't distinguish them, but
;; the field-set naturally does.

(defconst gitq--commit-fields
  '("sha" "author" "email" "date" "message" "tree" "parents-count")
  "Fields on a commit frame (`gitq--parse-commit-line').")

(defconst gitq--ref-fields
  '("sha" "name" "reftype")
  "Fields on a ref frame (`gitq--fetch-branches'/`-tags'/`-refs').")

(defconst gitq--worktree-fields
  '("path" "sha" "branch" "detached" "modified" "staged" "untracked")
  "Fields on a worktree frame (`gitq--fetch-worktrees').
NOTE: `modified'/`staged'/`untracked' are declared here (and always
have been, in the old global list) but `gitq--fetch-worktrees' never
actually sets them — there is no working-tree-status wiring yet. They
will type-check as valid but still always resolve to nil at runtime
until that's implemented; this table doesn't fix that, only the
field/frame-type mismatch class of bug.")

(defconst gitq--blob-fields
  '("sha" "path" "mode")
  "Fields on a blob or tree-entry frame (`gitq--fetch-blobs-at';
reached via `blobs' as a source, or `.tree.blobs'/`.tree.subtrees'/
`.tree.entries[...]' as a morphism).")

(defconst gitq--tree-object-fields
  '("sha")
  "Fields on a whole-tree-object frame (`.tree', not `.tree.entries').")

(defconst gitq--diff-fields
  '("sha" "path" "parent-sha")
  "Fields on a diff frame (`.diff').")

(defconst gitq--hunk-fields
  '("path" "start-line" "end-line" "commit-sha")
  "Fields on a hunk frame (`.diff.hunks'). Notably has no `sha' —
`grep'/`pickaxe' (which need one) cannot follow this morphism.")

(defconst gitq--line-fields
  '("sha" "path" "line-number" "content" "commit-sha")
  "Fields on a line frame (`grep' output).")

(defconst gitq--source-fields
  `((commits  . ,gitq--commit-fields)
    (ref      . ,gitq--commit-fields) ; HEAD or a bare branch/tag/sha source resolves to one commit
    (branches . ,gitq--ref-fields)
    (tags     . ,gitq--ref-fields)
    (refs     . ,gitq--ref-fields)
    (worktree . ,gitq--worktree-fields)
    (blobs    . ,gitq--blob-fields))
  "The field-set each source keyword's frames start the pipeline with.")

(defconst gitq--morphisms
  `((parent         :requires "parents-count" :yields ,gitq--commit-fields
                    :exec gitq--via-parent)
    (parent-adjoint :requires "sha"           :yields ,gitq--commit-fields
                    :exec gitq--via-parent-adjoint)
    (tree           :requires "tree"          :yields ,gitq--tree-object-fields
                    :exec gitq--via-tree)
    (tree-entries   :requires "sha"           :yields ,gitq--blob-fields
                    :exec gitq--via-tree-entries)
    (diff           :requires "sha"           :yields ,gitq--diff-fields
                    :exec gitq--via-diff)
    (diff-hunks     :requires "sha"           :yields ,gitq--hunk-fields
                    :exec gitq--via-diff-hunks)
    (history        :requires "path"          :yields ,gitq--commit-fields
                    :exec gitq--via-history)
    (commit         :requires "commit-sha"    :yields ,gitq--commit-fields
                    :exec gitq--via-commit))
  "The morphism registry: one entry per morphism symbol, the single
source of truth for the parser, the type checker, and the executor.

:requires — the field that must be present in the field-set flowing
into the morphism (its domain), or the pipeline is a parse-time domain
error (e.g. `.tree' needs `tree', which only commit frames have —
applying it to a `branches' source used to silently return empty
results, since ref frames' `:tree' is always nil).
:yields — the field-set of the frames it produces (its codomain),
which becomes the current field-set for the rest of the pipeline.
:exec — the function (FRAMES NODE) -> FRAMES implementing it.

Semantically each morphism maps one frame to a LIST of frames (parent
of a merge is several commits, a tree has many entries); :exec is that
map lifted pointwise over the incoming frame list with the results
appended — Kleisli-style composition over the list monad, which is why
chaining morphisms (`via .parent .tree' or the dotted `.parent.tree')
needs no special cases anywhere: the output of any morphism is always
the same shape of thing as its input.")

(defconst gitq--morphism-forms
  '(("\\.parent\\[\\([0-9]+\\)\\]" .
     (lambda (arg) (list :type 'via :morphism 'parent
                         :index (string-to-number arg))))
    ("\\.parent\\*"           . (lambda (_) (list :type 'via :morphism 'parent :star t)))
    ("\\.parent\\+"           . (lambda (_) (list :type 'via :morphism 'parent :plus t)))
    ("\\.parent†"             . (lambda (_) (list :type 'via :morphism 'parent-adjoint)))
    ("\\.parent"              . (lambda (_) (list :type 'via :morphism 'parent)))
    ("\\.tree\\.entries\\[Blob\\]" . (lambda (_) (list :type 'via :morphism 'tree-entries :filter 'blob)))
    ("\\.tree\\.entries\\[Tree\\]" . (lambda (_) (list :type 'via :morphism 'tree-entries :filter 'tree)))
    ("\\.tree\\.entries"      . (lambda (_) (list :type 'via :morphism 'tree-entries :filter nil)))
    ("\\.tree\\.blobs"        . (lambda (_) (list :type 'via :morphism 'tree-entries :filter 'blob)))
    ("\\.tree\\.subtrees"     . (lambda (_) (list :type 'via :morphism 'tree-entries :filter 'tree)))
    ("\\.tree"                . (lambda (_) (list :type 'via :morphism 'tree)))
    ("\\.entries\\[Blob\\]"   . (lambda (_) (list :type 'via :morphism 'tree-entries :filter 'blob)))
    ("\\.entries\\[Tree\\]"   . (lambda (_) (list :type 'via :morphism 'tree-entries :filter 'tree)))
    ("\\.entries"             . (lambda (_) (list :type 'via :morphism 'tree-entries :filter nil)))
    ("\\.diff\\.hunks"        . (lambda (_) (list :type 'via :morphism 'diff-hunks)))
    ("\\.diff"                . (lambda (_) (list :type 'via :morphism 'diff :ref nil)))
    ("\\.history"             . (lambda (_) (list :type 'via :morphism 'history)))
    ("\\.commit"              . (lambda (_) (list :type 'via :morphism 'commit))))
  "Surface forms a morphism path is built from: (REGEX . NODE-FN).
Each regex matches one segment of a dotted path; NODE-FN receives the
first capture group (or nil) and returns the via node for that segment.

A path like `.parent[0].tree.entries[Blob]' is parsed by repeatedly
taking the LONGEST form that matches at the current position (see
`gitq--parse-morphism-path'), so any morphisms whose types line up can
be composed by just writing them one after another — composition is
generic, not a hardcoded list of allowed combinations.  The fused
multi-segment forms here (`.tree.entries', `.diff.hunks', ...) are
single morphisms for efficiency and history, but they parse, type-check
and execute exactly as their name reads.")

(defun gitq--parse-morphism-path (path)
  "Parse PATH — one or more dotted morphism forms — into a list of via nodes.
Greedy longest-match against `gitq--morphism-forms' at each position;
a segment boundary must be a `.' or the end of the path.  Errors on
the first unrecognizable segment, naming it and the full path."
  (let ((pos 0) (len (length path)) nodes)
    (unless (and (> len 0) (eq (aref path 0) ?.))
      (error "gitq: unknown morphism '%s'" path))
    (while (< pos len)
      (let (best-node (best-end pos))
        (dolist (entry gitq--morphism-forms)
          (when (eq (string-match (car entry) path pos) pos)
            (let ((end (match-end 0))
                  (arg (match-string 1 path)))
              (when (and (> end best-end)
                         (or (= end len) (eq (aref path end) ?.)))
                (setq best-end end
                      best-node (funcall (cdr entry) arg))))))
        (unless best-node
          (error "gitq: unknown morphism '%s'%s"
                 (substring path pos)
                 (if (> pos 0) (format " (in '%s')" path) "")))
        (push best-node nodes)
        (setq pos best-end)))
    (nreverse nodes)))

(defun gitq--require-field (current-fields field context)
  "Error unless FIELD is present in CURRENT-FIELDS, naming CONTEXT.
CURRENT-FIELDS is nil-tolerant (an unknown/uninferred type just skips
the check) so this never turns an inference gap into a false error."
  (when (and current-fields (not (member field current-fields)))
    (error "gitq: '%s' needs a '%s' field, but the current frame only has: %s"
           context field (string-join current-fields ", "))))

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
          ;; A digit-starting bare word must consume the same extended
          ;; run as the identifier branch below (letters, more digits,
          ;; -/_///~/@/{/}), not just digits -- otherwise values that
          ;; start with a digit but contain letters or dashes (SHA
          ;; prefixes like 062062e9af, dates like 2026-05-25) silently
          ;; split at the digit/letter or digit/dash boundary into two
          ;; or more tokens instead of tokenizing as one bare word.
          (let ((s i))
            (while (and (< i len)
                        (let ((d (aref str i)))
                          (or (and (>= d ?a) (<= d ?z))
                              (and (>= d ?A) (<= d ?Z))
                              (and (>= d ?0) (<= d ?9))
                              (memq d '(?- ?_ ?/ ?~ ?@ ?{ ?})))))
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

(defun gitq--flat-parse-where (tokens current-fields)
  "Parse where-conditions from flat TOKENS, returning (node . remaining).
Step keywords and /terminals act as stage boundaries and are never consumed
as condition values.  FIELD tokens must be members of CURRENT-FIELDS — the
field-set actually carried by the frame type flowing into this `where'
(not the flat global union of every field on every frame type) — so
e.g. `commits where name == ...' is a parse-time error naming exactly
which fields a commit frame has, instead of silently matching nothing
at run time because commit frames have no `:name'."
  (unless (or (null tokens) (member (car tokens) current-fields)
              (gitq--flat-boundary-p (car tokens)))
    (error "gitq: field '%s' not valid here after 'where' (current frame has: %s)"
           (car tokens) (string-join current-fields ", ")))
  (let (conditions)
    (while (and tokens (member (car tokens) current-fields))
      (let* ((field-tok (pop tokens))
             (field     (intern field-tok))
             (ftype     (gitq--field-type field-tok))
             (next      (car tokens)))
        (cond
         ;; Bare flag: next token is a boundary, comma, or another field.
         ;; Only meaningful for flag-typed fields — a bare `where author'
         ;; used to parse as (author is t), which `is' evaluates as
         ;; equality against t, silently matching nothing.
         ((or (null next) (equal next ",")
              (member next current-fields)
              (gitq--flat-boundary-p next))
          (unless (eq ftype 'flag)
            (error "gitq: bare 'where %s' tests a flag, but '%s' is a %s field (add an operator and value)"
                   field-tok field-tok ftype))
          (push (list :field field :op 'is :value t) conditions))
         ;; Operator present
         (t
          (let* ((op-tok (pop tokens))
                 (op     (intern op-tok))
                 (sig    (assoc op-tok gitq--operator-signatures))
                 (next2  (car tokens)))
            ;; Operators are a closed set, and each accepts only some
            ;; field types — both are parse-time errors now, not
            ;; runtime surprises (a bogus operator errored only when a
            ;; frame reached it; a type mismatch like `date > ...' was
            ;; silently false forever).
            (unless sig
              (error "gitq: unknown where operator '%s' (expected one of: %s)"
                     op-tok
                     (mapconcat #'car gitq--operator-signatures ", ")))
            (unless (memq ftype (cdr sig))
              (error "gitq: operator '%s' does not apply to '%s' (a %s field; try: %s)"
                     op-tok field-tok ftype
                     (mapconcat #'car
                                (seq-filter (lambda (s) (memq ftype (cdr s)))
                                            gitq--operator-signatures)
                                ", ")))
            (cond
             ;; Step keyword immediately after an operator that requires a value:
             ;; this is always an error — the keyword must be quoted.
             ((gitq--flat-step-p next2)
              (error
               "gitq: '%s' requires a value; step keyword '%s' must be quoted: \"%s\""
               op-tok next2 next2))
             ;; No value after the operator.  Only `is' works valueless
             ;; (an explicit flag test); every other operator used to
             ;; get :value t here and silently match nothing.
             ((or (null next2) (equal next2 ",")
                  (member next2 current-fields)
                  (gitq--flat-terminal-p next2))
              (unless (eq op 'is)
                (error "gitq: operator '%s' requires a value, got %s"
                       op-tok
                       (if next2 (format "'%s'" next2) "end of input")))
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
                ;; A number-typed field compared with `equal'/arithmetic
                ;; against a non-numeric value can never match — say so
                ;; now instead of silently returning nothing.
                (when (and (eq ftype 'number) (not (numberp val)))
                  (error "gitq: '%s' is a number field; '%s' is not a number"
                         field-tok val-tok))
                (push (list :field field :op op :value val) conditions))))))))
      (when (equal (car tokens) ",")
        (pop tokens)
        (unless (and tokens (member (car tokens) current-fields))
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
  "Parse a via-step morphism path from flat TOKENS, returning (nodes . remaining).
The path may compose several morphisms (`.parent[0].tree.entries'), so
NODES is a list of via nodes executed left to right — see
`gitq--parse-morphism-path'.  When the FINAL morphism is `.diff', its
optional REF argument is consumed from the following token, unless that
token is a step keyword, /terminal, or another morphism path."
  (let* ((path  (pop tokens))
         (nodes (gitq--parse-morphism-path path))
         (last-node (car (last nodes))))
    (when (and (eq (plist-get last-node :morphism) 'diff)
               tokens
               (not (gitq--flat-boundary-p (car tokens)))
               (not (string-prefix-p "." (car tokens))))
      (plist-put last-node :ref (pop tokens)))
    (cons nodes tokens)))

(defun gitq--flat-parse-count (tok step-name)
  "Parse TOK as a non-negative integer count for STEP-NAME (\"take\"/\"skip\").
Errors naming the bad token rather than silently truncating it via
`string-to-number' -- a token like \"5x\" must never be read as 5."
  (unless (and tok (string-match-p "^[0-9]+$" tok))
    (error "gitq: '%s' requires a number, got '%s'" step-name (or tok "end of input")))
  (string-to-number tok))

(defun gitq--flat-parse-step (tokens current-fields)
  "Parse one step node from flat TOKENS (first token must be a step keyword).
CURRENT-FIELDS is the field-set carried by the frame type flowing into
this step. Returns (list NODE REMAINING-TOKENS NEW-FIELDS) — NEW-FIELDS
is the field-set active after this step, since `via'/`grep'/`pick'
change the frame shape and every later `where'/`sort'/`pick'/`path'/
`via' reference must be checked against the shape actually in effect
at that point, not a single flat global field list that can't tell a
commit's fields from a ref's or a hunk's."
  (let ((kw (pop tokens)))
    (pcase kw
      ("via"
       ;; A dotted path may compose several morphisms; type-check the
       ;; chain by folding each morphism's registry signature: its
       ;; :requires field must be in the field-set yielded by the
       ;; previous one, and its :yields becomes the input to the next.
       (let* ((path-tok (car tokens))
              (result   (gitq--flat-parse-via tokens))
              (nodes    (car result))
              (fields   current-fields))
         (dolist (node nodes)
           (let ((spec (alist-get (plist-get node :morphism) gitq--morphisms)))
             (when spec
               (gitq--require-field fields (plist-get spec :requires)
                                    (format "via %s" path-tok))
               (setq fields (plist-get spec :yields)))))
         (list nodes (cdr result) fields)))
      ("where"
       (let ((r (gitq--flat-parse-where tokens current-fields)))
         (list (car r) (cdr r) current-fields)))
      ("grep"
       (gitq--require-field current-fields "sha" "grep")
       (let* ((pat-tok (pop tokens))
              (regex   (string-prefix-p "/" pat-tok))
              (pattern (if regex (gitq--unregex pat-tok) (gitq--unquote pat-tok))))
         ;; Inline "path" qualifier removed in flat mode — use a separate path step.
         (list (list :type 'grep :pattern pattern :regex regex :path-filter nil)
               tokens gitq--line-fields)))
      ("pickaxe"
       (gitq--require-field current-fields "sha" "pickaxe")
       (let* ((pat-tok (pop tokens))
              (regex   (or (string-prefix-p "/" pat-tok)
                           (equal (car tokens) "regex")))
              (pattern (if (string-prefix-p "/" pat-tok)
                           (gitq--unregex pat-tok)
                         (gitq--unquote pat-tok))))
         (when (equal (car tokens) "regex") (pop tokens))
         (list (list :type 'pickaxe :pattern pattern :regex regex) tokens current-fields)))
      ("path"
       (gitq--require-field current-fields "path" "path")
       (list (list :type 'path :pattern (gitq--unquote (pop tokens))) tokens current-fields))
      ("pick"
       ;; Driven by field-list membership (plus comma), not the generic
       ;; step-keyword boundary check: `path' is both a reserved step
       ;; keyword (the standalone `path GLOB' step) and a legitimate
       ;; field (blob/diff/hunk/line frames all carry a :path key), so
       ;; `pick path, author' must recognize `path' as a field here even
       ;; though it is also a step keyword everywhere else.
       (let (fields)
         (while (and tokens (or (equal (car tokens) ",")
                                (member (car tokens) current-fields)))
           (let ((tok (pop tokens)))
             (unless (equal tok ",")
               (push tok fields))))
         (setq fields (nreverse fields))
         ;; `pick' with nothing to pick used to parse fine and project
         ;; every frame down to (:type projection) — data silently gone.
         (unless fields
           (error "gitq: 'pick' requires at least one field name, got %s"
                  (if tokens (format "'%s'" (car tokens)) "end of input")))
         (list (list :type 'pick :fields (mapcar #'intern fields)) tokens fields)))
      ("take"
       (list (list :type 'take :n (gitq--flat-parse-count (pop tokens) "take")) tokens current-fields))
      ("skip"
       (list (list :type 'skip :n (gitq--flat-parse-count (pop tokens) "skip")) tokens current-fields))
      ("first" (list (list :type 'first) tokens current-fields))
      ("last"  (list (list :type 'last)  tokens current-fields))
      ("sort"
       (let* ((f    (pop tokens))
              (neg  (string-prefix-p "-" f))
              (name (if neg (substring f 1) f)))
         (unless (member name current-fields)
           (error "gitq: field '%s' not valid here after 'sort' (current frame has: %s)"
                  name (string-join current-fields ", ")))
         (list (list :type 'sort :field (intern name) :desc neg) tokens current-fields)))
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
         nodes fields)
    (unless tokens (error "gitq: empty pipeline"))
    ;; Parse source (first stage)
    (let* ((result (gitq--flat-parse-source tokens)))
      (push (car result) nodes)
      (setq tokens (cdr result))
      (setq fields (alist-get (plist-get (car result) :source) gitq--source-fields)))
    ;; Parse steps and terminal
    (while tokens
      (let ((tok (car tokens)))
        (cond
         ((gitq--flat-terminal-p tok)
          (let* ((result (gitq--flat-parse-terminal tok (cdr tokens))))
            (push (car result) nodes)
            (setq tokens nil)))     ; terminal is always last
         ((gitq--flat-step-p tok)
          (let* ((result (gitq--flat-parse-step tokens fields))
                 (parsed (nth 0 result)))
            ;; `via' yields a LIST of nodes (a composed morphism path);
            ;; every other step yields a single node plist, which
            ;; starts with a keyword.
            (if (keywordp (car parsed))
                (push parsed nodes)
              (dolist (n parsed) (push n nodes)))
            (setq tokens (nth 1 result))
            (setq fields (nth 2 result))))
         (t
          (error "gitq: expected step keyword or /terminal, got '%s'" tok)))))
    (nreverse nodes)))

;;; Completion

(defconst gitq--complete-source-keywords
  '("commits" "branches" "tags" "refs" "worktrees" "blobs" "HEAD")
  "Source keywords offered at the start of a pipeline.")

(defconst gitq--complete-morphisms
  '(".parent" ".parent*" ".parent+" ".parent†" ".tree" ".tree.blobs"
    ".tree.subtrees" ".tree.entries" ".tree.entries[Blob]"
    ".tree.entries[Tree]" ".diff" ".diff.hunks" ".history" ".commit")
  "Morphism paths offered after `via'.
These are the canonical single-morphism forms; dotted compositions
(`.parent.tree', `.parent[0].diff', ...) are typed by hand and parsed
generically by `gitq--parse-morphism-path'.  Consistency with the
parser and the registry is locked down by tests, not by construction —
the parser accepts strictly more than this list offers.")

(defconst gitq--complete-where-operators
  '("==" "!=" ">" "<" ">=" "<=" "contains" "matches" "after" "before" "within" "is")
  "Operators offered after a field name in a where clause.")

(defconst gitq--complete-terminals
  (mapcar (lambda (entry) (concat "/" (car entry))) gitq--terminals)
  "Terminal /command keywords, derived from the `gitq--terminals'
registry so the two can never drift apart (completion used to offer
/worktree while the parser rejected it).")

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
    (".parent†"            . "children-of: commits whose parent is in the result")
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

(defun gitq--infer-fields-for-ctx (ctx)
  "Return the field-set active after fully-typed tokens CTX.
Replays the real pipeline parser (`gitq--flat-parse-source' then
`gitq--flat-parse-step' stage by stage) instead of re-implementing its
stage-skipping logic a second time, so the strict parser and completion
can never silently drift apart. CTX is typically a prefix of a full
pipeline — possibly ending mid-stage (e.g. right after `via' with no
morphism token yet) — so each stage is parsed inside its own
`condition-case': the first stage that can't be parsed from what's
typed so far just stops the walk there, returning the last
successfully-computed field-set instead of erroring out entirely."
  (condition-case nil
      (let* ((tokens ctx)
             (result (gitq--flat-parse-source tokens))
             (fields (alist-get (plist-get (car result) :source) gitq--source-fields)))
        (setq tokens (cdr result))
        (while tokens
          (cond
           ((gitq--flat-terminal-p (car tokens)) (setq tokens nil))
           ((gitq--flat-step-p (car tokens))
            (condition-case nil
                (let ((r (gitq--flat-parse-step tokens fields)))
                  (setq tokens (nth 1 r))
                  (setq fields (nth 2 r)))
              (error (setq tokens nil))))
           (t (setq tokens nil))))
        fields)
    (error gitq--commit-fields)))

(defun gitq--complete--current-type-fields (ctx)
  "Return the field-set valid to offer as `where'/`sort'/`pick' field
candidates at the end of CTX — the field-set flowing into whichever of
those three stages encloses the current position (found via
`gitq--complete--enclosing-step'), not the fields after it. `pick'
matters here: with zero fields picked so far, its OWN output field-set
is empty, so candidates must come from what's valid to pick *from*,
computed by inferring fields only up to (not including) the enclosing
stage keyword itself."
  (let* ((stage (gitq--complete--enclosing-step ctx))
         (idx   (and stage (cl-position stage ctx :test #'equal :from-end t))))
    (gitq--infer-fields-for-ctx (if idx (seq-take ctx idx) ctx))))

(defun gitq--morphism-requires (path)
  "Return the field the `via' morphism PATH (a `gitq--complete-morphisms'
candidate string) needs present in the current field-set, or nil if
PATH is unknown.  Only the FIRST morphism in the path constrains the
incoming frame type, so a composed path is filtered by its head.
Reuses `gitq--parse-morphism-path' to extract the morphism symbol, then
looks it up in `gitq--morphisms' — the same registry
`gitq--flat-parse-step' enforces against at parse time, so completion
can never offer a morphism the parser would then reject."
  (let* ((node (ignore-errors (car (gitq--parse-morphism-path path))))
         (morphism (and node (plist-get node :morphism))))
    (plist-get (alist-get morphism gitq--morphisms) :requires)))

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

     ;; After "via" → morphisms valid for the frame type flowing in here
     ;; (e.g. `.tree'/`.parent' never show up after a `branches'/`refs'
     ;; source, since ref frames carry no `:tree'/`:parents-count' —
     ;; the same domain check `gitq--flat-parse-step' enforces at parse
     ;; time, so completion can't offer something the parser would then
     ;; reject).
     ((equal last-ctx "via")
      (let ((fields (gitq--infer-fields-for-ctx (butlast ctx))))
        (seq-filter (lambda (m)
                      (let ((req (gitq--morphism-requires m)))
                        (or (null req) (member req fields))))
                    gitq--complete-morphisms)))

     ;; After ".diff" (the one morphism with an optional trailing REF
     ;; argument) → offer refs, but also let the user skip straight to
     ;; a step/terminal since the REF is optional.
     ((and (equal last-ctx ".diff") (equal prev-ctx "via"))
      (append (gitq--complete-refs) gitq--flat-step-keywords gitq--complete-terminals))

     ;; After "where" or "," (start of another condition) → field names
     ;; valid for the frame type flowing into this `where' (not the
     ;; flat global union of every field on every frame type).
     ((or (equal last-ctx "where") (equal last-ctx ","))
      (gitq--complete--current-type-fields ctx))

     ;; After a field that is part of a `where' clause → where
     ;; operators.  `sort'/`pick' fields never take one; `path' is also
     ;; a field name but is excluded from THIS check anyway, since the
     ;; enclosing-step guard already requires "where" specifically.
     ;; Validated against the current frame type's fields, not the flat
     ;; global list, so a hand-typed cross-type field (e.g. `name' after
     ;; `commits where') doesn't get offered where-operators at all.
     ((and last-ctx
           (equal (gitq--complete--enclosing-step ctx) "where")
           (member last-ctx (gitq--complete--current-type-fields ctx)))
      gitq--complete-where-operators)

     ;; After "sort" → field names (current frame type) with optional
     ;; "-" negation prefix
     ((equal last-ctx "sort")
      (let ((fields (gitq--complete--current-type-fields ctx)))
        (append fields (mapcar (lambda (f) (concat "-" f)) fields))))

     ;; After "pick" or pick-comma → field names valid for the frame
     ;; type flowing into `pick' (computed from what's typed *before*
     ;; `pick', since `pick' with nothing chosen yet has an empty output
     ;; field-set of its own).
     ((or (equal last-ctx "pick")
          (and (equal last-ctx ",") (member "pick" ctx)))
      (gitq--complete--current-type-fields ctx))

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

(defun gitq--history-search ()
  "Search `gitq--history' and splice the chosen pipeline into the minibuffer.
Bound to \\`C-r' while reading a gitq pipeline (see
`gitq--pipeline-map' and `gitq--read-pipeline').  Opens a *recursive*
minibuffer whose collection is `gitq--history' itself, so whatever
`completing-read' front-end is active (Vertico, in particular) shows
and live-filters the full history list exactly as it would any other
gitq candidate set — this is the \\`C-r'-in-a-shell equivalent asked
for, built without a dependency on `consult-history'.

Selecting an entry replaces the outer prompt's current contents with
it; nothing is executed or previewed until RET is pressed in the outer
prompt, same as if the text had been typed by hand."
  (interactive)
  (unless gitq--history
    (user-error "No gitq history yet"))
  (let* ((enable-recursive-minibuffers t)
         (choice (completing-read "gitq history: " gitq--history nil t)))
    (delete-minibuffer-contents)
    (insert choice)))

(defvar gitq--pipeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-r") #'gitq--history-search)
    map)
  "Keymap merged into the minibuffer's local map by `gitq--read-pipeline'.
Only active while reading a gitq pipeline, not in `completing-read'
prompts generally.")

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
real, terminal included.

\\`C-r' opens a history search over previously-run pipelines; see
`gitq--history-search'."
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
           (use-local-map (make-composed-keymap gitq--pipeline-map (current-local-map)))
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
