;;; git-branch-off-gitq-test.el --- ERT tests for gitq  -*- lexical-binding: t; -*-

;; Run with:
;;   cd test && ./run-tests.sh
;; or:
;;   emacs --batch -L .. -l git-branch-off-gitq.el -l git-branch-off-gitq-test.el \
;;         -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'seq)

;;; Helpers (mirror git-branch-off-test.el helpers)

(defmacro gitq-test--with-repo (&rest body)
  "Execute BODY with `default-directory' set to a fresh git repository."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "gitq-test-" t))
          (default-directory dir))
     (unwind-protect
         (progn
           (call-process "git" nil nil nil "init" "-q" "-b" "main")
           (call-process "git" nil nil nil "config" "user.name"  "Test User")
           (call-process "git" nil nil nil "config" "user.email" "test@example.com")
           ,@body)
       (delete-directory dir t))))

(defun gitq-test--commit (name content message)
  "Write NAME with CONTENT, stage, and commit with MESSAGE."
  (write-region content nil name nil 'silent)
  (call-process "git" nil nil nil "add" name)
  (call-process "git" nil nil nil "commit" "-q" "-m" message))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--unquote / gitq--unregex
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--unquote/basic ()
  (should (equal (gitq--unquote "\"alice\"") "alice")))

(ert-deftest gitq-test--unquote/with-spaces ()
  (should (equal (gitq--unquote "\"hello world\"") "hello world")))

(ert-deftest gitq-test--unquote/non-quoted ()
  "Non-quoted strings are returned unchanged."
  (should (equal (gitq--unquote "alice") "alice")))

(ert-deftest gitq-test--unregex/basic ()
  (should (equal (gitq--unregex "/^feat:/") "^feat:")))

(ert-deftest gitq-test--unregex/complex ()
  (should (equal (gitq--unregex "/TODO|FIXME/") "TODO|FIXME")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse-flat — source stages
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-source/commits ()
  (let ((node (car (gitq--parse-flat "commits /show"))))
    (should (eq (plist-get node :type) 'source))
    (should (eq (plist-get node :source) 'commits))
    (should (null (plist-get node :range)))))

(ert-deftest gitq-test--parse-source/commits-in-range ()
  (let ((node (car (gitq--parse-flat "commits in main..feature /show"))))
    (should (eq (plist-get node :source) 'commits))
    (should (equal (plist-get node :range) "main..feature"))))

(ert-deftest gitq-test--parse-source/head-ref ()
  (let ((node (car (gitq--parse-flat "HEAD /show"))))
    (should (eq (plist-get node :source) 'ref))
    (should (equal (plist-get node :ref) "HEAD"))))

(ert-deftest gitq-test--parse-source/branch-name ()
  (let ((node (car (gitq--parse-flat "main /show"))))
    (should (eq (plist-get node :source) 'ref))
    (should (equal (plist-get node :ref) "main"))))

(ert-deftest gitq-test--parse-source/branches ()
  (let ((node (car (gitq--parse-flat "branches /show"))))
    (should (eq (plist-get node :source) 'branches))))

(ert-deftest gitq-test--parse-source/tags ()
  (let ((node (car (gitq--parse-flat "tags /show"))))
    (should (eq (plist-get node :source) 'tags))))

(ert-deftest gitq-test--parse-source/worktrees ()
  (let ((node (car (gitq--parse-flat "worktrees /show"))))
    (should (eq (plist-get node :source) 'worktree))))

(ert-deftest gitq-test--parse-source/worktree-singular ()
  (let ((node (car (gitq--parse-flat "worktree /show"))))
    (should (eq (plist-get node :source) 'worktree))))

(ert-deftest gitq-test--parse-source/blobs ()
  (let ((node (car (gitq--parse-flat "blobs /show"))))
    (should (eq (plist-get node :source) 'blobs))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse-flat — via step
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-via/parent ()
  (let* ((nodes (gitq--parse-flat "commits via .parent /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :type) 'via))
    (should (eq (plist-get via :morphism) 'parent))
    (should (null (plist-get via :star)))))

(ert-deftest gitq-test--parse-via/parent-star ()
  (let* ((nodes (gitq--parse-flat "HEAD via .parent* /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'parent))
    (should (plist-get via :star))))

(ert-deftest gitq-test--parse-via/parent-plus ()
  (let* ((nodes (gitq--parse-flat "HEAD via .parent+ /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'parent))
    (should (plist-get via :plus))))

(ert-deftest gitq-test--parse-via/parent-index ()
  (let* ((nodes (gitq--parse-flat "HEAD via .parent[0] /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'parent))
    (should (= (plist-get via :index) 0))))

(ert-deftest gitq-test--parse-via/parent-index-1 ()
  (let* ((nodes (gitq--parse-flat "HEAD via .parent[1] /show"))
         (via   (nth 1 nodes)))
    (should (= (plist-get via :index) 1))))

(ert-deftest gitq-test--parse-via/tree ()
  (let* ((nodes (gitq--parse-flat "HEAD via .tree /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'tree))))

(ert-deftest gitq-test--parse-via/tree-entries-blob ()
  (let* ((nodes (gitq--parse-flat "HEAD via .tree.entries[Blob] /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'tree-entries))
    (should (eq (plist-get via :filter) 'blob))))

(ert-deftest gitq-test--parse-via/tree-entries-tree ()
  (let* ((nodes (gitq--parse-flat "HEAD via .tree.entries[Tree] /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'tree-entries))
    (should (eq (plist-get via :filter) 'tree))))

(ert-deftest gitq-test--parse-via/tree-entries-unfiltered ()
  "Without a type filter, :filter is nil."
  (let* ((nodes (gitq--parse-flat "HEAD via .tree.entries /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'tree-entries))
    (should (null (plist-get via :filter)))))

(ert-deftest gitq-test--parse-via/diff ()
  (let* ((nodes (gitq--parse-flat "HEAD via .diff /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'diff))))

(ert-deftest gitq-test--parse-via/diff-hunks ()
  (let* ((nodes (gitq--parse-flat "HEAD via .diff.hunks /show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'diff-hunks))))

(ert-deftest gitq-test--parse-via/history ()
  (let* ((nodes (gitq--parse-flat "blobs path \"auth.ts\" via .history /show"))
         (via   (nth 2 nodes)))
    (should (eq (plist-get via :morphism) 'history))))

(ert-deftest gitq-test--parse-via/commit ()
  "`.commit' resolves via `:commit-sha', which only hunk/line frames
carry (not blobs) -- reach a line frame first via `grep'."
  (let* ((nodes (gitq--parse-flat "commits grep \"x\" via .commit /show"))
         (via   (nth 2 nodes)))
    (should (eq (plist-get via :morphism) 'commit))))

(ert-deftest gitq-test--parse-via/unknown-morphism ()
  "An unknown morphism signals an error at parse time."
  (should-error (gitq--parse-flat "commits via .nonExistent /show")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse-flat — where step
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-where/equality ()
  (let* ((nodes (gitq--parse-flat "commits where author == \"alice\" /show"))
         (where (nth 1 nodes))
         (cond  (car (plist-get where :conditions))))
    (should (eq (plist-get where :type) 'where))
    (should (eq (plist-get cond :field) 'author))
    (should (eq (plist-get cond :op)    '==))
    (should (equal (plist-get cond :value) "alice"))))

(ert-deftest gitq-test--parse-where/contains ()
  (let* ((nodes (gitq--parse-flat "commits where message contains \"fix\" /show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :op) 'contains))
    (should (equal (plist-get cond :value) "fix"))))

(ert-deftest gitq-test--parse-where/matches-regex ()
  (let* ((nodes (gitq--parse-flat "commits where message matches /^feat:/ /show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :op) 'matches))
    (should (equal (plist-get cond :value) "^feat:"))))

(ert-deftest gitq-test--parse-where/numeric-gt ()
  (let* ((nodes (gitq--parse-flat "commits where parents-count > 1 /show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :field) 'parents-count))
    (should (eq (plist-get cond :op) '>))
    (should (= (plist-get cond :value) 1))))

(ert-deftest gitq-test--parse-where/multiple-conditions ()
  "Multiple where conditions separated by commas."
  (let* ((nodes (gitq--parse-flat "commits where author == \"alice\", message contains \"fix\" /show"))
         (conds (plist-get (nth 1 nodes) :conditions)))
    (should (= (length conds) 2))
    (should (eq (plist-get (nth 0 conds) :field) 'author))
    (should (eq (plist-get (nth 1 conds) :field) 'message))))

(ert-deftest gitq-test--parse-where/bare-flag ()
  "Bare modified flag (no op/value)."
  (let* ((nodes (gitq--parse-flat "worktree where modified /show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :field) 'modified))
    (should (eq (plist-get cond :op)    'is))
    (should (eq (plist-get cond :value) t))))

(ert-deftest gitq-test--parse-where/after ()
  (let* ((nodes (gitq--parse-flat "commits where date after \"2024-01-01\" /show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :op) 'after))
    (should (equal (plist-get cond :value) "2024-01-01"))))

(ert-deftest gitq-test--parse-where/within ()
  (let* ((nodes (gitq--parse-flat "commits where date within \"30 days\" /show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :op) 'within))
    (should (equal (plist-get cond :value) "30 days"))))

(ert-deftest gitq-test--parse-where/sha-equality ()
  (let* ((nodes (gitq--parse-flat "commits where sha == \"a3f9b2\" /show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (equal (plist-get cond :value) "a3f9b2"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse-flat — pick step
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-pick/single-field ()
  (let* ((nodes (gitq--parse-flat "commits pick sha /show"))
         (pick  (nth 1 nodes)))
    (should (eq (plist-get pick :type) 'pick))
    (should (equal (plist-get pick :fields) '(sha)))))

(ert-deftest gitq-test--parse-pick/multiple-fields ()
  (let* ((nodes (gitq--parse-flat "commits pick sha, message, author /show"))
         (pick  (nth 1 nodes)))
    (should (equal (plist-get pick :fields) '(sha message author)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse-flat — navigation steps
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-take ()
  (let* ((nodes (gitq--parse-flat "commits take 10 /show"))
         (take  (nth 1 nodes)))
    (should (eq (plist-get take :type) 'take))
    (should (= (plist-get take :n) 10))))

(ert-deftest gitq-test--parse-skip ()
  (let* ((nodes (gitq--parse-flat "commits skip 3 /show"))
         (skip  (nth 1 nodes)))
    (should (eq (plist-get skip :type) 'skip))
    (should (= (plist-get skip :n) 3))))

(ert-deftest gitq-test--parse-first ()
  (let* ((nodes (gitq--parse-flat "commits first /show"))
         (step  (nth 1 nodes)))
    (should (eq (plist-get step :type) 'first))))

(ert-deftest gitq-test--parse-last ()
  (let* ((nodes (gitq--parse-flat "commits last /show"))
         (step  (nth 1 nodes)))
    (should (eq (plist-get step :type) 'last))))

(ert-deftest gitq-test--parse-sort/ascending ()
  (let* ((nodes (gitq--parse-flat "commits sort date /show"))
         (sort  (nth 1 nodes)))
    (should (eq (plist-get sort :type) 'sort))
    (should (eq (plist-get sort :field) 'date))
    (should (null (plist-get sort :desc)))))

(ert-deftest gitq-test--parse-sort/descending ()
  (let* ((nodes (gitq--parse-flat "commits sort -date /show"))
         (sort  (nth 1 nodes)))
    (should (eq (plist-get sort :field) 'date))
    (should (plist-get sort :desc))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse-flat — grep / pickaxe / path
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-grep/literal ()
  (let* ((nodes (gitq--parse-flat "commits grep \"TODO\" /show"))
         (grep  (nth 1 nodes)))
    (should (eq (plist-get grep :type) 'grep))
    (should (equal (plist-get grep :pattern) "TODO"))
    (should (null (plist-get grep :regex)))
    (should (null (plist-get grep :path-filter)))))

(ert-deftest gitq-test--parse-grep/regex ()
  (let* ((nodes (gitq--parse-flat "commits grep /TODO|FIXME/ /show"))
         (grep  (nth 1 nodes)))
    (should (equal (plist-get grep :pattern) "TODO|FIXME"))
    (should (plist-get grep :regex))))

(ert-deftest gitq-test--parse-grep/with-path ()
  "Unlike pipe syntax's inline \"grep PATTERN path FILTER\", flat syntax
composes grep's path filtering as a separate `path' step instead --
grep itself never sets :path-filter."
  (let* ((nodes (gitq--parse-flat "commits grep \"TODO\" path \"*.ts\" /show"))
         (grep  (nth 1 nodes))
         (path  (nth 2 nodes)))
    (should (null (plist-get grep :path-filter)))
    (should (eq (plist-get path :type) 'path))
    (should (equal (plist-get path :pattern) "*.ts"))))

(ert-deftest gitq-test--parse-pickaxe/literal ()
  (let* ((nodes  (gitq--parse-flat "commits pickaxe \"SecretKey\" /show"))
         (pa     (nth 1 nodes)))
    (should (eq (plist-get pa :type) 'pickaxe))
    (should (equal (plist-get pa :pattern) "SecretKey"))
    (should (null (plist-get pa :regex)))))

(ert-deftest gitq-test--parse-pickaxe/regex ()
  (let* ((nodes (gitq--parse-flat "commits pickaxe /password\\s*=/ regex /show"))
         (pa    (nth 1 nodes)))
    (should (plist-get pa :regex))))

(ert-deftest gitq-test--parse-path ()
  "The standalone `path' step needs a `:path' field -- `blobs' (not
`commits', which are commit-shaped and carry no `:path') has one."
  (let* ((nodes (gitq--parse-flat "blobs path \"src/auth.ts\" /show"))
         (path  (nth 1 nodes)))
    (should (eq (plist-get path :type) 'path))
    (should (equal (plist-get path :pattern) "src/auth.ts"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse-flat — terminal operations
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-terminal/show ()
  (let* ((nodes (gitq--parse-flat "commits /show"))
         (term  (nth 1 nodes)))
    (should (eq (plist-get term :type) 'terminal))
    (should (eq (plist-get term :op)   'show))))

(ert-deftest gitq-test--parse-terminal/copy ()
  (let* ((term (car (last (gitq--parse-flat "commits /copy")))))
    (should (eq (plist-get term :op) 'copy))))

(ert-deftest gitq-test--parse-terminal/count ()
  (let* ((term (car (last (gitq--parse-flat "commits /count")))))
    (should (eq (plist-get term :op) 'count))))

(ert-deftest gitq-test--parse-terminal/branch-off-bare ()
  (let* ((term (car (last (gitq--parse-flat "commits /branch-off")))))
    (should (eq (plist-get term :op) 'branch-off))
    (should (null (plist-get term :name)))))

(ert-deftest gitq-test--parse-terminal/branch-off-named ()
  (let* ((term (car (last (gitq--parse-flat "commits /branch-off \"feature/x\"")))))
    (should (eq (plist-get term :op) 'branch-off))
    (should (equal (plist-get term :name) "feature/x"))))

(ert-deftest gitq-test--parse-terminal/branch-off-worktree ()
  (let* ((term (car (last (gitq--parse-flat "commits /branch-off \"feature/x\" worktree \"../wt\"")))))
    (should (equal (plist-get term :name) "feature/x"))
    (should (equal (plist-get term :worktree) "../wt"))))

(ert-deftest gitq-test--parse-terminal/amend-bare ()
  (let* ((term (car (last (gitq--parse-flat "HEAD /amend")))))
    (should (eq (plist-get term :op) 'amend))
    (should (null (plist-get term :no-edit)))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/amend-no-edit ()
  (let* ((term (car (last (gitq--parse-flat "HEAD /amend no-edit")))))
    (should (plist-get term :no-edit))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/amend-message ()
  (let* ((term (car (last (gitq--parse-flat "HEAD /amend \"new message\"")))))
    (should (null (plist-get term :no-edit)))
    (should (equal (plist-get term :message) "new message"))))

(ert-deftest gitq-test--parse-terminal/squash-bare ()
  (let* ((term (car (last (gitq--parse-flat "HEAD /squash")))))
    (should (eq (plist-get term :op) 'squash))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/squash-message ()
  (let* ((term (car (last (gitq--parse-flat "HEAD via .parent* take 3 /squash \"consolidated\"")))))
    (should (eq (plist-get term :op) 'squash))
    (should (equal (plist-get term :message) "consolidated"))))

(ert-deftest gitq-test--parse-terminal/reword-bare ()
  (let* ((term (car (last (gitq--parse-flat "commits /reword")))))
    (should (eq (plist-get term :op) 'reword))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/reword-message ()
  (let* ((term (car (last (gitq--parse-flat "commits /reword \"new message\"")))))
    (should (equal (plist-get term :message) "new message"))))

(ert-deftest gitq-test--parse-terminal/remove ()
  (let* ((term (car (last (gitq--parse-flat "commits /remove")))))
    (should (eq (plist-get term :op) 'remove))))

(ert-deftest gitq-test--parse-terminal/commit-bare ()
  (let* ((term (car (last (gitq--parse-flat "worktree /commit")))))
    (should (eq (plist-get term :op) 'commit))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/commit-message ()
  (let* ((term (car (last (gitq--parse-flat "worktree /commit \"fix: auth\"")))))
    (should (equal (plist-get term :message) "fix: auth"))))

(ert-deftest gitq-test--parse-terminal/mark ()
  (let* ((term (car (last (gitq--parse-flat "commits /mark \"stable\"")))))
    (should (eq (plist-get term :op) 'mark))
    (should (equal (plist-get term :label) "stable"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse-flat — full pipeline structure
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse/pipeline-length ()
  "A 4-stage pipeline parses to 4 nodes."
  (let ((nodes (gitq--parse-flat "commits where author == \"alice\" take 10 /show")))
    (should (= (length nodes) 4))))

(ert-deftest gitq-test--parse/pipeline-types ()
  "Node types are source, where, take, terminal in order."
  (let ((nodes (gitq--parse-flat "commits where author == \"alice\" take 10 /show")))
    (should (eq (plist-get (nth 0 nodes) :type) 'source))
    (should (eq (plist-get (nth 1 nodes) :type) 'where))
    (should (eq (plist-get (nth 2 nodes) :type) 'take))
    (should (eq (plist-get (nth 3 nodes) :type) 'terminal))))

(ert-deftest gitq-test--parse/empty-pipeline-errors ()
  "An empty string signals an error."
  (should-error (gitq--parse-flat "")))

(ert-deftest gitq-test--parse/complex-pipeline ()
  "Example 3 from spec: commits introducing a string via pickaxe.
Picks `sha, path, parent-sha' rather than the original spec wording's
`sha, date, author, path' -- after `via .diff' the frame is diff-shaped
and diff frames never carried `:date'/`:author' (only commits do), so
that combination could never actually resolve either field; this is
exactly the class of bug the field-type system now catches at parse
time instead of silently picking nil for them."
  (let* ((nodes (gitq--parse-flat
                 "commits via .diff pickaxe \"SecretKey\" pick sha, path, parent-sha"))
         (src   (nth 0 nodes))
         (via   (nth 1 nodes))
         (pa    (nth 2 nodes))
         (pick  (nth 3 nodes)))
    (should (eq (plist-get src :source) 'commits))
    (should (eq (plist-get via :morphism) 'diff))
    (should (eq (plist-get pa :type) 'pickaxe))
    (should (equal (plist-get pick :fields) '(sha path parent-sha)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse-commit-line
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-commit-line/basic ()
  "A well-formed commit line is parsed into a frame plist."
  (let* ((sha  "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2")
         (line (concat sha "\x00test@example.com\x00Test User\x00"
                       "2024-06-01 12:00:00 +0000\x00"
                       "0000000000000000000000000000000000000000\x00"
                       "tree1234567890\x00"
                       "Initial commit"))
         (frame (gitq--parse-commit-line line)))
    (should (eq (plist-get frame :type) 'commit))
    (should (equal (plist-get frame :sha) sha))
    (should (equal (plist-get frame :author) "Test User"))
    (should (equal (plist-get frame :email) "test@example.com"))
    (should (equal (plist-get frame :message) "Initial commit"))))

(ert-deftest gitq-test--parse-commit-line/multiple-parents ()
  "Multiple parents in a merge commit are parsed into a list."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "a\n" "first")
    (gitq-test--commit "f.txt" "b\n" "second")
    ;; Create a second branch with a diverging commit
    (call-process "git" nil nil nil "checkout" "-q" "-b" "side")
    (gitq-test--commit "g.txt" "c\n" "side commit")
    (call-process "git" nil nil nil "checkout" "-q" "main")
    ;; Merge with --no-ff to create an actual merge commit
    (call-process "git" nil nil nil "merge" "--no-ff" "-m" "Merge branch side" "side")
    (let* ((frames (gitq--fetch-commits))
           (merge  (cl-find-if (lambda (f)
                                 (equal (plist-get f :message) "Merge branch side"))
                               frames)))
      (should merge)
      (should (= (length (plist-get merge :parents)) 2)))))

(ert-deftest gitq-test--parse-commit-line/empty-sha ()
  "A line with an empty SHA returns nil."
  (should (null (gitq--parse-commit-line "\x00email\x00author\x00date\x00\x00tree\x00msg"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--path-matches
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--path-matches/exact ()
  (should (gitq--path-matches "src/auth.ts" "src/auth.ts")))

(ert-deftest gitq-test--path-matches/glob-extension ()
  (should (gitq--path-matches "src/auth.ts" "*.ts")))

(ert-deftest gitq-test--path-matches/glob-no-match ()
  (should-not (gitq--path-matches "src/auth.js" "*.ts")))

(ert-deftest gitq-test--path-matches/glob-path ()
  (should (gitq--path-matches "src/auth/token.ts" "src/auth*")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--eval-condition
;;; ─────────────────────────────────────────────────────────────────────────────

(defun gitq-test--cond (field op value)
  "Build a condition plist for testing."
  (list :field field :op op :value value))

(ert-deftest gitq-test--eval-condition/eq-string ()
  (let ((frame '(:type commit :author "Alice")))
    (should (gitq--eval-condition frame (gitq-test--cond 'author '== "Alice")))
    (should-not (gitq--eval-condition frame (gitq-test--cond 'author '== "Bob")))))

(ert-deftest gitq-test--eval-condition/contains ()
  (let ((frame '(:type commit :message "fix: auth token")))
    (should (gitq--eval-condition frame (gitq-test--cond 'message 'contains "auth")))
    (should-not (gitq--eval-condition frame (gitq-test--cond 'message 'contains "xyz")))))

(ert-deftest gitq-test--eval-condition/matches-regex ()
  (let ((frame '(:type commit :message "feat: new login")))
    (should (gitq--eval-condition frame (gitq-test--cond 'message 'matches "^feat:")))
    (should-not (gitq--eval-condition frame (gitq-test--cond 'message 'matches "^fix:")))))

(ert-deftest gitq-test--eval-condition/numeric-gt ()
  (let ((frame '(:type commit :parents ("a" "b"))))
    (should (gitq--eval-condition frame (gitq-test--cond 'parents-count '> 1)))
    (should-not (gitq--eval-condition frame (gitq-test--cond 'parents-count '> 2)))))

(ert-deftest gitq-test--eval-condition/numeric-eq ()
  (let ((frame '(:type commit :parents ("a"))))
    (should (gitq--eval-condition frame (gitq-test--cond 'parents-count '== 1)))))

(ert-deftest gitq-test--eval-condition/not-eq ()
  (let ((frame '(:type commit :author "Alice")))
    (should (gitq--eval-condition frame (gitq-test--cond 'author '!= "Bob")))
    (should-not (gitq--eval-condition frame (gitq-test--cond 'author '!= "Alice")))))

(ert-deftest gitq-test--eval-condition/is-bare-flag ()
  (let ((frame '(:type commit :modified t)))
    (should (gitq--eval-condition frame (gitq-test--cond 'modified 'is t)))
    (should-not (gitq--eval-condition '(:type commit) (gitq-test--cond 'modified 'is t)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--date-within
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--date-within/recent-date ()
  "A date from 1 hour ago is within 30 days."
  (let ((one-hour-ago (format-time-string "%Y-%m-%d %H:%M:%S %z"
                                          (time-subtract (current-time) 3600))))
    (should (gitq--date-within one-hour-ago "30 days"))))

(ert-deftest gitq-test--date-within/old-date ()
  "A date from 90 days ago is not within 30 days."
  (let ((old-date (format-time-string "%Y-%m-%d %H:%M:%S %z"
                                      (time-subtract (current-time) (* 90 86400)))))
    (should-not (gitq--date-within old-date "30 days"))))

(ert-deftest gitq-test--date-within/week ()
  "A date from 3 days ago is within 1 week."
  (let ((three-days-ago (format-time-string "%Y-%m-%d %H:%M:%S %z"
                                            (time-subtract (current-time) (* 3 86400)))))
    (should (gitq--date-within three-days-ago "1 week"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--exec-step — take / skip / first / last
;;; ─────────────────────────────────────────────────────────────────────────────

(defun gitq-test--commits (n)
  "Return N synthetic commit frames for testing."
  (cl-loop for i from 1 to n
           collect (list :type 'commit
                         :sha     (format "%040d" i)
                         :author  "Test"
                         :message (format "commit %d" i)
                         :date    "2024-01-01"
                         :parents nil)))

(ert-deftest gitq-test--exec-step/take ()
  (let* ((frames (gitq-test--commits 5))
         (result (gitq--exec-step frames '(:type take :n 3))))
    (should (= (length result) 3))
    (should (equal (plist-get (car result) :message) "commit 1"))))

(ert-deftest gitq-test--exec-step/skip ()
  (let* ((frames (gitq-test--commits 5))
         (result (gitq--exec-step frames '(:type skip :n 2))))
    (should (= (length result) 3))
    (should (equal (plist-get (car result) :message) "commit 3"))))

(ert-deftest gitq-test--exec-step/first ()
  (let* ((frames (gitq-test--commits 5))
         (result (gitq--exec-step frames '(:type first))))
    (should (= (length result) 1))
    (should (equal (plist-get (car result) :message) "commit 1"))))

(ert-deftest gitq-test--exec-step/last ()
  (let* ((frames (gitq-test--commits 5))
         (result (gitq--exec-step frames '(:type last))))
    (should (= (length result) 1))
    (should (equal (plist-get (car result) :message) "commit 5"))))

(ert-deftest gitq-test--exec-step/first-empty ()
  "first on empty list returns nil."
  (should (null (gitq--exec-step nil '(:type first)))))

(ert-deftest gitq-test--exec-step/take-more-than-available ()
  "Taking more frames than exist returns all frames."
  (let* ((frames (gitq-test--commits 3))
         (result (gitq--exec-step frames '(:type take :n 100))))
    (should (= (length result) 3))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--exec-step — where
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--exec-step/where-author ()
  (let* ((frames (list '(:type commit :author "Alice" :message "a" :parents nil)
                       '(:type commit :author "Bob"   :message "b" :parents nil)
                       '(:type commit :author "Alice" :message "c" :parents nil)))
         (result (gitq--exec-step frames
                                  '(:type where
                                    :conditions ((:field author :op == :value "Alice"))))))
    (should (= (length result) 2))
    (should (cl-every (lambda (f) (equal (plist-get f :author) "Alice")) result))))

(ert-deftest gitq-test--exec-step/where-message-contains ()
  (let* ((frames (list '(:type commit :message "fix: auth" :author "A" :parents nil)
                       '(:type commit :message "feat: new" :author "B" :parents nil)
                       '(:type commit :message "fix: log"  :author "C" :parents nil)))
         (result (gitq--exec-step frames
                                  '(:type where
                                    :conditions ((:field message :op contains :value "fix"))))))
    (should (= (length result) 2))))

(ert-deftest gitq-test--exec-step/where-merge-commits ()
  "Filter to merge commits (>1 parent)."
  (let* ((frames (list '(:type commit :parents ("a" "b") :message "merge")
                       '(:type commit :parents ("a")     :message "regular")
                       '(:type commit :parents ("a" "b" "c") :message "octopus")))
         (result (gitq--exec-step frames
                                  '(:type where
                                    :conditions ((:field parents-count :op > :value 1))))))
    (should (= (length result) 2))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--exec-step — pick
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--exec-step/pick ()
  "Projected keys are keywords, matching every other frame type -- a bare
symbol key would silently break keyword-based lookups downstream (e.g.
`gitq--frame-commit-sha', the results-buffer RET/b/c commands)."
  (let* ((frames (list '(:type commit :sha "abc" :author "Alice" :message "msg")))
         (result (gitq--exec-step frames '(:type pick :fields (sha author)))))
    (should (= (length result) 1))
    (should (equal (plist-get (car result) :sha) "abc"))
    (should (equal (plist-get (car result) :author) "Alice"))
    ;; message was not picked
    (should (null (plist-get (car result) :message)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--exec-step — sort
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--exec-step/sort-ascending ()
  (let* ((frames (list '(:type commit :date "2024-03-01")
                       '(:type commit :date "2024-01-01")
                       '(:type commit :date "2024-02-01")))
         (result (gitq--exec-step frames '(:type sort :field date :desc nil))))
    (should (equal (plist-get (nth 0 result) :date) "2024-01-01"))
    (should (equal (plist-get (nth 1 result) :date) "2024-02-01"))
    (should (equal (plist-get (nth 2 result) :date) "2024-03-01"))))

(ert-deftest gitq-test--exec-step/sort-descending ()
  (let* ((frames (list '(:type commit :date "2024-01-01")
                       '(:type commit :date "2024-03-01")
                       '(:type commit :date "2024-02-01")))
         (result (gitq--exec-step frames '(:type sort :field date :desc t))))
    (should (equal (plist-get (nth 0 result) :date) "2024-03-01"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Integration tests — require a real git repo
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--integration/fetch-commits ()
  "gitq--fetch-commits returns commit frames from the repo."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "a.txt" "hello\n" "initial commit")
    (gitq-test--commit "b.txt" "world\n" "second commit")
    (let ((frames (gitq--fetch-commits)))
      (should (>= (length frames) 2))
      (should (eq (plist-get (car frames) :type) 'commit))
      (should (plist-get (car frames) :sha))
      (should (plist-get (car frames) :message)))))

(ert-deftest gitq-test--integration/fetch-commit-by-ref ()
  "gitq--fetch-commit resolves a ref to a commit frame."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "test commit")
    (let ((frame (gitq--fetch-commit "HEAD")))
      (should (eq (plist-get frame :type) 'commit))
      (should (equal (plist-get frame :message) "test commit")))))

(ert-deftest gitq-test--integration/fetch-branches ()
  "gitq--fetch-branches returns the current branch."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((refs (gitq--fetch-branches)))
      (should (>= (length refs) 1))
      (should (cl-find-if (lambda (r) (equal (plist-get r :name) "main")) refs)))))

(ert-deftest gitq-test--integration/where-author ()
  "Where filter by author works against a real repo."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "a\n" "commit by test user")
    (let* ((frames (gitq--fetch-commits))
           (result (gitq--exec-step frames
                                    '(:type where
                                      :conditions ((:field author :op contains
                                                    :value "Test"))))))
      (should (>= (length result) 1))
      (should (cl-every
               (lambda (f)
                 (string-match-p "Test" (or (plist-get f :author) "")))
               result)))))

(ert-deftest gitq-test--integration/take-limits-results ()
  "take N limits the commit list."
  :tags '(integration)
  (gitq-test--with-repo
    (dotimes (i 5)
      (gitq-test--commit "f.txt" (format "%d\n" i) (format "commit %d" i)))
    (let* ((frames (gitq--fetch-commits))
           (result (gitq--exec-step frames '(:type take :n 2))))
      (should (= (length result) 2)))))

(ert-deftest gitq-test--integration/fetch-blobs ()
  "gitq--fetch-blobs-at returns blob entries from a tree."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "auth.ts" "const x = 1;\n" "add auth")
    (gitq-test--commit "log.ts"  "const y = 2;\n" "add log")
    (let* ((tree   (gitq--git-string "rev-parse" "HEAD^{tree}"))
           (blobs  (gitq--fetch-blobs-at tree)))
      (should (>= (length blobs) 2))
      (should (cl-find-if (lambda (b) (string-match-p "auth\\.ts" (or (plist-get b :path) "")))
                          blobs)))))

(ert-deftest gitq-test--integration/path-filter ()
  "Path filter on blob frames."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "auth.ts" "x\n" "add ts")
    (gitq-test--commit "util.js" "y\n" "add js")
    (let* ((tree  (gitq--git-string "rev-parse" "HEAD^{tree}"))
           (blobs (gitq--fetch-blobs-at tree))
           (result (gitq--exec-step blobs '(:type path :pattern "*.ts"))))
      (should (= (length result) 1))
      (should (string-match-p "\\.ts$" (plist-get (car result) :path))))))

(ert-deftest gitq-test--integration/via-parent ()
  "via .parent traverses one commit back."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "a\n" "first")
    (gitq-test--commit "f.txt" "b\n" "second")
    (let* ((head   (gitq--fetch-commit "HEAD"))
           (frames (gitq--exec-via (list head) '(:type via :morphism parent))))
      (should (= (length frames) 1))
      (should (equal (plist-get (car frames) :message) "first")))))

(ert-deftest gitq-test--integration/via-parent-star ()
  "via .parent* returns all ancestors including the start."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "a\n" "first")
    (gitq-test--commit "f.txt" "b\n" "second")
    (gitq-test--commit "f.txt" "c\n" "third")
    (let* ((head   (gitq--fetch-commit "HEAD"))
           (frames (gitq--exec-via (list head) '(:type via :morphism parent :star t))))
      ;; Should include all 3 commits
      (should (= (length frames) 3)))))

(ert-deftest gitq-test--integration/via-diff ()
  "via .diff produces diff frames with :path."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "a.txt" "line1\n" "initial")
    (gitq-test--commit "a.txt" "line1\nline2\n" "add line")
    (let* ((head   (gitq--fetch-commit "HEAD"))
           (frames (gitq--exec-via (list head) '(:type via :morphism diff))))
      (should (>= (length frames) 1))
      (should (eq (plist-get (car frames) :type) 'diff))
      (should (plist-get (car frames) :path)))))

(ert-deftest gitq-test--integration/via-diff-root-commit ()
  "via .diff on a root commit (no parent) must diff against the empty
tree via `--root', not crash or silently swallow a git error message
as if it were real path data.  Before the fix, `sha^' on a root commit
is an invalid revision; `gitq--git' mixed that error text into stdout,
so the bogus error lines came back looking like real (garbage) paths."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "a.txt" "line1\n" "initial")
    (let* ((root   (gitq--fetch-commit "HEAD"))
           (frames (gitq--exec-via (list root) '(:type via :morphism diff))))
      (should (= (length frames) 1))
      (should (equal (plist-get (car frames) :path) "a.txt"))
      (should (null (plist-get (car frames) :parent-sha))))))

(ert-deftest gitq-test--integration/git-does-not-leak-stderr-into-output ()
  "`gitq--git' must discard stderr, not mix it into the captured stdout
buffer -- otherwise a git error message gets split into lines and
returned as if it were real output."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "a.txt" "line1\n" "initial")
    ;; "HEAD^" on a root commit is an invalid revision -- git writes an
    ;; error to stderr and nothing to stdout.
    (should (null (gitq--git "diff-tree" "-r" "--name-only" "--no-commit-id"
                             "HEAD^" "HEAD")))))

(ert-deftest gitq-test--integration/pick-keys-are-keywords ()
  "Regression test: `pick' must project onto keyword keys so downstream
keyword-based lookups (`gitq--frame-commit-sha', the results-buffer
RET/b/c commands) still resolve on picked frames."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "a.txt" "line1\n" "initial commit")
    (let* ((result (car (gitq--exec-nodes (gitq--parse-flat "commits pick sha, message"))))
           (frame  (car result)))
      (should (equal (plist-get frame :message) "initial commit"))
      (should (equal (gitq--frame-commit-sha frame) (plist-get frame :sha))))))

(ert-deftest gitq-test--integration/via-tree-entries ()
  "via .tree.entries[Blob] returns blob frames."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "a.ts" "x\n" "add a")
    (gitq-test--commit "b.ts" "y\n" "add b")
    (let* ((head   (gitq--fetch-commit "HEAD"))
           (frames (gitq--exec-via (list head)
                                   '(:type via :morphism tree-entries :filter blob))))
      (should (>= (length frames) 2))
      (should (cl-every (lambda (f) (eq (plist-get f :type) 'blob)) frames)))))

(ert-deftest gitq-test--integration/parse-diff-hunks ()
  "gitq--parse-diff-hunks extracts hunk frames from diff text."
  :tags '(integration)
  (let* ((diff "diff --git a/src/auth.ts b/src/auth.ts\n\
index abc..def 100644\n\
--- a/src/auth.ts\n\
+++ b/src/auth.ts\n\
@@ -10,3 +10,5 @@\n\
 context\n\
+new line 1\n\
+new line 2\n\
 context2\n")
         (hunks (gitq--parse-diff-hunks diff "deadbeef00000000000000000000000000000000")))
    (should (= (length hunks) 1))
    (should (eq (plist-get (car hunks) :type) 'hunk))
    (should (equal (plist-get (car hunks) :path) "src/auth.ts"))
    (should (= (plist-get (car hunks) :start-line) 10))
    (should (equal (plist-get (car hunks) :commit-sha)
                   "deadbeef00000000000000000000000000000000"))))

(ert-deftest gitq-test--integration/parse-diff-hunks/multiple ()
  "Multiple hunks in one diff are all parsed."
  (let* ((diff "diff --git a/f.txt b/f.txt\n\
--- a/f.txt\n\
+++ b/f.txt\n\
@@ -1,1 +1,2 @@\n\
+added\n\
 context\n\
@@ -20,1 +21,2 @@\n\
+another\n\
 context\n")
         (hunks (gitq--parse-diff-hunks diff "abc")))
    (should (= (length hunks) 2))
    (should (= (plist-get (nth 0 hunks) :start-line) 1))
    (should (= (plist-get (nth 1 hunks) :start-line) 21))))

(ert-deftest gitq-test--integration/full-query-commits-show ()
  "Full gitq pipeline: commits take 1 /show — creates *gitq* buffer."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (gitq "commits take 1 /show")
    (let ((buf (get-buffer "*gitq*")))
      (should buf)
      (with-current-buffer buf
        (should (string-match-p "gitq:" (buffer-string)))
        (should (> (buffer-size) 0))))))

(ert-deftest gitq-test--integration/full-query-where-message ()
  "commits where message contains X /show filters correctly."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "a.txt" "1\n" "fix: auth issue")
    (gitq-test--commit "b.txt" "2\n" "feat: new widget")
    (gitq-test--commit "c.txt" "3\n" "fix: logging bug")
    (gitq "commits where message contains \"fix\" /show")
    (let ((buf (get-buffer "*gitq*")))
      (with-current-buffer buf
        ;; Should show 2 fix commits, not the feat one
        (should (string-match-p "fix" (buffer-string)))
        (should-not (string-match-p "feat" (buffer-string)))))))

(ert-deftest gitq-test--integration/full-query-branch-off ()
  "commits first /branch-off creates the branch."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (gitq "commits first /branch-off \"test-branch\"")
    (let ((branches (gitq--git "branch" "--list" "test-branch")))
      (should (>= (length branches) 1)))))

(ert-deftest gitq-test--integration/full-query-head-parent ()
  "HEAD via .parent /show navigates to parent commit."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "a\n" "the-parent-msg")
    (gitq-test--commit "f.txt" "b\n" "the-head-msg")
    (gitq "HEAD via .parent /show")
    (let ((buf (get-buffer "*gitq*")))
      (with-current-buffer buf
        ;; The results section (after the header) should show the parent message
        (let ((body (buffer-substring-no-properties (point-min) (point-max))))
          ;; parent message appears as a commit result line
          (should (string-match-p "the-parent-msg" body))
          ;; head message should NOT appear as a result (it's not the parent)
          (should-not (string-match-p "the-head-msg" body)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--exec-nodes
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--exec-nodes/no-terminal ()
  "A pipeline with no terminal returns (RESULT . nil)."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let* ((exec (gitq--exec-nodes (gitq--parse-flat "commits")))
           (result (car exec)) (terminal (cdr exec)))
      (should (null terminal))
      (should (= (length result) 1)))))

(ert-deftest gitq-test--exec-nodes/with-terminal-not-applied ()
  "A terminal is identified and returned, but never itself executed."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let* ((exec (gitq--exec-nodes (gitq--parse-flat "commits /branch-off \"unapplied-branch\"")))
           (result (car exec)) (terminal (cdr exec)))
      (should (eq (plist-get terminal :type) 'terminal))
      (should (eq (plist-get terminal :op) 'branch-off))
      (should (= (length result) 1))
      ;; gitq--exec-nodes must not have created the branch itself.
      (should (= (length (gitq--git "branch" "--list" "unapplied-branch")) 0)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--preview-frames
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--preview-frames/mid-token-is-not-ready ()
  "A partial source keyword (\"commi\" toward \"commits\") previews as nil,
not as a resolved-but-empty ref lookup."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (should (null (gitq--preview-frames "commi")))))

(ert-deftest gitq-test--preview-frames/mid-step-keyword-is-not-ready ()
  "A partial step keyword (\"wh\" toward \"where\") fails to parse, so
preview is nil."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (should (null (gitq--preview-frames "commits wh")))))

(ert-deftest gitq-test--preview-frames/complete-source-only ()
  "A bare, complete source keyword previews immediately."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((r (gitq--preview-frames "commits")))
      (should (eq (car r) :ok))
      (should (= (length (cdr r)) 1)))))

(ert-deftest gitq-test--preview-frames/complete-with-where ()
  "A complete pipeline with a where-clause previews the filtered result."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "commit by test user")
    (let ((r (gitq--preview-frames
              "commits where author contains \"Test\"")))
      (should (eq (car r) :ok))
      (should (= (length (cdr r)) 1)))))

(ert-deftest gitq-test--preview-frames/existing-branch-as-bare-source ()
  "An existing branch name used as a bare source resolves and previews."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((r (gitq--preview-frames "main")))
      (should (eq (car r) :ok))
      (should (= (length (cdr r)) 1)))))

(ert-deftest gitq-test--preview-frames/nonexistent-ref-is-not-ready ()
  "A bare source naming no real ref previews as nil, not as an empty result."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (should (null (gitq--preview-frames "zzzz-does-not-exist")))))

(ert-deftest gitq-test--preview-frames/destructive-terminal-not-applied ()
  "Previewing a pipeline whose terminal is destructive must not run it."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((r (gitq--preview-frames
              "commits /branch-off \"should-not-exist\"")))
      (should (eq (car r) :ok))
      (should (= (length (cdr r)) 1)))
    (should (= (length (gitq--git "branch" "--list" "should-not-exist")) 0))))

(ert-deftest gitq-test--preview-frames/outside-repo-is-not-ready ()
  "Outside a git repository, preview fails silently (nil), not with an error."
  (let* ((dir (make-temp-file "gitq-test-norepo-" t))
         (default-directory dir))
    (unwind-protect
        (should (null (gitq--preview-frames "commits")))
      (delete-directory dir t))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--token-kind
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--token-kind/source ()
  (should (equal (gitq--token-kind "commits") "source"))
  (should (equal (gitq--token-kind "HEAD") "source"))
  (should (equal (gitq--token-kind "in") "source")))

(ert-deftest gitq-test--token-kind/step ()
  (should (equal (gitq--token-kind "where") "step"))
  (should (equal (gitq--token-kind "take") "step")))

(ert-deftest gitq-test--token-kind/morphism ()
  (should (equal (gitq--token-kind ".parent") "morphism"))
  (should (equal (gitq--token-kind ".parent*") "morphism")))

(ert-deftest gitq-test--token-kind/field ()
  (should (equal (gitq--token-kind "date") "field"))
  ;; sort's negated "-field" form must resolve the same as "field".
  (should (equal (gitq--token-kind "-date") "field")))

(ert-deftest gitq-test--token-kind/operator ()
  (should (equal (gitq--token-kind "contains") "operator"))
  (should (equal (gitq--token-kind "==") "operator")))

(ert-deftest gitq-test--token-kind/terminal ()
  (should (equal (gitq--token-kind "/show") "terminal"))
  (should (equal (gitq--token-kind "/branch-off") "terminal")))

(ert-deftest gitq-test--token-kind/unknown-is-nil ()
  (should (null (gitq--token-kind "not-a-real-token"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--complete--enclosing-step
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--enclosing-step/where ()
  (should (equal (gitq--complete--enclosing-step '("commits" "where" "author" "==" "\"a\""))
                 "where")))

(ert-deftest gitq-test--enclosing-step/skips-over-comma-and-fields ()
  "The most recent stage keyword is found even across a comma-separated list."
  (should (equal (gitq--complete--enclosing-step
                  '("commits" "pick" "sha" "," "author" "," "date"))
                 "pick")))

(ert-deftest gitq-test--enclosing-step/most-recent-wins ()
  "A later step keyword shadows an earlier one."
  (should (equal (gitq--complete--enclosing-step
                  '("commits" "where" "author" "==" "\"a\"" "via" ".parent"))
                 "via")))

(ert-deftest gitq-test--enclosing-step/none ()
  (should (null (gitq--complete--enclosing-step '("commits")))))

(ert-deftest gitq-test--enclosing-step/path-field-not-mistaken-for-path-step ()
  "`path' inside an open `where'/`pick' resolves as a field continuing that
stage, not as a fresh `path' step -- see `gitq--field-names' docstring."
  (should (equal (gitq--complete--enclosing-step '("commits" "pick" "path"))
                 "pick"))
  (should (equal (gitq--complete--enclosing-step '("commits" "where" "path"))
                 "where"))
  (should (equal (gitq--complete--enclosing-step '("commits" "where" "modified" "path"))
                 "where"))
  ;; But a genuine standalone `path' step is still recognized as such.
  (should (equal (gitq--complete--enclosing-step '("commits" "take" "5" "path"))
                 "path")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--complete-where-values
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--complete-where-values/date-within-is-duration-examples ()
  "`within' gets duration examples, not literal dates, and needs no repo."
  (should (equal (gitq--complete-where-values "date" "within")
                 gitq--complete-date-within-examples)))

(ert-deftest gitq-test--complete-where-values/freeform-fields-return-nil ()
  "Fields with no git-derivable value domain stay free-text."
  (should (null (gitq--complete-where-values "message" "contains")))
  (should (null (gitq--complete-where-values "parents-count" ">")))
  (should (null (gitq--complete-where-values "modified" "is")))
  (should (null (gitq--complete-where-values "staged" "is")))
  (should (null (gitq--complete-where-values "untracked" "is"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--complete-candidates — where-value completion
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--complete-candidates/email-uses-email-not-name ()
  "`email' must complete against addresses, not author display names."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((cands (gitq--complete-candidates "commits where email == ")))
      (should (member "test@example.com" cands))
      (should-not (member "Test User" cands)))))

(ert-deftest gitq-test--complete-candidates/author ()
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (should (member "Test User" (gitq--complete-candidates "commits where author == ")))))

(ert-deftest gitq-test--complete-candidates/date-returns-real-dates ()
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((cands (gitq--complete-candidates "commits where date == ")))
      (should (= (length cands) 1))
      (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" (car cands))))))

(ert-deftest gitq-test--complete-candidates/date-after-also-gets-real-dates ()
  "Every where-operator on `date' (not just `==') gets the same date list."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (should (gitq--complete-candidates "commits where date after "))))

(ert-deftest gitq-test--complete-candidates/path-same-list-for-eq-and-contains ()
  "`path == ' and `path contains ' must offer the identical candidate set."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "a.txt" "x\n" "add a")
    (gitq-test--commit "b.txt" "y\n" "add b")
    (let ((eq-cands       (gitq--complete-candidates "commits where path == "))
          (contains-cands (gitq--complete-candidates "commits where path contains ")))
      (should (member "a.txt" eq-cands))
      (should (member "b.txt" eq-cands))
      (should (equal eq-cands contains-cands)))))

(ert-deftest gitq-test--complete-candidates/name-and-branch-are-refs ()
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (call-process "git" nil nil nil "tag" "v1.0")
    (let ((name-cands   (gitq--complete-candidates "commits where name == "))
          (branch-cands (gitq--complete-candidates "commits where branch == ")))
      (should (member "main" name-cands))
      (should (member "v1.0" name-cands))
      (should (equal name-cands branch-cands)))))

(ert-deftest gitq-test--complete-candidates/sha ()
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((cands (gitq--complete-candidates "commits where sha == ")))
      (should (= (length cands) 1))
      (should (string-match-p "^[0-9a-f]+$" (car cands))))))

(ert-deftest gitq-test--complete-candidates/message-and-boolean-flags-stay-freeform ()
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (should (null (gitq--complete-candidates "commits where message contains ")))
    (should (null (gitq--complete-candidates "commits where modified is ")))
    (should (null (gitq--complete-candidates "commits where parents-count > ")))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--complete-candidates — pick/sort/where comma-list correctness
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--complete-candidates/pick-field-never-offers-operators ()
  "pick fields are a plain comma list; they must never suggest ==, contains, etc."
  (let ((cands (gitq--complete-candidates "commits pick sha ")))
    (should-not (member "==" cands))
    (should-not (member "contains" cands))
    (should (member "," cands))
    (should (member "where" cands))))

(ert-deftest gitq-test--complete-candidates/pick-comma-continues-with-field-names ()
  (let ((cands (gitq--complete-candidates "commits pick sha, ")))
    (should (member "author" cands))
    (should-not (member "," cands))))

(ert-deftest gitq-test--complete-candidates/sort-field-never-offers-comma ()
  "sort takes exactly one field; unlike where/pick it has no comma-list."
  (let ((cands (gitq--complete-candidates "commits sort date ")))
    (should-not (member "," cands))
    (should-not (member "==" cands))))

(ert-deftest gitq-test--complete-candidates/where-condition-offers-comma-to-continue ()
  "After a complete where-value, \",\" should be offered to add another condition."
  (let ((cands (gitq--complete-candidates "commits where author == \"Alice\" ")))
    (should (member "," cands))
    (should (member "where" cands))))

(ert-deftest gitq-test--complete-candidates/pick-path-field-not-confused-with-step ()
  "`path' inside `pick' resolves as a field, not as the step keyword ending
the field list -- the same collision covered at the parser level in
git-branch-off-gitq-flat-test.el."
  (let ((cands (gitq--complete-candidates "commits pick path, ")))
    (should (member "author" cands))
    (should-not (member "," cands))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--complete-candidates — .diff optional ref, terminals
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--complete-candidates/diff-offers-refs-and-steps ()
  "`.diff's REF argument is optional: offer refs, but also let the user
skip straight to a step or terminal."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((cands (gitq--complete-candidates "HEAD via .diff ")))
      (should (member "main" cands))
      (should (member "where" cands))
      (should (member "/show" cands)))))

(ert-deftest gitq-test--complete-candidates/amend-offers-no-edit ()
  (should (equal (gitq--complete-candidates "commits /amend ") '("no-edit"))))

(ert-deftest gitq-test--complete-candidates/other-terminals-offer-nothing ()
  "A terminal always ends the pipeline; only /amend has a known-shape argument."
  (should (null (gitq--complete-candidates "commits /branch-off ")))
  (should (null (gitq--complete-candidates "commits /mark ")))
  (should (null (gitq--complete-candidates "commits /count "))))

(ert-deftest gitq-test--complete-candidates/commits-in-still-offers-refs ()
  "commits-in completion still works after being refactored onto
`gitq--complete-refs'."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (should (member "main" (gitq--complete-candidates "commits in ")))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: strict trailing-token errors
;;;
;;; Every step must consume every token it's given -- a leftover token
;;; (usually a value that needed quotes) must raise a clear error instead
;;; of silently truncating the query. Root case this covers: an unquoted,
;;; multi-word value like a full commit date/time tokenizes into several
;;; separate tokens, since dashes/colons/plus aren't part of the bare-word
;;; character class -- without this check, only the first token would be
;;; used as the value and the rest silently dropped, filtering on the
;;; wrong (truncated) value with no error at all.
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse/unquoted-multiword-value-errors ()
  "An unquoted value with spaces leaves leftover tokens and must error
rather than silently truncate."
  (should-error (gitq--parse-flat "commits where date == 2025-12-05 09:30:00 +0400")))

(ert-deftest gitq-test--parse/unquoted-multiword-value-quoted-works ()
  "The correct, quoted form must still parse fine."
  (let ((nodes (gitq--parse-flat "commits where date == \"2025-12-05 09:30:00 +0400\"")))
    (should (equal (plist-get (car (plist-get (nth 1 nodes) :conditions)) :value)
                   "2025-12-05 09:30:00 +0400"))))

(ert-deftest gitq-test--parse/where-trailing-garbage-errors ()
  (should-error (gitq--parse-flat "commits where author == \"Alice\" extra")))

(ert-deftest gitq-test--parse/pick-trailing-garbage-errors ()
  "A pick field list only accepts field tokens and commas."
  (should-error (gitq--parse-flat "commits pick sha not-a-field")))

(ert-deftest gitq-test--parse/via-trailing-garbage-errors ()
  (should-error (gitq--parse-flat "commits via .parent extra")))

(ert-deftest gitq-test--parse/via-diff-still-allows-optional-ref ()
  (let ((nodes (gitq--parse-flat "commits via .diff main")))
    (should (equal (plist-get (nth 1 nodes) :ref) "main"))))

(ert-deftest gitq-test--parse/grep-trailing-garbage-errors ()
  (should-error (gitq--parse-flat "commits grep \"foo\" extra")))

(ert-deftest gitq-test--parse/pickaxe-trailing-garbage-errors ()
  (should-error (gitq--parse-flat "commits pickaxe \"foo\" extra")))

(ert-deftest gitq-test--parse/pickaxe-regex-flag-still-works ()
  (let ((nodes (gitq--parse-flat "commits pickaxe \"foo\" regex")))
    (should (plist-get (nth 1 nodes) :regex))))

(ert-deftest gitq-test--parse/source-trailing-garbage-errors ()
  (should-error (gitq--parse-flat "branches extra"))
  (should-error (gitq--parse-flat "some-branch-name extra")))

(ert-deftest gitq-test--parse/take-skip-sort-path-trailing-garbage-errors ()
  (should-error (gitq--parse-flat "commits take 5 extra"))
  (should-error (gitq--parse-flat "commits skip 5 extra"))
  (should-error (gitq--parse-flat "commits sort date extra"))
  (should-error (gitq--parse-flat "commits path \"*.ts\" extra"))
  (should-error (gitq--parse-flat "commits first extra"))
  (should-error (gitq--parse-flat "commits last extra")))

(ert-deftest gitq-test--parse/terminal-trailing-garbage-errors ()
  (should-error (gitq--parse-flat "commits /show extra"))
  (should-error (gitq--parse-flat "commits /count extra")))

(ert-deftest gitq-test--parse/terminal-branch-off-with-worktree-still-works ()
  (let ((nodes (gitq--parse-flat "commits /branch-off \"name\" worktree \"/tmp/wt\"")))
    (should (equal (plist-get (car (last nodes)) :name) "name"))
    (should (equal (plist-get (car (last nodes)) :worktree) "/tmp/wt"))))

(ert-deftest gitq-test--parse/terminal-branch-off-name-with-slash ()
  "A branch name containing a literal / (e.g. \"feature/x\") must not be
misread as this token's own closing / by the regex-vs-terminal scan."
  (let ((nodes (gitq--parse-flat "commits /branch-off \"feature/x\"")))
    (should (equal (plist-get (car (last nodes)) :name) "feature/x"))))

(ert-deftest gitq-test--parse/terminal-amend-no-edit-still-works ()
  (let ((nodes (gitq--parse-flat "commits /amend no-edit")))
    (should (plist-get (car (last nodes)) :no-edit))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: tokenizer robustness against unterminated quotes/regexes
;;;
;;; A still-being-typed, not-yet-closed quote or /regex/ used to signal
;;; args-out-of-range instead of gracefully treating the rest of the
;;; string as its content -- a real crash risk since completion re-runs
;;; the tokenizer on every keystroke while the user is mid-value.
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--tokenize-flat/unterminated-quote-does-not-error ()
  (should (equal (gitq--tokenize-flat "where author == \"Ali")
                 '("where" "author" "==" "\"Ali"))))

(ert-deftest gitq-test--current-token/unterminated-quote-does-not-error ()
  (should (equal (gitq--current-token "commits where date == \"202") "\"202")))

(ert-deftest gitq-test--completion-table/unterminated-quote-does-not-error ()
  "Regression test for the exact crash Vertico would hit: re-running
completion on every keystroke while a quoted value is still open."
  (should (equal (all-completions "commits where date == \"202" #'gitq--completion-table)
                 nil)))

(ert-deftest gitq-test--tokenize-flat/terminated-quote-still-works ()
  "Sanity check: fixing the unterminated case must not break the normal,
fully-quoted, multi-word value case."
  (should (equal (gitq--tokenize-flat "where author == \"Alice Smith\"")
                 '("where" "author" "==" "\"Alice Smith\""))))

;; Regression tests: a bare (unquoted) value that starts with a digit but
;; contains letters or dashes -- SHA prefixes, plain dates -- used to split
;; at the digit/letter or digit/dash boundary because the digit-starting
;; tokenizer branch only consumed 0-9, unlike the letter-starting branch
;; which consumed a much wider character class.  Fixed by giving the
;; digit-starting branch the same continuation set.

(ert-deftest gitq-test--tokenize-flat/bare-sha-prefix-is-one-token ()
  (should (equal (gitq--tokenize-flat "where sha contains 062062e9af")
                 '("where" "sha" "contains" "062062e9af"))))

(ert-deftest gitq-test--tokenize-flat/bare-date-is-one-token ()
  (should (equal (gitq--tokenize-flat "where date after 2026-05-25")
                 '("where" "date" "after" "2026-05-25"))))

(ert-deftest gitq-test--parse/bare-sha-prefix-where-value ()
  (should (equal (gitq--parse-flat "commits where sha contains 062062e9af")
                 '((:type source :source commits :range nil)
                   (:type where :conditions ((:field sha :op contains :value "062062e9af")))))))

(ert-deftest gitq-test--parse/bare-date-where-value ()
  (should (equal (gitq--parse-flat "commits where date after 2026-05-25")
                 '((:type source :source commits :range nil)
                   (:type where :conditions ((:field date :op after :value "2026-05-25")))))))

;; Regression tests: `take'/`skip' must reject a non-purely-numeric token
;; rather than silently truncating it via `string-to-number' -- this became
;; a live risk once the tokenizer above started merging digit-leading runs
;; with trailing letters into a single token.

(ert-deftest gitq-test--parse/take-non-numeric-errors ()
  (should-error (gitq--parse-flat "commits take 5x")))

(ert-deftest gitq-test--parse/skip-non-numeric-errors ()
  (should-error (gitq--parse-flat "commits skip 3abc")))

(ert-deftest gitq-test--parse/take-still-works ()
  (should (equal (gitq--parse-flat "commits take 5")
                 '((:type source :source commits :range nil)
                   (:type take :n 5)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: structural field-type checking
;;; ─────────────────────────────────────────────────────────────────────────────
;;
;; `where'/`sort'/`pick'/`path'/`via' are validated against the field-set of
;; the frame type actually flowing into them at that point in the pipeline
;; (`gitq--source-fields'/`gitq--morphism-signatures', threaded through
;; `gitq--parse-flat' as `current-fields'), not the single flat global union
;; in `gitq--field-names'. A field valid on some other frame type is now a
;; parse-time error naming exactly which fields the current frame has,
;; instead of a silently-empty result at run time.

(ert-deftest gitq-test--type/where-field-invalid-for-source-errors ()
  "`name'/`branch' only exist on ref/worktree frames -- referencing them
after a `commits' source is a parse-time error, not a silent empty
result (which is what happened before: commit frames have no `:name',
so the condition always evaluated false)."
  (should-error (gitq--parse-flat "commits where name == main"))
  (should-error (gitq--parse-flat "commits where branch == main")))

(ert-deftest gitq-test--type/where-field-valid-for-matching-source-works ()
  (should (gitq--parse-flat "branches where name == main"))
  (should (gitq--parse-flat "worktrees where branch == main")))

(ert-deftest gitq-test--type/sort-field-invalid-for-source-errors ()
  (should-error (gitq--parse-flat "commits sort name"))
  (should (gitq--parse-flat "branches sort name")))

(ert-deftest gitq-test--type/pick-field-invalid-for-source-errors ()
  (should-error (gitq--parse-flat "commits pick name")))

(ert-deftest gitq-test--type/pick-narrows-fields-for-later-steps ()
  "After `pick', only the picked fields remain valid -- picking narrows
the field-set for anything chained after it, same as any other
field-shape-changing step."
  (should-error (gitq--parse-flat "commits pick sha where author == \"x\"")))

(ert-deftest gitq-test--type/via-domain-error-names-required-field ()
  "`.tree' needs `:tree', which only commit frames carry -- applying it
after a `branches' source (ref frames have no `:tree') is a parse-time
error, not the old silent empty result (`gitq--exec-via''s `tree' case
read a nil `:tree' and produced nothing)."
  (should-error (gitq--parse-flat "branches via .tree")))

(ert-deftest gitq-test--type/via-domain-valid-for-matching-source-works ()
  (should (gitq--parse-flat "commits via .tree"))
  (should (gitq--parse-flat "commits via .diff"))
  (should (gitq--parse-flat "branches via .diff")) ; ref frames do have :sha
  (should (gitq--parse-flat "commits via .diff.hunks via .commit")))

(ert-deftest gitq-test--type/grep-after-hunk-errors ()
  "hunk frames (from `.diff.hunks') have no `:sha' -- `grep' needs one.
Before this check, this used to crash deep inside `call-process' with
a literal nil argument instead of failing at parse time with a clear
message."
  (should-error (gitq--parse-flat "commits via .diff.hunks grep \"x\"")))

(ert-deftest gitq-test--type/pickaxe-after-hunk-errors ()
  (should-error (gitq--parse-flat "commits via .diff.hunks pickaxe \"x\"")))

(ert-deftest gitq-test--type/path-step-requires-path-field ()
  (should-error (gitq--parse-flat "commits path \"*.ts\""))
  (should (gitq--parse-flat "blobs path \"*.ts\""))
  (should (gitq--parse-flat "commits via .diff path \"*.ts\"")))

(ert-deftest gitq-test--type/grep-output-is-line-shaped ()
  "`grep' produces line frames -- fields valid afterward are line
fields (path/line-number/content/commit-sha/sha), not the source's own
original fields."
  (should (gitq--parse-flat "commits grep \"x\" where line-number > 1"))
  (should-error (gitq--parse-flat "commits grep \"x\" where author == \"a\"")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--complete-candidates — type-narrowed field/morphism candidates
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--complete-candidates/where-fields-narrowed-by-source ()
  (let ((commit-cands  (gitq--complete-candidates "commits where "))
        (branch-cands  (gitq--complete-candidates "branches where ")))
    (should (member "author" commit-cands))
    (should-not (member "name" commit-cands))
    (should-not (member "reftype" commit-cands))
    (should (member "name" branch-cands))
    (should (member "reftype" branch-cands))
    (should-not (member "author" branch-cands))))

(ert-deftest gitq-test--complete-candidates/pick-fields-narrowed-after-via-diff ()
  (let ((cands (gitq--complete-candidates "commits via .diff pick ")))
    (should (member "path" cands))
    (should (member "parent-sha" cands))
    (should-not (member "author" cands))
    (should-not (member "message" cands))))

(ert-deftest gitq-test--complete-candidates/via-morphisms-narrowed-by-source ()
  "`.tree'/`.parent*' (need `:tree'/`:parents-count', commit-only) are
excluded after a `branches' source; `.tree.blobs' (just needs `:sha',
which ref frames have) is still offered."
  (let ((cands (gitq--complete-candidates "branches via ")))
    (should-not (member ".tree" cands))
    (should-not (member ".parent*" cands))
    (should (member ".tree.blobs" cands))
    (should (member ".diff" cands))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: morphism composition (dotted chains parse generically)
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--compose/parent-parent ()
  "`.parent.parent' parses as TWO via nodes executed left to right."
  (let* ((nodes (gitq--parse-flat "HEAD via .parent.parent"))
         (vias  (seq-filter (lambda (n) (eq (plist-get n :type) 'via)) nodes)))
    (should (= (length vias) 2))
    (should (cl-every (lambda (n) (eq (plist-get n :morphism) 'parent)) vias))))

(ert-deftest gitq-test--compose/parent-index-tree ()
  "`.parent[0].tree' composes an indexed parent with the tree morphism."
  (let* ((nodes (gitq--parse-flat "HEAD via .parent[0].tree"))
         (vias  (seq-filter (lambda (n) (eq (plist-get n :type) 'via)) nodes)))
    (should (= (length vias) 2))
    (should (eq (plist-get (nth 0 vias) :morphism) 'parent))
    (should (= (plist-get (nth 0 vias) :index) 0))
    (should (eq (plist-get (nth 1 vias) :morphism) 'tree))))

(ert-deftest gitq-test--compose/fused-forms-stay-single-nodes ()
  "The fused multi-segment forms still parse to ONE node (longest match)."
  (dolist (path '(".tree.entries" ".tree.entries[Blob]" ".tree.blobs"
                  ".tree.subtrees" ".diff.hunks"))
    (let* ((nodes (gitq--parse-flat (format "HEAD via %s" path)))
           (vias  (seq-filter (lambda (n) (eq (plist-get n :type) 'via)) nodes)))
      (should (= (length vias) 1)))))

(ert-deftest gitq-test--compose/type-error-mid-chain ()
  "`.tree.parent' is a domain error: a tree object has no parents-count."
  (let ((err (should-error (gitq--parse-flat "HEAD via .tree.parent"))))
    (should (string-match-p "parents-count" (error-message-string err)))))

(ert-deftest gitq-test--compose/diff-ref-after-chain ()
  "A trailing `.diff' in a chain still takes its optional REF token."
  (let* ((nodes (gitq--parse-flat "HEAD via .parent.diff main"))
         (diff  (car (last nodes))))
    (should (eq (plist-get diff :morphism) 'diff))
    (should (equal (plist-get diff :ref) "main"))))

(ert-deftest gitq-test--compose/entries-history-typechecks ()
  "`.tree.entries.history' composes: blob entries carry `path', which
`.history' requires — the whole point of typing morphism chains."
  (let* ((nodes (gitq--parse-flat "HEAD via .tree.entries.history"))
         (vias  (seq-filter (lambda (n) (eq (plist-get n :type) 'via)) nodes)))
    (should (= (length vias) 2))
    (should (eq (plist-get (nth 1 vias) :morphism) 'history))))

(ert-deftest gitq-test--compose/unknown-segment-names-it ()
  "An unknown segment errors naming the bad segment and the full path."
  (let ((err (should-error (gitq--parse-flat "HEAD via .parent.bogus"))))
    (should (string-match-p "\\.bogus" (error-message-string err)))))

(ert-deftest gitq-test--registry/every-completion-morphism-parses-and-is-registered ()
  "Every morphism completion candidate parses, and every morphism symbol
it produces has a full entry (:requires :yields :exec) in `gitq--morphisms'."
  (dolist (path gitq--complete-morphisms)
    (let ((nodes (gitq--parse-morphism-path path)))
      (should nodes)
      (dolist (node nodes)
        (let ((spec (alist-get (plist-get node :morphism) gitq--morphisms)))
          (should spec)
          (should (plist-get spec :requires))
          (should (plist-get spec :yields))
          (should (fboundp (plist-get spec :exec))))))))

(ert-deftest gitq-test--registry/every-morphism-form-is-registered ()
  "Every surface form in `gitq--morphism-forms' constructs a node whose
morphism symbol is registered — the executor can dispatch anything the
path parser can produce."
  (dolist (entry gitq--morphism-forms)
    (let ((node (funcall (cdr entry) "0")))
      (should (alist-get (plist-get node :morphism) gitq--morphisms)))))

(ert-deftest gitq-test--registry/morphism-requires-known-fields ()
  "Every :requires and :yields field in the morphism registry is a known
field name (so type errors always name real fields)."
  (dolist (entry gitq--morphisms)
    (let ((spec (cdr entry)))
      (should (member (plist-get spec :requires) gitq--field-names))
      (dolist (f (plist-get spec :yields))
        (should (member f gitq--field-names))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: scalar field types (where operators, sort comparators)
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--scalar/unknown-operator-is-parse-error ()
  (let ((err (should-error (gitq--parse-flat "commits where author sortof \"x\""))))
    (should (string-match-p "unknown where operator" (error-message-string err)))))

(ert-deftest gitq-test--scalar/numeric-op-on-date-is-parse-error ()
  "`date > ...' used to be silently false forever; now it is a type error
suggesting the date operators."
  (let ((err (should-error (gitq--parse-flat "commits where date > 2020-01-01"))))
    (should (string-match-p "after" (error-message-string err)))))

(ert-deftest gitq-test--scalar/operator-without-value-is-parse-error ()
  "`author ==' with no value used to parse as :value t and match nothing."
  (let ((err (should-error (gitq--parse-flat "commits where author == take 5"))))
    (should (string-match-p "requires a value" (error-message-string err))))
  (let ((err (should-error (gitq--parse-flat "commits where author == /show"))))
    (should (string-match-p "requires a value" (error-message-string err)))))

(ert-deftest gitq-test--scalar/non-numeric-value-on-number-field-errors ()
  (let ((err (should-error (gitq--parse-flat "commits where parents-count == \"two\""))))
    (should (string-match-p "not a number" (error-message-string err)))))

(ert-deftest gitq-test--scalar/is-still-valueless-on-flags ()
  (let* ((nodes (gitq--parse-flat "worktrees where detached is"))
         (cond1 (car (plist-get (cadr nodes) :conditions))))
    (should (eq (plist-get cond1 :op) 'is))
    (should (eq (plist-get cond1 :value) t))))

(ert-deftest gitq-test--scalar/pick-without-fields-is-parse-error ()
  "`pick' with nothing to pick used to project every frame to nothing."
  (let ((err (should-error (gitq--parse-flat "commits pick /show"))))
    (should (string-match-p "at least one field" (error-message-string err)))))

(ert-deftest gitq-test--scalar/sort-numeric-field-sorts-numerically ()
  "`sort parents-count' used to crash: numbers went through `string<'."
  (let* ((frames (list (list :type 'commit :sha "a" :parents '("x" "y"))
                       (list :type 'commit :sha "b" :parents nil)
                       (list :type 'commit :sha "c" :parents '("z"))))
         (asc  (gitq--exec-step frames '(:type sort :field parents-count)))
         (desc (gitq--exec-step frames '(:type sort :field parents-count :desc t))))
    (should (equal (mapcar (lambda (f) (plist-get f :sha)) asc)  '("b" "c" "a")))
    (should (equal (mapcar (lambda (f) (plist-get f :sha)) desc) '("a" "c" "b")))))

(ert-deftest gitq-test--scalar/exec-step-unknown-type-errors ()
  "An unknown step type is an internal error, never a silent pass-through."
  (should-error (gitq--exec-step (gitq-test--commits 2) '(:type bogus-step))))

(ert-deftest gitq-test--registry/every-field-has-a-scalar-type ()
  (dolist (field gitq--field-names)
    (should (assoc field gitq--field-types))))

(ert-deftest gitq-test--registry/operator-completions-match-signatures ()
  "The where-operator completion list and the operator signature table
hold exactly the same operators."
  (should (equal (sort (copy-sequence gitq--complete-where-operators) #'string<)
                 (sort (mapcar #'car gitq--operator-signatures) #'string<))))

(provide 'git-branch-off-gitq-test)
;;; git-branch-off-gitq-test.el ends here
