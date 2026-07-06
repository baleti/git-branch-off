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
;;; Tests: gitq--split-pipeline
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--split/simple ()
  "Split a simple two-stage pipeline."
  (should (equal (gitq--split-pipeline "commits | show")
                 '("commits" "show"))))

(ert-deftest gitq-test--split/three-stages ()
  "Split a three-stage pipeline."
  (should (equal (gitq--split-pipeline "commits | where .author == \"alice\" | show")
                 '("commits" "where .author == \"alice\"" "show"))))

(ert-deftest gitq-test--split/pipe-inside-string ()
  "Pipe characters inside quoted strings are not treated as separators."
  (should (equal (gitq--split-pipeline "commits | where .message == \"a|b\" | show")
                 '("commits" "where .message == \"a|b\"" "show"))))

(ert-deftest gitq-test--split/pipe-inside-regex ()
  "Pipe characters inside /regex/ literals are not treated as separators."
  (should (equal (gitq--split-pipeline "commits | where .message matches /foo|bar/ | show")
                 '("commits" "where .message matches /foo|bar/" "show"))))

(ert-deftest gitq-test--split/whitespace ()
  "Leading and trailing whitespace is stripped from each stage."
  (should (equal (gitq--split-pipeline "  commits  |  show  ")
                 '("commits" "show"))))

(ert-deftest gitq-test--split/single-stage ()
  "A pipeline with a single stage produces a one-element list."
  (should (equal (gitq--split-pipeline "commits") '("commits"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--tokenize
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--tokenize/keywords ()
  "Keywords and identifiers are tokenized separately."
  (should (equal (gitq--tokenize "where .author == \"alice\"")
                 '("where" ".author" "==" "\"alice\""))))

(ert-deftest gitq-test--tokenize/dotted-path ()
  "Dotted morphism paths like .tree.entries[Blob] are one token."
  (should (equal (gitq--tokenize "via .tree.entries[Blob]")
                 '("via" ".tree.entries[Blob]"))))

(ert-deftest gitq-test--tokenize/kleene-star ()
  ".parent* is one token."
  (should (equal (gitq--tokenize "via .parent*")
                 '("via" ".parent*"))))

(ert-deftest gitq-test--tokenize/quoted-string ()
  "A quoted string with spaces is a single token."
  (should (equal (gitq--tokenize "squash \"consolidated commit\"")
                 '("squash" "\"consolidated commit\""))))

(ert-deftest gitq-test--tokenize/regex-literal ()
  "A /regex/ literal is a single token."
  (should (equal (gitq--tokenize "where .message matches /^feat:/")
                 '("where" ".message" "matches" "/^feat:/"))))

(ert-deftest gitq-test--tokenize/comma ()
  "Commas are separate tokens."
  (should (equal (gitq--tokenize "pick .sha, .message, .author")
                 '("pick" ".sha" "," ".message" "," ".author"))))

(ert-deftest gitq-test--tokenize/operators ()
  "Two-character operators are single tokens."
  (should (equal (gitq--tokenize "where .parents.count >= 2")
                 '("where" ".parents.count" ">=" "2"))))

(ert-deftest gitq-test--tokenize/number ()
  "Numbers are tokenized as strings."
  (should (equal (gitq--tokenize "take 10")
                 '("take" "10"))))

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
;;; Tests: gitq--parse — source stages
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-source/commits ()
  (let ((node (car (gitq--parse "commits | show"))))
    (should (eq (plist-get node :type) 'source))
    (should (eq (plist-get node :source) 'commits))
    (should (null (plist-get node :range)))))

(ert-deftest gitq-test--parse-source/commits-in-range ()
  (let ((node (car (gitq--parse "commits in main..feature | show"))))
    (should (eq (plist-get node :source) 'commits))
    (should (equal (plist-get node :range) "main..feature"))))

(ert-deftest gitq-test--parse-source/head-ref ()
  (let ((node (car (gitq--parse "HEAD | show"))))
    (should (eq (plist-get node :source) 'ref))
    (should (equal (plist-get node :ref) "HEAD"))))

(ert-deftest gitq-test--parse-source/branch-name ()
  (let ((node (car (gitq--parse "main | show"))))
    (should (eq (plist-get node :source) 'ref))
    (should (equal (plist-get node :ref) "main"))))

(ert-deftest gitq-test--parse-source/branches ()
  (let ((node (car (gitq--parse "branches | show"))))
    (should (eq (plist-get node :source) 'branches))))

(ert-deftest gitq-test--parse-source/tags ()
  (let ((node (car (gitq--parse "tags | show"))))
    (should (eq (plist-get node :source) 'tags))))

(ert-deftest gitq-test--parse-source/worktrees ()
  (let ((node (car (gitq--parse "worktrees | show"))))
    (should (eq (plist-get node :source) 'worktree))))

(ert-deftest gitq-test--parse-source/worktree-singular ()
  (let ((node (car (gitq--parse "worktree | show"))))
    (should (eq (plist-get node :source) 'worktree))))

(ert-deftest gitq-test--parse-source/blobs ()
  (let ((node (car (gitq--parse "blobs | show"))))
    (should (eq (plist-get node :source) 'blobs))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse — via step
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-via/parent ()
  (let* ((nodes (gitq--parse "commits | via .parent | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :type) 'via))
    (should (eq (plist-get via :morphism) 'parent))
    (should (null (plist-get via :star)))))

(ert-deftest gitq-test--parse-via/parent-star ()
  (let* ((nodes (gitq--parse "HEAD | via .parent* | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'parent))
    (should (plist-get via :star))))

(ert-deftest gitq-test--parse-via/parent-plus ()
  (let* ((nodes (gitq--parse "HEAD | via .parent+ | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'parent))
    (should (plist-get via :plus))))

(ert-deftest gitq-test--parse-via/parent-index ()
  (let* ((nodes (gitq--parse "HEAD | via .parent[0] | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'parent))
    (should (= (plist-get via :index) 0))))

(ert-deftest gitq-test--parse-via/parent-index-1 ()
  (let* ((nodes (gitq--parse "HEAD | via .parent[1] | show"))
         (via   (nth 1 nodes)))
    (should (= (plist-get via :index) 1))))

(ert-deftest gitq-test--parse-via/tree ()
  (let* ((nodes (gitq--parse "HEAD | via .tree | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'tree))))

(ert-deftest gitq-test--parse-via/tree-entries-blob ()
  (let* ((nodes (gitq--parse "HEAD | via .tree.entries[Blob] | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'tree-entries))
    (should (eq (plist-get via :filter) 'blob))))

(ert-deftest gitq-test--parse-via/tree-entries-tree ()
  (let* ((nodes (gitq--parse "HEAD | via .tree.entries[Tree] | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'tree-entries))
    (should (eq (plist-get via :filter) 'tree))))

(ert-deftest gitq-test--parse-via/tree-entries-unfiltered ()
  "Without a type filter, :filter is nil."
  (let* ((nodes (gitq--parse "HEAD | via .tree.entries | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'tree-entries))
    (should (null (plist-get via :filter)))))

(ert-deftest gitq-test--parse-via/diff ()
  (let* ((nodes (gitq--parse "HEAD | via .diff | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'diff))))

(ert-deftest gitq-test--parse-via/diff-hunks ()
  (let* ((nodes (gitq--parse "HEAD | via .diff.hunks | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'diff-hunks))))

(ert-deftest gitq-test--parse-via/history ()
  (let* ((nodes (gitq--parse "blobs | path \"auth.ts\" | via .history | show"))
         (via   (nth 2 nodes)))
    (should (eq (plist-get via :morphism) 'history))))

(ert-deftest gitq-test--parse-via/commit ()
  (let* ((nodes (gitq--parse "blobs | via .commit | show"))
         (via   (nth 1 nodes)))
    (should (eq (plist-get via :morphism) 'commit))))

(ert-deftest gitq-test--parse-via/unknown-morphism ()
  "An unknown morphism signals an error at parse time."
  (should-error (gitq--parse "commits | via .nonExistent | show")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse — where step
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-where/equality ()
  (let* ((nodes (gitq--parse "commits | where .author == \"alice\" | show"))
         (where (nth 1 nodes))
         (cond  (car (plist-get where :conditions))))
    (should (eq (plist-get where :type) 'where))
    (should (eq (plist-get cond :field) 'author))
    (should (eq (plist-get cond :op)    '==))
    (should (equal (plist-get cond :value) "alice"))))

(ert-deftest gitq-test--parse-where/contains ()
  (let* ((nodes (gitq--parse "commits | where .message contains \"fix\" | show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :op) 'contains))
    (should (equal (plist-get cond :value) "fix"))))

(ert-deftest gitq-test--parse-where/matches-regex ()
  (let* ((nodes (gitq--parse "commits | where .message matches /^feat:/ | show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :op) 'matches))
    (should (equal (plist-get cond :value) "^feat:"))))

(ert-deftest gitq-test--parse-where/numeric-gt ()
  (let* ((nodes (gitq--parse "commits | where .parents.count > 1 | show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :field) 'parents-count))
    (should (eq (plist-get cond :op) '>))
    (should (= (plist-get cond :value) 1))))

(ert-deftest gitq-test--parse-where/multiple-conditions ()
  "Multiple where conditions separated by commas."
  (let* ((nodes (gitq--parse "commits | where .author == \"alice\", .message contains \"fix\" | show"))
         (conds (plist-get (nth 1 nodes) :conditions)))
    (should (= (length conds) 2))
    (should (eq (plist-get (nth 0 conds) :field) 'author))
    (should (eq (plist-get (nth 1 conds) :field) 'message))))

(ert-deftest gitq-test--parse-where/bare-flag ()
  "Bare .modified flag (no op/value)."
  (let* ((nodes (gitq--parse "worktree | where .modified | show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :field) 'modified))
    (should (eq (plist-get cond :op)    'is))
    (should (eq (plist-get cond :value) t))))

(ert-deftest gitq-test--parse-where/after ()
  (let* ((nodes (gitq--parse "commits | where .date after \"2024-01-01\" | show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :op) 'after))
    (should (equal (plist-get cond :value) "2024-01-01"))))

(ert-deftest gitq-test--parse-where/within ()
  (let* ((nodes (gitq--parse "commits | where .date within \"30 days\" | show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (eq (plist-get cond :op) 'within))
    (should (equal (plist-get cond :value) "30 days"))))

(ert-deftest gitq-test--parse-where/sha-equality ()
  (let* ((nodes (gitq--parse "commits | where .sha == \"a3f9b2\" | show"))
         (cond  (car (plist-get (nth 1 nodes) :conditions))))
    (should (equal (plist-get cond :value) "a3f9b2"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse — pick step
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-pick/single-field ()
  (let* ((nodes (gitq--parse "commits | pick .sha | show"))
         (pick  (nth 1 nodes)))
    (should (eq (plist-get pick :type) 'pick))
    (should (equal (plist-get pick :fields) '(sha)))))

(ert-deftest gitq-test--parse-pick/multiple-fields ()
  (let* ((nodes (gitq--parse "commits | pick .sha, .message, .author | show"))
         (pick  (nth 1 nodes)))
    (should (equal (plist-get pick :fields) '(sha message author)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse — navigation steps
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-take ()
  (let* ((nodes (gitq--parse "commits | take 10 | show"))
         (take  (nth 1 nodes)))
    (should (eq (plist-get take :type) 'take))
    (should (= (plist-get take :n) 10))))

(ert-deftest gitq-test--parse-skip ()
  (let* ((nodes (gitq--parse "commits | skip 3 | show"))
         (skip  (nth 1 nodes)))
    (should (eq (plist-get skip :type) 'skip))
    (should (= (plist-get skip :n) 3))))

(ert-deftest gitq-test--parse-first ()
  (let* ((nodes (gitq--parse "commits | first | show"))
         (step  (nth 1 nodes)))
    (should (eq (plist-get step :type) 'first))))

(ert-deftest gitq-test--parse-last ()
  (let* ((nodes (gitq--parse "commits | last | show"))
         (step  (nth 1 nodes)))
    (should (eq (plist-get step :type) 'last))))

(ert-deftest gitq-test--parse-sort/ascending ()
  (let* ((nodes (gitq--parse "commits | sort .date | show"))
         (sort  (nth 1 nodes)))
    (should (eq (plist-get sort :type) 'sort))
    (should (eq (plist-get sort :field) 'date))
    (should (null (plist-get sort :desc)))))

(ert-deftest gitq-test--parse-sort/descending ()
  (let* ((nodes (gitq--parse "commits | sort -.date | show"))
         (sort  (nth 1 nodes)))
    (should (eq (plist-get sort :field) 'date))
    (should (plist-get sort :desc))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse — grep / pickaxe / path
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-grep/literal ()
  (let* ((nodes (gitq--parse "commits | grep \"TODO\" | show"))
         (grep  (nth 1 nodes)))
    (should (eq (plist-get grep :type) 'grep))
    (should (equal (plist-get grep :pattern) "TODO"))
    (should (null (plist-get grep :regex)))
    (should (null (plist-get grep :path-filter)))))

(ert-deftest gitq-test--parse-grep/regex ()
  (let* ((nodes (gitq--parse "commits | grep /TODO|FIXME/ | show"))
         (grep  (nth 1 nodes)))
    (should (equal (plist-get grep :pattern) "TODO|FIXME"))
    (should (plist-get grep :regex))))

(ert-deftest gitq-test--parse-grep/with-path ()
  (let* ((nodes (gitq--parse "commits | grep \"TODO\" path \"*.ts\" | show"))
         (grep  (nth 1 nodes)))
    (should (equal (plist-get grep :path-filter) "*.ts"))))

(ert-deftest gitq-test--parse-pickaxe/literal ()
  (let* ((nodes  (gitq--parse "commits | pickaxe \"SecretKey\" | show"))
         (pa     (nth 1 nodes)))
    (should (eq (plist-get pa :type) 'pickaxe))
    (should (equal (plist-get pa :pattern) "SecretKey"))
    (should (null (plist-get pa :regex)))))

(ert-deftest gitq-test--parse-pickaxe/regex ()
  (let* ((nodes (gitq--parse "commits | pickaxe /password\\s*=/ regex | show"))
         (pa    (nth 1 nodes)))
    (should (plist-get pa :regex))))

(ert-deftest gitq-test--parse-path ()
  (let* ((nodes (gitq--parse "commits | path \"src/auth.ts\" | show"))
         (path  (nth 1 nodes)))
    (should (eq (plist-get path :type) 'path))
    (should (equal (plist-get path :pattern) "src/auth.ts"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse — terminal operations
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse-terminal/show ()
  (let* ((nodes (gitq--parse "commits | show"))
         (term  (nth 1 nodes)))
    (should (eq (plist-get term :type) 'terminal))
    (should (eq (plist-get term :op)   'show))))

(ert-deftest gitq-test--parse-terminal/copy ()
  (let* ((term (car (last (gitq--parse "commits | copy")))))
    (should (eq (plist-get term :op) 'copy))))

(ert-deftest gitq-test--parse-terminal/count ()
  (let* ((term (car (last (gitq--parse "commits | count")))))
    (should (eq (plist-get term :op) 'count))))

(ert-deftest gitq-test--parse-terminal/branch-off-bare ()
  (let* ((term (car (last (gitq--parse "commits | branch-off")))))
    (should (eq (plist-get term :op) 'branch-off))
    (should (null (plist-get term :name)))))

(ert-deftest gitq-test--parse-terminal/branch-off-named ()
  (let* ((term (car (last (gitq--parse "commits | branch-off \"feature/x\"")))))
    (should (eq (plist-get term :op) 'branch-off))
    (should (equal (plist-get term :name) "feature/x"))))

(ert-deftest gitq-test--parse-terminal/branch-off-worktree ()
  (let* ((term (car (last (gitq--parse "commits | branch-off \"feature/x\" worktree \"../wt\"")))))
    (should (equal (plist-get term :name) "feature/x"))
    (should (equal (plist-get term :worktree) "../wt"))))

(ert-deftest gitq-test--parse-terminal/amend-bare ()
  (let* ((term (car (last (gitq--parse "HEAD | amend")))))
    (should (eq (plist-get term :op) 'amend))
    (should (null (plist-get term :no-edit)))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/amend-no-edit ()
  (let* ((term (car (last (gitq--parse "HEAD | amend no-edit")))))
    (should (plist-get term :no-edit))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/amend-message ()
  (let* ((term (car (last (gitq--parse "HEAD | amend \"new message\"")))))
    (should (null (plist-get term :no-edit)))
    (should (equal (plist-get term :message) "new message"))))

(ert-deftest gitq-test--parse-terminal/squash-bare ()
  (let* ((term (car (last (gitq--parse "HEAD | squash")))))
    (should (eq (plist-get term :op) 'squash))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/squash-message ()
  (let* ((term (car (last (gitq--parse "HEAD | via .parent* | take 3 | squash \"consolidated\"")))))
    (should (eq (plist-get term :op) 'squash))
    (should (equal (plist-get term :message) "consolidated"))))

(ert-deftest gitq-test--parse-terminal/reword-bare ()
  (let* ((term (car (last (gitq--parse "commits | reword")))))
    (should (eq (plist-get term :op) 'reword))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/reword-message ()
  (let* ((term (car (last (gitq--parse "commits | reword \"new message\"")))))
    (should (equal (plist-get term :message) "new message"))))

(ert-deftest gitq-test--parse-terminal/remove ()
  (let* ((term (car (last (gitq--parse "commits | remove")))))
    (should (eq (plist-get term :op) 'remove))))

(ert-deftest gitq-test--parse-terminal/commit-bare ()
  (let* ((term (car (last (gitq--parse "worktree | commit")))))
    (should (eq (plist-get term :op) 'commit))
    (should (null (plist-get term :message)))))

(ert-deftest gitq-test--parse-terminal/commit-message ()
  (let* ((term (car (last (gitq--parse "worktree | commit \"fix: auth\"")))))
    (should (equal (plist-get term :message) "fix: auth"))))

(ert-deftest gitq-test--parse-terminal/mark ()
  (let* ((term (car (last (gitq--parse "commits | mark \"stable\"")))))
    (should (eq (plist-get term :op) 'mark))
    (should (equal (plist-get term :label) "stable"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tests: gitq--parse — full pipeline structure
;;; ─────────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-test--parse/pipeline-length ()
  "A 4-stage pipeline parses to 4 nodes."
  (let ((nodes (gitq--parse "commits | where .author == \"alice\" | take 10 | show")))
    (should (= (length nodes) 4))))

(ert-deftest gitq-test--parse/pipeline-types ()
  "Node types are source, where, take, terminal in order."
  (let ((nodes (gitq--parse "commits | where .author == \"alice\" | take 10 | show")))
    (should (eq (plist-get (nth 0 nodes) :type) 'source))
    (should (eq (plist-get (nth 1 nodes) :type) 'where))
    (should (eq (plist-get (nth 2 nodes) :type) 'take))
    (should (eq (plist-get (nth 3 nodes) :type) 'terminal))))

(ert-deftest gitq-test--parse/empty-pipeline-errors ()
  "An empty string signals an error."
  (should-error (gitq--parse "")))

(ert-deftest gitq-test--parse/complex-pipeline ()
  "Example 3 from spec: commits introducing a string via pickaxe."
  (let* ((nodes (gitq--parse
                 "commits | via .diff | pickaxe \"SecretKey\" | pick .sha, .date, .author, .path"))
         (src   (nth 0 nodes))
         (via   (nth 1 nodes))
         (pa    (nth 2 nodes))
         (pick  (nth 3 nodes)))
    (should (eq (plist-get src :source) 'commits))
    (should (eq (plist-get via :morphism) 'diff))
    (should (eq (plist-get pa :type) 'pickaxe))
    (should (equal (plist-get pick :fields) '(sha date author path)))))

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
  (let* ((frames (list '(:type commit :sha "abc" :author "Alice" :message "msg")))
         (result (gitq--exec-step frames '(:type pick :fields (sha author)))))
    (should (= (length result) 1))
    (should (equal (plist-get (car result) 'sha) "abc"))
    (should (equal (plist-get (car result) 'author) "Alice"))
    ;; message was not picked
    (should (null (plist-get (car result) 'message)))))

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
  "Full gitq pipeline: commits | take 1 | show — creates *gitq* buffer."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (gitq "commits | take 1 | show")
    (let ((buf (get-buffer "*gitq*")))
      (should buf)
      (with-current-buffer buf
        (should (string-match-p "gitq:" (buffer-string)))
        (should (> (buffer-size) 0))))))

(ert-deftest gitq-test--integration/full-query-where-message ()
  "commits | where .message contains X | show filters correctly."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "a.txt" "1\n" "fix: auth issue")
    (gitq-test--commit "b.txt" "2\n" "feat: new widget")
    (gitq-test--commit "c.txt" "3\n" "fix: logging bug")
    (gitq "commits | where .message contains \"fix\" | show")
    (let ((buf (get-buffer "*gitq*")))
      (with-current-buffer buf
        ;; Should show 2 fix commits, not the feat one
        (should (string-match-p "fix" (buffer-string)))
        (should-not (string-match-p "feat" (buffer-string)))))))

(ert-deftest gitq-test--integration/full-query-branch-off ()
  "commits | first | branch-off creates the branch."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (gitq "commits | first | branch-off \"test-branch\"")
    (let ((branches (gitq--git "branch" "--list" "test-branch")))
      (should (>= (length branches) 1)))))

(ert-deftest gitq-test--integration/full-query-head-parent ()
  "HEAD | via .parent | show navigates to parent commit."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "a\n" "the-parent-msg")
    (gitq-test--commit "f.txt" "b\n" "the-head-msg")
    (gitq "HEAD | via .parent | show")
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
    (should (null (gitq--preview-frames "commi" #'gitq--parse-flat)))))

(ert-deftest gitq-test--preview-frames/mid-step-keyword-is-not-ready ()
  "A partial step keyword (\"wh\" toward \"where\") fails to parse, so
preview is nil."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (should (null (gitq--preview-frames "commits wh" #'gitq--parse-flat)))))

(ert-deftest gitq-test--preview-frames/complete-source-only ()
  "A bare, complete source keyword previews immediately."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((r (gitq--preview-frames "commits" #'gitq--parse-flat)))
      (should (eq (car r) :ok))
      (should (= (length (cdr r)) 1)))))

(ert-deftest gitq-test--preview-frames/complete-with-where ()
  "A complete pipeline with a where-clause previews the filtered result."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "commit by test user")
    (let ((r (gitq--preview-frames
              "commits where .author contains \"Test\"" #'gitq--parse-flat)))
      (should (eq (car r) :ok))
      (should (= (length (cdr r)) 1)))))

(ert-deftest gitq-test--preview-frames/existing-branch-as-bare-source ()
  "An existing branch name used as a bare source resolves and previews."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((r (gitq--preview-frames "main" #'gitq--parse-flat)))
      (should (eq (car r) :ok))
      (should (= (length (cdr r)) 1)))))

(ert-deftest gitq-test--preview-frames/nonexistent-ref-is-not-ready ()
  "A bare source naming no real ref previews as nil, not as an empty result."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (should (null (gitq--preview-frames "zzzz-does-not-exist" #'gitq--parse-flat)))))

(ert-deftest gitq-test--preview-frames/destructive-terminal-not-applied ()
  "Previewing a pipeline whose terminal is destructive must not run it."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "init")
    (let ((r (gitq--preview-frames
              "commits /branch-off \"should-not-exist\"" #'gitq--parse-flat)))
      (should (eq (car r) :ok))
      (should (= (length (cdr r)) 1)))
    (should (= (length (gitq--git "branch" "--list" "should-not-exist")) 0))))

(ert-deftest gitq-test--preview-frames/pipe-syntax ()
  "Preview also works with the pipe-syntax parser."
  :tags '(integration)
  (gitq-test--with-repo
    (gitq-test--commit "f.txt" "x\n" "commit by test user")
    (let ((r (gitq--preview-frames
              "commits | where .author contains \"Test\"" #'gitq--parse)))
      (should (eq (car r) :ok))
      (should (= (length (cdr r)) 1)))))

(ert-deftest gitq-test--preview-frames/outside-repo-is-not-ready ()
  "Outside a git repository, preview fails silently (nil), not with an error."
  (let* ((dir (make-temp-file "gitq-test-norepo-" t))
         (default-directory dir))
    (unwind-protect
        (should (null (gitq--preview-frames "commits" #'gitq--parse-flat)))
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
  (should (equal (gitq--token-kind ".date") "field"))
  ;; sort's negated "-.field" form must resolve the same as ".field".
  (should (equal (gitq--token-kind "-.date") "field")))

(ert-deftest gitq-test--token-kind/operator ()
  (should (equal (gitq--token-kind "contains") "operator"))
  (should (equal (gitq--token-kind "==") "operator")))

(ert-deftest gitq-test--token-kind/terminal ()
  (should (equal (gitq--token-kind "/show") "terminal"))
  (should (equal (gitq--token-kind "/branch-off") "terminal")))

(ert-deftest gitq-test--token-kind/unknown-is-nil ()
  (should (null (gitq--token-kind "not-a-real-token"))))

(provide 'git-branch-off-gitq-test)
;;; git-branch-off-gitq-test.el ends here
