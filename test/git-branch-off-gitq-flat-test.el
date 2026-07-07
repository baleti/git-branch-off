;;; git-branch-off-gitq-flat-test.el --- Disambiguation tests for gitq flat syntax  -*- lexical-binding: t -*-

;; This test suite provides exhaustive coverage of the flat-syntax parser's
;; disambiguation properties.  The grammar is unambiguous iff:
;;
;;   P1. Every /command token is uniquely identified as a terminal.
;;   P2. Every /regex/ token is uniquely identified as a regex literal.
;;   P3. Step keywords always start a new stage and never appear as values.
;;   P4. Former terminal identifiers (commit, show, count …) are now plain
;;       identifiers and are unambiguous as values.
;;   P5. Where bare flags correctly terminate before step keywords.
;;   P6. Source range parsing terminates at step keywords.
;;   P7. .diff's optional REF arg is not consumed when REF is a step keyword.
;;
;; Each test names the specific ambiguity case it rules out.
;;
;; Field references (where/sort/pick) are bare words (e.g. "author", not
;; ".author") -- only morphism paths after `via' (.parent, .diff, ...) keep
;; the leading ".". Fields are validated against a closed list
;; (gitq--field-names); "type" is not a real field, so tests that need an
;; arbitrary field placeholder use "message" instead.

(require 'ert)
(require 'cl-lib)
(require 'git-branch-off-gitq)

;;; Helpers

(defmacro gitq-flat-test--parse (str)
  "Return the AST list for a flat pipeline string STR."
  `(gitq--parse-flat ,str))

(defmacro gitq-flat-test--src (str)
  "Return the source node from parsing STR."
  `(car (gitq--parse-flat ,str)))

(defmacro gitq-flat-test--nodes (str)
  "Return all nodes from parsing STR."
  `(gitq--parse-flat ,str))

(defmacro gitq-flat-test--steps (str)
  "Return the step nodes (excluding source) from parsing STR."
  `(cdr (gitq--parse-flat ,str)))

(defun gitq-flat-test--tok (str)
  "Tokenize STR with the flat tokenizer, return token list."
  (gitq--tokenize-flat str))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PROPERTY P1: /command tokens are uniquely identified as terminals
;;; ─────────────────────────────────────────────────────────────────────────
;;
;; For each /command token:
;;   - gitq--flat-terminal-p returns t
;;   - gitq--flat-step-p returns nil
;;   - The tokenizer emits it as a single token starting with /

(defconst gitq-flat-test--all-terminals
  '("/show" "/copy" "/insert" "/count" "/branch-off" "/amend"
    "/squash" "/reword" "/remove" "/delete" "/commit" "/stage"
    "/mark" "/worktree")
  "All /terminal keywords in flat syntax.")

(ert-deftest gitq-flat-test/p1-terminal-predicate-all ()
  "P1: gitq--flat-terminal-p returns t for every /command."
  (dolist (t-kw gitq-flat-test--all-terminals)
    (should (gitq--flat-terminal-p t-kw))
    (should-not (gitq--flat-step-p t-kw))))

(ert-deftest gitq-flat-test/p1-terminal-not-confused-with-regex ()
  "P1: /command is not a regex; /command/ is a regex, not a terminal."
  (dolist (t-kw gitq-flat-test--all-terminals)
    (let* ((bare-name  (substring t-kw 1))
           (as-regex   (format "/%s/" bare-name)))
      ;; /command is a terminal
      (should (gitq--flat-terminal-p t-kw))
      ;; /command/ is a regex (ends with /)
      (should-not (gitq--flat-terminal-p as-regex)))))

(ert-deftest gitq-flat-test/p1-tokenizer-emits-slash-command ()
  "P1: tokenizer emits each /command as a single token."
  (dolist (t-kw gitq-flat-test--all-terminals)
    (should (equal (gitq-flat-test--tok t-kw) (list t-kw)))))

(ert-deftest gitq-flat-test/p1-tokenizer-slash-command-in-pipeline ()
  "P1: /terminal token survives tokenization in a complete pipeline."
  (dolist (t-kw gitq-flat-test--all-terminals)
    (let ((tokens (gitq-flat-test--tok (format "commits %s" t-kw))))
      (should (equal (car tokens) "commits"))
      (should (equal (cadr tokens) t-kw)))))

(ert-deftest gitq-flat-test/p1-tokenizer-regex-has-closing-slash ()
  "P1: /foo/ is tokenized as a regex literal, not a /command."
  ;; /fix/ should become a single token "/fix/" (regex), not "/fix" (terminal)
  (let ((tokens (gitq-flat-test--tok "grep /fix/")))
    (should (equal tokens '("grep" "/fix/"))))
  ;; /foo/ is NOT a terminal
  (should-not (gitq--flat-terminal-p "/fix/")))

(ert-deftest gitq-flat-test/p1-grep-regex-then-terminal ()
  "P1: grep /pattern/ /terminal — two distinct / tokens with no confusion."
  (let ((tokens (gitq-flat-test--tok "grep /fix/ /count")))
    (should (equal tokens '("grep" "/fix/" "/count")))
    ;; Second / token is a terminal
    (should (gitq--flat-terminal-p "/count"))
    ;; First / token is a regex
    (should-not (gitq--flat-terminal-p "/fix/"))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PROPERTY P2: /regex/ tokens are uniquely identified as regex literals
;;; ─────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-flat-test/p2-regex-predicate ()
  "P2: /pattern/ tokens are not terminals (closing / present)."
  (dolist (pat '("/foo/" "/fix/" "/feature.*/" "/^chore:/" "/[0-9]+/"))
    (should-not (gitq--flat-terminal-p pat))
    (should-not (gitq--flat-step-p pat))))

(ert-deftest gitq-flat-test/p2-tokenizer-preserves-regex ()
  "P2: regex literals with spaces or special chars tokenize correctly."
  ;; A regex like /foo bar/ — the tokenizer scans to the closing /
  (let ((tokens (gitq-flat-test--tok "where message matches /fix.*/")))
    (should (member "/fix.*/" tokens)))
  (let ((tokens (gitq-flat-test--tok "grep /[0-9]+/")))
    (should (equal tokens '("grep" "/[0-9]+/")))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PROPERTY P3: Step keywords are always stage boundaries
;;; ─────────────────────────────────────────────────────────────────────────
;;
;; For each step keyword K, test that in every position it could theoretically
;; appear as a value, it instead acts as a stage boundary.

(defconst gitq-flat-test--all-step-keywords
  gitq--flat-step-keywords
  "All step keywords that act as stage boundaries.")

(ert-deftest gitq-flat-test/p3-step-predicate-all ()
  "P3: gitq--flat-step-p returns t for every step keyword."
  (dolist (kw gitq-flat-test--all-step-keywords)
    (should (gitq--flat-step-p kw))
    (should-not (gitq--flat-terminal-p kw))))

(ert-deftest gitq-flat-test/p3-step-keyword-starts-stage-after-source ()
  "P3: every step keyword immediately after a source starts a step node.
Uses `blobs' (not `commits') for the `path' iteration specifically,
since `path' needs a `:path' field and commit frames don't carry one."
  (dolist (kw (remove "first" (remove "last" gitq-flat-test--all-step-keywords)))
    ;; Provide a dummy argument so the step parser doesn't fail on missing args.
    ;; We just check that the second node is the expected step type.
    (let* ((source (if (equal kw "path") "blobs" "commits"))
           (dummy (pcase kw
                    ("take"     "1")
                    ("skip"     "0")
                    ("sort"     "date")
                    ("via"      ".parent")
                    ("grep"     "\"x\"")
                    ("pickaxe"  "\"x\"")
                    ("path"     "\"*\"")
                    ("pick"     "sha")
                    ;; a bare non-flag field is a type error now, so the
                    ;; `where' dummy must be a full typed condition
                    ("where"    "message contains \"x\"")
                    (_          "")))
           (pipeline (string-trim (format "%s %s %s" source kw dummy)))
           (nodes    (gitq--parse-flat pipeline)))
      (should (= (length nodes) 2) )
      (should (eq (plist-get (car nodes) :type) 'source))
      (should (eq (plist-get (cadr nodes) :type) (intern kw))))))

(ert-deftest gitq-flat-test/p3-first-last-start-stage-after-source ()
  "P3: first and last start stages after source."
  (let ((nodes (gitq--parse-flat "commits first")))
    (should (eq (plist-get (cadr nodes) :type) 'first)))
  (let ((nodes (gitq--parse-flat "commits last")))
    (should (eq (plist-get (cadr nodes) :type) 'last))))

(ert-deftest gitq-flat-test/p3-step-keyword-after-step ()
  "P3: every step keyword after another step starts a new step node.
Bare where-conditions are flag-typed only, so the fixture uses a
worktree source (`modified' is a flag; commit frames have none)."
  (let ((nodes (gitq--parse-flat "worktrees where modified take 3")))
    (should (= (length nodes) 3))
    (should (eq (plist-get (nth 0 nodes) :type) 'source))
    (should (eq (plist-get (nth 1 nodes) :type) 'where))
    (should (eq (plist-get (nth 2 nodes) :type) 'take))
    (should (= (plist-get (nth 2 nodes) :n) 3))))

(ert-deftest gitq-flat-test/p3-where-value-step-keyword-errors ()
  "P3: unquoted step keyword in where value position is an error."
  (dolist (kw gitq-flat-test--all-step-keywords)
    ;; where message contains KEYWORD — KEYWORD is reserved, must error
    (should-error
     (gitq--parse-flat (format "commits where message contains %s" kw))
     :type 'error)))

(ert-deftest gitq-flat-test/p3-where-quoted-step-keyword-is-value ()
  "P3: quoted step keyword in where value position is accepted as a string value."
  (dolist (kw gitq-flat-test--all-step-keywords)
    ;; where message contains "KEYWORD" — quoted, so it's a value
    (let* ((pipeline (format "commits where message contains \"%s\"" kw))
           (nodes    (gitq--parse-flat pipeline))
           (where    (cadr nodes))
           (cond1    (car (plist-get where :conditions))))
      (should (eq (plist-get where :type) 'where))
      (should (equal (plist-get cond1 :value) kw)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PROPERTY P4: Former terminal identifiers are now plain values
;;; ─────────────────────────────────────────────────────────────────────────
;;
;; The old |syntax| used bare keywords show/count/commit/remove/stage/mark as
;; terminals.  In flat syntax these are just identifiers and can appear freely
;; as where-clause values without quoting.

(defconst gitq-flat-test--former-terminals
  '("show" "copy" "insert" "count" "branch-off" "amend" "squash"
    "reword" "remove" "delete" "commit" "stage" "mark" "worktree")
  "Bare identifiers that were terminals in |syntax| but are plain values now.")

(ert-deftest gitq-flat-test/p4-former-terminals-not-reserved ()
  "P4: former terminal identifiers are not step keywords or /terminals."
  (dolist (id gitq-flat-test--former-terminals)
    (should-not (gitq--flat-step-p id))
    (should-not (gitq--flat-terminal-p id))))

(ert-deftest gitq-flat-test/p4-commit-as-where-value ()
  "P4: 'commit' can appear unquoted as a where-clause value."
  (let* ((nodes (gitq--parse-flat "commits where message == commit"))
         (cond1 (car (plist-get (cadr nodes) :conditions))))
    (should (equal (plist-get cond1 :value) "commit"))))

(ert-deftest gitq-flat-test/p4-all-former-terminals-as-where-values ()
  "P4: every former terminal identifier is accepted unquoted as a where value."
  (dolist (id gitq-flat-test--former-terminals)
    (let* ((pipeline (format "commits where message == %s" id))
           (nodes    (gitq--parse-flat pipeline))
           (cond1    (car (plist-get (cadr nodes) :conditions))))
      (should (equal (plist-get cond1 :value) id)))))

(ert-deftest gitq-flat-test/p4-commit-then-count-unambiguous ()
  "P4: 'commit' as value is distinct from '/commit' terminal and '/count' terminal."
  ;; where message == commit → value "commit", no terminal
  (let* ((nodes (gitq--parse-flat "commits where message == commit"))
         (where (cadr nodes)))
    (should (eq (plist-get where :type) 'where))
    (should-not (eq (plist-get where :type) 'terminal)))
  ;; where message == commit /count → value "commit", then /count terminal
  (let* ((nodes (gitq--parse-flat "commits where message == commit /count"))
         (term  (car (last nodes))))
    (should (eq (plist-get term :type) 'terminal))
    (should (eq (plist-get term :op) 'count))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PROPERTY P5: Where bare flags terminate before step keywords
;;; ─────────────────────────────────────────────────────────────────────────
;;
;; where FLAG STEP args — FLAG is a bare boolean condition; STEP starts next stage.
;; The old |syntax| parser had a bug where STEP would be consumed as the operator.
;; The flat parser treats step keywords as boundaries so FLAG is always a bare flag.

(ert-deftest gitq-flat-test/p5-bare-flag-before-take ()
  "P5: 'where modified take 5' — modified is a bare flag, take starts next step."
  (let* ((nodes (gitq--parse-flat "worktrees where modified take 5"))
         (where (cadr nodes))
         (take  (nth 2 nodes))
         (cond1 (car (plist-get where :conditions))))
    (should (eq (plist-get where :type) 'where))
    (should (eq (plist-get cond1 :op) 'is))
    (should (eq (plist-get cond1 :value) t))
    (should (eq (plist-get take :type) 'take))
    (should (= (plist-get take :n) 5))))

(ert-deftest gitq-flat-test/p5-bare-flags-all-step-keywords ()
  "P5: 'where flag KEYWORD …' correctly identifies bare flag for every step keyword.
Bare where-conditions are only valid on flag-typed fields now, and only
worktree frames carry flags — so the source is `worktrees' throughout
and each keyword's dummy arg must be valid against worktree fields
(sha, path, branch, and the flags). `via .parent' would be a domain
error on worktree frames (no `parents-count'), so the via dummy is
`.parent†', which only needs `sha'.

`path' is excluded: on a path-carrying frame, `path' directly after a
where-condition always chains as another where FIELD, never as the
standalone path step — see the dedicated ambiguity test below."
  (dolist (kw (remove "path" (remove "first" (remove "last" gitq-flat-test--all-step-keywords))))
    (let* ((arg (pcase kw
                  ("take" "1") ("skip" "0") ("sort" "branch")
                  ("via" ".parent†") ("grep" "\"x\"") ("pickaxe" "\"x\"")
                  ("pick" "sha") ("where" "staged") (_ "")))
           (pipeline (string-trim (format "worktrees where modified %s %s" kw arg)))
           (nodes    (gitq--parse-flat pipeline))
           (where    (cadr nodes))
           (cond1    (car (plist-get where :conditions))))
      (should (eq (plist-get cond1 :op) 'is) )
      (should (eq (plist-get cond1 :value) t)
              ))))

(ert-deftest gitq-flat-test/p5-multi-condition-bare-flag ()
  "P5: 'where modified, staged take 3' — both bare flags, then take."
  (let* ((nodes  (gitq--parse-flat "worktrees where modified, staged take 3"))
         (where  (cadr nodes))
         (conds  (plist-get where :conditions))
         (take   (nth 2 nodes)))
    (should (= (length conds) 2))
    (should (eq (plist-get (nth 0 conds) :op) 'is))
    (should (eq (plist-get (nth 1 conds) :op) 'is))
    (should (eq (plist-get take :type) 'take))
    (should (= (plist-get take :n) 3))))

(ert-deftest gitq-flat-test/p5-bare-flag-before-terminal ()
  "P5: 'where modified /show' — modified is a bare flag, /show is terminal."
  (let* ((nodes (gitq--parse-flat "worktrees where modified /show"))
         (where (cadr nodes))
         (term  (nth 2 nodes))
         (cond1 (car (plist-get where :conditions))))
    (should (eq (plist-get cond1 :op) 'is))
    (should (eq (plist-get term :type) 'terminal))
    (should (eq (plist-get term :op) 'show))))

(ert-deftest gitq-flat-test/p5-path-step-after-where-is-loud ()
  "On a path-carrying frame, a `path' STEP directly after a where-clause
is unwritable — `path' chains as a where FIELD instead, and its glob
argument then fails operator validation LOUDLY.  It used to silently
parse as a garbage condition (operator `\"*\"', value t) that matched
nothing.  Write `path GLOB' before `where', or use `where path matches'."
  (let ((err (should-error (gitq--parse-flat "worktrees where modified path \"*\""))))
    (should (string-match-p "unknown where operator" (error-message-string err)))))

(ert-deftest gitq-flat-test/p5-bare-non-flag-field-still-hits-boundary ()
  "P5 under typing: a bare NON-flag field before a step keyword is a
flag-type error — proving the boundary was still detected (the step
keyword was NOT consumed as a where-operator, which would raise the
unknown-operator error instead)."
  (let ((err (should-error (gitq--parse-flat "commits where author take 5"))))
    (should (string-match-p "tests a flag" (error-message-string err)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PROPERTY P6: Source range terminates at step keywords
;;; ─────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-flat-test/p6-range-simple ()
  "P6: 'commits in main..feature' is parsed as range 'main..feature'."
  (let* ((src (gitq-flat-test--src "commits in main..feature")))
    (should (equal (plist-get src :range) "main..feature"))))

(ert-deftest gitq-flat-test/p6-range-then-take ()
  "P6: 'commits in main..HEAD take 5' — range is 'main..HEAD', then take 5."
  (let* ((nodes (gitq--parse-flat "commits in main..HEAD take 5"))
         (src   (car nodes))
         (take  (cadr nodes)))
    (should (equal (plist-get src :range) "main..HEAD"))
    (should (eq (plist-get take :type) 'take))
    (should (= (plist-get take :n) 5))))

(ert-deftest gitq-flat-test/p6-range-terminates-at-each-step-keyword ()
  "P6: 'commits in REF KEYWORD ...' — every step keyword ends the range.
`path' is excluded from this loop (commits are commit-shaped, no
`:path' field, so `path' can never immediately follow a bare `commits
in REF' regardless of the range-termination behavior under test here)
and covered separately by `p6-range-then-diff-then-path' below."
  (dolist (kw (remove "path" (remove "first" (remove "last" gitq-flat-test--all-step-keywords))))
    (let* ((arg (pcase kw
                  ("take" "1") ("skip" "0") ("sort" "date")
                  ("via" ".parent") ("grep" "\"x\"") ("pickaxe" "\"x\"")
                  ("pick" "sha") ("where" "message contains \"x\"") (_ "")))
           (pipeline (string-trim (format "commits in main %s %s" kw arg)))
           (nodes    (gitq--parse-flat pipeline))
           (src      (car nodes)))
      ;; Range should be "main", not "main take" etc.
      (should (equal (plist-get src :range) "main"))
      ;; Second node should be the step
      (should (eq (plist-get (cadr nodes) :type) (intern kw))))))

(ert-deftest gitq-flat-test/p6-range-then-diff-then-path ()
  "P6: range parsing plus the `path' step, once the frame is diff-shaped
(via `.diff', which carries `:path') rather than immediately after a
bare commit-shaped source."
  (let* ((nodes (gitq--parse-flat "commits in main via .diff path \"*.ts\""))
         (src   (car nodes)))
    (should (equal (plist-get src :range) "main"))
    (should (eq (plist-get (nth 2 nodes) :type) 'path))))

(ert-deftest gitq-flat-test/p6-range-multiple-tokens ()
  "P6: multi-token ranges like 'main..HEAD~3' parse as one range."
  (let* ((src (gitq-flat-test--src "commits in main..HEAD~3")))
    ;; Tokenizer splits HEAD~3 — check the join still works
    (should (stringp (plist-get src :range)))
    (should (string-match-p "main" (plist-get src :range)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PROPERTY P7: via .diff optional REF not consumed when it is a step keyword
;;; ─────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-flat-test/p7-via-diff-explicit-ref ()
  "P7: 'via .diff HEAD' — HEAD is the ref for .diff (not a step keyword)."
  (let* ((nodes (gitq--parse-flat "commits via .diff HEAD"))
         (via   (cadr nodes)))
    (should (eq (plist-get via :morphism) 'diff))
    (should (equal (plist-get via :ref) "HEAD"))))

(ert-deftest gitq-flat-test/p7-via-diff-step-keyword-not-consumed-as-ref ()
  "P7: 'HEAD via .diff take 5' — take is a step keyword, not the .diff ref."
  (let* ((nodes (gitq--parse-flat "HEAD via .diff take 5"))
         (via   (cadr nodes))
         (take  (nth 2 nodes)))
    (should (eq (plist-get via :morphism) 'diff))
    (should (null (plist-get via :ref)))       ; ref NOT consumed
    (should (eq (plist-get take :type) 'take))
    (should (= (plist-get take :n) 5))))

(ert-deftest gitq-flat-test/p7-via-diff-terminal-not-consumed-as-ref ()
  "P7: 'HEAD via .diff /show' — /show is terminal, not the .diff ref."
  (let* ((nodes (gitq--parse-flat "HEAD via .diff /show"))
         (via   (cadr nodes))
         (term  (nth 2 nodes)))
    (should (null (plist-get via :ref)))
    (should (eq (plist-get term :op) 'show))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; All /terminal keywords parse correctly in flat mode
;;; ─────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-flat-test/all-terminals-parse ()
  "Every /terminal keyword produces a terminal node at end of pipeline.
/delete is an alias: it parses to the same op as /remove, so the
executor can never again silently ignore it."
  (let ((simple-terminals
         '(("/show"     . show)
           ("/copy"     . copy)
           ("/insert"   . insert)
           ("/count"    . count)
           ("/remove"   . remove)
           ("/delete"   . remove)
           ("/stage"    . stage)
           ("/worktree" . worktree))))
    (dolist (pair simple-terminals)
      (let* ((nodes (gitq--parse-flat (format "commits %s" (car pair))))
             (term  (car (last nodes))))
        (should (eq (plist-get term :type) 'terminal))
        (should (eq (plist-get term :op) (cdr pair)))))))

(ert-deftest gitq-flat-test/terminal-branch-off-with-name ()
  "/branch-off \"feat\" produces branch-off terminal with name."
  (let* ((nodes (gitq--parse-flat "commits first /branch-off \"feat\""))
         (term  (car (last nodes))))
    (should (eq (plist-get term :op) 'branch-off))
    (should (equal (plist-get term :name) "feat"))))

(ert-deftest gitq-flat-test/terminal-squash-with-message ()
  "/squash \"msg\" produces squash terminal with message."
  (let* ((nodes (gitq--parse-flat "HEAD via .parent* /squash \"consolidated\""))
         (term  (car (last nodes))))
    (should (eq (plist-get term :op) 'squash))
    (should (equal (plist-get term :message) "consolidated"))))

(ert-deftest gitq-flat-test/terminal-amend-no-edit ()
  "/amend no-edit produces amend terminal with :no-edit t."
  (let* ((nodes (gitq--parse-flat "commits first /amend no-edit"))
         (term  (car (last nodes))))
    (should (eq (plist-get term :op) 'amend))
    (should (plist-get term :no-edit))))

(ert-deftest gitq-flat-test/terminal-commit-with-message ()
  "/commit \"msg\" produces commit terminal with message."
  (let* ((nodes (gitq--parse-flat "commits first /commit \"my message\""))
         (term  (car (last nodes))))
    (should (eq (plist-get term :op) 'commit))
    (should (equal (plist-get term :message) "my message"))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Tricky cross-product cases: values that look like keywords
;;; ─────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-flat-test/cross-former-terminal-in-where-multi-step ()
  "Former terminal as where value, followed by a step keyword."
  ;; 'commits where author == main sort date /show'
  ;; 'main' is not a keyword; sort starts step; /show is terminal.
  (let* ((nodes (gitq--parse-flat "commits where author == main sort date /show"))
         (where (cadr nodes))
         (sort  (nth 2 nodes))
         (term  (nth 3 nodes))
         (cond1 (car (plist-get where :conditions))))
    (should (equal (plist-get cond1 :value) "main"))
    (should (eq (plist-get sort :type) 'sort))
    (should (eq (plist-get term :op) 'show))))

(ert-deftest gitq-flat-test/cross-regex-value-then-terminal ()
  "Regex value in where, then /terminal — not confused."
  (let* ((nodes (gitq--parse-flat "commits where message matches /fix.*/ /count"))
         (where (cadr nodes))
         (term  (nth 2 nodes))
         (cond1 (car (plist-get where :conditions))))
    (should (equal (plist-get cond1 :value) "fix.*"))
    (should (eq (plist-get cond1 :op) 'matches))
    (should (eq (plist-get term :op) 'count))))

(ert-deftest gitq-flat-test/cross-operator-then-step-keyword-is-error ()
  "where FIELD OP STEP_KEYWORD — OP needs a value but gets a reserved word."
  ;; 'contains take' — 'take' is reserved, should error
  (should-error
   (gitq--parse-flat "commits where message contains take /show")
   :type 'error)
  ;; 'contains sort' — 'sort' is reserved, should error
  (should-error
   (gitq--parse-flat "commits where message contains sort /show")
   :type 'error))

(ert-deftest gitq-flat-test/cross-pick-fields-then-terminal ()
  "pick field1, field2 followed by /terminal — fields stop at terminal."
  (let* ((nodes (gitq--parse-flat "commits pick sha, message /show"))
         (pick  (cadr nodes))
         (term  (nth 2 nodes)))
    (should (eq (plist-get pick :type) 'pick))
    (should (equal (plist-get pick :fields) '(sha message)))
    (should (eq (plist-get term :op) 'show))))

(ert-deftest gitq-flat-test/cross-via-parent-then-where-then-take ()
  "via .parent* where author contains alice take 5 — three steps, each distinct."
  (let* ((nodes (gitq--parse-flat
                 "commits via .parent* where author contains alice take 5 /show")))
    (should (= (length nodes) 5))
    (should (eq (plist-get (nth 0 nodes) :type) 'source))
    (should (eq (plist-get (nth 1 nodes) :type) 'via))
    (should (eq (plist-get (nth 2 nodes) :type) 'where))
    (should (eq (plist-get (nth 3 nodes) :type) 'take))
    (should (eq (plist-get (nth 4 nodes) :type) 'terminal))))

(ert-deftest gitq-flat-test/cross-path-step-distinct-from-grep-inline ()
  "In flat mode, 'grep PAT path GLOB' means grep then path step (two stages)."
  (let* ((nodes (gitq--parse-flat "commits via .tree.blobs grep \"TODO\" path \"*.el\" /show")))
    ;; 5 nodes: source via grep path terminal
    (should (= (length nodes) 5))
    (should (eq (plist-get (nth 2 nodes) :type) 'grep))
    ;; path is a SEPARATE step (not inline in grep)
    (should (eq (plist-get (nth 3 nodes) :type) 'path))
    (should (equal (plist-get (nth 3 nodes) :pattern) "*.el"))
    ;; grep has no inline path-filter
    (should (null (plist-get (nth 2 nodes) :path-filter)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Source variety tests
;;; ─────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-flat-test/source-keywords ()
  "Each source keyword parses to the correct source type."
  (should (eq (plist-get (gitq-flat-test--src "branches") :source) 'branches))
  (should (eq (plist-get (gitq-flat-test--src "tags") :source) 'tags))
  (should (eq (plist-get (gitq-flat-test--src "refs") :source) 'refs))
  (should (eq (plist-get (gitq-flat-test--src "blobs") :source) 'blobs))
  (should (eq (plist-get (gitq-flat-test--src "worktrees") :source) 'worktree))
  (should (eq (plist-get (gitq-flat-test--src "commits") :source) 'commits)))

(ert-deftest gitq-flat-test/source-named-ref ()
  "An unrecognised source token is treated as a named ref."
  (let* ((src (gitq-flat-test--src "main")))
    (should (eq (plist-get src :source) 'ref))
    (should (equal (plist-get src :ref) "main")))
  (let* ((src (gitq-flat-test--src "HEAD")))
    (should (eq (plist-get src :source) 'ref))
    (should (equal (plist-get src :ref) "HEAD"))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Error cases — bad input must error, never silently misparsed
;;; ─────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-flat-test/error-empty-pipeline ()
  "Empty pipeline errors."
  (should-error (gitq--parse-flat "") :type 'error)
  (should-error (gitq--parse-flat "   ") :type 'error))

(ert-deftest gitq-flat-test/error-unknown-step-keyword ()
  "Unrecognised token after source errors."
  (should-error (gitq--parse-flat "commits frobnicate") :type 'error))

(ert-deftest gitq-flat-test/error-every-step-keyword-as-unquoted-where-value ()
  "Every step keyword in an unquoted where value position errors — exhaustive."
  (dolist (kw gitq-flat-test--all-step-keywords)
    (should-error
     (gitq--parse-flat (format "commits where message contains %s" kw))
     :type 'error)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Bare field names: closed-list validation and the `path' collision
;;; ─────────────────────────────────────────────────────────────────────────
;;
;; Fields dropped their leading "." (see gitq--field-names): where/sort/pick
;; now take bare identifiers, validated against a closed list instead of
;; recognized structurally by a leading dot. `path' is both a reserved step
;; keyword (the standalone `path GLOB' step) and a legitimate field (blob/
;; diff/hunk/line frames' path) -- these tests confirm both meanings resolve
;; correctly depending on context.

(ert-deftest gitq-flat-test/field-unknown-errors-after-where ()
  (should-error (gitq--parse-flat "commits where notarealfield == \"x\"") :type 'error))

(ert-deftest gitq-flat-test/field-unknown-errors-after-comma ()
  (should-error
   (gitq--parse-flat "commits where author == \"a\", notarealfield contains \"b\"")
   :type 'error))

(ert-deftest gitq-flat-test/field-unknown-errors-after-sort ()
  (should-error (gitq--parse-flat "commits sort notarealfield") :type 'error))

;; These use `blobs' rather than `commits' as the source: `path' is only
;; a real field on blob/diff/hunk/line frames (commit frames have no
;; `:path' at all), so exercising the field-vs-step-keyword collision
;; needs a source that structurally has the field to begin with.

(ert-deftest gitq-flat-test/field-path-as-where-condition ()
  "`path' resolves as a field when used in a where condition."
  (let* ((nodes (gitq--parse-flat "blobs where path == \"src/x.ts\""))
         (cond1 (car (plist-get (cadr nodes) :conditions))))
    (should (eq (plist-get cond1 :field) 'path))
    (should (equal (plist-get cond1 :value) "src/x.ts"))))

(ert-deftest gitq-flat-test/field-path-chained-without-comma ()
  "`path' resolves as a second, comma-less chained field, not a new stage.
The chained-bare-field position needs a flag-typed first field under
the type rules, so the fixture is a worktree pipeline (`modified' is a
flag and worktree frames also carry `path')."
  (let* ((nodes (gitq--parse-flat "worktrees where modified path == \"src/x.ts\""))
         (where (cadr nodes))
         (conds (plist-get where :conditions)))
    (should (= (length nodes) 2))              ; source, where (no terminal)
    (should (= (length conds) 2))
    (should (eq (plist-get (nth 0 conds) :field) 'modified))
    (should (eq (plist-get (nth 1 conds) :field) 'path))))

(ert-deftest gitq-flat-test/field-path-in-pick-comma-list ()
  "`path' resolves as a field inside `pick', not as a boundary ending the list."
  (let* ((nodes (gitq--parse-flat "blobs pick path, mode"))
         (pick  (cadr nodes)))
    (should (= (length nodes) 2))
    (should (equal (plist-get pick :fields) '(path mode)))))

(ert-deftest gitq-flat-test/field-path-in-pick-comma-less ()
  "`path' resolves as a field inside `pick' even without a comma."
  (let* ((nodes (gitq--parse-flat "blobs pick path mode"))
         (pick  (cadr nodes)))
    (should (equal (plist-get pick :fields) '(path mode)))))

(ert-deftest gitq-flat-test/field-path-still-works-as-standalone-step ()
  "`path' is still the standalone glob-filter step when it starts a fresh stage."
  (let* ((nodes (gitq--parse-flat "blobs path \"*.ts\"")))
    (should (eq (plist-get (cadr nodes) :type) 'path))
    (should (equal (plist-get (cadr nodes) :pattern) "*.ts"))))

(ert-deftest gitq-flat-test/field-path-standalone-step-after-another-step ()
  "`path' as a standalone step still works right after a prior, unrelated step."
  (let* ((nodes (gitq--parse-flat "blobs take 5 path \"*.ts\"")))
    (should (eq (plist-get (nth 1 nodes) :type) 'take))
    (should (eq (plist-get (nth 2 nodes) :type) 'path))))

(ert-deftest gitq-flat-test/field-sort-negation-bare ()
  "sort -field (bare, no dot) still tokenizes and parses as descending."
  (should (equal (gitq--tokenize-flat "sort -date") '("sort" "-date")))
  (let ((nodes (gitq--parse-flat "commits sort -date")))
    (should (eq (plist-get (cadr nodes) :field) 'date))
    (should (plist-get (cadr nodes) :desc))))

(ert-deftest gitq-flat-test/field-newly-added-fields-work ()
  "Fields present only on specific frame types (not the old \"well-known\"
subset) are still recognized: commit-sha, start-line, end-line via .diff.hunks."
  (let* ((nodes (gitq--parse-flat
                 "HEAD via .diff.hunks where commit-sha == \"abc\""))
         (where (nth 2 nodes))
         (cond1 (car (plist-get where :conditions))))
    (should (eq (plist-get cond1 :field) 'commit-sha))))

(provide 'git-branch-off-gitq-flat-test)
;;; git-branch-off-gitq-flat-test.el ends here
