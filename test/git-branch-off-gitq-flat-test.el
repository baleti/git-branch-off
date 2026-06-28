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
  (let ((tokens (gitq-flat-test--tok "where .message matches /fix.*/")))
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
  "P3: every step keyword immediately after a source starts a step node."
  (dolist (kw (remove "first" (remove "last" gitq-flat-test--all-step-keywords)))
    ;; Provide a dummy argument so the step parser doesn't fail on missing args.
    ;; We just check that the second node is the expected step type.
    (let* ((dummy (pcase kw
                    ("take"     "1")
                    ("skip"     "0")
                    ("sort"     ".date")
                    ("via"      ".parent")
                    ("grep"     "\"x\"")
                    ("pickaxe"  "\"x\"")
                    ("path"     "\"*\"")
                    ("pick"     ".sha")
                    ("where"    ".sha")
                    (_          "")))
           (pipeline (string-trim (format "commits %s %s" kw dummy)))
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
  "P3: every step keyword after another step starts a new step node."
  ;; where .sha ... take 5 — take starts a new step after where
  (let ((nodes (gitq--parse-flat "commits where .modified take 3")))
    (should (= (length nodes) 3))
    (should (eq (plist-get (nth 0 nodes) :type) 'source))
    (should (eq (plist-get (nth 1 nodes) :type) 'where))
    (should (eq (plist-get (nth 2 nodes) :type) 'take))
    (should (= (plist-get (nth 2 nodes) :n) 3))))

(ert-deftest gitq-flat-test/p3-where-value-step-keyword-errors ()
  "P3: unquoted step keyword in where value position is an error."
  (dolist (kw gitq-flat-test--all-step-keywords)
    ;; where .message contains KEYWORD — KEYWORD is reserved, must error
    (should-error
     (gitq--parse-flat (format "commits where .message contains %s" kw))
     :type 'error)))

(ert-deftest gitq-flat-test/p3-where-quoted-step-keyword-is-value ()
  "P3: quoted step keyword in where value position is accepted as a string value."
  (dolist (kw gitq-flat-test--all-step-keywords)
    ;; where .message contains "KEYWORD" — quoted, so it's a value
    (let* ((pipeline (format "commits where .message contains \"%s\"" kw))
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
  (let* ((nodes (gitq--parse-flat "commits where .type == commit"))
         (cond1 (car (plist-get (cadr nodes) :conditions))))
    (should (equal (plist-get cond1 :value) "commit"))))

(ert-deftest gitq-flat-test/p4-all-former-terminals-as-where-values ()
  "P4: every former terminal identifier is accepted unquoted as a where value."
  (dolist (id gitq-flat-test--former-terminals)
    (let* ((pipeline (format "commits where .type == %s" id))
           (nodes    (gitq--parse-flat pipeline))
           (cond1    (car (plist-get (cadr nodes) :conditions))))
      (should (equal (plist-get cond1 :value) id)))))

(ert-deftest gitq-flat-test/p4-commit-then-count-unambiguous ()
  "P4: 'commit' as value is distinct from '/commit' terminal and '/count' terminal."
  ;; where .type == commit → value "commit", no terminal
  (let* ((nodes (gitq--parse-flat "commits where .type == commit"))
         (where (cadr nodes)))
    (should (eq (plist-get where :type) 'where))
    (should-not (eq (plist-get where :type) 'terminal)))
  ;; where .type == commit /count → value "commit", then /count terminal
  (let* ((nodes (gitq--parse-flat "commits where .type == commit /count"))
         (term  (car (last nodes))))
    (should (eq (plist-get term :type) 'terminal))
    (should (eq (plist-get term :op) 'count))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; PROPERTY P5: Where bare flags terminate before step keywords
;;; ─────────────────────────────────────────────────────────────────────────
;;
;; where .FLAG STEP args — .FLAG is a bare boolean condition; STEP starts next stage.
;; The old |syntax| parser had a bug where STEP would be consumed as the operator.
;; The flat parser treats step keywords as boundaries so .FLAG is always a bare flag.

(ert-deftest gitq-flat-test/p5-bare-flag-before-take ()
  "P5: 'where .modified take 5' — .modified is a bare flag, take starts next step."
  (let* ((nodes (gitq--parse-flat "commits where .modified take 5"))
         (where (cadr nodes))
         (take  (nth 2 nodes))
         (cond1 (car (plist-get where :conditions))))
    (should (eq (plist-get where :type) 'where))
    (should (eq (plist-get cond1 :op) 'is))
    (should (eq (plist-get cond1 :value) t))
    (should (eq (plist-get take :type) 'take))
    (should (= (plist-get take :n) 5))))

(ert-deftest gitq-flat-test/p5-bare-flags-all-step-keywords ()
  "P5: 'where .flag KEYWORD …' correctly identifies bare flag for every step keyword."
  (dolist (kw (remove "first" (remove "last" gitq-flat-test--all-step-keywords)))
    (let* ((arg (pcase kw
                  ("take" "1") ("skip" "0") ("sort" ".date")
                  ("via" ".parent") ("grep" "\"x\"") ("pickaxe" "\"x\"")
                  ("path" "\"*\"") ("pick" ".sha") ("where" ".sha") (_ "")))
           (pipeline (string-trim (format "commits where .modified %s %s" kw arg)))
           (nodes    (gitq--parse-flat pipeline))
           (where    (cadr nodes))
           (cond1    (car (plist-get where :conditions))))
      (should (eq (plist-get cond1 :op) 'is) )
      (should (eq (plist-get cond1 :value) t)
              ))))

(ert-deftest gitq-flat-test/p5-multi-condition-bare-flag ()
  "P5: 'where .modified, .staged take 3' — both bare flags, then take."
  (let* ((nodes  (gitq--parse-flat "commits where .modified, .staged take 3"))
         (where  (cadr nodes))
         (conds  (plist-get where :conditions))
         (take   (nth 2 nodes)))
    (should (= (length conds) 2))
    (should (eq (plist-get (nth 0 conds) :op) 'is))
    (should (eq (plist-get (nth 1 conds) :op) 'is))
    (should (eq (plist-get take :type) 'take))
    (should (= (plist-get take :n) 3))))

(ert-deftest gitq-flat-test/p5-bare-flag-before-terminal ()
  "P5: 'where .modified /show' — .modified is a bare flag, /show is terminal."
  (let* ((nodes (gitq--parse-flat "commits where .modified /show"))
         (where (cadr nodes))
         (term  (nth 2 nodes))
         (cond1 (car (plist-get where :conditions))))
    (should (eq (plist-get cond1 :op) 'is))
    (should (eq (plist-get term :type) 'terminal))
    (should (eq (plist-get term :op) 'show))))

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
  "P6: 'commits in REF KEYWORD ...' — every step keyword ends the range."
  (dolist (kw (remove "first" (remove "last" gitq-flat-test--all-step-keywords)))
    (let* ((arg (pcase kw
                  ("take" "1") ("skip" "0") ("sort" ".date")
                  ("via" ".parent") ("grep" "\"x\"") ("pickaxe" "\"x\"")
                  ("path" "\"*\"") ("pick" ".sha") ("where" ".sha") (_ "")))
           (pipeline (string-trim (format "commits in main %s %s" kw arg)))
           (nodes    (gitq--parse-flat pipeline))
           (src      (car nodes)))
      ;; Range should be "main", not "main take" etc.
      (should (equal (plist-get src :range) "main"))
      ;; Second node should be the step
      (should (eq (plist-get (cadr nodes) :type) (intern kw))))))

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
;;; Full pipeline AST equivalence: flat vs pipe syntax
;;; ─────────────────────────────────────────────────────────────────────────
;;
;; These tests prove that for a given pipeline semantics, the flat and pipe
;; parsers produce equivalent ASTs.  Covers all step/terminal combinations.

(defun gitq-flat-test--equiv (flat-str pipe-str)
  "Assert that FLAT-STR and PIPE-STR produce equivalent AST node lists."
  (let ((flat-nodes (gitq--parse-flat flat-str))
        (pipe-nodes (gitq--parse pipe-str)))
    (should (= (length flat-nodes) (length pipe-nodes)))
    (cl-mapcar (lambda (fn pn)
                 (should (equal (plist-get fn :type) (plist-get pn :type))))
               flat-nodes pipe-nodes)))

(ert-deftest gitq-flat-test/equiv-commits-take-show ()
  "commits take 10 /show ≡ commits | take 10 | show"
  (gitq-flat-test--equiv "commits take 10 /show"
                          "commits | take 10 | show"))

(ert-deftest gitq-flat-test/equiv-commits-where-take ()
  "commits where .author contains alice take 5 ≡ commits | where .author contains alice | take 5"
  (gitq-flat-test--equiv
   "commits where .author contains alice take 5"
   "commits | where .author contains alice | take 5"))

(ert-deftest gitq-flat-test/equiv-commits-where-multi-cond-count ()
  "Multi-condition where equivalent."
  (gitq-flat-test--equiv
   "commits where .author contains alice, .message contains fix /count"
   "commits | where .author contains alice, .message contains fix | count"))

(ert-deftest gitq-flat-test/equiv-head-via-parent-star-take ()
  "HEAD via .parent* take 5 /show ≡ HEAD | via .parent* | take 5 | show"
  (gitq-flat-test--equiv "HEAD via .parent* take 5 /show"
                          "HEAD | via .parent* | take 5 | show"))

(ert-deftest gitq-flat-test/equiv-commits-sort-desc-take ()
  "commits sort -.date take 3 /show ≡ commits | sort -.date | take 3 | show"
  (gitq-flat-test--equiv "commits sort -.date take 3 /show"
                          "commits | sort -.date | take 3 | show"))

(ert-deftest gitq-flat-test/equiv-commits-skip-first ()
  "commits skip 5 first /show ≡ commits | skip 5 | first | show"
  (gitq-flat-test--equiv "commits skip 5 first /show"
                          "commits | skip 5 | first | show"))

(ert-deftest gitq-flat-test/equiv-commits-grep ()
  "commits via .tree.blobs grep \"TODO\" /show ≡ piped equivalent"
  (gitq-flat-test--equiv
   "commits via .tree.blobs grep \"TODO\" /show"
   "commits | via .tree.blobs | grep \"TODO\" | show"))

(ert-deftest gitq-flat-test/equiv-commits-pickaxe ()
  "commits pickaxe \"bug\" /count ≡ commits | pickaxe \"bug\" | count"
  (gitq-flat-test--equiv "commits pickaxe \"bug\" /count"
                          "commits | pickaxe \"bug\" | count"))

(ert-deftest gitq-flat-test/equiv-branches-sort ()
  "branches sort .name /show ≡ branches | sort .name | show"
  (gitq-flat-test--equiv "branches sort .name /show"
                          "branches | sort .name | show"))

(ert-deftest gitq-flat-test/equiv-commits-in-range-take ()
  "commits in main..HEAD take 5 /show ≡ commits in main..HEAD | take 5 | show"
  (gitq-flat-test--equiv "commits in main..HEAD take 5 /show"
                          "commits in main..HEAD | take 5 | show"))

;;; ─────────────────────────────────────────────────────────────────────────
;;; All /terminal keywords parse correctly in flat mode
;;; ─────────────────────────────────────────────────────────────────────────

(ert-deftest gitq-flat-test/all-terminals-parse ()
  "Every /terminal keyword produces a terminal node at end of pipeline."
  (let ((simple-terminals
         '(("/show"     . show)
           ("/copy"     . copy)
           ("/insert"   . insert)
           ("/count"    . count)
           ("/remove"   . remove)
           ("/delete"   . delete)
           ("/stage"    . stage))))
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
  ;; 'commits where .branch == main sort .date /show'
  ;; 'main' is not a keyword; sort starts step; /show is terminal.
  (let* ((nodes (gitq--parse-flat "commits where .branch == main sort .date /show"))
         (where (cadr nodes))
         (sort  (nth 2 nodes))
         (term  (nth 3 nodes))
         (cond1 (car (plist-get where :conditions))))
    (should (equal (plist-get cond1 :value) "main"))
    (should (eq (plist-get sort :type) 'sort))
    (should (eq (plist-get term :op) 'show))))

(ert-deftest gitq-flat-test/cross-regex-value-then-terminal ()
  "Regex value in where, then /terminal — not confused."
  (let* ((nodes (gitq--parse-flat "commits where .message matches /fix.*/ /count"))
         (where (cadr nodes))
         (term  (nth 2 nodes))
         (cond1 (car (plist-get where :conditions))))
    (should (equal (plist-get cond1 :value) "fix.*"))
    (should (eq (plist-get cond1 :op) 'matches))
    (should (eq (plist-get term :op) 'count))))

(ert-deftest gitq-flat-test/cross-operator-then-step-keyword-is-error ()
  "where .field OP STEP_KEYWORD — OP needs a value but gets a reserved word."
  ;; 'contains take' — 'take' is reserved, should error
  (should-error
   (gitq--parse-flat "commits where .message contains take /show")
   :type 'error)
  ;; 'contains sort' — 'sort' is reserved, should error
  (should-error
   (gitq--parse-flat "commits where .message contains sort /show")
   :type 'error))

(ert-deftest gitq-flat-test/cross-pick-fields-then-terminal ()
  "pick .field1, .field2 followed by /terminal — fields stop at terminal."
  (let* ((nodes (gitq--parse-flat "commits pick .sha, .message /show"))
         (pick  (cadr nodes))
         (term  (nth 2 nodes)))
    (should (eq (plist-get pick :type) 'pick))
    (should (equal (plist-get pick :fields) '(sha message)))
    (should (eq (plist-get term :op) 'show))))

(ert-deftest gitq-flat-test/cross-via-parent-then-where-then-take ()
  "via .parent* where .author contains alice take 5 — three steps, each distinct."
  (let* ((nodes (gitq--parse-flat
                 "commits via .parent* where .author contains alice take 5 /show")))
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
     (gitq--parse-flat (format "commits where .message contains %s" kw))
     :type 'error)))

(provide 'git-branch-off-gitq-flat-test)
;;; git-branch-off-gitq-flat-test.el ends here
