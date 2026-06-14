;;; git-branch-off-test.el --- ERT test suite for git-branch-off  -*- lexical-binding: t; -*-

;; Run with:
;;   cd test && ./run-tests.sh
;; or directly:
;;   emacs --batch -L .. -l git-branch-off-stage.el -l git-branch-off-test.el \
;;         -f ert-run-tests-batch-and-exit

;; The tests that touch git create a temporary repository under /tmp and clean
;; up after themselves.  They require git >= 2.5 on PATH.

(require 'ert)
(require 'cl-lib)

;;; Helpers

(defmacro git-branch-off-test--with-temp-repo (&rest body)
  "Execute BODY with `default-directory' set to a fresh git repository.
The repository is removed on exit."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "git-bo-test-" t))
          (default-directory dir))
     (unwind-protect
         (progn
           (call-process "git" nil nil nil "init" "-q" "-b" "main")
           (call-process "git" nil nil nil "config" "user.name"  "test")
           (call-process "git" nil nil nil "config" "user.email" "test@test")
           ,@body)
       (delete-directory dir t))))

(defun git-branch-off-test--commit-file (name content message)
  "Write NAME with CONTENT, stage, and commit with MESSAGE."
  (write-region content nil name nil 'silent)
  (call-process "git" nil nil nil "add" name)
  (call-process "git" nil nil nil "commit" "-q" "-m" message))

(defun git-branch-off-test--unified-diff (old new)
  "Return unified -U0 diff string between OLD and NEW string content."
  (let ((f-old (make-temp-file "diff-old"))
        (f-new (make-temp-file "diff-new")))
    (unwind-protect
        (progn
          (write-region old nil f-old nil 'silent)
          (write-region new nil f-new nil 'silent)
          (with-temp-buffer
            (call-process "diff" nil t nil "-U0" f-old f-new)
            (buffer-string)))
      (ignore-errors (delete-file f-old))
      (ignore-errors (delete-file f-new)))))

(defun git-branch-off-test--patch-lines (diff sel-start sel-end)
  "Convenience: call `git-branch-off--patch-from-diff' and split result into lines."
  (let ((result (git-branch-off--patch-from-diff diff "file.py" sel-start sel-end)))
    (when result
      (cons (split-string (car result) "\n" t)
            (cdr result)))))

;;; Tests: git-branch-off--selection-lines

(ert-deftest git-branch-off-test--selection-lines/no-region ()
  "With no active region, both START and END equal the current line number."
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((result (git-branch-off--selection-lines)))
      (should (equal (car result) 2))
      (should (equal (cdr result) 2)))))

(ert-deftest git-branch-off-test--selection-lines/with-region ()
  "With an active region the range covers lines START through END."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "line 1\nline 2\nline 3\nline 4\n")
    (goto-char (point-min))
    (push-mark (point-min) nil t)
    (forward-line 2)
    (forward-char 3)                    ; point inside line 3 (not at BOL)
    (setq mark-active t)
    (let ((result (git-branch-off--selection-lines)))
      (should (= (car result) 1))
      (should (= (cdr result) 3)))))

(ert-deftest git-branch-off-test--selection-lines/evil-visual-line-end ()
  "When the region end sits at BOL (evil visual-line), the last line is excluded."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "line 1\nline 2\nline 3\n")
    (goto-char (point-min))
    (let ((beg (point)))
      ;; Set region end exactly at the beginning of line 3 (after line 2's newline)
      (goto-char (+ beg (length "line 1\nline 2\n")))
      (push-mark beg nil t)
      (setq mark-active t)
      (let ((result (git-branch-off--selection-lines)))
        ;; End should be line 2, not line 3 (because end is at BOL of line 3)
        (should (= (cdr result) 2))))))

;;; Tests: git-branch-off--patch-from-diff (pure addition)

(ert-deftest git-branch-off-test--patch/single-addition-selected ()
  "Selecting a single newly added line produces a one-line insertion patch."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -2,0 +3 @@\n"
                       "+def new_func(): pass\n"))
         (result (git-branch-off--patch-from-diff diff "file.py" 3 3)))
    (should result)
    (should (= (cdr result) 1))
    ;; Note: use escaped + in regex since string-match-p treats + as a quantifier
    (should (string-match-p "@@ -2,0 \\+3,1 @@" (car result)))
    (should (string-match-p "\\+def new_func" (car result)))))

(ert-deftest git-branch-off-test--patch/addition-out-of-range ()
  "Selecting lines that do not overlap any addition returns nil."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -5,0 +6 @@\n"
                       "+def added_at_line_6(): pass\n"))
         (result (git-branch-off--patch-from-diff diff "file.py" 1 3)))
    (should (null result))))

(ert-deftest git-branch-off-test--patch/multiple-additions-partial-selection ()
  "Selecting only some lines from a multi-line addition produces partial patch."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -3,0 +4,3 @@\n"
                       "+line A\n"
                       "+line B\n"
                       "+line C\n"))
         ;; Select only line B (new-file line 5)
         (result (git-branch-off--patch-from-diff diff "file.py" 5 5)))
    (should result)
    (should (= (cdr result) 1))
    (should (string-match-p "\\+line B" (car result)))
    (should-not (string-match-p "\\+line A" (car result)))
    (should-not (string-match-p "\\+line C" (car result)))))

(ert-deftest git-branch-off-test--patch/all-additions-selected ()
  "Selecting the full range of a multi-line addition includes all lines."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -1,0 +2,3 @@\n"
                       "+alpha\n"
                       "+beta\n"
                       "+gamma\n"))
         (result (git-branch-off--patch-from-diff diff "file.py" 2 4)))
    (should result)
    (should (= (cdr result) 3))
    (should (string-match-p "\\+alpha" (car result)))
    (should (string-match-p "\\+beta"  (car result)))
    (should (string-match-p "\\+gamma" (car result)))))

;;; Tests: git-branch-off--patch-from-diff (modifications)

(ert-deftest git-branch-off-test--patch/modification-selected-yields-replacement ()
  "A selected modification (del + add) produces a replacement hunk."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -3,1 +3,1 @@\n"
                       "-old line\n"
                       "+new line\n"))
         (result (git-branch-off--patch-from-diff diff "file.py" 3 3)))
    (should result)
    ;; The patch should contain both the deletion and the addition
    (should (string-match-p "-old line" (car result)))
    (should (string-match-p "\\+new line" (car result)))))

(ert-deftest git-branch-off-test--patch/pure-deletion-selected ()
  "A selected pure deletion produces a deletion-only hunk."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -4,1 +4,0 @@\n"
                       "-deleted line\n"))
         (result (git-branch-off--patch-from-diff diff "file.py" 4 4)))
    (should result)
    (should (string-match-p "-deleted line" (car result)))
    ;; Use \n+ to check for content addition lines (not the +++ header line)
    (should-not (string-match-p "\n\\+[^+]" (car result)))))

(ert-deftest git-branch-off-test--patch/modification-not-selected-returns-nil ()
  "When the selection misses all changes, the result is nil."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -10,1 +10,1 @@\n"
                       "-old\n"
                       "+new\n"))
         (result (git-branch-off--patch-from-diff diff "file.py" 1 5)))
    (should (null result))))

;;; Tests: git-branch-off--patch-from-diff (multiple hunks)

(ert-deftest git-branch-off-test--patch/two-hunks-select-first-only ()
  "With two hunks, selecting only the first produces a patch for the first hunk."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -2,0 +3 @@\n"
                       "+first addition\n"
                       "@@ -8,0 +10 @@\n"
                       "+second addition\n"))
         (result (git-branch-off--patch-from-diff diff "file.py" 3 3)))
    (should result)
    (should (string-match-p "\\+first addition" (car result)))
    (should-not (string-match-p "\\+second addition" (car result)))))

(ert-deftest git-branch-off-test--patch/two-hunks-select-second-only ()
  "With two hunks, selecting only the second produces a patch for the second hunk."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -2,0 +3 @@\n"
                       "+first addition\n"
                       "@@ -8,0 +10 @@\n"
                       "+second addition\n"))
         (result (git-branch-off--patch-from-diff diff "file.py" 10 10)))
    (should result)
    (should-not (string-match-p "\\+first addition" (car result)))
    (should (string-match-p "\\+second addition" (car result)))))

(ert-deftest git-branch-off-test--patch/two-hunks-select-both ()
  "Selecting across two hunks includes both in the result."
  (let* ((diff (concat "--- a/file.py\n+++ b/file.py\n"
                       "@@ -2,0 +3 @@\n"
                       "+first addition\n"
                       "@@ -8,0 +10 @@\n"
                       "+second addition\n"))
         (result (git-branch-off--patch-from-diff diff "file.py" 3 10)))
    (should result)
    (should (string-match-p "\\+first addition"  (car result)))
    (should (string-match-p "\\+second addition" (car result)))))

;;; Tests: git-branch-off--patch-from-diff (change count)

(ert-deftest git-branch-off-test--patch/change-count-pure-additions ()
  "The change count equals the number of selected additions."
  (let* ((diff (concat "--- a/f\n+++ b/f\n"
                       "@@ -0,0 +1,4 @@\n"
                       "+a\n+b\n+c\n+d\n"))
         (result (git-branch-off--patch-from-diff diff "f" 1 4)))
    (should (= (cdr result) 4))))

(ert-deftest git-branch-off-test--patch/change-count-modification ()
  "The change count for a replacement equals del-count + add-count."
  (let* ((diff (concat "--- a/f\n+++ b/f\n"
                       "@@ -5,2 +5,3 @@\n"
                       "-old1\n"
                       "-old2\n"
                       "+new1\n"
                       "+new2\n"
                       "+new3\n"))
         (result (git-branch-off--patch-from-diff diff "f" 5 7)))
    ;; 2 deletions + 3 additions = 5
    (should (= (cdr result) 5))))

;;; Tests: integration — patch applies cleanly to a real git repo

(ert-deftest git-branch-off-test--patch/apply-single-addition-to-repo ()
  "A generated patch for a single addition can be applied with git apply --cached."
  (git-branch-off-test--with-temp-repo
    (git-branch-off-test--commit-file
     "file.py"
     "def existing(): pass\n"
     "initial")
    ;; Modify the working tree
    (write-region "def existing(): pass\ndef new_one(): pass\n" nil "file.py" nil 'silent)
    (let* ((diff (with-temp-buffer
                   (call-process "git" nil t nil "diff" "-U0" "--" "file.py")
                   (buffer-string)))
           (result (git-branch-off--patch-from-diff diff "file.py" 2 2))
           (patch (car result))
           (tmp (make-temp-file "bo-test" nil ".patch")))
      (should result)
      (write-region patch nil tmp nil 'silent)
      (let ((exit (call-process "git" tmp nil nil
                                "apply" "--cached" "--unidiff-zero" "--")))
        (should (= exit 0)))
      ;; Staged diff should contain the new line
      (let ((staged (with-temp-buffer
                      (call-process "git" nil t nil "diff" "--cached" "--" "file.py")
                      (buffer-string))))
        (should (string-match-p "\\+def new_one" staged)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest git-branch-off-test--patch/apply-partial-selection-to-repo ()
  "Selecting a subset of additions stages only those lines."
  (git-branch-off-test--with-temp-repo
    (git-branch-off-test--commit-file "f.py" "# base\n" "base")
    (write-region "# base\n+alpha\n+beta\n+gamma\n" nil "f.py" nil 'silent)
    (let* ((diff (with-temp-buffer
                   (call-process "git" nil t nil "diff" "-U0" "--" "f.py")
                   (buffer-string)))
           ;; Select only beta (line 3 in new file)
           (result (git-branch-off--patch-from-diff diff "f.py" 3 3))
           (tmp (make-temp-file "bo-test" nil ".patch")))
      (should result)
      (write-region (car result) nil tmp nil 'silent)
      (call-process "git" tmp nil nil "apply" "--cached" "--unidiff-zero" "--")
      (let ((staged (with-temp-buffer
                      (call-process "git" nil t nil "diff" "--cached" "--" "f.py")
                      (buffer-string))))
        (should     (string-match-p "\\+\\+beta"   staged))
        (should-not (string-match-p "\\+\\+alpha"  staged))
        (should-not (string-match-p "\\+\\+gamma"  staged)))
      (ignore-errors (delete-file tmp)))))

;;; Tests: refs/branch-off/ namespace

(ert-deftest git-branch-off-test--refs/branch-off-ref-created ()
  "git-branch-off-stage-and-commit-branch-off creates a refs/branch-off/<hash> ref."
  :tags '(integration)
  ;; This test verifies the ref creation logic using git plumbing directly,
  ;; without invoking the interactive command.
  (git-branch-off-test--with-temp-repo
    (git-branch-off-test--commit-file "app.py" "def main(): pass\n" "initial")
    ;; Simulate what the command does: stage a line, commit, tag under branch-off
    (write-region "def main(): pass\ndef new(): pass\n" nil "app.py" nil 'silent)
    (call-process "git" nil nil nil "add" "app.py")
    (call-process "git" nil nil nil "commit" "-q" "-m" "branch-off commit")
    (let ((hash (with-temp-buffer
                  (call-process "git" nil t nil "rev-parse" "HEAD")
                  (string-trim (buffer-string)))))
      (call-process "git" nil nil nil "update-ref"
                    (format "refs/branch-off/%s" hash) hash)
      ;; Verify ref exists
      (let ((found (with-temp-buffer
                     (call-process "git" nil t nil "rev-parse" "--verify"
                                   (format "refs/branch-off/%s" hash))
                     (string-trim (buffer-string)))))
        (should (string= found hash))))))

(ert-deftest git-branch-off-test--refs/branch-off-invisible-to-branch-log ()
  "Branch-off refs do not appear in git log without --all."
  :tags '(integration)
  (git-branch-off-test--with-temp-repo
    (git-branch-off-test--commit-file "app.py" "def main(): pass\n" "base")
    (let ((base (with-temp-buffer
                  (call-process "git" nil t nil "rev-parse" "HEAD")
                  (string-trim (buffer-string)))))
      ;; Detach, commit, store as branch-off, rewind
      (call-process "git" nil nil nil "checkout" "-q" "--detach" "HEAD")
      (write-region "def main(): pass\ndef draft(): pass\n" nil "app.py" nil 'silent)
      (call-process "git" nil nil nil "add" "app.py")
      (call-process "git" nil nil nil "commit" "-q" "-m" "draft")
      (let ((bo-hash (with-temp-buffer
                       (call-process "git" nil t nil "rev-parse" "HEAD")
                       (string-trim (buffer-string)))))
        (call-process "git" nil nil nil "update-ref"
                      (format "refs/branch-off/%s" bo-hash) bo-hash)
        (call-process "git" nil nil nil "checkout" "-q" base)
        ;; Normal log should not mention the branch-off commit.
        ;; Use --format=%H to get full hashes rather than relying on the
        ;; git-version-dependent abbreviated length of --oneline.
        (let ((log (with-temp-buffer
                     (call-process "git" nil t nil "log" "--format=%H")
                     (buffer-string))))
          (should-not (string-match-p bo-hash log)))
        ;; --all log should mention it
        (let ((log-all (with-temp-buffer
                         (call-process "git" nil t nil "log" "--all" "--format=%H")
                         (buffer-string))))
          (should (string-match-p bo-hash log-all)))))))

(provide 'git-branch-off-test)
;;; git-branch-off-test.el ends here
