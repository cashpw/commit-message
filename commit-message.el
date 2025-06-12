;;; commit-message.el --- Commit message utilities -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: April 14, 2025
;; Modified: April 14, 2025
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashpw/commit-message
;; Package-Requires: ((emacs "24.3") (s "1.13.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Commit message utilities.
;;
;;  (add-hook 'git-commit-setup-hook 'commit-message-insert-message)
;;
;;; Code:

(require 'eieio)
(require 's)

(defgroup commit-message nil
  "Group for commit-message package."
  :tag "Commit message")

(defclass
  commit-message-category ()
  ((name
    :initarg
    :name
    :type string
    :documentation "Human-useful name for this category.")
   (short
    :initarg
    :short
    :type string
    :documentation "Short string representation of the category name.")
   (aliases
    :initarg
    :aliases
    :initform '()
    :type list
    :documentation "List of aliases (strings) to use, in addition to \"name\" when selecting category."))
  :group 'commit-message)

(defcustom commit-message-categories
  `(,(commit-message-category :name "Fix" :short "fix" :aliases '("Bug"))
    ,(commit-message-category :name "Feature" :short "feat" :aliases '("Add"))
    ,(commit-message-category :name "Document" :short "docs")
    ,(commit-message-category
      :name "Refactor"
      :short "refactor"
      :aliases '("Remove" "Delete"))
    ,(commit-message-category :name "Chore" :short "chore")
    ,(commit-message-category :name "Continuous integration" :short "ci")
    ,(commit-message-category :name "Documentation" :short "docs")
    ,(commit-message-category :name "Performance" :short "perf")
    ,(commit-message-category :name "Revert" :short "revert")
    ,(commit-message-category :name "Style" :short "style")
    ,(commit-message-category :name "Test" :short "test")
    ,(commit-message-category
      :name "Launch"
      :short "launch"
      :aliases '("Rollout" "Deploy"))
    ,(commit-message-category
      :name "Release"
      :short "release"
      :aliases '("Version")))
  "List of commit message categories.

This set is based on https://www.conventionalcommits.org/en/v1.0.0."
  :group 'commit-message
  :type (repeat 'sexp))

(defcustom commit-message-scopes '()
  "List of scopes."
  :group 'commit-message
  :type (repeat 'sexp))

(defcustom commit-message-scopes-accrue t
  "Add new scopes to `commit-message-scopes' when non-nil."
  :group 'commit-message
  :type 'boolean)

(defun commit-message--categories-as-alist (categories)
  "Return CATEGORIES (list of `commit-message-category' as an alist."
  (cl-reduce
   (lambda (a b) (append a b))
   (cl-mapcar
    (lambda (category)
      (with-slots
          (name short aliases) category
        (append
         `((,name . ,category) (,short . ,category))
         (cl-mapcar (lambda (alias) `(,alias . ,category)) aliases))))
    categories)))

(defun commit-message-read-category ()
  "Return commit category as selected by user."
  (let* ((categories-alist
          (commit-message--categories-as-alist commit-message-categories)))
    (cdr
     (assoc
      (completing-read "Category: " categories-alist
                       nil
                       'require-match)
      categories-alist))))

(defun commit-message-read-scope ()
  "Return scope as selected by user. Optionally update list of scopes."
  (when (y-or-n-p "Scope? ")
    (let ((scope
           (completing-read
            "Scope: " commit-message-scopes
            ;; predicate
            nil
            ;; Allow non-matching selections so the user can define new scopes
            nil)))
      (when commit-message-scopes-accrue
        (add-to-list 'commit-message-scopes scope))
      scope)))

(defun commit-message-read-breaking ()
  "Return non-nil if this is a breaking change."
  (y-or-n-p "Breaking change? "))

(defun commit-message-default-message-builder ()
  "Return defualt commit message."
  (let ((category (commit-message-read-category))
        (scope (commit-message-read-scope))
        (breaking (commit-message-read-breaking)))
    (with-slots
        (short) category
      (let ((wrapped-scope
             (if scope
                 (format "(%s)" scope)
               ""))
            (breaking-bang
             (if breaking
                 "!"
               "")))
        (s-lex-format "${short}${wrapped-scope}${breaking-bang}: CURSOR")))))

(defcustom commit-message-builder-fn #'commit-message-default-message-builder
  "Return commit message."
  :group 'commit-message
  :type 'function)

(defcustom commit-message-comment-prefix "#"
  "Comment prefix character."
  :group 'commit-message
  :type 'string)

(defun commit-message--comment-p (str)
  "Return non-nil if STR is commented out."
  (string-prefix-p commit-message-comment-prefix str))

(defun commit-message--commit-empty-p ()
  "Return non-nil if the buffer contains an empty commit message.

An empty commit message is one in which all lines are either commented out or
empty."
  (not
   (seq-find
    (lambda (line)
      (and
       (not (string-empty-p line))
       (not (commit-message--comment-p line))))
    (string-split (buffer-string) "\n"))))

(defun commit-message-maybe-insert-message ()
  "Insert chosen commit message unless there's already one present.

Optionally put cursor at CURSOR."
  (when (commit-message--commit-empty-p)
    (commit-message-insert-message)))

(defun commit-message-insert-message ()
  "Insert chosen commit message.

Optionally put cursor at CURSOR."
  (insert
   (funcall commit-message-builder-fn))
  (commit-message-place-cursor))

(defcustom commit-message-cursor-target "CURSOR"
  "Target string for cursor setting.

See `commit-message-place-cursor'."
  :group 'commit-message
  :type 'string)

(defun commit-message-place-cursor ()
  "Place the cursor on CURSOR."
  (let ((original-point (point))
        ;; Use case sensitive search.
        (case-fold-search nil))
    (goto-char (point-min))
    (let ((sf (search-forward commit-message-cursor-target nil t)))
      (message "end point: %s" sf)
      (if (not sf)
          (goto-char original-point)
        (message "start point (0): %s" (match-beginning 0))
        (let ((cursor-point (match-beginning 0)))
          (replace-match "")
          (insert " ")
          (goto-char cursor-point))))))

(provide 'commit-message)
;;; commit-message.el ends here
