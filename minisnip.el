;;; minisnip.el --- Minimal snippets on top of abbrev + tempo  -*- lexical-binding: t; -*-

;; Author: M. Rincon
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; A personal snippet system leveraging Emacs's built in facility's.
;;
;;   * abbrev -- triggering, context gating, and auto-expansion.
;;   * tempo  -- templates, fields, one-shot mirrors, mark navigation.
;;
;; You write the templates on an elisp file so there is no parser. Just use
;; minisnip-define. The template can run arbitrary Elisp. There are two ways to
;; fire a snippet:
;;
;;   :auto t  -- expands hands-free whenever its :condition holds (e.g. inside a
;;               math environment via `texmathp'), the same way abbrev works.
;;   default  -- expands only when you press the trigger key (`minisnip-tab').
;;               This is useful so that short keys like "list" do not just fire.
;;
;; Usage:
;;   (require 'minisnip)
;;   (require 'minisnip-templates)
;;   (add-hook 'latex-mode-hook #'minisnip-mode)
;;   (add-hook 'minimd-ts-mode-hook #'minisnip-mode)

;;; Code:

(require 'abbrev)
(require 'tempo)

;; We need this forward declaration.
(defvar minisnip-mode)

(defgroup minisnip nil
  "Minimal personal snippet system."
  :group 'abbrev
  :prefix "minisnip-")

(defcustom minisnip-parents
  '((latex-mode text-mode)
    (org-mode text-mode)
    (minimd-ts-mode text-mode))
  "Alist mapping a major mode to modes whose snippets it inherits."
  :type '(alist :key-type symbol :value-type (repeat symbol)))

(defcustom minisnip-fallback #'indent-for-tab-command
  "Command run by `minisnip-tab' when there is nothing to do.

This runs when there is nothing to expanded or navigated and TAB has
no other binding in the buffer."
  :type 'function)

;;; Per-mode abbrev tables
(defvar minisnip--tables (make-hash-table :test 'eq)
  "Map of major-mode symbol to its minisnip abbrev table.")

(defvar minisnip--expanding nil
  "Non-nil while `minisnip-tab' is explicitly requesting an expansion.
Gates non-auto templates so they never fire during ordinary typing.")

(defvar-local minisnip--region nil
  "Cons (START . END) of markers bounding the most recent expansion.")

(defvar-local minisnip--field-bounds nil
  "(START . END) markers over the default text of the active snippet.")

(defvar-local minisnip--active-field nil
  "END marker of the selected default  or nil.")

(defun minisnip--table (mode)
  "Return, creating if needed, the minisnip abbrev table for MODE."
  (or (gethash mode minisnip--tables)
      (puthash mode (make-abbrev-table) minisnip--tables)))

(defun minisnip--wire-parents (mode &optional extra)
  "Set MODE's abbrev-table parents from `minisnip-parents'.
EXTRA, if non-nil, is an additional parent table (e.g. the buffer's
original `local-abbrev-table') appended after the configured parents."
  (abbrev-table-put
   (minisnip--table mode)
   :parents (append
             (mapcar #'minisnip--table (cdr (assq mode minisnip-parents)))
             (and extra (list extra)))))

;;; Templates
(defun minisnip-define (modes &rest props)
  "Define a snippet in MODES (a major-mode symbol or a list of modes).

Keyword PROPS:
  :key       Trigger string (required).
  :body      List of tempo elements (required, quoted).
  :auto      If non-nil, expand hands-free while :condition holds.
  :condition A function. Expansion is allowed only when it is non-nil,
             evaluated at point.  Defaults to t (always eligible)."
  (let ((key       (plist-get props :key))
        (body      (plist-get props :body))
        (auto      (plist-get props :auto))
        (condition (plist-get props :condition)))
    (unless (and key body)
      (error "`minisnip-define': :key and :body are required"))
    (dolist (mode (if (listp modes) modes (list modes)))
      (let* ((tname (format "minisnip-%s-%s" mode key))
             (tempo-fn (tempo-define-template tname body))
             (hook (intern (format "minisnip--insert-%s-%s" mode key))))
        (fset hook (lambda () (minisnip--expand tempo-fn) t))
        (put hook 'no-self-insert t)
        (define-abbrev (minisnip--table mode) key "" hook
          :system t
          :enable-function
          (lambda ()
            (and (or (null condition) (eval condition t))
                 (or auto minisnip--expanding))))))))

(defun minisnip--expand (tempo-fn)
  "Insert TEMPO-FN's template."
  (setq tempo-marks nil)
  (minisnip--reset-fields)
  (funcall tempo-fn)
  (setq minisnip--region
        (and tempo-marks
             (cons (copy-marker (car tempo-marks) t)
                   (copy-marker (car (last tempo-marks)) t))))
  (minisnip--select-default))

(defun minisnip--in-region-p ()
  "Non-nil when point sits inside the most recent expansion's field span."
  (and minisnip--region
       (marker-buffer (car minisnip--region))
       (< (marker-position (car minisnip--region)) (point))
       (<= (point) (marker-position (cdr minisnip--region)))))

;;; Editable defaults for the templates
(defun minisnip--reset-fields ()
  "Forget default-field bookkeeping from any previous expansion."
  (dolist (b minisnip--field-bounds)
    (set-marker (car b) nil)
    (set-marker (cdr b) nil))
  (setq minisnip--field-bounds nil
        minisnip--active-field nil))

(defun minisnip--insert-field (default)
  "Insert DEFAULT as an editable field: a tab stop whose text is selectable."
  (tempo-insert-mark (point-marker))     ; tab stop before the default
  (let ((start (point-marker)))
    (insert default)
    (push (cons start (point-marker)) minisnip--field-bounds)))

(defun minisnip--tempo-element (element)
  "Handle the minisnip (field DEFAULT) element for tempo.
Returns \"\" so tempo inserts nothing more when ELEMENT is handled."
  (when (and (consp element)
             (eq (car element) 'field)
             (stringp (cadr element)))
    (minisnip--insert-field (cadr element))
    ""))

(add-hook 'tempo-user-element-functions #'minisnip--tempo-element)

(defun minisnip--select-default ()
  "If point sits on a default field, select the text allowing replacement."
  (setq minisnip--active-field nil)
  (catch 'done
    (dolist (b minisnip--field-bounds)
      (when (and (marker-position (car b)) (= (point) (car b)))
        (set-mark (cdr b))
        (setq minisnip--active-field (cdr b))
        (activate-mark)
        (setq deactivate-mark nil)
        (throw 'done t)))))

(defun minisnip--clear-active-field ()
  "Delete the currently selected default to easy allow replacement."
  (when (and minisnip--active-field (marker-position minisnip--active-field))
    (delete-region (min (point) minisnip--active-field)
                   (max (point) minisnip--active-field)))
  (setq minisnip--active-field nil))

(defun minisnip--pre-command ()
  "Replace a selected default on edit; drop the selection on other commands.
Installed buffer-locally on `pre-command-hook' so that typing over a
preselected default works without `delete-selection-mode'."
  (when minisnip--active-field
    (cond
     ((memq this-command '(self-insert-command yank yank-pop))
      (minisnip--clear-active-field))
     ;; Leave state alone: Navigation reselects the next field itself.
     ((memq this-command '(minisnip-tab minisnip-next-field minisnip-prev-field))
      nil)
     (t (setq minisnip--active-field nil)))))

;;; Field prompting / mirrors. These are helper functions for snippets
(defun minisnip-pick (name prompt &optional choices default)
  "Read a value once and store it under NAME for mirroring with (s NAME).
PROMPT is the minibuffer prompt.  If CHOICES is non-nil, offer them via
`completing-read'; otherwise read free text.  DEFAULT is the initial value.
Returns \"\" so the call inserts nothing itself."
  (tempo-save-named
   name
   (if choices
       (completing-read prompt choices nil nil nil nil default)
     (read-string prompt nil nil default)))
  "")

(defun minisnip-image-files ()
  "Return image files under the current directory, for the `img' template.
Paths are relative to `default-directory', so links stay portable."
  (ignore-errors
    (mapcar (lambda (f) (file-relative-name f default-directory))
            (directory-files-recursively
             "." "\\.\\(png\\|gif\\|tiff\\|jpe?g\\|pnm\\)\\'"))))

;;; Trigger and field navigation
(defun minisnip-next-field ()
  "Move to the next field of the active snippet, if any."
  (interactive)
  (when (minisnip--in-region-p)
    (tempo-forward-mark)
    (minisnip--select-default)))

(defun minisnip-prev-field ()
  "Move to the previous field of the active snippet, if any."
  (interactive)
  (when (minisnip--in-region-p)
    (tempo-backward-mark)
    (minisnip--select-default)))

(defun minisnip--mode-chain (mode)
  "List MODE followed by its transitive parents from `minisnip-parents'."
  (let ((seen '()) (stack (list mode)))
    (while stack
      (let ((m (pop stack)))
        (unless (memq m seen)
          (push m seen)
          (setq stack (append (cdr (assq m minisnip-parents)) stack)))))
    (nreverse seen)))

(defun minisnip--keys (mode)
  "Sorted list of snippet keys available in MODE, including inherited ones."
  (let ((keys '()))
    (dolist (m (minisnip--mode-chain mode))
      (let ((tbl (gethash m minisnip--tables)))
        (when tbl
          (mapatoms (lambda (sym)
                      (let ((name (symbol-name sym)))
                        (when (and (> (length name) 0) (not (member name keys)))
                          (push name keys))))
                    tbl))))
    (sort keys #'string<)))

;;;###autoload
(defun minisnip-expand ()
  "Insert a snippet chosen by key via completion.
Unlike typing the key, this ignores a template's :condition, so a
math-only snippet can be inserted anywhere on purpose."
  (interactive)
  (let* ((keys (minisnip--keys major-mode))
         (key (if keys
                  (completing-read "Snippet: " keys nil t)
                (user-error "No minisnip snippets defined for %s" major-mode)))
         (sym (and key (abbrev-symbol key local-abbrev-table)))
         (hook (and sym (symbol-function sym))))
    (if (functionp hook)
        (funcall hook)
      (user-error "No snippet for %S" key))))

(defun minisnip-tab ()
  "Do the right thing for TAB: navigate, expand, or fall back.
If a field of the active snippet lies ahead, jump to it.  Otherwise try to
expand the snippet before point.  If neither applies, run whatever TAB would
do without `minisnip-mode' (org cycling, indentation, ...)."
  (interactive)
  (let ((start (point)))
    (minisnip-next-field)
    (when (= (point) start)
      (unless (let ((minisnip--expanding t)) (expand-abbrev))
        (minisnip--fallback)))))

(defun minisnip--fallback ()
  "Run the command TAB would run without `minisnip-mode' active."
  (let* ((minisnip-mode nil)            ; hide our keymap from the lookup
         (cmd (key-binding (this-command-keys-vector) t)))
    (cond ((commandp cmd) (call-interactively cmd))
          ((commandp minisnip-fallback) (call-interactively minisnip-fallback)))))

;;; Minor mode
(defvar minisnip-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'minisnip-tab)
    (define-key map (kbd "<tab>") #'minisnip-tab)
    (define-key map (kbd "<backtab>") #'minisnip-prev-field)
    map)
  "Keymap active in `minisnip-mode' buffers.")

(defvar-local minisnip--saved-abbrev-table nil
  "The buffer's `local-abbrev-table' before `minisnip-mode' replaced it.")

;;;###autoload
(define-minor-mode minisnip-mode
  "Expand minisnip templates via abbrev + tempo in this buffer."
  :lighter " Msnip"
  :keymap minisnip-mode-map
  (if minisnip-mode
      (progn
        (abbrev-mode 1)
        (add-hook 'pre-command-hook #'minisnip--pre-command nil t)
        (let ((orig local-abbrev-table)
              (own (minisnip--table major-mode)))
          ;; Remember the real table so it can be restored on disable, but
          ;; never save our own table back over it (re-enabling is a no-op).
          (unless (eq orig own)
            (setq minisnip--saved-abbrev-table orig))
          (minisnip--wire-parents major-mode (and (not (eq orig own)) orig))
          (setq-local local-abbrev-table own)))
    ;; Disable: drop field state, our hook, and restore the original table.
    (remove-hook 'pre-command-hook #'minisnip--pre-command t)
    (minisnip--reset-fields)
    (when minisnip--saved-abbrev-table
      (setq-local local-abbrev-table minisnip--saved-abbrev-table)
      (setq minisnip--saved-abbrev-table nil))))

(provide 'minisnip)
;;; minisnip.el ends here
