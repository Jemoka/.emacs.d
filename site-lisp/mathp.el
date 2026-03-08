;;; typst-ts-mathp.el --- texmathp equivalent for typst-ts-mode -*- lexical-binding: t; -*-

;; This is a moral equivalent of AUCTeX's `texmathp' for Typst buffers
;; using typst-ts-mode and its tree-sitter parse tree.
;;
;; Usage:
;;   (typst-ts-mode-mathp)        ; returns t/nil, sets typst-ts-mode-mathp-why
;;
;; `typst-ts-mode-mathp-why' is a cons (TYPE . NODE) where TYPE is the
;; string "inline" or "block" and NODE is the enclosing math tree-sitter
;; node.

(require 'treesit)

(defvar typst-ts-mode-mathp-why nil
  "Info about why `typst-ts-mode-mathp' returned t.
Set after each call.  Value is a cons cell (TYPE . NODE) where:
  TYPE is \"inline\" for $x^2$ or \"block\" for $ x^2 $
  NODE is the enclosing tree-sitter math node.
Nil when point is not in math.")

(defun typst-ts-mode-mathp--block-p (math-node)
  "Return t if MATH-NODE is a block (display) equation.
In Typst, block equations have whitespace after the opening $
and before the closing $, e.g.: $ x^2 + y^2 $
Inline equations have none: $x^2 + y^2$"
  (let* ((text (treesit-node-text math-node))
         (len  (length text)))
    ;; Guard: must at least be "$X$" (3 chars).  The delimiters are
    ;; always a single "$" in Typst (unlike LaTeX's $$ vs $).
    (and (>= len 3)
         (string-match-p "^\\$[[:space:]\n]" text)
         (string-match-p "[[:space:]\n]\\$\\'" text))))

(defun typst-ts-mode-mathp ()
  "Determine if point is inside Typst math mode.
Returns t when inside a math node, nil otherwise.

Additional information is stored in `typst-ts-mode-mathp-why':
a cons cell (TYPE . NODE) where TYPE is \"inline\" or \"block\"
and NODE is the enclosing tree-sitter math node.

This function requires `typst-ts-mode' with tree-sitter active.
It operates on the live parse tree, so it reflects the current
buffer state without needing syntactic heuristics."
  (interactive)
  (setq typst-ts-mode-mathp-why nil)
  (unless (treesit-parser-list)
    (user-error "No tree-sitter parser active in this buffer"))
  (let* ((pos  ;; Stay inside the delimiters: at eob or just after "$"
               ;; the node-at returns things outside the math node, so
               ;; nudge one char back when we're right after a "$".
          (let ((p (point)))
            (if (and (> p (point-min))
                     (eq (char-before p) ?$))
                (1- p)
              p)))
         (node (treesit-node-at pos))
         (math-node
          (treesit-parent-until
           node
           (lambda (n) (string= (treesit-node-type n) "math"))
           t)))                          ; include node itself
    (when math-node
      (let* ((block-p   (typst-ts-mode-mathp--block-p math-node))
             (math-type (if block-p "block" "inline")))
        (setq typst-ts-mode-mathp-why (cons math-type math-node))
        (when (called-interactively-p 'any)
          (message "Point is in %s math (node %s..%s)"
                   math-type
                   (treesit-node-start math-node)
                   (treesit-node-end math-node)))
        t))))

(provide 'typst-ts-mathp)
;;; typst-ts-mathp.el ends here
