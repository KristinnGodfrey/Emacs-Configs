;;; init.el --- Initialization file for Emacs
;;; package --- Summary
;;; Code:
;;; Commentary:
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;;lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))


(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-r") 'redo)
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "C-x u") 'undo-tree-visualize)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "M-e") 'end-of-buffer)
(global-set-key (kbd "M-a") 'beginning-of-buffer)

(global-set-key (kbd "C-c C-e") 'org-export-dispatch)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(eval-after-load "org"
  '(require 'ox-md nil t))

;; (defun org/get-headline-string-element  (headline backend info)
;;   (let ((prop-point (next-property-change 0 headline)))
;;     (if prop-point (plist-get (text-properties-at prop-point headline) :parent))))

;; (defun org/ensure-latex-clearpage (headline backend info)
;;   (when (org-export-derived-backend-p backend 'latex)
;;     (let ((elmnt (org/get-headline-string-element headline backend info)))
;;       (when (member "newpage" (org-element-property :tags elmnt))
;;         (concat "\\clearpage\n" headline)))))

(setq org-image-actual-width '(300))
(setq org-html-link-org-files-as-html nil)
(setq org-image-actual-width 100)

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun indent-file ()
  "Run org babel codeblock formatting in sequence."
  (interactive)
  (call-interactively 'mark-whole-buffer)
  (call-interactively 'indent-region)
	(call-interactively 'goto-last-change))

(global-set-key (kbd "C-t") 'my-run-org-babel-codeblock-format)
(global-set-key (kbd "C-;") 'goto-last-change)
(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-c C-p") 'org-backward-element)
(global-set-key (kbd "C-c C-n") 'org-forward-element)
(global-set-key (kbd "M-}") 'outline-next-visible-heading)
(global-set-key (kbd "M-{") 'outline-previous-visible-heading)



(setq org-toggle-inline-image nil)

(provide 'init)
;;; init.el ends here
