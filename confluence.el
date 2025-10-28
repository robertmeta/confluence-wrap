;;; confluence.el --- Emacs interface for Confluence CLI -*- lexical-binding: t -*-

;; Author: Robert Melton
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, confluence, documentation

;;; Commentary:

;; This package provides an Emacs interface to the Confluence CLI tool.
;; It allows you to search, read, and manage Confluence pages from within Emacs.

;;; Code:

(defgroup confluence nil
  "Interface to Confluence CLI."
  :group 'tools)

(defcustom confluence-command "confluence"
  "Path to the confluence command-line tool."
  :type 'string
  :group 'confluence)

(defcustom confluence-search-limit 25
  "Number of search results to return."
  :type 'integer
  :group 'confluence)

(defcustom confluence-default-space nil
  "Default Confluence space key to use.
If nil, searches all spaces."
  :type '(choice (const :tag "All spaces" nil)
                 (string :tag "Space key"))
  :group 'confluence)

(defvar confluence-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'confluence-view-page-at-point)
    (define-key map (kbd "s") 'confluence-search)
    (define-key map (kbd "g") 'confluence-refresh)
    (define-key map (kbd "r") 'confluence-refresh)
    (define-key map (kbd "i") 'confluence-page-info-at-point)
    (define-key map (kbd "o") 'confluence-open-in-browser-at-point)
    (define-key map (kbd "S") 'confluence-list-spaces)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "?") 'confluence-help)
    map)
  "Keymap for `confluence-mode'.")

(defvar confluence-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "i") 'confluence-show-page-info)
    (define-key map (kbd "o") 'confluence-open-page-in-browser)
    (define-key map (kbd "s") 'confluence-search)
    (define-key map (kbd "?") 'confluence-help)
    map)
  "Keymap for `confluence-view-mode'.")

(defvar-local confluence-current-query nil
  "The current search query.")

(defvar-local confluence-search-results nil
  "Cached search results for the current buffer.")

(defvar-local confluence-current-page-id nil
  "The current page ID being viewed.")

;;; Utility functions

(defun confluence--run-command (&rest args)
  "Run confluence command with ARGS and return output."
  (with-temp-buffer
    (let ((exit-code (apply 'call-process confluence-command nil t nil args)))
      (if (zerop exit-code)
          (buffer-string)
        (error "Confluence command failed: %s" (buffer-string))))))

(defun confluence--parse-search-results (output)
  "Parse search OUTPUT into structured data."
  (let ((lines (split-string output "\n" t))
        results
        current-result)
    (dolist (line lines)
      (cond
       ;; Match result line: "1. Title (ID: 12345)"
       ((string-match "^\\([0-9]+\\)\\. \\(.*?\\) (ID: \\([0-9]+\\))$" line)
        (when current-result
          (push current-result results))
        (setq current-result
              (list :index (match-string 1 line)
                    :title (match-string 2 line)
                    :id (match-string 3 line)
                    :preview "")))
       ;; Preview lines (indented)
       ((and current-result (string-match "^   \\(.*\\)$" line))
        (let ((preview-line (match-string 1 line)))
          (setq current-result
                (plist-put current-result :preview
                          (concat (plist-get current-result :preview)
                                  (if (string-empty-p (plist-get current-result :preview))
                                      "" " ")
                                  preview-line)))))))
    (when current-result
      (push current-result results))
    (nreverse results)))

(defun confluence--get-page-at-point ()
  "Get the page data at point."
  (get-text-property (point) 'confluence-page))

;;; Emacspeak integration

(defun confluence--emacspeak-speak-line ()
  "Custom Emacspeak line speaking for confluence."
  (when (and (boundp 'emacspeak-speak-mode) emacspeak-speak-mode)
    (let ((page (confluence--get-page-at-point)))
      (when page
        (let* ((title (plist-get page :title))
               (id (plist-get page :id))
               (preview (plist-get page :preview))
               (clean-preview (replace-regexp-in-string "@@@hl@@@\\|@@@endhl@@@" "" preview))
               (speech-text (format "%s, page %s, %s"
                                    title
                                    id
                                    (if (> (length clean-preview) 100)
                                        (concat (substring clean-preview 0 100) "...")
                                      clean-preview))))
          (dtk-speak speech-text))
        t))))

;;; Display functions

(defun confluence--format-search-result (result)
  "Format RESULT for display."
  (let* ((title (plist-get result :title))
         (id (plist-get result :id))
         (preview (plist-get result :preview))
         ;; Clean up highlight markers
         (clean-preview (replace-regexp-in-string "@@@hl@@@\\|@@@endhl@@@" "" preview)))
    (format "%s (ID: %s)\n  %s"
            title
            id
            (if (> (length clean-preview) 150)
                (concat (substring clean-preview 0 150) "...")
              clean-preview))))

(defun confluence--insert-search-result (result)
  "Insert RESULT into the buffer."
  (let ((start (point))
        (formatted (confluence--format-search-result result)))
    (insert formatted)
    (put-text-property start (point) 'confluence-page result)
    ;; Add Emacspeak-specific spoken text
    (when (featurep 'emacspeak)
      (let* ((title (plist-get result :title))
             (id (plist-get result :id))
             (preview (plist-get result :preview))
             (clean-preview (replace-regexp-in-string "@@@hl@@@\\|@@@endhl@@@" "" preview))
             (speech-text (format "%s, page %s, %s"
                                  title
                                  id
                                  (if (> (length clean-preview) 100)
                                      (concat (substring clean-preview 0 100) "...")
                                    clean-preview))))
        (put-text-property start (point) 'emacspeak-speak speech-text)))
    (insert "\n\n")))

(defun confluence--display-search-results (query results)
  "Display search RESULTS for QUERY."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "Confluence Search: %s\n" query))
    (insert (format "Found %d results\n\n" (length results)))
    ;; Show appropriate keybindings based on whether Evil mode is active
    (if (and (boundp 'evil-mode) evil-mode (eq evil-state 'normal))
        ;; Evil mode keybindings
        (insert "Commands: [RET] view  [i] info  [o] open in browser  [s] new search  [S] spaces  [r] refresh  [q] quit  [?] help\n")
      ;; Emacs keybindings
      (insert "Commands: [RET] view  [i] info  [o] open in browser  [s] new search  [S] spaces  [g/r] refresh  [q] quit  [?] help\n"))
    (insert "\n")
    (if (null results)
        (insert "No results found.\n")
      (dolist (result results)
        (confluence--insert-search-result result)))))

;;; Interactive commands

;;;###autoload
(defun confluence-search (query)
  "Search Confluence for QUERY."
  (interactive "sSearch Confluence: ")
  (message "Searching Confluence for '%s'..." query)
  (let* ((args (list "search" query "--limit" (number-to-string confluence-search-limit)))
         (output (apply 'confluence--run-command args))
         (results (confluence--parse-search-results output)))
    (with-current-buffer (get-buffer-create (format "*Confluence Search: %s*" query))
      (confluence-mode)
      (setq confluence-current-query query)
      (setq confluence-search-results results)
      (confluence--display-search-results query results)
      (goto-char (point-min))
      (forward-line 5)
      (switch-to-buffer (current-buffer)))
    (message "Found %d results for '%s'" (length results) query)))

(defun confluence-refresh ()
  "Refresh the current search results."
  (interactive)
  (when confluence-current-query
    (message "Refreshing search results...")
    (let* ((args (list "search" confluence-current-query
                       "--limit" (number-to-string confluence-search-limit)))
           (output (apply 'confluence--run-command args))
           (results (confluence--parse-search-results output)))
      (setq confluence-search-results results)
      (let ((line (line-number-at-pos)))
        (confluence--display-search-results confluence-current-query results)
        (goto-char (point-min))
        (forward-line (1- line)))
      (message "Loaded %d results" (length results)))))

(defun confluence-view-page-at-point ()
  "View the Confluence page at point."
  (interactive)
  (let ((page (confluence--get-page-at-point)))
    (when page
      (let ((page-id (plist-get page :id)))
        (confluence-view-page page-id)))))

(defun confluence-view-page (page-id)
  "View Confluence page with PAGE-ID."
  (interactive "sPage ID: ")
  (message "Loading page %s..." page-id)
  (let ((content (confluence--run-command "read" page-id))
        (info (confluence--run-command "info" page-id)))
    (with-current-buffer (get-buffer-create (format "*Confluence Page: %s*" page-id))
      (confluence-view-mode)
      (setq confluence-current-page-id page-id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert info)
        (insert "\n")
        (insert (make-string 80 ?-))
        (insert "\n\n")
        (insert content)
        (goto-char (point-min)))
      (switch-to-buffer (current-buffer)))
    (message "Loaded page %s" page-id)))

(defun confluence-page-info-at-point ()
  "Show info for the Confluence page at point."
  (interactive)
  (let ((page (confluence--get-page-at-point)))
    (when page
      (let* ((page-id (plist-get page :id))
             (info (confluence--run-command "info" page-id)))
        (message "%s" info)))))

(defun confluence-show-page-info ()
  "Show info for the current page being viewed."
  (interactive)
  (when confluence-current-page-id
    (let ((info (confluence--run-command "info" confluence-current-page-id)))
      (message "%s" info))))

(defun confluence-open-in-browser-at-point ()
  "Open the Confluence page at point in a browser."
  (interactive)
  (let ((page (confluence--get-page-at-point)))
    (when page
      (let ((page-id (plist-get page :id)))
        (confluence-open-page-in-browser page-id)))))

(defun confluence-open-page-in-browser (&optional page-id)
  "Open Confluence PAGE-ID in a browser."
  (interactive)
  (let ((id (or page-id confluence-current-page-id)))
    (when id
      ;; Extract base URL from confluence config or use default
      (let ((url (format "https://confluenceent.cms.gov/pages/viewpage.action?pageId=%s" id)))
        (browse-url url)
        (message "Opening page %s in browser..." id)))))

(defun confluence-list-spaces ()
  "List all Confluence spaces."
  (interactive)
  (message "Loading Confluence spaces...")
  (let ((output (confluence--run-command "spaces")))
    (with-current-buffer (get-buffer-create "*Confluence Spaces*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Confluence Spaces\n\n")
        (insert output)
        (goto-char (point-min))
        (view-mode)
        (switch-to-buffer (current-buffer)))
      (message "Loaded Confluence spaces"))))

(defun confluence-help ()
  "Display help for Confluence mode."
  (interactive)
  (with-current-buffer (get-buffer-create "*Confluence Help*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Confluence Mode Help\n" 'face 'bold))
      (insert (make-string 80 ?=) "\n\n")

      (insert (propertize "FEATURES:\n" 'face 'bold))
      (insert "  • Search Confluence pages\n")
      (insert "  • View page content and metadata\n")
      (insert "  • Open pages in web browser\n")
      (insert "  • Browse Confluence spaces\n")
      (insert "  • Full Emacspeak integration for screen readers\n")
      (insert "  • Evil mode support with vim-like keybindings\n\n")

      (insert (propertize "EMACS KEYBINDINGS (Search Results):\n" 'face 'bold))
      (insert "  RET     - View page content\n")
      (insert "  s       - New search\n")
      (insert "  g/r     - Refresh current search\n")
      (insert "  i       - Show page info\n")
      (insert "  o       - Open page in browser\n")
      (insert "  S       - List all spaces\n")
      (insert "  n       - Next line\n")
      (insert "  p       - Previous line\n")
      (insert "  q       - Quit window\n")
      (insert "  ?       - Show this help\n\n")

      (insert (propertize "EMACS KEYBINDINGS (Page View):\n" 'face 'bold))
      (insert "  i       - Show page info\n")
      (insert "  o       - Open page in browser\n")
      (insert "  s       - New search\n")
      (insert "  q       - Quit window\n")
      (insert "  ?       - Show this help\n\n")

      (when (and (boundp 'evil-mode) evil-mode)
        (insert (propertize "EVIL MODE KEYBINDINGS (Search Results):\n" 'face 'bold))
        (insert "  RET     - View page content\n")
        (insert "  s       - New search\n")
        (insert "  r, R    - Refresh current search\n")
        (insert "  gr      - Refresh (Vim-style)\n")
        (insert "  i       - Show page info\n")
        (insert "  o       - Open page in browser\n")
        (insert "  S       - List all spaces\n")
        (insert "  j, k    - Move down/up\n")
        (insert "  gg      - Jump to top\n")
        (insert "  G       - Jump to bottom\n")
        (insert "  q, ZZ   - Quit window\n")
        (insert "  ?       - Show this help\n\n")

        (insert (propertize "EVIL MODE KEYBINDINGS (Page View):\n" 'face 'bold))
        (insert "  i       - Show page info\n")
        (insert "  o       - Open page in browser\n")
        (insert "  s       - New search\n")
        (insert "  j, k    - Move down/up\n")
        (insert "  gg      - Jump to top\n")
        (insert "  G       - Jump to bottom\n")
        (insert "  q, ZZ   - Quit window\n")
        (insert "  ?       - Show this help\n\n"))

      (insert "Press 'q' to close this help buffer.")
      (goto-char (point-min))
      (view-mode)
      (switch-to-buffer (current-buffer)))))

;;; Major modes

(define-derived-mode confluence-mode special-mode "Confluence"
  "Major mode for viewing Confluence search results.

Emacs keybindings:
\\{confluence-mode-map}

Evil mode keybindings (when Evil is loaded):
  j, k       - Navigate down/up
  RET        - View page content
  s          - New search
  r, R, gr   - Refresh
  i          - Show page info
  o          - Open in browser
  S          - List spaces
  gg, G      - Jump to top/bottom
  q, ZZ, ZQ  - Quit window
  ?          - Show help"
  (setq truncate-lines nil)
  (setq buffer-read-only t)
  ;; Set up Emacspeak integration
  (when (featurep 'emacspeak)
    (add-hook 'post-command-hook 'confluence--emacspeak-post-command nil t)))

(define-derived-mode confluence-view-mode special-mode "Confluence-View"
  "Major mode for viewing Confluence pages.

Emacs keybindings:
\\{confluence-view-mode-map}

Evil mode keybindings (when Evil is loaded):
  j, k       - Navigate down/up
  i          - Show page info
  o          - Open in browser
  s          - New search
  gg, G      - Jump to top/bottom
  q, ZZ, ZQ  - Quit window
  ?          - Show help"
  (setq truncate-lines nil)
  (setq buffer-read-only t))

(defun confluence--emacspeak-post-command ()
  "Emacspeak post-command hook for confluence mode."
  (when (and (featurep 'emacspeak)
             (eq major-mode 'confluence-mode)
             (memq this-command '(next-line previous-line)))
    (confluence--emacspeak-speak-line)))

;;; Evil mode integration

(with-eval-after-load 'evil
  ;; Use normal state for vim-like bindings
  (evil-set-initial-state 'confluence-mode 'normal)
  (evil-set-initial-state 'confluence-view-mode 'normal)

  ;; Define Evil keybindings for confluence-mode
  (evil-define-key 'normal confluence-mode-map
    (kbd "RET") 'confluence-view-page-at-point
    "j" 'next-line
    "k" 'previous-line
    "s" 'confluence-search
    "i" 'confluence-page-info-at-point
    "o" 'confluence-open-in-browser-at-point
    "S" 'confluence-list-spaces
    "r" 'confluence-refresh                  ; Primary refresh key
    "R" 'confluence-refresh                  ; Uppercase variant
    "g" nil                                  ; Prefix key for 'gr' and 'gg'
    "gr" 'confluence-refresh                 ; Vim-style refresh
    "gg" 'evil-goto-first-line
    "G" 'evil-goto-line
    "q" 'quit-window
    "ZZ" 'quit-window                        ; Vim-style quit
    "ZQ" 'quit-window                        ; Vim-style force quit
    "?" 'confluence-help)

  ;; Define Evil keybindings for confluence-view-mode
  (evil-define-key 'normal confluence-view-mode-map
    "j" 'next-line
    "k" 'previous-line
    "i" 'confluence-show-page-info
    "o" 'confluence-open-page-in-browser
    "s" 'confluence-search
    "g" nil                                  ; Prefix key for 'gg'
    "gg" 'evil-goto-first-line
    "G" 'evil-goto-line
    "q" 'quit-window
    "ZZ" 'quit-window                        ; Vim-style quit
    "ZQ" 'quit-window                        ; Vim-style force quit
    "?" 'confluence-help))

;;; Emacspeak advice

(with-eval-after-load 'emacspeak
  ;; Override emacspeak-speak-line for confluence-mode
  (defadvice emacspeak-speak-line (around confluence-mode activate)
    "Use custom line speaking in confluence-mode."
    (if (and (eq major-mode 'confluence-mode)
             (get-text-property (point) 'emacspeak-speak))
        (dtk-speak (get-text-property (point) 'emacspeak-speak))
      ad-do-it))

  (defadvice confluence-search (after emacspeak activate)
    "Provide auditory feedback when searching."
    (when (ems-interactive-p)
      (emacspeak-icon 'open-object)))

  (defadvice confluence-view-page (after emacspeak activate)
    "Provide auditory feedback when viewing a page."
    (when (ems-interactive-p)
      (emacspeak-icon 'open-object))))

(provide 'confluence)

;;; confluence.el ends here
