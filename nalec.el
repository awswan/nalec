;;; nalec.el --- NAtural Language Commands for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines some interactive functions that take natural language
;; instructions as input.  These are processed using large language models via
;; the llm library.

;;; Code:

(require 'llm)

(defun nalec--require-provider (symbol)
  (pcase symbol
     ('openai (require 'llm-openai))
     ('openai-compatible (require 'llm-openai))
     ('gemini (require 'llm-gemini))
     ('vertex (require 'llm-vertex))
     ('claude (require 'llm-claude))
     ('ollama (require 'llm-ollama))))

(defgroup nalec nil "NAtural Language Commands for Emacs (nalec)." :group 'external)
(defcustom nalec-temperature 0.2 "Temperature to use in prompts." :type 'float)
(defcustom nalec-provider-type 'none "Type of llm provider to use."
  :set (lambda (symbol value)
	 (nalec--require-provider value)
	 (set-default-toplevel-value symbol value))
  :type '(choice (const :tag "None" none)
		 (const :tag "OpenAI" openai)
		 (const :tag "OpenAI compatible" openai-compatible)
		 (const :tag "Gemini" gemini)
		 (const :tag "Vertex" vertex)
		 (const :tag "Claude" claude)
		 (const :tag "Ollama" ollama)))
(defcustom nalec-provider-options '() "Options for llm provider"
  :type '(plist :value-type string :tag "Options for llm provider"
		:options (:key :chat-model)))

(defun nalec-provider ()
  (apply
   (pcase nalec-provider-type
     ('none (error "Set an llm provider type in options to use nalec"))
     ('openai 'make-llm-openai)
     ('openai-compatible 'make-llm-openai-compatible)
     ('gemini 'make-llm-gemini)
     ('vertex 'make-llm-vertex)
     ('claude 'make-llm-claude)
     ('ollama 'make-llm-ollama))
   nalec-provider-options))

(defvar nalec-command-status)
(defvar nalec-most-recent-command)
(defvar nalec-insert-prompt)
(defvar nalec-regexp-prompt)
(defvar nalec-most-recent-request)
(defvar nalec-insert-session-mode nil)
(defvar nalec-region-start (make-marker))
(defvar nalec-region-end (make-marker))
(set-marker-insertion-type nalec-region-end t)

(defvar nalec-turbo-mode nil)
(defun nalec-toggle-turbo-mode ()
  (interactive)
  (setq nalec-turbo-mode (not nalec-turbo-mode))
  (message
   (if nalec-turbo-mode
       "Nalec turbo mode enabled"
     "Nalec turbo mode disabled")))

;; Same context prompt is used for both inserting and replacing
(defun nalec-insert-prompt-context ()
  (concat
   "The user is editing a file in emacs. Generate text to insert directly\
 into the file based on their instructions. "
   (if nalec-turbo-mode
       "Briefly explain your reasoning and then enclose the text\
 to insert in a code block."
   "Enclose the text to be inserted inside a code block. Do not include explanation.")))

(defun nalec-regexp-prompt-context ()
  (concat
   "You assist the user by generating emacs regular expression\
 and replacement strings to carry out their tasks.\n"
   (if nalec-turbo-mode
       "Briefly explain your reasoning and then give the regular\
 expression and replace string as a json object with two string fields\
 labelled \"regular_expression\" and \"replacement_string\". "
     "Return the answer as a json object with two string fields\
 labelled \"regular_expression\" and \"replacement_string\". ")
   "The regexp must be in valid emacs regexp format. For example to\
 match a whitespace character use `\s-` and to match `{` and `{`\
 include them directly without backslash."))

;; Message prompts
(defun nalec-insert-prompt-text (desc)
  "Generate user prompt for `nalec-insert'.
DESC is a description of the text to insert."
  (concat
   (format "I am working on a file in %s with %s lines. \
Generate text to insert matching the following description: %s\n"
	  major-mode
	  (count-lines (point-min) (point-max))
	  desc)
   (when (not (bolp))
     (concat (if (eolp)
		 (concat "The text will be inserted at the end of the following line:\n"
			 (thing-at-point 'line t))
	       (concat "The text will be inserted in the middle of the line below.\n"
		       (buffer-substring-no-properties (line-beginning-position) (point))
		       " YOUR TEXT GOES HERE "
		       (buffer-substring-no-properties (point) (line-end-position))))
	     "\nOnly return the text to insert, not the whole line."))))

(defun nalec-replace-prompt-text (instr original)
  "Generate user prompt for `nalec-replace'.
INSTR contains instructions and ORIGINAL is the original block of text."
  (format "I am working on a file in %s. Adjust the text selected from\
 the current buffer below according to these instructions: %s.\n\n%s"
	  major-mode
	  instr
	  original))

(defun nalec-yank-prompt-text (instr)
  (format "I am working on a file in %s and about to insert the text below\
 into the buffer. First adjust it according to these instructions: %s\n\n%s"
	  major-mode
	  instr
	  (current-kill 0)))

(defun nalec-regexp-prompt-text (instr)
  "Generate user prompt for `nalec-regexp'.
INSTR contains instructions for building the regexp and replacement text."
  (format "I am working on a file in emacs %s.\
 I would like to carry out the following action using regular expression\
 search and replace: %s."
	  major-mode
	  instr))

(defun nalec-redo-prompt-text (instr)
  "Generate user prompt for `nalec-redo'.
INSTR contains natural language instructions."
  (format "Your last reply was not quite right.\
 Please redo according to the following instructions: %s" instr))

(defun nalec--extract-codeblock (str)
  ;; finds the last codeblock in str
  (string-match ".*```.*?\n\\(\\([^`]`\\{,2\\}\\)*?\\)\\(\n```\\|\\'\\)" str)
  (or (match-string 1 str) ""))

(defun nalec--insert-callback (text)
  (with-current-buffer (marker-buffer nalec-region-start)
    (save-excursion
      (goto-char nalec-region-start)
      (delete-region nalec-region-start nalec-region-end)
      (insert (nalec--extract-codeblock text))))
  (if (and (eq (current-buffer) (marker-buffer nalec-region-start))
           (>= (point) nalec-region-start) (<= (point) nalec-region-end))
	(goto-char nalec-region-end)))

(defun nalec--error-callback (_ msg)
  (message "An llm error occured during nalec command: %s" msg)
  (setq nalec-command-status 'llm-error))

(defun nalec-start-session ()
  (interactive)
  (setq nalec-insert-prompt nil)
  (setq nalec-insert-session-mode t))

(defun nalec-end-session ()
  (interactive)
  (setq nalec-insert-session-mode nil))

(defun nalec-insert-at-point (prompt-text final-callback)
  (set-marker nalec-region-start (point))
  (set-marker nalec-region-end (point))
  (setq nalec-most-recent-command 'nalec-insert)
  (setq nalec-command-status 'in-progress)
  (if (and nalec-insert-session-mode nalec-insert-prompt)
      (llm-chat-prompt-append-response nalec-insert-prompt prompt-text)
    (setq nalec-insert-prompt (llm-make-chat-prompt prompt-text
						    :context (nalec-insert-prompt-context)
						    :temperature nalec-temperature)))
  (setq nalec-most-recent-request
	(llm-chat-streaming
	 (nalec-provider)
	 nalec-insert-prompt
	 #'nalec--insert-callback
	 (lambda (text)
	   (nalec--insert-callback text)
	   (setq nalec-command-status 'finished)
	   (funcall final-callback text))
	 #'nalec--error-callback)))

(defun nalec-insert (desc)
  "Insert text based on natural language instructions.
DESC is a string description of the text to be inserted."
  (interactive "MInsert text matching description: ")
  (when (string-empty-p desc) (error "Instructions required for nalec insert"))
  (message "Requesting insertion text from llm...")
  (nalec-insert-at-point (nalec-insert-prompt-text desc)
			 (lambda (_) (message "Finished nalec insert"))))

(defun nalec-replace (instr)
  "Replace selected region based on natural language instructions.
INSTR is a string containing natural language instructions for modifying
the selected region."
  (interactive
   (list (read-string "Replace region by (default \"whatever is appropriate\"): "
		 nil 'minibuffer-history
		 "whatever is appropriate" t)))
  (message "got instructions %s" instr)
  (let ((original (buffer-substring-no-properties
		    (region-beginning) (region-end))))
    (set-marker nalec-region-start (region-beginning))
    (set-marker nalec-region-end (region-end))
    (delete-region (region-beginning) (region-end))
    (message "Requesting replacement text from llm...")
    (nalec-insert-at-point
     (nalec-replace-prompt-text instr original)
     (lambda (text) (message
		 "Finished nalec replace region with %s characters changed"
		 (string-distance original text))))))

(defun nalec-yank (instr)
  "Get text from kill ring, adjust it according to instructions, and insert.
INSTR is a string containing the natural language instructions."
  (interactive
   (list (read-string "Adapt yank by (default \"whatever is appropriate\"): "
		 nil 'minibuffer-history
		 "whatever is appropriate" t)))
  (message "Sending text to llm...")
  (nalec-insert-at-point (nalec-yank-prompt-text instr)
			 (lambda (_) (message "Finished nalec yank"))))

(defun nalec--handle-regexp-response (resp)
  "Callback function for `nalec-regexp'.
Argument RESP is the response from the llm."
  (let ((resp-object (json-parse-string (nalec--extract-codeblock resp))))
    (setq nalec-command-status 'finished)
    (condition-case err
	(query-replace-regexp (gethash "regular_expression" resp-object)
			      (gethash "replacement_string" resp-object)
			      nil
			      (point-min)
			      (point-max))
      (invalid-regexp
       (message "Invalid regexp received: %s" (error-message-string err))
       (setq nalec-command-status 'regexp-format-error)))))

(defun nalec-regexp (instr)
  "Carry out regexp search and replace based on natural language instructions.
INSTR is a string describing the aim of the regular expression search and
replace.  It is used to generate regexp and replacement strings that are then
passed to `query-replace-regexp'."
  (interactive "sReplace using regexp: ")
  (let* ((prompt
	  (llm-make-chat-prompt
	   (nalec-regexp-prompt-text instr)
	   :context (nalec-regexp-prompt-context)
	   :temperature nalec-temperature))
	 (llm-request (llm-chat-async (nalec-provider)
				      prompt
				      #'nalec--handle-regexp-response
				      #'nalec--error-callback)))
    (setq nalec-command-status 'in-progress)
    (setq nalec-most-recent-command 'nalec-regexp)
    (setq nalec-regexp-prompt prompt)
    (setq nalec-most-recent-request llm-request)))

(defun nalec-redo (instr)
  "Try the last command again with additional instructions.
INSTR contains natural language instructions to be added to the chat."
  (interactive "sRedo with instructions: ")
  (when (eq nalec-command-status 'finished)
    (pcase nalec-most-recent-command
      ((or 'nalec-insert 'nalec-replace)
       (llm-chat-prompt-append-response
	nalec-insert-prompt
	(nalec-redo-prompt-text instr))
       (setq nalec-command-status 'in-progress)
       (delete-region nalec-region-start nalec-region-end)
       (message "Sent redo instructions to llm...")
       (setq nalec-most-recent-request
	     (llm-chat-streaming
	      (nalec-provider)
	      nalec-insert-prompt
	      #'nalec--insert-callback
	      (lambda (text)
		(nalec--insert-callback text)
		(message "Finished nalec redo")
		(setq nalec-command-status 'finished))
	      #'nalec--error-callback)))
      ('nalec-regexp
       (llm-chat-prompt-append-response
	nalec-regexp-prompt
	(nalec-redo-prompt-text instr))
       (setq nalec-command-status 'in-progress)
       (llm-chat-async (nalec-provider)
		       nalec-regexp-prompt
		       #'nalec--handle-regexp-response
		       #'nalec--error-callback)))))

(defun nalec-cancel ()
  "Cancel the most recent nalec command."
  (interactive)
  (if (eq nalec-command-status 'in-progress)
      (progn
	(llm-cancel-request nalec-most-recent-request)
	(setq nalec-command-status 'cancelled))
    (message "No nalec command in progress")))

(provide 'nalec)
;;; nalec.el ends here
