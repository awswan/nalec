;;; nalec.el --- NAtural Language Commands for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines some interactive functions that take natural language
;; instructions as input.  These are processed using large language models via
;; the llm library.

;;; Code:

(require 'llm)

(defgroup nalec nil "NAtural Language Commands for Emacs (nalec)." :group 'external)
(defcustom nalec-temperature 0.2 "Temperature to use in prompts." :type 'float)
(defcustom nalec-provider-type 'none "Type of llm provider to use."
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
     ('openai (require 'llm-openai) 'make-llm-openai)
     ('openai-compatible (require 'llm-openai) 'make-llm-openai-compatible)
     ('gemini (require 'llm-gemini) 'make-llm-gemini)
     ('vertex (require 'llm-vertex) 'make-llm-vertex)
     ('claude (require 'llm-claude) 'make-llm-claude)
     ('ollama (require 'llm-ollama) 'make-llm-ollama))
   nalec-provider-options))

(defvar nalec-command-status)
(defvar nalec-most-recent-command)
(defvar nalec-most-recent-prompt)
(defvar nalec-most-recent-request)
(defvar nalec-most-recent-start (make-marker))
(defvar nalec-most-recent-end (make-marker))
(set-marker-insertion-type nalec-most-recent-end t)

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
  (format "I am working on a file in %s with %s lines. \
Generate text to insert matching the following description: %s."
	  major-mode
	  (count-lines (point-min) (point-max))
	  desc))

(defun nalec-replace-prompt-text (instr original)
  "Generate user prompt for `nalec-replace'.
INSTR contains instructions and ORIGINAL is the original block of text."
  (format "I am working on a file in %s. Adjust\
 the text below according to these instructions: %s.\n\n%s"
	  major-mode
	  instr
	  original))

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
  (string-match ".*```.*?\n\\(\\([^`]`\\{,2\\}\\)*?\\)\\(```\\|\\'\\)" str)
  (or (match-string 1 str) ""))

(defun nalec--insert-callback (text)
  (with-current-buffer (marker-buffer nalec-most-recent-start)
    (save-excursion
      (goto-char nalec-most-recent-start)
      (delete-region nalec-most-recent-start nalec-most-recent-end)
      (insert (nalec--extract-codeblock text))))
  (if (and (eq (current-buffer) (marker-buffer nalec-most-recent-start))
           (>= (point) nalec-most-recent-start) (<= (point) nalec-most-recent-end))
	(goto-char nalec-most-recent-end)))

(defun nalec--error-callback (_ msg)
  (message "An llm error occured during nalec command: %s" msg)
  (setq nalec-command-status 'llm-error))

(defun nalec-insert-at-point (prompt final-callback)
  (set-marker nalec-most-recent-start (point))
  (set-marker nalec-most-recent-end (point))
  (setq nalec-most-recent-command 'nalec-insert)
  (setq nalec-command-status 'in-progress)
  (setq nalec-most-recent-prompt prompt)
  (setq nalec-most-recent-request
	(llm-chat-streaming
	 (nalec-provider)
	 prompt
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
  (nalec-insert-at-point
   (llm-make-chat-prompt (nalec-insert-prompt-text desc)
			 :context (nalec-insert-prompt-context)
			 :temperature nalec-temperature)
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
  (let* ((original (buffer-substring-no-properties
		    (region-beginning) (region-end)))
	 (prompt (llm-make-chat-prompt (nalec-replace-prompt-text instr original)
				       :context (nalec-insert-prompt-context)
				       :temperature nalec-temperature)))
    (message "Requesting replacement text from llm...")
    (nalec-insert-at-point
     prompt
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
  (nalec-insert-at-point
   (llm-make-chat-prompt (nalec-replace-prompt-text instr (current-kill 0))
			 :context (nalec-insert-prompt-context)
			 :temperature nalec-temperature)
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
    (setq nalec-most-recent-prompt prompt)
    (setq nalec-most-recent-request llm-request)))

(defun nalec-redo (instr)
  "Try the last command again with additional instructions.
INSTR contains natural language instructions to be added to the chat."
  (interactive "sRedo with instructions: ")
  (when (eq nalec-command-status 'finished)
    (pcase nalec-most-recent-command
      ((or 'nalec-insert 'nalec-replace)
       (llm-chat-prompt-append-response
	nalec-most-recent-prompt
	(nalec-redo-prompt-text instr))
       (setq nalec-command-status 'in-progress)
       (delete-region nalec-most-recent-start nalec-most-recent-end)
       (message "Sent redo instructions to llm...")
       (setq nalec-most-recent-request (llm-chat-streaming
	(nalec-provider)
	nalec-most-recent-prompt
	#'nalec--insert-callback
	(lambda (text)
	  (nalec--insert-callback text)
	  (message "Finished nalec redo")
	  (setq nalec-command-status 'finished))
	#'nalec--error-callback)))
      ('nalec-regexp
       (llm-chat-prompt-append-response
	nalec-most-recent-prompt
	(nalec-redo-prompt-text instr))
       (setq nalec-command-status 'in-progress)
       (llm-chat-async (nalec-provider)
		       nalec-most-recent-prompt
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
