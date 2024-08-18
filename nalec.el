;;; nalec.el --- A NAtural Language Commander for Emacs
;; -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines several interactive functions that take natural language
;; instructions as input.  These are processed using large language models via
;; the llm library.

;;; Code:

(require 'llm)
(require 'llm-openai)

(defgroup nalec nil "NAtural Language Commander for Emacs (nalec)." :group 'external)
(defcustom nalec-openai-api-key "" "A valid OpenAI API key." :type 'string)
(defcustom nalec-openai-model "gpt-4o-mini" "OpenAI model to use for nalec." :type 'string)

(make-variable-buffer-local 'nalec-command-status)
(make-variable-buffer-local 'nalec-most-recent-command)
(make-variable-buffer-local 'nalec-most-recent-prompt)
(make-variable-buffer-local 'nalec-most-recent-request)

(defun nalec-provider ()
  "Generate an llm-provider.  Currently only works for openai."
  (make-llm-openai :key nalec-openai-api-key :chat-model nalec-openai-model))

;; Same context prompt is used for both inserting and replacing
(defvar nalec-insert-prompt-context
  "The user is editing a file in emacs. Generate text to insert directly\
 into the file based on their instructions. Do not include explanation.\
 Do not quote inside a code block.")

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
  (format "I am working on a file in %s. Alter\
 the text below according to these instructions: %s.\n\n%s"
	  major-mode
	  instr
	  original))

(defun nalec-regexp-prompt-text (instr)
  "Generate user prompt for `nalec-regexp'.
INSTR contains instructions for building the regexp and replacement text."
  (format "I am working on a file in emacs %s.\
 I would like to carry out the following action using regular expression\
 search and replace: %s.
 Generate both the regular expression string and replacement string.\
 Return the answer as a json object with two string fields labelled\
 \"regular_expression\" and \"replacement_string\"."
	  major-mode
	  instr))

(defun nalec-redo-prompt-text (instr)
  "Generate user prompt for `nalec-redo'.
INSTR contains natural language instructions."
  (format "Your last reply was not quite right.\
 Please redo according to the following instructions: %s" instr))

(defun nalec-insert (desc)
  "Insert text based on natural language instructions.
DESC is a string description of the text to be inserted."
  (interactive "sInsert text matching description: ")
  (when (not (string-empty-p desc))
    (let* ((prompt (llm-make-chat-prompt (nalec-insert-prompt-text desc)
					 :context nalec-insert-prompt-context
					 :temperature 0.1))
	   (buffer (current-buffer))
	   (pt (point))
	   (llm-request (llm-chat-streaming-to-point
			 (nalec-provider)
			 prompt
			 buffer
			 pt
			 (lambda () (message "Finished nalec insert")
			   (setq nalec-command-status 'finished)))))
      (setq nalec-most-recent-command 'nalec-insert)
      (setq nalec-command-status 'in-progress)
      (setq nalec-most-recent-prompt prompt)
      (setq nalec-most-recent-request llm-request))))

(defun nalec-replace (instr)
  "Replace selected region based on natural language instructions.
INSTR is a string containing natural language instructions for modifying
the selected region."
  (interactive "sReplace region by: ")
  (when (string-empty-p instr) (setq instr "whatever is appropriate"))
  (let* ((original (buffer-substring-no-properties
		    (region-beginning) (region-end)))
	 (prompt (llm-make-chat-prompt (nalec-replace-prompt-text instr original)
				       :context nalec-insert-prompt-context
				       :temperature 0.1))
	 (_x (delete-region (region-beginning) (region-end)))
	 (llm-request (llm-chat-streaming-to-point
		       (nalec-provider)
		       prompt
		       (current-buffer)
		       (point)
		       (lambda () (message "Finished nalec replace region")
			 (setq nalec-command-status 'finished)))))
    (setq nalec-most-recent-command 'nalec-replace)
    (setq nalec-most-recent-request llm-request)
    (setq nalec-command-status 'in-progress)
    (setq nalec-most-recent-prompt prompt)))

(defun nalec--handle-regexp-response (resp)
  "Callback function for `nalec-regexp'.
Argument RESP is the response from the llm."
  (let ((resp-object (json-parse-string resp)))
    (setq nalec-command-status 'finished)
    (query-replace-regexp (gethash "regular_expression" resp-object)
			  (gethash "replacement_string" resp-object))))

(defun nalec-regexp (instr)
  "Carry out regexp search and replace based on natural language instructions.
INSTR is a string describing the aim of the regular expression search and
replace.  It is used to generate regexp and replacement strings that are then
passed to `query-replace-regexp'."
  (interactive "sReplace using regexp: ")
  (let* ((prompt
	  (llm-make-chat-prompt
	   (nalec-regexp-prompt-text instr)
	   :temperature 0.1
	   :non-standard-params ;; NB: Specific to openai
	   `(("response_format" .
	      ,(let ((table (make-hash-table :test 'equal)))
		(puthash "type" "json_object" table)
		table)))))
	 (llm-request (llm-chat-async (nalec-provider)
				      prompt
				      #'nalec--handle-regexp-response
				      (lambda (_ msg) (error msg)))))
    (setq nalec-command-status 'in-progress)
    (setq nalec-most-recent-command 'nalec-regexp-replace)
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
       (llm-chat-streaming-to-point
	(nalec-provider)
	nalec-most-recent-prompt
	(current-buffer)
	(point)
	(lambda () (message "Finished nalec redo")
	  (setq nalec-command-status 'finished))))
      ('nalec-regexp
       (llm-chat-prompt-append-response
	nalec-most-recent-prompt
	(nalec-redo-prompt-text instr))
       (setq nalec-command-status 'in-progress)
       (llm-chat-async (nalec-provider)
		       nalec-most-recent-prompt
		       #'nalec--handle-regexp-response
		       (lambda (_ msg) (error msg)))))))

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
