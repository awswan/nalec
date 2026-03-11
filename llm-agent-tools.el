;;; -*- lexical-binding: t; -*-

(require 'llm)
(require 'pcre2el)

(defvar nalec-roi-start 1)
(defvar nalec-roi-end 1)

(defun line-offset-from-point (point lines)
  "The position after moving by lines"
  (save-excursion
    (goto-char point)
    (forward-line lines)
    (point)))

(defun read-selection ()
  (buffer-substring-no-properties nalec-roi-start nalec-roi-end))

(defun select-beginning (lines)
  "Select the first few lines of the current buffer."
  (setq nalec-roi-start 1)
  (setq nalec-roi-end (line-offset-from-point 1 lines))
  (read-selection))

(defun select-end (lines)
  "Select the last few lines of the current buffer."
  (setq nalec-roi-start (line-offset-from-point (point-max) (- lines)))
  (setq nalec-roi-end (point-max))
  (read-selection))

(defun resize-selection (start-lines end-lines)
  "Resize the selected region"
  (setq nalec-roi-start (line-offset-from-point nalec-roi-start start-lines))
  (setq nalec-roi-end (line-offset-from-point nalec-roi-end end-lines))
  (read-selection))

(defun replace-entire-roi (replacement-text)
  (delete-region nalec-roi-start nalec-roi-end)
  (save-excursion
    (goto-char nalec-roi-start)
    (insert replacement-text "\n")))

(defun replace-string-in-roi (string replacement)
  (replace-string-in-region string replacement nalec-roi-start nalec-roi-end))

(defun select-roi-by-regexp (regexp lines-before lines-after from-start)
  "Move selection to the next text searching regular expression."
  (save-excursion
    (goto-char (if from-start 1 nalec-roi-end))
    (re-search-forward (rxt-pcre-to-elisp regexp))
    (setq nalec-roi-start (line-offset-from-point (point) (- lines-before)))
    (setq nalec-roi-end (line-offset-from-point (point) (+ lines-after 1))))
  (read-selection))

(defun select-next-region (lines)
  (setq nalec-roi-start nalec-roi-end)
  (setq nalec-roi-end (line-offset-from-point nalec-roi-end (+ lines 1))))

(defun insert-text (offset contents)
  (save-excursion
    (goto-char nalec-roi-start)
    (forward-line offset)
    (insert contents "\n")))

(defun nalec--buffer-list ()
  "Return list of current buffers, by their names and with silly ones removed."
  (seq-filter (lambda (s) (not (string-prefix-p " " s))) (mapcar 'buffer-name (buffer-list))))

(defun read-entire-buffer (buffer)
  (with-current-buffer buffer (buffer-string)))

(defconst llm-buffer-tools
      (list
       (llm-make-tool
	:function 'nalec--buffer-list
	:name "buffer_list"
	:description "Get a list of all open buffers")
       (llm-make-tool
	:function 'read-entire-buffer
	:name "read_entire_buffer"
	:description "Read the entire contents of the given buffer"
	:args '((:name "buffer" :type string :description "The buffer to read")))))

(defconst llm-select-tools
      (list
       (llm-make-tool
	:function 'select-roi-by-regexp
	:name "select_region_around_regexp"
	:description "Select a region around the next text matching a regexp, returning the current contents of the region"
	:args '((:name "regexp"
		       :type string
		       :description "A regular expression.")
		(:name "lines_before"
		       :type number
		       :description "Lines to include before the line with the match")
		(:name "lines_after"
		       :type number
		       :description "Lines to include after the line with the match")
		(:name "from_start"
		       :type boolean
		       :description "If true start searching from the start of the buffer, otherwise search from the end of the currently selected region")))
       (llm-make-tool
	:function 'select-beginning
	:name "select_beginning"
	:description "Select lines at the beginning of the buffer"
	:args '((:name "lines" :type number :description "number of lines")))
       (llm-make-tool
	:function 'select-end
	:name "select_end"
	:description "Select lines at the end of the buffer"
	:args '((:name "lines" :type number :description "number of lines")))
       (llm-make-tool
	:function 'resize-selection
	:name "resize_selection"
	:description "Resize the selected area by adding or removing lines to the top and bottom."
	:args '((:name "lines_before"
		       :type number
		       :description "Lines to add to the beginning of the selection. Use a negative number to remove lines from the top of the selection.")
		(:name "lines_after"
		       :type number
		       :description "Lines to add to the end of the selection. Use a negative number to remove lines from the end of the selection.")))))

(defconst llm-edit-tools
      (list
       (llm-make-tool
	:function 'replace-entire-roi
	:name "replace_entire_selection"
	:description "Replace the text inside the currently selected area with new text."
	:args '((:name "replacement_text" :type string :description "Replacement text to be inserted.")))
       (llm-make-tool
	:function 'replace-string-in-roi
	:name "replace_string_in_region"
	:description "Replace all occurances of the given string in the current selection with a replacement string"
	:args '((:name "string" :type string :description "String to replace")
		(:name "replacement" :type string :description "Replacement text")))
       (llm-make-tool
	:function 'insert-text
	:name "insert_text"
	:description "Insert text at the start of a line in the currently selected region."
	:args '((:name "offset" :type number :description "Line offset from the top of the currently selected region")
		(:name "text" :type string :description "Text to insert")))))

(provide 'llm-agent-tools)
