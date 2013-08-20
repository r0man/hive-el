;;; hive.el --- Hive SQL mode extension

;; Copyright (C) 2013 Roman Scherer

;; Author: Roman Scherer <roman@burningswell.com>
;; Version: 0.2.0
;; Package-Requires: ((sql "3.0"))
;; Keywords: sql hive

;;; Commentary:

;; This package adds Hive to the sql-mode product list, and provides some
;; convenience functionality for using Hive via sql-interactive-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'sql)

(defcustom sql-hive-program "hive"
  "Command to start the Hive client."
  :type 'file
  :group 'SQL)

(defcustom sql-hive-options '()
  "List of additional options for `sql-hive-program'."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-hive-login-params '()
  "List of login parameters needed to connect to Hive."
  :type 'sql-login-params
  :group 'SQL)

(defcustom sql-hive-prompt "hive> "
  "Hive prompt."
  :type 'string
  :group 'SQL)

(defcustom sql-hive-format-results-as-org-table nil
  "Whether to print query results as a table.  Uses the table feature of
`org-mode'.  Requires hive.cli.print.header=true.  In sql-interactive-mode you
can toggle this by calling the function
`sql-hive-toggle-format-results-as-org-table'."
  :type 'boolean
  :group 'SQL)

(defconst sql-hive-results-header-re
  "^\\([a-z_][.A-Za-z0-9_-]*\t\\)*[a-z_][.A-Za-z0-9_-]*$"
  "Matches the header line of data results.  Assumes
hive.cli.print.header=true.")

(defconst sql-hive-results-terminator-re "^Time taken: [0-9.-]+"
  "Matches the line of hive output that always immediately follows a
resultset.")

(defvar sql-hive--resultset-partial nil
  "Temporarily save incomplete results between invocations of
`sql-hive-format-results-as-table'.  Since hive return results in pieces, a
single output may not represent a complete result set.")

(defun sql-hive-format-results-as-table (output)
  "Formats output from hive (tab-delimited fields, with column names in first
row) and as a table.  Uses org-table."
  (if (not sql-hive-format-results-as-org-table)
      output
    (let ((case-fold-search nil))
      (if (or (string-match sql-hive-results-header-re output)
              sql-hive--resultset-partial)
        (let* ((results-start (if sql-hive--resultset-partial
                                  0 (match-beginning 0)))
               (results-end (string-match
                             sql-hive-results-terminator-re
                             output results-start))
               (prompt-begin (string-match
                              (concat "^" sql-hive-prompt "$")
                              output (or results-end results-start)))
               (resultset (concat sql-hive--resultset-partial
                                  (substring output results-start results-end))))
          (condition-case err
            (cond
              ((null prompt-begin)
               ;; Midway through result set.  Store it and wait for more.
               (message  "Waiting for more hive output (%d rows so far)"
                         (- (count-non-empty-lines resultset) 1))
               (setq sql-hive--resultset-partial resultset)
               (substring output 0 results-start))
               ;; TODO(mweaver): May want to write partial raw results to a temp
               ;; file (instead of string + temp buffer), in case of crash
               ;; before we can format it.
              ((null results-end)
               ;; This isn't a table after all. Just dump the accumulated
               ;; results.
               (setq sql-hive--resultset-partial nil)
               (concat resultset
                       (substring output prompt-begin)))
              (t
               (message "DEBUG sql-hive-format-results-as-table: %s"
                        (substring output results-end))
               (setq sql-hive--resultset-partial nil)
               (concat
                (substring output 0 results-start)
                (sql-hive-results-to-table resultset)
                (substring output results-end) ))
                )
            (error
               (message "ERROR: %s" err)
               ;;(message "DEBUG: results-start=%d, results-end=%d, resultset=%s"
               ;;  (or (and (boundp 'results-start) results-start) -1)
               ;;  (or (and (boundp 'results-end) results-end) -1)
               ;;  (or (and (boundp 'resultset) resultset) "<empty resultset>"))

               ;; If formatting fails, always fall back to outputting the raw
               ;; results.
             ) ))
        output)
      )))

(defun count-non-empty-lines (str)
  "Returns the number of non-empty lines in `str', which can't be nil."
  (let ((count (if (> (length str) 0) 1 0))
        (pos 0))
    (while (string-match "\n.+" str pos)
      (setq pos (match-end 0))
      (setq count (1+ count)))
    count))

(defun sql-hive-results-to-table (resultset)
  (require 'org-table)
  (with-temp-buffer
    (org-mode)
    (insert resultset)
    (org-table-convert-region (point-min)  (point-max) '(16))
    (goto-char (point-min))
    (org-table-insert-hline t)    ;; insert line above
    (org-table-insert-hline nil)  ;; insert line below
    (goto-char (point-max))
    (forward-line -1)
    (org-table-insert-hline nil)
    (buffer-substring (point-min) (point-max))
    ))

(defun sql-hive-region-as-table (beg end)
  "This is for testing sql-hive-results-to-table.  Formats the hive
result set in region"
  (interactive "r")
  (let ((results (buffer-substring beg end)))
    (delete-region beg end)
    (insert (sql-hive-results-to-table results)) ))

(defun sql-hive-toggle-format-results-as-org-table ()
  "Toggles variable `sql-hive-format-results-as-org-table', which formats the hive
resultset as an org-mode table."
  (interactive)
  (unless (eq major-mode 'sql-interactive-mode)
    (error "Please only call this in `sql-interactive mode', for now at least."))
  (if sql-hive-format-results-as-org-table
      (progn
        (remove-hook 'comint-preoutput-filter-functions
                     'sql-hive-format-results-as-table t)
        (setq sql-hive-format-results-as-org-table nil)
        (message "DEBUG sql-hive-toggle-format-results-as-org-table: Removed formatting hook")
        )
    (add-hook 'comint-preoutput-filter-functions
              'sql-hive-format-results-as-table t t)
    (setq sql-hive-format-results-as-org-table t)
    (message "DEBUG sql-hive-toggle-format-results-as-org-table: Added formatting hook")
    )
    ;; TODO(mweaver): Only if a sql-hive process is running should we fiddle
    ;; with the hooks. (?)
  )

;; Handle empty input

(defconst sql-hive-empty-input-string "   ;   "
  "The no-op we pass on to hive when you enter an empty line")

(defun handle-empty-input-filter (input)
  "Hive doesn't handle an empty input nicely.  So replace it with a bogus no-op
input (and then delete the echo via `sql-hive-scrub-empty-input-output')."
  (if (or (not (string-equal "" input))
          comint-input-sender-no-newline)
      input
    sql-hive-empty-input-string))

(defun sql-hive-scrub-empty-input-output (str)
  "Delete the output which is just an echo of the bogus input."
  (if (string-equal str
                    (concat sql-hive-empty-input-string "\n" sql-hive-prompt))
      sql-hive-prompt
    str))

;; Font lock support

(defvar sql-mode-hive-font-lock-keywords
  (eval-when-compile
    (list
     ;; For query results when `sql-hive-format-results-as-org-table' is set.
     (list "^|[ -].*[ -]|$" 0 'font-lock-function-name-face t)

     ;; Hive Reserved
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
       "add" "after" "all" "alter" "analyze" "and" "archive" "as" "asc" "before"
       "between" "binary" "both" "bucket" "buckets" "by" "cascade" "case" "cast"
       "change" "cluster" "clustered" "clusterstatus" "collection" "column"
       "columns" "comment" "compute" "concatenate" "continue" "create" "cross"
       "cube" "current" "cursor" "data" "database" "databases" "dbproperties"
       "deferred" "delete" "delimited" "dependency" "desc" "describe"
       "directories" "directory" "disable" "distinct" "distribute" "drop" "else"
       "enable" "end" "escaped" "exchange" "exclusive" "exists" "explain"
       "export" "extended" "external" "false" "fetch" "fields" "fileformat"
       "first" "following" "for" "format" "formatted" "from" "full" "function"
       "functions" "grant" "group" "grouping" "having" "hold_ddltime"
       "idxproperties" "if" "ignore" "import" "in" "index" "indexes" "inner"
       "inpath" "inputdriver" "inputformat" "insert" "intersect" "into" "is"
       "items" "join" "keys" "$elem$" "$key$" "$value$" "lateral" "left" "less"
       "like" "limit" "lines" "load" "local" "location" "lock" "locks" "long"
       "macro" "mapjoin" "materialized" "minus" "more" "msck" "noscan" "not"
       "no_drop" "null" "of" "offline" "on" "option" "or" "orc" "order" "out"
       "outer" "outputdriver" "outputformat" "over" "overwrite" "partialscan"
       "partition" "partitioned" "partitions" "percent" "plus" "preceding"
       "preserve" "pretty" "procedure" "protection" "purge" "range" "rcfile"
       "read" "readonly" "reads" "rebuild" "recordreader" "recordwriter"
       "reduce" "regexp" "rename" "repair" "replace" "restrict" "revoke" "right"
       "rlike" "role" "rollup" "row" "rows" "schema" "schemas" "select" "semi"
       "sequencefile" "serde" "serdeproperties" "set" "sets" "shared" "show"
       "show_database" "skewed" "sort" "sorted" "ssl" "statistics" "stored"
       "streamtable" "table" "tables" "tablesample" "tblproperties" "temporary"
       "terminated" "textfile" "then" "to" "touch" "transform" "trigger" "true"
       "truncate" "unarchive" "unbounded" "undo" "union" "uniquejoin" "unlock"
       "unset" "unsigned" "update" "use" "user" "using" "utc" "utc_tmestamp"
       "view" "when" "where" "while" "window" "with"
       )

     ;; Hive Data Types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
       "bigint" "binary" "boolean" "date" "datetime" "decimal" "double" "float"
       "int" "smallint" "string" "timestamp" "tinyint" "uniontype")

     ;; Functions (defined as of 01-Jul-2013)
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
        "abs" "acos" "and" "array" "array_contains" "ascii" "asin" "assert_true"
        "atan" "avg" "between" "bin" "case" "ceil" "ceiling" "coalesce"
        "collect_set" "compute_stats" "concat" "concat_ws" "context_ngrams"
        "conv" "corr" "cos" "count" "covar_pop" "covar_samp" "create_union"
        "cume_dist" "date_add" "date_sub" "datediff" "day" "dayofmonth"
        "degrees" "dense_rank" "div" "e" "elt" "ewah_bitmap" "ewah_bitmap_and"
        "ewah_bitmap_empty" "ewah_bitmap_or" "exp" "explode" "field"
        "find_in_set" "first_value" "floor" "format_number" "from_unixtime"
        "from_utc_timestamp" "get_json_object" "hash" "hex" "histogram_numeric"
        "hour" "if" "in" "in_file" "index" "inline" "instr" "isnotnull" "isnull"
        "java_method" "json_tuple" "lag" "last_value" "lcase" "lead" "length"
        "like" "ln" "locate" "log" "log10" "log2" "lower" "lpad" "ltrim" "map"
        "map_keys" "map_values" "max" "min" "minute" "month" "named_struct"
        "negative" "ngrams" "noop" "noopwithmap" "not" "npath" "ntile" "nvl"
        "or" "parse_url" "parse_url_tuple" "percent_rank" "percentile"
        "percentile_approx" "pi" "pmod" "positive" "pow" "power" "printf"
        "radians" "rand" "rank" "reflect" "reflect2" "regexp" "regexp_extract"
        "regexp_replace" "repeat" "reverse" "rlike" "round" "row_number" "rpad"
        "rtrim" "second" "sentences" "sign" "sin" "size" "sort_array" "space"
        "split" "sqrt" "stack" "std" "stddev" "stddev_pop" "stddev_samp"
        "str_to_map" "struct" "substr" "substring" "sum" "tan" "to_date"
        "to_unix_timestamp" "to_utc_timestamp" "translate" "trim" "ucase"
        "unhex" "unix_timestamp" "upper" "var_pop" "var_samp" "variance"
        "weekofyear" "when" "windowingtablefunction" "xpath" "xpath_boolean"
        "xpath_double" "xpath_float" "xpath_int" "xpath_long" "xpath_number"
        "xpath_short" "xpath_string" "year"
     )
     (list "'.*?'\\|\".*?\"" 0 'font-lock-string-face t)
      ))
  "Hive SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-hive-font-lock-keywords'.")


(add-hook 'sql-interactive-mode-hook
  '(lambda ()
     (when (eq sql-product 'hive)
       (add-hook 'comint-preoutput-filter-functions
                 'sql-hive-scrub-empty-input-output
                 nil t)
       (when sql-hive-format-results-as-org-table
         (add-hook 'comint-preoutput-filter-functions
                   'sql-hive-format-results-as-table t t)
         (setq sql-hive--resultset-partial nil) )
       (local-set-key (kbd "C-c f") 'sql-hive-toggle-format-results-as-org-table)
      )))

(defun sql-comint-hive (product options)
  "Create comint buffer and connect to Hive."
  (let ((params options))
    (sql-comint product params)))

;;;###autoload
(defun sql-hive (&optional buffer)
  "Run vsql as an inferior process."
  (interactive "P")
  (sql-product-interactive 'hive buffer))

(setq sql-product-alist
      (cons `(hive
              :name "Hive"
              :sqli-program sql-hive-program
              :sqli-options sql-hive-options
              :sqli-login sql-hive-login-params
              :sqli-comint-func sql-comint-hive
              :prompt-regexp ,(concat "^" sql-hive-prompt)
              :prompt-length 5
              :prompt-cont-regexp "^    > "
              :input-filter handle-empty-input-filter
              :font-lock sql-mode-hive-font-lock-keywords
              )
            sql-product-alist))

(provide 'hive)

;;; hive.el ends here
