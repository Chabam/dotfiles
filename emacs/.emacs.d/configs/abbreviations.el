(abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:_].*\\|.*\\)")
(define-abbrev global-abbrev-table ":github:" "git@github.com:chabam/")

(provide 'abbreviations)
