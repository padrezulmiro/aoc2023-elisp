;;; day2.el --- Solution for AOC 2023, day 2 -*- lexical-binding: t; -*-

(defun aoc-day2-main ()
  (interactive)
  (let* ((input-path (f-join (f-dirname buffer-file-name) "input_day2.txt"))
         (input-lines (butlast (s-lines (f-read-text input-path))))
         (configuration (make-hash-table :test 'equal))
         (sum 0))

    (puthash "red" 12 configuration)
    (puthash "green" 13 configuration)
    (puthash "blue" 14 configuration)

    (dolist (line input-lines)
      (setq sum (+ sum (aoc-day2-power-cube-set (aoc-day2-get-minset line)))))

    (message "Advent of Code 2023: The sum was %d" sum)))

(defun aoc-day2-parse-game (line config)
  (let* ((id)
         (n-cubes)
         (start 0)
         (pattern-id "\\(?:Game \\)\\([[:digit:]]+\\)\\(?::\\)")
         (pattern-r "\\([[:digit:]]+\\)\\(?: red\\)")
         (pattern-g "\\([[:digit:]]+\\)\\(?: green\\)")
         (pattern-b "\\([[:digit:]]+\\)\\(?: blue\\)"))

    (string-match pattern-id line)
    (setq id (string-to-number (match-string 1 line)))

    (while (and (/= id 0) (string-match pattern-r line start))
      (setq n-cubes (string-to-number (match-string 1 line)))
      (when (> n-cubes (gethash "red" config))
        (setq id 0))
      (setq start (match-end 0)))

    (setq start 0)
    (while (and (/= id 0) (string-match pattern-g line start))
      (setq n-cubes (string-to-number (match-string 1 line)))
      (when (> n-cubes (gethash "green" config))
        (setq id 0))
      (setq start (match-end 0)))

    (setq start 0)
    (while (and (/= id 0) (string-match pattern-b line start))
      (setq n-cubes (string-to-number (match-string 1 line)))
      (when (> n-cubes (gethash "blue" config))
        (setq id 0))
      (setq start (match-end 0)))

    id))

(defun aoc-day2-get-minset (line)
  (let* ((minset (make-vector 3 0))
         (start 0)
         (pattern-r "\\([[:digit:]]+\\)\\(?: red\\)")
         (pattern-g "\\([[:digit:]]+\\)\\(?: green\\)")
         (pattern-b "\\([[:digit:]]+\\)\\(?: blue\\)"))

    (while (string-match pattern-r line start)
      (when (> (string-to-number (match-string 1 line)) (aref minset 0))
        (aset minset 0 (string-to-number (match-string 1 line))))
      (setq start (match-end 0)))

    (setq start 0)
    (while (string-match pattern-g line start)
      (when (> (string-to-number (match-string 1 line)) (aref minset 1))
        (aset minset 1 (string-to-number (match-string 1 line))))
      (setq start (match-end 0)))

    (setq start 0)
    (while (string-match pattern-b line start)
      (when (> (string-to-number (match-string 1 line)) (aref minset 2))
        (aset minset 2 (string-to-number (match-string 1 line))))
      (setq start (match-end 0)))

    minset))

(defun aoc-day2-power-cube-set (set)
  (seq-reduce #'* set 1))
