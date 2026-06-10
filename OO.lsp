; OO Smart Offset v2.0
; Created by [AR. KISHAN S. LAKHANI]
; © 2025 — All rights reserved
; Do not redistribute without permission

;;; ============================================
;;; OO - SMART OFFSET FOR AUTOCAD
;;; Version: 2.0 (Standard Flow - Distance First)
;;; ============================================

;;; ----- GLOBAL VARIABLES -----
(setq *oo-temp-entities* nil)
(setq *oo-multiple-mode* nil)
(setq *oo-base-pt* nil)
(setq *oo-ref-mode* nil)
(setq *oo-ref-pt* nil)
(setq *oo-between-mode* nil)
(setq *oo-between-pt1* nil)
(setq *oo-between-pt2* nil)
(setq *oo-both-sides* nil)
(setq *oo-erase-original* nil)
(setq *oo-current-layer* nil)
(setq *oo-selected-ent* nil)
(setq *oo-last-distance* nil)
(if (not *ll-default-unit*) (setq *ll-default-unit* "mm"))

;;; ============================================
;;; UNIT FUNCTIONS
;;; ============================================
(defun oo:get-drawing-factor ()
  (cond
    ((= (getvar "INSUNITS") 1) (/ 1.0 25.4))
    ((= (getvar "INSUNITS") 2) (/ 1.0 304.8))
    ((= (getvar "INSUNITS") 4) 1.0)
    ((= (getvar "INSUNITS") 5) 0.1)
    ((= (getvar "INSUNITS") 6) 0.001)
    (T 1.0)))

(defun oo:get-drawing-name ()
  (cond
    ((= (getvar "INSUNITS") 1) "in")
    ((= (getvar "INSUNITS") 2) "ft")
    ((= (getvar "INSUNITS") 4) "mm")
    ((= (getvar "INSUNITS") 5) "cm")
    ((= (getvar "INSUNITS") 6) "m")
    (T "units")))

(defun oo:mm-to-drawing (mm-val)
  (* mm-val (oo:get-drawing-factor)))

(defun oo:drawing-to-mm (draw-val)
  (/ draw-val (oo:get-drawing-factor)))

(defun oo:format-dist (mm-val)
  (strcat (rtos (oo:mm-to-drawing mm-val) 2 4) " " (oo:get-drawing-name)))

(defun oo:get-unit-factor (unit-str)
  (cond
    ((= unit-str "mm") 1.0)
    ((= unit-str "\"") 25.4)
    ((= unit-str "in") 25.4)
    ((= unit-str "i") 25.4)
    ((= unit-str "'") 304.8)
    ((= unit-str "ft") 304.8)
    ((= unit-str "f") 304.8)
    ((= unit-str "cm") 10.0)
    ((= unit-str "c") 10.0)
    ((= unit-str "m") 1000.0)
    ((= unit-str "yd") 914.4)
    ((= unit-str "y") 914.4)
    ((= unit-str "[]") 1.0)
    (T nil)))

(defun oo:get-default-factor ()
  (cond
    ((= *ll-default-unit* "mm") 1.0)
    ((= *ll-default-unit* "cm") 10.0)
    ((= *ll-default-unit* "m") 1000.0)
    ((= *ll-default-unit* "in") 25.4)
    ((= *ll-default-unit* "ft") 304.8)
    (T 1.0)))

;;; ============================================
;;; PARSING FUNCTIONS
;;; ============================================
(defun oo:is-digit (ch)
  (member ch '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ".")))

(defun oo:is-unit-char (ch)
  (member ch '("\"" "'" "i" "f" "c" "m" "y" "[")))

(defun oo:is-operator (ch)
  (member ch '("+" "-" "*" "/")))

(defun oo:convert-fraction (frac-str / pos num denom)
  (setq pos (vl-string-search "/" frac-str))
  (if pos
    (progn
      (setq num (atof (substr frac-str 1 pos)))
      (setq denom (atof (substr frac-str (+ pos 2))))
      (if (and denom (/= denom 0)) (/ num denom) 0.0))
    (atof frac-str)))

(defun oo:preprocess-fractions (expr / result i len ch temp whole-part frac-part dash-pos)
  (setq result "" i 1 len (strlen expr) temp "")
  (while (<= i len)
    (setq ch (substr expr i 1))
    (cond
      ((member ch '("[" "]"))
       (setq result (strcat result ch) i (1+ i)))
      ((and (oo:is-digit ch) (= temp ""))
       (setq temp ch i (1+ i))
       (while (and (<= i len)
                   (or (oo:is-digit (substr expr i 1))
                       (member (substr expr i 1) '("-" "/"))))
         (setq temp (strcat temp (substr expr i 1)) i (1+ i)))
       (if (vl-string-search "/" temp)
         (progn
           (setq dash-pos (vl-string-search "-" temp))
           (if (and dash-pos (> dash-pos 0) (vl-string-search "/" (substr temp (+ dash-pos 2))))
             (progn
               (setq whole-part (atof (substr temp 1 dash-pos)))
               (setq frac-part (oo:convert-fraction (substr temp (+ dash-pos 2))))
               (setq result (strcat result (rtos (+ whole-part frac-part) 2 6))))
             (setq result (strcat result (rtos (oo:convert-fraction temp) 2 6)))))
         (setq result (strcat result temp)))
       (setq temp ""))
      (T (setq result (strcat result ch) i (1+ i)))))
  result)

(defun oo:preprocess (expr / result i len ch next-ch in-feet)
  (setq expr (vl-string-trim " " expr))
  (setq expr (oo:preprocess-fractions expr))
  (setq result "" i 1 len (strlen expr) in-feet nil)
  (while (<= i len)
    (setq ch (substr expr i 1))
    (setq next-ch (if (< i len) (substr expr (1+ i) 1) ""))
    (cond
      ((= ch "'")
       (setq result (strcat result ch) in-feet T)
       (cond
         ((= next-ch "-") (setq result (strcat result "+") i (+ i 2)))
         ((oo:is-digit next-ch) (setq result (strcat result "+") i (1+ i)))
         (T (setq i (1+ i)))))
      ((= ch "\"") (setq result (strcat result ch) in-feet nil i (1+ i)))
      (T (setq result (strcat result ch) i (1+ i)))))
  (if (and in-feet (> (strlen result) 0) (oo:is-digit (substr result (strlen result) 1)))
    (setq result (strcat result "\"")))
  result)

(defun oo:tokenize (expr / tokens i len ch num-start num-str unit-str is-percent)
  (setq expr (strcase expr T) tokens '() i 1 len (strlen expr))
  (while (<= i len)
    (setq ch (substr expr i 1))
    (cond
      ((= ch " ") (setq i (1+ i)))
      ((= ch "[")
       (setq i (1+ i) num-start i)
       (while (and (<= i len) (/= (substr expr i 1) "]")) (setq i (1+ i)))
       (setq num-str (substr expr num-start (- i num-start)) i (1+ i))
       (setq tokens (append tokens (list (list (atof num-str) "[]" nil)))))
      ((oo:is-digit ch)
       (setq num-start i is-percent nil)
       (while (and (<= i len) (oo:is-digit (substr expr i 1))) (setq i (1+ i)))
       (setq num-str (substr expr num-start (- i num-start)))
       (if (and (<= i len) (= (substr expr i 1) "%"))
         (progn
           (setq is-percent T i (1+ i))
           (setq tokens (append tokens (list (list (atof num-str) "%" T)))))
         (progn
           (setq unit-str nil)
           (if (and (<= i len) (oo:is-unit-char (substr expr i 1)))
             (progn
               (setq unit-str (substr expr i 1) i (1+ i))
               (if (and (<= i len) (member (substr expr i 1) '("n" "t" "m" "d")))
                 (setq unit-str (strcat unit-str (substr expr i 1)) i (1+ i)))))
           (setq tokens (append tokens (list (list (atof num-str) unit-str nil)))))))
      ((oo:is-operator ch) (setq tokens (append tokens (list ch)) i (1+ i)))
      (T (setq i (1+ i)))))
  tokens)

(defun oo:token-to-mm (token / num unit factor)
  (if (and token (= (type token) 'LIST))
    (progn
      (setq num (car token) unit (cadr token))
      (if unit
        (progn
          (setq factor (oo:get-unit-factor unit))
          (if factor (* num factor) (* num (oo:get-default-factor))))
        (* num (oo:get-default-factor))))
    0.0))

(defun oo:is-percent-token (token)
  (and (= (type token) 'LIST) (= (length token) 3) (caddr token)))

(defun oo:evaluate-tokens (tokens / new-tokens i count tok prev-val next-val result current-op val)
  (if (null tokens) 0.0
    (progn
      (setq new-tokens '() i 0 count (length tokens))
      (while (< i count)
        (setq tok (nth i tokens))
        (cond
          ((= tok "*")
           (if (and (> (length new-tokens) 0) (< (1+ i) count))
             (progn
               (setq prev-val (oo:token-to-mm (last new-tokens)))
               (setq next-val (oo:token-to-mm (nth (1+ i) tokens)))
               (setq result (* prev-val next-val))
               (setq new-tokens (reverse (cdr (reverse new-tokens))))
               (setq new-tokens (append new-tokens (list (list result "[]" nil))))
               (setq i (+ i 2)))
             (setq i (1+ i))))
          ((= tok "/")
           (if (and (> (length new-tokens) 0) (< (1+ i) count))
             (progn
               (setq prev-val (oo:token-to-mm (last new-tokens)))
               (setq next-val (oo:token-to-mm (nth (1+ i) tokens)))
               (if (/= next-val 0) (setq result (/ prev-val next-val)) (setq result 0.0))
               (setq new-tokens (reverse (cdr (reverse new-tokens))))
               (setq new-tokens (append new-tokens (list (list result "[]" nil))))
               (setq i (+ i 2)))
             (setq i (1+ i))))
          (T (setq new-tokens (append new-tokens (list tok)) i (1+ i)))))
      (setq tokens new-tokens result 0.0 current-op "+" i 0 count (length tokens))
      (while (< i count)
        (setq tok (nth i tokens))
        (cond
          ((= (type tok) 'STR) (setq current-op tok))
          ((= (type tok) 'LIST)
           (if (oo:is-percent-token tok)
             (setq val (* result (/ (car tok) 100.0)))
             (setq val (oo:token-to-mm tok)))
           (if (= current-op "+") (setq result (+ result val)) (setq result (- result val)))))
        (setq i (1+ i)))
      result)))

(defun oo:check-parens-balance (expr / i len ch depth)
  (setq depth 0 i 1 len (strlen expr))
  (while (and (<= i len) (>= depth 0))
    (setq ch (substr expr i 1))
    (if (= ch "(") (setq depth (1+ depth)))
    (if (= ch ")") (setq depth (1- depth)))
    (setq i (1+ i)))
  (= depth 0))

(defun oo:has-parens (expr)
  (or (vl-string-search "(" expr) (vl-string-search ")" expr)))

(defun oo:find-innermost-paren (expr / i len ch open-pos close-pos)
  (setq i 1 len (strlen expr) open-pos nil close-pos nil)
  (while (<= i len)
    (setq ch (substr expr i 1))
    (if (= ch "(") (setq open-pos i))
    (if (and (= ch ")") open-pos) (progn (setq close-pos i) (setq i (1+ len))))
    (setq i (1+ i)))
  (if (and open-pos close-pos) (list open-pos close-pos) nil))

(defun oo:evaluate-no-parens (expr / tokens processed)
  (if (and expr (/= expr ""))
    (progn
      (setq processed (oo:preprocess expr))
      (setq tokens (oo:tokenize processed))
      (if tokens (oo:evaluate-tokens tokens) nil))
    nil))

(defun oo:eval-parens (expr / paren-info start-pos end-pos inner-expr inner-result before-paren after-paren new-expr result-str)
  (if (or (null expr) (= expr "")) nil
    (if (not (oo:check-parens-balance expr)) nil
      (progn
        (setq paren-info (oo:find-innermost-paren expr))
        (if paren-info
          (progn
            (setq start-pos (car paren-info) end-pos (cadr paren-info))
            (setq inner-expr (substr expr (1+ start-pos) (- end-pos start-pos 1)))
            (if (or (null inner-expr) (= (vl-string-trim " " inner-expr) "")) nil
              (progn
                (setq inner-result (oo:eval-parens inner-expr))
                (if (and inner-result (numberp inner-result) (> inner-result 0))
                  (progn
                    (setq before-paren (if (> start-pos 1) (substr expr 1 (1- start-pos)) ""))
                    (setq after-paren (if (< end-pos (strlen expr)) (substr expr (1+ end-pos)) ""))
                    (setq result-str (rtos inner-result 2 8))
                    (setq new-expr (strcat before-paren "[" result-str "]" after-paren))
                    (oo:eval-parens new-expr))
                  nil))))
          (oo:evaluate-no-parens expr))))))

(defun oo:parse (expr)
  (if (and expr (/= expr ""))
    (if (oo:has-parens expr)
      (oo:eval-parens expr)
      (oo:evaluate-no-parens expr))
    nil))

;;; ============================================
;;; BETWEEN MODE PARSING
;;; ============================================
(defun oo:is-ratio (input / pos)
  (and input
       (setq pos (vl-string-search ":" input))
       pos (> pos 0) (< pos (1- (strlen input)))))

(defun oo:is-percentage (input)
  (and input (> (strlen input) 1) (= (substr input (strlen input) 1) "%")))

(defun oo:parse-ratio (input / pos num1 num2 total)
  (setq pos (vl-string-search ":" input))
  (if pos
    (progn
      (setq num1 (atof (substr input 1 pos)))
      (setq num2 (atof (substr input (+ pos 2))))
      (setq total (+ num1 num2))
      (if (> total 0) (/ num1 total) 0.5))
    0.5))

(defun oo:parse-percentage (input / num-str)
  (setq num-str (substr input 1 (1- (strlen input))))
  (/ (atof num-str) 100.0))

(defun oo:parse-between-input (input pt1 pt2 / total-dist fraction dist-mm)
  (setq total-dist (distance pt1 pt2))
  (cond
    ((or (null input) (= input "")) (setq fraction 0.5))
    ((oo:is-ratio input) (setq fraction (oo:parse-ratio input)))
    ((oo:is-percentage input) (setq fraction (oo:parse-percentage input)))
    (T
     (setq dist-mm (oo:parse input))
     (if (and dist-mm (> dist-mm 0))
       (progn
         (setq dist-mm (oo:mm-to-drawing dist-mm))
         (if (> total-dist 0)
           (setq fraction (/ dist-mm total-dist))
           (setq fraction 0.5)))
       (setq fraction nil))))
  (if (and fraction (>= fraction 0))
    (* total-dist fraction)
    nil))

(defun oo:get-between-display (input pt1 pt2 / total-dist fraction dist-val pct-val)
  (setq total-dist (distance pt1 pt2))
  (cond
    ((or (null input) (= input ""))
     (setq dist-val (* total-dist 0.5))
     (strcat "[Between] 50% = " (oo:format-dist (oo:drawing-to-mm dist-val))))
    ((oo:is-ratio input)
     (setq fraction (oo:parse-ratio input))
     (setq dist-val (* total-dist fraction))
     (strcat "[Between] " input " = " (rtos (* fraction 100) 2 1) "% = " 
             (oo:format-dist (oo:drawing-to-mm dist-val))))
    ((oo:is-percentage input)
     (setq fraction (oo:parse-percentage input))
     (setq dist-val (* total-dist fraction))
     (strcat "[Between] " input " = " (oo:format-dist (oo:drawing-to-mm dist-val))))
    (T
     (setq dist-mm (oo:parse input))
     (if (and dist-mm (> dist-mm 0))
       (progn
         (setq pct-val (/ (* (oo:mm-to-drawing dist-mm) 100.0) total-dist))
         (strcat "[Between] " input " = " (oo:format-dist dist-mm) " (" (rtos pct-val 2 1) "%)"))
       (strcat "[Between] " input " = ???")))))

;;; ============================================
;;; PARSE OFFSET INPUT
;;; ============================================
(defun oo:parse-input (input / pos-at pos-hash pos-g count-str dist-str
                               count dist-mm spacing-mm gap-mm result)
  (setq input (vl-string-trim " " input))
  (if (= input "") (list nil nil nil nil "EMPTY")
    (progn
      (setq pos-at (vl-string-search "@" input))
      (setq pos-hash (vl-string-search "#" input))
      (setq pos-g (vl-string-search "g" (strcase input T)))
      
      (cond
        (pos-at
         (setq count-str (substr input 1 pos-at))
         (setq count (atoi count-str))
         (setq dist-str (substr input (+ pos-at 2)))
         (setq spacing-mm (oo:parse dist-str))
         (if (and count (> count 0) spacing-mm (> spacing-mm 0))
           (list "SPACING" count spacing-mm nil nil)
           (list nil nil nil nil "ERROR")))
        
        (pos-hash
         (setq count-str (substr input 1 pos-hash))
         (setq count (atoi count-str))
         (setq dist-str (substr input (+ pos-hash 2)))
         (setq dist-mm (oo:parse dist-str))
         (if (and count (> count 1) dist-mm (> dist-mm 0))
           (progn
             (setq spacing-mm (/ dist-mm (- count 1)))
             (list "DIVIDE" count spacing-mm dist-mm nil))
           (list nil nil nil nil "ERROR")))
        
        (pos-g
         (setq dist-str (substr input 1 pos-g))
         (setq gap-str (substr input (+ pos-g 2)))
         (setq dist-mm (oo:parse dist-str))
         (setq gap-mm (oo:parse gap-str))
         (if (and dist-mm (> dist-mm 0))
           (if (and gap-mm (> gap-mm 0))
             (list "GAP" 2 dist-mm gap-mm nil)
             (list "SINGLE" 1 dist-mm nil nil))
           (list nil nil nil nil "ERROR")))
        
        (T
         (setq dist-mm (oo:parse input))
         (if (and dist-mm (> dist-mm 0))
           (list "SINGLE" 1 dist-mm nil nil)
           (list nil nil nil nil "ERROR")))))))

;;; ============================================
;;; CLEANUP
;;; ============================================
(defun oo:cleanup-all ()
  (foreach ent *oo-temp-entities*
    (if (and ent (entget ent)) (entdel ent)))
  (setq *oo-temp-entities* nil)
  (setq *oo-multiple-mode* nil)
  (setq *oo-base-pt* nil)
  (setq *oo-ref-mode* nil)
  (setq *oo-ref-pt* nil)
  (setq *oo-between-mode* nil)
  (setq *oo-between-pt1* nil)
  (setq *oo-between-pt2* nil)
  (setq *oo-both-sides* nil)
  (setq *oo-erase-original* nil)
  (setq *oo-current-layer* nil)
  (setq *oo-selected-ent* nil)
  (redraw))

(defun oo:add-temp-entity (ent)
  (if ent (setq *oo-temp-entities* (cons ent *oo-temp-entities*))))

(defun oo:reset-ref-mode ()
  (setq *oo-ref-mode* nil *oo-ref-pt* nil))

(defun oo:reset-between-mode ()
  (setq *oo-between-mode* nil *oo-between-pt1* nil *oo-between-pt2* nil))

(defun oo:reset-all-modes ()
  (oo:reset-ref-mode)
  (oo:reset-between-mode)
  (setq *oo-both-sides* nil))

;;; ============================================
;;; OSNAP HELPER
;;; ============================================
(defun oo:enable-osnap (/ current-osmode)
  (setq current-osmode (getvar "OSMODE"))
  (if (= (logand current-osmode 16384) 16384)
    (setvar "OSMODE" (- current-osmode 16384)))
  current-osmode)

;;; ============================================
;;; ERROR HANDLER
;;; ============================================
(defun oo:error-handler (msg)
  (if (not (member msg '("Function cancelled" "quit / exit abort")))
    (princ (strcat "\nOO Error: " msg)))
  (oo:cleanup-all)
  (if *oo-old-cmdecho* (setvar "CMDECHO" *oo-old-cmdecho*))
  (if *oo-old-highlight* (setvar "HIGHLIGHT" *oo-old-highlight*))
  (if *oo-old-osmode* (setvar "OSMODE" *oo-old-osmode*))
  (princ))

;;; ============================================
;;; DISPLAY FUNCTIONS
;;; ============================================
(defun oo:get-text-height ()
  (/ (getvar "VIEWSIZE") 35.0))

(defun oo:delete-display ()
  (foreach ent *oo-temp-entities*
    (if (and ent (entget ent)) (entdel ent)))
  (setq *oo-temp-entities* nil))

(defun oo:create-display (pt txt / th pad tw bw bh bx by p1 p2 p3 p4 tx ty solid-ent border-ent text-ent)
  (oo:delete-display)
  (if (and pt txt (/= txt ""))
    (progn
      (setq th (oo:get-text-height) pad (* th 0.5))
      (setq tw (* (strlen txt) th 0.75 1.2))
      (setq bw (+ tw (* pad 2)))
      (setq bh (+ th (* pad 1.8)))
      (setq bx (+ (car pt) (* th 0.5)))
      (setq by (+ (cadr pt) (* th 1.0)))
      (setq p1 (list bx by 0.0))
      (setq p2 (list (+ bx bw) by 0.0))
      (setq p3 (list (+ bx bw) (+ by bh) 0.0))
      (setq p4 (list bx (+ by bh) 0.0))
      (setq tx (+ bx pad))
      (setq ty (+ by (* pad 0.8)))
      
      (setq solid-ent (entmakex (list '(0 . "SOLID") '(100 . "AcDbEntity") '(100 . "AcDbTrace")
                                      '(62 . 2) (cons 10 p1) (cons 11 p2) (cons 12 p4) (cons 13 p3))))
      (oo:add-temp-entity solid-ent)
      
      (setq border-ent (entmakex (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                       '(62 . 1) '(90 . 4) '(70 . 1) (cons 43 (* th 0.05))
                                       (cons 10 (list bx by)) (cons 10 (list (+ bx bw) by))
                                       (cons 10 (list (+ bx bw) (+ by bh))) (cons 10 (list bx (+ by bh))))))
      (oo:add-temp-entity border-ent)
      
      (setq text-ent (entmakex (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                                     '(62 . 1) (cons 10 (list tx ty 0.0)) (cons 11 (list tx ty 0.0))
                                     (cons 40 th) (cons 1 txt) '(50 . 0.0) '(72 . 0) '(73 . 0) '(100 . "AcDbText"))))
      (oo:add-temp-entity text-ent))))

;;; ============================================
;;; DRAW MARKER
;;; ============================================
(defun oo:draw-marker (pt color / size)
  (if pt
    (progn
      (setq size (* (/ (getvar "VIEWSIZE") 100.0) 1.5))
      (grdraw (list (- (car pt) size) (cadr pt) 0) 
              (list (+ (car pt) size) (cadr pt) 0) color 0)
      (grdraw (list (car pt) (- (cadr pt) size) 0) 
              (list (car pt) (+ (cadr pt) size) 0) color 0))))

;;; ============================================
;;; BUILD DISPLAY STRING
;;; ============================================
(defun oo:build-display (typed-str parsed-result / mode count dist gap prefix)
  (setq mode (car parsed-result) count (cadr parsed-result) dist (caddr parsed-result) gap (cadddr parsed-result))
  (setq prefix "")
  (if *oo-multiple-mode* (setq prefix (strcat prefix "[Multiple]")))
  (if *oo-ref-mode* (setq prefix (strcat prefix "[Reference]")))
  (if *oo-between-mode* (setq prefix (strcat prefix "[Between]")))
  (if *oo-both-sides* (setq prefix (strcat prefix "[BothSides]")))
  (if *oo-erase-original* (setq prefix (strcat prefix "[Erase]")))
  (if *oo-current-layer* (setq prefix (strcat prefix "[CurrentLayer]")))
  (if (/= prefix "") (setq prefix (strcat prefix " ")))
  
  (cond
    ((null mode) (strcat prefix typed-str " = ???"))
    ((= mode "SINGLE")
     (strcat prefix typed-str " = " (oo:format-dist dist)))
    ((= mode "SPACING")
     (strcat prefix typed-str " = " (itoa count) "x @ " (oo:format-dist dist)))
    ((= mode "DIVIDE")
     (strcat prefix typed-str " = " (itoa count) " in " (oo:format-dist gap)))
    ((= mode "GAP")
     (strcat prefix typed-str " = " (oo:format-dist dist) " + " (oo:format-dist gap) " gap"))
    (T (strcat prefix typed-str))))

;;; ============================================
;;; EXECUTE OFFSET FUNCTIONS
;;; ============================================
(defun oo:execute-single (ent dist-drawing side-pt / result)
  (setq result (vl-cmdf "_.OFFSET" dist-drawing ent side-pt ""))
  (if *oo-erase-original*
    (progn
      (entdel ent)
      (princ (strcat "\n  >> OFFSET " (oo:format-dist (oo:drawing-to-mm dist-drawing)) " (original erased)")))
    (princ (strcat "\n  >> OFFSET " (oo:format-dist (oo:drawing-to-mm dist-drawing)))))
  (setq *oo-last-distance* (oo:drawing-to-mm dist-drawing))
  result)

(defun oo:execute-both-sides (ent dist-drawing side-pt / ent-data ent-type p1 p2 center other-side)
  (vl-cmdf "_.OFFSET" dist-drawing ent side-pt "")
  (setq ent-data (entget ent))
  (setq ent-type (cdr (assoc 0 ent-data)))
  (cond
    ((= ent-type "LINE")
     (setq p1 (cdr (assoc 10 ent-data)))
     (setq p2 (cdr (assoc 11 ent-data)))
     (setq center (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
     (setq other-side (polar center (+ (angle center side-pt) pi) (* dist-drawing 2))))
    ((member ent-type '("CIRCLE" "ARC"))
     (setq center (cdr (assoc 10 ent-data)))
     (setq other-side (polar center (+ (angle center side-pt) pi) (* dist-drawing 2))))
    (T (setq other-side nil)))
  (if other-side
    (vl-cmdf "_.OFFSET" dist-drawing ent other-side ""))
  (if *oo-erase-original*
    (progn
      (entdel ent)
      (princ (strcat "\n  >> OFFSET BOTH " (oo:format-dist (oo:drawing-to-mm dist-drawing)) " (original erased)")))
    (princ (strcat "\n  >> OFFSET BOTH " (oo:format-dist (oo:drawing-to-mm dist-drawing)))))
  (setq *oo-last-distance* (oo:drawing-to-mm dist-drawing)))

(defun oo:execute-array (ent spacing-drawing count side-pt / i dist)
  (princ (strcat "\n  >> OFFSET " (itoa count) "x @ " (oo:format-dist (oo:drawing-to-mm spacing-drawing)) "..."))
  (setq i 1)
  (while (<= i count)
    (setq dist (* spacing-drawing i))
    (vl-cmdf "_.OFFSET" dist ent side-pt "")
    (setq i (1+ i)))
  (princ " Done!")
  (setq *oo-last-distance* (oo:drawing-to-mm spacing-drawing)))

(defun oo:execute-gap (ent dist-drawing gap-drawing side-pt / total-dist)
  (vl-cmdf "_.OFFSET" dist-drawing ent side-pt "")
  (setq total-dist (+ dist-drawing gap-drawing))
  (vl-cmdf "_.OFFSET" total-dist ent side-pt "")
  (princ (strcat "\n  >> OFFSET " (oo:format-dist (oo:drawing-to-mm dist-drawing)) 
                 " + " (oo:format-dist (oo:drawing-to-mm gap-drawing)) " gap"))
  (setq *oo-last-distance* (oo:drawing-to-mm dist-drawing)))

;;; ============================================
;;; THROUGH POINT OFFSET
;;; ============================================
(defun oo:offset-through-point (ent / through-pt)
  (oo:delete-display)
  (redraw)
  (princ "\n  [Through Point]")
  (setq through-pt (getpoint "\n  Pick point to offset through: "))
  (if (null through-pt)
    (progn (princ "\n  Cancelled.") nil)
    (progn
      (vl-cmdf "_.OFFSET" "T" ent through-pt "")
      (princ "\n  >> OFFSET through point!")
      T)))

;;; ============================================
;;; SNAP DISTANCE
;;; ============================================
(defun oo:snap-distance (/ pt1 pt2 dist-drawing)
  (oo:delete-display)
  (redraw)
  (princ "\n  [Snap Distance] Pick two points to measure distance")
  (setq pt1 (getpoint "\n  First point: "))
  (if (null pt1)
    (progn (princ "\n  Cancelled.") nil)
    (progn
      (setq pt2 (getpoint pt1 "\n  Second point: "))
      (if (null pt2)
        (progn (princ "\n  Cancelled.") nil)
        (progn
          (setq dist-drawing (distance pt1 pt2))
          (princ (strcat "\n  Distance: " (oo:format-dist (oo:drawing-to-mm dist-drawing))))
          dist-drawing)))))

;;; ============================================
;;; PROMPT STRINGS (Full Names)
;;; ============================================
(defun oo:main-prompt ()
  "\nOffset distance [Snap/Reference/Between/Through/Qboth/Erase/Layer/Multiple/X-exit]: ")

(defun oo:select-prompt ()
  "\nSelect object to offset [or X-exit]: ")

(defun oo:side-prompt ()
  "\nPick side to offset [or X-exit]: ")

;;; ============================================
;;; OO - MAIN COMMAND (STANDARD FLOW)
;;; Distance FIRST -> Select Object -> Pick Side
;;; ============================================
(defun c:OO (/ ent ent-sel base-pt typed-str grdata code data cursor-pt
               dist-mm dist-drawing side-pt loop-input display-str
               parsed-result mode count spacing gap offset-done
               snap-dist phase got-distance got-object
               *error* *oo-old-cmdecho* *oo-old-highlight* *oo-old-osmode*)
  
  (setq *error* oo:error-handler)
  (setq *oo-old-cmdecho* (getvar "CMDECHO"))
  (setq *oo-old-highlight* (getvar "HIGHLIGHT"))
  (setq *oo-old-osmode* (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "HIGHLIGHT" 1)
  
  (oo:enable-osnap)
  (oo:cleanup-all)
  (setq offset-done nil)
  
  ;; Header
  (princ "\n")
  (princ "\n+===========================================================+")
  (princ "\n|  OO v2.0 - SMART OFFSET (Standard Flow)                   |")
  (princ (strcat "\n|  Units: " (oo:get-drawing-name) " | Default: " *ll-default-unit* "                                 |"))
  (princ "\n+===========================================================+")
  (princ "\n|  5@100=Spacing  5#500=Divide  100g50=Gap                  |")
  (princ "\n|  S=Snap  R=Reference  B=Between  T=Through                |")
  (princ "\n|  Q=BothSides  E=Erase  L=Layer  M=Multiple  X=Exit       |")
  (princ "\n+===========================================================+")
  (princ "\n")
  
  ;; ===== PHASE 1: GET DISTANCE =====
  (setq typed-str "" cursor-pt (getvar "LASTPOINT") got-distance nil)
  (setq phase "DISTANCE")
  (princ (oo:main-prompt))
  
  (while (and (not offset-done) (= phase "DISTANCE"))
    (setq grdata (grread T 15 0) code (car grdata) data (cadr grdata))
    
    (cond
      ;; MOUSE MOVE - show live preview
      ((= code 5)
       (setq cursor-pt data)
       (cond
         (*oo-between-mode*
          (redraw)
          (oo:draw-marker *oo-between-pt1* 1)
          (oo:draw-marker *oo-between-pt2* 1)
          (grdraw *oo-between-pt1* *oo-between-pt2* 5 1)
          (if (and typed-str (/= typed-str ""))
            (oo:create-display cursor-pt (oo:get-between-display typed-str *oo-between-pt1* *oo-between-pt2*))
            (oo:create-display cursor-pt "[Between] ENTER=50% | 25% | 100mm | 1:2")))
         
         (*oo-ref-mode*
          (if (and typed-str (/= typed-str ""))
            (progn
              (setq parsed-result (oo:parse-input typed-str))
              (if (car parsed-result)
                (oo:create-display cursor-pt (oo:build-display typed-str parsed-result))
                (oo:create-display cursor-pt (strcat "[Reference] " typed-str " = ???"))))
            (oo:delete-display))
          (redraw)
          (oo:draw-marker *oo-ref-pt* 1))
         
         (T
          (if (and typed-str (/= typed-str ""))
            (progn
              (setq parsed-result (oo:parse-input typed-str))
              (oo:create-display cursor-pt (oo:build-display typed-str parsed-result)))
            (oo:delete-display))
          (redraw))))
      
      ;; MOUSE CLICK in distance phase - ignore (need typed distance)
      ((= code 3)
       (setq cursor-pt data))
      
      ;; KEYBOARD
      ((= code 2)
       (cond
         ;; B - Between mode
         ((and (member data '(66 98)) (= typed-str "") (not *oo-between-mode*) (not *oo-ref-mode*))
          (oo:delete-display)
          (oo:reset-all-modes)
          (redraw)
          (princ "\n  [Between Mode]")
          (princ "\n  Pick FIRST point (A): ")
          (setq *oo-between-pt1* (getpoint))
          (if *oo-between-pt1*
            (progn
              (princ "\n  Pick SECOND point (B): ")
              (setq *oo-between-pt2* (getpoint *oo-between-pt1*))
              (if *oo-between-pt2*
                (progn
                  (setq *oo-between-mode* T)
                  (princ (strcat "\n  Distance A-B: " (oo:format-dist (oo:drawing-to-mm (distance *oo-between-pt1* *oo-between-pt2*)))))
                  (princ "\nPosition [ENTER=50% | 25% | 100mm | 1:2]: "))
                (progn
                  (princ "\n  Cancelled.")
                  (oo:reset-between-mode)
                  (princ (oo:main-prompt)))))
            (progn
              (princ "\n  Cancelled.")
              (oo:reset-between-mode)
              (princ (oo:main-prompt)))))
         
         ;; R - Reference mode
         ((and (member data '(82 114)) (= typed-str "") (not *oo-ref-mode*) (not *oo-between-mode*))
          (oo:delete-display)
          (oo:reset-all-modes)
          (redraw)
          (princ "\n  Pick REFERENCE point (offset distance FROM here): ")
          (setq *oo-ref-pt* (getpoint))
          (if *oo-ref-pt*
            (progn
              (setq *oo-ref-mode* T)
              (princ "\n  [Reference Mode] Type distance from reference point.")
              (princ "\nDistance from reference [Z=Cancel]: "))
            (progn
              (princ "\n  Cancelled.")
              (oo:reset-ref-mode)
              (princ (oo:main-prompt)))))
         
         ;; Z - Cancel mode
         ((and (member data '(90 122)) (= typed-str "") (or *oo-ref-mode* *oo-between-mode*))
          (oo:reset-all-modes)
          (oo:delete-display)
          (redraw)
          (princ "\n  [Mode Cancelled]")
          (princ (oo:main-prompt)))
         
         ;; S - Snap distance
         ((and (member data '(83 115)) (= typed-str "") (not *oo-ref-mode*) (not *oo-between-mode*))
          (setq snap-dist (oo:snap-distance))
          (if snap-dist
            (progn
              (setq typed-str (rtos (oo:drawing-to-mm snap-dist) 2 2))
              (princ (strcat "\n  Snap Distance: " typed-str "mm"))
              (princ "\n  Press ENTER to confirm or keep typing: "))
            (princ (oo:main-prompt))))
         
         ;; T - Through point (special - needs object first)
         ((and (member data '(84 116)) (= typed-str "") (not *oo-ref-mode*) (not *oo-between-mode*))
          (oo:delete-display)
          (redraw)
          (princ "\n  [Through Point] Select object first: ")
          (setq ent-sel (entsel))
          (if ent-sel
            (progn
              (setq ent (car ent-sel))
              (if (oo:offset-through-point ent)
                (progn
                  (setq offset-done T)
                  (setq phase "DONE"))
                (princ (oo:main-prompt))))
            (progn
              (princ "\n  Nothing selected.")
              (princ (oo:main-prompt)))))
         
         ;; Q - Both sides toggle
         ((and (member data '(81 113)) (= typed-str ""))
          (setq *oo-both-sides* (not *oo-both-sides*))
          (princ (if *oo-both-sides* "\n  >> BOTH SIDES mode ON" "\n  >> SINGLE SIDE mode")))
         
         ;; E - Erase original toggle
         ((and (member data '(69 101)) (= typed-str ""))
          (setq *oo-erase-original* (not *oo-erase-original*))
          (princ (if *oo-erase-original* "\n  >> ERASE original ON" "\n  >> KEEP original")))
         
         ;; L - Current layer toggle
         ((and (member data '(76 108)) (= typed-str ""))
          (setq *oo-current-layer* (not *oo-current-layer*))
          (if *oo-current-layer*
            (progn (setvar "OFFSETGAPTYPE" 0) (princ "\n  >> CURRENT LAYER mode ON"))
            (princ "\n  >> SOURCE LAYER mode")))
         
         ;; M - Multiple mode
         ((and (member data '(77 109)) (= typed-str ""))
          (setq *oo-multiple-mode* (not *oo-multiple-mode*))
          (princ (if *oo-multiple-mode* "\n  >> MULTIPLE mode ON (keep offsetting)" "\n  >> SINGLE mode")))
         
         ;; X - Exit
         ((and (member data '(88 120)) (= typed-str ""))
          (oo:reset-all-modes)
          (oo:delete-display)
          (redraw)
          (setq offset-done T phase "DONE"))
         
         ;; ENTER - confirm distance
         ((= data 13)
          (oo:delete-display)
          (cond
            ;; Use last distance if empty
            ((and (= typed-str "") *oo-last-distance* (not *oo-between-mode*) (not *oo-ref-mode*))
             (setq typed-str (rtos *oo-last-distance* 2 2))
             (princ (strcat "\n  Using last distance: " (oo:format-dist *oo-last-distance*)))
             (setq got-distance T phase "SELECT"))
            
            ;; Between mode - confirm
            (*oo-between-mode*
             (setq dist-drawing (oo:parse-between-input typed-str *oo-between-pt1* *oo-between-pt2*))
             (if dist-drawing
               (progn
                 (princ (strcat "\n  Between distance: " (oo:format-dist (oo:drawing-to-mm dist-drawing))))
                 (setq got-distance T phase "SELECT"))
               (progn
                 (princ "\n  ERROR: Invalid between input.")
                 (princ "\nPosition [ENTER=50% | 25% | 100mm | 1:2]: "))))
            
            ;; Reference mode - confirm
            (*oo-ref-mode*
             (if (and typed-str (/= typed-str ""))
               (progn
                 (setq parsed-result (oo:parse-input typed-str))
                 (if (car parsed-result)
                   (progn
                     (princ (strcat "\n  Reference distance confirmed: " typed-str))
                     (setq got-distance T phase "SELECT"))
                   (progn
                     (princ (strcat "\n  ERROR: \"" typed-str "\""))
                     (princ "\nDistance from reference [Z=Cancel]: "))))
               (progn
                 (oo:reset-ref-mode)
                 (redraw)
                 (setq offset-done T phase "DONE"))))
            
            ;; Standard - confirm typed distance
            (T
             (if (and typed-str (/= typed-str ""))
               (progn
                 (setq parsed-result (oo:parse-input typed-str))
                 (if (car parsed-result)
                   (progn
                     (princ (strcat "\n  Distance confirmed: " (oo:build-display typed-str parsed-result)))
                     (setq got-distance T phase "SELECT"))
                   (progn
                     (princ (strcat "\n  ERROR: \"" typed-str "\""))
                     (princ (oo:main-prompt)))))
               (progn
                 (redraw)
                 (setq offset-done T phase "DONE"))))))
         
         ;; BACKSPACE
         ((= data 8)
          (if (> (strlen typed-str) 0)
            (setq typed-str (substr typed-str 1 (1- (strlen typed-str)))))
          (if (/= typed-str "")
            (cond
              (*oo-between-mode*
               (oo:create-display cursor-pt (oo:get-between-display typed-str *oo-between-pt1* *oo-between-pt2*)))
              (T
               (setq parsed-result (oo:parse-input typed-str))
               (oo:create-display cursor-pt (oo:build-display typed-str parsed-result))))
            (oo:delete-display)))
         
         ;; ESCAPE
         ((= data 27)
          (oo:delete-display)
          (oo:reset-all-modes)
          (redraw)
          (setq offset-done T phase "DONE"))
         
         ;; OTHER KEYS - type distance
         ((and (>= data 32) (<= data 126))
          (setq typed-str (strcat typed-str (chr data)))
          (cond
            (*oo-between-mode*
             (oo:create-display cursor-pt (oo:get-between-display typed-str *oo-between-pt1* *oo-between-pt2*)))
            (T
             (setq parsed-result (oo:parse-input typed-str))
             (oo:create-display cursor-pt (oo:build-display typed-str parsed-result)))))))))
  
  ;; ===== PHASE 2: SELECT OBJECT & PICK SIDE (LOOP) =====
  (while (and (= phase "SELECT") (not offset-done))
    (oo:delete-display)
    (redraw)
    (princ (oo:select-prompt))
    (setq ent-sel (entsel))
    
    (if (null ent-sel)
      (progn
        (princ "\n  Nothing selected. Exiting.")
        (setq offset-done T))
      (progn
        (setq ent (car ent-sel))
        (setq *oo-selected-ent* ent)
        (princ "\n  Object selected. Pick side to offset:")
        
        ;; ===== PHASE 3: PICK SIDE =====
        (setq got-object T)
        (while got-object
          (setq grdata (grread T 15 0) code (car grdata) data (cadr grdata))
          
          (cond
            ;; MOUSE MOVE
            ((= code 5)
             (setq cursor-pt data)
             (cond
               (*oo-between-mode*
                (setq dist-drawing (oo:parse-between-input typed-str *oo-between-pt1* *oo-between-pt2*))
                (if (null dist-drawing)
                  (setq dist-drawing (oo:parse-between-input "" *oo-between-pt1* *oo-between-pt2*)))
                (redraw)
                (oo:draw-marker *oo-between-pt1* 1)
                (oo:draw-marker *oo-between-pt2* 1)
                (grdraw *oo-between-pt1* *oo-between-pt2* 5 1)
                (oo:create-display cursor-pt (oo:get-between-display typed-str *oo-between-pt1* *oo-between-pt2*)))
               (T
                (setq parsed-result (oo:parse-input typed-str))
                (oo:create-display cursor-pt (oo:build-display typed-str parsed-result))
                (redraw))))
            
            ;; MOUSE CLICK - execute offset
            ((= code 3)
             (oo:delete-display)
             (setq cursor-pt data)
             (setq side-pt cursor-pt)
             
             (cond
               ;; BETWEEN MODE
               (*oo-between-mode*
                (setq dist-drawing (oo:parse-between-input typed-str *oo-between-pt1* *oo-between-pt2*))
                (if dist-drawing
                  (progn
                    (if *oo-both-sides*
                      (oo:execute-both-sides ent dist-drawing side-pt)
                      (oo:execute-single ent dist-drawing side-pt))
                    (setq got-object nil)
                    (if (not *oo-multiple-mode*)
                      (setq offset-done T)
                      (princ "\n  [Multiple Mode] Select next object...")))
                  (princ "\n  ERROR: Invalid between input")))
               
               ;; REFERENCE MODE
               (*oo-ref-mode*
                (setq parsed-result (oo:parse-input typed-str))
                (setq mode (car parsed-result) dist-mm (caddr parsed-result))
                (if (and mode dist-mm)
                  (progn
                    (setq dist-drawing (oo:mm-to-drawing dist-mm))
                    (if *oo-both-sides*
                      (oo:execute-both-sides ent dist-drawing side-pt)
                      (oo:execute-single ent dist-drawing side-pt))
                    (setq got-object nil)
                    (if (not *oo-multiple-mode*)
                      (setq offset-done T)
                      (princ "\n  [Multiple Mode] Select next object...")))
                  (princ (strcat "\n  ERROR: \"" typed-str "\""))))
               
               ;; STANDARD MODE
               (T
                (setq parsed-result (oo:parse-input typed-str))
                (setq mode (car parsed-result) count (cadr parsed-result)
                      spacing (caddr parsed-result) gap (cadddr parsed-result))
                (if mode
                  (progn
                    (setq dist-drawing (oo:mm-to-drawing spacing))
                    (redraw)
                    (cond
                      ((= mode "SINGLE")
                       (if *oo-both-sides*
                         (oo:execute-both-sides ent dist-drawing side-pt)
                         (oo:execute-single ent dist-drawing side-pt)))
                      ((or (= mode "SPACING") (= mode "DIVIDE"))
                       (oo:execute-array ent dist-drawing count side-pt))
                      ((= mode "GAP")
                       (oo:execute-gap ent dist-drawing (oo:mm-to-drawing gap) side-pt)))
                    (setq got-object nil)
                    (if (not *oo-multiple-mode*)
                      (setq offset-done T)
                      (princ "\n  [Multiple Mode] Select next object...")))
                  (princ (strcat "\n  ERROR: \"" typed-str "\""))))))
            
            ;; KEYBOARD in side-pick phase
            ((= code 2)
             (cond
               ;; ESCAPE or X - cancel
               ((or (= data 27) (and (member data '(88 120))))
                (oo:delete-display)
                (redraw)
                (setq got-object nil offset-done T))))))))))
  
  (oo:cleanup-all)
  (oo:reset-all-modes)
  (setvar "CMDECHO" *oo-old-cmdecho*)
  (setvar "HIGHLIGHT" *oo-old-highlight*)
  (setvar "OSMODE" *oo-old-osmode*)
  (princ))

;;; ============================================
;;; OOX - CLEANUP
;;; ============================================
(defun c:OOX ()
  (princ "\n  Cleaning...")
  (oo:cleanup-all)
  (command "_.REGEN")
  (princ " Done!")
  (princ))

;;; ============================================
;;; OOH - HELP
;;; ============================================
(defun c:OOH ()
  (princ "\n")
  (princ "\n  ╔════════════════════════════════════════════════════════════╗")
  (princ "\n  ║  OO v2.0 - SMART OFFSET (Standard Flow)                  ║")
  (princ "\n  ╠════════════════════════════════════════════════════════════╣")
  (princ "\n  ║                                                          ║")
  (princ "\n  ║  FLOW: Enter Distance -> Select Object -> Pick Side      ║")
  (princ "\n  ║        (Same as standard AutoCAD OFFSET!)                ║")
  (princ "\n  ║                                                          ║")
  (princ "\n  ║  BASIC: 100 | 100mm | 5\" | 2'-6\" | (100+50)*2           ║")
  (princ "\n  ║                                                          ║")
  (princ "\n  ║  ARRAY:                                                  ║")
  (princ "\n  ║    5@100     5 offsets, 100mm spacing                    ║")
  (princ "\n  ║    5#500     5 offsets within 500mm                      ║")
  (princ "\n  ║                                                          ║")
  (princ "\n  ║  GAP:                                                    ║")
  (princ "\n  ║    100g50    100mm offset + 50mm gap + line              ║")
  (princ "\n  ║              (wall + cavity!)                            ║")
  (princ "\n  ║                                                          ║")
  (princ "\n  ╟────────────────────────────────────────────────────────────╢")
  (princ "\n  ║  KEYS (press before typing distance):                    ║")
  (princ "\n  ║                                                          ║")
  (princ "\n  ║    S = Snap       Pick two points to measure distance    ║")
  (princ "\n  ║    R = Reference  Offset distance FROM a picked point    ║")
  (princ "\n  ║    B = Between    Offset between two picked points       ║")
  (princ "\n  ║    T = Through    Offset through a picked point          ║")
  (princ "\n  ║    Q = BothSides  Toggle offset to both sides            ║")
  (princ "\n  ║    E = Erase      Toggle erase original after offset     ║")
  (princ "\n  ║    L = Layer      Toggle use current layer               ║")
  (princ "\n  ║    M = Multiple   Toggle keep offsetting more objects    ║")
  (princ "\n  ║    Z = Cancel     Cancel Reference/Between mode          ║")
  (princ "\n  ║    X = Exit       Exit command                           ║")
  (princ "\n  ║                                                          ║")
  (princ "\n  ╚════════════════════════════════════════════════════════════╝")
  (princ "\n")
  (princ))

;;; ============================================
;;; LOAD MESSAGE
;;; ============================================
(princ "\n")
(princ "\n  ╔════════════════════════════════════════════════════════════╗")
(princ "\n  ║  OO v2.0 - SMART OFFSET LOADED!                          ║")
(princ "\n  ╠════════════════════════════════════════════════════════════╣")
(princ "\n  ║                                                          ║")
(princ "\n  ║  FLOW: Distance -> Select Object -> Pick Side            ║")
(princ "\n  ║        (Same as standard AutoCAD OFFSET!)                ║")
(princ "\n  ║                                                          ║")
(princ "\n  ║  KEYS:                                                   ║")
(princ "\n  ║    S = Snap         R = Reference    B = Between         ║")
(princ "\n  ║    T = Through      Q = BothSides    E = Erase           ║")
(princ "\n  ║    L = Layer        M = Multiple     X = Exit            ║")
(princ "\n  ║                                                          ║")
(princ "\n  ║  SPECIAL: 5@100=Array | 5#500=Divide | 100g50=Gap        ║")
(princ "\n  ╠════════════════════════════════════════════════════════════╣")
(princ "\n  ║  Commands: OO  OOX  OOH                                  ║")
(princ "\n  ╚════════════════════════════════════════════════════════════╝")
(princ "\n")
(princ)