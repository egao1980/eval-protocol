(in-package #:eval-protocol)

;;; LLM-as-judge. Core stays LLM-free; this subsystem depends on llm-protocol.
;;; Structured output is a JSON Schema hash-table (no schema-protocol).

(defclass llm-judge-scorer (eval-scorer)
  ((backend :initarg :backend :accessor llm-judge-scorer-backend :initform nil)
   (prompt :initarg :prompt :accessor llm-judge-scorer-prompt :initform nil)))

(defun make-llm-judge-scorer (&key backend prompt)
  (make-instance 'llm-judge-scorer :backend backend :prompt prompt))

(defun %judge-output-schema ()
  (let ((schema (make-hash-table :test 'equal))
        (props (make-hash-table :test 'equal))
        (score (make-hash-table :test 'equal))
        (verdict (make-hash-table :test 'equal))
        (rationale (make-hash-table :test 'equal)))
    (setf (gethash "type" score) "number"
          (gethash "minimum" score) 0
          (gethash "maximum" score) 1
          (gethash "type" verdict) "string"
          (gethash "type" rationale) "string"
          (gethash "score" props) score
          (gethash "verdict" props) verdict
          (gethash "rationale" props) rationale
          (gethash "type" schema) "object"
          (gethash "properties" schema) props
          (gethash "required" schema) '("score" "verdict" "rationale"))
    schema))

(defun %judge-prompt (scorer case actual)
  (or (llm-judge-scorer-prompt scorer)
      (format nil
              (concatenate
               'string
               "You are an evaluation judge. Score ACTUAL against EXPECTED."
               "~%"
               "Return JSON with keys score (number 0..1), verdict (\"pass\" or \"fail\"), "
               "rationale (short string)."
               "~%~%"
               "Input: ~s"
               "~%"
               "Expected: ~s"
               "~%"
               "Actual: ~s")
              (eval-case-input case)
              (eval-case-expected case)
              actual)))

(defun %skip-ws (string start)
  (loop for i from start below (length string)
        unless (member (char string i) '(#\Space #\Tab #\Newline #\Return))
          return i
        finally (return (length string))))

(defun %parse-json-string (string start)
  (unless (and (< start (length string)) (char= (char string start) #\"))
    (error "expected JSON string"))
  (let ((out (make-array 16 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for i from (1+ start) below (length string)
          for c = (char string i)
          do (case c
               (#\"
                (return (values (copy-seq out) (1+ i))))
               (#\\
                (when (>= (1+ i) (length string))
                  (error "unterminated JSON escape"))
                (let ((e (char string (1+ i))))
                  (vector-push-extend
                   (case e
                     (#\" #\")
                     (#\\ #\\)
                     (#\/ #\/)
                     (#\b #\Backspace)
                     (#\f #\Page)
                     (#\n #\Newline)
                     (#\r #\Return)
                     (#\t #\Tab)
                     (t e))
                   out)
                  (incf i)))
               (t (vector-push-extend c out)))
          finally (error "unterminated JSON string"))))

(defun %parse-json-number (string start)
  (let* ((end start)
         (len (length string)))
    (when (and (< end len) (char= (char string end) #\-))
      (incf end))
    (loop while (and (< end len) (digit-char-p (char string end)))
          do (incf end))
    (when (and (< end len) (char= (char string end) #\.))
      (incf end)
      (loop while (and (< end len) (digit-char-p (char string end)))
            do (incf end)))
    (when (and (< end len) (member (char string end) '(#\e #\E)))
      (incf end)
      (when (and (< end len) (member (char string end) '(#\+ #\-)))
        (incf end))
      (loop while (and (< end len) (digit-char-p (char string end)))
            do (incf end)))
    (when (= end start)
      (error "expected JSON number"))
    (values (read-from-string string t nil :start start :end end) end)))

(defun %parse-json-value (string start)
  (let ((i (%skip-ws string start)))
    (when (>= i (length string))
      (error "unexpected end of JSON"))
    (let ((c (char string i)))
      (cond
        ((char= c #\")
         (%parse-json-string string i))
        ((or (char= c #\-) (digit-char-p c))
         (%parse-json-number string i))
        ((char= c #\{)
         (%parse-json-object string i))
        ((char= c #\[)
         (%parse-json-array string i))
        ((and (<= (+ i 4) (length string))
              (string= string "true" :start1 i :end1 (+ i 4)))
         (values t (+ i 4)))
        ((and (<= (+ i 5) (length string))
              (string= string "false" :start1 i :end1 (+ i 5)))
         (values nil (+ i 5)))
        ((and (<= (+ i 4) (length string))
              (string= string "null" :start1 i :end1 (+ i 4)))
         (values nil (+ i 4)))
        (t (error "invalid JSON at ~d" i))))))

(defun %parse-json-object (string start)
  (let ((i (%skip-ws string start))
        (table (make-hash-table :test 'equal)))
    (unless (and (< i (length string)) (char= (char string i) #\{))
      (error "expected JSON object"))
    (setf i (%skip-ws string (1+ i)))
    (when (and (< i (length string)) (char= (char string i) #\}))
      (return-from %parse-json-object (values table (1+ i))))
    (loop
      (multiple-value-bind (key j) (%parse-json-string string i)
        (setf i (%skip-ws string j))
        (unless (and (< i (length string)) (char= (char string i) #\:))
          (error "expected colon in JSON object"))
        (multiple-value-bind (val k) (%parse-json-value string (1+ i))
          (setf (gethash key table) val
                i (%skip-ws string k))))
      (cond
        ((and (< i (length string)) (char= (char string i) #\,))
         (setf i (%skip-ws string (1+ i))))
        ((and (< i (length string)) (char= (char string i) #\}))
         (return (values table (1+ i))))
        (t (error "expected comma or end of JSON object"))))))

(defun %parse-json-array (string start)
  (let ((i (%skip-ws string start))
        (acc nil))
    (unless (and (< i (length string)) (char= (char string i) #\[))
      (error "expected JSON array"))
    (setf i (%skip-ws string (1+ i)))
    (when (and (< i (length string)) (char= (char string i) #\]))
      (return-from %parse-json-array (values #() (1+ i))))
    (loop
      (multiple-value-bind (val j) (%parse-json-value string i)
        (push val acc)
        (setf i (%skip-ws string j)))
      (cond
        ((and (< i (length string)) (char= (char string i) #\,))
         (setf i (%skip-ws string (1+ i))))
        ((and (< i (length string)) (char= (char string i) #\]))
         (return (values (coerce (nreverse acc) 'vector) (1+ i))))
        (t (error "expected comma or end of JSON array"))))))

(defun %try-parse-json (source)
  (cond
    ((hash-table-p source) source)
    ((not (stringp source)) nil)
    (t
     (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) source)))
       (when (and (plusp (length s)) (char= (char s 0) #\{))
         (or (let* ((pkg (find-package '#:json-protocol))
                    (decode (and pkg (find-symbol "DECODE" pkg)))
                    (backend (and pkg (find-symbol "*JSON-BACKEND*" pkg))))
               (when (and decode (fboundp decode) backend (symbol-value backend))
                 (ignore-errors (funcall decode s))))
             (ignore-errors (nth-value 0 (%parse-json-object s 0)))))))))

(defun %payload-get (payload &rest keys)
  (dolist (key keys)
    (let ((v (cond
               ((hash-table-p payload)
                (or (gethash key payload)
                    (gethash (string-downcase (string key)) payload)
                    (gethash (intern (string-upcase (string key)) :keyword)
                             payload)))
               ((listp payload)
                (or (getf payload key)
                    (getf payload (intern (string-upcase (string key))
                                          :keyword)))))))
      (when v (return v)))))

(defun %coerce-judge-score (raw)
  (cond
    ((null raw) 0)
    ((numberp raw)
     (min 1 (max 0 raw)))
    ((stringp raw)
     (min 1 (max 0 (read-from-string raw))))
    (t 0)))

(defun %coerce-judge-verdict (raw score)
  (let ((v (cond
             ((keywordp raw) raw)
             ((stringp raw) (intern (string-upcase raw) :keyword))
             ((symbolp raw) (intern (string-upcase (symbol-name raw)) :keyword))
             (t nil))))
    (or v (if (>= score 1) :pass :fail))))

(defun %score-from-judge-payload (payload)
  (unless payload
    (error 'scorer-error :message "judge returned unparseable output"))
  (let* ((value (%coerce-judge-score (%payload-get payload "score" :score)))
         (verdict (%coerce-judge-verdict
                   (%payload-get payload "verdict" :verdict)
                   value))
         (rationale (let ((r (%payload-get payload "rationale" :rationale)))
                      (if r (princ-to-string r) ""))))
    (make-eval-score :value value :verdict verdict :rationale rationale)))

(defun %score-from-judge-response (response)
  (let* ((out (llm-protocol:llm-response-output response))
         (text (llm-protocol:llm-response-text response))
         (payload (or (and (hash-table-p out) out)
                      (%try-parse-json out)
                      (%try-parse-json text))))
    (%score-from-judge-payload payload)))

(defmethod score-case ((scorer llm-judge-scorer) case actual)
  (let* ((backend (llm-judge-scorer-backend scorer))
         (schema (%judge-output-schema))
         (prompt (%judge-prompt scorer case actual))
         (response (llm-protocol:generate backend prompt :output schema)))
    (%score-from-judge-response response)))
