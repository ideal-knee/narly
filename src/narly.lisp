;; Narly

(defvar *narly-readtable* (copy-readtable nil))
(setf (readtable-case *narly-readtable*) :preserve)

(defvar *narly-macros* ())

(defun string-join (elements delimiter)
  (format nil (format nil "~~{~~a~~^~a~~}" delimiter) elements) )

(defun narly-eval (form)
  (cond

    ;; Character literal
    ((characterp form)
     (if (char= form #\Newline)
         "'\\n'"
         (format nil "'~a'" form) ) )

    ;; String literal
    ((stringp form) (format nil "\"~a\"" form))

    ;; Symbol or numeric literal
    ((atom form)
     (let ((form-string (format nil "~a" form)))
       (if (null (find #\( form-string))
         (substitute #\_ #\- form-string)
         form-string ) ) )

    ;; Render code
    ((eq (first form) '|narly-render|)
     (destructuring-bind (&key (|context| "~a") (|separator| "")) (second form)
       (format nil |context| (string-join (mapcar #'narly-eval (subseq form 2)) |separator|)) ) )

    ;; File include
    ((eq (first form) '|narly-include|)
     (narly (open (format nil "~a.n" (second form)) :direction :input)) )

    ;; Macro definition
    ((eq (first form) '|define-narly-macro|)
     (destructuring-bind (name &rest arguments-and-body) (rest form)
       (push `(,name . ,arguments-and-body) *narly-macros*) )
     "" )

    ;; Macro call
    ((assoc (first form) *narly-macros*)
     (destructuring-bind (arguments &rest body)
         (rest (assoc (first form) *narly-macros*))
       (narly-eval (eval `(destructuring-bind ,arguments ',(rest form)
                            ,@body ))) ) )

    ;; Naive function call
    ;; ?? Default case--could hide errors...
    ;; ?? Only works for languages with C-like function call syntax
    (t (format nil "~a(~a)"
               (narly-eval (first form))
               (string-join (mapcar #'narly-eval (rest form)) ", ") )) ) )

(defun narly (in-stream)
  (let ((*readtable* *narly-readtable*))
    (do ((form (read in-stream nil 'eof) (read in-stream nil 'eof)))
	((eql form 'eof) "")
      (format t "~a" (narly-eval form)) ) ) )

(narly (open "src/c.n"))

