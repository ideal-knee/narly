;; Narly

(defvar *narly-macros* ())

(defun string-join (elements delimiter)
  (format nil (format nil "~~{~~a~~^~a~~}" delimiter) elements) )

(defun narly-eval (form)
  (cond

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
    ((eq (car form) '|narly-include|)
     (narly (open (format nil "~a.n" (cadr form)) :direction :input)) )

    ;; Macro definition
    ((eq (car form) '|define-narly-macro|)
     (destructuring-bind (name &rest arguments-and-body) (cdr form)
       (push `(,name . ,arguments-and-body) *narly-macros*) )
     "" )

    ;; Macro call
    ((assoc (car form) *narly-macros*)
     (destructuring-bind (arguments &rest body)
         (cdr (assoc (car form) *narly-macros*))
       (narly-eval (eval `(destructuring-bind ,arguments ',(cdr form)
                            ,@body ))) ) )

    ;; Naive function call
    ;; ?? Default case--could hide errors...
    ;; ?? Only works for languages with C-like function call syntax
    (t (format nil "~a(~a)"
               (narly-eval (car form))
               (string-join (mapcar #'narly-eval (cdr form)) ", ") )) ) )

(defun narly (in-stream)
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (do ((form (read in-stream nil 'eof) (read in-stream nil 'eof)))
	((eql form 'eof) "")
      (format t "~a" (narly-eval form)) ) ) )

(narly (open "src/c.n"))

