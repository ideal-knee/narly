;; UL: The User/Ultimate/Universal Language

(defvar *ul-macros* ())

(defun string-join (elements delimiter)
  (format nil (format nil "~~{~~a~~^~a~~}" delimiter) elements) )

(defun ul-eval (form)
  (cond

    ;; String literal
    ((stringp form) (format nil "\"~a\"" form))

    ;; Symbol or numeric literal
    ((atom form) (format nil "~a" form))

    ;; UL macro definition
    ((eq (car form) '|define-macro|)
     (destructuring-bind (name &rest arguments-and-body) (cdr form)
       (push `(,name . ,arguments-and-body) *ul-macros*) )
     "" )

    ;; File include
    ((eq (car form) '|include|)
     (ul (open (cadr form) :direction :input)) )

    ;; UL macro call
    ((assoc (car form) *ul-macros*)
     (destructuring-bind (arguments &rest body)
         (cdr (assoc (car form) *ul-macros*))
       (ul-eval (eval `(destructuring-bind ,arguments ',(cdr form)
                         ,@body ))) ) )

    ;; Naive UL function call
    ;; ?? Default case--could hide errors...
    ;; ?? Only works for languages with C-like function call syntax
    (t (format nil "~a(~a)"
               (ul-eval (car form))
               (string-join (mapcar #'ul-eval (cdr form)) ", ") )) ) )

(defun ul (&optional (in-stream *standard-input*))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (do ((form (read in-stream nil 'eof) (read in-stream nil 'eof)))
	((eql form 'eof) "")
      (format t "~a" (ul-eval form)) ) ) )

(ul)

