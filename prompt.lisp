(in-package :cl-user)

(defpackage :prompt
  (:use
   :common-lisp
   :cl-ppcre)
  (:export
   :result
   :end-prompt
   :poparg
   :defprompt
   :defdefault
   :command
   :prompt))

(in-package :prompt)

(defmacro defprompt ((name desc prompts &optional argnames) &body body)
  (let ((sym (gensym)))
    `(let ((,sym (intern ,name)))
       (setf (get 'handler ,sym)
             #'(lambda (prompt-line prompts command)
                 (declare (ignorable command prompt-line))
                 (labels ((end-prompt ()
                            (setf (get 'done prompts) t))
                          (prompt-as (str)
                            (princ str)
                            (force-output)
                            (let ((r (parse-subargs (read-line))))
                              (if (consp r)
                                  r
                                  (prompt-as str))))
                          (poparg1 ()
                            (loop
                               as arg = (car (get 'args prompts))
                               when (> (length arg) 0)
                               return arg
                               end
                               do
                                 (setf (get 'args prompts) (prompt-as (concatenate 'string (nextname prompts) "> ")))))
                          (poparg ()
                            (let ((res (poparg1)))
                              (setf (get 'args prompts) (cdr (get 'args prompts)))
                              (setf (get 'anames prompts) (cdr (get 'anames prompts)))
                              res))
                          (result (v)
                            (setf (get 'result prompts) v)))
                   ,@body)))
       (setf (get 'name ,sym) ,name)
       (setf (get 'desc ,sym) ,desc)
       (setf (get 'argnames ,sym) ,argnames)
       (push ,sym ,prompts))))

(defmacro defdefault ((prompts) &body body)
  `(defprompt ("default" "parse remaining commands" ,prompts)
     ,@body))

(defun nextname (prompts)
  (let ((args (get 'anames prompts)))
    (if args
        (car args)
        "ARG")))
  
(defun show-help (prompts)
  (princ "Usage:")
  (terpri)
  (terpri)
  (loop for prompt in prompts
       unless (string= (get 'name prompt) "default")
     do (format t "~A: ~A~%" (get 'name prompt) (get 'desc prompt))))

(defun parse-subargs (s)
  (labels ((r1 (s)
             (when (consp s)
               (let ((f (car s)))
                 (if (string= "\"" f)
                     (r2 (cdr s) "")
                     (cons f (r1 (cdr s)))))))
           (r2 (s l)
             (when (consp s)
               (let ((f (car s)))
                 (if (string= "\"" f)
                     (cons (string-trim " " l) (r1 (cdr s)))
                     (r2 (cdr s) (concatenate 'string l " " f)))))))
    (remove-if #'(lambda (x) (string= "" x))
               (r1 (split " "
                          (regex-replace-all "\""
                                             s
                                             " \" "))))))
  
(defun parse-args (s)
  (let ((cmd (car (split " " s))))
    (values cmd (parse-subargs (subseq s (length cmd))))))

(defun interpret-command (command prompts)
  (multiple-value-bind (cmd args) (parse-args command)
    (unless (if (= 0 (length command))
                t
                (if (string= cmd (get 'helpname prompts))
                    (progn 
                      (show-help prompts)
                      t)
                    (loop for prompt in prompts
                       for l = (length (get 'name prompt))
                       when (and (>= (length cmd) l)
                                 (string= cmd (get 'name prompt)))
                       return (progn
                                (setf (get 'anames prompts) (get 'argnames prompt))
                                (setf (get 'args prompts) args)
                                (funcall (get 'handler prompt) (subseq command (length (get 'name prompt))) prompts command)
                                t))))
      (loop for prompt in prompts
         when (string= "default" (get 'name prompt))
         do (progn
              (setf (get 'args prompts) (cons cmd args))
              (funcall (get 'handler prompt) command prompts command))))))

(defun prompt (str prompts &key (helpname "h"))
  (setf (get 'prompt prompts) str)
  (setf (get 'done prompts) nil)
  (setf (get 'result prompts) nil)
  (setf (get 'anames prompts) nil)
  (setf (get 'helpname prompts) helpname)
  (loop for command = "" then (read-line)
     until (or (interpret-command command prompts)
               (get 'done prompts))
     do
       (princ (get 'prompt prompts))
       (force-output))
  (get 'result prompts))
       

