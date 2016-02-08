(in-package :cl-user)

(defpackage :prompt
  (:use :common-lisp :cl-ppcre)
  (:export
   :result
   :end-prompt
   :poparg
   :poparg1
   :prompt-as
   :defprompt
   :defdefault
   :prompt-line
   :command
   :prompt))

(in-package :prompt)

(defmacro defprompt ((name desc prompts) &body body)
  (let ((sym (gensym)))
    `(let ((,sym (intern ,name)))
       (setf (get 'handler ,sym)
             #'(lambda (prompt-line prompts command)
                 (labels ((end-prompt ()
                            (setf (get 'done prompts) t))
                          (prompt-as (str)
                            (loop 
                               for response = '("")
                               do
                                 (princ str)
                                 (force-output)
                               until (> (length (car (setq response (split " " (read-line))))) 0)
                               finally (return response)))
                          (poparg1 ()
                            (loop
                               as arg = (car (get 'args prompts))
                               when (> (length arg) 0)
                               return arg
                               end
                               do
                                 (setf (get 'args prompts) (prompt-as "ARG> "))))
                          (poparg ()
                            (let ((res (poparg1)))
                              (setf (get 'args prompts) (cdr (get 'args prompts)))
                              res))
                          (result (v)
                            (setf (get 'result prompts) v)))
                   ,@body)))
       (setf (get 'name ,sym) ,name)
       (setf (get 'desc ,sym) ,desc)
       (push ,sym ,prompts))))

(defmacro defdefault ((prompts) &body body)
  `(defprompt ("default" "parse remaining commands" ,prompts)
     ,@body))

(defun show-help (prompts)
  (princ "Usage:")
  (terpri)
  (terpri)
  (loop for prompt in prompts
       unless (string= (get 'name prompt) "default")
     do (format t "~A: ~A~%" (get 'name prompt) (get 'desc prompt))))

(defun parse-args (s)
  (let ((cmd (car (split " " s))))
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
      (values cmd 
              (remove-if #'(lambda (x) (string= "" x))
                         (r1 (split " "
                                    (regex-replace-all "\""
                                                       (subseq s (length cmd))
                                                       " \" "))))))))

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
  (setf (get 'helpname prompts) helpname)
  (loop for command = "" then (read-line)
     until (or (interpret-command command prompts)
               (get 'done prompts))
     do
       (princ (get 'prompt prompts))
       (force-output))
  (get 'result prompts))
       

