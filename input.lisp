(in-package :crawl)

(defvar *inputs* (make-hash-table))
(defvar *controls* (make-hash-table))

(defun bind-input (key control)
  (setf (gethash key *inputs*) control)
  (bind-button key :pressed (lambda () (setf (gethash control *controls*) t)))
  (bind-button key :released (lambda () (setf (gethash control *controls*) nil)))
  )

(defun input (control)
  (gethash control *controls*))
