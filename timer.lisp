(in-package :crawl)


(defun elapsed-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second)
  )

(defstruct timer
  (countdown nil)
  (start nil)
  )

(defun start-timer (secs)
  (make-timer :countdown secs :start (elapsed-seconds))
  )

(defun timer-elapsed (timer)
  (- (elapsed-seconds) (timer-start timer))
  )

(defun timer-ended (timer)
  (cond
    ((null timer) t)
    ((> (timer-elapsed timer) (timer-countdown timer))))
  )

(let ((last-tick (elapsed-seconds)))
  (defun get-ticks ()
    (let* ((now (elapsed-seconds))
           (ticks (- now last-tick)))
      (setf last-tick now)
      ticks))
  )
