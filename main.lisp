(load "crawl.asd")

(ql:quickload '(trivial-gamekit cl-tiled))

(cl:defpackage :crawl
  (:use :cl :gamekit :cl-tiled)
  (:export crawl))

(load "timer.lisp")
(load "animation.lisp")
(load "mob.lisp")
(load "camera.lisp")
(load "input.lisp")
(load "player.lisp")
(load "world.lisp")

(in-package :crawl)

(defgame crawl () ()
  (:viewport-width 640)
  (:viewport-height 480)
  (:viewport-title "Crawl"))

(register-resource-package
  :keyword (asdf:system-relative-pathname :crawl "assets/"))

(define-image :human "human_middle_outline.png")
(define-image :tiles "tileset.png")
(define-font :bitpotion "BitPotion.ttf")
(defvar *font* nil)

(defvar *human* nil)
(setf *human* (make-sheet :image :human
                          :grid (vec2 10 4)
                          :cell-size (vec2 32 32)))

(defvar *player* nil)
(setf *player* (make-instance 'player))
(setf (animations *player*)
      '(idle (anim-cells *human* 10)
        walk-right (anim-cells *human* 20 21 20 22)
        walk-left (anim-cells *human* 39 38 39 37)
        walk-up (anim-cells *human* 0 1 0 2)
        walk-down (anim-cells *human* 10 11 10 12)
        punch (anim-cells *human* 12 13 14)
        hurt (anim-cells *human* 15 16)
        ))
(setf (anim *player*) 'idle)
(setf (pos *player*) (vec2 120 90))
(setf (spd *player*) 50)

(defvar *camera* nil)
(setf *camera* (make-instance 'camera
                              :subject *player*
                              :viewport-size (vec2 320 240)))

(setf *world* (load-map (asdf:system-relative-pathname :crawl "level0.tmx")))

(defmethod post-initialize ((this crawl))
  (print "Starting...")
  (bind-input :up :walk-up)
  (bind-input :down :walk-down)
  (bind-input :left :walk-left)
  (bind-input :right :walk-right)
  (setf *font* (make-font :bitpotion 16))
  )

(defmethod act ((app crawl))
  (let ((now (elapsed-seconds))
        (ticks (get-ticks)))
    (update *player* ticks)
    (update *camera* ticks)
    (let ((rest-of-frame (- (/ 1 100) (- (elapsed-seconds) now))))
      (if (> rest-of-frame 0)
        (sleep rest-of-frame)))
    )
  )

(defun draw-status (label vals origin)
  (draw-text (format nil "~A: ~{ ~A ~}" label vals)
             (subt origin (vec2 -1 1))
             :fill-color (vec4 0 0 0 1)
             :font *font*)
  (draw-text (format nil "~A: ~{ ~A ~}" label vals)
             origin
             :fill-color (vec4 1 1 1 1)
             :font *font*))

(defmethod draw ((app crawl))
    (with-pushed-canvas ()
      (scale-canvas 2.0 2.0)
      (translate-canvas (- (x (pos *camera*))) (- (y (pos *camera*))))
      (render-world)
      (draw-mob *player*)
    )
    (draw-status "Camera" `(,(x (pos *camera*)) ,(y (pos *camera*))) (vec2 5 5))
    (draw-status "Player" `(,(x (pos *player*)) ,(y (pos *player*))) (vec2 5 21))
  )

(start 'crawl)

(stop)
