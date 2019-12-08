;;;; overworld.lisp
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)


(in-package #:paper-rogue.overworld)

;; Overworld exploration

(defgeneric draw-entity (entity pane))

(defclass entity ()
  ((status :accessor status :initform :alive)
   (x-position :accessor x-pos :initform 0)
   (y-position :accessor y-pos :initform 0)))

(defmethod draw-entity ((entity entity) pane)
  nil)

(defclass drawable-entity (entity)
  ((color :accessor color :initform +red+)))

(defmethod draw-entity ((entity drawable-entity) pane)
  (let ((x (* 8 (x-pos entity))) (y (* 8 (y-pos entity))))
    (draw-rectangle* pane x y (+ x 8) (+ y 8) :ink (color entity))))

(defconstant +starting-hp+ 10)
(defconstant +starting-fp+ 5)

(defparameter *player-start-attacks*
  (list prcombat:*attack-jump* prcombat:*attack-hammer-smash*))

(defclass player (drawable-entity)
  ((color :accessor color :initform +green+)
   (name :accessor name :initarg :name)
   (combat-player :accessor combat-player)
   (max-hp :accessor max-hp :initform +starting-hp+)
   (max-fp :accessor max-fp :initform +starting-fp+)
   (hp :accessor hp)
   (fp :accessor fp)
   (attack-power :accessor attack-power :initform 1)
   (level :accessor level :initform 1)
   (attacks :accessor attacks :initform *player-start-attacks*)
   (items :accessor items :initform nil)
   (badges :accessor badges :initform nil)))

(defmethod initialize-instance :after ((obj player) &key)
  (setf (hp obj) (max-hp obj)
        (fp obj) (max-fp obj))
  (setf (combat-player obj)
        (make-instance 'prcombat:player
                       :name (name obj)
                       :hp (hp obj)
                       :fp (fp obj)
                       :attack-power (attack-power obj)
                       :defense-power 0
                       :position :ground
                       :attacks (attacks obj)
                       :items (items obj)
                       :badges (badges obj))))

(defmethod prepare-for-battle ((player player))
  (setf (combat-player player)
        (make-instance 'prcombat:player
                       :name (name player)
                       :max-hp (max-hp player)
                       :max-fp (max-fp player)
                       :hp (hp player)
                       :fp (fp player)
                       :attack-power (attack-power player)
                       :defense-power 0
                       :position :ground
                       :attacks (attacks player)
                       :items (items player)
                       :badges (badges player))))
