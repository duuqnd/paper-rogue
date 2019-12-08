;;;; creatures.lisp
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)


(in-package #:paper-rogue.combat)

(defclass creature ()
  ((name :accessor name)
   (max-hp :accessor max-hp)
   (hp :accessor hp)
   (attack-power :accessor attack-power)
   (defense-power :accessor defense-power)
   (avalible-attacks :accessor attacks)
   (held-item :accessor held-item :initarg :held-item)
   (position :accessor pos :initarg :position :documentation
             "Examples: :ground, :air")))

(defmethod initialize-instance :after ((obj creature) &key)
  (unless (typep obj 'player)
    (setf (hp obj) (max-hp obj))))

(defmethod print-object ((object creature) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "~a"
            (name object)
            #+nil
            (cond
              ((grounded-creature-p object)
               "Grounded")
              ((airborne-creature-p object)
               "Airborne")))))

(defun grounded-creature-p (obj)
  (and (typep obj 'creature) (equal (pos obj) :ground)))

(defun airborne-creature-p (obj)
  (and (typep obj 'creature) (equal (pos obj) :air)))

(defun print-creature-with-index (creature index)
  (format nil "~d: ~a (~a)"
          index
          (name creature)
          (cond
            ((grounded-creature-p creature)
             "Grounded")
            ((airborne-creature-p creature)
             "Airborne"))))

(defclass player (creature)
  ((name :accessor name :initarg :name)
   (max-hp :accessor max-hp :initarg :max-hp)
   (max-fp :accessor max-fp :initarg :max-fp)
   (hp :accessor hp :initarg :hp)
   (fp :accessor fp :initarg :fp)
   (attack-power :accessor attack-power :initarg :attack-power)
   (defense-power :accessor defense-power :initarg :defense-power)
   (avalible-attacks :accessor attacks :initarg :attacks)
   (consumable-items :accessor items :initarg :items)
   (badges :accessor badges :initarg :badges)))

(defmethod initialize-instance :after ((obj player) &key)
  (values))

(defmethod calculate-damage ((creature creature) damage)
  (if (<= (- damage (defense-power creature)) 0)
      0
      (- damage (defense-power creature))))

(defmethod damage ((creature creature) damage)
  (battle-message (format nil "~d damage inflicted on ~a!~%"
                        (calculate-damage creature damage)
                        (name creature)))
  (setf (hp creature) (- (hp creature) (calculate-damage creature damage)))
  (when (<= (hp creature) 0)
    (battle-message (format nil "~a has been knocked out!" (name creature)))
    (setf (hp creature) 0)))

(defmethod kill ((creature creature)))

(defgeneric choose-attack (creature))


(defclass half-dead-bug (creature)
  ((name :accessor name :initform "Half-dead Bug")
   (max-hp :accessor max-hp :initform 2)
   (attack-power :accessor attack-power :initform 1)
   (defense-power :accessor defense-power :initform 0)
   (avalible-attacks :accessor avalible-attacks :initform
                     (list *attack-bonk* *attack-stare*))))

(defmethod choose-attack ((creature half-dead-bug))
  (nth (game-rand 2) (avalible-attacks creature)))
