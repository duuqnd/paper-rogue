;;;; attacks.lisp
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)


(in-package #:paper-rogue.combat)

(defgeneric use-attack (attack target attacker))
(defmethod get-possible-targets (attack targets attacker))

(defclass attack ()
  ((name :reader name
         :allocation :class)
   (description :reader description
                :allocation :class)
   (restrictions :reader restrictions
                 :initform nil
                 :allocation :class)
   (fp-cost :reader fp-cost
            :initform 0
            :allocation :class)))

(defmethod get-possible-targets ((attack attack) targets (attacker creature))
  (when (member :ground-only (restrictions attack))
    (setf targets (remove-if-not #'grounded-creature-p targets)))
  (when (member :first-only (restrictions attack))
    (setf targets (list (car targets))))
  (remove nil targets))

(defmethod attack-works ((attack attack) (player player))
  (if (>= (fp player) (fp-cost attack))
      t
      (progn (battle-message "Not enough FP!") nil)))

(defclass bonk (attack)
  ((name :reader name
         :initform "Bonk"
         :allocation :class)))

(defmethod use-attack ((attack bonk) (target creature) (attacker creature))
  (damage target 1))

(defparameter *attack-bonk* (make-instance 'bonk))

(defclass stare (attack)
  ((name :reader name
         :initform "Stare"
         :allocation :class)))

(defmethod use-attack ((attack stare) (target creature) (attacker creature))
  (battle-message (format nil "The ~a stares blankly at you." (name attacker)))
  (damage target 0))

(defparameter *attack-stare* (make-instance 'stare))

(defclass jump (attack)
  ((name :reader name
         :initform "Jump"
         :allocation :class)
   (description :reader description
                :initform "Jumps on an enemy's head and deals your attack power as damage")))

(defmethod use-attack ((attack jump) (target creature) (attacker creature))
  (damage target (attack-power attacker)))

(defparameter *attack-jump* (make-instance 'jump))

(defclass hammer-smash (attack)
  ((name :reader name
         :initform "Hammer Smash"
         :allocation :class)
   (description :reader description
                :initform "Smashes the nearest enemy on the ground with your hammer and deals your attack power as damage. Works against spiked enemies.")
   (restrictions :reader restrictions
                 :initform '(:ground-only :first-only))))

(defmethod use-attack ((attack hammer-smash) (target creature) (attacker creature))
  (damage target (attack-power attacker)))

(defparameter *attack-hammer-smash* (make-instance 'hammer-smash))

(defclass power-jump (attack)
  ((name :reader name
         :initform "Power Jump"
         :allocation :class)
   (fp-cost :reader fp-cost
            :initform 2
            :allocation :class)))

(defmethod use-attack ((attack power-jump) (target creature) (attacker creature))
  (damage target (+ 1 (attack-power attacker))))

(defparameter *attack-power-jump* (make-instance 'power-jump))

(defclass power-smash (attack)
  ((name :reader name
         :initform "Power Smash"
         :allocation :class)
   (fp-cost :reader fp-cost
            :initform 4
            :allocation :class)))

(defmethod use-attack ((attack power-smash) (target creature) (attacker creature))
  (damage target (+ 2 (attack-power attacker))))

(defparameter *attack-power-smash* (make-instance 'power-smash))
