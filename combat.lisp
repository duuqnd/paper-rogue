;;;; combat.lisp
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)


(in-package #:paper-rogue.combat)

(defun begin-battle (player enemies)
  (let ((turn 0)
        (in-battle t))

    (battle-message "BATTLE!!")
    (battle-message "")
    (battle-message "Avalible attacks:")
    (dotimes (index (length (attacks player)))
      (battle-message (format nil "~d: ~a" index (name (nth index (attacks player))))))

    (battle-message "")
    (battle-message "Enemies:")
    (dotimes (index (length enemies))
      (battle-message (print-creature-with-index (nth index enemies) index)))
    (battle-message "")
    
    (loop until (not in-battle) do
         (progn
           (setf enemies (step-battle player enemies (incf turn)))
           (unless enemies
             (setf in-battle nil)
             (battle-message "Victory!")
             (battle-message "TODO: Calculate star points."))))))

(defun step-battle (player enemies turn)
  (battle-message (format nil "Beginning turn ~d!" turn))
  (let-player-attack player enemies)
  (setf enemies
        (mapcar (lambda (enemy) (if (and (typep enemy 'creature)
                                         (> (hp enemy) 0))
                                    enemy nil))
                enemies))
  (mapc (lambda (enemy) (let-enemy-attack enemy player))
        (remove-if #'null enemies))
  enemies)

(defun let-player-attack (player enemies)
  (let* ((attack (get-player-attack player))
         (targets (get-possible-targets attack enemies player)))
    (if targets
        (use-attack attack (get-player-target targets) player)
        (progn
          (battle-message "There's no one to use that attack on!")
          (let-player-attack player enemies)))))

(defun get-player-attack (player)
  (declare (ignorable player))
  (get-player-attack-from-ui))

#+nil
(defun get-player-attack (player)
  (format t "Enter attack number: ")
  (let* ((attack-number (parse-integer (read-line)))
         (attack (nth attack-number (attacks player))))
    (if attack
        attack
        (progn
          (battle-message "Not a vaild attack number!")
          (get-player-attack player)))))

(defun get-player-target (targets)
  (declare (ignorable targets))
  (get-player-target-from-ui))

#+nil
(defun get-player-target (targets)
  (battle-message "Avalible targets:")
  (dotimes (index (length targets))
    (battle-message (print-creature-with-index (nth index targets) index)))
  (format t "Enter target number: ")
  (let* ((target-number (parse-integer (read-line)))
         (target (nth target-number targets)))
    (if target
        target
        (progn
          (battle-message "Not a vaild target number!")
          (get-player-target targets)))))

(defun let-enemy-attack (enemy player)
  (let ((attack (choose-attack enemy)))
    (battle-message (format nil "~a used ~a!" (name enemy) (name attack)))
    (use-attack attack player enemy)))

(defun debug-battle ()
  (begin-battle *player* (list (make-instance 'half-dead-bug :position :air :held-item nil) (make-instance 'half-dead-bug :position :ground :held-item nil))))
