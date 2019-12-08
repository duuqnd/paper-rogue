;;;; overworld-interface.lisp
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)


(in-package #:paper-rogue.overworld)

;; Overworld exploration interface

(defconstant +map-height+ 45)
(defconstant +map-width+ 128)

(defparameter *player* (make-instance 'player :name "Unnamed Cave Explorer"))
(defparameter *map* (make-array (list +map-height+ +map-width+)
                                :element-type 'unsigned-byte :adjustable nil))
(defparameter *active-entities* (list *player*))

(defclass scrollbar-preserving-mixin () ())
(defclass map-screen (scrollbar-preserving-mixin application-pane) ())

(defmethod redisplay-frame-pane :around
    ((frame application-frame) (pane scrollbar-preserving-mixin)
     &key force-p)
  (declare (ignorable force-p))
  (let ((x (gadget-value (slot-value (pane-scroller pane) 'climi::hscrollbar)))
        (y (gadget-value (slot-value (pane-scroller pane) 'climi::vscrollbar))))
    (call-next-method)
    (scroll-extent pane x y)))

(define-application-frame paper-rogue ()
  ()
  (:menu-bar t)
  (:panes
   (map-screen map-screen
               :display-time :command-loop
               :display-function #'display-map-screen
               :incremental-redisplay t
               :end-of-page-action :scroll
               :end-of-line-action :scroll
               :height 300)
   (status-screen :application-pane
                  :display-function #'display-status-screen
                  :width 300)
   (interactor :interactor
               :width 300))
  (:layouts
   (default
       (vertically ()
         (3/4
          (labelling (:label "Map")
            (scrolling () map-screen)))
         (horizontally ()
           (labelling (:label "Interactor") interactor)
           (1/16
            (labelling (:label "Your Status") status-screen)))))))

(defun run-overworld ()
  (run-frame-top-level (make-application-frame 'paper-rogue)))

(defun display-map-screen (frame pane)
  (declare (ignorable frame))
  (dotimes (y +map-height+)
    (dotimes (x +map-width+)
      (draw-rectangle* pane (* x 8) (* y 8) (+ 8 (* x 8)) (+ 8 (* y 8))
                       :ink (if (zerop (aref *map* y x)) +white+ +black+))))
  (mapc (lambda (entity) (draw-entity entity pane)) *active-entities*))

(defun display-status-screen (frame pane)
  (declare (ignore frame))
  (format pane "Name: ~a~%" (name *player*))
  (format pane "HP: ~2d/~2d~%" (hp *player*) (max-hp *player*))
  (format pane "FP: ~2d/~2d~%" (fp *player*) (max-fp *player*))
  (format pane "Attack power: ~d~%" (attack-power *player*))
  (format pane "Level: ~2d~%" (level *player*)))

(defun update-game-state ())

;; Player movement commands

(define-paper-rogue-command (com-move-up :name t) ()
  (decf (y-pos *player*))
  (update-game-state))

(define-paper-rogue-command (com-move-down :name t) ()
  (incf (y-pos *player*))
  (update-game-state))

(define-paper-rogue-command (com-move-right :name t) ()
  (incf (x-pos *player*))
  (update-game-state))

(define-paper-rogue-command (com-move-left :name t) ()
  (decf (x-pos *player*))
  (update-game-state))

(define-paper-rogue-command (com-debug-randomize-map :name t) ()
  (dotimes (y +map-height+)
    (dotimes (x +map-width+)
      (setf (aref *map* y x) (game-rand 2)))))

(define-paper-rogue-command (com-debug-toggle-square :name t) ((x 'number) (y 'number))
  (setf (aref *map* y x) (if (zerop (aref *map* y x)) 1 0)))

(define-paper-rogue-command (com-debug-battle :name t) ()
  (prepare-for-battle *player*)
  (setf paper-rogue.battle-interface:*player* (combat-player *player*)
        paper-rogue.battle-interface:*enemies*
        (list (make-instance 'prcombat:half-dead-bug :position :ground)
              (make-instance 'prcombat:half-dead-bug :position :ground)))
  (paper-rogue.battle-interface::run-battle :run-on-new-thread nil))

(define-paper-rogue-command (com-test-accepting-values :name t)
    ((value1 'number)
     (value2 'form))
  (format t "~a~%~a~%" value1 value2))
