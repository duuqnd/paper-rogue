;;;; battle-interface.lisp
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)


(in-package #:paper-rogue.battle-interface)

;; Battle Interface
(defclass attack-view (view) ())
(defparameter *attack-view* (make-instance 'attack-view))

(defclass item-view (view) ())
(defparameter *item-view* (make-instance 'item-view))

"NOTE: The battle system currently requires that you create a
prcombat:player class instance in the *player* variable before
you start the battle interface."

(defvar *player* nil "This is the player information. It is set by the overworld code when it triggers a battle.")
(defvar *enemies* nil "This is a list of enemies to battle. It is set by the overworld code when it triggers a battle.")

(define-application-frame battle ()
  ((player :accessor player :initform *player*)
   (enemies :accessor enemies :initform *enemies*)
   (attack :accessor attack :initform nil)
   (target :accessor target :initform nil)
   (hud-pane :accessor hud-pane :initform nil))
  (:menu-bar nil)
  (:pointer-documentation t)
  (:panes
   (battle-log :application-pane
               :display-time nil
               :end-of-line-action :wrap*
               :width 2
               :height 50)
   (battle-screen :application-pane
                  :display-time t
                  :display-function #'display-battle-screen
                  :width 70
                  :height 50)
   (action-list :application-pane
                :display-function #'display-action-list
                :default-view *attack-view*
                :height 50)
   (battle-hud :label-pane
               :label (get-hud-string))
   (interactor :interactor))

  (:layouts
   (default
       (vertically ()
         (horizontally ()
           battle-hud
           '+fill+
           (make-pane 'push-button
                      :label "Clear Battle Log"
                      :activate-callback #'com-clear-battle-log-button)
           (make-pane 'push-button
                      :label "End Turn"
                      :activate-callback #'com-end-turn-button))
         (horizontally ()
           (labelling (:label "Battle Screen") battle-screen)
           (labelling (:label "Battle Log")
             (scrolling
                 (:width 2 :height 50)
               battle-log)))
         (horizontally ()
           (labelling (:label "Choose Action")
             (tabling ()
               (list
                (make-pane 'push-button
                           :label "Attack"
                           :activate-callback #'com-attack-view-button)
                (make-pane 'push-button
                           :label "Use Item"
                           :activate-callback #'com-item-view-button))
               (list
                (make-pane 'push-button
                           :label "Strategy")
                (make-pane 'push-button
                           :label "Special Moves"))))
           (labelling (:label "Action List")
             (scrolling (:height 50) action-list)))))))

(defgeneric display-action-list-with-view (frame pane view))

(defun display-action-list (frame pane)
  (display-action-list-with-view
   frame pane
   (stream-default-view (find-pane-named *application-frame* 'action-list))))

(defmethod display-battle-screen ((frame battle) pane)
  (declare (ignorable frame))
  (dotimes (index (length (enemies frame)))
    (let ((enemy (nth index (enemies frame))))
      (when enemy
        (with-output-as-presentation (pane enemy 'prcombat:creature)
          (surrounding-output-with-border
              (pane :shape
                    (if (equal enemy (target frame)) :drop-shadow :crossout))
            (draw-rectangle* pane
                             (+ (* 5 (1+ index)) index (* index 20))
                             10
                             (+ (* 5 (1+ index)) index (* (1+ index) 20))
                             40 :ink +red+)))))))

(defmethod display-action-list-with-view (frame pane (view attack-view))
  (declare (ignorable frame view))
  (dolist (attack (prcombat:attacks (player *application-frame*)))
    (with-output-as-presentation (pane attack 'prcombat:attack)
      (format pane "~a~%" (prcombat:name attack))))
  (force-output pane))

(defmethod display-action-list-with-view (frame pane (view item-view))
  (declare (ignorable frame))
  (format pane "You have no items!")
  (force-output pane))

(defun battle-message (message)
  (format (find-pane-named *application-frame* 'battle-log) "~a~%" message))

(defun update-hud ()
  (setf (clime:label-pane-label
         (find-pane-named *application-frame* 'battle-hud))
        (get-hud-string)))

(defun get-hud-string ()
  (format nil "HP: ~2d/~2d    FP: ~2d/~2d"
          (prcombat:hp (player *application-frame*))
          (prcombat:max-hp (player *application-frame*))
          (prcombat:fp (player *application-frame*))
          (prcombat:max-fp (player *application-frame*))))

(defun get-player-attack-from-ui ()
  (attack *application-frame*))

(defun get-player-target-from-ui ()
  (target *application-frame*))

(define-battle-command (com-attack-view-button) ((button 'object))
  (declare (ignorable button))
  (setf (stream-default-view (find-pane-named *application-frame* 'action-list))
        *attack-view*)
  (window-clear (find-pane-named *application-frame* 'action-list))
  (display-action-list *application-frame* (find-pane-named *application-frame* 'action-list)))

(define-battle-command (com-item-view-button) ((button 'object))
  (declare (ignorable button))
  (setf (stream-default-view (find-pane-named *application-frame* 'action-list))
        *item-view*)
  (window-clear (find-pane-named *application-frame* 'action-list))
  (display-action-list *application-frame* (find-pane-named *application-frame* 'action-list)))

(define-battle-command (com-clear-battle-log) ()
  (window-clear (find-pane-named *application-frame* 'battle-log)))

(define-battle-command (com-clear-battle-log-button) ((button 'object))
  (declare (ignorable button))
  (com-clear-battle-log))

(define-battle-command (com-end-turn) ()
  (cond
    ((not (attack *application-frame*))
     (battle-message "You need to choose an action first!"))
    ((not (member (target *application-frame*)
                  (prcombat:get-possible-targets
                   (attack *application-frame*)
                   (enemies *application-frame*)
                   (player *application-frame*))))
     (battle-message "Invalid target! Use \"Describe Action\" to get a description of an action."))
    (t
     (setf (enemies *application-frame*)
           (prcombat:step-battle (player *application-frame*)
                                 (enemies *application-frame*) 0))
     (setf (target *application-frame*) nil)
     (unless (remove-if #'null (enemies *application-frame*))
       (battle-message "Victory!")
       (battle-message "This is the point where I'd calculate your EXP, but that's not implemented yet!"))
     (window-clear (find-pane-named *application-frame* 'battle-screen))
     (display-battle-screen *application-frame* (find-pane-named *application-frame* 'battle-screen))
     (update-hud))))

(define-battle-command (com-end-turn-button) ((button 'object))
  (declare (ignorable button))
  (com-end-turn))

(define-battle-command (com-describe-action) ((object attack))
  (format (find-pane-named *application-frame* 'battle-log)
          "Description of ~a: ~a~%"
          (prcombat:name object)
          (prcombat:description object)))

(define-presentation-to-command-translator describe-action
    (prcombat:attack com-describe-action battle
                               :gesture :describe)
    (object)
  (list object))

(define-battle-command (com-perform-action) ((action prcombat:attack))
  (declare (ignorable action))
  (let ((target (accept 'prcombat:creature :prompt "Choose target")))
    (if target
        (setf (target *application-frame*) target
              (attack *application-frame*) action)
        (format (find-pane-named *application-frame* 'battle-log)
                "Cancelled.")))
  (window-clear (find-pane-named *application-frame* 'battle-screen))
  (display-battle-screen *application-frame* (find-pane-named *application-frame* 'battle-screen)))

(define-presentation-to-command-translator perform-action
    (prcombat:attack com-perform-action battle
                               :gesture :select)
    (object)
  (list object))

(define-battle-command (com-show-enemy-list :name t) ()
  (format t "~a~%" (enemies *application-frame*)))

(defun run-battle (&key (run-on-new-thread t))
  (if run-on-new-thread
      (bt:make-thread (lambda () (run-battle :run-on-new-thread nil))
                      :name "Battle Window")
      (run-frame-top-level (make-application-frame 'battle))))

(defun run-paper-rogue ()
  (bt:make-thread #'run-paper-rogue-on-current-thread :name "CLIM Window"))

(defun run-paper-rogue-on-current-thread ()
  (run-frame-top-level (make-application-frame 'game)))

(define-battle-command (com-quit :name t :menu t) ()
  (frame-exit *application-frame*))

(defun game-message (message)
  (format t "~a~%" message))
