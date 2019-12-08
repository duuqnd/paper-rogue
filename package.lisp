;;;; package.lisp
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)

(defpackage #:paper-rogue.utilities
  (:use #:clim-lisp #:clim)
  (:export :game-rand
           :game-message))

(defpackage #:paper-rogue.battle-interface
  (:use #:clim-lisp #:clim)
  (:export :battle-message
           :get-player-attack-from-ui
           :get-player-target-from-ui
           :get-player-action

           :*player*
           :*enemies*))

(defpackage #:paper-rogue.combat
  (:use #:clim-lisp #:paper-rogue.utilities #:paper-rogue.battle-interface)
  (:nicknames #:prcombat)
  (:export :step-battle
           ;; Creature
           :creature
           :player
           ;; Creature accessors and methods
           :name
           :hp
           :fp
           :max-hp
           :max-fp
           :attack-power
           :defense-power
           :attacks
           :avalible-attacks
           :pos
           :items
           :grounded-creature-p
           :airborne-creature-p
           :badges

           ;; Creatures
           :half-dead-bug

           ;; Attack
           :attack
           ;; Attack accessors and methods
           :name
           :description
           :use-attack
           :get-possible-targets
           :attack-works

           ;; Attacks
           :*attack-bonk*
           :*attack-stare*
           :*attack-jump*
           :*attack-hammer-smash*
           :*attack-power-jump*
           :*attack-power-smash*))

(defpackage #:paper-rogue.overworld
  (:use #:clim-lisp #:clim #:paper-rogue.utilities)
  (:import-from #:paper-rogue.combat
                :creature
                :name
                :description
                :items
                :badges
                :attack-power
                :defense-power
                :max-hp
                :hp
                :max-fp
                :fp

                ;; Creatures
                :half-dead-bug))

(defpackage #:paper-rogue
  (:use #:cl)
  (:local-nicknames (#:combat #:paper-rogue.combat)
                    (#:combat-ui #:paper-rogue.battle-interface)
                    (#:overworld #:paper-rogue.overworld)))

(in-package #:paper-rogue)
