;;;; paper-rogue.asd
;;
;;;; Copyright (c) 2019 John Lorentzson (Duuqnd)


(asdf:defsystem #:paper-rogue
  :description "A Roguelike for CLIM inspired by Paper Mario"
  :author "John Lorentzson (Duuqnd)"
  :license  "GNU GPL v2 or later"
  :version "0.0.1"
  :serial t
  :depends-on (:mcclim)
  :components ((:file "package")
               (:file "paper-rogue")
               (:file "utilities")
               (:file "battle-interface")
               (:file "creatures")
               (:file "attacks")
               (:file "combat")

               (:file "map-utilities")
               (:file "overworld")
               (:file "overworld-interface")))
