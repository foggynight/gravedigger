(asdf:defsystem :gravedigger
  :description "Procedural dungeon generator for rogue-likes and other
tile-based games."
  :author "Robert Coffey"
  :license "GPLv2"
  :version "0.1.0"

  :pathname "src/"

  :serial t
  :components ((:file "package")

               (:file "tile")
               (:file "dungeon")

               (:file "region"))
