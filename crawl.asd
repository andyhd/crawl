(asdf:defsystem :crawl
  :version "0.1"
  :description "Dungeon Crawl"
  :author "Andy Driver <andy@pagezero.net>"
  :license "MIT"
  :depends-on (trivial-gamekit
               cl-tiled)
  :serial t
  :components ((:file "timer")
               (:file "animation")
               (:file "mob")
               (:file "player")
               (:file "input")
               (:file "main")))
