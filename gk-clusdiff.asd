(asdf:defsystem "gk-clusdiff"
  :description "Comparing clusterings stuff."
  :version "0.1"
  :author "George Kettleborough"
  :licence "GNU GPLv3"
  :serial t
  :components ((:file "packages")
               (:file "deltas")
               (:file "metrics")
               (:file "clusdiff")
               (:file "utils")
               (:file "criteria")))
