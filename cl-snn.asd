(asdf:defsystem "cl-snn"
  :description "cl-snn: Clozure CL compatible simulator for spiking neural nets."
  :version "0.0.1"
  :author "bob <bobatnet@gmail.com>"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:module "octave"
				:components ((:file "ext-octave")))			
	       (:file "combinatorics")
	       (:file "csv-parser")
	       (:file "def-clsnn")
	       (:file "snn-aux")
	       (:file "snn-macros")
	       (:file "snn2")
	       (:file "snn-form-side")
	       (:file "snn-scratchpad")))
