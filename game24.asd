;;;; game24.asd

(asdf:defsystem #:game24
    :serial t
    :depends-on (#:hunchentoot
                 #:html-template
                 #:jsown
                 #:lazy-bone)
    :components ((:file "package")
		 (:file "game24")))
