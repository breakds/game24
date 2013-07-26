;;;; package.lisp

(defpackage #:breakds.game24
  (:nicknames #:game24)
  (:use #:cl
        #:lazy-bone
        #:hunchentoot
        #:exmac)
  (:import-from #:parenscript #:ps* #:ps #:create
                #:chain #:defpsmacro #:new #:getprop
                #:@ #:for-in #:eql)
  (:export #:start-server
           #:stop-server))
           

