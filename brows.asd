(asdf:defsystem :brows
  :description "Opens URLs in browsers"

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (

               :boots
               :cl-ppcre
               :deploy
               :iterate
               :losh

               )

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "brows"
  :entry-point "brows:toplevel"

  :serial t
  :components
  ((:module "vendor" :serial t
    :components ((:file "quickutils-package")
                 (:file "quickutils")))
   (:file "package")
   (:module "src" :serial t
    :components
    ((:file "main")))))