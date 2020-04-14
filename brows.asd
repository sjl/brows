(asdf:defsystem :brows
  :description "Opens URLs in browsers"

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :version "0.0.1"

  :depends-on (

               :boots
               :cl-ppcre
               :external-program
               :iterate

               )

  :serial t
  :components
  ((:module "src" :serial t :components
    ((:file "package")
     (:file "main")))))
