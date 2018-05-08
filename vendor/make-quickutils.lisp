(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :ensure-list
               :once-only
               :rcurry
               :read-file-into-string
               :symb
               :with-gensyms

               )
  :package "BROWS.QUICKUTILS")
