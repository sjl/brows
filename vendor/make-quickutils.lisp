(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :read-file-into-string
               :curry
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "BROWS.QUICKUTILS")
