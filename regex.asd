(asdf:defsystem regex
  :version "0"
  :description "A Regular Expression Engine"
  :author "Michael Parker <mparker762@hotmail.com>"
  :licence "BSD-style"
  :components ((:file "packages")
               (:file "macs" :depends-on ("packages"))
               (:file "parser" :depends-on ("packages" "macs"))
               (:file "optimize" :depends-on ("packages" "macs"))
               (:file "gen" :depends-on ("packages" "macs"))
               (:file "closure" :depends-on ("packages" "macs"))
               (:file "regex" :depends-on ("packages" "macs" "parser"
                                                      "optimize" "gen" "closure"))
               (:file "regexp-test-suite" :depends-on ("packages" "regex"))
               (:file "retest" :depends-on ("packages" "regex" "regexp-test-suite"))
               (:static-file "regex.system" :pathname "regex.system")
               (:static-file "regex.translations" :pathname "regex.translations"))
  ;; :long-description ""
  )
