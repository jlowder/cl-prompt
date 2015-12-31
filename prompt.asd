(defsystem prompt
    :name "Prompt"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :licence "MIT"
    :description "Generic command line prompt support"
    :long-description "Library that provides a simple interactive command-line prompt."
    :depends-on (:cl-ppcre)
    :components ((:file "prompt")))
