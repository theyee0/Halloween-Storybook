#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require 'asdf)
(load "halloween-storybook.asd")
(asdf:load-system "halloween-storybook")
(setq uiop:*image-entry-point* #'halloween-storybook::main)
(uiop:dump-image "halloween-storybook.exe" :executable t)
