(defpackage discotl
  (use :cl :asdf))
(use-package :discotl)

(defsystem discotl
    :depends-on (:plump
                 :dexador))
