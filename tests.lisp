(in-package :statecharts)
;; implementation of the statecharts watch example

(defparameter /alarm-1-status/
  (c "alarm 1 status" (d "disabled")
    (s "disabled")
    (s "enabled")
    (-> "d" "disabled" "enabled")
    (-> "d" "enabled" "disabled")))

(defparameter /alarm-2-status/
  (c "alarm 2 status" (d "disabled")
    (s "disabled")
    (s "enabled")
    (-> "d" "disabled" "enabled")
    (-> "d" "enabled" "disabled")))

(defparameter /chime-status/
  (c "chime status" (d "disabled")
    (s "disabled")
    (s "enabled")
    (-> "d" "disabled" "enabled")
    (-> "d" "enabled" "disabled")))

