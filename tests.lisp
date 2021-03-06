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

(defparameter /light/
  (c "light" (d "off")
    (s "off")
    (s "on")
    (-> "b" "off" "on")
    (-> "^b" "off" "on")))

(defparameter /power/
  (c "power" (d "ok")
    (s "ok")
    (s "blink")
    (-> "battery weakens" "ok" "blink")))

(defparameter /chime-status/
  (c "chime status" (d "disabled")
    (s "disabled")
    (c "enabled" (d "quiet")
      (s "quiet")
      (s "beep")
      (-> "T is whole hour" "quiet" "beep")
      (-> "2 seconds in beep" "beep" "quiet"))
    (-> "d" "disabled" "enabled")
    (-> "d" "enabled" "disabled")))


(defparameter /regular/
  (o "reg outer" ()
    (c "regular" (d "time")
      (s "time")
      (s "date")
      (c "update" (h "sec")
	(s "sec")
	(s "1 min")
	(s "10 min")
	(s "hr")
	(s "month")
	(s "date")
	(s "day")
	(s "year")
	(s "mode")
	(-> "c" "sec" "1 min")
	(-> "c" "1 min" "10 min")
	(-> "c" "10 min" "hr")
	(-> "c" "hr" "month")
	(-> "c" "month" "date")
	(-> "c" "date" "day")
	(-> "c" "day" "year")
	(-> "c" "year" "mode")
	)
      (-> "2 min in update" "update" "time")
      (-> "c" '("update" "mode") "time")
      (-> "b" "update" "time")
      (-> "d" "time" "date")
      (-> "d" "date" "time"))
    (c "beep-test" (d "00")
      (s "00")
      (s "10")
      (s "01")
      (s "beep")
      (-> "b" "00" "10")
      (-> "^b" "10" "00")
      (-> "b" "01" "beep")
      (-> "^b" "beep" "01")
      (-> "d" "00" "01")
      (-> "^d" "01" "00")
      (-> "d" "10" "beep")
      (-> "^d" "beep" "10"))))


(defparameter /out/
  (c "out" (d "alarm 1")
    (c "chime" (h "off")
      (s "off")
      (s "on")
      (-> "d" "on" "off")
      (-> "d" "off" "on"))
    (c "alarm 1" (h "off")
      (s "off")
      (s "on")
      (-> "d" "off" "on")
      (-> "d" "on" "off"))
    (c "alarm 2" (h "off")
      (s "off")
      (s "on")
      (-> "d" "off" "on")
      (-> "d" "on" "off"))
    (c "update 1" (h "hr")
      (s "hr")
      (s "10 min")
      (s "1 min")
      (-> "c" "hr" "10 min")
      (-> "c" "10 min" "1 min"))
    (c "update 2" (h "hr")
      (s "hr")
      (s "10 min")
      (s "1 min")
      (-> "c" "hr" "10 min")
      (-> "c" "10 min" "1 min"))
    (-> "a" "alarm 2" "chime")
    (-> "a" "alarm 1" "alarm 2")
    (-> "c" "alarm 1" '("update 1" "hr"))
    (-> "c" '("update 1" "1 min") "alarm 1")
    (-> "b" "update 1" "alarm 1")
    (-> "c" "alarm 2" '("update 2" "hr"))
    (-> "c" '("update 2" "1 min") "alarm 2")
    (-> "b" "update 2" "alarm 2")))


(defparameter /displays/
  (c "displays" (d "reg outer")
    (s "wait")
    /regular/
    /out/
    (-> "2 sec in wait" "wait" '("reg outer" "regular" "update" "sec"))
    (-> "^c" "wait" '("reg outer" "regular" "time"))
    (-> "c" '("reg outer" "regular" "time") "wait")))

(defparameter /main/
  (o "main" ()
    /displays/
    /alarm-1-status/
    /alarm-2-status/
    /chime-status/
    /light/
    /power/))

(defparameter /watch/
  (c "watch" (d "dead")
    (s "dead")
    /main/
    (-> "weak battery dies" "main" "dead")
    (-> "battery removed" "main" "dead")
    (-> "battery inserted" "dead" "main")))


(defstatechart (sc/watch) /watch/)
  

(defparameter *watch-fsm*
  (make-instance 'environment :fsm (sc:create-fsm-runtime sc/watch :debug t)))


#+nil
(sb-sprof:with-profiling ()
  (defstatechart (sc/watch) /watch/))


