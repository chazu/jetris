#!/usr/bin/env janet
(use jaylib)

(def block-size 10)
(def player-count 3)

(def field-width 10)
(def field-height 20)

(def screen-width (* field-width player-count block-size))
(def screen-height (* field-height block-size))

(var state nil)

(math/seedrandom (os/cryptorand 8))

(defn make-state []
  @{})

#
# Field
#

(defn empty-field []
  (map (fn [_] (map (fn [_] 0) (range 10))) (range 20)))

(defn field-row-to-string [row]
  (string/join (map (fn [cell] (string cell)) row)
               " "))

(defn print-row [row]
  (print (field-row-to-string row)))

(defn print-field [field]
  (map print-row field))

(defn make-field [player-number]
  @{
    :player-number player-number
    :data "poop"
   })

# Possible TODO - add hook for handling input _before_ update state
(defn engine/loop [init-fn update-fn draw-fn width height window-title]
  (init-window width height window-title)
  (set-target-fps 60)
  (init-fn)
  (while (not (window-should-close))
    (update-fn)
    (draw-fn))
  (close-window))

(defn my-init []
  (set state (make-state)))

(defn update-state []
  )

(defn draw []
  (begin-drawing)
  (clear-background :black)
  (end-drawing))

######################
# grip it and rip it #
######################

#(engine/loop my-init update-state draw screen-width screen-height "Jetris")
(var field (empty-field))
(var test-row @[0 0 0 0 0 0 1 0 0 1])

(print-field field)
