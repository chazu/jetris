#!/usr/bin/env janet
(use jaylib)

(def block-size 30)
(def player-count 3)

(def field-width 10)
(def field-height 20)

(def screen-width
  (let [pad (* (+ 1 player-count) block-size)]
    (+ (* field-width player-count block-size) pad)))
(def screen-height 
  (let [pad (* block-size 2)]
    (+ (* field-height block-size) pad)))

(var state nil)

(math/seedrandom (os/cryptorand 8))

#
# Field
#

(defn empty-field []
  (map (fn [_] (map (fn [_] 0) (range field-width))) (range field-height)))

(defn make-state []
  @{:fields (map (fn [_] (empty-field)) (range player-count))})

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

(defn draw-field-border [field-number]
  (let [field-px-width (* field-width block-size)
        field-px-height (* field-height block-size)
        x-offset (+ (* field-number field-px-width) (* (+ field-number 1) block-size))
        y-offset block-size]
    (draw-rectangle-lines x-offset y-offset field-px-width field-px-height :white)))

(defn draw-field-blocks[]
)

(defn draw-field [field-index]
  (let [field ((state :fields) field-index)]
    (for i 0 player-count
      (draw-field-border i)
      (draw-field-blocks))))

(defn draw-fields []
  (for i 0 player-count (draw-field i)))
  
(defn draw []
  (begin-drawing)
  (clear-background :black)
  (draw-fields)
  (end-drawing))

######################
# grip it and rip it #
######################

(engine/loop my-init update-state draw screen-width screen-height "Jetris")
# (var field (empty-field))
# (var test-row @[0 0 0 0 0 0 1 0 0 1])

# (print-field field)
