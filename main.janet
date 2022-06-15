#!/usr/bin/env janet
(use jaylib)

(use "./tetroids")

(def block-size 30)
(def player-count 1)

(def field-width 10)
(def field-height 20)

(var normal-frames-per-tick 60)
(var fast-frames-per-tick 5)

(var frame-counter -1)
(var frames-per-tick normal-frames-per-tick)

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

(defn random-0-or-1 []
  (if (> (math/random) 0.5) 1 0))

(defn random-field []
  (map (fn [_] (map (fn [_] (random-0-or-1)) (range field-width))) (range field-height)))

(defn make-field [player-number]
  @{:player-number player-number
    :current-tetromino nil
    :cells (empty-field)})

(defn make-state []
  @{:fields (map (fn [x] (make-field x)) (range player-count))})

(defn field-row-to-string [row]
  (string/join (map (fn [cell] (string cell)) row)
               " "))

(defn print-row [row]
  (print (field-row-to-string row)))

(defn print-field [field]
  (map print-row field))

(defn make-tetromino [shape x y]
  @{:shape shape
    :x x
    :y y
    :orientation 0})

(defn set-player-key [player key value]
  (set (((state :fields) player) key) value))

(defn get-player-key [player key]
  (((state :fields) player) key))

(defn get-player-field [player]
  ((state :fields) player) :cells)

(defn spawn-tetromino [player]
  (set-player-key player :current-tetromino (make-tetromino :bar 0 0)))

(defn field-cell-at-x-y [x y]
  (let [field-index 0
        field-cells (((state :fields) field-index) :cells)]
        ((field-cells y) x)))

(defn detect-tetromino-collision [tetromino]
  (var does-collide false)
  (let [shape (tetroids (tetromino :shape))
        orientation (tetromino :orientation)
        rotated-shape (shape orientation)
        x (tetromino :x)
        y (tetromino :y)]
    (eachp (row-index row) rotated-shape
      (eachp (column-index cell) row
        (if (= does-collide false)
          (if (> cell 0)
            (let [cell-x (+ column-index x)
                  cell-y (+ row-index y)]
              (if (or
                (< cell-x 0) # check field left boundary
                (>= cell-x field-width) # check field right boundary
                (>= cell-y field-height) # check field bottom boundary
                (> (field-cell-at-x-y cell-x cell-y) 0)) # check collision with field blocks
                  (set does-collide true))))))))
    does-collide)

(defn advance-tetromino [player]
  (let [tetromino (get-player-key player :current-tetromino)
        original-y ((get-player-key player :current-tetromino) :y)
        new-y (+ 1 ((get-player-key player :current-tetromino) :y))]
    (set ((((state :fields) player) :current-tetromino) :y) new-y)
    (if (detect-tetromino-collision tetromino)
      (set ((((state :fields) player) :current-tetromino) :y) original-y))))

(defn advance-tetrominos []
  (for i 0 player-count (advance-tetromino i)))

(defn wrapped-inc [x limit]
  (% (+ 1 x) limit))

(defn wrapped-dec [x limit]
  (if (= x 0) (- limit 1) (- x 1)))

(defn clamp [number min max]
  (cond (> number max) max
    (< number min) min
    :else number))

# TODO this is not taking the player number into account
(defn rotate-tetromino [dir]
  (let [tetromino (((state :fields) 0) :current-tetromino)
        shape (tetromino :shape)
        orientation (tetromino :orientation)
        new-orientation
        (if (= dir :cw)
          (wrapped-inc orientation (length (tetroids shape)))
          (wrapped-dec orientation (length (tetroids shape))))]
    (set ((((state :fields) 0) :current-tetromino) :orientation) new-orientation)))

(defn strafe-left []
  (let [tetromino (get-player-key 0 :current-tetromino)
        current-x (tetromino :x)
        next-x (- current-x 1)]
    (set (tetromino :x) next-x)
    (if (detect-tetromino-collision tetromino)
      (set (tetromino :x) current-x))))

(defn strafe-right []
  (let [tetromino (get-player-key 0 :current-tetromino)
        current-x (tetromino :x)
        next-x (+ current-x 1)]
    (set (tetromino :x) next-x)
    (if (detect-tetromino-collision tetromino)
      (set (tetromino :x) current-x))))


(defn handle-input []
  (if (key-pressed? :x) (rotate-tetromino :cw))
  (if (key-pressed? :z) (rotate-tetromino :ccw))
  (if (key-pressed? :right) (strafe-right))
  (if (key-pressed? :left) (strafe-left))
  (if (key-down? :down)
    (set frames-per-tick fast-frames-per-tick)
    (set frames-per-tick normal-frames-per-tick)))

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
  (set state (make-state))
  (spawn-tetromino 0))

(defn is-tick? []
  (and
    (not (= frame-counter 0))
    (= (% frame-counter frames-per-tick) 0)))

(defn update-state []
  (++ frame-counter)
  (handle-input)
  (if (is-tick?)
    (advance-tetrominos)))

#
# Field Drawing
#

(defn draw-field-border [field-index]
  (let [field-px-width (* field-width block-size)
        field-px-height (* field-height block-size)
        x-offset (+ (* field-index field-px-width) (* (+ field-index 1) block-size))
        y-offset block-size]
    (draw-rectangle-lines x-offset y-offset field-px-width field-px-height :white)))

(defn draw-field-block [field-index row-index cell-index cell color]
  (if (= cell 1)
    (let [field-px-width (* field-width block-size)
          field-px-height (* field-height block-size)
          field-x-offset (+ (* field-px-width field-index) block-size)
          block-x-offset (* block-size cell-index)
          field-y-offset block-size
          x-offset (+ field-x-offset block-x-offset)
          y-offset (+ field-y-offset (* block-size row-index))]
      (draw-rectangle x-offset y-offset block-size block-size color))))

(defn draw-field-blocks [field-index]
  (let [cells (((state :fields) field-index) :cells)]
    (eachp (row-index row) cells
      (eachp (cell-index cell) row
        (draw-field-block field-index row-index cell-index cell :blue)))))

(defn draw-current-tetromino [field-index]
  (let [tetromino (((state :fields) field-index) :current-tetromino)
        shape (tetromino :shape)
        orientation (tetromino :orientation)
        x (tetromino :x)
        y (tetromino :y)]
    (eachp (row-index row) ((tetroids shape) orientation)
      (eachp (cell-index cell) row
        (draw-field-block field-index (+ row-index y) (+ cell-index x) cell :red)))))

(defn draw-field [field-index]
  (draw-field-blocks field-index)
  (draw-current-tetromino field-index)
  (draw-field-border field-index))

(defn draw-fields []
  (for i 0 player-count (draw-field i)))

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

(engine/loop my-init
             update-state
             draw
             screen-width screen-height "Jetris")
