(ns smash.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            ;[applied-science.js-interop :as j]
            [sitefox.ui :refer [log]]
            ["rot-js" :as ROT]
            [smash.physics :refer [simulate]]))

(log "loaded")

(defn initial-state []
  {:screen :menu})

(defonce state (r/atom (initial-state)))

(defn get-map []
  (js/console.log "hi")
  (let [digger (ROT/Map.Digger. 25 25 (clj->js {:corridorLength [2 3]
                                                :dugPercentage 0.3 ;TODO: increase this as you go deeper
                                                }))
        positions (atom {})]
    (js/console.log digger)
    (.create digger
             (fn [x y v]
               ; (js/console.log (clj->js args))
               (swap! positions assoc [x y] v)))
    (js/console.log "digger:" digger)
    (js/console.log (clj->js positions))
    (js/console.log (aget digger "_rooms"))
    (js/console.log (aget digger "_corridors"))
    {:tiles @positions
     :rooms (-> digger (aget "_rooms") js/JSON.stringify js/JSON.parse (js->clj :keywordize-keys true))
     :corridors (-> digger (aget "_corridors") js/JSON.stringify js/JSON.parse (js->clj :keywordize-keys true))
     :size [(aget digger "_width") (aget digger "_height")]
     :digger digger}))

(defn expand-pos [[x y]]
  [(- x 0.5) (+ y 0.5)])

(defn start-game [state]
  (swap! state
         #(-> %
              (assoc :map (get-map))
              (assoc :screen :game))))

(defn process-game-key [state ev]
  (js/console.log (aget ev "keyCode"))
  (case (aget ev "keyCode")
    27 (swap! state assoc :screen :menu)
    nil))

(defn generate-greebles [{:keys [x y width height]}]
  (let [greeble-count (inc (int (* (ROT/RNG.getUniform) 5)))]
    (for [_g (range greeble-count)]
      (let [gs (* (ROT/RNG.getUniform) 5)
            [rx ry] (map (fn [_] (ROT/RNG.getItem #js [1 2])) (range 2))
            [gx gy] (map #(* (ROT/RNG.getUniform) (nth [width height] %)) (range 2))
            [gxf gyf] (map #(+ (* (- (ROT/RNG.getUniform) 0.5) 20) %) [gx gy])]
        (case (ROT/RNG.getItem #js [:circle :arc])
          :circle [:circle {:cx (+ x gx) :cy (+ y gy) :r gs
                            :fill "none"
                            :stroke "#5A5A56"
                            :stroke-width 1}]
          :arc [:path {:d (str "M " (+ x gx) "," (+ y gy)
                               "A " rx " " ry " "
                               "0 0 1"
                               (+ x gxf) " " (+ y gyf))
                       :fill "none"
                       :stroke "#5A5A56"
                       :stroke-width 1}])))))

(defn is-adjacent-tile [[x y] tiles]
  (>
   (count
     (filter identity
             (for [ox [-1 0 1]
                   oy [-1 0 1]]
               (= (get tiles [(+ ox x) (+ oy y)]) 0))))
   0))

(defn component-menu [state]
  [:div#menu
   [:h1 "ball smash" [:br] "dungeon"]
   [:p "a video game about smashing balls"]
   (when (:map @state)
     [:p [:button {:on-click #(swap! state assoc :screen :game)} "resume game"]])
   [:p [:button {:on-click #(start-game state)} "new game"]]])

(defn component-defs [scale]
  [:defs
   [:pattern {:id "hatch"
              :x 0
              :y 0
              :width scale
              :height scale
              :pattern-units "userSpaceOnUse"}
    [:rect {:x 0 :y 0 :width scale :height scale :fill "#FAF8F1"}]
    [:path {:d (str
                 "M " 0 ",0"
                 "V " scale
                 "M " (/ scale 3) ",0"
                 "V " scale
                 "M " (* (/ scale 3) 2) ",0"
                 "V " scale)
            :stroke-width 1
            :stroke "#555"}]]])

(defn component-game [state]
  (js/console.log "state" (clj->js @state))
  (let [scale 30
        stroke-width 7
        game-map (:map @state)
        {:keys [size rooms corridors tiles]} game-map
        _tile-positions (remove nil?
                               (for [[[x y] v] tiles]
                                 (when (= v 0)
                                   {:key (str "tile" [x y])
                                    :x (* (- x 0.5) scale)
                                    :y (* (- y 0.5) scale)
                                    :width scale
                                    :height scale})))
        adjacent-positions (remove nil?
                                   (for [[[x y] v] tiles]
                                     (when (and (= v 1) (is-adjacent-tile [x y] tiles))
                                       {:key (str "adjacent" [x y])
                                        :cx (* x scale)
                                        :cy (* y scale)
                                        :r (* scale 0.75)
                                        :x (* (- x 0.5) scale)
                                        :y (* (- y 0.5) scale)
                                        :width scale
                                        :height scale})))
        room-positions (for [r rooms]
                         (let [{:keys [_x1 _x2 _y1 _y2 _doors]} r
                               xs (expand-pos (sort [_x1 _x2]))
                               ys (expand-pos (sort [_y1 _y2]))
                               w (- (second xs) (first xs))
                               h (- (second ys) (first ys))
                               doors (for [[xy _v] _doors]
                                       (let [[x y] (-> xy name (.split ",") (.map #(js/parseInt %)))]
                                         {:key (str "door" [x y] xs ys)
                                          :x (* (- x 0.5) scale)
                                          :y (* (- y 0.5) scale)
                                          :width scale
                                          :height scale}))]
                           {:key (str "room" xs ys)
                            :x (* (first xs) scale)
                            :y (* (first ys) scale)
                            :width (* w scale)
                            :height (* h scale)
                            :doors doors}))
        corridor-positions (for [c corridors]
                             (let [{:keys [_startX _startY _endX _endY]} c
                                   [_x1 _x2 _y1 _y2] [_startX _endX _startY _endY]
                                   xs (expand-pos (sort [_x1 _x2]))
                                   ys (expand-pos (sort [_y1 _y2]))
                                   w (- (second xs) (first xs))
                                   h (- (second ys) (first ys))]
                               {:key (str "corridor" xs ys)
                                :x (* (first xs) scale)
                                :y (* (first ys) scale)
                                :width (* w scale)
                                :height (* h scale)}))
        [sx sy] (map #(+ (* % scale) (* scale 2)) size)
        room1 (first room-positions)
        thing {:x (+ (:x room1) (/ (:width room1) 2))
               :y (+ (:y room1) (/ (:height room1) 2))
               :radius 10
               :velocity [0.05 0.05]}]
    [:svg {:on-key-down #(process-game-key state %)
           :on-click #(simulate adjacent-positions [thing])
           :tabIndex 0
           :ref #(when % (.focus %))
           :viewBox (str "-" scale " -" scale " " sx " " sy)
           :width sx
           :height sy}

     [component-defs scale]

     ; draw hatching

     (for [{:keys [cx cy r] :as pos} adjacent-positions]
       [:g {:transform (str "rotate(" (* (js/Math.random) 360) " " cx " " cy ")" )}
        [:circle {:key (:key pos)
                  :fill "url(#hatch)"
                  :cx (+ cx (* (- (js/Math.random) 0.5) (* scale 0.5)))
                  :cy (+ cy (* (- (js/Math.random) 0.5) (* scale 0.5)))
                  :r r}]])

     ; draw outlines

     (for [pos-map room-positions]
       [:rect (merge pos-map
                     {:stroke "#5A5A56"
                      :stroke-width stroke-width
                      :stroke-linejoin "round"})])

     (for [{:keys [doors]} room-positions]
       (for [pos-map doors]
         [:rect (merge pos-map {:stroke "#5A5A56"
                                :stroke-width stroke-width
                                :stroke-linejoin "round"})]))

     (for [pos-map corridor-positions]
       [:rect (merge pos-map {:stroke "#5A5A56"
                              :stroke-width stroke-width
                              :stroke-linejoin "round"})])

     ; draw interiors

     (for [{:keys [doors]} room-positions
           pos-map doors]

         [:rect (merge pos-map {:fill "#E9E7DC"})])

     (for [pos-map room-positions]
       [:rect (merge pos-map {:fill "#E9E7DC"})])

     (for [pos-map corridor-positions]
       [:rect (merge pos-map {:fill "#E9E7DC"})])

     ; draw greebles
     (for [pos room-positions]
       (generate-greebles pos))

    (for [pos corridor-positions]
       (generate-greebles pos))]))

(defn component-main [state]
  [:main
   (case (:screen @state)
     :menu [component-menu state]
     :game [component-game state])])

(defn start {:dev/after-load true} []
  (rdom/render [component-main state]
               (js/document.getElementById "app")))

(defn init []
  (start))
