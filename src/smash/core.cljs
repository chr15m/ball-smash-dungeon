(ns smash.core
  {:clj-kondo/config '{:lint-as {applied-science.js-interop/let clojure.core/let}}}
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [applied-science.js-interop :as j]
            [sitefox.ui :refer [log]]
            ["rot-js" :as ROT]
            ["djb2a$default" :as djb2a]
            [smash.physics :refer [simulate]]))

(log "loaded")

(def initial-scale 25)
(def map-size 25)

(defn initial-state []
  {:screen :menu})

(defonce state (r/atom (initial-state)))

(defn make-rng [& args]
  (js/console.log "make-rng" (.join (clj->js args) "-"))
  (-> (ROT/RNG.clone) (.setSeed (djb2a (.join (clj->js args) "-")))))

(defn make-map []
  (js/console.log "hi")
  (let [digger (ROT/Map.Digger. map-size map-size
                                (clj->js {:corridorLength [1 7]
                                          :roomWidth [4 8]
                                          :roomHeight [4 8]
                                          :dugPercentage 0.15 ;TODO: increase this as you go deeper
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

(defn is-adjacent-tile [[x y] tiles]
  (>
   (count
     (filter identity
             (for [ox [-1 0 1]
                   oy [-1 0 1]]
               (= (get tiles [(+ ox x) (+ oy y)]) 0))))
   0))

(defn calculate-level [game-map scale]
  (let [{:keys [rooms corridors tiles]} game-map
        tile-positions (remove nil?
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
                                :height (* h scale)}))]
    {:tile-positions tile-positions
     :adjacent-positions adjacent-positions
     :room-positions room-positions
     :corridor-positions corridor-positions}))

(defn start-game [state]
  (let [scale initial-scale
        seed (-> (random-uuid) str (.replace (js/RegExp. "-" "g") "") (.substr 0 16))
        game-map (make-map)
        level (calculate-level game-map scale)
        room1 (first (:room-positions level))
        player {:player true
                :x (+ (:x room1) (/ (:width room1) 2))
                :y (+ (:y room1) (/ (:height room1) 2))
                :radius (/ scale 3)
                :velocity [(* (- (js/Math.random) 0.5) 10) (* (- (js/Math.random) 0.5) 10)]
                :uuid (.slice (str (random-uuid)) 0 8)}]
    (swap! state
           #(-> %
                (assoc :seed seed
                       :scale scale
                       :game-map game-map
                       :screen :game
                       :entities [player]
                       :level (calculate-level game-map scale))
                (dissoc :simulation)))))

(defn process-game-key [state ev]
  (js/console.log (aget ev "keyCode"))
  (case (aget ev "keyCode")
    27 (swap! state assoc :screen :menu)
    nil))

(defn generate-greebles [rng {:keys [x y width height]}]
  (let [greeble-count (inc (int (* (.getUniform rng) 5)))]
    (for [_g (range greeble-count)]
      (let [gs (* (.getUniform rng) 5)
            [rx ry] (map (fn [_] (.getItem rng #js [1 2])) (range 2))
            [gx gy] (map #(* (.getUniform rng) (nth [width height] %)) (range 2))
            [gxf gyf] (map #(+ (* (- (.getUniform rng) 0.5) 20) %) [gx gy])]
        (case (.getItem rng #js [:circle :arc])
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

(defn run-simulation! [state adjacent-tiles entities]
  (swap! state assoc :simulation (simulate adjacent-tiles entities)))

; *** components *** ;

(defn component-menu [state]
  [:div#menu
   [:h1 "ball smash" [:br] "dungeon"]
   [:p "a video game about smashing balls"]
   (when (:game-map @state)
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

(defn offset-tuple [steps s]
  (str
    (int (- (-> s :position :x) (-> steps first :position :x)))
    " "
    (int (- (-> s :position :y) (-> steps first :position :y)))))

(defn compute-path [steps]
  (str
    "M 0 0"
    (apply str
           (for [s (rest steps)]
             (str "L " (offset-tuple steps s))))))

(defn animate-body [steps]
  (let [ms (* (count steps) (/ 1000 120))]
    [:circle {:cx (-> steps first :position :x)
              :cy (-> steps first :position :y)
              :r (-> steps first :entity :radius)
              :fill "#5A5A56"}
     [:animateMotion {:dur (str ms "ms")
                     :repeatCount 1
                     :fill "freeze" ; pause at the end
                     :begin "50ms"
                     :path (compute-path steps)}]]))

(defn component-animated-bodies [state]
  (let [simulation (-> @state :simulation :sim)
        animated-bodies
        (vals (reduce
                (fn [body-steps step]
                  (reduce
                    (fn [body-steps body]
                      (update-in body-steps [(:uuid body)] conj body))
                    body-steps step))
                {} simulation))]
    [:g {:ref (fn [el]
                ; reset the SVG timing so the animations play
                (when el
                  (j/call-in el [:parentNode :setCurrentTime] 0)))}
     (for [body animated-bodies]
       (animate-body (reverse body)))]))

(defn component-simulation-count [state]
  (let [simulation (-> @state :simulation :sim)]
    [:text {:x 0 :y 0} (count simulation) " frames"]))

(defn component-background [seed level scale]
  (let [rng (make-rng "bg" seed)
        stroke-width 7
        {:keys [adjacent-positions room-positions corridor-positions]} level]
    [:g
     ; draw hatching
     (for [{:keys [cx cy r] :as pos} adjacent-positions]
       [:g {:transform (str "rotate(" (* (.getUniform rng) 360) " " cx " " cy ")" )}
        [:circle {:key (:key pos)
                  :fill "url(#hatch)"
                  :cx (+ cx (* (- (.getUniform rng) 0.5) (* scale 0.5)))
                  :cy (+ cy (* (- (.getUniform rng) 0.5) (* scale 0.5)))
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
       [:rect (merge (dissoc pos-map :doors) {:fill "#E9E7DC"})])

     (for [pos-map corridor-positions]
       [:rect (merge pos-map {:fill "#E9E7DC"})])

     ; draw greebles

     (for [pos room-positions]
       (generate-greebles rng pos))

     (for [pos corridor-positions]
       (generate-greebles rng pos))]))

(defn component-game [state]
  (js/console.log "state" (clj->js @state))
  (let [{:keys [scale game-map level seed entities]} @state
        size (:size game-map)
        [sx sy] (map #(+ (* % scale) (* scale 2)) size)]
    [:svg {:on-key-down #(process-game-key state %)
           :on-click #(run-simulation! state (:adjacent-positions level) entities)
           :tabIndex 0
           :ref #(when % (.focus %))
           :viewBox (str "-" scale " -" scale " " sx " " sy)
           :width sx
           :height sy}

     [component-defs scale]

     [component-background seed level scale]

     [component-simulation-count state]

     [component-animated-bodies state]

     #_ (for [step simulation
           body step]
       [:circle {:cx (-> body :position :x)
                 :cy (-> body :position :y)
                 :r (-> body :entity :radius)
                 :fill "none"
                 :stroke "red"}])

     ; draw level rects
     #_ (for [body (-> @state :simulation :bodies)]
          (let [v1 (nth (:vertices body) 0)
                v2 (nth (:vertices body) 1)
                v3 (nth (:vertices body) 2)
                v4 (nth (:vertices body) 3)]
            [:path {:d (str "M " (:x v1) " " (:y v1) " "
                            "L " (:x v2) " " (:y v2) " "
                            "L " (:x v3) " " (:y v3) " "
                            "L " (:x v4) " " (:y v4) " "
                            "L " (:x v1) " " (:y v1))
                    :stroke "red"
                    :fill "none"}]))]))

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
